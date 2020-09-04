unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  {Custom Exceptions}
  EDupHandle = class(Exception);

  TForm1 = class(TForm)
    MainCounterEdit: TEdit;
    Thread1CounterEdit: TEdit;
    Thread1Button: TButton;
    MainButton: TButton;
    MaxCountEdit: TEdit;
    CountToLabel: TLabel;
    MainCounterLabel: TLabel;
    Thread1CounterLabel: TLabel;
    Thread2CounterEdit: TEdit;
    Thread2Button: TButton;
    Thread2CounterLabel: TLabel;
    Thread3CounterEdit: TEdit;
    Thread3Button: TButton;
    Thread3CounterLabel: TLabel;
    Bevel1: TBevel;
    CommType: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure MainButtonClick(Sender: TObject);
    procedure UpdateValue;
    procedure ProtectMaxCount(Locked: Boolean);
    procedure ThreadButtonClick(Sender: TObject);
    procedure WaitForThreads;
  private
    { Private declarations }
    FGUIProcHandle : THandle;
  public
    { Public declarations }
  end;

  {Define the processing thread}
  TMyProcessingThread = class(TThread)
    FID          : Integer;
    FThreadValue : Cardinal;
    FCommType    : Integer;
    FMaxValue    : Integer;
  private
    FCallerThread  : Cardinal;
    FHPCFreq       : Int64;           {Frequency of high-performance system clock, if one exists}
    procedure UpdateValue;
    procedure Done;
  protected
    procedure Execute; override;
  public
    constructor Create(ID: Integer; CallingThread: Cardinal; CommType: Integer; MaxValue: Integer); reintroduce;
  end;

  {Define regular procedure}
  procedure UpdateThreadEdit(Value: Cardinal); stdcall;

var
  Form1: TForm1;
  MainValue : Integer;
  MyThread : array[0..2] of TMyProcessingThread;
  ThreadButton : array[0..2] of TButton;
  ThreadEdit : array[0..2] of TEdit;
  Processing : Integer;                              {Flag indicating number of threads processing}

const
  ThreadDone = MaxDWORD and $3FFFFFFF;                 {Our indicator in APC calls to UpdateThreadEdit that the thread is done}

implementation

{$R *.dfm}

{##############################################################################}
{##############################################################################}
{############################## TForm1 Routines ###############################}
{##############################################################################}
{##############################################################################}

procedure TForm1.FormCreate(Sender: TObject);
begin
  {Duplicate our "GUI" thread's pseudo-handle to make it usable by any of our threads}
  if not DuplicateHandle(GetCurrentProcess, GetCurrentThread, GetCurrentProcess, @FGUIProcHandle, 0, False, DUPLICATE_SAME_ACCESS) then
    FGUIProcHandle := INVALID_HANDLE_VALUE; {Failed to create process handle}
  {Collect thread buttons and edits}
  ThreadButton[0] := Thread1Button;
  ThreadButton[1] := Thread2Button;
  ThreadButton[2] := Thread3Button;
  ThreadEdit[0] := Thread1CounterEdit;
  ThreadEdit[1] := Thread2CounterEdit;
  ThreadEdit[2] := Thread3CounterEdit;
end;

{------------------------------------------------------------------------------}

procedure TForm1.MainButtonClick(Sender: TObject);
{GUI Thread's counting method.  This serves as the visual model for Processing Threads' counting routine as well, though that is handled elsewhere}
var
  Idx : Integer;
  MaxValue : Integer;
begin
  inc(Processing);
  {Disable changes to "Count To" or main button clicks}
  ProtectMaxCount(True);
  TButton(Sender).Enabled := False;
  {Perform counting operation}
  MaxValue := StrToInt(MaxCountEdit.Text);
  for Idx := 0 to MaxValue do
    begin
    MainValue := Idx;
    UpdateValue;
    end;
  {Signal done; re-enable changes and main button clicks as appropriate}
  MainButton.Enabled := True;
  dec(Processing);
  if Processing = 0 then ProtectMaxCount(False);
end;

{------------------------------------------------------------------------------}

procedure TForm1.ThreadButtonClick(Sender: TObject);
begin
  inc(Processing);
  {Abort if no GUI Thread process handle}
  if FGUIProcHandle = INVALID_HANDLE_VALUE then raise EDupHandle.Create('Unable to create GUI handle; thread processing disabled.');
  {Block further "Count To" changes or this thread's button clicks}
  ProtectMaxCount(True);
  TButton(Sender).Enabled := False;
  {Perform counting operation (it its own thread)}
  MyThread[TEdit(Sender).Tag] := TMyProcessingThread.Create(TEdit(Sender).Tag, FGUIProcHandle, CommType.ItemIndex, StrToInt(MaxCountEdit.Text));
  {Wait for thread messages (if QueueUserAPC() communication type selected)}
  if CommType.ItemIndex = 1 {QueueUserAPC} then WaitForThreads;
end;

{------------------------------------------------------------------------------}

procedure TForm1.WaitForThreads;
{Wait for processing threads to finish.  This is accomplished by sleeping in an "alertable" state for a short time; waking upon the next thread message or time expiration.
 Once woken, either the UpdateThreadEdit regular procedure is executed (if a processing thread queued a request via asynchronous procedure call) or Windows messages are processed
 for this application via  Application.ProcessMessages.
 This method finishes and exits when all threads are done processing.}
begin
  while Processing > 0 do {Wait for processing threads to finish; meanwhile update the screen (directly through Application.ProcessMessages / indirectly through threaded APC calls to UpdateThreadEdit)}
    if (sleepex(100, True) <> STATUS_USER_APC) then Application.ProcessMessages;
end;

{------------------------------------------------------------------------------}

procedure TForm1.UpdateValue;
{Main GUI thread's "update" method.}
begin
  MainCounterEdit.Text := IntToStr(MainValue);
//  Form1.Edit1.Update;
  Application.ProcessMessages;
end;

{------------------------------------------------------------------------------}

procedure TForm1.ProtectMaxCount(Locked: Boolean);
begin
  MaxCountEdit.ReadOnly := Locked;
  case Locked of
    True : MaxCountEdit.Color := clBtnShadow;
    False: MaxCountEdit.Color := clWindow;
  end;
end;




{##############################################################################}
{##############################################################################}
{####################### Regular Procedure (non-class) ########################}
{##############################################################################}
{##############################################################################}

procedure UpdateThreadEdit(Value: Cardinal);
{Update Thread's Edit.
 NOTE: This method is called by the TMyProcessingThread via APC (Asynchronous Procedure Call) when using the QueueUserAPC thread communication method.}
var
  ID : Integer;
begin
  {Extract thread ID and value}
  ID := Value shr 30;
  Value := Value and $3FFFFFFF;
  {Update GUI}
  if not (Value = ThreadDone) then
    begin {Thread still processing; show output}
    ThreadEdit[ID].Text := inttostr(Value);
    ThreadEdit[ID].Update;
    end
  else
    begin {Thread done; re-enable changes and button clicks as appropriate}
    ThreadButton[ID].Enabled := True;
    dec(Processing);
    if Processing = 0 then Form1.ProtectMaxCount(False);
    end;
end;




{##############################################################################}
{##############################################################################}
{######################## TMyProcessingThread Routines ########################}
{##############################################################################}
{##############################################################################}

{The TMyProcessingThread is created by the GUI thread as needed.  The TCommunicationThread runs unencumbered by any GUI messages; it has no GUI components to deal with,
 so it performs fast processing.  The TMyProcessingThread either makes Synchronize()'d (GUI-blocked) calls to its UpdateValue and Done methods or it makes queued (non-blocking)
 asynchronous procedure calls to the UpdateThreadEdit procedure.}

procedure TMyProcessingThread.UpdateValue;
{Thread's "GUI update" method; called within the GUI thread's context via Synchronize().}
begin
  ThreadEdit[FID].Text := IntToStr(FThreadValue);
//  FEdit.Update;
  Application.ProcessMessages;
end;

{------------------------------------------------------------------------------}

procedure TMyProcessingThread.Done;
{Thread's "Done" method; called within the GUI thread's context via Synchronize().}
begin
  ThreadButton[FID].Enabled := True;
  dec(Processing);
  if Processing = 0 then Form1.ProtectMaxCount(False);
end;

{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{ooooooooooooooooooooooooooooo Protected Routines ooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}

procedure TMyProcessingThread.Execute;
{Thread's highest execution method.}
var
  Idx : Cardinal;
begin
  {Perform counting operation}
  for Idx := 0 to FMaxValue do
    begin
    FThreadValue := Idx;
    case FCommType of
      0: Synchronize(UpdateValue);
      1: QueueUserAPC(@UpdateThreadEdit, FCallerThread, (FID shl 30) + (FThreadValue and $3FFFFFFF));
    end;
    end;
  {Signal done; re-enable changes and button clicks as appropriate}
  case FCommType of
    0: Synchronize(Done);
    1: QueueUserAPC(@UpdateThreadEdit, FCallerThread, (FID shl 30) + ThreadDone);
  end;
end;

{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{ooooooooooooooooooooooooooooooo Public Routines oooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}

constructor TMyProcessingThread.Create(ID: Integer; CallingThread: Cardinal; CommType: Integer; MaxValue: Integer);
{Create the thread. This method (TMyProcessingThread.Create) is the only method within this object that is executed in the context of the GUI thread.}
var
  Idx : Integer;
begin
  {NOTE: This method is executed in the context of the calling thread (GUI thread), making it safe to access global objects that are not thread-aware.}
  FreeOnTerminate := True;
//  OnTerminate := Finish;  {Thread cleanup method}}
  {Store caller, max value, and output edit information}
  FID := ID;
  FCallerThread := CallingThread;
  FCommType := CommType;
  FMaxValue := MaxValue;
//  {Try to get high performance counter frequency and convert it to milliseconds (FHPCFreq is 0 if no HPC available)}
//  QueryPerformanceFrequency(FHPCFreq);
//  if FHPCFreq > 0 then FHPCFreq := FHPCFreq div 1000;
  FThreadValue := 0;
  {Start thread}
  inherited Create(False);
end;

end.
