unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  {Custom Exceptions}
  EDupHandle = class(Exception);

  {Define form class that application's "GUI" thread processes}
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
    ThreadCountEdit: TEdit;
    ThreadCountLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure MainButtonClick(Sender: TObject);
    procedure ProtectMaxCount(Locked: Boolean);
    procedure ThreadButtonClick(Sender: TObject);
    procedure WaitForThreads;
    procedure Update;
  protected
    { Protected declarations }
    procedure IncProcs;
    procedure DecProcs;
  private
    { Private declarations }
    FGUIProcHandle : THandle;
  public
    { Public declarations }
  end;

  {Define types of thread communication possible (in this application)}
  TCommType = (Sync, QAPC);

  {Define the non-GUI processing thread class}
  TProcessingThread = class(TThread)
  private
    FID            : Cardinal;                        {The unique ID given to this instance; indicates what button and edit control is associated with the thread}
    FCounter       : Cardinal;                        {The counter value (this is what is "processed" by the thread in this example application)}
    FCommType      : TCommType;                       {Thread communication type (technique).  0 = communicate via blocking Synchronize() calls, 1 = communicate via non-blocking QueueUserAPC() calls}
    FMaxValue      : Cardinal;                        {Terminus point in "processing"; the highest value to count to}
    FCallerThread  : Cardinal;                        {The caller (creator) of this instance; communicate back to it.  This is the handle to the GUI thread in this example application}
//    FHPCFreq       : Int64;                           {Frequency of high-performance system clock, if one exists}
    procedure SyncUpdate;                             {Thread-initiated and GUI-Synchronize()'d update of display routine.  Used only when FCommType is Synch in this example application}
    procedure SyncDone;                               {Thread-initiated and GUI-Synchronize()'d "done" routine.   Used only when FCommType is Synch in this example application}
  protected
    procedure Execute; override;                      {This is the main method of thread; when this method exits, the thread terminates}
  public
    constructor Create(ID, CallingThread: Cardinal; CommType: TCommType; MaxValue: Cardinal); reintroduce;
  end;

  {Define regular procedures}
  {This can be called through QueueUserAPC() via non-GUI threads}
  procedure QAPCUpdate(Value: Cardinal); stdcall;     {Thread-initiated and GUI-APC()'d routine to update the display.  Used only when CommType is QAPC in this example application}
  procedure QAPCDone(Value: Cardinal); stdcall;       {Thread-initiated and GUI-APC()'d routine to note thread is done.  Used only when CommType is QAPC in this example application}

var
  Form1        : TForm1;
  MainValue    : Integer;
  ThreadButton : array[0..2] of TButton;
  ThreadEdit   : array[0..2] of TEdit;
  ProcThread   : array[0..2] of TProcessingThread;
  QtyProcs     : Integer;                             {Number of threads processing}

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
    begin {Failed to create GUI thread handle; lock down}
    FGUIProcHandle := INVALID_HANDLE_VALUE;
    ProtectMaxCount(True);
    Thread1Button.Enabled := False;
    Thread2Button.Enabled := False;
    Thread3Button.Enabled := False;
    raise EDupHandle.Create('Unable to create GUI handle; thread processing disabled.');
    end;

  {Collect process thread's buttons and edits}
  ThreadButton[0] := Thread1Button;
  ThreadButton[1] := Thread2Button;
  ThreadButton[2] := Thread3Button;
  ThreadEdit[0] := Thread1CounterEdit;
  ThreadEdit[1] := Thread2CounterEdit;
  ThreadEdit[2] := Thread3CounterEdit;
end;

{------------------------------------------------------------------------------}

procedure TForm1.MainButtonClick(Sender: TObject);
{GUI Thread's counting method.}
var
  Idx : Integer;
  MaxValue : Integer;
begin
  {Block further "Count To" changes or main thread's button clicks}
  ProtectMaxCount(True);
  TButton(Sender).Enabled := False;

  {Perform counting operation}
  MaxValue := StrToInt(MaxCountEdit.Text);
  for Idx := 0 to MaxValue do
    begin
    MainValue := Idx;
    Update;
    sleepex(0, True);
    end;

  {Signal done; re-enable main button and max count changesand as appropriate}
  MainButton.Enabled := True;
  if QtyProcs = 0 then ProtectMaxCount(False);
end;

{------------------------------------------------------------------------------}

procedure TForm1.ProtectMaxCount(Locked: Boolean);
{Disable/Enable Max Count field}
begin
  MaxCountEdit.ReadOnly := Locked;
  case Locked of
    True : MaxCountEdit.Color := clBtnShadow;
    False: MaxCountEdit.Color := clWindow;
  end;
end;

{------------------------------------------------------------------------------}

procedure TForm1.ThreadButtonClick(Sender: TObject);
begin
  {Increment count of processing threads and block this thread's button}
  IncProcs;
  TButton(Sender).Enabled := False;

  {Perform counting operation (it its own thread)}
  ProcThread[TEdit(Sender).Tag] := TProcessingThread.Create(TEdit(Sender).Tag, FGUIProcHandle, TCommType(CommType.ItemIndex), StrToInt(MaxCountEdit.Text));

  {Wait for thread messages (if QAPC communication type selected)}
  if TCommType(CommType.ItemIndex) = QAPC then WaitForThreads;
end;

{------------------------------------------------------------------------------}

procedure TForm1.IncProcs;
{Increment quantity of processing threads}
begin
  inc(QtyProcs);
  Form1.ThreadCountEdit.Text := inttostr(QtyProcs);
  ProtectMaxCount(True);
end;

{------------------------------------------------------------------------------}

procedure TForm1.DecProcs;
{Decrement quantity of processing threads}
begin
  dec(QtyProcs);
  Form1.ThreadCountEdit.Text := inttostr(QtyProcs);
  if QtyProcs = 0 then Form1.ProtectMaxCount(False);
end;

{------------------------------------------------------------------------------}

procedure TForm1.WaitForThreads;
{Wait for processing threads to finish.  This is accomplished by sleeping in an "alertable" state for a short time; waking upon the next thread message or time expiration.
 Once woken, either the UpdateThreadEdit regular procedure is executed (if a processing thread queued a request via asynchronous procedure call) or Windows messages are processed
 for this application via  Application.ProcessMessages.
 This method finishes and exits when all threads are done processing.}
begin
  while QtyProcs > 0 do {Wait for processing threads to finish; meanwhile update the screen (directly through Application.ProcessMessages / indirectly through threaded APC calls to UpdateThreadEdit)}
    begin
    sleepex(0, True);
//    Application.ProcessMessages;
    end;
//    if (sleepex(0, False) <> STATUS_USER_APC) then Application.ProcessMessages;
end;

{------------------------------------------------------------------------------}

procedure TForm1.Update;
{Main GUI thread's "update" method.}
begin
  MainCounterEdit.Text := inttostr(MainValue);
  ThreadCountEdit.Text := inttostr(QtyProcs);
//  Form1.MainCounterEdit.Update;
  Application.ProcessMessages;
end;



{##############################################################################}
{##############################################################################}
{###################### Regular (non-class) Procedures ########################}
{##############################################################################}
{##############################################################################}

procedure QAPCUpdate(Value: Cardinal);
{TProcessingThread's "GUI, please update" method; called only when CommType is QAPC (Queued Asynchronous Procedure Call).
 This method is executed in the context of the GUI thread- it is safe to access global objects.
 Value is (31:30 thread's given ID, 29:0 thread's count result.}
var
  ID : Integer;
begin
  {Extract thread ID and value}
  ID := Value shr 30;

  {Update GUI}
  ThreadEdit[ID].Text := inttostr(Value and $3FFFFFFF);
//  ThreadEdit[ID].Update;  {Force control's GUI update}
  Application.ProcessMessages;
end;

{------------------------------------------------------------------------------}

procedure QAPCDone(Value: Cardinal); stdcall;
{TProcessingThread's "GUI, I'm done" method; called only when CommType is QAPC (Queued Asynchronous Procedure Call).
 This method is executed in the context of the GUI thread- it is safe to access global objects.
 Value is (31:30 thread's given ID, 29:0 unused.}
var
  ID : Integer;
begin
  {Extract thread ID}
  ID := Value shr 30;

  {Update GUI; re-enable changes and button clicks as appropriate}
  ThreadButton[ID].Enabled := True;
  Form1.DecProcs;

  Application.ProcessMessages;
end;



{##############################################################################}
{##############################################################################}
{######################### TProcessingThread Routines #########################}
{##############################################################################}
{##############################################################################}

{The TProcessingThread is created by the GUI thread as needed.  It runs independent of the GUI thread; however, it may be affected by the communication technique used to
 provide updates to the GUI thread, so care must be taken depending on application need.

 To actively communicate results to the GUI thread, TProcessingThread either makes:
   * Synchronize()'d (blocked) calls to its own SyncUpdate and SyncDone methods (below)
   -- or --
   * QueueUserAPC()'d (non-blocked) asynchronous procedure calls to the unit's QAPCUpdate and QAPCDone procedures (above)

 In both cases, the target methods/procedures are run in the context of the GUI thread; but one causes the thread to halt (blocked) until the GUI is finished whereas the
 other allows the thread to continue (non-blocked) regardless of the GUI.}

procedure TProcessingThread.SyncUpdate;
{Thread's "GUI, please update" method; called only when CommType is Sync.
 This method is executed in the context of the GUI thread- it is safe to access global objects.}
begin
  {Update GUI}
  ThreadEdit[FID].Text := IntToStr(FCounter);
  ThreadEdit[FID].Update;
//  Application.ProcessMessages;
end;

{------------------------------------------------------------------------------}

procedure TProcessingThread.SyncDone;
{Thread's "GUI, I'm done" method; called only when CommType is Sync.
 This method is executed in the context of the GUI thread- it is safe to access global objects.}
begin
  {Update GUI; re-enable changes and button clicks as appropriate}
  ThreadButton[FID].Enabled := True;
  Form1.DecProcs;
end;

{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{ooooooooooooooooooooooooooooo Protected Routines ooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}

procedure TProcessingThread.Execute;
{Processing Thread's highest execution method.  When this method exits, thread terminates}
var
  Idx : Cardinal;
begin
  {Perform counting operation}
  for Idx := 0 to FMaxValue do
    begin
    {Update the count}
    FCounter := Idx;
    {Communicate the new result to caller "GUI" thread}
    case FCommType of
      Sync: Synchronize(SyncUpdate);
      QAPC: QueueUserAPC(@QAPCUpdate, FCallerThread, (FID shl 30) + (FCounter and $3FFFFFFF));
    end;
    end;

  {Signal done; re-enable changes and button clicks as appropriate}
  case FCommType of
    Sync: Synchronize(SyncDone);
    QAPC: QueueUserAPC(@QAPCDone, FCallerThread, FID shl 30);
  end;
end;

{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{ooooooooooooooooooooooooooooooo Public Routines oooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}

constructor TProcessingThread.Create(ID, CallingThread: Cardinal; CommType: TCommType; MaxValue: Cardinal);
{Create the processing thread (non-GUI thread).
 This method is executed in the context of the GUI thread- it is safe to access global objects.}
begin
  FreeOnTerminate := True;  {Free memory upon thread termination}
//  OnTerminate := Finish;  {Thread cleanup method, if any}

  {Initialize ID, caller, communication type, and max value information}
  FID := ID;
  FCallerThread := CallingThread;
  FCommType := CommType;
  FMaxValue := MaxValue;

  {Initialize counter (the place where the "processing" of this thread operates)}
  FCounter := 0;

//  {Try to get high performance counter frequency and convert it to milliseconds (FHPCFreq is 0 if no HPC available)}
//  QueryPerformanceFrequency(FHPCFreq);
//  if FHPCFreq > 0 then FHPCFreq := FHPCFreq div 1000;

  {Finish thread creation and set to active state.  After this, the .Execute method automatically runs}
  inherited Create(False);
end;

end.
