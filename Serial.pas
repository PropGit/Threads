unit Serial;

interface

uses
  Windows, SysUtils, StdCtrls, StrUtils, Classes, Controls, Forms, Dialogs, Graphics, Math, DateUtils, {$IFNDEF ISLIB} PTRegistry, {$ELSE} PLRegistry, {$ENDIF} PortList, Global;

type

  {Define the processing thread}
  TMyProcessingThread = class(TThread)
  private
    FCallerThread  : Cardinal;
    FHPCFreq       : Int64;           {Frequency of high-performance system clock, if one exists}
  protected
    procedure Execute; override;
  public
    constructor Create(CallingThread: Cardinal); reintroduce;
  end;

  {Define the Propeller Serial object}
  TPropellerSerial = class(TObject)
  private
    FGUIProcHandle : THandle;                    {Handle to GUI Thread (main process)}
    FCommThread    : TCommunicationThread;       {Communication thread}
    procedure Communicate(PropModel: TPropModel = UN; BinaryImage: PByteArray = nil; BinaryByteCount: Integer = 0; DownloadCommand: Byte = 0);
    procedure WaitForCommunicationThread;
  public
    constructor Create; reintroduce;
  end;

var
  Propeller      : TPropellerSerial;             {Instance of TPropellerSerial}

implementation

uses

{##############################################################################}
{##############################################################################}
{############################# Global Routines ################################}
{##############################################################################}
{##############################################################################}

procedure UpdateSerialStatus(Value: Cardinal);
{Update serial status (identify/download progress).
 NOTE: This method is called multiple times by the TCommunicationThread via APC (Asynchronous Procedure Call) when it has a status update.}
var
  Idx    : Integer;
  FIdx   : Integer;
  MsgID  : TMessageType;
  PModel : Integer;
  Port   : Integer;
  Ver    : Integer;
{$IFDEF ISLIBWRAP}
const
  {TMessageType IDs to Propellent Message IDs}
  IDTx : array[TMessageType] of Integer = (000 {mtProgress},           000 {mtClosingPort},            451 {mtDone},                   506 {mtLoadingRAM},
                                           507 {mtVerifyingRAM},       508 {mtProgrammingEEPROM},      509 {mtVerifyingEEPROM},        503 {mtCheckingPort},
                                           505 {mtFoundPropeller},     504 {mtPortScanned},            510 {mtPortUnopened},           511 {mtPortUnconfigured},
                                           512 {mtPortUnusable},       513 {mtPortUnreadable},         514 {mtPortUnwritable},         301 {mtNoPropellerAnyPort},
                                           302 {mtNoPropellerOnPort},  303 {mtNoPorts},                304 {mtNoUsablePorts},          305 {mtPortNoExist},
                                           305 {mtPortNoExist},        351 {mtRAMChecksumError},       352 {mtEEPROMProgrammingError}, 353 {mtEEPROMVerifyError},
                                           306 {mtPropellerLost},      307 {mtNoRead},                 308 {mtNoWrite},                309 {mtNoEvent},
                                           310 {mtWaitFailed});
{$ENDIF}

  {----------------}

  function MultiModuleCheckMode: Boolean;
  {Returns true if operation is a multi-module version check that experienced a no-Propeller or no-port type event}
  begin
    Result := (MsgID in [mtNoPropellerAnyPort..mtPortNoExist]) and (VersionTarget = UN);
  end;

  {----------------}

  function MultiModuleChecking: Boolean;
  {Returns true if operation is an "unfinished" multi-module version check}
  begin
    Result := MultiModuleCheckMode and (PModel < 2);
  end;

  {----------------}


begin
  {Update last-update-time}
  LastUpdateTime := gettickcount;
  {Retrieve and format text message}
  MsgID := TMessageType(Value and $FF);
  PModel := 1 + ((Value shr 15) and $1);  {This value included only in specific messages}
  Ver := (Value shr 8) and $7F;           {This value included only in specific messages}
  Port := Value shr 16;                   {This value included only in specific messages}
  if MsgID = mtProgress then exit;
  {Activate progress bar (no effect if already active)}
  ProgressForm.Progress;
  LastMsg := CommunicationMsg[MsgID];
  {Need to fill in fields?}
  Idx := pos('<', LastMsg);
  while Idx > 0 do
    begin
    FIdx := ord(LastMsg[Idx+1])-ord('0'); {<0> = Port, <1> = Logged Messages, <2> = Version, <3> = PropModel}
    delete(LastMsg, Idx, 3);
    case FIdx of
      0: insert('COM'+inttostr(Port), LastMsg, Idx);
      1: insert(LoggedMsgs, LastMsg, Idx);
      2: if PModel = 1 then {Propeller 1 version}
           insert(chr(ord('@')+Ver), LastMsg, Idx)
         else               {Propeller 2 version}
           begin
           insert(P2Desc[Ver].Revision, LastMsg, Idx);
           end;
      3: if not MultiModuleCheckMode then {Normal operation; insert Propeller model number}
           insert(inttostr(PModel), LastMsg, Idx)
         else
           delete(LastMsg, Idx, 1);       {Multi-module version check in progress; leave out model and remove extra space}
    end; {case}
    Idx := pos('<', LastMsg);
    end;
  {Process message}
  CommInProgress := not (MsgID in [mtDone, mtNoPropellerAnyPort..mtWaitFailed]);                                             {Flag the state of communcation (continuing or ending?)}
  case MsgID of
    mtLoadingRAM..
          mtCheckingPort   : begin                                                                                           {Status message (non-error) (excluding mtFoundPropeller)}
                             ProgressForm.SetMsg(LastMsg);
                             {$IFNDEF ISLIB}
                             if MsgID = mtCheckingPort then Propeller.SaveAutoRecoverSerial('COM'+inttostr(Port));             {If checking port, save auto-recover-serial file}
                             {$ENDIF}
                             {$IFDEF ISLIBWRAP}
                             StdOutMsg(pmtEvent, inttostr(IDTx[MsgID])+'-'+LastMsg);                                           {Send message on standard output}
                             {$ENDIF}
                             end;
    {$IFNDEF ISLIB}
    mtClosingPort          : Propeller.ClearAutoRecoverSerial;                                                               {Closing port message (non-error)}
    {$ENDIF}
    mtFoundPropeller       : begin                                                                                           {Found Propeller message (non-error)}
                             LastPortUsed := Port;                                                                             {Log port ID}
                             Propeller.SetVerResult(TPropModel(PModel), Ver, LastPortUsed);                                    {Log model, version, and port for Propeller object}
                             LastMsg := LastMsg + #$0A#$0D#$0A#$0D + ifthen(PModel = 1, P1Desc, P2Desc[Ver].Description);
                             if VersionCheck then                                                                              {Are we checking version only?}
                               begin
                               ProgressForm.Close;                                                                               {Close progress form}
                               {$IFDEF ISLIB}
                               if (CPrefs[GUIDisplay].IValue in [0, 2]) then                                                     {Dialog display needed?}
                               {$ENDIF}
                                 messagedlg(LastMsg, mtInformation, [mbOK], 0);                                                    {Display version message}
                               end;
                             {$IFDEF ISLIBWRAP}
                             StdOutMsg(pmtEvent, inttostr(IDTx[MsgID])+'-'+LastMsg);                                           {Send message on standard output}
                             {$ENDIF}
                             end;
    mtPortScanned..
          mtPortUnwritable : begin                                                                                           {Port status message (non-error)}
                             LoggedMsgs := LoggedMsgs + #$D#$A + '      ' + LastMsg;                                           {Log message}
                             {$IFDEF ISLIBWRAP}
                             StdOutMsg(pmtEvent, inttostr(IDTx[MsgID])+'-'+LastMsg);                                           {Send message on standard output}
                             {$ENDIF}
                             end;
    mtNoPropellerAnyPort..
          mtWaitFailed     : begin                                                                                           {Error message}
                             if MsgID in [mtNoPropellerAnyPort..mtPortNoExist] then LastPortUsed := 0;                       {Clear last port if port error}
                             if not MultiModuleChecking then                                                                 {If not partway through multi-model version check}
                               begin
                               ProgressForm.Close;                                                                             {Close progress form}
                               {$IFDEF ISLIBWRAP}
                               StdOutMsg(pmtError, inttostr(IDTx[MsgID])+'-'+LastMsg);                                         {Send message on standard output}
                               {$ENDIF}
                               {$IFDEF ISLIB}
                               if (CPrefs[GUIDisplay].IValue in [0, 2]) then                                                   {Dialog display needed?}
                               {$ENDIF}
                                 Propeller.ShowError(LastMsg, ifthen(Port = 65535, MAXINT, Port) * ord(MsgID in [mtNoPropellerAnyPort..mtNoUsablePorts])); {Display error message}
                                 {Note: For second parameter (ExcludedPorts):
                                        =0      = no ports excluded from the search (or we do not wish to show that level of detail).
                                     =MAXINT    = indicates we're fixed to scan one port only, all others were ignored; additional detail and button will be provided.
                                        <0      = indicates ID of port with fatal error last session; additional detail and buttons will be provided.
                                   >0 & !MAXINT = indicates number of ports excluded from search; additional detail and button will be provided.}
                               end
                             else                                                                                            {else (we are partway through multi-model version check)}
                               LoggedMsgs := '';                                                                               {Clear previous port scanning messages; we're about to do it again}
                             end;
  end; {case}
  if not CommInProgress and not MultiModuleChecking then
    {Close progress and wrap up status if finished}
    begin
    ProgressForm.Close;
    {$IFDEF ISLIBWRAP}
    if MsgID = mtDone then StdOutMsg(pmtInfo, inttostr(IDTx[mtDone])+'-'+LastMsg);                                           {Send message on standard output}
    {$ENDIF}
    end;
end;

{##############################################################################}
{##############################################################################}
{######################## TMyProcessingThread Routines ########################}
{##############################################################################}
{##############################################################################}

{The TMyProcessingThread is created by the TPropellerSerial object when a call is made to TPropellerSerial.Download or TPropellerSerial.GetVersion.  The TPropellerSerial object executes
in the context of the main GUI thread.  The TCommunicationThread runs unencumbered by any GUI messages; it has no GUI components to deal with, so it performs fast processing.
The TCommunicationThread makes asynchronous procedure calls the serial class' UpdateSerialStatus procedure.}

{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooo Propeller Chip High-Level Serial Routines ooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}

procedure TCommunicationThread.UpdateGUIWithVersion;
{Update GUI - Found hardware}
begin
  //P1 version is expressed raw / P2 version is expressed with bit 7 set
  QueueUserAPC(@UpdateSerialStatus, FCallerThread, ord(mtFoundPropeller) + ((ModelBit shl 7) + FVersion) shl 8 + (strtoint(FComPort) shl 16));
end;

{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{ooooooooooooooooooooooooooooo Protected Routines ooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}

procedure TCommunicationThread.Execute;
{Thread's main method.}
begin
  {Exit if I/O event handle invalid}
  if FCommIOEvent = 0 then Error(ord(mtNoEvent));
  {If binary image provided, find Propeller and download to it, otherwise, find Propeller and report version and port}
  if FBinImage <> nil then TalkToHardware else GetHardwareVersion;
  {Update GUI - Done}
  QueueUserAPC(@UpdateSerialStatus, FCallerThread, ord(mtDone));
end;

{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{ooooooooooooooooooooooooooooooo Public Routines oooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}

constructor TMyProcessingThread.Create(CallingThread: Cardinal);
{Create the thread. This method (TMyProcessingThread.Create) is the only method within this object that is executed in the context of the GUI thread.
 NOTE: The regular procedure UpdateSerialStatus (with stdcall declaration) is called via APCs from this thread to provide status updates.}
var
  Idx : Integer;
begin
  {NOTE: This method is executed in the context of the calling thread (GUI thread), making it safe to access global objects that are not thread-aware.}
  FreeOnTerminate := True;
//  OnTerminate := Finish;
  {Store caller and last-used information}
  FCallerThread := CallingThread;
  {Try to get high performance counter frequency and convert it to milliseconds (FHPCFreq is 0 if no HPC available)}
  QueryPerformanceFrequency(FHPCFreq);
  if FHPCFreq > 0 then FHPCFreq := FHPCFreq div 1000;
  {Start thread}
  inherited Create(False);
end;

{##############################################################################}
{##############################################################################}
{######################### TPropellerSerial Routines ##########################}
{##############################################################################}
{##############################################################################}

{The TPropellerSerial object is created upon application startup.  Calls to the .Download and .GetVersion methods (by the main GUI thread) cause a TCommunicationThread to be created and
executed.  The GUI thread (still executing the .Download or .GetVersion method) then processes normal Windows messages (GUI related) and also waits for asynchronous procedure calls that are
queued by the TCommunicationThread to convey status information and TPropeller.  Those APC calls are also executed in the context of the GUI thread and effectively update the ProgressForm
as well as generate error or information dialogs.  This way, the GUI thread responds to Windows messages and also updates the ProgressForm's state in a timely manner, while all communication
can freely execute with little or no interruption (only those that are associated with normal O.S. task switching).}

{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooo Private Routines oooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}

{$IFNDEF ISLIB}
procedure TPropellerSerial.ClearAutoRecoverSerial;
{Clear (remove) auto-recover-serial file for FARSFile.}
begin
  if FARSFile = '' then exit;                                                   {Exit if no auto recover serial file}
  deletefile(pchar(FARSFile));
  FARSFile := '';
end;
{$ENDIF}
{------------------------------------------------------------------------------}

procedure TPropellerSerial.Communicate(PropModel: TPropModel = UN; BinaryImage: PByteArray = nil; BinaryByteCount: Integer = 0; DownloadCommand: Byte = 0);
{Communicate with the PropModel Propeller chip.  If BinaryImage = nil, a Version Check is performed, otherwise, a Download is performed.
This method launches a Communication Thread to scan available serial ports and search for a Propeller chip, then either checks the version or downloads
an application (BinaryImage) to that Propeller chip.}
begin
  LastUpdateTime := gettickcount;                                                                                                     {Initialize last-update-time}
  CommInProgress := True;                                                                                                             {Set communication flag}
  FCommThread := TCommunicationThread.Create(FGUIProcHandle, LastPortUsed, Model, BinaryImage, BinaryByteCount, DownloadCommand);     {Create communication thread}
  WaitForCommunicationThread;
end;

{------------------------------------------------------------------------------}

procedure TPropellerSerial.WaitForCommunicationThread;
{Wait for communication thread to finish.  This is accomplished by sleeping in an alertable state for 1/2 the ProgressForm's increment delay period, or until the next message is received
from the communication thread.  Once woken, the UpdateSerialStatus regular procedure is executed (if communication thread queued a message via asynchronous procedure call) and Windows messages
are processed for this application.  This method finishes and exits when CommInProgress is false; the communication thread indicated it was done.}
begin
  while CommInProgress do {Wait for communication thread to finish, updating the screen as necessary along the way.}
    begin
    if (sleepex(ProgressForm.GetIncDelay div 2, True) <> STATUS_USER_APC) and (gettickcount - LastUpdateTime > 5500) then ProgressForm.NoProgress;
    application.processmessages;
    end;
end;

{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{ooooooooooooooooooooooooooooo Public Routines oooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}
{oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo}

constructor TPropellerSerial.Create;
{Create Propeller Serial object}
begin
  {Duplicate our "GUI" thread's pseudo-handle to make it usable by any of our threads}
  if not DuplicateHandle(GetCurrentProcess, GetCurrentThread, GetCurrentProcess, @FGUIProcHandle, 0, False, DUPLICATE_SAME_ACCESS) then
    FGUIProcHandle := INVALID_HANDLE_VALUE; {Failed to create process handle}
  LastPortUsed := 0;
  {$IFNDEF ISLIB}
  FARSFile := '';
  {$ENDIF}
  {Create Error Form}
  FErrorForm := TErrorForm.Create(Application);
  inherited Create;
  if FGUIProcHandle = INVALID_HANDLE_VALUE then raise EDupHandle.Create('Unable to create GUI handle; serial communication disabled.');
end;


end.

