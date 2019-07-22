(*
    "The contents of this file are subject to the Mozilla Public License
    Version 1.1 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at
    http://www.mozilla.org/MPL/

    Software distributed under the License is distributed on an "AS IS"
    basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
    License for the specific language governing rights and limitations
    under the License.

    The Initial Developer of the Original Code is
      Henri Gourvest <hgourvest@gmail.com>.
*)

unit dorService;

interface
uses
{$if Defined(madExcept) and Defined(CONSOLEAPP)}
  madExcept,
{$ifend}
  Windows,
  WinSvc,
  WinSock2,
  SysUtils,
  Classes,
  dorSocketStub,
  dorWebsocket,
  superobject;

type
  TDORService = class;
  TDORServiceTerminateProc = reference to procedure(var CanStop: Boolean);

  TDORService = class
  private
    FThreads: TDORThread;
    FName: string;
    FDisplayName: string;
{$IFNDEF CONSOLEAPP}
    FDescription: string;
    FDependencies: string;
    FServiceType: Cardinal;
    FStartType: Cardinal;
{$ENDIF}
    FTerminateProc: TDORServiceTerminateProc;
    function Start: boolean;
    procedure Suspend;
    procedure Resume;
  protected
{$IFNDEF CONSOLEAPP}
    procedure InstallService;
    procedure RemoveService;
{$ENDIF}
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Run;
    function CreateThread(clazz: TDORThreadClass): TDORThread;

    property Threads: TDORThread read FThreads;
    property Name: string read FName write FName;
    property DisplayName: string read FDisplayName write FDisplayName;
{$IFNDEF CONSOLEAPP}
    property Description: string read FDescription write FDescription;
    property Dependencies: string read FDependencies write FDependencies;
    property ServiceType: Cardinal read FServiceType write FServiceType;
    property StartType: Cardinal read FStartType write FStartType;
{$ENDIF}
    property OnTerminate: TDORServiceTerminateProc read FTerminateProc write FTerminateProc;
  end;

  function Application: TDORService;

{$IFNDEF CONSOLEAPP}
const
  SERVICE_WIN32_OWN_PROCESS     = $00000010;
  SERVICE_WIN32_SHARE_PROCESS   = $00000020;
  SERVICE_INTERACTIVE_PROCESS   = $00000100;
  SERVICE_WIN32                 = (SERVICE_WIN32_OWN_PROCESS or
                                   SERVICE_WIN32_SHARE_PROCESS);

  SERVICE_BOOT_START            = $00000000;
  SERVICE_SYSTEM_START          = $00000001;
  SERVICE_AUTO_START            = $00000002;
  SERVICE_DEMAND_START          = $00000003;
  SERVICE_DISABLED              = $00000004;
{$ENDIF}


procedure Terminate;

implementation

var
  FApplication: TDORService = nil;

function Application: TDORService;
begin
  if FApplication = nil then
  begin
  {$IFDEF CONSOLEAPP}
    AllocConsole;
  {$ENDIF}
    FApplication := TDORService.Create;
  end;
  Result := FApplication;
end;

{$IFNDEF CONSOLEAPP}
var
  ServiceStatus: TServiceStatus;
  ServiceStatusHandle: THandle;
  ServiceCheckPoint: Cardinal;
{$ENDIF}

procedure Terminate;
var
  CanStop: Boolean;
begin
  if FApplication <> nil then
  begin
    TCustomObserver.TriggerEvent(SO(['event', 'application_terminate']));
    if Assigned(FApplication.OnTerminate) then
    begin
      CanStop := True;
      repeat
        Sleep(100);
        FApplication.OnTerminate(CanStop);
      until CanStop;
    end;
    FApplication.OnTerminate := nil;

    FApplication.Free;
    FApplication := nil;

    while TDORThread.ThreadCount > 0 do
      Sleep(10);
  end;
end;

{$IFDEF CONSOLEAPP}
function CtrlCHandler(dwCtrlType: DWORD): Boolean; stdcall;
begin
  SetConsoleCtrlHandler(@CtrlCHandler, False);
  Result := (CTRL_C_EVENT = dwCtrlType); { Don't stop on CTRL+C }
  Terminate;
end;
{$ENDIF}

{$IFNDEF CONSOLEAPP}
procedure ReportSvcStatus(CurrentState, Win32ExitCode, WaitHint: Cardinal);
begin
  ServiceStatus.dwCurrentState  := CurrentState;
  ServiceStatus.dwWin32ExitCode := Win32ExitCode;
  ServiceStatus.dwWaitHint      := WaitHint;

  if CurrentState = SERVICE_START_PENDING then
    ServiceStatus.dwControlsAccepted := 0
  else
    ServiceStatus.dwControlsAccepted := SERVICE_ACCEPT_STOP or SERVICE_ACCEPT_PAUSE_CONTINUE or SERVICE_ACCEPT_SHUTDOWN;

  if (CurrentState = SERVICE_RUNNING) or (CurrentState = SERVICE_STOPPED) then
    ServiceStatus.dwCheckPoint := 0
  else
  begin
    Inc(ServiceCheckPoint);
    ServiceStatus.dwCheckPoint := ServiceCheckPoint;
  end;

  SetServiceStatus(ServiceStatusHandle, ServiceStatus);
end;

procedure ServiceCtrlHandler(ControlCode: LongWord); stdcall;
begin
  case ControlCode of
    SERVICE_CONTROL_INTERROGATE:
      begin
        // Fall through to send current status.
      end;

    SERVICE_CONTROL_PAUSE:
      begin
        // Do whatever it takes to pause here.
        ReportSvcStatus(SERVICE_PAUSE_PENDING, NO_ERROR, 3000);
        Application.Suspend;
        ReportSvcStatus(SERVICE_PAUSED, NO_ERROR, 0);
      end;

    SERVICE_CONTROL_CONTINUE:
      begin
        // Do whatever it takes to continue here.
        ReportSvcStatus(SERVICE_CONTINUE_PENDING, NO_ERROR, 3000);
        Application.Resume;
        ReportSvcStatus(SERVICE_RUNNING, NO_ERROR, 0);
      end;

    SERVICE_CONTROL_STOP, SERVICE_CONTROL_SHUTDOWN:
      begin
        // Do whatever it takes to stop here.
        ReportSvcStatus(SERVICE_STOP_PENDING, NO_ERROR, 0);
        Terminate;
        Exit;
      end;
  else
    if not (ControlCode in [128..255]) then
      raise Exception.CreateFmt('Unrecognized ControlCode %d', [ControlCode])
  {
    else
      User OpCode
  }
  end;

  ReportSvcStatus(ServiceStatus.dwCurrentState, NO_ERROR, 0);
end;

procedure ServiceMain(argc: LongWord; argv: PChar); stdcall;
var
  OldThreadId: Cardinal;
begin
  ServiceStatusHandle := RegisterServiceCtrlHandler(
    @Application.FName[1], @ServiceCtrlHandler);

  if ServiceStatusHandle = 0 then
  begin
    raise Exception.CreateFmt('RegisterServiceCtrlHandler failed %d', [GetLastError]);
    Exit;
  end;

  ServiceStatus.dwServiceType := Application.FServiceType;
  ServiceStatus.dwServiceSpecificExitCode := 0;

  ReportSvcStatus(SERVICE_START_PENDING, NO_ERROR, 3000);

  // This is where the service does its work.
  if Application.Start then
  begin
    ReportSvcStatus(SERVICE_RUNNING, NO_ERROR, 0);

    OldThreadId := MainThreadID;
    MainThreadID := GetCurrentThreadId;
    try
      while FApplication <> nil do
      begin
        CheckSynchronize;
        Sleep(10);
      end;
    finally
      MainThreadID := OldThreadId;
    end;

    ReportSvcStatus(SERVICE_STOPPED, NO_ERROR, 0);
  end
  else
    ReportSvcStatus(SERVICE_STOPPED, GetLastError, 0)
end;

function StartService: boolean;
var
  DispatchTable: array[0..1] of TServiceTableEntry;
begin
  DispatchTable[0].lpServiceName := @Application.Name[1];
  DispatchTable[0].lpServiceProc := @ServiceMain;
  DispatchTable[1].lpServiceName := nil;
  DispatchTable[1].lpServiceProc := nil;
  Result := StartServiceCtrlDispatcher(DispatchTable[0]);
end;
{$ENDIF}

{ TDORService }

{$IFNDEF CONSOLEAPP}

{$if defined(VER210)}
{ Required to set the service description. Not defined in WinSvc before XE2 }
const
  SERVICE_CONFIG_DESCRIPTION = 1;

function ChangeServiceConfig2(hService: THandle; dwInfoLevel: DWORD;
    lpInfo: Pointer): BOOL; stdcall; external advapi32 name 'ChangeServiceConfig2W';
{$ifend}

procedure TDORService.InstallService;
var
  Service: THandle;
  SCManager: THandle;
  Path: array[0..511] of Char;
  data: PChar;
begin
   if (GetModuleFileName(0, Path, 512) = 0) then
   begin
     raise Exception.CreateFmt('Unable to install %s - %s',
       [FDisplayName, SysErrorMessage(GetLastError)]);
     Exit;
   end;
   SCManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT or SC_MANAGER_CREATE_SERVICE);
   if (SCManager <> 0) then
   begin
      if FDependencies <> '' then
        data := @FDependencies[1] else
        data := nil;
      Service := CreateService(SCManager, @FName[1], @FDisplayName[1],
        SERVICE_QUERY_STATUS or SERVICE_CHANGE_CONFIG, FServiceType, FStartType,
        SERVICE_ERROR_NORMAL, Path, nil, nil, data, nil, nil);

      if FDescription <> '' then
      begin
        data := PChar(FDescription);
        ChangeServiceConfig2(Service, SERVICE_CONFIG_DESCRIPTION, @data);
      end;

      if (Service <> 0) then
        CloseServiceHandle(Service) else
        raise Exception.CreateFmt('CreateService failed - %s', [SysErrorMessage(GetLastError)]);
      CloseServiceHandle(SCManager);
   end
   else
     raise Exception.CreateFmt('OpenSCManager failed - %s', [SysErrorMessage(GetLastError)])
end;
{$ENDIF}

{$IFNDEF CONSOLEAPP}
procedure TDORService.RemoveService;
var
  Service: THandle;
  SCManager: THandle;
  Status: TServiceStatus;
begin
  SCManager := OpenSCManager(nil, nil, SC_MANAGER_CONNECT);
  if (SCManager <> 0) then
  begin
     Service := OpenService(SCManager, @FName[1], _DELETE or SERVICE_STOP or
       SERVICE_QUERY_STATUS);
     if (Service <> 0) then
     begin
       if (ControlService(Service, SERVICE_CONTROL_STOP, Status)) then
       begin
         Sleep(1000);
         while (QueryServiceStatus(Service, Status)) do
           if (Status.dwCurrentState = SERVICE_STOP_PENDING) then
             Sleep(1000) else Break;
         if not (Status.dwCurrentState = SERVICE_STOPPED) then
           raise Exception.CreateFmt('%s failed to stop.', [FDisplayName]);
       end;
       if not DeleteService(Service) then
         raise Exception.CreateFmt('DeleteService failed - %s', [SysErrorMessage(GetLastError)]);
       CloseServiceHandle(Service);
     end
     else
       raise Exception.CreateFmt('OpenService failed - %s', [SysErrorMessage(GetLastError)]);
     CloseServiceHandle(SCManager);
  end
  else
    raise Exception.CreateFmt('OpenSCManager failed - %s', [SysErrorMessage(GetLastError)]);
end;
{$ENDIF}

procedure TDORService.Run;
{$IFDEF CONSOLEAPP}
var
  Cmd: string;
  i: Integer;
  input: TInputRecord;
  inputread: Cardinal;
label tryagain;
{$ENDIF}
begin
{$if defined(DEBUG)}
  TThread.NameThreadForDebugging(AnsiString(Self.ClassName));
{$ifend}

{$IFDEF CONSOLEAPP}
  Start;
  writeln('---------------------------------------');
  writeln(application.DisplayName);
  writeln('---------------------------------------');
  writeln('[COMMANDS]');
  writeln(' > EXIT   : Stop application.');
  writeln(' > PAUSE  : Pause application.');
  writeln(' > RESUME : Resume paused application.');
  writeln(' > CLEAR  : Disconnect all clients.');
{$ifdef madExcept}
  writeln(' > RESTART: Restart application (madexcept).');
  writeln(' > REPORT : Generate a bug report (madexcept).');
{$endif}
  writeln('---------------------------------------');

  SetConsoleCtrlHandler(@CtrlCHandler, True);

tryagain:
  PeekConsoleInput(GetStdHandle(STD_INPUT_HANDLE), input, 1, inputread);
  if inputread = 0 then
  begin
    if FApplication = nil then
      Exit;
    Sleep(1);
    CheckSynchronize;
    goto tryagain;
  end;
  ReadConsoleInput(GetStdHandle(STD_INPUT_HANDLE), input, 1, inputread);
  if (inputread = 1) and (input.EventType = KEY_EVENT) and (input.Event.KeyEvent.bKeyDown) then
    if input.Event.KeyEvent.AsciiChar = #$D then
    begin
    {$ifdef madExcept}
      if SameText(Cmd, 'restart') then RestartApplication else
      if SameText(Cmd, 'report') then
      begin
        try
          raise Exception.Create('Thread Status');
        except
          on E: Exception do
            HandleException(etNormal, E);
        end;
      end else
    {$endif}
      if SameText(Cmd, 'exit') then
      begin
        Terminate;
        Exit;
      end else
      if SameText(Cmd, 'pause') then Suspend else
      if SameText(Cmd, 'resume') then Resume else
      if SameText(Cmd, 'clear') then
      begin
        FThreads.Lock;
        try
          FThreads.Resume;
          //sleep(100);
          for i := 0 to FThreads.ChildCount - 1 do
            if FThreads[i] is TSocketServer then
              FThreads[i].ChildClear;
        finally
          FThreads.UnLock;
        end;
      end else
        writeln('Unknow command');

      cmd := '';
      Writeln('');
    end else
    begin
      Cmd := Cmd + input.Event.KeyEvent.UnicodeChar;
      Write(input.Event.KeyEvent.AsciiChar);
    end;
  CheckSynchronize;
  goto tryagain;
{$ELSE}
  if ParamCount > 0 then
  if SameText(ParamStr(1), 'install') then
  begin
    InstallService;
    Terminate;
    Exit;
  end else
  if SameText(ParamStr(1), 'uninstall') then
  begin
    RemoveService;
    Terminate;
    Exit;
  end;
  if not StartService then
    Terminate;
{$ENDIF}
end;

constructor TDORService.Create;
begin
  FThreads := TDORThread.Create(nil);
{$IFNDEF CONSOLEAPP}
  FServiceType := SERVICE_WIN32_OWN_PROCESS;
  FStartType := SERVICE_DEMAND_START;
{$ENDIF}
end;

destructor TDORService.Destroy;
begin
  FThreads.Free;
  WSACleanup;
  FApplication := nil;
  inherited;
end;

function TDORService.Start: boolean;
var
  Data: TWSAData;
begin
  WSAStartup($0202, Data);
  Result := True;
  try
    FThreads.Start;
  except
    Result := False;
  end;
end;

procedure TDORService.Suspend;
begin
  FThreads.Suspend;
end;

procedure TDORService.Resume;
begin
  FThreads.Resume;
end;

function TDORService.CreateThread(clazz: TDORThreadClass): TDORThread;
begin
  Result := clazz.Create(FThreads);
end;

initialization
{$IFNDEF CONSOLEAPP}
  ServiceCheckPoint := 1;
{$ENDIF}
  Application;

end.
