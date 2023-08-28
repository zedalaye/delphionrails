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
{$WARN SYMBOL_DEPRECATED OFF}

unit dorSocketStub;

interface
uses
  Windows, Winsock2, dorOpenSSL, dorOpenSslHelpers,
  Generics.Collections,
  dorUtils, Classes, superobject;

type
  // forward declarations
  TDORThread = class;
  TClientStub = class;
  TSocketServer = class;

  TClientStubClass = class of TClientStub;
  TAbstractServerClass = class of TAbstractServer;
  TDORThreadClass = class of TDORThread;

  PThreadList = ^TThreadList;
  TThreadList = array[0..(Maxint div 16) - 1] of TDORThread;

  TDORThread = class(TInterfacedObject)
  private
    FThread: TThread;
    FPaused: boolean;
    FCriticalSection: TRtlCriticalSection;
    FOwner: TDORThread;
    FChildList: PThreadList;
    FChildCount: Integer;
    FChildCapacity: Integer;
    FThreadRefCount: Integer;
    function ChildGet(Index: Integer): TDORThread;
    procedure ChildSetCapacity(NewCapacity: Integer);
    function ChildAdd(Item: TDORThread): Integer;
    procedure ChildDelete(Index: Integer);
    function ChildIndexOf(Item: TDORThread): Integer;
    function GetChildCount: Integer;
    function GetStopped: boolean;
    function GetThreadId: Cardinal;
  protected
    function Run: Cardinal; virtual;
    procedure Stop; virtual;
  public
    constructor Create(AOwner: TDORThread); virtual;
    destructor Destroy; override;

    class function ThreadCount: integer;
    class procedure BeginThread;
    class procedure EndThread;

    procedure ChildClear; virtual;
    function ChildRemove(Item: TDORThread): Integer;

    procedure Suspend;
    procedure Resume;
    procedure Start;
    procedure Lock;
    procedure UnLock;

    property Owner: TDORThread read FOwner;
    property ChildCount: Integer read GetChildCount;
    property ChildItems[Index: Integer]: TDORThread read ChildGet; default;
    property Stopped: boolean read GetStopped;

    property ThreadId: Cardinal read GetThreadId;
  end;

  TCustomObserver = class(TDORThread)
  private
    type
      TEventStorage = class
      private
        FEvents: ISOCriticalObject;
        FIntercept: ISOCriticalObject;
        FObservers: ISOCriticalObject;
        function Empty: ISuperObject;
      public
        constructor Create; virtual;
        procedure Trigger(const Event: ISuperObject);
      end;

      TEventProcessor = class(TDORThread)
      protected
        function Run: Cardinal; override;
      end;
    class var
      EventStorage: TEventStorage;
  public
    type
      TEventProc = reference to procedure(const event: ISuperObject);
  private
    FIntercepting: Boolean;
    FEvents: ISOCriticalObject;
    FEventProc: TDictionary<string, TEventProc>;
  protected
    procedure Intercept;
    procedure doOnEvent(const Event: ISuperObject); virtual;
    procedure doOnInternalEvent(const Event: ISuperObject); virtual;
    function ExtractEvents: ISuperObject; virtual;
    function ProcessEvents: Integer; virtual;
  public
    procedure RegisterEvent(const name: string; proc: TEventProc = nil); virtual;
    procedure UnregisterEvent(const name: string); virtual;
    procedure UnregisterEvents; virtual;
    constructor Create(AOwner: TDORThread); override;
    destructor Destroy; override;
    class procedure TriggerEvent(const Event: ISuperObject); virtual;
    procedure TriggerInternalEvent(const Event: ISuperObject); virtual;
    class constructor Create;
    class destructor Destroy;
  end;

  IReadWrite = interface
  ['{EA82DA8F-F2AB-4B93-AAAA-E388C9E72F20}']
    function ClientIP: AnsiString;
    function Read(var buf; len, Timeout: Cardinal): Cardinal;
    function Write(var buf; len, Timeout: Cardinal): Cardinal;
    function IsSSL: Boolean;
    function HavePeerCertificate: Boolean;
    function SSLSubject(const key: AnsiString): AnsiString;
    function SSLIssuer(const key: AnsiString): AnsiString;
    procedure Flush;
    procedure Close;
  end;

  TNullSocket = class(TInterfacedObject, IReadWrite)
  protected
    function ClientIP: AnsiString;
    function Read(var buf; len, Timeout: Cardinal): Cardinal;
    function Write(var buf; len, Timeout: Cardinal): Cardinal;
    function IsSSL: Boolean;
    function HavePeerCertificate: Boolean;
    function SSLSubject(const key: AnsiString): AnsiString;
    function SSLIssuer(const key: AnsiString): AnsiString;
    procedure Flush;
    procedure Close;
  end;

  TRWSocket = class(TInterfacedObject, IReadWrite)
  private const
    BUFFER_SIZE = 1024;
  private
    FSocket: TSocket;
    FOwned: Boolean;
    FClientIP: AnsiString;
    FReadTimeout: Cardinal;
    FWriteTimeout: Cardinal;
    FBuffer: array[0..BUFFER_SIZE - 1] of Byte;
    FBufferPos: Integer;
  protected
    function ClientIP: AnsiString; virtual;
    function Read(var buf; len, Timeout: Cardinal): Cardinal; virtual;
    function Write(var buf; len, Timeout: Cardinal): Cardinal; virtual;
    function IsSSL: Boolean; virtual;
    function HavePeerCertificate: Boolean; virtual;
    function SSLSubject(const key: AnsiString): AnsiString; virtual;
    function SSLIssuer(const key: AnsiString): AnsiString; virtual;
    procedure Flush;
    procedure Close;
  public
    constructor Create(Socket: TSocket; const ClientIP: AnsiString; Owned: Boolean); virtual;
  end;

  TSSLRWSocket = class(TInterfacedObject, IReadWrite)
  private const
    BUFFER_SIZE = 1024;
  private
    FSocket: TSocket;
    FOwned: Boolean;
    FClientIP: AnsiString;
    FReadTimeout: Cardinal;
    FWriteTimeout: Cardinal;
    // SSL
    FCtx: PSSL_CTX;
    FSsl: PSSL;
    FX509: PX509;
    FPassword: AnsiString;
    FConnected: Boolean;
    FBuffer: array[0..BUFFER_SIZE - 1] of Byte;
    FBufferPos: Integer;
    procedure CloseSSL;
  protected
    function ClientIP: AnsiString;
    function Read(var buf; len, Timeout: Cardinal): Cardinal; virtual;
    function Write(var buf; len, Timeout: Cardinal): Cardinal; virtual;
    function IsSSL: Boolean; virtual;
    function HavePeerCertificate: Boolean; virtual;
    function SSLSubject(const key: AnsiString): AnsiString; virtual;
    function SSLIssuer(const key: AnsiString): AnsiString; virtual;
    procedure Flush;
    procedure Close;
  public
    constructor Create(Socket: TSocket; const ClientIP: AnsiString; Owned: Boolean; Verify: Integer;
      const password, CertificateFile, PrivateKeyFile, CertCAFile: AnsiString); virtual;
    destructor Destroy; override;
  end;

  TOnSocketStub = function(socket: TSocket; const ClientIP: AnsiString): IReadWrite;

  TAbstractServer = class(TDORThread)
  private
    FStubClass: TClientStubClass;
    FOnSocketStub: TOnSocketStub;
    FAddress: TSockAddr;
    FSocketHandle: TSocket;
    FPort: Word;
    FBind: Longint;
  public
    property Address: TSockAddr read FAddress;
    property SocketHandle: TSocket read FSocketHandle;
    constructor CreateServer(Port: Word; const Bind: string;
      const StubClass: TClientStubClass;
      const OnSocketStub: TOnSocketStub = nil); virtual;
  end;

  TSocketServer = class(TAbstractServer)
  protected
    function Run: Cardinal; override;
    procedure Stop; override;
  end;

  TUDPServer = class(TAbstractServer)
  protected
    function Run: Cardinal; override;
    procedure Stop; override;
  end;

  TClientStub = class(TDORThread)
  private
    FSource: IReadWrite;
    function GetSource: IReadWrite;
  protected
    function Run: Cardinal; override;
    procedure Stop; override;
    procedure Release;
  public
    property Source: IReadWrite read GetSource;
    constructor CreateStub(AOwner: TSocketServer; const Source: IReadWrite); virtual;
  end;

threadvar
  CurrentDorThread: TDORThread;

implementation
uses
  SysUtils, Math, dorService;

var
  AThreadCount: Integer = 0;

type
  TThreadRun = class(TThread)
  private
    FOwner: TDORThread;
  public
    constructor Create(owner: TDORThread);
    procedure Execute; override;
  end;

{ TThreadRun }

constructor TThreadRun.Create(owner: TDORThread);
begin
  FreeOnTerminate := True;
  FOwner := owner;
  inherited Create(False);
end;

procedure TThreadRun.Execute;
begin
{$if defined(DEBUG)}
  TThread.NameThreadForDebugging(AnsiString(Self.ClassName));
{$ifend}

  CurrentDorThread := FOwner;
  InterlockedIncrement(CurrentDorThread.FThreadRefCount);
  try
    CurrentDorThread.Run;
  finally
    if InterlockedDecrement(CurrentDorThread.FThreadRefCount) = 0 then
      CurrentDorThread.Free
    else if CurrentDorThread.FOwner <> nil then
      CurrentDorThread.FOwner.ChildRemove(CurrentDorThread);
    CurrentDorThread := nil;
  end;
end;

{ TDORThread }

constructor TDORThread.Create(AOwner: TDORThread);
begin
  BeginThread;
  inherited Create;
  InitializeCriticalSection(FCriticalSection);
  FPaused := False;
  FOwner := AOwner;
  FThread := nil;
  FThreadRefCount := 0;
  if (FOwner <> nil) then
    FOwner.ChildAdd(Self);
end;

destructor TDORThread.Destroy;
begin
  Stop;
  ChildClear;
  DeleteCriticalSection(FCriticalSection);
  inherited;
  EndThread;
end;

class procedure TDORThread.EndThread;
begin
  InterlockedDecrement(AThreadCount);
end;

procedure TDORThread.Suspend;
var i: integer;
begin
  if not FPaused then
  begin
    Lock;
    try
      if FThread <> nil then
        FThread.Suspend;
      for i := 0 to ChildCount - 1 do
        ChildItems[i].Suspend;
      FPaused := True;
    finally
      UnLock;
    end;
  end;
end;

procedure TDORThread.Resume;
var i: integer;
begin
  if FPaused then
  begin
    lock;
    try
      if FThread <> nil then
        FThread.Resume;
      for i := 0 to ChildCount - 1 do
        ChildItems[i].Resume;
      FPaused := False;
    finally
      UnLock;
    end;
  end;
end;

function TDORThread.Run: Cardinal;
begin
  Result := 0;
end;

// Childs ...

class procedure TDORThread.BeginThread;
begin
  InterlockedIncrement(AThreadCount);
end;

function TDORThread.ChildAdd(Item: TDORThread): Integer;
var
  Delta: Integer;
begin
  Lock;
  try
    Result := FChildCount;
    if Result = FChildCapacity then
    begin
      if FChildCapacity > 64 then
        Delta := FChildCapacity div 4
      else
        if FChildCapacity > 8 then
          Delta := 16
        else
          Delta := 4;
      ChildSetCapacity(FChildCapacity + Delta);
    end;
    FChildList^[Result] := Item;
    InterlockedIncrement(TDORThread(Item).FThreadRefCount);
    Inc(FChildCount);
  finally
    UnLock;
  end;
end;

procedure TDORThread.ChildClear;
begin
  Lock;
  try
    while FChildCount > 0 do
      ChildRemove(ChildGet(0));
    ChildSetCapacity(0);
  finally
    UnLock;
  end;
end;

procedure TDORThread.ChildDelete(Index: Integer);
begin
  if (Index < 0) or (Index >= FChildCount) then
    Exit;

  with ChildGet(Index) do
    if InterlockedDecrement(FThreadRefCount) = 0 then
      Free
    else
      Stop;

  Dec(FChildCount);
  if Index < FChildCount then
    System.Move(FChildList^[Index + 1], FChildList^[Index],
      (FChildCount - Index) * SizeOf(Pointer));
end;

function TDORThread.ChildGet(Index: Integer): TDORThread;
begin
  Lock;
  try
    if (Index < 0) or (Index >= FChildCount) then
      raise Exception.CreateFmt('List index out of bounds (%d)', [Index]);
    Result := FChildList^[Index];
  finally
    UnLock;
  end;
end;

function TDORThread.ChildIndexOf(Item: TDORThread): Integer;
begin
  Result := 0;
  while (Result < FChildCount) and (FChildList^[Result] <> Item) do
    Inc(Result);
  if Result = FChildCount then
    Result := -1;
end;

function TDORThread.ChildRemove(Item: TDORThread): Integer;
begin
  Lock;
  try
    Result := ChildIndexOf(Item);
    if Result >= 0 then
      ChildDelete(Result);
  finally
    UnLock;
  end;
end;

procedure TDORThread.ChildSetCapacity(NewCapacity: Integer);
begin
  Lock;
  try
    if (NewCapacity < FChildCount) or (NewCapacity > (Maxint div 16)) then
      raise Exception.CreateFmt('List capacity out of bounds (%d)', [NewCapacity]);
    if NewCapacity <> FChildCapacity then
    begin
      ReallocMem(FChildList, NewCapacity * SizeOf(Pointer));
      FChildCapacity := NewCapacity;
    end;
  finally
    UnLock;
  end;
end;

procedure TDORThread.Lock;
begin
  EnterCriticalSection(FCriticalSection);
end;

procedure TDORThread.UnLock;
begin
  LeaveCriticalSection(FCriticalSection);
end;

function TDORThread.GetChildCount: Integer;
begin
  Lock;
  try
    Result := FChildCount;
  finally
    UnLock;
  end;
end;

procedure TDORThread.Start;
var
  i: Integer;
begin
  Lock;
  try
    if (FThread = nil) and (ClassType <> TDORThread) then
      FThread := TThreadRun.Create(Self);
    for i := 0 to ChildCount - 1 do
      ChildItems[i].Start;
  finally
    UnLock;
  end;
end;

procedure TDORThread.Stop;
begin
  if FThread <> nil then
    FThread.Terminate;
end;

function TDORThread.GetStopped: boolean;
begin
  if FThread <> nil then
    Result := TThreadRun(FThread).Terminated
  else
    Result := True;
end;

function TDORThread.GetThreadId: Cardinal;
begin
  if FThread <> nil then
    Result := FThread.ThreadID
  else
    Result := 0;
end;

class function TDORThread.ThreadCount: integer;
begin
  Result := AThreadCount;
end;

{ TCustomObserver }

procedure TCustomObserver.doOnInternalEvent(const Event: ISuperObject);
begin

end;

procedure TCustomObserver.doOnEvent(const Event: ISuperObject);
begin

end;

function TCustomObserver.ExtractEvents: ISuperObject;
begin
  Result := FEvents.Extract;
end;

procedure TCustomObserver.TriggerInternalEvent(const Event: ISuperObject);
begin
  FEvents.Lock;
  try
    FEvents.AsArray.Add(Event);
  finally
    FEvents.Unlock;
  end;
end;

class procedure TCustomObserver.TriggerEvent(const Event: ISuperObject);
begin
  EventStorage.Trigger(Event);
end;

procedure TCustomObserver.UnregisterEvent(const name: string);
var
  l: ISuperObject;
  j: Integer;
begin
  with EventStorage.FObservers do
  begin
    Lock;
    try
      l := AsObject[name];
      if l <> nil then
      for j := 0 to l.AsArray.Length - 1 do
        if l.AsArray[j] = FEvents then
        begin
          l.AsArray.Delete(j);
          Break;
        end;
    finally
      Unlock;
    end;
    FEventProc.Remove(name);
  end;
end;

procedure TCustomObserver.UnregisterEvents;
var
  l: ISuperObject;
  j: Integer;
  name: string;
begin
  with EventStorage.FObservers do
  begin
    for name in FEventProc.Keys do
    begin
      Lock;
      try
        l := AsObject[name];
        if l <> nil then
        for j := 0 to l.AsArray.Length - 1 do
          if l.AsArray[j] = FEvents then
          begin
            l.AsArray.Delete(j);
            Break;
          end;
      finally
        Unlock;
      end;
    end;
    FEventProc.Clear;
  end;

  if FIntercepting then
    with EventStorage.FIntercept do
    begin
      Lock;
      try
        for j := 0 to AsArray.Length - 1 do
          if AsArray[j] = FEvents then
          begin
            AsArray.Delete(j);
            Break;
          end;
      finally
        Unlock;
      end;
    end;

end;

function TCustomObserver.ProcessEvents: Integer;
var
  Event: ISuperObject;
  proc: TEventProc;
begin
  Result := 0;
  for Event in ExtractEvents do
  begin
    Inc(Result);
    if ObjectIsType(Event, stObject) and ObjectIsType(Event.AsObject['event'], stString) then
    begin
      if FEventProc.TryGetValue(Event.S['event'], proc) then
        proc(Event);
    end
    else
      doOnInternalEvent(Event);
  end;
end;

procedure TCustomObserver.Intercept;
begin
  if not FIntercepting then
    with EventStorage.FIntercept do
    begin
      Lock;
      try
        AsArray.Add(FEvents);
      finally
        Unlock;
      end;
      FIntercepting := True;
    end;
end;

procedure TCustomObserver.RegisterEvent(const name: string; proc: TEventProc);
var
  l: ISuperObject;
begin
  with EventStorage.FObservers do
    begin
      Lock;
      try
        l := AsObject[name];
        if l = nil then
        begin
          l := TSuperObject.Create(stArray);
          AsObject[name] := l;
        end;
        if Assigned(proc) then
          FEventProc.AddOrSetValue(name, proc)
        else
          FEventProc.AddOrSetValue(name,
            procedure(const event: ISuperObject)
            begin
              doOnEvent(event)
            end
          );
        l.AsArray.Add(FEvents);
      finally
        Unlock;
      end;
    end;
end;

class constructor TCustomObserver.Create;
begin
  EventStorage := TEventStorage.Create;
  Application.CreateThread(TEventProcessor);
end;

destructor TCustomObserver.Destroy;
begin
  UnregisterEvents;
  FEvents := nil;
  FEventProc.Free;
  inherited;
end;

constructor TCustomObserver.Create(AOwner: TDORThread);
begin
  inherited;
  FEventProc := TDictionary<string, TEventProc>.Create;
  FEvents := TSOCriticalObject.Create(stArray);
  FIntercepting := False;
end;

class destructor TCustomObserver.Destroy;
begin
  while TDORThread.ThreadCount > 0 do Sleep(200);
  EventStorage.Free;
end;

{ TCustomObserver.TEventStorage }

constructor TCustomObserver.TEventStorage.Create;
begin
  FEvents := TSOCriticalObject.Create(stArray);
  FObservers := TSOCriticalObject.Create(stObject);
  FIntercept := TSOCriticalObject.Create(stArray);
end;

function TCustomObserver.TEventStorage.Empty: ISuperObject;
begin
  FEvents.Lock;
  try
    if FEvents.AsArray.Length > 0 then
      Result := FEvents.Extract
    else
      Result := nil;
  finally
    FEvents.Unlock;
  end;
end;

procedure TCustomObserver.TEventStorage.Trigger(const Event: ISuperObject);
begin
  FEvents.Lock;
  try
//    OutputDebugString(PChar(Format('[EVENT] [%d] %s', [FEvents.AsArray.Length, Event.AsString])));
    FEvents.AsArray.Add(Event);
  finally
    FEvents.Unlock;
  end;
end;

{ TCustomObserver.TEventProcessor }

function TCustomObserver.TEventProcessor.Run: Cardinal;
var
  events, event, box: ISuperObject;
begin
{$if defined(DEBUG)}
  TThread.NameThreadForDebugging(AnsiString(Self.ClassName));
{$ifend}

  while not Stopped do
  begin
    events := EventStorage.Empty;
    if events <> nil then
      for event in events do
        if event <> nil then
        begin
          EventStorage.FIntercept.Lock;
          try
            for box in EventStorage.FIntercept do
            begin
              ISOCriticalObject(box).Lock;
              try
                box.AsArray.Add(event.Clone);
              finally
                ISOCriticalObject(box).Unlock;
              end;
            end;
          finally
            EventStorage.FIntercept.Unlock;
          end;

          EventStorage.FObservers.Lock;
          try
            for box in EventStorage.FObservers.N[event.AsObject.S['event']] do
            begin
              ISOCriticalObject(box).Lock;
              try
                box.AsArray.Add(event.Clone);
              finally
                ISOCriticalObject(box).Unlock;
              end;
            end;
          finally
            EventStorage.FObservers.Unlock;
          end;
        end;
{$IFDEF SWITCHTOTHREAD}
    if not SwitchToThread then
{$ENDIF}
      Sleep(1);
  end;
  Result := 0;
end;

{ TRWSocket }

constructor TRWSocket.Create(Socket: TSocket; const ClientIP: AnsiString; Owned: Boolean);
begin
  inherited Create;
  FBufferPos := 0;
  FSocket := Socket;
  FOwned := Owned;
  FClientIP := ClientIP;
  FReadTimeout := 0;
  FWriteTimeout := 0;
end;

procedure TRWSocket.Close;
begin
  Flush;
  if FOwned then
    closesocket(FSocket);
  FSocket := INVALID_SOCKET;
end;

function TRWSocket.ClientIP: AnsiString;
begin
  Result := FClientIP;
end;

function TRWSocket.HavePeerCertificate: Boolean;
begin
  Result := False;
end;

function TRWSocket.IsSSL: Boolean;
begin
  Result := False;
end;

function TRWSocket.SSLIssuer(const key: AnsiString): AnsiString;
begin
  Result := '';
end;

function TRWSocket.SSLSubject(const key: AnsiString): AnsiString;
begin
  Result := '';
end;

function TRWSocket.Read(var buf; len, Timeout: Cardinal): Cardinal;
var
 p: PByte;
begin
  if (FReadTimeout <> Timeout) then
  begin
    setsockopt(FSocket, SOL_SOCKET, SO_RCVTIMEO, @Timeout, SizeOf(Timeout));
    FReadTimeout := Timeout;
  end;

  Result := 0;
  p := @Buf;
  while len > 0 do
    if Winsock2.recv(FSocket, p^, 1, 0) = 1 then
    begin
      Dec(len, 1);
      Inc(p);
      Inc(Result);
    end
    else
      Break;
end;

function TRWSocket.Write(var buf; len, Timeout: Cardinal): Cardinal;
var
  l: Cardinal;
  p: PByte;
begin
  Result := len;

  SocketTuneSendBuffer(FSocket);

  if (FWriteTimeout <> Timeout) then
  begin
    setsockopt(FSocket, SOL_SOCKET, SO_SNDTIMEO, @Timeout, SizeOf(Timeout));
    FWriteTimeout := Timeout;
  end;

  l := Min(len, BUFFER_SIZE - FBufferPos);
  p := PByte(@buf);
  while l > 0 do
  begin
    Move(p^, FBuffer[FBufferPos], l);
    Dec(len, l);
    Inc(p, l);
    Inc(FBufferPos, l);

    if FBufferPos = BUFFER_SIZE then
    begin
      Winsock2.send(FSocket, FBuffer, BUFFER_SIZE, 0);
      FBufferPos := 0;
    end;
    l := Min(len, BUFFER_SIZE - FBufferPos);
  end;
end;

procedure TRWSocket.Flush;
begin
  if FBufferPos > 0 then
  begin
    Winsock2.send(FSocket, FBuffer, FBufferPos, 0);
    FBufferPos := 0;
  end;
end;

{ TAbstractServer }

constructor TAbstractServer.CreateServer(Port: Word; const Bind: string;
  const StubClass: TClientStubClass;
  const OnSocketStub: TOnSocketStub);
begin
  inherited Create(Application.Threads);
  FStubClass := StubClass;
  FOnSocketStub := OnSocketStub;
  FSocketHandle := INVALID_SOCKET;
  FPort := Port;
  FBind := inet_addr(PAnsiChar(AnsiString(Bind)));
end;

{ TSocketServer }

function TSocketServer.Run: Cardinal;
//type
//  TTcpKeepalive = record
//    onoff: Cardinal;
//    keepalivetime: Cardinal;
//    keepaliveinterval: Cardinal;
//  end;
const
  DOR_MAX_CONN = SOMAXCONN; // 200
var
  InputSocket: TSocket;
  InputAddress: TSockAddrIn;
  InputLen: Integer;
  Stub: TDORThread;
  linger: TLinger;
  optval: Integer;
//  bytes: DWORD;
//  alive: TTcpKeepalive;
begin
{$if defined(DEBUG)}
  TThread.NameThreadForDebugging(AnsiString(Self.ClassName));
{$ifend}

  Result := 0;
  FSocketHandle := socket(AF_INET, SOCK_STREAM, 0);
  PSockAddrIn(@FAddress).sin_addr.s_addr := FBind;
  PSockAddrIn(@FAddress).sin_family := AF_INET;
  PSockAddrIn(@FAddress).sin_port := htons(FPort);

  { Activates Socket ReuseAddr option }
  optval := 1;
  if setsockopt(FSocketHandle, SOL_SOCKET, SO_REUSEADDR, PAnsiChar(@optval), SizeOf(optval)) <> 0 then
    RaiseLastOSError(WSAGetLastError);

  { Disable Naggle algorithm }
//  optval := 1;
//  if setsockopt(FSocketHandle, IPPROTO_TCP, TCP_NODELAY, PAnsiChar(@optval), SizeOf(optval)) <> 0 then
//    RaiseLastOSError(WSAGetLastError);

  { Sets size of output buffer }
//  optval := 0;
//  if setsockopt(FSocketHandle, SOL_SOCKET, SO_SNDBUF, PAnsiChar(@optval), SizeOf(optval)) <> 0 then
//    RaiseLastOSError(WSAGetLastError);

  { Activates Socket KeepAlive option }
  optval := 1;
  if setsockopt(FSocketHandle, SOL_SOCKET, SO_KEEPALIVE, PAnsiChar(@optval), SizeOf(optval)) <> 0 then
    RaiseLastOSError(WSAGetLastError);

  { SIO_KEEPALIVE_VALS : Activates Socket KeepAlive option with KeepAlive values (time = 20s, interval = 1s) }
//  alive.onoff := 1;
//  alive.keepaliveinterval := 1000;
//  alive.keepalivetime := 20 * 1000;
//  bytes := 0;
//  if WSAIoctl(FSocketHandle, _WSAIOW(IOC_VENDOR, 4), @alive, SizeOf(alive), nil, 0, bytes, nil, nil) = SOCKET_ERROR then
//    RaiseLastOSError(WSAGetLastError);

  { Activates Socket Linger on close() option }
  linger.l_onoff := 1;
  linger.l_linger := 5;
  if setsockopt(FSocketHandle, SOL_SOCKET, SO_LINGER, PAnsiChar(@linger), sizeof(linger)) <> 0 then
    RaiseLastOSError(WSAGetLastError);

  if bind(FSocketHandle, FAddress, SizeOf(FAddress)) <> 0 then
  begin
    Stop;
    raise Exception.Create('Can''t bind()');
  end;

  if (listen(FSocketHandle, DOR_MAX_CONN) <> 0) then
  begin
    Stop;
    raise Exception.Create('Can''t listen()');
  end;

//  { Borrowed from Firebird source code : optimization for loopback connections
//    SIO_LOOPBACK_FAST_PATH = _WSAIOW(IOC_VENDOR, 16)
//    Only for Windows 8+ and Windows Server 2012+ }
//  optval := 1;
//  bytes := 0;
//  if WSAIoctl(FSocketHandle, _WSAIOW(IOC_VENDOR, 16), @optval, SizeOf(optval),	nil, 0, bytes, nil, nil) = SOCKET_ERROR then
//    RaiseLastOSError(WSAGetLastError);

  InputLen := SizeOf(InputAddress);
  while not Stopped do
  try
    InputSocket := accept(FSocketHandle, @InputAddress, @InputLen);
    if (InputSocket <> INVALID_SOCKET) then
    begin
      if not Assigned(FOnSocketStub) then
        Stub := FStubClass.CreateStub(Self, TRWSocket.Create(InputSocket, inet_ntoa(InputAddress.sin_addr), True))
      else
        Stub := FStubClass.CreateStub(Self, FOnSocketStub(InputSocket, inet_ntoa(InputAddress.sin_addr)));

      if Stub <> nil then
        Stub.Start
      else
        closesocket(InputSocket);
    end;
  except
    // the server must continue to listen !
  end;
end;

procedure TSocketServer.Stop;
begin
  inherited;
  if FSocketHandle <> INVALID_SOCKET then
  begin
    shutdown(FSocketHandle, SD_BOTH);
    closesocket(FSocketHandle);
    FSocketHandle := INVALID_SOCKET;
  end;
end;

{ TUDPServer }

function TUDPServer.Run: Cardinal;
begin
{$if defined(DEBUG)}
  TThread.NameThreadForDebugging(AnsiString(Self.ClassName));
{$ifend}

  Result := 0;
  FSocketHandle := socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP);
  PSockAddrIn(@FAddress).sin_addr.s_addr := FBind;
  PSockAddrIn(@FAddress).sin_family := AF_INET;
  PSockAddrIn(@FAddress).sin_port := htons(FPort);

  if bind(FSocketHandle, FAddress, SizeOf(FAddress)) <> 0 then
  begin
    Stop;
    raise Exception.Create('can''t bind.');
  end;
end;

procedure TUDPServer.Stop;
begin
  inherited;
  if FSocketHandle <> INVALID_SOCKET then
  begin
    shutdown(FSocketHandle, SD_BOTH);
    closesocket(FSocketHandle);
    FSocketHandle := INVALID_SOCKET;
  end;
end;

{ TClientStub }

constructor TClientStub.CreateStub(AOwner: TSocketServer; const Source: IReadWrite);
begin
  inherited Create(AOwner);
  FSource := Source;
end;

function TClientStub.Run: Cardinal;
begin
  // you must implement this method
  Result := 0;
end;

procedure TClientStub.Stop;
begin
  inherited;
  if FSource <> nil then
  begin
    FSource.Close;
    Release;
  end;
end;

function TClientStub.GetSource: IReadWrite;
begin
  if Assigned(FSource) then
    Result := FSource
  else
    Result := TNullSocket.Create;
end;

procedure TClientStub.Release;
begin
  FSource := nil;
end;

{ SSL Error Handling }

function SSLError(const Ssl: PSSL; const ReturnCode: Integer; const SSLMethod: string): Integer;

  function SSLErrorMsg(const ErrorCode: Integer): string; inline;
  begin
    case ErrorCode of
      SSL_ERROR_NONE:                 Result := 'NONE';
      SSL_ERROR_SSL:                  Result := 'SSL';
      SSL_ERROR_WANT_READ:            Result := 'WANT READ';
      SSL_ERROR_WANT_WRITE:           Result := 'WANT WRITE';
      SSL_ERROR_WANT_X509_LOOKUP:     Result := 'WANT X509 LOOKUP';
      SSL_ERROR_SYSCALL:              Result := 'SYSCALL';
      SSL_ERROR_ZERO_RETURN:          Result := 'ZERO_RETURN';
      SSL_ERROR_WANT_CONNECT:         Result := 'CONNECT';
      SSL_ERROR_WANT_ACCEPT:          Result := 'ACCEPT';
      SSL_ERROR_WANT_ASYNC:           Result := 'ASYNC';
      SSL_ERROR_WANT_ASYNC_JOB:       Result := 'ASYNC JOB';
      SSL_ERROR_WANT_CLIENT_HELLO_CB: Result := 'CLIENT HELLO CB';
    else
      Result := Format('%d:UNKNOWN', [ErrorCode]);
    end;
  end;

  procedure SSLExploreErrorStack(const SSLMethod: string; const ReturnCode, ErrorCode: Integer); inline;
  const
    ERROR_BUF_LEN = 512;
  var
    err: Integer;
    err_buf: array[0..ERROR_BUF_LEN - 1] of Byte;
    err_msg: string;
  begin
    err := ERR_get_error();
    while err <> SSL_ERROR_NONE do
    begin
      ERR_error_string_n(err, @err_buf[0], ERROR_BUF_LEN);
      err_msg := TEncoding.Default.GetString(err_buf);
      OutputDebugString(PChar(Format('%s=%d, ssl_error=%d (%s) %s', [
        SSLMethod, ReturnCode, ErrorCode, SSLErrorMsg(ErrorCode), err_msg
      ])));
      err := ERR_get_error();
    end;
  end;

begin
  Result := SSL_get_error(SSL, ReturnCode);
  case Result of
    SSL_ERROR_NONE: { allright ! do nothing };
    SSL_ERROR_SSL, SSL_ERROR_SYSCALL:
      SSLExploreErrorStack(SSLMethod, ReturnCode, Result);
    else
      OutputDebugString(PChar(Format('%s=%d, ssl_error: %d (%s)', [
        SSLMethod, ReturnCode, Result, SSLErrorMsg(Result)
      ])));
  end;
end;

{ SSL Private Key Password Provider }

function SSLPasswordCallback(buffer: PAnsiChar; size, rwflag: Integer;
  this: TSSLRWSocket): Integer; cdecl;
var
  password: AnsiString;
begin
  password := this.FPassword;
  if Length(password) > (Size - 1) then
    SetLength(password, Size - 1);
  Result := Length(password);
  Move(PAnsiChar(password)^, buffer^, Result + 1);
end;

{ TSSLRWSocket }

constructor TSSLRWSocket.Create(Socket: TSocket; const ClientIP: AnsiString;
  Owned: Boolean; Verify: Integer; const password, CertificateFile,
  PrivateKeyFile, CertCAFile: AnsiString);
label
  error;
begin
  inherited Create;
  FSocket := Socket;
  FClientIP := ClientIP;
  FOwned := Owned;
  FReadTimeout := 0;
  FWriteTimeout := 0;

  // SSL
  FCtx := nil;
  FSsl := nil;
  FConnected := False;

  FPassword := password;
  FCtx := SSL_CTX_new(TLS_method);
  SSL_CTX_set_min_proto_version(FCtx, TLS1_2_VERSION);
  SSL_CTX_set_options(FCtx, SSL_OP_NO_SSLv3 or SSL_OP_NO_COMPRESSION);
  SSL_CTX_set_cipher_list(FCtx, DOR_SSL_CIPHER_LIST);

  SSL_CTX_set_verify(FCtx, Verify, nil);
  SSL_CTX_set_default_passwd_cb_userdata(FCtx, Self);
  SSL_CTX_set_default_passwd_cb(FCtx, @SSLPasswordCallback);

  if CertificateFile <> '' then
    if SSL_CTX_use_certificate_chain_file(FCtx, PAnsiChar(CertificateFile)) <> 1 then
      if SSL_CTX_use_certificate_file(FCtx, PAnsiChar(CertificateFile), SSL_FILETYPE_PEM) <> 1 then
        if SSL_CTX_use_certificate_file(FCtx, PAnsiChar(CertificateFile), SSL_FILETYPE_ASN1) <> 1 then
          goto error;

  if PrivateKeyFile <> '' then
    if SSL_CTX_use_RSAPrivateKey_file(FCtx, PAnsiChar(PrivateKeyFile), SSL_FILETYPE_PEM) <> 1 then
      if SSL_CTX_use_RSAPrivateKey_file(FCtx, PAnsiChar(PrivateKeyFile), SSL_FILETYPE_ASN1) <> 1 then
        goto error;

  if CertCAFile <> '' then
    if SSL_CTX_load_verify_locations(FCtx, PAnsiChar(CertCAFile), nil) <> 1 then
      goto error;

  FSsl := SSL_new(FCtx);
  if FSsl = nil then
    goto error;

  ERR_clear_error;
  if SSLError(FSsl, SSL_set_fd(FSsl, FSocket), 'SSL_set_fd') <> SSL_ERROR_NONE then
    goto error;

  ERR_clear_error;
  if SSLError(FSsl, SSL_accept(FSsl), 'SSL_accept') <> SSL_ERROR_NONE then
    goto error;

  FX509 := SSL_get_peer_certificate(FSsl);

  FConnected := True;
  Exit;

error:
  CloseSSL;
end;

destructor TSSLRWSocket.Destroy;
begin
  CloseSSL;
  OPENSSL_thread_stop;
  inherited;
end;

function TSSLRWSocket.ClientIP: AnsiString;
begin
  Result := FClientIP;
end;

procedure TSSLRWSocket.Close;
begin
  if FOwned then
  begin
    Flush;
    closesocket(FSocket);
  end;
  FSocket := INVALID_SOCKET;
end;

procedure TSSLRWSocket.CloseSSL;
begin
  FConnected := False;
  if FX509 <> nil then
  begin
    X509_free(FX509);
    FX509 := nil;
  end;
  if FSsl <> nil then
  begin
    SSL_free(FSsl);
    FSsl := nil;
  end;
  if FCtx <> nil then
  begin
    SSL_CTX_free(FCtx);
    FCtx := nil;
  end;
end;

function TSSLRWSocket.HavePeerCertificate: Boolean;
begin
  Result := FX509 <> nil;
end;

function TSSLRWSocket.IsSSL: Boolean;
begin
  Result := True;
end;

function TSSLRWSocket.SSLIssuer(const key: AnsiString): AnsiString;
begin
  if FX509 <> nil then
    Result := X509NameFind(X509_get_issuer_name(FX509), key)
  else
    Result := '';
end;

function TSSLRWSocket.SSLSubject(const key: AnsiString): AnsiString;
begin
  if FX509 <> nil then
    Result := X509NameFind(X509_get_subject_name(FX509), key)
  else
    Result := '';
end;

function TSSLRWSocket.Read(var buf; len, Timeout: Cardinal): Cardinal;
var
  p: PByte;
begin
  Result := 0;
  if FConnected then
  begin
    if (FReadTimeout <> Timeout) then
    begin
      setsockopt(FSocket, SOL_SOCKET, SO_RCVTIMEO, @Timeout, SizeOf(Timeout));
      FReadTimeout := Timeout;
    end;

    p := @Buf;
    while len > 0 do
    begin
      if FSsl <> nil then
      begin
        ERR_clear_error;
        if SSLError(FSSl, SSL_read(FSsl, p, 1), '[Read] SSL_read') = SSL_ERROR_NONE then
        begin
          Dec(len);
          Inc(p);
          Inc(Result);
        end
        else
          Break;
      end
      else
        Break;
    end;
  end;
end;

function TSSLRWSocket.Write(var buf; len, Timeout: Cardinal): Cardinal;
var
  l: Cardinal;
  p: PByte;
begin
  if FConnected then
  begin
    Result := len;

    SocketTuneSendBuffer(FSocket);

    if (FWriteTimeout <> Timeout) then
    begin
      setsockopt(FSocket, SOL_SOCKET, SO_SNDTIMEO, @Timeout, SizeOf(Timeout));
      FWriteTimeout := Timeout;
    end;

    l := Min(len, BUFFER_SIZE - FBufferPos);
    p := PByte(@buf);
    while l > 0 do
    begin
      Move(p^, FBuffer[FBufferPos], l);
      Dec(len, l);
      Inc(p, l);
      Inc(FBufferPos, l);

      if FBufferPos = BUFFER_SIZE then
      begin
        if FSsl <> nil then
        begin
          ERR_clear_error;
          if SSLError(FSsl, SSL_write(FSsl, @FBuffer, BUFFER_SIZE), '[Write] SSL_write') <> SSL_ERROR_NONE then
            Break;
        end;
        FBufferPos := 0;
      end;

      l := Min(len, BUFFER_SIZE - FBufferPos);
    end;
  end
  else
    Result := 0;
end;

procedure TSSLRWSocket.Flush;
begin
  if FBufferPos > 0 then
  begin
    if FSsl <> nil then
    begin
      ERR_clear_error;
      SSLError(FSsl, SSL_write(FSsl, @FBuffer, FBufferPos), '[Flush] SSL_write');
    end;
    FBufferPos := 0;
  end;
end;

{ TNullSocket }

function TNullSocket.ClientIP: AnsiString;
begin
  Result := '';
end;

procedure TNullSocket.Close;
begin
  { do nothing }
end;

procedure TNullSocket.Flush;
begin
  { do nothing }
end;

function TNullSocket.HavePeerCertificate: Boolean;
begin
  Result := False;
end;

function TNullSocket.IsSSL: Boolean;
begin
  Result := False;
end;

function TNullSocket.SSLIssuer(const key: AnsiString): AnsiString;
begin
  Result := '';
end;

function TNullSocket.SSLSubject(const key: AnsiString): AnsiString;
begin
  Result := '';
end;

function TNullSocket.Read(var buf; len, Timeout: Cardinal): Cardinal;
begin
  Result := 0;
end;

function TNullSocket.Write(var buf; len, Timeout: Cardinal): Cardinal;
begin
  Result := 0;
end;

initialization
  IsMultiThread := true;

end.
