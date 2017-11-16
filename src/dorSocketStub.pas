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
  Windows, Winsock, dorOpenSSL,
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
    function ChildRemove(Item: TDORThread): Integer;
    function GetChildCount: Integer;
    function GetStopped: boolean;
  protected
    function Run: Cardinal; virtual;
    procedure Stop; virtual;
  public
    class function ThreadCount: integer;
    class procedure BeginThread;
    class procedure EndThread;
    property Owner: TDORThread read FOwner;
    procedure ChildClear; virtual;
    procedure Suspend;
    procedure Resume;
    procedure Start;
    procedure Lock;
    procedure UnLock;
    constructor Create(AOwner: TDORThread); virtual;
    destructor Destroy; override;
    property ChildCount: Integer read GetChildCount;
    property ChildItems[Index: Integer]: TDORThread read ChildGet; default;
    property Stopped: boolean read GetStopped;
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
    function Read(var buf; len, Timeout: Cardinal): Cardinal;
    function Write(var buf; len, Timeout: Cardinal): Cardinal;
    function IsSSL: Boolean;
    function HavePeerCertificate: Boolean;
    function SSLSubject(const key: AnsiString): AnsiString;
    function SSLIssuer(const key: AnsiString): AnsiString;
    procedure Close;
  end;

  TRWSocket = class(TInterfacedObject, IReadWrite)
  private
    FSocket: TSocket;
    FOwned: Boolean;
    FReadTimeout: Cardinal;
    FWriteTimeout: Cardinal;
  protected
    function Read(var buf; len, Timeout: Cardinal): Cardinal; virtual;
    function Write(var buf; len, Timeout: Cardinal): Cardinal; virtual;
    function IsSSL: Boolean; virtual;
    function HavePeerCertificate: Boolean; virtual;
    function SSLSubject(const key: AnsiString): AnsiString; virtual;
    function SSLIssuer(const key: AnsiString): AnsiString; virtual;
    procedure Close;
  public
    constructor Create(Socket: TSocket; Owned: Boolean); virtual;
    destructor Destroy; override;
  end;

  TSSLRWSocket = class(TInterfacedObject, IReadWrite)
  private
    FSocket: TSocket;
    FOwned: Boolean;
    FReadTimeout: Cardinal;
    FWriteTimeout: Cardinal;
    // SSL
    FCtx: PSSL_CTX;
    FSsl: PSSL;
    FX509: PX509;
    FPassword: AnsiString;
    FConnected: Boolean;
    procedure CloseSSL;
  protected
    function Read(var buf; len, Timeout: Cardinal): Cardinal; virtual;
    function Write(var buf; len, Timeout: Cardinal): Cardinal; virtual;
    function IsSSL: Boolean; virtual;
    function HavePeerCertificate: Boolean; virtual;
    function SSLSubject(const key: AnsiString): AnsiString; virtual;
    function SSLIssuer(const key: AnsiString): AnsiString; virtual;
    procedure Close;
  public
    constructor Create(Socket: TSocket; Owned: Boolean; Verify: Integer;
      const password, CertificateFile, PrivateKeyFile, CertCAFile: AnsiString); virtual;
    destructor Destroy; override;
  end;

  TOnSocketStub = function(socket: TSocket): IReadWrite;

  TAbstractServer = class(TDORThread)
  private
    FStubClass: TClientStubClass;
    FOnSocketStub: TOnSocketStub;
    FAddress: TSockAddr;
    FSocketHandle: LongInt;
    FPort: Word;
    FBind: Longint;
  public
    property Address: TSockAddr read FAddress;
    property SocketHandle: LongInt read FSocketHandle;
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
  protected
    function Run: Cardinal; override;
    procedure Stop; override;
    procedure Release;
  public
    property Source: IReadWrite read FSource;
    constructor CreateStub(AOwner: TSocketServer; const Source: IReadWrite); virtual;
  end;

threadvar
  CurrentDorThread: TDORThread;

implementation
uses
  SysUtils, dorService;

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
      CurrentDorThread.Free else
      if CurrentDorThread.FOwner <> nil then
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
  if (Index < 0) or (Index >= FChildCount) then exit;

  with ChildGet(Index) do
    if InterlockedDecrement(FThreadRefCount) = 0 then
      Free else
      Stop;

  Dec(FChildCount);
  if Index < FChildCount then
    System.Move(FChildList^[Index + 1], FChildList^[Index],
      (FChildCount - Index) * SizeOf(Pointer));
end;

function TDORThread.ChildGet(Index: Integer): TDORThread;
begin
  Result := nil;
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
    if ClassType <> TDORThread then
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
    Result := TThreadRun(FThread).Terminated else
    Result := True;
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
          FEventProc.AddOrSetValue(name, proc) else
          FEventProc.AddOrSetValue(name, procedure(const event: ISuperObject) begin doOnEvent(event) end);
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
      Result := FEvents.Extract else
      Result := nil;
  finally
    FEvents.Unlock;
  end;
end;

procedure TCustomObserver.TEventStorage.Trigger(const Event: ISuperObject);
begin
  FEvents.Lock;
  try
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

procedure TRWSocket.Close;
begin
  if FOwned then
  begin
    CloseSocket(FSocket);
    Sleep(1);
  end;
  FSocket := INVALID_SOCKET;
end;

constructor TRWSocket.Create(Socket: TSocket; Owned: Boolean);
begin
  inherited Create;
  FSocket := Socket;
  FOwned := Owned;
  FReadTimeout := 0;
  FWriteTimeout := 0;
end;

destructor TRWSocket.Destroy;
begin
  Close;
  inherited;
end;

function TRWSocket.HavePeerCertificate: Boolean;
begin
  Result := False;
end;

function TRWSocket.IsSSL: Boolean;
begin
  Result := False;
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
    if Winsock.recv(FSocket, p^, 1, 0) = 1 then
    begin
      Dec(len, 1);
      Inc(p);
      Inc(Result);
    end else
      Break;
end;


function TRWSocket.SSLIssuer(const key: AnsiString): AnsiString;
begin
  Result := '';
end;

function TRWSocket.SSLSubject(const key: AnsiString): AnsiString;
begin
  Result := '';
end;

function TRWSocket.Write(var buf; len, Timeout: Cardinal): Cardinal;
begin
  if (FWriteTimeout <> Timeout) then
  begin
    setsockopt(FSocket, SOL_SOCKET, SO_SNDTIMEO, @Timeout, SizeOf(Timeout));
    FWriteTimeout := Timeout;
  end;

  Result := Winsock.send(FSocket, buf, len, 0);
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
var
  InputSocket: longint;
  InputAddress: TSockAddr;
  InputLen: Integer;
  Stub: TDORThread;
  SO_True: Integer;
begin
{$if defined(DEBUG)}
  TThread.NameThreadForDebugging(AnsiString(Self.ClassName));
{$ifend}

  SO_True := -1;
  Result := 0;
  FSocketHandle := socket(AF_INET, SOCK_STREAM, 0);
  FAddress.sin_addr.s_addr := FBind;
  FAddress.sin_family := AF_INET;
  FAddress.sin_port := htons(FPort);

  SetSockOpt(FSocketHandle, SOL_SOCKET, SO_REUSEADDR, PAnsiChar(@SO_True), SizeOf(SO_True));
  SetSockOpt(FSocketHandle, IPPROTO_TCP, TCP_NODELAY, PAnsiChar(@SO_True), SizeOf(SO_True));

  if bind(FSocketHandle, FAddress, SizeOf(FAddress)) <> 0 then
  begin
    Stop;
    raise Exception.Create('can''t bind.');
  end;

  if (listen(FSocketHandle, 200) <> 0) then
  begin
    Stop;
    raise Exception.Create('can''t listen.');
  end;

  InputLen := SizeOf(InputAddress);
  while not Stopped do
  try
    InputSocket := accept(FSocketHandle, @InputAddress, @InputLen);
    if (InputSocket <> INVALID_SOCKET) then
    begin
      if not Assigned(FOnSocketStub) then
        Stub := FStubClass.CreateStub(Self, TRWSocket.Create(InputSocket, True)) else
        Stub := FStubClass.CreateStub(Self, FOnSocketStub(InputSocket));

      if Stub <> nil then
        Stub.Start else
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
    closesocket(FSocketHandle);
    InterlockedExchange(LongInt(FSocketHandle), LongInt(INVALID_SOCKET));
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
  FAddress.sin_addr.s_addr := FBind;
  FAddress.sin_family := AF_INET;
  FAddress.sin_port := htons(FPort);

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
    closesocket(FSocketHandle);
    InterlockedExchange(LongInt(FSocketHandle), LongInt(INVALID_SOCKET));
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

procedure TClientStub.Release;
begin
  FSource := nil;
end;

{ TSSLRWSocket }

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

procedure TSSLRWSocket.Close;
begin
  if FOwned then
  begin
    CloseSocket(FSocket);
    Sleep(1);
    CloseSSL;
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
  if Fssl <> nil then
  begin
    SSL_free(FSsl);
    FSsl := nil;
  end;
  if Fctx <> nil then
  begin
    SSL_CTX_free(FCtx);
    FCtx := nil;
  end;
end;

constructor TSSLRWSocket.Create(Socket: TSocket; Owned: Boolean;
  Verify: Integer; const password, CertificateFile,
  PrivateKeyFile, CertCAFile: AnsiString);
label
  error;
begin
  inherited Create;
  FSocket := Socket;
  FOwned := Owned;
  FReadTimeout := 0;
  FWriteTimeout := 0;

  // SSL
  FCtx := nil;
  FSsl := nil;
  FConnected := False;

  FPassword := password;
  FCtx := SSL_CTX_new(SSLv23_method);
{$IFDEF NO_SSLV3}
  SSL_CTX_set_options(FCtx, SSL_OP_NO_SSLv3);
{$ENDIF}
  SSL_CTX_set_cipher_list(FCtx, 'DEFAULT');
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

  if SSL_set_fd(FSsl, FSocket) <> 1 then
    goto error;

  if SSL_accept(FSsl) <> 1 then
    goto error;

  FX509 := SSL_get_peer_certificate(FSsl);

  FConnected := True;
  Exit;
error:
  CloseSSL;
end;

destructor TSSLRWSocket.Destroy;
begin
  Close;
  inherited;
end;

function TSSLRWSocket.HavePeerCertificate: Boolean;
begin
  Result := FX509 <> nil;
end;

function TSSLRWSocket.IsSSL: Boolean;
begin
  Result := True;
end;

function TSSLRWSocket.Read(var buf; len, Timeout: Cardinal): Cardinal;
var
 p: PByte;
begin
  if FConnected then
  begin
    if (FReadTimeout <> Timeout) then
    begin
      setsockopt(FSocket, SOL_SOCKET, SO_RCVTIMEO, @Timeout, SizeOf(Timeout));
      FReadTimeout := Timeout;
    end;

    Result := 0;
    p := @Buf;

    while len > 0 do
      if SSL_read(FSsl, p, 1) = 1 then
      begin
        Dec(len, 1);
        Inc(p);
        Inc(Result);
      end else
        Break;
  end else
    Result := 0;
end;

function TSSLRWSocket.SSLIssuer(const key: AnsiString): AnsiString;
begin
  if FX509 <> nil then
    Result := X509NameFind(X509_get_issuer_name(FX509), key) else
    Result := '';
end;

function TSSLRWSocket.SSLSubject(const key: AnsiString): AnsiString;
begin
  if FX509 <> nil then
    Result := X509NameFind(X509_get_subject_name(FX509), key) else
    Result := '';
end;

function TSSLRWSocket.Write(var buf; len, Timeout: Cardinal): Cardinal;
begin
  if FConnected then
  begin
    if (FWriteTimeout <> Timeout) then
    begin
      setsockopt(FSocket, SOL_SOCKET, SO_SNDTIMEO, @Timeout, SizeOf(Timeout));
      FWriteTimeout := Timeout;
    end;
    Result := SSL_write(FSsl, @buf, len)
  end else
    Result := 0;
end;

initialization
  IsMultiThread := true;

end.
