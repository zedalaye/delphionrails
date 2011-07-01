unit dorXMPPClient;
{$define XMPP_DEBUG_CONSOLE}

interface
uses
  SysUtils, Windows, Classes, WinSock, dorXML, dorOpenSSL,
  Generics.Collections;

type
  IXMPPClient = interface;
  IXMPPPresence = interface;
  IXMPPIQ = interface;
  IXMPPMessage = interface;

  TXMPPIQType = (iqGet, iqSet, iqResult, iqError);
  TXMPPMessageType = (mtNone, mtNormal, mtChat, mtGroupChat, mtHeadline, mtError);
  TXMPPErrorType = (etAuth, etCancel, etContinue, etModify, etWait);

  TXMPPErrorEvent = reference to procedure(const msg: string);
  TXMPPIQEvent = reference to procedure(const node: IXMPPIQ);
  TXMPPEvent = reference to procedure(const node: IXMPPPresence);
  TXMPPIQResponse = reference to procedure(const node: IXMPPIQ);
  TXMPPMessageEvent = reference to procedure(const node: IXMPPMessage);

  TXMPPOnSynchronize = reference to procedure(const node: IXMLNode; const proc: TProc<IXMLNode>);

  TXMPPReadyState = (rsOffline, rsConnecting, rsConnected, rsAuthenticating, rsAuthenticated, rsOpen, rsClosing);
  TXMPPReadyStateChange = reference to procedure(const xmpp: IXMPPClient);
  TXMPPOption = (xoDontForceEncryption, xoPlaintextAuth, xoRegister);
  TXMPPOptions = set of TXMPPOption;


  TXMPPPresenceType = (ptNone, ptUnavailable, ptSubscribe, ptSubscribed,
    ptUnsubscribe, ptUnsubscribed, ptProbe, ptError);
  TXMPPPresenceShow = (psNone, psAway, psChat, psDnd, psXa);

  IXMPPClient = interface
    ['{FD2264FF-460E-4DCD-BA8B-D1D7BCB45E6A}']
    function GetJID: string;
    function GetReadyState: TXMPPReadyState;
    procedure SendXML(const xml: IXMLNode);
    procedure SendIQ(const IQ: IXMPPIQ; const callback: TXMPPIQResponse);
    procedure Close;
    function Open(const user, pass: string;
      const resource: string = ''; const host: string = ''; port: Word = 5222;
      options: TXMPPOptions = []): Boolean;
    function GetOnReadyStateChange: TXMPPReadyStateChange;
    procedure SetOnReadyStateChange(const cb: TXMPPReadyStateChange);

    function GetOnError: TXMPPErrorEvent;
    function GetOnIQ: TXMPPIQEvent;
    function GetOnPresence: TXMPPEvent;
    function GetOnMessage: TXMPPMessageEvent;
    function GetOnSynchronize: TXMPPOnSynchronize;

    procedure SetOnError(const value: TXMPPErrorEvent);
    procedure SetOnIQ(const value: TXMPPIQEvent);
    procedure SetOnPresence(const value: TXMPPEvent);
    procedure SetOnMessage(const value: TXMPPMessageEvent);
    procedure SetOnSynchronize(const value: TXMPPOnSynchronize);

    property OnError: TXMPPErrorEvent read GetOnError write SetOnError;
    property OnIQ: TXMPPIQEvent read GetOnIQ write SetOnIQ;
    property OnPresence: TXMPPEvent read GetOnPresence write SetOnPresence;
    property OnMessage: TXMPPMessageEvent read GetOnMessage write SetOnMessage;
    property OnSynchronize: TXMPPOnSynchronize read GetOnSynchronize write SetOnSynchronize;

    property ReadyState: TXMPPReadyState read getReadyState;
    property OnReadyStateChange: TXMPPReadyStateChange read GetOnReadyStateChange write SetOnReadyStateChange;
    property JID: string read GetJID;
  end;

  IXMPPPresence = interface(IXMLNode)
    ['{AA624DDD-C548-462E-B381-8C7A1616CF49}']
    function GetPresence: TXMPPPresenceType;
    function GetShow: TXMPPPresenceShow;
    function GetStatus: string;
    function GetPriority: ShortInt;
    function GetDest: string;
    function GetSrc: string;
    procedure SetPresence(const value: TXMPPPresenceType);
    procedure SetShow(const value: TXMPPPresenceShow);
    procedure SetStatus(const value: string);
    procedure SetPriority(const value: ShortInt);
    procedure SetDest(const value: string);
    procedure SetSrc(const value: string);
    property kind: TXMPPPresenceType read GetPresence write SetPresence;
    property Show: TXMPPPresenceShow read GetShow write SetShow;
    property Status: string read GetStatus write SetStatus;
    property Priority: ShortInt read GetPriority write SetPriority;
    property Dest: string read GetDest write SetDest;
    property Src: string read GetSrc write SetSrc;
  end;

  IXMPPIQ = interface(IXMLNode)
    ['{2CF1E3EB-F59B-40E7-AAD8-E2CE31E3406B}']
    function Reply: IXMPPIQ;
    function Error(kind: TXMPPErrorType): IXMPPIQ;
    function GetDest: string;
    function GetSrc: string;
    function GetIQ: TXMPPIQType;
    function GetId: string;
    procedure SetDest(const value: string);
    procedure SetSrc(const value: string);
    procedure SetIQ(value: TXMPPIQType);
    procedure SetId(const value: string);
    property Dest: string read GetDest write SetDest;
    property Src: string read GetSrc write SetSrc;
    property Kind: TXMPPIQType read GetIQ write SetIQ;
    property Id: string read GetId write SetId;
  end;

  IXMPPMessage = interface(IXMLNode)
    ['{6E79F7DE-8E36-437D-A59A-16D836FF797E}']
    function GetMessage: TXMPPMessageType;
    function GetDest: string;
    function GetSrc: string;
    function GetId: string;
    procedure SetMessage(const value: TXMPPMessageType);
    procedure SetDest(const value: string);
    procedure SetSrc(const value: string);
    procedure SetId(const value: string);
    property Dest: string read GetDest write SetDest;
    property Src: string read GetSrc write SetSrc;
    property Kind: TXMPPMessageType read GetMessage write SetMessage;
    property Id: string read GetId write SetId;
  end;

  TXMPPMessage = class(TXMLNode, IXMPPPresence, IXMPPIQ, IXMPPMessage)
  protected
    function GetPresence: TXMPPPresenceType;
    function GetShow: TXMPPPresenceShow;
    function GetStatus: string;
    function GetPriority: ShortInt;
    function GetDest: string;
    function GetSrc: string;
    function GetIQ: TXMPPIQType;
    function GetId: string;
    function GetMessage: TXMPPMessageType;
    procedure SetPresence(const value: TXMPPPresenceType);
    procedure SetShow(const value: TXMPPPresenceShow);
    procedure SetStatus(const value: string);
    procedure SetPriority(const value: ShortInt);
    procedure SetDest(const value: string);
    procedure SetSrc(const value: string);
    procedure SetIQ(value: TXMPPIQType);
    procedure SetId(const value: string);
    procedure SetMessage(const value: TXMPPMessageType);
    function Reply: IXMPPIQ;
    function Error(kind: TXMPPErrorType): IXMPPIQ;
  public
    class function CreatePresence(Kind: TXMPPPresenceType = ptNone; show: TXMPPPresenceShow = psNone;
      const Status: string = ''; Priority: ShortInt = 0; const dest: string = ''; const src: string = ''): IXMPPPresence;
    class function CreateIQ(Kind: TXMPPIQType = iqGet; const dest: string = ''; const src: string = ''; const id: string = ''): IXMPPIQ;
    class function CreateMessage(kind: TXMPPMessageType; const dest: string = ''; const src: string = ''; const id: string = ''): IXMPPMessage;
  end;

  TXMPPClient = class(TInterfacedObject, IXMPPClient)
  private
    FJID: string;
    FOnError: TXMPPErrorEvent;
    FOnIQ: TXMPPIQEvent;
    FOnPresence: TXMPPEvent;
    FOnMessage: TXMPPMessageEvent;
    FOnSynchronize: TXMPPOnSynchronize;
    FReadyState: TXMPPReadyState;
    FSocket: TSocket;
    FGenId: Integer;
    FOnStateChange: TXMPPReadyStateChange;
    FLockWrite: TRTLCriticalSection;
    FLockEvents: TRTLCriticalSection;
    // SSL
    FCtx: PSSL_CTX;
    FSsl: PSSL;
    FPassword: AnsiString;
    FCertificateFile: AnsiString;
    FPrivateKeyFile: AnsiString;
    FCertCAFile: AnsiString;
    FEvents: TDictionary<Integer, TXMPPIQResponse>;
    procedure doSynchronize(const node: IXMLNode; const proc: TProc<IXMLNode>);
    function StartSSL: Boolean;
    procedure Listen(const domain, resource: string);
    function SockSend(var Buf; len, flags: Integer): Integer;
    function SockRecv(var Buf; len, flags: Integer): Integer;
    procedure SetReadyState(rs: TXMPPReadyState);
    function HostName(const user, host: string): AnsiString;
    function DomainName(const user, host: string): string;
    function UserName(const user: string): string;
    function Connect(const Host: AnsiString; Port: Word): Boolean;
    function Login(const user, domain, pass: string; options: TXMPPOptions): Boolean;
  protected
    function GetJID: string;
    function GetOnReadyStateChange: TXMPPReadyStateChange;
    procedure SetOnReadyStateChange(const cb: TXMPPReadyStateChange);
    function getReadyState: TXMPPReadyState; virtual;
    function Open(const user, pass: string;
     const resource: string = ''; const host: string = ''; port: Word = 5222;
     options: TXMPPOptions = []): Boolean;
    procedure SendFmt(const data: string; params: array of const);
    procedure Send(const data: string);
    procedure SendIQ(const IQ: IXMPPIQ; const callback: TXMPPIQResponse);
    procedure Close;
    function GetOnError: TXMPPErrorEvent;
    function GetOnIQ: TXMPPIQEvent;
    function GetOnPresence: TXMPPEvent;
    function GetOnMessage: TXMPPMessageEvent;
    procedure SetOnError(const value: TXMPPErrorEvent);
    procedure SetOnIQ(const value: TXMPPIQEvent);
    procedure SetOnPresence(const value: TXMPPEvent);
    procedure SetOnMessage(const value: TXMPPMessageEvent);
    procedure SendXML(const xml: IXMLNode);
    function GetOnSynchronize: TXMPPOnSynchronize;
    procedure SetOnSynchronize(const value: TXMPPOnSynchronize);
  public
    constructor Create(const password: AnsiString = '';
      const CertificateFile: AnsiString = ''; const PrivateKeyFile: AnsiString = '';
      const CertCAFile: AnsiString = ''); virtual;
    destructor Destroy; override;
    class constructor Create;
    class destructor Destroy;
  end;

implementation
uses
  AnsiStrings, dorUtils, dorHTTP, dorMD5, dorPunyCode;

const
  XML_STREAM_STREAM = '<stream:stream to="%s" xmlns="jabber:client" xmlns:stream="http://etherx.jabber.org/streams" version="1.0">';

function SSLPasswordCallback(buffer: PAnsiChar; size, rwflag: Integer;
  this: TXMPPClient): Integer; cdecl;
var
  password: AnsiString;
begin
  password := this.FPassword;
  if Length(password) > (Size - 1) then
    SetLength(password, Size - 1);
  Result := Length(password);
  Move(PAnsiChar(password)^, buffer^, Result + 1);
end;

procedure ParseDigest(const data: AnsiString; event: Tproc<AnsiString, AnsiString>);
type
  TState = (stStartKey, stKey, stStartValue, stQuoted, stValue);
var
  p: PAnsiChar;
  st: TState;
  key1, key2, value1: PAnsiChar;
  k: AnsiChar;
begin
  st := stStartKey;
  p := PAnsiChar(data);
  k := '"';
  key1 := nil;
  key2 := nil;
  value1 := nil;
  while True do
  begin
    case st of
      stStartKey:
        case p^ of
          ',', ' ':;
          #0: Exit;
        else
          key1 := p;
          st := stKey;
        end;
      stKey:
        case p^ of
          '=':
            begin
              key2 := p;
              st := stStartValue;
            end;
          ',', #0: Exit;
        end;
      stStartValue:
        case p^ of
          '"', '''':
            begin
              k := p^;
              value1 := p+1;
              st := stQuoted;
            end;
          #0, ',': Exit;
        else
          value1 := p;
          st := stValue;
        end;
      stQuoted:
        case p^ of
          #0: Exit;
        else
          if p^ = k then
          begin
            event(AnsiString(Copy(key1, 0, key2-key1)), AnsiString(Copy(value1, 0, p-value1)));
            st := stStartKey;
          end
        end;
      stValue:
        case p^ of
          #0:
            begin
              event(AnsiString(Copy(key1, 0, key2-key1)), AnsiString(Copy(value1, 0, p-value1)));
              Exit;
            end;
          ',':
            begin
              event(AnsiString(Copy(key1, 0, key2-key1)), AnsiString(Copy(value1, 0, p-value1)));
              st := stStartKey;
            end;
        end;
    end;
    Inc(p);
  end;
end;

function StrToHex(const Value: Ansistring): Ansistring;
var
  n: Integer;
begin
  Result := '';
  for n := 1 to Length(Value) do
    Result := Result + AnsiString(IntToHex(Byte(Value[n]), 2));
  Result := LowerCase(Result);
end;

(******************************************************************************)
(* TThreadIt                                                                  *)
(* run anonymous method in a thread                                           *)
(******************************************************************************)

type
  TThreadIt = class(TThread)
  private
    FProc: TProc;
  protected
    procedure Execute; override;
    constructor Create(const proc: TProc);
  end;

  constructor TThreadIt.Create(const proc: TProc);
  begin
    FProc := proc;
    FreeOnTerminate := True;
    inherited Create(False);
  end;

  procedure TThreadIt.Execute;
  begin
    FProc();
  end;

{ TXMPPClient }

procedure TXMPPClient.Close;
begin
  if FReadyState in [rsConnecting .. rsOpen] then
  begin
    SetReadyState(rsClosing);

    Send('</stream:stream>');
    Sleep(1); // let the socket remotely close

    closesocket(FSocket);
    Sleep(1); // let the listen thread close

    FSocket := INVALID_SOCKET;

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
    SetReadyState(rsOffline);
  end;
end;

function TXMPPClient.Open(const user, pass: string;
  const resource: string = ''; const host: string = ''; port: Word = 5222;
  options: TXMPPOptions = []): Boolean;
begin
  if FReadyState <> rsOffline then
    Exit(False);
  Close;

  if not Connect(HostName(user, host), port) then
    Exit(False);

  if not Login(UserName(user), DomainName(user, host), pass, Options) then
    Exit(False);

  TThreadIt.Create(procedure begin listen(DomainName(user, host), resource) end);

  Result := True;
end;

function Thread(const proc: TProc): THandle;
  function run(const proc: TProc): Integer; stdcall;
  begin
    proc;
    Result := 0;
  end;
begin
  CreateThread(nil, 0, @run, PPointer(@proc)^, 0, Result);
end;

function TXMPPClient.Connect(const Host: AnsiString; Port: Word): Boolean;
var
  AHost: PHostEnt;
  addr: TSockAddrIn;
  done: THandle;
  ret: Integer;
  trhandle: THandle;
begin
  SetReadyState(rsConnecting);

  // find host
  Ahost := gethostbyname(PAnsiChar(host));
  if AHost = nil then
  begin
    if Assigned(FOnError) then
      FOnError(Format('Host not found: %s', [host]));
    SetReadyState(rsOffline);
    Exit(False);
  end;

  // socket
  FSocket := socket(AF_INET, SOCK_STREAM, 0);
  if FSocket = INVALID_SOCKET then
  begin
    if Assigned(FOnError) then
      FOnError('Unexpected error: can''t allocate socket handle.');
    SetReadyState(rsOffline);
    Exit(False);
  end;

  // connect
  FillChar(addr, SizeOf(addr), 0);
  addr.sin_family := AF_INET;
  addr.sin_port := htons(port);
  addr.sin_addr.S_addr := PInteger(AHost.h_addr^)^;

  ret := 1;
  done := CreateEvent(nil, True, False, nil);
  try
    trhandle := Thread(procedure begin
      ret := WinSock.connect(FSocket, addr, SizeOf(addr));
      SetEvent(done);
    end);
    WaitForSingleObject(done, 1000);
  finally
    CloseHandle(done);
    TerminateThread(trhandle, 0);
  end;

  if ret <> 0 then
  begin
    if Assigned(FOnError) then
      FOnError(format('Cant''t connect to host: %s:%d', [host, port]));
    FSocket := INVALID_SOCKET;
    SetReadyState(rsOffline);
    Exit(False);
  end;

  SetReadyState(rsConnected);
  Result := True;
end;

class constructor TXMPPClient.Create;
var
  Data: TWSAData;
begin
  WSAStartup($0202, Data);
end;

destructor TXMPPClient.Destroy;
begin
  //FOnSynchronize := nil;
  Close;
  DeleteCriticalSection(FLockWrite);
  DeleteCriticalSection(FLockEvents);
  FEvents.Free;
  inherited;
end;

constructor TXMPPClient.Create(const password, CertificateFile,
  PrivateKeyFile, CertCAFile: AnsiString);
begin
  inherited Create;
  FGenId := 0;
  FReadyState := rsOffline;
  FSocket := INVALID_SOCKET;
  FCtx := nil;
  FSsl := nil;
  FPassword := password;
  FCertificateFile := CertificateFile;
  FPrivateKeyFile := PrivateKeyFile;
  FCertCAFile := CertCAFile;
  InitializeCriticalSection(FLockWrite);
  InitializeCriticalSection(FLockEvents);
  FEvents := TDictionary<Integer, TXMPPIQResponse>.Create;
end;

function TXMPPClient.getReadyState: TXMPPReadyState;
begin
  Result := FReadyState;
end;

function TXMPPClient.HostName(const user, host: string): AnsiString;
var
  d: PChar;
begin
  if host <> '' then Exit(PunycodeEncodeDomain(host));

  d := StrScan(PChar(user), '@');
  if d = nil then
    Exit('');

  inc(d);
  Result := PunycodeEncodeDomain(d);
end;

procedure TXMPPClient.Listen(const domain, resource: string);
const
  XML_IQ_SESSION = '<iq type="set"><session xmlns="urn:ietf:params:xml:ns:xmpp-session"/></iq>';
var
  stack: TStack<IXMLNode>;
  n: IXMLNode;
  mustreconnect: Boolean;
  ev: TFunc<Boolean>;
  doiqresult: TProc<TXMPPIQType>;
  doiqevent: TProc<TXMPPIQType>;
  domessage: TProc<TXMPPMessageType>;
  events: TDictionary<string, TFunc<Boolean>>;
label
  redo;
begin
  doiqresult :=
    procedure(resp: TXMPPIQType)
    var
      idstr: string;
      id: Integer;
      rep: TXMPPIQResponse;
    begin
      if n.Attributes.TryGetValue('id', idstr) and
        TryStrToInt(idstr, id) then
      begin
        //rep := nil;
        EnterCriticalSection(FLockEvents);
        try
          if FEvents.TryGetValue(id, rep) then
            FEvents.Remove(id);
        finally
          LeaveCriticalSection(FLockEvents);
        end;
        if Assigned(rep) then
        begin
          doSynchronize(n, procedure (node: IXMLNode) begin
            rep(node as IXMPPIQ) end);
        end;
      end;
    end;

  doiqevent :=
    procedure(event: TXMPPIQType)
    begin
      if Assigned(FOnIQ) then
        doSynchronize(n,
          procedure (node: IXMLNode) begin
            FOnIQ(node as IXMPPIQ)
          end);
    end;

  domessage :=
    procedure(typ: TXMPPMessageType)
    begin
      if Assigned(FOnMessage) then
        doSynchronize(n,
          procedure (node: IXMLNode) begin
            FOnMessage(node as IXMPPMessage)
          end);
    end;
  stack := TStack<IXMLNode>.Create;
  events := TDictionary<string, TFunc<Boolean>>.Create;
  try
    SetReadyState(rsConnecting);
    Send('<?xml version="1.0" ?>');
    SendFmt(XML_STREAM_STREAM, [domain]);
redo:
    //<<< stream:features
    events.Add('stream:features', function : Boolean
      var
        iq: IXMPPIQ;
      begin
        Result := True;
        if n.FirstChild('bind') <> nil then
        begin
           iq := TXMPPMessage.CreateIQ(iqSet);
           with iq.Append('bind') do begin
             Attributes.Add('xmlns', 'urn:ietf:params:xml:ns:xmpp-bind');
             Append('resource').Text := resource;
           end;
           SendIQ(iq, procedure(const node: IXMPPIQ) begin
             if node.Kind = iqResult then
               FJID := node.FirstChild('bind').FirstChild('jid').Text
           end);
        end;
        Sleep(1000);  // jabberd2
        if n.FirstChild('session') <> nil then
          Send(XML_IQ_SESSION);
        events.Add('presence',
          function: Boolean
          begin
            if Assigned(FOnPresence) then
            doSynchronize(n, procedure (node: IXMLNode) begin
              FOnPresence(node as IXMPPPresence) end);
            Result := True;
          end);
        events.Add('iq',
          function: Boolean
          var
            typ: string;
            e: TFunc<Boolean>;
          begin
            if n.Attributes.TryGetValue('type', typ) and
                events.TryGetValue('iq@' + typ, e) then
              Result := e else
              Result := True;
          end);
        events.Add('iq@get',
          function: Boolean
          begin
            doiqevent(iqGet);
            Result := True;
          end);
        events.Add('iq@set',
          function: Boolean
          begin
            doiqevent(iqSet);
            Result := True;
          end);
        events.Add('iq@result',
          function: Boolean begin doiqresult(iqResult); result := True end);
        events.Add('iq@error',
          function: Boolean begin doiqresult(iqError); result := True end);
        events.Add('message',
          function: Boolean
          var
            typ: string;
            e: TFunc<Boolean>;
          begin
            if n.Attributes.TryGetValue('type', typ) then
            begin
              if events.TryGetValue('message@' + typ, e) then
                Result := e else
                Result := True;
            end else
            begin
              domessage(mtNone);
              Result := True;
            end;
          end);
        events.Add('message@normal', function: Boolean
          begin
            domessage(mtNormal);
            Result := True;
          end);
        events.Add('message@chat', function: Boolean
          begin
            domessage(mtChat);
            Result := True;
          end);
        events.Add('message@groupchat', function: Boolean
          begin
            domessage(mtGroupChat);
            Result := True;
          end);
        events.Add('message@headline', function: Boolean
          begin
            domessage(mtHeadline);
            Result := True;
          end);
        events.Add('message@error', function: Boolean
          begin
            domessage(mtError);
            Result := True;
          end);
        SetReadyState(rsOpen);
        events.Remove('stream:features');
      end);

    mustreconnect := False;
    XMLParseSAX(
      function (var c: AnsiChar): Boolean
      begin
        Result := SockRecv(c, 1, 0) = 1
      end,
      function(node: TXMLNodeState; const name: RawByteString; const value: string): Boolean
      begin
        Result := True;
        case node of
          xtOpen:
            begin
{$IFDEF XMPP_DEBUG_CONSOLE}
              writeln(StringOfChar(' ', stack.Count * 3) + 'node: ' + string(name));
{$ENDIF}
              if (stack.Count = 1) then
                stack.Push(TXMPPMessage.Create(name)) else
                stack.Push(TXMLNode.Create(name));
            end;
          xtClose:
            begin
              n := stack.Pop;
              if stack.count > 1 then
                stack.Peek.ChildNodes.Add(n) else
                if events.TryGetValue(string(n.Name), ev) then
                  Result := ev else
                  Result := True;
            end;
          xtAttribute:
            begin
{$IFDEF XMPP_DEBUG_CONSOLE}
              writeln(StringOfChar(' ', stack.Count * 3) + 'attr: ' + string(name) + '=' + value);
{$ENDIF}
              stack.Peek.Attributes.AddOrSetValue(name, value);
            end;
          xtText:
            begin
{$IFDEF XMPP_DEBUG_CONSOLE}
              writeln(StringOfChar(' ', stack.Count * 3) + 'text: ' + value);
{$ENDIF}
              stack.Peek.ChildNodes.Add(TXMLNodeText.Create(value));
            end;
          xtCData:
            begin
{$IFDEF XMPP_DEBUG_CONSOLE}
              writeln(StringOfChar(' ', stack.Count * 3) + 'cdata: ' + value);
{$ENDIF}
              stack.Peek.ChildNodes.Add(TXMLNodeCDATA.Create(value));
            end;
        end;
      end, CP_UTF8);
    if mustreconnect then
      goto redo;
  finally
    stack.Free;
    events.Free;

    // compiler bug: anonymous method not released :/
    doiqresult := nil;
    doiqevent  := nil;
    domessage   := nil;
    ev := nil;
  end;

  if FReadyState in [rsConnecting .. rsOpen] then // remotely closed
    doSynchronize(nil, procedure (node: IXMLNode) begin
      Close;
    end);
end;

function TXMPPClient.Login(const user, domain, pass: string; options: TXMPPOptions): Boolean;
const
  XML_STARTTLS = '<starttls xmlns="%s"/>';
  XML_AUTH_PLAIN = '<auth xmlns="urn:ietf:params:xml:ns:xmpp-sasl" mechanism="PLAIN" xmlns:ga="http://www.google.com/talk/protocol/auth" ga:client-uses-full-bind-result="true">%s</auth>';
  XML_AUTH_MD5 = '<auth xmlns="urn:ietf:params:xml:ns:xmpp-sasl" mechanism="DIGEST-MD5" xmlns:ga="http://www.google.com/talk/protocol/auth" ga:client-uses-full-bind-result="true"/>';
var
  stack: TStack<IXMLNode>;
  n, StreamFeatures: IXMLNode;
  mustreconnect: Boolean;
  ev, dologin: TFunc<Boolean>;
  reconnect: TProc;
  events: TDictionary<string, TFunc<Boolean>>;
  authenticated: Boolean;
label
  redo;
begin
  SetReadyState(rsAuthenticating);
  authenticated := False;
  reconnect := procedure begin
    events.Clear;
    stack.Clear;
    SendFmt(XML_STREAM_STREAM, [domain]);
    mustreconnect := True;
  end;

  dologin := function: Boolean
  var
    anode, mechanism: IXMLNode;
  begin
    anode := StreamFeatures.FirstChild('mechanisms');
    if anode <> nil then
    begin
      Result := False;
      for mechanism in anode.ChildNodes do
        if ((FSsl <> nil) or (xoPlaintextAuth in options)) and (mechanism.Text = 'PLAIN') then
        begin
          SendFmt(XML_AUTH_PLAIN, [StrTobase64(#0+user+#0+pass)]);
          Result := True;
          Break;
        end else
        if mechanism.Text = 'DIGEST-MD5' then
        begin
          Send(XML_AUTH_MD5);
          events.Add('challenge', function: Boolean
            var
              dic: TDictionary<AnsiString, AnsiString>;
              g: TGUID;
              a1, a2: AnsiString;
              cnonce, rspauth, realm: AnsiString;
              response: AnsiString;
            begin
              Result := True;
              dic := TDictionary<AnsiString, AnsiString>.Create;
              try
                ParseDigest(AnsiString(Base64ToStr(n.Text)),
                  procedure(key, value: AnsiString)
                    begin dic.AddOrSetValue(key, value) end);

                if dic.TryGetValue('rspauth', rspauth) then
                begin
                  if rspauth = AnsiString(stack.Peek.Attributes['<expected>'])  then
                  begin
                    Send('<response xmlns="urn:ietf:params:xml:ns:xmpp-sasl"/>');
                    stack.Peek.Attributes.Remove('<expected>');
                    events.Remove('challenge');
                  end else
                    Exit(False);
                end else
                begin
                  CreateGUID(g);
                  cnonce := AnsiString(GUIDToString(g));

                  if not dic.TryGetValue('realm', realm) then
                    realm := AnsiString(domain);

                  a1 := md5(AnsiString(user) + ':' + realm + ':' + AnsiString(pass))
                    + ':' + dic['nonce'] + ':' + cnonce;
                  a2 := 'AUTHENTICATE:xmpp/' + realm;
                  response := strtohex(md5(strtohex(md5(a1)) + ':' + dic['nonce'] +
                    ':00000001:' + cnonce + ':auth:' + strtohex(md5(a2))));

                  a2 := ':xmpp/' + realm;
                  stack.Peek.Attributes.AddOrSetValue('<expected>',
                    string(strtohex(md5(strtohex(md5(a1))+':' + dic['nonce'] +
                    ':00000001:' + cnonce + ':auth:' + strtohex(md5(a2))))));

                  Send(format('<response xmlns="urn:ietf:params:xml:ns:xmpp-sasl">%s</response>',[
                    StrTobase64(format('username="%s",realm="%s",nonce="%s",cnonce="%s",'+
                      'nc=00000001,qop=auth,digest-uri="xmpp/%s",response=%s,charset=utf-8', [
                      AnsiString(user), realm, dic['nonce'], cnonce, realm, response]))]));
                end;
              finally
                dic.Free;
              end;
            end);
          Result := True;
          Break;
        end;

      if Result then
      begin
        //>>> success
        events.Add('success', function: Boolean
          begin
            authenticated := True;
            Result := False;
          end);
        //>>> failure
        events.Add('failure', function: Boolean
        var
          iq: IXMPPIQ;
        begin
          if (xoRegister in options) then //and (StreamFeatures.FirstChild('register') <> nil) then
          begin
            iq := TXMPPMessage.CreateIQ(iqSet, domain);
            with iq.Append('query') do
            begin
              Attributes.Add('xmlns', 'jabber:iq:register');
              Append('username').Text := user;
              Append('password').Text := pass;
            end;
            SendXML(iq);
            events.Clear;
            events.Add('iq', function: Boolean begin
                if n.Attributes['type'] = 'result' then
                begin
                  events.Clear;
                  Result := dologin
                end else
                  Result := False;
              end);
            Result := True;
          end else
            Result := False
        end);
      end;
    end else
      Send('</stream:stream>'); // no need to authenticate
    Result := True;
  end;

  stack := TStack<IXMLNode>.Create;
  events := TDictionary<string, TFunc<Boolean>>.Create;
  try
    Send('<?xml version="1.0" ?>');
    SendFmt(XML_STREAM_STREAM, [domain]);
redo:
    //<<< stream:features
    events.Add('stream:features', function : Boolean
      var
        ANode: IXMLNode;
      begin
        Result := True;
        StreamFeatures := n;
        anode := n.FirstChild('starttls');
        if (anode <> nil) and (not (xoDontForceEncryption in options) or (anode.FirstChild('required') <> nil)) then
        begin
          //>>> starttls
          SendFmt(XML_STARTTLS, [anode.Attributes['xmlns']]);
          //<<< proceed
          events.Add('proceed', function : Boolean
          begin
            if not StartSSl then
              Exit(False);
            reconnect;
            Result := False;
          end);
          //<<< failure
          events.Add('failure', function : Boolean begin Result := False end);
        end else
          Result := dologin;
        events.Remove('stream:features');
      end);

    mustreconnect := False;
    XMLParseSAX(
      function (var c: AnsiChar): Boolean
      begin
        Result := SockRecv(c, 1, 0) = 1
      end,
      function(node: TXMLNodeState; const name: RawByteString; const value: string): Boolean
      begin
        Result := True;
        case node of
          xtOpen:
            begin
{$IFDEF XMPP_DEBUG_CONSOLE}
              writeln(StringOfChar(' ', stack.Count * 3) + 'node: ' + string(name));
{$ENDIF}
              if (stack.Count = 1) then
                stack.Push(TXMPPMessage.Create(name)) else
                stack.Push(TXMLNode.Create(name));
            end;
          xtClose:
            begin
              n := stack.Pop;
              if stack.count > 1 then
                stack.Peek.ChildNodes.Add(n) else
                if events.TryGetValue(string(n.Name), ev) then
                  Result := ev else
                  Result := True;
            end;
          xtAttribute:
            begin
{$IFDEF XMPP_DEBUG_CONSOLE}
              writeln(StringOfChar(' ', stack.Count * 3) + 'attr: ' + string(name) + '=' + value);
{$ENDIF}
              stack.Peek.Attributes.AddOrSetValue(name, value);
            end;
          xtText:
            begin
{$IFDEF XMPP_DEBUG_CONSOLE}
              writeln(StringOfChar(' ', stack.Count * 3) + 'text: ' + value);
{$ENDIF}
              stack.Peek.ChildNodes.Add(TXMLNodeText.Create(value));
            end;
          xtCData:
            begin
{$IFDEF XMPP_DEBUG_CONSOLE}
              writeln(StringOfChar(' ', stack.Count * 3) + 'cdata: ' + value);
{$ENDIF}
              stack.Peek.ChildNodes.Add(TXMLNodeCDATA.Create(value));
            end;
        end;
      end, CP_UTF8);
    if mustreconnect then
      goto redo;
  finally
    stack.Free;
    events.Free;

    // compiler bug: anonymous method not released :/
    reconnect := nil;
    ev := nil;
    dologin := nil;
  end;
  Result := authenticated;
  if Result then
    SetReadyState(rsAuthenticated);
end;

procedure TXMPPClient.SetOnError(const value: TXMPPErrorEvent);
begin
  FOnError := value;
end;

procedure TXMPPClient.SetOnIQ(const value: TXMPPIQEvent);
begin
  FOnIQ := value;
end;

procedure TXMPPClient.SetOnMessage(const value: TXMPPMessageEvent);
begin
  FOnMessage := value;
end;

procedure TXMPPClient.SetOnPresence(const value: TXMPPEvent);
begin
  FOnPresence := value;
end;

procedure TXMPPClient.SetOnReadyStateChange(const cb: TXMPPReadyStateChange);
begin
  FOnStateChange := cb;
end;

procedure TXMPPClient.SetOnSynchronize(const value: TXMPPOnSynchronize);
begin
  FOnSynchronize := value;
end;

procedure TXMPPClient.SetReadyState(rs: TXMPPReadyState);
begin
  FReadyState := rs;
  if Assigned(FOnStateChange) then
    doSynchronize(nil, procedure (node: IXMLNode)
      begin FOnStateChange(Self) end);
end;

function TXMPPClient.SockSend(var Buf; len, flags: Integer): Integer;
begin
  if FSsl <> nil then
    Result := SSL_write(FSsl, @Buf, len) else
    Result := WinSock.send(FSocket, Buf, len, flags);
end;

function TXMPPClient.StartSSL: Boolean;
begin
  Result := False;

  FCtx := SSL_CTX_new(TLSv1_method);
  SSL_CTX_set_cipher_list(FCtx, 'DEFAULT');

  SSL_CTX_set_default_passwd_cb_userdata(FCtx, Self);
  SSL_CTX_set_default_passwd_cb(FCtx, @SSLPasswordCallback);

  if FCertificateFile <> '' then
    if SSL_CTX_use_certificate_chain_file(FCtx, PAnsiChar(FCertificateFile)) <> 1 then
      if SSL_CTX_use_certificate_file(FCtx, PAnsiChar(FCertificateFile), SSL_FILETYPE_PEM) <> 1 then
        if SSL_CTX_use_certificate_file(FCtx, PAnsiChar(FCertificateFile), SSL_FILETYPE_ASN1) <> 1 then
        begin
          if Assigned(FOnError) then
            FOnError('SSL: Can''t use certificate');
          Exit;
        end;

  if FPrivateKeyFile <> '' then
    if SSL_CTX_use_RSAPrivateKey_file(FCtx, PAnsiChar(FPrivateKeyFile), SSL_FILETYPE_PEM) <> 1 then
      if SSL_CTX_use_RSAPrivateKey_file(FCtx, PAnsiChar(FPrivateKeyFile), SSL_FILETYPE_ASN1) <> 1 then
      begin
        if Assigned(FOnError) then
          FOnError('SSL: Can''t use key file');
        Exit;
      end;

  if FCertCAFile <> '' then
    if SSL_CTX_load_verify_locations(FCtx, PAnsiChar(FCertCAFile), nil) <> 1 then
    begin
      if Assigned(FOnError) then
        FOnError('SSL: Can''t use CA Cert');
      Exit;
    end;

  FSsl := SSL_new(FCtx);
  SSL_set_fd(FSsl, FSocket);
  if SSL_connect(FSsl) <> 1 then
  begin
    if Assigned(FOnError) then
      FOnError('SSL: connection error');
    Exit;
  end;
  Result := True;
end;

function TXMPPClient.UserName(const user: string): string;
var
  d, p: PChar;
begin
  p := PChar(user);
  d := StrScan(p, '@');
  if d = nil then
    Exit(user) else
    SetString(Result, p, d - p);
end;

procedure TXMPPClient.SendFmt(const data: string; params: array of const);
begin
  Send(Format(data, params));
end;

procedure TXMPPClient.SendIQ(const IQ: IXMPPIQ; const callback: TXMPPIQResponse);
var
  id: Integer;
  cb: TXMPPIQResponse;
begin
  id := InterlockedIncrement(FGenId);
  IQ.NullAttr['id'] := IntToStr(id);
  SendXML(IQ);
  if Assigned(callback) then
  begin
    cb := callback;
    EnterCriticalSection(FLockEvents);
    try
      FEvents.Add(id, cb);
    finally
      LeaveCriticalSection(FLockEvents);
    end;
  end;
end;

procedure TXMPPClient.SendXML(const xml: IXMLNode);
begin
  EnterCriticalSection(FLockWrite);
  try
    xml.SaveToXML(
      procedure(const data: string)
        begin
          //write(data);
          Send(data);
        end);
  finally
    LeaveCriticalSection(FLockWrite);
  end;
end;

procedure TXMPPClient.Send(const data: string);
var
  rb: UTF8String;
begin
  EnterCriticalSection(FLockWrite);
  try
    rb := UTF8String(data);
    SockSend(PAnsiChar(rb)^, Length(rb), 0);
  finally
    LeaveCriticalSection(FLockWrite);
  end;
end;

function TXMPPClient.SockRecv(var Buf; len, flags: Integer): Integer;
begin
  if FSsl <> nil then
    try
      Result := SSL_read(FSsl, @Buf, len)
    except
      // sometime openssl can raise an AV exception error on closing socket.
      Result := 0;
    end else
    Result := recv(FSocket, Buf, len, flags);
end;

class destructor TXMPPClient.Destroy;
begin
  WSACleanup;
end;

function TXMPPClient.DomainName(const user, host: string): string;
var
  d: PChar;
begin
  d := StrScan(PChar(user), '@');
  if d = nil then
    Exit(host);

  inc(d);
  Result := d;
end;

procedure TXMPPClient.doSynchronize(const node: IXMLNode; const proc: TProc<IXMLNode>);
begin
  if Assigned(FOnSynchronize) then
    FOnSynchronize(node, proc) else
    TThread.Synchronize(nil, procedure begin proc(node) end);
end;

function TXMPPClient.GetJID: string;
begin
  Result := FJID;
end;

function TXMPPClient.GetOnError: TXMPPErrorEvent;
begin
  Result := FOnError;
end;

function TXMPPClient.GetOnIQ: TXMPPIQEvent;
begin
  Result := FOnIQ;
end;

function TXMPPClient.GetOnMessage: TXMPPMessageEvent;
begin
  Result := FonMessage;
end;

function TXMPPClient.GetOnPresence: TXMPPEvent;
begin
  Result := FOnPresence;
end;

function TXMPPClient.GetOnReadyStateChange: TXMPPReadyStateChange;
begin
  Result := FOnStateChange;
end;

function TXMPPClient.GetOnSynchronize: TXMPPOnSynchronize;
begin
  Result := FOnSynchronize;
end;

{ TXMPPMessage }

class function TXMPPMessage.CreateIQ(Kind: TXMPPIQType; const dest, src, id: string): IXMPPIQ;
begin
  Result := Create('iq');
  Result.kind := Kind;
  Result.Dest := dest;
  Result.Src := src;
  Result.Id := id;
end;

class function TXMPPMessage.CreateMessage(kind: TXMPPMessageType; const dest,
  src, id: string): IXMPPMessage;
begin
  Result := Create('message');
  Result.Dest := dest;
  Result.Src := src;
  Result.Id := id;
end;

class function TXMPPMessage.CreatePresence(Kind: TXMPPPresenceType;
  show: TXMPPPresenceShow; const Status: string; Priority: ShortInt;
  const dest, src: string): IXMPPPresence;
begin
  Result := Create('presence');
  // attributes
  Result.Kind := Kind;
  Result.Src := src;
  Result.Dest := dest;

  // nodes
  Result.Show := Show;
  Result.Status := Status;
  Result.Priority := Priority;
end;

function TXMPPMessage.Error(kind: TXMPPErrorType): IXMPPIQ;
const
  kindstr: array[TXMPPErrorType] of string =
    ('auth', 'cancel', 'continue', 'modify', 'wait');
begin
  Result := TXMPPMessage.CreateIQ(iqError, GetSrc, GetDest, GetId);
  Result.Append('error').Attributes.Add('type', kindstr[kind]);
end;

function TXMPPMessage.GetDest: string;
begin
  Result := NullAttr['to'];
end;

function TXMPPMessage.GetId: string;
begin
  Result := NullAttr['id'];
end;

function TXMPPMessage.GetIQ: TXMPPIQType;
var
  iq: string;
begin
  if HasAttributes and Attributes.TryGetValue('type', iq) then
    case Length(iq) of
      3: case iq[1] of
           'g': if SameStr('get', iq) then Exit(iqGet);
           's': if SameStr('set', iq) then Exit(iqSet);
         end;
      5: if SameStr('error', iq) then Exit(iqError);
      6: if SameStr('result', iq) then Exit(iqResult);
    end;
  raise Exception.CreateFmt('Invalid iq.type=%', [iq]);
end;

function TXMPPMessage.GetMessage: TXMPPMessageType;
var
  msg: string;
begin
  if HasAttributes and Attributes.TryGetValue('type', msg) then
    case Length(msg) of
      4: if SameStr(msg, 'chat') then Exit(mtChat);
      5: if SameStr(msg, 'error') then Exit(mtError);
      6: if SameStr(msg, 'normal') then Exit(mtNormal);
      8: if SameStr(msg, 'headline') then Exit(mtHeadline);
      9: if SameStr(msg, 'groupchat') then Exit(mtGroupChat);
    end;
  Result := mtNone;
end;

function TXMPPMessage.GetPresence: TXMPPPresenceType;
var
  presence: string;
begin
  if HasAttributes and Attributes.TryGetValue('type', presence) then
    case Length(presence) of
      5:
        case presence[1] of
          'p': if SameStr(presence, 'probe') then Exit(ptProbe);
          'e': if SameStr(presence, 'error') then Exit(ptError);
        end;
      9:  if SameStr(presence, 'subscribe') then Exit(ptSubscribe);
      10: if SameStr(presence, 'subscribed') then Exit(ptSubscribed);
      11:
        case presence[3] of
          'a': if SameStr(presence, 'unavailable') then Exit(ptUnavailable);
          's': if SameStr(presence, 'unsubscribe') then Exit(ptUnsubscribe);
        end;
      12: if SameStr(presence, 'unsubscribed') then Exit(ptUnsubscribed);
    end;
  Result := ptNone;
end;

function TXMPPMessage.GetPriority: ShortInt;
var
  ret: Integer;
begin
   if TryStrToInt(NullChild['priority'], ret) then
     Result := ret else
     Result := 0;
end;

function TXMPPMessage.GetShow: TXMPPPresenceShow;
var
  sh: string;
begin
  sh := NullChild['show'];
  case Length(sh) of
    2: if SameStr(sh, 'xa') then Exit(psXa);
    3: if SameStr(sh, 'dnd') then Exit(psDnd);
    4: case sh[1] of
         'a': if SameStr(sh, 'away') then Exit(psAway);
         'c': if SameStr(sh, 'chat') then Exit(psChat);
       end;
  end;
  Result := psNone;
end;

function TXMPPMessage.GetSrc: string;
begin
  Result := NullAttr['from'];
end;

function TXMPPMessage.GetStatus: string;
begin
  Result := NullChild['status']
end;

function TXMPPMessage.Reply: IXMPPIQ;
var
  n, n2: IXMLNode;
  xmlns: string;
begin
  Result := TXMPPMessage.CreateIQ(iqResult, GetSrc, GetDest, GetId);
  if HasChildNodes then
  begin
    n := ChildNodes.First;
    if (n <> nil) and n.Attributes.TryGetValue('xmlns', xmlns) then
    begin
      n2 := TXMLNode.Create(n.Name);
      n2.Attributes.Add('xmlns', xmlns);
      Result.ChildNodes.Add(n2);
    end;
  end;
end;

procedure TXMPPMessage.SetDest(const value: string);
begin
  NullAttr['to'] := value;
end;

procedure TXMPPMessage.SetId(const value: string);
begin
  NullAttr['id'] := value;
end;

procedure TXMPPMessage.SetIQ(value: TXMPPIQType);
const
  iqs: array[TXMPPIQType] of string = ('get', 'set', 'result', 'error');
begin
  NullAttr['type'] := iqs[value];
end;

procedure TXMPPMessage.SetMessage(const value: TXMPPMessageType);
const
  msgs: array[TXMPPMessageType] of string = ('',
    'normal', 'chat', 'groupchat', 'headline', 'error');
begin
  NullAttr['type'] := msgs[value];
end;

procedure TXMPPMessage.SetPresence(const value: TXMPPPresenceType);
const
  presences: array[TXMPPPresenceType] of string = ('', 'unavailable',
    'subscribe', 'subscribed', 'unsubscribe', 'unsubscribed', 'probe', 'error');
begin
  NullAttr['type'] := presences[value];
end;

procedure TXMPPMessage.SetPriority(const value: ShortInt);
begin
  if value <> 0 then
    NullChild['priority'] := IntToStr(value) else
    NullChild['priority'] := '';
end;

procedure TXMPPMessage.SetShow(const value: TXMPPPresenceShow);
const
  psh: array[TXMPPPresenceShow] of string = ('', 'away', 'chat', 'dnd', 'xa');
begin
  NullChild['show'] := psh[value];
end;

procedure TXMPPMessage.SetSrc(const value: string);
begin
  NullAttr['from'] := value;
end;

procedure TXMPPMessage.SetStatus(const value: string);
begin
  NullChild['status'] := value;
end;

end.


