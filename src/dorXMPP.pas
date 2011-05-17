unit dorXMPP;

(*******************************************************************************
  Sample usage:

  xmpp := TXMPPClient.Create;
  xmpp.OnReadyStateChange := procedure(const sender: IXMPPClient)
    begin
      if Sender.ReadyState = rsOpen then
        Sender.Send('<presence/>');
    end;
  xmpp.OnEvent := function (const sender: IXMPPClient; const node: IXMLNode): Boolean
    begin
      if (node.Name = 'iq') and (node.FindChild('ping') <> nil) then
           sender.SendFmt('<iq from="%s" to="%s" id="%s" type="result"/>',
                  [node.Attr['to'], node.Attr['from'], node.Attr['id']]);
      Result := True;
    end;
  xmpp.Open('xmpp://talk.google.com', 'username', 'gmail.com', 'password');
*)

interface
uses SysUtils, Windows, WinSock, dorXML, dorOpenSSL, Generics.Collections;

type
  IXMPPClient = interface;

  TIQType = (iqGet, iqSet);
  TIQResponse = (iqResult, iqError);

  TXMPPMessage = reference to procedure(const msg: string);
  TXMPPIQEvent = reference to procedure(const sender: IXMPPClient; action: TIQType; const node: IXMLNode);
  TXMPPIQResponse = reference to procedure(const sender: IXMPPClient; result: TIQResponse; const node: IXMLNode);
  TXMPPReadyState = (rsOffline, rsConnecting, rsOpen, rsClosing);
  TXMPPReadyStateChange = reference to procedure(const xmpp: IXMPPClient);
  TXMPPOption = (xoDontForceEncryption, xoPlaintextAuth);
  TXMPPOptions = set of TXMPPOption;

  IXMPPClient = interface
    ['{FD2264FF-460E-4DCD-BA8B-D1D7BCB45E6A}']
    function GetReadyState: TXMPPReadyState;
    procedure Send(const data: string);
    procedure SendFmt(const data: string; params: array of const);
    procedure SendIQ(action: TIQType; const dest, data: string; const callback: TXMPPIQResponse);
    procedure Close;
    procedure Open(const url, user, domain, pass: string;
      const resource: string = ''; options: TXMPPOptions = []);
    function GetOnReadyStateChange: TXMPPReadyStateChange;
    procedure SetOnReadyStateChange(const cb: TXMPPReadyStateChange);

    function GetOnError: TXMPPMessage;
    function GetOnIQ: TXMPPIQEvent;

    procedure SetOnError(const value: TXMPPMessage);
    procedure SetOnIQ(const value: TXMPPIQEvent);

    property OnError: TXMPPMessage read GetOnError write SetOnError;
    property OnIQ: TXMPPIQEvent read GetOnIQ write SetOnIQ;

    property ReadyState: TXMPPReadyState read getReadyState;
    property OnReadyStateChange: TXMPPReadyStateChange read GetOnReadyStateChange write SetOnReadyStateChange;
  end;

  TXMPPClient = class(TInterfacedObject, IXMPPClient)
  private
    FOnError: TXMPPMessage;
    FOnIQ: TXMPPIQEvent;
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
    function StartSSL: Boolean;
    procedure Listen(const user, domain, pass, resource: string; options: TXMPPOptions);
    function SockSend(var Buf; len, flags: Integer): Integer;
    function SockRecv(var Buf; len, flags: Integer): Integer;
    procedure SetReadyState(rs: TXMPPReadyState);
  protected
    function GetOnReadyStateChange: TXMPPReadyStateChange;
    procedure SetOnReadyStateChange(const cb: TXMPPReadyStateChange);
    function getReadyState: TXMPPReadyState; virtual;
    procedure Open(const url, user, domain, pass, resource: string;
      options: TXMPPOptions);
    procedure SendFmt(const data: string; params: array of const);
    procedure Send(const data: string);
    procedure SendIQ(action: TIQType; const dest, data: string; const callback: TXMPPIQResponse);
    procedure Close;
    function GetOnError: TXMPPMessage;
    function GetOnIQ: TXMPPIQEvent;
    procedure SetOnError(const value: TXMPPMessage);
    procedure SetOnIQ(const value: TXMPPIQEvent);
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
  Classes, AnsiStrings, dorUtils, dorHTTP, dorMD5;

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
    FreeOnTerminate := True;
    FProc := proc;
    inherited Create(False);
  end;

  procedure TThreadIt.Execute;
  begin
    FProc();
  end;

{ TXMPPClient }

procedure TXMPPClient.Close;
begin
  if FReadyState > rsOffline then
  begin
    SetReadyState(rsClosing);
    Send('</stream:stream>');
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

procedure TXMPPClient.Open(const url, user, domain, pass, resource: string; options: TXMPPOptions);
var
  dom: AnsiString;
  protocol: string;
  uri: RawByteString;
  port: Word;
  host: PHostEnt;
  addr: TSockAddrIn;
begin
  if FReadyState <> rsOffline then
    Exit;

  SetReadyState(rsConnecting);
  // parse
  if not HTTPParseURL(PChar(url), protocol, dom, port, uri) then
  begin
    if Assigned(FOnError) then
      FOnError(Format('Can''t parse url: %s', [url]));
    Exit;
  end;

  if protocol = 'xmpp' then
  begin
    if port = 0 then
      port := 5222;
  end else
    Exit;

  // find host
  host := gethostbyname(PAnsiChar(dom));
  if host = nil then
  begin
    if Assigned(FOnError) then
      FOnError(Format('Host not found: %s', [dom]));
    Exit;
  end;

  // socket
  FSocket := socket(AF_INET, SOCK_STREAM, 0);
  if FSocket = INVALID_SOCKET then
  begin
    if Assigned(FOnError) then
      FOnError('Unexpected error: can''t allocate socket handle.');
    Exit;
  end;

  // connect
  FillChar(addr, SizeOf(addr), 0);
  addr.sin_family := AF_INET;
  addr.sin_port := htons(port);
  addr.sin_addr.S_addr := PInteger(host.h_addr^)^;
  if connect(FSocket, addr, SizeOf(addr)) <> 0 then
  begin
    if Assigned(FOnError) then
      FOnError(format('Cant''t connect to host: %s:%d', [domain, port]));
    Exit;
  end;

  TThreadIt.Create(procedure begin listen(user, domain, pass, resource, Options) end);
end;

class constructor TXMPPClient.Create;
var
  Data: TWSAData;
begin
  WSAStartup($0202, Data);
end;

destructor TXMPPClient.Destroy;
begin
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

procedure TXMPPClient.Listen(const user, domain, pass, resource: string; options: TXMPPOptions);
const
  XML_STREAM_STREAM = '<stream:stream to="%s" xmlns="jabber:client" xmlns:stream="http://etherx.jabber.org/streams" version="1.0">';
  XML_STARTTLS = '<starttls xmlns="%s"/>';
  XML_AUTH_PLAIN = '<auth xmlns="urn:ietf:params:xml:ns:xmpp-sasl" mechanism="PLAIN" xmlns:ga="http://www.google.com/talk/protocol/auth" ga:client-uses-full-bind-result="true">%s</auth>';
  XML_AUTH_MD5 = '<auth xmlns="urn:ietf:params:xml:ns:xmpp-sasl" mechanism="DIGEST-MD5" xmlns:ga="http://www.google.com/talk/protocol/auth" ga:client-uses-full-bind-result="true"/>';
  XML_IQ_BIND = '<iq type="set"><bind xmlns="urn:ietf:params:xml:ns:xmpp-bind"><resource>%s</resource></bind></iq>';
  XML_IQ_SESSION = '<iq type="set"><session xmlns="urn:ietf:params:xml:ns:xmpp-session"/></iq>';
var
  stack: TStack<IXMLNode>;
  n: IXMLNode;
  mustreconnect: Boolean;
  ev: TFunc<Boolean>;
  reconnect: TProc;
  events: TDictionary<string, TFunc<Boolean>>;
label
  redo;
begin
  reconnect := procedure begin
    events.Clear;
    stack.Clear;
    SendFmt(XML_STREAM_STREAM, [domain]);
    mustreconnect := True;
  end;;

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
        anode, mechanism: IXMLNode;
      begin
        Result := True;
        anode := n.FindChild('starttls');
        if (anode <> nil) and (not (xoDontForceEncryption in options) or (anode.FindChild('required') <> nil)) then
        begin
          //>>> starttls
          SendFmt(XML_STARTTLS, [anode.Attr['xmlns']]);
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
        begin
          anode := n.FindChild('mechanisms');
          if anode <> nil then
          begin
            Result := False;
            for mechanism in anode.Children do
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
                        if rspauth = AnsiString(stack.Peek.Attr['<expected>'])  then
                        begin
                          Send('<response xmlns="urn:ietf:params:xml:ns:xmpp-sasl"/>');
                          stack.Peek.Attr.Remove('<expected>');
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
                        stack.Peek.Attr.Add('<expected>',
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
                  reconnect;
                  Result := False;
                end);
              //>>> failure
              events.Add('failure', function: Boolean begin
                Result := False
              end);
            end;
          end else
          begin
            writeln(events.Count);
            if n.FindChild('bind') <> nil then SendFmt(XML_IQ_BIND, [resource]);
            if n.FindChild('session') <> nil then Send(XML_IQ_SESSION);
            events.Add('iq',
              function: Boolean
              var
                typ: string;
                e: TFunc<Boolean>;
              begin
                if n.Attr.TryGetValue('type', typ) and
                    events.TryGetValue('iq@' + typ, e) then
                  Result := e else
                  Result := True;
              end);
            events.Add('iq@get',
              function: Boolean
              begin
                if Assigned(FOnIQ) then
                  TThread.Synchronize(nil, procedure
                    begin FOnIQ(Self, iqGet, n) end);
                Result := True;
              end);
            events.Add('iq@set',
              function: Boolean
              begin
                if Assigned(FOnIQ) then
                  TThread.Synchronize(nil, procedure
                    begin FOnIQ(Self, iqSet, n) end);
                Result := True;
              end);
            events.Add('iq@result',
              function: Boolean
              var
                idstr: string;
                id: Integer;
                rep: TXMPPIQResponse;
              begin
                if n.Attr.TryGetValue('id', idstr) and
                  TryStrToInt(idstr, id) then
                begin
                  rep := nil;
                  EnterCriticalSection(FLockEvents);
                  try
                    if FEvents.TryGetValue(id, rep) then
                      FEvents.Remove(id);
                  finally
                    LeaveCriticalSection(FLockEvents);
                  end;
                  if Assigned(rep) then
                    TThread.Synchronize(nil, procedure begin
                      rep(Self, iqResult, n) end);
                end;
                Result := True;
              end);
            events.Add('iq@error',
              function: Boolean
              var
                idstr: string;
                id: Integer;
                rep: TXMPPIQResponse;
              begin
                if n.Attr.TryGetValue('id', idstr) and
                  TryStrToInt(idstr, id) then
                begin
                  rep := nil;
                  EnterCriticalSection(FLockEvents);
                  try
                    if FEvents.TryGetValue(id, rep) then
                      FEvents.Remove(id);
                  finally
                    LeaveCriticalSection(FLockEvents);
                  end;
                  if Assigned(rep) then
                    TThread.Synchronize(nil, procedure begin
                      rep(Self, iqError, n) end);
                end;
                Result := True;
              end);
            SetReadyState(rsOpen);
          end;
        end;
        events.Remove('stream:features');
      end);

    mustreconnect := False;
    XMLParseSAX(CP_UTF8,
      function (var c: AnsiChar): Boolean
      begin
        Result := SockRecv(c, 1, 0) = 1
      end,
      function(node: TXMLNodeType; const name, value: string): Boolean
      begin
        Result := True;
        case node of
          xtOpen:
            begin
{$IFDEF XMPP_DEBUG_CONSOLE}
              writeln(StringOfChar(' ', stack.Count * 3) + 'node: ' + name);
{$ENDIF}
              stack.Push(TXMLNode.Create(name));
            end;
          xtClose:
            begin
              n := stack.Pop;
              if stack.count > 1 then
                stack.Peek.Children.Add(n) else
                if events.TryGetValue(n.Name, ev) then
                  Result := ev else
//                    if Assigned(FOnIQ) then
//                    begin
//                      TThread.Synchronize(nil, procedure begin _ret_ := FOnIQ(Self, n) end);
//                      Result := _ret_;
//                    end else
                  Result := True;
            end;
          xtAttribute:
            begin
{$IFDEF XMPP_DEBUG_CONSOLE}
              writeln(StringOfChar(' ', stack.Count * 3) + 'attr: ' + name + '=' + value);
{$ENDIF}
              stack.Peek.Attr.AddOrSetValue(name, value);
            end;
          xtText:
            begin
{$IFDEF XMPP_DEBUG_CONSOLE}
              writeln(StringOfChar(' ', stack.Count * 3) + 'text: ' + value);
{$ENDIF}
              with stack.Peek do
                Text := value;
            end;
        end;
      end);
    if mustreconnect then
      goto redo;
  finally
    stack.Free;
    events.Free;
  end;


  if FReadyState > rsOffline then // remotely closed
    TThread.Synchronize(nil, procedure begin
      Close;
    end);
end;

procedure TXMPPClient.SetOnError(const value: TXMPPMessage);
begin
  FOnError := value;
end;

procedure TXMPPClient.SetOnIQ(const value: TXMPPIQEvent);
begin
  FOnIQ := value;
end;

procedure TXMPPClient.SetOnReadyStateChange(const cb: TXMPPReadyStateChange);
begin
  FOnStateChange := cb;
end;

procedure TXMPPClient.SetReadyState(rs: TXMPPReadyState);
begin
  FReadyState := rs;
  if Assigned(FOnStateChange) then
    TThread.Synchronize(nil, procedure begin FOnStateChange(Self) end);
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

procedure TXMPPClient.SendFmt(const data: string; params: array of const);
begin
  Send(Format(data, params));
end;

procedure TXMPPClient.SendIQ(action: TIQType; const dest, data: string; const callback: TXMPPIQResponse);
const
  Typ: array[TIQType] of string = ('get', 'set');
var
  req: string;
  id: Integer;
begin
  id := InterlockedIncrement(FGenId);
  req := Format('<iq type="%s" id="%d"', [Typ[action], id]);
  if dest <> '' then
    req := req + ' to="' + dest + '"';
  req := req + '>' + data + '</iq>';

  if Assigned(callback) then
  begin
    EnterCriticalSection(FLockEvents);
    try
      FEvents.Add(id, callback);
    finally
      LeaveCriticalSection(FLockEvents);
    end;
  end;
  Send(req);
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
    Result := SSL_read(FSsl, @Buf, len) else
    Result := recv(FSocket, Buf, len, flags);
end;

class destructor TXMPPClient.Destroy;
begin
  WSACleanup;
end;

function TXMPPClient.GetOnError: TXMPPMessage;
begin
  Result := FOnError;
end;

function TXMPPClient.GetOnIQ: TXMPPIQEvent;
begin
  Result := FOnIQ;
end;

function TXMPPClient.GetOnReadyStateChange: TXMPPReadyStateChange;
begin
  Result := FOnStateChange;
end;

end.


