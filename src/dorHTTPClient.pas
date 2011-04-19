unit dorHTTPClient;

interface
uses
  SysUtils, Generics.Collections, WinSock, Classes;

type
 IHTTPRequest = interface;

  TReadyState = (
    rsUninitialized,
    rsOpen,
    rsSent,
    rsReceiving,
    rsLoaded);

  TOnReadyStateChange = reference to procedure(const request: IHTTPRequest);

  EHTTPRequest = exception;

  IHTTPRequest = interface
    ['{105BFAD3-A4AE-459E-8EA6-377E9E065827}']
    function Open(const method: RawByteString; const url: string; async: Boolean = False;
      const user: string = ''; const password: string = ''): Boolean;
    procedure Abort;
    procedure SetRequestHeader(const header, value: RawByteString);
    function GetRequestHeader(const header: RawByteString): RawByteString;
    function GetResponseHeader(const header: RawByteString): RawByteString;
    function Send(data: TStream = nil): Boolean;
    function SendText(const data: string; encoding: TEncoding = nil): Boolean;
    function GetStatus: Word;
    function GetStatusText: RawByteString;
    procedure SetOnReadyStateChange(const ready: TOnReadyStateChange);
    function GetOnReadyStateChange: TOnReadyStateChange;
    function GetResponseStream: TStream;
    function GetResponseText: string;

    property RequestHeader[const header: RawByteString]: RawByteString read GetRequestHeader write SetRequestHeader;
    property Status: Word read GetStatus;
    property StatusText: RawByteString read GetStatusText;
    property OnReadyStateChange: TOnReadyStateChange read GetOnReadyStateChange write SetOnReadyStateChange;
    property ResponseStream: TStream read GetResponseStream;
    property ResponseText: string read GetResponseText;
  end;

  THTTPRequest = class(TInterfacedObject, IHTTPRequest)
  private
  type
    TOnHeaderEvent = reference to function(const value: RawByteString): Boolean;
    TCookie = record
      path: RawByteString;
      domain: RawByteString;
      value: RawByteString;
    end;
  private
    FSocket: TSocket;
    FDomain: AnsiString;
    FPort: Word;
    FPath: RawByteString;
    FMethod: RawByteString;
    FAsync: Boolean;
    FUser: string;
    FPassword: string;
    FResponseData: TStream;
    FRequestHeader: TDictionary<RawByteString, RawByteString>;
    FResponseHeader: TDictionary<RawByteString, RawByteString>;
    FResponseEvents: TDictionary<RawByteString, TOnHeaderEvent>;
    FCookies: TDictionary<RawByteString, TCookie>;

    FStatus: Word;
    FStatusText: RawByteString;
    FOnReadyStateChange: TOnReadyStateChange;
    FReadyState: TReadyState;
    FCharsets: TDictionary<RawByteString, Integer>;
    FReadError: Boolean;
    // SSL
    FCtx: Pointer;
    FSsl: Pointer;
    FSSLPassword: AnsiString;
    FCertificateFile: AnsiString;
    FPrivateKeyFile: AnsiString;
    FCertCAFile: AnsiString;

    procedure SetResponseHeader(const header, value: RawByteString);
    procedure HTTPWriteLine(const data: RawByteString);
    function SockSend(var Buf; len: Integer): Integer;
    function SockRecv(var Buf; len: Integer): Integer;
    procedure LoadDefaultHeader;
    procedure LoadCharsets;
    procedure LoadEvents;

    procedure SetReadyState(ready: TReadyState);
    function Receive: Boolean;
    procedure SendHeaders(data: TStream);
    function TCPConnect(const domain: RawByteString; port: Word; ssl: Boolean): Boolean;
    procedure TCPDisconnect;
    function TCPReconnect: Boolean;

  protected
    function Open(const method: RawByteString; const url: string; async: Boolean; const user, password: string): Boolean;
    procedure Abort;
    procedure SetRequestHeader(const header, value: RawByteString);
    function GetRequestHeader(const header: RawByteString): RawByteString;
    function GetResponseHeader(const header: RawByteString): RawByteString;
    function Send(data: TStream): Boolean;
    function GetStatus: Word;
    function GetStatusText: RawByteString;
    procedure SetOnReadyStateChange(const ready: TOnReadyStateChange);
    function GetOnReadyStateChange: TOnReadyStateChange;
    function GetResponseStream: TStream;
    function GetResponseText: string;
    function SendText(const data: string; encoding: TEncoding): Boolean;
  public
    constructor Create(const SSLPassword: AnsiString = ''; const CertificateFile: AnsiString = '';
      const PrivateKeyFile: AnsiString = ''; const CertCAFile: AnsiString = ''); virtual;
    destructor Destroy; override;
    class constructor Create;
    class destructor Destroy;
  end;

implementation
uses Windows, AnsiStrings, ZLib, dorUtils, dorOpenSSL, dorHTTP;

type
  TContentEncoding = (
    encUnknown,
    encDeflate,
    encGZIP
  );

function SSLPasswordCallback(buffer: PAnsiChar; size, rwflag: Integer;
  this: THTTPRequest): Integer; cdecl;
var
  password: AnsiString;
begin
  password := this.FSSLPassword;
  if Length(password) > (Size - 1) then
    SetLength(password, Size - 1);
  Result := Length(password);
  Move(PAnsiChar(password)^, buffer^, Result + 1);
end;

{ THTTPRequest }

procedure THTTPRequest.Abort;
begin
  TCPDisconnect;
  FReadyState := rsUninitialized;
  FResponseData.Size := 0;
  LoadDefaultHeader;
  FResponseHeader.Clear;
  FStatus := 0;
  FStatusText := '';
  FCookies.Clear;
end;

constructor THTTPRequest.Create(const SSLPassword: AnsiString = ''; const CertificateFile: AnsiString = '';
  const PrivateKeyFile: AnsiString = ''; const CertCAFile: AnsiString = '');
begin
  FSocket := INVALID_SOCKET;
  FPort := 80;
  FCtx := nil;
  FSsl := nil;
  FSSLPassword := SSLPassword;
  FCertificateFile := CertificateFile;
  FPrivateKeyFile := PrivateKeyFile;
  FCertCAFile := CertCAFile;
  FResponseData := TPooledMemoryStream.Create;
  FRequestHeader := TDictionary<RawByteString, RawByteString>.Create;
  FResponseHeader := TDictionary<RawByteString, RawByteString>.Create;
  FCookies := TDictionary<RawByteString, TCookie>.Create;
  FCharsets := TDictionary<RawByteString, Integer>.Create;
  LoadCharsets;
  FResponseEvents := TDictionary<RawByteString, TOnHeaderEvent>.Create;
  LoadEvents;
  Abort; // reset
end;

destructor THTTPRequest.Destroy;
begin
  Abort;
  FResponseData.Free;
  FRequestHeader.Free;
  FResponseHeader.Free;
  FCharsets.Free;
  FResponseEvents.Free;
  FCookies.Free;
  inherited;
end;

function THTTPRequest.GetOnReadyStateChange: TOnReadyStateChange;
begin
  Result := FOnReadyStateChange;
end;

function THTTPRequest.GetRequestHeader(
  const header: RawByteString): RawByteString;
begin
  if not FRequestHeader.TryGetValue(LowerCase(header), Result) then
    Result := '';
end;

function THTTPRequest.GetResponseHeader(const header: RawByteString): RawByteString;
begin
  if (FReadyState < rsReceiving) or not FResponseHeader.TryGetValue(LowerCase(header), Result) then
    Result := '';
end;

function THTTPRequest.GetResponseStream: TStream;
begin
  if FReadyState <> rsLoaded then
    raise EHTTPRequest.Create('document is not loaded');
  Result := FResponseData;
end;

function THTTPRequest.GetResponseText: string;
var
  strings: TStringList;
  encoding: TEncoding;
  charset: Integer;
  freecharset: Boolean;
  contenttype: RawByteString;
begin
  if FReadyState <> rsLoaded then
    raise EHTTPRequest.Create('document is not loaded');

  freecharset := False;
  encoding := nil;
  strings := TStringList.Create;
  try
    if FResponseHeader.TryGetValue('content-type', contenttype) then
    begin
      HTTPParseHeader(contenttype, True, function (group: Integer; const key: RawByteString;
        const value: RawByteString): Boolean
      begin
        if LowerCase(key) = 'charset' then
        begin
          if FCharsets.TryGetValue(LowerCase(value), charset) then
          case charset of
            CP_UTF8: encoding := TEncoding.UTF8;
            CP_UTF7: encoding := TEncoding.UTF7;
          else
            encoding := TMBCSEncoding.Create(charset);
            freecharset := True;
          end;
          Result := False;
        end else
          Result := True;
      end);
    end;

    strings.LoadFromStream(FResponseData, encoding);
    Result := strings.Text;
  finally
    if freecharset and (encoding <> nil) then
      encoding.Free;

    strings.Free;
  end;
end;

function THTTPRequest.GetStatus: Word;
begin
  Result := FStatus;
end;

function THTTPRequest.GetStatusText: RawByteString;
begin
  Result := FStatusText;
end;

procedure THTTPRequest.HTTPWriteLine(const data: RawByteString);
var
  rb: RawByteString;
begin
  rb := RawByteString(data) + #13#10;
  SockSend(PAnsiChar(rb)^, Length(rb));
end;

function THTTPRequest.Open(const method: RawByteString; const url: string; async: Boolean;
  const user, password: string): Boolean;
var
  protocol: string;
  Domain: AnsiString;
  Port: Word;
  ssl: Boolean;
label
  error, keepsocket;
begin
  if not (FReadyState in [rsUninitialized, rsLoaded]) then
    raise EHTTPRequest.Create('Connextion is not ready');

  if not HTTPParseURL(PChar(url), protocol, Domain, Port, FPath) then
    Exit(False);

  if protocol = 'http' then
  begin
    ssl := False;
    if Port = 0 then
      Port := 80;
  end else
    if protocol = 'https' then
    begin
      ssl := True;
      if Port = 0 then
        Port := 443;
    end else
      Exit(False);

  if (FSocket <> INVALID_SOCKET) then
  begin
    if (FDomain = Domain) and (FPort = Port) and (ssl = (FSsl <> nil)) then
      goto keepsocket else
      Abort;
  end;

  if not TCPConnect(Domain, Port, ssl) then
    goto error;

  FDomain := Domain;
  FPort := Port;
  LoadDefaultHeader;
keepsocket:
  FAsync := async;
  FUser := user;
  FPassword := password;
  FMethod := method;
  SetReadyState(rsOpen);
  Exit(True);
error:
  Abort;
  Result := False;
end;

function THTTPRequest.Receive: Boolean;
var
  str: RawByteString;
  len, rcv: Integer;
  buff: array[0..1023] of AnsiChar;
  strm: TPooledMemoryStream;
  encoding: TContentEncoding;
begin
  FReadError := False;
  FResponseData.Size := 0;
  FResponseHeader.Clear;

  if not HTTPParse(
    function (var buf; len: Integer): Integer
    begin
      Result := SockRecv(buf, len);
    end,
    function (code: Integer; const mesg: RawByteString): Boolean
    begin
      FStatus := code;
      FStatusText := mesg;
      Result := True;
    end,
    function (const key: RawByteString; const value: RawByteString): Boolean
    begin
      SetResponseHeader(LowerCase(key), value);
      Result := True;
    end) then
      Exit(False);

  if FResponseHeader.TryGetValue('content-encoding', str) then
  begin
    if (str = 'deflate') then
    begin
      encoding := encDeflate;
    end else
    if (str = 'gzip') then
      encoding := encGZIP else
      encoding := encUnknown;
  end else
    encoding := encUnknown;

  if FResponseHeader.TryGetValue('transfer-encoding', str) and (str = 'chunked') then
  begin
    if not HTTPReadChunked(
      function (var buf; len: Integer): Integer
      begin
        Result := SockRecv(buf, len);
      end,
      function (var buf; len: Integer): Integer
      begin
        SetReadyState(rsReceiving);
        Result := FResponseData.Write(buf, len);
      end) then
        Exit(False);
  end else
  if FResponseHeader.TryGetValue('content-length', str) and
    TryStrToInt(string(str), len) and (len > 0) then
  begin
    while len > 0 do
    begin
      SetReadyState(rsReceiving);
      if len >= SizeOf(buff) then
      begin
        rcv := SockRecv(buff, SizeOf(buff));
        if rcv <> SizeOf(buff) then
          Exit(False);
      end else
      begin
        rcv := SockRecv(buff, len);
        if rcv <> len then
          Exit(False);
      end;
      FResponseData.Write(buff, rcv);
      Dec(len, rcv);
    end;
  end;

  if FResponseData.Size > 0 then
  begin
    case encoding of
      encDeflate:
        begin
          strm := TPooledMemoryStream.Create();
          FResponseData.Seek(0, soFromBeginning);
          if DecompressStream(FResponseData, strm, True) then
          begin
            FResponseData.Free;
            FResponseData := strm;
          end else
            strm.Free;
        end;
      encGZIP:
        begin
          strm := TPooledMemoryStream.Create();
          FResponseData.Seek(0, soFromBeginning);
          if DecompressGZipStream(FResponseData, strm) then
          begin
            FResponseData.Free;
            FResponseData := strm;
          end else
          begin
            strm.Free;
            Exit(False);
          end;
        end;
    end;
    FResponseData.Seek(0, soFromBeginning);
  end;

  SetReadyState(rsLoaded);
  Result := True;
end;

function THTTPRequest.Send(data: TStream): Boolean;
begin
  if FReadyState <> rsOpen then
    raise EHTTPRequest.Create('socket is not open');
  SendHeaders(data);

  Result := Receive;
  // reconnect ?
  if FReadError then
  begin
    if TCPReconnect then
    begin
      SendHeaders(data);
      Result := Receive;
    end;
  end;
  // redirect ?
  if Result and ((FStatus = 301) or (FStatus = 302)) and FResponseHeader.TryGetValue('location', FPath) then
  begin
    SetReadyState(rsOpen);
    Result := Send(data);
  end;

end;

procedure THTTPRequest.SendHeaders(data: TStream);
var
  pair: TPair<RawByteString, RawByteString>;
  cook: TPair<RawByteString, TCookie>;
  cookie: RawByteString;
  cookiecount: Integer;
  buffer: array[0..1023] of AnsiChar;
  read: Integer;
begin
  HTTPWriteLine(FMethod + ' ' + FPath + ' HTTP/1.1');

  if ((FSsl = nil) and (FPort <> 80)) or ((FSsl <> nil) and (FPort <> 443)) then
    FRequestHeader.AddOrSetValue('host', FDomain + ':' + RawByteString(IntToStr(FPort))) else
    FRequestHeader.AddOrSetValue('host', FDomain);

  for pair in FRequestHeader do
    HTTPWriteLine(pair.Key + ': ' + pair.Value);

  cookiecount := 0;
  for cook in FCookies do
    if Pos(cook.Value.path, FPath) = 1 then
    begin
      if cookiecount > 0 then
        cookie := cookie + '; ' else
        cookie := 'cookie: ';
      cookie := cookie + cook.Key + '=' + cook.Value.value;
      Inc(cookiecount);
    end;
  if cookiecount > 0 then
    HTTPWriteLine(cookie);

  if (data <> nil) and (data.Size > 0) then
  begin
    HTTPWriteLine('content-length: ' + RawByteString(IntToStr(data.Size)));
    HTTPWriteLine('');
    data.Seek(0, soFromBeginning);
    repeat
      read := data.Read(buffer, SizeOf(buffer));
      if read > 0 then
        SockSend(buffer, read);
    until read = 0;

  end else
    HTTPWriteLine('');
  SetReadyState(rsSent);
end;

function THTTPRequest.SendText(const data: string; encoding: TEncoding): Boolean;
var
  stream: TStringStream;
begin
  if encoding = nil then
    encoding := TEncoding.UTF8;
  stream := TStringStream.Create(data, encoding);
  try
    Result := Send(stream);
  finally
    stream.Free;
  end;
end;

procedure THTTPRequest.SetOnReadyStateChange(const ready: TOnReadyStateChange);
begin
  FOnReadyStateChange := ready;
end;

procedure THTTPRequest.SetReadyState(ready: TReadyState);
begin
  FReadyState := ready;
  if Assigned(FOnReadyStateChange) then
    FOnReadyStateChange(Self)
end;

procedure THTTPRequest.SetRequestHeader(const header, value: RawByteString);
begin
  if FReadyState <> rsOpen then
    raise EHTTPRequest.Create('Connection is not open');
  FRequestHeader.AddOrSetValue(LowerCase(header), value);
end;

procedure THTTPRequest.SetResponseHeader(const header, value: RawByteString);
var
  event: TOnHeaderEvent;
begin
  if not FResponseEvents.TryGetValue(header, event) or event(value) then
    FResponseHeader.AddOrSetValue(header, value);
end;

function THTTPRequest.SockRecv(var Buf; len: Integer): Integer;
var
  rcv: Integer;
  p: PByte;
begin
  Result := 0;
  p := @buf;
  while len > 0 do
  begin
    if FSsl <> nil then
      rcv := SSL_read(FSsl, p, 1) else
      rcv := recv(FSocket, p^, 1, 0);
    if rcv <> 1 then
    begin
      FReadError := True;
      Exit;
    end;

    Dec(len, 1);
    Inc(Result, 1);
    Inc(p);
  end;
end;

function THTTPRequest.SockSend(var Buf; len: Integer): Integer;
begin
  if FSsl <> nil then
    Result := SSL_write(FSsl, @Buf, len) else
    Result := WinSock.send(FSocket, Buf, len, 0);
end;

function THTTPRequest.TCPConnect(const domain: RawByteString; port: Word; ssl: Boolean): Boolean;
var
  host: PHostEnt;
  addr: TSockAddrIn;
begin
  Result := True;
  // find host
  host := gethostbyname(PAnsiChar(Domain));
  if host = nil then Exit(False);

  // socket
  FSocket := socket(AF_INET, SOCK_STREAM, 0);
  if FSocket = INVALID_SOCKET then Exit(False);

  // connect
  FillChar(addr, SizeOf(addr), 0);
  addr.sin_family := AF_INET;
  addr.sin_port := htons(Port);
  addr.sin_addr.S_addr := PInteger(host.h_addr^)^;
  if connect(FSocket, addr, SizeOf(addr)) <> 0 then
    Exit(False);

  if ssl then
  begin
    FCtx := SSL_CTX_new(SSLv23_method);
    SSL_CTX_set_cipher_list(FCtx, 'DEFAULT');

    SSL_CTX_set_default_passwd_cb_userdata(FCtx, Self);
    SSL_CTX_set_default_passwd_cb(FCtx, @SSLPasswordCallback);

    if FCertificateFile <> '' then
      if SSL_CTX_use_certificate_chain_file(FCtx, PAnsiChar(FCertificateFile)) <> 1 then
        if SSL_CTX_use_certificate_file(FCtx, PAnsiChar(FCertificateFile), SSL_FILETYPE_PEM) <> 1 then
          if SSL_CTX_use_certificate_file(FCtx, PAnsiChar(FCertificateFile), SSL_FILETYPE_ASN1) <> 1 then
            Exit(False);

    if FPrivateKeyFile <> '' then
      if SSL_CTX_use_RSAPrivateKey_file(FCtx, PAnsiChar(FPrivateKeyFile), SSL_FILETYPE_PEM) <> 1 then
        if SSL_CTX_use_RSAPrivateKey_file(FCtx, PAnsiChar(FPrivateKeyFile), SSL_FILETYPE_ASN1) <> 1 then
          Exit(False);

    if FCertCAFile <> '' then
      if SSL_CTX_load_verify_locations(FCtx, PAnsiChar(FCertCAFile), nil) <> 1 then
        Exit(False);

    FSsl := SSL_new(FCtx);
    SSL_set_fd(FSsl, FSocket);
    if SSL_connect(FSsl) <> 1 then
      Exit(False);
  end;
end;

procedure THTTPRequest.TCPDisconnect;
begin
  if FSocket <> INVALID_SOCKET then
  begin
    closesocket(FSocket);
    FSocket := INVALID_SOCKET;
    Sleep(1);

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
end;

function THTTPRequest.TCPReconnect: Boolean;
var
  ssl: Boolean;
begin
  ssl := FSsl <> nil;
  TCPDisconnect;
  Result := TCPConnect(FDomain, FPort, ssl);
end;

class constructor THTTPRequest.Create;
var
  Data: TWSAData;
begin
  WSAStartup($0202, Data);
end;

class destructor THTTPRequest.Destroy;
begin
  WSACleanup;
end;

procedure THTTPRequest.LoadCharsets;
begin
  FCharsets.Add('utf-8',        CP_UTF8);
  FCharsets.Add('utf-7',        CP_UTF7);

  FCharsets.Add('iso-8859-1',   28591);
  FCharsets.Add('iso-8859-2',   28592);
  FCharsets.Add('iso-8859-3',   28593);
  FCharsets.Add('iso-8859-4',   28594);
  FCharsets.Add('iso-8859-5',   28595);
  FCharsets.Add('iso-8859-6',   28596);
  FCharsets.Add('iso-8859-7',   28597);
  FCharsets.Add('iso-8859-8',   28598);
  FCharsets.Add('iso-8859-9',   28599);
  FCharsets.Add('iso 8859-15',  28605);
  FCharsets.Add('iso-2022-jp',  50220);
  FCharsets.Add('shift_jis',    932);
  FCharsets.Add('euc-jp',       20932);
  FCharsets.Add('ascii',        20127);
  FCharsets.Add('windows-1251', 1251);
  FCharsets.Add('windows-1252', 1252);
end;

procedure THTTPRequest.LoadDefaultHeader;
begin
  FRequestHeader.Clear;
  FRequestHeader.Add('accept', '*/*');
  FRequestHeader.Add('accept-charset', 'utf-8;q=0.7,*;q=0.3');
  FRequestHeader.Add('accept-encoding', 'deflate,gzip');
  FRequestHeader.Add('accept-language', 'en-US;q=0.6,en;q=0.4');
  FRequestHeader.Add('cache-control', 'max-age=0');
  FRequestHeader.Add('connection', 'keep-alive');
  FRequestHeader.Add('user-agent', 'Mozilla/5.0');
end;

procedure THTTPRequest.LoadEvents;
begin
  FResponseEvents.Add('set-cookie',
    function (const value: RawByteString): Boolean
    var
      name: RawByteString;
      cookie: TCookie;
    begin
      if HTTPParseHeader(value, False,
        function(group: Integer; const key: RawByteString; const value: RawByteString): Boolean
        begin
          if group = 0 then
          begin
            name := key;
            cookie.value := value;
          end else
            if SameText(key, RawByteString('path')) then
               cookie.path := value else
               if SameText(key, RawByteString('domain')) then
                 cookie.domain := value;
          Result := True;
        end) then
          FCookies.AddOrSetValue(name, cookie);
      Result := False;
    end);
end;

end.
