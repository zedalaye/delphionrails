unit dorHTTPClient;

interface
uses
  SysUtils, AnsiStrings, dorUtils, Generics.Collections, WinSock, Classes;

type
 IHTTPRequest = interface;

  TReadyState = (
    rsUninitialized,
    rsOpen,
    rsSent,
    rsReceiving,
    rsLoaded);

  TOnReadyStateChange = reference to procedure(const request: IHTTPRequest);

  THTTPHeader = record
    name, value: RawByteString;
    class function Make(const name, value: RawByteString): THTTPHeader; static;
  end;

  THeaderCollection = TDictionary<RawByteString,THTTPHeader>.TValueCollection;

  EHTTPRequest = Exception;

  IHTTPRequest = interface
    ['{105BFAD3-A4AE-459E-8EA6-377E9E065827}']
    function Open(const method: RawByteString; const url: string; async: Boolean = False;
      const user: string = ''; const password: string = ''; urlencode: Boolean = True): Boolean;
    procedure Abort;
    procedure SetRequestHeader(const header, value: RawByteString);
    function GetRequestHeader(const header: RawByteString): RawByteString;
    function GetResponseHeader(const header: RawByteString): RawByteString;
    function Send(data: TStream = nil; TimeOut: Cardinal = 0; SpeedLimit: Cardinal = 0): Boolean;
    function SendText(const data: string; encoding: TEncoding = nil; TimeOut: Cardinal = 0; SpeedLimit: Cardinal = 0): Boolean;
    function SendFile(const FileName: string; TimeOut: Cardinal = 0; SpeedLimit: Cardinal = 0): Boolean;
    function GetStatus: Word;
    function GetStatusText: RawByteString;
    procedure SetOnReadyStateChange(const ready: TOnReadyStateChange);
    function GetOnReadyStateChange: TOnReadyStateChange;
    function GetReadyState: TReadyState;
    function GetResponseStream: TStream;
    procedure SetResponseStream(stream: TStream);
    function GetResponseText: string;
    function GetRequestHeaders: THeaderCollection;
    function GetResponseHeaders: THeaderCollection;
    function GetSynchronize: Boolean;
    procedure SetSynchronize(value: Boolean);

    property RequestHeader[const header: RawByteString]: RawByteString read GetRequestHeader write SetRequestHeader;
    property ResponseHeader[const header: RawByteString]: RawByteString read GetResponseHeader;
    property Status: Word read GetStatus;
    property StatusText: RawByteString read GetStatusText;
    property OnReadyStateChange: TOnReadyStateChange read GetOnReadyStateChange write SetOnReadyStateChange;
    property ResponseStream: TStream read GetResponseStream write SetResponseStream;
    property ResponseText: string read GetResponseText;
    property ReadyState: TReadyState read GetReadyState;
    property Synchronize: Boolean read GetSynchronize write SetSynchronize;
  end;

  THTTPRequest = class(TInterfacedObject, IHTTPRequest)
  private
  type
    TOnHeaderEvent = reference to function(const value: RawByteString): Boolean;
    TContentEncoding = (
      encUnknown,
      encDeflate,
      encGZIP);
    TCookie = record
      path: RawByteString;
      domain: RawByteString;
      value: RawByteString;
    end;
    TThreadAsync = class(TThread)
    private
      FThis: IHTTPRequest;
      FData: TPooledMemoryStream;
      FTimeOut: Cardinal;
      FSpeedLimit: Cardinal;
    protected
      procedure Execute; override;
    public
      constructor Create(const this: IHTTPRequest; data: TStream; TimeOut, SpeedLimit: Cardinal);
      destructor Destroy; override;
    end;
  private
    FSocket: TSocket;
    FProtocol: string;
    FDomain: AnsiString;
    FPort: Word;
    FPath: RawByteString;
    FMethod: RawByteString;
    FAsync: Boolean;
    FUser: string;
    FPassword: string;
    FResponseData: TStream;
    FResponseDataOwned: Boolean;
    FRequestHeader: TDictionary<RawByteString, THTTPHeader>;
    FResponseHeader: TDictionary<RawByteString, THTTPHeader>;
    FResponseEvents: TDictionary<RawByteString, TOnHeaderEvent>;
    FCookies: TDictionary<RawByteString, TCookie>;

    FStatus: Word;
    FStatusText: RawByteString;
    FOnReadyStateChange: TOnReadyStateChange;
    FReadyState: TReadyState;
    FCharsets: TDictionary<RawByteString, Integer>;
    FReadError: Boolean;
    FRedirectCount: Integer;

    FSynchronize: Boolean;

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
    function Receive(TimeOut: Cardinal): Boolean;
    procedure SendHeaders(data: TStream; SpeedLimit: Cardinal);
    function TCPConnect(const domain: RawByteString; port: Word; ssl: Boolean): Boolean;
    procedure TCPDisconnect;
    function TCPReconnect: Boolean;

    function InternalSend(data: TSTream; TimeOut, SpeedLimit: Cardinal): Boolean;
    function InternalOpen(const method: RawByteString; const url: string; async: Boolean; const user, password: string; urlencode: Boolean): Boolean;
    procedure InternalSetRequestHeader(const header, value: RawByteString);
    function IsRedirecting: Boolean;
  protected
    function Open(const method: RawByteString; const url: string; async: Boolean; const user, password: string; urlencode: Boolean = True): Boolean;
    procedure Abort;
    procedure SetRequestHeader(const header, value: RawByteString);
    function GetRequestHeader(const header: RawByteString): RawByteString;
    function GetResponseHeader(const header: RawByteString): RawByteString;
    function Send(data: TStream; TimeOut, SpeedLimit: Cardinal): Boolean;
    function SendText(const data: string; encoding: TEncoding; TimeOut: Cardinal; SpeedLimit: Cardinal = 0): Boolean;
    function SendFile(const FileName: string; TimeOut, SpeedLimit: Cardinal): Boolean;
    function GetStatus: Word;
    function GetStatusText: RawByteString;
    function GetReadyState: TReadyState;
    procedure SetOnReadyStateChange(const ready: TOnReadyStateChange);
    function GetOnReadyStateChange: TOnReadyStateChange;
    function GetResponseStream: TStream;
    procedure SetResponseStream(stream: TStream);
    function GetResponseText: string;
    function GetRequestHeaders: THeaderCollection;
    function GetResponseHeaders: THeaderCollection;
    function GetSynchronize: Boolean;
    procedure SetSynchronize(value: Boolean);
  public
    constructor Create(const SSLPassword: AnsiString = ''; const CertificateFile: AnsiString = '';
      const PrivateKeyFile: AnsiString = ''; const CertCAFile: AnsiString = ''); virtual;
    destructor Destroy; override;
    class constructor Create;
    class destructor Destroy;
  end;

implementation
uses Windows, ZLib, dorOpenSSL, dorHTTP;

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
  FReadyState := rsUninitialized;
  if FResponseData <> nil then
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
  FResponseData := nil;
  FResponseDataOwned := False;
  FRequestHeader := TDictionary<RawByteString, THTTPHeader>.Create;
  FResponseHeader := TDictionary<RawByteString, THTTPHeader>.Create;
  FCookies := TDictionary<RawByteString, TCookie>.Create;
  FCharsets := TDictionary<RawByteString, Integer>.Create;
  FSynchronize := True;
  LoadCharsets;
  FResponseEvents := TDictionary<RawByteString, TOnHeaderEvent>.Create;
  LoadEvents;
  Abort; // reset
end;

destructor THTTPRequest.Destroy;
begin
  TCPDisconnect;
  if (FResponseDataOwned) and (FResponseData <> nil) then
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

function THTTPRequest.GetReadyState: TReadyState;
begin
  Result := FReadyState;
end;

function THTTPRequest.GetRequestHeader(
  const header: RawByteString): RawByteString;
var
  rec: THTTPHeader;
begin
  if FRequestHeader.TryGetValue(LowerCase(header), rec) then
    Result := rec.value else
    Result := '';
end;

function THTTPRequest.GetRequestHeaders: THeaderCollection;
begin
  Result := FRequestHeader.Values;
end;

function THTTPRequest.GetResponseHeader(const header: RawByteString): RawByteString;
var
  rec: THTTPHeader;
begin
  if (FReadyState >= rsReceiving) and FResponseHeader.TryGetValue(LowerCase(header), rec) then
    Result := rec.value else
    Result := '';
end;

function THTTPRequest.GetResponseHeaders: THeaderCollection;
begin
  Result := FResponseHeader.Values;
end;

function THTTPRequest.GetResponseStream: TStream;
begin
  if FReadyState <> rsLoaded then
    raise EHTTPRequest.Create('Document is not loaded.');
  Result := FResponseData;
end;

function THTTPRequest.GetResponseText: string;
var
  strings: TStringList;
  encoding: TEncoding;
  charset: Integer;
  freecharset: Boolean;
  contenttype: THTTPHeader;
begin
  if FReadyState <> rsLoaded then
    raise EHTTPRequest.Create('Document is not loaded.');

  freecharset := False;
  encoding := nil;
  strings := TStringList.Create;
  try
    if FResponseHeader.TryGetValue('content-type', contenttype) then
    begin
      HTTPParseHeader(contenttype.value, True, function (group: Integer; const key: RawByteString;
        const value: RawByteString): Boolean
      begin
        if AnsiStrings.SameText(key, 'charset') then
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

    FResponseData.Seek(0, soFromBeginning);
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

function THTTPRequest.GetSynchronize: Boolean;
begin
  Result := FSynchronize;
end;

procedure THTTPRequest.HTTPWriteLine(const data: RawByteString);
var
  rb: RawByteString;
begin
  rb := RawByteString(data) + #13#10;
  SockSend(PAnsiChar(rb)^, Length(rb));
end;

function THTTPRequest.InternalOpen(const method: RawByteString;
  const url: string; async: Boolean; const user, password: string; urlencode: Boolean): Boolean;
var
  Protocol: string;
  Domain: AnsiString;
  Port: Word;
  ssl: Boolean;
label
  error, keepsocket;
begin
  if (url <> '') and (url[1] = '/') then
  begin
    if urlencode then
      FPath := HTTPEncode(url) else
      FPath := RawbyteString(url);
  end else
    begin
      if not HTTPParseURL(PChar(url), Protocol, Domain, Port, FPath, urlencode) then
        Exit(False);

      if SameText(Protocol, 'http') then
      begin
        ssl := False;
        if Port = 0 then
          Port := 80;
      end else
        if SameText(Protocol, RawbyteString('https')) then
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

      FProtocol := Protocol;
      FDomain := Domain;
      FPort := Port;
      LoadDefaultHeader;
    end;

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

function THTTPRequest.InternalSend(data: TSTream; TimeOut, SpeedLimit: Cardinal): Boolean;
var
  str: THTTPHeader;
begin
  SendHeaders(data, SpeedLimit);
  Result := Receive(TimeOut);
  // Reconnect ?
  if FReadError then
  begin
    if TCPReconnect then
    begin
      SendHeaders(data, SpeedLimit);
      Result := Receive(TimeOut);
    end;
  end;

  // Redirect ?
  if Result and IsRedirecting and FResponseHeader.TryGetValue('location', str) then
  begin
    Inc(FRedirectCount);
    if FRedirectCount > 10 then
      raise EHTTPRequest.Create('Too many redirections.');

    Result := InternalOpen(FMethod, string(str.value), FAsync, FUser, FPassword, False);
    if Result then
      Result := InternalSend(data, TimeOut, SpeedLimit);
  end;
end;

procedure THTTPRequest.InternalSetRequestHeader(const header,
  value: RawByteString);
begin
  FRequestHeader.AddOrSetValue(LowerCase(header), THTTPHeader.Make(header, value));
end;

function THTTPRequest.IsRedirecting: Boolean;
begin
  Result := (FStatus = 201) or (FStatus = 301) or (FStatus = 302);
end;

function THTTPRequest.Open(const method: RawByteString; const url: string; async: Boolean;
  const user, password: string; urlencode: Boolean = True): Boolean;
begin
  if not (FReadyState in [rsUninitialized, rsLoaded]) then
    raise EHTTPRequest.Create('Connection is not ready.');
  Result := InternalOpen(method, url, async, user, password, urlencode);
end;

function THTTPRequest.Receive(TimeOut: Cardinal): Boolean;
var
  str: THTTPHeader;
  len, rcv: Integer;
  buff: array[0..1023] of AnsiChar;
  strm: TStream;
  encoding: TContentEncoding;
  t: timeval;
begin
  FReadError := False;
  strm := nil;
  if FResponseData = nil then
  begin
    FResponseData := TPooledMemoryStream.Create;
    FResponseDataOwned := True;
  end else
    FResponseData.Size := 0;
  FResponseHeader.Clear;

  t.tv_sec := TimeOut;
  t.tv_usec := 0;
  setsockopt(FSocket, SOL_SOCKET, SO_RCVTIMEO, @t, SizeOf(t));

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
      SetResponseHeader(key, value);
      Result := True;
    end) then
      Exit(False);

  if FResponseHeader.TryGetValue('content-encoding', str) then
  begin
    if AnsiStrings.SameText(str.value, 'deflate') then
    begin
      encoding := encDeflate;
    end else
    if AnsiStrings.SameText(str.value, 'gzip') then
      encoding := encGZIP else
      encoding := encUnknown;
  end else
    encoding := encUnknown;

  if encoding = encUnknown then
    strm := FResponseData else
    strm := TPooledMemoryStream.Create;

  if FResponseHeader.TryGetValue('transfer-encoding', str) and AnsiStrings.SameText(str.value, 'chunked') then
  begin
    if not HTTPReadChunked(
      function (var buf; len: Integer): Integer
      begin
        Result := SockRecv(buf, len);
      end,
      function (var buf; len: Integer): Integer
      begin
        SetReadyState(rsReceiving);
        Result := strm.Write(buf, len);
      end) then
        Exit(False);
  end else
  if FResponseHeader.TryGetValue('content-length', str) and
    TryStrToInt(string(str.value), len) and (len > 0) then
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
      strm.Write(buff, rcv);
      Dec(len, rcv);
    end;
  end;

  if (strm <> nil) then
    case encoding of
      encDeflate:
        begin
          strm.Seek(0, soFromBeginning);
          try
            if (strm.Size > 0) then
              if not DecompressStream(strm, FResponseData, True) then
                Exit(False);
          finally
            strm.Free;
          end;
        end;
      encGZIP:
        begin
          strm.Seek(0, soFromBeginning);
          try
            if (strm.Size > 0) then
              if not DecompressGZipStream(strm, FResponseData) then
                Exit(False);
          finally
            strm.Free;
          end;
        end;
    end;
  FResponseData.Seek(0, soFromBeginning);
  Result := True;
end;

function THTTPRequest.Send(data: TStream; TimeOut, SpeedLimit: Cardinal): Boolean;
begin
  if FReadyState <> rsOpen then
    raise EHTTPRequest.Create('Socket is not open.');
  FRedirectCount := 0;
  if FAsync then
  begin
    TThreadAsync.Create(Self, data, TimeOut, SpeedLimit);
    Result := True;
  end else
  begin
    Result := InternalSend(data, TimeOut, SpeedLimit);
    SetReadyState(rsLoaded);
  end;
end;

function THTTPRequest.SendFile(const FileName: string; TimeOut, SpeedLimit: Cardinal): Boolean;
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := Send(stream, TimeOut, SpeedLimit);
  finally
    stream.Free;
  end;
end;

procedure THTTPRequest.SendHeaders(data: TStream; SpeedLimit: Cardinal);
var
  pair: THTTPHeader;
  cook: TPair<RawByteString, TCookie>;
  cookie: RawByteString;
  cookiecount: Integer;
  buffer: array[0..1023] of AnsiChar;
  read: Integer;
  deflate: TPooledMemoryStream;
  Total, Rate, Start, Curr, Freq: Int64;
begin
  HTTPWriteLine(FMethod + ' ' + FPath + ' HTTP/1.1');

  if ((FSsl = nil) and (FPort <> 80)) or ((FSsl <> nil) and (FPort <> 443)) then
    HTTPWriteLine('Host: ' + FDomain + ':' + RawByteString(IntToStr(FPort))) else
    HTTPWriteLine('Host: ' + FDomain);

  for pair in FRequestHeader.Values do
    HTTPWriteLine(pair.name + ': ' + pair.value);

  cookiecount := 0;
  for cook in FCookies do
    if Pos(cook.Value.path, FPath) = 1 then
    begin
      if cookiecount > 0 then
        cookie := cookie + '; ' else
        cookie := 'Cookie: ';
      cookie := cookie + cook.Key + '=' + cook.Value.value;
      Inc(cookiecount);
    end;
  if cookiecount > 0 then
    HTTPWriteLine(cookie);

  if (FUser <> '') and (FPassword <> '') then
    HTTPWriteLine('Authorization: Basic ' + RawByteString(StrTobase64(FUser + ':' + FPassword)));

  if (data <> nil) and (data.Size > 0) then
  begin
    data.Seek(0, soFromBeginning);

    if AnsiStrings.SameText(GetRequestHeader('Content-Encoding'), 'deflate') then
    begin
      deflate := TPooledMemoryStream.Create;
      CompressStream(data, deflate);
      data := deflate;
      data.Seek(2, soFromBeginning);
    end else
      deflate := nil;


    HTTPWriteLine('Content-Length: ' + RawByteString(IntToStr(data.Size - data.Position)));
    HTTPWriteLine('');

    Total := 0;
    if SpeedLimit > 0 then
    begin
      QueryPerformanceFrequency(Freq);
      QueryPerformanceCounter(Start);
    end;

    AllocConsole;
    repeat
      read := data.Read(buffer, SizeOf(buffer));
      if read > 0 then
      begin
        if SpeedLimit > 0 then
        begin
          Inc(Total, read);
          while True do
          begin
            QueryPerformanceCounter(Curr);
            Rate := Round((Total / 1024) / ((Curr - Start) / Freq));
            if Rate < SpeedLimit then
              Break else
              Sleep(1000);
          end;
        end;
        Writeln(Total);
        SockSend(buffer, read);
      end;
    until read = 0;

    if deflate <> nil then
      deflate.Free;
  end else
    HTTPWriteLine('');
  SetReadyState(rsSent);
end;

function THTTPRequest.SendText(const data: string; encoding: TEncoding; TimeOut, SpeedLimit: Cardinal): Boolean;
var
  stream: TStringStream;
begin
  if encoding = nil then
    encoding := TEncoding.UTF8;
  stream := TStringStream.Create(data, encoding);
  try
    Result := Send(stream, TimeOut, SpeedLimit);
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
    if FAsync and FSynchronize then
      TThread.Synchronize(nil, procedure
        begin FOnReadyStateChange(Self) end) else
      FOnReadyStateChange(Self)
end;

procedure THTTPRequest.SetRequestHeader(const header, value: RawByteString);
begin
  if FReadyState <> rsOpen then
    raise EHTTPRequest.Create('Connection is not open.');
  InternalSetRequestHeader(header, value);
end;

procedure THTTPRequest.SetResponseHeader(const header, value: RawByteString);
var
  event: TOnHeaderEvent;
  low: RawByteString;
begin
  low := LowerCase(header);
  if not FResponseEvents.TryGetValue(low, event) or event(value) then
    FResponseHeader.AddOrSetValue(low, THTTPHeader.Make(header, value));
end;

procedure THTTPRequest.SetResponseStream(stream: TStream);
begin
  if FResponseDataOwned and (FResponseData <> nil) then
    FResponseData.Free;
  FResponseData := stream;
  FResponseDataOwned := False;
end;

procedure THTTPRequest.SetSynchronize(value: Boolean);
begin
  FSynchronize := value;
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
      if (WSAGetLastError = WSAETIMEDOUT) then
        raise EHTTPRequest.Create('Timeout.');
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
  InternalSetRequestHeader('Accept', '*/*');
  InternalSetRequestHeader('Accept-Charset', 'utf-8;q=0.7,*;q=0.3');
  InternalSetRequestHeader('Accept-Encoding', 'deflate,gzip');
  InternalSetRequestHeader('Accept-Language', 'en-US;q=0.6,en;q=0.4');
  InternalSetRequestHeader('Cache-Control', 'max-age=0');
  InternalSetRequestHeader('Connection', 'keep-alive');
  InternalSetRequestHeader('Keep-Alive', '300');
  InternalSetRequestHeader('User-Agent', 'Mozilla/5.0');
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
            if AnsiStrings.SameText(key, 'path') then
               cookie.path := value else
               if AnsiStrings.SameText(key, 'domain') then
                 cookie.domain := value;
          Result := True;
        end) then
          FCookies.AddOrSetValue(name, cookie);
      Result := False;
    end);
end;

{ THTTPRequest.TThreadAsync }

constructor THTTPRequest.TThreadAsync.Create(const this: IHTTPRequest;
  data: TStream; TimeOut, SpeedLimit: Cardinal);
begin
  FreeOnTerminate := True;
  FThis := this;
  FTimeOut := TimeOut;
  FSpeedLimit := SpeedLimit;
  if data <> nil then
  begin
    FData := TPooledMemoryStream.Create;
    FData.LoadFromStream(data);
  end else
    FData := nil;
  inherited Create(False);
end;

destructor THTTPRequest.TThreadAsync.Destroy;
begin
  if FData <> nil then
    FData.Free;
  FThis := nil;
  inherited;
end;

procedure THTTPRequest.TThreadAsync.Execute;
begin
  THTTPRequest(FThis).InternalSend(FData, FTimeOut, FSpeedLimit);
  THTTPRequest(FThis).SetReadyState(rsLoaded);
end;

{ THTTPHeader }

class function THTTPHeader.Make(const name,
  value: RawByteString): THTTPHeader;
begin
  Result.name := name;
  Result.value := value;
end;

end.
