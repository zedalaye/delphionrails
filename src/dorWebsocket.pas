unit dorWebsocket;

interface
uses SysUtils, WinSock, dorHTTP, dorOpenSSL;

type
  TWSMessage = reference to procedure(const msg: string);
  TWSReadyState = (rsConnecting, rsOpen, rsClosing, rsClosed);

  IWebSocket = interface
    ['{A9BC9CE6-7C13-4B02-BF66-033EB0BB943E}']
    function GetReadyState: TWSReadyState;
    procedure Send(const data: string);
    procedure Close;
    procedure Open(const url: string; const origin: RawByteString = 'null');

    function GetOnOpen: TProc;
    function GetOnClose: TProc;
    function GetOnError: TWSMessage;
    function GetOnMessage: TWSMessage;
    function GetOnAddField: TOnHTTPAddField;

    procedure SetOnOpen(const value: TProc);
    procedure SetOnClose(const value: TProc);
    procedure SetOnError(const value: TWSMessage);
    procedure SetOnMessage(const value: TWSMessage);
    procedure SetOnAddField(const value: TOnHTTPAddField);

    property OnOpen: TProc read GetOnOpen write SetOnOpen;
    property OnClose: TProc read GetOnClose write SetOnClose;
    property OnError: TWSMessage read GetOnError write SetOnError;
    property OnMessage: TWSMessage read GetOnMessage write SetOnMessage;
    property OnAddField: TOnHTTPAddField read GetOnAddField write SetOnAddField;

    property ReadyState: TWSReadyState read getReadyState;
  end;

  TWebSocket = class(TInterfacedObject, IWebSocket)
  private
    FOnOpen: TProc;
    FOnClose: TProc;
    FOnError: TWSMessage;
    FOnMessage: TWSMessage;
    FOnAddField: TOnHTTPAddField;
    FReadyState: TWSReadyState;
    FSocket: TSocket;
    // SSL
    FCtx: PSSL_CTX;
    FSsl: PSSL;
    FPassword: AnsiString;
    FCertificateFile: AnsiString;
    FPrivateKeyFile: AnsiString;
    FCertCAFile: AnsiString;
    procedure Listen;
    procedure HTTPWriteLine(const data: RawByteString);
    function SockSend(var Buf; len, flags: Integer): Integer;
    function SockRecv(var Buf; len, flags: Integer): Integer;
  protected
    function getReadyState: TWSReadyState; virtual;
    procedure Send(const data: string);
    procedure Open(const url: string; const origin: RawByteString = 'null');
    procedure Close;
    function GetOnOpen: TProc;
    function GetOnClose: TProc;
    function GetOnError: TWSMessage;
    function GetOnMessage: TWSMessage;
    function GetOnAddField: TOnHTTPAddField;
    procedure SetOnOpen(const value: TProc);
    procedure SetOnClose(const value: TProc);
    procedure SetOnError(const value: TWSMessage);
    procedure SetOnMessage(const value: TWSMessage);
    procedure SetOnAddField(const value: TOnHTTPAddField);
  public
    constructor Create(const password: AnsiString = '';
      const CertificateFile: AnsiString = ''; const PrivateKeyFile: AnsiString = '';
      const CertCAFile: AnsiString = ''); virtual;
    destructor Destroy; override;
    class constructor Create;
    class destructor Destroy;
  end;

implementation
uses Classes, AnsiStrings, dorMD5, dorPunyCode, Generics.Collections;

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


{$REGION 'WEBSOCKET'}

(******************************************************************************)
(* WEBSOCKET                                                                  *)
(******************************************************************************)

function WS_GenerateKeyNumber(out number: Cardinal): string;
var
  spaces, max, n, i, pos: Integer;
  product: Cardinal;
  ch: AnsiChar;
  function bigendian(c: Cardinal): Cardinal;
  var
    i: array[0..3] of Byte absolute c;
    o: array[0..3] of Byte absolute Result;
  begin
    o[0] := i[3];
    o[1] := i[2];
    o[2] := i[1];
    o[3] := i[0];
  end;
const
  RANDCHAR_SIZE = $30 - $21 + $7F - $3A;
begin
	// 16.  Let /spaces_n/ be a random integer from 1 to 12 inclusive.
  spaces := Random(12) + 1;

	// 17. Let /max_n/ be the largest integer not greater than
	//     4,294,967,295 divided by /spaces_n/
	max := Integer(4294967295 div Cardinal(spaces));

	// 18. Let /number_n/ be a random integer from 0 to /max_n/ inclusive.
	number := Random(max + 1);

	// 19. Let /product_n/ be the result of multiplying /number_n/ and
	//     /spaces_n/ together.
	product := number * Cardinal(spaces);

	// 20. Let /key_n/ be a string consisting of /product_n/, expressed
	// in base ten using the numerals in the range U+0030 DIGIT ZERO (0)
	// to U+0039 DIGIT NINE (9).
	Result := inttostr(product);

	// 21. Insert between one and twelve random characters from the ranges
	//     U+0021 to U+002F and U+003A to U+007E into /key_n/ at random
	//     positions.
	n := Random(12) + 1;
	for i := 0 to n - 1 do
  begin
		pos := Random(length(Result)) + 1;
    ch := AnsiChar(Random(RANDCHAR_SIZE) + $21);
    if ch >= '0' then
       inc(ch, 10);
    Result := Copy(Result, 1, pos) + Char(ch) + Copy(Result, pos+1, Length(Result) - pos);
		//Result := key[0:pos] + string(ch) + key[pos:]
	end;

	// 22. Insert /spaces_n/ U+0020 SPACE characters into /key_n/ at random
	//     positions other than the start or end of the string.
	for i := 0 to spaces - 1 do
  begin
		pos := Random(Length(Result)-1) + 1;
    Result := Copy(Result, 1, pos) + ' ' + Copy(Result, pos+1, Length(Result) - pos);
		//key = key[0:pos] + " " + key[pos:]
  end;
  number := bigendian(number)
end;

function SSLPasswordCallback(buffer: PAnsiChar; size, rwflag: Integer;
  this: TWebSocket): Integer; cdecl;
var
  password: AnsiString;
begin
  password := this.FPassword;
  if Length(password) > (Size - 1) then
    SetLength(password, Size - 1);
  Result := Length(password);
  Move(PAnsiChar(password)^, buffer^, Result + 1);
end;

{ TWebSocket }

procedure TWebSocket.Close;
begin
  if FReadyState = rsOpen then
  begin
    FReadyState := rsClosing;
    closesocket(FSocket);
    Sleep(1); // let the listen thread close
    FSocket := INVALID_SOCKET;
    FReadyState := rsClosed;
    if Assigned(FOnClose) then
      FOnClose();
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

procedure TWebSocket.HTTPWriteLine(const data: RawByteString);
var
  rb: RawByteString;
begin
  rb := RawByteString(data) + #13#10;
  SockSend(PAnsiChar(rb)^, Length(rb), 0);
end;

procedure TWebSocket.Open(const url: string; const origin: RawByteString = 'null');
var
  domain: AnsiString;
  protocol: string;
  uri: RawByteString;
  ssl: Boolean;
  port: Word;
  host: PHostEnt;
  addr: TSockAddrIn;
  i: Integer;
  ReadTimeOut: Integer;
  dic: TDictionary<RawByteString, RawByteString>;
  value: RawByteString;
  challenge: packed record
    num1, num2: Cardinal;
    key3: array[0..7] of Byte;
  end;
  md5_expected, md5_returned: array[0..15] of Byte;

begin
  if FReadyState <> rsClosed then
    Exit;

  FReadyState := rsConnecting;
  // parse
  if not HTTPParseURL(PChar(url), protocol, domain, port, uri, True) then
  begin
    if Assigned(FOnError) then
      FOnError(Format('Can''t parse url: %s', [url]));
    Exit;
  end;

  if protocol = 'ws' then
  begin
    ssl := False;
    if port = 0 then
      port := 80;
  end else
    if protocol = 'wss' then
    begin
      ssl := True;
      if port = 0 then
        port := 443;
    end else
      begin
        if Assigned(FOnError) then
          FOnError('Invalid protocol');
        Exit;
      end;

  // find host
  host := gethostbyname(PAnsiChar(domain));
  if host = nil then
  begin
    if Assigned(FOnError) then
      FOnError(Format('Host not found: %s', [domain]));
    Exit;
  end;

  // socket
  FSocket := socket(AF_INET, SOCK_STREAM, 0);
  try
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
    end;

    //uri := HTTPEncode(uri);
    HTTPWriteLine('GET ' + uri + ' HTTP/1.1');
    HTTPWriteLine('Upgrade: WebSocket');
    HTTPWriteLine('Connection: Upgrade');
    HTTPWriteLine('Host: ' + domain);
    HTTPWriteLine('Origin: ' + origin);
    HTTPWriteLine('Sec-WebSocket-Key1: ' + RawbyteString(WS_GenerateKeyNumber(challenge.num1)));
    HTTPWriteLine('Sec-WebSocket-Key2: ' + RawbyteString(WS_GenerateKeyNumber(challenge.num2)));
    if Assigned(FOnAddField) then
      FOnAddField(function (const key: RawByteString; const value: RawByteString): Boolean begin
        HTTPWriteLine(RawbyteString(key) + ': ' + value);
        Result := True;
      end);

  //  writeline('Sec-WebSocket-Protocol: sample');
    HTTPWriteLine('');
    for i := 0 to 7 do
      challenge.key3[i] := Random(256);
    SockSend(challenge.key3, SizeOf(challenge.key3), 0);

    dic := TDictionary<RawByteString, RawByteString>.Create;
    try
      ReadTimeOut := 3000;
      setsockopt(FSocket, SOL_SOCKET, SO_RCVTIMEO, @ReadTimeOut, SizeOf(ReadTimeOut));
      if not HTTPParse(
        function (var buf; len: Integer): Integer
        begin
          Result := SockRecv(buf, len, 0)
        end,
        function (code: Integer; const mesg: RawByteString): Boolean
          begin
            Result := code = 101;
            if not Result and Assigned(FOnError) then
              FOnError(Format('Invalid response code: %d %s', [code, mesg]));
          end,
        function (const key: RawByteString; const value: RawByteString): Boolean
        begin
          dic.AddOrSetValue(lowercase(key), value);
          Result := True;
        end) then
          begin
            if Assigned(FOnError) then
              FOnError('Unexpected error when parsing HTTP header');
            Exit;
          end;

      if not((dic.TryGetValue('upgrade', value) and (value = 'WebSocket')) and
      (dic.TryGetValue('connection', value) and (LowerCase(string(value)) = 'upgrade'))) then
        begin
          if Assigned(FOnError) then
            FOnError('Invalid upgrade header field');
          Exit;
        end;

//      if not (dic.TryGetValue('sec-websocket-origin', value) and (value = 'null')) then
//        begin
//          if Assigned(FOnError) then
//            FOnError('');
//          Exit;
//        end;

      if not (dic.TryGetValue('sec-websocket-location', value) and (value = 'ws://' + domain + uri)) then
        begin
          if Assigned(FOnError) then
            FOnError('Invalid sec-websocket-location header field');
          Exit;
        end;

      MD5(@challenge, SizeOf(challenge), @md5_expected);
      if SockRecv(md5_returned, SizeOf(md5_returned), 0) <> SizeOf(md5_returned) then
      begin
        if Assigned(FOnError) then
          FOnError('Server didn''t send challenge response');
        Exit;
      end;

      if not CompareMem(@md5_expected, @md5_returned, SizeOf(md5_expected)) then
      begin
        if Assigned(FOnError) then
          FOnError('Websocket challenge failed');
        Exit;
      end;

      ReadTimeOut := 0;
      setsockopt(FSocket, SOL_SOCKET, SO_RCVTIMEO, @ReadTimeOut, SizeOf(ReadTimeOut));
    finally
      dic.Free;
    end;

    FReadyState := rsOpen;
    if Assigned(FOnOpen) then
      FOnOpen();

    Listen;
  finally
    if FReadyState = rsConnecting then
    begin
      closesocket(FSocket);
      FSocket := INVALID_SOCKET;
      FReadyState := rsClosed;
    end;
  end;
end;

class constructor TWebSocket.Create;
var
  Data: TWSAData;
begin
  WSAStartup($0202, Data);
end;

destructor TWebSocket.Destroy;
begin
  Close;
  inherited;
end;

constructor TWebSocket.Create(const password, CertificateFile,
  PrivateKeyFile, CertCAFile: AnsiString);
begin
  inherited Create;
  FReadyState := rsClosed;
  FSocket := INVALID_SOCKET;
  FCtx := nil;
  FSsl := nil;
  FPassword := password;
  FCertificateFile := CertificateFile;
  FPrivateKeyFile := PrivateKeyFile;
  FCertCAFile := CertCAFile;
end;

function TWebSocket.getReadyState: TWSReadyState;
begin
  Result := FReadyState;
end;

procedure TWebSocket.Listen;
begin
  TThreadIt.Create(procedure
    var
      c: AnsiChar;
      st: Boolean;
      rb: RawByteString;
    begin
      st := True;
      while (FReadyState = rsOpen) and (SockRecv(c, 1, 0) = 1) do
      begin
        if st then
          case c of
            #0:
              begin
                rb := '';
                st := False;
              end;
          else
            Break;
          end
        else
          case c of
            #$FF:
              if FReadyState = rsOpen then
              begin
                TThread.Synchronize(nil, procedure begin
                  if Assigned(FOnMessage) then
                    FOnMessage(string(UTF8ToString(rb)));
                end);
                st := True;
              end;
          else
            rb := rb + c;
          end
      end;
      if FReadyState = rsOpen then // remotely closed
        TThread.Synchronize(nil, procedure begin
          Close;
        end);
    end);
end;

procedure TWebSocket.Send(const data: string);
var
  rb: RawByteString;
begin
  rb := #$00 + RawByteString(UTF8String(data)) + #$FF;
  SockSend(PAnsiChar(rb)^, Length(rb), 0);
end;

procedure TWebSocket.SetOnAddField(const value: TOnHTTPAddField);
begin
  FOnAddField := value;
end;

procedure TWebSocket.SetOnClose(const value: TProc);
begin
  FOnClose := value;
end;

procedure TWebSocket.SetOnError(const value: TWSMessage);
begin
  FOnError := value;
end;

procedure TWebSocket.SetOnMessage(const value: TWSMessage);
begin
  FOnMessage := value;
end;

procedure TWebSocket.SetOnOpen(const value: TProc);
begin
  FOnOpen := value;
end;

function TWebSocket.SockSend(var Buf; len, flags: Integer): Integer;
begin
  if FSsl <> nil then
    Result := SSL_write(FSsl, @Buf, len) else
    Result := WinSock.send(FSocket, Buf, len, flags);
end;

function TWebSocket.SockRecv(var Buf; len, flags: Integer): Integer;
begin
  if FSsl <> nil then
    Result := SSL_read(FSsl, @Buf, len) else
    Result := recv(FSocket, Buf, len, flags);
end;

class destructor TWebSocket.Destroy;
begin
  WSACleanup;
end;

function TWebSocket.GetOnAddField: TOnHTTPAddField;
begin
  Result := FOnAddField;
end;

function TWebSocket.GetOnClose: TProc;
begin
  Result := FOnClose;
end;

function TWebSocket.GetOnError: TWSMessage;
begin
  Result := FOnError;
end;

function TWebSocket.GetOnMessage: TWSMessage;
begin
  Result := FOnMessage;
end;

function TWebSocket.GetOnOpen: TProc;
begin
  Result := FOnOpen;
end;

{$ENDREGION}
end.

