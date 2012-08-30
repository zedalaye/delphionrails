unit dorWebsocket;

interface
uses SysUtils, WinSock, Classes, SyncObjs, dorHTTP, dorOpenSSL;

type
  TWSMessage = reference to procedure(const msg: string);
  TWSBinary = reference to procedure(stream: TStream);
  TWSReadyState = (rsConnecting, rsOpen, rsClosing, rsClosed);

  IWebSocket = interface
    ['{A9BC9CE6-7C13-4B02-BF66-033EB0BB943E}']
    function GetReadyState: TWSReadyState;
    procedure Send(const data: string);
    procedure Ping(const data: string);
    procedure Close;
    procedure Open(const url: string; autoPong: Boolean = True; const origin: RawByteString = 'null');

    function GetOnOpen: TProc;
    function GetOnClose: TProc;
    function GetOnError: TWSMessage;
    function GetOnMessage: TWSMessage;
    function GetOnPing: TWSMessage;
    function GetOnPong: TWSMessage;
    function GetOnBinaryData: TWSBinary;
    function GetOnAddField: TOnHTTPAddField;

    procedure SetOnOpen(const value: TProc);
    procedure SetOnClose(const value: TProc);
    procedure SetOnError(const value: TWSMessage);
    procedure SetOnMessage(const value: TWSMessage);
    procedure SetOnPing(const value: TWSMessage);
    procedure SetOnPong(const value: TWSMessage);
    procedure SetOnBinaryData(const value: TWSBinary);
    procedure SetOnAddField(const value: TOnHTTPAddField);

    property OnOpen: TProc read GetOnOpen write SetOnOpen;
    property OnClose: TProc read GetOnClose write SetOnClose;
    property OnError: TWSMessage read GetOnError write SetOnError;
    property OnMessage: TWSMessage read GetOnMessage write SetOnMessage;
    property OnPing: TWSMessage read GetOnPing write SetOnPing;
    property OnPong: TWSMessage read GetOnPong write SetOnPong;
    property OnBinaryData: TWSBinary read GetOnBinaryData write SetOnBinaryData;

    property OnAddField: TOnHTTPAddField read GetOnAddField write SetOnAddField;

    property ReadyState: TWSReadyState read getReadyState;
  end;

  TWebSocket = class(TInterfacedObject, IWebSocket)
  private
    FOnOpen: TProc;
    FOnClose: TProc;
    FOnError: TWSMessage;
    FOnMessage: TWSMessage;
    FOnPing: TWSMessage;
    FOnPong: TWSMessage;
    FOnBinaryData: TWSBinary;
    FOnAddField: TOnHTTPAddField;
    FReadyState: TWSReadyState;
    FSocket: TSocket;
    FLockSend: TCriticalSection;
    FAutoPong: Boolean;
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
    procedure Output(b: Byte; data: Pointer; len: Int64);
    procedure OutputString(b: Byte; const str: string);
  protected
    function getReadyState: TWSReadyState; virtual;
    procedure Send(const data: string);
    procedure Ping(const data: string);
    procedure Open(const url: string; autoPong: Boolean; const origin: RawByteString = 'null');
    procedure Close;
    function GetOnOpen: TProc;
    function GetOnClose: TProc;
    function GetOnError: TWSMessage;
    function GetOnMessage: TWSMessage;
    function GetOnPing: TWSMessage;
    function GetOnPong: TWSMessage;
    function GetOnBinaryData: TWSBinary;
    function GetOnAddField: TOnHTTPAddField;
    procedure SetOnOpen(const value: TProc);
    procedure SetOnClose(const value: TProc);
    procedure SetOnError(const value: TWSMessage);
    procedure SetOnMessage(const value: TWSMessage);
    procedure SetOnPing(const value: TWSMessage);
    procedure SetOnPong(const value: TWSMessage);
    procedure SetOnBinaryData(const value: TWSBinary);
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
uses AnsiStrings, dorMD5, dorPunyCode, Generics.Collections, dorUtils;

const
  // Non Control Frames
  OPContinuation = $0;
  OPText =         $1;
  OPBinary =       $2;

  // Control Frames
  OPClose =        $8;
  OPPing =         $9;
  OPPong =         $A;

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

procedure TWebSocket.Open(const url: string; autoPong: Boolean; const origin: RawByteString);
var
  domain: AnsiString;
  protocol: string;
  uri: RawByteString;
  ssl: Boolean;
  port: Word;
  host: PHostEnt;
  addr: TSockAddrIn;
  ReadTimeOut: Integer;
  dic: TDictionary<RawByteString, RawByteString>;
  value: RawByteString;
  key: RawByteString;
  guid: TGUID;
  buffer: array[0..SHA_DIGEST_LENGTH - 1] of AnsiChar;
begin
  FAutoPong := autoPong;

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
    HTTPWriteLine('sec-websocket-version: 13');
    CreateGUID(guid);
    key := RawByteString(BytesToBase64(@guid, SizeOf(guid)));
    HTTPWriteLine('sec-websocket-key: ' + key);

    key := AnsiString(key) + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
    SHA1(PAnsiChar(key), Length(key), @buffer);
    key := RawByteString(BytesToBase64(@buffer, SizeOf(buffer)));

    if Assigned(FOnAddField) then
      FOnAddField(function (const key: RawByteString; const value: RawByteString): Boolean begin
        HTTPWriteLine(RawbyteString(key) + ': ' + value);
        Result := True;
      end);
    HTTPWriteLine('');

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

      if not((dic.TryGetValue('upgrade', value) and (LowerCase(string(value)) = 'websocket')) and
      (dic.TryGetValue('connection', value) and (LowerCase(string(value)) = 'upgrade'))) then
        begin
          if Assigned(FOnError) then
            FOnError('Invalid upgrade header field');
          Exit;
        end;

      if not dic.TryGetValue('sec-websocket-accept', value) then
        begin
          if Assigned(FOnError) then
            FOnError('Invalid Sec-WebSocket-Accept header field');
          Exit;
        end;

      if key <> value then
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

procedure TWebSocket.Output(b: Byte; data: Pointer; len: Int64);
var
  lenarray: array[0..7] of Byte absolute len;
  d: Cardinal;
  p: Pointer;
  g: TGUID;
begin
  FLockSend.Enter;
  try
    SockSend(b, 1, 0);
    if len < 126 then
    begin
      b := len or $80;
      SockSend(b, 1, 0);
    end else
      if len < High(Word) then
      begin
        b := 126 or $80;
        SockSend(b, 1, 0);
        SockSend(lenarray[1], 1, 0);
        SockSend(lenarray[0], 1, 0);
      end else
      begin
        b := 127 or $80;
        SockSend(b, 1, 0);
        SockSend(lenarray[7], 1, 0);
        SockSend(lenarray[6], 1, 0);
        SockSend(lenarray[5], 1, 0);
        SockSend(lenarray[4], 1, 0);
        SockSend(lenarray[3], 1, 0);
        SockSend(lenarray[2], 1, 0);
        SockSend(lenarray[1], 1, 0);
        SockSend(lenarray[0], 1, 0);
      end;

    CreateGUID(g); // entropy
    SockSend(g.D1, SizeOf(g.D1), 0);
    p := data;
    while len >= 4 do
    begin
      d := Cardinal(p^) xor g.D1;
      SockSend(d, SizeOf(d), 0);
      Inc(NativeInt(p), 4);
      Dec(len, 4);
    end;

    if len > 0 then
    begin
      Move(p^, d, len);
      d := d xor g.D1;
      SockSend(d, len, 0);
    end;
  finally
    FLockSend.Leave;
  end;
end;

procedure TWebSocket.OutputString(b: Byte; const str: string);
var
  utf8: UTF8String;
begin
  utf8 := UTF8String(str);
  Output(b, PAnsiChar(utf8), Length(utf8));
end;

procedure TWebSocket.Ping(const data: string);
begin
  OutputString($80 or OPPing, data)
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
  FLockSend.Free;
  inherited;
end;

constructor TWebSocket.Create(const password, CertificateFile,
  PrivateKeyFile, CertCAFile: AnsiString);
begin
  inherited Create;
  FLockSend := TCriticalSection.Create;
  FReadyState := rsClosed;
  FSocket := INVALID_SOCKET;
  FCtx := nil;
  FSsl := nil;
  FPassword := password;
  FCertificateFile := CertificateFile;
  FPrivateKeyFile := PrivateKeyFile;
  FCertCAFile := CertCAFile;
  FAutoPong := True;
end;

function TWebSocket.getReadyState: TWSReadyState;
begin
  Result := FReadyState;
end;

procedure TWebSocket.Listen;
begin
  TThreadIt.Create(procedure
    type
      TState = (stStart, stNext, stPayload16, stPayload64, stMask, stData);
    var
      b, opcode: Byte;
      closecode: Word;
      state: TState;
      fin, havemask: Boolean;
      payloadLength: Int64;
      pos: Integer;
      mask: array[0..3] of Byte;
      stream: TPooledMemoryStream;
      data: UTF8String;
      procedure EndMask;
      begin
        if payloadLength > 0 then
        begin
          state := stData;
          pos := 0;
        end else
          state := stStart;
      end;
    begin
      state := stStart;
      pos := 0;
      payloadLength := 0;
      opcode := 0;
      fin := False;
      closecode := 0;
      havemask := False;

      stream := TPooledMemoryStream.Create;
      try
        while (FReadyState = rsOpen) and (SockRecv(b, 1, 0) = 1) do
        begin
          case state of
            stStart:
              begin
                fin := (b and $80) <> 0;
                if (b and $70) <> 0 then Exit; // reserved
                opcode := b and $0F;
                closecode := 0;
                state := stNext;
              end;
            stNext:
              begin
                havemask := b and $80 = 1;
                payloadLength := b and $7F;

                if (payloadLength < 126) then
                begin
                  if havemask then
                    state := stMask else
                    EndMask;
                  pos := 0;
                end else
                if (payloadLength = 126) then
                begin
                  pos := 0;
                  state := stPayload16;
                end  else
                begin
                  pos := 0;
                  state := stPayload64;
                end;
              end;
            stPayload16:
              begin
                case pos of
                  0: payloadLength := b;
                  1:
                    begin
                      payloadLength := payloadLength shl 8 or b;
                      if havemask then
                        state := stMask else
                        EndMask;
                      pos := 0;
                      Continue;
                    end;
                end;
                Inc(pos);
              end;
            stPayload64:
              begin
                case pos of
                  0   : payloadLength := b;
                  1..6: payloadLength := payloadLength shl 8 or b;
                  7:
                    begin
                      payloadLength := payloadLength shl 8 or b;
                      if havemask then
                        state := stMask else
                        EndMask;
                      pos := 0;
                      Continue
                    end;
                end;
                Inc(pos);
              end;
            stMask:
              case pos of
                0..2:
                  begin
                    mask[pos] := b;
                    Inc(pos);
                  end;
                3:
                  begin
                    mask[3] := b;
                    EndMask;
                  end;
              end;
            stData:
              begin
                if havemask then
                  b := b xor mask[pos mod 4];
                case opcode of
                  OPClose: closecode := closecode shl 8 or b;
                else
                  stream.Write(b, 1);
                end;

                Dec(payloadLength);
                Inc(pos);

                if (payloadLength = 0) then
                begin
                  if fin and (opcode <> OPContinuation) and (FReadyState = rsOpen) then
                  begin
                    case opcode of
                      OPClose: Break;
                      OPText:
                        begin
                          SetLength(data, stream.Size);
                          stream.Seek(0, soFromBeginning);
                          stream.Read(PAnsiChar(data)^, stream.Size);
                          TThread.Synchronize(nil, procedure begin
                            if Assigned(FOnMessage) then
                              FOnMessage(string(data));
                          end);
                        end;
                      OPPing:
                        begin
                          SetLength(data, stream.Size);
                          stream.Seek(0, soFromBeginning);
                          stream.Read(PAnsiChar(data)^, stream.Size);
                          TThread.Synchronize(nil, procedure begin
                            if FAutoPong then
                              Output($80 or OPPong, PAnsiChar(data), Length(data));
                            if Assigned(FOnPing) then
                              FOnPing(string(data));
                          end);
                        end;
                      OPPong:
                        begin
                          SetLength(data, stream.Size);
                          stream.Seek(0, soFromBeginning);
                          stream.Read(PAnsiChar(data)^, stream.Size);
                          TThread.Synchronize(nil, procedure begin
                            if Assigned(FOnPong) then
                              FOnPong(string(data));
                          end);
                        end;
                      OPBinary:
                        begin
                          stream.Seek(0, soFromBeginning);
                          TThread.Synchronize(nil, procedure begin
                            if Assigned(FOnBinaryData) then
                              FOnBinaryData(stream);
                            stream.Free;
                          end);
                          stream := TPooledMemoryStream.Create;
                        end;
                    end;
                    stream.Size := 0;
                  end;
                  state := stStart;
                end;
              end;
          end;
        end;
      finally
        stream.Free;
      end;
      if FReadyState = rsOpen then // remotely closed
        TThread.Synchronize(nil, procedure begin
          Close;
        end);
    end);
end;

procedure TWebSocket.Send(const data: string);
begin
  OutputString($80 or OPText, data)
end;

procedure TWebSocket.SetOnAddField(const value: TOnHTTPAddField);
begin
  FOnAddField := value;
end;

procedure TWebSocket.SetOnBinaryData(const value: TWSBinary);
begin
  FOnBinaryData := value;
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

procedure TWebSocket.SetOnPing(const value: TWSMessage);
begin
  FOnPing := value;
end;

procedure TWebSocket.SetOnPong(const value: TWSMessage);
begin
  FOnPong := value;
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

function TWebSocket.GetOnBinaryData: TWSBinary;
begin
  Result := FOnBinaryData;
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

function TWebSocket.GetOnPing: TWSMessage;
begin
  Result := FOnPing;
end;

function TWebSocket.GetOnPong: TWSMessage;
begin
  Result := FOnPOng;
end;

{$ENDREGION}
end.

