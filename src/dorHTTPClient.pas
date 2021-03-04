unit dorHTTPClient;

interface

uses
  WinSock2,
  SysUtils, Classes, AnsiStrings, Generics.Collections,
  dorUtils;

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

  TCookie = record
    path: RawByteString;
    domain: RawByteString;
    value: RawByteString;
    expires: RawByteString;
    max_age: Integer;
  end;

  THeaderCollection = TDictionary<RawByteString, THTTPHeader>.TValueCollection;

  EHTTPRequest = Exception;

  IHTTPRequest = interface
    ['{105BFAD3-A4AE-459E-8EA6-377E9E065827}']
    function Open(const method: RawByteString; const url: string; async: Boolean = False;
      const user: string = ''; const password: string = ''; urlencode: Boolean = True): Boolean;
    procedure Abort;

    function Send(data: TStream = nil): Boolean;
    function SendText(const data: string; encoding: TEncoding = nil): Boolean;
    function SendFile(const FileName: string): Boolean;

    procedure ClearRequestHeaders;

    function GetCookie(const Name: RawByteString): TCookie;
    function GetDownloadRate: Cardinal;
    function GetFollowRedirect: Boolean;
    function GetOnReadyStateChange: TOnReadyStateChange;
    function GetReadyState: TReadyState;
    function GetRequestHeader(const header: RawByteString): RawByteString;
    function GetRequestHeaders: THeaderCollection;
    function GetResetDataStream: Boolean;
    function GetResponseHeader(const header: RawByteString): RawByteString;
    function GetResponseHeaders: THeaderCollection;
    function GetResponseStream: TStream;
    function GetResponseText: string;
    function GetStatus: Word;
    function GetStatusText: RawByteString;
    function GetSynchronize: Boolean;
    function GetTimeout: Cardinal;
    function GetUploadRate: Cardinal;

    procedure SetCookie(const Name: RawByteString; Value: TCookie);
    procedure SetDownloadRate(value: Cardinal);
    procedure SetFollowRedirect(value: Boolean);
    procedure SetOnReadyStateChange(const ready: TOnReadyStateChange);
    procedure SetRequestHeader(const header, value: RawByteString);
    procedure SetResetDataStream(const value: Boolean);
    procedure SetResponseStream(stream: TStream);
    procedure SetSynchronize(value: Boolean);
    procedure SetTimeout(value: Cardinal);
    procedure SetUploadRate(value: Cardinal);

    property Cookie[const name: RawByteString]: TCookie read GetCookie write SetCookie;
    property DownloadRate: Cardinal read GetDownloadRate write SetDownloadRate;
    property FollowRedirect: Boolean read GetFollowRedirect write SetFollowRedirect;
    property OnReadyStateChange: TOnReadyStateChange read GetOnReadyStateChange write SetOnReadyStateChange;
    property ReadyState: TReadyState read GetReadyState;
    property RequestHeader[const header: RawByteString]: RawByteString read GetRequestHeader write SetRequestHeader;
    property ResetDataStream: Boolean read GetResetDataStream write SetResetDataStream;
    property ResponseHeader[const header: RawByteString]: RawByteString read GetResponseHeader;
    property ResponseStream: TStream read GetResponseStream write SetResponseStream;
    property ResponseText: string read GetResponseText;
    property Status: Word read GetStatus;
    property StatusText: RawByteString read GetStatusText;
    property Synchronize: Boolean read GetSynchronize write SetSynchronize;
    property Timeout: Cardinal read GetTimeout write SetTimeout;
    property UploadRate: Cardinal read GetUploadRate write SetUploadRate;
  end;

  THTTPRequest = class(TInterfacedObject, IHTTPRequest)
  private
    const BUFFER_SIZE = 1024;
  private
  type
    TOnHeaderEvent = reference to function(const value: RawByteString): Boolean;
    TContentEncoding = (
      encUnknown,
      encDeflate,
      encGZIP);
    TThreadAsync = class(TThread)
    private
      FThis: IHTTPRequest;
      FData: TPooledMemoryStream;
      FTimeOut: Cardinal;
      FUploadRate: Cardinal;
      FDownloadRate: Cardinal;
    protected
      procedure Execute; override;
    public
      constructor Create(const this: IHTTPRequest; data: TStream; TimeOut, DownloadRate, UploadRate: Cardinal);
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
    FRequestHeaders: TDictionary<RawByteString, THTTPHeader>;
    FResponseHeaders: TDictionary<RawByteString, THTTPHeader>;
    FResponseEvents: TDictionary<RawByteString, TOnHeaderEvent>;
    FCookies: TDictionary<RawByteString, TCookie>;

    FStatus: Word;
    FStatusText: RawByteString;
    FOnReadyStateChange: TOnReadyStateChange;
    FReadyState: TReadyState;
    FCharsets: TDictionary<RawByteString, Integer>;
    FReadError: Boolean;
    FWriteError: Boolean;
    FRedirectCount: Integer;

    FSynchronize: Boolean;

    // SSL
    FCtx: Pointer;
    FSsl: Pointer;
    FSSLPassword: AnsiString;
    FCertificateFile: AnsiString;
    FPrivateKeyFile: AnsiString;
    FCertCAFile: AnsiString;
    FTLSHostname: AnsiString;

    // Options
    FDownloadRate: Cardinal;
    FUploadRate: Cardinal;
    FTimeout: Cardinal;
    FFollowRedirect: Boolean;
    FResetDataStream: Boolean;

    // Buffer
    FBuffer: array[0..BUFFER_SIZE - 1] of Byte;
    FBufferPos: Cardinal;

    procedure SetResponseHeader(const header, value: RawByteString);
    procedure HTTPWriteLine(const data: RawByteString);
    function SockSend(var Buf; len: Integer): Integer;
    function SockRecv(var Buf; len: Integer): Integer;
    procedure Flush;
    procedure LoadDefaultHeaders;
    procedure LoadCharsets;
    procedure LoadEvents;

    procedure SetReadyState(ready: TReadyState);
    function Receive(TimeOut, DownloadRate: Cardinal): Boolean;
    procedure SendHeaders(data: TStream; UploadRate: Cardinal);
    function TCPConnect(const domain: RawByteString; port: Word; ssl: Boolean): Boolean;
    procedure TCPDisconnect;
    function TCPReconnect: Boolean;

    function InternalSend(data: TSTream; TimeOut, DownloadRate, UploadRate: Cardinal): Boolean;
    function InternalOpen(const method: RawByteString; const url: string;
      async: Boolean; const user, password: string; urlencode: Boolean): Boolean;
    procedure InternalSetRequestHeader(const header, value: RawByteString);
    function IsRedirecting: Boolean;
  private
    class constructor Create;
    class destructor Destroy;
  protected
    function GetCookie(const Name: RawByteString): TCookie;
    function GetDownloadRate: Cardinal;
    function GetFollowRedirect: Boolean;
    function GetOnReadyStateChange: TOnReadyStateChange;
    function GetReadyState: TReadyState;
    function GetRequestHeader(const header: RawByteString): RawByteString;
    function GetRequestHeaders: THeaderCollection;
    function GetResetDataStream: Boolean;
    function GetResponseHeader(const header: RawByteString): RawByteString;
    function GetResponseHeaders: THeaderCollection;
    function GetResponseStream: TStream;
    function GetResponseText: string;
    function GetStatus: Word;
    function GetStatusText: RawByteString;
    function GetSynchronize: Boolean;
    function GetTimeout: Cardinal;
    function GetUploadRate: Cardinal;

    procedure SetCookie(const Name: RawByteString; Value: TCookie);
    procedure SetDownloadRate(value: Cardinal);
    procedure SetFollowRedirect(value: Boolean);
    procedure SetOnReadyStateChange(const ready: TOnReadyStateChange);
    procedure SetRequestHeader(const header, value: RawByteString);
    procedure SetResetDataStream(const value: Boolean);
    procedure SetResponseStream(stream: TStream);
    procedure SetSynchronize(value: Boolean);
    procedure SetTimeout(value: Cardinal);
    procedure SetUploadRate(value: Cardinal);
  public
    constructor Create(const SSLPassword: AnsiString = ''; const CertificateFile: AnsiString = '';
      const PrivateKeyFile: AnsiString = ''; const CertCAFile: AnsiString = '';
      const TLSHostname: AnsiString = ''); virtual;
    destructor Destroy; override;

    function Open(const method: RawByteString; const url: string; async: Boolean;
      const user, password: string; urlencode: Boolean = True): Boolean;
    procedure Abort;

    procedure ClearRequestHeaders;

    function Send(data: TStream): Boolean;
    function SendText(const data: string; encoding: TEncoding): Boolean;
    function SendFile(const FileName: string): Boolean;
  end;

implementation

uses
  Windows, Math, ZLib, dorOpenSSL, dorHTTP;

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
  if FResponseDataOwned and (FResponseData <> nil) then
    FResponseData.Size := 0;
  LoadDefaultHeaders;
  FResponseHeaders.Clear;
  FStatus := 0;
  FStatusText := '';
  FCookies.Clear;
end;

constructor THTTPRequest.Create(const SSLPassword: AnsiString = '';
  const CertificateFile: AnsiString = ''; const PrivateKeyFile: AnsiString = '';
  const CertCAFile: AnsiString = ''; const TLSHostname: AnsiString = '');
begin
  FSocket := INVALID_SOCKET;
  FPort := 80;

  FTimeout := 0;
  FDownloadRate := 0;
  FUploadRate := 0;
  FBufferPos := 0;
  FFollowRedirect := True;
  FResetDataStream := True;
  FSynchronize := True;

  FCtx := nil;
  FSsl := nil;
  FSSLPassword := SSLPassword;
  FCertificateFile := CertificateFile;
  FPrivateKeyFile := PrivateKeyFile;
  FCertCAFile := CertCAFile;
  FTLSHostname := TLSHostname;

  FResponseData := nil;
  FResponseDataOwned := False;

  FRequestHeaders  := TDictionary<RawByteString, THTTPHeader>.Create;
  FResponseHeaders := TDictionary<RawByteString, THTTPHeader>.Create;
  FResponseEvents  := TDictionary<RawByteString, TOnHeaderEvent>.Create;

  FCookies := TDictionary<RawByteString, TCookie>.Create;
  FCharsets := TDictionary<RawByteString, Integer>.Create;

  LoadCharsets;
  LoadEvents;

  Abort; // reset
end;

destructor THTTPRequest.Destroy;
begin
  TCPDisconnect;
  if (FResponseDataOwned) and (FResponseData <> nil) then
    FResponseData.Free;
  FRequestHeaders.Free;
  FResponseHeaders.Free;
  FCharsets.Free;
  FResponseEvents.Free;
  FCookies.Free;

  inherited;
end;

function THTTPRequest.GetCookie(const name: RawByteString): TCookie;
begin
  if not FCookies.TryGetValue(name, Result) then
  begin
    Result.path := '';
    Result.domain := '';
    Result.value := '';
    Result.expires := '';
    Result.max_age := 0;
  end;
end;

function THTTPRequest.GetDownloadRate: Cardinal;
begin
  Result := FDownloadRate;
end;

function THTTPRequest.GetFollowRedirect: Boolean;
begin
  Result := FFollowRedirect;
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
  if FRequestHeaders.TryGetValue(LowerCase(header), rec) then
    Result := rec.value
  else
    Result := '';
end;

function THTTPRequest.GetRequestHeaders: THeaderCollection;
begin
  Result := FRequestHeaders.Values;
end;

function THTTPRequest.GetResetDataStream: Boolean;
begin
  Result := FResetDataStream;
end;

function THTTPRequest.GetResponseHeader(const header: RawByteString): RawByteString;
var
  rec: THTTPHeader;
begin
  if (FReadyState >= rsReceiving) and FResponseHeaders.TryGetValue(LowerCase(header), rec) then
    Result := rec.value
  else
    Result := '';
end;

function THTTPRequest.GetResponseHeaders: THeaderCollection;
begin
  Result := FResponseHeaders.Values;
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
    if FResponseHeaders.TryGetValue('content-type', contenttype) then
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
        end
        else
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

function THTTPRequest.GetTimeout: Cardinal;
begin
  Result := FTimeout;
end;

function THTTPRequest.GetUploadRate: Cardinal;
begin
  Result := FUploadRate;
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
      FPath := HTTPEncode(url)
    else
      FPath := RawbyteString(url);
  end
  else
  begin
    if not HTTPParseURL(PChar(url), Protocol, Domain, Port, FPath, urlencode) then
      Exit(False);

    if SameText(Protocol, 'http') then
    begin
      ssl := False;
      if Port = 0 then
        Port := 80;
    end
    else if SameText(Protocol, RawbyteString('https')) then
    begin
      ssl := True;
      if Port = 0 then
        Port := 443;
    end
    else
      Exit(False);

    if (FSocket <> INVALID_SOCKET) then
    begin
      if (FDomain = Domain) and (FPort = Port) and (ssl = (FSsl <> nil)) then
        goto keepsocket
      else
        Abort;
    end;

    if not TCPConnect(Domain, Port, ssl) then
      goto error;

    FProtocol := Protocol;
    FDomain := Domain;
    FPort := Port;
    LoadDefaultHeaders;
  end;

keepsocket:
  FAsync := async;
  FUser := user;
  FPassword := password;
  FMethod := method;
  FStatus := 0;
  FStatusText := '';
  SetReadyState(rsOpen);
  Exit(True);
error:
  Abort;
  Result := False;
end;

function THTTPRequest.InternalSend(data: TSTream; TimeOut, DownloadRate, UploadRate: Cardinal): Boolean;
var
  str: THTTPHeader;
begin
  SendHeaders(data, UploadRate);
  if FWriteError then
    if TCPReconnect then
      SendHeaders(data, UploadRate);

  Result := Receive(TimeOut, DownloadRate);
  if FReadError then
    if TCPReconnect then
    begin
      SendHeaders(data, UploadRate);
      Result := Receive(TimeOut, DownloadRate);
    end;

  // Redirect ?
  if Result and IsRedirecting and FResponseHeaders.TryGetValue('location', str) then
  begin
    Inc(FRedirectCount);
    if FRedirectCount > 10 then
      raise EHTTPRequest.Create('Too many redirections.');

    Result := InternalOpen(FMethod, string(str.value), FAsync, FUser, FPassword, False);
    if Result then
      Result := InternalSend(data, TimeOut, DownloadRate, UploadRate);
  end;
end;

procedure THTTPRequest.InternalSetRequestHeader(const header,
  value: RawByteString);
begin
  FRequestHeaders.AddOrSetValue(LowerCase(header), THTTPHeader.Make(header, value));
end;

function THTTPRequest.IsRedirecting: Boolean;
begin
  Result := FFollowRedirect and ((FStatus = 201) or (FStatus = 301) or (FStatus = 302));
end;

function THTTPRequest.Open(const method: RawByteString; const url: string; async: Boolean;
  const user, password: string; urlencode: Boolean = True): Boolean;
begin
  if not (FReadyState in [rsUninitialized, rsLoaded]) then
    raise EHTTPRequest.Create('Connection is not ready.');
  Result := InternalOpen(method, url, async, user, password, urlencode);
end;

function THTTPRequest.Receive(TimeOut, DownloadRate: Cardinal): Boolean;
var
  str: THTTPHeader;
  len, rcv: Integer;
  buff: array[0..1023] of AnsiChar;
  strm: TStream;
  encoding: TContentEncoding;
  t: DWORD; // linux: timeval
  Total, Rate, Start, Curr, Freq: Int64;
  wait: TProc<Integer>;
begin

  wait := procedure(l: Integer)
          begin
            if DownloadRate > 0 then
            begin
              Inc(Total, l);
              while True do
              begin
                QueryPerformanceCounter(Curr);
                Rate := Round((Total / 1024) / ((Curr - Start) / Freq));
                if Rate < DownloadRate then
                  Break
                else
                  SleepEx(1000, True);
              end;
            end;
          end;

  try
    FReadError := False;
    strm := nil;
    if FResponseData = nil then
    begin
      FResponseData := TPooledMemoryStream.Create;
      FResponseDataOwned := True;
    end
    else
      FResponseData.Size := 0;
    FResponseHeaders.Clear;

    t := TimeOut * 1000;
    setsockopt(FSocket, SOL_SOCKET, SO_RCVTIMEO, @t, SizeOf(t));

    if not HTTPParse(
      function (var buf; len: Integer): Integer
      begin
        wait(len);
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
      end)
    then
      Exit(False);

    if FResponseHeaders.TryGetValue('content-encoding', str) then
    begin
      if AnsiStrings.SameText(str.value, 'deflate') then
      begin
        encoding := encDeflate;
      end
      else if AnsiStrings.SameText(str.value, 'gzip') then
        encoding := encGZIP
      else
        encoding := encUnknown;
    end
    else
      encoding := encUnknown;

    if encoding = encUnknown then
      strm := FResponseData
    else
      strm := TPooledMemoryStream.Create;

    Total := 0;
    if DownloadRate > 0 then
    begin
      QueryPerformanceFrequency(Freq);
      QueryPerformanceCounter(Start);
    end;

    if FResponseHeaders.TryGetValue('transfer-encoding', str) and AnsiStrings.SameText(str.value, 'chunked') then
    begin
      if not HTTPReadChunked(
        function (var buf; len: Integer): Integer
        begin
          wait(len);
          Result := SockRecv(buf, len);
        end,
        function (var buf; len: Integer): Integer
        begin
          SetReadyState(rsReceiving);
          Result := strm.Write(buf, len);
        end)
      then
        Exit(False);
    end
    else if FResponseHeaders.TryGetValue('content-length', str) and
      TryStrToInt(string(str.value), len) and (len > 0) then
    begin
      while len > 0 do
      begin
        SetReadyState(rsReceiving);
        if len >= SizeOf(buff) then
        begin
          wait(SizeOf(buff));
          rcv := SockRecv(buff, SizeOf(buff));
          if rcv <> SizeOf(buff) then
            Exit(False);
        end
        else
        begin
          wait(len);
          rcv := SockRecv(buff, len);
          if rcv <> len then
            Exit(False);
        end;
        strm.Write(buff, rcv);
        Dec(len, rcv);
      end;
    end
    else if FResponseHeaders.TryGetValue('connection', str) and (string(str.value) = 'close') then
    begin
      repeat
        SetReadyState(rsReceiving);
        wait(SizeOf(buff));
        rcv := SockRecv(buff, SizeOf(buff));
        strm.Write(buff, rcv);
      until rcv = 0;
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
  finally
    wait := nil;
  end;
end;

function THTTPRequest.Send(data: TStream): Boolean;
begin
  if FReadyState <> rsOpen then
    raise EHTTPRequest.Create('Socket is not open.');
  FRedirectCount := 0;
  if FAsync then
  begin
    TThreadAsync.Create(Self, data, FTimeOut, FDownloadRate, FUploadRate);
    Result := True;
  end
  else
  begin
    Result := InternalSend(data, FTimeOut, FDownloadRate, FUploadRate);
    SetReadyState(rsLoaded);
  end;
end;

function THTTPRequest.SendFile(const FileName: string): Boolean;
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := Send(stream);
  finally
    stream.Free;
  end;
end;

procedure THTTPRequest.SendHeaders(data: TStream; UploadRate: Cardinal);
var
  pair: THTTPHeader;
  cook: TPair<RawByteString, TCookie>;
  cookie: RawByteString;
  cookiecount: Integer;
  buffer: array[0..1023] of AnsiChar;
  read: Integer;
  encoding: string;
  compressed: TPooledMemoryStream;
  Total, Rate, Start, Curr, Freq: Int64;
begin
  FWriteError := False;
  HTTPWriteLine(FMethod + ' ' + FPath + ' HTTP/1.1');

  if ((FSsl = nil) and (FPort <> 80)) or ((FSsl <> nil) and (FPort <> 443)) then
    HTTPWriteLine('Host: ' + FDomain + ':' + RawByteString(IntToStr(FPort)))
  else
    HTTPWriteLine('Host: ' + FDomain);

  for pair in FRequestHeaders.Values do
    HTTPWriteLine(pair.name + ': ' + pair.value);

  cookiecount := 0;
  for cook in FCookies do
    if Pos(cook.Value.path, FPath) = 1 then
    begin
      if cookiecount > 0 then
        cookie := cookie + '; '
      else
        cookie := 'Cookie: ';
      cookie := cookie + cook.Key + '=' + cook.Value.value;
      Inc(cookiecount);
    end;
  if cookiecount > 0 then
    HTTPWriteLine(cookie);

  if (FUser <> '') and (FPassword <> '') then
    HTTPWriteLine('Authorization: Basic ' + RawByteString(StrTobase64(FUser + ':' + FPassword)));

  if (data <> nil) and (data.Size >= 0) then
  begin
    if FResetDataStream then
      data.Seek(0, soFromBeginning);

    encoding := string(GetRequestHeader('Content-Encoding'));
    if SameText(encoding, 'deflate') or SameText(encoding, 'gzip') then
    begin
      compressed := TPooledMemoryStream.Create;
      if SameText(encoding, 'deflate') then
      begin
        CompressStream(data, compressed);
        compressed.Seek(2, soFromBeginning);
      end
      else
      begin
        CompressGZipStream(data, compressed);
        compressed.Seek(0, soFromBeginning);
      end;
      data := compressed;
    end
    else
      compressed := nil;

    HTTPWriteLine('Content-Length: ' + RawByteString(IntToStr(data.Size - data.Position)));
    HTTPWriteLine('');

    Total := 0;
    if UploadRate > 0 then
    begin
      QueryPerformanceFrequency(Freq);
      QueryPerformanceCounter(Start);
    end;

    repeat
      read := data.Read(buffer, SizeOf(buffer));
      if read > 0 then
      begin
        if UploadRate > 0 then
        begin
          Inc(Total, read);
          while True do
          begin
            QueryPerformanceCounter(Curr);
            Rate := Round((Total / 1024) / ((Curr - Start) / Freq));
            if Rate < UploadRate then
              Break
            else
              Sleep(1000);
          end;
        end;
        SockSend(buffer, read);
        if FWriteError then
          Exit;
      end;
    until read = 0;

    if compressed <> nil then
      compressed.Free;
  end
  else
  begin
    HTTPWriteLine('Content-Length: 0');
    HTTPWriteLine('');
  end;

  Flush;
  if FWriteError then
    Exit;

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

procedure THTTPRequest.SetCookie(const Name: RawByteString; Value: TCookie);
begin
  FCookies.AddOrSetValue(Name, Value);
end;

procedure THTTPRequest.SetDownloadRate(value: Cardinal);
begin
  FDownloadRate := value;
end;

procedure THTTPRequest.SetFollowRedirect(value: Boolean);
begin
  FFollowRedirect := value;
end;

procedure THTTPRequest.SetOnReadyStateChange(const ready: TOnReadyStateChange);
begin
  FOnReadyStateChange := ready;
end;

procedure THTTPRequest.SetReadyState(ready: TReadyState);
begin
  FReadyState := ready;
  if FAsync and FSynchronize then
    TThread.Synchronize(nil,
    procedure
      begin
        if Assigned(FOnReadyStateChange) then
          FOnReadyStateChange(Self)
      end
    )
  else if Assigned(FOnReadyStateChange) then
    FOnReadyStateChange(Self)
end;

procedure THTTPRequest.SetRequestHeader(const header, value: RawByteString);
begin
  if FReadyState <> rsOpen then
    raise EHTTPRequest.Create('Connection is not open.');
  InternalSetRequestHeader(header, value);
end;

procedure THTTPRequest.SetResetDataStream(const value: Boolean);
begin
  FResetDataStream := Value;
end;

procedure THTTPRequest.SetResponseHeader(const header, value: RawByteString);
var
  event: TOnHeaderEvent;
  low: RawByteString;
begin
  low := LowerCase(header);
  if not FResponseEvents.TryGetValue(low, event) or event(value) then
    FResponseHeaders.AddOrSetValue(low, THTTPHeader.Make(header, value));
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

procedure THTTPRequest.SetTimeout(value: Cardinal);
begin
  FTimeout := value;
end;

procedure THTTPRequest.SetUploadRate(value: Cardinal);
begin
  FUploadRate := value;
end;

function THTTPRequest.SockRecv(var Buf; len: Integer): Integer;
var
  rcv: Integer;
  p: PByte;
  last_error: Integer;
begin
  Result := 0;
  p := @buf;
  while len > 0 do
  begin
    if FSsl <> nil then
      rcv := SSL_read(FSsl, p, 1)
    else
      rcv := recv(FSocket, p^, 1, 0);

    if rcv <> 1 then
    begin
      last_error := WSAGetLastError;
      if (last_error = WSAETIMEDOUT) then
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
var
  l: Cardinal;
  p: PByte;
  sent: Integer;
  last_error: Integer;
begin
  Result := len;

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
        sent := SSL_write(FSsl, @FBuffer, BUFFER_SIZE)
      else
        sent := WinSock2.send(FSocket, FBuffer, BUFFER_SIZE, 0);

      if sent <> BUFFER_SIZE then
      begin
        last_error := WSAGetLastError;
        if (last_error = WSAETIMEDOUT) then
          raise EHTTPRequest.Create('Timeout.');
        FWriteError := last_error <> 0;
      end;

      FBufferPos := 0;
    end;

    l := Min(len, BUFFER_SIZE - FBufferPos);
  end;
end;

{$IFNDEF RELEASE}

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

{$ENDIF}

function THTTPRequest.TCPConnect(const domain: RawByteString; port: Word; ssl: Boolean): Boolean;
var
  host: WinSock2.PHostEnt;
  addr: TSockAddr;
  rc: Integer;
begin
  Result := True;
  // find host
  host := gethostbyname(PAnsiChar(Domain));
  if host = nil then
    Exit(False);

  // socket
  FSocket := socket(AF_INET, SOCK_STREAM, 0);
  if FSocket = INVALID_SOCKET then
    Exit(False);

  // connect
  FillChar(addr, SizeOf(addr), 0);
  PSockAddrIn(@addr).sin_family := AF_INET;
  PSockAddrIn(@addr).sin_port := htons(Port);
  PSockAddrIn(@addr).sin_addr.S_addr := PInteger(host.h_addr^)^;
  if connect(FSocket, addr, SizeOf(addr)) <> 0 then
    Exit(False);

  SocketTuneSendBuffer(FSocket);

  if ssl then
  begin
    FCtx := SSL_CTX_new(TLS_client_method());
    SSL_CTX_set_min_proto_version(FCtx, TLS1_2_VERSION);
    SSL_CTX_set_options(FCtx, SSL_OP_NO_SSLv3 or SSL_OP_NO_COMPRESSION);
    SSL_CTX_set_cipher_list(FCtx, DOR_SSL_CIPHER_LIST);

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
    SSL_set_mode(FSsl, SSL_get_mode(FSsl) or SSL_MODE_AUTO_RETRY);
    SSL_set_fd(FSsl, FSocket);

    if FTLSHostname <> '' then
      SSL_set_tlsext_host_name(FSsl, PAnsichar(FTLSHostname));

    ERR_clear_error;
    rc := SSL_connect(FSsl);
    if rc <> 1 then
    begin
    {$IFNDEF RELEASE}
      SSLError(FSsl, rc, 'SSL_connect');
    {$ENDIF}
      Exit(False);
    end;
  end;
end;

procedure THTTPRequest.TCPDisconnect;
begin
  if FSocket <> INVALID_SOCKET then
  begin
    Flush;
    shutdown(FSocket, SD_BOTH);
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

procedure THTTPRequest.ClearRequestHeaders;
begin
  FRequestHeaders.Clear;
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

procedure THTTPRequest.Flush;
var
  sent: Integer;
  last_error: Integer;
begin
  if FBufferPos > 0 then
  begin
    if FSsl <> nil then
      sent := SSL_write(FSsl, @FBuffer, FBufferPos)
    else
      sent := WinSock2.send(FSocket, FBuffer, FBufferPos, 0);

    if sent <> Integer(FBufferPos) then
    begin
      last_error := WSAGetLastError;
      if (last_error = WSAETIMEDOUT) then
        raise EHTTPRequest.Create('Timeout.');
      FWriteError := last_error <> 0;
    end;

    FBufferPos := 0;
  end;
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

procedure THTTPRequest.LoadDefaultHeaders;
begin
  ClearRequestHeaders;
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
          end
          else if AnsiStrings.SameText(key, 'path') then
            cookie.path := value
          else if AnsiStrings.SameText(key, 'domain') then
            cookie.domain := value
          else if AnsiStrings.SameText(key, 'expires') then
            cookie.expires := value
          else if AnsiStrings.SameText(key, 'max-age') then
            cookie.max_age := StrToIntDef(string(value), 0);

          Result := True;
        end)
      then
        FCookies.AddOrSetValue(name, cookie);
      Result := False;
    end);
end;

{ THTTPRequest.TThreadAsync }

constructor THTTPRequest.TThreadAsync.Create(const this: IHTTPRequest;
  data: TStream; TimeOut, DownloadRate, UploadRate: Cardinal);
begin
  FreeOnTerminate := True;
  FThis := this;
  FTimeOut := TimeOut;
  FUploadRate := UploadRate;
  FDownloadRate := DownloadRate;
  if data <> nil then
  begin
    FData := TPooledMemoryStream.Create;
    FData.LoadFromStream(data);
  end
  else
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
  THTTPRequest(FThis).InternalSend(FData, FTimeOut, FDownloadRate, FUploadRate);
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
