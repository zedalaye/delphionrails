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

unit dorHTTPStub;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, StrUtils, Classes, Rtti, Hash, NetEncoding,
{$IFDEF DEBUG}
  Diagnostics,
{$ENDIF}
  supertypes, superobject,
  dorSocketStub, dorUtils;

type
  THTTPMessage = class(TSuperObject)
  private
    FContent: TPooledMemoryStream;
    function GetContentString: SOString;
    function GetContentObject: ISuperObject;
  public
    constructor Create(jt: TSuperType = stObject); override;
    destructor Destroy; override;

    function _AddRef: Integer; override; stdcall;
    function _Release: Integer; override; stdcall;

    procedure Clear(all: boolean = false); override;

    property Content: TPooledMemoryStream read FContent;
    property ContentString: SOString read GetContentString;
    property ContentObject: ISuperObject read GetContentObject;
  end;

{$if defined(DEBUG)}
  PLuaStackInfo = ^TLuaStackInfo;
  TLuaStackInfo = record
  private
    next: PLuaStackInfo;
  public
    line: Integer;
    name: PAnsiChar;
    source: PAnsiChar;
    function Push(line: Integer; name, source: PAnsiChar): PLuaStackInfo;
    function Pop: PLuaStackInfo;
  end;

  TLuaDebug = class
    LuaStack: PLuaStackInfo;
  end;
{$endif}

  TGetPasswordProc = reference to procedure(var key, iv: PByte);
  TGetPathProc = reference to function: string;
  TGetAuthProc = reference to function(const Method, User, Password: string): Boolean;

  TRequestProcessor = class
  public
    class function BuildFormats: ISuperObject;

    class function DecodeSession(const Request: THTTPMessage;
      const GetPassProc: TGetPasswordProc): ISuperObject;

    class function EncodeSession(const Response: THTTPMessage; const Session: ISuperObject;
      const GetPassProc: TGetPasswordProc): Boolean;

    class function Authenticate(const AuthData: string; var User: string;
      const GetAuthProc: TGetAuthProc): Boolean;

    class procedure DecodeBody(const Request: THTTPMessage;
      const Params: ISuperObject);

    class function ProcessRoute(IsWebSocket: Boolean;
      const Context: TSuperRttiContext; const Formats: ISuperObject;
      const Request: THTTPMessage; const Params: ISuperObject): TRttiInstanceType;

    class procedure ApplyContentType(const Formats, Params: ISuperObject;
      const Response: THTTPMessage);

    class function ProcessAction(
      Klass: TRttiInstanceType;
      const Context: TSuperRttiContext; const Source: IReadWrite;
      const Formats: ISuperObject; const Session: ISuperObject;
      const Request: THTTPMessage; const Params: ISuperObject;
      const Response: THTTPMessage; const Return: ISuperObject;
      var ErrorCode: Integer; var FileToSend: string
    ): Boolean;

    class function ResolveView(const Context: TSuperRttiContext; const Formats: ISuperObject;
      const Request: THTTPMessage; const Params: ISuperObject): TRttiInstanceType;

    class function ProcessView(
      Klass: TRttiInstanceType;
      const Context: TSuperRttiContext; const Source: IReadWrite;
      const Formats: ISuperObject; const Session: ISuperObject;
      const Request: THTTPMessage; const Params: ISuperObject;
      const Response: THTTPMessage; const Return: ISuperObject;
      var ErrorCode: Integer; var FileToSend: string
    ): Boolean;

    class function RenderScript(const Formats, Session: ISuperObject;
      const Request: THTTPMessage; const Params: ISuperObject;
      const Response: THTTPMessage; const Return: ISuperObject;
      var ErrorCode: Integer;
      const GetRootPathProc: TGetPathProc): Boolean;

    class function PrepareSendFile(const FileToSend: string; const Formats: ISuperObject;
      const Response: THTTPMessage; var Compress: Boolean; var ErrorCode: Integer): Boolean;

    class function PrepareStaticFile(const Formats: ISuperObject;
      const Request: THTTPMessage; const Params: ISuperObject; const Response: THTTPMessage;
      var IsStatic, Compress: Boolean; var FileToSend: string; var ErrorCode: Integer;
      const GetRootPathProc: TGetPathProc): Boolean;

    class procedure ApplyCompress(var Compress: Boolean; const Request, Response: THTTPMessage);
  end;

  THTTPStub = class(TClientStub)
  private
    FContext: TSuperRttiContext;
    FFormats: ISuperObject;
    FRequest: THTTPMessage;
    FParams: ISuperObject;
    FResponse: THTTPMessage;
    FReturn: ISuperObject;
    FSession: ISuperObject;
    FErrorCode: Integer;
    FFileToSend: string;
    FCompress: Boolean;
    FCompressLevel: Integer;
    FIsStatic: Boolean;
    FWebSocketVersion: Integer;
    function DecodeFields(str: PChar): boolean;
    function DecodeCommand(str: PChar): boolean;
    procedure WriteLine(str: RawByteString);
    procedure SendEmpty;
    procedure SendFile(const filename: string);
    procedure SendStream(Stream: TStream);
    function DecodeContent: boolean; virtual;
    procedure doBeforeProcessRequest; virtual;
    procedure doAfterProcessRequest; virtual;
  protected
    function Run: Cardinal; override;

    function HandleCORS(const Method, Path: string; var Origin, AllowedHeaders, AllowedMethods: string; var MaxAge: Cardinal): Boolean; virtual;
    procedure GetPassPhrase(var key, iv: PByte); virtual;
    function ProcessAuth(const Method, User, Password: string): Boolean; virtual;
    function ProcessRequest: Boolean; virtual;

    function Upgrade: Cardinal; virtual;
    function WebSocket: Cardinal; virtual;

    function GetRootPath: string; virtual;
  public
    constructor CreateStub(AOwner: TSocketServer; const ASocket: IReadWrite); override;
    destructor Destroy; override;

    procedure Render(const obj: ISuperObject; format: boolean = false); overload;
    procedure Render(const str: string); overload;

    procedure Redirect(const location: string); overload;
    procedure Redirect(const controler, action: string; const id: string = ''); overload;

    property Context: TSuperRttiContext read FContext;
    property Request: THTTPMessage read FRequest;
    property Params: ISuperObject read FParams;
    property Response: THTTPMessage read FResponse;
    property Return: ISuperObject read FReturn;
    property Session: ISuperObject read FSession;
    property ErrorCode: Integer read FErrorCode write FErrorCode;
    property FileToSend: string read FFileToSend write FFileToSend;

    property Compress: Boolean read FCompress write FCompress;
    property CompressLevel: Integer read FCompressLevel write FCompressLevel;
  end;

function HTTPInterprete(src: PSOChar; named: Boolean = False; sep: SOChar = ';';
  StrictSep: Boolean = False; codepage: Integer = 0): ISuperObject;

const
  DEFAULT_CP = 65001; // UTF-8
  DEFAULT_CHARSET = 'utf-8';

implementation

uses
  superxmlparser,
  dorOpenSslHelpers, dorOpenSSL,
  dorHTTP, dorLua,
  dorActionController, dorActionView, dorActionWebsocket
  {$ifdef madExcept}, madexcept {$endif}
  {$ifdef UNICODE}, AnsiStrings{$endif}
  {$ifdef UNIX}, baseunix{$endif}
;

const
  CR = #13;
  LF = #10;
  SP = #32; // space
  HT = #9;  // backspace
  NL = #0;  // NULL
  SL = '/';
  PT = '.';
  CRLF = CR+LF;

(* default limit on bytes in Request-Line (Method+URI+HTTP-version) *)
  DEFAULT_LIMIT_REQUEST_LINE = 8190;
(* default limit on bytes in any one header field  *)
  DEFAULT_LIMIT_REQUEST_FIELDSIZE = 8190;
(* default limit on number of request header fields *)
  DEFAULT_LIMIT_REQUEST_FIELDS = 100;

  ReadTimeOut: Integer = 60000; // 1 minute
  COOKIE_NAME = 'Cookie';

function lua_print(state: Plua_State): Integer; cdecl;
var
  p: THTTPMessage;
  o: ISuperObject;
begin
  lua_getglobal(state, '@response');
  p := lua_touserdata(state, -1);
  if p <> nil then
    for var I := 1 to lua_gettop(state) do
    begin
      o := lua_tosuperobject(state, I);
      if o <> nil then
        // THTTPStub.Render()
        p.Content.WriteString(o.AsString, false, DEFAULT_CP);
    end;
  Result := 0;
end;

function lua_gettickcount(state: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(state, Integer(GetTickCount));
  Result := 1;
end;

{$if defined(DEBUG)}
procedure script_hook(L: Plua_State; ar: Plua_Debug); cdecl;
var
  p: TLuaDebug;
begin
  lua_getglobal(L, '@debug');
  p := lua_touserdata(L, -1);
  if p <> nil then
  begin
    case ar.event of
      LUA_HOOKLINE:
        p.LuaStack.line := ar.currentline;
      LUA_HOOKCALL:
        begin
          lua_getinfo(L, 'Snl', ar);
          if ar.name <> nil then
            p.LuaStack := p.LuaStack.Push(ar.currentline, ar.name, ar.source)
          else
            p.LuaStack := p.LuaStack.Push(ar.currentline, ar.what, ar.source);
        end;
      LUA_HOOKRET :
        p.LuaStack := p.LuaStack.Pop;
    end;
  end;
end;
{$endif}

function WrapValue(const S: string): ISuperObject; inline;
begin
  try
    Result := TSuperObject.ParseString(PChar(S), False, False);
  except
    Result := nil;
  end;
  if Result = nil then
    Result := TSuperObject.Create(S);
end;

function HTTPInterprete(src: PSOChar; named: Boolean = False; sep: SOChar = ';';
  StrictSep: Boolean = False; codepage: Integer = 0): ISuperObject;
var
  P1: PSOChar;
  S: SOString;
  i: integer;
  obj, obj2, value: ISuperObject;
begin
  if named then
    Result := TSuperObject.create(stObject)
  else
    Result := TSuperObject.create(stArray);

  if not StrictSep then
    while {$IFDEF UNICODE}(src^ < #256) and {$ENDIF} (AnsiChar(src^) in [#1..' ']) do
      Inc(src);

  while src^ <> #0 do
  begin
    P1 := src;
    while ((not StrictSep and (src^ >= ' ')) or (StrictSep and (src^ <> #0)))
      and (src^ <> sep)
    do
      Inc(src);

    SetString(S, P1, src - P1);
    if codepage > 0 then
      S := MBUDecode(HTTPDecode(S), codepage);

    if named then
    begin
      i := pos('=', S);
      // named
      if i > 1 then
      begin
        S[i] := #0;
        obj := Result[S];
//        if sep = '&' then
//          value := WrapValue(PChar(@S[i+1])) else
        value := TSuperObject.Create(PSOChar(@S[i+1]));
        if obj = nil then
          Result[S] := value
        else
        begin
          if obj.IsType(stArray) then
            obj.AsArray.Add(value)
          else
          begin
            obj2 := TSuperObject.Create(stArray);
            Result[S] := obj2;
            obj2.AsArray.Add(obj);
            obj2.AsArray.Add(value);
          end;
        end;
      end
      else
      begin
        // unamed value ignored
      end;
    end
    else
    begin
      value := TSuperObject.Create(S);
      if value = nil then
//      if sep = '&' then
//        value := WrapValue(PChar(s)) else
        value := TSuperObject.Create(s);
      Result.AsArray.Add(value);
    end;
    if not StrictSep then
      while {$IFDEF UNICODE}(src^ < #256) and {$ENDIF} (AnsiChar(src^) in [#1..' ']) do
        Inc(src);
    if src^ = sep then
    begin
      P1 := src;
      Inc(P1);
      if (P1^ = #0) and not named then
        Result.AsArray.Add(TSuperObject.Create(''));
      repeat
        Inc(src);
      until not (not StrictSep and {$IFDEF UNICODE}(src^ < #256) and {$ENDIF} (AnsiChar(src^) in [#1..' ']));
    end;
  end;
end;

function EncodeObject(const obj: ISuperObject; const key, iv: PByte): SOString;
var
  StreamA, streamB: TPooledMemoryStream;
begin
  StreamB := TPooledMemoryStream.Create;
  StreamA := TPooledMemoryStream.Create;
  try
    // ansi
    obj.SaveTo(StreamA);

    // zlib
    StreamA.Seek(0, soFromBeginning);
    CompressStream(StreamA, StreamB, 4);

    // aes
    StreamA.Seek(0, soFromBeginning);
    AesEncryptStream(StreamB, StreamA, key, iv);
    StreamA.Size := StreamA.Position;

    // base64
    StreamB.Seek(0, soFromBeginning);
    StreamToBase64(StreamA, StreamB);
    StreamB.Size := StreamB.Position;

    // string
    Result := StreamToStr(StreamB);
  finally
    StreamA.Free;
    StreamB.Free;
  end;
end;

function DecodeObject(const str: SOString; const key, iv: PByte): ISuperObject;
var
  StreamA, StreamB: TPooledMemoryStream;
begin
  StreamA := TPooledMemoryStream.Create;
  StreamB := TPooledMemoryStream.Create;
  try
    // base64
    Base64ToStream(str, streamA);
    streamA.Size := streamA.Position;

    // aes
    AesDecryptStream(StreamA, StreamB, key, iv);

    // zlib
    StreamA.Seek(0, soFromBeginning);
    StreamB.Seek(0, soFromBeginning);
    DecompressStream(StreamB, StreamA);
    StreamA.Size := StreamA.Position;

    // superobject
    StreamA.Seek(0, soFromBeginning);
    Result := TSuperObject.ParseStream(StreamA, False);
  finally
    StreamA.Free;
    StreamB.Free;
  end;
end;

function CamelCase(const s: string): string;
var
  P, C, R: PChar;
  First: Boolean;
begin
  GetMem(C, Length(s) * SizeOf(Char));
  try
    R := C;
    P := PChar(s);
    First := True;
    while P^ <> #0 do
    begin
      if P^ = '_' then
      begin
        Inc(P);
        First := True;
        Continue;
      end;
      if First then
      begin
        R^ := UpCase(P^);
        First := False;
      end
      else
        R^ := P^;
      Inc(R);
      Inc(P);
    end;
    SetString(Result, C, R - C);
  finally
    FreeMem(C);
  end;
end;

{ THTTPMessage }

procedure THTTPMessage.Clear(all: boolean);
begin
  inherited;
  FContent.Clear;
end;

constructor THTTPMessage.Create(jt: TSuperType);
begin
  Inherited create(jt);
  FContent := TPooledMemoryStream.Create;
  DataPtr := Self;
end;

destructor THTTPMessage.Destroy;
begin
  inherited;
  FContent.Free;
end;

function THTTPMessage.GetContentObject: ISuperObject;
begin
  FContent.Seek(0, soFromBeginning);
  Result := TSuperObject.ParseStream(FContent, False);
end;

function THTTPMessage.GetContentString: SOString;
var
  Data: RawByteString;
begin
  FContent.Seek(0, soFromBeginning);
  SetLength(Data, FContent.Size);
  FContent.Read(PAnsiChar(Data)^, FContent.Size);
  Result := SOString(Data);
end;

function THTTPMessage._AddRef: Integer;
begin
  Result := inherited;
end;

function THTTPMessage._Release: Integer;
begin
  Result := inherited;
end;

{ THTTPStub }

function THTTPStub.DecodeFields(str: PChar): boolean;
var
  p: PChar;
  prop: string;
begin
  p := StrScan(str, ':');
  if p = nil then
    Result := False
  else
    with FRequest.ForcePath('env') do
    begin
      prop := LowerCase(Copy(str, 1, p-str));
      AsObject.S[prop] := p+2;
      Result := True;
    end;
end;

function THTTPStub.DecodeContent: boolean;
const
  BLOCK_SIZE = 8*1024;
var
  ContentLength: Integer;
  len, total: Integer;
  b: array[0..BLOCK_SIZE] of Byte;
  ContentEncoding: string;
  stream: TPooledMemoryStream;
  sniff: array[0..1] of Byte;
  rawdeflate: Boolean;

  function max_block_size(total: Integer): Integer; inline;
  begin
    if total < BLOCK_SIZE then
      Result := total
    else
      Result := BLOCK_SIZE;
  end;

begin
  Result := True;
  ContentLength := FRequest.I['env.content-length'];
  if ContentLength > 0 then
  begin
    FRequest.FContent.Size := ContentLength;
    FRequest.FContent.Seek(0, soFromBeginning);
    total := ContentLength;
    repeat
      len := Source.Read(b[0], max_block_size(total), ReadTimeOut);
      if len > 0 then
        FRequest.FContent.Write(b[0], len);
      Dec(total, len);
    until (total = 0) or (len <= 0);
    Result := total = 0;

    ContentEncoding := FRequest.S['env.content-encoding'];
    if SameText(ContentEncoding, 'deflate') or SameText(ContentEncoding, 'gzip') then
    begin
      stream := TPooledMemoryStream.Create;
      try
        FRequest.FContent.Seek(0, soFromBeginning);
        if SameText(ContentEncoding, 'deflate') then
        begin
          { Tolerate both a conformant zlib stream (current clients) and a raw
            deflate stream with the zlib header stripped (legacy DOR clients):
            sniff the first 2 bytes and decode accordingly. addflag=True tells
            DecompressStream to prepend a synthetic zlib header to raw deflate. }
          rawdeflate := True;
          if FRequest.FContent.Read(sniff, 2) = 2 then
            rawdeflate := not IsZlibHeader(sniff[0], sniff[1]);
          FRequest.FContent.Seek(0, soFromBeginning);
          DecompressStream(FRequest.FContent, stream, rawdeflate);
        end
        else
          DecompressGZipStream(FRequest.FContent, stream);
        { Exchange request.FContent with stream of decompressed data }
        stream := InterlockedExchangePointer(Pointer(FRequest.FContent), Pointer(stream));
      finally
        stream.Free;
      end;
    end;
  end;
end;

function THTTPStub.DecodeCommand(str: PChar): boolean;

  function DecodeURI(uri: PChar; len: integer; out data: string): boolean;
  const
    hexcodes = ['0'..'9', 'A'..'F', 'a'..'f'];
  var
    i: integer;
    raw_data: RawByteString;
  begin
    data := '';
    raw_data := '';
    while len > 0 do
    begin
      if (uri^ = '%') then
      begin
        // PARANOIA !!
        if (len > 2) and
          {$IFDEF UNICODE}(uri[1] < #256) and {$ENDIF}(AnsiChar(uri[1]) in hexcodes) and
          {$IFDEF UNICODE}(uri[2] < #256) and {$ENDIF}(AnsiChar(uri[2]) in hexcodes) and
          TryStrToInt('$' + uri[1] + uri[2], i) and
          (i in [32..255])
        then
        begin
          raw_data := raw_data + AnsiChar(i);
          inc(uri, 3);
          dec(len, 3);
        end
        else
        begin
          Result := False;
          Exit;
        end;
      end
      else if uri^ = '+' then
      begin
        raw_data := raw_data + ' ';
        inc(uri, 1);
        dec(len, 1);
      end
      else
      begin
        raw_data := raw_data + AnsiChar(uri^);
        inc(uri, 1);
        dec(len, 1);
      end;
    end;
    data := MBUDecode(raw_data, DEFAULT_CP);
    Result := True;
  end;

var
  marker: PChar;
  param, value: string;
  i: Integer;
begin
  Result := False;

  marker := StrScan(str, SP);
  if marker = nil then
    Exit;

  FRequest.AsObject.S['method'] := Copy(str, 0, marker - str);
  str := marker;

  // SP
  if (str^ <> SP) then
    Exit;

  // URI
  Inc(str);
  marker := Str;
  while not ({$IFDEF UNICODE}(str^ < #256) and {$ENDIF}(AnsiChar(Str^) in [SP, NL, '?'])) do
    inc(str);

  if (str > marker) and (str^ <> NL) then
  begin
    if DecodeURI(marker, str - marker, value) then
      FRequest.AsObject.S['uri'] := value
    else
      Exit;
  end
  else
    Exit;

  // Parameters
  if str^ = '?' then
  begin
    Inc(str);
    marker := Str;
    param := '';
    value := '';
    while True do
      case str^ of
        '&', SP, NL:
          begin
            if (param <> '') and (str > marker) then
            begin
              if not DecodeURI(marker, str - marker, value) then exit;
              FRequest['params.'+param] := WrapValue(value);
            end;
            if {$IFDEF UNICODE}(str^ < #256) and {$ENDIF}(AnsiChar(str^) in [SP, NL]) then
              Break;
            param := '';
            value := '';
            inc(Str);
            marker := Str;
          end;
        '=':
          begin
            if (str > marker) then
              if not DecodeURI(marker, str - marker, param) then
                Exit;
            Inc(Str);
            marker := Str;
          end;
      else
        Inc(Str);
        continue;
      end;
  end;

  // SP expected
  if (str^ <> SP) then
    Exit;
  repeat
    Inc(str);
  until str^ <> SP;

  // HTTP/
  if not ((str[0] = 'H') and (str[1] = 'T') and (str[2] = 'T') and
     (str[3] = 'P') and (str[4] = SL)) then
    Exit;
  str := PChar(@str[5]);

  // version major
  marker := str;
  while {$IFDEF UNICODE}(str^ < #256) and{$ENDIF}(AnsiChar(str^) in ['0'..'9']) do
    Inc(str);
  if (str > marker) and (str^ <> NL) then
  begin
    if TryStrToInt(copy(marker, 0, str - marker), i) then
      FRequest.I['http-version.major'] := i
    else
      Exit;
  end
  else
    Exit;

  // .
  if (str^ <> PT) then
    Exit;
  Inc(str);

  // version minor
  marker := str;
  while {$IFDEF UNICODE}(str^ < #256) and{$ENDIF} (AnsiChar(str^) in ['0'..'9']) do
    Inc(str);
  if (str > marker) then
  begin
    if TryStrToInt(copy(marker, 0, str - marker), i) then
      FRequest.I['http-version.minor']  := i
    else
      Exit;
  end
  else
    Exit;

  if (str^ <> NL) then
    Exit;

  Result := True;
end;

function THTTPStub.Run: Cardinal;
var
  buffer: string;
  cursor, line, len: integer;
  c: Char;
{$IFDEF UNIX}
  FDSet: TFDSet;
  TimeOut: TTimeVal;
  r: integer;
{$ENDIF}
{$if defined(DEBUG)}
  diag: TStopwatch;
{$endif}
begin
{$if defined(DEBUG)}
  TThread.NameThreadForDebugging(AnsiString(Self.ClassName));
  diag := TStopwatch.Create;
{$ifend}

  result := 0;
  cursor := 0;
  len := 0;
  line := 0;
  c := #0;
  while not Stopped do
  begin
    //ProcessEvents;
    inc(cursor);
    if cursor > len then
    begin
      inc(len, 255);
      SetLength(buffer, len);
    end;

    // check sizes
    if ((line = 0) and (cursor >= DEFAULT_LIMIT_REQUEST_LINE)) or
       ((line > 0) and (cursor >= DEFAULT_LIMIT_REQUEST_FIELDSIZE)) or
       (line > DEFAULT_LIMIT_REQUEST_FIELDS) then
      Exit;

{$IFDEF UNIX}
    repeat
      // stop listening when stoped
      fpFD_ZERO(FDSet);
      fpFD_SET(SocketHandle, FDSet);
      TimeOut.tv_sec := 1; //1 sec
      TimeOut.tv_usec := 0;
      r := fpSelect(SocketHandle + 1, @FDSet, nil, nil, @TimeOut);
      if Stopped then exit;
    until r > 0;
{$ENDIF}

    if Source.Read(c, 1, ReadTimeOut) <> 1 then
      Exit;

    case c of
    CR: dec(cursor){->LF};
    LF:
      begin
        if cursor = 1 then
        begin
          if not DecodeContent then
            Exit;

          FReturn := TSuperObject.Create;
          FParams := TSuperObject.Create;
          FSession := TSuperObject.Create;
          try
            doBeforeProcessRequest;
            if pos('Upgrade', FRequest.S['env.connection']) > 0 then
              Exit(Upgrade);
            try
              try
                ProcessRequest; // <<<<<<<<<<<<<<<
              except
                on E: Exception do
                begin
                  FErrorCode := 500;
                {$ifdef madExcept}
                  with NewException(etNormal, E) do
                  begin
                    FResponse.Content.WriteString(BugReport, False);
                    AutoSaveBugReport(BugReport);
                  end;
                {$else}
                  Response.Content.WriteString(E.Message, False);
                  Response.Content.WriteString(CRLF, False);
                  Response.Content.WriteString(E.StackTrace, False);
                {$endif}
                end;
              end;
            finally
              doAfterProcessRequest;
            end;
          finally
            FParams := nil;
            FReturn := nil;
            FSession := nil;
          end;

          line := 0;
          cursor := 0;
        {$if defined(DEBUG)}
          diag.Stop;
          OutputDebugString(PChar(Format('REQUEST PROCESSED IN %dms', [diag.ElapsedMilliseconds])));
          diag.Reset;
        {$endif}
        end
        else
        begin
          buffer[cursor] := NL;
          if line = 0 then
          begin
            if not DecodeCommand(Pointer(Buffer)) then
              Exit;
          end
          else
          begin
            if not DecodeFields(Pointer(Buffer)) then
              Exit;
          end;
          cursor := 0;
          inc(line);
        end;
      end;
    else
      buffer[cursor] := c;
    {$if defined(DEBUG)}
      if not diag.IsRunning then
        diag.Start;
    {$endif}
    end;
  end;
end;

constructor THTTPStub.CreateStub(AOwner: TSocketServer; const ASocket: IReadWrite);
begin
  inherited;

  FRequest := THTTPMessage.Create;
  FRequest._AddRef;

  FResponse := THTTPMessage.Create;
  FResponse._AddRef;

  FContext := TSuperRttiContext.Create;
  FFormats := TRequestProcessor.BuildFormats;
end;

destructor THTTPStub.Destroy;
begin
  FFormats := nil;
  FContext.Free;

  FRequest._Release;
  FResponse._Release;

  inherited;
end;

procedure THTTPStub.doAfterProcessRequest;
var
  ite: TSuperObjectIter;
begin
  { Set Server Name and Version }
  FResponse.AsObject.S['Server'] := 'DOR 1.0';

  { Client accept compressed (deflate) content and the content we have can be
    compressed }
  TRequestProcessor.ApplyCompress(FCompress, FRequest, FResponse);

  { Encode Session Cookie }
  if not FIsStatic then
    TRequestProcessor.EncodeSession(FResponse, FSession, GetPassPhrase);

  { Response has no content and there is no file to send : looks like the expected
    resource is not found -> 404 }
  if (FErrorCode < 300) and (FErrorCode <> 204) and (FResponse.Content.Size = 0) and (FFileToSend = '') then
    FErrorCode := 404;

  { Output Response Status Code }
  WriteLine(HttpResponseStrings(FErrorCode));

  { Output Response Headers }
  if ObjectFindFirst(FResponse, ite) then
  repeat
    case ObjectGetType(ite.val) of
      stArray:
        for var obj in ite.val do
          WriteLine(RawByteString(ite.key + ': ' + obj.AsString));
      stNull: ;
    else
      WriteLine(RawByteString(ite.key + ': ' + ite.val.AsString));
    end;
  until not ObjectFindNext(ite);
  ObjectFindClose(ite);

  { Output Response Body }
  if FFileToSend <> '' then
    SendFile(FFileToSend)
  else
    SendStream(FResponse.Content);

  Source.Flush;

  FSession.Clear(True);
  FReturn.Clear(True);
  FResponse.Clear(True);
  FParams.Clear(True);
  FRequest.Clear(True);
end;

function THTTPStub.ProcessAuth(const Method, User, Password: string): Boolean;
begin
  Result := False;
end;

procedure THTTPStub.doBeforeProcessRequest;
begin
  FErrorCode := 0;
  FCompress := False;
  FCompressLevel := 5;
  FFileToSend := '';
  FIsStatic := False;

  { Extract/Decode some useful Request properties }
  with FRequest.AsObject do
  begin
    S['remote-ip']    := string(Source.ClientIP);
  { for reverse proxies... cf. https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Forwarded-For
    O['x-forwarded-for'] := HTTPInterprete(PSOChar(Request.S['env.x-forwarded-for']), false, ',');
    O['forwarded'] := HTTPInterprete(PSOChar(Request.S['env.forwarded']), true); }
    O['cookies']        := HTTPInterprete(PSOChar(FRequest.S['env.cookie']), true);
    O['content-type']   := HTTPInterprete(PSOChar(FRequest.S['env.content-type']), false, ';');
    O['accept']         := HTTPInterprete(PSOChar(FRequest.S['env.accept']), false, ',');
    { Copy Content-Length }
    I['content-length'] := FRequest.I['env.content-length'];
  end;

  { Decode Session Cookie }
  FSession := TRequestProcessor.DecodeSession(FRequest, GetPassPhrase);

  { Merge Query String Parameters with decoded Request Body  }
  TRequestProcessor.DecodeBody(FRequest, FParams);

{$IFDEF CONSOLEAPP}
{$IFDEF DEBUG}
  //Writeln(FParams.AsString);
{$ENDIF}
{$ENDIF}
end;

procedure THTTPStub.GetPassPhrase(var key, iv: PByte);
begin
  key := nil;
  iv := nil;
end;

function THTTPStub.GetRootPath: string;
begin
  Result := ExtractFilePath(ParamStr(0));
end;

function THTTPStub.HandleCORS(const Method, Path: string; var Origin,
  AllowedHeaders, AllowedMethods: string; var MaxAge: Cardinal): Boolean;
begin
  Result := False;
end;

procedure THTTPStub.Render(const obj: ISuperObject; format: boolean);
begin
  obj.SaveTo(FResponse.Content, format);
end;

procedure THTTPStub.Redirect(const controler, action: string; const id: string);
begin
  if id = '' then
    Redirect('/' + controler + '/' + action + '.' + FParams.S['format'])
  else
    Redirect('/' + controler + '/' + action + '/' + id + '.' +  FParams.S['format']);
end;

procedure THTTPStub.Render(const str: string);
begin
  FResponse.Content.WriteString(str, false, DEFAULT_CP);
end;

procedure THTTPStub.Redirect(const location: string);
begin
  FErrorCode := 302;
  FResponse.AsObject.S['Location'] := Location;
end;

function THTTPStub.ProcessRequest: Boolean;
var
  uri, method, origin, allowed_headers, allowed_methods: string;
  max_age: Cardinal;
  // references the controller class through Rtti
  Klass: TRttiInstanceType;
begin
  Result := False;

  { Handle CORS : Defaults to "No" because HandleCORS() returns False unless
    overriden }

  uri    := FRequest.AsObject.S['uri'];
  method := FRequest.AsObject.S['method'];
  origin := FRequest.S['env.origin'];

  if method = 'OPTIONS' then
  begin
    method := FRequest.S['env.access-control-request-method'];
    allowed_headers := FRequest.S['env.access-control-request-headers'];

    { CORS Preflight request }

    if method <> '' then
    begin
      allowed_methods := 'OPTIONS, GET, ' + method;
      max_age := 86400;

      if HandleCORS(method, uri, origin, allowed_headers, allowed_methods, max_age) then
      begin
        FResponse.AsObject.S['Connection'] := 'keep-alive';
        FResponse.AsObject.S['Access-Control-Allow-Origin'] := origin;
        FResponse.AsObject.S['Access-Control-Allow-Methods'] := allowed_methods;
        FResponse.AsObject.S['Access-Control-Allow-Headers'] := allowed_headers;
        FResponse.AsObject.I['Access-Control-Max-Age'] := max_age;
        FErrorCode := 204; // no-content
        Exit;
      end;
    end;
  end;

  allowed_methods := method;
  allowed_headers := '';
  max_age := 0;

  if HandleCORS(method, uri, origin, allowed_headers, allowed_methods, max_age) then
    FResponse.AsObject.S['Access-Control-Allow-Origin'] := origin;

  var user := '';
  if TRequestProcessor.Authenticate(FRequest.S['env.authorization'], user, ProcessAuth) then
    FSession.S['user'] := user;

  { Decode the current route in the context of a controller action }
  Klass := TRequestProcessor.ProcessRoute(False, FContext, FFormats, FRequest, FParams);

  { Params have been fully decoded here so we can set the Response Content-Type }
  TRequestProcessor.ApplyContentType(FFormats, Params, Response);

  { Instantiate the controller and run the action }
  Result := TRequestProcessor.ProcessAction(Klass, FContext, Source, FFormats, FSession,
    FRequest, FParams, FResponse, FReturn,
    FErrorCode, FFileToSend
  );

  { Early return if the action
    - rendered something or
    - decided to return "no content" (204) or
    - asked for a redirect (300), a client error (400) or a server error (500)
  }
  if Stopped or (FResponse.FContent.Size > 0) or (FErrorCode = 204) or (FErrorCode >= 300) then
    Exit;

  { Early return if the original action was not a GET or a POST }
  method := FRequest.AsObject.S['method'];
  if (method <> 'GET') and (method <> 'POST') then
    Exit;

  { Prepare to send a file }
  if FFileToSend <> '' then
  begin
    if TRequestProcessor.PrepareSendFile(FFileToSend, FFormats, FResponse, FCompress, FErrorCode) then
      Result := False;
    Exit;
  end;

  { Resolve and run Hardcoded View }
  Klass := TRequestProcessor.ResolveView(FContext, FFormats, FRequest, FParams);
  if TRequestProcessor.ProcessView(Klass, FContext, Source, FFormats, FSession,
           FRequest, FParams, FResponse, FReturn,
           FErrorCode, FFileToSend)
  then
    Result := False;

  { Resolve and run View from File }
  if FResponse.Content.Size = 0 then
    if TRequestProcessor.RenderScript(
         FFormats, FSession,
         FRequest, FParams, FResponse, FReturn,
         FErrorCode,
         GetRootPath
       )
    then
      Result := False;

  { Cache-Control and FCompress will be overriden by TRequestProcessor.PrepareStaticFile()
    if appropriate }
  FResponse.AsObject.S['Cache-Control'] := 'private, max-age=0';
  FCompress := FFormats.B[FParams.AsObject.S['format'] + '.istext'];

  { Prepare to send a static file }
  if FResponse.Content.Size = 0 then
    if TRequestProcessor.PrepareStaticFile(
         FFormats, FRequest, FParams, FResponse,
         FIsStatic, FCompress, FFileToSend, FErrorCode,
         GetRootPath
       )
    then
      Result := False;
end;

procedure THTTPStub.SendEmpty;
begin
  WriteLine('Content-Length: 0');
  WriteLine('');
end;

procedure THTTPStub.SendFile(const filename: string);
var
  stream: TFileStream;
begin
  if FileExists(filename) then
  begin
    stream := TFileStream.Create(filename, fmOpenRead or fmShareDenyWrite);
    try
      SendStream(stream);
    finally
      stream.Free;
    end;
  end
  else
    SendEmpty;
end;

function THTTPStub.WebSocket: Cardinal;
const
  // Non Control Frames
  OPContinuation = $0;
  OPText =         $1;
  OPBinary =       $2;

  // Control Frames
  OPClose =        $8;
  OPPing =         $9;
  OPPong =         $A;

type
  TState = (stStartOldMode, stOldMode, stStartNewMode, stNext,
    stPayload16, stPayload64, stMask, stData);
var
  b: Byte;
  stream: TPooledMemoryStream;
  state: TState;
  data: UTF8String;
  klass: TRttiInstanceType;
  inst: TActionWebsocket;

  fin: Boolean;
  opcode: Byte;
  payloadLength: Int64;
  pos: Integer;
  mask: array[0..3] of Byte;
  closecode: Word;
begin
  Result := 0;
  pos := 0;
  payloadLength := 0;
  opcode := 0;
  fin := False;
  closecode := 0;

  Klass := TRequestProcessor.ProcessRoute(True, FContext, FFormats, FRequest, FParams);

  if (klass <> nil) then
  begin
    // double check
    Assert(klass.MetaclassType.InheritsFrom(TActionWebsocket));
    // build WebSocket instance
    inst := TActionWebsocketClass(klass.MetaclassType).Create(FWebSocketVersion,
      FContext, Source, FRequest, FParams, FSession
    );
    // double check
    Assert(inst is TActionWebsocket);
  end
  else
    Exit;

  inst.Initialize;
  inst.Start;

  if FWebSocketVersion = 0 then
    state := stStartOldMode
  else
    state := stStartNewMode;

  stream := TPooledMemoryStream.Create;
  try
    while not Stopped do
      if Source.Read(b, 1, 0) = 1 then
      begin
        case state of
          stStartOldMode:
            if b = 0 then
              state := stOldMode
            else
              Exit;
          stOldMode:
            begin
              if b <> $FF then
                stream.Write(b, 1)
              else
              begin
                SetLength(data, stream.Size);
                stream.Seek(0, soFromBeginning);
                stream.Read(PAnsiChar(data)^, stream.Size);
                inst.TriggerInternalEvent(TSuperObject.Create(string(data)));
                stream.Size := 0;
                state := stStartOldMode;
              end;
            end;
          stStartNewMode:
            begin
              fin := (b and $80) <> 0;
              if (b and $70) <> 0 then // reserved
                Exit;
              opcode := b and $0F;
              closecode := 0;
              state := stNext;
            end;
          stNext:
            begin
              // maskBit is necessary on server side
              if b and $80 = 0 then
                Exit;
              payloadLength := b and $7F;

              if (payloadLength < 126) then
              begin
                state := stMask;
                pos := 0;
              end
              else if (payloadLength = 126) then
              begin
                pos := 0;
                state := stPayload16;
              end
              else
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
                    state := stMask;
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
                    state := stMask;
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
                  if payloadLength > 0 then
                  begin
                    state := stData;
                    pos := 0;
                  end
                  else
                    state := stStartNewMode;
                end;
            end;
          stData:
            begin
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
                if fin and (opcode <> OPContinuation) then
                begin
                  case opcode of
                    OPClose:
                      begin
                        inst.TriggerInternalEvent(SO(['opcode', opcode, 'data', closecode, 'source', Source.ClientIP]));
                        Exit;
                      end;
                    OPText, OPPing, OPPong:
                      begin
                        SetLength(data, stream.Size);
                        stream.Seek(0, soFromBeginning);
                        stream.Read(PAnsiChar(data)^, stream.Size);
                        inst.TriggerInternalEvent(SO(['opcode', opcode, 'data', data, 'source', Source.ClientIP]));
                      end;
                    OPBinary:
                      begin
                        stream.Seek(0, soFromBeginning);
                        inst.TriggerInternalEvent(SO(['opcode', opcode, 'data', stream, 'source', Source.ClientIP]));
                        stream := TPooledMemoryStream.Create;
                      end;
                  end;
                  stream.Size := 0;
                end;
                state := stStartNewMode;
              end;
            end;
        end;
      end
    else
      Exit;
  finally
    stream.Free;
  end;
end;

procedure THTTPStub.WriteLine(str: RawByteString);
begin
  str := str + CRLF;
  Source.Write(PAnsiChar(str)^, length(str), 0);
end;

procedure THTTPStub.SendStream(Stream: TStream);

  procedure SendIt(s: TSTream);
  var
    size: Integer;
    buffer: array[0..1023] of byte;
  begin
    WriteLine('');
    size := s.Read(buffer, sizeof(buffer));
    while size > 0 do
    begin
      Source.Write(buffer, size, 0);
      size := s.Read(buffer, sizeof(buffer));
    end;
  end;

var
  streamout: TPooledMemoryStream;
begin
  if (FCompress) and (stream.Size > 0) then
  begin
    streamout := TPooledMemoryStream.Create;
    try
      stream.Seek(0, soFromBeginning);
      if SameText(FResponse.AsObject.S['Content-Encoding'], 'gzip') then
        CompressGZipStream(stream, streamout, FCompressLevel)
      else
        { full zlib stream (RFC 1950), as required by Content-Encoding: deflate }
        CompressStream(stream, streamout, FCompressLevel);
      WriteLine(format(AnsiString('Content-Length: %d'), [streamout.size]));
      streamout.Seek(0, soFromBeginning);
      SendIt(streamout);
    finally
      streamout.Free;
    end;
  end
  else
  begin
    WriteLine(format(AnsiString('Content-Length: %d'), [Stream.size]));
    Stream.Seek(0, soFromBeginning);
    SendIt(Stream);
  end;
end;

function THTTPStub.Upgrade: Cardinal;

  function doWebSocket04: Cardinal;

    function getKeyNumber(const key: string; out spaces: Cardinal): Cardinal;
    var
      i: Integer;
    begin
      Result := 0;
      spaces := 0;
      for i := 1 to Length(key) do
        case key[i] of
          '0'..'9': Result := Result*10 + ord(key[i]) - ord('0');
          ' ': inc(spaces);
        end;
    end;

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

  var
    key1, key2, origin, protocol: ISuperObject;

    location: RawByteString;
    keyNumber1, keyNumber2, space1, space2: Cardinal;
    challenge: packed record
      part1, part2: Cardinal;
      key3: array[0..7] of Byte;
    end;
    response: TBytes;
  begin
    Result := 0;
    origin := FRequest['env.origin'];
    if not ObjectIsType(origin, stString) then Exit;

    key1 := FRequest['env.sec-websocket-key1'];
    if not ObjectIsType(key1, stString) then
      Exit;
    key2 := FRequest['env.sec-websocket-key2'];
    if not ObjectIsType(key2, stString) then
      Exit;
    if Source.Read(challenge.key3, SizeOf(challenge.key3), 0) <> SizeOf(challenge.key3) then
      Exit;
    if Copy(origin.AsString, 1, 8) = 'https://' then
      location := RawByteString('wss://' + FRequest.s['env.host'] + FRequest.S['uri'])
    else
      location := RawByteString('ws://' + FRequest.s['env.host'] + FRequest.S['uri']);
    keyNumber1 := getKeyNumber(key1.AsString, space1);
    keyNumber2 := getKeyNumber(key2.AsString, space2);
    if (space1 = 0) or (space2 = 0) then
      Exit;
    if (keyNumber1 mod space1 <> 0) or (keyNumber2 mod space2 <> 0) then
      Exit;
    challenge.part1 := bigendian(keyNumber1 div space1);
	  challenge.part2 := bigendian(keyNumber2 div space2);

    var MD5 := THashMD5.Create;
    MD5.Update(challenge, SizeOf(challenge));
    response := MD5.HashAsBytes;

    WriteLine('HTTP/1.1 101 WebSocket Protocol Handshake');
	  WriteLine('Upgrade: WebSocket');
	  WriteLine('Connection: Upgrade');
	  WriteLine('Sec-WebSocket-Location: ' + location);
	  WriteLine('Sec-WebSocket-Origin: ' + RawByteString(origin.AsString) );
    protocol := FRequest['env.sec-websocket-protocol'];
    if ObjectIsType(protocol, stString) then
      WriteLine('Sec-WebSocket-Protocol: ' + RawByteString(protocol.asstring));
    WriteLine('');
    Source.Write(response[0], Length(response), 0);
    Source.Flush;
    Result := WebSocket;
  end;

  function doWebSocketNew: Cardinal;
  var
    key, origin: ISuperObject;
    response: string;
  begin
    Result := 0;
    origin := FRequest['env.sec-websocket-origin'];
    if not ObjectIsType(origin, stString) then
    begin
      origin := FRequest['env.origin'];
      if not ObjectIsType(origin, stString) then
        Exit;
    end;

    key := FRequest['env.sec-websocket-key'];
    if not ObjectIsType(key, stString) then Exit;

    response := TNetEncoding.Base64String.EncodeBytesToString(
      THashSHA1.GetHashBytes(key.AsString + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11')
    );

    WriteLine('HTTP/1.1 101 WebSocket Protocol Handshake');
    WriteLine('Upgrade: websocket');
    WriteLine('Connection: Upgrade');
    WriteLine('Sec-WebSocket-Origin: ' + RawByteString(origin.AsString));
    WriteLine('Sec-WebSocket-Accept: ' + RawByteString(response));
    WriteLine('');
    Source.Flush;
    Result := WebSocket;
  end;

begin
  Result := 0;
  if SameText(FRequest.S['env.upgrade'], 'WebSocket') then
  begin
    FWebSocketVersion := FRequest.I['env.sec-websocket-version'];
    case FWebSocketVersion of
      0: Result := doWebSocket04;
    else
      // 4 > 13
      Result := doWebSocketNew;
    end;
  end;
end;

{$if defined(DEBUG)}

{ TLuaStackInfo }

function TLuaStackInfo.Pop: PLuaStackInfo;
begin
  Result := Next;
  FreeMem(@Self);
end;

function TLuaStackInfo.Push(line: Integer; name,
  source: PAnsiChar): PLuaStackInfo;
begin
  GetMem(Result, SizeOf(TLuaStackInfo));
  Result.next := @self;
  Result.line := line;
  Result.name := name;
  Result.source := source;
end;

{$endif}

{ TRequestProcessor }

class function TRequestProcessor.DecodeSession(const Request: THTTPMessage;
  const GetPassProc: TGetPasswordProc): ISuperObject;
var
  key, iv: PByte;
begin
  GetPassProc(key, iv);
  if (key <> nil) and (iv <> nil) then
  begin
    var obj := Request.AsObject['cookies'].AsObject[COOKIE_NAME];
    case ObjectGetType(obj) of
      stString:
        Result := DecodeObject(obj.AsString, key, iv);
      stArray:
        Result := DecodeObject(obj.AsArray.S[0], key, iv);
    else
      Result := TSuperObject.Create(stObject);
    end;
    if not ObjectIsType(Result, stObject) then
      Result := TSuperObject.Create(stObject);
  end
  else
    Result := TSuperObject.Create(stObject);
end;

class function TRequestProcessor.EncodeSession(const Response: THTTPMessage;
  const Session: ISuperObject; const GetPassProc: TGetPasswordProc): Boolean;
var
  key, iv: PByte;
begin
  Result := False;
  GetPassProc(key, iv);
  if (key <> nil) and (iv <> nil) then
  begin
    Response.S['Set-Cookie[]'] := COOKIE_NAME + '=' + EncodeObject(Session, key, iv) + '; path=/';
    Result := True;
  end;
end;

class procedure TRequestProcessor.DecodeBody(const Request: THTTPMessage;
  const Params: ISuperObject);
begin
  { Merge query params }
  Params.Merge(Request['params'], true);

  { Merge body }
  if (Request.I['content-length'] > 0) then
  begin
    var f := StrRScan(PChar(Request.S['content-type[0]']), '/');
    if f <> nil then
      if f = '/json' then
      begin
        Params.Merge(Request.ContentObject);
        Params.AsObject.S['format'] := 'json';
      end
      else if f = '/xml' then
      begin
        Params.Merge(XMLParseStream(Request.Content, true));
        Params.AsObject.S['format'] := 'xml';
      end
      else if f = '/x-www-form-urlencoded' then
      begin
        var obj := HTTPInterprete(PSOChar(Request.ContentString), True, '&', False, DEFAULT_CP);
        try
          Params.Merge(obj, True);
          Params.AsObject.S['format'] := 'html';
        finally
          obj := nil;
        end;
      end;
  end;
end;

class function TRequestProcessor.ProcessRoute(IsWebSocket: Boolean;
  const Context: TSuperRttiContext; const Formats: ISuperObject;
  const Request: THTTPMessage; const Params: ISuperObject): TRttiInstanceType;

type
  TScope = (sController, sView, sWebsocket);

  function Scope(View: Boolean): TScope;
  begin
    if IsWebSocket then
      Result := sWebsocket
    else if View then
      Result := sView
    else
      Result := sController;
  end;

  function KindUnitName(View: Boolean): string;
  begin
    case Scope(View) of
      sController: Result := 'controller';
      sView:       Result := 'view';
      sWebsocket:  Result := 'websocket';
    end;
  end;

  function KindClassName(View: Boolean): string;
  begin
    case Scope(View) of
      sController: Result := 'Controller';
      sView:       Result := 'View';
      sWebsocket:  Result := 'Websocket';
    end;
  end;

  function KindClass(View: Boolean): TClass;
  begin
    case Scope(View) of
      sController: Result := TActionController;
      sView:       Result := TActionView;
      sWebsocket:  Result := TActionWebsocket;
    else
      Result := nil;
    end;
  end;

  function ResolveController(const Name: string; var Klass: TRttiInstanceType; var Namespace: string): Boolean;

    function BuildQualifiedClassName(View: Boolean): string;
    begin
      (* {name}_{controller_or_websocket_or_view}.T{Name}{ControllerOrWebsocketOrView} *)
      Result := Format('%s_%s.T%s%s', [Name, KindUnitName(View), CamelCase(Name), KindClassName(View)]);
    end;

    function FindKlass(View: Boolean): TRttiInstanceType;
    begin
      var K := Context.Context.FindType(BuildQualifiedClassName(View));

      if (K <> nil) and (K is TRttiInstanceType) then
        Result := TRttiInstanceType(K)
      else
        Exit(nil);

      { Check if Ancestor matches }
      if not Result.MetaclassType.InheritsFrom(KindClass(View)) then
        Exit(nil);

      { Check if Namespace matches }
      if Namespace <> '' then
      begin
        var NamespaceFound := False;

        for var A in Result.GetAttributes do
          if A is NamespaceAttribute then
            if (A as NamespaceAttribute).Namespace = Namespace then
            begin
              NamespaceFound := True;
              Break;
            end;

        if not NamespaceFound then
          Result := nil;
      end;
    end;

  begin
    Klass := FindKlass(False);
    if Klass = nil then
      Klass := FindKlass(True);
    Result := Klass <> nil;
  end;

  procedure SetAction(const A, F: PSOChar; var Action, Format: string); inline;
  begin
    Assert(F > A);

    var S := '';
    SetString(S, A, F - A);

    Action := LowerCase(S);
    Format := LowerCase(F + 1);
  end;

  function HasAction(Klass: TRttiInstanceType; const Action, Method: string): Boolean; overload;
  begin
    { Websockets don't have actions }
    if not IsWebSocket and (Klass <> nil) then
      Result := Klass.GetMethod(Action + '_' + Method) <> nil
    else
      Result := False;
  end;

  function HasAction(Klass: TRttiInstanceType; const Action: string): Boolean; overload;
  begin
    Result := HasAction(Klass, Action, Request.AsObject.S['method']);
  end;

var
  Namespace, Controller, Action, Format: string;
  uri: ISuperObject;
begin
  Result := nil;

  Namespace  := '';
  Controller := '';
  Action     := '';
  Format     := '';

  uri := HTTPInterprete(PSOChar(Request.S['uri']), False, '/', False, DEFAULT_CP);

  { obj[0] is always empty, obj[1] = '' if uri = '' }
  if (uri.AsArray.Length = 2) and (uri.AsArray.S[1] <> '') then
  begin
    // Should match
    // * /:controller_or_websocket
    // * /:action(.:format) [ controller only ]

    var S := Trim(uri.AsArray.S[1]);
    var P := PSOChar(S);
    var F := StrRScan(P, '.');

    if F = nil then
    begin
      S := LowerCase(S);
      if ResolveController(S, Result, Namespace) or IsWebSocket then
        Controller := S
      else
        Action := S;
    end
    else
      SetAction(P, F, Action, Format);
  end
  else if uri.AsArray.Length > 2 then
  begin
    // (/:namespace/...)/:controller(/:action.:format)(/:id)
    //
    // Should match
    // * :controller + :action(.format)
    // * :controller + :action(.format) + :id
    // * :namespace + :controller
    // * :namespace + :controller + :action(.format)
    // * :namespace + :controller + :action(.format) + :id

    var LastNamespace  := '';
    var LastController := '';
    var LastAction     := '';

    for var I := 1 to uri.AsArray.Length - 1 do
    begin
      var S := Trim(uri.AsArray.S[I]);
      var P := PSOChar(S);
      var F := StrRScan(P, '.');

      if (Result = nil) and (F = nil) then
      begin
        S := LowerCase(S);
        if ResolveController(S, Result, Namespace) then
          Controller := S
        else
        begin
          { Keep URI path history }
          if LastController <> '' then
            LastNamespace := LastNamespace + '/' + LastController;
          LastController := LastAction;
          LastAction := S;

          Namespace := Namespace + '/' + S;
        end;
      end
      else if F = nil then { Klass <> nil }
      begin
        Assert(Result <> nil);

        var O: ISuperObject := WrapValue(P);
        if ObjectIsType(O, stString) then
        begin
          var A := LowerCase(O.AsString);
          if HasAction(Result, A) then
            Action := A;
        end
        else
          Params.AsObject['id'] := O;

        Break;
      end
      else { F <> nil }
        SetAction(P, F, Action, Format);
    end;

    { No controller found }
    if Result = nil then
    begin
      Namespace  := LastNamespace;
      Controller := LastController;
      Action     := LastAction;
    end;
  end;

  // default controller is application
  if Controller = '' then
    Controller := 'application';

  // default action matches ruby on rails conventions
  if IsWebSocket then
  begin
    Action := '';
    Format := '';
  end
  else
  begin
    // REST routes based on Method and presence of "id"
    if Action = '' then
    begin
      var Method := UpperCase(Request.AsObject.S['method']);
      var HasId := Params.AsObject['id'] <> nil;

      if HasId then
      begin
        if Method = 'GET' then
          Action := 'show'
        else if (Method = 'PUT') or (Method = 'PATCH') then
        begin
          Action := 'update';
          Request.AsObject.S['method'] := 'POST'; // overrides request method
        end
        else if Method = 'DELETE' then
        begin
          Action := 'delete';
          Request.AsObject.S['method'] := 'POST'; // overrides request method
        end;
      end
      else if Method = 'POST' then
        Action := 'create';
    end;

    // default action is index
    if Action = '' then
      Action := 'index';

    // detect current formats
    if Format = '' then
    begin
      var Accept := Request.AsObject['accept'];
      if ObjectIsType(Accept, stArray) and (Accept.AsArray.Length >= 1) then
      begin
        var A := Accept.AsArray.S[0];
        if A = Formats.S['json.content'] then      // application/json
          Format := 'json'
        else if A = Formats.S['xml.content'] then  // text/xml
          Format := 'xml'
        else if A = Formats.S['html.content'] then // text/html
          Format := 'html'
        else if A = Formats.S['text.content'] then  // plain/text
          Format := 'text';
      end;
    end;

    // default format is html
    if Format = '' then
      Format := 'html';
  end;

  // Klass is still nil but we now have defaults to search for a controller
  if (Result = nil) and not IsWebSocket then
    if not (ResolveController(Controller, Result, Namespace) and HasAction(Result, Action)) then
      Result := nil;

  // Klass is not nil but it is a TActionView
  if (Result <> nil) and Result.MetaclassType.InheritsFrom(TActionView) then
    Result := nil;

  // store action context
  with Params.AsObject do
  begin
    S['namespace']  := Namespace;
    S['controller'] := Controller;
    S['action']     := Action;
    S['format']     := Format;
  end;
end;

class function TRequestProcessor.PrepareSendFile(const FileToSend: string;
  const Formats: ISuperObject; const Response: THTTPMessage;
  var Compress: Boolean; var ErrorCode: Integer): Boolean;
begin
  Result := False;

  var Ext := LowerCase(ExtractFileExt(FileToSend));
  System.Delete(ext, 1, 1);
  Response.AsObject.S['Content-Type'] := Formats.S[ext + '.content'];
  if Response.AsObject.S['Content-Type'] = '' then
    Response.AsObject.S['Content-Type'] := 'application/binary';
  Compress := Formats.B[ext + '.istext'];

  if FileExists(FileToSend) then
  begin
    ErrorCode := 200;
    Result := True;
    Response.AsObject.S['Content-Disposition'] := 'attachment; filename=' + ExtractFileName(FileToSend);
    Response.AsObject.S['Content-Transfer-Encoding'] := 'binary';
  end
  else
    ErrorCode := 404;
end;

class function TRequestProcessor.PrepareStaticFile(const Formats: ISuperObject;
  const Request: THTTPMessage; const Params: ISuperObject; const Response: THTTPMessage;
  var IsStatic, Compress: Boolean; var FileToSend: string; var ErrorCode: Integer;
  const GetRootPathProc: TGetPathProc): Boolean;
const
  STATIC_DIR = 'static';
var
  path, static_path, final_path, rel, str: string;
  rec: TSearchRec;
begin
  Result := False;

  path := GetRootPathProc;
  static_path := path + STATIC_DIR;

  rel := Request.S['uri'];
  if AnsiChar(rel[Length(rel)]) in ['/','\'] then
    rel := rel + 'index.' + Params.AsObject.S['format'];

  str := static_path + rel;
  final_path := ExpandFileName(str);
  str := '';

  if Pos(static_path, final_path) <> 1 then
    Exit;

  if FindFirst(final_path, faAnyFile, rec) = 0 then
  begin
    Result := True;

    { Although the rec.Time is platform bounded and deprecated (on Windows) in
      favor of the new TimeStamp property, all we need here is an integer
      timestamp that changes with the resource }
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
    IsStatic := True;
    if Request.B['env.if-none-match'] and
      (Request.S['env.if-none-match'] = IntToStr(rec.Time) + '-' + IntToStr(rec.Size)) then
    begin
      Compress := False;
      ErrorCode := 304;
      FileToSend := '';
      FindClose(rec);
      Exit;
    end;
    Response.AsObject.S['Cache-Control'] := 'public, no-cache';
    Response.AsObject.S['ETag'] := IntToStr(rec.Time) + '-' + IntToStr(rec.Size);
    FileToSend := final_path;
    Compress := Formats.B[Params.AsObject.S['format'] + '.istext'];
    FindClose(rec);
    ErrorCode := 200;
  {$WARN SYMBOL_DEPRECATED ON}
  {$WARN SYMBOL_PLATFORM ON}
  end;
end;

class function TRequestProcessor.ProcessAction(Klass: TRttiInstanceType;
  const Context: TSuperRttiContext; const Source: IReadWrite;
  const Formats, Session: ISuperObject;
  const Request: THTTPMessage; const Params: ISuperObject;
  const Response: THTTPMessage; const Return: ISuperObject;
  var ErrorCode: Integer; var FileToSend: string): Boolean;
begin
  Result := False;
  if Klass = nil then
    Exit;

  // double check
  Assert(Klass.MetaclassType.InheritsFrom(TActionController));

  { more direct way }
  var Inst := TActionControllerClass(Klass.MetaclassType).Create;
  try
    // double check;
    Assert(Inst is TActionController);
    if TActionController(Inst).InstanceInvoke(Context, Source,
         Request, Params, Response, Return, Session,
         ErrorCode, FileToSend
       )
    then
      Result := True;
  finally
    Inst.Free;
  end;
end;

class function TRequestProcessor.ProcessView(Klass: TRttiInstanceType;
  const Context: TSuperRttiContext; const Source: IReadWrite; const Formats,
  Session: ISuperObject; const Request: THTTPMessage;
  const Params: ISuperObject; const Response: THTTPMessage;
  const Return: ISuperObject; var ErrorCode: Integer;
  var FileToSend: string): Boolean;
begin
  Result := False;
  if Klass = nil then
    Exit;

  // double check
  Assert(Klass.MetaclassType.InheritsFrom(TActionView));

  { more direct way }
  var Inst := TActionViewClass(Klass.MetaclassType).Create;
  try
    // double check;
    Assert(Inst is TActionView);
    if TActionView(inst).InstanceInvoke(Context, Source,
         Request, Params, Response, Return, Session,
         ErrorCode, FileToSend
       )
    then
      Result := True
  finally
    Inst.Free;
  end;
end;

class function TRequestProcessor.ResolveView(const Context: TSuperRttiContext;
  const Formats: ISuperObject; const Request: THTTPMessage;
  const Params: ISuperObject): TRttiInstanceType;
begin
  (* {name}_{controller}.T{Name}{View} *)
  var QualifiedClassName := Format('%s_view.T%sView', [Params.S['controller'], CamelCase(Params.S['controller'])]);

  var K := Context.Context.FindType(QualifiedClassName);

  if (K = nil) or not (K is TRttiInstanceType) then
    Result := nil
  else
    Result := TRttiInstanceType(K);

  { Check if Ancestor matches }
  if (Result <> nil) and not Result.MetaclassType.InheritsFrom(TActionView) then
    Result := nil;
end;

class procedure TRequestProcessor.ApplyContentType(const Formats,
  Params: ISuperObject; const Response: THTTPMessage);
begin
  { Params have been fully decoded here so we can set the Response Content-Type }
  with Params.AsObject do
    if Formats[S['format'] + '.charset'] <> nil then
      Response.AsObject.S['Content-Type'] := Formats.S[S['format'] + '.content'] + '; charset=' + Formats.S[S['format'] + '.charset']
    else
      Response.AsObject.S['Content-Type'] := Formats.S[S['format'] + '.content'];
end;

class function TRequestProcessor.Authenticate(const AuthData: string;
  var User: string; const GetAuthProc: TGetAuthProc): Boolean;
var
  P: Integer;
  Kind, Data, Password: string;
begin
  Result := False;

  P := Pos(' ', AuthData);
  if P <= 1 then
    Exit;

  Kind := Copy(AuthData, 1, P - 1);
  Data := Copy(AuthData, P + 1, MaxInt);

  if SameText(Kind, 'basic') then
  begin
    Data := Base64ToStr(Data);

    P := Pos(':', Data);
    if P <= 1 then
      Exit;

    User := Copy(Data, 1, P - 1);
    Password := Copy(Data, P + 1, MaxInt);

    Result := GetAuthProc(Kind, User, Password);
  end;
end;

class procedure TRequestProcessor.ApplyCompress(var Compress: Boolean;
  const Request, Response: THTTPMessage);
var
  ae: string;
begin
  if Compress then
  begin
    ae := Request.S['env.accept-encoding'];
    { Prefer gzip over deflate for backward compatibility: legacy DOR clients
      decode "deflate" by prepending a zlib header to what they assume is a raw
      stream, which breaks against the now-conformant full zlib stream. Their
      gzip decode path (unchanged) reads the body as raw deflate and works with
      our conformant gzip output, so routing through gzip keeps old AND new
      clients working. Only clients accepting deflate but not gzip (necessarily
      strict/standard clients) get deflate, for which the conformant stream is
      correct. }
    if Pos('gzip', ae) > 0 then
      Response.AsObject.S['Content-Encoding'] := 'gzip'
    else if Pos('deflate', ae) > 0 then
      Response.AsObject.S['Content-Encoding'] := 'deflate'
    else
      Compress := False;
  end;
end;

class function TRequestProcessor.RenderScript(const Formats, Session: ISuperObject;
  const Request: THTTPMessage; const Params: ISuperObject;
  const Response: THTTPMessage; const Return: ISuperObject;
  var ErrorCode: Integer;
  const GetRootPathProc: TGetPathProc): Boolean;

  procedure Render(const Str: string);
  begin
    Response.Content.WriteString(Str, False, DEFAULT_CP);
  end;

var
  state: Plua_State;
{$if defined(DEBUG)}
  LuaDebug: TLuaDebug;
{$endif}

  procedure printerror;
  begin
    Response.Content.Clear;
    Render(
      '<!doctype html>'#10 +
      '<html lang="en">'#10+
      '<head>'#10+
      '  <meta charset="utf-8"/>'+
      '  <title>LUA Error</title>'#10+
      '  <style>'#10+
      '    body { background-color: #fff; color: #333; }'#10+
      '    body, p, ol, ul, td {'#10+
      '      font-family: verdana, arial, helvetica, sans-serif;'#10+
      '      font-size:   13px;'#10+
      '      line-height: 18px;'#10+
      '    }'#10+
      '    pre {'#10+
      '      background-color: #eee;'#10+
      '      padding: 10px;'#10+
      '      font-size: 11px;'#10+
      '    }'#10+
      '  </style>'#10+
      '</head>'#10+
      '<body>'#10+
      '<h1>Error</h1>'#10+
      '<p>'#10+
      '  Showing <i>' + Request.AsObject.S['uri'] + '</i>'#10+
      '  <pre><code>'+ string(UTF8String(lua_tostring(state, 1))) + '</code></pre>'#10+
      '</p>'#10
    );

{$if defined(DEBUG)}
   Render(
      '<h1>Trace</h1>'#10+
      '<pre><code>'
   );

    while LuaDebug.LuaStack <> nil do
    begin
      Render(Format('%s:%d:in %s'#10, [LuaDebug.LuaStack.source, LuaDebug.LuaStack.line, LuaDebug.LuaStack.name]));
      LuaDebug.LuaStack := LuaDebug.LuaStack.Pop;
    end;

   Render('</pre></code>'#10);
   Render(
     '<h1>Params</h1>'#10+
     '<pre><code>'+
     Return.AsJson(True, False) +
     '</pre></code>'#10
   );
{$endif}

    Render('</body></html>');
  end;

  procedure PushObject(const Obj: ISuperObject; const GlobalName: string);
  var
    keys: ISuperObject;
    ite: TSuperObjectIter;
  begin
    keys := TSuperObject.Create(stArray);

    if ObjectFindFirst(Obj, ite) then
    repeat
      lua_pushsuperobject(state, ite.val);
      lua_setglobal(state, PAnsiChar(UTF8Encode(ite.key)));
      keys.AsArray.Add(ite.key);
    until not ObjectFindNext(ite);
    ObjectFindClose(ite);

    lua_pushsuperobject(state, keys);
    lua_setglobal(state, PAnsiChar(UTF8Encode(GlobalName)));
  end;

const
  VIEWS_DIR = 'view';
  LAYOUTS_DIR = 'layout';
var
  path, views_path, layouts_path, rel, str, final_path: string;
begin
  Result := False;

  path := GetRootPathProc;
  views_path := path + VIEWS_DIR + '\';
  layouts_path := path + LAYOUTS_DIR + '\';

  with Params.AsObject do
    rel := S['controller'] + '/' + S['action'] + '.' + S['format'];

  str := views_path + rel;
  final_path := ExpandFileName(str);
  str := '';

  if Pos(views_path, final_path) <> 1 then
    Exit;

  if FileExists(final_path) then
  begin
{$if defined(DEBUG)}
    LuaDebug := TLuaDebug.Create;
{$endif}
    state := lua_newstate(lua_Alloc(@lua_app_alloc), nil);
    try
      luaL_openlibs(state);

      lua_pushlightuserdata(state, Response);
      lua_setglobal(state, '@response');
      lua_pushcfunction(state, lua_CFunction(@lua_print)); // need @response
      lua_setglobal(state, 'print');

      lua_pushcfunction(state, lua_CFunction(@lua_gettickcount));
      lua_setglobal(state, 'gettickcount');

{$if defined(DEBUG)}
      lua_pushlightuserdata(state, LuaDebug);
      lua_setglobal(state, '@debug');
      lua_sethook(state, lua_Hook(@script_hook), LUA_MASKCALL or LUA_MASKRET or LUA_MASKLINE, 0);
{$endif}

      PushObject(Params, '__param_keys__');
      PushObject(Return, '__return_keys__');

      lua_pushsuperobject(state, Session);
      lua_setglobal(state, 'session');

      if lua_processsor_dofile(state, final_path, PAnsiChar(UTF8String(VIEWS_DIR + '/' + rel)), 't') then
      begin
        with Params.AsObject do
          rel := S['controller'] + '.' + S['format'];

        str := layouts_path + rel;
        final_path := ExpandFileName(str);
        str := '';

        if Pos(layouts_path, final_path) <> 1 then
          Exit;

        if FileExists(final_path) then
        begin
          if not lua_processsor_dofile(state, final_path, PAnsiChar(UTF8String(LAYOUTS_DIR + '/' + rel)), 't') then
            printerror;
        end
        else
        begin
          rel := 'application.' + Params.AsObject.S['format'];

          str := layouts_path + rel;
          final_path := ExpandFileName(str);
          str := '';

          if Pos(layouts_path, final_path) <> 1 then
            Exit;

          if FileExists(final_path) then
            if not lua_processsor_dofile(state, final_path, PAnsiChar(UTF8String(LAYOUTS_DIR + '/' + rel)), 't') then
               printerror;
        end;
      end
      else
        printerror;
    finally
{$if defined(DEBUG)}
      while LuaDebug.LuaStack <> nil do
        LuaDebug.LuaStack := LuaDebug.LuaStack.Pop;
      LuaDebug.Free;
{$endif}
      lua_close(state);
    end;

    if ErrorCode = 0 then
      ErrorCode := 200;

    Result := True;
  end;
end;

class function TRequestProcessor.BuildFormats: ISuperObject;
begin
  { https://developer.mozilla.org/fr/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Complete_list_of_MIME_types }

  Result := TSuperObject.Create;
  with Result do
  begin
    S['aac.content']   := 'audio/aac';
    S['wav.content']   := 'audio/x-wav';
    S['oga.content']   := 'audio/ogg';
    S['weba.content']  := 'audio/webm';

    S['avi.content']   := 'video/ms-video';
    S['ogv.content']   := 'video/ogg';
    S['mpeg.content']  := 'video/mpeg';
    S['webm.content']  := 'video/webm';

    S['bz.content']    := 'application/x-bzip';
    S['bz2.content']   := 'application/x-bzip2';
    S['rar.content']   := 'application/x-rar-compressed';
    S['tar.content']   := 'application/x-tar';
    S['zip.content']   := 'application/zip';
    S['7z.content']    := 'application/x-7z-compressed';

    S['pdf.content']   := 'application/pdf';
    S['ogx.content']   := 'application/ogg';
    S['doc.content']   := 'application/msword';
    S['docx.content']  := 'application/vnd.openxmlformats-officedocument.wordprocessingml.document';
    S['ppt.content']   := 'application/vnd.ms-powerpoint';
    S['pptx.content']  := 'application/vnd.openxmlformats-officedocument.presentationml.presentation';
    S['xls.content']   := 'application/vnd.ms-excel';
    S['xlsx.content']  := 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet';

    S['epub.content']  := 'application/epub+zip';
    S['jar.content']   := 'application/java-archive';
    S['odp.content']   := 'application/vnd.oasis.opendocument.presentation';
    S['ods.content']   := 'application/vnd.oasis.opendocument.spreadsheet';
    S['odt.content']   := 'application/vnd.oasis.opendocument.text';

    S['eot.content']   := 'application/vnd.ms-fontobject';
    S['otf.content']   := 'font/otf';
    S['ttf.content']   := 'font/ttf';
    S['woff.content']  := 'font/woff';
    S['woff2.content'] := 'font/woff2';

    S['txt.content']   := 'text/plain';
    S['txt.charset']   := DEFAULT_CHARSET;
    B['txt.istext']    := True;

    S['text.content']  := 'text/plain';
    S['text.charset']  := DEFAULT_CHARSET;
    B['text.istext']   := True;

    S['css.content']   := 'text/css';
    B['css.istext']    := True;

    S['csv.content']   := 'text/csv';
    S['csv.charset']   := DEFAULT_CHARSET;
    B['csv.istext']    := True;

    S['rtf.content']   := 'application/rtf';
    S['rtf.charset']   := DEFAULT_CHARSET;
    B['rtf.istext']    := True;

    S['htm.content']   := 'text/html';
    S['htm.charset']   := DEFAULT_CHARSET;
    B['htm.istext']    := True;

    S['html.content']  := 'text/html';
    S['html.charset']  := DEFAULT_CHARSET;
    B['html.istext']   := True;

    S['ics.content']   := 'text/calendar';
    B['ics.istext']    := True;

    S['xhtml.content'] := 'application/xhtml+xml';
    S['xhtml.charset'] := DEFAULT_CHARSET;
    B['xhtml.istext']  := True;

    S['xml.content']   := 'application/xml';
    B['xml.istext']    := True;

    S['json.content']  := 'application/json';
    B['json.istext']   := True;

    S['png.content']   := 'image/png';
    S['jpeg.content']  := 'image/jpeg';
    S['jpg.content']   := 'image/jpeg';
    S['gif.content']   := 'image/gif';
    S['ico.content']   := 'image/x-icon';
    S['tif.content']   := 'image/tiff';
    S['tiff.content']  := 'image/tiff';
    S['webp.content']  := 'image/webp';

    S['js.content']    := 'application/javascript';
    B['js.istext']     := True;

    S['ts.content']    := 'application/typescript';
    B['ts.istext']     := True;

    S['svg.content']   := 'image/svg+xml';
    B['svg.istext']    := True;
  end;
end;

end.



