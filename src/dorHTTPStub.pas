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
  dorSocketStub, dorUtils, classes, supertypes, superobject;

type
  THTTPMessage = class(TSuperObject)
  private
    FContent: TPooledMemoryStream;
    function GetContentString: SOString;
  public
    property Content: TPooledMemoryStream read FContent;
    property ContentString: SOString read GetContentString;
    constructor Create(jt: TSuperType = stObject); override;
    destructor Destroy; override;
    procedure Clear(all: boolean = false); override;
  end;

{$IFDEF DEBUG}
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
{$ENDIF}

  THTTPStub = class(TClientStub)
  private
    FRequest: THTTPMessage;
    FResponse: THTTPMessage;
    FReturn: ISuperObject;
    FParams: ISuperObject;
    FFormats: ISuperObject;
    FSession: ISuperObject;
    FContext: TSuperRttiContext;
    FErrorCode: Integer;
    FCompress: Boolean;
    FCompressLevel: Integer;
    FSendFile: string;
    FIsStatic: Boolean;
    FWebSocketVersion: Integer;
{$IFDEF DEBUG}
    FLuaStack: PLuaStackInfo;
{$ENDIF}
    function DecodeFields(str: PChar): boolean;
    function DecodeCommand(str: PChar): boolean;
    procedure WriteLine(str: RawByteString);
    procedure SendEmpty;
    procedure SendFile(const filename: string);
    procedure SendStream(Stream: TStream);
    procedure RenderInternal;
    function RenderScript: Boolean;
    function DecodeContent: boolean; virtual;
    function doAuthenticate(const AuthData: string; var User: string): Boolean; virtual;
    procedure doBeforeProcessRequest; virtual;
    procedure doAfterProcessRequest; virtual;
  protected
    function BasicAuth(const User, Password: string): Boolean; virtual;
    function ProcessRequest: Boolean; virtual;
    function Run: Cardinal; override;
    function Upgrade: Cardinal; virtual;
    function WebSocket: Cardinal; virtual;
    procedure GetPassPhrase(var key, iv: PByte); virtual;
    function GetRootPath: string; virtual;
  public
    constructor CreateStub(AOwner: TSocketServer; const ASocket: IReadWrite); override;
    destructor Destroy; override;
    procedure Render(const obj: ISuperObject; format: boolean = false); overload;
    procedure Render(const str: string); overload;
    procedure Redirect(const location: string); overload;
    procedure Redirect(const controler, action: string; const id: string = ''); overload;
    property FileToSend: string read FSendFile write FSendFile;
    property Return: ISuperObject read FReturn;
    property Request: THTTPMessage read FRequest;
    property Response: THTTPMessage read FResponse;
    property Session: ISuperObject read FSession;
    property Params: ISuperObject read FParams;
    property ErrorCode: Integer read FErrorCode write FErrorCode;
    property Compress: Boolean read FCompress write FCompress;
    property CompressLevel: Integer read FCompressLevel write FCompressLevel;
    property Context: TSuperRttiContext read FContext;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, StrUtils, Rtti,
  superxmlparser,
  dorOpenSSL, dorOpenSslHelpers, dorHTTP, dorLua,
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

  DEFAULT_CP = 65001;
  DEFAULT_CHARSET = 'utf-8';

  ReadTimeOut: Integer = 60000; // 1 minute
  COOKIE_NAME = 'Cookie';

function lua_print(state: Plua_State): Integer; cdecl;
var
  p: Pointer;
  i: Integer;
  o: ISuperObject;
begin
  lua_getglobal(state, '@this');
  p := lua_touserdata(state, -1);
  if p <> nil then
    for i := 1 to lua_gettop(state) do
    begin
      o := lua_tosuperobject(state, i);
      if o <> nil then
        THTTPStub(p).Render(o.AsString);
    end;
  Result := 0;
end;

function lua_gettickcount(state: Plua_State): Integer; cdecl;
begin
  lua_pushinteger(state, Integer(GetTickCount));
  Result := 1;
end;

{$IFDEF DEBUG}
procedure script_hook(L: Plua_State; ar: Plua_Debug); cdecl;
var
  h: THTTPStub;
begin
  lua_getglobal(L, '@this');
  h := lua_touserdata(L, -1);
  if h <> nil then
  begin
    case ar.event of
      LUA_HOOKLINE:
        h.FLuaStack.line := ar.currentline;
      LUA_HOOKCALL:
        begin
          lua_getinfo(L, 'Snl', ar);
          if ar.name <> nil then
            h.FLuaStack := h.FLuaStack.Push(ar.currentline, ar.name, ar.source)
          else
            h.FLuaStack := h.FLuaStack.Push(ar.currentline, ar.what, ar.source);
        end;
      LUA_HOOKRET :
        h.FLuaStack := h.FLuaStack.Pop;
    end;
  end;
end;
{$ENDIF}

function DecodeValue(const p: PChar): ISuperObject; inline;
begin
  try
    Result := TSuperObject.ParseString(p, False, False);
  except
    Result := nil;
  end;
  if Result = nil then
    Result := TSuperObject.Create(p);
end;

function MBUDecode(const str: RawByteString; cp: Word): UnicodeString;
begin
  SetLength(Result, MultiByteToWideChar(cp, 0, PAnsiChar(str), length(str), nil, 0));
  MultiByteToWideChar(cp, 0, PAnsiChar(str), length(str), PWideChar(Result), Length(Result));
end;

function HTTPInterprete(src: PSOChar; named: boolean = false; sep: SOChar = ';'; StrictSep: boolean = false; codepage: Integer = 0): ISuperObject;
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
    while ((not StrictSep and (src^ >= ' ')) or
          (StrictSep and (src^ <> #0))) and (src^ <> sep) do
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
//          value := DecodeValue(PChar(@S[i+1])) else
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
//        value := DecodeValue(PChar(s)) else
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

function THTTPMessage.GetContentString: SOString;
var
  Data: RawByteString;
begin
  FContent.Seek(0, soFromBeginning);
  SetLength(Data, FContent.Size);
  FContent.Read(PAnsiChar(Data)^, FContent.Size);
  Result := SOString(Data);
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
    with Request.ForcePath('env') do
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

  function max_block_size(total: Integer): Integer; inline;
  begin
    if total < BLOCK_SIZE then
      Result := total
    else
      Result := BLOCK_SIZE;
  end;

begin
  Result := True;
  ContentLength := Request.I['env.content-length'];
  if ContentLength > 0 then
  begin
    Request.FContent.Size := ContentLength;
    Request.FContent.Seek(0, soFromBeginning);
    total := ContentLength;
    repeat
      len := Source.Read(b[0], max_block_size(total), ReadTimeOut);
      if len > 0 then
        Request.FContent.Write(b[0], len);
      Dec(total, len);
    until (total = 0) or (len <= 0);
    Result := total = 0;

    ContentEncoding := Request.S['env.content-encoding'];
    if SameText(ContentEncoding, 'deflate') or SameText(ContentEncoding, 'gzip') then
    begin
      stream := TPooledMemoryStream.Create;
      try
        request.FContent.Seek(0, soFromBeginning);
        if SameText(ContentEncoding, 'deflate') then
          DecompressStream(Request.FContent, stream, True)
        else
          DecompressGZipStream(Request.FContent, stream);
        { Exchange request.FContent with stream of decompressed data }
        stream := InterlockedExchangePointer(Pointer(request.FContent), Pointer(stream));
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
  begin
    data := '';
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
          data := data + char(i);
          inc(uri, 3);
          dec(len, 3);
        end
        else
        begin
          Result := False;
          Exit;
        end;
      end
      else
      begin
        data := data + uri^;
        inc(uri, 1);
        dec(len, 1);
      end;
    end;
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

  Request.AsObject.S['method'] := Copy(str, 0, marker - str);
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
      Request.AsObject.S['uri'] := value
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
              Request['params.'+string(HTTPDecode(param))] := DecodeValue(PChar(string(HTTPDecode(value))));
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
  str := @str[5];

  // version major
  marker := str;
  while {$IFDEF UNICODE}(str^ < #256) and{$ENDIF}(AnsiChar(str^) in ['0'..'9']) do
    Inc(str);
  if (str > marker) and (str^ <> NL) then
  begin
    if TryStrToInt(copy(marker, 0, str - marker), i) then
      Request.I['http-version.major'] := i
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
      Request.I['http-version.minor']  := i
    else
      Exit;
  end
  else
    Exit;

  if (str^ <> NL) then
    Exit;

  Result := True;
end;

procedure THTTPStub.RenderInternal;
var
  clazz: TRttiType;
  inst: TObject;
begin
  with params.AsObject do
  begin
    clazz := Context.Context.FindType(format('%s_view.T%sView', [S['controller'], CamelCase(S['controller'])]));
    if (clazz <> nil) and (clazz is  TRttiInstanceType) then
    begin
      with TRttiInstanceType(clazz) do
        inst := GetMethod('create').Invoke(MetaclassType, []).AsObject;
      try
        if inst is TActionView then
          TActionView(inst).Invoke
        else
          ErrorCode := 404;
      finally
        inst.Free;
      end;
    end
    else
      ErrorCode := 404;
  end;
end;

function THTTPStub.RenderScript: Boolean;
var
  state: Plua_State;
  ite: TSuperObjectIter;
  path, str, rel: string;

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
      '</p>'#10);

{$IFDEF DEBUG}
   Render(
      '<h1>Trace</h1>'#10+
      '<pre><code>');

    while FLuaStack <> nil do
    begin
      Render(format('%s:%d:in %s'#10, [FLuaStack.source, FLuaStack.line, FLuaStack.name]));
      FLuaStack := FLuaStack.Pop;
    end;

   Render('</pre></code>'#10);
   Render(
     '<h1>Params</h1>'#10+
     '<pre><code>'+
     Return.AsJSon(true, false)+
     '</pre></code>'#10);
{$ENDIF}
    Render('</body></html>');
  end;

begin
  Result := False;
  with Params.AsObject do
  begin
    path := GetRootPath;
    rel := 'view/' + S['controller'] + '/' + S['action'] + '.' + S['format'];
    str := path + rel;
    if FileExists(str) then
    begin
      Result := True;
{$IFDEF DEBUG}
      FLuaStack := nil;
{$ENDIF}
      state := lua_newstate(@lua_app_alloc, nil);
      try
        luaL_openlibs(state);
        lua_pushlightuserdata(state, Self);
        lua_setglobal(state, '@this');
        lua_pushcfunction(state, @lua_print);
        lua_setglobal(state, 'print');
        lua_pushcfunction(state, lua_gettickcount);
        lua_setglobal(state, 'gettickcount');
{$IFDEF DEBUG}
        lua_sethook(state, @script_hook, LUA_MASKCALL or LUA_MASKRET or LUA_MASKLINE, 0);
{$ENDIF}

        if ObjectFindFirst(FParams, ite) then
        repeat
          lua_pushsuperobject(state, ite.val);
          lua_setglobal(state, PAnsiChar(UTF8Encode(ite.key)));
        until not ObjectFindNext(ite);
        ObjectFindClose(ite);

        if ObjectFindFirst(FReturn, ite) then
        repeat
          lua_pushsuperobject(state, ite.val);
          lua_setglobal(state, PAnsiChar(UTF8Encode(ite.key)));
        until not ObjectFindNext(ite);
        ObjectFindClose(ite);

        lua_pushsuperobject(state, FSession);
        lua_setglobal(state, 'session');

        if lua_processsor_dofile(state, str, PAnsiChar(UTF8String(rel)), 't') then
        begin
          rel := 'layout/' + S['controller'] + '.' + S['format'];
          str := path + rel;
          if FileExists(str) then
          begin
            if not lua_processsor_dofile(state, str, PAnsiChar(UTF8String(rel)), 't') then
              printerror;
          end
          else
          begin
            rel := 'layout/application.' + S['format'];
            str := path + rel;
            if FileExists(str) then
              if not lua_processsor_dofile(state, str, PAnsiChar(UTF8String(rel)), 't') then
                 printerror;
          end;
        end
        else
          printerror;
      finally
{$IFDEF DEBUG}
        while FLuaStack  <> nil do
          FLuaStack := FLuaStack.Pop;
{$ENDIF}
        lua_close(state);
      end;
      ErrorCode := 200;
      Exit;
    end;
  end;
  ErrorCode := 404;
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
begin
{$if defined(DEBUG)}
  TThread.NameThreadForDebugging(AnsiString(Self.ClassName));
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
            if pos('Upgrade', Request.S['env.connection']) > 0 then
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
                    Response.Content.WriteString(BugReport, False);
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

  { Cf. https://developer.mozilla.org/fr/docs/Web/HTTP/Basics_of_HTTP/MIME_types/Complete_list_of_MIME_types }

  FFormats := TSuperObject.Create;
  with FFormats do
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

    S['xhtml.content'] := 'application/xhtml+xml';
    S['xhtml.charset'] := DEFAULT_CHARSET;
    B['xhtml.istext']  := True;

    S['xml.content']   := 'application/xml';
    B['xml.istext']    := True;

    S['xml.content']   := 'text/calendar';
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

    S['css.content']   := 'text/css';
    B['css.istext']    := True;

    S['js.content']    := 'application/javascript';
    B['js.istext']     := True;

    S['ts.content']    := 'application/typescript';
    B['ts.istext']     := True;

    S['svg.content']   := 'image/svg+xml';
    B['svg.istext']    := True;
  end;
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
  key, iv: PByte;
  obj: ISuperObject;
begin
  if FCompress then
  begin
    if Pos('deflate', Request.S['env.accept-encoding']) > 0 then
      FResponse.AsObject.S['Content-Encoding'] := 'deflate'
    else
      FCompress := False;
  end;

  FResponse.AsObject.S['Server'] := 'DOR 1.0';
  if not FIsStatic then
  begin
    GetPassPhrase(key, iv);
    if (key <> nil) and (iv <> nil) then
      FResponse.S['Set-Cookie[]'] := COOKIE_NAME + '=' + EncodeObject(FSession, key, iv) + '; path=/';
  end;
  WriteLine(HttpResponseStrings(FErrorCode));
  if ObjectFindFirst(Response, ite) then
  repeat
    case ObjectGetType(ite.val) of
      stArray:
        for obj in ite.val do
          WriteLine(RawByteString(ite.key + ': ' + obj.AsString));
      stNull: ;
    else
      WriteLine(RawByteString(ite.key + ': ' + ite.val.AsString));
    end;
  until not ObjectFindNext(ite);
  ObjectFindClose(ite);

  if FSendFile <> '' then
    SendFile(FSendFile)
  else
    SendStream(Response.Content);

  Source.Flush;

  FReturn.Clear(true);
  FParams.Clear(true);
  FSession.Clear(true);
  Request.Clear(true);
  Response.Clear(true);
end;

function THTTPStub.doAuthenticate(const AuthData: string;
  var User: string): Boolean;
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

    Result := BasicAuth(User, Password);
  end;
end;

function THTTPStub.BasicAuth(const User, Password: string): Boolean;
begin
  Result := False;
end;

procedure THTTPStub.doBeforeProcessRequest;
var
  obj: ISuperObject;
  key, iv: PByte;
  // p: PSOChar;
  f: PChar;
begin
  FErrorCode := 0;
  FCompress := False;
  FCompressLevel := 5;
  FSendFile := '';
  FIsStatic := False;

  with Request.AsObject do
  begin
    S['remote-ip'] := string(Source.ClientIP);
  { for reverse proxies... cf. https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Forwarded-For
    O['x-forwarded-for'] := HTTPInterprete(PSOChar(Request.S['env.x-forwarded-for']), false, ',');
    O['forwarded'] := HTTPInterprete(PSOChar(Request.S['env.forwarded']), true); }
    O['cookies'] := HTTPInterprete(PSOChar(Request.S['env.cookie']), true);
    O['content-type'] := HTTPInterprete(PSOChar(Request.S['env.content-type']), false, ';');
    O['accept'] := HTTPInterprete(PSOChar(Request.S['env.accept']), false, ',');
  end;

  GetPassPhrase(key, iv);
  if (key <> nil) and (iv <> nil) then
  begin
    obj := Request.AsObject['cookies'].AsObject[COOKIE_NAME];
    case ObjectGetType(obj) of
      stString: FSession := DecodeObject(obj.AsString, key, iv);
      stArray: FSession := DecodeObject(obj.AsArray.S[0], key, iv);
    else
      FSession := TSuperObject.Create(stObject);
    end;
    if not ObjectIsType(FSession, stObject) then
      FSession := TSuperObject.Create(stObject);
  end
  else
   FSession := TSuperObject.Create(stObject);

  // get parameters
  FParams.Merge(Request['params'], true);
  if (Request.I['env.content-length'] > 0) then
  begin
    f := StrRScan(PChar(Request.S['content-type[0]']), '/');
    if f <> nil then
      if(f = '/json') then
      begin
        FParams.Merge(Request.ContentString);
        FParams.AsObject.S['format'] := 'json';
      end else
      if (f = '/xml') then
      begin
        FParams.Merge(XMLParseStream(Request.Content, true));
        FParams.AsObject.S['format'] := 'xml';
      end else
      if (f = '/x-www-form-urlencoded') then
      begin
        obj := HTTPInterprete(PSOChar(Request.ContentString), true, '&', false, DEFAULT_CP);
        try
          FParams.Merge(obj, true);
          FParams.AsObject.S['format'] := 'html';
        finally
          obj := nil;
        end;
      end;
  end;

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

procedure THTTPStub.Render(const obj: ISuperObject; format: boolean);
begin
  obj.SaveTo(Response.Content, format);
end;

procedure THTTPStub.Redirect(const controler, action: string; const id: string);
begin
  if id = '' then
    Redirect('/' + controler + '/' + action + '.' +  FParams.S['format'])
  else
    Redirect('/' + controler + '/' + action + '/' + id + '.' +  FParams.S['format']);
end;

procedure THTTPStub.Render(const str: string);
begin
  Response.Content.WriteString(str, false, DEFAULT_CP);
end;

procedure THTTPStub.Redirect(const location: string);
begin
  FErrorCode := 302;
  FResponse.AsObject.S['Location'] := Location;
end;

function THTTPStub.ProcessRequest: Boolean;

  function ResolveController(const Name: string; var Klass: TRttiInstanceType; var Namespace: string): Boolean;
  begin
    var K := Context.Context.FindType(Format('%s_controller.T%sController', [Name, CamelCase(Name)]));

    if (K = nil) or not (K is TRttiInstanceType) then
      Klass := nil
    else
      Klass := TRttiInstanceType(K);

    { Check if Namespace matches }
    if (Klass <> nil) and (Namespace <> '') then
    begin
      var NamespaceFound := False;

      for var A in Klass.GetAttributes do
        if A is NamespaceAttribute then
          if (A as NamespaceAttribute).Namespace = Namespace then
          begin
            NamespaceFound := True;
            Break;
          end;

      if not NamespaceFound then
        Klass := nil;
    end;

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

  function HasAction(Klass: TRttiInstanceType; const Action, Method: string): Boolean; overload; inline;
  begin
    if Klass <> nil then
      Result := Klass.GetMethod(Action + '_' + Method) <> nil
    else
      Result := False;
  end;

  function HasAction(Klass: TRttiInstanceType; const Action: string): Boolean; overload;
  begin
    Result := HasAction(Klass, Action, FRequest.AsObject.S['method']);
  end;

var
  uri: ISuperObject;
  path, str, ext, user: string;
  rec: TSearchRec;

  Namespace, Controller, Action, Format: string;
  Klass: TRttiInstanceType;
  Inst: TObject;
begin
  Result := False;

  Namespace  := '';
  Controller := '';
  Action     := '';
  Format     := '';

  uri := HTTPInterprete(PSOChar(Request.S['uri']), False, '/', False, DEFAULT_CP);

  { obj[0] is always empty, obj[1] = '' if uri = '' }
  if (uri.AsArray.Length = 2) and (uri.AsArray.S[1] <> '') then
  begin
    // Should match
    // * /:controller
    // * /:action(.:format)

    var S := Trim(uri.AsArray.S[1]);
    var P := PSOChar(S);
    var F := StrRScan(P, '.');

    if F = nil then
    begin
      S := LowerCase(S);
      if ResolveController(S, Klass, Namespace) then
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

      if (Klass = nil) and (F = nil) then
      begin
        S := LowerCase(S);
        if ResolveController(S, Klass, Namespace) then
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
        Assert(Klass <> nil);

        var O: ISuperObject := DecodeValue(P);
        if ObjectIsType(O, stString) then
        begin
          var A := LowerCase(O.AsString);
          if HasAction(Klass, A) then
            Action := A;
        end
        else
          FParams.AsObject['id'] := O;

        Break;
      end
      else { F <> nil }
        SetAction(P, F, Action, Format);
    end;

    { No controller found }
    if Klass = nil then
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
  if Action = '' then
  begin
    var Method := UpperCase(FRequest.AsObject.S['method']);
    var HasId := FParams.AsObject['id'] <> nil;

    if HasId then
    begin
      if Method = 'GET' then
        Action := 'show'
      else if (Method = 'PUT') or (Method = 'PATCH') then
      begin
        Action := 'update';
        FRequest.AsObject.S['method'] := 'POST'; // overrides request method
      end
      else if Method = 'DELETE' then
      begin
        Action := 'delete';
        FRequest.AsObject.S['method'] := 'POST'; // overrides request method
      end;
    end
    else if Method = 'POST' then
      Action := 'create';
  end;

  if Action = '' then
    Action := 'index';

  // detect current formats
  if Format = '' then
  begin
    var Accept := FRequest.AsObject['accept'];
    if ObjectIsType(Accept, stArray) and (Accept.AsArray.Length > 1) then
    begin
      var A := Accept.AsArray.S[0];
      if A = FFormats.S['json.content'] then      // application/json
        Format := 'json'
      else if A = FFormats.S['xml.content'] then  // text/xml
        Format := 'xml'
      else if A = FFormats.S['html.content'] then // text/html
        Format := 'html';
    end
  end;

  // default format is html
  if Format = '' then
    Format := 'html';

  // store action context
  with FParams.AsObject do
  begin
    S['namespace']  := Namespace;
    S['controller'] := Controller;
    S['action']     := Action;
    S['format']     := Format;
  end;

  user := '';
  if doAuthenticate(FRequest.S['env.authorization'], user) then
    FSession.S['user'] := user;

  with FParams.AsObject do
    if FFormats[S['format'] + '.charset'] <> nil then
      FResponse.AsObject.S['Content-Type'] := FFormats.S[S['format'] + '.content'] + '; charset=' + FFormats.S[S['format'] + '.charset']
    else
      FResponse.AsObject.S['Content-Type'] := FFormats.S[S['format'] + '.content'];

  { If controller has not already been found, that means we are looking for
    the application controller that is not namespaced.
    Optimization: If the method with aim to call does not exist, do not
    instanciate the controller. }
  Namespace := '';
  if ((Klass <> nil) or ResolveController(Controller, Klass, Namespace)) and HasAction(Klass, Action) then
  begin
    with Klass do
      inst := GetMethod('create').Invoke(MetaclassType, []).AsObject;
    try
      if inst is TActionController then
      begin
        if TActionController(inst).Invoke then
          Result := True;
      end;
    finally
      inst.Free;
    end;
  end;

  if Stopped or (Response.FContent.Size > 0) or (FErrorCode >= 300) or (FErrorCode = 204) then
    Exit;

  if (FRequest.AsObject.S['method'] <> 'GET') and (Request.AsObject.S['method'] <> 'POST') then
    Exit;

  // file ?
  if (FSendFile <> '') then
  begin
    ext := LowerCase(ExtractFileExt(FSendFile));
    System.Delete(ext, 1, 1);
    FResponse.AsObject.S['Content-Type'] := FFormats.S[ext + '.content'];
    if FResponse.AsObject.S['Content-Type'] = '' then
      FResponse.AsObject.S['Content-Type'] := 'application/binary';
    Compress := FFormats.B[ext + '.istext'];

    if FileExists(FSendFile) then
    begin
      FErrorCode := 200;
      Result := True;
      FResponse.AsObject.S['Content-Disposition'] := 'attachment; filename=' + ExtractFileName(FSendFile);
      FResponse.AsObject.S['Content-Transfer-Encoding'] := 'binary';
    end
    else
      FErrorCode := 404;
    Exit;
  end;

  // view ?
  RenderInternal;
  if not (FErrorCode in [200..207]) then
    if RenderScript then
      Result := True;
  if FErrorCode in [200..207] then
  begin
    FResponse.AsObject.S['Cache-Control'] := 'private, max-age=0';
    Compress := FFormats.B[Params.AsObject.S['format'] + '.istext'];
    Exit;
  end;

  // static ?
  str := Request.S['uri'];
  path := GetRootPath + 'static';

  if (AnsiChar(str[Length(str)]) in ['/','\']) then
    str := str + 'index.' + FParams.AsObject.S['format'];
  if FindFirst(path + str, faAnyFile, rec) = 0 then
  begin
    Result := True;
    { Although the rec.Time is platform bounded and deprecated (on Windows) in
      favor of the new TimeStamp property, all we need here is an integer
      timestamp that changes with the resource }
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
    FIsStatic := True;
    if Request.B['env.if-none-match'] and
      (Request.S['env.if-none-match'] = IntToStr(rec.Time) + '-' + IntToStr(rec.Size)) then
    begin
      FCompress := False;
      FErrorCode := 304;
      FSendFile := '';
      FindClose(rec);
      Exit;
    end;
    FResponse.AsObject.S['Cache-Control'] := 'public, max-age=3600';
    FResponse.AsObject.S['ETag'] := IntToStr(rec.Time) + '-' + IntToStr(rec.Size);
    FSendFile := path + str;
    Compress := FFormats.B[Params.AsObject.S['format'] + '.istext'];
    FindClose(rec);
    FErrorCode := 200;
  {$WARN SYMBOL_DEPRECATED ON}
  {$WARN SYMBOL_PLATFORM ON}
  end
  else
    FErrorCode :=  404;
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
  t: TRttiType;
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

  if FParams.AsObject['controller'] <> nil then
    with FParams.AsObject do
    begin
      // controller
      t := Context.Context.FindType(format('%s_websocket.T%sWebsocket', [S['controller'], CamelCase(S['controller'])]));
      if (t <> nil) and (t is  TRttiInstanceType) then
      begin
        if TRttiInstanceType(t).MetaclassType.InheritsFrom(TActionWebsocket) then
          inst := TActionWebsocketClass(TRttiInstanceType(t).MetaclassType).Create(FWebSocketVersion)
        else
          Exit;
      end
      else
        Exit;
    end
    else
      Exit;
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
  if FCompress then
  begin
    streamout := TPooledMemoryStream.Create;
    try
      stream.Seek(0, soFromBeginning);
      CompressStream(stream, streamout, FCompressLevel);
      // don't send first 2 bytes !
      WriteLine(format(AnsiString('Content-Length: %d'), [streamout.size - 2]));
      streamout.Seek(2, soFromBeginning);
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
    response: array[0..MD5_DIGEST_LENGTH - 1] of Byte;
  begin
    Result := 0;
    origin := Request['env.origin'];
    if not ObjectIsType(origin, stString) then Exit;

    key1 := Request['env.sec-websocket-key1'];
    if not ObjectIsType(key1, stString) then
      Exit;
    key2 := Request['env.sec-websocket-key2'];
    if not ObjectIsType(key2, stString) then
      Exit;
    if Source.Read(challenge.key3, SizeOf(challenge.key3), 0) <> SizeOf(challenge.key3) then
      Exit;
    if Copy(origin.AsString, 1, 8) = 'https://' then
      location := RawByteString('wss://' + Request.s['env.host'] + Request.S['uri'])
    else
      location := RawByteString('ws://' + Request.s['env.host'] + Request.S['uri']);
    keyNumber1 := getKeyNumber(key1.AsString, space1);
    keyNumber2 := getKeyNumber(key2.AsString, space2);
    if (space1 = 0) or (space2 = 0) then
      Exit;
    if (keyNumber1 mod space1 <> 0) or (keyNumber2 mod space2 <> 0) then
      Exit;
    challenge.part1 := bigendian(keyNumber1 div space1);
	  challenge.part2 := bigendian(keyNumber2 div space2);
    if MD5(@challenge, SizeOf(challenge), @response) = nil then
      Exit;
    WriteLine('HTTP/1.1 101 WebSocket Protocol Handshake');
	  WriteLine('Upgrade: WebSocket');
	  WriteLine('Connection: Upgrade');
	  WriteLine('Sec-WebSocket-Location: ' + location);
	  WriteLine('Sec-WebSocket-Origin: ' + RawByteString(origin.AsString) );
    protocol := Request['env.sec-websocket-protocol'];
    if ObjectIsType(protocol, stString) then
      WriteLine('Sec-WebSocket-Protocol: ' + RawByteString(protocol.asstring));
    WriteLine('');
    Source.Write(response, SizeOf(response), 0);
    Source.Flush;
    Result := WebSocket;
  end;

  function doWebSocketNew: Cardinal;
  var
    key, origin: ISuperObject;
    ret: RawByteString;
    buffer: array[0..SHA_DIGEST_LENGTH - 1] of AnsiChar;
  begin
    Result := 0;
    origin := Request['env.sec-websocket-origin'];
    if not ObjectIsType(origin, stString) then
    begin
      origin := Request['env.origin'];
      if not ObjectIsType(origin, stString) then
        Exit;
    end;

    key := Request['env.sec-websocket-key'];
    if not ObjectIsType(key, stString) then Exit;

    ret := AnsiString(key.AsString) + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11';
    SHA1(PAnsiChar(ret), Length(ret), @buffer);
    ret := RawByteString(BytesToBase64(@buffer, SizeOf(buffer)));
    WriteLine('HTTP/1.1 101 WebSocket Protocol Handshake');
	  WriteLine('Upgrade: websocket');
	  WriteLine('Connection: Upgrade');
	  WriteLine('Sec-WebSocket-Origin: ' + RawByteString(origin.AsString));
    WriteLine('Sec-WebSocket-Accept: ' + ret);
    WriteLine('');
    Source.Flush;
    Result := WebSocket;
  end;

begin
  Result := 0;
  if SameText(Request.S['env.upgrade'], 'WebSocket') then
  begin
    FWebSocketVersion := Request.I['env.sec-websocket-version'];
    case FWebSocketVersion of
      0: Result := doWebSocket04;
    else
      // 4 > 13
      Result := doWebSocketNew;
    end;
  end;
end;

{$IFDEF DEBUG}
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

{$ENDIF}

end.



