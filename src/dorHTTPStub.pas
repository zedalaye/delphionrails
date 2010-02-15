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
{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
interface
uses
  dorSocketStub,
  {$IFDEF FPC}sockets,{$ELSE}Winsock,{$ENDIF}
  Generics.Collections, dorUtils, classes, superobject;

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
  TLuaStackInfo = record
    line: Integer;
    name: PAnsiChar;
    source: PAnsiChar;
    constructor Create(line: Integer; name, source: PAnsiChar);
  end;
{$ENDIF}

  THTTPStub = class(TSocketStub)
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
{$IFDEF DEBUG}
    FLuaStack: TStack<TLuaStackInfo>;
{$ENDIF}
    function DecodeFields(str: PChar): boolean;
    function DecodeCommand(str: PChar): boolean;
    procedure WriteLine(str: RawByteString);
    procedure SendEmpty;
    procedure SendFile(const filename: string);
    procedure SendStream(Stream: TStream);
    procedure RenderInternal;
    procedure RenderScript;
    function DecodeContent: boolean; virtual;
    procedure doBeforeProcessRequest; virtual;
    procedure doAfterProcessRequest; virtual;
  protected
    procedure ProcessRequest; virtual;
    function Run: Cardinal; override;
    function GetPassPhrase: AnsiString; virtual;
  public
    constructor CreateStub(AOwner: TSocketServer; ASocket: longint; AAddress: TSockAddr); override;
    destructor Destroy; override;
    procedure Render(const obj: ISuperObject; format: boolean = false); overload;
    procedure Render(const str: string); overload;
    procedure Redirect(const location: string); overload;
    procedure Redirect(const controler, action: string; const id: string = ''); overload;
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
 windows,
{$ENDIF}
  SysUtils, StrUtils, superxmlparser, dorOpenSSL, dorLua,
  dorActionCOntroller, dorActionView, Rtti
  {$ifdef madExcept}, madexcept {$endif}
{$IFDEF UNICODE}, AnsiStrings{$ENDIF}
{$IFDEF UNIX}, baseunix{$ENDIF}
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
      LUA_HOOKCALL:
        begin
          lua_getinfo(L, 'lnS', ar);
          h.FLuaStack.Push(TLuaStackInfo.Create(ar.currentline , ar.name, ar.source));
        end;
      LUA_HOOKRET : h.FLuaStack.Pop;
    end;
  end;
end;
{$ENDIF}

function HttpResponseStrings(code: integer): RawByteString;
begin
  case code of
    100: Result := '100 Continue';
    101: Result := '101 Switching Protocols';
    102: Result := 'Processing'; // WebDAV

    200: Result := 'HTTP/1.1 200 OK';
    201: Result := 'HTTP/1.1 201 Created';
    202: Result := 'HTTP/1.1 202 Accepted';
    203: Result := 'HTTP/1.1 203 Non-Authoritative Information';
    204: Result := 'HTTP/1.1 204 No Content';
    205: Result := 'HTTP/1.1 205 Reset Content';
    206: Result := 'HTTP/1.1 206 Partial Content';
    207: Result := 'HTTP/1.1 207 Multi-Status'; // WebDAV

    300: Result := 'HTTP/1.1 300 Multiple Choices';
    301: Result := 'HTTP/1.1 301 Moved Permanently';
    302: Result := 'HTTP/1.1 302 Found';
    303: Result := 'HTTP/1.1 303 See Other';
    304: Result := 'HTTP/1.1 304 Not Modified';
    305: Result := 'HTTP/1.1 305 Use Proxy';
    306: Result := 'HTTP/1.1 306 unused';
    307: Result := 'HTTP/1.1 307 Temporary Redirect';

    400: Result := 'HTTP/1.1 400 Bad Request';
    401: Result := 'HTTP/1.1 401 Authorization Required';
    402: Result := 'HTTP/1.1 402 Payment Required';
    403: Result := 'HTTP/1.1 403 Forbidden';
    404: Result := 'HTTP/1.1 404 Not Found';
    405: Result := 'HTTP/1.1 405 Method Not Allowed';
    406: Result := 'HTTP/1.1 406 Not Acceptable';
    407: Result := 'HTTP/1.1 407 Proxy Authentication Required';
    408: Result := 'HTTP/1.1 408 Request Time-out';
    409: Result := 'HTTP/1.1 409 Conflict';
    410: Result := 'HTTP/1.1 410 Gone';
    411: Result := 'HTTP/1.1 411 Length Required';
    412: Result := 'HTTP/1.1 412 Precondition Failed';
    413: Result := 'HTTP/1.1 413 Request Entity Too Large';
    414: Result := 'HTTP/1.1 414 Request-URI Too Large';
    415: Result := 'HTTP/1.1 415 Unsupported Media Type';
    416: Result := 'HTTP/1.1 416 Requested Range Not Satisfiable';
    417: Result := 'HTTP/1.1 417 Expectation Failed';
    418: Result := 'HTTP/1.1 418 I''m a teapot';
    422: Result := 'HTTP/1.1 422 Unprocessable Entity'; // WebDAV
    423: Result := 'HTTP/1.1 417 Locked'; // WebDAV
    424: Result := 'HTTP/1.1 424 Failed Dependency'; // WebDAV
    425: Result := 'HTTP/1.1 425 Unordered Collection'; // WebDAV
    426: Result := 'HTTP/1.1 426 Upgrade Required';
    449: Result := 'HTTP/1.1 449 Retry With';
    450: Result := 'HTTP/1.1 450 Blocked by Windows Parental Controls';

    500: Result := 'HTTP/1.1 500 Internal Server Error';
    501: Result := 'HTTP/1.1 501 Method Not Implemented';
    502: Result := 'HTTP/1.1 502 Bad Gateway';
    503: Result := 'HTTP/1.1 503 Service Temporarily Unavailable';
    504: Result := 'HTTP/1.1 504 Gateway Time-out';
    505: Result := 'HTTP/1.1 505 HTTP Version Not Supported';
    506: Result := 'HTTP/1.1 506 Variant Also Negotiates';
    507: Result := 'HTTP/1.1 507 Insufficient Storage'; // WebDAV
    509: Result := 'HTTP/1.1 509 Bandwidth Limit Exceeded';
    510: Result := 'HTTP/1.1 510 Not Extended';
  else
    Result := 'HTTP/1.1 ' + RawByteString(inttostr(code));
  end;
end;

function MBUDecode(const str: RawByteString; cp: Word): UnicodeString;
begin
  SetLength(Result, MultiByteToWideChar(cp, 0, PAnsiChar(str), length(str), nil, 0));
  MultiByteToWideChar(cp, 0, PAnsiChar(str), length(str), PWideChar(Result), Length(Result));
end;

function HTTPDecode(const AStr: string; codepage: Integer = 0): String;
var
  Sp, Rp, Cp: PAnsiChar;
  src, dst: RawByteString;
begin
  src := RawByteString(AStr);
  SetLength(dst, Length(src));
  Sp := PAnsiChar(src);
  Rp := PAnsiChar(dst);
  while Sp^ <> #0 do
  begin
    case Sp^ of
      '+': Rp^ := ' ';
      '%': begin
             Inc(Sp);
             if Sp^ = '%' then
               Rp^ := '%'
             else
             begin
               Cp := Sp;
               Inc(Sp);
               if (Cp^ <> #0) and (Sp^ <> #0) then
                 Rp^ := AnsiChar(StrToInt('$' + Char(Cp^) + Char(Sp^)))
               else
               begin
                 dst := '';
                 Exit;
               end;
             end;
           end;
    else
      Rp^ := Sp^;
    end;
    Inc(Rp);
    Inc(Sp);
  end;
  SetLength(dst, Rp - PAnsiChar(dst));
  Result := MBUDecode(dst, codepage)
end;

function HTTPInterprete(src: PSOChar; named: boolean = false; sep: SOChar = ';'; StrictSep: boolean = false; codepage: Integer = 0): ISuperObject;
var
  P1: PSOChar;
  S: SOString;
  i: integer;
  obj, obj2, value: ISuperObject;
begin
    if named then
      Result := TSuperObject.create(stObject) else
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
        S := HTTPDecode(S, codepage);
      if named then
      begin
        i := pos('=', S);
        // named
        if i > 1 then
        begin
          S[i] := #0;
          obj := Result[S];
          if sep = '&' then
            value := TSuperObject.ParseString(PSOChar(@S[i+1]), false) else
            value := TSuperObject.Create(PSOChar(@S[i+1]));
          if obj = nil then
            Result[S] := value else
            begin
              if obj.IsType(stArray) then
                obj.AsArray.Add(value) else
                begin
                  obj2 := TSuperObject.Create(stArray);
                  Result[S] := obj2;
                  obj2.AsArray.Add(obj);
                  obj2.AsArray.Add(value);
                end;
            end;
        end else
        begin
          // unamed value ignored
        end;
      end else
      begin
        value := TSuperObject.Create(S);
        if value = nil then
          if sep = '&' then
            value := TSuperObject.ParseString(PChar(s), false) else
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

function EncodeObject(const obj: ISuperObject; const pass: AnsiString): SOString;
var
  StreamA, streamB: TPooledMemoryStream;
begin
  StreamB := TPooledMemoryStream.Create;
  StreamA := TPooledMemoryStream.Create;
  try
    // ansi
    obj.SaveTo(StreamA);

    // zlib
    CompressStream(StreamA, StreamB, 4);

    // aes
    StreamA.Seek(0, soFromBeginning);
    AesEncryptStream(StreamB, StreamA, PAnsiChar(pass), 128);
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

function DecodeObject(const str: SOString; const pass: AnsiString): ISuperObject;
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
    AesDecryptStream(StreamA, StreamB, PAnsiChar(pass), 128);

    // zlib
    StreamA.Seek(0, soFromBeginning);
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

function maj(const s: string): string;
begin
  Result := s;
  if Length(s) > 0 then
    Result[1] := UpCase(Result[1]);
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
    Result := false else
    with Request.ForcePath('env') do
    begin
      prop := LowerCase(Copy(str, 1, p-str));
      AsObject.S[prop] := p+2;
      Result := true;
    end;
end;

function THTTPStub.DecodeContent: boolean;
var
  ContentLength: integer;
begin
  ContentLength := Request.I['env.content-length'];
  if ContentLength > 0 then
  begin
    Request.FContent.Size := ContentLength;
    Request.FContent.LoadFromSocket(SocketHandle, false);
  end;
  result := true;
end;

function THTTPStub.DecodeCommand(str: PChar): boolean;
  function DecodeURI(uri: PChar; len: integer; out data: string): boolean;
  const hexcodes = ['0'..'9', 'A'..'F', 'a'..'f'];
  var i: integer;
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
          (i in [32..255]) then
            begin
              data := data + char(i);
              inc(uri, 3);
              dec(len, 3);
            end else
            begin
              Result := False;
              Exit;
            end;
      end else
      begin
        data := data + uri^;
        inc(uri, 1);
        dec(len, 1);
      end;
    end;
    Result := true;
  end;
var
  marker: PChar;
  param, value: string;
  i: integer;
begin
  result := false;
  marker := StrScan(str, SP);
  if marker = nil then exit;
  Request.AsObject.S['method'] := copy(str, 0, marker - str);
  str := marker;

  // SP
  if (str^ <> SP) then
    exit;

  // URI
  inc(str);
  marker := Str;
  while not ({$IFDEF UNICODE}(str^ < #256) and {$ENDIF}(AnsiChar(Str^) in [SP, NL, '?'])) do
    inc(str);
  if (str > marker) and (str^ <> NL) then
  begin
    if DecodeURI(marker, str - marker, value) then
      Request.AsObject.S['uri'] := value else
      exit;
  end else
    exit;

  // decode parametters
  if str^ = '?' then
  begin
    inc(str);
    marker := Str;
    param := '';
    value := '';
    while true do
      case str^ of
        '&', SP, NL: begin
               if (param <> '') and (str > marker) then
               begin
                 if not DecodeURI(marker, str - marker, value) then exit;
                 Request['params.'+HTTPDecode(param)] := TSuperObject.ParseString(PChar(HTTPDecode(value)), False);
               end;
               if {$IFDEF UNICODE}(str^ < #256) and {$ENDIF}(AnsiChar(str^) in [SP, NL]) then
                 Break;
               param := '';
               value := '';
               inc(Str);
               marker := Str;
             end;
        '=': begin
               if (str > marker) then
                 if not DecodeURI(marker, str - marker, param) then
                   Exit;
               inc(Str);
               marker := Str;
             end;
      else
        inc(Str);
        continue;
      end;

  end;

  // SP expected
  if (str^ <> SP) then exit;

  // HTTP/
  inc(str);
  if not ((str[0] = 'H') and (str[1] = 'T') and (str[2] = 'T') and
     (str[3] = 'P') and (str[4] = SL)) then
    exit;
  str := @str[5];

  // version major
  marker := str;
  while {$IFDEF UNICODE}(str^ < #256) and{$ENDIF}(AnsiChar(str^) in ['0'..'9']) do inc(str);
  if (str > marker) and (str^ <> NL) then
  begin
    if TryStrToInt(copy(marker, 0, str - marker), i) then
      Request.I['http-version.major'] := i else
      exit;
  end else
    exit;

  // .
  if (str^ <> PT) then
    exit;
  inc(str);

  // version minor
  marker := str;
  while {$IFDEF UNICODE}(str^ < #256) and{$ENDIF} (AnsiChar(str^) in ['0'..'9']) do inc(str);
  if (str > marker) then
  begin
    if TryStrToInt(copy(marker, 0, str - marker), i) then
      Request.I['http-version.minor']  := i else
      exit;
  end else
    exit;

  if (str^ <> NL) then exit;

  result := true;
end;

procedure THTTPStub.RenderInternal;
var
  clazz: TRttiType;
  inst: TObject;
begin
  with params.AsObject do
  begin
    clazz := Context.Context.FindType(format('%s_view.T%sView', [S['controller'], maj(S['controller'])]));
    if (clazz <> nil) and (clazz is  TRttiInstanceType)  then
    begin
      with TRttiInstanceType(clazz) do
        inst := GetMethod('create').Invoke(MetaclassType, []).AsObject;
      try
        if inst is TActionView then
          TActionView(inst).Invoke else
          ErrorCode := 404;
      finally
        inst.Free;
      end;
    end else
      ErrorCode := 404;
  end;
end;



procedure THTTPStub.RenderScript;
var
  state: Plua_State;
  ite: TSuperObjectIter;
  path, str, rel: string;
  procedure printerror;
  begin
    Response.Content.Clear;
    Render(
      '<html xmlns="http://www.w3.org/1999/xhtml">'#10+
      '<head>'#10+
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

    while FLuaStack.Count > 1 do
      with FLuaStack.Pop do
        Render(format('%s:%d:in %s'#10, [source, line, name]));

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
  with Params.AsObject do
  begin

    path := ExtractFilePath(ParamStr(0));
    rel := 'view/' + S['controller'] + '/' + S['action'] + '.' + S['format'];
    str := path + rel;
    if FileExists(str) then
    begin
{$IFDEF DEBUG}
      FLuaStack := TStack<TLuaStackInfo>.Create;
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
        lua_sethook(state, @script_hook, LUA_MASKCALL or LUA_MASKRET, 0);
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

        if lua_processsor_dofile(state, str, PAnsiChar(UTF8String(rel))) then
        begin
          rel := 'layout/' + S['controller'] + '.' + S['format'];
          str := path + rel;
          if FileExists(str) then
          begin
            if not lua_processsor_dofile(state, str, PAnsiChar(UTF8String(rel))) then
              printerror;
          end else
            begin
              rel := 'layout/application.' + S['format'];
              str := path + rel;
              if FileExists(str) then
                if not lua_processsor_dofile(state, str, PAnsiChar(UTF8String(rel))) then
                   printerror;
            end;
        end else
          printerror;
      finally
{$IFDEF DEBUG}
        FLuaStack.Free;
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
  result := 0;
  cursor := 0;
  len := 0;
  line := 0;
  c := #0;
  while not Stopped do
  begin
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
    if receive(SocketHandle, c, 1, 0) <> 1 then exit;
    case c of
    CR: dec(cursor){->LF};
    LF: begin
          if cursor = 1 then
          begin
            if not DecodeContent then
              exit;

              FReturn := TSuperObject.Create;
              FParams := TSuperObject.Create;
              FSession := TSuperObject.Create;
              try
                doBeforeProcessRequest;
                try
                  try
                    ProcessRequest; // <<<<<<<<<<<<<<<
                  except
                    on E: Exception do
                    begin
                      FErrorCode := 500;
                      Response.Content.WriteString(E.Message, false);
                    {$ifdef madExcept}
                      HandleException(etNormal, E);
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
          end else
          begin
            buffer[cursor] := NL;
            if line = 0 then
            begin
              if not DecodeCommand(Pointer(Buffer)) then
                exit;
            end else
            begin
              if not DecodeFields(Pointer(Buffer)) then
                exit;
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

constructor THTTPStub.CreateStub(AOwner: TSocketServer; ASocket: longint;
  AAddress: TSockAddr);
begin
  inherited;
  FRequest := THTTPMessage.Create;
  FRequest._AddRef;
  FResponse := THTTPMessage.Create;
  FResponse._AddRef;


  FContext := TSuperRttiContext.Create;

  FFormats := TSuperObject.Create;
  with FFormats do
  begin
    S['htm.content'] := 'text/html';
    S['htm.charset'] := DEFAULT_CHARSET;
    B['htm.istext'] := True;
    S['html.content'] := 'text/html';
    S['html.charset'] := DEFAULT_CHARSET;
    B['html.istext'] := True;
    S['xml.content'] := 'text/xml';
    B['xml.istext'] := True;
    S['json.content'] := 'text/json';
    B['json.istext'] := True;
    S['png.content'] := 'image/png';
    S['jpeg.content'] := 'image/jpeg';
    S['jpg.content'] := 'image/jpeg';
    S['gif.content'] := 'image/gif';
    S['css.content'] := 'text/css';
    B['css.istext'] := True;
    S['js.content'] := 'text/javascript';
    B['js.istext'] := True;
    S['svg.content'] := 'image/svg+xml';
    B['svg.istext'] := True;
  end;

  // connexion timout
{$IFDEF FPC}
  fpsetsockopt(Socket, SOL_SOCKET, SO_RCVTIMEO, @ReadTimeOut, SizeOf(ReadTimeOut));
{$ELSE}
  setsockopt(ASocket, SOL_SOCKET, SO_RCVTIMEO, @ReadTimeOut, SizeOf(ReadTimeOut));
{$ENDIF}
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
  pass: AnsiString;
  obj: ISuperObject;
begin
  if FCompress then
    FResponse.AsObject.S['Content-Encoding'] := 'deflate';

  FResponse.AsObject.S['Server'] := 'DOR 1.0';
  if not FIsStatic then
  begin
    pass := GetPassPhrase;
    if pass <> '' then
      FResponse.S['Set-Cookie[]'] := COOKIE_NAME + '=' + EncodeObject(FSession, pass) + '; path=/';
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
    SendFile(FSendFile) else
    SendStream(Response.Content);

  FReturn.Clear(true);
  FParams.Clear(true);
  FSession.Clear(true);
  Request.Clear(true);
  Response.Clear(true);
end;

procedure THTTPStub.doBeforeProcessRequest;
  function interprete(v: PSOChar; const name: string; parse: boolean): boolean;
  var
    p: PChar;
    str: string;
  begin
    str := trim(v);
    if str <> '' then
    begin
      p := StrRScan(PChar(str), '.');
      if p <> nil then
      begin
        FParams.AsObject.S['format'] := LowerCase(p + 1);
        setlength(str, p - PChar(str));
      end;
      if parse then
        FParams.AsObject.S[name] := LowerCase(str) else
        FParams.AsObject[name] := TSuperObject.ParseString(PChar(str), false);
      Result := true;
    end else
      Result := false
  end;
var
  obj: ISuperObject;
  pass: AnsiString;
  p: PSOChar;
  f: PChar;
begin
  FErrorCode := 0;
  FCompress := False;
  FCompressLevel := 5;
  FSendFile := '';
  FIsStatic := False;

  with Request.AsObject do
  begin
    O['cookies'] := HTTPInterprete(PSOChar(Request.S['env.cookie']), true);
    O['content-type'] := HTTPInterprete(PSOChar(Request.S['env.content-type']), false, ';');
    O['accept'] := HTTPInterprete(PSOChar(Request.S['env.accept']), false, ',');
  end;

  pass := GetPassPhrase;
  if pass <> '' then
  begin
    obj := Request.AsObject['cookies'].AsObject[COOKIE_NAME];
   case ObjectGetType(obj) of
      stString: FSession := DecodeObject(obj.AsString, pass);
      stArray: FSession := DecodeObject(obj.AsArray.S[0], pass);
    else
      FSession := TSuperObject.Create(stObject);
    end;
    if not ObjectIsType(FSession, stObject) then
      FSession := TSuperObject.Create(stObject);
  end else
   FSession := TSuperObject.Create(stObject);

  // get parametters
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

  obj := HTTPInterprete(PSOChar(Request.S['uri']), false, '/', false, DEFAULT_CP);
  begin
    if interprete(PSOChar(obj.AsArray.S[1]), 'controller', true) then
    if interprete(PSOChar(obj.AsArray.S[2]), 'action', true) then
       interprete(PSOChar(obj.AsArray.S[3]), 'id', false);
  end;

  // default controller is application
  if (FParams.AsObject['controller'] = nil) then
    FParams.AsObject.S['controller'] := 'application';


  // default action is index
  if (FParams.AsObject['action'] = nil) then
    (FParams.AsObject.S['action'] := 'index');

  // detect format
  if (FParams.AsObject['format'] = nil) then
  begin
    p := StrRScan(PSOChar(Request.S['uri']), '.');
    if p <> nil then
      FParams.AsObject.S['format'] := LowerCase(p + 1) else
      FParams.AsObject.S['format'] := 'html';
  end;
end;

function THTTPStub.GetPassPhrase: AnsiString;
begin
  Result := '';
end;

procedure THTTPStub.Render(const obj: ISuperObject; format: boolean);
begin
  obj.SaveTo(Response.Content, format);
end;

procedure THTTPStub.Redirect(const controler, action: string; const id: string);
begin
  if id = '' then
    Redirect('/' + controler + '/' + action + '.' +  FParams.S['format']) else
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

procedure THTTPStub.ProcessRequest;
var
  str: string;
  path: string;
  rec: TSearchRec;
  clazz: TRttiType;
  inst: TObject;
begin
  inherited;
  with FParams.AsObject do
    if FFormats[S['format'] + '.charset'] <> nil then
      FResponse.AsObject.S['Content-Type'] := FFormats.S[S['format'] + '.content'] + '; charset=' + FFormats.S[S['format'] + '.charset'] else
      FResponse.AsObject.S['Content-Type'] := FFormats.S[S['format'] + '.content'];

  path := ExtractFilePath(ParamStr(0));
  if FParams.AsObject['controller'] <> nil then
    with FParams.AsObject do
    begin
      // controller
      clazz := Context.Context.FindType(format('%s_controller.T%sController', [S['controller'], maj(S['controller'])]));
      if (clazz <> nil) and (clazz is  TRttiInstanceType)  then
      begin
        with TRttiInstanceType(clazz) do
          inst := GetMethod('create').Invoke(MetaclassType, []).AsObject;
        try
          if inst is TActionController then
            TActionController(inst).Invoke;
        finally
          inst.Free;
        end;
      end;

      if (FErrorCode >= 300) then Exit;

      if (Request.AsObject.S['method'] <> 'GET') and (Request.AsObject.S['method'] <> 'POST') then
        Exit;

      // view ?
      RenderInternal;
      if not (FErrorCode in [200..207]) then
        RenderScript;
      if FErrorCode in [200..207] then
        begin
          FResponse.AsObject.S['Cache-Control'] := 'private, max-age=0';
          Compress := FFormats.B[Params.AsObject.S['format'] + '.istext'];
          Exit;
        end;
    end;

  // static ?
  str := Request.S['uri'];
  path := path + 'static';

  if (AnsiChar(str[Length(str)]) in ['/','\']) then
    str := str + 'index.' + FParams.AsObject.S['format'];
  if FindFirst(path + str, faAnyFile, rec) = 0 then
  begin
    FIsStatic := True;
    if Request.B['env.if-none-match'] and
      (Request.S['env.if-none-match'] = IntToStr(rec.Time) + '-' + IntToStr(rec.Size)) then
    begin
      FCompress := False;
      FErrorCode := 304;
      FSendFile := '';
      Exit;
    end;
    FResponse.AsObject.S['Cache-Control'] := 'max-age=946080000, public';
    FResponse.AsObject.S['ETag'] := IntToStr(rec.Time) + '-' + IntToStr(rec.Size);
    FSendFile := path + str;
    Compress := FFormats.B[Params.AsObject.S['format'] + '.istext'];
    FindClose(rec);
    FErrorCode := 200;
  end else
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
  end else
    SendEmpty;
end;

procedure THTTPStub.WriteLine(str: RawByteString);
begin
  str := str + CRLF;
{$IFDEF FPC}
  fpsend(SocketHandle, PChar(str), length(str), 0);
{$ELSE}
  send(SocketHandle, PAnsiChar(str)^, length(str), 0);
{$ENDIF}
end;

procedure THTTPStub.SendStream(Stream: TStream);
  procedure SendIt(s: TSTream);
  var
    size: Integer;
    buffer: array[0..1023] of byte;
  begin
    //WriteLine(format('Content-Length: %d', [s.size]));
    WriteLine('');
    //s.Seek(0, soFromBeginning);
    size := s.Read(buffer, sizeof(buffer));
    while size > 0 do
    begin
{$IFDEF FPC}
      fpsend(SocketHandle, @buffer, size, 0);
{$ELSE}
      send(SocketHandle, buffer, size, 0);
{$ENDIF}
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
      CompressStream(stream, streamout, FCompressLevel);
      // don't send first 2 bytes !
      WriteLine(format(AnsiString('Content-Length: %d'), [streamout.size - 2]));
      streamout.Seek(2, soFromBeginning);
      SendIt(streamout);
    finally
      streamout.Free;
    end;
  end else
  begin
    WriteLine(format(AnsiString('Content-Length: %d'), [Stream.size]));
    Stream.Seek(0, soFromBeginning);
    SendIt(Stream);
  end;
end;

{$IFDEF DEBUG}
{ TLuaStackInfo }

constructor TLuaStackInfo.Create(line: Integer; name, source: PAnsiChar);
begin
  Self.line := line;
  Self.name := name;
  Self.source := source;
end;
{$ENDIF}

end.



