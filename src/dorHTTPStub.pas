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
  dorUtils, classes, superobject;

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
    function DecodeFields(str: PChar): boolean;
    function DecodeCommand(str: PChar): boolean;
    procedure WriteLine(str: RawByteString);
    procedure SendEmpty;
    procedure SendFile(const filename: string);
    procedure SendStream(Stream: TStream);
    function RenderInternal: TSuperInvokeResult;
    function RenderScript(const params: ISuperObject): Boolean;
    function DecodeContent: boolean; virtual;
    procedure doBeforeProcessRequest; virtual;
    procedure doAfterProcessRequest; virtual;
  protected
    procedure ProcessRequest; virtual;
    function Run: Cardinal; override;
    procedure Render(const obj: ISuperObject; format: boolean = false); overload;
    procedure Render(const str: string); overload;
    procedure Redirect(const location: string); overload;
    procedure Redirect(const controler, action: string; const id: string = ''); overload;
    function GetPassPhrase: AnsiString; virtual;
  public
    constructor CreateStub(AOwner: TSocketServer; ASocket: longint; AAddress: TSockAddr); override;
    destructor Destroy; override;
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
SysUtils, StrUtils, dorOpenSSL, dorLua, Rtti {$ifdef madExcept}, madexcept {$endif}
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

function lua_render(state: Plua_State): Integer; cdecl;
var
  p: Pointer;
  obj: ISuperObject;
  str: string;
begin
  Result := 0;
  lua_getglobal(state, '@this');
  p := lua_touserdata(state, -1);
  if (p <> nil) and (lua_gettop(state) = 2) then
  with THTTPStub(p) do
  begin
    obj := lua_tosuperobject(state, 1);
    case ObjectGetType(obj) of
    stObject:
      begin
        with FParams.AsObject do
        begin
          if obj.AsObject['controller'] = nil then
            obj.AsObject.S['controller'] := S['controller'];
          obj.AsObject.S['format'] := S['format'];
        end;

        case RenderInternal of
        irMethothodError:
          begin
            with obj.AsObject do
              str := ExtractFilePath(ParamStr(0)) + 'view/' + S['controller'] + '/' + S['action'] + '.' + S['format'];
            if FileExists(str) then
              lua_processsor_dofile(state, str);
          end;
        end;
      end;
    end;
  end;
end;

function HttpResponseStrings(code: integer): RawByteString;
begin
  case code of
    100: Result := '100 Continue';
    101: Result := '101 Switching Protocols';

    200: Result := 'HTTP/1.1 200 OK';
    201: Result := 'HTTP/1.1 201 Created';
    202: Result := 'HTTP/1.1 202 Accepted';
    203: Result := 'HTTP/1.1 203 Non-Authoritative Information';
    204: Result := 'HTTP/1.1 204 No Content';
    205: Result := 'HTTP/1.1 205 Reset Content';
    206: Result := 'HTTP/1.1 206 Partial Content';

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

    500: Result := 'HTTP/1.1 500 Internal Server Error';
    501: Result := 'HTTP/1.1 501 Method Not Implemented';
    502: Result := 'HTTP/1.1 502 Bad Gateway';
    503: Result := 'HTTP/1.1 503 Service Temporarily Unavailable';
    504: Result := 'HTTP/1.1 504 Gateway Time-out';
    505: Result := 'HTTP/1.1 505 HTTP Version Not Supported';
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
    //StreamA.SaveToFile('c:\test.aes');

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

function THTTPStub.RenderInternal: TSuperInvokeResult;
var
  ret: ISuperObject;
begin
  with params.AsObject do
   Result := TrySOInvoke(FContext, Self, 'view_' + S['controller'] + '_' + S['action'] + '_' + S['format'], FReturn, ret);
end;

function THTTPStub.RenderScript(const params: ISuperObject): Boolean;
var
  state: Plua_State;
  ite: TSuperObjectIter;
  path, str: string;
begin
  with params.AsObject do
  begin
    path := ExtractFilePath(ParamStr(0));
    str := path + 'view/' + S['controller'] + '/' + S['action'] + '.' + S['format'];
    if FileExists(str) then
    begin

      state := lua_newstate(@lua_app_alloc, nil);
      try
        luaL_openlibs(state);
        lua_pushlightuserdata(state, Self);
        lua_setglobal(state, '@this');
        lua_pushcfunction(state, @lua_print);
        lua_setglobal(state, 'print');
        lua_pushcfunction(state, lua_gettickcount);
        lua_setglobal(state, 'gettickcount');
        lua_pushcfunction(state, lua_render);
        lua_setglobal(state, 'render');

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
        lua_processsor_dofile(state, str);
        str := path + 'layout/' + S['controller'] + '.' + S['format'];
        if FileExists(str) then
          lua_processsor_dofile(state, str) else
          begin
            str := path + 'layout/application.' + S['format'];
            if FileExists(str) then
              lua_processsor_dofile(state, str);
          end;
      finally
        lua_close(state);
      end;
      Exit(True);
    end;
  end;
  Result := False;
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
begin
  if FCompress then
    FResponse.AsObject.S['Content-Encoding'] := 'deflate';

  FResponse.AsObject.S['Server'] := 'DOR 1.0';
  if not FIsStatic then
  begin
    pass := GetPassPhrase;
    if pass <> '' then
      FResponse.AsObject.S['Set-Cookie'] := COOKIE_NAME + '=' + EncodeObject(FSession, pass) + '; path=/';
  end;
  WriteLine(HttpResponseStrings(FErrorCode));
  if ObjectFindFirst(Response, ite) then
  repeat
    WriteLine(RawByteString(ite.key + ': ' + ite.val.AsString));
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
  function interprete(v: PSOChar; name: string): boolean;
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
        FParams.AsObject.S['format'] := p + 1;
        setlength(str, p - PChar(str));
      end;
      FParams.AsObject[name] := TSuperObject.ParseString(PChar(str), false);
      Result := true;
    end else
      Result := false
  end;
var
  obj: ISuperObject;
  pass: AnsiString;
begin
  FErrorCode := 200;
  FCompress := False;
  FCompressLevel := 5;
  FSendFile := '';
  FIsStatic := False;

  with Request.AsObject do
  begin
    O['cookies'] := HTTPInterprete(PSOChar(Request.S['env.cookie']), true);
    O['content-type'] := HTTPInterprete(PSOChar(Request.S['env.content-type']));
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
  end else
   FSession := TSuperObject.Create(stObject);

  // get parametters
  FParams.Merge(Request['params'], true);
  if (Request.S['method'] = 'POST') then
    if(Request.S['accept[0]'] = 'application/json') then
    begin
      FParams.Merge(Request.ContentString);
      FParams.AsObject.S['format'] := 'json';
    end else
    if(Request.S['content-type[0]'] = 'application/x-www-form-urlencoded') then
    begin
      obj := HTTPInterprete(PSOChar(Request.ContentString), true, '&', false, DEFAULT_CP);
      try
        FParams.Merge(obj, true);
        FParams.AsObject.S['format'] := 'html';
      finally
        obj := nil;
      end;
    end;

   obj := HTTPInterprete(PSOChar(Request.S['uri']), false, '/', false, DEFAULT_CP);
   begin
     if interprete(PSOChar(obj.AsArray.S[1]), 'controller') then
     if interprete(PSOChar(obj.AsArray.S[2]), 'action') then
        interprete(PSOChar(obj.AsArray.S[3]), 'id');
   end;

  // default controller is application
  if (FParams.AsObject['controller'] = nil) then
    FParams.AsObject.S['controller'] := 'application';


  // default action is index
  if (FParams.AsObject['action'] = nil) then
    (FParams.AsObject.S['action'] := 'index');

  // detect format
  if (FParams.AsObject['format'] = nil) then
    FParams.AsObject.S['format'] := 'html';
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
  obj, ret: ISuperObject;
  ite: TSuperAvlEntry;
  rec: TSearchRec;
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
      for obj in FParams do
        if obj <> nil then
          obj.DataPtr := Pointer(1);

      case TrySOInvoke(FContext, Self, 'ctrl_' + S['controller'] + '_' +
        S['action'] + '_' + Request.S['method'], FParams, ret) of
      irParamError: FErrorCode := 400;
      irError: FErrorCode := 500;
      else
        for ite in FParams.AsObject do
          if (ite.Value <> nil) and (ite.Value.DataPtr = nil) then
            FReturn.AsObject[ite.Name] := ite.Value;
      end;

      if FErrorCode <> 200 then Exit;

      // view ?
      if (RenderInternal = irSuccess) or
        RenderScript(FParams) then
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

end.



