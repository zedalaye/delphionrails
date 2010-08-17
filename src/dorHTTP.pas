unit dorHTTP;

interface
uses WinSock;

type
  TOnHTTPField = reference to function(const key: string; const value: RawByteString): Boolean;
  TOnHTTPAddField = reference to procedure(const add: TOnHTTPField);
  TOnHTTPResponse = reference to function(code: Integer; const mesg: RawByteString): Boolean;

function HTTPParse(socket: TSocket; const onResponse: TOnHTTPResponse;
  const onfield: TOnHTTPField): Boolean;

function HTTPEncode(const AStr: string): RawByteString;
function HTTPDecode(const AStr: string): RawByteString;
function HttpResponseStrings(code: integer): RawByteString;

implementation
uses SysUtils;

(******************************************************************************)
(* HTTPParse                                                                  *)
(* parse HTTP header fields                                                   *)
(******************************************************************************)

function HTTPParse(socket: TSocket;
  const onResponse: TOnHTTPResponse;
  const onfield: TOnHTTPField): Boolean;
var
  c: AnsiChar;
  st, pos: Integer;
  key, value: RawByteString;
begin
  st := 0;
  pos := 0;
  while WinSock.recv(socket, c, 1, 0) = 1 do
  begin
    case st of
      // proto HTTP
      0:
         begin
           case pos of
             0: if c <> 'H' then Exit(False);
             1: if c <> 'T' then Exit(False);
             2: if c <> 'T' then Exit(False);
             3: if c <> 'P' then Exit(False);
             4: if c <> '/' then Exit(False);
             5: if c <> '1' then Exit(False);
             6: if c <> '.' then Exit(False);
             7: if c <> '1' then Exit(False); // only 1.1 have upgrade
             8: if (c = ' ') then begin st := 1; pos := 0; Continue; end else Exit(False);
           end;
           inc(pos);
         end;
      // result code
      1: case c of
           '0'..'9': pos := pos * 10 + ord(c) - Ord('0');
           ' ':
             begin
               st := 2;
               value := '';
             end;
           #13:
             begin
                st := 3;
                value := '';
             end;
         else
           Exit(False);
         end;
      // result message
      2: case c of
           #13: st := 3;
         else
           value := value + c;
         end;
      // end message
      3:
        begin
          if c <> #10 then Exit(False);
          if Assigned(onResponse) then
            if not onResponse(pos, value) then
              Exit(False);
          key := '';
          value := '';
          st := 4;
        end;
      // field name
      4:
        case c of
          'a'..'z', 'A'..'Z', '0'..'1', '-': key := key + c;
          ':': st := 5;
          #13: st := -1;
        else
          Exit(False);
        end;
      5:
        begin
          if c <> ' ' then Exit(False);
          st := 6;
        end;
      6:
        case c of
          #13: st := 7;
        else
          value := value + c;
        end;
      7: if c = #10 then
         begin
           st := 4;
           if Assigned(onfield) then
             if not onfield(string(key), value) then
               Exit(False);
           key := '';
           value := '';
         end else
           Exit(False);
      -1: Exit(c = #10);
    else
      Exit(False);
    end;
  end;
  Result := False;
end;

function HTTPEncode(const AStr: string): RawByteString;
const
  NoConversion = ['A'..'Z','a'..'z','*','@','.','_','-',
                  '0'..'9','$','!','''','(',')', '/'];
var
  Sp: PChar;
  Rp: PAnsiChar;
begin
  SetLength(Result, Length(AStr) * 3);
  Sp := PChar(AStr);
  Rp := PAnsiChar(Result);
  while Sp^ <> #0 do
  begin
    if (Sp^ < #256) and (AnsiChar(Sp^) in NoConversion) then
      Rp^ := AnsiChar(Sp^)
    else
      if Sp^ = ' ' then
        Rp^ := '+'
      else
      begin
        FormatBuf(Rp^, 3, AnsiString('%%%.2x'), 6, [Ord(Sp^)]);
        Inc(Rp,2);
      end;
    Inc(Rp);
    Inc(Sp);
  end;
  SetLength(Result, Rp - PAnsiChar(Result));
end;

function HTTPDecode(const AStr: string): RawByteString;
var
  Sp, Rp, Cp: PAnsiChar;
  src: RawByteString;
begin
  src := RawByteString(AStr);
  SetLength(Result, Length(src));
  Sp := PAnsiChar(src);
  Rp := PAnsiChar(Result);
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
                 Result := '';
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
  SetLength(Result, Rp - PAnsiChar(Result));
end;

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


end.
