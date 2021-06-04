unit dorHTTP;

interface

uses
  SysUtils, AnsiStrings,
  WinSock2;

type
  TOnHTTPReadWrite = reference to function(var Buf; len: Integer): Integer;
  TOnHTTPField = reference to function(const key: RawByteString; const value: RawByteString): Boolean;
  TOnHTTPAddField = reference to procedure(const add: TOnHTTPField);
  TOnHTTPResponse = reference to function(code: Integer; const mesg: RawByteString): Boolean;
  TOnHTTPHeaderField = reference to function(group: Integer; const key: RawByteString; const value: RawByteString): Boolean;

function HTTPParse(const receive: TOnHTTPReadWrite; const onResponse: TOnHTTPResponse;
  const onfield: TOnHTTPField): Boolean;

function HTTPEncode(const AStr: string): RawByteString;
function HTTPDecode(const AStr: string): RawByteString;
function HttpStatusString(code: Integer): string;
function HttpResponseStrings(code: Integer): RawByteString;
function HTTPParseURL(const uri: PChar; out protocol: string;
  out domain: AnsiString; out port: Word; out path: RawByteString; encode: Boolean): Boolean;
function HTTPParseHeader(const header: RawByteString; subkeys: Boolean; const onfield: TOnHTTPHeaderField): Boolean;
function HTTPReadChunked(const read, write: TOnHTTPReadWrite): Boolean;

function HTMLEncode(const AStr: string): string;

implementation

uses
  dorPunyCode;

function HTTPParseURL(const uri: PChar; out protocol: string;
  out domain: AnsiString; out port: Word; out path: RawByteString; encode: Boolean): Boolean;
type
  TState = (sStart, stSlash, sDomain, sPort);
label
  redo;
var
  p, d, dot1, dot2: PChar;
  s: TState;
  o: Integer;
  procedure pushdot;
  begin
    if dot1 = nil then
      dot1 := p else
      begin
        dot2 := dot1;
        dot1 := p;
      end;
  end;

  procedure getdomain;
  var
    len: Cardinal;
  begin
    if dot2 = nil then pushdot;
    Inc(dot2);
    if (PunycodeEncode(dot1-dot2, PPunyCode(dot2), len) = pcSuccess) and
      (Cardinal(dot1-dot2 + 1) <> len) then
    begin
      SetLength(domain, len);
      PunycodeEncode(dot1-dot2, PPunyCode(dot2), len, PByte(domain));
      domain := AnsiString(Copy(d, 0, dot2 - d)) + 'xn--' +
        domain + AnsiString(Copy(dot1, 0, p - dot1));
    end else
      SetString(domain, d,  p - d);
  end;
begin
  protocol := '';
  domain := 'localhost';
  port := 0;
  path := '/';

  d := nil;
  dot1 := nil;
  dot2 := nil;
  p := uri;
  s := sStart;
  o := 0;

  while True do
    begin
redo:
      case s of
        sStart:
          case p^ of
            ':':
              begin
                protocol := LowerCase(protocol);
                s := stSlash;
                o := 0;
              end;
            #0 : Exit(False);
          else
            protocol := protocol + p^;
          end;
        stSlash:
          case o of
            0: if p^ = '/' then Inc(o) else
              begin
                s := sDomain;
                o := 1;
                dot1 := p;
                goto redo;
              end;
            1: if p^ = '/' then
               begin
                 s := sDomain;
                 o := 0;
                 pushdot;
               end else
                 Exit(False);
          else
            Exit(False)
          end;
        sDomain:
          case o of
            0:
             begin
               case p^ of
                 #0: Exit(True);
                 '.': dot1 := p;
               end;
               d := p;
               inc(o);
             end
          else
            case p^ of
              ':':
                begin
                  if p - d >= 1 then
                    getdomain;
                  s := sPort;
                  port := 0;
                  o := 0;
                end;
              '/':
                begin
                  if p - d >= 1 then
                    getdomain;
                  if encode then
                    path := HTTPEncode(p) else
                    path := rawbytestring(string(p));
                  Exit(True);
                end;
              '.': pushdot;
              #0 :
                begin
                  if p - d >= 1 then
                    getdomain;
                  Exit(True);
                end;
            end;
          end;
        sPort:
          case p^ of
            '0'..'9': port := port * 10 + Ord(p^) - Ord('0');
            '/':
              begin
                if encode then
                  path := HTTPEncode(p) else
                  path := rawbytestring(string(p));
                Exit(True);
              end;
            #0: Exit(True);
          else
            Exit(False);
          end;
      else
        Exit(False);
      end;
      Inc(p);
    end;
end;

(******************************************************************************)
(* HTTPParse                                                                  *)
(* parse HTTP header fields                                                   *)
(******************************************************************************)

function HTTPParse(const receive: TOnHTTPReadWrite;
  const onResponse: TOnHTTPResponse;
  const onfield: TOnHTTPField): Boolean;
var
  c: AnsiChar;
  st, pos: Integer;
  key, value: RawByteString;
//{$if defined(DEBUG)}
//  whole: RawByteString;
//{$endif}
begin
  st := 0;
  pos := 0;
  while receive(c, 1) = 1 do
  begin
//  {$if defined(DEBUG)}
//    whole := whole + c;
//  {$endif}

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
             7: if not (c in ['0', '1']) then Exit(False);
             8: if (c = ' ') then
                begin
                  st := 1;
                  pos := 0;
                  Continue;
                end
                else
                  Exit(False);
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
          'a'..'z', 'A'..'Z', '0'..'9', '-', '_': key := key + c;
          ':': st := 5;
          #13: st := -1;
        else
          Exit(False);
        end;
      5:
        begin
          if c <> ' ' then
            Exit(False);
          st := 6;
        end;
      6:
        case c of
          #13: st := 7;
        else
          value := value + c;
        end;
      7:
        if c = #10 then
        begin
          st := 4;
          if Assigned(onfield) then
            if not onfield(key, value) then
              Exit(False);
          key := '';
          value := '';
        end
        else
          Exit(False);
      -1:
        Exit(c = #10);
    else
      Exit(False);
    end;
  end;

  Result := False;
end;

function HTTPEncode(const AStr: string): RawByteString;
const
  NoConversion = ['A'..'Z', 'a'..'z', '0'..'9', '!', '#', '&', '''', '(', ')',
    '*', '-', '.', '/', ':', ';', '=', '?', '@', '_'];
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
        AnsiStrings.FormatBuf(Rp^, 3, AnsiString('%%%.2x'), 6, [Ord(Sp^)]);
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

function HTTPReadChunked(const read, write: TOnHTTPReadWrite): Boolean;
type
  TState = (stStart, stCR1, stLF1, stChunk, stCR2, stLF2, stCR3, stLF3);
label
  redo;
var
  c: AnsiChar;
  st: TState;
  size, len, rcv: Integer;
  buff: array[0..1023] of AnsiChar;
begin
  st := stStart;
  len := 0;
  while True do
  begin
    if read(c, 1) <> 1 then Exit(False);
redo:
    case st of
      stStart:
        case c of
          'A'..'F': len := (len * 16) + (Ord(c) - 55);
          'a'..'f': len := (len * 16) + (Ord(c) - 87);
          '0'..'9': len := (len * 16) + (Ord(c) - 48);
          #13: st := stLF1;
        else
          st := stCR1;
        end;
      stCR1: if c = #13 then st := stLF1;
      stLF1: if c = #10 then
        begin
          st := stChunk;
          goto redo;
        end else Exit(False);
      stChunk:
        begin
          size := len;
          while size > 0 do
          begin
            if size >= SizeOf(buff) then
            begin
              rcv := read(buff, SizeOf(buff));
              if rcv <> SizeOf(buff) then
                Exit(False);
            end else
            begin
              rcv := read(buff, size);
              if rcv <> size then
                Exit(False);
            end;
            Dec(size, rcv);
            Write(buff, rcv);
          end;
          st := stCR2;
        end;
      stCR2:
        case c of
          #13: st := stLF2;
          #10:
            begin
              st := stLF2;
              goto redo;
            end
        else
          Exit(False);
        end;
      stLF2:
        if c = #10 then
        begin
          if len > 0 then
          begin
            st := stStart;
            len := 0;
          end else
            Exit(True);
        end else
          Exit(False);
    end;
  end;
end;

function HTTPParseHeader(const header: RawByteString; subkeys: Boolean; const onfield: TOnHTTPHeaderField): Boolean;
type
  TState = (stStart, stEat, stKey, stKeyEnd, stValue, stValueEnd);
var
  group: Integer;
  p: PAnsiChar;
  st, saved: TState;
  k, v: RawByteString;
  delimitersKey, delimitersValue: set of AnsiChar;
label
  redo;
begin
  delimitersKey := [';', '=', #0];
  delimitersValue := [';', #0];
  if subkeys then
  begin
    Include(delimitersKey, ',');
    Include(delimitersKey, ' ');
    Include(delimitersValue, ',');
    Include(delimitersValue, ' ');
  end;

  group := 0;
  p := PAnsiChar(header);
  st := stEat;
  saved := stStart;
  while True do
  begin
redo:
    case st of
      stEat:
        begin
          if p^ <> ' ' then
          begin
            st := saved;
            goto redo;
          end;
        end;
      stStart:
        case p^ of
          ';':
            begin
              inc(group);
              st := stEat;
              saved := stStart;
            end;
          #0 : Exit(True);
        else
          st := stEat;
          saved := stKey;
          goto redo;
        end;
      stKey:
        if not (p^ in delimitersKey) then
          k := k + p^ else
          begin
            st := stEat;
            saved := stKeyEnd;
            goto redo;
          end;
      stKeyEnd:
        case p^ of
          ';':
            begin
              if not onfield(group, k, '') then
                Exit(True);
              Inc(group);
              k := '';
              st := stEat;
              saved := stKey;
            end;
          ',':
            begin
              if not onfield(group, k, '') then
                Exit(True);
              k := '';
              st := stEat;
              saved := stKey;
            end;
          '=':
            begin
              st := stEat;
              saved := stValue;
            end;
          #0:
            begin
              onfield(group, k, '');
              Exit(True);
            end;
        else
          Exit(False);
        end;
      stValue:
        if not (p^ in delimitersValue) then
          v := v + p^ else
          begin
            st := stEat;
            saved := stValueEnd;
            goto redo;
          end;
      stValueEnd:
        case p^ of
          ';':
            begin
              if not onfield(group, k, v) then
                Exit(True);
              Inc(group);
              k := '';
              v := '';
              st := stEat;
              saved := stKey;
            end;
          ',':
            begin
              if not onfield(group, k, v) then
                Exit(True);
              k := '';
              v := '';
              st := stEat;
              saved := stKey;
            end;
          #0:
            begin
              onfield(group, k, v);
              Exit(True);
            end;
        else
          Exit(False);
        end;
    end;
    inc(p);
  end;
end;

function HttpStatusString(code: Integer): string;
begin
  case code of
    { Information responses }
    100: Result := 'Continue';
    101: Result := 'Switching Protocol';
    102: Result := 'Processing'; // WebDAV
    { Successful responses }
    200: Result := 'OK';
    201: Result := 'Created';
    202: Result := 'Accepted';
    203: Result := 'Non-Authoritative Information';
    204: Result := 'No Content';
    205: Result := 'Reset Content';
    206: Result := 'Partial Content';
    207: Result := 'Multi-Status'; // WebDAV
    208: Result := 'Multi-Status'; // WebDAV
    226: Result := 'IM Used'; // HTTP Delta Encoding
    { Redirection messages }
    300: Result := 'Multiple Choice';
    301: Result := 'Moved Permanently';
    302: Result := 'Found';
    303: Result := 'See Other';
    304: Result := 'Not Modified';
    305: Result := 'Use Proxy';
    306: Result := 'unused';
    307: Result := 'Temporary Redirect';
    308: Result := 'Permanent Redirect';
    { Client error responses }
    400: Result := 'Bad Request';
    401: Result := 'Unauthorized';
    402: Result := 'Payment Required';
    403: Result := 'Forbidden';
    404: Result := 'Not Found';
    405: Result := 'Method Not Allowed';
    406: Result := 'Not Acceptable';
    407: Result := 'Proxy Authentication Required';
    408: Result := 'Request Timeout';
    409: Result := 'Conflict';
    410: Result := 'Gone';
    411: Result := 'Length Required';
    412: Result := 'Precondition Failed';
    413: Result := 'Payload Too Large';
    414: Result := 'URI Too Long';
    415: Result := 'Unsupported Media Type';
    416: Result := 'Requested Range Not Satisfiable';
    417: Result := 'Expectation Failed';
    418: Result := 'I''m a teapot';
    421: Result := 'Misdirected Request';
    422: Result := 'Unprocessable Entity'; // WebDAV
    423: Result := 'Locked'; // WebDAV
    424: Result := 'Failed Dependency'; // WebDAV
    425: Result := 'Unordered Collection'; // WebDAV
    426: Result := 'Upgrade Required';
    428: Result := 'Precondition Required';
    429: Result := 'Too Many Requests';
    431: Result := 'Request Header Fields Too Large';
    449: Result := 'Retry With';
    450: Result := 'Blocked by Windows Parental Controls';
    451: Result := 'Unavailable For Legal Reasons';
    { Server error responses }
    500: Result := 'Internal Server Error';
    501: Result := 'Not Implemented';
    502: Result := 'Bad Gateway';
    503: Result := 'Service Unavailable';
    504: Result := 'Gateway Timeout';
    505: Result := 'HTTP Version Not Supported';
    506: Result := 'Variant Also Negotiates';
    507: Result := 'Insufficient Storage'; // WebDAV
    508: Result := 'Loop Detected';
    509: Result := 'Bandwidth Limit Exceeded';
    510: Result := 'Not Extended';
    511: Result := 'Network Authentication Required';
  else
    Result := IntToStr(code);
  end;
end;

function HttpResponseStrings(code: integer): RawByteString;
begin
  case code of
    100, 101:
      Result := RawByteString(Format('%d %s', [code, HttpStatusString(code)]));
    102:
      Result := 'Processing'; // WebDAV
    200..208, 226,
    300..308,
    400..418, 421..426, 428..429, 431, 449..451,
    500..511:
      Result := RawByteString(Format('HTTP/1.1 %d %s', [code, HttpStatusString(code)]))
  else
    Result := 'HTTP/1.1 ' + RawByteString(IntToStr(code));
  end;
end;

function HTMLEncode(const AStr: string): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(AStr) do
    case AStr[I] of
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '&': Result := Result + '&amp;';
      '"': Result := Result + '&quot;';
    else
      Result := Result + AStr[I];
    end;
end;

end.
