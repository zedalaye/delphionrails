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

end.
