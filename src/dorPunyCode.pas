(*
 * punycode.c from RFC 3492prop
 * http://www.nicemice.net/idn/
 * Adam M. Costello
 * http://www.nicemice.net/amc/
 *
 * This is ANSI C code (C89) implementing Punycode (RFC 3492prop).
 * Delphi Conversion by:
 *   Henri Gourvest <hgourvest@gmail.com>
 *   http://www.progdigy.com
 * contributor
 *   J. Heffernan <info@heffs.org.uk>

usage:

function PEncode(const str: UnicodeString): AnsiString;
var
  len: Cardinal;
begin
  if str = '' then
  begin
    Result := '';
    exit;
  end;
  if (PunycodeEncode(Length(str), PPunyCode(str), len) = pcSuccess) and (Length(str) + 1 <> len) then
  begin
    SetLength(Result, len);
    PunycodeEncode(Length(str), PPunyCode(str), len, PByte(Result));
  end else
    Result := str;
end;

function PDecode(const str: AnsiString): UnicodeString;
var
  outputlen: Cardinal;
begin
  if str = '' then
  begin
    Result := '';
    exit;
  end;
  outputlen := 0;
  if (PunycodeDecode(Length(str), PByte(str), outputlen) = pcSuccess) and (Length(str) <> outputlen) then
  begin
    SetLength(Result, outputlen);
    PunycodeDecode(Length(str), PByte(str), outputlen, PPunycode(Result));
  end else
    Result := str;
end;

procedure Test(const Input: UnicodeString);
begin
  if PDecode(PEncode(Input))<>Input then
    raise EAssertionFailed.CreateFmt('Round-trip failed: %s', [Input]);
end;

begin
  Test('президент');
  Test('David Heffernan');
  Test('');
  Test('A');
end.

 *)

unit dorPunyCode;

interface

type
  TPunyCodeStatus = (
    pcSuccess,
    pcBadInput,   (* Input is invalid.                       *)
    pcBigOutput,  (* Output would exceed the space provided. *)
    pcOverflow    (* Input needs wider integers to process.  *)
  );

  TPunyCode = Word;
  TPunyCodeArray = array[0..(High(Integer) div SizeOf(TPunyCode)) - 1] of TPunyCode;
  PPunycode = ^TPunyCodeArray;

function PunycodeDecode(inputlen: Cardinal; const input: PByte;
  var outputlen: Cardinal; output: PPunycode = nil;
  caseflags: PByte = nil): TPunyCodeStatus;

function PunycodeEncode(inputlen: Cardinal; const input: PPunycode;
  var outputlen: Cardinal; const output: PByte = nil;
  const caseflags: PByte = nil): TPunyCodeStatus; overload;

function PunycodeEncodeDomain(const str: string): AnsiString; overload;

implementation
uses SysUtils;

(*** Bootstring parameters for Punycode ***)
const
  PUNY_BASE = 36;
  PUNY_TMIN = 1;
  PUNY_TMAX = 26;
  PUNY_SKEW = 38;
  PUNY_DAMP = 700;
  PUNY_INITIAL_BIAS = 72;
  PUNY_INITIAL_N = $80;
  PUNY_DELIMITER = $2D;

(* PUNY_maxint is the maximum value of a TPunyCode variable: *)
  PUNY_maxint: TPunyCode = High(TPunyCode);


(* flagged(bcp) tests whether a basic code point is flagged *)
(* (uppercase).  The behavior is undefined if bcp is not a  *)
(* basic code point.                                        *)

function PUNY_flagged(bcp: Byte): Byte; inline;
begin
  Result := Ord(bcp - 65 < 26);
end;

(* DecodeDigit(cp) returns the numeric value of a basic code *)
(* point (for use in representing integers) in the range 0 to *)
(* BASE-1, or BASE if cp is does not represent a value.       *)

function PUNY_DecodeDigit(cp: Cardinal): Cardinal; inline;
begin
  if (cp - 48 < 10) then
    Result := cp - 22
  else if (cp - 65 < 26) then
    Result := cp - 65
  else if (cp - 97 < 26) then
    Result := cp - 97
  else
    Result := PUNY_BASE;
end;

(* EncodeDigit(d,flag) returns the basic code point whose value      *)
(* (when used for representing integers) is d, which needs to be in   *)
(* the range 0 to BASE-1.  The lowercase form is used unless flag is  *)
(* nonzero, in which case the uppercase form is used.  The behavior   *)
(* is undefined if flag is nonzero and digit d has no uppercase form. *)

function PUNY_EncodeDigit(d: Cardinal; flag: Boolean): Byte; inline;
begin
  Result := d + 22 + 75 * Byte(Byte(d < 26) - (Byte(flag) shl 5));
  (*  0..25 map to ASCII a..z or A..Z *)
  (* 26..35 map to ASCII 0..9         *)
end;

(* EncodeBasic(bcp,flag) forces a basic code point to lowercase *)
(* if flag is zero, uppercase if flag is nonzero, and returns    *)
(* the resulting code point.  The code point is unchanged if it  *)
(* is caseless.  The behavior is undefined if bcp is not a basic *)
(* code point.                                                   *)

function PUNY_EncodeBasic(bcp: TPunyCode; flag: Integer): Byte; inline;
begin
  Dec(bcp, ord(bcp - 97 < 26) shl 5);
  Result := bcp + (((not flag) and ord(bcp - 65 < 26)) shl 5);
end;

(*** Bias adaptation function ***)

function PUNY_Adapt(delta, numpoints: Cardinal; firsttime: Boolean): Cardinal; inline;
var
  k: TPunyCode;
begin
  if firsttime then
    delta := delta div PUNY_DAMP
  else
    delta := delta shr 1;

  (* delta shr 1 is a faster way of doing delta div 2 *)
  Inc(delta, delta div numpoints);

  k := 0;
  while (delta > ((PUNY_BASE - PUNY_TMIN) * PUNY_TMAX) / 2) do
  begin
    delta := delta div (PUNY_BASE - PUNY_TMIN);
    Inc(k, PUNY_BASE);
  end;

  Result := k + (PUNY_BASE - PUNY_TMIN + 1) * delta div (delta + PUNY_SKEW);
end;

(* PunycodeEncode() converts Unicode to Punycode.  The input     *)
(* is represented as an array of Unicode code points (not code    *)
(* units; surrogate pairs are not allowed), and the output        *)
(* will be represented as an array of ASCII code points.  The     *)
(* output string is *not* null-terminated; it will contain        *)
(* zeros if and only if the input contains zeros.  (Of course     *)
(* the caller can leave room for a terminator and add one if      *)
(* needed.)  The inputlen is the number of code points in         *)
(* the input.  The outputlen is an in/out argument: the           *)
(* caller passes in the maximum number of code points that it     *)
(* can receive, and on successful return it will contain the      *)
(* number of code points actually output.  The case_flags array   *)
(* holds input_length boolean values, where nonzero suggests that *)
(* the corresponding Unicode character be forced to uppercase     *)
(* after being decoded (if possible), and zero suggests that      *)
(* it be forced to lowercase (if possible).  ASCII code points    *)
(* are encoded literally, except that ASCII letters are forced    *)
(* to uppercase or lowercase according to the corresponding       *)
(* uppercase flags.  If case_flags is a null pointer then ASCII   *)
(* letters are left as they are, and other code points are        *)
(* treated as if their uppercase flags were zero.  The return     *)
(* value can be any of the TPunyCodeStatus values defined above   *)
(* except pcBadInput; if not pcSuccess, then       *)
(* output_size and output might contain garbage.                  *)

function PunycodeEncode(inputlen: Cardinal; const input: PPunycode;
  var outputlen: Cardinal; const output: PByte = nil;
  const caseflags: PByte = nil): TPunyCodeStatus;
var
  outidx, maxout, n, delta, h, b, bias, m, q, k, t: Cardinal;
  j: Integer;
begin
  (* Initialize the state: *)

  n := PUNY_INITIAL_N;
  outidx := 0;
  delta := outidx;
  maxout := outputlen;
  bias := PUNY_INITIAL_BIAS;

  (* Handle the basic code points: *)

  for j := 0 to inputlen - 1 do
  begin
    if (input[j] < $80) then
    begin
      if (output <> nil) then
      begin
        if (maxout - outidx < 2) then
          Exit(pcBigOutput);
        if (caseflags <> nil) then
          output[outidx] := PUNY_EncodeBasic(input[j], caseflags[j])
        else
          output[outidx] := input[j];
      end;

      Inc(outidx);
    end;
    (* else if (input[j] < n) return pcBadInput; *)
    (* (not needed for Punycode with unsigned code points) *)
  end;

  b := outidx;
  h := b;

  (* h is the number of code points that have been handled, b is the *)
  (* number of basic code points, and out is the number of characters *)
  (* that have been output. *)

  if (b > 0) then
  begin
    if (output <> nil) then
      output[outidx] := PUNY_DELIMITER;
    Inc(outidx);
  end;

  (* Main encoding loop: *)

  while (h < inputlen) do
  begin
    (* All non-basic code points < n have been *)
    (* handled already.  Find the next larger one: *)

    m := PUNY_maxint;
    for j := 0 to inputlen - 1 do
      (* if (basic(input[j])) continue; *)
      (* (not needed for Punycode) *)
      if ((input[j] >= n) and (input[j] < m)) then
        m := input[j];

    (* Increase delta enough to advance the decoder's *)
    (* <n,i> state to <m,0>, but guard against overflow: *)

    if (m - n > (PUNY_maxint - delta) / (h + 1)) then
      Exit(pcOverflow);
    Inc(delta, (m - n) * (h + 1));
    n := m;

    for j := 0 to inputlen - 1 do
    begin
      (* Punycode does not need to check whether input[j] is basic: *)
      if (input[j] < n (* or basic(input[j]) *) ) then
      begin
        Inc(delta);
        if (delta = 0) then
          Exit(pcOverflow);
      end;

      if (input[j] = n) then
      begin
        (* Represent delta as a generalized variable-length integer: *)

        q := delta;
        k := PUNY_BASE;
        while true do
        begin
          if (output <> nil) then
            if (outidx >= maxout) then
              Exit(pcBigOutput);
          if k <= bias (* + TMIN *) then (* +TMIN not needed *)
            t := PUNY_TMIN
          else if k >= bias + PUNY_TMAX then
            t := PUNY_TMAX
          else
            t := k - bias;
          if (q < t) then
            break;
          if (output <> nil) then
            output[outidx] := PUNY_EncodeDigit(t + (q - t) mod (PUNY_BASE - t), False);
          Inc(outidx);
          q := (q - t) div (PUNY_BASE - t);
          Inc(k, PUNY_BASE);
        end;
        if (output <> nil) then
          output[outidx] := PUNY_EncodeDigit(q,
            (caseflags <> nil) and (caseflags[j] <> 0));
        Inc(outidx);
        bias := PUNY_Adapt(delta, h + 1, h = b);
        delta := 0;
        Inc(h);
      end;
    end;

    Inc(delta);
    Inc(n);
  end;

  outputlen := outidx;
  Result := pcSuccess;
end;

(* PunycodeDecode() converts Punycode to Unicode.  The input is  *)
(* represented as an array of ASCII code points, and the output   *)
(* will be represented as an array of Unicode code points.  The   *)
(* input_length is the number of code points in the input.  The   *)
(* output_length is an in/out argument: the caller passes in      *)
(* the maximum number of code points that it can receive, and     *)
(* on successful return it will contain the actual number of      *)
(* code points output.  The case_flags array needs room for at    *)
(* least output_length values, or it can be a null pointer if the *)
(* case information is not needed.  A nonzero flag suggests that  *)
(* the corresponding Unicode character be forced to uppercase     *)
(* by the caller (if possible), while zero suggests that it be    *)
(* forced to lowercase (if possible).  ASCII code points are      *)
(* output already in the proper case, but their flags will be set *)
(* appropriately so that applying the flags would be harmless.    *)
(* The return value can be any of the TPunyCodeStatus values      *)
(* defined above; if not pcSuccess, then output_length,    *)
(* output, and case_flags might contain garbage.  On success, the *)
(* decoder will never need to write an output_length greater than *)
(* input_length, because of how the encoding is defined.          *)

function PunycodeDecode(inputlen: Cardinal; const input: PByte;
  var outputlen: Cardinal; output: PPunycode;
  caseflags: PByte): TPunyCodeStatus;
var
  outidx, i, maxout, bias, b, inidx, oldi, w, k, digit, t, n : Cardinal;
  j: Integer;
begin

  (* Initialize the state: *)

  n := PUNY_INITIAL_N;
  outidx := 0;
  i := outidx;
  maxout := outputlen;
  bias := PUNY_INITIAL_BIAS;

  (* Handle the basic code points:  Let b be the number of input code *)
  (* points before the last DELIMITER, or 0 if there is none, then *)
  (* copy the first b code points to the output. *)

  b := 0;
  for j := 0 to inputlen - 1 do
    if input[j] = PUNY_DELIMITER then
      b := j;

  if output <> nil then
    if (b > maxout) then
      Exit(pcBigOutput);

  for j := 0 to b - 1 do
  begin
    if (caseflags <> nil) then
      caseflags[outidx] := PUNY_flagged(input[j]);
    if (input[j] >= $80) then
      Exit(pcBadInput);
    if output <> nil then
      output[outidx] := input[j];
    Inc(outidx);
  end;

  (* Main decoding loop:  Start just after the last DELIMITER if any *)
  (* basic code points were copied; start at the beginning otherwise. *)

  if (b > 0) then
    inidx := b + 1
  else
    inidx := 0;

  while inidx < inputlen do
  begin
    (* in is the index of the next character to be consumed, and *)
    (* out is the number of code points in the output array. *)

    (* Decode a generalized variable-length integer into delta, *)
    (* which gets added to i.  The overflow checking is easier *)
    (* if we increase i as we go, then subtract off its starting *)
    (* value at the end to obtain delta. *)

    oldi := i;
    w := 1;
    k := PUNY_BASE;
    while true do
    begin
      if (inidx >= inputlen) then
        Exit(pcBadInput);
      digit := PUNY_DecodeDigit(input[inidx]);
      Inc(inidx);
      if (digit >= PUNY_BASE) then
        Exit(pcBadInput);
      if (digit > (PUNY_maxint - i) / w) then
        Exit(pcOverflow);
      Inc(i, digit * w);
      if k <= bias (* + TMIN *) then
        t := PUNY_TMIN
      else (* +TMIN not needed *)
      if k >= bias + PUNY_TMAX then
        t := PUNY_TMAX
      else
        t := k - bias;
      if (digit < t) then
        break;
      if (w > (PUNY_maxint / (PUNY_BASE - t))) then
        Exit(pcOverflow);
      w := w * (PUNY_BASE - t);
      Inc(k, PUNY_BASE);
    end;

    bias := PUNY_Adapt(i - oldi, outidx + 1, oldi = 0);

    (* i was supposed to wrap around from out+1 to 0, *)
    (* incrementing n each time, so we'll fix that now: *)

    if (i / (outidx + 1) > PUNY_maxint - n) then
      Exit(pcOverflow);
    Inc(n, i div (outidx + 1));
    i := i mod (outidx + 1);

    (* Insert n at position i of the output: *)

    (* not needed for Punycode: *)
    (* if (DecodeDigit(n) <= BASE) return punycode_invalid_input; *)
    if output <> nil then
      if (outidx >= maxout) then
        Exit(pcBigOutput);

    if (caseflags <> nil) then
    begin
      move(caseflags[i], caseflags[i + 1], outidx - i);

      (* Case of last character determines uppercase flag: *)
      caseflags[i] := PUNY_flagged(input[inidx - 1]);
    end;

    if output <> nil then
    begin
      move(output[i], output[i + 1], (outidx - i) * SizeOf(TPunyCode));
      output[i] := n;
    end;
    Inc(i);

    Inc(outidx);
  end;

  outputlen := outidx;
  Result := pcSuccess;
end;

function PunycodeEncodeDomain(const str: UnicodeString): AnsiString; overload;
var
  inlen, outlen: Cardinal;
  ansi: AnsiString;
  p, s: PWideChar;
  a: PAnsiChar;

  procedure doit(dot: Boolean);
  begin
    inlen := p - s;
    if (PunycodeEncode(inlen, PPunyCode(s), outlen) = pcSuccess) and (inlen + 1 <> outlen) then
    begin
      if dot then
        SetLength(ansi, outlen + 4 + 1) else
        SetLength(ansi, outlen + 4);
      a := PAnsiChar(ansi);
      Move(PAnsiChar('xn--')^, a^, 4);
      inc(a, 4);
      PunycodeEncode(inlen, PPunyCode(s), outlen, PByte(a));
      if dot then
      begin
        inc(a, outlen);
        a^ := '.';
      end;
    end else
      if dot then
        SetString(ansi, s, inlen + 1) else
        SetString(ansi, s, inlen);
    Result := Result + ansi;
  end;
begin
  Result := '';
  p := PWideChar(str);
  s := p;

  while True do
  case p^ of
    '.':
      begin
        doit(True);
        Inc(p);
        s := p;
      end;
    #0 :
      begin
        doit(False);
        Break;
      end;
  else
    Inc(p);
  end;
end;

end.
