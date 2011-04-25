unit dorMD5;

(* Copyright (C) 1991-2, RSA Data Security, Inc. Created 1991. All
rights reserved.

License to copy and use this software is granted provided that it
is identified as the "RSA Data Security, Inc. MD5 Message-Digest
Algorithm" in all material mentioning or referencing this software
or this function.

License is also granted to make and use derivative works provided
that such works are identified as "derived from the RSA Data
Security, Inc. MD5 Message-Digest Algorithm" in all material
mentioning or referencing the derived work.

RSA Data Security, Inc. makes no representations concerning either
the merchantability of this software or the suitability of this
software for any particular purpose. It is provided "as is"
without express or implied warranty of any kind.

These notices must be retained in any copies of any part of this
documentation and/or software. *)

(* conversion by Henri Gourvest <hgourvest@gmail.com> *)

interface

//******************************************************************************
// MD5
//******************************************************************************

type
  TMD5Ctx = packed record
    state : array[0..3] of Cardinal; (* state (ABCD) *)
    count : array[0..1] of Cardinal; (* number of bits, modulo 2^64 (lsb first) *)
    buffer: array[0..63] of Byte;    (* input buffer *)
  end;
  PMD5Ctx = ^TMD5Ctx;

procedure MD5Init(context: PMD5Ctx);
procedure MD5Update(context: PMD5Ctx; input: PByte; inputLen: Cardinal);
procedure MD5Final(digest: PByte; context: PMD5Ctx);
procedure MD5(input: PByte; len: Cardinal; output: PByte); overload;
function MD5(const Value: RawByteString): RawByteString; overload;

implementation

(* Constants for MD5Transform routine. *)

const
  MD5_S11 = 7;
  MD5_S12 = 12;
  MD5_S13 = 17;
  MD5_S14 = 22;
  MD5_S21 = 5;
  MD5_S22 = 9;
  MD5_S23 = 14;
  MD5_S24 = 20;
  MD5_S31 = 4;
  MD5_S32 = 11;
  MD5_S33 = 16;
  MD5_S34 = 23;
  MD5_S41 = 6;
  MD5_S42 = 10;
  MD5_S43 = 15;
  MD5_S44 = 21;

  MD5_PADDING: array[0..63] of Byte = (
    $80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

(* F, G, H and I are basic MD5 functions. *)

function MD5_F(const x, y, z: Cardinal): Cardinal; inline;
begin
  Result := (x and y) or ((not x) and z);
end;

function MD5_G(const x, y, z: Cardinal): Cardinal; inline;
begin
  Result := (x and z) or (y and (not z));
end;

function MD5_H(const x, y, z: Cardinal): Cardinal; inline;
begin
  Result := x xor y xor z;
end;

function MD5_I(const x, y, z: Cardinal): Cardinal; inline;
begin
  Result := y xor (x or (not z));
end;

(* ROTATE_LEFT rotates x left n bits. *)

function MD5_ROTATE_LEFT(const x: Cardinal; const n: Byte): Cardinal; inline;
begin
  Result := (x shl n) or (x shr (32-n));
end;

(* FF, GG, HH, and II transformations for rounds 1, 2, 3, and 4.
   Rotation is separate from addition to prevent recomputation. *)

procedure MD5_FF(var a: Cardinal; const b, c, d, x: Cardinal; const s: Byte; const ac: Cardinal); inline;
begin
  Inc(a, MD5_F(b, c, d) + x + Cardinal(ac));
  a := MD5_ROTATE_LEFT(a, s);
  Inc(a, b);
end;

procedure MD5_GG(var a: Cardinal; const b, c, d, x: Cardinal; const s: Byte; const ac: Cardinal); inline;
begin
  Inc(a, MD5_G(b, c, d) + x + ac);
  a := MD5_ROTATE_LEFT(a, s);
  Inc(a, b);
end;

procedure MD5_HH(var a: Cardinal; const b, c, d, x: Cardinal; const s: Byte; const ac: Cardinal); inline;
begin
  Inc(a, MD5_H(b, c, d) + x + ac);
  a := MD5_ROTATE_LEFT(a, s);
  Inc(a, b);
end;

procedure MD5_II(var a: Cardinal; const b, c, d, x: Cardinal; const s: Byte; const ac: Cardinal); inline;
begin
  Inc(a, MD5_I(b, c, d) + x + ac);
  a := MD5_ROTATE_LEFT (a, s);
  Inc(a, b);
end;

(* Decodes input (unsigned char) into output (Cardinal). Assumes len is
  a multiple of 4. *)
procedure MD5_Decode(const output: PCardinal; input: PByte; len: Cardinal);
var
  j: Cardinal;
  p: PCardinal;
begin
  p := output;
  j := 0;
  while j < len do
  begin
    p^ :=
      input[j] or
      (input[j+1] shl 8) or
      (input[j+2] shl 16) or
      (input[j+3] shl 24);
    Inc(p);
    Inc(j, 4);
  end;
end;

(* MD5 basic transformation. Transforms state based on block. *)

procedure MD5Transform(state: PCardinal; const block: PByte);
var
  a, b, c, d: Cardinal;
  x: array[0..15] of Cardinal;
type
  TState = array[0..3] of Cardinal;
  PState = ^TState;
begin
  a := PState(state)[0];
  b := PState(state)[1];
  c := PState(state)[2];
  d := PState(state)[3];

  MD5_Decode (@x, block, 64);

  (* Round 1 *)
  MD5_FF(a, b, c, d, x[ 0], MD5_S11, $d76aa478); (* 1 *)
  MD5_FF(d, a, b, c, x[ 1], MD5_S12, $e8c7b756); (* 2 *)
  MD5_FF(c, d, a, b, x[ 2], MD5_S13, $242070db); (* 3 *)
  MD5_FF(b, c, d, a, x[ 3], MD5_S14, $c1bdceee); (* 4 *)
  MD5_FF(a, b, c, d, x[ 4], MD5_S11, $f57c0faf); (* 5 *)
  MD5_FF(d, a, b, c, x[ 5], MD5_S12, $4787c62a); (* 6 *)
  MD5_FF(c, d, a, b, x[ 6], MD5_S13, $a8304613); (* 7 *)
  MD5_FF(b, c, d, a, x[ 7], MD5_S14, $fd469501); (* 8 *)
  MD5_FF(a, b, c, d, x[ 8], MD5_S11, $698098d8); (* 9 *)
  MD5_FF(d, a, b, c, x[ 9], MD5_S12, $8b44f7af); (* 10 *)
  MD5_FF(c, d, a, b, x[10], MD5_S13, $ffff5bb1); (* 11 *)
  MD5_FF(b, c, d, a, x[11], MD5_S14, $895cd7be); (* 12 *)
  MD5_FF(a, b, c, d, x[12], MD5_S11, $6b901122); (* 13 *)
  MD5_FF(d, a, b, c, x[13], MD5_S12, $fd987193); (* 14 *)
  MD5_FF(c, d, a, b, x[14], MD5_S13, $a679438e); (* 15 *)
  MD5_FF(b, c, d, a, x[15], MD5_S14, $49b40821); (* 16 *)

 (* Round 2 *)
  MD5_GG(a, b, c, d, x[ 1], MD5_S21, $f61e2562); (* 17 *)
  MD5_GG(d, a, b, c, x[ 6], MD5_S22, $c040b340); (* 18 *)
  MD5_GG(c, d, a, b, x[11], MD5_S23, $265e5a51); (* 19 *)
  MD5_GG(b, c, d, a, x[ 0], MD5_S24, $e9b6c7aa); (* 20 *)
  MD5_GG(a, b, c, d, x[ 5], MD5_S21, $d62f105d); (* 21 *)
  MD5_GG(d, a, b, c, x[10], MD5_S22,  $2441453); (* 22 *)
  MD5_GG(c, d, a, b, x[15], MD5_S23, $d8a1e681); (* 23 *)
  MD5_GG(b, c, d, a, x[ 4], MD5_S24, $e7d3fbc8); (* 24 *)
  MD5_GG(a, b, c, d, x[ 9], MD5_S21, $21e1cde6); (* 25 *)
  MD5_GG(d, a, b, c, x[14], MD5_S22, $c33707d6); (* 26 *)
  MD5_GG(c, d, a, b, x[ 3], MD5_S23, $f4d50d87); (* 27 *)
  MD5_GG(b, c, d, a, x[ 8], MD5_S24, $455a14ed); (* 28 *)
  MD5_GG(a, b, c, d, x[13], MD5_S21, $a9e3e905); (* 29 *)
  MD5_GG(d, a, b, c, x[ 2], MD5_S22, $fcefa3f8); (* 30 *)
  MD5_GG(c, d, a, b, x[ 7], MD5_S23, $676f02d9); (* 31 *)
  MD5_GG(b, c, d, a, x[12], MD5_S24, $8d2a4c8a); (* 32 *)

  (* Round 3 *)
  MD5_HH(a, b, c, d, x[ 5], MD5_S31, $fffa3942); (* 33 *)
  MD5_HH(d, a, b, c, x[ 8], MD5_S32, $8771f681); (* 34 *)
  MD5_HH(c, d, a, b, x[11], MD5_S33, $6d9d6122); (* 35 *)
  MD5_HH(b, c, d, a, x[14], MD5_S34, $fde5380c); (* 36 *)
  MD5_HH(a, b, c, d, x[ 1], MD5_S31, $a4beea44); (* 37 *)
  MD5_HH(d, a, b, c, x[ 4], MD5_S32, $4bdecfa9); (* 38 *)
  MD5_HH(c, d, a, b, x[ 7], MD5_S33, $f6bb4b60); (* 39 *)
  MD5_HH(b, c, d, a, x[10], MD5_S34, $bebfbc70); (* 40 *)
  MD5_HH(a, b, c, d, x[13], MD5_S31, $289b7ec6); (* 41 *)
  MD5_HH(d, a, b, c, x[ 0], MD5_S32, $eaa127fa); (* 42 *)
  MD5_HH(c, d, a, b, x[ 3], MD5_S33, $d4ef3085); (* 43 *)
  MD5_HH(b, c, d, a, x[ 6], MD5_S34,  $4881d05); (* 44 *)
  MD5_HH(a, b, c, d, x[ 9], MD5_S31, $d9d4d039); (* 45 *)
  MD5_HH(d, a, b, c, x[12], MD5_S32, $e6db99e5); (* 46 *)
  MD5_HH(c, d, a, b, x[15], MD5_S33, $1fa27cf8); (* 47 *)
  MD5_HH(b, c, d, a, x[ 2], MD5_S34, $c4ac5665); (* 48 *)

  (* Round 4 *)
  MD5_II(a, b, c, d, x[ 0], MD5_S41, $f4292244); (* 49 *)
  MD5_II(d, a, b, c, x[ 7], MD5_S42, $432aff97); (* 50 *)
  MD5_II(c, d, a, b, x[14], MD5_S43, $ab9423a7); (* 51 *)
  MD5_II(b, c, d, a, x[ 5], MD5_S44, $fc93a039); (* 52 *)
  MD5_II(a, b, c, d, x[12], MD5_S41, $655b59c3); (* 53 *)
  MD5_II(d, a, b, c, x[ 3], MD5_S42, $8f0ccc92); (* 54 *)
  MD5_II(c, d, a, b, x[10], MD5_S43, $ffeff47d); (* 55 *)
  MD5_II(b, c, d, a, x[ 1], MD5_S44, $85845dd1); (* 56 *)
  MD5_II(a, b, c, d, x[ 8], MD5_S41, $6fa87e4f); (* 57 *)
  MD5_II(d, a, b, c, x[15], MD5_S42, $fe2ce6e0); (* 58 *)
  MD5_II(c, d, a, b, x[ 6], MD5_S43, $a3014314); (* 59 *)
  MD5_II(b, c, d, a, x[13], MD5_S44, $4e0811a1); (* 60 *)
  MD5_II(a, b, c, d, x[ 4], MD5_S41, $f7537e82); (* 61 *)
  MD5_II(d, a, b, c, x[11], MD5_S42, $bd3af235); (* 62 *)
  MD5_II(c, d, a, b, x[ 2], MD5_S43, $2ad7d2bb); (* 63 *)
  MD5_II(b, c, d, a, x[ 9], MD5_S44, $eb86d391); (* 64 *)

  Inc(PState(state)[0], a);
  Inc(PState(state)[1], b);
  inc(PState(state)[2], c);
  Inc(PState(state)[3], d);
end;


(* MD5 initialization. Begins an MD5 operation, writing a new context. *)
procedure MD5Init(context: PMD5Ctx);
begin
  context.count[0] := 0;
  context.count[1] := 0;
  (* Load magic initialization constants. *)
  context.state[0] := $67452301;
  context.state[1] := $efcdab89;
  context.state[2] := $98badcfe;
  context.state[3] := $10325476;
end;

(* MD5 block update operation. Continues an MD5 message-digest
  operation, processing another message block, and updating the
  context. *)

procedure MD5Update(context: PMD5Ctx; input: PByte; inputLen: Cardinal);
var
  i, index, partLen: Cardinal;
begin
  (* Compute number of bytes mod 64 *)
  index := Cardinal((context.count[0] shr 3) and $3F);

  (* Update number of bits *)
  Inc(context.count[0], (inputLen shl 3));
  if (context.count[0]  < (inputLen shl 3)) then
    Inc(context.count[1]);
  Inc(context.count[1], inputLen shr 29);

  partLen := 64 - index;

  (* Transform as many times as possible. *)
  if (inputLen >= partLen) then
  begin
    move(input^, context.buffer[index], partLen);
    MD5Transform (@context.state, @context.buffer);
    i := partLen;
    while i + 63 < inputLen do
    begin
      MD5Transform(@context.state, @input[i]);
      Inc(i, 64);
    end;
    index := 0;
  end else
    i := 0;

  (* Buffer remaining input *)
  Move(input[i], context.buffer[index], inputLen-i);
end;

(* Encodes input (Cardinal) into output (unsigned char). Assumes len is
  a multiple of 4. *)
procedure MD5_Encode(output: PByte; input: PCardinal; len: Cardinal);
var
  j: Cardinal;
  p: PCardinal;
begin
  p := input;
  j := 0;
  while j < len do
  begin
    output[j]   := Byte(p^ and $ff);
    output[j+1] := Byte((p^ shr 8) and $ff);
    output[j+2] := Byte((p^ shr 16) and $ff);
    output[j+3] := Byte((p^ shr 24) and $ff);
    Inc(p);
    Inc(j, 4);
  end;
end;

(* MD5 finalization. Ends an MD5 message-digest operation, writing the
  the message digest and zeroizing the context. *)

procedure MD5Final(digest: PByte; context: PMD5Ctx);
var
  bits: array[0..7] of Byte;
  index, padLen: Cardinal;
begin
  (* Save number of bits *)
  MD5_Encode(@bits, @context.count, 8);

  (* Pad out to 56 mod 64 *)
  index := (context.count[0] shr 3) and $3f;
  if (index < 56) then
    padLen := 56 - index else
    padLen := 120 - index;

  MD5Update(context, @MD5_PADDING, padLen);

  (* Append length (before padding) *)
  MD5Update(context, @bits, 8);

  (* Store state in digest *)
  MD5_Encode(digest, @context.state, 16);
end;

procedure MD5(input: PByte; len: Cardinal; output: PByte);
var
  context: TMD5Ctx;
begin
  FillChar(context, SizeOf(context), 0);
  MD5Init(@context);
  MD5Update(@context, input, len);
  MD5Final(output, @context);
end;

function MD5(const Value: RawByteString): RawByteString; overload;
begin
  SetLength(Result, 16);
  MD5(PByte(Value), Length(Value), PByte(Result));
end;

end.
