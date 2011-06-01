unit dorOpenSSL;

interface
uses Classes;

const
  LIBEAY = 'libeay32.dll';
  SSLEAY = 'ssleay32.dll';

(*******************************************************************************
 * opensslconf
 ******************************************************************************)

type
  IDEA_INT = Cardinal;
  MD2_INT = Cardinal;
  RC2_INT = Cardinal;
  RC4_INT = Cardinal;
  RC4_CHUNK = UInt64;
  DES_LONG = Cardinal;

(*******************************************************************************
 * stack
 ******************************************************************************)

type
  fn_stack_cmp = function(const p1, p2: PPAnsiChar): Integer; cdecl;
  fn_sk_pop_free = procedure(p: Pointer); cdecl;

  PSTACK = ^STACK;
  stack_st = record
   num: integer;
   data: PPAnsiChar;
   sorted: Integer;
   num_alloc: integer;
   comp: fn_stack_cmp;
  end;
  STACK = stack_st;

//#define M_sk_num(sk)  ((sk) ? (sk)->num:-1)
//#define M_sk_value(sk,n) ((sk) ? (sk)->data[n] : NULL)

function sk_num(const PSTACK ): Integer; cdecl; external LIBEAY;
function sk_value(const p: PSTACK; i: Integer): PAnsiChar; cdecl; external LIBEAY;
function sk_set(p: PSTACK; i: integer; c: PAnsiChar): PAnsiChar; cdecl; external LIBEAY;
function sk_new(cmp: fn_stack_cmp): PSTACK; cdecl; external LIBEAY;
function sk_new_null(): PSTACK; cdecl; external LIBEAY;
procedure sk_free(p: PSTACK); cdecl; external LIBEAY;
procedure sk_pop_free(st: PSTACK; func: fn_sk_pop_free); cdecl; external LIBEAY;
function sk_insert(sk: PSTACK; data: PAnsiChar; where: Integer): Integer; cdecl; external LIBEAY;
function sk_delete(st: PSTACK; loc: Integer): PAnsiChar; cdecl; external LIBEAY;
function sk_delete_ptr(st: PSTACK; p: PAnsiChar): PAnsiChar; cdecl; external LIBEAY;
function sk_find(st: PSTACK; data: PAnsiChar): Integer; cdecl; external LIBEAY;
function sk_find_ex(st: PSTACK; data: PAnsiChar): Integer; cdecl; external LIBEAY;
function sk_push(st: PSTACK; data: PAnsiChar): Integer; cdecl; external LIBEAY;
function sk_unshift(st: PSTACK; data: PAnsiChar): Integer; cdecl; external LIBEAY;
function sk_shift(st: PSTACK): PAnsiChar; cdecl; external LIBEAY;
function sk_pop(st: PSTACK): PAnsiChar; cdecl; external LIBEAY;
procedure sk_zero(st: PSTACK); cdecl; external LIBEAY;
// TODO:
//int ( *sk_set_cmp_func (sk: PSTACK; c: fn_stack_cmp))
//   (const char * const *, const char * const *);
function sk_dup(st: PSTACK): PSTACK; cdecl; external LIBEAY;
procedure sk_sort(st: PSTACK); cdecl; external LIBEAY;
function sk_is_sorted(const st: PSTACK): Integer; cdecl; external LIBEAY;


(*******************************************************************************
 * aes
 ******************************************************************************)

const
  _AES_ENCRYPT = 1;
  _AES_DECRYPT = 0;

(* Because array size can't be a const in C, the following two are macros.
   Both sizes are in bytes. *)
  AES_MAXNR = 14;
  AES_BLOCK_SIZE = 16;


(* This should be a hidden type, but EVP requires that the size be known *)
type
  PAES_KEY = ^AES_KEY;
  aes_key_st = record
    rd_key: array[0..(4 *(AES_MAXNR + 1)) - 1] of LongWord;
    rounds: Integer;
  end;
  AES_KEY = aes_key_st;

function AES_options(): PAnsiChar; cdecl; external LIBEAY;

function AES_set_encrypt_key(const userKey: PAnsiChar; const bits: Integer; key: PAES_KEY): Integer; cdecl; external LIBEAY;
function AES_set_decrypt_key(const userKey: PAnsiChar; const bits: Integer; key: PAES_KEY): Integer; cdecl; external LIBEAY;

procedure AES_encrypt(const in_: PAnsiChar; out_: PAnsiChar; const key: PAES_KEY); cdecl; external LIBEAY;
procedure AES_decrypt(const in_: PAnsiChar; out_: PAnsiChar; const key: PAES_KEY); cdecl; external LIBEAY;

procedure AES_ecb_encrypt(const in_: PAnsiChar; out_: PAnsiChar; const key: PAES_KEY; const enc: Integer); cdecl; external LIBEAY;
procedure AES_cbc_encrypt(const in_: PAnsiChar; out_: PAnsiChar; const length: LongWord; const key: PAES_KEY; ivec: PAnsiChar; const enc: Integer); cdecl; external LIBEAY;
procedure AES_cfb128_encrypt(const in_: PAnsiChar; out_: PAnsiChar; const length: LongWord; const key: PAES_KEY; ivec: PAnsiChar; num: PInteger; const enc: Integer); cdecl; external LIBEAY;
procedure AES_cfb1_encrypt(const in_: PAnsiChar; out_: PAnsiChar; const length: LongWord; const key: PAES_KEY; ivec: PAnsiChar; num: PInteger; const enc: Integer); cdecl; external LIBEAY;
procedure AES_cfb8_encrypt(const in_: PAnsiChar; out_: PAnsiChar; const length: LongWord; const key: PAES_KEY; ivec: PAnsiChar; num: PInteger; const enc: Integer); cdecl; external LIBEAY;
procedure AES_cfbr_encrypt_block(const in_: PAnsiChar;out_: PAnsiChar; const nbits: Integer;const key: PAES_KEY; ivec: PAnsiChar;const enc: Integer); cdecl; external LIBEAY;
procedure AES_ofb128_encrypt(const in_: PAnsiChar; out_: PAnsiChar; const length: LongWord; const key: PAES_KEY; ivec: PAnsiChar; num: PInteger); cdecl; external LIBEAY;

type
  TAesBlock = array[0..AES_BLOCK_SIZE-1] of AnsiChar;
procedure AES_ctr128_encrypt(const in_: PAnsiChar; out_: PAnsiChar; const length: LongWord; const key: PAES_KEY; ivec, ecount_buf: TAesBlock; num: PCardinal); cdecl; external LIBEAY;

(* For IGE, see also http://www.links.org/files/openssl-ige.pdf *)
(* NB: the IV is _two_ blocks long *)
procedure AES_ige_encrypt(const in_: PAnsiChar; out_: PAnsiChar; const length: LongWord; const key: PAES_KEY; ivec: PAnsiChar; const enc: Integer); cdecl; external LIBEAY;
(* NB: the IV is _four_ blocks long *)
procedure AES_bi_ige_encrypt(const in_: PAnsiChar; out_: PAnsiChar; const length: LongWord; const key: PAES_KEY; const key2: PAES_KEY; const ivec: PAnsiChar; const enc: Integer); cdecl; external LIBEAY;
function AES_wrap_key(key: PAES_KEY; const iv: PAnsiChar; out_: PAnsiChar; const in_: PAnsiChar; inlen: Cardinal): Integer; cdecl; external LIBEAY;
function AES_unwrap_key(key: PAES_KEY; const iv: PAnsiChar; out_: PAnsiChar; const in_: PAnsiChar; inlen: Cardinal): Integer; cdecl; external LIBEAY;

(*******************************************************************************
 * md5
 ******************************************************************************)

type
  MD5_LONG = Cardinal;

const
  MD5_CBLOCK	= 64;
  MD5_LBLOCK  = MD5_CBLOCK div 4;
  MD5_DIGEST_LENGTH = 16;

type
  MD5state_st = record
    A, B, C, D: MD5_LONG;
    Nl, Nh: MD5_LONG;
    data: array[0..MD5_LBLOCK-1] of MD5_LONG;
    num : Cardinal;
	end;
  MD5_CTX = MD5state_st;
  PMD5_CTX = ^MD5_CTX;

{$ifdef OPENSSL_FIPS}
int private_MD5_Init(MD5_CTX *c);
{$endif}
function MD5_Init(c: PMD5_CTX): Integer; cdecl; external LIBEAY;
function MD5_Update(c: PMD5_CTX; const data: Pointer; len: Cardinal): Integer; cdecl; external LIBEAY;
function MD5_Final(md: PByte; c: PMD5_CTX): Integer; cdecl; external LIBEAY;
function MD5(const d: PByte; n: Cardinal; md: PByte): PByte; cdecl; external LIBEAY;
procedure MD5_Transform(c: PMD5_CTX; b: PByte); cdecl; external LIBEAY;

procedure AesEncryptStream(InStream, OutStream: TStream; const pass: PAnsiChar; bits: Integer);
procedure AesDecryptStream(InStream, OutStream: TStream; const pass: PAnsiChar; bits: Integer);


(*******************************************************************************
 * SSL
 ******************************************************************************)

const
  X509_FILETYPE_PEM	    = 1;
  X509_FILETYPE_ASN1    = 2;
  X509_FILETYPE_DEFAULT = 3;

(* use either SSL_VERIFY_NONE or SSL_VERIFY_PEER, the last 2 options
 * are 'ored' with SSL_VERIFY_PEER if they are desired *)
  SSL_VERIFY_NONE                 = $00;
  SSL_VERIFY_PEER                 = $01;
  SSL_VERIFY_FAIL_IF_NO_PEER_CERT = $02;
  SSL_VERIFY_CLIENT_ONCE          = $04;

  SSL_FILETYPE_ASN1 = X509_FILETYPE_ASN1;
  SSL_FILETYPE_PEM  = X509_FILETYPE_PEM;

type
  PSSL = Pointer;
  PSSL_CTX = Pointer;
  PSSL_METHOD = Pointer;
  PX509 = Pointer;
  PX509_NAME = Pointer;

  function SSL_library_init: Integer; cdecl; external SSLEAY;
  function SSL_CTX_set_cipher_list(arg0: PSSL_CTX; str: PAnsiChar): Integer; cdecl; external SSLEAY;
  function SSL_CTX_new(meth: PSSL_METHOD): PSSL_CTX; cdecl; external SSLEAY;
  procedure SSL_CTX_free(arg0: PSSL_CTX); cdecl; external SSLEAY;
  function SSL_set_fd(s: PSSL; fd: Longint): Integer; cdecl; external SSLEAY;
  function SSLv2_method: PSSL_METHOD; cdecl; external SSLEAY;
  function SSLv3_method: PSSL_METHOD; cdecl; external SSLEAY;
  function TLSv1_method: PSSL_METHOD; cdecl; external SSLEAY;
  function SSLv23_method: PSSL_METHOD; cdecl; external SSLEAY;
  function SSL_CTX_use_RSAPrivateKey_file(ctx: PSSL_CTX; const filename: PAnsiChar; typ: Integer):Integer; cdecl; external SSLEAY;
  function SSL_CTX_use_certificate_file(ctx: PSSL_CTX; const filename: PAnsiChar; typ: Integer):Integer; cdecl; external SSLEAY;
  function SSL_CTX_use_certificate_chain_file(ctx: PSSL_CTX; const _file: PAnsiChar):Integer; cdecl; external SSLEAY;
  procedure SSL_CTX_set_default_passwd_cb(ctx: PSSL_CTX; cb: Pointer); cdecl; external SSLEAY;
  procedure SSL_CTX_set_default_passwd_cb_userdata(ctx: PSSL_CTX; u: Pointer); cdecl; external SSLEAY;
  function SSL_CTX_load_verify_locations(ctx: PSSL_CTX; const CAfile: PAnsiChar; const CApath: PAnsiChar):Integer; cdecl; external SSLEAY;
  function SSL_new(ctx: PSSL_CTX):PSSL; cdecl; external SSLEAY;
  procedure SSL_free(ssl: PSSL); cdecl; external SSLEAY;
  function SSL_connect(ssl: PSSL):Integer; cdecl; external SSLEAY;
  function SSL_accept(ssl: PSSL):Integer; cdecl; external SSLEAY;
  function SSL_shutdown(ssl: PSSL):Integer; cdecl; external SSLEAY;
  function SSL_read(ssl: PSSL; buf: Pointer; num: Integer): Integer; cdecl; external SSLEAY;
  function SSL_write(ssl: PSSL; const buf: Pointer; num: Integer): Integer; cdecl; external SSLEAY;
  procedure SSL_CTX_set_verify(ctx: PSSL_CTX; mode: Integer; arg2: Pointer); cdecl; external SSLEAY;
  procedure OPENSSL_add_all_algorithms_noconf; cdecl; external LIBEAY;

  function SSL_get_peer_certificate(ssl: PSSL): PX509; cdecl; external SSLEAY;
  procedure X509_free(x509: PX509); cdecl; external LIBEAY;
  function X509_NAME_oneline(name: PX509_NAME; buf: PAnsiChar; size: Integer): PAnsiChar; cdecl; external LIBEAY;
  function X509_get_subject_name(x509: PX509): PX509_NAME; cdecl; external LIBEAY;
  function X509_get_issuer_name(x509: PX509): PX509_NAME; cdecl; external LIBEAY;

type
  TOnX509KeyValue = reference to function(const key, value: AnsiString): Boolean;

  function X509NameOneline(const name: PX509_NAME): AnsiString;
  function X509NameParse(const name: PX509_NAME; const onkey: TOnX509KeyValue): Boolean;
  function X509NameFind(const name: PX509_NAME; const key: AnsiString): AnsiString;


const
  SHA_LBLOCK = 16;
  SHA_CBLOCK = SHA_LBLOCK * 4; (* SHA treats input data as a
                                * contiguous array of 32 bit
                                * wide big-endian values. *)
  SHA_LAST_BLOCK = SHA_CBLOCK - 8;
  SHA_DIGEST_LENGTH = 20;

type
  SHA_LONG = Cardinal;
  PSHA_CTX = ^SHA_CTX;
  SHA_CTX = record
    h0,h1,h2,h3,h4: SHA_LONG;
    Nl,Nh: SHA_LONG;
    data: array[0..SHA_LBLOCK-1] of SHA_LONG;
    num: Cardinal;
	end;

function SHA1_Init(c: PSHA_CTX): Integer; cdecl; external LIBEAY;
function SHA1_Update(c: PSHA_CTX; const data: Pointer; len: Cardinal): Integer; cdecl; external LIBEAY;
function SHA1_Final(md: PAnsiChar; c: PSHA_CTX): Integer; cdecl; external LIBEAY;
function SHA1(const d: PAnsiChar; n: Cardinal; md: PAnsiChar): PAnsiChar; cdecl; external LIBEAY;
procedure SHA1_Transform(c: PSHA_CTX; const data: PAnsiChar); cdecl; external LIBEAY;

implementation

procedure AesEncryptStream(InStream, OutStream: TStream; const pass: PAnsiChar; bits: Integer);
var
  inbuffer, outbuffer: TAesBlock;
  key: AES_KEY;
  len: Integer;
begin
  instream.Seek(0, soFromBeginning);
  AES_set_encrypt_key(pass, bits, @key);
  inbuffer[0] := AnsiChar(sizeof(TAesBlock) - ((InStream.Size+1)  mod sizeof(TAesBlock)));
  len := InStream.Read(inbuffer[1], sizeof(TAesBlock)-1) + 1;
  while len > 0 do
  begin
    AES_encrypt(@inbuffer, @outbuffer, @key);
    OutStream.Write(outbuffer, SizeOf(TAesBlock));
    len := InStream.Read(inbuffer, sizeof(TAesBlock));
  end;
end;

procedure AesDecryptStream(InStream, OutStream: TStream; const pass: PAnsiChar; bits: Integer);
var
  inbuffer, outbuffer: TAesBlock;
  key: AES_KEY;
  m: Byte;
  i, c: Integer;
begin
  if InStream.Size mod SizeOf(TAesBlock) <> 0 then Exit;

  InStream.Seek(0, soFromBeginning);

  c := InStream.Size div sizeof(TAesBlock);
  AES_set_decrypt_key(pass, bits, @key);

  InStream.Read(inbuffer, sizeof(TAesBlock));
  AES_decrypt(@inbuffer, @outbuffer, @key);
  m := Byte(outbuffer[0]);
  if c <> 1 then
    OutStream.Write(outbuffer[1], sizeof(TAesBlock)-1) else
    begin
      OutStream.Write(outbuffer[1], sizeof(TAesBlock)-1-m);
      Exit;
    end;

  for i := 2 to c-1 do
  begin
    InStream.Read(inbuffer, sizeof(TAesBlock));
    AES_decrypt(@inbuffer, @outbuffer, @key);
    OutStream.Write(outbuffer, sizeof(TAesBlock));
  end;

  InStream.Read(inbuffer, sizeof(TAesBlock));
  AES_decrypt(@inbuffer, @outbuffer, @key);
  if SizeOf(TAesBlock) <> m then
    OutStream.Write(outbuffer, sizeof(TAesBlock)-m) else
    OutStream.Write(outbuffer, sizeof(TAesBlock));
end;

function X509NameOneline(const name: PX509_NAME): AnsiString;
var
  buff: array[0..1024] of AnsiChar;
begin
  if name <> nil then
    Result := X509_NAME_oneline(name, @buff, SizeOf(buff)) else
    Result := '';
end;

function X509NameParse(const name: PX509_NAME; const onkey: TOnX509KeyValue): Boolean;
type
  TState = (stStart, stkey, stValue);
var
  buff: array[0..1024] of AnsiChar;
  p: PAnsiChar;
  st: TState;
  key, value: AnsiString;
begin
  if (name <> nil) and Assigned(onkey) then
  begin
    Result := True;
    p := X509_NAME_oneline(name, @buff, SizeOf(buff));
    if p <> nil then
    begin
      st := stStart;
      while True do
      begin
        case st of
          stStart:
            if (p^ = '/') then
            begin
              key := '';
              st := stkey;
            end else
              Exit(False);
          stkey:
            case p^ of
              '=':
                begin
                  value := '';
                  st := stValue;
                end;
              #0: Exit(False);
            else
              key := key + p^;
            end;
          stValue:
            case p^ of
              '/':
                begin
                  if not onkey(key, value) then
                    Exit(True);
                  key := '';
                  st := stkey;
                end;
               #0:
                 begin
                   onkey(key, value);
                   Exit(True);
                 end;
            else
              value := value + p^;
            end;
        end;
        Inc(p);
      end;
    end;
  end else
    Result := False;
end;

function X509NameFind(const name: PX509_NAME; const key: AnsiString): AnsiString;
var
  return: AnsiString;
begin
  return := '';
  X509NameParse(name,
    function(const k, v: AnsiString): Boolean
    begin
      if k = key then
      begin
        Return := v;
        Result := False;
      end else
        Result := True;
    end);
  Result := return;
end;

initialization
  SSL_library_init;
  OPENSSL_add_all_algorithms_noconf;

end.
