unit dorOpenSSL;

{$IFNDEF CPUX64}
  {$ALIGN ON}
{$ENDIF}

interface

uses Classes;

const
  LIBEAY = 'libeay32.dll';
  SSLEAY = 'ssleay32.dll';

type
  TNotImplemented = record end;
  PNotImplemented = ^TNotImplemented;

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



(******************************************************************************
 * EVP
(******************************************************************************)

const
  EVP_MAX_MD_SIZE = 64;	(* longest known is SHA512 *)
  EVP_MAX_KEY_LENGTH = 32;
  EVP_MAX_IV_LENGTH = 16;
  EVP_MAX_BLOCK_LENGTH = 32;

  PKCS5_SALT_LEN = 8;
(* Default PKCS#5 iteration count *)
  PKCS5_DEFAULT_ITER = 2048;

  EVP_PK_RSA = $0001;
  EVP_PK_DSA = $0002;
  EVP_PK_D = $0004;
  EVP_PK_E = $0008;
  EVP_PKT_SIGN = $0010;
  EVP_PKT_EN = $0020;
  EVP_PKT_EXCH = $0040;
  EVP_PKS_RS = $0100;
  EVP_PKS_DS = $0200;
  EVP_PKS_EC = $0400;
  EVP_PKT_EXP = $1000; (* <= 512 bit key *)

type
  PEVP_CIPHER_CTX = ^EVP_CIPHER_CTX;
  PEVP_CIPHER = ^EVP_CIPHER;
  PEVP_MD = ^EVP_MD;
  PEVP_MD_CTX = ^EVP_MD_CTX;
  PEVP_PKEY_CTX = PNotImplemented;

  EVP_CIPHER = record
    nid: Integer;
    block_size: Integer;
    key_len: Integer;		(* Default value for variable length ciphers *)
    iv_len: Integer;
    flags: LongWord;	(* Various flags *)
    init: function(ctx: PEVP_CIPHER_CTX; const key, iv: PAnsiChar; enc: Integer): Integer; cdecl; (* init key *)
    do_cipher: function(ctx: PEVP_CIPHER_CTX; out_: PAnsiChar; const in_: PAnsiChar; inl: Cardinal): Integer; cdecl; (* encrypt/decrypt data *)
    cleanup: function(ctx: PEVP_CIPHER_CTX): Integer; (* cleanup ctx *)
    ctx_size: Integer;		(* how big ctx->cipher_data needs to be *)
    set_asn1_parameters: function(ctx: PEVP_CIPHER_CTX; ASN1: PNotImplemented): Integer; cdecl; (* Populate a ASN1_TYPE with parameters *)
    get_asn1_parameters: function(ctx: PEVP_CIPHER_CTX; ASN1: PNotImplemented): Integer; cdecl; (* Get parameters from a ASN1_TYPE *)
    ctrl: function(ctx: PEVP_CIPHER_CTX; type_, arg: Integer; ptr: Pointer): Integer; cdecl; (* Miscellaneous operations *)
    app_data: Pointer;		(* Application data *)
  end;

  EVP_CIPHER_CTX = record
    cipher: PEVP_CIPHER;
    engine: PNotImplemented;	(* functional reference if 'cipher' is ENGINE-provided *)
    encrypt: Integer;		(* encrypt or decrypt *)
    buf_len: Integer;		(* number we have left *)

    oiv: array[0..EVP_MAX_IV_LENGTH-1] of AnsiChar;	(* original iv *)
    iv: array[0..EVP_MAX_IV_LENGTH-1] of AnsiChar; (* working iv *)
    buf: array[0..EVP_MAX_BLOCK_LENGTH-1] of AnsiChar; (* saved partial block *)
    num: Integer; (* used by cfb/ofb mode *)

    app_data: Pointer; (* application stuff *)
    key_len: Integer;	 (* May change for variable length cipher *)
    flags: LongWord;	(* Various flags *)
    cipher_data: Pointer; (* per EVP data *)
    final_used: Integer;
    block_mask: Integer;
    final: array[0..EVP_MAX_BLOCK_LENGTH-1] of AnsiChar; (* possible final block *)
	end;

  EVP_MD = record
    type_: Integer;
    pkey_type: Integer;
    md_size: Integer;
    flags: LongWord;
    init: function(ctx: PEVP_MD_CTX): Integer; cdecl;
    update: function(ctx: PEVP_MD_CTX; const data: Pointer; count: Cardinal): Integer; cdecl;
    final_: function(ctx: PEVP_MD_CTX; md: PAnsiChar): Integer; cdecl;
    copy: function(to_: PEVP_MD_CTX; const from: PEVP_MD_CTX): Integer; cdecl;
    cleanup: function(ctx: PEVP_MD_CTX): Integer; cdecl;

    (* FIXME: prototype these some day *)
    sign: function(type_: Integer; const m: PAnsiChar; m_length: Cardinal;
      sigret: PAnsiChar; siglen: PCardinal; key: Pointer): Integer; cdecl;
    verify: function(type_: Integer; const m: PAnsiChar; m_length: Cardinal;
      const sigbuf: PAnsiChar; siglen: Cardinal; key: Pointer): Integer; cdecl;
    required_pkey_type: array[0..4] of Integer; (*EVP_PKEY_xxx *)
    block_size: Integer;
    ctx_size: Integer; (* how big does the ctx->md_data need to be *)
    (* control function *)
    md_ctrl: function(ctx: PEVP_MD_CTX; cmd, p1: Integer; p2: Pointer): Integer; cdecl;
  end;

  EVP_MD_CTX = record
    digest: PEVP_MD;
    engine: PNotImplemented; (* functional reference if 'digest' is ENGINE-provided *)
    flags: LongWord;
    md_data: Pointer;
    (* Public key context for sign/verify *)
    pctx: PEVP_PKEY_CTX;
    (* Update function: usually copied from EVP_MD *)
    update: function(ctx: PEVP_MD_CTX; const data: Pointer; count: Cardinal): Integer; cdecl;
  end;


function EVP_CipherInit_ex(ctx: PEVP_CIPHER_CTX; const cipher: PEVP_CIPHER;
  impl: PNotImplemented; const key, iv: PByte; enc: Integer): Integer; cdecl; external LIBEAY;

function EVP_CipherUpdate(ctx: PEVP_CIPHER_CTX; out_: Pointer;
  outl: PInteger; const in_: Pointer; inl: Integer): Integer; cdecl; external LIBEAY;

function EVP_CipherFinal_ex(ctx: PEVP_CIPHER_CTX; outm: Pointer; outl: PInteger): Integer; cdecl; external LIBEAY;

procedure EVP_CIPHER_CTX_init(a: PEVP_CIPHER_CTX); cdecl; external LIBEAY;
procedure EVP_CIPHER_CTX_free(a: PEVP_CIPHER_CTX); cdecl; external LIBEAY;
function EVP_CIPHER_CTX_cipher(const ctx: PEVP_CIPHER_CTX): PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_CIPHER_CTX_block_size(const ctx: PEVP_CIPHER_CTX): Integer; cdecl; external LIBEAY;
function EVP_CIPHER_CTX_copy(out_: PEVP_CIPHER_CTX; const in_: PEVP_CIPHER_CTX): Integer; cdecl; external LIBEAY;
function EVP_CIPHER_CTX_cleanup(ctx: PEVP_CIPHER_CTX): Integer; cdecl; external LIBEAY;
function EVP_CIPHER_CTX_key_length(const ctx: PEVP_CIPHER_CTX): Integer; cdecl; external LIBEAY;
function EVP_CIPHER_CTX_iv_length(const ctx: PEVP_CIPHER_CTX): Integer; cdecl; external LIBEAY;
function EVP_CIPHER_CTX_set_key_length(x: PEVP_CIPHER_CTX; keylen: Integer): Integer; cdecl; external LIBEAY;
function EVP_CIPHER_CTX_set_padding(c: PEVP_CIPHER_CTX; pad: Integer): Integer; cdecl; external LIBEAY;

function EVP_CIPHER_nid(const cipher: PEVP_CIPHER): Integer; cdecl; external LIBEAY;
function EVP_CIPHER_iv_length(const cipher: PEVP_CIPHER): Integer; cdecl; external LIBEAY;
function EVP_CIPHER_block_size(const cipher: PEVP_CIPHER): Integer; cdecl; external LIBEAY;

function EVP_CIPHER_name(e: PEVP_CIPHER): PAnsiChar; inline;

function EVP_MD_CTX_md(const ctx: PEVP_MD_CTX): PEVP_MD; cdecl; external LIBEAY;
function EVP_MD_CTX_create: PEVP_MD_CTX; cdecl; external LIBEAY;
function EVP_MD_CTX_copy(out_: PEVP_MD_CTX; const in_: PEVP_MD_CTX): Integer; cdecl; external LIBEAY;

function EVP_MD_CTX_size(e: PEVP_MD_CTX): Integer; inline;
function EVP_MD_CTX_block_size(e: PEVP_MD_CTX): Integer; inline;
function EVP_MD_CTX_type(e: PEVP_MD_CTX): Integer; inline;

function EVP_MD_type(const md: PEVP_MD): Integer; cdecl; external LIBEAY;
function EVP_MD_size(const md: PEVP_MD): Integer; cdecl; external LIBEAY;
function EVP_MD_block_size(const md: PEVP_MD): Integer; cdecl; external LIBEAY;
function EVP_MD_flags(const md: PEVP_MD): LongWord; cdecl; external LIBEAY;

function EVP_MD_name(e: PEVP_MD): PAnsiChar; inline;
function EVP_MD_nid(e: PEVP_MD): Integer; inline;

function EVP_DigestInit_ex(ctx: PEVP_MD_CTX; const type_: PEVP_MD; impl: PNotImplemented): Integer; cdecl; external LIBEAY;
function EVP_DigestUpdate(ctx: PEVP_MD_CTX; const d: Pointer; cnt: Cardinal): Integer; cdecl; external LIBEAY;
function EVP_DigestFinal_ex(ctx: PEVP_MD_CTX; md: PAnsiChar; s: PCardinal): Integer; cdecl; external LIBEAY;

function EVP_get_cipherbyname(const name: PAnsiChar): PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_get_digestbyname(const name: PAnsiChar): PEVP_MD; cdecl; external LIBEAY;

function EVP_BytesToKey(const type_: PEVP_CIPHER; const md: PEVP_MD;
  const salt, data: Pointer; datal, count: Integer; key, iv: PByte): Integer; cdecl; external LIBEAY;

function EVP_md_null: PEVP_MD; cdecl; external LIBEAY;
function EVP_md2: PEVP_MD; cdecl; external LIBEAY;
function EVP_md4: PEVP_MD; cdecl; external LIBEAY;
function EVP_md5: PEVP_MD; cdecl; external LIBEAY;

function EVP_sha: PEVP_MD; cdecl; external LIBEAY;
function EVP_sha1: PEVP_MD; cdecl; external LIBEAY;

function EVP_dss: PEVP_MD; cdecl; external LIBEAY;
function EVP_dss1: PEVP_MD; cdecl; external LIBEAY;

function EVP_ecdsa: PEVP_MD; cdecl; external LIBEAY;

function EVP_sha224: PEVP_MD; cdecl; external LIBEAY;
function EVP_sha256: PEVP_MD; cdecl; external LIBEAY;
function EVP_sha384: PEVP_MD; cdecl; external LIBEAY;
function EVP_sha512: PEVP_MD; cdecl; external LIBEAY;

function EVP_mdc2: PEVP_MD; cdecl; external LIBEAY;

function EVP_ripemd160: PEVP_MD; cdecl; external LIBEAY;

function EVP_whirlpool: PEVP_MD; cdecl; external LIBEAY;

function EVP_enc_null: PEVP_CIPHER; cdecl; external LIBEAY;		{ does nothing :-) }

function EVP_des_ecb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_des_ede: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_des_ede3: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_des_ede_ecb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_des_ede3_ecb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_des_cfb64: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_des_cfb1: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_des_cfb8: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_des_ede_cfb64: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_des_ede_cfb1: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_des_ede_cfb8: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_des_ede3_cfb64: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_des_ede3_cfb1: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_des_ede3_cfb8: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_des_ofb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_des_ede_ofb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_des_ede3_ofb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_des_cbc: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_des_ede_cbc: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_des_ede3_cbc: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_desx_cbc: PEVP_CIPHER; cdecl; external LIBEAY;

{* This should now be supported through the dev_crypto ENGINE. But also, why are
 * rc4 and md5 declarations made here inside a "NO_DES" precompiler branch? *}
function EVP_dev_crypto_des_ede3_cbc: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_dev_crypto_rc4: PEVP_CIPHER; cdecl; external LIBEAY;

function EVP_dev_crypto_md5: PEVP_MD; cdecl; external LIBEAY;

function EVP_rc4: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_rc4_40: PEVP_CIPHER; cdecl; external LIBEAY;

function EVP_idea_ecb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_idea_cfb64: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_idea_ofb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_idea_cbc: PEVP_CIPHER; cdecl; external LIBEAY;

function EVP_rc2_ecb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_rc2_cbc: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_rc2_40_cbc: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_rc2_64_cbc: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_rc2_cfb64: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_rc2_ofb: PEVP_CIPHER; cdecl; external LIBEAY;

function EVP_bf_ecb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_bf_cbc: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_bf_cfb64: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_bf_ofb: PEVP_CIPHER; cdecl; external LIBEAY;

function EVP_cast5_ecb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_cast5_cbc: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_cast5_cfb64: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_cast5_ofb: PEVP_CIPHER; cdecl; external LIBEAY;

function EVP_rc5_32_12_16_cbc: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_rc5_32_12_16_ecb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_rc5_32_12_16_cfb64: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_rc5_32_12_16_ofb: PEVP_CIPHER; cdecl; external LIBEAY;

function EVP_aes_128_ecb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_aes_128_cbc: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_aes_128_cfb1: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_aes_128_cfb8: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_aes_128_cfb128: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_aes_128_ofb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_aes_128_ctr: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_aes_192_ecb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_aes_192_cbc: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_aes_192_cfb1: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_aes_192_cfb8: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_aes_192_cfb128: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_aes_192_ofb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_aes_192_ctr: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_aes_256_ecb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_aes_256_cbc: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_aes_256_cfb1: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_aes_256_cfb8: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_aes_256_cfb128: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_aes_256_ofb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_aes_256_ctr: PEVP_CIPHER; cdecl; external LIBEAY;

function EVP_camellia_128_ecb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_camellia_128_cbc: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_camellia_128_cfb1: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_camellia_128_cfb8: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_camellia_128_cfb128: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_camellia_128_ofb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_camellia_192_ecb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_camellia_192_cbc: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_camellia_192_cfb1: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_camellia_192_cfb8: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_camellia_192_cfb128: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_camellia_192_ofb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_camellia_256_ecb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_camellia_256_cbc: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_camellia_256_cfb1: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_camellia_256_cfb8: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_camellia_256_cfb128: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_camellia_256_ofb: PEVP_CIPHER; cdecl; external LIBEAY;

function EVP_seed_ecb: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_seed_cbc: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_seed_cfb128: PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_seed_ofb: PEVP_CIPHER; cdecl; external LIBEAY;

function EVP_add_cipher(const cipher: PEVP_CIPHER): Integer; cdecl; external LIBEAY;
function EVP_add_digest(const digest: PEVP_MD): Integer; cdecl; external LIBEAY;

procedure EVP_cleanup; cdecl; external LIBEAY;

// Crypto

procedure OPENSSL_add_all_algorithms_noconf; cdecl; external LIBEAY;
procedure OPENSSL_add_all_algorithms_conf; cdecl; external LIBEAY;
procedure OPENSSL_add_all_ciphers; cdecl; external LIBEAY;
procedure OPENSSL_add_all_digests; cdecl; external LIBEAY;
procedure OPENSSL_cleanse(ptr: Pointer; len: Cardinal); cdecl; external LIBEAY;

// Objects

function OBJ_nid2sn(n: Integer): PAnsiChar; cdecl; external LIBEAY;

(******************************************************************************
 * BIO
(******************************************************************************)

const
{ These are used in the following macros and are passed to BIO_ctrl() }

  BIO_CTRL_RESET = 1;  { opt - rewind/zero etc }
  BIO_CTRL_EOF = 2;  { opt - are we at the eof }
  BIO_CTRL_INFO	= 3;  { opt - extra tit-bits }
  BIO_CTRL_SET = 4;  { man - set the 'IO' type }
  BIO_CTRL_GET = 5;  { man - get the 'IO' type }
  BIO_CTRL_PUSH	= 6;  { opt - internal, used to signify change }
  BIO_CTRL_POP = 7;  { opt - internal, used to signify change }
  BIO_CTRL_GET_CLOSE = 8;  { man - set the 'close' on free }
  BIO_CTRL_SET_CLOSE = 9;  { man - set the 'close' on free }
  BIO_CTRL_PENDING = 10;  { opt - is their more data buffered }
  BIO_CTRL_FLUSH = 11;  { opt - 'flush' buffered output }
  BIO_CTRL_DUP = 12;  { man - extra stuff for 'duped' BIO }
  BIO_CTRL_WPENDING	= 13;  { opt - number of bytes still to write }

  BIO_C_SET_CONNECT = 100;
  BIO_C_DO_STATE_MACHINE = 101;
  BIO_C_SET_NBIO = 102;
  BIO_C_SET_PROXY_PARAM = 103;
  BIO_C_SET_FD = 104;
  BIO_C_GET_FD = 105;
  BIO_C_SET_FILE_PTR = 106;
  BIO_C_GET_FILE_PTR = 107;
  BIO_C_SET_FILENAME = 108;
  BIO_C_SET_SSL	= 109;
  BIO_C_GET_SSL = 110;
  BIO_C_SET_MD = 111;
  BIO_C_GET_MD = 112;
  BIO_C_GET_CIPHER_STATUS	= 113;
  BIO_C_SET_BUF_MEM	= 114;
  BIO_C_GET_BUF_MEM_PTR	= 115;
  BIO_C_GET_BUFF_NUM_LINES = 116;
  BIO_C_SET_BUFF_SIZE	= 117;
  BIO_C_SET_ACCEPT = 118;
  BIO_C_SSL_MODE = 119;
  BIO_C_GET_MD_CTX = 120;
  BIO_C_GET_PROXY_PARAM = 121;
  BIO_C_SET_BUFF_READ_DATA = 122; { data to read first }
  BIO_C_GET_CONNECT = 123;
  BIO_C_GET_ACCEPT = 124;
  BIO_C_SET_SSL_RENEGOTIATE_BYTES = 125;
  BIO_C_GET_SSL_NUM_RENEGOTIATES = 126;
  BIO_C_SET_SSL_RENEGOTIATE_TIMEOUT	= 127;
  BIO_C_FILE_SEEK = 128;
  BIO_C_GET_CIPHER_CTX = 129;
  BIO_C_SET_BUF_MEM_EOF_RETURN = 130; { return end of input value }
  BIO_C_SET_BIND_MODE	= 131;
  BIO_C_GET_BIND_MODE	= 132;
  BIO_C_FILE_TELL	= 133;
  BIO_C_GET_SOCKS	= 134;
  BIO_C_SET_SOCKS	= 135;

  BIO_FLAGS_BASE64_NO_NL = $100;
  BIO_FLAGS_MEM_RDONLY = $200;

type
  CRYPTO_EX_DATA = record
  end;

  PBIO_METHOD = ^BIO_METHOD;

  PBIO = ^BIO;
  BIO = record
	  method: PBIO_METHOD;
    callback: function(bio: PBIO; mode: Integer; argp: PAnsiChar; argi: Integer; argl: LongInt; ret: LongInt): LongInt;
    cb_arg: PAnsiChar;
    init: Integer;
    shutdown: Integer;
    flags: Integer;
    retry_reason: Integer;
    num: Integer;
    ptr: Pointer;
    next_bio: PBIO;
    prev_bio: PBIO;
    references: Integer;
    num_read: LongWord;
    num_write: LongWord;
    ex_data: CRYPTO_EX_DATA;
  end;

  bio_info_cb = procedure(bio: PBIO; i_param: Integer; const data: PAnsiChar; i2_param: Integer; l_param: LongInt; l2_param: LongInt);

  BIO_METHOD = record
	  type_: Integer;
	  name: PAnsiChar;
    bwrite: function(bio: PBIO; const data: PAnsiChar; len: Integer): Integer; cdecl;
    bread: function(bio: PBIO; data: PAnsiChar; len: Integer): Integer; cdecl;
    bputs: function(bio: PBIO; const data: PAnsiChar): Integer; cdecl;
    bgets: function(bio: PBIO; data: PAnsiChar; len: Integer): Integer; cdecl;
    ctrl: function(bio: PBIO; mode: Integer; l_param: LongInt; ptr: Pointer): LongInt; cdecl;
    create: function(bio: PBIO): Integer; cdecl;
    destroy: function(bio: PBIO): Integer; cdecl;
    callback_ctrl: function(bio: PBIO; i_param: Integer; bio_info_callback: bio_info_cb): LongInt; cdecl;
  end;

function BIO_new(type_: PBIO_METHOD): PBIO; cdecl; external LIBEAY;
procedure BIO_set_flags(b: PBIO; flags: Integer); cdecl; external LIBEAY;
function BIO_f_base64: PBIO_METHOD; cdecl; external LIBEAY;
function BIO_s_mem: PBIO_METHOD; cdecl; external LIBEAY;
function BIO_new_mem_buf(buf: Pointer; len: Integer): PBIO; cdecl; external LIBEAY;
function BIO_push(b: PBIO; append: PBIO): PBIO; cdecl; external LIBEAY;
function BIO_write(b: PBIO; const data: Pointer; len: Integer): Integer; cdecl; external LIBEAY;
function BIO_read(b: PBIO; data: Pointer; len: Integer): Integer; cdecl; external LIBEAY;
function BIO_ctrl(bp: PBIO; cmd: Integer; larg: Longint; parg: Pointer): LongInt; cdecl; external LIBEAY;
procedure	BIO_free_all(a: PBIO); cdecl; external LIBEAY;

function BIO_flush(b: PBIO): Integer; inline;
function BIO_get_mem_ptr(b: PBIO; ptr: Pointer): LongInt; inline;

(******************************************************************************
 * Buffer
(******************************************************************************)

type
  PBUF_MEM = ^BUF_MEM;
  BUF_MEM = record
    length: Cardinal;
    data: PAnsiChar;
    max: Cardinal;
  end;

(******************************************************************************
 * RAND
(******************************************************************************)

type
  PRAND_METHOD = ^RAND_METHOD;
  RAND_METHOD = record
    seed: procedure(const buf: Pointer; num: Integer); cdecl;
    bytes: function(buf: PByte; num: Integer): Integer; cdecl;
    cleanup: procedure; cdecl;
    add: procedure(const buf: Pointer; num: Integer; entropy: Double); cdecl;
    pseudorand: function(buf: PByte; num: Integer): Integer; cdecl;
    status: procedure; cdecl;
  end;

procedure RAND_cleanup; cdecl; external LIBEAY;

procedure RAND_seed(const buf: Pointer; num: Integer); cdecl; external LIBEAY;
procedure RAND_add(const buf: Pointer; num: Integer; entropy: Double); cdecl; external LIBEAY;

function RAND_status: Integer; cdecl; external LIBEAY;

function RAND_bytes(buf: PByte; num: Integer): Integer; cdecl; external LIBEAY;
function RAND_pseudo_bytes(buf: PByte; num: Integer): Integer; cdecl; external LIBEAY;

procedure RAND_set_rand_method(const meth: PRAND_METHOD); cdecl; external LIBEAY;
function RAND_get_rand_method: PRAND_METHOD; cdecl; external LIBEAY;
function RAND_SSLeay: PRAND_METHOD; cdecl; external LIBEAY;

(******************************************************************************
 * ERR
(******************************************************************************)

procedure ERR_load_crypto_strings; cdecl; external LIBEAY;
procedure ERR_free_strings; cdecl; external LIBEAY;

function ERR_get_error: LongWord; cdecl; external LIBEAY;

procedure ERR_clear_error; cdecl; external LIBEAY;

function ERR_error_string(e: LongWord; buf: PAnsiChar): PAnsiChar; cdecl; external LIBEAY;
procedure ERR_error_string_n(e: LongWord; buf: PAnsiChar; len: Cardinal); cdecl; external LIBEAY;
function ERR_lib_error_string(e: LongWord): PAnsiChar; cdecl; external LIBEAY;
function ERR_func_error_string(e: LongWord): PAnsiChar; cdecl; external LIBEAY;
function ERR_reason_error_string(e: LongWord): PAnsiChar; cdecl; external LIBEAY;

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

function EVP_CIPHER_name(e: PEVP_CIPHER): PAnsiChar;
begin
	Result := OBJ_nid2sn(EVP_CIPHER_nid(e));
end;

function EVP_MD_nid(e: PEVP_MD): Integer;
begin
  Result := EVP_MD_type(e);
end;

function EVP_MD_name(e: PEVP_MD): PAnsiChar;
begin
  Result := OBJ_nid2sn(EVP_MD_nid(e));
end;

function EVP_MD_CTX_size(e: PEVP_MD_CTX): Integer;
begin
  Result := EVP_MD_size(EVP_MD_CTX_md(e));
end;

function EVP_MD_CTX_block_size(e: PEVP_MD_CTX): Integer;
begin
  Result := EVP_MD_block_size(EVP_MD_CTX_md(e));
end;

function EVP_MD_CTX_type(e: PEVP_MD_CTX): Integer;
begin
  Result := EVP_MD_type(EVP_MD_CTX_md(e));
end;

function BIO_flush(b: PBIO): Integer;
begin
  Result := Integer(BIO_ctrl(b, BIO_CTRL_FLUSH, 0, nil));
end;

function BIO_get_mem_ptr(b: PBIO; ptr: Pointer): LongInt;
begin
  Result := BIO_ctrl(b, BIO_C_GET_BUF_MEM_PTR, 0, ptr);
end;

initialization
  SSL_library_init;
  OPENSSL_add_all_algorithms_noconf;

end.
