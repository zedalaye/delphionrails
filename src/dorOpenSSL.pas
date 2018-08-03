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

  SSL_CTRL_OPTIONS = 32;
  SSL_CTRL_CLEAR_OPTIONS      = 77;

const
  SSL_OP_MICROSOFT_SESS_ID_BUG            = $00000001;
  SSL_OP_NETSCAPE_CHALLENGE_BUG           = $00000002;
  {* Allow initial connection to servers that don't support RI *}
  SSL_OP_LEGACY_SERVER_CONNECT            = $00000004;
  SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG = $00000008;
  SSL_OP_TLSEXT_PADDING                   = $00000010;
  SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER       = $00000020;
  SSL_OP_SAFARI_ECDHE_ECDSA_BUG           = $00000040;
  SSL_OP_SSLEAY_080_CLIENT_DH_BUG         = $00000080;
  SSL_OP_TLS_D5_BUG                       = $00000100;
  SSL_OP_TLS_BLOCK_PADDING_BUG            = $00000200;

  { * Hasn't done anything since OpenSSL 0.9.7h, retained for compatibility * }
  SSL_OP_MSIE_SSLV2_RSA_PADDING           = $0;
  { * Refers to ancient SSLREF and SSLv2, retained for compatibility * }
  SSL_OP_SSLREF2_REUSE_CERT_TYPE_BUG      = $0;

{*
 * Disable SSL 3.0/TLS 1.0 CBC vulnerability workaround that was added in
 * OpenSSL 0.9.6d.  Usually (depending on the application protocol) the
 * workaround is not needed.  Unfortunately some broken SSL/TLS
 * implementations cannot handle it at all, which is why we include it in
 * SSL_OP_ALL.
 *}
{* added in 0.9.6e *}
  SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS      = $00000800;

{*
 * SSL_OP_ALL: various bug workarounds that should be rather harmless.  This
 * used to be 0x000FFFFFL before 0.9.7.
 *}
  SSL_OP_ALL                              = $80000BFF;

{* DTLS options *}
  SSL_OP_NO_QUERY_MTU                     = $00001000;
{* Turn on Cookie Exchange (on relevant for servers) *}
  SSL_OP_COOKIE_EXCHANGE                  = $00002000;
{* Don't use RFC4507 ticket extension *}
  SSL_OP_NO_TICKET                        = $00004000;
{* Use Cisco's "speshul" version of DTLS_BAD_VER (as client)  *}
  SSL_OP_CISCO_ANYCONNECT                 = $00008000;

{* As server, disallow session resumption on renegotiation *}
  SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION   = $00010000;
{* Don't use compression even if supported *}
  SSL_OP_NO_COMPRESSION                           = $00020000;
{* Permit unsafe legacy renegotiation *}
  SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION        = $00040000;
{* If set, always create a new key when using tmp_ecdh parameters *}
  SSL_OP_SINGLE_ECDH_USE                          = $00080000;
{ * Does nothing: retained for compatibility *}
  SSL_OP_SINGLE_DH_USE                            = $00100000;
{ * Does nothing: retained for compatibiity *}
  SSL_OP_EPHEMERAL_RSA                            = $0;
{*
 * Set on servers to choose the cipher according to the server's preferences
 *}
  SSL_OP_CIPHER_SERVER_PREFERENCE                 = $00400000;
{*
 * If set, a server will allow a client to issue a SSLv3.0 version number as
 * latest version supported in the premaster secret, even when TLSv1.0
 * (version 3.1) was announced in the client hello. Normally this is
 * forbidden to prevent version rollback attacks.
 *}
  SSL_OP_TLS_ROLLBACK_BUG                         = $00800000;

  SSL_OP_NO_SSLv2                                 = $01000000;
  SSL_OP_NO_SSLv3                                 = $02000000;
  SSL_OP_NO_TLSv1                                 = $04000000;
  SSL_OP_NO_TLSv1_2                               = $08000000;
  SSL_OP_NO_TLSv1_1                               = $10000000;

  SSL_OP_NO_DTLSv1                                = $04000000;
  SSL_OP_NO_DTLSv1_2                              = $08000000;

  SSL_OP_NO_SSL_MASK = SSL_OP_NO_SSLv2 + SSL_OP_NO_SSLv3
                     + SSL_OP_NO_TLSv1 + SSL_OP_NO_TLSv1_1 + SSL_OP_NO_TLSv1_2;

{*
 * These next two were never actually used for anything since SSLeay zap so
 * we have some more flags.
 *}
{*
 * The next flag deliberately changes the ciphertest, this is a check for the
 * PKCS#1 attack
 *}
  SSL_OP_PKCS1_CHECK_1                            = $0;
  SSL_OP_PKCS1_CHECK_2                            = $0;

  SSL_OP_NETSCAPE_CA_DN_BUG                       = $20000000;
  SSL_OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG          = $40000000;
{*
 * Make server add server-hello extension from early version of cryptopro
 * draft, when GOST ciphersuite is negotiated. Required for interoperability
 * with CryptoPro CSP 3.x
 *}
  SSL_OP_CRYPTOPRO_TLSEXT_BUG                     = $80000000;

type
  PSSL = Pointer;
  PSSL_CTX = Pointer;
  PSSL_METHOD = Pointer;

  PX509 = Pointer;
  PPX509 = ^PX509;

  PX509_REQ = Pointer;
  PPX509_REQ = ^PX509_REQ;

  PX509_CRL = Pointer;
  PPX509_CRL = ^PX509_CRL;

  PX509_NAME = Pointer;

  PPKCS7 = Pointer;
  PPPKCS7 = ^PPKCS7;

  function SSL_library_init: Integer; cdecl; external SSLEAY;
  function SSL_CTX_set_cipher_list(arg0: PSSL_CTX; str: PAnsiChar): Integer; cdecl; external SSLEAY;
  function SSL_CTX_new(meth: PSSL_METHOD): PSSL_CTX; cdecl; external SSLEAY;
  procedure SSL_CTX_free(arg0: PSSL_CTX); cdecl; external SSLEAY;
  function SSL_set_fd(s: PSSL; fd: Longint): Integer; cdecl; external SSLEAY;

  { General-purpose version-flexible SSL/TLS methods for OpenSSL 1.0.x }
  function SSLv23_method: PSSL_METHOD; cdecl; external SSLEAY;

  { General-purpose version-flexible SSL/TLS methods for OpenSSL 1.1.x }
//  function TLS_method: PSSL_METHOD; cdecl; external SSLEAY;

  function SSL_CTX_ctrl(ctx: PSSL_CTX; cmd: Integer; larg: LongInt; parg: Pointer): LongInt; cdecl; external SSLEAY;

  function SSL_CTX_use_RSAPrivateKey_file(ctx: PSSL_CTX; const filename: PAnsiChar; typ: Integer):Integer; cdecl; external SSLEAY;
  function SSL_CTX_use_certificate_file(ctx: PSSL_CTX; const filename: PAnsiChar; typ: Integer):Integer; cdecl; external SSLEAY;
  function SSL_CTX_use_certificate_chain_file(ctx: PSSL_CTX; const _file: PAnsiChar):Integer; cdecl; external SSLEAY;
  procedure SSL_CTX_set_default_passwd_cb(ctx: PSSL_CTX; cb: Pointer); cdecl; external SSLEAY;
  procedure SSL_CTX_set_default_passwd_cb_userdata(ctx: PSSL_CTX; u: Pointer); cdecl; external SSLEAY;
  function SSL_CTX_load_verify_locations(ctx: PSSL_CTX; const CAfile: PAnsiChar; const CApath: PAnsiChar):Integer; cdecl; external SSLEAY;
  procedure SSL_CTX_set_cert_verify_callback(ctx: PSSL_CTX; cb: Pointer; arg: Pointer); cdecl; external SSLEAY;
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

  function SSL_CTX_set_options(ctx: PSSL_CTX; op: LongInt): LongInt;
  function SSL_CTX_clear_options(ctx: PSSL_CTX; op: LongInt): LongInt;
  function SSL_CTX_get_options(ctx: PSSL_CTX): LongInt;

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
 * ENGINE
(******************************************************************************)

type
  PENGINE = Pointer;

(******************************************************************************
 * BN
(******************************************************************************)

type
  PBN = Pointer;
  PBN_CTX = Pointer;

(******************************************************************************
 * BIGNUM
(******************************************************************************)

type
  PBIGNUM = Pointer;

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
  PPByte = ^PByte;

  PEVP_CIPHER = ^EVP_CIPHER;
  PEVP_CIPHER_CTX = ^EVP_CIPHER_CTX;

  PEVP_MD = ^EVP_MD;
  PPEVP_MD = ^PEVP_MD;

  PEVP_MD_CTX = ^EVP_MD_CTX;
  PPEVP_MD_CTX = ^PEVP_MD_CTX;

  PEVP_PKEY_CTX = Pointer;
  PPEVP_PKEY_CTX = ^PEVP_PKEY_CTX;

  PEVP_PKEY = Pointer;
  PPEVP_PKEY = ^PEVP_PKEY;

  PRSA = Pointer;
  PPRSA = ^PRSA;

  PDSA = Pointer;
  PPDSA = ^PDSA;

  PDH = Pointer;
  PPDH = ^PDH;

  PEC_KEY = Pointer;
  PPEC_KEY = ^PEC_KEY;

  PEC_GROUP = Pointer;
  PEC_POINT = Pointer;
  PEC_KEY_METHOD = Pointer;

  TPointConversionForm = (
    (** the point is encoded as z||x, where the octet z specifies
     *  which solution of the quadratic equation y is  *)
    POINT_CONVERSION_COMPRESSED = 2,
    (** the point is encoded as z||x||y, where z is the octet 0x02  *)
    POINT_CONVERSION_UNCOMPRESSED = 4,
    (** the point is encoded as z||x||y, where the octet z specifies
     *  which solution of the quadratic equation y is  *)
    POINT_CONVERSION_HYBRID = 6
  );

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
    ctrl: function(ctx: PEVP_CIPHER_CTX; &type, arg: Integer; ptr: Pointer): Integer; cdecl; (* Miscellaneous operations *)
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
    &type: Integer;
    pkey_type: Integer;
    md_size: Integer;
    flags: LongWord;
    init: function(ctx: PEVP_MD_CTX): Integer; cdecl;
    update: function(ctx: PEVP_MD_CTX; const data: Pointer; count: Cardinal): Integer; cdecl;
    final_: function(ctx: PEVP_MD_CTX; md: PAnsiChar): Integer; cdecl;
    copy: function(to_: PEVP_MD_CTX; const from: PEVP_MD_CTX): Integer; cdecl;
    cleanup: function(ctx: PEVP_MD_CTX): Integer; cdecl;

    (* FIXME: prototype these some day *)
    sign: function(&type: Integer; const m: PAnsiChar; m_length: Cardinal;
      sigret: PAnsiChar; siglen: PCardinal; key: Pointer): Integer; cdecl;
    verify: function(&type: Integer; const m: PAnsiChar; m_length: Cardinal;
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


function EVP_EncryptInit_ex(ctx: PEVP_CIPHER_CTX; &type: PEVP_CIPHER; imple: PENGINE; key: PByte; iv: PByte): Integer; cdecl; external LIBEAY;
function EVP_EncryptUpdate(ctx: PEVP_CIPHER_CTX; &out: PByte; var outl: Integer; &in: PByte; inl: Integer): Integer; cdecl; external LIBEAY;
function EVP_EncryptFinal_ex(ctx: PEVP_CIPHER_CTX; &out: PByte; var outl: Integer): Integer; cdecl; external LIBEAY;

function EVP_DecryptInit_ex(ctx: PEVP_CIPHER_CTX; const &type: PEVP_CIPHER; impl: PENGINE; key: PByte; iv: PByte): Integer; cdecl; external LIBEAY;
function EVP_DecryptUpdate(ctx: PEVP_CIPHER_CTX; &out: PByte; var outl: Integer; &in: PByte; inl: Integer): Integer; cdecl; external LIBEAY;
function EVP_DecryptFinal_ex(ctx: PEVP_CIPHER_CTX; outm: PByte; var outl: Integer): Integer; cdecl; external LIBEAY;

function EVP_CipherInit_ex(ctx: PEVP_CIPHER_CTX; const &type: PEVP_CIPHER; impl: PENGINE; key: PByte; iv: PByte; enc: Integer): Integer; cdecl; external LIBEAY;
function EVP_CipherUpdate(ctx: PEVP_CIPHER_CTX; &out: PByte; var outl: Integer; &in: PByte; inl: Integer): Integer; cdecl; external LIBEAY;
function EVP_CipherFinal_ex(ctx: PEVP_CIPHER_CTX; outm: PByte; var outl: Integer): Integer; cdecl; external LIBEAY;

function EVP_EncryptInit(ctx: PEVP_CIPHER_CTX; const &type: PEVP_CIPHER; key: PByte; iv: PByte): Integer; cdecl; external LIBEAY;
function EVP_EncryptFinal(ctx: PEVP_CIPHER_CTX; &out: PByte; var outl: Integer): Integer; cdecl; external LIBEAY;

function EVP_DecryptInit(ctx: PEVP_CIPHER_CTX; const &type: PEVP_CIPHER; key: PByte; iv: PByte): Integer; cdecl; external LIBEAY;
function EVP_DecryptFinal(ctx: PEVP_CIPHER_CTX; outm: PByte; var outl: Integer): Integer; cdecl; external LIBEAY;

function EVP_CipherInit(ctx: PEVP_CIPHER_CTX; const &type: PEVP_CIPHER; key: PByte; iv: PByte; enc: Integer): Integer; cdecl; external LIBEAY;
function EVP_CipherFinal(ctx: PEVP_CIPHER_CTX; outm: PByte; var outl: Integer): Integer; cdecl; external LIBEAY;

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
procedure EVP_MD_CTX_destroy(ctx: PEVP_MD_CTX); cdecl; external LIBEAY;
function EVP_MD_CTX_copy(out_: PEVP_MD_CTX; const in_: PEVP_MD_CTX): Integer; cdecl; external LIBEAY;

// OpenSSL 1.1  : create and destroyy have been renamed to new and free
//function EVP_MD_CTX_new: PEVP_MD_CTX; cdecl; external LIBEAY;
//function EVP_MD_CTX_reset(ctx: PEVP_MD_CTX): Integer; cdecl; external LIBEAY;
//procedure EVP_MD_CTX_free(ctx: PEVP_MD_CTX); cdecl; external LIBEAY;

function EVP_MD_CTX_size(e: PEVP_MD_CTX): Integer; inline;
function EVP_MD_CTX_block_size(e: PEVP_MD_CTX): Integer; inline;
function EVP_MD_CTX_type(e: PEVP_MD_CTX): Integer; inline;

function EVP_MD_type(const md: PEVP_MD): Integer; cdecl; external LIBEAY;
function EVP_MD_size(const md: PEVP_MD): Integer; cdecl; external LIBEAY;
function EVP_MD_block_size(const md: PEVP_MD): Integer; cdecl; external LIBEAY;
function EVP_MD_flags(const md: PEVP_MD): LongWord; cdecl; external LIBEAY;

function EVP_MD_name(e: PEVP_MD): PAnsiChar; inline;
function EVP_MD_nid(e: PEVP_MD): Integer; inline;

function EVP_DigestInit(ctx: PEVP_MD_CTX; const &type: PEVP_MD): Integer; cdecl; external LIBEAY;
function EVP_DigestFinal(ctx: PEVP_MD_CTX; md: PByte; var s: Cardinal): Integer; cdecl; external LIBEAY;

function EVP_DigestInit_ex(ctx: PEVP_MD_CTX; const &type: PEVP_MD; impl: PENGINE): Integer; cdecl; external LIBEAY;
function EVP_DigestFinal_ex(ctx: PEVP_MD_CTX; md: PByte; var s: Cardinal): Integer; cdecl; external LIBEAY;

function EVP_DigestSignInit(ctx: PEVP_MD_CTX; pctx: PPEVP_PKEY_CTX; const &type: PEVP_MD; e: PENGINE; pkey: PEVP_PKEY): Integer; cdecl; external LIBEAY;
function EVP_DigestSignFinal(ctx: PEVP_MD_CTX; sig: PByte; var siglen: Cardinal): Integer; cdecl; external LIBEAY;

function EVP_DigestVerifyInit(ctx: PEVP_MD_CTX; pctx: PPEVP_PKEY_CTX; const &type: PEVP_MD; e: PENGINE; pkey: PEVP_PKEY): Integer; cdecl; external LIBEAY;
function EVP_DigestVerifyFinal(ctx: PEVP_MD_CTX; const sig: PByte; siglen: Cardinal): Integer; cdecl; external LIBEAY;

function EVP_DigestUpdate(ctx: PEVP_MD_CTX; const d: Pointer; cnt: Cardinal): Integer; cdecl; external LIBEAY;

function EVP_SignInit_ex(ctx: PEVP_MD_CTX; const &type: PEVP_MD; impl: PENGINE): Integer; // EVP_DigestInit_ex(a,b,c)
function EVP_SignInit(ctx: PEVP_MD_CTX; const &type: PEVP_MD): Integer;  // EVP_DigestInit
function EVP_SignUpdate(ctx: PEVP_MD_CTX; const d: Pointer; cnt: Cardinal): Integer; // EVP_DigestUpdate

function EVP_VerifyInit_ex(ctx: PEVP_MD_CTX; const &type: PEVP_MD; impl: PENGINE): Integer; // EVP_DigestInit_ex
function EVP_VerifyInit(ctx: PEVP_MD_CTX; const &type: PEVP_MD): Integer; // EVP_DigestInit
function EVP_VerifyUpdate(ctx: PEVP_MD_CTX; const d: Pointer; cnt: Cardinal): Integer; // EVP_DigestUpdate

function EVP_DigestSignUpdate(ctx: PEVP_MD_CTX; const d: Pointer; cnt: Cardinal): Integer; // EVP_DigestUpdate
function EVP_DigestVerifyUpdate(ctx: PEVP_MD_CTX; const d: Pointer; cnt: Cardinal): Integer; // EVP_DigestUpdate

function EVP_OpenInit(ctx: PEVP_CIPHER_CTX; &type: PEVP_CIPHER; ek: PByte; ekl: Integer; iv: PByte; priv: PEVP_PKEY): Integer; cdecl; external LIBEAY;
function EVP_OpenFinal(ctx: PEVP_CIPHER_CTX; &out: PByte; var outl: Integer): Integer; cdecl; external LIBEAY;

function EVP_OpenUpdate(ctx: PEVP_CIPHER_CTX; &out: PByte; var outl: Integer; &in: PByte; inl: Integer): Integer; // EVP_DecryptUpdate(a,b,c,d,e)

function EVP_SealInit(ctx: PEVP_CIPHER_CTX; const &type: PEVP_CIPHER; ek: PPByte; ek1: PInteger; iv: PByte; pubk: PPEVP_PKEY; npubk: Integer): Integer; cdecl; external LIBEAY;
function EVP_SealFinal(ctx: PEVP_CIPHER_CTX; &out: PByte; var outl: Integer): Integer; cdecl; external LIBEAY;

function EVP_SealUpdate(ctx: PEVP_CIPHER_CTX; &out: PByte; var outl: Integer; &in: PByte; inl: Integer): Integer; // EVP_EncryptUpdate(a,b,c,d,e)

function EVP_get_cipherbyname(const name: PAnsiChar): PEVP_CIPHER; cdecl; external LIBEAY;
function EVP_get_digestbyname(const name: PAnsiChar): PEVP_MD; cdecl; external LIBEAY;

function EVP_BytesToKey(const &type: PEVP_CIPHER; const md: PEVP_MD;
  const salt, data: Pointer; datal, count: Integer; key, iv: PByte): Integer; cdecl; external LIBEAY;

(* EVP_PKEY - Key Pairs *)

function EVP_PKEY_new: PEVP_PKEY; cdecl; external LIBEAY;
procedure EVP_PKEY_up_ref(key: PEVP_PKEY); cdecl; external LIBEAY;
procedure EVP_PKEY_free(key: PEVP_PKEY); cdecl; external LIBEAY;

function EVP_PKEY_set1_RSA(pkey: PEVP_PKEY; key: PRSA): Integer; cdecl; external LIBEAY;
function EVP_PKEY_set1_DSA(pkey: PEVP_PKEY; key: PDSA): Integer; cdecl; external LIBEAY;
function EVP_PKEY_set1_DH(pkey: PEVP_PKEY; key: PDH): Integer; cdecl; external LIBEAY;
function EVP_PKEY_set1_EC_KEY(pkey: PEVP_PKEY; key: PEC_KEY): Integer; cdecl; external LIBEAY;

function EVP_PKEY_get1_RSA(pkey: PEVP_PKEY): PRSA; cdecl; external LIBEAY;
function EVP_PKEY_get1_DSA(pkey: PEVP_PKEY): PDSA; cdecl; external LIBEAY;
function EVP_PKEY_get1_DH(pkey: PEVP_PKEY): PDH; cdecl; external LIBEAY;
function EVP_PKEY_get1_EC_KEY(pkey: PEVP_PKEY): PEC_KEY; cdecl; external LIBEAY;

function EVP_PKEY_get0_RSA(pkey: PEVP_PKEY): PRSA; cdecl; external LIBEAY;
function EVP_PKEY_get0_DSA(pkey: PEVP_PKEY): PDSA; cdecl; external LIBEAY;
function EVP_PKEY_get0_DH(pkey: PEVP_PKEY): PDH; cdecl; external LIBEAY;
function EVP_PKEY_get0_EC_KEY(pkey: PEVP_PKEY): PEC_KEY; cdecl; external LIBEAY;

function EVP_PKEY_assign_RSA(pkey: PEVP_PKEY; key: PRSA): Integer; cdecl; external LIBEAY;
function EVP_PKEY_assign_DSA(pkey: PEVP_PKEY; key: PDSA): Integer; cdecl; external LIBEAY;
function EVP_PKEY_assign_DH(pkey: PEVP_PKEY; key: PDH): Integer; cdecl; external LIBEAY;
function EVP_PKEY_assign_EC_KEY(pkey: PEVP_PKEY; key: PEC_KEY): Integer; cdecl; external LIBEAY;

function EVP_PKEY_type(&type: Integer): Integer; cdecl; external LIBEAY;

(* EVP: EC_KEY - Elliptic Curve Keys *)

function EC_KEY_new: PEC_KEY; cdecl; external LIBEAY;
function EC_KEY_get_flags(const key: PEC_KEY): Integer; cdecl; external LIBEAY;
procedure EC_KEY_set_flags(key: PEC_KEY; flags: Integer); cdecl; external LIBEAY;
procedure EC_KEY_clear_flags(key: PEC_KEY; flags: Integer); cdecl; external LIBEAY;
function EC_KEY_new_by_curve_name(nid: Integer): PEC_KEY; cdecl; external LIBEAY;
procedure EC_KEY_free(key: PEC_KEY); cdecl; external LIBEAY;
function EC_KEY_copy(dst: PEC_KEY; const src: PEC_KEY): PEC_KEY; cdecl; external LIBEAY;
function EC_KEY_dup(const src: PEC_KEY): PEC_KEY; cdecl; external LIBEAY;
function EC_KEY_up_ref(key: PEC_KEY): Integer; cdecl; external LIBEAY;
function EC_KEY_get0_group(const key: PEC_KEY): PEC_GROUP; cdecl; external LIBEAY;
function EC_KEY_set_group(key: PEC_KEY; const group: PEC_GROUP): Integer; cdecl; external LIBEAY;
function EC_KEY_get0_private_key(const key: PEC_KEY): PBIGNUM; cdecl; external LIBEAY;
function EC_KEY_set_private_key(key: PEC_KEY; const prv: PBIGNUM): Integer; cdecl; external LIBEAY;
function EC_KEY_get0_public_key(const key: PEC_KEY): PEC_POINT; cdecl; external LIBEAY;
function EC_KEY_set_public_key(key: PEC_KEY; const pub: PEC_POINT): Integer; cdecl; external LIBEAY;
function EC_KEY_get_conv_form(const key: PEC_KEY): TPointConversionForm; cdecl; external LIBEAY;
procedure EC_KEY_set_conv_form(eckey: PEC_KEY; cform: TPointConversionForm); cdecl; external LIBEAY;
procedure EC_KEY_set_asn1_flag(eckey: PEC_KEY; asn1_flag: Integer); cdecl; external LIBEAY;
function EC_KEY_precompute_mult(key: PEC_KEY; ctx: PBN_CTX): Integer; cdecl; external LIBEAY;
function EC_KEY_generate_key(key: PEC_KEY): Integer; cdecl; external LIBEAY;
function EC_KEY_check_key(const key: PEC_KEY): Integer; cdecl; external LIBEAY;
function EC_KEY_set_public_key_affine_coordinates(key: PEC_KEY; x: PBIGNUM; y: PBIGNUM): Integer; cdecl; external LIBEAY;
function EC_KEY_get_method(const key: PEC_KEY): PEC_KEY_METHOD; cdecl; external LIBEAY;
function EC_KEY_set_method(key: PEC_KEY; const meth: PEC_KEY_METHOD): Integer; cdecl; external LIBEAY;

(* EVP: MD - Message Digest *)

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

(* EVP: CIPHER *)

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
  BIO_NOCLOSE = 0;
  BIO_CLOSE = 1;

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
  BIO_CTRL_SET_CALLBACK = 14; { opt - set callback function }
  BIO_CTRL_GET_CALLBACK = 15; { opt - set callback function }

  BIO_CTRL_SET_FILENAME = 30; { BIO_s_file special }

  BIO_FP_READ   = $02;
  BIO_FP_WRITE  = $04;
  BIO_FP_APPEND = $08;
  BIO_FP_TEXT   = $10;

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

  BIO_C_SET_WRITE_BUF_SIZE = 136; { for BIO_s_bio }
  BIO_C_GET_WRITE_BUF_SIZE = 137;
  BIO_C_MAKE_BIO_PAIR = 138;
  BIO_C_DESTROY_BIO_PAIR = 139;
  BIO_C_GET_WRITE_GUARANTEE = 140;
  BIO_C_GET_READ_REQUEST = 141;
  BIO_C_SHUTDOWN_WR = 142;
  BIO_C_NREAD0 = 143;
  BIO_C_NREAD = 144;
  BIO_C_NWRITE0 = 145;
  BIO_C_NWRITE = 146;
  BIO_C_RESET_READ_REQUEST = 147;
  BIO_C_SET_MD_CTX = 148;

  BIO_C_SET_PREFIX = 149;
  BIO_C_GET_PREFIX = 150;
  BIO_C_SET_SUFFIX = 151;
  BIO_C_GET_SUFFIX = 152;

  BIO_C_SET_EX_ARG = 153;
  BIO_C_GET_EX_ARG = 154;

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
	  &type: Integer;
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

function BIO_new(&type: PBIO_METHOD): PBIO; cdecl; external LIBEAY;
procedure BIO_set_flags(b: PBIO; flags: Integer); cdecl; external LIBEAY;
function BIO_new_mem_buf(buf: Pointer; len: Integer): PBIO; cdecl; external LIBEAY;
function BIO_push(b: PBIO; append: PBIO): PBIO; cdecl; external LIBEAY;
function BIO_write(b: PBIO; const data: Pointer; len: Integer): Integer; cdecl; external LIBEAY;
function BIO_read(b: PBIO; data: Pointer; len: Integer): Integer; cdecl; external LIBEAY;
function BIO_ctrl(bp: PBIO; cmd: Integer; larg: Longint; parg: Pointer): LongInt; cdecl; external LIBEAY;
procedure	BIO_free_all(a: PBIO); cdecl; external LIBEAY;

function BIO_flush(b: PBIO): Integer; inline;
function BIO_get_mem_ptr(b: PBIO; ptr: Pointer): LongInt; inline;

function BIO_s_mem: PBIO_METHOD; cdecl; external LIBEAY;
function BIO_s_file: PBIO_METHOD; cdecl; external LIBEAY;
function BIO_s_socket: PBIO_METHOD; cdecl; external LIBEAY;
function BIO_s_connect: PBIO_METHOD; cdecl; external LIBEAY;
function BIO_s_accept: PBIO_METHOD; cdecl; external LIBEAY;
function BIO_s_fd: PBIO_METHOD; cdecl; external LIBEAY;
function BIO_s_bio: PBIO_METHOD; cdecl; external LIBEAY;
function BIO_s_null: PBIO_METHOD; cdecl; external LIBEAY;
function BIO_f_null: PBIO_METHOD; cdecl; external LIBEAY;
function BIO_f_buffer: PBIO_METHOD; cdecl; external LIBEAY;
function BIO_f_nbio_test: PBIO_METHOD; cdecl; external LIBEAY;
function BIO_s_datagram: PBIO_METHOD; cdecl; external LIBEAY;
function BIO_s_datagram_sctp: PBIO_METHOD; cdecl; external LIBEAY;

function BIO_f_md: PBIO_METHOD; cdecl; external LIBEAY;
function BIO_f_base64: PBIO_METHOD; cdecl; external LIBEAY;
function BIO_f_cipher: PBIO_METHOD; cdecl; external LIBEAY;
function BIO_f_reliable: PBIO_METHOD; cdecl; external LIBEAY;

procedure BIO_set_cipher(b: PBIO; const c: PEVP_CIPHER; const k: PByte; const i: PByte; enc: Integer); cdecl; external LIBEAY;

function BIO_new_file(const filename: PAnsiChar; const mode: PAnsiChar): PBIO; cdecl; external LIBEAY;

function BIO_read_filename(b: PBIO; name: PAnsiChar): LongInt;
function BIO_write_filename(b: PBIO; name: PAnsiChar): LongInt;
function BIO_append_filename(b: PBIO; name: PAnsiChar): LongInt;
function BIO_rw_filename(b: PBIO; name: PAnsiChar): LongInt;

function BIO_set_md(b: PBIO; md: PEVP_MD): LongInt;
function BIO_get_md(b: PBIO; md: PPEVP_MD): LongInt;
function BIO_get_md_ctx(b: PBIO; ctx: PPEVP_MD_CTX): LongInt;
function BIO_set_md_ctx(b: PBIO; ctx: PEVP_MD_CTX): LongInt;

(******************************************************************************
 * PEM
(******************************************************************************)
type
  pem_password_cb = function(buf: PAnsiChar; size: Integer; rwflag: Integer; u: Pointer): Integer; cdecl;

function PEM_read_bio_PrivateKey(bp: PBIO; x: PPEVP_PKEY; cb: pem_password_cb; u: Pointer): PEVP_PKEY; cdecl; external LIBEAY;
// P_PKEY *PEM_read_PrivateKey(FILE *fp, EVP_PKEY **x, pem_password_cb *cb, void *u);

function PEM_write_bio_PrivateKey(bp: PBIO; x: PEVP_PKEY; const enc: PEVP_CIPHER; kstr: PByte; klen: Integer; cb: pem_password_cb; u: Pointer): Integer; cdecl; external LIBEAY;
// int PEM_write_PrivateKey(FILE *fp, EVP_PKEY *x, const EVP_CIPHER *enc, unsigned char *kstr, int klen, pem_password_cb *cb, void *u);

function PEM_write_bio_PKCS8PrivateKey(bp: PBIO; x: PEVP_PKEY; const enc: PEVP_CIPHER; kstr: PByte; klen: Integer; cb: pem_password_cb; u: Pointer): Integer; cdecl; external LIBEAY;
// int PEM_write_PKCS8PrivateKey(FILE *fp, EVP_PKEY *x, const EVP_CIPHER *enc, char *kstr, int klen, pem_password_cb *cb, void *u);

function PEM_write_bio_PKCS8PrivateKey_nid(bp: PBIO; x: PEVP_PKEY; nid: Integer; kstr: PByte; klen: Integer; cb: pem_password_cb; u: Pointer): Integer; cdecl; external LIBEAY;
// int PEM_write_PKCS8PrivateKey_nid(FILE *fp, EVP_PKEY *x, int nid, unsigned char *kstr, int klen, pem_password_cb *cb, void *u);

function PEM_read_bio_PUBKEY(bp: PBIO; x: PPEVP_PKEY; cb: pem_password_cb; u: Pointer): PEVP_PKEY; cdecl; external LIBEAY;
// EVP_PKEY *PEM_read_PUBKEY(FILE *fp, EVP_PKEY **x, pem_password_cb *cb, void *u);

function PEM_write_bio_PUBKEY(bp: PBIO; x: PEVP_PKEY): Integer; cdecl; external LIBEAY;
// int PEM_write_PUBKEY(FILE *fp, EVP_PKEY *x);

function PEM_read_bio_RSAPrivateKey(bp: PBIO; x: PPRSA; cb: pem_password_cb; u: Pointer): PRSA; cdecl; external LIBEAY;
// RSA *PEM_read_RSAPrivateKey(FILE *fp, RSA **x, pem_password_cb *cb, void *u);

function PEM_write_bio_RSAPrivateKey(bp: PBIO; x: PRSA; const enc: PEVP_CIPHER; kstr: PByte; klen: Integer; cb: pem_password_cb; u: Pointer): Integer; cdecl; external LIBEAY;
// int PEM_write_RSAPrivateKey(FILE *fp, RSA *x, const EVP_CIPHER *enc, unsigned char *kstr, int klen, pem_password_cb *cb, void *u);

function PEM_read_bio_RSAPublicKey(bp: PBIO; x: PPRSA; cb: pem_password_cb; u: Pointer): PRSA; cdecl; external LIBEAY;
// RSA *PEM_read_RSAPublicKey(FILE *fp, RSA **x, pem_password_cb *cb, void *u);

function PEM_write_bio_RSAPublicKey(bp: PBIO; x: PRSA): Integer; cdecl; external LIBEAY;
// int PEM_write_RSAPublicKey(FILE *fp, RSA *x);

function PEM_read_bio_RSA_PUBKEY(bp: PBIO; x: PPRSA; cb: pem_password_cb; u: Pointer): PRSA; cdecl; external LIBEAY;
// RSA *PEM_read_RSA_PUBKEY(FILE *fp, RSA **x, pem_password_cb *cb, void *u);

function PEM_write_bio_RSA_PUBKEY(bp: PBIO; x: PRSA): Integer; cdecl; external LIBEAY;
// int PEM_write_RSA_PUBKEY(FILE *fp, RSA *x);

function PEM_read_bio_DSAPrivateKey(bp: PBIO; x: PPDSA; cb: pem_password_cb; u: Pointer): PDSA; cdecl; external LIBEAY;
// DSA *PEM_read_DSAPrivateKey(FILE *fp, DSA **x, pem_password_cb *cb, void *u);

function PEM_write_bio_DSAPrivateKey(bp: PBIO; x: PDSA; const enc: PEVP_CIPHER; kstr: PByte; klen: Integer; cb: pem_password_cb; u: Pointer): Integer; cdecl; external LIBEAY;
// int PEM_write_DSAPrivateKey(FILE *fp, DSA *x, const EVP_CIPHER *enc, unsigned char *kstr, int klen, pem_password_cb *cb, void *u);

function PEM_read_bio_DSA_PUBKEY(bp: PBIO; x: PPDSA; cb: pem_password_cb; u: Pointer): PDSA; cdecl; external LIBEAY;
// DSA *PEM_read_DSA_PUBKEY(FILE *fp, DSA **x, pem_password_cb *cb, void *u);

function PEM_write_bio_DSA_PUBKEY(bp: PBIO; x: PDSA): Integer; cdecl; external LIBEAY;
// int PEM_write_DSA_PUBKEY(FILE *fp, DSA *x);

function PEM_read_bio_DSAparams(bp: PBIO; x: PPDSA; cb: pem_password_cb; u: Pointer): PDSA; cdecl; external LIBEAY;
// DSA *PEM_read_DSAparams(FILE *fp, DSA **x, pem_password_cb *cb, void *u);

function PEM_write_bio_DSAparams(bp: PBIO; x: PDSA): Integer; cdecl; external LIBEAY;
// int PEM_write_DSAparams(FILE *fp, DSA *x);

function PEM_read_bio_DHparams(bp: PBIO; x: PPDH; cb: pem_password_cb; u: Pointer): PDH; cdecl; external LIBEAY;
// DH *PEM_read_DHparams(FILE *fp, DH **x, pem_password_cb *cb, void *u);

function PEM_write_bio_DHparams(bp: PBIO; x: PDH): Integer; cdecl; external LIBEAY;
// int PEM_write_DHparams(FILE *fp, DH *x);

function PEM_read_bio_X509(bp: PBIO; x: PPX509; cb: pem_password_cb; u: Pointer): PX509; cdecl; external LIBEAY;
// X509 *PEM_read_X509(FILE *fp, X509 **x, pem_password_cb *cb, void *u);

function PEM_write_bio_X509(bp: PBIO; x: PX509): Integer; cdecl; external LIBEAY;
// int PEM_write_X509(FILE *fp, X509 *x);

function PEM_read_bio_X509_AUX(bp: PBIO; x: PPX509; cb: pem_password_cb; u: Pointer): PX509; cdecl; external LIBEAY;
// X509 *PEM_read_X509_AUX(FILE *fp, X509 **x, pem_password_cb *cb, void *u);

function PEM_write_bio_X509_AUX(bp: PBIO; x: PX509): Integer; cdecl; external LIBEAY;
// int PEM_write_X509_AUX(FILE *fp, X509 *x);

function PEM_read_bio_X509_REQ(bp: PBIO; x: PPX509_REQ; cb: pem_password_cb; u: Pointer): PX509_REQ; cdecl; external LIBEAY;
//  X509_REQ *PEM_read_X509_REQ(FILE *fp, X509_REQ **x, pem_password_cb *cb, void *u);

function PEM_write_bio_X509_REQ(bp: PBIO; x: PX509_REQ): Integer; cdecl; external LIBEAY;
// int PEM_write_X509_REQ(FILE *fp, X509_REQ *x);

function PEM_write_bio_X509_REQ_NEW(bp: PBIO; x: PX509_REQ): Integer; cdecl; external LIBEAY;
// int PEM_write_X509_REQ_NEW(FILE *fp, X509_REQ *x);

function PEM_read_bio_X509_CRL(bp: PBIO; x: PPX509_CRL; cb: pem_password_cb; u: Pointer): PX509_CRL; cdecl; external LIBEAY;
// X509_CRL *PEM_read_X509_CRL(FILE *fp, X509_CRL **x, pem_password_cb *cb, void *u);

function PEM_write_bio_X509_CRL(bp: PBIO; x: PX509_CRL): Integer; cdecl; external LIBEAY;
// int PEM_write_X509_CRL(FILE *fp, X509_CRL *x);

function PEM_read_bio_PKCS7(bp: PBIO; x: PPPKCS7; cn: pem_password_cb; u: Pointer): PPKCS7; cdecl; external LIBEAY;
// PKCS7 *PEM_read_PKCS7(FILE *fp, PKCS7 **x, pem_password_cb *cb, void *u);

function PEM_write_bio_PKCS7(bp: PBIO; x: PPKCS7): Integer; cdecl; external LIBEAY;
// int PEM_write_PKCS7(FILE *fp, PKCS7 *x);

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

function SSL_CTX_set_options(ctx: PSSL_CTX; op: LongInt): LongInt;
begin
  Result := SSL_CTX_ctrl(ctx, SSL_CTRL_OPTIONS, op, nil);
end;

function SSL_CTX_clear_options(ctx: PSSL_CTX; op: LongInt): LongInt;
begin
  Result := SSL_CTX_ctrl(ctx, SSL_CTRL_CLEAR_OPTIONS, op, nil);
end;

function SSL_CTX_get_options(ctx: PSSL_CTX): LongInt;
begin
  Result := SSL_CTX_ctrl(ctx, SSL_CTRL_OPTIONS, 0, nil);
end;

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

function EVP_SignInit_ex(ctx: PEVP_MD_CTX; const &type: PEVP_MD; impl: PENGINE): Integer;
begin
  Result := EVP_DigestInit_ex(ctx, &type, impl);
end;

function EVP_SignInit(ctx: PEVP_MD_CTX; const &type: PEVP_MD): Integer;
begin
  Result := EVP_DigestInit(ctx, &type);
end;

function EVP_SignUpdate(ctx: PEVP_MD_CTX; const d: Pointer; cnt: Cardinal): Integer;
begin
  Result := EVP_DigestUpdate(ctx, d, cnt);
end;

function EVP_VerifyInit_ex(ctx: PEVP_MD_CTX; const &type: PEVP_MD; impl: PENGINE): Integer;
begin
  Result := EVP_DigestInit_ex(ctx, &type, impl);
end;

function EVP_VerifyInit(ctx: PEVP_MD_CTX; const &type: PEVP_MD): Integer;
begin
  Result := EVP_DigestInit(ctx, &type);
end;

function EVP_VerifyUpdate(ctx: PEVP_MD_CTX; const d: Pointer; cnt: Cardinal): Integer;
begin
  Result := EVP_DigestUpdate(ctx, d, cnt);
end;

function EVP_DigestSignUpdate(ctx: PEVP_MD_CTX; const d: Pointer; cnt: Cardinal): Integer;
begin
  Result := EVP_DigestUpdate(ctx, d, cnt);
end;

function EVP_DigestVerifyUpdate(ctx: PEVP_MD_CTX; const d: Pointer; cnt: Cardinal): Integer;
begin
  Result := EVP_DigestUpdate(ctx, d, cnt);
end;

function EVP_OpenUpdate(ctx: PEVP_CIPHER_CTX; &out: PByte; var outl: Integer; &in: PByte; inl: Integer): Integer;
begin
  Result := EVP_DecryptUpdate(ctx, &out, outl, &in, inl);
end;

function EVP_SealUpdate(ctx: PEVP_CIPHER_CTX; &out: PByte; var outl: Integer; &in: PByte; inl: Integer): Integer;
begin
  Result := EVP_EncryptUpdate(ctx, &out, outl, &in, inl);
end;

function BIO_read_filename(b: PBIO; name: PAnsiChar): LongInt;
begin
  Result := BIO_ctrl(b, BIO_C_SET_FILENAME, BIO_CLOSE or BIO_FP_READ, name);
end;

function BIO_write_filename(b: PBIO; name: PAnsiChar): LongInt;
begin
  Result := BIO_ctrl(b, BIO_C_SET_FILENAME, BIO_CLOSE or BIO_FP_WRITE, name);
end;

function BIO_append_filename(b: PBIO; name: PAnsiChar): LongInt;
begin
  Result := BIO_ctrl(b,BIO_C_SET_FILENAME, BIO_CLOSE or BIO_FP_APPEND, name);
end;

function BIO_rw_filename(b: PBIO; name: PAnsiChar): LongInt;
begin
  Result := BIO_ctrl(b, BIO_C_SET_FILENAME, BIO_CLOSE or BIO_FP_READ or BIO_FP_WRITE, name)
end;

function BIO_set_md(b: PBIO; md: PEVP_MD): LongInt;
begin
  Result := BIO_ctrl(b, BIO_C_SET_MD, 0, md);
end;

function BIO_get_md(b: PBIO; md: PPEVP_MD): LongInt;
begin
  Result := BIO_ctrl(b, BIO_C_GET_MD, 0, md);
end;

function BIO_get_md_ctx(b: PBIO; ctx: PPEVP_MD_CTX): LongInt;
begin
  Result := BIO_ctrl(b, BIO_C_GET_MD_CTX, 0, ctx);
end;

function BIO_set_md_ctx(b: PBIO; ctx: PEVP_MD_CTX): LongInt;
begin
  Result := BIO_ctrl(b, BIO_C_SET_MD_CTX, 0, ctx);
end;

initialization
  SSL_library_init;
  OPENSSL_add_all_algorithms_noconf;

end.
