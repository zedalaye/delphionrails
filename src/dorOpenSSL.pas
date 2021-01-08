unit dorOpenSSL;

{$MINENUMSIZE 4}

interface

uses Classes;

const
(*
   Secure Defaults:
    This cipher list should be used in combination with :
     SSL_CTX_set_min_proto_version(FCtx, TLS1_2_VERSION);
     or remove entirely and use :
     SSL_CTX_set_min_proto_version(FCtx, TLS1_3_VERSION);
*)
  DOR_SSL_CIPHER_LIST = 'ECDH+AESGCM:ECDH+CHACHA20:ECDH+AES256:ECDH+AES128:!aNULL:!SHA1:!AESCCM';

const
  LIB_CRYPTO = 'libcrypto-1_1.dll';
  LIB_SSL = 'libssl-1_1.dll';

(*******************************************************************************
 * inititalization
 ******************************************************************************)

const
  (* Standard initialisation options *)
  OPENSSL_INIT_NO_LOAD_CRYPTO_STRINGS = $00000001;
  OPENSSL_INIT_LOAD_CRYPTO_STRINGS    = $00000002;
  OPENSSL_INIT_ADD_ALL_CIPHERS        = $00000004;
  OPENSSL_INIT_ADD_ALL_DIGESTS        = $00000008;
  OPENSSL_INIT_NO_ADD_ALL_CIPHERS     = $00000010;
  OPENSSL_INIT_NO_ADD_ALL_DIGESTS     = $00000020;
  OPENSSL_INIT_LOAD_CONFIG            = $00000040;
  OPENSSL_INIT_NO_LOAD_CONFIG         = $00000080;
  OPENSSL_INIT_ASYNC                  = $00000100;
  OPENSSL_INIT_ENGINE_RDRAND          = $00000200;
  OPENSSL_INIT_ENGINE_DYNAMIC         = $00000400;
  OPENSSL_INIT_ENGINE_OPENSSL         = $00000800;
  OPENSSL_INIT_ENGINE_CRYPTODEV       = $00001000;
  OPENSSL_INIT_ENGINE_CAPI            = $00002000;
  OPENSSL_INIT_ENGINE_PADLOCK         = $00004000;
  OPENSSL_INIT_ENGINE_AFALG           = $00008000;
(* OPENSSL_INIT_ZLIB                  = $00010000; *)
  OPENSSL_INIT_ATFORK                 = $00020000;
(* OPENSSL_INIT_BASE_ONLY             = $00040000; *)
  OPENSSL_INIT_NO_ATEXIT              = $00080000;
(* OPENSSL_INIT flag range 0xfff00000 reserved for OPENSSL_init_ssl() *)
(* Max OPENSSL_INIT flag value is 0x80000000 *)

  OPENSSL_INIT_NO_LOAD_SSL_STRINGS    = $00100000;
  OPENSSL_INIT_LOAD_SSL_STRINGS       = $00200000;

  OPENSSL_INIT_CRYPTO_DEFAULT         = (OPENSSL_INIT_ADD_ALL_CIPHERS or OPENSSL_INIT_ADD_ALL_DIGESTS);
  OPENSSL_INIT_SSL_DEFAULT            = (OPENSSL_INIT_LOAD_SSL_STRINGS or OPENSSL_INIT_LOAD_CRYPTO_STRINGS);

type
  POPENSSL_INIT_SETTINGS = Pointer;

  function OPENSSL_init_ssl(opts: UInt64; const settings: POPENSSL_INIT_SETTINGS): Integer; cdecl; external LIB_SSL;
  function OPENSSL_init_crypto(opts: UInt64; const settings: POPENSSL_INIT_SETTINGS): Integer; cdecl; external LIB_CRYPTO;

  procedure OPENSSL_thread_stop; cdecl; external LIB_CRYPTO;

(*******************************************************************************
 * version
 ******************************************************************************)

const
  OPENSSL_VERSION_STR = 0;  // OPENSSL_VERSION
  OPENSSL_CFLAGS      = 1;
  OPENSSL_BUILT_ON    = 2;
  OPENSSL_PLATFORM    = 3;
  OPENSSL_DIR         = 4;
  OPENSSL_ENGINES_DIR = 5;

function OpenSSL_version_num: Cardinal; cdecl; external LIB_CRYPTO;
function OpenSSL_version(t: Integer): PAnsiChar; cdecl; external LIB_CRYPTO;

(*******************************************************************************
 * aes
 ******************************************************************************)

const
  AES_BLOCK_SIZE = 16;

(*******************************************************************************
 * md5 (deprecated)
 ******************************************************************************)

const
  MD5_DIGEST_LENGTH = 16;

function MD5(const d: PByte; n: Cardinal; md: PByte): PByte; cdecl; external LIB_CRYPTO;

(*******************************************************************************
 * sha1 (deprecated)
 ******************************************************************************)

const
  SHA_DIGEST_LENGTH = 20;

function SHA1(const d: PAnsiChar; n: Cardinal; md: PAnsiChar): PAnsiChar; cdecl; external LIB_CRYPTO;

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
  EVP_MAX_MD_SIZE      = 64;	(* longest known is SHA512 *)
  EVP_MAX_KEY_LENGTH   = 32;
  EVP_MAX_IV_LENGTH    = 16;
  EVP_MAX_BLOCK_LENGTH = 32;

  PKCS5_SALT_LEN     = 8;
  (* Default PKCS#5 iteration count *)
  PKCS5_DEFAULT_ITER = 2048;

  EVP_PK_RSA   = $0001;
  EVP_PK_DSA   = $0002;
  EVP_PK_D     = $0004;
  EVP_PK_E     = $0008;
  EVP_PKT_SIGN = $0010;
  EVP_PKT_EN   = $0020;
  EVP_PKT_EXCH = $0040;
  EVP_PKS_RS   = $0100;
  EVP_PKS_DS   = $0200;
  EVP_PKS_EC   = $0400;
  EVP_PKT_EXP  = $1000; (* <= 512 bit key *)

type
  PPByte = ^PByte;

  PEVP_CIPHER = Pointer;
  PEVP_CIPHER_CTX = Pointer;

  PEVP_MD = Pointer;
  PPEVP_MD = ^PEVP_MD;

  PEVP_MD_CTX = Pointer;
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

function EVP_EncryptInit(ctx: PEVP_CIPHER_CTX; const &type: PEVP_CIPHER; key: PByte; iv: PByte): Integer; cdecl; external LIB_CRYPTO;
function EVP_EncryptFinal(ctx: PEVP_CIPHER_CTX; &out: PByte; var outl: Integer): Integer; cdecl; external LIB_CRYPTO;

function EVP_EncryptUpdate(ctx: PEVP_CIPHER_CTX; &out: PByte; var outl: Integer; &in: PByte; inl: Integer): Integer; cdecl; external LIB_CRYPTO;

function EVP_EncryptInit_ex(ctx: PEVP_CIPHER_CTX; &type: PEVP_CIPHER; imple: PENGINE; key: PByte; iv: PByte): Integer; cdecl; external LIB_CRYPTO;
function EVP_EncryptFinal_ex(ctx: PEVP_CIPHER_CTX; &out: PByte; var outl: Integer): Integer; cdecl; external LIB_CRYPTO;

function EVP_DecryptInit(ctx: PEVP_CIPHER_CTX; const &type: PEVP_CIPHER; key: PByte; iv: PByte): Integer; cdecl; external LIB_CRYPTO;
function EVP_DecryptFinal(ctx: PEVP_CIPHER_CTX; outm: PByte; var outl: Integer): Integer; cdecl; external LIB_CRYPTO;

function EVP_DecryptUpdate(ctx: PEVP_CIPHER_CTX; &out: PByte; var outl: Integer; &in: PByte; inl: Integer): Integer; cdecl; external LIB_CRYPTO;

function EVP_DecryptInit_ex(ctx: PEVP_CIPHER_CTX; const &type: PEVP_CIPHER; impl: PENGINE; key: PByte; iv: PByte): Integer; cdecl; external LIB_CRYPTO;
function EVP_DecryptFinal_ex(ctx: PEVP_CIPHER_CTX; outm: PByte; var outl: Integer): Integer; cdecl; external LIB_CRYPTO;

function EVP_CipherInit(ctx: PEVP_CIPHER_CTX; const &type: PEVP_CIPHER; key: PByte; iv: PByte; enc: Integer): Integer; cdecl; external LIB_CRYPTO;
function EVP_CipherInit_ex(ctx: PEVP_CIPHER_CTX; const &type: PEVP_CIPHER; impl: PENGINE; key: PByte; iv: PByte; enc: Integer): Integer; cdecl; external LIB_CRYPTO;

function EVP_CipherUpdate(ctx: PEVP_CIPHER_CTX; &out: PByte; var outl: Integer; &in: PByte; inl: Integer): Integer; cdecl; external LIB_CRYPTO;

function EVP_CipherFinal(ctx: PEVP_CIPHER_CTX; outm: PByte; var outl: Integer): Integer; cdecl; external LIB_CRYPTO;
function EVP_CipherFinal_ex(ctx: PEVP_CIPHER_CTX; outm: PByte; var outl: Integer): Integer; cdecl; external LIB_CRYPTO;

function EVP_DigestInit(ctx: PEVP_MD_CTX; const &type: PEVP_MD): Integer; cdecl; external LIB_CRYPTO;
function EVP_DigestFinal(ctx: PEVP_MD_CTX; md: PByte; var s: Cardinal): Integer; cdecl; external LIB_CRYPTO;

function EVP_DigestInit_ex(ctx: PEVP_MD_CTX; const &type: PEVP_MD; impl: PENGINE): Integer; cdecl; external LIB_CRYPTO;
function EVP_DigestFinal_ex(ctx: PEVP_MD_CTX; md: PByte; var s: Cardinal): Integer; cdecl; external LIB_CRYPTO;

function EVP_DigestSignInit(ctx: PEVP_MD_CTX; pctx: PPEVP_PKEY_CTX; const &type: PEVP_MD; e: PENGINE; pkey: PEVP_PKEY): Integer; cdecl; external LIB_CRYPTO;
function EVP_DigestSignFinal(ctx: PEVP_MD_CTX; sig: PByte; var siglen: Cardinal): Integer; cdecl; external LIB_CRYPTO;

function EVP_DigestVerifyInit(ctx: PEVP_MD_CTX; pctx: PPEVP_PKEY_CTX; const &type: PEVP_MD; e: PENGINE; pkey: PEVP_PKEY): Integer; cdecl; external LIB_CRYPTO;
function EVP_DigestVerifyFinal(ctx: PEVP_MD_CTX; const sig: PByte; siglen: Cardinal): Integer; cdecl; external LIB_CRYPTO;

function EVP_DigestUpdate(ctx: PEVP_MD_CTX; const d: Pointer; cnt: Cardinal): Integer; cdecl; external LIB_CRYPTO;

function EVP_SignInit_ex(ctx: PEVP_MD_CTX; const &type: PEVP_MD; impl: PENGINE): Integer; // EVP_DigestInit_ex(a,b,c)
function EVP_SignInit(ctx: PEVP_MD_CTX; const &type: PEVP_MD): Integer;  // EVP_DigestInit
function EVP_SignUpdate(ctx: PEVP_MD_CTX; const d: Pointer; cnt: Cardinal): Integer; // EVP_DigestUpdate

function EVP_VerifyInit_ex(ctx: PEVP_MD_CTX; const &type: PEVP_MD; impl: PENGINE): Integer; // EVP_DigestInit_ex
function EVP_VerifyInit(ctx: PEVP_MD_CTX; const &type: PEVP_MD): Integer; // EVP_DigestInit
function EVP_VerifyUpdate(ctx: PEVP_MD_CTX; const d: Pointer; cnt: Cardinal): Integer; // EVP_DigestUpdate

function EVP_DigestSignUpdate(ctx: PEVP_MD_CTX; const d: Pointer; cnt: Cardinal): Integer; // EVP_DigestUpdate
function EVP_DigestVerifyUpdate(ctx: PEVP_MD_CTX; const d: Pointer; cnt: Cardinal): Integer; // EVP_DigestUpdate

function EVP_OpenInit(ctx: PEVP_CIPHER_CTX; &type: PEVP_CIPHER; ek: PByte; ekl: Integer; iv: PByte; priv: PEVP_PKEY): Integer; cdecl; external LIB_CRYPTO;
function EVP_OpenFinal(ctx: PEVP_CIPHER_CTX; &out: PByte; var outl: Integer): Integer; cdecl; external LIB_CRYPTO;

function EVP_OpenUpdate(ctx: PEVP_CIPHER_CTX; &out: PByte; var outl: Integer; &in: PByte; inl: Integer): Integer; // EVP_DecryptUpdate(a,b,c,d,e)

function EVP_SealInit(ctx: PEVP_CIPHER_CTX; const &type: PEVP_CIPHER; ek: PPByte; ek1: PInteger; iv: PByte; pubk: PPEVP_PKEY; npubk: Integer): Integer; cdecl; external LIB_CRYPTO;
function EVP_SealFinal(ctx: PEVP_CIPHER_CTX; &out: PByte; var outl: Integer): Integer; cdecl; external LIB_CRYPTO;

function EVP_SealUpdate(ctx: PEVP_CIPHER_CTX; &out: PByte; var outl: Integer; &in: PByte; inl: Integer): Integer; // EVP_EncryptUpdate(a,b,c,d,e)

function EVP_CIPHER_CTX_new: PEVP_CIPHER_CTX; cdecl; external LIB_CRYPTO;
function EVP_CIPHER_CTX_reset(ctx: PEVP_CIPHER_CTX): Integer; cdecl; external LIB_CRYPTO;
procedure EVP_CIPHER_CTX_free(ctx: PEVP_CIPHER_CTX); cdecl; external LIB_CRYPTO;

function EVP_CIPHER_CTX_cipher(const ctx: PEVP_CIPHER_CTX): PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_CIPHER_CTX_block_size(const ctx: PEVP_CIPHER_CTX): Integer; cdecl; external LIB_CRYPTO;
function EVP_CIPHER_CTX_copy(out_: PEVP_CIPHER_CTX; const in_: PEVP_CIPHER_CTX): Integer; cdecl; external LIB_CRYPTO;
function EVP_CIPHER_CTX_cleanup(ctx: PEVP_CIPHER_CTX): Integer; cdecl; external LIB_CRYPTO;
function EVP_CIPHER_CTX_key_length(const ctx: PEVP_CIPHER_CTX): Integer; cdecl; external LIB_CRYPTO;
function EVP_CIPHER_CTX_iv_length(const ctx: PEVP_CIPHER_CTX): Integer; cdecl; external LIB_CRYPTO;
function EVP_CIPHER_CTX_set_key_length(x: PEVP_CIPHER_CTX; keylen: Integer): Integer; cdecl; external LIB_CRYPTO;
function EVP_CIPHER_CTX_set_padding(c: PEVP_CIPHER_CTX; pad: Integer): Integer; cdecl; external LIB_CRYPTO;

function EVP_CIPHER_nid(const cipher: PEVP_CIPHER): Integer; cdecl; external LIB_CRYPTO;
function EVP_CIPHER_iv_length(const cipher: PEVP_CIPHER): Integer; cdecl; external LIB_CRYPTO;
function EVP_CIPHER_block_size(const cipher: PEVP_CIPHER): Integer; cdecl; external LIB_CRYPTO;

function EVP_CIPHER_name(e: PEVP_CIPHER): PAnsiChar; inline;

function EVP_MD_CTX_md(const ctx: PEVP_MD_CTX): PEVP_MD; cdecl; external LIB_CRYPTO;
function EVP_MD_CTX_copy(out_: PEVP_MD_CTX; const in_: PEVP_MD_CTX): Integer; cdecl; external LIB_CRYPTO;

function EVP_MD_CTX_new: PEVP_MD_CTX; cdecl; external LIB_CRYPTO;
function EVP_MD_CTX_reset(ctx: PEVP_MD_CTX): Integer; cdecl; external LIB_CRYPTO;
procedure EVP_MD_CTX_free(ctx: PEVP_MD_CTX); cdecl; external LIB_CRYPTO;

function EVP_MD_CTX_size(e: PEVP_MD_CTX): Integer; inline;
function EVP_MD_CTX_block_size(e: PEVP_MD_CTX): Integer; inline;
function EVP_MD_CTX_type(e: PEVP_MD_CTX): Integer; inline;

function EVP_MD_type(const md: PEVP_MD): Integer; cdecl; external LIB_CRYPTO;
function EVP_MD_size(const md: PEVP_MD): Integer; cdecl; external LIB_CRYPTO;
function EVP_MD_block_size(const md: PEVP_MD): Integer; cdecl; external LIB_CRYPTO;
function EVP_MD_flags(const md: PEVP_MD): LongWord; cdecl; external LIB_CRYPTO;

function EVP_MD_name(e: PEVP_MD): PAnsiChar; inline;
function EVP_MD_nid(e: PEVP_MD): Integer; inline;

function EVP_get_cipherbyname(const name: PAnsiChar): PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_get_digestbyname(const name: PAnsiChar): PEVP_MD; cdecl; external LIB_CRYPTO;

function EVP_BytesToKey(const &type: PEVP_CIPHER; const md: PEVP_MD;
  const salt, data: Pointer; datal, count: Integer; key, iv: PByte): Integer; cdecl; external LIB_CRYPTO;

(* EVP_PKEY - Key Pairs *)

function EVP_PKEY_new: PEVP_PKEY; cdecl; external LIB_CRYPTO;
procedure EVP_PKEY_up_ref(key: PEVP_PKEY); cdecl; external LIB_CRYPTO;
procedure EVP_PKEY_free(key: PEVP_PKEY); cdecl; external LIB_CRYPTO;

function EVP_PKEY_set1_RSA(pkey: PEVP_PKEY; key: PRSA): Integer; cdecl; external LIB_CRYPTO;
function EVP_PKEY_set1_DSA(pkey: PEVP_PKEY; key: PDSA): Integer; cdecl; external LIB_CRYPTO;
function EVP_PKEY_set1_DH(pkey: PEVP_PKEY; key: PDH): Integer; cdecl; external LIB_CRYPTO;
function EVP_PKEY_set1_EC_KEY(pkey: PEVP_PKEY; key: PEC_KEY): Integer; cdecl; external LIB_CRYPTO;

function EVP_PKEY_get1_RSA(pkey: PEVP_PKEY): PRSA; cdecl; external LIB_CRYPTO;
function EVP_PKEY_get1_DSA(pkey: PEVP_PKEY): PDSA; cdecl; external LIB_CRYPTO;
function EVP_PKEY_get1_DH(pkey: PEVP_PKEY): PDH; cdecl; external LIB_CRYPTO;
function EVP_PKEY_get1_EC_KEY(pkey: PEVP_PKEY): PEC_KEY; cdecl; external LIB_CRYPTO;

function EVP_PKEY_get0_RSA(pkey: PEVP_PKEY): PRSA; cdecl; external LIB_CRYPTO;
function EVP_PKEY_get0_DSA(pkey: PEVP_PKEY): PDSA; cdecl; external LIB_CRYPTO;
function EVP_PKEY_get0_DH(pkey: PEVP_PKEY): PDH; cdecl; external LIB_CRYPTO;
function EVP_PKEY_get0_EC_KEY(pkey: PEVP_PKEY): PEC_KEY; cdecl; external LIB_CRYPTO;

function EVP_PKEY_assign_RSA(pkey: PEVP_PKEY; key: PRSA): Integer; cdecl; external LIB_CRYPTO;
function EVP_PKEY_assign_DSA(pkey: PEVP_PKEY; key: PDSA): Integer; cdecl; external LIB_CRYPTO;
function EVP_PKEY_assign_DH(pkey: PEVP_PKEY; key: PDH): Integer; cdecl; external LIB_CRYPTO;
function EVP_PKEY_assign_EC_KEY(pkey: PEVP_PKEY; key: PEC_KEY): Integer; cdecl; external LIB_CRYPTO;

function EVP_PKEY_type(&type: Integer): Integer; cdecl; external LIB_CRYPTO;

(* EVP: EC_KEY - Elliptic Curve Keys *)

type
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

function EC_KEY_new: PEC_KEY; cdecl; external LIB_CRYPTO;
function EC_KEY_get_flags(const key: PEC_KEY): Integer; cdecl; external LIB_CRYPTO;
procedure EC_KEY_set_flags(key: PEC_KEY; flags: Integer); cdecl; external LIB_CRYPTO;
procedure EC_KEY_clear_flags(key: PEC_KEY; flags: Integer); cdecl; external LIB_CRYPTO;
function EC_KEY_new_by_curve_name(nid: Integer): PEC_KEY; cdecl; external LIB_CRYPTO;
procedure EC_KEY_free(key: PEC_KEY); cdecl; external LIB_CRYPTO;
function EC_KEY_copy(dst: PEC_KEY; const src: PEC_KEY): PEC_KEY; cdecl; external LIB_CRYPTO;
function EC_KEY_dup(const src: PEC_KEY): PEC_KEY; cdecl; external LIB_CRYPTO;
function EC_KEY_up_ref(key: PEC_KEY): Integer; cdecl; external LIB_CRYPTO;
function EC_KEY_get0_group(const key: PEC_KEY): PEC_GROUP; cdecl; external LIB_CRYPTO;
function EC_KEY_set_group(key: PEC_KEY; const group: PEC_GROUP): Integer; cdecl; external LIB_CRYPTO;
function EC_KEY_get0_private_key(const key: PEC_KEY): PBIGNUM; cdecl; external LIB_CRYPTO;
function EC_KEY_set_private_key(key: PEC_KEY; const prv: PBIGNUM): Integer; cdecl; external LIB_CRYPTO;
function EC_KEY_get0_public_key(const key: PEC_KEY): PEC_POINT; cdecl; external LIB_CRYPTO;
function EC_KEY_set_public_key(key: PEC_KEY; const pub: PEC_POINT): Integer; cdecl; external LIB_CRYPTO;
function EC_KEY_get_conv_form(const key: PEC_KEY): TPointConversionForm; cdecl; external LIB_CRYPTO;
procedure EC_KEY_set_conv_form(eckey: PEC_KEY; cform: TPointConversionForm); cdecl; external LIB_CRYPTO;
procedure EC_KEY_set_asn1_flag(eckey: PEC_KEY; asn1_flag: Integer); cdecl; external LIB_CRYPTO;
function EC_KEY_precompute_mult(key: PEC_KEY; ctx: PBN_CTX): Integer; cdecl; external LIB_CRYPTO;
function EC_KEY_generate_key(key: PEC_KEY): Integer; cdecl; external LIB_CRYPTO;
function EC_KEY_check_key(const key: PEC_KEY): Integer; cdecl; external LIB_CRYPTO;
function EC_KEY_set_public_key_affine_coordinates(key: PEC_KEY; x: PBIGNUM; y: PBIGNUM): Integer; cdecl; external LIB_CRYPTO;
function EC_KEY_get_method(const key: PEC_KEY): PEC_KEY_METHOD; cdecl; external LIB_CRYPTO;
function EC_KEY_set_method(key: PEC_KEY; const meth: PEC_KEY_METHOD): Integer; cdecl; external LIB_CRYPTO;

(* EVP: MD - Message Digest *)

function EVP_md_null: PEVP_MD; cdecl; external LIB_CRYPTO;
function EVP_md5: PEVP_MD; cdecl; external LIB_CRYPTO;

function EVP_sha1: PEVP_MD; cdecl; external LIB_CRYPTO;
function EVP_sha224: PEVP_MD; cdecl; external LIB_CRYPTO;
function EVP_sha256: PEVP_MD; cdecl; external LIB_CRYPTO;
function EVP_sha384: PEVP_MD; cdecl; external LIB_CRYPTO;
function EVP_sha512: PEVP_MD; cdecl; external LIB_CRYPTO;
function EVP_sha512_224: PEVP_MD; cdecl; external LIB_CRYPTO;
function EVP_sha512_256: PEVP_MD; cdecl; external LIB_CRYPTO;
function EVP_sha3_224: PEVP_MD; cdecl; external LIB_CRYPTO;
function EVP_sha3_256: PEVP_MD; cdecl; external LIB_CRYPTO;
function EVP_sha3_384: PEVP_MD; cdecl; external LIB_CRYPTO;
function EVP_sha3_512: PEVP_MD; cdecl; external LIB_CRYPTO;

function EVP_dss: PEVP_MD; cdecl; external LIB_CRYPTO;
function EVP_dss1: PEVP_MD; cdecl; external LIB_CRYPTO;

function EVP_ecdsa: PEVP_MD; cdecl; external LIB_CRYPTO;

function EVP_mdc2: PEVP_MD; cdecl; external LIB_CRYPTO;

function EVP_ripemd160: PEVP_MD; cdecl; external LIB_CRYPTO;

function EVP_whirlpool: PEVP_MD; cdecl; external LIB_CRYPTO;

(* EVP: CIPHER *)

function EVP_enc_null: PEVP_CIPHER; cdecl; external LIB_CRYPTO;		{ does nothing :-) }

function EVP_des_ecb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_des_ede: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_des_ede3: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_des_ede_ecb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_des_ede3_ecb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_des_cfb64: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_des_cfb1: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_des_cfb8: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_des_ede_cfb64: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_des_ede_cfb1: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_des_ede_cfb8: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_des_ede3_cfb64: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_des_ede3_cfb1: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_des_ede3_cfb8: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_des_ofb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_des_ede_ofb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_des_ede3_ofb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_des_cbc: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_des_ede_cbc: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_des_ede3_cbc: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_desx_cbc: PEVP_CIPHER; cdecl; external LIB_CRYPTO;

{* This should now be supported through the dev_crypto ENGINE. But also, why are
 * rc4 and md5 declarations made here inside a "NO_DES" precompiler branch? *}
function EVP_dev_crypto_des_ede3_cbc: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_dev_crypto_rc4: PEVP_CIPHER; cdecl; external LIB_CRYPTO;

function EVP_dev_crypto_md5: PEVP_MD; cdecl; external LIB_CRYPTO;

function EVP_rc4: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_rc4_40: PEVP_CIPHER; cdecl; external LIB_CRYPTO;

function EVP_idea_ecb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_idea_cfb64: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_idea_ofb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_idea_cbc: PEVP_CIPHER; cdecl; external LIB_CRYPTO;

function EVP_rc2_ecb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_rc2_cbc: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_rc2_40_cbc: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_rc2_64_cbc: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_rc2_cfb64: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_rc2_ofb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;

function EVP_bf_ecb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_bf_cbc: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_bf_cfb64: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_bf_ofb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;

function EVP_cast5_ecb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_cast5_cbc: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_cast5_cfb64: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_cast5_ofb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;

function EVP_rc5_32_12_16_cbc: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_rc5_32_12_16_ecb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_rc5_32_12_16_cfb64: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_rc5_32_12_16_ofb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;

function EVP_aes_128_ecb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_aes_128_cbc: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_aes_128_cfb1: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_aes_128_cfb8: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_aes_128_cfb128: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_aes_128_ofb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_aes_128_ctr: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_aes_192_ecb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_aes_192_cbc: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_aes_192_cfb1: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_aes_192_cfb8: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_aes_192_cfb128: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_aes_192_ofb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_aes_192_ctr: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_aes_256_ecb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_aes_256_cbc: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_aes_256_cfb1: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_aes_256_cfb8: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_aes_256_cfb128: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_aes_256_ofb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_aes_256_ctr: PEVP_CIPHER; cdecl; external LIB_CRYPTO;

function EVP_camellia_128_ecb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_camellia_128_cbc: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_camellia_128_cfb1: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_camellia_128_cfb8: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_camellia_128_cfb128: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_camellia_128_ofb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_camellia_192_ecb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_camellia_192_cbc: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_camellia_192_cfb1: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_camellia_192_cfb8: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_camellia_192_cfb128: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_camellia_192_ofb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_camellia_256_ecb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_camellia_256_cbc: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_camellia_256_cfb1: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_camellia_256_cfb8: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_camellia_256_cfb128: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_camellia_256_ofb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;

function EVP_seed_ecb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_seed_cbc: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_seed_cfb128: PEVP_CIPHER; cdecl; external LIB_CRYPTO;
function EVP_seed_ofb: PEVP_CIPHER; cdecl; external LIB_CRYPTO;

function EVP_add_cipher(const cipher: PEVP_CIPHER): Integer; cdecl; external LIB_CRYPTO;
function EVP_add_digest(const digest: PEVP_MD): Integer; cdecl; external LIB_CRYPTO;

procedure EVP_cleanup; cdecl; external LIB_CRYPTO;

// Crypto

procedure OPENSSL_cleanse(ptr: Pointer; len: Cardinal); cdecl; external LIB_CRYPTO;

// Objects

function OBJ_nid2sn(n: Integer): PAnsiChar; cdecl; external LIB_CRYPTO;

(******************************************************************************
 * BIO
(******************************************************************************)

const
  BIO_NOCLOSE = 0;
  BIO_CLOSE   = 1;

{ These are used in the following macros and are passed to BIO_ctrl() }

  BIO_CTRL_RESET        = 1;  { opt - rewind/zero etc }
  BIO_CTRL_EOF          = 2;  { opt - are we at the eof }
  BIO_CTRL_INFO	        = 3;  { opt - extra tit-bits }
  BIO_CTRL_SET          = 4;  { man - set the 'IO' type }
  BIO_CTRL_GET          = 5;  { man - get the 'IO' type }
  BIO_CTRL_PUSH	        = 6;  { opt - internal, used to signify change }
  BIO_CTRL_POP          = 7;  { opt - internal, used to signify change }
  BIO_CTRL_GET_CLOSE    = 8;  { man - set the 'close' on free }
  BIO_CTRL_SET_CLOSE    = 9;  { man - set the 'close' on free }
  BIO_CTRL_PENDING      = 10; { opt - is their more data buffered }
  BIO_CTRL_FLUSH        = 11; { opt - 'flush' buffered output }
  BIO_CTRL_DUP          = 12; { man - extra stuff for 'duped' BIO }
  BIO_CTRL_WPENDING	    = 13; { opt - number of bytes still to write }
  BIO_CTRL_SET_CALLBACK = 14; { opt - set callback function }
  BIO_CTRL_GET_CALLBACK = 15; { opt - set callback function }

  BIO_CTRL_SET_FILENAME = 30; { BIO_s_file special }

  BIO_FP_READ   = $02;
  BIO_FP_WRITE  = $04;
  BIO_FP_APPEND = $08;
  BIO_FP_TEXT   = $10;

  BIO_C_SET_CONNECT                 = 100;
  BIO_C_DO_STATE_MACHINE            = 101;
  BIO_C_SET_NBIO                    = 102;
  BIO_C_SET_PROXY_PARAM             = 103;
  BIO_C_SET_FD                      = 104;
  BIO_C_GET_FD                      = 105;
  BIO_C_SET_FILE_PTR                = 106;
  BIO_C_GET_FILE_PTR                = 107;
  BIO_C_SET_FILENAME                = 108;
  BIO_C_SET_SSL	                    = 109;
  BIO_C_GET_SSL                     = 110;
  BIO_C_SET_MD                      = 111;
  BIO_C_GET_MD                      = 112;
  BIO_C_GET_CIPHER_STATUS	          = 113;
  BIO_C_SET_BUF_MEM	                = 114;
  BIO_C_GET_BUF_MEM_PTR	            = 115;
  BIO_C_GET_BUFF_NUM_LINES          = 116;
  BIO_C_SET_BUFF_SIZE	              = 117;
  BIO_C_SET_ACCEPT                  = 118;
  BIO_C_SSL_MODE                    = 119;
  BIO_C_GET_MD_CTX                  = 120;
  BIO_C_GET_PROXY_PARAM             = 121;
  BIO_C_SET_BUFF_READ_DATA          = 122; { data to read first }
  BIO_C_GET_CONNECT                 = 123;
  BIO_C_GET_ACCEPT                  = 124;
  BIO_C_SET_SSL_RENEGOTIATE_BYTES   = 125;
  BIO_C_GET_SSL_NUM_RENEGOTIATES    = 126;
  BIO_C_SET_SSL_RENEGOTIATE_TIMEOUT	= 127;
  BIO_C_FILE_SEEK                   = 128;
  BIO_C_GET_CIPHER_CTX              = 129;
  BIO_C_SET_BUF_MEM_EOF_RETURN      = 130; { return end of input value }
  BIO_C_SET_BIND_MODE	              = 131;
  BIO_C_GET_BIND_MODE	              = 132;
  BIO_C_FILE_TELL                   = 133;
  BIO_C_GET_SOCKS                   = 134;
  BIO_C_SET_SOCKS                   = 135;

  BIO_C_SET_WRITE_BUF_SIZE          = 136; { for BIO_s_bio }
  BIO_C_GET_WRITE_BUF_SIZE          = 137;
  BIO_C_MAKE_BIO_PAIR               = 138;
  BIO_C_DESTROY_BIO_PAIR            = 139;
  BIO_C_GET_WRITE_GUARANTEE         = 140;
  BIO_C_GET_READ_REQUEST            = 141;
  BIO_C_SHUTDOWN_WR                 = 142;
  BIO_C_NREAD0                      = 143;
  BIO_C_NREAD                       = 144;
  BIO_C_NWRITE0                     = 145;
  BIO_C_NWRITE                      = 146;
  BIO_C_RESET_READ_REQUEST          = 147;
  BIO_C_SET_MD_CTX                  = 148;

  BIO_C_SET_PREFIX                  = 149;
  BIO_C_GET_PREFIX                  = 150;
  BIO_C_SET_SUFFIX                  = 151;
  BIO_C_GET_SUFFIX                  = 152;

  BIO_C_SET_EX_ARG                  = 153;
  BIO_C_GET_EX_ARG                  = 154;

  BIO_FLAGS_BASE64_NO_NL = $100;
  BIO_FLAGS_MEM_RDONLY   = $200;

type
  PBIO = Pointer;
  PBIO_METHOD = Pointer;

function BIO_new(&type: PBIO_METHOD): PBIO; cdecl; external LIB_CRYPTO;
procedure BIO_free(a: PBIO); cdecl; external LIB_CRYPTO;
procedure BIO_free_all(a: PBIO); cdecl; external LIB_CRYPTO;

procedure BIO_set_flags(b: PBIO; flags: Integer); cdecl; external LIB_CRYPTO;
function BIO_new_mem_buf(buf: Pointer; len: Integer): PBIO; cdecl; external LIB_CRYPTO;
function BIO_push(b: PBIO; append: PBIO): PBIO; cdecl; external LIB_CRYPTO;
function BIO_write(b: PBIO; const data: Pointer; len: Integer): Integer; cdecl; external LIB_CRYPTO;
function BIO_read(b: PBIO; data: Pointer; len: Integer): Integer; cdecl; external LIB_CRYPTO;
function BIO_ctrl(bp: PBIO; cmd: Integer; larg: Longint; parg: Pointer): LongInt; cdecl; external LIB_CRYPTO;

function BIO_flush(b: PBIO): Integer; inline;
function BIO_get_mem_ptr(b: PBIO; ptr: Pointer): LongInt; inline;

function BIO_s_mem: PBIO_METHOD; cdecl; external LIB_CRYPTO;
function BIO_s_file: PBIO_METHOD; cdecl; external LIB_CRYPTO;
function BIO_s_socket: PBIO_METHOD; cdecl; external LIB_CRYPTO;
function BIO_s_connect: PBIO_METHOD; cdecl; external LIB_CRYPTO;
function BIO_s_accept: PBIO_METHOD; cdecl; external LIB_CRYPTO;
function BIO_s_fd: PBIO_METHOD; cdecl; external LIB_CRYPTO;
function BIO_s_bio: PBIO_METHOD; cdecl; external LIB_CRYPTO;
function BIO_s_null: PBIO_METHOD; cdecl; external LIB_CRYPTO;
function BIO_f_null: PBIO_METHOD; cdecl; external LIB_CRYPTO;
function BIO_f_buffer: PBIO_METHOD; cdecl; external LIB_CRYPTO;
function BIO_f_nbio_test: PBIO_METHOD; cdecl; external LIB_CRYPTO;
function BIO_s_datagram: PBIO_METHOD; cdecl; external LIB_CRYPTO;
function BIO_s_datagram_sctp: PBIO_METHOD; cdecl; external LIB_CRYPTO;

function BIO_f_md: PBIO_METHOD; cdecl; external LIB_CRYPTO;
function BIO_f_base64: PBIO_METHOD; cdecl; external LIB_CRYPTO;
function BIO_f_cipher: PBIO_METHOD; cdecl; external LIB_CRYPTO;
function BIO_f_reliable: PBIO_METHOD; cdecl; external LIB_CRYPTO;

procedure BIO_set_cipher(b: PBIO; const c: PEVP_CIPHER; const k: PByte; const i: PByte; enc: Integer); cdecl; external LIB_CRYPTO;

function BIO_new_file(const filename: PAnsiChar; const mode: PAnsiChar): PBIO; cdecl; external LIB_CRYPTO;

function BIO_read_filename(b: PBIO; name: PAnsiChar): LongInt; inline;
function BIO_write_filename(b: PBIO; name: PAnsiChar): LongInt; inline;
function BIO_append_filename(b: PBIO; name: PAnsiChar): LongInt; inline;
function BIO_rw_filename(b: PBIO; name: PAnsiChar): LongInt; inline;

function BIO_set_md(b: PBIO; md: PEVP_MD): LongInt; inline;
function BIO_get_md(b: PBIO; md: PPEVP_MD): LongInt; inline;
function BIO_get_md_ctx(b: PBIO; ctx: PPEVP_MD_CTX): LongInt; inline;
function BIO_set_md_ctx(b: PBIO; ctx: PEVP_MD_CTX): LongInt; inline;

function BIO_get_close(b: PBIO): LongInt; inline;
function BIO_set_close(b: PBIO; flags: LongInt): Integer; inline;

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

  SSL_CTRL_SET_TMP_DH                      = 3;

  SSL_CTRL_SET_TMP_ECDH                    = 4;
  SSL_CTRL_SET_TMP_DH_CB                   = 6;
  SSL_CTRL_GET_CLIENT_CERT_REQUEST         = 9;
  SSL_CTRL_GET_NUM_RENEGOTIATIONS          = 10;
  SSL_CTRL_CLEAR_NUM_RENEGOTIATIONS        = 11;
  SSL_CTRL_GET_TOTAL_RENEGOTIATIONS        = 12;
  SSL_CTRL_GET_FLAGS                       = 13;
  SSL_CTRL_EXTRA_CHAIN_CERT                = 14;
  SSL_CTRL_SET_MSG_CALLBACK                = 15;
  SSL_CTRL_SET_MSG_CALLBACK_ARG            = 16;
(* only applies to datagram connections *)
  SSL_CTRL_SET_MTU                         = 17;
(* Stats *)
  SSL_CTRL_SESS_NUMBER                     = 20;
  SSL_CTRL_SESS_CONNECT                    = 21;
  SSL_CTRL_SESS_CONNECT_GOOD               = 22;
  SSL_CTRL_SESS_CONNECT_RENEGOTIATE        = 23;
  SSL_CTRL_SESS_ACCEPT                     = 24;
  SSL_CTRL_SESS_ACCEPT_GOOD                = 25;
  SSL_CTRL_SESS_ACCEPT_RENEGOTIATE         = 26;
  SSL_CTRL_SESS_HIT                        = 27;
  SSL_CTRL_SESS_CB_HIT                     = 28;
  SSL_CTRL_SESS_MISSES                     = 29;
  SSL_CTRL_SESS_TIMEOUTS                   = 30;
  SSL_CTRL_SESS_CACHE_FULL                 = 31;
  SSL_CTRL_OPTIONS                         = 32;
  SSL_CTRL_MODE                            = 33;
  SSL_CTRL_GET_READ_AHEAD                  = 40;
  SSL_CTRL_SET_READ_AHEAD                  = 41;
  SSL_CTRL_SET_SESS_CACHE_SIZE             = 42;
  SSL_CTRL_GET_SESS_CACHE_SIZE             = 43;
  SSL_CTRL_SET_SESS_CACHE_MODE             = 44;
  SSL_CTRL_GET_SESS_CACHE_MODE             = 45;
  SSL_CTRL_GET_MAX_CERT_LIST               = 50;
  SSL_CTRL_SET_MAX_CERT_LIST               = 51;
  SSL_CTRL_SET_MAX_SEND_FRAGMENT           = 52;
(* see tls1.h for macros based on these *)
  SSL_CTRL_SET_TLSEXT_SERVERNAME_CB        = 53;
  SSL_CTRL_SET_TLSEXT_SERVERNAME_ARG       = 54;
  SSL_CTRL_SET_TLSEXT_HOSTNAME             = 55;
  SSL_CTRL_SET_TLSEXT_DEBUG_CB             = 56;
  SSL_CTRL_SET_TLSEXT_DEBUG_ARG            = 57;
  SSL_CTRL_GET_TLSEXT_TICKET_KEYS          = 58;
  SSL_CTRL_SET_TLSEXT_TICKET_KEYS          = 59;
(* SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT        = 60; *)
(* SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT_CB     = 61; *)
(* SSL_CTRL_SET_TLSEXT_OPAQUE_PRF_INPUT_CB_ARG = 62; *)
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB        = 63;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_CB_ARG    = 64;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_TYPE      = 65;
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_EXTS      = 66;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_EXTS      = 67;
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_IDS       = 68;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_IDS       = 69;
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_OCSP_RESP = 70;
  SSL_CTRL_SET_TLSEXT_STATUS_REQ_OCSP_RESP = 71;
  SSL_CTRL_SET_TLSEXT_TICKET_KEY_CB        = 72;
  SSL_CTRL_SET_TLS_EXT_SRP_USERNAME_CB     = 75;
  SSL_CTRL_SET_SRP_VERIFY_PARAM_CB         = 76;
  SSL_CTRL_SET_SRP_GIVE_CLIENT_PWD_CB      = 77;
  SSL_CTRL_SET_SRP_ARG                     = 78;
  SSL_CTRL_SET_TLS_EXT_SRP_USERNAME        = 79;
  SSL_CTRL_SET_TLS_EXT_SRP_STRENGTH        = 80;
  SSL_CTRL_SET_TLS_EXT_SRP_PASSWORD        = 81;
  DTLS_CTRL_GET_TIMEOUT                    = 73;
  DTLS_CTRL_HANDLE_TIMEOUT                 = 74;
  SSL_CTRL_GET_RI_SUPPORT                  = 76;
  SSL_CTRL_CLEAR_OPTIONS                   = 77;
  SSL_CTRL_CLEAR_MODE                      = 78;
  SSL_CTRL_SET_NOT_RESUMABLE_SESS_CB       = 79;
  SSL_CTRL_GET_EXTRA_CHAIN_CERTS           = 82;
  SSL_CTRL_CLEAR_EXTRA_CHAIN_CERTS         = 83;
  SSL_CTRL_CHAIN                           = 88;
  SSL_CTRL_CHAIN_CERT                      = 89;
  SSL_CTRL_GET_GROUPS                      = 90;
  SSL_CTRL_SET_GROUPS                      = 91;
  SSL_CTRL_SET_GROUPS_LIST                 = 92;
  SSL_CTRL_GET_SHARED_GROUP                = 93;
  SSL_CTRL_SET_SIGALGS                     = 97;
  SSL_CTRL_SET_SIGALGS_LIST                = 98;
  SSL_CTRL_CERT_FLAGS                      = 99;
  SSL_CTRL_CLEAR_CERT_FLAGS                = 100;
  SSL_CTRL_SET_CLIENT_SIGALGS              = 101;
  SSL_CTRL_SET_CLIENT_SIGALGS_LIST         = 102;
  SSL_CTRL_GET_CLIENT_CERT_TYPES           = 103;
  SSL_CTRL_SET_CLIENT_CERT_TYPES           = 104;
  SSL_CTRL_BUILD_CERT_CHAIN                = 105;
  SSL_CTRL_SET_VERIFY_CERT_STORE           = 106;
  SSL_CTRL_SET_CHAIN_CERT_STORE            = 107;
  SSL_CTRL_GET_PEER_SIGNATURE_NID          = 108;
  SSL_CTRL_GET_PEER_TMP_KEY                = 109;
  SSL_CTRL_GET_RAW_CIPHERLIST              = 110;
  SSL_CTRL_GET_EC_POINT_FORMATS            = 111;
  SSL_CTRL_GET_CHAIN_CERTS                 = 115;
  SSL_CTRL_SELECT_CURRENT_CERT             = 116;
  SSL_CTRL_SET_CURRENT_CERT                = 117;
  SSL_CTRL_SET_DH_AUTO                     = 118;
  DTLS_CTRL_SET_LINK_MTU                   = 120;
  DTLS_CTRL_GET_LINK_MIN_MTU               = 121;
  SSL_CTRL_GET_EXTMS_SUPPORT               = 122;
  SSL_CTRL_SET_MIN_PROTO_VERSION           = 123;
  SSL_CTRL_SET_MAX_PROTO_VERSION           = 124;
  SSL_CTRL_SET_SPLIT_SEND_FRAGMENT         = 125;
  SSL_CTRL_SET_MAX_PIPELINES               = 126;
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_TYPE      = 127;
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_CB        = 128;
  SSL_CTRL_GET_TLSEXT_STATUS_REQ_CB_ARG    = 129;
  SSL_CTRL_GET_MIN_PROTO_VERSION           = 130;
  SSL_CTRL_GET_MAX_PROTO_VERSION           = 131;
  SSL_CTRL_GET_SIGNATURE_NID               = 132;
  SSL_CTRL_GET_TMP_KEY                     = 133;

  SSL_CERT_SET_FIRST                       = 1;
  SSL_CERT_SET_NEXT                        = 2;
  SSL_CERT_SET_SERVER                      = 3;

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

const
  SSL_ERROR_NONE                  = 0;
  SSL_ERROR_SSL                   = 1;
  SSL_ERROR_WANT_READ             = 2;
  SSL_ERROR_WANT_WRITE            = 3;
  SSL_ERROR_WANT_X509_LOOKUP      = 4;
  SSL_ERROR_SYSCALL               = 5; {* look at error stack/return
                                        * value/errno *}
  SSL_ERROR_ZERO_RETURN           = 6;
  SSL_ERROR_WANT_CONNECT          = 7;
  SSL_ERROR_WANT_ACCEPT           = 8;
  SSL_ERROR_WANT_ASYNC            = 9;
  SSL_ERROR_WANT_ASYNC_JOB        = 10;
  SSL_ERROR_WANT_CLIENT_HELLO_CB  = 11;

const

  (*
   * Allow SSL_write(..., n) to return r with 0 < r < n (i.e. report success
   * when just a single record has been written):
   *)
  SSL_MODE_ENABLE_PARTIAL_WRITE = $00000001;
  (*
   * Make it possible to retry SSL_write() with changed buffer location (buffer
   * contents must stay the same!); this is not the default to avoid the
   * misconception that non-blocking SSL_write() behaves like non-blocking
   * write():
   *)
  SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER = $00000002;
  (*
   * Never bother the application with retries if the transport is blocking:
   *)
  SSL_MODE_AUTO_RETRY = $00000004;
  (*
   * Don't attempt to automatically build certificate chain
   *)
  SSL_MODE_NO_AUTO_CHAIN = $00000008;
  (*
   * Save RAM by releasing read and write buffers when they're empty. (SSL3 and
   * TLS only.) Released buffers are freed.
   *)
  SSL_MODE_RELEASE_BUFFERS = $00000010;
  (*
   * Send the current time in the Random fields of the ClientHello and
   * ServerHello records for compatibility with hypothetical implementations
   * that require it.
   *)
  SSL_MODE_SEND_CLIENTHELLO_TIME = $00000020;
  SSL_MODE_SEND_SERVERHELLO_TIME = $00000040;
  (*
   * Send TLS_FALLBACK_SCSV in the ClientHello. To be set only by applications
   * that reconnect with a downgraded protocol version; see
   * draft-ietf-tls-downgrade-scsv-00 for details. DO NOT ENABLE THIS if your
   * application attempts a normal handshake. Only use this in explicit
   * fallback retries, following the guidance in
   * draft-ietf-tls-downgrade-scsv-00.
   *)
  SSL_MODE_SEND_FALLBACK_SCSV = $00000080;
  (*
   * Support Asynchronous operation
   *)
  SSL_MODE_ASYNC = $00000100;
  (*
   * Don't use the kernel TLS data-path for sending.
   *)
  SSL_MODE_NO_KTLS_TX = $00000200;
  (*
   * When using DTLS/SCTP, include the terminating zero in the label
   * used for computing the endpoint-pair shared secret. Required for
   * interoperability with implementations having this bug like these
   * older version of OpenSSL:
   * - OpenSSL 1.0.0 series
   * - OpenSSL 1.0.1 series
   * - OpenSSL 1.0.2 series
   * - OpenSSL 1.1.0 series
   * - OpenSSL 1.1.1 and 1.1.1a
   *)
  SSL_MODE_DTLS_SCTP_LABEL_LENGTH_BUG = $00000400;
  (*
   * Don't use the kernel TLS data-path for receiving.
   *)
  SSL_MODE_NO_KTLS_RX = $00000800;

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

  PX509_STORE = Pointer;

  PPKCS7 = Pointer;
  PPPKCS7 = ^PPKCS7;

  { General-purpose version-flexible SSL/TLS methods for OpenSSL 1.1.x }
  function TLS_method: PSSL_METHOD; cdecl; external LIB_SSL;
  function TLS_server_method: PSSL_METHOD; cdecl; external LIB_SSL;
  function TLS_client_method: PSSL_METHOD; cdecl; external LIB_SSL;

  function SSL_CTX_new(const meth: PSSL_METHOD): PSSL_CTX; cdecl; external LIB_SSL;
  procedure SSL_CTX_free(arg0: PSSL_CTX); cdecl; external LIB_SSL;
  function SSL_CTX_set_cipher_list(arg0: PSSL_CTX; str: PAnsiChar): Integer; cdecl; external LIB_SSL;
  function SSL_CTX_ctrl(ctx: PSSL_CTX; cmd: Integer; larg: LongInt; parg: Pointer): LongInt; cdecl; external LIB_SSL;
  function SSL_CTX_use_PrivateKey(ctx: PSSL_CTX; pkey: PEVP_PKEY):Integer; cdecl; external LIB_SSL;
  function SSL_CTX_use_RSAPrivateKey(ctx: PSSL_CTX; rsa: PRSA):Integer; cdecl; external LIB_SSL;
  function SSL_CTX_use_RSAPrivateKey_file(ctx: PSSL_CTX; const filename: PAnsiChar; typ: Integer):Integer; cdecl; external LIB_SSL;
  function SSL_CTX_use_certificate(ctx: PSSL_CTX; x: PX509):Integer; cdecl; external LIB_SSL;
  function SSL_CTX_use_certificate_file(ctx: PSSL_CTX; const filename: PAnsiChar; typ: Integer):Integer; cdecl; external LIB_SSL;
  function SSL_CTX_use_certificate_chain_file(ctx: PSSL_CTX; const _file: PAnsiChar):Integer; cdecl; external LIB_SSL;
  procedure SSL_CTX_set_default_passwd_cb(ctx: PSSL_CTX; cb: Pointer); cdecl; external LIB_SSL;
  procedure SSL_CTX_set_default_passwd_cb_userdata(ctx: PSSL_CTX; u: Pointer); cdecl; external LIB_SSL;
  function SSL_CTX_load_verify_locations(ctx: PSSL_CTX; const CAfile: PAnsiChar; const CApath: PAnsiChar):Integer; cdecl; external LIB_SSL;
  procedure SSL_CTX_set_cert_verify_callback(ctx: PSSL_CTX; cb: Pointer; arg: Pointer); cdecl; external LIB_SSL;
  procedure SSL_CTX_set_verify(ctx: PSSL_CTX; mode: Integer; arg2: Pointer); cdecl; external LIB_SSL;
  procedure SSL_CTX_set_cert_store(ctx: PSSL_CTX; store: PX509_STORE); cdecl; external LIB_CRYPTO;

  function SSL_CTX_set_options(ctx: PSSL_CTX; op: LongInt): LongInt; inline;
  function SSL_CTX_clear_options(ctx: PSSL_CTX; op: LongInt): LongInt; inline;
  function SSL_CTX_get_options(ctx: PSSL_CTX): LongInt; inline;

  function SSL_new(ctx: PSSL_CTX):PSSL; cdecl; external LIB_SSL;
  procedure SSL_free(ssl: PSSL); cdecl; external LIB_SSL;
  function SSL_set_fd(s: PSSL; fd: Longint): Integer; cdecl; external LIB_SSL;
  function SSL_ctrl(ssl: PSSL; cmd: Integer; larg: Integer; parg: Pointer): Integer; cdecl; external LIB_SSL;
  function SSL_connect(ssl: PSSL):Integer; cdecl; external LIB_SSL;
  function SSL_accept(ssl: PSSL):Integer; cdecl; external LIB_SSL;
  function SSL_shutdown(ssl: PSSL):Integer; cdecl; external LIB_SSL;
  function SSL_read(ssl: PSSL; buf: Pointer; num: Integer): Integer; cdecl; external LIB_SSL;
  function SSL_write(ssl: PSSL; const buf: Pointer; num: Integer): Integer; cdecl; external LIB_SSL;
  function SSL_get_error(ssl: PSSL; ret: Integer): Integer; cdecl; external LIB_SSL;
  function SSL_pending(const ssl: PSSL): Integer; cdecl; external LIB_SSL;
  function SSL_has_pending(const ssl: PSSL): Integer; cdecl; external LIB_SSL;
  function SSL_get_peer_certificate(ssl: PSSL): PX509; cdecl; external LIB_SSL;

  function SSL_set_mode(ctx: PSSL; mode: LongInt): LongInt; inline;
  function SSL_get_mode(ctx: PSSL): LongInt; inline;

  procedure X509_free(x509: PX509); cdecl; external LIB_CRYPTO;
  function X509_NAME_oneline(name: PX509_NAME; buf: PAnsiChar; size: Integer): PAnsiChar; cdecl; external LIB_CRYPTO;
  function X509_get_subject_name(x509: PX509): PX509_NAME; cdecl; external LIB_CRYPTO;
  function X509_get_issuer_name(x509: PX509): PX509_NAME; cdecl; external LIB_CRYPTO;

  function X509_STORE_add_cert(ctx: PX509_STORE; x: PX509): Integer; cdecl; external LIB_SSL;

const
  SSL3_VERSION   = $0300;
  TLS1_VERSION   = $0301;
  TLS1_1_VERSION = $0302;
  TLS1_2_VERSION = $0303;
  TLS1_3_VERSION = $0304;

  function SSL_CTX_set_min_proto_version(ctx: PSSL_CTX; version: Integer): Integer; inline;
  function SSL_CTX_set_max_proto_version(ctx: PSSL_CTX; version: Integer): Integer; inline;
  function SSL_CTX_get_min_proto_version(ctx: PSSL_CTX): Integer; inline;
  function SSL_CTX_get_max_proto_version(ctx: PSSL_CTX): Integer; inline;

(******************************************************************************
 * PEM
(******************************************************************************)
type
  pem_password_cb = function(buf: PAnsiChar; size: Integer; rwflag: Integer; u: Pointer): Integer; cdecl;

function PEM_read_bio_PrivateKey(bp: PBIO; x: PPEVP_PKEY; cb: pem_password_cb; u: Pointer): PEVP_PKEY; cdecl; external LIB_CRYPTO;
function PEM_write_bio_PrivateKey(bp: PBIO; x: PEVP_PKEY; const enc: PEVP_CIPHER; kstr: PByte; klen: Integer; cb: pem_password_cb; u: Pointer): Integer; cdecl; external LIB_CRYPTO;
function PEM_write_bio_PKCS8PrivateKey(bp: PBIO; x: PEVP_PKEY; const enc: PEVP_CIPHER; kstr: PByte; klen: Integer; cb: pem_password_cb; u: Pointer): Integer; cdecl; external LIB_CRYPTO;
function PEM_write_bio_PKCS8PrivateKey_nid(bp: PBIO; x: PEVP_PKEY; nid: Integer; kstr: PByte; klen: Integer; cb: pem_password_cb; u: Pointer): Integer; cdecl; external LIB_CRYPTO;
function PEM_read_bio_PUBKEY(bp: PBIO; x: PPEVP_PKEY; cb: pem_password_cb; u: Pointer): PEVP_PKEY; cdecl; external LIB_CRYPTO;
function PEM_write_bio_PUBKEY(bp: PBIO; x: PEVP_PKEY): Integer; cdecl; external LIB_CRYPTO;
function PEM_read_bio_RSAPrivateKey(bp: PBIO; x: PPRSA; cb: pem_password_cb; u: Pointer): PRSA; cdecl; external LIB_CRYPTO;
function PEM_write_bio_RSAPrivateKey(bp: PBIO; x: PRSA; const enc: PEVP_CIPHER; kstr: PByte; klen: Integer; cb: pem_password_cb; u: Pointer): Integer; cdecl; external LIB_CRYPTO;
function PEM_read_bio_RSAPublicKey(bp: PBIO; x: PPRSA; cb: pem_password_cb; u: Pointer): PRSA; cdecl; external LIB_CRYPTO;
function PEM_write_bio_RSAPublicKey(bp: PBIO; x: PRSA): Integer; cdecl; external LIB_CRYPTO;
function PEM_read_bio_RSA_PUBKEY(bp: PBIO; x: PPRSA; cb: pem_password_cb; u: Pointer): PRSA; cdecl; external LIB_CRYPTO;
function PEM_write_bio_RSA_PUBKEY(bp: PBIO; x: PRSA): Integer; cdecl; external LIB_CRYPTO;
function PEM_read_bio_DSAPrivateKey(bp: PBIO; x: PPDSA; cb: pem_password_cb; u: Pointer): PDSA; cdecl; external LIB_CRYPTO;
function PEM_write_bio_DSAPrivateKey(bp: PBIO; x: PDSA; const enc: PEVP_CIPHER; kstr: PByte; klen: Integer; cb: pem_password_cb; u: Pointer): Integer; cdecl; external LIB_CRYPTO;
function PEM_read_bio_DSA_PUBKEY(bp: PBIO; x: PPDSA; cb: pem_password_cb; u: Pointer): PDSA; cdecl; external LIB_CRYPTO;
function PEM_write_bio_DSA_PUBKEY(bp: PBIO; x: PDSA): Integer; cdecl; external LIB_CRYPTO;
function PEM_read_bio_DSAparams(bp: PBIO; x: PPDSA; cb: pem_password_cb; u: Pointer): PDSA; cdecl; external LIB_CRYPTO;
function PEM_write_bio_DSAparams(bp: PBIO; x: PDSA): Integer; cdecl; external LIB_CRYPTO;
function PEM_read_bio_DHparams(bp: PBIO; x: PPDH; cb: pem_password_cb; u: Pointer): PDH; cdecl; external LIB_CRYPTO;
function PEM_write_bio_DHparams(bp: PBIO; x: PDH): Integer; cdecl; external LIB_CRYPTO;
function PEM_read_bio_X509(bp: PBIO; x: PPX509; cb: pem_password_cb; u: Pointer): PX509; cdecl; external LIB_CRYPTO;
function PEM_write_bio_X509(bp: PBIO; x: PX509): Integer; cdecl; external LIB_CRYPTO;
function PEM_read_bio_X509_AUX(bp: PBIO; x: PPX509; cb: pem_password_cb; u: Pointer): PX509; cdecl; external LIB_CRYPTO;
function PEM_write_bio_X509_AUX(bp: PBIO; x: PX509): Integer; cdecl; external LIB_CRYPTO;
function PEM_read_bio_X509_REQ(bp: PBIO; x: PPX509_REQ; cb: pem_password_cb; u: Pointer): PX509_REQ; cdecl; external LIB_CRYPTO;
function PEM_write_bio_X509_REQ(bp: PBIO; x: PX509_REQ): Integer; cdecl; external LIB_CRYPTO;
function PEM_write_bio_X509_REQ_NEW(bp: PBIO; x: PX509_REQ): Integer; cdecl; external LIB_CRYPTO;
function PEM_read_bio_X509_CRL(bp: PBIO; x: PPX509_CRL; cb: pem_password_cb; u: Pointer): PX509_CRL; cdecl; external LIB_CRYPTO;
function PEM_write_bio_X509_CRL(bp: PBIO; x: PX509_CRL): Integer; cdecl; external LIB_CRYPTO;
function PEM_read_bio_PKCS7(bp: PBIO; x: PPPKCS7; cn: pem_password_cb; u: Pointer): PPKCS7; cdecl; external LIB_CRYPTO;
function PEM_write_bio_PKCS7(bp: PBIO; x: PPKCS7): Integer; cdecl; external LIB_CRYPTO;

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

function RAND_poll: Integer; cdecl; external LIB_CRYPTO;

procedure RAND_seed(const buf: Pointer; num: Integer); cdecl; external LIB_CRYPTO;
procedure RAND_add(const buf: Pointer; num: Integer; entropy: Double); cdecl; external LIB_CRYPTO;

function RAND_status: Integer; cdecl; external LIB_CRYPTO;

function RAND_bytes(buf: PByte; num: Integer): Integer; cdecl; external LIB_CRYPTO;
function RAND_priv_bytes(buf: PByte; num: Integer): Integer; cdecl; external LIB_CRYPTO;

procedure RAND_set_rand_method(const meth: PRAND_METHOD); cdecl; external LIB_CRYPTO;
function RAND_get_rand_method: PRAND_METHOD; cdecl; external LIB_CRYPTO;
function RAND_OpenSSL: PRAND_METHOD; cdecl; external LIB_CRYPTO;

(******************************************************************************
 * ERR
(******************************************************************************)

function ERR_get_error: LongWord; cdecl; external LIB_CRYPTO;

procedure ERR_clear_error; cdecl; external LIB_CRYPTO;

function ERR_error_string(e: LongWord; buf: PAnsiChar): PAnsiChar; cdecl; external LIB_CRYPTO;
procedure ERR_error_string_n(e: LongWord; buf: PAnsiChar; len: Cardinal); cdecl; external LIB_CRYPTO;
function ERR_lib_error_string(e: LongWord): PAnsiChar; cdecl; external LIB_CRYPTO;
function ERR_func_error_string(e: LongWord): PAnsiChar; cdecl; external LIB_CRYPTO;
function ERR_reason_error_string(e: LongWord): PAnsiChar; cdecl; external LIB_CRYPTO;

implementation

(* SSL_CTX *)

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

function SSL_CTX_set_min_proto_version(ctx: PSSL_CTX; version: Integer): Integer;
begin
  Result := SSL_CTX_ctrl(ctx, SSL_CTRL_SET_MIN_PROTO_VERSION, version, nil);
end;

function SSL_CTX_set_max_proto_version(ctx: PSSL_CTX; version: Integer): Integer;
begin
  Result := SSL_CTX_ctrl(ctx, SSL_CTRL_SET_MAX_PROTO_VERSION, version, nil);
end;

function SSL_CTX_get_min_proto_version(ctx: PSSL_CTX): Integer;
begin
  Result := SSL_CTX_ctrl(ctx, SSL_CTRL_GET_MIN_PROTO_VERSION, 0, nil);
end;

function SSL_CTX_get_max_proto_version(ctx: PSSL_CTX): Integer;
begin
Result := SSL_CTX_ctrl(ctx, SSL_CTRL_GET_MAX_PROTO_VERSION, 0, nil);
end;

(* SSL *)

function SSL_set_mode(ctx: PSSL; mode: LongInt): LongInt;
begin
  Result := SSL_ctrl(ctx, SSL_CTRL_MODE, mode, nil);
end;

function SSL_get_mode(ctx: PSSL): LongInt;
begin
  Result := SSL_ctrl(ctx, SSL_CTRL_MODE, 0, nil);
end;

(* EVP_CIPHER *)

function EVP_CIPHER_name(e: PEVP_CIPHER): PAnsiChar;
begin
	Result := OBJ_nid2sn(EVP_CIPHER_nid(e));
end;

(* EVP_MD *)

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

(* EVP *)

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

(* BIO *)

function BIO_flush(b: PBIO): Integer;
begin
  Result := Integer(BIO_ctrl(b, BIO_CTRL_FLUSH, 0, nil));
end;

function BIO_get_mem_ptr(b: PBIO; ptr: Pointer): LongInt;
begin
  Result := BIO_ctrl(b, BIO_C_GET_BUF_MEM_PTR, 0, ptr);
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

function BIO_get_close(b: PBIO): LongInt;
begin
  Result := BIO_ctrl(b, BIO_CTRL_GET_CLOSE, 0, nil);
end;

function BIO_set_close(b: PBIO; flags: LongInt): Integer;
begin
  Result := BIO_ctrl(b, BIO_CTRL_SET_CLOSE, flags, nil);
end;

initialization
  OPENSSL_init_crypto(OPENSSL_INIT_CRYPTO_DEFAULT, nil);
  OPENSSL_init_ssl(OPENSSL_INIT_SSL_DEFAULT, nil);

end.
