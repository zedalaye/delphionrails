unit dorOpenSSLCipher;

interface

uses
  SysUtils,
  dorOpenSSL, dorUtils;

type
  ECipherError = class(Exception)
  end;

  TCipherMode = (cmDecrypt = 0, cmEncrypt = 1);

  TOpenSSLAPI = class
  protected
    class procedure CheckErrors(ResultCode: Integer);
  end;

  TCipher = class(TOpenSSLAPI)
  private
    type
      TKey = array[0..EVP_MAX_KEY_LENGTH - 1] of Byte;
      TIV  = array[0..EVP_MAX_IV_LENGTH - 1] of Byte;
  private
    FInitialized: Boolean;
    FCipherContext: EVP_CIPHER_CTX;
    function GetName: string;
    function GetKeyLength: Integer;
    procedure SetKeyLength(Value: Integer);
    function GetIvLength: Integer;
    procedure SetPadding(Value: Integer);
    function GetBlockSize: Integer;
  public
    constructor Create(const Name: string);
    destructor Destroy; override;
    procedure reset;
    procedure init(mode: TCipherMode);
    procedure encrypt;
    procedure decrypt;
    procedure pkcs5_keyivgen(const Password: RawByteString; const Salt: RawByteString = ''; Iterations: Integer = 2048; const Digest: string = 'md5');
    function update(const Data: RawByteString): RawByteString;
    function final: RawByteString;
    procedure set_key(const key: RawByteString);
    procedure set_iv(const iv: RawByteString);
    property name: string read GetName;
    property key_length: Integer read GetKeyLength write SetKeyLength;
    property iv_length: Integer read GetIvLength;
    property block_size: Integer read GetBlockSize;
    property padding: Integer write SetPadding;
  end;

  TRand = class(TOpenSSLAPI)
  public
    class function rand_array(num_bytes: Integer): TArray<Byte>;
    class function rand_raw(num_bytes: Integer): RawByteString;
  end;

  TAes = class
  private
    FCipher: TCipher;
  public
    constructor Create(Size: Integer = 256);
    destructor Destroy; override;
    function Encrypt(const Password, Data: RawByteString; Binary: Boolean = False): RawByteString;
    function Decrypt(const Password, Data: RawByteString; Binary: Boolean = False): RawByteString;
  end;

function EncodeBase64(const Data: RawByteString; NoNL: Boolean = True): RawByteString;
function DecodeBase64(const Data: RawByteString; NoNL: Boolean = True): RawByteString;

implementation

function EncodeBase64(const Data: RawByteString; NoNL: Boolean = True): RawByteString;
var
  b64, bmem: PBIO;
  buf: PBUF_MEM;
begin
  b64 := BIO_new(BIO_f_base64);
  if NoNL then
    BIO_set_flags(b64, BIO_FLAGS_BASE64_NO_NL);
  bmem := BIO_new(BIO_s_mem);
  try
    b64 := BIO_push(b64, bmem);
    BIO_write(b64, @Data[1], Length(Data));
    BIO_flush(b64);
    BIO_get_mem_ptr(b64, @buf);
    SetLength(Result, buf^.length);
    Move(buf^.data^, Result[1], buf^.length);
  finally
    BIO_free_all(b64);
  end;
end;

function DecodeBase64(const Data: RawByteString; NoNL: Boolean = True): RawByteString;
var
  b64, bmem: PBIO;
  l: Integer;
begin
  l := Length(Data);
  b64 := BIO_new(BIO_f_base64);
  if NoNL then
    BIO_set_flags(b64, BIO_FLAGS_BASE64_NO_NL);
  bmem := BIO_new_mem_buf(@Data[1], l);
  try
    bmem := BIO_push(b64, bmem);
    SetLength(Result, l);
    l := BIO_read(bmem, @Result[1], l);
    SetLength(Result, l);
  finally
    BIO_free_all(bmem);
  end;
end;

{ TAes }

constructor TAes.Create(Size: Integer);
begin
  inherited Create;
  FCipher := TCipher.Create('aes-' + IntToStr(Size) + '-cbc');
end;

destructor TAes.Destroy;
begin
  FCipher.Free;
  inherited;
end;

function TAes.Encrypt(const Password, Data: RawByteString;
  Binary: Boolean = False): RawByteString;
var
  S: RawByteString;
  R: RawByteString;
begin
  S := TRand.rand_raw(PKCS5_SALT_LEN);
  FCipher.encrypt;
  FCipher.pkcs5_keyivgen(Password, S, 1);
  R := 'Salted__' + S + FCipher.update(Data) + FCipher.final;
  if Binary then
    Result := R
  else
    Result := EncodeBase64(R, False);
end;

function TAes.Decrypt(const Password, Data: RawByteString;
  Binary: Boolean = False): RawByteString;
var
  R, S: RawByteString;
begin
  if not Binary then
    R := DecodeBase64(Data, False)
  else
    R := Data;

  Assert(Copy(R, 1, 8) = 'Salted__');

  S := Copy(R, 9, PKCS5_SALT_LEN); // Salt
  R := Copy(R, 9 + PKCS5_SALT_LEN, MaxInt); // Encrypted Data

  FCipher.decrypt;
  FCipher.pkcs5_keyivgen(Password, S, 1);

  Result := FCipher.update(R) + FCipher.final;
end;

{ TOpenSSLAPI }

class procedure TOpenSSLAPI.CheckErrors(ResultCode: Integer);
const
  MIN_ERROR_TEXT_SIZE = 120;
var
  e: LongWord;
  txt: RawByteString;
begin
  if ResultCode <> 1 then
  begin
    SetLength(txt, MIN_ERROR_TEXT_SIZE);
    e := ERR_get_error;
    ERR_error_string(e, Pointer(txt));
    raise ECipherError.CreateFmt('CipherError' + #13#10 + 'ErrorCode: %d' + #13#10 + '%s', [e, string(txt)]);
  end;
end;

{ TCipher }

constructor TCipher.Create(const Name: string);
var
  Cipher: PEVP_CIPHER;
  Key: TKey;
begin
  FInitialized := False;
  Cipher := EVP_get_cipherbyname(PAnsiChar(AnsiString(Name)));
  if Cipher = nil then
    raise ECipherError.CreateFmt('unsupported cipher algorithm (%s)', [name]);
  FillChar(Key, SizeOf(TKey), 0);
  CheckErrors(EVP_CipherInit_ex(@FCipherContext, Cipher, nil, PByte(@Key), nil, -1));
  FInitialized := True;
end;

destructor TCipher.Destroy;
begin
  if FInitialized then
    EVP_CIPHER_CTX_free(@FCipherContext);
end;

procedure TCipher.reset;
begin
  CheckErrors(EVP_CipherInit_ex(@FCipherContext, nil, nil, nil, nil, -1));
end;

procedure TCipher.init(mode: TCipherMode);
begin
  CheckErrors(EVP_CipherInit_ex(@FCipherContext, nil, nil, nil, nil, Ord(mode)));
end;

procedure TCipher.encrypt;
begin
  init(cmEncrypt);
end;

procedure TCipher.decrypt;
begin
  init(cmDecrypt);
end;

procedure TCipher.pkcs5_keyivgen(const Password: RawByteString;
  const Salt: RawByteString = ''; Iterations: Integer = 2048; const Digest: string = 'md5');
var
  key: TKey;
  iv: TIV;
  digester: PEVP_MD;
  s: Pointer;
begin
  FillChar(key, SizeOf(TKey), 0);
  FillChar(iv, SizeOf(TIV), 0);

  if Length(Salt) in [0, PKCS5_SALT_LEN] then
    s := Pointer(Salt)
  else
    raise ECipherError.Create('salt must be nothing or an array of 8-octets');

  digester := EVP_get_digestbyname(PAnsiChar(AnsiString(Digest)));
  if digester = nil then
    raise ECipherError.CreateFmt('unsupported digest algorithm (%s)', [Digest]);

  EVP_BytesToKey(
    EVP_CIPHER_CTX_cipher(@FCipherContext),
    digester,
    s,
    PAnsiChar(Password),
    Length(Password),
    Iterations,
    PByte(@key),
    PByte(@iv)
  );

  CheckErrors(
    EVP_CipherInit_ex(@FCipherContext, nil, nil, @key, @iv, -1)
  );

  OPENSSL_cleanse(@key, SizeOf(TKey));
  OPENSSL_cleanse(@iv, SizeOf(TIv));
end;

function TCipher.update(const Data: RawByteString): RawByteString;
var
  in_len, out_len: Integer;
begin
  in_len := Length(Data);
  if in_len = 0 then
    raise ECipherError.Create('data must not be empty');

  out_len := in_len + EVP_CIPHER_CTX_block_size(@FCipherContext);
  SetLength(Result, out_len);

  CheckErrors(
    EVP_CipherUpdate(
      @FCipherContext,
      Pointer(Result),
      @out_len,
      Pointer(Data),
      in_len
    )
  );

  Assert(out_len < Length(Result));
  SetLength(Result, out_len);
end;

function TCipher.final: RawByteString;
var
  out_len: Integer;
begin
  SetLength(Result, EVP_CIPHER_CTX_block_size(@FCipherContext));
  CheckErrors(
    EVP_CipherFinal_ex(
      @FCipherContext,
      Pointer(Result),
      @out_len
    )
  );
  SetLength(Result, out_len);
end;

procedure TCipher.set_key(const key: RawByteString);
begin
  if Length(key) < EVP_CIPHER_CTX_key_length(@FCipherContext) then
    raise ECipherError.Create('key length too short');

  CheckErrors(
    EVP_CipherInit_ex(
      @FCipherContext,
      nil,
      nil,
      Pointer(key),
      nil,
      -1
    )
  );
end;

procedure TCipher.set_iv(const iv: RawByteString);
begin
  if Length(iv) < EVP_CIPHER_CTX_iv_length(@FCipherContext) then
    raise ECipherError.Create('key length too short');

  CheckErrors(
    EVP_CipherInit_ex(
      @FCipherContext,
      nil,
      nil,
      nil,
      Pointer(iv),
      -1
    )
  );
end;

function TCipher.GetName: string;
begin
  Result := string(EVP_CIPHER_name(EVP_CIPHER_CTX_cipher(@FCipherContext)));
end;

function TCipher.GetKeyLength: Integer;
begin
  Result := EVP_CIPHER_CTX_key_length(@FCipherContext);
end;

procedure TCipher.SetKeyLength(Value: Integer);
begin
  CheckErrors(
    EVP_CIPHER_CTX_set_key_length(@FCipherContext, Value)
  );
end;

function TCipher.GetIvLength: Integer;
begin
  Result := EVP_CIPHER_CTX_iv_length(@FCipherContext);
end;

procedure TCipher.SetPadding(Value: Integer);
begin
  CheckErrors(
    EVP_CIPHER_CTX_set_padding(@FCipherContext, Value)
  );
end;

function TCipher.GetBlockSize;
begin
  Result := EVP_CIPHER_CTX_block_size(@FCipherContext);
end;

{ TRand }

class function TRand.rand_array(num_bytes: Integer): TArray<Byte>;
begin
  SetLength(Result, num_bytes);
  CheckErrors(
    RAND_bytes(@Result, num_bytes)
  );
end;

class function TRand.rand_raw(num_bytes: Integer): RawByteString;
begin
  SetLength(Result, num_bytes);
  CheckErrors(
    RAND_bytes(Pointer(Result), num_bytes)
  );
end;

end.
