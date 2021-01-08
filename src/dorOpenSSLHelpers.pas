unit dorOpenSSLHelpers;

interface

uses
  Classes, dorOpenSSL;

function BIO_get_mem_ptr(b: PBIO; ptr: Pointer): LongInt;

procedure AesEncryptStream(InStream, OutStream: TStream; const key, iv: PByte);
procedure AesDecryptStream(InStream, OutStream: TStream; const key, iv: PByte);

type
  TOnX509KeyValue = reference to function(const key, value: AnsiString): Boolean;

// function X509NameOneline(const name: PX509_NAME): AnsiString;
// function X509NameParse(const name: PX509_NAME; const onkey: TOnX509KeyValue): Boolean;
function X509NameFind(const name: PX509_NAME; const key: AnsiString): AnsiString;

implementation

function BIO_get_mem_ptr(b: PBIO; ptr: Pointer): LongInt;
begin
  Result := BIO_ctrl(b, BIO_C_GET_BUF_MEM_PTR, 0, ptr);
end;

const
  STREAM_BLOCK_SIZE = 1024;

procedure AesEncryptStream(InStream, OutStream: TStream; const key, iv: PByte);
var
  ctx: PEVP_CIPHER_CTX;
  inbuffer: array[0..STREAM_BLOCK_SIZE - 1] of Byte;
  outbuffer: array[0..STREAM_BLOCK_SIZE - 1] of Byte;
  inlen, outlen: Integer;
begin
  instream.Seek(0, soFromBeginning);

  ctx := EVP_CIPHER_CTX_new;
  try
    if EVP_EncryptInit(ctx, EVP_aes_128_cbc, key, iv) <> 1 then
      Exit;

    inlen := InStream.Read(inbuffer, STREAM_BLOCK_SIZE);
    while inlen > 0 do
    begin
      if EVP_EncryptUpdate(ctx, @outbuffer[0], outlen, @inbuffer[0], inlen) <> 1 then
        Exit;

      OutStream.Write(outbuffer, outlen);
      inlen := InStream.Read(inbuffer, STREAM_BLOCK_SIZE);
    end;

    if EVP_EncryptFinal(ctx, @outbuffer[0], outlen) <> 1 then
      Exit;

    OutStream.Write(outbuffer, outlen);
  finally
    EVP_CIPHER_CTX_free(ctx);
  end;
end;

procedure AesDecryptStream(InStream, OutStream: TStream; const key, iv: PByte);
var
  ctx: PEVP_CIPHER_CTX;
  inbuffer: array[0..STREAM_BLOCK_SIZE - 1] of Byte;
  outbuffer: array[0..STREAM_BLOCK_SIZE - 1] of Byte;
  inlen, outlen: Integer;
begin
  if InStream.Size mod AES_BLOCK_SIZE <> 0 then
    Exit;

  InStream.Seek(0, soFromBeginning);

  ctx := EVP_CIPHER_CTX_new;
  try
    if EVP_DecryptInit(ctx, EVP_aes_128_cbc, key, iv) <> 1 then
      Exit;

    inlen := InStream.Read(inbuffer, STREAM_BLOCK_SIZE);
    while inlen > 0 do
    begin
      if EVP_DecryptUpdate(ctx, @outbuffer[0], outlen, @inbuffer[0], inlen) <> 1 then
        Exit;

      OutStream.Write(outbuffer, outlen);
      inlen := InStream.Read(inbuffer, STREAM_BLOCK_SIZE);
    end;

    if EVP_DecryptFinal(ctx, @outbuffer[0], outlen) <> 1 then
      Exit;

    OutStream.Write(outbuffer, outlen);
  finally
    EVP_CIPHER_CTX_free(ctx);
  end;
end;

function X509NameOneline(const name: PX509_NAME): AnsiString;
var
  buff: array[0..1024] of AnsiChar;
begin
  if name <> nil then
    Result := X509_NAME_oneline(name, @buff, SizeOf(buff))
  else
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
            end
            else
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
      end
      else
        Result := True;
    end
  );
  Result := return;
end;

end.
