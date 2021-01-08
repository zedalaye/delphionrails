unit WebServer;

interface

uses
  WinSock2,
  dorHTTPStub, dorSocketStub, dorOpenSSL;

implementation

type
  THTTPConnection = class(THTTPStub)
  protected
    procedure GetPassPhrase(var key, iv: PByte); override;
    function ProcessRequest: Boolean; override;
  end;

{ THTTPConnection }

(*
  Generate both Key and IV using Ruby :

    require 'securerandom'

    def gen_bytes(len)
      SecureRandom.random_bytes(len).each_byte.map { |b| "$%02x" % b }
    end

    def dump_bytes(name, bytes)
      puts "  #{name}: array[0..#{bytes.length} - 1] of Byte = ("
      puts "    " + bytes.each_slice(16).map{ |slice| slice.join(",") }.join(",\n    ");
      puts "  );"
    end

    puts "const"
    dump_bytes("KEY_BYTES", gen_bytes(128));
    dump_bytes("IV_BYTES", gen_bytes(128));
*)

procedure THTTPConnection.GetPassPhrase(var key, iv: PByte);
const
  KEY_BYTES: array[0..128 - 1] of Byte = (
    $4f,$c1,$94,$40,$b4,$c4,$ff,$c4,$49,$f8,$fe,$b3,$d1,$36,$b2,$2e,
    $a7,$71,$75,$87,$fa,$ed,$72,$af,$ca,$ff,$3b,$95,$b4,$ae,$27,$04,
    $ed,$d5,$1e,$36,$a5,$6f,$b5,$13,$e5,$11,$1c,$4c,$08,$cd,$59,$65,
    $81,$c4,$09,$e6,$db,$c9,$80,$26,$d0,$69,$f1,$9e,$ff,$73,$26,$26,
    $8a,$97,$1b,$c4,$69,$24,$d9,$84,$47,$16,$3e,$8b,$47,$5f,$a5,$91,
    $74,$ea,$d9,$fc,$6f,$6c,$3b,$ba,$96,$c5,$07,$0b,$1d,$34,$73,$ee,
    $0d,$37,$f6,$44,$78,$2d,$d6,$2a,$64,$33,$63,$41,$92,$0d,$ae,$23,
    $af,$3a,$06,$1c,$7e,$84,$95,$1a,$3b,$99,$60,$87,$b7,$b2,$2f,$60
  );
  IV_BYTES: array[0..128 - 1] of Byte = (
    $18,$5f,$33,$4c,$b1,$f7,$09,$0d,$c6,$8d,$8c,$96,$6f,$0b,$8b,$64,
    $fe,$c4,$29,$6a,$39,$2d,$04,$77,$14,$52,$69,$ba,$6b,$6e,$63,$51,
    $cd,$3c,$b4,$5e,$17,$3e,$a9,$99,$69,$c4,$7e,$6c,$fd,$4b,$80,$f5,
    $77,$41,$fc,$6a,$c6,$58,$26,$0a,$c0,$79,$c1,$b8,$b4,$fd,$d5,$1b,
    $1b,$d5,$0c,$a4,$f7,$8d,$aa,$78,$d5,$1e,$d0,$59,$d9,$e3,$76,$3a,
    $ea,$d1,$08,$09,$8a,$8d,$fd,$3e,$25,$7c,$28,$4a,$79,$b2,$fe,$0b,
    $60,$6e,$d5,$02,$d2,$a2,$cb,$43,$c6,$59,$5c,$c7,$ec,$03,$e5,$48,
    $fd,$c8,$ec,$e1,$46,$74,$2a,$39,$4a,$a9,$98,$a7,$d6,$84,$d8,$fa
  );
begin
   key := @KEY_BYTES[0];
   iv := @IV_BYTES[0];
end;

function THTTPConnection.ProcessRequest: Boolean;
begin
  Result := inherited;
  if Result and (ErrorCode = 404) and (Params.S['format'] = 'json') then
  begin
    Render(Return);
    ErrorCode := 200;
  end;
end;

function GetSLLHandler(socket: TSocket; const ClientIP: AnsiString): IReadWrite;
begin
  Result := TSSLRWSocket.Create(
    socket, ClientIp,
    True,
    SSL_VERIFY_PEER {or SSL_VERIFY_FAIL_IF_NO_PEER_CERT},
    'delphionrails',
    'server.crt', 'server.key', 'cacert.pem'
  ) as IReadWrite;
end;

initialization
  TSocketServer.CreateServer(3000, '0.0.0.0', THTTPConnection);
  TSocketServer.CreateServer(3443, '0.0.0.0', THTTPConnection, GetSLLHandler);

end.
