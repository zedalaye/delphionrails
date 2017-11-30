unit WebServer;
interface
uses
  dorHTTPStub, dorSocketStub;

type
  THTTPConnection = class(THTTPStub)
  protected
    function GetPassPhrase: AnsiString; override;
    function ProcessRequest: Boolean; override;
  end;

implementation
uses dorOpenSSL, WinSock2;

{ THTTPConnection }

function THTTPConnection.GetPassPhrase: AnsiString;
const
  PASS_PHRASE: AnsiString = 'dc62rtd6fc14ss6df464c2s3s3rt324h14vh27d3fc321h2vfghv312';
begin
  Result := PASS_PHRASE;
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

function GetSLLHandler(socket: TSocket): IReadWrite;
begin
  Result := TSSLRWSocket.Create(socket, True,
    SSL_VERIFY_PEER {or SSL_VERIFY_FAIL_IF_NO_PEER_CERT},
    'delphionrails', 'server.crt', 'server.key', 'cacert.pem') as IReadWrite;
end;

initialization
  TSocketServer.CreateServer(80, '0.0.0.0', THTTPConnection);
  TSocketServer.CreateServer(443, '0.0.0.0', THTTPConnection, GetSLLHandler);
end.
