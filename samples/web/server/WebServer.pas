unit WebServer;
interface
uses
  dorHTTPStub, dorSocketStub;

type
  THTTPConnection = class(THTTPStub)
  protected
    function GetPassPhrase: AnsiString; override;
    procedure ProcessRequest; override;
  end;

implementation

{ THTTPConnection }

function THTTPConnection.GetPassPhrase: AnsiString;
const
  PASS_PHRASE: AnsiString = 'dc62rtd6fc14ss6df464c2s3s3rt324h14vh27d3fc321h2vfghv312';
begin
  Result := PASS_PHRASE;
end;

procedure THTTPConnection.ProcessRequest;
begin
  inherited;
  if (ErrorCode = 404) and (Params.S['format'] = 'json') then
  begin
    Render(Return);
    ErrorCode := 200;
  end;
end;

function GetSLLHandler(socket: LongInt): IReadWrite;
begin
  Result := TSSLRWSocket.Create(socket, True, True, 'delphionrails',
   'server.crt', 'server.key', '') as IReadWrite;
end;

initialization
  TSocketServer.CreateServer(80, '0.0.0.0', THTTPConnection);
  TSocketServer.CreateServer(443, '0.0.0.0', THTTPConnection, GetSLLHandler);
end.
