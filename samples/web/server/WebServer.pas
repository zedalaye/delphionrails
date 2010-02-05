unit WebServer;
interface
uses
  Windows, dorHTTPStub, dorSocketStub, Winsock;

type
  THTTPServer = class(TSocketServer)
  protected
    function doOnCreateStub(Socket: longint; AAddress: TSockAddr): TSocketStub; override;
  end;

  THTTPConnexion = class(THTTPStub)
  protected
    function GetPassPhrase: AnsiString; override;
    procedure ProcessRequest; override;
  public

  end;

implementation
uses dorService;

{ THTTPConnexion }

function THTTPConnexion.GetPassPhrase: AnsiString;
const
  PASS_PHRASE: AnsiString = 'dc62rtd6fc14ss6df464c2s3s3rt324h14vh27d3fc321h2vfghv312';
begin
  Result := PASS_PHRASE;
end;

procedure THTTPConnexion.ProcessRequest;
begin
  inherited;
  if (ErrorCode = 404) and (Params.S['format'] = 'json') then
  begin
    Render(Return);
    ErrorCode := 200;
  end;
end;

{ THTTPServer }

{$REGION 'TCP SERVER'}

function THTTPServer.doOnCreateStub(Socket: longint;
  AAddress: TSockAddr): TSocketStub;
begin
  Result := THTTPConnexion.CreateStub(Self, Socket, AAddress);
end;
{$ENDREGION}

initialization
  Application.CreateServer(THTTPServer, 81);

end.


