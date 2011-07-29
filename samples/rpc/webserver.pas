unit webserver;

interface

uses
  dorHTTPStub, dorSocketStub;

type
  THTTPConnection = class(THTTPStub)
  protected
    procedure ProcessRequest; override;
  end;

implementation
uses WinSock;

{ THTTPConnection }

procedure THTTPConnection.ProcessRequest;
begin
  inherited;
  if (ErrorCode = 404) and (Params.S['format'] = 'json') then
  begin
    Render(Return);
    ErrorCode := 200;
  end;
end;

initialization
  TSocketServer.CreateServer(80, '0.0.0.0', THTTPConnection);
end.
