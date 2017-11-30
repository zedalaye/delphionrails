unit webserver;

interface

uses
  dorHTTPStub, dorSocketStub;

type
  THTTPConnection = class(THTTPStub)
  protected
    function ProcessRequest: Boolean; override;
  end;

implementation
uses WinSock;

{ THTTPConnection }

function THTTPConnection.ProcessRequest: Boolean;
begin
  inherited;
  if (ErrorCode = 404) and (Params.S['format'] = 'json') then
  begin
    Render(Return);
    ErrorCode := 200;
  end;
  Result := True;
end;

initialization
  TSocketServer.CreateServer(80, '0.0.0.0', THTTPConnection);
end.
