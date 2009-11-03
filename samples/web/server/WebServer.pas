unit WebServer;
{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
interface
uses
Windows, PDGHTTPStub, PDGSocketStub,
{$IFDEF FPC}sockets,{$ELSE}Winsock, {$ENDIF}
  superobject, mypool;

type
  THTTPServer = class(TSocketServer)
  protected
    function doOnCreateStub(Socket: longint; AAddress: TSockAddr): TSocketStub; override;
  end;

  THTTPConnexion = class(THTTPStub)
  public
    procedure ctrl_application_index_get;
    procedure view_application_index_html;
    procedure ctrl_test_index_get(id: boolean);
  end;

implementation
uses SysUtils, PDGDB, PDGService;

{ THTTPServer }

procedure THTTPConnexion.ctrl_application_index_get;
begin
  beep;
end;

procedure THTTPConnexion.ctrl_test_index_get(id: boolean);
begin
  beep;
end;

procedure THTTPConnexion.view_application_index_html;
begin
  HTTPOutput('<b>hello<b>');
end;

{ THTTPServer }

function THTTPServer.doOnCreateStub(Socket: longint;
  AAddress: TSockAddr): TSocketStub;
begin
  Result := THTTPConnexion.CreateStub(Self, Socket, AAddress);
end;

initialization
  Application.CreateServer(THTTPServer, 81);

end.
