unit WebServer;
{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}
interface
uses
Windows, dorHTTPStub, dorSocketStub,
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
  end;

implementation
uses SysUtils, dorDB, dorService;

{ THTTPServer }

procedure THTTPConnexion.ctrl_application_index_get;
begin
//  Context.B['session.foo'] := true;
//  Context.S['session.pouet'] := 'tralala';
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
