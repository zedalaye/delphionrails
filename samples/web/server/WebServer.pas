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
    procedure view_demo_getimg_png;
  end;

implementation
uses SysUtils, dorDB, dorService, dorCairolib, dorCairo;

{ THTTPServer }

procedure THTTPConnexion.ctrl_application_index_get;
begin

end;

{ THTTPServer }

function THTTPServer.doOnCreateStub(Socket: longint;
  AAddress: TSockAddr): TSocketStub;
begin
  Result := THTTPConnexion.CreateStub(Self, Socket, AAddress);
end;


// cairo demo
procedure THTTPConnexion.view_demo_getimg_png;
var
  ctx: ICairoContext;
  surf: ICairoSurface;
  pat, lin: ICairoPattern;
  i, j: integer;
begin
  surf := TImageSurface.Create(CAIRO_FORMAT_RGB24, 200, 200);
  ctx := TCairoContext.Create(surf);
  ctx.SetSourceColor(aclWhite);
  ctx.Paint;

  ctx.Scale(200, 200);
  pat := TCairoPattern.CreateRadial(0.25, 0.25, 0.1,  0.5, 0.5, 0.5);
  pat.AddColorStopRGB(0, 1.0, 0.8, 0.8);
  pat.AddColorStopRGB(1, 0.9, 0.0, 0.0);

  for i := 1 to 10 do
    for j := 1 to 10 do
       ctx.Rectangle(i/10.0 - 0.09, j/10.0 - 0.09, 0.08, 0.08);
  ctx.Source := pat;
  ctx.Fill;

  lin := TCairoPattern.CreateLinear(0.25, 0.35, 0.75, 0.65);
  lin.AddColorStopRGBA(0.00,  1, 1, 1, 0);
  lin.AddColorStopRGBA(0.25,  0, 1, 0, 0.5);
  lin.AddColorStopRGBA(0.50,  1, 1, 1, 0);
  lin.AddColorStopRGBA(0.75,  0, 0, 1, 0.5);
  lin.AddColorStopRGBA(1.00,  1, 1, 1, 0);

  ctx.Rectangle(0.0, 0.0, 1, 1);
  ctx.source := lin;
  ctx.Fill;

  surf.WriteToPNGStream(Response.Content);
end;

initialization
  Application.CreateServer(THTTPServer, 81);

end.
