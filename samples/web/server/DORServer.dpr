program DORServer;

{$IFDEF CONSOLEAPP}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$R *.res}

uses
  dorService,
  mypool in 'mypool.pas',
  WebServer in 'WebServer.pas',
  application_controller in 'application_controller.pas',
  blog_controller in 'blog_controller.pas',
  ajax_controller in 'ajax_controller.pas',
  cairo_controller in 'cairo_controller.pas',
  cairo_view in 'cairo_view.pas',
  application_view in 'application_view.pas',
  application_websocket in 'application_websocket.pas',
  chat_websocket in 'chat_websocket.pas';

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
  Application.Name := 'DORWEBSRV';
  Application.DisplayName := 'Delphi On Rails Server';
  Application.Run;
end.
