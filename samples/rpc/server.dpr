program server;

{$APPTYPE CONSOLE}

{$IFDEF CONSOLEAPP}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$R *.res}

uses
  dorService,
  webserver in 'webserver.pas',
  test_controller in 'test_controller.pas',
  test_websocket in 'test_websocket.pas';

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
  Application.Name := 'DORWEBSRV';
  Application.DisplayName := 'Delphi On Rails Server';
  Application.Run;
end.
