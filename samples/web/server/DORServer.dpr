program DORServer;

{$IFDEF CONSOLEAPP}
  {$APPTYPE CONSOLE}
{$ENDIF}

{$R *.res}

uses
  dorService,
  mypool in 'mypool.pas',
  WebServer in 'WebServer.pas';

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := true;
{$ENDIF}
  Application.Name := 'DORWEBSRV';
  Application.DisplayName := 'Delphi On Rails Server';
  Application.Run;
end.
