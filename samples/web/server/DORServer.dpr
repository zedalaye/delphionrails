program DORServer;

{$IFDEF CONSOLEAPP}
{$IFNDEF FPC}
  {$APPTYPE CONSOLE}
{$ENDIF}
{$ENDIF}
uses
  dorService in '..\..\..\src\dorService.pas',
  dorSocketStub in '..\..\..\src\dorSocketStub.pas',
  WebServer in 'WebServer.pas',
  dorHTTPStub in '..\..\..\src\dorHTTPStub.pas',
  mypool in 'mypool.pas',
  dorUtils in '..\..\..\src\dorUtils.pas';

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Name := 'DORWEBSRV';
  Application.DisplayName := 'Delphi On Rails Server';
  Application.Run;
end.
