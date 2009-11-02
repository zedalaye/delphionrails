program AppServer;

{$IFDEF CONSOLEAPP}
{$IFNDEF FPC}
  {$APPTYPE CONSOLE}
{$ENDIF}
{$ENDIF}
uses
  PDGService in '..\..\..\src\PDGService.pas',
  PDGSocketStub in '..\..\..\src\PDGSocketStub.pas',
  WebServer in 'WebServer.pas',
  PDGHTTPStub in '..\..\..\src\PDGHTTPStub.pas',
  mypool in 'mypool.pas',
  PDGUtils in '..\..\..\src\PDGUtils.pas';

begin
  ReportMemoryLeaksOnShutdown := true;
  Application.Name := 'PDGWEBSRV';
  Application.DisplayName := 'Progdigy WEB Server';
  Application.Run;
end.
