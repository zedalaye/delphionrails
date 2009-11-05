unit mypool;
{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface
uses dorDB, dorUIB, SuperObject;

var
  pool: IDBConnectionPool;

implementation
uses SysUtils, dorSocketStub, dorUtils;

procedure init;
var
  obj: ISuperObject;
begin
  obj := TSuperObject.ParseFile(ExtractFilePath(ParamStr(0)) + 'appserver.json', false);
  pool := TDBUIBConnectionPool.Create(obj['database'], 0);
end;

initialization
 init;

finalization
  while TDORThread.ThreadCount > 0 do sleep(100);
  pool := nil;

end.
