unit mypool;

interface
uses dorDB, dorUIB, SuperObject;

type

  TBlog = record
    id: Integer;
    title: string;
    body: string;
    function validate: boolean;
  end;

var
  pool: IDBConnectionPool;

implementation
uses SysUtils, dorSocketStub, dorHTTPStub, dorUtils, Classes;

procedure init;
var
  obj: ISuperObject;
begin
  obj := TSuperObject.ParseFile(ExtractFilePath(ParamStr(0)) + 'conf.json', false);
  pool := TDBUIBConnectionPool.Create('pool', 0,
    obj.S['database.databasename'],
    obj.S['database.username'],
    obj.S['database.password']);
end;

{ TBlog }

function TBlog.validate: boolean;
begin
  if Length(title) > 50 then
  begin
    with CurrentDorThread as THTTPStub do
      Return.S['errors[]'] := 'title must be less than 50 characters';
    Result := False;
  end else
    Result := True;
end;


initialization
 init;

finalization
  while TDORThread.ThreadCount > 0 do sleep(100);
  pool := nil;

end.

