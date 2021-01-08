unit ajax_controller;

interface

uses
  SysUtils, Math, dorDB,
  superobject,
  application_controller;

type
  TAjaxController = class(TApplicationController)
  public
    procedure getdata_get(const sord, sidx: string; rows: Integer;
      var page: Integer; out records, total: Integer);
  end;

implementation

uses
   mypool;

{ TAjaxController }

procedure TAjaxController.getdata_get(const sord, sidx: string; rows: Integer;
  var page: Integer; out records, total: Integer);
var
  start: Integer;
  lines, line: ISuperObject;
begin
  with pool.Connection.Transaction do
  begin
    records := Cell('select COUNT(*) as "count" from blog').AsInteger;
    if records > 0 then
      total := Ceil(records / rows) else
      total := 0;
    page := Min(page, total);
    start := Max(0, rows * page - rows);

    lines := TSuperObject.Create(stArray);
    for line in Table(Format('SELECT FIRST %d SKIP %d id, title, post_date FROM blog ORDER BY %s %s',
      [rows, start, sidx, sord])) do
      lines.AsArray.Add(so(['id', line['0'], 'cell', line]));

    Return['rows'] := lines;
  end;
end;

initialization
  TAjaxController.Register;
end.
