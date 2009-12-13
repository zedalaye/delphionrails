unit ajax_controller;

interface
uses application_controller, superobject, mypool;

type
  TAjaxController = class(TApplicationController)
  public
    procedure getdata_get(const sord, sidx: string; rows: Integer;
      var page: Integer; out records, total: Integer);
  end;

implementation
uses SysUtils, Math;

{ TAjaxController }

procedure TAjaxController.getdata_get(const sord, sidx: string; rows: Integer;
  var page: Integer; out records, total: Integer);
var
  start: Integer;
  lines, line: ISuperObject;
begin
  with pool.GetConnection.newContext do
  begin
    records := Execute(newSelect('select COUNT(*) as "count" from blog', true)).I['count'];
    if records > 0 then
      total := Ceil(records / rows) else
      total := 0;
    page := Min(page, total);
    start := Max(0, rows * page - rows);

    lines := TSuperObject.Create(stArray);
    for line in Execute(newSelect(Format('SELECT FIRST %d SKIP %d id, title, post_date FROM blog ORDER BY %s %s',
      [rows, start, sidx, sord]), false, true)) do
      lines.AsArray.Add(so(['id', line['0'], 'cell', line]));

    Return['rows'] := lines;
  end;
end;

initialization
  TAjaxController.Register;
end.
