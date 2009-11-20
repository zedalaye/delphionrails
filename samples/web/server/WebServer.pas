unit WebServer;
interface
uses
  Windows, dorHTTPStub, dorSocketStub, Winsock, superobject, mypool,
  dorCairolib, dorCairo;

type
  THTTPServer = class(TSocketServer)
  protected
    function doOnCreateStub(Socket: longint; AAddress: TSockAddr): TSocketStub; override;
  end;

  THTTPConnexion = class(THTTPStub)
  private
    procedure PaintImg(const ctx: ICairoContext);
  protected
    function GetPassPhrase: AnsiString; override;
  public
    type
      TBlog = record
        id: Integer;
        title: string;
        body: string;
      end;

    // BLOG
    procedure ctrl_blog_index_get;
    procedure ctrl_blog_new_post(const title, body: string);
    procedure ctrl_blog_view_get(id: Integer);
    procedure ctrl_blog_edit_get(id: Integer);
    procedure ctrl_blog_edit_post(const data: TBlog);
    procedure ctrl_blog_delete_post(id: Integer);

    // CAIRO
    procedure ctrl_cairo_getimg_get(x, y: Integer);
    procedure view_cairo_getimg_png;
    procedure view_cairo_getimg_svg;
    procedure view_cairo_getimg_pdf;
    procedure view_cairo_getimg_ps;

    // AJAX
    procedure ctrl_ajax_getdata_get(_search: Boolean; const sord, sidx: string; rows, page: Integer);
    procedure view_ajax_getdata_json(page, total, records: Integer; const data: ISuperObject);
  end;

implementation
uses SysUtils, dorDB, dorService;

{ THTTPConnexion }

{$REGION 'BLOG'}
procedure THTTPConnexion.ctrl_ajax_getdata_get(_search: Boolean;
  const sord, sidx: string; rows, page: Integer);
var
  count, pages, start: Integer;
begin
  with pool.GetConnection.newContext do
  begin
    count := Execute(newSelect('select COUNT(*) as "count" from blog', true)).I['count'];
    if count > 0 then
      pages := (count div rows) + 1 else
      pages := 0;
    if (page > pages) then
      page := pages;
    start := rows * page - rows;
    Context['data'] := Execute(newSelect(Format('SELECT FIRST %d SKIP %d id, title FROM blog ORDER BY %s %s', [rows, start, sidx, sord])));
    Context.I['page'] := page;
    Context.I['total'] := pages;
    Context.I['records'] := count;
  end;
end;

procedure THTTPConnexion.view_ajax_getdata_json(page, total, records: Integer; const data: ISuperObject);
var
  line, rows: ISuperObject;
begin
  rows := TSuperObject.Create(stArray);

  for line in data do
    rows.AsArray.Add(
      so(['id', line['id'], 'cell', SA([line['id'], line['title']])]));

  Render(SO(['page', page, 'total', total, 'records', records, 'rows', rows]));
end;

procedure THTTPConnexion.ctrl_blog_delete_post(id: Integer);
begin
  with pool.GetConnection.newContext do
    Execute(newCommand('delete from blog where id = ?'), id);
  Redirect('blog', 'index');
end;

procedure THTTPConnexion.ctrl_blog_edit_get(id: Integer);
begin
  with pool.GetConnection.newContext do
    Context['data'] := Execute(newSelect('select * from blog where id = ?', true), id);
end;

procedure THTTPConnexion.ctrl_blog_edit_post(const data: TBlog);
begin
  with pool.GetConnection.newContext do
   Execute(newCommand('update blog set title = ?, body = ? where id = ?'),
     [data.title, data.body, data.id]);
  Context.S['info'] := 'updated';
end;

procedure THTTPConnexion.ctrl_blog_index_get;
begin
  with pool.GetConnection.newContext do
    Context['data'] := Execute(newSelect('select title, id from blog order by post_date'));
end;

procedure THTTPConnexion.ctrl_blog_new_post(const title, body: string);
begin
  with pool.GetConnection.newContext do
   Redirect(Execute(newFunction('insert into blog (title, body) values (?, ?) returning id'),
     [title, body]).Format('/blog/view/%id%'));
end;

procedure THTTPConnexion.ctrl_blog_view_get(id: Integer);
begin
  with pool.GetConnection.newContext do
    Context['data'] := Execute(newSelect('select * from blog where id = ?', true), id);
  if Context['data'] = nil then
    ErrorCode := 404;
  Compress := true;
end;

{$ENDREGION}

{$REGION 'CAIRO'}

procedure THTTPConnexion.ctrl_cairo_getimg_get(x, y: Integer);
begin
  Context.I['x'] := x;
  Context.I['y'] := y;
end;

procedure THTTPConnexion.PaintImg(const ctx: ICairoContext);
var
  pat, lin: ICairoPattern;
  i, j: integer;
begin
  ctx.SetSourceColor(aclWhite);
  ctx.Paint;

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

  ctx.SetSourceColor(aclBlack);
  ctx.SelectFontFace('Sans', CAIRO_FONT_SLANT_ITALIC, CAIRO_FONT_WEIGHT_BOLD);
  ctx.SetFontSize(0.3);
  ctx.MoveTo(0, 0.5);
  ctx.ShowText('Hello');
end;

procedure THTTPConnexion.view_cairo_getimg_pdf;
var
  ctx: ICairoContext;
  surf: ICairoSurface;
begin
  surf := TPDFSurface.Create(Response.Content, Context.I['x'], Context.I['y']);
  ctx := TCairoContext.Create(surf);
  ctx.Scale(Context.I['x'], Context.I['y']);
  PaintImg(ctx);
end;

procedure THTTPConnexion.view_cairo_getimg_png;
var
  ctx: ICairoContext;
  surf: ICairoSurface;
begin
  surf := TImageSurface.Create(CAIRO_FORMAT_RGB24, Context.I['x'], Context.I['y']);
  ctx := TCairoContext.Create(surf);
  ctx.Scale(Context.I['x'], Context.I['y']);

  PaintImg(ctx);

  surf.WriteToPNGStream(Response.Content);
end;

procedure THTTPConnexion.view_cairo_getimg_ps;
var
  ctx: ICairoContext;
  surf: ICairoSurface;
begin
  surf := TPostScriptSurface.Create(Response.Content, Context.I['x'], Context.I['y']);
  ctx := TCairoContext.Create(surf);
  ctx.Scale(Context.I['x'], Context.I['y']);
  PaintImg(ctx);
end;

procedure THTTPConnexion.view_cairo_getimg_svg;
var
  ctx: ICairoContext;
  surf: ICairoSurface;
begin
  surf := TSVGSurface.Create(Response.Content, Context.I['x'], Context.I['y']);
  ctx := TCairoContext.Create(surf);
  ctx.Scale(Context.I['x'], Context.I['y']);
  PaintImg(ctx);
end;

{$ENDREGION}

{$REGION 'CUSOMIZE'}

function THTTPConnexion.GetPassPhrase: AnsiString;
const
  PASS_PHRASE: AnsiString = 'dc62rtd6fc14ss6df464c2s3s3rt324h14vh27d3fc321h2vfghv312';
begin
  Result := PASS_PHRASE;
end;

{$ENDREGION}

{ THTTPServer }

{$REGION 'TCP SERVER'}

function THTTPServer.doOnCreateStub(Socket: longint;
  AAddress: TSockAddr): TSocketStub;
begin
  Result := THTTPConnexion.CreateStub(Self, Socket, AAddress);
end;
{$ENDREGION}

initialization
  Application.CreateServer(THTTPServer, 81);

end.


