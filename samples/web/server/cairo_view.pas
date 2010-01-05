unit cairo_view;

interface
uses application_view, dorCairolib, dorCairo;

type
  TCairoView = class(TApplicationView)
  private
    procedure PaintImg(const ctx: ICairoContext);
  public
    procedure getimg_pdf(x, y: Integer);
    procedure getimg_png(x, y: Integer);
    procedure getimg_ps(x, y: Integer);
    procedure getimg_svg(x, y: Integer);
  end;

implementation


procedure TCairoView.PaintImg(const ctx: ICairoContext);
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

procedure TCairoView.getimg_pdf(x, y: Integer);
var
  ctx: ICairoContext;
  surf: ICairoSurface;
begin
  surf := TPDFSurface.Create(Response.Content, x, y);
  ctx := TCairoContext.Create(surf);
  ctx.Scale(x, y);
  PaintImg(ctx);
end;

procedure TCairoView.getimg_png(x, y: Integer);
var
  ctx: ICairoContext;
  surf: ICairoSurface;
begin
  surf := TImageSurface.Create(CAIRO_FORMAT_RGB24, x, y);
  ctx := TCairoContext.Create(surf);
  ctx.Scale(x, y);
  PaintImg(ctx);
  surf.WriteToPNGStream(Response.Content);
end;

procedure TCairoView.getimg_ps(x, y: Integer);
var
  ctx: ICairoContext;
  surf: ICairoSurface;
begin
  surf := TPostScriptSurface.Create(Response.Content, x, y);
  ctx := TCairoContext.Create(surf);
  ctx.Scale(x, y);
  PaintImg(ctx);
end;

procedure TCairoView.getimg_svg(x, y: Integer);
var
  ctx: ICairoContext;
  surf: ICairoSurface;
begin
  surf := TSVGSurface.Create(Response.Content, x, y);
  ctx := TCairoContext.Create(surf);
  ctx.Scale(x, y);
  PaintImg(ctx);
end;

initialization
  TCairoView.register;

end.
