unit cairo_controller;

interface
uses application_view;

type
  TCairoController = class(TApplicationView)
  public
    procedure getimg_get(var x, y: Integer);
  end;

implementation

{ TCairoController }

procedure TCairoController.getimg_get(var x, y: Integer);
begin
  // validate input and send params to view automatically
end;

initialization
  TCairoController.Register;

end.
