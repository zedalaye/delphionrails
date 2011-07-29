unit test_controller;


interface
uses dorActionController;

type
  TTestController = class(TActionController)
  public
    procedure echo_get(const invalue: string; out outvalue: string);
    procedure echo2_get(var g: TGUID; var d: TDateTime; var i: Integer;
      var b: Boolean; var a: Tarray<Byte>; var s: string);
  end;

implementation

{ TTestController }

procedure TTestController.echo2_get(var g: TGUID; var d: TDateTime;
  var i: Integer; var b: Boolean; var a: TArray<Byte>; var s: string);
begin
  s := 'pong';
end;

procedure TTestController.echo_get(const invalue: string; out outvalue: string);
begin
  outvalue := invalue;
end;

initialization
  TTestController.Register;

end.
