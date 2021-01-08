unit application_controller;

interface

uses
  dorActionController;

type
  TApplicationController = class(TActionController)
  end;

implementation

initialization
  TApplicationController.Register;

end.
