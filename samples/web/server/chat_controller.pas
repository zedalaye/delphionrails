unit chat_controller;

interface

uses
  application_controller;

type
  TChatController = class(TApplicationController)  
  end;

implementation

{ TChatController }

initialization
  TChatController.Register;

end.
