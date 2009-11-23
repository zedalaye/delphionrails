program client;

uses
  Forms,
  main in 'main.pas' {Form45},
  superobject in '..\..\..\src\superobject\superobject.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm45, Form45);
  Application.Run;
end.
