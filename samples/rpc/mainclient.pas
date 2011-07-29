unit mainclient;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, dorWebsocket, superobject;

type
  TForm13 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Edit1: TEdit;
    Button2: TButton;
    Button3: TButton;
    Edit2: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button3Click(Sender: TObject);
  private
    { Déclarations privées }
    FWebS: IWebSocket;
    FCtx: TSuperRttiContext;
  public
    { Déclarations publiques }
    procedure ws_echo(const value: string);
  end;

  TMessage = record
    g: TGUID;
    d: TDateTime;
    i: Integer;
    b: Boolean;
    a: array of Byte;
    s: string;
  end;

var
  Form13: TForm13;

implementation
uses dorHTTPClient;

{$R *.dfm}

procedure TForm13.Button1Click(Sender: TObject);
var
  req: IHTTPRequest;
begin
  req := THTTPRequest.Create;
  req.Open('GET', 'http://localhost/test/echo.json');
  req.RequestHeader['Content-Type'] := 'application/json';
  if req.SendText(SO(['invalue', Edit1.Text]).AsString) then
    Memo1.Lines.Add(SO(req.ResponseText).S['outvalue']);
end;

procedure TForm13.Button2Click(Sender: TObject);
var
  req: IHTTPRequest;
  data: TMessage;
begin
  req := THTTPRequest.Create;
  req.Open('GET', 'http://localhost/test/echo2.json');
  req.RequestHeader['Content-Type'] := 'application/json';

  // init
  CreateGUID(data.g);
  data.d := Now;
  data.i := 123;
  data.b := True;
  SetLength(data.a, 2);
  data.a[0] := 1;
  data.a[1] := 2;
  data.s := 'ping';

  if req.SendText(FCtx.AsJson<TMessage>(data).AsString) then
  begin
    data := FCtx.AsType<TMessage>(SO(req.ResponseText));
    Memo1.Lines.Add(req.ResponseText);
  end;
end;

procedure TForm13.Button3Click(Sender: TObject);
begin
  FWebS.Send(SO(['action', 'echo', 'value', Edit2.Text]).AsString);
end;

procedure TForm13.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FCtx.Free;
end;

procedure TForm13.FormCreate(Sender: TObject);
begin
  FWebS := TWebSocket.Create;
  FCtx := TSuperRttiContext.Create;
  FWebS.OnMessage := procedure(const msg: string)
  var
    obj, ret: ISuperObject;
  begin
    obj := SO(msg);
    TrySOInvoke(FCtx, Self, 'ws_' + obj.S['action'], obj, ret);
  end;
  FWebS.Open('ws://localhost/test');
end;

procedure TForm13.ws_echo(const value: string);
begin
  Memo1.Lines.Add(value);
end;

end.
