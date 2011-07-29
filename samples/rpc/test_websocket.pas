unit test_websocket;

interface
uses dorActionWebsocket, superobject;

type
  TTestWebsocket = class(TActionWebsocket)
  private
    FCtx: TSuperRttiContext;
  public
    procedure ws_echo(const value: string);

    procedure InputMessage(const msg: string); override;
    constructor Create(Version: Integer); override;
    destructor Destroy; override;

  end;

implementation

{ TChatWebsocket }

constructor TTestWebsocket.Create(Version: Integer);
begin
  inherited Create(Version);
  FCtx := TSuperRttiContext.Create;
end;

destructor TTestWebsocket.Destroy;
begin
  FCtx.Free;
  inherited;
end;

procedure TTestWebsocket.InputMessage(const msg: string);
var
  obj, ret: ISuperObject;
begin
  obj := SO(msg);
  TrySOInvoke(FCtx, Self, 'ws_' + obj.S['action'], obj, ret);
end;

procedure TTestWebsocket.ws_echo(const value: string);
begin
  OutputMessage(SO(['action', 'echo', 'value', value]).AsString);
end;

initialization
  TTestWebsocket.Register;

end.
