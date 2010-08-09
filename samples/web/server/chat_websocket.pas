unit chat_websocket;

interface
uses application_websocket;

type
  TChatWebsocket = class(TApplicationWebsocket)
  public
    procedure InputMessage(const msg: string); override;
    constructor Create; virtual;
  end;

implementation
uses superobject;

{ TChatWebsocket }

constructor TChatWebsocket.Create;
begin
  RegisterEvent('chatmessage', procedure (const event: ISuperObject) begin
    OutputMessage(event.S['msg']);
  end);
end;

procedure TChatWebsocket.InputMessage(const msg: string);
begin
  TriggerEvent(SO([
    'event', 'chatmessage',
    'msg', msg
    ]));
end;

initialization
  TChatWebsocket.Register;

end.
