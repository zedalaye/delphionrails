unit chat_websocket;

interface
uses application_websocket;

type
  TChatWebsocket = class(TApplicationWebsocket)
  public
    procedure InputMessage(const msg: string); override;
    procedure chatmessage_event(const msg: string);
  end;

implementation
uses superobject;

{ TChatWebsocket }

procedure TChatWebsocket.chatmessage_event(const msg: string);
begin
  OutputMessage(msg);
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
