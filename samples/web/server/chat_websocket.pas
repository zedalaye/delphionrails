unit chat_websocket;

interface

uses
  superobject,
  application_websocket;

type
  TChatWebsocket = class(TApplicationWebsocket)
  public
    procedure Initialize; override;
    procedure InputMessage(const msg, source: string); override;
  end;

implementation

{ TChatWebsocket }

procedure TChatWebsocket.Initialize;
begin
  RegisterEvent('chatmessage',
    procedure (const event: ISuperObject) begin
      OutputMessage(event.S['msg']);
    end);
end;

procedure TChatWebsocket.InputMessage(const msg, source: string);
begin
  TriggerEvent(SO([
    'event', 'chatmessage',
    'msg', msg
    ]));
end;

initialization
  TChatWebsocket.Register;

end.
