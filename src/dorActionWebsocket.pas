(*
    "The contents of this file are subject to the Mozilla Public License
    Version 1.1 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at
    http://www.mozilla.org/MPL/

    Software distributed under the License is distributed on an "AS IS"
    basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
    License for the specific language governing rights and limitations
    under the License.

    The Initial Developer of the Original Code is
      Henri Gourvest <hgourvest@gmail.com>.
*)

unit dorActionWebsocket;

interface
uses superobject, dorHTTPStub;

type
  TActionWebsocket = class
  protected
    // This empty method is called to force RTTI
    // Could be used for somethingelse later
    class procedure Register;
    class function Params: ISuperObject; virtual;
    class function Request: THTTPMessage; virtual;
    class function Session: ISuperObject; virtual;
    class function Context: TSuperRttiContext; virtual;
    class procedure OutputMessage(const msg: string);
    class procedure TriggerEvent(const Event: ISuperObject);
  public
    procedure InputMessage(const msg: string); virtual;
  end;

implementation
uses dorSocketStub, WinSock;

{ TActionController }

class function TActionWebsocket.Context: TSuperRttiContext;
begin
  Result := (CurrentThread as THTTPStub).Context;
end;

procedure TActionWebsocket.InputMessage(const msg: string);
begin

end;

class procedure TActionWebsocket.OutputMessage(const msg: string);
var
  utf8: UTF8String;
begin
  utf8 := #0 + UTF8String(msg) + #255;
  send((CurrentThread as TSocketStub).SocketHandle, PAnsiChar(utf8)^, Length(utf8), 0);
end;

class function TActionWebsocket.Params: ISuperObject;
begin
  Result := (CurrentThread as THTTPStub).Params;
end;

class function TActionWebsocket.Request: THTTPMessage;
begin
  Result := (CurrentThread as THTTPStub).Request;
end;

class function TActionWebsocket.Session: ISuperObject;
begin
  Result := (CurrentThread as THTTPStub).Session;
end;

class procedure TActionWebsocket.TriggerEvent(const Event: ISuperObject);
begin
  TCustomObserver.TriggerEvent(Event);
end;

class procedure TActionWebsocket.Register;
begin

end;

end.

