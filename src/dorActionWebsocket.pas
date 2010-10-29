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
uses Windows, WinSock, superobject, dorSocketStub;

type
  TActionWebsocket = class(TCustomObserver)
  private
    FStub: TClientStub;
    FCriticalSection: TRTLCriticalSection;
    FParams: ISuperObject;
    FSession: ISuperObject;
  protected
    class procedure Register;
    function Run: Cardinal; override;
    procedure doOnInternalEvent(const Event: ISuperObject); override;
  public
    procedure OutputMessage(const msg: string);
    procedure InputMessage(const msg: string); virtual;
    constructor Create; reintroduce; virtual;
    destructor Destroy; override;

    property Params: ISuperObject read FParams;
    property Session: ISuperObject read FSession;
  end;

  TActionWebsocketClass = class of TActionWebsocket;
implementation
uses SysUtils, dorhttpstub;

{ TActionController }

constructor TActionWebsocket.Create;
begin
  InitializeCriticalSection(FCriticalSection);
  FStub := (CurrentThread as TClientStub);
  FParams := (CurrentThread as THTTPStub).Params;
  FSession := (CurrentThread as THTTPStub).Session;
  inherited Create(CurrentThread);
end;

destructor TActionWebsocket.Destroy;
begin
  DeleteCriticalSection(FCriticalSection);
  inherited;
end;

procedure TActionWebsocket.doOnInternalEvent(const Event: ISuperObject);
begin
  if ObjectIsType(Event, stString) then
    InputMessage(Event.AsString);
end;

procedure TActionWebsocket.InputMessage(const msg: string);
begin

end;

procedure TActionWebsocket.OutputMessage(const msg: string);
var
  utf8: UTF8String;
begin
  EnterCriticalSection(FCriticalSection);
  try
    utf8 := #0 + UTF8String(msg) + #255;
    FStub.Source.Write(PAnsiChar(utf8)^, Length(utf8), 0);
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

class procedure TActionWebsocket.Register;
begin

end;

function TActionWebsocket.Run: Cardinal;
begin
  Result := 0;
  while not Stopped do
  begin
{$IFDEF SWITCHTOTHREAD}
    if not SwitchToThread then
{$ENDIF}
      sleep(1);
    ProcessEvents;
  end;
end;

end.

