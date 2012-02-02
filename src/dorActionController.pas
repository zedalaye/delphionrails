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

unit dorActionController;

interface
uses superobject, dorHTTPStub;

type
  TActionController = class
  protected
    // This empty method is called to force RTTI
    // Could be used for somethingelse later
    class procedure Register;
    class function Params: ISuperObject; virtual;
    class function Return: ISuperObject; virtual;
    class function Request: THTTPMessage; virtual;
    class function Response: THTTPMessage; virtual;
    class function Session: ISuperObject; virtual;
    class function ErrorCode: Integer; virtual;
    class procedure SetErrorCode(code: Integer); virtual;
    class function Context: TSuperRttiContext; virtual;
    class procedure Redirect(const location: string); overload;
    class procedure Redirect(const controler, action: string; const id: string = ''); overload;
    class function HaveSLL: Boolean; virtual;
    class function HavePeerCertificate: Boolean; virtual;
    class function SSLSubject(const key: AnsiString): AnsiString; virtual;
    class function SSLIssuer(const key: AnsiString): AnsiString; virtual;
  public
    procedure Invoke; virtual;
  end;

implementation
uses dorSocketStub;

{ TActionController }

class function TActionController.Context: TSuperRttiContext;
begin
  Result := (CurrentDorThread as THTTPStub).Context;
end;

class function TActionController.ErrorCode: Integer;
begin
  Result := (CurrentDorThread as THTTPStub).ErrorCode;
end;

class function TActionController.HavePeerCertificate: Boolean;
begin
  Result := (CurrentDorThread as TClientStub).Source.HavePeerCertificate;
end;

class function TActionController.HaveSLL: Boolean;
begin
  Result := (CurrentDorThread as TClientStub).Source.IsSSL;
end;

procedure TActionController.Invoke;
var
  obj: ISuperObject;
  ctx: TSuperRttiContext;
  ite: TSuperAvlEntry;
begin
  ctx := (CurrentDorThread as THTTPStub).Context;
  for obj in Params do
    if obj <> nil then
      obj.DataPtr := Pointer(1);

  case TrySOInvoke(ctx, Self, Params.AsObject.S['action'] + '_' + Request.AsObject.S['method'], Params, obj) of
    irParamError: SetErrorCode(400);
    irError:
      SetErrorCode(500);
  else
    for ite in Params.AsObject do
      if (ite.Value <> nil) and (ite.Value.DataPtr = nil) then
        Return.AsObject[ite.Name] := ite.Value;
    if (obj <> nil) then
      Return.AsObject['result'] := obj;
    if ErrorCode = 0 then
      SetErrorCode(200);
  end;
end;

class function TActionController.Params: ISuperObject;
begin
  Result := (CurrentDorThread as THTTPStub).Params;
end;

class function TActionController.Request: THTTPMessage;
begin
  Result := (CurrentDorThread as THTTPStub).Request;
end;

class function TActionController.Response: THTTPMessage;
begin
  Result := (CurrentDorThread as THTTPStub).Response;
end;

class function TActionController.Return: ISuperObject;
begin
  Result := (CurrentDorThread as THTTPStub).Return;
end;

class function TActionController.Session: ISuperObject;
begin
  Result := (CurrentDorThread as THTTPStub).Session;
end;

class procedure TActionController.Redirect(const location: string);
begin
  (CurrentDorThread as THTTPStub).Redirect(location);
end;

class procedure TActionController.Redirect(const controler, action, id: string);
begin
  (CurrentDorThread as THTTPStub).Redirect(controler, action, id);
end;

class procedure TActionController.Register;
begin

end;

class procedure TActionController.SetErrorCode(code: Integer);
begin
  (CurrentDorThread as THTTPStub).ErrorCode := code;
end;

class function TActionController.SSLIssuer(const key: AnsiString): AnsiString;
begin
  Result := (CurrentDorThread as TClientStub).Source.SSLIssuer(key);
end;

class function TActionController.SSLSubject(const key: AnsiString): AnsiString;
begin
  Result := (CurrentDorThread as TClientStub).Source.SSLSubject(key);
end;

end.

