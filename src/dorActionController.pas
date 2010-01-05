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
  public
    procedure Invoke; virtual;
  end;

implementation
uses dorSocketStub;

{ TActionController }

class function TActionController.Context: TSuperRttiContext;
begin
  Result := (CurrentThread as THTTPStub).Context;
end;

class function TActionController.ErrorCode: Integer;
begin
  Result := (CurrentThread as THTTPStub).ErrorCode;
end;

procedure TActionController.Invoke;
var
  obj: ISuperObject;
  ctx: TSuperRttiContext;
  ite: TSuperAvlEntry;
begin
  ctx := (CurrentThread as THTTPStub).Context;
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
    if ErrorCode = 0 then
      SetErrorCode(200);
  end;
end;

class function TActionController.Params: ISuperObject;
begin
  Result := (CurrentThread as THTTPStub).Params;
end;

class function TActionController.Request: THTTPMessage;
begin
  Result := (CurrentThread as THTTPStub).Request;
end;

class function TActionController.Response: THTTPMessage;
begin
  Result := (CurrentThread as THTTPStub).Response;
end;

class function TActionController.Return: ISuperObject;
begin
  Result := (CurrentThread as THTTPStub).Return;
end;

class function TActionController.Session: ISuperObject;
begin
  Result := (CurrentThread as THTTPStub).Session;
end;

class procedure TActionController.Redirect(const location: string);
begin
  (CurrentThread as THTTPStub).Redirect(location);
end;

class procedure TActionController.Redirect(const controler, action, id: string);
begin
  (CurrentThread as THTTPStub).Redirect(controler, action, id);
end;

class procedure TActionController.Register;
begin

end;

class procedure TActionController.SetErrorCode(code: Integer);
begin
  (CurrentThread as THTTPStub).ErrorCode := code;
end;

end.

