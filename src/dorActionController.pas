unit dorActionController;

interface
uses superobject, dorHTTPStub;

type
  TActionController = class
  protected
    // This empry method is called to force RTTI
    // Could be used for somethingelse later
    class procedure Register;
    class function Params: ISuperObject; virtual;
    class function Return: ISuperObject; virtual;
    class function Request: THTTPMessage; virtual;
    class function Session: ISuperObject; virtual;
    class function ErrorCode: Integer; virtual;
    class procedure SetErrorCode(code: Integer); virtual;
    class function Context: TSuperRttiContext; virtual;
    class procedure Redirect(const location: string); overload;
    class procedure Redirect(const controler, action: string; const id: string = ''); overload;
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

class function TActionController.Params: ISuperObject;
begin
  Result := (CurrentThread as THTTPStub).Params;
end;

class function TActionController.Request: THTTPMessage;
begin
  Result := (CurrentThread as THTTPStub).Request;
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

