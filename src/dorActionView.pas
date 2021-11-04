unit dorActionView;

interface
uses dorHTTPStub, superobject;

type
  TActionView = class
  protected
    // This empty method is called to force RTTI
    // Could be used for somethingelse later
    class procedure Register;
    class function Return: ISuperoBject;
    class function Response: THTTPMessage; virtual;
    class function Request: THTTPMessage; virtual;
    class function Params: ISuperObject; virtual;
    class procedure Render(const obj: ISuperObject; format: boolean = false); overload;
    class procedure Render(const str: string); overload;
    class function ErrorCode: Integer; virtual;
    class procedure SetErrorCode(code: Integer); virtual;
  public
    procedure Invoke;
  end;


implementation
uses dorSocketStub;


{ TActionView }

class function TActionView.Return: ISuperoBject;
begin
  Result := (CurrentDorThread as THTTPStub).Return;
end;

class procedure TActionView.SetErrorCode(code: Integer);
begin
  (CurrentDorThread as THTTPStub).ErrorCode := code;
end;

class procedure TActionView.Render(const obj: ISuperObject; format: boolean);
begin
 (CurrentDorThread as THTTPStub).Render(obj, format);
end;

class function TActionView.ErrorCode: Integer;
begin
  Result := (CurrentDorThread as THTTPStub).ErrorCode;
end;

procedure TActionView.Invoke;
var
  ctx: TSuperRttiContext;
  ret: ISuperObject;
begin
  ctx := (CurrentDorThread as THTTPStub).Context;
  with (CurrentDorThread as THTTPStub).Params.AsObject do
    case TrySOInvoke(ctx, Self, S['action'] + '_' + S['format'], Return, ret) of
      irSuccess:
        if (CurrentDorThread as THTTPStub).ErrorCode = 0 then
          SetErrorCode(200);
      irMethodError:
        SetErrorCode(404);
      irParamError:
        SetErrorCode(400);
    else
      SetErrorCode(500);
    end;
end;

class function TActionView.Params: ISuperObject;
begin
  Result := (CurrentDorThread as THTTPStub).Params;
end;

class procedure TActionView.Register;
begin

end;

class procedure TActionView.Render(const str: string);
begin
  (CurrentDorThread as THTTPStub).Render(str);
end;

class function TActionView.Request: THTTPMessage;
begin
  Result := (CurrentDorThread as THTTPStub).Request;
end;

class function TActionView.Response: THTTPMessage;
begin
  Result := (CurrentDorThread as THTTPStub).Response;
end;


end.
