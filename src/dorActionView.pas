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
  Result := (CurrentThread as THTTPStub).Return;
end;

class procedure TActionView.SetErrorCode(code: Integer);
begin
  (CurrentThread as THTTPStub).ErrorCode := code;
end;

class procedure TActionView.Render(const obj: ISuperObject; format: boolean);
begin
 (CurrentThread as THTTPStub).Render(obj, format);
end;

class function TActionView.ErrorCode: Integer;
begin
  Result := (CurrentThread as THTTPStub).ErrorCode;
end;

procedure TActionView.Invoke;
var
  ctx: TSuperRttiContext;
  ret: ISuperObject;
begin
  ctx := (CurrentThread as THTTPStub).Context;
  with (CurrentThread as THTTPStub).Params.AsObject do
    case TrySOInvoke(ctx, Self, S['action'] + '_' + S['format'], Return, ret) of
      irSuccess: SetErrorCode(200);
      irMethothodError: SetErrorCode(404);
      irParamError: SetErrorCode(400);
    else
      SetErrorCode(500);
    end;
end;

class procedure TActionView.Register;
begin

end;

class procedure TActionView.Render(const str: string);
begin
  (CurrentThread as THTTPStub).Render(str);
end;

class function TActionView.Request: THTTPMessage;
begin
  Result := (CurrentThread as THTTPStub).Request;
end;

class function TActionView.Response: THTTPMessage;
begin
  Result := (CurrentThread as THTTPStub).Response;
end;


end.
