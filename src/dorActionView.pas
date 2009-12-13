unit dorActionView;

interface
uses dorHTTPStub, superobject;

type
  TActionView = class
  protected
    // This empry method is called to force RTTI
    // Could be used for somethingelse later
    class procedure Register;
    class function Return: ISuperoBject;
    class function Response: THTTPMessage;
    class procedure Render(const obj: ISuperObject; format: boolean = false); overload;
    class procedure Render(const str: string); overload;
  end;


implementation
uses dorSocketStub;


{ TActionView }

class function TActionView.Return: ISuperoBject;
begin
  Result := (CurrentThread as THTTPStub).Return;
end;

class procedure TActionView.Render(const obj: ISuperObject; format: boolean);
begin
 (CurrentThread as THTTPStub).Render(obj, format);
end;

class procedure TActionView.Register;
begin

end;

class procedure TActionView.Render(const str: string);
begin
  (CurrentThread as THTTPStub).Render(str);
end;

class function TActionView.Response: THTTPMessage;
begin
  Result := (CurrentThread as THTTPStub).Response;
end;


end.
