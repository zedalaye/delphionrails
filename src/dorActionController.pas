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

uses
  SysUtils, Classes, Rtti, superobject, dorHTTPStub;

type
  AuthRealmAttribute = class(TCustomAttribute)
  private
    FRealm: string;
  public
    constructor Create(Realm: string);
    property Realm: string read FRealm;
  end;

  BasicAuthAttribute = class(TCustomAttribute)
  private
    FAuth: Boolean;
    FUser: string;
  public
    constructor Create; overload;
    constructor Create(Auth: Boolean); overload;
    constructor Create(const User: string); overload;
    property Auth: Boolean read FAuth;
    property User: string read FUser;
  end;

  TAuthContext = record
  type
    TOptionalValue<T> = record
      Value: T;
      HasValue: Boolean;
    end;
  var
    CheckAuth: TOptionalValue<Boolean>;
    User: TOptionalValue<string>;
    Realm: string;
    function Valid: Boolean;
    procedure Assign(Attributes: TArray<TCustomAttribute>); overload;
    procedure Assign(Attr: AuthRealmAttribute); overload;
    procedure Assign(Attr: BasicAuthAttribute); overload;
  public
    constructor Create(const ARealm: string);
  end;

  TActionController = class
  private
    FEtag: Boolean;
    procedure CalcETag;
    function CheckAuth(const method: string): Boolean;
  public
    type TInvokeTrace = (itBefore, itAfter, itError);
  protected
    // This empty method is called to force RTTI
    // Could be used for something else later
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
    class procedure SendFile(const path: string);
    class procedure Send(stream: TStream = nil);
    class function HaveSLL: Boolean; virtual;
    class function HavePeerCertificate: Boolean; virtual;
    class function SSLSubject(const key: AnsiString): AnsiString; virtual;
    class function SSLIssuer(const key: AnsiString): AnsiString; virtual;
    procedure ETag;
    class procedure TraceInvoke(When: TInvokeTrace; Result: TSuperInvokeResult); virtual;
  public
    function Invoke: Boolean; virtual;
  end;

implementation

uses
  dorSocketStub, dorOpenSSL;

{ TActionController }

procedure TActionController.ETag;
begin
  FEtag := True;
end;

procedure TActionController.CalcETag;
var
  stream: TMemoryStream;
  buffer: array[0..SHA_DIGEST_LENGTH - 1] of AnsiChar;
  buffer2: array[0..(SHA_DIGEST_LENGTH * 2) - 1] of AnsiChar;
begin
  stream := TMemoryStream.Create;
  try
    stream.Size := Return.CalcSize;
    Return.SaveTo(stream);
    SHA1(stream.Memory, stream.Size, @buffer);
    BinToHex(PAnsiChar(@buffer), PAnsiChar(@buffer2), SHA_DIGEST_LENGTH);

    if Request['env'].AsObject.S['if-none-match'] = string(buffer2) then
      SetErrorCode(304) else
      begin
        Response.AsObject.S['Cache-Control'] := 'max-age=946080000, public';
        Response.AsObject.S['ETag'] := string(buffer2);
      end;
  finally
    stream.Free;
  end;
end;

function TActionController.CheckAuth(const method: string): Boolean;
const
  REALM = 'Private Zone';
var
  Auth: TAuthContext;
  Klass: TClass;
  Typ: TRttiType;
  Meth: TRttiMethod;
begin
  Auth := TAuthContext.Create(REALM);

  var Ctx := (CurrentDorThread as THTTPStub).Context;

  Klass := Self.ClassType;
  Typ := Ctx.Context.GetType(Klass);

  { Collect Auth Attributes from action method }
  Meth := Typ.GetMethod(method);
  if Meth <> nil then
    Auth.Assign(Meth.GetAttributes);

  { And walk the ancestor hierarchy up to TActionController }
  repeat
    Auth.Assign(Typ.GetAttributes);
    Klass := Klass.ClassParent;
    Typ := Ctx.Context.GetType(Klass);
  until Klass = TActionController;

  if Auth.Valid and Auth.CheckAuth.Value then
    if Session.S['user'] = Auth.User.Value then
      Result := True
    else
    begin
      Response.AsObject.S['WWW-Authenticate'] := 'Basic realm="' + Auth.Realm + '"';
      SetErrorCode(401);
      Result := False;
    end
  else
    Result := True;
end;

function TActionController.Invoke: Boolean;
var
  method: string;
  obj: ISuperObject;
  ctx: TSuperRttiContext;
  ite: TSuperAvlEntry;
begin
  Result := False;
  FEtag := False;
  ctx := (CurrentDorThread as THTTPStub).Context;
  for obj in Params do
    if obj <> nil then
      obj.DataPtr := Pointer(1);

  method := Params.AsObject.S['action'] + '_' + Request.AsObject.S['method'];

  if not CheckAuth(method) then
    Exit;

  TraceInvoke(itBefore, irSuccess);

  case TrySOInvoke(ctx, Self, method, Params, obj) of
    irParamError:
      begin
        TraceInvoke(itError, irParamError);
        SetErrorCode(400);
      end;
    irError:
      begin
        TraceInvoke(itError, irError);
        SetErrorCode(500);
      end;
    irMethodError:
      begin
        TraceInvoke(itError, irMethodError);
        Result := False;
      end
  else
    Result := True;
    for ite in Params.AsObject do
      if (ite.Value <> nil) and (ite.Value.DataPtr = nil) then
        Return.AsObject[ite.Name] := ite.Value;
    if (obj <> nil) then
      Return.AsObject['result'] := obj;
    if ErrorCode = 0 then
      SetErrorCode(200);

    TraceInvoke(itAfter, irSuccess);
  end;

  if FEtag then
    CalcETag;
end;

class procedure TActionController.TraceInvoke(When: TInvokeTrace; Result: TSuperInvokeResult);
begin
  { Just do nothing }
end;

class procedure TActionController.Send(stream: TStream);
begin
  with (CurrentDorThread as THTTPStub) do
  begin
    if stream <> nil then
      Response.Content.LoadFromStream(stream);
  end;
end;

class procedure TActionController.SendFile(const path: string);
begin
  (CurrentDorThread as THTTPStub).FileToSend := path;
end;

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

class procedure TActionController.Register;
begin

end;

{ AuthRealmAttribute }

constructor AuthRealmAttribute.Create(Realm: string);
begin
  inherited Create;
  FRealm := Realm;
end;

{ BasicAuthAttribute }

constructor BasicAuthAttribute.Create(const User: string);
begin
  inherited Create;
  FUser := User;
  FAuth := True;
end;

constructor BasicAuthAttribute.Create(Auth: Boolean);
begin
  inherited Create;
  FAuth := Auth;
end;

constructor BasicAuthAttribute.Create;
begin
  Create(True);
end;

{ TAuthContext }

constructor TAuthContext.Create(const ARealm: string);
begin
  Self.Realm := ARealm;
  Self.User.Value := '';
  Self.User.HasValue := False;
  Self.CheckAuth.Value := False;
  Self.CheckAuth.HasValue := False;
end;

function TAuthContext.Valid: Boolean;
begin
  Result := CheckAuth.HasValue and User.HasValue;
end;

procedure TAuthContext.Assign(Attributes: TArray<TCustomAttribute>);
begin
  for var A in Attributes do
    if A is BasicAuthAttribute then
      Assign(BasicAuthAttribute(A))
    else if A is AuthRealmAttribute then
      Assign(AuthRealmAttribute(A));
end;

procedure TAuthContext.Assign(Attr: BasicAuthAttribute);
begin
  if not CheckAuth.HasValue then
  begin
    CheckAuth.Value := Attr.Auth;
    CheckAuth.HasValue := True;
  end;

  if (not User.HasValue) and (Attr.User <> '') then
  begin
    User.Value := Attr.User;
    User.HasValue := True;
  end;
end;

procedure TAuthContext.Assign(Attr: AuthRealmAttribute);
begin
  Realm := Attr.Realm;
end;

end.

