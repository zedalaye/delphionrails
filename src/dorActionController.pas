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
  SysUtils, Classes, Rtti,
  superobject,
  dorSocketStub, dorHTTPStub;

type
  NamespaceAttribute = class(TCustomAttribute)
  private
    FNamespace: string;
  public
    constructor Create(Namespace: string);
    property Namespace: string read FNamespace;
  end;

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
    FContext: TSuperRttiContext;  // in (ro)
    FSource: IReadWrite;          // in (ro)
    FParams: ISuperObject;        // in
    FRequest: THTTPMessage;       // in
    FReturn: ISuperObject;        // out
    FResponse: THTTPMessage;      // out
    FSession: ISuperObject;       // in (rw)
    FErrorCode: Integer;          // out
    FFileToSend: string;          // out

    FEtag: Boolean;
    procedure CalcETag;

    function CheckAuth(const method: string): Boolean;
  public
    type TInvokeTrace = (itBefore, itAfter, itError);
  protected
    {
      This empty method is called to force RTTI
      Could be used for something else later
    }
    class procedure Register;
    procedure TraceInvoke(When: TInvokeTrace; Result: TSuperInvokeResult); virtual;
  public
    { Uses Params and Request as input and sets Return, Response, ErrorCode and FileToSend }
    function Invoke: Boolean; virtual;

    {
      This method is called by the router in THTTPStub.ProcessRequest which creates
      an instance of TActionController.
      It sets all parameters to instance variables before calling Invoke()
    }
    function InstanceInvoke(const Context: TSuperRttiContext; const Source: IReadWrite;
      const Request: THTTPMessage; const Params: ISuperObject;
      const Response: THTTPMessage; const Return: ISuperObject;
      const Session: ISuperObject;
      var ErrorCode: Integer; var FileToSend: string): Boolean;

    property Context: TSuperRttiContext read FContext;
    property Params: ISuperObject read FParams;
    property Request: THTTPMessage read FRequest;
    property Return: ISuperObject read FReturn;
    property Response: THTTPMessage read FResponse;
    property Session: ISuperObject read FSession;

    property ErrorCode: Integer read FErrorCode write FErrorCode;

    procedure Redirect(const location: string); overload;
    procedure Redirect(const controler, action: string; const id: string = ''); overload;

    procedure SendFile(const path: string);
    procedure Send(stream: TStream = nil);

    function HaveSLL: Boolean; virtual;
    function HavePeerCertificate: Boolean; virtual;
    function SSLSubject(const key: AnsiString): AnsiString; virtual;
    function SSLIssuer(const key: AnsiString): AnsiString; virtual;

    procedure ETag;
  end;

  TActionControllerClass = class of TActionController;

implementation

uses
  dorOpenSSL;

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
      ErrorCode := 304
    else
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

  Klass := Self.ClassType;
  Typ := FContext.Context.GetType(Klass);

  { Collect Auth Attributes from action method }
  Meth := Typ.GetMethod(method);
  if Meth <> nil then
    Auth.Assign(Meth.GetAttributes);

  { And walk the ancestor hierarchy up to TActionController }
  repeat
    Auth.Assign(Typ.GetAttributes);
    Klass := Klass.ClassParent;
    Typ := FContext.Context.GetType(Klass);
  until Klass = TActionController;

  if Auth.Valid and Auth.CheckAuth.Value then
    if Session.S['user'] = Auth.User.Value then
      Result := True
    else
    begin
      Response.AsObject.S['WWW-Authenticate'] := 'Basic realm="' + Auth.Realm + '"';
      ErrorCode := 401;
      Result := False;
    end
  else
    Result := True;
end;

function TActionController.InstanceInvoke(const Context: TSuperRttiContext; const Source: IReadWrite;
  const Request: THTTPMessage; const Params: ISuperObject;
  const Response: THTTPMessage; const Return: ISuperObject;
  const Session: ISuperObject;
  var ErrorCode: Integer; var FileToSend: string): Boolean;
begin
  FContext    := Context;
  FSource     := Source;
  FParams     := Params;
  FRequest    := Request;
  FReturn     := Return;
  FResponse   := Response;
  FSession    := Session;
  FErrorCode  := ErrorCode;
  FFileToSend := FileToSend;
  try
    Result := Invoke;
  finally
    ErrorCode   := FErrorCode;
    FileToSend  := FFileToSend;
  end;
end;

function TActionController.Invoke: Boolean;
var
  method: string;
  obj: ISuperObject;
  ite: TSuperAvlEntry;
begin
  Assert(FContext  <> nil);
  // Assert(FSource   <> nil);
  Assert(FParams   <> nil);
  Assert(FRequest  <> nil);
  Assert(FReturn   <> nil);
  Assert(FResponse <> nil);
  Assert(FSession  <> nil);

  Result := False;
  FEtag := False;

  for obj in Params do
    if obj <> nil then
      obj.DataPtr := Pointer(1);

  method := Params.AsObject.S['action'] + '_' + Request.AsObject.S['method'];

  if not CheckAuth(method) then
    Exit;

  TraceInvoke(itBefore, irSuccess);

  case TrySOInvoke(FContext, Self, method, Params, obj) of
    irParamError:
      begin
        TraceInvoke(itError, irParamError);
        ErrorCode := 400;
      end;
    irError:
      begin
        TraceInvoke(itError, irError);
        ErrorCode := 500;
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
      ErrorCode := 200;

    TraceInvoke(itAfter, irSuccess);
  end;

  if FEtag then
    CalcETag;
end;

procedure TActionController.TraceInvoke(When: TInvokeTrace; Result: TSuperInvokeResult);
begin
  { Just do nothing }
end;

procedure TActionController.Send(stream: TStream);
begin
  if stream <> nil then
    FResponse.Content.LoadFromStream(stream);
end;

procedure TActionController.SendFile(const path: string);
begin
  FFileToSend := path;
end;

function TActionController.HavePeerCertificate: Boolean;
begin
  Result := FSource.HavePeerCertificate;
end;

function TActionController.HaveSLL: Boolean;
begin
  Result := FSource.IsSSL;
end;

procedure TActionController.Redirect(const location: string);
begin
  FErrorCode := 302;
  FResponse.AsObject.S['Location'] := Location;
end;

procedure TActionController.Redirect(const controler, action, id: string);
begin
  if id = '' then
    Redirect('/' + controler + '/' + action + '.' +  FParams.S['format'])
  else
    Redirect('/' + controler + '/' + action + '/' + id + '.' +  FParams.S['format']);
end;

function TActionController.SSLIssuer(const key: AnsiString): AnsiString;
begin
  Result := FSource.SSLIssuer(key);
end;

function TActionController.SSLSubject(const key: AnsiString): AnsiString;
begin
  Result := FSource.SSLSubject(key);
end;

class procedure TActionController.Register;
begin

end;

{ NamespaceAttribute }

constructor NamespaceAttribute.Create(Namespace: string);
begin
  inherited Create;
  FNamespace := LowerCase(Namespace);
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

