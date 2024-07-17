unit dorActionView;

interface

uses
  superobject,
  dorSocketStub, dorHTTPStub;

type
  TActionView = class
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
  protected
    // This empty method is called to force RTTI
    // Could be used for somethingelse later
    class procedure Register;

    procedure Render(const Obj: ISuperObject; Format: boolean = False); overload;
    procedure Render(const Str: string); overload;
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
  end;

  TActionViewClass = class of TActionView;

implementation

{ TActionView }

function TActionView.InstanceInvoke(const Context: TSuperRttiContext; const Source: IReadWrite;
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

function TActionView.Invoke: Boolean;
var
  ret: ISuperObject;
begin
  Assert(FContext  <> nil);
  Assert(FSource   <> nil);
  Assert(FParams   <> nil);
  Assert(FRequest  <> nil);
  Assert(FReturn   <> nil);
  Assert(FResponse <> nil);
  Assert(FSession  <> nil);

  Result := False;

  with FParams.AsObject do
    case TrySOInvoke(FContext, Self, S['action'] + '_' + S['format'], Return, ret) of
      irSuccess:
      begin
        Result := True;
        if ErrorCode = 0 then
          ErrorCode := 200;
      end;
      irMethodError:
        ErrorCode := 404;
      irParamError:
        ErrorCode := 400;
    else
      ErrorCode := 500;
    end;
end;

class procedure TActionView.Register;
begin

end;

procedure TActionView.Render(const Str: string);
begin
  FResponse.Content.WriteString(str, false, DEFAULT_CP);
end;

procedure TActionView.Render(const Obj: ISuperObject; Format: boolean);
begin
  Obj.SaveTo(FResponse.Content, Format);
end;

end.
