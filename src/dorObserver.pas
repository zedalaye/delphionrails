unit dorObserver;

interface

uses
{$if defined(DEBUG)}Classes,{$ifend}
  dorSocketStub, superobject, dorUtils;

type
  TDorObserver = class(TCustomObserver)
  private
    FEvent: ISuperObject;
    FContext: TSuperRttiContext;
  protected
    class function IsEvent(const name: string; out event: string): Boolean; virtual;
    procedure doOnEvent(const Event: ISuperObject); override;
    function Run: Cardinal; override;
    property Event: ISuperObject read FEvent;
  public
    constructor Create(AOwner: TDORThread); override;
    destructor Destroy; override;
  end;

implementation
uses
{$ifdef madExcept}
  madExcept,
{$endif}
  Windows, dorService, SysUtils, rtti, typinfo;

{ TDorObserver }

constructor TDorObserver.Create(AOwner: TDORThread);
var
  t: TRttiType;
  m: TRttiMethod;
  e: string;
begin
  inherited;
  FContext := TSuperRttiContext.Create;
  t := FContext.Context.GetType(ClassType);
  for m in t.GetMethods do
    if IsEvent(m.Name, e) then
      RegisterEvent(e);
end;

destructor TDorObserver.Destroy;
var
  t: TRttiType;
  m: TRttiMethod;
  e: string;
begin
  t := FContext.Context.GetType(ClassType);
  for m in t.GetMethods do
    if IsEvent(m.Name, e) then
      UnregisterEvent(e);
  FContext.Free;
  inherited;
end;

procedure TDorObserver.doOnEvent(const Event: ISuperObject);
begin
  FEvent := Event;
  try
    SOInvoke(Self, Event.AsObject.S['event'] + '_event', Event, FContext);
  finally
    FEvent := nil;
  end;
end;

class function TDorObserver.IsEvent(const name: string;
  out event: string): Boolean;
var
  p: PChar;
begin
  p := StrRScan(PChar(name), '_');
  if p <> nil then
  begin
    if lowercase(p) = '_event' then
    begin
      event := LowerCase(copy(name, 1, p - PChar(name)));
      Result := true;
    end else
      Result := False;
  end else
    Result := False;
end;

function TDorObserver.Run: Cardinal;
begin
{$if defined(DEBUG)}
  TThread.NameThreadForDebugging(AnsiString(Self.ClassName));
{$ifend}

  while not Stopped do
  begin
    try
      ProcessEvents;
    except
{$ifdef madExcept}
      on E: Exception do
        HandleException(etNormal);
{$endif}
    end;
{$IFDEF SWITCHTOTHREAD}
    if not SwitchToThread then
{$ENDIF}
      sleep(1);
  end;
  Result := 0;
end;

end.
