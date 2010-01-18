unit dorObserver;

interface
uses dorSocketStub, superobject, dorUtils;

type

  TDorObserver = class(TDORThread)
  private
    FEvents: ISOCriticalObject;
    FEvent: ISuperObject;
    FContext: TSuperRttiContext;
  protected
    procedure doOnEvent(const Event: ISuperObject); virtual;
    procedure ProcessEvents; virtual;
    class procedure Trigger(const Event: ISuperObject); virtual;
    function Run: Cardinal; override;
    property Event: ISuperObject read FEvent;
  public
    constructor Create(AOwner: TDORThread); override;
    destructor Destroy; override;
  end;

  TEventStorage = class
  private
    FEvents: ISOCriticalObject;
    FObservers: ISuperObject;
    function Empty: ISuperObject;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Trigger(const Event: ISuperObject);
  end;

  TEventProcessor = class(TDORThread)
  protected
    function Run: Cardinal; override;
  end;

implementation
uses
  dorService, SysUtils, rtti, typinfo;

var
  EventStorage: TEventStorage;

{ TObserver }

constructor TDorObserver.Create(AOwner: TDORThread);
var
  t: TRttiType;
  m: TRttiMethod;
  l: ISuperObject;
begin
  inherited;
  FContext := TSuperRttiContext.Create;
  FEvents := TSOCriticalObject.Create(stArray);
  t := FContext.Context.GetType(ClassType);
  for m in t.GetMethods do
    if m.Visibility = mvPublic then
    begin
      l := EventStorage.FObservers.AsObject[lowercase(m.Name)];
      if l = nil then
      begin
        l := TSuperObject.Create(stArray);
        EventStorage.FObservers.AsObject[lowercase(m.Name)] := l;
      end;
      l.AsArray.Add(FEvents);
    end;
end;

destructor TDorObserver.Destroy;
begin
  FContext.Free;
  inherited;
end;

procedure TDorObserver.doOnEvent(const Event: ISuperObject);
begin
  FEvent := Event;
  try
    SOInvoke(Self, Event.AsObject.s['event'], Event, FContext);
  finally
    FEvent := nil;
  end;
end;

class procedure TDorObserver.Trigger(const Event: ISuperObject);
begin
  EventStorage.Trigger(Event);
end;

procedure TDorObserver.ProcessEvents;
var
  Event: ISuperObject;
begin
  FEvents.Lock;
  try
    for Event in FEvents do
      doOnEvent(Event);
    FEvents.Clear;
  finally
    FEvents.Unlock;
  end;
end;

function TDorObserver.Run: Cardinal;
begin
  while not Stopped do
  begin
    ProcessEvents;
    sleep(1);
  end;
  Result := 0;
end;

{ TEventStorage }

constructor TEventStorage.Create;
begin
  FEvents := TSOCriticalObject.Create(stArray);
  FObservers := TSuperObject.Create(stObject);
end;

destructor TEventStorage.Destroy;
begin
  {TODO -ohenri : Sérialiser}
  inherited;
end;

function TEventStorage.Empty: ISuperObject;
var
  obj: ISuperObject;
begin
  FEvents.Lock;
  try
    if FEvents.AsArray.Length > 0 then
    begin
      Result := TSuperObject.Create(stArray);
      for obj in FEvents do
        Result.AsArray.Add(obj);
      FEvents.Clear;
    end else
      Result := nil;
  finally
    FEvents.Unlock;
  end;
end;

procedure TEventStorage.Trigger(const Event: ISuperObject);
begin
  FEvents.Lock;
  try
    FEvents.AsArray.Add(Event);
  finally
    FEvents.Unlock;
  end;
end;

{ TEventProcessor }

function TEventProcessor.Run: Cardinal;
var
  events, event, box: ISuperObject;
begin
  while not Stopped do
  begin
    events := EventStorage.Empty;
    if events <> nil then
      for event in events do
        for box in EventStorage.FObservers.N[event.AsObject.S['event']] do
        begin
          ISOCriticalObject(box).Lock;
          try
            box.AsArray.Add(event.Clone);
          finally
            ISOCriticalObject(box).Unlock;
          end;
        end;
    sleep(1);
  end;
  Result := 0;
end;

initialization
  EventStorage := TEventStorage.Create;
  Application.CreateThread(TEventProcessor);

finalization
  while TDORThread.ThreadCount > 0 do Sleep(200);
  EventStorage.Free;

end.
