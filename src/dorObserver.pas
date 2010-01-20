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
    class function Intercept: Boolean; virtual;
    class function IsEvent(const name: string; out event: string): Boolean; virtual;

    function Run: Cardinal; override;
    property Event: ISuperObject read FEvent;

  public
    class procedure Trigger(const Event: ISuperObject); virtual;
    constructor Create(AOwner: TDORThread); override;
    destructor Destroy; override;

    class constructor Create;
    class destructor Destroy;
  end;

implementation
uses
  dorService, SysUtils, rtti, typinfo;

type
  TEventStorage = class
  private
    FEvents: ISOCriticalObject;
    FIntercept: ISOCriticalObject;
    FObservers: ISOCriticalObject;
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

var
  EventStorage: TEventStorage;

{ TObserver }

constructor TDorObserver.Create(AOwner: TDORThread);
var
  t: TRttiType;
  m: TRttiMethod;
  l: ISuperObject;
  e: string;
begin
  inherited;
  FContext := TSuperRttiContext.Create;
  FEvents := TSOCriticalObject.Create(stArray);
  t := FContext.Context.GetType(ClassType);
  for m in t.GetMethods do
    if IsEvent(m.Name, e) then
    with EventStorage.FObservers do
      begin
        Lock;
        try
          l := AsObject[e];
          if l = nil then
          begin
            l := TSuperObject.Create(stArray);
            AsObject[e] := l;
          end;
          l.AsArray.Add(FEvents);
        finally
          Unlock;
        end;
      end;
  if Intercept then
    with EventStorage.FIntercept do
    begin
      Lock;
      try
        AsArray.Add(FEvents);
      finally
        Unlock;
      end;
    end;
end;

destructor TDorObserver.Destroy;
var
  t: TRttiType;
  m: TRttiMethod;
  l: ISuperObject;
  j: Integer;
  e: string;
begin
  t := FContext.Context.GetType(ClassType);
  for m in t.GetMethods do
    if IsEvent(m.Name, e) then
      with EventStorage.FObservers do
      begin
        Lock;
        try
          l := AsObject[e];
          if l <> nil then
          for j := 0 to l.AsArray.Length - 1 do
            if l.AsArray[j] = FEvents then
            begin
              l.AsArray.Delete(j);
              Break;
            end;
        finally
          Unlock;
        end;
      end;

  if Intercept then
    with EventStorage.FIntercept do
    begin
      Lock;
      try
        for j := 0 to AsArray.Length - 1 do
          if AsArray[j] = FEvents then
          begin
            AsArray.Delete(j);
            Break;
          end;
      finally
        Unlock;
      end;
    end;

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

class function TDorObserver.Intercept: Boolean;
begin
  Result := False;
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

class procedure TDorObserver.Trigger(const Event: ISuperObject);
begin
  EventStorage.Trigger(Event);
end;

procedure TDorObserver.ProcessEvents;
var
  Event: ISuperObject;
begin
  for Event in FEvents.Extract do
    doOnEvent(Event);
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

class constructor TDorObserver.Create;
begin
  EventStorage := TEventStorage.Create;
  Application.CreateThread(TEventProcessor);
end;

class destructor TDorObserver.Destroy;
begin
  while TDORThread.ThreadCount > 0 do Sleep(200);
  EventStorage.Free;
end;

{ TEventStorage }

constructor TEventStorage.Create;
begin
  FEvents := TSOCriticalObject.Create(stArray);
  FObservers := TSOCriticalObject.Create(stObject);
  FIntercept := TSOCriticalObject.Create(stArray);
end;

destructor TEventStorage.Destroy;
begin
  {TODO -ohenri : Sérialiser}
  inherited;
end;

function TEventStorage.Empty: ISuperObject;
begin
  FEvents.Lock;
  try
    if FEvents.AsArray.Length > 0 then
      Result := FEvents.Extract else
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
      begin
        EventStorage.FIntercept.Lock;
        try
          for box in EventStorage.FIntercept do
          begin
            ISOCriticalObject(box).Lock;
            try
              box.AsArray.Add(event.Clone);
            finally
              ISOCriticalObject(box).Unlock;
            end;
          end;
        finally
          EventStorage.FIntercept.Unlock;
        end;

        EventStorage.FObservers.Lock;
        try
          for box in EventStorage.FObservers.N[event.AsObject.S['event']] do
          begin
            ISOCriticalObject(box).Lock;
            try
              box.AsArray.Add(event.Clone);
            finally
              ISOCriticalObject(box).Unlock;
            end;
          end;
        finally
          EventStorage.FObservers.Unlock;
        end;
      end;
    sleep(1);
  end;
  Result := 0;
end;

end.
