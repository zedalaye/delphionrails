unit dorRedis;

interface
uses SysUtils, WinSock, SyncObjs, Generics.Collections;

type
  TRedisState = (rsConnecting, rsOpen, rsClosing, rsClosed);
  TRedisMessage = reference to procedure(const err: string);
  TRedisResponse = reference to procedure(const err: string;
    const data: TArray<string>);
  TRedisSynchronize = reference to procedure(const res: TRedisResponse;
    const err: string; const data: TArray<string>);
  TRedisGetParam = reference to function(index: Integer): string;

  IRedisClient = interface
    ['{109A05E5-73FE-4407-8AC6-5697647756F2}']
    procedure Open(const host: string; port: Word = 6379);
    procedure Close;
    procedure Call(count: Integer; const data: TRedisGetParam; const onresponse: TRedisResponse = nil);
    procedure Send(const data: array of Const; const onresponse: TRedisResponse = nil);

    function getReadyState: TRedisState;
    function GetOnClose: TProc;
    function GetOnError: TRedisMessage;
    function GetOnOpen: TProc;
    function GetOnSynchronize: TRedisSynchronize;

    procedure SetOnClose(const value: TProc);
    procedure SetOnError(const value: TRedisMessage);
    procedure SetOnOpen(const value: TProc);
    procedure SetOnSynchronize(const sync: TRedisSynchronize);

    property OnOpen: TProc read GetOnOpen write SetOnOpen;
    property OnClose: TProc read GetOnClose write SetOnClose;
    property OnError: TRedisMessage read GetOnError write SetOnError;
    property OnSynchronize: TRedisSynchronize read GetOnSynchronize write SetOnSynchronize;
  end;

  TRedisClient = class(TInterfacedObject, IRedisClient)
  type
    TCommand = (cmdMulti, cmdExec, cmdDiscard, cmdOther);
    TMultiState = (msNone, msMulti, msExec, msReturn);
    TResponseEntry = record
      command: TCommand;
      response: TRedisResponse;
    end;
  private
    FReadyState: TRedisState;
    FSocket: TSocket;
    FOnError: TRedisMessage;
    FOnSynchronize: TRedisSynchronize;
    FOnOpen: TProc;
    FOnClose: TProc;
    FResponses: TQueue<TResponseEntry>;
    FRespSection: TCriticalSection;
    FFormatSettings: TFormatSettings;
    FSync: Boolean;
    // Thread variables
    FMulti: TMultiState;
    FMultiresponses: TQueue<TRedisResponse>;
    procedure Listen;
    procedure Return(const callback: TRedisResponse; const err: string; const data: TArray<string>);
  protected
    procedure Open(const host: string; port: Word);
    procedure Close;
    procedure Call(count: Integer; const getData: TRedisGetParam; const onresponse: TRedisResponse);
    procedure Send(const data: array of Const; const onresponse: TRedisResponse);
    function getReadyState: TRedisState;
    function GetOnClose: TProc;
    function GetOnError: TRedisMessage;
    function GetOnOpen: TProc;
    function GetOnSynchronize: TRedisSynchronize;
    procedure SetOnClose(const value: TProc);
    procedure SetOnError(const value: TRedisMessage);
    procedure SetOnOpen(const value: TProc);
    procedure SetOnSynchronize(const sync: TRedisSynchronize);
  public
    constructor Create(sync: Boolean); virtual;
    destructor Destroy; override;
    class constructor Create;
    class destructor Destroy;
  end;


implementation
uses Classes;

(******************************************************************************)
(* TThreadIt                                                                  *)
(* run anonymous method in a thread                                           *)
(******************************************************************************)

type
  TThreadIt = class(TThread)
  private
    FProc: TProc;
  protected
    procedure Execute; override;
    constructor Create(const proc: TProc);
  end;

  constructor TThreadIt.Create(const proc: TProc);
  begin
    FreeOnTerminate := True;
    FProc := proc;
    inherited Create(False);
  end;

  procedure TThreadIt.Execute;
  begin
    FProc();
  end;

{ TRedisClient }

procedure TRedisClient.Call(count: Integer; const getData: TRedisGetParam;
  const onresponse: TRedisResponse);

function ParseCommand(const cmd: string): TCommand;
begin
  Result := cmdOther;
  case cmd[1] of
    'm', 'M': if SameText(cmd, 'multi') then Result := cmdMulti;
    'e', 'E': if SameText(cmd, 'exec') then Result := cmdExec;
    'd', 'D': if SameText(cmd, 'discard') then Result := cmdDiscard;
  end;
end;

var
  i: Integer;
  buff, item: UTF8String;
  entry: TResponseEntry;
begin
  buff := UTF8String('*' + IntToStr(count) + #13#10);
  WinSock.send(FSocket, PAnsiChar(buff)^, Length(buff), 0);

  entry.command := ParseCommand(getData(0));
  entry.response := onresponse;

  FRespSection.Enter;
  try
    FResponses.Enqueue(entry);
  finally
    FRespSection.Leave;
  end;

  for i := 0 to count - 1 do
  begin
    item := utf8string(getData(i));
    buff := UTF8String('$' + IntToStr(Length(item)) + #13#10);
    WinSock.send(FSocket, PAnsiChar(buff)^, Length(buff), 0);
    item := item + #13#10;
    WinSock.send(FSocket, PAnsiChar(item)^, Length(item), 0);
  end;
  if FSync then
    Listen;
end;

procedure TRedisClient.Close;
begin
  if FReadyState = rsOpen then
  begin
    FReadyState := rsClosing;
    closesocket(FSocket);
    FSocket := INVALID_SOCKET;
    FReadyState := rsClosed;
    if Assigned(FOnClose) then
      FOnClose();
  end;
end;

procedure TRedisClient.Open(const host: string; port: Word);
var
  phost: PHostEnt;
  addr: TSockAddrIn;
  domain: AnsiString;
begin
  if FReadyState <> rsClosed then
    Exit;

  FReadyState := rsConnecting;
  domain := AnsiString(host);

  // find host
  phost := gethostbyname(PAnsiChar(domain));
  if phost = nil then
  begin
    if Assigned(FOnError) then
      FOnError(Format('Host not found: %s', [host]));
    Exit;
  end;

  // socket
  FSocket := socket(AF_INET, SOCK_STREAM, 0);
  try
    if FSocket = INVALID_SOCKET then
    begin
      if Assigned(FOnError) then
        FOnError('Unexpected error: can''t allocate socket handle.');
      Exit;
    end;

    // connect
    FillChar(addr, SizeOf(addr), 0);
    addr.sin_family := AF_INET;
    addr.sin_port := htons(port);
    addr.sin_addr.S_addr := PInteger(phost.h_addr^)^;
    if connect(FSocket, addr, SizeOf(addr)) <> 0 then
    begin
      if Assigned(FOnError) then
        FOnError(format('Cant''t connect to host: %s:%d', [host, port]));
      Exit;
    end;

    FReadyState := rsOpen;
    if Assigned(FOnOpen) then
      FOnOpen();
    if not FSync then
      Listen;
  finally
    if FReadyState = rsConnecting then
    begin
      closesocket(FSocket);
      FSocket := INVALID_SOCKET;
      FReadyState := rsClosed;
    end;
  end;
end;

procedure TRedisClient.Return(const callback: TRedisResponse; const err: string; const data: TArray<string>);
begin
  if Assigned(callback) then
  begin
    if FSync then
      callback(err, data) else
      if Assigned(FOnSynchronize) then
        FOnSynchronize(callback, err, data) else
        TThreadIt.Synchronize(nil, procedure begin
          callback(err, data);
        end);
  end;
end;

class constructor TRedisClient.Create;
var
  Data: TWSAData;
begin
  WSAStartup($0202, Data);
end;

destructor TRedisClient.Destroy;
begin
  Close;
  FResponses.Free;
  FRespSection.Free;
  FMultiresponses.Free;
  inherited;
end;

constructor TRedisClient.Create(sync: Boolean);
begin
  inherited Create;
  FSync := sync;
  FMulti := msNone;
  FMultiresponses := TQueue<TRedisResponse>.Create;
  FFormatSettings := TFormatSettings.Create;
  FFormatSettings.DecimalSeparator := '.';
  FReadyState := rsClosed;
  FSocket := INVALID_SOCKET;
  FResponses := TQueue<TResponseEntry>.Create;
  FRespSection := TCriticalSection.Create;
end;

function TRedisClient.getReadyState: TRedisState;
begin
  Result := FReadyState;
end;

procedure TRedisClient.Listen;
var
  method: TProc;
begin
  method := procedure
    var
      c: AnsiChar;
      isneg: Boolean;
      st, n, count: Integer;
      item: RawByteString;
      items, dummy: TArray<string>;
      e: TResponseEntry;

      function Invoke(const err: string = ''): Boolean;
      begin
        case FMulti of
          msNone:
          begin
            Return(e.response, err, items);
            Result := FSync;
          end;
          msMulti:
            begin
              case e.command of
                cmdExec, cmdDiscard:
                  Return(e.response, err, items);
              end;
              Result := FSync;
            end;
          msReturn:
            begin
              Return(e.response, err, items);
              if FMultiresponses.Count = 1 then
              begin
                FMulti := msNone;
                Return(FMultiresponses.Dequeue(), '', dummy);
                Result := FSync;
              end else
                Result := False;
            end;
        else
          Result := FSync;
        end;
      end;

      procedure getstate;
      begin
        if FMulti <> msReturn then
        begin
          FRespSection.Enter;
          try
            e := FResponses.Dequeue;
          finally
            FRespSection.Leave;
          end;

          case e.command of
            cmdOther:
              if (FMulti = msMulti) then
                FMultiresponses.Enqueue(e.response);
            cmdMulti:
              begin
                if FMulti <> msNone then
                  raise Exception.Create('MULTI calls can not be nested');
                FMulti := msMulti;
              end;
            cmdExec:
              begin
                if FMulti <> msMulti then
                  raise Exception.Create('EXEC without MULTI');
                FMultiresponses.Enqueue(e.response);
                FMulti := msExec;
              end;
            cmdDiscard:
              begin
                if FMulti <> msMulti then
                  raise Exception.Create('DISCARD without MULTI');
                FMulti := msNone;
                FMultiresponses.Clear;
              end;
          end;
        end else
        begin
          e.command := cmdOther;
          e.response := FMultiresponses.Dequeue();
        end;
      end;

      procedure unexpected;
      begin
        raise Exception.Create('unexpected');
      end;
    begin
      n := 0; count := 0; isneg := False;
      st := 1;
      while (FReadyState = rsOpen) and (recv(FSocket, c, 1, 0) = 1) do
      begin
        case st of
        // initial state
        1:
          begin
            getstate;
            case c of
              '+':
                begin
                  st := 2;
                  item := '';
                end;
              '-':
                begin
                  st := 3;
                  item := '';
                end;
              ':':
                begin
                  st := 4;
                  n := 0;
                end;
              '$':
                begin
                  count := 1;
                  st := 5;
                  SetLength(items, 0);
                end;
              '*':
                begin
                  st := 9;
                  SetLength(items, 0);
                  count := 0;
                end;
            else
              st := 0;
            end;
          end;
        // single line reply
        2:
          case c of
            #13: ;
            #10:
              begin
                st := 1;
                SetLength(items, 1);
                items[0] := string(UTF8String(item));
                if invoke() then Exit;
              end;
          else
            item := item + c;
          end;
        // error message
        3 :
          case c of
            #13: ;
            #10:
              begin
                st := 1;
                SetLength(items, 0);
                if invoke(string(item)) then Exit;
              end;
          else
            item := item + c;
          end;
        // integer reply
        4:
          case c of
            '0'..'9':
              n := (n * 10) + Ord(c) - Ord('0');
            #13: ;
            #10:
              begin
                st := 1;
                SetLength(items, 1);
                items[0] := IntToStr(n);
                if invoke() then Exit;
              end;
          else
            unexpected
          end;
        // bulk reply
        5:
          begin
            if c <> '-' then
            begin
              n := Ord(c) - Ord('0');
              isneg := False;
            end else
            begin
              n := 0;
              isneg := True;
            end;
            st := 6;
          end;
        6:
          case c of
            '0'..'9':
              n := (n * 10) + Ord(c) - Ord('0');
            #13: ;
            #10:
              begin
                if not isneg then
                begin
                  if n > 0 then
                    st := 7 else
                    st := 8;
                  item := '';
                end else
                begin
                  dec(count);
                  if count = 0 then
                  begin
                    st := 1;
                    if invoke() then Exit;
                  end else
                    st := 10;
                end;
              end;
          else
            unexpected
          end;
        7:
          begin
            dec(n);
            item := item + c;
            if n = 0 then st := 8;
          end;
        8:
          case c of
            #13: ;
            #10:
              begin
                SetLength(items, Length(items) + 1);
                items[Length(items) - 1] := string(UTF8String(item));
                dec(count);
                if count = 0 then
                begin
                  st := 1;
                  if invoke() then Exit;
                end else
                  st := 10;
              end;
          else
            unexpected
          end;
        // FMulti bulk reply
        9:
          case c of
            '0'..'9':
              count := (count * 10) + Ord(c) - Ord('0');
            #13: ;
            #10:
              case FMulti of
              msExec:
                begin
                  st := 1;
                  if count > 0 then
                    FMulti := msReturn else
                    begin
                      FMulti := msNone;
                      if invoke() then Exit;
                    end;
                end;
              else
                if count > 0 then
                  st := 10 else
                begin
                  st := 1;
                  if invoke() then Exit;
                end;
              end;
          else
            unexpected
          end;
        10:
          case c of
            '$': st := 5;
          else
            unexpected
          end;
        else
          unexpected
        end;
      end;
      if FReadyState = rsOpen then // remotely closed
        if FSync then
          Close else
          TThread.Synchronize(nil, procedure begin Close end);
    end;
  if FSync then
    method() else
    TThreadIt.Create(method);
end;

procedure TRedisClient.Send(const data: array of Const; const onresponse: TRedisResponse);
type
  TVarRecArray = array[0..0] of TVarRec;
  PVarRecArray = ^TVarRecArray;
var
  len: Integer;
  arr: PVarRecArray;
begin
  len := Length(data);
  arr := @data[0];
  Call(len,
    function(index: Integer): string
    var
      item: PVarRec;
    begin
      item := @arr[index];
      case item.VType of
        vtUnicodeString: Result := string(item.VUnicodeString);
        vtInteger : Result := IntToStr(item.VInteger);
        vtInt64   : Result := IntToStr(item.VInt64^);
        vtBoolean : Result := BoolToStr(item.VBoolean);
        vtChar    : Result := string(item.VChar);
        vtWideChar: Result := string(item.VWideChar);
        vtExtended: Result := FloatToStr(item.VExtended^, FFormatSettings);
        vtCurrency: Result := CurrToStr(item.VCurrency^, FFormatSettings);
        vtString  : Result := string(item.VString^);
        vtPChar   : Result := string(AnsiString(item.VPChar));
        vtAnsiString: Result := string(AnsiString(item.VAnsiString));
        vtWideString: Result := string(PWideChar(item.VWideString));
        vtVariant:
          with TVarData(item.VVariant^) do
          case VType of
            varSmallInt: Result := IntToStr(VSmallInt);
            varInteger:  Result := IntToStr(VInteger);
            varSingle:   Result := FloatToStr(VSingle, FFormatSettings);
            varDouble:   Result := FloatToStr(VDouble, FFormatSettings);
            varCurrency: Result := CurrToStr(VCurrency, FFormatSettings);
            varOleStr:   Result := string(VOleStr);
            varBoolean:  Result := BoolToStr(VBoolean);
            varShortInt: Result := IntToStr(VShortInt);
            varByte:     Result := IntToStr(VByte);
            varWord:     Result := IntToStr(VWord);
            varLongWord: Result := IntToStr(VLongWord);
            varInt64:    Result := IntToStr(VInt64);
            varString:   Result := string(AnsiString(VString));
            varUString:  Result := string(VUString);
          else
            Result := '';
          end;
      else
        Result := '';
      end;
    end,
    onresponse);
end;

procedure TRedisClient.SetOnClose(const value: TProc);
begin
  FOnClose := value;
end;

procedure TRedisClient.SetOnError(const value: TRedisMessage);
begin
  FOnError := value;
end;

procedure TRedisClient.SetOnOpen(const value: TProc);
begin
  FOnOpen := value;
end;

procedure TRedisClient.SetOnSynchronize(const sync: TRedisSynchronize);
begin
  FOnSynchronize := sync;
end;

class destructor TRedisClient.Destroy;
begin
  WSACleanup;
end;

function TRedisClient.GetOnClose: TProc;
begin
  Result := FOnClose;
end;

function TRedisClient.GetOnError: TRedisMessage;
begin
  Result := FOnError;
end;

function TRedisClient.GetOnOpen: TProc;
begin
  Result := FOnOpen;
end;

function TRedisClient.GetOnSynchronize: TRedisSynchronize;
begin
  Result := FOnSynchronize
end;

end.

