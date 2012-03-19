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

  IRedisClient = interface
    ['{109A05E5-73FE-4407-8AC6-5697647756F2}']
    procedure Open(const host: string; port: Word);
    procedure Close;
    procedure Send(const data: array of Const; const onresponse: TRedisResponse = nil);
    procedure SendSync(const data: array of Const; const onresponse: TRedisResponse = nil);

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
  private
    FReadyState: TRedisState;
    FSocket: TSocket;
    FOnError: TRedisMessage;
    FOnSynchronize: TRedisSynchronize;
    FOnOpen: TProc;
    FOnClose: TProc;
    FResponses: TQueue<TRedisResponse>;
    FRespSection: TCriticalSection;
    FSyncSignal: TEvent;
    //FSyncResponse: TRedisResponse;
    procedure Listen;
    procedure Return(const err: string; const data: TArray<string>);
  protected
    procedure Open(const host: string; port: Word);
    procedure Close;
    procedure Send(const data: array of Const; const onresponse: TRedisResponse);
    procedure SendSync(const data: array of Const; const onresponse: TRedisResponse);
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
    constructor Create; virtual;
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

procedure TRedisClient.Return(const err: string; const data: TArray<string>);
var
  callback: TRedisResponse;
begin
  FRespSection.Enter;
  try
    callback := FResponses.Dequeue();
  finally
    FRespSection.Leave;
  end;
  if Assigned(callback) then
  begin
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
  FSyncSignal.Free;
  inherited;
end;

constructor TRedisClient.Create;
begin
  inherited Create;
  FReadyState := rsClosed;
  FSocket := INVALID_SOCKET;
  FResponses := TQueue<TRedisResponse>.Create;
  FRespSection := TCriticalSection.Create;
  FSyncSignal := TEvent.Create;
end;

function TRedisClient.getReadyState: TRedisState;
begin
  Result := FReadyState;
end;

procedure TRedisClient.Listen;
begin
  TThreadIt.Create(procedure
    var
      c: AnsiChar;
      isneg: Boolean;
      st, n, count: Integer;
      item: RawByteString;
      items: TArray<string>;
    begin
      n := 0; count := 0; isneg := False;
      st := 1;
      while (FReadyState = rsOpen) and (recv(FSocket, c, 1, 0) = 1) do
      begin
        case st of
        // initial state
        1:
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
        // single line reply
        2:
          case c of
            #13: ;
            #10:
              begin
                st := 1;
                SetLength(items, 1);
                items[0] := string(UTF8String(item));
                Return('', items);
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
                Return(string(item), items);
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
                Return('', items);
              end;
          else
            Assert(False); // unexpected
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
                    Return('', items);
                  end else
                    st := 10;
                end;
              end;
          else
            Assert(False); // unexpected
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
                  Return('', items);
                end else
                  st := 10;
              end;
          else
            Assert(False); // unexpected
          end;
        // multi bulk reply
        9:
          case c of
            '0'..'9':
              count := (count * 10) + Ord(c) - Ord('0');
            #13: ;
            #10:
              if count > 0 then
                st := 10 else
              begin
                st := 1;
                Return('', items);
              end;
          else
            Assert(False); // unexpected
          end;
        10:
          case c of
            '$': st := 5;
          else
            Assert(False); // unexpected
          end;
        else
          Assert(False)// unexpected;
        end;
      end;
      if FReadyState = rsOpen then // remotely closed
        TThread.Synchronize(nil, procedure begin
          Close;
        end);
    end);
end;

procedure TRedisClient.Send(const data: array of Const; const onresponse: TRedisResponse);
var
  count, i: Integer;
  buff, item: UTF8String;
begin
  FRespSection.Enter;
  try
    FResponses.Enqueue(onresponse);
  finally
    FRespSection.Leave;
  end;

  count := Length(data);
  buff := UTF8String('*' + IntToStr(count) + #13#10);
  WinSock.send(FSocket, PAnsiChar(buff)^, Length(buff), 0);

  for i := 0 to count - 1 do
  begin
    case TVarRec(data[i]).VType of
      vtUnicodeString: item := UTF8String(string(TVarRec(data[i]).VUnicodeString));
      vtInteger : item := UTF8String(IntToStr(TVarRec(data[i]).VInteger));
      vtInt64   : item := UTF8String(IntToStr(TVarRec(data[i]).VInt64^));
      vtBoolean : item := UTF8String(BoolToStr(TVarRec(data[i]).VBoolean));
      vtChar    : item := UTF8String(TVarRec(data[i]).VChar);
      vtWideChar: item := UTF8String(TVarRec(data[i]).VWideChar);
      vtExtended: item := UTF8String(FloatToStr(TVarRec(data[i]).VExtended^));
      vtCurrency: item := UTF8String(CurrToStr(TVarRec(data[i]).VCurrency^));
      vtString  : item := UTF8String(TVarRec(data[i]).VString^);
      vtPChar   : item := UTF8String(TVarRec(data[i]).VPChar^);
      vtAnsiString: item := UTF8String(AnsiString(TVarRec(data[i]).VAnsiString));
      vtWideString: item := UTF8String(PWideChar(TVarRec(data[i]).VWideString));
      vtVariant:
        with TVarData(TVarRec(data[i]).VVariant^) do
        case VType of
          varSmallInt: item := UTF8String(IntToStr(VSmallInt));
          varInteger:  item := UTF8String(IntToStr(VInteger));
          varSingle:   item := UTF8String(FloatToStr(VSingle));
          varDouble:   item := UTF8String(FloatToStr(VDouble));
          varCurrency: item := UTF8String(CurrToStr(VCurrency));
          varOleStr:   item := UTF8String(VOleStr);
          varBoolean:  item := UTF8String(BoolToStr(VBoolean));
          varShortInt: item := UTF8String(IntToStr(VShortInt));
          varByte:     item := UTF8String(IntToStr(VByte));
          varWord:     item := UTF8String(IntToStr(VWord));
          varLongWord: item := UTF8String(IntToStr(VLongWord));
          varInt64:    item := UTF8String(IntToStr(VInt64));
          varString:   item := UTF8String(AnsiString(VString));
          varUString:  item := UTF8String(string(VUString));
        else
          item := '';
        end;
    else
      item := '';
    end;
    buff := UTF8String('$' + IntToStr(Length(item)) + #13#10);
    WinSock.send(FSocket, PAnsiChar(buff)^, Length(buff), 0);
    item := item + #13#10;
    WinSock.send(FSocket, PAnsiChar(item)^, Length(item), 0);
  end;
end;

procedure TRedisClient.SendSync(const data: array of Const;
  const onresponse: TRedisResponse);
begin
  Send(data,
    procedure(const err: string; const data: TArray<string>) begin
      onresponse(err, data);
      FSyncSignal.SetEvent;
    end);
  while FSyncSignal.WaitFor(100) = wrTimeout do
    CheckSynchronize;
  CheckSynchronize;
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

