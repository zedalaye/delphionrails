unit dorRedis;

interface
uses SysUtils, WinSock, SyncObjs, Generics.Collections;

type
  TRedisState = (rsConnecting, rsOpen, rsClosing, rsClosed);
  TProcByte = reference to procedure(value: Byte);
  TProcInteger = reference to procedure(value: Integer);
  TProcCardinal = reference to procedure(value: Cardinal);
  TProcInt64 = reference to procedure(value: Int64);
  TProcUInt64 = reference to procedure(value: UInt64);
  TProcBoolean = reference to procedure(value: Boolean);
  TProcString = reference to procedure(const value: string);
  TProcDouble = reference to procedure(value: Double);
  TProcKeyValue = reference to procedure(const key, value: string);

  TRedisSynchronize = reference to procedure(const res: TProcString;
    const value: string);
  TRedisGetParam = reference to function(index: Integer): string;

  IRedisClient = interface
    ['{109A05E5-73FE-4407-8AC6-5697647756F2}']
    procedure Open(const host: string; port: Word = 6379);
    procedure Close;
    procedure Call(count: Cardinal; const getData: TRedisGetParam;
      const onresponse: TProcString = nil;
      const onerror: TProcString = nil;
      const onreturn: TProcString = nil); overload;
    procedure Call(const data: array of Const;
      const onresponse: TProcString = nil;
      const onerror: TProcString = nil;
      const onreturn: TProcString = nil); overload;
    procedure Call(const data: string;
      const onresponse: TProcString = nil;
      const onerror: TProcString = nil;
      const onreturn: TProcString = nil); overload;
    function getReadyState: TRedisState;
    function GetOnClose: TProc;
    function GetOnError: TProcString;
    function GetOnOpen: TProc;
    function GetOnSynchronize: TRedisSynchronize;

    procedure SetOnClose(const value: TProc);
    procedure SetOnError(const value: TProcString);
    procedure SetOnOpen(const value: TProc);
    procedure SetOnSynchronize(const sync: TRedisSynchronize);

    property OnOpen: TProc read GetOnOpen write SetOnOpen;
    property OnClose: TProc read GetOnClose write SetOnClose;
    property OnError: TProcString read GetOnError write SetOnError;
    property OnSynchronize: TRedisSynchronize read GetOnSynchronize write SetOnSynchronize;
  end;

  IRedisClientSync = interface(IRedisClient)
  ['{E6EB6CD5-262E-49A7-863A-F84FB65DE421}']
    procedure Auth(const password: string);
    procedure BGRewriteAOF;
    procedure BGSave;
    // keys
    function Del(const keys: array of const): Integer;
    function Exists(const key: string): Boolean;
    function Expire(const key: string; timeout: Cardinal): Boolean;
    function ExpireAt(const key: string; date: TDateTime): Boolean;
    function Keys(const pattern: string; const onkey: TProcString = nil): Int64;
    function Move(const key: string; db: Word): Boolean;
    function ObjectRefcount(const key: string): Integer;
    function ObjectEncoding(const key: string): string;
    function ObjectIdletime(const key: string): Int64;
    function Persist(const key: string): Boolean;
    function PExpire(const key: string; milliseconds: UInt64): Boolean;
    function PExpireAt(const key: string; date: TDateTime): Boolean;
    function PTTL(const key: string): UInt64;
    function RandomKey: string;
    procedure Rename(const key, newkey: string);
    function RenameNX(const key, newkey: string): Boolean;
    function Sort(const pattern: string; const onkey: TProcString = nil): Int64;
    // string
    function Append(const key, value: string): Int64;
    function Decr(const key: string): Int64;
    function DecrBy(const key: string; decrement: Int64): Int64;
    function Get(const key: string): string;
    function GetBit(const key: string; offset: Cardinal): Boolean;
    function GetRange(const key: string; start, stop: Integer): string;
    function GetSet(const key, value: string): string;
    function Incr(const key: string): Int64;
    function IncrBy(const key: string; increment: Int64): Int64;
    function IncrByFloat(const key: string; increment: Double): Double;
    procedure MGet(const keys: array of const; const onvalue: TProcString);
    procedure MSet(const items: array of const);
    function MSetNX(const items: array of const): Boolean;
    procedure PSetEx(const key: string; milliseconds: UInt64; const value: string);
    procedure Put(const key, value: string);
    function SetBit(const key: string; offset: Cardinal; value: Boolean): Boolean;
    procedure SetEx(const key: string; seconds: Cardinal; const value: string);
    function SetNX(const key, value: string): Boolean;
    function SetRange(const key: string; offset: Cardinal; const value: string): Cardinal;
    function StrLen(const key: string): Cardinal;
    // hash
    function HDel(const key: string; const fields: array of const): Cardinal;
    function HExists(const key, field: string): Boolean;
    function HGet(const key, field: string): string;
    procedure HGetAll(const key: string; const onKeyValue: TProcKeyValue);
    function HIncrBy(const key, field: string; increment: Int64): Int64;
    function HIncrByFloat(const key, field: string; increment: Double): Double;
    procedure HKeys(const key: string; const onkey: TProcString);
    function HLen(const key: string): Cardinal;
    procedure HMGet(const key: string; const fields: array of const; const onValue: TProcString);
    procedure HMSet(const key: string; const items: array of const);
    function HSet(const key, field, value: string): Boolean;
    function HSetNX(const key, field, value: string): Boolean;
    procedure HVals(const key: string; const onValue: TProcString);
  end;

  IRedisClientAsync = interface(IRedisClient)
  ['{C234E5CE-199E-4D68-8877-6FC82B8DB1D6}']
    procedure Auth(const password: string; const Result: TProc = nil);
    procedure BGRewriteAOF(const Result: TProc = nil);
    procedure BGSave(const Result: TProc = nil);
    //keys
    procedure Del(const keys: array of const; const Result: TProcInteger = nil);
    procedure Exists(const key: string; const Result: TProcBoolean);
    procedure Expire(const key: string; timeout: Cardinal; const Result: TProcBoolean = nil);
    procedure ExpireAt(const key: string; date: TDateTime; const Result: TProcBoolean = nil);
    procedure Keys(const pattern: string; const onkey: TProcString; const Result: TProc = nil);
    procedure Move(const key: string; db: Word; const Result: TProcBoolean);
    procedure ObjectRefcount(const key: string; const Result: TProcInteger);
    procedure ObjectEncoding(const key: string; const Result: TProcString);
    procedure ObjectIdletime(const key: string; const Result: TProcInt64);
    procedure Persist(const key: string; const Result: TProcBoolean);
    procedure PExpire(const key: string; milliseconds: UInt64; const Result: TProcBoolean);
    procedure PExpireAt(const key: string; date: TDateTime; const Result: TProcBoolean = nil);
    procedure PTTL(const key: string; const Result: TProcUInt64);
    procedure RandomKey(const Result: TProcString);
    procedure Rename(const key, newkey: string; const Result: TProc = nil);
    procedure RenameNX(const key, newkey: string; const Result: TProcBoolean = nil);
    procedure Sort(const pattern: string; const onkey: TProcString; const Result: TProc = nil);
    // string
    procedure Append(const key, value: string; const Result: TProcInt64);
    procedure Decr(const key: string; const Result: TProcInt64 = nil);
    procedure DecrBy(const key: string; decrement: Int64; const Result: TProcInt64 = nil);
    procedure Get(const key: string; const Result: TProcString);
    procedure GetBit(const key: string; offset: Cardinal; const Result: TProcBoolean);
    procedure GetRange(const key: string; start, stop: Integer; const Result: TProcString);
    procedure GetSet(const key, value: string; const Result: TProcString);
    procedure Incr(const key: string; const Result: TProcInt64 = nil);
    procedure IncrBy(const key: string; increment: Int64; const Result: TProcInt64 = nil);
    procedure IncrByFloat(const key: string; increment: Double; const Result: TProcDouble = nil);
    procedure MGet(const keys: array of const; const onvalue: TProcString; const Result: TProc = nil);
    procedure MSet(const items: array of const; const Result: TProc = nil);
    procedure MSetNX(const items: array of const; const Result: TProcBoolean = nil);
    procedure PSetEx(const key: string; milliseconds: UInt64; const value: string; const Result: TProc = nil);
    procedure Put(const key, value: string; const Result: TProc = nil);
    procedure SetBit(const key: string; offset: Cardinal; value: Boolean; const Result: TProcBoolean = nil);
    procedure SetEx(const key: string; seconds: Cardinal; const value: string; const Result: TProc = nil);
    procedure SetNX(const key, value: string; const Result: TProcBoolean = nil);
    procedure SetRange(const key: string; offset: Cardinal; const value: string; const Result: TProcCardinal);
    procedure StrLen(const key: string; const Result: TProcCardinal);
    // hash
    procedure HDel(const key: string; const fields: array of const; const Result: TProcCardinal = nil);
    procedure HExists(const key, field: string; const Result: TProcBoolean);
    procedure HGet(const key, field: string; const Result: TProcString);
    procedure HGetAll(const key: string; const onKeyValue: TProcKeyValue; const Result: TProc = nil);
    procedure HIncrBy(const key, field: string; increment: Int64; const Result: TProcInt64 = nil);
    procedure HIncrByFloat(const key, field: string; increment: Double; const Result: TProcDouble = nil);
    procedure HKeys(const key: string; const onkey: TProcString; const Result: TProc = nil);
    procedure HLen(const key: string; const Result: TProcCardinal);
    procedure HMGet(const key: string; const fields: array of const; const onValue: TProcString; const Result: TProc = nil);
    procedure HMSet(const key: string; const items: array of const; const Result: TProc = nil);
    procedure HSet(const key, field, value: string; const Result: TProcBoolean = nil);
    procedure HSetNX(const key, field, value: string; const Result: TProcBoolean = nil);
    procedure HVals(const key: string; const onValue: TProcString; const Result: TProc = nil);
  end;

  TRedisClient = class(TInterfacedObject, IRedisClient)
  type
    TCommand = (cmdMulti, cmdExec, cmdDiscard, cmdOther);
    TMultiState = (msNone, msMulti, msExec, msReturn);
    TResponseEntry = record
      command: TCommand;
      onerror: TProcString;
      onresponse: TProcString;
      onreturn: TProcString;
    end;
  private
    FReadyState: TRedisState;
    FSocket: TSocket;
    FOnError: TProcString;
    FOnSynchronize: TRedisSynchronize;
    FOnOpen: TProc;
    FOnClose: TProc;
    FResponses: TQueue<TResponseEntry>;
    FRespSection: TCriticalSection;
    FFormatSettings: TFormatSettings;
    FSync: Boolean;
    // Thread variables
    FMulti: TMultiState;
    FMultiresponses: TQueue<TResponseEntry>;
    procedure Listen;
    procedure Return(const callback: TProcString; const value: string = '');
  protected
    procedure Open(const host: string; port: Word);
    procedure Close;
    procedure Call(count: Cardinal; const getData: TRedisGetParam;
      const onresponse: TProcString = nil;
      const onerror: TProcString = nil;
      const onreturn: TProcString = nil); overload;
    procedure Call(const data: array of Const;
      const onresponse: TProcString = nil;
      const onerror: TProcString = nil;
      const onreturn: TProcString = nil); overload;
    procedure Call(const data: string;
      const onresponse: TProcString = nil;
      const onerror: TProcString = nil;
      const onreturn: TProcString = nil); overload;
    function getReadyState: TRedisState;
    function GetOnClose: TProc;
    function GetOnError: TProcString;
    function GetOnOpen: TProc;
    function GetOnSynchronize: TRedisSynchronize;
    procedure SetOnClose(const value: TProc);
    procedure SetOnError(const value: TProcString);
    procedure SetOnOpen(const value: TProc);
    procedure SetOnSynchronize(const sync: TRedisSynchronize);
  public
    constructor Create(sync: Boolean); virtual;
    destructor Destroy; override;
    class constructor Create;
    class destructor Destroy;
  end;

  TRedisClientSync = class(TRedisClient, IRedisClientSync)
  protected
    procedure doError(const error: string); virtual;
  protected
    procedure Auth(const password: string);
    procedure BGRewriteAOF;
    procedure BGSave;
    // keys
    function Del(const keys: array of const): Integer;
    function Exists(const key: string): Boolean;
    function Expire(const key: string; seconds: Cardinal): Boolean;
    function ExpireAt(const key: string; date: TDateTime): Boolean;
    function Keys(const pattern: string; const onkey: TProcString): Int64;
    function Move(const key: string; db: Word): Boolean;
    function ObjectRefcount(const key: string): Integer;
    function ObjectEncoding(const key: string): string;
    function ObjectIdletime(const key: string): Int64;
    function Persist(const key: string): Boolean;
    function PExpire(const key: string; milliseconds: UInt64): Boolean;
    function PExpireAt(const key: string; date: TDateTime): Boolean;
    function PTTL(const key: string): UInt64;
    function RandomKey: string;
    procedure Rename(const key, newkey: string);
    function RenameNX(const key, newkey: string): Boolean;
    function Sort(const pattern: string; const onkey: TProcString = nil): Int64;
    // string
    function Append(const key, value: string): Int64;
    function Decr(const key: string): Int64;
    function DecrBy(const key: string; decrement: Int64): Int64;
    function Get(const key: string): string;
    function GetBit(const key: string; offset: Cardinal): Boolean;
    function GetRange(const key: string; start, stop: Integer): string;
    function GetSet(const key, value: string): string;
    function Incr(const key: string): Int64;
    function IncrBy(const key: string; increment: Int64): Int64;
    function IncrByFloat(const key: string; increment: Double): Double;
    procedure MGet(const keys: array of const; const onvalue: TProcString);
    procedure MSet(const items: array of const);
    function MSetNX(const items: array of const): Boolean;
    procedure PSetEx(const key: string; milliseconds: UInt64; const value: string);
    procedure Put(const key, value: string);
    function SetBit(const key: string; offset: Cardinal; value: Boolean): Boolean;
    procedure SetEx(const key: string; seconds: Cardinal; const value: string);
    function SetNX(const key, value: string): Boolean;
    function SetRange(const key: string; offset: Cardinal; const value: string): Cardinal;
    function StrLen(const key: string): Cardinal;
    // hash
    function HDel(const key: string; const fields: array of const): Cardinal;
    function HExists(const key, field: string): Boolean;
    function HGet(const key, field: string): string;
    procedure HGetAll(const key: string; const onKeyValue: TProcKeyValue);
    function HIncrBy(const key, field: string; increment: Int64): Int64;
    function HIncrByFloat(const key, field: string; increment: Double): Double;
    procedure HKeys(const key: string; const onkey: TProcString);
    function HLen(const key: string): Cardinal;
    procedure HMGet(const key: string; const fields: array of const; const onValue: TProcString);
    procedure HMSet(const key: string; const items: array of const);
    function HSet(const key, field, value: string): Boolean;
    function HSetNX(const key, field, value: string): Boolean;
    procedure HVals(const key: string; const onValue: TProcString);
  public
    constructor Create; reintroduce;
  end;

  TRedisClientAsync = class(TRedisClient, IRedisClientAsync)
  protected
    procedure doError(const error: string); virtual;
  protected
    procedure Auth(const password: string; const Result: TProc);
    procedure BGRewriteAOF(const Result: TProc);
    procedure BGSave(const Result: TProc);
    // keys
    procedure Del(const keys: array of const; const Result: TProcInteger);
    procedure Exists(const key: string; const Result: TProcBoolean);
    procedure Expire(const key: string; seconds: Cardinal; const Result: TProcBoolean);
    procedure ExpireAt(const key: string; date: TDateTime; const Result: TProcBoolean);
    procedure Keys(const pattern: string; const onkey: TProcString = nil; const Result: TProc = nil);
    procedure Move(const key: string; db: Word; const Result: TProcBoolean);
    procedure ObjectRefcount(const key: string; const Result: TProcInteger);
    procedure ObjectEncoding(const key: string; const Result: TProcString);
    procedure ObjectIdletime(const key: string; const Result: TProcInt64);
    procedure Persist(const key: string; const Result: TProcBoolean);
    procedure PExpire(const key: string; milliseconds: UInt64; const Result: TProcBoolean);
    procedure PExpireAt(const key: string; date: TDateTime; const Result: TProcBoolean);
    procedure PTTL(const key: string; const Result: TProcUInt64);
    procedure RandomKey(const Result: TProcString);
    procedure Rename(const key, newkey: string; const Result: TProc = nil);
    procedure RenameNX(const key, newkey: string; const Result: TProcBoolean = nil);
    procedure Sort(const pattern: string; const onkey: TProcString; const Result: TProc = nil);
    // string
    procedure Append(const key, value: string; const Result: TProcInt64);
    procedure Decr(const key: string; const Result: TProcInt64 = nil);
    procedure DecrBy(const key: string; decrement: Int64; const Result: TProcInt64 = nil);
    procedure Get(const key: string; const Result: TProcString);
    procedure GetBit(const key: string; offset: Cardinal; const Result: TProcBoolean);
    procedure GetRange(const key: string; start, stop: Integer; const Result: TProcString);
    procedure GetSet(const key, value: string; const Result: TProcString);
    procedure Incr(const key: string; const Result: TProcInt64 = nil);
    procedure IncrBy(const key: string; increment: Int64; const Result: TProcInt64 = nil);
    procedure IncrByFloat(const key: string; increment: Double; const Result: TProcDouble = nil);
    procedure MGet(const keys: array of const; const onvalue: TProcString; const Result: TProc = nil);
    procedure MSet(const items: array of const; const Result: TProc = nil);
    procedure MSetNX(const items: array of const; const Result: TProcBoolean = nil);
    procedure PSetEx(const key: string; milliseconds: UInt64; const value: string; const Result: TProc = nil);
    procedure Put(const key, value: string; const Result: TProc = nil);
    procedure SetBit(const key: string; offset: Cardinal; value: Boolean; const Result: TProcBoolean = nil);
    procedure SetEx(const key: string; seconds: Cardinal; const value: string; const Result: TProc = nil);
    procedure SetNX(const key, value: string; const Result: TProcBoolean = nil);
    procedure SetRange(const key: string; offset: Cardinal; const value: string; const Result: TProcCardinal);
    procedure StrLen(const key: string; const Result: TProcCardinal);
    //Hash
    procedure HDel(const key: string; const fields: array of const; const Result: TProcCardinal = nil);
    procedure HExists(const key, field: string; const Result: TProcBoolean);
    procedure HGet(const key, field: string; const Result: TProcString);
    procedure HGetAll(const key: string; const onKeyValue: TProcKeyValue; const Result: TProc = nil);
    procedure HIncrBy(const key, field: string; increment: Int64; const Result: TProcInt64 = nil);
    procedure HIncrByFloat(const key, field: string; increment: Double; const Result: TProcDouble = nil);
    procedure HKeys(const key: string; const onkey: TProcString; const Result: TProc = nil);
    procedure HLen(const key: string; const Result: TProcCardinal);
    procedure HMGet(const key: string; const fields: array of const; const onValue: TProcString; const Result: TProc = nil);
    procedure HMSet(const key: string; const items: array of const; const Result: TProc = nil);
    procedure HSet(const key, field, value: string; const Result: TProcBoolean = nil);
    procedure HSetNX(const key, field, value: string; const Result: TProcBoolean = nil);
    procedure HVals(const key: string; const onValue: TProcString; const Result: TProc = nil);
  public
    constructor Create; reintroduce;
  end;

implementation
uses Classes, DateUtils, SysConst;

type
  TVarRecArray = array[0..0] of TVarRec;
  PVarRecArray = ^TVarRecArray;

procedure ParseCommand(const cmd: string; list: TStringList);
var
  s, e: PChar;
  st: Integer;
  procedure Push;
  var
    item: string;
  begin
    SetString(item, s, e-s);
    list.Add(item);
  end;
begin
  s := PChar(cmd);
  e := s;
  st := 0;
  while True do
    case st of
      0:
        case s^ of
          ' ': Inc(s);
          '"':
            begin
              Inc(s);
              e := s;
              st := 2;
            end;
          #0: Exit;
        else
          st := 1;
          e := s;
          inc(e);
        end;
      1:
        case e^ of
          ' ':
            begin
              Push;
              st := 0;
              s := e;
              inc(s);
            end;
          #0:
            begin
              Push;
              Exit;
            end;
          '"':
            begin
              Push;
              Inc(e);
              s := e;
              st := 2;
            end;
        else
          Inc(e);
        end;
      2:
        case e^ of
          #0: Exit;
          '"':
            begin
              Push;
              s := e;
              inc(s);
              st := 0;
            end;
        else
          Inc(e);
        end;
    end;
end;

function VarItem(const item: PVarRec; var fs: TFormatSettings): string;
begin
  case item.VType of
    vtUnicodeString: Result := string(item.VUnicodeString);
    vtInteger : Result := IntToStr(item.VInteger);
    vtInt64   : Result := IntToStr(item.VInt64^);
    vtBoolean : Result := BoolToStr(item.VBoolean);
    vtChar    : Result := string(item.VChar);
    vtWideChar: Result := string(item.VWideChar);
    vtExtended: Result := FloatToStr(item.VExtended^, fs);
    vtCurrency: Result := CurrToStr(item.VCurrency^, fs);
    vtString  : Result := string(item.VString^);
    vtPChar   : Result := string(AnsiString(item.VPChar));
    vtAnsiString: Result := string(AnsiString(item.VAnsiString));
    vtWideString: Result := string(PWideChar(item.VWideString));
    vtVariant:
      with TVarData(item.VVariant^) do
      case VType of
        varSmallInt: Result := IntToStr(VSmallInt);
        varInteger:  Result := IntToStr(VInteger);
        varSingle:   Result := FloatToStr(VSingle, fs);
        varDouble:   Result := FloatToStr(VDouble, fs);
        varCurrency: Result := CurrToStr(VCurrency, fs);
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
end;

function DateTimeToMilisec(const AValue: TDateTime): Int64;
begin
  Result := MilliSecondsBetween(UnixDateDelta, AValue);
  if AValue < UnixDateDelta then
    Result := -Result;
end;

function StrToUInt64(const S: string): UInt64;
var
  E: Integer;
begin
  Val(S, Result, E);
  if E <> 0 then
    raise EConvertError.CreateResFmt(@SInvalidInteger, [S]);
end;

// sync/async return functions

function Ret(const proc: TProcCardinal): TProcString; overload;
begin
  if not Assigned(proc) then
    Result := nil else
    Result := procedure(const data: string) begin
      proc(StrToInt(data))
    end;
end;

function Ret(var v: Cardinal): TProcString; overload;
var
  r: PCardinal;
begin
  r := @v;
  Result := procedure(const data: string) begin
    r^ := StrToInt(data);
  end;
end;

function Ret(const proc: TProcByte): TProcString; overload;
begin
  if not Assigned(proc) then
    Result := nil else
    Result := procedure(const data: string) begin
      proc(StrToInt(data))
    end;
end;

function Ret(var v: Byte): TProcString; overload;
var
  r: PByte;
begin
  r := @v;
  Result := procedure(const data: string) begin
    r^ := StrToInt(data);
  end;
end;

function Ret(const proc: TProcInteger): TProcString; overload;
begin
  if not Assigned(proc) then
    Result := nil else
    Result := procedure(const data: string) begin
      proc(StrToInt(data))
    end;
end;

function Ret(var v: Integer): TProcString; overload;
var
  r: PInteger;
begin
  r := @v;
  Result := procedure(const data: string) begin
    r^ := StrToInt(data);
  end;
end;

function Ret(const proc: TProcInt64): TProcString; overload;
begin
  if not Assigned(proc) then
    Result := nil else
    Result := procedure(const data: string) begin
      proc(StrToInt64(data))
    end;
end;

function Ret(var v: Int64): TProcString; overload;
var
  r: PInt64;
begin
  r := @v;
  Result := procedure(const data: string) begin
    r^ := StrToInt64(data);
  end;
end;

function Ret(const proc: TProcUInt64): TProcString; overload;
begin
  if not Assigned(proc) then
    Result := nil else
    Result := procedure(const data: string) begin
      proc(StrToUInt64(data))
    end;
end;

function Ret(var v: UInt64): TProcString; overload;
var
  r: PUInt64;
begin
  r := @v;
  Result := procedure(const data: string) begin
    r^ := StrToUInt64(data);
  end;
end;

function Ret(const proc: TProcBoolean): TProcString; overload;
begin
  if not Assigned(proc) then
    Result := nil else
    Result := procedure(const data: string) begin
      proc(data <> '0')
    end;
end;

function Ret(var v: Boolean): TProcString; overload;
var
  r: PBoolean;
begin
  r := @v;
  Result := procedure(const data: string) begin
    r^ := data <> '0';
  end;
end;

function Ret(const proc: TProc): TProcString; overload;
begin
  if not Assigned(proc) then
    Result := nil else
    Result := procedure(const data: string) begin
      proc()
    end;
end;


type
  PFormatSettings = ^TFormatSettings;

function Ret(const proc: TProcDouble; var fs: TFormatSettings): TProcString; overload;
var
  f: PFormatSettings;
begin
  if not Assigned(proc) then
    Result := nil else
    begin
      f := @fs;
      Result := procedure(const data: string) begin
        proc(StrToFloat(data, f^))
      end;
    end;
end;

function Ret(var v: Double; var fs: TFormatSettings): TProcString; overload;
var
  r: PDouble;
  f: PFormatSettings;
begin
  r := @v;
  f := @fs;
  Result := procedure(const data: string) begin
    r^ := StrToFloat(data, f^);
  end;
end;

function Ret(var v: string): TProcString; overload;
var
  p: PString;
begin
  p := @v;
  Result := procedure(const data: string) begin
    p^ := data;
  end;
end;

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

procedure TRedisClient.Call(count: Cardinal; const getData: TRedisGetParam;
  const onresponse, onerror, onreturn: TProcString);

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

  entry.command  := ParseCommand(getData(0));
  entry.onresponse := onresponse;
  entry.onerror    := onerror;
  entry.onreturn      := onreturn;

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

procedure TRedisClient.Call(const data: string; const onresponse, onerror,
  onreturn: TProcString);
var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    ParseCommand(data, list);
    Call(list.Count,
      function(index: Integer): string
      begin
        Result := list[index];
      end,
      onresponse, onerror, onreturn);
  finally
    list.Free;
  end;
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

procedure TRedisClient.Return(const callback: TProcString; const value: string);
begin
  if Assigned(callback) then
  begin
    if FSync then
      callback(value) else
      if Assigned(FOnSynchronize) then
        FOnSynchronize(callback, value) else
        TThreadIt.Synchronize(nil, procedure begin
          callback(value);
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
  FMultiresponses := TQueue<TResponseEntry>.Create;
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
      st: Integer;
      n, count: Int64;
      item: RawByteString;
      e: TResponseEntry;

      function Quit: Boolean;
      begin
        case FMulti of
          msNone:
          begin
            Return(e.onreturn);
            Result := FSync;
          end;
          msMulti:
            begin
              case e.command of
                cmdExec, cmdDiscard:
                  Return(e.onreturn);
              end;
              Result := FSync;
            end;
          msReturn:
            begin
              Return(e.onreturn);
              if FMultiresponses.Count = 1 then
              begin
                FMulti := msNone;
                Return(FMultiresponses.Dequeue().onreturn);
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
              begin
                FMultiresponses.Enqueue(e);
                e.onresponse := nil;
              end;
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
                FMultiresponses.Enqueue(e);
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
          e := FMultiresponses.Dequeue();
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
                end;
              '*':
                begin
                  st := 9;
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
                Return(e.onresponse, string(UTF8String(item)));
                if Quit then Exit;
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
                Return(e.onerror, string(item));
                if Quit then Exit;
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
                Return(e.onresponse, IntToStr(n));
                if Quit then Exit;
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
                    if Quit then Exit;
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
                Return(e.onresponse, string(UTF8String(item)));
                dec(count);
                if count = 0 then
                begin
                  st := 1;
                  if Quit then Exit;
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
                      if Quit then Exit;
                    end;
                end;
              else
                if count > 0 then
                  st := 10 else
                begin
                  st := 1;
                  if Quit then Exit;
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
          TThread.Synchronize(nil,
            procedure begin Close end);
    end;
  if FSync then
    method() else
    TThreadIt.Create(method);
end;

procedure TRedisClient.Call(const data: array of Const;
  const onresponse, onerror, onreturn: TProcString);
var
  len: Integer;
  arr: PVarRecArray;
begin
  len := Length(data);
  arr := @data[0];
  Call(len,
    function(index: Integer): string
    begin
      Result := VarItem(@arr[index], FFormatSettings);
    end,
    onresponse, onerror, onreturn);
end;

procedure TRedisClient.SetOnClose(const value: TProc);
begin
  FOnClose := value;
end;

procedure TRedisClient.SetOnError(const value: TProcString);
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

function TRedisClient.GetOnError: TProcString;
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

{ TRedisClientSync }

function TRedisClientSync.Append(const key, value: string): Int64;
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'APPEND';
        1: Result := key;
        2: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.Auth(const password: string);
begin
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'AUTH';
        1: Result := password;
      end;
    end,
    nil, doError);
end;

procedure TRedisClientSync.BGRewriteAOF;
begin
  Call(1,
    function(index: Integer): string begin
      Result := 'BGREWRITEAOF' end,
    nil, doError);
end;


procedure TRedisClientSync.BGSave;
begin
  Call(1,
    function(index: Integer): string begin
      Result := 'BGSAVE' end,
    nil, doError);
end;

constructor TRedisClientSync.Create;
begin
  inherited Create(True);
end;

function TRedisClientSync.Decr(const key: string): Int64;
begin
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'DECR';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.DecrBy(const key: string; decrement: Int64): Int64;
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'DECRBY';
        1: Result := key;
        2: Result := IntToStr(decrement);
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.Del(const keys: array of const): Integer;
var
  arr: PVarRecArray;
begin
  arr := @keys[0];
  Call(Length(keys) + 1,
    function(index: Integer): string begin
      case index of
        0: Result := 'DEL';
      else
        Result := VarItem(@arr[index-1], FFormatSettings);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.doError(const error: string);
begin
  if Assigned(FOnError) then
    FOnError(error) else
    raise Exception.Create(error);
end;

function TRedisClientSync.Exists(const key: string): Boolean;
begin
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'EXISTS';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.Expire(const key: string; seconds: Cardinal): Boolean;
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'EXPIRE';
        1: Result := key;
        2: Result := IntToStr(seconds);
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.ExpireAt(const key: string; date: TDateTime): Boolean;
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'EXPIRE';
        1: Result := key;
        2: Result := IntToStr(DateTimeToUnix(date));
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.Get(const key: string): string;
begin
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'GET';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.GetBit(const key: string; offset: Cardinal): Boolean;
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'GETBIT';
        1: Result := key;
        2: Result := IntToStr(offset);
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.GetRange(const key: string; start,
  stop: Integer): string;
begin
  Call(4,
    function(index: Integer): string begin
      case index of
        0: Result := 'GETRANGE';
        1: Result := key;
        2: Result := IntToStr(start);
        3: Result := IntToStr(stop);
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.GetSet(const key, value: string): string;
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'GETSET';
        1: Result := key;
        2: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.HDel(const key: string;
  const fields: array of const): Cardinal;
var
  arr: PVarRecArray;
begin
  arr := @fields[0];
  Call(2 + Length(fields),
    function(index: Integer): string begin
      case index of
        0: Result := 'HDEL';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2], FFormatSettings);
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.HExists(const key, field: string): Boolean;
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'HEXISTS';
        1: Result := key;
        2: Result := field;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.HGet(const key, field: string): string;
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'HGET';
        1: Result := key;
        2: Result := field;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.HGetAll(const key: string;
  const onKeyValue: TProcKeyValue);
var
  k: Boolean;
  str: string;
begin
  k := True;
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'HGETALL';
        1: Result := key;
      end;
    end,
    procedure(const data: string) begin
      if k then
        str := data else
        onKeyValue(str, data);
      k := not k;
    end, doError);
end;

function TRedisClientSync.HIncrBy(const key, field: string;
  increment: Int64): Int64;
begin
  Call(4,
    function(index: Integer): string begin
      case index of
        0: Result := 'HINCRBY';
        1: Result := key;
        2: Result := field;
        3: Result := IntToStr(increment);
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.HIncrByFloat(const key, field: string;
  increment: Double): Double;
begin
  Call(4,
    function(index: Integer): string begin
      case index of
        0: Result := 'HINCRBYFLOAT';
        1: Result := key;
        2: Result := field;
        3: Result := FloatToStr(increment, FFormatSettings);
      end;
    end,
    Ret(Result, FFormatSettings), doError);
end;

procedure TRedisClientSync.HKeys(const key: string; const onkey: TProcString);
begin
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'HKEYS';
        1: Result := key;
      end;
    end,
    onkey, doError);
end;

function TRedisClientSync.HLen(const key: string): Cardinal;
begin
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'HLEN';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.HMGet(const key: string;
  const fields: array of const; const onValue: TProcString);
var
  arr: PVarRecArray;
begin
  arr := @fields;
  Call(2 + Length(fields),
    function(index: Integer): string begin
      case index of
        0: Result := 'HMGET';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2], FFormatSettings);
      end;
    end,
    onValue, doError);
end;

procedure TRedisClientSync.HMSet(const key: string;
  const items: array of const);
var
  arr: PVarRecArray;
begin
  arr := @items;
  Call(2 + Length(items),
    function(index: Integer): string begin
      case index of
        0: Result := 'HMSET';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2], FFormatSettings);
      end;
    end,
    nil, doError);
end;

function TRedisClientSync.HSet(const key, field, value: string): Boolean;
begin
  Call(4,
    function(index: Integer): string begin
      case index of
        0: Result := 'HSET';
        1: Result := key;
        2: Result := field;
        3: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.HSetNX(const key, field, value: string): Boolean;
begin
  Call(4,
    function(index: Integer): string begin
      case index of
        0: Result := 'HSETNX';
        1: Result := key;
        2: Result := field;
        3: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.HVals(const key: string; const onValue: TProcString);
begin
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'HVALS';
        1: Result := key;
      end;
    end,
    onValue, doError);
end;

function TRedisClientSync.Incr(const key: string): Int64;
begin
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'INCR';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.IncrBy(const key: string; increment: Int64): Int64;
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'INCRBY';
        1: Result := key;
        2: Result := IntToStr(increment);
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.IncrByFloat(const key: string;
  increment: Double): Double;
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'INCRBYFLOAT';
        1: Result := key;
        2: Result := FloatToStr(increment, FFormatSettings);
      end;
    end,
    Ret(Result, FFormatSettings), doError);
end;

function TRedisClientSync.Keys(const pattern: string;
  const onkey: TProcString): Int64;
var
  ret: Int64;
begin
  ret := 0;
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'KEYS';
        1: Result := pattern;
      end;
    end,
    procedure(const data: string) begin
      inc(ret);
      if Assigned(onkey) then
        onkey(data);
    end, doError);
  Result := ret;
end;

procedure TRedisClientSync.MGet(const keys: array of const;
  const onvalue: TProcString);
var
  arr: PVarRecArray;
begin
  arr := @keys[0];
  Call(Length(keys) + 1,
    function(index: Integer): string begin
      case index of
        0: Result := 'MGET';
      else
        Result := VarItem(@arr[index-1], FFormatSettings);
      end;
    end,
    onvalue, doError);
end;

function TRedisClientSync.Move(const key: string; db: Word): Boolean;
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'MOVE';
        1: Result := key;
        2: Result := IntToStr(db);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.MSet(const items: array of const);
var
  arr: PVarRecArray;
begin
  arr := @items[0];
  Call(Length(items) + 1,
    function(index: Integer): string begin
      case index of
        0: Result := 'MSET';
      else
        Result := VarItem(@arr[index-1], FFormatSettings);
      end;
    end,
    nil, doError);
end;

function TRedisClientSync.MSetNX(const items: array of const): Boolean;
var
  arr: PVarRecArray;
begin
  arr := @items[0];
  Call(Length(items) + 1,
    function(index: Integer): string begin
      case index of
        0: Result := 'MSETNX';
      else
        Result := VarItem(@arr[index-1], FFormatSettings);
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.ObjectEncoding(const key: string): string;
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'OBJECT';
        1: Result := 'ENCODING';
        2: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.ObjectIdletime(const key: string): Int64;
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'OBJECT';
        1: Result := 'IDLETIME';
        2: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.ObjectRefcount(const key: string): Integer;
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'OBJECT';
        1: Result := 'REFCOUNT';
        2: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.Persist(const key: string): Boolean;
begin
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'PERSIST';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.PExpire(const key: string;
  milliseconds: UInt64): Boolean;
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'PEXPIRE';
        1: Result := key;
        2: Result := IntToStr(milliseconds);
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.PExpireAt(const key: string;
  date: TDateTime): Boolean;
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'EXPIRE';
        1: Result := key;
        2: Result := IntToStr(DateTimeToMilisec(date));
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.PSetEx(const key: string;
  milliseconds: UInt64; const value: string);
begin
  Call(4,
    function(index: Integer): string begin
      case index of
        0: Result := 'PSETEX';
        1: Result := key;
        2: Result := IntToStr(milliseconds);
        3: Result := value;
      end;
    end,
    nil, doError);
end;

function TRedisClientSync.PTTL(const key: string): UInt64;
begin
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'PTTL';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.Put(const key, value: string);
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'SET';
        1: Result := key;
        2: Result := value;
      end;
    end,
    nil, doError);
end;

function TRedisClientSync.RandomKey: string;
begin
  Call(1,
    function(index: Integer): string begin
      Result := 'RANDOMKEY';
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.Rename(const key, newkey: string);
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'RENAME';
        1: Result := key;
        2: Result := newkey;
      end;
    end,
    nil, doError);
end;

function TRedisClientSync.RenameNX(const key, newkey: string): Boolean;
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'RENAME';
        1: Result := key;
        2: Result := newkey;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.SetBit(const key: string; offset: Cardinal;
  value: Boolean): Boolean;
begin
  Call(4,
    function(index: Integer): string begin
      case index of
        0: Result := 'SETBIT';
        1: Result := key;
        2: Result := IntToStr(offset);
        3: if value then
             Result := '1' else
             Result := '0';
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.SetEx(const key: string; seconds: Cardinal;
  const value: string);
begin
  Call(4,
    function(index: Integer): string begin
      case index of
        0: Result := 'SETEX';
        1: Result := key;
        2: Result := IntToStr(seconds);
        3: Result := value;
      end;
    end);
end;

function TRedisClientSync.SetNX(const key, value: string): Boolean;
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'SETNX';
        1: Result := key;
        2: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.SetRange(const key: string;
  offset: Cardinal; const value: string): Cardinal;
begin
  Call(4,
    function(index: Integer): string begin
      case index of
        0: Result := 'SETRANGE';
        1: Result := key;
        2: Result := IntToStr(offset);
        3: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.Sort(const pattern: string;
  const onkey: TProcString): Int64;
var
  list: TStringList;
  ret: Int64;
begin
  ret := 0;
  list := TStringList.Create;
  try
    ParseCommand(pattern, list);
    Call(list.Count + 1,
      function(index: Integer): string
      begin
        case index of
          0: Result := 'SORT';
        else
          Result := list[index - 1];
        end;
      end,
      procedure(const data: string) begin
        inc(ret);
        if Assigned(onkey) then
          onkey(data);
      end, doError);
  finally
    list.Free;
  end;
  Result := ret;
end;

function TRedisClientSync.StrLen(const key: string): Cardinal;
begin
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'STRLEN';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

{ TRedisClientAsync }

procedure TRedisClientAsync.Append(const key, value: string;
  const Result: TProcInt64);
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'APPEND';
        1: Result := key;
        2: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.Auth(const password: string; const Result: TProc);
begin
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'AUTH';
        1: Result := password;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.BGRewriteAOF(const Result: TProc);
begin
  Call(1,
    function(index: Integer): string begin
      Result := 'BGREWRITEAOF' end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.BGSave(const Result: TProc);
begin
  Call(1,
    function(index: Integer): string begin
      Result := 'BGSAVE' end,
    Ret(Result), doError);
end;

constructor TRedisClientAsync.Create;
begin
  inherited Create(False);
end;

procedure TRedisClientAsync.Decr(const key: string; const Result: TProcInt64);
begin
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'DECR';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.DecrBy(const key: string; decrement: Int64;
  const Result: TProcInt64);
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'DECRBY';
        1: Result := key;
        2: Result := IntToStr(decrement);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.Del(const keys: array of const;
  const Result: TProcInteger);
var
  arr: PVarRecArray;
begin
  Assert(Length(keys) > 0);
  arr := @keys[0];
  Call(Length(keys) + 1,
    function(index: Integer): string begin
      case index of
        0: Result := 'DEL';
      else
        Result := VarItem(@arr[index-1], FFormatSettings);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.doError(const error: string);
begin
  if Assigned(FOnError) then
    FOnError(error) else
    raise Exception.Create(error);
end;

procedure TRedisClientAsync.Exists(const key: string; const Result: TProcBoolean);
begin
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'EXISTS';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.Expire(const key: string; seconds: Cardinal;
  const Result: TProcBoolean);
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'EXPIRE';
        1: Result := key;
        2: Result := IntToStr(seconds);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.ExpireAt(const key: string; date: TDateTime;
  const Result: TProcBoolean);
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'EXPIRE';
        1: Result := key;
        2: Result := IntToStr(DateTimeToUnix(date));
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.Get(const key: string; const Result: TProcString);
begin
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'GET';
        1: Result := key;
      end;
    end,
    Result, doError);
end;

procedure TRedisClientAsync.GetBit(const key: string; offset: Cardinal;
  const Result: TProcBoolean);
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'GETBIT';
        1: Result := key;
        2: Result := IntToStr(offset);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.GetRange(const key: string; start, stop: Integer;
  const Result: TProcString);
begin
  Call(4,
    function(index: Integer): string begin
      case index of
        0: Result := 'GETRANGE';
        1: Result := key;
        2: Result := IntToStr(start);
        3: Result := IntToStr(stop);
      end;
    end,
    Result, doError);
end;

procedure TRedisClientAsync.GetSet(const key, value: string;
  const Result: TProcString);
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'GETSET';
        1: Result := key;
        2: Result := value;
      end;
    end,
    Result, doError);
end;

procedure TRedisClientAsync.HDel(const key: string;
  const fields: array of const; const Result: TProcCardinal);
var
  arr: PVarRecArray;
begin
  arr := @fields[0];
  Call(2 + Length(fields),
    function(index: Integer): string begin
      case index of
        0: Result := 'HDEL';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2], FFormatSettings);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.HExists(const key, field: string;
  const Result: TProcBoolean);
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'HEXISTS';
        1: Result := key;
        2: Result := field;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.HGet(const key, field: string;
  const Result: TProcString);
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'HGET';
        1: Result := key;
        2: Result := field;
      end;
    end,
    result, doError);
end;

procedure TRedisClientAsync.HGetAll(const key: string;
  const onKeyValue: TProcKeyValue; const Result: TProc);
var
  k: Boolean;
  str: string;
begin
  k := True;
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'HGETALL';
        1: Result := key;
      end;
    end,
    procedure(const data: string) begin
      if k then
        str := data else
        onKeyValue(str, data);
      k := not k;
    end, doError, Ret(Result));
end;

procedure TRedisClientAsync.HIncrBy(const key, field: string; increment: Int64;
  const Result: TProcInt64);
begin
  Call(4,
    function(index: Integer): string begin
      case index of
        0: Result := 'HINCRBY';
        1: Result := key;
        2: Result := field;
        3: Result := IntToStr(increment);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.HIncrByFloat(const key, field: string;
  increment: Double; const Result: TProcDouble);
begin
  Call(4,
    function(index: Integer): string begin
      case index of
        0: Result := 'HINCRBYFLOAT';
        1: Result := key;
        2: Result := field;
        3: Result := FloatToStr(increment, FFormatSettings);
      end;
    end,
    Ret(Result, FFormatSettings), doError);
end;

procedure TRedisClientAsync.HKeys(const key: string; const onkey: TProcString;
  const Result: TProc);
begin
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'HKEYS';
        1: Result := key;
      end;
    end,
    onkey, doError, Ret(Result));
end;

procedure TRedisClientAsync.HLen(const key: string;
  const Result: TProcCardinal);
begin
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'HLEN';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.HMGet(const key: string;
  const fields: array of const; const onValue: TProcString;
  const Result: TProc);
var
  arr: PVarRecArray;
begin
  arr := @fields;
  Call(2 + Length(fields),
    function(index: Integer): string begin
      case index of
        0: Result := 'HMGET';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2], FFormatSettings);
      end;
    end,
    onValue, doError, Ret(Result));
end;

procedure TRedisClientAsync.HMSet(const key: string;
  const items: array of const; const Result: TProc);
var
  arr: PVarRecArray;
begin
  arr := @items;
  Call(2 + Length(items),
    function(index: Integer): string begin
      case index of
        0: Result := 'HMSET';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2], FFormatSettings);
      end;
    end,
    nil, doError, Ret(Result));
end;

procedure TRedisClientAsync.HSet(const key, field, value: string;
  const Result: TProcBoolean);
begin
  Call(4,
    function(index: Integer): string begin
      case index of
        0: Result := 'HSET';
        1: Result := key;
        2: Result := field;
        3: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.HSetNX(const key, field, value: string;
  const Result: TProcBoolean);
begin
  Call(4,
    function(index: Integer): string begin
      case index of
        0: Result := 'HSETNX';
        1: Result := key;
        2: Result := field;
        3: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.HVals(const key: string; const onValue: TProcString;
  const Result: TProc);
begin
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'HVALS';
        1: Result := key;
      end;
    end,
    onValue, doError, Ret(Result));
end;

procedure TRedisClientAsync.Incr(const key: string; const Result: TProcInt64);
begin
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'INCR';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.IncrBy(const key: string; increment: Int64;
  const Result: TProcInt64);
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'INCRBY';
        1: Result := key;
        2: Result := IntToStr(increment);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.IncrByFloat(const key: string; increment: Double;
  const Result: TProcDouble);
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'INCRBYFLOAT';
        1: Result := key;
        2: Result := FloatToStr(increment, FFormatSettings);
      end;
    end,
    Ret(Result, FFormatSettings), doError);
end;

procedure TRedisClientAsync.Keys(const pattern: string;
  const onkey: TProcString; const Result: TProc);
begin
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'KEYS';
        1: Result := pattern;
      end;
    end,
    onkey, doError, Ret(Result));
end;

procedure TRedisClientAsync.MGet(const keys: array of const;
  const onvalue: TProcString; const Result: TProc);
var
  arr: PVarRecArray;
begin
  arr := @keys[0];
  Call(Length(keys) + 1,
    function(index: Integer): string begin
      case index of
        0: Result := 'MGET';
      else
        Result := VarItem(@arr[index-1], FFormatSettings);
      end;
    end,
    onvalue, doError, Ret(Result));
end;

procedure TRedisClientAsync.Move(const key: string; db: Word;
  const Result: TProcBoolean);
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'MOVE';
        1: Result := key;
        2: Result := IntToStr(db);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.MSet(const items: array of const;
  const Result: TProc);
var
  arr: PVarRecArray;
begin
  arr := @items[0];
  Call(Length(items) + 1,
    function(index: Integer): string begin
      case index of
        0: Result := 'MSET';
      else
        Result := VarItem(@arr[index-1], FFormatSettings);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.MSetNX(const items: array of const;
  const Result: TProcBoolean);
var
  arr: PVarRecArray;
begin
  arr := @items[0];
  Call(Length(items) + 1,
    function(index: Integer): string begin
      case index of
        0: Result := 'MSETNX';
      else
        Result := VarItem(@arr[index-1], FFormatSettings);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.ObjectEncoding(const key: string;
  const Result: TProcString);
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'OBJECT';
        1: Result := 'ENCODING';
        2: Result := key;
      end;
    end,
    Result, doError);
end;

procedure TRedisClientAsync.ObjectIdletime(const key: string;
  const Result: TProcInt64);
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'OBJECT';
        1: Result := 'IDLETIME';
        2: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.ObjectRefcount(const key: string;
  const Result: TProcInteger);
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'OBJECT';
        1: Result := 'REFCOUNT';
        2: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.Persist(const key: string;
  const Result: TProcBoolean);
begin
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'PERSIST';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.PExpire(const key: string; milliseconds: UInt64;
  const Result: TProcBoolean);
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'PEXPIRE';
        1: Result := key;
        2: Result := IntToStr(milliseconds);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.PExpireAt(const key: string; date: TDateTime;
  const Result: TProcBoolean);
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'PEXPIRE';
        1: Result := key;
        2: Result := IntToStr(DateTimeToMilisec(date));
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.PSetEx(const key: string; milliseconds: UInt64;
  const value: string; const Result: TProc);
begin
  Call(4,
    function(index: Integer): string begin
      case index of
        0: Result := 'PSETEX';
        1: Result := key;
        2: Result := IntToStr(milliseconds);
        3: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.PTTL(const key: string; const Result: TProcUInt64);
begin
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'PTTL';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.Put(const key, value: string; const Result: TProc);
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'SET';
        1: Result := key;
        2: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.RandomKey(const Result: TProcString);
begin
  Call(1,
    function(index: Integer): string begin
      Result := 'RANDOMKEY';
    end,
    Result, doError);
end;

procedure TRedisClientAsync.Rename(const key, newkey: string;
  const Result: TProc);
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'RENAME';
        1: Result := key;
        2: Result := newkey;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.RenameNX(const key, newkey: string;
  const Result: TProcBoolean);
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'RENAME';
        1: Result := key;
        2: Result := newkey;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.SetBit(const key: string; offset: Cardinal;
  value: Boolean; const Result: TProcBoolean);
begin
  Call(4,
    function(index: Integer): string begin
      case index of
        0: Result := 'SETBIT';
        1: Result := key;
        2: Result := IntToStr(offset);
        3: if value then
             Result := '1' else
             Result := '0';
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.SetEx(const key: string; seconds: Cardinal;
  const value: string; const Result: TProc);
begin
  Call(4,
    function(index: Integer): string begin
      case index of
        0: Result := 'SETEX';
        1: Result := key;
        2: Result := IntToStr(seconds);
        3: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.SetNX(const key, value: string;
  const Result: TProcBoolean);
begin
  Call(3,
    function(index: Integer): string begin
      case index of
        0: Result := 'SETNX';
        1: Result := key;
        2: Result := value;
      end;
    end,
    Ret(Result), doError);
end;


procedure TRedisClientAsync.SetRange(const key: string; offset: Cardinal;
  const value: string; const Result: TProcCardinal);
begin
  Call(4,
    function(index: Integer): string begin
      case index of
        0: Result := 'SETRANGE';
        1: Result := key;
        2: Result := IntToStr(offset);
        3: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.Sort(const pattern: string;
  const onkey: TProcString; const Result: TProc);
var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    ParseCommand(pattern, list);
    Call(list.Count + 1,
      function(index: Integer): string
      begin
        case index of
          0: Result := 'SORT';
        else
          Result := list[index - 1];
        end;
      end,
      onkey, doError, Ret(Result));
  finally
    list.Free;
  end;
end;

procedure TRedisClientAsync.StrLen(const key: string;
  const Result: TProcCardinal);
begin
  Call(2,
    function(index: Integer): string begin
      case index of
        0: Result := 'STRLEN';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

end.

