unit dorRedis;

interface
uses SysUtils, WinSock, SyncObjs, Generics.Collections;

type
  PNString = ^NString;
  NString = record
  private
    FValue: string;
    FIsNull: Boolean;
  public
    class operator Implicit(const Value: string): NString;
    class operator Implicit(const Value: NString): string;
    property IsNull: Boolean read FIsNull;
  end;

  TRedisState = (rsConnecting, rsOpen, rsClosing, rsClosed);
  TProcByte = reference to procedure(value: Byte);
  TProcInteger = reference to procedure(value: Integer);
  TProcCardinal = reference to procedure(value: Cardinal);
  TProcInt64 = reference to procedure(value: Int64);
  TProcUInt64 = reference to procedure(value: UInt64);
  TProcBoolean = reference to procedure(value: Boolean);
  TProcString = reference to procedure(const value: NString);
  TProcDouble = reference to procedure(value: Double);
  TProcExtended = reference to procedure(value: Extended);
  TProcKeyValue = reference to procedure(const key, value: string);

  TRedisSynchronize = reference to procedure(const res: TProcString;
    const value: string);
  TRedisGetParam = reference to function(index: Cardinal): string;

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
    // keys
    function Del(const keys: array of const): Integer; overload;
    function Del(const key: string): Boolean; overload;
    function Exists(const key: string): Boolean;
    function Expire(const key: string; timeout: Cardinal): Boolean;
    function ExpireAt(const key: string; date: TDateTime): Boolean;
    procedure Keys(const pattern: string; const onkey: TProcString);
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
    procedure Sort(const query: string; const onkey: TProcString);
    // string
    function Append(const key, value: string): Int64;
    function Decr(const key: string): Int64;
    function DecrBy(const key: string; decrement: Int64): Int64;
    function Get(const key: string): NString;
    function GetBit(const key: string; offset: Cardinal): Boolean;
    function GetRange(const key: string; start, stop: Integer): string;
    function GetSet(const key, value: string): NString;
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
    function HGet(const key, field: string): NString;
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
    // list
    procedure BLPop(const keys: array of const; timeout: Cardinal; const onValue: TProcString);
    procedure BRPop(const keys: array of const; timeout: Cardinal; const onValue: TProcString);
    function BRPopLPush(const source, destination: string; timeout: Cardinal): NString;
    function LIndex(const key: string; index: Cardinal): NString;
    function LInsertBefore(const key, pivot, value: string): Cardinal;
    function LInsertAfter(const key, pivot, value: string): Cardinal;
    function LLen(const key: string): Cardinal;
    function LPop(const key: string): NString;
    function LPush(const key: string; const values: array of const): Cardinal;
    function LPushX(const key, value: string): Cardinal;
    procedure LRange(const key: string; start, stop: Int64; const onValue: TProcString);
    function LRem(const key: string; count: Int64; const value: string): Cardinal;
    procedure LSet(const key: string; index: Int64; const value: string);
    procedure LTrim(const key: string; start, stop: Int64);
    function RPop(const key: string): NString;
    function RPopLPush(const source, destination: string): NString;
    function RPush(const key: string; const values: array of const): Cardinal;
    function RPushX(const key, value: string): Cardinal;
    // sets
    function SAdd(const key: string; const members: array of const): Cardinal;
    function SCard(const key: string): Cardinal;
    procedure SDiff(const keys: array of const; const onValue: TProcString);
    function SDiffStore(const destination: string; const keys: array of const): Cardinal;
    procedure SInter(const keys: array of const; const onValue: TProcString);
    function SInterStore(const destination: string; const keys: array of const): Cardinal;
    function SIsMember(const key, member: string): Boolean;
    procedure SMembers(const key: string; const onValue: TProcString);
    function SMove(const source, destination, member: string): Boolean;
    function SPop(const key: string): NString;
    function SRandMember(const key: string): NString;
    function SRem(const key: string; const members: array of const): Cardinal;
    procedure SUnion(const keys: array of const; const onValue: TProcString);
    function SUnionStore(const destination: string; const keys: array of const): Cardinal;
    // sorted sets
    function ZAdd(const key: string; const items: array of const): Cardinal;
    function ZCard(const key: string): Cardinal;
    function ZCount(const key, min, max: string): Cardinal;
    function ZIncrBy(const key: string; increment: Int64; const member: string): Extended; overload;
    function ZIncrBy(const key: string; increment: Extended; const member: string): Extended; overload;
    function ZInterStore(const destination: string; const keys: array of const; const options: string = ''): Cardinal;
    procedure ZRange(const key: string; start, stop: Int64; scores: Boolean; const onValue: TProcString);
    procedure ZRangeByScore(const key, min, max: string; const onValue: TProcString; const options: string = ''); overload;
    function ZRank(const key, member: string): Cardinal;
    function ZRem(const key: string; const members: array of const): Cardinal;
    function ZRemRangeByRank(const key: string; start, stop: Int64): Cardinal;
    function ZRemRangeByScore(const key, min, max: string): Cardinal;
    procedure ZRevRange(const key: string; start, stop: Int64; scores: Boolean; const onValue: TProcString);
    procedure ZRevRangeByScore(const key, min, max: string; const onValue: TProcString; const options: string = ''); overload;
    function ZRevRank(const key, member: string): Cardinal;
    function ZScore(const key, member: string): NString;
    function ZUnionStore(const destination: string; const keys: array of const; const options: string = ''): Cardinal;
    // Transaction
    procedure Discard;
    procedure Exec;
    procedure Multi;
    procedure Unwatch;
    procedure Watch(const keys: array of const);
    // connection
    procedure Auth(const password: string);
    function Echo(const msg: string): string;
    procedure Ping;
    procedure Quit;
    procedure Select(const index: Byte);
    // server
    procedure BGRewriteAOF;
    procedure BGSave;
    function ConfigGet(const param: string): string;
    procedure ConfigSet(const param, value: string);
    procedure ConfigResetStat;
    function DBSize: Cardinal;
    procedure FlushAll;
    procedure FlushDB;
    function LastSave: Cardinal;
    procedure Save;
    procedure Shutdown(const options: string = '');
    procedure SlaveOf(const host: string; port: Word);
  end;

  IRedisClientAsync = interface(IRedisClient)
  ['{C234E5CE-199E-4D68-8877-6FC82B8DB1D6}']
    //keys
    procedure Del(const keys: array of const; const Result: TProcInteger = nil); overload;
    procedure Del(const key: string; const Result: TProcBoolean = nil); overload;
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
    procedure Sort(const query: string; const onkey: TProcString; const Result: TProc = nil);
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
    // list
    procedure BLPop(const keys: array of const; timeout: Cardinal; const onValue: TProcString; const Result: TProc = nil);
    procedure BRPop(const keys: array of const; timeout: Cardinal; const onValue: TProcString; const Result: TProc = nil);
    procedure BRPopLPush(const source, destination: string; timeout: Cardinal; const Result: TProcString = nil);
    procedure LIndex(const key: string; index: Cardinal; const Result: TProcString);
    procedure LInsertBefore(const key, pivot, value: string; const Result: TProcCardinal = nil);
    procedure LInsertAfter(const key, pivot, value: string; const Result: TProcCardinal = nil);
    procedure LLen(const key: string; const Result: TProcCardinal);
    procedure LPop(const key: string; const Result: TProcString);
    procedure LPush(const key: string; const values: array of const; const Result: TProcCardinal = nil);
    procedure LPushX(const key, value: string; const Result: TProcCardinal = nil);
    procedure LRange(const key: string; start, stop: Int64; const onValue: TProcString; const Result: TProc = nil);
    procedure LRem(const key: string; count: Int64; const value: string; const Result: TProcCardinal);
    procedure LSet(const key: string; index: Int64; const value: string; const Result: TProc = nil);
    procedure LTrim(const key: string; start, stop: Int64; const Result: TProc = nil);
    procedure RPop(const key: string; const Result: TProcString);
    procedure RPopLPush(const source, destination: string; const Result: TProcString = nil);
    procedure RPush(const key: string; const values: array of const; const Result: TProcCardinal = nil);
    procedure RPushX(const key, value: string; const Result: TProcCardinal = nil);
    // sets
    procedure SAdd(const key: string; const members: array of const; const Result: TProcCardinal);
    procedure SCard(const key: string; const Result: TProcCardinal);
    procedure SDiff(const keys: array of const; const onValue: TProcString; const Result: TProc = nil);
    procedure SDiffStore(const destination: string; const keys: array of const; const Result: TProcCardinal = nil);
    procedure SInter(const keys: array of const; const onValue: TProcString; const Result: TProc = nil);
    procedure SInterStore(const destination: string; const keys: array of const; const Result: TProcCardinal = nil);
    procedure SIsMember(const key, member: string; const Result: TProcBoolean = nil);
    procedure SMembers(const key: string; const onValue: TProcString; const Result: TProc = nil);
    procedure SMove(const source, destination, member: string; const Result: TProcBoolean = nil);
    procedure SPop(const key: string; const Result: TProcString);
    procedure SRandMember(const key: string; const Result: TProcString);
    procedure SRem(const key: string; const members: array of const; const Result: TProcCardinal = nil);
    procedure SUnion(const keys: array of const; const onValue: TProcString; const Result: TProc = nil);
    procedure SUnionStore(const destination: string; const keys: array of const; const Result: TProcCardinal = nil);
    // ordered sets
    procedure ZAdd(const key: string; const items: array of const; const Result: TProcCardinal = nil);
    procedure ZCard(const key: string; const Result: TProcCardinal);
    procedure ZCount(const key, min, max: string; const Result: TProcCardinal);
    procedure ZIncrBy(const key: string; increment: Int64; const member: string; const Result: TProcExtended = nil); overload;
    procedure ZIncrBy(const key: string; increment: Extended; const member: string; const Result: TProcExtended = nil); overload;
    procedure ZInterStore(const destination: string; const keys: array of const; const options: string = ''; const Result: TProcCardinal = nil);
    procedure ZRange(const key: string; start, stop: Int64; scores: Boolean; const onValue: TProcString; const Result: TProc = nil);
    procedure ZRangeByScore(const key, min, max: string; const onValue: TProcString;
      const Result: TProc = nil; const options: string = '');
    procedure ZRank(const key, member: string; const Result: TProcCardinal);
    procedure ZRem(const key: string; const members: array of const; const Result: TProcCardinal = nil);
    procedure ZRemRangeByRank(const key: string; start, stop: Int64; const Result: TProcCardinal = nil);
    procedure ZRemRangeByScore(const key, min, max: string; const Result: TProcCardinal = nil);
    procedure ZRevRange(const key: string; start, stop: Int64; scores: Boolean; const onValue: TProcString; const Result: TProc = nil);
    procedure ZRevRangeByScore(const key, min, max: string; const onValue: TProcString;
      const Result: TProc = nil; const options: string = '');
    procedure ZRevRank(const key, member: string; const Result: TProcCardinal);
    procedure ZScore(const key, member: string; const Result: TProcString);
    procedure ZUnionStore(const destination: string; const keys: array of const; const options: string = ''; const Result: TProcCardinal = nil);
    // transactions
    procedure Discard(const Result: TProc = nil);
    procedure Exec(const Result: TProc = nil);
    procedure Multi(const Result: TProc = nil);
    procedure Unwatch(const Result: TProc = nil);
    procedure Watch(const keys: array of const; const Result: TProc = nil);
    // connection
    procedure Auth(const password: string; const Result: TProc = nil);
    procedure Echo(const msg: string; const Result: TProcString = nil);
    procedure Ping(const Result: TProc = nil);
    procedure Quit(const Result: TProc = nil);
    procedure Select(const index: Byte; const Result: TProc = nil);
    // server
    procedure BGRewriteAOF(const Result: TProc = nil);
    procedure BGSave(const Result: TProc = nil);
    procedure ConfigGet(const param: string; const Result: TProcString = nil);
    procedure ConfigSet(const param, value: string; const Result: TProc = nil);
    procedure ConfigResetStat(const Result: TProc = nil);
    procedure DBSize(const Result: TProcCardinal);
    procedure FlushAll(const Result: TProc = nil);
    procedure FlushDB(const Result: TProc = nil);
    procedure LastSave(const Result: TProcCardinal);
    procedure Save(const Result: TProc = nil);
    procedure Shutdown(const options: string = ''; const Result: TProc = nil);
    procedure SlaveOf(const host: string; port: Word; const Result: TProc = nil);
    procedure Monitor(const onValue: TProcString);
  end;

  TRedisClient = class(TInterfacedObject, IRedisClient)
  type
    TCommand = (cmdMulti, cmdExec, cmdDiscard, cmdMonitor, cmdOther);
    TMultiState = (msNone, msMulti, msExec, msMonitoring, msReturn);
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
    procedure Return(const callback: TProcString; const value: NString);
    function VarItem(const item: PVarRec): string;
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
  private
    procedure SimpleCommand(const cmd: string);
  protected
    procedure doError(const error: NString); virtual;
  public
    // keys
    function Del(const keys: array of const): Integer; overload;
    function Del(const key: string): Boolean; overload;
    function Exists(const key: string): Boolean;
    function Expire(const key: string; seconds: Cardinal): Boolean;
    function ExpireAt(const key: string; date: TDateTime): Boolean;
    procedure Keys(const pattern: string; const onkey: TProcString);
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
    procedure Sort(const query: string; const onkey: TProcString);
    // string
    function Append(const key, value: string): Int64;
    function Decr(const key: string): Int64;
    function DecrBy(const key: string; decrement: Int64): Int64;
    function Get(const key: string): NString;
    function GetBit(const key: string; offset: Cardinal): Boolean;
    function GetRange(const key: string; start, stop: Integer): string;
    function GetSet(const key, value: string): NString;
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
    function HGet(const key, field: string): NString;
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
    // list
    procedure BLPop(const keys: array of const; timeout: Cardinal; const onValue: TProcString);
    procedure BRPop(const keys: array of const; timeout: Cardinal; const onValue: TProcString);
    function BRPopLPush(const source, destination: string; timeout: Cardinal): NString;
    function LIndex(const key: string; index: Cardinal): NString;
    function LInsertBefore(const key, pivot, value: string): Cardinal;
    function LInsertAfter(const key, pivot, value: string): Cardinal;
    function LLen(const key: string): Cardinal;
    function LPop(const key: string): NString;
    function LPush(const key: string; const values: array of const): Cardinal;
    function LPushX(const key, value: string): Cardinal;
    procedure LRange(const key: string; start, stop: Int64; const onValue: TProcString);
    function LRem(const key: string; count: Int64; const value: string): Cardinal;
    procedure LSet(const key: string; index: Int64; const value: string);
    procedure LTrim(const key: string; start, stop: Int64);
    function RPop(const key: string): NString;
    function RPopLPush(const source, destination: string): NString;
    function RPush(const key: string; const values: array of const): Cardinal;
    function RPushX(const key, value: string): Cardinal;
    // sets
    function SAdd(const key: string; const members: array of const): Cardinal;
    function SCard(const key: string): Cardinal;
    procedure SDiff(const keys: array of const; const onValue: TProcString);
    function SDiffStore(const destination: string; const keys: array of const): Cardinal;
    procedure SInter(const keys: array of const; const onValue: TProcString);
    function SInterStore(const destination: string; const keys: array of const): Cardinal;
    function SIsMember(const key, member: string): Boolean;
    procedure SMembers(const key: string; const onValue: TProcString);
    function SMove(const source, destination, member: string): Boolean;
    function SPop(const key: string): NString;
    function SRandMember(const key: string): NString;
    function SRem(const key: string; const members: array of const): Cardinal;
    procedure SUnion(const keys: array of const; const onValue: TProcString);
    function SUnionStore(const destination: string; const keys: array of const): Cardinal;
    // orderd sets
    function ZAdd(const key: string; const items: array of const): Cardinal;
    function ZCard(const key: string): Cardinal;
    function ZCount(const key, min, max: string): Cardinal;
    function ZIncrBy(const key: string; increment: Int64; const member: string): Extended; overload;
    function ZIncrBy(const key: string; increment: Extended; const member: string): Extended; overload;
    function ZInterStore(const destination: string; const keys: array of const; const options: string): Cardinal;
    procedure ZRange(const key: string; start, stop: Int64; scores: Boolean; const onValue: TProcString); overload;
    procedure ZRangeByScore(const key, min, max: string; const onValue: TProcString; const options: string);
    function ZRank(const key, member: string): Cardinal;
    function ZRem(const key: string; const members: array of const): Cardinal;
    function ZRemRangeByRank(const key: string; start, stop: Int64): Cardinal;
    function ZRemRangeByScore(const key, min, max: string): Cardinal;
    procedure ZRevRange(const key: string; start, stop: Int64; scores: Boolean; const onValue: TProcString); overload;
    procedure ZRevRangeByScore(const key, min, max: string; const onValue: TProcString; const options: string);
    function ZRevRank(const key, member: string): Cardinal;
    function ZScore(const key, member: string): NString;
    function ZUnionStore(const destination: string; const keys: array of const; const options: string): Cardinal;
    // transaction
    procedure Discard;
    procedure Exec;
    procedure Multi;
    procedure Unwatch;
    procedure Watch(const keys: array of const);
    // connection
    procedure Auth(const password: string);
    function Echo(const msg: string): string;
    procedure Ping;
    procedure Quit;
    procedure Select(const index: Byte);
    // server
    procedure BGRewriteAOF;
    procedure BGSave;
    function ConfigGet(const param: string): string;
    procedure ConfigSet(const param, value: string);
    procedure ConfigResetstat;
    function DBSize: Cardinal;
    procedure FlushAll;
    procedure FlushDB;
    function LastSave: Cardinal;
    procedure Save;
    procedure Shutdown(const options: string = '');
    procedure SlaveOf(const host: string; port: Word);
  public
    constructor Create; reintroduce;
  end;

  TRedisClientAsync = class(TRedisClient, IRedisClientAsync)
  private
    procedure SimpleCommand(const cmd: string; Result: TProc);
  protected
    procedure doError(const error: NString); virtual;
  public
    // keys
    procedure Del(const keys: array of const; const Result: TProcInteger); overload;
    procedure Del(const key: string; const Result: TProcBoolean); overload;
    procedure Exists(const key: string; const Result: TProcBoolean);
    procedure Expire(const key: string; seconds: Cardinal; const Result: TProcBoolean);
    procedure ExpireAt(const key: string; date: TDateTime; const Result: TProcBoolean);
    procedure Keys(const pattern: string; const onkey: TProcString; const Result: TProc);
    procedure Move(const key: string; db: Word; const Result: TProcBoolean);
    procedure ObjectRefcount(const key: string; const Result: TProcInteger);
    procedure ObjectEncoding(const key: string; const Result: TProcString);
    procedure ObjectIdletime(const key: string; const Result: TProcInt64);
    procedure Persist(const key: string; const Result: TProcBoolean);
    procedure PExpire(const key: string; milliseconds: UInt64; const Result: TProcBoolean);
    procedure PExpireAt(const key: string; date: TDateTime; const Result: TProcBoolean);
    procedure PTTL(const key: string; const Result: TProcUInt64);
    procedure RandomKey(const Result: TProcString);
    procedure Rename(const key, newkey: string; const Result: TProc);
    procedure RenameNX(const key, newkey: string; const Result: TProcBoolean);
    procedure Sort(const query: string; const onkey: TProcString; const Result: TProc);
    // string
    procedure Append(const key, value: string; const Result: TProcInt64);
    procedure Decr(const key: string; const Result: TProcInt64);
    procedure DecrBy(const key: string; decrement: Int64; const Result: TProcInt64);
    procedure Get(const key: string; const Result: TProcString);
    procedure GetBit(const key: string; offset: Cardinal; const Result: TProcBoolean);
    procedure GetRange(const key: string; start, stop: Integer; const Result: TProcString);
    procedure GetSet(const key, value: string; const Result: TProcString);
    procedure Incr(const key: string; const Result: TProcInt64);
    procedure IncrBy(const key: string; increment: Int64; const Result: TProcInt64);
    procedure IncrByFloat(const key: string; increment: Double; const Result: TProcDouble);
    procedure MGet(const keys: array of const; const onvalue: TProcString; const Result: TProc);
    procedure MSet(const items: array of const; const Result: TProc);
    procedure MSetNX(const items: array of const; const Result: TProcBoolean);
    procedure PSetEx(const key: string; milliseconds: UInt64; const value: string; const Result: TProc);
    procedure Put(const key, value: string; const Result: TProc);
    procedure SetBit(const key: string; offset: Cardinal; value: Boolean; const Result: TProcBoolean);
    procedure SetEx(const key: string; seconds: Cardinal; const value: string; const Result: TProc);
    procedure SetNX(const key, value: string; const Result: TProcBoolean);
    procedure SetRange(const key: string; offset: Cardinal; const value: string; const Result: TProcCardinal);
    procedure StrLen(const key: string; const Result: TProcCardinal);
    //Hash
    procedure HDel(const key: string; const fields: array of const; const Result: TProcCardinal);
    procedure HExists(const key, field: string; const Result: TProcBoolean);
    procedure HGet(const key, field: string; const Result: TProcString);
    procedure HGetAll(const key: string; const onKeyValue: TProcKeyValue; const Result: TProc);
    procedure HIncrBy(const key, field: string; increment: Int64; const Result: TProcInt64);
    procedure HIncrByFloat(const key, field: string; increment: Double; const Result: TProcDouble);
    procedure HKeys(const key: string; const onkey: TProcString; const Result: TProc);
    procedure HLen(const key: string; const Result: TProcCardinal);
    procedure HMGet(const key: string; const fields: array of const; const onValue: TProcString; const Result: TProc);
    procedure HMSet(const key: string; const items: array of const; const Result: TProc);
    procedure HSet(const key, field, value: string; const Result: TProcBoolean);
    procedure HSetNX(const key, field, value: string; const Result: TProcBoolean);
    procedure HVals(const key: string; const onValue: TProcString; const Result: TProc);
    // list
    procedure BLPop(const keys: array of const; timeout: Cardinal; const onValue: TProcString; const Result: TProc);
    procedure BRPop(const keys: array of const; timeout: Cardinal; const onValue: TProcString; const Result: TProc);
    procedure BRPopLPush(const source, destination: string; timeout: Cardinal; const Result: TProcString);
    procedure LIndex(const key: string; index: Cardinal; const Result: TProcString);
    procedure LInsertBefore(const key, pivot, value: string; const Result: TProcCardinal);
    procedure LInsertAfter(const key, pivot, value: string; const Result: TProcCardinal);
    procedure LLen(const key: string; const Result: TProcCardinal);
    procedure LPop(const key: string; const Result: TProcString);
    procedure LPush(const key: string; const values: array of const; const Result: TProcCardinal);
    procedure LPushX(const key, value: string; const Result: TProcCardinal);
    procedure LRange(const key: string; start, stop: Int64; const onValue: TProcString; const Result: TProc);
    procedure LRem(const key: string; count: Int64; const value: string; const Result: TProcCardinal);
    procedure LSet(const key: string; index: Int64; const value: string; const Result: TProc);
    procedure LTrim(const key: string; start, stop: Int64; const Result: TProc);
    procedure RPop(const key: string; const Result: TProcString);
    procedure RPopLPush(const source, destination: string; const Result: TProcString);
    procedure RPush(const key: string; const values: array of const; const Result: TProcCardinal);
    procedure RPushX(const key, value: string; const Result: TProcCardinal);
    // sets
    procedure SAdd(const key: string; const members: array of const; const Result: TProcCardinal);
    procedure SCard(const key: string; const Result: TProcCardinal);
    procedure SDiff(const keys: array of const; const onValue: TProcString; const Result: TProc);
    procedure SDiffStore(const destination: string; const keys: array of const; const Result: TProcCardinal);
    procedure SInter(const keys: array of const; const onValue: TProcString; const Result: TProc);
    procedure SInterStore(const destination: string; const keys: array of const; const Result: TProcCardinal);
    procedure SIsMember(const key, member: string; const Result: TProcBoolean);
    procedure SMembers(const key: string; const onValue: TProcString; const Result: TProc);
    procedure SMove(const source, destination, member: string; const Result: TProcBoolean);
    procedure SPop(const key: string; const Result: TProcString);
    procedure SRandMember(const key: string; const Result: TProcString);
    procedure SRem(const key: string; const members: array of const; const Result: TProcCardinal);
    procedure SUnion(const keys: array of const; const onValue: TProcString; const Result: TProc);
    procedure SUnionStore(const destination: string; const keys: array of const; const Result: TProcCardinal);
    // ordered sets
    procedure ZAdd(const key: string; const items: array of const; const Result: TProcCardinal);
    procedure ZCard(const key: string; const Result: TProcCardinal);
    procedure ZCount(const key, min, max: string; const Result: TProcCardinal); overload;
    procedure ZIncrBy(const key: string; increment: Int64; const member: string; const Result: TProcExtended); overload;
    procedure ZIncrBy(const key: string; increment: Extended; const member: string; const Result: TProcExtended); overload;
    procedure ZInterStore(const destination: string; const keys: array of const; const options: string; const Result: TProcCardinal);
    procedure ZRange(const key: string; start, stop: Int64; scores: Boolean; const onValue: TProcString; const Result: TProc);
    procedure ZRangeByScore(const key, min, max: string; const onValue: TProcString;
      const Result: TProc; const options: string);
    procedure ZRank(const key, member: string; const Result: TProcCardinal);
    procedure ZRem(const key: string; const members: array of const; const Result: TProcCardinal);
    procedure ZRemRangeByRank(const key: string; start, stop: Int64; const Result: TProcCardinal = nil);
    procedure ZRemRangeByScore(const key, min, max: string; const Result: TProcCardinal);
    procedure ZRevRange(const key: string; start, stop: Int64; scores: Boolean; const onValue: TProcString; const Result: TProc);
    procedure ZRevRangeByScore(const key, min, max: string; const onValue: TProcString;
      const Result: TProc; const options: string);
    procedure ZRevRank(const key, member: string; const Result: TProcCardinal);
    procedure ZScore(const key, member: string; const Result: TProcString);
    procedure ZUnionStore(const destination: string; const keys: array of const; const options: string; const Result: TProcCardinal);
    // transaction
    procedure Discard(const Result: TProc);
    procedure Exec(const Result: TProc);
    procedure Multi(const Result: TProc);
    procedure Unwatch(const Result: TProc);
    procedure Watch(const keys: array of const; const Result: TProc);
    // connection
    procedure Auth(const password: string; const Result: TProc);
    procedure Echo(const msg: string; const Result: TProcString);
    procedure Ping(const Result: TProc);
    procedure Quit(const Result: TProc);
    procedure Select(const index: Byte; const Result: TProc);
    // server
    procedure BGRewriteAOF(const Result: TProc);
    procedure BGSave(const Result: TProc);
    procedure ConfigGet(const param: string; const Result: TProcString);
    procedure ConfigSet(const param, value: string; const Result: TProc);
    procedure ConfigResetStat(const Result: TProc);
    procedure DBSize(const Result: TProcCardinal);
    procedure FlushAll(const Result: TProc);
    procedure FlushDB(const Result: TProc);
    procedure LastSave(const Result: TProcCardinal);
    procedure Save(const Result: TProc);
    procedure Shutdown(const options: string; const Result: TProc);
    procedure SlaveOf(const host: string; port: Word; const Result: TProc);
    procedure Monitor(const onValue: TProcString);
  public
    constructor Create; reintroduce;
  end;

implementation
uses Classes, DateUtils, SysConst;

const
  null: NString = (FIsNull: True);

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
    Result := procedure(const data: NString) begin
      if not data.IsNull then
        proc(StrToInt(data)) else
        proc(0);
    end;
end;

function Ret(var v: Cardinal): TProcString; overload;
var
  r: PCardinal;
begin
  r := @v;
  Result := procedure(const data: NString) begin
    if not data.IsNull then
      r^ := StrToInt(data) else
      r^ := 0;
  end;
end;

function Ret(const proc: TProcByte): TProcString; overload;
begin
  if not Assigned(proc) then
    Result := nil else
    Result := procedure(const data: NString) begin
      if not data.IsNull then
        proc(StrToInt(data)) else
        proc(0);
    end;
end;

function Ret(var v: Byte): TProcString; overload;
var
  r: PByte;
begin
  r := @v;
  Result := procedure(const data: NString) begin
    if not data.IsNull then
      r^ := StrToInt(data) else
      r^ := 0;
  end;
end;

function Ret(const proc: TProcInteger): TProcString; overload;
begin
  if not Assigned(proc) then
    Result := nil else
    Result := procedure(const data: NString) begin
      if not data.IsNull then
        proc(StrToInt(data)) else
        proc(0);
    end;
end;

function Ret(var v: Integer): TProcString; overload;
var
  r: PInteger;
begin
  r := @v;
  Result := procedure(const data: NString) begin
    if not data.IsNull then
      r^ := StrToInt(data) else
      r^ := 0;
  end;
end;

function Ret(const proc: TProcInt64): TProcString; overload;
begin
  if not Assigned(proc) then
    Result := nil else
    Result := procedure(const data: NString) begin
      if not data.IsNull then
        proc(StrToInt64(data)) else
        proc(0);
    end;
end;

function Ret(var v: Int64): TProcString; overload;
var
  r: PInt64;
begin
  r := @v;
  Result := procedure(const data: NString) begin
    if not data.IsNull then
      r^ := StrToInt64(data) else
      r^ := 0;
  end;
end;

function Ret(const proc: TProcUInt64): TProcString; overload;
begin
  if not Assigned(proc) then
    Result := nil else
    Result := procedure(const data: NString) begin
      if not data.IsNull then
        proc(StrToUInt64(data)) else
        proc(0);
    end;
end;

function Ret(var v: UInt64): TProcString; overload;
var
  r: PUInt64;
begin
  r := @v;
  Result := procedure(const data: NString) begin
    if not data.IsNull then
      r^ := StrToUInt64(data) else
      r^ := 0;
  end;
end;

function Ret(const proc: TProcBoolean): TProcString; overload;
begin
  if not Assigned(proc) then
    Result := nil else
    Result := procedure(const data: NString) begin
      if not data.IsNull then
        proc(data.FValue <> '0') else
        proc(False);
    end;
end;

function Ret(var v: Boolean): TProcString; overload;
var
  r: PBoolean;
begin
  r := @v;
  Result := procedure(const data: NString) begin
    if not data.IsNull then
      r^ := data.FValue <> '0' else
      r^ := False;
  end;
end;

function Ret(const proc: TProc): TProcString; overload;
begin
  if not Assigned(proc) then
    Result := nil else
    Result := procedure(const data: NString) begin
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
      Result := procedure(const data: NString) begin
        if not data.IsNull then
          proc(StrToFloat(data, f^)) else
          proc(0.0);
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
  Result := procedure(const data: NString) begin
    if not data.IsNull then
      r^ := StrToFloat(data, f^) else
      r^ := 0.0;
  end;
end;

function Ret(const proc: TProcExtended; var fs: TFormatSettings): TProcString; overload;
var
  f: PFormatSettings;
begin
  if not Assigned(proc) then
    Result := nil else
    begin
      f := @fs;
      Result := procedure(const data: NString) begin
        if not data.IsNull then
          proc(StrToFloat(data, f^)) else
          proc(0.0);
      end;
    end;
end;

function Ret(var v: Extended; var fs: TFormatSettings): TProcString; overload;
var
  r: PExtended;
  f: PFormatSettings;
begin
  r := @v;
  f := @fs;
  Result := procedure(const data: NString) begin
    if not data.IsNull then
      r^ := StrToFloat(data, f^) else
      r^ := 0.0;
  end;
end;

function Ret(var v: string): TProcString; overload;
var
  p: PString;
begin
  p := @v;
  Result := procedure(const data: NString) begin
    p^ := data;
  end;
end;

function Ret(var v: NString): TProcString; overload;
var
  p: PNString;
begin
  p := @v;
  Result := procedure(const data: NString) begin
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

{ NString }

class operator NString.Implicit(const Value: string): NString;
begin
  Result.FValue := Value;
  Result.FIsNull := False;
end;

class operator NString.Implicit(const Value: NString): string;
begin
  Result := Value.FValue;
end;

{ TRedisClient }

procedure TRedisClient.Call(count: Cardinal; const getData: TRedisGetParam;
  const onresponse, onerror, onreturn: TProcString);

function ParseCommand(const cmd: string): TCommand;
begin
  Result := cmdOther;
  case cmd[1] of
    'm', 'M':
      case cmd[2] of
        'u', 'U': if SameText(cmd, 'multi') then Result := cmdMulti;
        'o', 'O': if SameText(cmd, 'monitor') then Result := cmdMonitor;
      end;
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

  entry.command    := ParseCommand(getData(0));
  entry.onresponse := onresponse;
  entry.onerror    := onerror;
  entry.onreturn   := onreturn;

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
      function(index: Cardinal): string
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

procedure TRedisClient.Return(const callback: TProcString; const value: NString);
var
  s: NString;
begin
  if Assigned(callback) then
  begin
    if FSync then
      callback(value) else
      if Assigned(FOnSynchronize) then
        FOnSynchronize(callback, value) else
        begin
          s := value;
          TThreadIt.Queue(nil, procedure begin
            callback(s);
          end);
        end;
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
            Return(e.onreturn, null);
            Result := FSync;
          end;
          msMulti:
            begin
              case e.command of
                cmdExec, cmdDiscard:
                  Return(e.onreturn, null);
              end;
              Result := FSync;
            end;
          msReturn:
            begin
              Return(e.onreturn, null);
              if FMultiresponses.Count = 1 then
              begin
                FMulti := msNone;
                Return(FMultiresponses.Dequeue().onreturn, null);
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
        if FMulti = msMonitoring then
          Exit
        else
        if (FMulti <> msReturn) then
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
            cmdMonitor:
              begin
                if FMulti <> msNone then
                  raise Exception.Create('MONITOR can''t be used now');
                FMulti := msMonitoring;
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
                  Return(e.onresponse, null);
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
    function(index: Cardinal): string begin
      Result := VarItem(@arr[index]);
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

function TRedisClient.VarItem(const item: PVarRec): string;
begin
  case item.VType of
    vtUnicodeString: Result := string(item.VUnicodeString);
    vtInteger : Result := IntToStr(item.VInteger);
    vtInt64   : Result := IntToStr(item.VInt64^);
    vtBoolean : Result := IntToStr(Ord(item.VBoolean));
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
        varBoolean:  Result := IntToStr(Ord(VBoolean));
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'AUTH';
        1: Result := password;
      end;
    end,
    nil, doError);
end;

procedure TRedisClientSync.BGRewriteAOF;
begin
  SimpleCommand('BGREWRITEAOF');
end;

procedure TRedisClientSync.BGSave;
begin
  SimpleCommand('BGSAVE');
end;

procedure TRedisClientSync.BLPop(const keys: array of const; timeout: Cardinal;
  const onValue: TProcString);
var
  count: Cardinal;
  arr: PVarRecArray;
begin
  count := 2 + Length(keys);
  arr := @keys;
  Call(count,
    function(index: Cardinal): string begin
      if index = 0 then
        Result := 'BLPOP'
      else
        if index < count - 1 then
          Result := VarItem(@arr[index-1]) else
          Result := IntToStr(timeout);
    end,
    onValue, doError);
end;

procedure TRedisClientSync.BRPop(const keys: array of const; timeout: Cardinal;
  const onValue: TProcString);
var
  count: Cardinal;
  arr: PVarRecArray;
begin
  count := 2 + Length(keys);
  arr := @keys;
  Call(count,
    function(index: Cardinal): string begin
      if index = 0 then
        Result := 'BRPOP'
      else
        if index < count - 1 then
          Result := VarItem(@arr[index-1]) else
          Result := IntToStr(timeout);
    end,
    onValue, doError);
end;

function TRedisClientSync.BRPopLPush(const source, destination: string;
  timeout: Cardinal): NString;
begin
  Call(4,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'BRPOPLPUSH';
        1: Result := source;
        2: Result := destination;
        3: Result := IntToStr(timeout);
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.ConfigGet(const param: string): string;
begin
  Call(3,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'CONFIG';
        1: Result := 'GET';
        2: Result := param;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.ConfigResetstat;
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'CONFIG';
        1: Result := 'RESETSTAT';
      end;
    end,
    nil, doError);
end;

procedure TRedisClientSync.ConfigSet(const param, value: string);
begin
  Call(4,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'CONFIG';
        1: Result := 'SET';
        2: Result := param;
        3: Result := value;
      end;
    end,
    nil, doError);
end;

constructor TRedisClientSync.Create;
begin
  inherited Create(True);
end;

function TRedisClientSync.DBSize: Cardinal;
begin
  Call(1,
    function(index: Cardinal): string begin
      Result := 'DBSIZE';
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.Decr(const key: string): Int64;
begin
  Call(2,
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'DECRBY';
        1: Result := key;
        2: Result := IntToStr(decrement);
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.Del(const key: string): Boolean;
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'DEL';
        1: Result := key;
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'DEL';
      else
        Result := VarItem(@arr[index-1]);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.Discard;
begin
  SimpleCommand('DISCARD');
end;

procedure TRedisClientSync.doError(const error: NString);
begin
  if Assigned(FOnError) then
    FOnError(error) else
    raise Exception.Create(error);
end;

function TRedisClientSync.Echo(const msg: string): string;
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ECHO';
        1: Result := msg;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.Exec;
begin
  SimpleCommand('EXEC');
end;

function TRedisClientSync.Exists(const key: string): Boolean;
begin
  Call(2,
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'EXPIRE';
        1: Result := key;
        2: Result := IntToStr(DateTimeToUnix(date));
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.FlushAll;
begin
  SimpleCommand('FLUSHALL');
end;

procedure TRedisClientSync.FlushDB;
begin
  SimpleCommand('FLUSHDB');
end;

function TRedisClientSync.Get(const key: string): NString;
begin
  Call(2,
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'GETRANGE';
        1: Result := key;
        2: Result := IntToStr(start);
        3: Result := IntToStr(stop);
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.GetSet(const key, value: string): NString;
begin
  Call(3,
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'HDEL';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2]);
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.HExists(const key, field: string): Boolean;
begin
  Call(3,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'HEXISTS';
        1: Result := key;
        2: Result := field;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.HGet(const key, field: string): NString;
begin
  Call(3,
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'HGETALL';
        1: Result := key;
      end;
    end,
    procedure(const data: NString) begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'HMGET';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2]);
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'HMSET';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2]);
      end;
    end,
    nil, doError);
end;

function TRedisClientSync.HSet(const key, field, value: string): Boolean;
begin
  Call(4,
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'INCRBYFLOAT';
        1: Result := key;
        2: Result := FloatToStr(increment, FFormatSettings);
      end;
    end,
    Ret(Result, FFormatSettings), doError);
end;

procedure TRedisClientSync.Keys(const pattern: string;
  const onkey: TProcString);
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'KEYS';
        1: Result := pattern;
      end;
    end,
    onkey, doError);
end;

function TRedisClientSync.LastSave: Cardinal;
begin
  Call(1,
    function(index: Cardinal): string begin
      Result := 'LASTSAVE';
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.LIndex(const key: string; index: Cardinal): NString;
begin
  Call(3,
    function(i: Cardinal): string begin
      case i of
        0: Result := 'LINDEX';
        1: Result := key;
        2: Result := IntToStr(index);
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.LInsertAfter(const key, pivot,
  value: string): Cardinal;
begin
  Call(5,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'LINSERT';
        1: Result := key;
        2: Result := 'AFTER';
        3: Result := pivot;
        4: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.LInsertBefore(const key, pivot, value: string): Cardinal;
begin
  Call(5,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'LINSERT';
        1: Result := key;
        2: Result := 'BEFORE';
        3: Result := pivot;
        4: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.LLen(const key: string): Cardinal;
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'LLEN';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.LPop(const key: string): NString;
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'LPOP';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.LPush(const key: string;
  const values: array of const): Cardinal;
var
  arr: PVarRecArray;
begin
  arr := @values;
  Call(2 + Length(values),
    function(index: Cardinal): string begin
      case index of
        0: Result := 'LPUSH';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2]);
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.LPushX(const key, value: string): Cardinal;
begin
  Call(3,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'LPUSHX';
        1: Result := key;
        2: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.LRange(const key: string; start, stop: Int64;
  const onValue: TProcString);
begin
  Call(4,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'LRANGE';
        1: Result := key;
        2: Result := IntToStr(start);
        3: Result := IntToStr(stop);
      end;
    end,
    onValue, doError);
end;

function TRedisClientSync.LRem(const key: string; count: Int64;
  const value: string): Cardinal;
begin
  Call(4,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'LREM';
        1: Result := key;
        2: Result := IntToStr(count);
        3: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.LSet(const key: string; index: Int64;
  const value: string);
begin
  Call(4,
    function(i: Cardinal): string begin
      case i of
        0: Result := 'LSET';
        1: Result := key;
        2: Result := IntToStr(index);
        3: Result := value;
      end;
    end,
    nil, doError);
end;

procedure TRedisClientSync.LTrim(const key: string; start, stop: Int64);
begin
  Call(4,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'LTRIM';
        1: Result := key;
        2: Result := IntToStr(start);
        3: Result := IntToStr(stop);
      end;
    end,
    nil, doError);
end;

procedure TRedisClientSync.MGet(const keys: array of const;
  const onvalue: TProcString);
var
  arr: PVarRecArray;
begin
  arr := @keys[0];
  Call(Length(keys) + 1,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'MGET';
      else
        Result := VarItem(@arr[index-1]);
      end;
    end,
    onvalue, doError);
end;

function TRedisClientSync.Move(const key: string; db: Word): Boolean;
begin
  Call(3,
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'MSET';
      else
        Result := VarItem(@arr[index-1]);
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'MSETNX';
      else
        Result := VarItem(@arr[index-1]);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.Multi;
begin
  SimpleCommand('MULTI');
end;

function TRedisClientSync.ObjectEncoding(const key: string): string;
begin
  Call(3,
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'EXPIRE';
        1: Result := key;
        2: Result := IntToStr(DateTimeToMilisec(date));
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.Ping;
begin
  SimpleCommand('PING');
end;

procedure TRedisClientSync.PSetEx(const key: string;
  milliseconds: UInt64; const value: string);
begin
  Call(4,
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SET';
        1: Result := key;
        2: Result := value;
      end;
    end,
    nil, doError);
end;

procedure TRedisClientSync.Quit;
begin
  SimpleCommand('QUIT');
end;

function TRedisClientSync.RandomKey: string;
begin
  Call(1,
    function(index: Cardinal): string begin
      Result := 'RANDOMKEY';
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.Rename(const key, newkey: string);
begin
  Call(3,
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'RENAME';
        1: Result := key;
        2: Result := newkey;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.RPop(const key: string): NString;
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'RPOP';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.RPopLPush(const source, destination: string): NString;
begin
  Call(3,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'RPOPLPUSH';
        1: Result := source;
        2: Result := destination;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.RPush(const key: string;
  const values: array of const): Cardinal;
var
  arr: PVarRecArray;
begin
  arr := @values;
  Call(2 + Length(values),
    function(index: Cardinal): string begin
      case index of
        0: Result := 'RPUSH';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2]);
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.RPushX(const key, value: string): Cardinal;
begin
  Call(3,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'RPUSHX';
        1: Result := key;
        2: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.SAdd(const key: string;
  const members: array of const): Cardinal;
var
  arr: PVarRecArray;
begin
  arr := @members;
  Call(2 + Length(members),
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SADD';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2]);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.Save;
begin
  SimpleCommand('SAVE');
end;

function TRedisClientSync.SCard(const key: string): Cardinal;
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SCARD';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.SDiff(const keys: array of const;
  const onValue: TProcString);
var
  arr: PVarRecArray;
begin
  arr := @keys;
  Call(1 + Length(keys),
    function(index: Cardinal): string begin
      if index = 0 then
        Result := 'SDIFF' else
        Result := VarItem(@arr[index-1]);
    end,
    onValue, doError);
end;

function TRedisClientSync.SDiffStore(const destination: string;
  const keys: array of const): Cardinal;
var
  arr: PVarRecArray;
begin
  Call(2 + Length(keys),
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SDIFFSTORE';
        1: Result := destination;
      else
        Result := VarItem(@arr[index-2]);
      end;
    end,
    Ret(Result), doError)
end;

procedure TRedisClientSync.Select(const index: Byte);
begin
  Call(2,
    function(i: Cardinal): string begin
      case i of
        0: Result := 'SELECT';
        1: Result := IntToStr(index);
      end;
    end,
    nil, doError)
end;

function TRedisClientSync.SetBit(const key: string; offset: Cardinal;
  value: Boolean): Boolean;
begin
  Call(4,
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SETRANGE';
        1: Result := key;
        2: Result := IntToStr(offset);
        3: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.Shutdown(const options: string);
var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    ParseCommand(options, list);
    Call(list.Count + 1,
      function(index: Cardinal): string
      begin
        case index of
          0: Result := 'SHUTDOWN';
        else
          Result := list[index - 1];
        end;
      end,
      nil, doError);
  finally
    list.Free;
  end;
end;

procedure TRedisClientSync.SimpleCommand(const cmd: string);
begin
  Call(1,
    function(index: Cardinal): string begin
      Result := cmd;
    end,
    nil, doError);
end;

procedure TRedisClientSync.SInter(const keys: array of const;
  const onValue: TProcString);
var
  arr: PVarRecArray;
begin
  arr := @keys;
  Call(1 + Length(keys),
    function(index: Cardinal): string begin
      if index = 0 then
        Result := 'SINTER' else
        Result := VarItem(@arr[index-1]);
    end,
    onValue, doError);
end;

function TRedisClientSync.SInterStore(const destination: string;
  const keys: array of const): Cardinal;
var
  arr: PVarRecArray;
begin
  Call(2 + Length(keys),
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SINTERSTORE';
        1: Result := destination;
      else
        Result := VarItem(@arr[index-2]);
      end;
    end,
    Ret(Result), doError)
end;

function TRedisClientSync.SIsMember(const key, member: string): Boolean;
begin
  Call(3,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SISMEMBER';
        1: Result := key;
        2: Result := member;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.SlaveOf(const host: string; port: Word);
begin
  Call(3,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SLAVEOF';
        1: Result := host;
        2: Result := IntToStr(port);
      end;
    end,
    nil, doError);
end;

procedure TRedisClientSync.SMembers(const key: string;
  const onValue: TProcString);
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SMEMBERS';
        1: Result := key;
      end;
    end,
    onValue, doError);
end;

function TRedisClientSync.SMove(const source, destination,
  member: string): Boolean;
begin
  Call(4,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SMOVE';
        1: Result := source;
        2: Result := destination;
        3: Result := member;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.Sort(const query: string;
  const onkey: TProcString);
var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    ParseCommand(query, list);
    Call(list.Count + 1,
      function(index: Cardinal): string
      begin
        case index of
          0: Result := 'SORT';
        else
          Result := list[index - 1];
        end;
      end,
      onkey, doError);
  finally
    list.Free;
  end;
end;

function TRedisClientSync.SPop(const key: string): NString;
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SPOP';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.SRandMember(const key: string): NString;
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SRANDMEMBER';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.SRem(const key: string;
  const members: array of const): Cardinal;
var
  arr: PVarRecArray;
begin
  arr := @members;
  Call(2 + Length(members),
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SREM';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2]);
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.StrLen(const key: string): Cardinal;
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'STRLEN';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.SUnion(const keys: array of const;
  const onValue: TProcString);
var
  arr: PVarRecArray;
begin
  arr := @keys;
  Call(1 + Length(keys),
    function(index: Cardinal): string begin
      if index = 0 then
        Result := 'SUNION' else
        Result := VarItem(@arr[index-1])
    end,
    onValue, doError);
end;

function TRedisClientSync.SUnionStore(const destination: string;
  const keys: array of const): Cardinal;
var
  arr: PVarRecArray;
begin
  Call(2 + Length(keys),
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SUNIONSTORE';
        1: Result := destination;
      else
        Result := VarItem(@arr[index-2]);
      end;
    end,
    Ret(Result), doError)
end;

procedure TRedisClientSync.Unwatch;
begin
  SimpleCommand('UNWATCH');
end;

procedure TRedisClientSync.Watch(const keys: array of const);
var
  arr: PVarRecArray;
begin
  arr := @keys;
  Call(1 + Length(keys),
    function(index: Cardinal): string begin
      case index of
        0: Result := 'WATCH';
      else
        Result := VarItem(@arr[index-1]);
      end;
    end,
    nil, doError);
end;


function TRedisClientSync.ZAdd(const key: string;
  const items: array of const): Cardinal;
var
  arr: PVarRecArray;
begin
  arr := @items;
  Call(2 + Length(items),
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZADD';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2]);
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.ZCard(const key: string): Cardinal;
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZCARD';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.ZCount(const key, min, max: string): Cardinal;
begin
  Call(4,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZCOUNT';
        1: Result := key;
        2: Result := min;
        3: Result := max;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.ZIncrBy(const key: string; increment: Int64;
  const member: string): Extended;
begin
  Call(4,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZINCRBY';
        1: Result := key;
        2: Result := IntToStr(increment);
        3: Result := member;
      end;
    end,
    Ret(Result, FFormatSettings), doError);
end;

function TRedisClientSync.ZIncrBy(const key: string; increment: Extended;
  const member: string): Extended;
begin
  Call(4,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZINCRBY';
        1: Result := key;
        2: Result := FloatToStr(increment, FFormatSettings);
        3: Result := member;
      end;
    end,
    Ret(Result, FFormatSettings), doError);
end;

function TRedisClientSync.ZInterStore(const destination: string;
  const keys: array of const; const options: string): Cardinal;
var
  list: TStringList;
  arr: PVarRecArray;
  arrlen: Cardinal;
begin
  arr := @keys;
  arrlen := Length(keys);
  list := TStringList.Create;
  try
    ParseCommand(options, list);
    Call(3 + arrlen + Cardinal(list.Count),
      function(index: Cardinal): string
      begin
        case index of
          0: Result := 'ZINTERSTORE';
          1: Result := destination;
          2: Result := IntToStr(arrlen);
        else
          if index < arrlen + 3 then
            Result := VarItem(@arr[index-3]) else
            Result := list[index - 3 - arrlen];
        end;
      end,
      Ret(Result), doError);
  finally
    list.Free;
  end;
end;

procedure TRedisClientSync.ZRange(const key: string; start, stop: Int64;
  scores: Boolean; const onValue: TProcString);
var
  count: Cardinal;
begin
  if not scores then
    count := 4 else
    count := 5;
  Call(count,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZRANGE';
        1: Result := key;
        2: Result := IntToStr(start);
        3: Result := IntToStr(stop);
        4: Result := 'WITHSCORES';
      end;
    end,
    onValue, doError);
end;

procedure TRedisClientSync.ZRangeByScore(const key, min, max: string;
  const onValue: TProcString; const options: string);
var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    ParseCommand(options, list);
    Call(4 + list.Count,
      function(index: Cardinal): string
      begin
        case index of
          0: Result := 'ZRANGEBYSCORE';
          1: Result := key;
          2: Result := min;
          3: Result := max;
        else
          Result := list[index - 4];
        end;
      end,
      onValue, doError);
  finally
    list.Free;
  end;
end;

function TRedisClientSync.ZRank(const key, member: string): Cardinal;
begin
  Call(3,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZRANK';
        1: Result := key;
        2: Result := member;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.ZRem(const key: string;
  const members: array of const): Cardinal;
var
  arr: PVarRecArray;
begin
  arr := @members;
  Call(2 + Length(members),
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZREM';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2]);
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.ZRemRangeByRank(const key: string; start,
  stop: Int64): Cardinal;
begin
  Call(4,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZREMRANGEBYRANK';
        1: Result := key;
        2: Result := IntToStr(start);
        3: Result := IntToStr(stop);
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.ZRemRangeByScore(const key, min,
  max: string): Cardinal;
begin
  Call(4,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZREMRANGEBYSCORE';
        1: Result := key;
        2: Result := min;
        3: Result := max;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientSync.ZRevRange(const key: string; start, stop: Int64;
  scores: Boolean; const onValue: TProcString);
var
  count: Cardinal;
begin
  if not scores then
    count := 4 else
    count := 5;
  Call(count,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZREVRANGE';
        1: Result := key;
        2: Result := IntToStr(start);
        3: Result := IntToStr(stop);
        4: Result := 'WITHSCORES';
      end;
    end,
    onValue, doError);
end;

procedure TRedisClientSync.ZRevRangeByScore(const key, min, max: string;
  const onValue: TProcString; const options: string);
var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    ParseCommand(options, list);
    Call(4 + list.Count,
      function(index: Cardinal): string
      begin
        case index of
          0: Result := 'ZREVRANGEBYSCORE';
          1: Result := key;
          2: Result := min;
          3: Result := max;
        else
          Result := list[index - 4];
        end;
      end,
      onValue, doError);
  finally
    list.Free;
  end;
end;

function TRedisClientSync.ZRevRank(const key, member: string): Cardinal;
begin
  Call(3,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZREVRANK';
        1: Result := key;
        2: Result := member;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.ZScore(const key, member: string): NString;
begin
  Call(3,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZSCORE';
        1: Result := key;
        2: Result := member;
      end;
    end,
    Ret(Result), doError);
end;

function TRedisClientSync.ZUnionStore(const destination: string;
  const keys: array of const; const options: string): Cardinal;
var
  list: TStringList;
  arr: PVarRecArray;
  arrlen: Cardinal;
begin
  arr := @keys;
  arrlen := Length(keys);
  list := TStringList.Create;
  try
    ParseCommand(options, list);
    Call(3 + arrlen + Cardinal(list.Count),
      function(index: Cardinal): string
      begin
        case index of
          0: Result := 'ZUNIONSTORE';
          1: Result := destination;
          2: Result := IntToStr(arrlen);
        else
          if index < arrlen + 3 then
            Result := VarItem(@arr[index-3]) else
            Result := list[index - 3 - arrlen];
        end;
      end,
      Ret(Result), doError);
  finally
    list.Free;
  end;
end;

{ TRedisClientAsync }

procedure TRedisClientAsync.Append(const key, value: string;
  const Result: TProcInt64);
begin
  Call(3,
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'AUTH';
        1: Result := password;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.BGRewriteAOF(const Result: TProc);
begin
  SimpleCommand('BGREWRITEAOF', Result);
end;

procedure TRedisClientAsync.BGSave(const Result: TProc);
begin
  SimpleCommand('BGSAVE', Result);
end;

procedure TRedisClientAsync.BLPop(const keys: array of const; timeout: Cardinal;
  const onValue: TProcString; const Result: TProc);
var
  count: Cardinal;
  arr: PVarRecArray;
begin
  count := 2 + Length(keys);
  arr := @keys;
  Call(count,
    function(index: Cardinal): string begin
      if index = 0 then
        Result := 'BLPOP'
      else
        if index < count - 1 then
          Result := VarItem(@arr[index-1]) else
          Result := IntToStr(timeout);
    end,
    onValue, doError, Ret(Result));
end;

procedure TRedisClientAsync.BRPop(const keys: array of const; timeout: Cardinal;
  const onValue: TProcString; const Result: TProc);
var
  count: Cardinal;
  arr: PVarRecArray;
begin
  count := 2 + Length(keys);
  arr := @keys;
  Call(count,
    function(index: Cardinal): string begin
      if index = 0 then
        Result := 'BRPOP'
      else
        if index < count - 1 then
          Result := VarItem(@arr[index-1]) else
          Result := IntToStr(timeout);
    end,
    onValue, doError, Ret(Result));
end;

procedure TRedisClientAsync.BRPopLPush(const source, destination: string;
  timeout: Cardinal; const Result: TProcString);
begin
  Call(4,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'BRPOPLPUSH';
        1: Result := source;
        2: Result := destination;
        3: Result := IntToStr(timeout);
      end;
    end,
    Result, doError);
end;

procedure TRedisClientAsync.ConfigGet(const param: string;
  const Result: TProcString);
begin
  Call(3,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'CONFIG';
        1: Result := 'GET';
        2: Result := param;
      end;
    end,
    Result, doError);
end;

procedure TRedisClientAsync.ConfigResetStat(const Result: TProc);
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'CONFIG';
        1: Result := 'RESETSTAT';
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.ConfigSet(const param, value: string;
  const Result: TProc);
begin
  Call(4,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'CONFIG';
        1: Result := 'SET';
        2: Result := param;
        3: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

constructor TRedisClientAsync.Create;
begin
  inherited Create(False);
end;

procedure TRedisClientAsync.DBSize(const Result: TProcCardinal);
begin
  Call(1,
    function(index: Cardinal): string begin
      Result := 'DBSIZE';
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.Decr(const key: string; const Result: TProcInt64);
begin
  Call(2,
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'DECRBY';
        1: Result := key;
        2: Result := IntToStr(decrement);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.Del(const key: string; const Result: TProcBoolean);
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'DEL';
        1: Result := key;
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'DEL';
      else
        Result := VarItem(@arr[index-1]);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.Discard(const Result: TProc);
begin
  SimpleCommand('DISCARD', Result);
end;

procedure TRedisClientAsync.doError(const error: NString);
begin
  if Assigned(FOnError) then
    FOnError(error) else
    raise Exception.Create(error);
end;

procedure TRedisClientAsync.Echo(const msg: string; const Result: TProcString);
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ECHO';
        1: Result := msg;
      end;
    end,
    Result, doError);
end;

procedure TRedisClientAsync.Exec(const Result: TProc);
begin
  SimpleCommand('EXEC', Result);
end;

procedure TRedisClientAsync.Exists(const key: string; const Result: TProcBoolean);
begin
  Call(2,
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'EXPIRE';
        1: Result := key;
        2: Result := IntToStr(DateTimeToUnix(date));
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.FlushAll(const Result: TProc);
begin
  SimpleCommand('FLUSHALL', Result);
end;

procedure TRedisClientAsync.FlushDB(const Result: TProc);
begin
  SimpleCommand('FLUSHDB', Result);
end;

procedure TRedisClientAsync.Get(const key: string; const Result: TProcString);
begin
  Call(2,
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'HDEL';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2]);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.HExists(const key, field: string;
  const Result: TProcBoolean);
begin
  Call(3,
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'HGETALL';
        1: Result := key;
      end;
    end,
    procedure(const data: NString) begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'HMGET';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2]);
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'HMSET';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2]);
      end;
    end,
    nil, doError, Ret(Result));
end;

procedure TRedisClientAsync.HSet(const key, field, value: string;
  const Result: TProcBoolean);
begin
  Call(4,
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'KEYS';
        1: Result := pattern;
      end;
    end,
    onkey, doError, Ret(Result));
end;

procedure TRedisClientAsync.LastSave(const Result: TProcCardinal);
begin
  Call(1,
    function(index: Cardinal): string begin
      Result := 'LASTSAVE';
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.LIndex(const key: string; index: Cardinal;
  const Result: TProcString);
begin
  Call(3,
    function(i: Cardinal): string begin
      case i of
        0: Result := 'LINDEX';
        1: Result := key;
        2: Result := IntToStr(index);
      end;
    end,
    Result, doError);
end;

procedure TRedisClientAsync.LInsertAfter(const key, pivot, value: string;
  const Result: TProcCardinal);
begin
  Call(5,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'LINSERT';
        1: Result := key;
        2: Result := 'AFTER';
        3: Result := pivot;
        4: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.LInsertBefore(const key, pivot, value: string;
  const Result: TProcCardinal);
begin
  Call(5,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'LINSERT';
        1: Result := key;
        2: Result := 'BEFORE';
        3: Result := pivot;
        4: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.LLen(const key: string;
  const Result: TProcCardinal);
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'LLEN';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.LPop(const key: string; const Result: TProcString);
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'LPOP';
        1: Result := key;
      end;
    end,
    Result, doError);
end;

procedure TRedisClientAsync.LPush(const key: string;
  const values: array of const; const Result: TProcCardinal);
var
  arr: PVarRecArray;
begin
  arr := @values;
  Call(2 + Length(values),
    function(index: Cardinal): string begin
      case index of
        0: Result := 'LPUSH';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2]);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.LPushX(const key, value: string;
  const Result: TProcCardinal);
begin
  Call(3,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'LPUSHX';
        1: Result := key;
        2: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.LRange(const key: string; start, stop: Int64;
  const onValue: TProcString; const Result: TProc);
begin
  Call(4,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'LRANGE';
        1: Result := key;
        2: Result := IntToStr(start);
        3: Result := IntToStr(stop);
      end;
    end,
    onValue, doError, Ret(Result));
end;

procedure TRedisClientAsync.LRem(const key: string; count: Int64;
  const value: string; const Result: TProcCardinal);
begin
  Call(4,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'LREM';
        1: Result := key;
        2: Result := IntToStr(count);
        3: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.LSet(const key: string; index: Int64;
  const value: string; const Result: TProc);
begin
  Call(4,
    function(i: Cardinal): string begin
      case i of
        0: Result := 'LSET';
        1: Result := key;
        2: Result := IntToStr(index);
        3: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.LTrim(const key: string; start, stop: Int64;
  const Result: TProc);
begin
  Call(4,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'LTRIM';
        1: Result := key;
        2: Result := IntToStr(start);
        3: Result := IntToStr(stop);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.MGet(const keys: array of const;
  const onvalue: TProcString; const Result: TProc);
var
  arr: PVarRecArray;
begin
  arr := @keys[0];
  Call(Length(keys) + 1,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'MGET';
      else
        Result := VarItem(@arr[index-1]);
      end;
    end,
    onvalue, doError, Ret(Result));
end;

procedure TRedisClientAsync.Monitor(const onValue: TProcString);
begin
  Call(1,
    function(index: Cardinal): string begin
      Result := 'MONITOR';
    end,
    onValue);
end;

procedure TRedisClientAsync.Move(const key: string; db: Word;
  const Result: TProcBoolean);
begin
  Call(3,
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'MSET';
      else
        Result := VarItem(@arr[index-1]);
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'MSETNX';
      else
        Result := VarItem(@arr[index-1]);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.Multi(const Result: TProc);
begin
  SimpleCommand('MULTI', Result);
end;

procedure TRedisClientAsync.ObjectEncoding(const key: string;
  const Result: TProcString);
begin
  Call(3,
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'PEXPIRE';
        1: Result := key;
        2: Result := IntToStr(DateTimeToMilisec(date));
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.Ping(const Result: TProc);
begin
  SimpleCommand('PING', Result);
end;

procedure TRedisClientAsync.PSetEx(const key: string; milliseconds: UInt64;
  const value: string; const Result: TProc);
begin
  Call(4,
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SET';
        1: Result := key;
        2: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.Quit(const Result: TProc);
begin
  SimpleCommand('QUIT', Result);
end;

procedure TRedisClientAsync.RandomKey(const Result: TProcString);
begin
  Call(1,
    function(index: Cardinal): string begin
      Result := 'RANDOMKEY';
    end,
    Result, doError);
end;

procedure TRedisClientAsync.Rename(const key, newkey: string;
  const Result: TProc);
begin
  Call(3,
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'RENAME';
        1: Result := key;
        2: Result := newkey;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.RPop(const key: string; const Result: TProcString);
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'RPOP';
        1: Result := key;
      end;
    end,
    Result, doError);
end;

procedure TRedisClientAsync.RPopLPush(const source, destination: string;
  const Result: TProcString);
begin
  Call(3,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'RPOPLPUSH';
        1: Result := source;
        2: Result := destination;
      end;
    end,
    Result, doError);
end;

procedure TRedisClientAsync.RPush(const key: string;
  const values: array of const; const Result: TProcCardinal);
var
  arr: PVarRecArray;
begin
  arr := @values;
  Call(2 + Length(values),
    function(index: Cardinal): string begin
      case index of
        0: Result := 'RPUSH';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2]);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.RPushX(const key, value: string;
  const Result: TProcCardinal);
begin
  Call(3,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'RPUSHX';
        1: Result := key;
        2: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.SAdd(const key: string;
  const members: array of const; const Result: TProcCardinal);
var
  arr: PVarRecArray;
begin
  arr := @members;
  Call(2 + Length(members),
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SADD';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2]);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.Save(const Result: TProc);
begin
  SimpleCommand('SAVE', Result);
end;

procedure TRedisClientAsync.SCard(const key: string;
  const Result: TProcCardinal);
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SCARD';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.SDiff(const keys: array of const;
  const onValue: TProcString; const Result: TProc);
var
  arr: PVarRecArray;
begin
  arr := @keys;
  Call(1 + Length(keys),
    function(index: Cardinal): string begin
      if index = 0 then
        Result := 'SDIFF' else
        Result := VarItem(@arr[index-1]);
    end,
    onValue, doError, Ret(Result));
end;

procedure TRedisClientAsync.SDiffStore(const destination: string;
  const keys: array of const; const Result: TProcCardinal);
var
  arr: PVarRecArray;
begin
  Call(2 + Length(keys),
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SDIFFSTORE';
        1: Result := destination;
      else
        Result := VarItem(@arr[index-2]);
      end;
    end,
    Ret(Result), doError)
end;

procedure TRedisClientAsync.Select(const index: Byte; const Result: TProc);
begin
  Call(2,
    function(i: Cardinal): string begin
      case i of
        0: Result := 'SELECT';
        1: Result := IntToStr(index);
      end;
    end,
    Ret(Result), doError)
end;

procedure TRedisClientAsync.SetBit(const key: string; offset: Cardinal;
  value: Boolean; const Result: TProcBoolean);
begin
  Call(4,
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
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
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SETRANGE';
        1: Result := key;
        2: Result := IntToStr(offset);
        3: Result := value;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.Shutdown(const options: string;
  const Result: TProc);
var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    ParseCommand(options, list);
    Call(list.Count + 1,
      function(index: Cardinal): string
      begin
        case index of
          0: Result := 'SHUTDOWN';
        else
          Result := list[index - 1];
        end;
      end,
      Ret(Result), doError);
  finally
    list.Free;
  end;
end;

procedure TRedisClientAsync.SimpleCommand(const cmd: string; Result: TProc);
begin
  Call(1,
    function(index: Cardinal): string begin
      Result := cmd end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.SInter(const keys: array of const;
  const onValue: TProcString; const Result: TProc);
var
  arr: PVarRecArray;
begin
  arr := @keys;
  Call(1 + Length(keys),
    function(index: Cardinal): string begin
      if index = 0 then
        Result := 'SINTER' else
        Result := VarItem(@arr[index-1]);
    end,
    onValue, doError, Ret(Result));
end;

procedure TRedisClientAsync.SInterStore(const destination: string;
  const keys: array of const; const Result: TProcCardinal);
var
  arr: PVarRecArray;
begin
  Call(2 + Length(keys),
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SINTERSTORE';
        1: Result := destination;
      else
        Result := VarItem(@arr[index-2]);
      end;
    end,
    Ret(Result), doError)
end;

procedure TRedisClientAsync.SIsMember(const key, member: string;
  const Result: TProcBoolean);
begin
  Call(3,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SISMEMBER';
        1: Result := key;
        2: Result := member;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.SlaveOf(const host: string; port: Word;
  const Result: TProc);
begin
  Call(3,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SLAVEOF';
        1: Result := host;
        2: Result := IntToStr(port);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.SMembers(const key: string;
  const onValue: TProcString; const Result: TProc);
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SMEMBERS';
        1: Result := key;
      end;
    end,
    onValue, doError, Ret(Result));
end;

procedure TRedisClientAsync.SMove(const source, destination, member: string;
  const Result: TProcBoolean);
begin
  Call(4,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SMOVE';
        1: Result := source;
        2: Result := destination;
        3: Result := member;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.Sort(const query: string;
  const onkey: TProcString; const Result: TProc);
var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    ParseCommand(query, list);
    Call(list.Count + 1,
      function(index: Cardinal): string
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

procedure TRedisClientAsync.SPop(const key: string; const Result: TProcString);
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SPOP';
        1: Result := key;
      end;
    end,
    Result, doError);
end;

procedure TRedisClientAsync.SRandMember(const key: string;
  const Result: TProcString);
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SRANDMEMBER';
        1: Result := key;
      end;
    end,
    Result, doError);
end;

procedure TRedisClientAsync.SRem(const key: string;
  const members: array of const; const Result: TProcCardinal);
var
  arr: PVarRecArray;
begin
  arr := @members;
  Call(2 + Length(members),
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SREM';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2]);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.StrLen(const key: string;
  const Result: TProcCardinal);
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'STRLEN';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.SUnion(const keys: array of const;
  const onValue: TProcString; const Result: TProc);
var
  arr: PVarRecArray;
begin
  arr := @keys;
  Call(1 + Length(keys),
    function(index: Cardinal): string begin
      if index = 0 then
        Result := 'SUNION' else
        Result := VarItem(@arr[index-1])
    end,
    onValue, doError, Ret(Result));
end;

procedure TRedisClientAsync.SUnionStore(const destination: string;
  const keys: array of const; const Result: TProcCardinal);
var
  arr: PVarRecArray;
begin
  Call(2 + Length(keys),
    function(index: Cardinal): string begin
      case index of
        0: Result := 'SUNIONSTORE';
        1: Result := destination;
      else
        Result := VarItem(@arr[index-2]);
      end;
    end,
    Ret(Result), doError)
end;

procedure TRedisClientAsync.Unwatch(const Result: TProc);
begin
  SimpleCommand('UNWATCH', Result);
end;

procedure TRedisClientAsync.Watch(const keys: array of const;
  const Result: TProc);
var
  arr: PVarRecArray;
begin
  arr := @keys;
  Call(1 + Length(keys),
    function(index: Cardinal): string begin
      case index of
        0: Result := 'WATCH';
      else
        Result := VarItem(@arr[index-1]);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.ZAdd(const key: string; const items: array of const;
  const Result: TProcCardinal);
var
  arr: PVarRecArray;
begin
  arr := @items;
  Call(2 + Length(items),
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZADD';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2]);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.ZCard(const key: string;
  const Result: TProcCardinal);
begin
  Call(2,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZCARD';
        1: Result := key;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.ZCount(const key, min, max: string;
  const Result: TProcCardinal);
begin
  Call(4,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZCOUNT';
        1: Result := key;
        2: Result := min;
        3: Result := max;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.ZIncrBy(const key: string; increment: Int64;
  const member: string; const Result: TProcExtended);
begin
  Call(4,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZINCRBY';
        1: Result := key;
        2: Result := IntToStr(increment);
        3: Result := member;
      end;
    end,
    Ret(Result, FFormatSettings), doError);
end;

procedure TRedisClientAsync.ZIncrBy(const key: string; increment: Extended;
  const member: string; const Result: TProcExtended);
begin
  Call(4,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZINCRBY';
        1: Result := key;
        2: Result := FloatToStr(increment, FFormatSettings);
        3: Result := member;
      end;
    end,
    Ret(Result, FFormatSettings), doError);
end;

procedure TRedisClientAsync.ZInterStore(const destination: string;
  const keys: array of const; const options: string;
  const Result: TProcCardinal);
var
  list: TStringList;
  arr: PVarRecArray;
  arrlen: Cardinal;
begin
  arr := @keys;
  arrlen := Length(keys);
  list := TStringList.Create;
  try
    ParseCommand(options, list);
    Call(3 + arrlen + Cardinal(list.Count),
      function(index: Cardinal): string
      begin
        case index of
          0: Result := 'ZINTERSTORE';
          1: Result := destination;
          2: Result := IntToStr(arrlen);
        else
          if index < arrlen + 3 then
            Result := VarItem(@arr[index-3]) else
            Result := list[index - 3 - arrlen];
        end;
      end,
      Ret(Result), doError);
  finally
    list.Free;
  end;
end;

procedure TRedisClientAsync.ZRange(const key: string; start, stop: Int64;
  scores: Boolean; const onValue: TProcString; const Result: TProc);
var
  count: Cardinal;
begin
  if not scores then
    count := 4 else
    count := 5;
  Call(count,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZRANGE';
        1: Result := key;
        2: Result := IntToStr(start);
        3: Result := IntToStr(stop);
        4: Result := 'WITHSCORES';
      end;
    end,
    onValue, doError);
end;

procedure TRedisClientAsync.ZRangeByScore(const key, min, max: string;
  const onValue: TProcString; const Result: TProc; const options: string);
var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    ParseCommand(options, list);
    Call(4 + list.Count,
      function(index: Cardinal): string
      begin
        case index of
          0: Result := 'ZRANGEBYSCORE';
          1: Result := key;
          2: Result := min;
          3: Result := max;
        else
          Result := list[index - 4];
        end;
      end,
      onValue, doError, Ret(Result));
  finally
    list.Free;
  end;
end;

procedure TRedisClientAsync.ZRank(const key, member: string;
  const Result: TProcCardinal);
begin
  Call(3,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZRANK';
        1: Result := key;
        2: Result := member;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.ZRem(const key: string;
  const members: array of const; const Result: TProcCardinal);
var
  arr: PVarRecArray;
begin
  arr := @members;
  Call(2 + Length(members),
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZREM';
        1: Result := key;
      else
        Result := VarItem(@arr[index-2]);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.ZRemRangeByRank(const key: string; start,
  stop: Int64; const Result: TProcCardinal);
begin
  Call(4,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZREMRANGEBYRANK';
        1: Result := key;
        2: Result := IntToStr(start);
        3: Result := IntToStr(stop);
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.ZRemRangeByScore(const key, min, max: string;
  const Result: TProcCardinal);
begin
  Call(4,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZREMRANGEBYSCORE';
        1: Result := key;
        2: Result := min;
        3: Result := max;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.ZRevRange(const key: string; start, stop: Int64;
  scores: Boolean; const onValue: TProcString; const Result: TProc);
var
  count: Cardinal;
begin
  if not scores then
    count := 4 else
    count := 5;
  Call(count,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZREVRANGE';
        1: Result := key;
        2: Result := IntToStr(start);
        3: Result := IntToStr(stop);
        4: Result := 'WITHSCORES';
      end;
    end,
    onValue, doError);
end;

procedure TRedisClientAsync.ZRevRangeByScore(const key, min, max: string;
  const onValue: TProcString; const Result: TProc; const options: string);
var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    ParseCommand(options, list);
    Call(4 + list.Count,
      function(index: Cardinal): string
      begin
        case index of
          0: Result := 'ZREVRANGEBYSCORE';
          1: Result := key;
          2: Result := min;
          3: Result := max;
        else
          Result := list[index - 4];
        end;
      end,
      onValue, doError, Ret(Result));
  finally
    list.Free;
  end;
end;

procedure TRedisClientAsync.ZRevRank(const key, member: string;
  const Result: TProcCardinal);
begin
  Call(3,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZREVRANK';
        1: Result := key;
        2: Result := member;
      end;
    end,
    Ret(Result), doError);
end;

procedure TRedisClientAsync.ZScore(const key, member: string;
  const Result: TProcString);
begin
  Call(3,
    function(index: Cardinal): string begin
      case index of
        0: Result := 'ZSCORE';
        1: Result := key;
        2: Result := member;
      end;
    end,
    Result, doError);
end;

procedure TRedisClientAsync.ZUnionStore(const destination: string;
  const keys: array of const; const options: string;
  const Result: TProcCardinal);
var
  list: TStringList;
  arr: PVarRecArray;
  arrlen: Cardinal;
begin
  arr := @keys;
  arrlen := Length(keys);
  list := TStringList.Create;
  try
    ParseCommand(options, list);
    Call(3 + arrlen + Cardinal(list.Count),
      function(index: Cardinal): string
      begin
        case index of
          0: Result := 'ZUNIONSTORE';
          1: Result := destination;
          2: Result := IntToStr(arrlen);
        else
          if index < arrlen + 3 then
            Result := VarItem(@arr[index-3]) else
            Result := list[index - 3 - arrlen];
        end;
      end,
      Ret(Result), doError);
  finally
    list.Free;
  end;
end;

end.

