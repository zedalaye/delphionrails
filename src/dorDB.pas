unit dorDB;
{$ifdef FPC}
{$mode ObjFpc}{$H+}
{$endif}
interface
uses supertypes, superobject, superdate, classes, dorUtils, SysUtils, Generics.Collections;

type
  IDBConnectionPool = interface;
  IDBConnection = interface;
  IDBTransaction = interface;
  IDBQuery = interface;
  IDBBlob = interface;

  TQueryOption = (
    qoSingleton,
    qoArray,
    qoValue
  );
  TQueryOptions = set of TQueryOption;

  IDBConnectionPool = interface
    ['{27621D9A-AAE9-4E24-82F5-A18D84E415F3}']
    function GetConnection: IDBConnection;
    function GetSize: Integer;
    procedure ClearPool;
    procedure Lock;
    procedure Unlock;
  end;

  IDBConnection = interface
  ['{843E105A-B8E0-42A9-AFA0-CF5AA843DB8B}']
    function Transaction(const Options: ISuperObject = nil): IDBTransaction; overload;
    function Transaction(const OtherConnections: array of IDBConnection; const Options: ISuperObject = nil): IDBTransaction; overload;
    function Transaction(const Options: SOString): IDBTransaction; overload;
    function Query(const Options: ISuperObject = nil): IDBQuery; overload;
    function Query(const sql: SOString; options: TQueryOptions = []): IDBQuery; overload;
    function Singleton(const Sql: SOString): IDBQuery;
    function Table(const Sql: SOString): IDBQuery;
    function Row(const Sql: SOString): IDBQuery;
    function List(const Sql: SOString): IDBQuery;
    function Cell(const Sql: SOString): IDBQuery;
    procedure ExecuteImmediate(const Options: SOString); overload;
  end;

  IDBTransaction = interface
  ['{51992399-2D1A-47EF-9DB1-C5654325F41B}']
    function Query(const Options: ISuperObject = nil; const Connection: IDBConnection = nil): IDBQuery; overload;
    function Query(const Sql: SOString; options: TQueryOptions = []; const Connection: IDBConnection = nil): IDBQuery; overload;
    function Singleton(const Sql: SOString; const Connection: IDBConnection = nil): IDBQuery;
    function Table(const Sql: SOString; const Connection: IDBConnection = nil): IDBQuery;
    function Row(const Sql: SOString; const Connection: IDBConnection = nil): IDBQuery;
    function List(const sql: SOString; const Connection: IDBConnection = nil): IDBQuery;
    function Cell(const sql: SOString; const Connection: IDBConnection = nil): IDBQuery;
    procedure ExecuteImmediate(const Options: SOString); overload;
    function Execute(const Query: IDBQuery; const params: ISuperObject = nil): ISuperObject; overload;
    function Execute(const Query: IDBQuery; const params: array of const): ISuperObject; overload;
    function Execute(const Query: IDBQuery; const params: SOString): ISuperObject; overload;
    function Execute(const Query: IDBQuery; const params: Variant): ISuperObject; overload;
    procedure OnCommit(const proc: TProc);
    procedure OnRollback(const proc: TProc);
  end;

  IDBQuery = interface
  ['{A39B974A-96EA-4047-A57B-A2B3EBE7BABD}']
    function Execute(const params: ISuperObject = nil; const transaction: IDBTransaction = nil): ISuperObject; overload;
    function Execute(const params: array of const; const transaction: IDBTransaction = nil): ISuperObject; overload;
    function Execute(const params: SOString; const transaction: IDBTransaction = nil): ISuperObject; overload;
    function Execute(const params: Variant; const transaction: IDBTransaction = nil): ISuperObject; overload;
    function GetInputMeta: ISuperObject;
    function GetOutputMeta: ISuperObject;
  end;

  IDBBlob = interface
  ['{F478FC21-00B3-49C7-8531-85572AD3C98E}']
    function getData: TStream;
  end;

  IDBDateTime = interface
  ['{11B92F12-7E04-4442-A84E-9252FDAE2C37}']
    function AsDateTime: Double;
  end;

  // Abstact classes

  TDBConnection = class(TSuperObject, IDBConnection)
  protected
    procedure ExecuteImmediate(const Options: SOString); virtual;
    function Transaction(const Options: ISuperObject = nil): IDBTransaction; overload; virtual; abstract;
    function Transaction(const OtherConnections: array of IDBConnection; const Options: ISuperObject = nil): IDBTransaction; overload; virtual; abstract;
    function Transaction(const Options: SOString): IDBTransaction; overload; virtual;
    function Query(const Options: ISuperObject = nil): IDBQuery; overload; virtual;
    function Query(const Sql: SOString; options: TQueryOptions = []): IDBQuery; overload; virtual;
    function Singleton(const Sql: SOString): IDBQuery;
    function Table(const Sql: SOString): IDBQuery;
    function Row(const Sql: SOString): IDBQuery;
    function List(const Sql: SOString): IDBQuery;
    function Cell(const Sql: SOString): IDBQuery;
  end;

  TDBTransaction = class(TSuperObject, IDBTransaction)
  private
    FCommitEvent: TList<TProc>;
    FRollbackEvent: TList<TProc>;
  protected
    procedure TriggerCommitEvent;
    procedure TriggerRollbackEvent;
    procedure ExecuteImmediate(const Options: SOString); virtual; abstract;
    function Query(const Options: ISuperObject = nil; const Connection: IDBConnection = nil): IDBQuery; overload; virtual; abstract;
    function Query(const sql: SOString; options: TQueryOptions = []; const Connection: IDBConnection = nil): IDBQuery; overload; virtual;
    function Singleton(const sql: SOString; const Connection: IDBConnection = nil): IDBQuery;
    function Table(const sql: SOString; const Connection: IDBConnection = nil): IDBQuery;
    function Row(const sql: SOString; const Connection: IDBConnection = nil): IDBQuery;
    function List(const sql: SOString; const Connection: IDBConnection = nil): IDBQuery;
    function Cell(const sql: SOString; const Connection: IDBConnection = nil): IDBQuery;
    function Execute(const Query: IDBQuery; const params: ISuperObject = nil): ISuperObject; overload; virtual;
    function Execute(const Query: IDBQuery; const params: array of const): ISuperObject; overload; virtual;
    function Execute(const Query: IDBQuery; const params: SOString): ISuperObject; overload; virtual;
    function Execute(const Query: IDBQuery; const params: Variant): ISuperObject; overload; virtual;
    procedure OnCommit(const proc: TProc);
    procedure OnRollback(const proc: TProc);
  public
    constructor Create(jt: TSuperType = stObject); override;
    destructor Destroy; override;
  end;

  TDBQuery = class(TSuperObject, IDBQuery)
  protected
    function Execute(const params: ISuperObject = nil; const transaction: IDBTransaction = nil): ISuperObject; overload; virtual; abstract;
    function Execute(const params: array of const; const transaction: IDBTransaction = nil): ISuperObject; overload; virtual;
    function Execute(const params: SOString; const transaction: IDBTransaction = nil): ISuperObject; overload; virtual;
    function Execute(const params: Variant; const transaction: IDBTransaction = nil): ISuperObject; overload; virtual;
    function GetInputMeta: ISuperObject; virtual; abstract;
    function GetOutputMeta: ISuperObject; virtual; abstract;
  end;

  TDBBinary = class(TSuperObject, IDBBlob)
  private
    FStream: TPooledMemoryStream;
  public
    constructor Create(stream: TStream = nil); reintroduce; overload;
    constructor Create(const filename: string); reintroduce; overload;
    constructor Create(buffer: Pointer; len: Integer); reintroduce; overload;
    constructor CreateFromBase64(const base64: string);
    destructor Destroy; override;
    function Clone: ISuperObject; override;
    function Write(writer: TSuperWriter; format: boolean; escape: boolean; level: integer): Integer; override;
    function getData: TStream;

    function AsBoolean: Boolean; override; // true if length > 0
    function AsInteger: SuperInt; override; // stream length
  end;

  TDBDateTime = class(TSuperObject, IDBDateTime)
  protected
    function AsDateTime: Double;
  end;

  function blob(stream: TStream = nil): ISuperObject; overload;
  function blob(const filename: string): ISuperObject; overload;
  function blob(buffer: Pointer; len: Integer): ISuperObject; overload;

implementation

function DecodeValue(const s: SOString): ISuperObject; inline;
begin
  Result := TSuperObject.ParseString(PSOChar(s), False, False);
  if Result = nil then
    Result := TSuperObject.Create(s);
end;

function blob(stream: TStream = nil): ISuperObject; overload;
begin
  Result := TDBBinary.Create(stream);
end;

function blob(const filename: string): ISuperObject; overload;
begin
  Result := TDBBinary.Create(filename);
end;

function blob(buffer: Pointer; len: Integer): ISuperObject; overload;
begin
  Result := TDBBinary.Create(buffer, len);
end;

{ TDBConnection }

function TDBConnection.Query(const Options: ISuperObject): IDBQuery;
begin
  Result := Transaction.Query(Options);
end;

function TDBConnection.Cell(const Sql: SOString): IDBQuery;
begin
  Result := Query(sql, [qoSingleton, qoValue]);
end;

procedure TDBConnection.ExecuteImmediate(const Options: SOString);
begin
  Transaction.ExecuteImmediate(Options);
end;

function TDBConnection.List(const Sql: SOString): IDBQuery;
begin
  Result := Query(sql, [qoValue]);
end;

function TDBConnection.Query(const sql: SOString; options: TQueryOptions): IDBQuery;
begin
  Result := Transaction.Query(sql, options);
end;

function TDBConnection.Row(const Sql: SOString): IDBQuery;
begin
  Result := Query(sql, [qoSingleton, qoArray]);
end;

function TDBConnection.Singleton(const sql: SOString): IDBQuery;
begin
  Result := Query(sql, [qoSingleton]);
end;

function TDBConnection.Table(const Sql: SOString): IDBQuery;
begin
  Result := Query(sql, [qoArray]);
end;

function TDBConnection.Transaction(const Options: SOString): IDBTransaction;
begin
  Result := Transaction(TSuperObject.ParseString(PSOChar(Options), false));
end;


{ TDBContext }

procedure TDBTransaction.OnCommit(const proc: TProc);
begin
  FCommitEvent.Add(proc);
end;

procedure TDBTransaction.OnRollback(const proc: TProc);
begin
  FRollbackEvent.Add(proc);
end;

function TDBTransaction.Cell(const sql: SOString;
  const Connection: IDBConnection): IDBQuery;
begin
  Result := Query(sql, [qoSingleton, qoValue], Connection);
end;

constructor TDBTransaction.Create(jt: TSuperType);
begin
  inherited;
  FCommitEvent := TList<TProc>.Create;
  FRollbackEvent := TList<TProc>.Create;
end;

destructor TDBTransaction.Destroy;
begin
  FCommitEvent.Free;
  FRollbackEvent.Free;
  inherited;
end;

function TDBTransaction.Execute(const Query: IDBQuery;
  const params: Variant): ISuperObject;
begin
  Result := Query.Execute(so(params), Self);
end;

function TDBTransaction.List(const sql: SOString;
  const Connection: IDBConnection): IDBQuery;
begin
  Result := Query(sql, [qoValue], Connection);
end;

function TDBTransaction.Query(const sql: SOString; options: TQueryOptions; const Connection: IDBConnection): IDBQuery;
var
  obj: ISuperObject;
begin
  obj := SO(['sql', sql]);
  if qoSingleton in options then obj.B['singleton'] := True;
  if qoArray in options then obj.B['array'] := True;
  if qoValue in options then obj.B['value'] := True;

//  if qoNoCursor in options then obj.B['nocursor'] := True;
  Result := Query(obj, Connection);
end;

function TDBTransaction.Row(const sql: SOString;
  const Connection: IDBConnection): IDBQuery;
begin
  Result := Query(sql, [qoSingleton, qoArray], Connection);
end;

function TDBTransaction.Singleton(const sql: SOString;
  const Connection: IDBConnection): IDBQuery;
begin
  Result := Query(sql, [qoSingleton], Connection);
end;

function TDBTransaction.Table(const sql: SOString;
  const Connection: IDBConnection): IDBQuery;
begin
  Result := Query(sql, [qoArray], Connection);
end;

procedure TDBTransaction.TriggerCommitEvent;
var
  p: TProc;
begin
  for p in FCommitEvent do p();
end;

procedure TDBTransaction.TriggerRollbackEvent;
var
  p: TProc;
begin
  for p in FRollbackEvent do p();
end;

function TDBTransaction.Execute(const Query: IDBQuery;
  const params: SOString): ISuperObject;
begin
  Result := Query.Execute(DecodeValue(params), Self);
end;

function TDBTransaction.Execute(const Query: IDBQuery;
  const params: array of const): ISuperObject;
begin
  Result := Query.Execute(SA(params), Self);
end;

function TDBTransaction.Execute(const Query: IDBQuery;
  const params: ISuperObject = nil): ISuperObject;
begin
  Result := Query.Execute(params, Self);
end;

{ TDBCommand }

function TDBQuery.Execute(const params: Variant;
  const transaction: IDBTransaction): ISuperObject;
begin
  Result := Execute(SO(params), transaction);
end;

function TDBQuery.Execute(const params: SOString;
  const transaction: IDBTransaction): ISuperObject;
begin
  Result := Execute(DecodeValue(params), transaction);
end;

function TDBQuery.Execute(const params: array of const;
  const transaction: IDBTransaction): ISuperObject;
begin
  Result := Execute(SA(params), transaction);
end;

{ TDBBinary }

function TDBBinary.AsBoolean: Boolean;
begin
  Result := FStream.Size > 0;
end;

function TDBBinary.AsInteger: SuperInt;
begin
  Result := FStream.Size;
end;

function TDBBinary.Clone: ISuperObject;
var
  blob: TDBBinary;
begin
  blob := TDBBinary.Create;
  blob.FStream.LoadFromStream(FStream);
  Result := blob;
end;

constructor TDBBinary.Create(stream: TStream);
begin
  inherited Create('[BINARY]');
  FStream := TPooledMemoryStream.Create;
  if Stream <> nil then
    FStream.LoadFromStream(stream);
end;

constructor TDBBinary.Create(const filename: string);
begin
  inherited Create('[BINARY]');
  FStream := TPooledMemoryStream.Create;
  if filename <> '' then
    FStream.LoadFromFile(filename);
end;

constructor TDBBinary.Create(buffer: Pointer; len: Integer);
begin
  inherited Create('[BINARY]');
  FStream := TPooledMemoryStream.Create;
  if (buffer <> nil) and (len > 0) then
    FStream.Write(buffer^, len);
end;

constructor TDBBinary.CreateFromBase64(const base64: string);
begin
  inherited Create('[BINARY]');
  FStream := TPooledMemoryStream.Create;
  Base64ToStream(base64, FStream);
end;

destructor TDBBinary.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TDBBinary.getData: TStream;
begin
  Result := FStream;
end;

function TDBBinary.Write(writer: TSuperWriter; format: boolean; escape: boolean;
  level: integer): Integer;
const
  Base64Code: PSOChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  QUOTE: SOChar = '"';
  EQ2: PSOChar = '==';
  EQ4: PSOChar = 'A===';
var
  V: array[0..2] of byte;
  C: array[0..3] of SOChar;
begin
  FStream.Seek(0, soFromBeginning);
  Result := 0;
  inc(Result, writer.Append(@QUOTE, 1));
  while true do
    case FStream.Read(V, 3) of
    3: begin
         C[0] := Base64Code[(V[0] shr 2) and $3F];
         C[1] := Base64Code[((V[0] shl 4) and $3F) or V[1] shr 4];
         C[2] := Base64Code[((V[1] shl 2) and $3F) or V[2] shr 6];
         C[3] := Base64Code[V[2] and $3F];
         inc(Result, writer.Append(@C, 4));
       end;
    2: begin
         C[0] := Base64Code[(V[0] shr 2) and $3F];
         C[1] := Base64Code[((V[0] shl 4) and $3F) or V[1] shr 4];
         C[2] := Base64Code[((V[1] shl 2) and $3F) or 0    shr 6];
         inc(Result, writer.Append(@C, 3));
         inc(Result, writer.Append(EQ2, 1));
         Break;
       end;
    1: begin
         C[0] := Base64Code[(V[0] shr 2) and $3F];
         C[1] := Base64Code[((V[0] shl 4) and $3F) or 0 shr 4];
         inc(Result, writer.Append(@C, 2));
         inc(Result, writer.Append(EQ2, 2));
         Break;
       end;
    0: begin
         if FStream.Position = 0 then
           inc(Result, writer.Append(EQ4, 4));
         Break;
       end;
    end;
  inc(Result, writer.Append(@QUOTE, 1));
end;

{ TDBDateTime }

function TDBDateTime.AsDateTime: Double;
begin
  Result := JavaToDelphiDateTime(AsInteger);
end;

end.
