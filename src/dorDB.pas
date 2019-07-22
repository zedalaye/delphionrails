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
    function Connection: IDBConnection;
    function Size: Integer;
    procedure ClearPool;
    procedure Lock;
    procedure Unlock;
    function RefCount: Integer;
  end;

  IDBConnection = interface
  ['{843E105A-B8E0-42A9-AFA0-CF5AA843DB8B}']
    function Transaction(const Options: ISuperObject = nil): IDBTransaction; overload;
    function Transaction(const OtherConnections: array of IDBConnection; const Options: ISuperObject = nil): IDBTransaction; overload;
    function Transaction(const Options: string): IDBTransaction; overload;
    function Query(const Sql: string): IDBQuery; overload;
    procedure ExecuteImmediate(const Sql: string); overload;

    function Execute(const Query: IDBQuery; const Params: ISuperObject = nil; Options: TQueryOptions = []): ISuperObject; overload;
    function Execute(const Query: IDBQuery; const Params: array of const; Options: TQueryOptions = []): ISuperObject; overload;
    function Execute(const Sql: string; const Params: ISuperObject = nil; Options: TQueryOptions = []): ISuperObject; overload;
    function Execute(const Sql: string; const Params: array of const; Options: TQueryOptions = []): ISuperObject; overload;

    // single object

    function Singleton(const Query: IDBQuery; const Params: ISuperObject = nil): ISuperObject; overload;
    function Singleton(const Query: IDBQuery; const Params: array of const): ISuperObject; overload;
    function Singleton(const Sql: string; const Params: ISuperObject = nil): ISuperObject; overload;
    function Singleton(const Sql: string; const Params: array of const): ISuperObject; overload;

    // list of objects

    function List(const Query: IDBQuery; const Params: ISuperObject = nil): ISuperObject; overload;
    function List(const Query: IDBQuery; const Params: array of const): ISuperObject; overload;
    function List(const Sql: string; const Params: ISuperObject = nil): ISuperObject; overload;
    function List(const Sql: string; const Params: array of const): ISuperObject; overload;

    // array of something with no property names

    // table = array of array of values (array of rows)

    function Table(const Query: IDBQuery; const Params: ISuperObject = nil): ISuperObject; overload;
    function Table(const Query: IDBQuery; const Params: array of const): ISuperObject; overload;
    function Table(const Sql: string; const Params: ISuperObject = nil): ISuperObject; overload;
    function Table(const Sql: string; const Params: array of const): ISuperObject; overload;

    // row = array of values (array of cells)

    function Row(const Query: IDBQuery; const Params: ISuperObject = nil): ISuperObject; overload;
    function Row(const Query: IDBQuery; const Params: array of const): ISuperObject; overload;
    function Row(const Sql: string; const Params: ISuperObject = nil): ISuperObject; overload;
    function Row(const Sql: string; const Params: array of const): ISuperObject; overload;

    // cell = single value

    function Cell(const Query: IDBQuery; const Params: ISuperObject = nil): ISuperObject; overload;
    function Cell(const Query: IDBQuery; const Params: array of const): ISuperObject; overload;
    function Cell(const Sql: string; const Params: ISuperObject = nil): ISuperObject; overload;
    function Cell(const Sql: string; const Params: array of const): ISuperObject; overload;
  end;

  TExecuteResult = record
    IsResult: Boolean;
    HasAffectedRows: Boolean;
    Selected: Cardinal;
    Inserted: Cardinal;
    Updated: Cardinal;
    Deleted: Cardinal;
    function ItemResult: Boolean;
    function Changed: Cardinal;
    constructor Create(AIsResult, AHasAffectedRows: Boolean; ASelected, AInserted, AUpdated, ADeleted: Cardinal); overload;
    constructor Create(AIsResult, AHasAffectedRows: Boolean); overload;
  end;

  TExecuteCallback = reference to procedure(const item: ISuperObject; const result: TExecuteResult);

  IDBTransaction = interface
  ['{51992399-2D1A-47EF-9DB1-C5654325F41B}']
    function Query(const Sql: string; const Connection: IDBConnection = nil): IDBQuery;
    procedure ExecuteImmediate(const Sql: string); overload;

    function Execute(const Query: IDBQuery; const Params: ISuperObject = nil; Options: TQueryOptions = []; const callback: TExecuteCallback = nil): ISuperObject; overload;
    function Execute(const Query: IDBQuery; const Params: array of const; Options: TQueryOptions = []; const callback: TExecuteCallback = nil): ISuperObject; overload;
    function Execute(const Sql: string; const Params: ISuperObject = nil; Options: TQueryOptions = []; const callback: TExecuteCallback = nil): ISuperObject; overload;
    function Execute(const Sql: string; const Params: array of const; Options: TQueryOptions = []; const callback: TExecuteCallback = nil): ISuperObject; overload;

    // single object

    function Singleton(const Query: IDBQuery; const Params: ISuperObject = nil): ISuperObject; overload;
    function Singleton(const Query: IDBQuery; const Params: array of const): ISuperObject; overload;
    function Singleton(const Sql: string; const Params: ISuperObject = nil): ISuperObject; overload;
    function Singleton(const Sql: string; const Params: array of const): ISuperObject; overload;

    // list of objects

    function List(const Query: IDBQuery; const Params: ISuperObject = nil): ISuperObject; overload;
    function List(const Query: IDBQuery; const Params: array of const): ISuperObject; overload;
    function List(const Sql: string; const Params: ISuperObject = nil): ISuperObject; overload;
    function List(const Sql: string; const Params: array of const): ISuperObject; overload;

    // array of something with no property names

    // table = array of array of values (array of rows)

    function Table(const Query: IDBQuery; const Params: ISuperObject = nil): ISuperObject; overload;
    function Table(const Query: IDBQuery; const Params: array of const): ISuperObject; overload;
    function Table(const Sql: string; const Params: ISuperObject = nil): ISuperObject; overload;
    function Table(const Sql: string; const Params: array of const): ISuperObject; overload;

    // row = array of values (array of cells)

    function Row(const Query: IDBQuery; const Params: ISuperObject = nil): ISuperObject; overload;
    function Row(const Query: IDBQuery; const Params: array of const): ISuperObject; overload;
    function Row(const Sql: string; const Params: ISuperObject = nil): ISuperObject; overload;
    function Row(const Sql: string; const Params: array of const): ISuperObject; overload;

    // cell = single value

    function Cell(const Query: IDBQuery; const Params: ISuperObject = nil): ISuperObject; overload;
    function Cell(const Query: IDBQuery; const Params: array of const): ISuperObject; overload;
    function Cell(const Sql: string; const Params: ISuperObject = nil): ISuperObject; overload;
    function Cell(const Sql: string; const Params: array of const): ISuperObject; overload;

    procedure Rollback(value: boolean);
    procedure OnCommit(const proc: TProc);
    procedure OnRollback(const proc: TProc);
  end;

  IDBQuery = interface
  ['{A39B974A-96EA-4047-A57B-A2B3EBE7BABD}']
    function Execute(const Params: ISuperObject = nil; Options: TQueryOptions = []; const Transaction: IDBTransaction = nil): ISuperObject; overload;
    function Execute(const Params: array of const; Options: TQueryOptions = []; const Transaction: IDBTransaction = nil): ISuperObject; overload;

    // execute with callback

    procedure Execute(const Params: ISuperObject; const callback: TExecuteCallback; Options: TQueryOptions = []; const Transaction: IDBTransaction = nil); overload;
    procedure Execute(const Params: array of const; const callback: TExecuteCallback; Options: TQueryOptions = []; const Transaction: IDBTransaction = nil); overload;

    // single object

    function Singleton(const Params: ISuperObject = nil; const Transaction: IDBTransaction = nil): ISuperObject; overload;
    function Singleton(const Params: array of const; const Transaction: IDBTransaction = nil): ISuperObject; overload;

    // list of objects

    function List(const Params: ISuperObject = nil; const Transaction: IDBTransaction = nil): ISuperObject; overload;
    function List(const Params: array of const; const Transaction: IDBTransaction = nil): ISuperObject; overload;

    // array of something with no property names

    // table = array of array of values (array of rows)

    function Table(const Params: ISuperObject = nil; const Transaction: IDBTransaction = nil): ISuperObject; overload;
    function Table(const Params: array of const; const Transaction: IDBTransaction = nil): ISuperObject; overload;

    // row = array of values (array of cells)

    function Row(const Params: ISuperObject = nil; const Transaction: IDBTransaction = nil): ISuperObject; overload;
    function Row(const Params: array of const; const Transaction: IDBTransaction = nil): ISuperObject; overload;

    // cell = single value

    function Cell(const Params: ISuperObject = nil; const Transaction: IDBTransaction = nil): ISuperObject; overload;
    function Cell(const Params: array of const; const Transaction: IDBTransaction = nil): ISuperObject; overload;

    function GetInputMeta: ISuperObject;
    function GetOutputMeta(byindex: Boolean): ISuperObject;
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

  TDBConnection = class(TInterfacedObject, IDBConnection)
  protected
    function Transaction(const Options: ISuperObject = nil): IDBTransaction; overload; virtual; abstract;
    function Transaction(const OtherConnections: array of IDBConnection; const Options: ISuperObject = nil): IDBTransaction; overload; virtual; abstract;
    function Transaction(const Options: string): IDBTransaction; overload; virtual;
    function Query(const Sql: string): IDBQuery; virtual;
    procedure ExecuteImmediate(const Sql: string); virtual;

    function Execute(const Query: IDBQuery; const Params: ISuperObject = nil; Options: TQueryOptions = []): ISuperObject; overload; virtual;
    function Execute(const Query: IDBQuery; const Params: array of const; Options: TQueryOptions = []): ISuperObject; overload; virtual;
    function Execute(const Sql: string; const Params: ISuperObject = nil; Options: TQueryOptions = []): ISuperObject; overload; virtual;
    function Execute(const Sql: string; const Params: array of const; Options: TQueryOptions = []): ISuperObject; overload; virtual;

    function Singleton(const Query: IDBQuery; const Params: ISuperObject = nil): ISuperObject; overload; virtual;
    function Singleton(const Query: IDBQuery; const Params: array of const): ISuperObject; overload; virtual;
    function Singleton(const Sql: string; const Params: ISuperObject = nil): ISuperObject; overload; virtual;
    function Singleton(const Sql: string; const Params: array of const): ISuperObject; overload; virtual;

    function List(const Query: IDBQuery; const Params: ISuperObject = nil): ISuperObject; overload; virtual;
    function List(const Query: IDBQuery; const Params: array of const): ISuperObject; overload; virtual;
    function List(const Sql: string; const Params: ISuperObject = nil): ISuperObject; overload; virtual;
    function List(const Sql: string; const Params: array of const): ISuperObject; overload; virtual;

    function Table(const Query: IDBQuery; const Params: ISuperObject = nil): ISuperObject; overload; virtual;
    function Table(const Query: IDBQuery; const Params: array of const): ISuperObject; overload; virtual;
    function Table(const Sql: string; const Params: ISuperObject = nil): ISuperObject; overload; virtual;
    function Table(const Sql: string; const Params: array of const): ISuperObject; overload; virtual;

    function Row(const Query: IDBQuery; const Params: ISuperObject = nil): ISuperObject; overload; virtual;
    function Row(const Query: IDBQuery; const Params: array of const): ISuperObject; overload; virtual;
    function Row(const Sql: string; const Params: ISuperObject = nil): ISuperObject; overload; virtual;
    function Row(const Sql: string; const Params: array of const): ISuperObject; overload; virtual;

    function Cell(const Query: IDBQuery; const Params: ISuperObject = nil): ISuperObject; overload; virtual;
    function Cell(const Query: IDBQuery; const Params: array of const): ISuperObject; overload; virtual;
    function Cell(const Sql: string; const Params: ISuperObject = nil): ISuperObject; overload; virtual;
    function Cell(const Sql: string; const Params: array of const): ISuperObject; overload; virtual;
  end;

  TDBTransaction = class(TInterfacedObject, IDBTransaction)
  private
    FCommitEvent: TList<TProc>;
    FRollbackEvent: TList<TProc>;
  protected
    procedure TriggerCommitEvent;
    procedure TriggerRollbackEvent;
    function Query(const Sql: string; const Connection: IDBConnection = nil): IDBQuery; virtual; abstract;
    procedure ExecuteImmediate(const Sql: string); virtual; abstract;

    function Execute(const Query: IDBQuery; const Params: ISuperObject = nil; Options: TQueryOptions = []; const Callback: TExecuteCallback = nil): ISuperObject; overload; virtual;
    function Execute(const Query: IDBQuery; const Params: array of const; Options: TQueryOptions = []; const Callback: TExecuteCallback = nil): ISuperObject; overload; virtual;
    function Execute(const Sql: string; const Params: ISuperObject = nil; Options: TQueryOptions = []; const Callback: TExecuteCallback = nil): ISuperObject; overload; virtual;
    function Execute(const Sql: string; const Params: array of const; Options: TQueryOptions = []; const Callback: TExecuteCallback = nil): ISuperObject; overload; virtual;

    function Singleton(const Query: IDBQuery; const Params: ISuperObject = nil): ISuperObject; overload; virtual;
    function Singleton(const Query: IDBQuery; const Params: array of const): ISuperObject; overload; virtual;
    function Singleton(const Sql: string; const Params: ISuperObject = nil): ISuperObject; overload; virtual;
    function Singleton(const Sql: string; const Params: array of const): ISuperObject; overload; virtual;

    function List(const Query: IDBQuery; const Params: ISuperObject = nil): ISuperObject; overload; virtual;
    function List(const Query: IDBQuery; const Params: array of const): ISuperObject; overload; virtual;
    function List(const Sql: string; const Params: ISuperObject = nil): ISuperObject; overload; virtual;
    function List(const Sql: string; const Params: array of const): ISuperObject; overload; virtual;

    function Table(const Query: IDBQuery; const Params: ISuperObject = nil): ISuperObject; overload; virtual;
    function Table(const Query: IDBQuery; const Params: array of const): ISuperObject; overload; virtual;
    function Table(const Sql: string; const Params: ISuperObject = nil): ISuperObject; overload; virtual;
    function Table(const Sql: string; const Params: array of const): ISuperObject; overload; virtual;

    function Row(const Query: IDBQuery; const Params: ISuperObject = nil): ISuperObject; overload; virtual;
    function Row(const Query: IDBQuery; const Params: array of const): ISuperObject; overload; virtual;
    function Row(const Sql: string; const Params: ISuperObject = nil): ISuperObject; overload; virtual;
    function Row(const Sql: string; const Params: array of const): ISuperObject; overload; virtual;

    function Cell(const Query: IDBQuery; const Params: ISuperObject = nil): ISuperObject; overload; virtual;
    function Cell(const Query: IDBQuery; const Params: array of const): ISuperObject; overload; virtual;
    function Cell(const Sql: string; const Params: ISuperObject = nil): ISuperObject; overload; virtual;
    function Cell(const Sql: string; const Params: array of const): ISuperObject; overload; virtual;

    procedure Rollback(value: boolean); virtual; abstract;
    procedure OnCommit(const proc: TProc);
    procedure OnRollback(const proc: TProc);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TDBQuery = class(TInterfacedObject, IDBQuery)
  protected
    function Execute(const Params: ISuperObject = nil; Options: TQueryOptions = []; const Transaction: IDBTransaction = nil): ISuperObject; overload; virtual; abstract;
    function Execute(const Params: array of const; Options: TQueryOptions = []; const Transaction: IDBTransaction = nil): ISuperObject; overload; virtual;

    procedure Execute(const Params: ISuperObject; const callback: TExecuteCallback; Options: TQueryOptions = []; const Transaction: IDBTransaction = nil); overload; virtual; abstract;
    procedure Execute(const Params: array of const; const callback: TExecuteCallback; Options: TQueryOptions = []; const Transaction: IDBTransaction = nil); overload; virtual;

    function Singleton(const Params: ISuperObject = nil; const Transaction: IDBTransaction = nil): ISuperObject; overload; virtual;
    function Singleton(const Params: array of const; const Transaction: IDBTransaction = nil): ISuperObject; overload; virtual;

    function List(const Params: ISuperObject = nil; const Transaction: IDBTransaction = nil): ISuperObject; overload; virtual;
    function List(const Params: array of const; const Transaction: IDBTransaction = nil): ISuperObject; overload; virtual;

    function Table(const Params: ISuperObject = nil; const Transaction: IDBTransaction = nil): ISuperObject; overload; virtual;
    function Table(const Params: array of const; const Transaction: IDBTransaction = nil): ISuperObject; overload; virtual;

    function Row(const Params: ISuperObject = nil; const Transaction: IDBTransaction = nil): ISuperObject; overload; virtual;
    function Row(const Params: array of const; const Transaction: IDBTransaction = nil): ISuperObject; overload; virtual;

    function Cell(const Params: ISuperObject = nil; const Transaction: IDBTransaction = nil): ISuperObject; overload; virtual;
    function Cell(const Params: array of const; const Transaction: IDBTransaction = nil): ISuperObject; overload; virtual;

    function GetInputMeta: ISuperObject; virtual; abstract;
    function GetOutputMeta(byindex: Boolean): ISuperObject; virtual; abstract;
  end;

  TDBBinary = class(TSuperObject, IDBBlob)
  private
    FStream: TPooledMemoryStream;
  public
    constructor Create(stream: TStream = nil); reintroduce; overload;
    constructor Create(const stream: IStreamPersist); reintroduce; overload;
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

function DecodeValue(const s: string): ISuperObject; inline;
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

function TDBConnection.Query(const Sql: string): IDBQuery;
begin
  Result := Transaction.Query(Sql);
end;

procedure TDBConnection.ExecuteImmediate(const Sql: string);
begin
  Transaction.ExecuteImmediate(Sql);
end;

function TDBConnection.Transaction(const Options: string): IDBTransaction;
begin
  Result := Transaction(TSuperObject.ParseString(PSOChar(Options), false));
end;

function TDBConnection.Cell(const Query: IDBQuery;
  const Params: ISuperObject): ISuperObject;
begin
  Result := Query.Cell(Params);
end;

function TDBConnection.Cell(const Query: IDBQuery;
  const Params: array of const): ISuperObject;
begin
  Result := Query.Cell(Params);
end;

function TDBConnection.Row(const Sql: string;
  const Params: ISuperObject): ISuperObject;
begin
  Result := Row(Query(Sql), Params);
end;

function TDBConnection.Row(const Sql: string;
  const Params: array of const): ISuperObject;
begin
  Result := Row(Query(Sql), Params);
end;

function TDBConnection.Row(const Query: IDBQuery;
  const Params: array of const): ISuperObject;
begin
  Result := Query.Row(Params);
end;

function TDBConnection.Row(const Query: IDBQuery;
  const Params: ISuperObject): ISuperObject;
begin
  Result := Query.Row(Params);
end;

function TDBConnection.Singleton(const Query: IDBQuery;
  const Params: ISuperObject): ISuperObject;
begin
  Result := Query.Singleton(Params);
end;

function TDBConnection.Singleton(const Query: IDBQuery;
  const Params: array of const): ISuperObject;
begin
  Result := Query.Singleton(Params);
end;

function TDBConnection.Cell(const Sql: string;
  const Params: ISuperObject): ISuperObject;
begin
  Result := Cell(Query(Sql), Params);
end;

function TDBConnection.Cell(const Sql: string;
  const Params: array of const): ISuperObject;
begin
  Result := Cell(Query(Sql), Params);
end;

function TDBConnection.Execute(const Sql: string; const Params: ISuperObject;
  Options: TQueryOptions): ISuperObject;
begin
  Result := Execute(Query(Sql), Params, Options);
end;

function TDBConnection.Execute(const Sql: string; const Params: array of const;
  Options: TQueryOptions): ISuperObject;
begin
  Result := Execute(Query(Sql), Params, Options);
end;

function TDBConnection.Execute(const Query: IDBQuery;
  const Params: array of const; Options: TQueryOptions): ISuperObject;
begin
  Result := Query.Execute(params, Options);
end;

function TDBConnection.Execute(const Query: IDBQuery;
  const Params: ISuperObject; Options: TQueryOptions): ISuperObject;
begin
  Result := Query.Execute(params, Options);
end;

function TDBConnection.List(const Query: IDBQuery;
  const Params: array of const): ISuperObject;
begin
  Result := Query.List(Params);
end;

function TDBConnection.List(const Query: IDBQuery;
  const Params: ISuperObject): ISuperObject;
begin
  Result := Query.List(Params);
end;

function TDBConnection.Table(const Query: IDBQuery;
  const Params: ISuperObject): ISuperObject;
begin
  Result := Query.Table(Params);
end;

function TDBConnection.Table(const Query: IDBQuery;
  const Params: array of const): ISuperObject;
begin
  Result := Query.Table(Params);
end;

function TDBConnection.List(const Sql: string;
  const Params: array of const): ISuperObject;
begin
  Result := List(Query(Sql), Params);
end;

function TDBConnection.List(const Sql: string;
  const Params: ISuperObject): ISuperObject;
begin
  Result := List(Query(Sql), Params);
end;

function TDBConnection.Singleton(const Sql: string;
  const Params: ISuperObject): ISuperObject;
begin
  Result := Singleton(Query(Sql), Params);
end;

function TDBConnection.Singleton(const Sql: string;
  const Params: array of const): ISuperObject;
begin
  Result := Singleton(Query(Sql), Params);
end;

function TDBConnection.Table(const Sql: string;
  const Params: array of const): ISuperObject;
begin
  Result := Table(Query(Sql), Params);
end;

function TDBConnection.Table(const Sql: string;
  const Params: ISuperObject): ISuperObject;
begin
  Result := Table(Query(Sql), Params);
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

function TDBTransaction.Cell(const Query: IDBQuery;
  const Params: ISuperObject): ISuperObject;
begin
  Result := Query.Cell(Params, Self);
end;

function TDBTransaction.Cell(const Query: IDBQuery;
  const Params: array of const): ISuperObject;
begin
  Result := Query.Cell(Params, Self);
end;

function TDBTransaction.Row(const Sql: string;
  const Params: ISuperObject): ISuperObject;
begin
  Result := Row(Query(Sql), Params);
end;

function TDBTransaction.Row(const Sql: string;
  const Params: array of const): ISuperObject;
begin
  Result := Row(Query(Sql), Params);
end;

function TDBTransaction.Row(const Query: IDBQuery;
  const Params: array of const): ISuperObject;
begin
  Result := Query.Row(Params, Self);
end;

function TDBTransaction.Row(const Query: IDBQuery;
  const Params: ISuperObject): ISuperObject;
begin
  Result := Query.Row(Params, Self);
end;

function TDBTransaction.Singleton(const Query: IDBQuery;
  const Params: ISuperObject): ISuperObject;
begin
  Result := Query.Singleton(Params, Self);
end;

function TDBTransaction.Singleton(const Query: IDBQuery;
  const Params: array of const): ISuperObject;
begin
  Result := Query.Singleton(Params, Self);
end;

function TDBTransaction.Cell(const Sql: string;
  const Params: ISuperObject): ISuperObject;
begin
  Result := Cell(Query(Sql), Params);
end;

function TDBTransaction.Cell(const Sql: string;
  const Params: array of const): ISuperObject;
begin
  Result := Cell(Query(Sql), Params);
end;

constructor TDBTransaction.Create;
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

function TDBTransaction.Execute(const Sql: string; const Params: ISuperObject;
  Options: TQueryOptions; const Callback: TExecuteCallback): ISuperObject;
begin
  Result := Execute(Query(Sql), Params, Options, Callback);
end;

function TDBTransaction.Execute(const Sql: string; const Params: array of const;
  Options: TQueryOptions; const Callback: TExecuteCallback): ISuperObject;
begin
  Result := Execute(Query(Sql), Params, Options, Callback);
end;

function TDBTransaction.Execute(const Query: IDBQuery;
  const Params: array of const; Options: TQueryOptions;
  const Callback: TExecuteCallback): ISuperObject;
begin
  if Assigned(Callback) then
  begin
    Query.Execute(Params, Callback, Options, Self);
    Result := nil;
  end
  else
    Result := Query.Execute(Params, Options, Self);
end;

function TDBTransaction.Execute(const Query: IDBQuery;
  const Params: ISuperObject; Options: TQueryOptions;
  const Callback: TExecuteCallback): ISuperObject;
begin
  if Assigned(Callback) then
  begin
    Query.Execute(Params, Callback, Options, Self);
    Result := nil;
  end
  else
    Result := Query.Execute(Params, Options, Self);
end;

function TDBTransaction.List(const Query: IDBQuery;
  const Params: array of const): ISuperObject;
begin
  Result := Query.List(Params, Self);
end;

function TDBTransaction.List(const Query: IDBQuery;
  const Params: ISuperObject): ISuperObject;
begin
  Result := Query.List(Params, Self);
end;

function TDBTransaction.Table(const Query: IDBQuery;
  const Params: ISuperObject): ISuperObject;
begin
  Result := Query.Table(Params, Self);
end;

function TDBTransaction.Table(const Query: IDBQuery;
  const Params: array of const): ISuperObject;
begin
  Result := Query.Table(Params, Self);
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

function TDBTransaction.List(const Sql: string;
  const Params: array of const): ISuperObject;
begin
  Result := List(Query(Sql), Params);
end;

function TDBTransaction.List(const Sql: string;
  const Params: ISuperObject): ISuperObject;
begin
  Result := List(Query(Sql), Params);
end;

function TDBTransaction.Singleton(const Sql: string;
  const Params: ISuperObject): ISuperObject;
begin
  Result := Singleton(Query(Sql), Params);
end;

function TDBTransaction.Singleton(const Sql: string;
  const Params: array of const): ISuperObject;
begin
  Result := Singleton(Query(Sql), Params);
end;

function TDBTransaction.Table(const Sql: string;
  const Params: array of const): ISuperObject;
begin
  Result := Table(Query(Sql), Params);
end;

function TDBTransaction.Table(const Sql: string;
  const Params: ISuperObject): ISuperObject;
begin
  Result := Table(Query(Sql), Params);
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

constructor TDBBinary.Create(const stream: IStreamPersist);
begin
  inherited Create('[BINARY]');
  FStream := TPooledMemoryStream.Create;
  if Stream <> nil then
    stream.SaveToStream(FStream);
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

{ TDBQuery }

function TDBQuery.Cell(const Params: ISuperObject;
  const Transaction: IDBTransaction): ISuperObject;
begin
  Result := Execute(Params, [qoSingleton, qoValue], Transaction);
end;

function TDBQuery.Cell(const Params: array of const;
  const Transaction: IDBTransaction): ISuperObject;
begin
  Result := Execute(Params, [qoSingleton, qoValue], Transaction);
end;

procedure TDBQuery.Execute(const Params: array of const;
  const callback: TExecuteCallback; Options: TQueryOptions;
  const Transaction: IDBTransaction);
begin
  Execute(SA(Params), callback, Options, Transaction);
end;

function TDBQuery.Execute(const Params: array of const; Options: TQueryOptions;
  const Transaction: IDBTransaction): ISuperObject;
begin
  Result := Execute(SA(Params), Options, Transaction);
end;

function TDBQuery.List(const Params: ISuperObject;
  const Transaction: IDBTransaction): ISuperObject;
begin
  Result := Execute(Params, [qoValue], Transaction);
end;

function TDBQuery.List(const Params: array of const;
  const Transaction: IDBTransaction): ISuperObject;
begin
  Result := Execute(Params, [qoValue], Transaction);
end;

function TDBQuery.Row(const Params: ISuperObject;
  const Transaction: IDBTransaction): ISuperObject;
begin
  Result := Execute(Params, [qoSingleton, qoArray], Transaction);
end;

function TDBQuery.Row(const Params: array of const;
  const Transaction: IDBTransaction): ISuperObject;
begin
  Result := Execute(Params, [qoSingleton, qoArray], Transaction);
end;

function TDBQuery.Singleton(const Params: ISuperObject;
  const Transaction: IDBTransaction): ISuperObject;
begin
  Result := Execute(Params, [qoSingleton], Transaction);
end;

function TDBQuery.Singleton(const Params: array of const;
  const Transaction: IDBTransaction): ISuperObject;
begin
  Result := Execute(Params, [qoSingleton], Transaction);
end;

function TDBQuery.Table(const Params: array of const;
  const Transaction: IDBTransaction): ISuperObject;
begin
  Result := Execute(Params, [qoArray], Transaction);
end;

function TDBQuery.Table(const Params: ISuperObject;
  const Transaction: IDBTransaction): ISuperObject;
begin
  Result := Execute(Params, [qoArray], Transaction);
end;

{ TExecuteResult }

function TExecuteResult.Changed: Cardinal;
begin
  Result := Self.Inserted + Self.Updated + Self.Deleted;
end;

constructor TExecuteResult.Create(AIsResult, AHasAffectedRows: Boolean;
  ASelected, AInserted, AUpdated, ADeleted: Cardinal);
begin
  Self.IsResult := AIsResult;
  Self.HasAffectedRows := AHasAffectedRows;
  Self.Selected := ASelected;
  Self.Inserted := AInserted;
  Self.Updated := AUpdated;
  Self.Deleted := ADeleted;
end;

constructor TExecuteResult.Create(AIsResult, AHasAffectedRows: Boolean);
begin
  Self.IsResult := AIsResult;
  Self.HasAffectedRows := AHasAffectedRows;
  Self.Selected := 0;
  Self.Inserted := 0;
  Self.Updated := 0;
  Self.Deleted := 0;
end;

function TExecuteResult.ItemResult: Boolean;
begin
  Result := (not IsResult) or (not HasAffectedRows)
end;

end.
