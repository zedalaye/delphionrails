unit dorDuckDB;

(*
  DuckDB connector for the dorDB interfaces (IDBConnectionPool, IDBConnection,
  IDBTransaction, IDBQuery).

  Targets the official DuckDB C API (duckdb.dll / libduckdb.so, version >= 1.0)
  available at https://duckdb.org.

  Usage:

    var
      cnx: IDBConnection;
      rows, row: ISuperObject;
    begin
      // file database ('' or ':memory:' for an in-memory database)
      cnx := TDBDuckDBConnection.Create('data.duckdb');
      cnx.ExecuteImmediate('CREATE TABLE IF NOT EXISTS t(id INTEGER, name VARCHAR)');

      // positional parameters
      cnx.Execute('INSERT INTO t VALUES (?, ?)', [1, 'hello']);

      // named parameters use the $name syntax, bound from object keys
      rows := cnx.List('SELECT * FROM t WHERE id >= $min', SO(['min', 1]));
      for row in rows do
        WriteLn(row.S['name']);
    end;

  A DuckDB database file can only be opened once per process: use
  TDBDuckDBConnectionPool to share the database instance between several
  connections (one connection per thread).

  Bulk insertion uses the DuckDB Appender (orders of magnitude faster than
  prepared INSERTs), typically to fill a staging table then export it with
  COPY ... TO '....parquet':

    var
      cnx: TDBDuckDBConnection;
      app: IDuckDBAppender;
    begin
      cnx := TDBDuckDBConnection.Create(':memory:');
      cnx.ExecuteImmediate('CREATE TABLE faits(magasin VARCHAR, caisse INTEGER, ' +
        'ts TIMESTAMP, montant DECIMAL(18,4))');
      app := cnx.Appender('faits');
      app.AppendString('M042');
      app.AppendInt(3);
      app.AppendDateTime(Now);
      app.AppendCurrency(19.99); // exact, no float involved
      app.EndRow;
      // ... more rows ...
      app.Close; // flushes and surfaces constraint violations
      app := nil;
      cnx.ExecuteImmediate('COPY (FROM faits) TO ''part-001.parquet'' (FORMAT PARQUET)');
    end;

  Values appended in a type different from the column type are implicitly cast
  by DuckDB (requires duckdb >= 1.1); AppendCurrency relies on this to reach
  DECIMAL columns exactly (text of the fixed point value, no float involved).
*)

interface

uses
  Classes, SysUtils, SyncObjs, Generics.Collections,
  supertypes, superobject,
  dorDB;

type
  EDuckDBError = class(Exception);

  TDuckDBDatabase = type Pointer;
  TDuckDBConnectionHandle = type Pointer;
  TDuckDBPreparedStatement = type Pointer;
  TDuckDBConfig = type Pointer;

  TDuckDBResult = record
    deprecated_column_count: UInt64;
    deprecated_row_count: UInt64;
    deprecated_rows_changed: UInt64;
    deprecated_columns: Pointer;
    deprecated_error_message: PAnsiChar;
    internal_data: Pointer;
  end;
  PDuckDBResult = ^TDuckDBResult;

  TDuckDBBlob = record
    data: Pointer;
    size: UInt64;
  end;

  TDuckDBHugeInt = record
    lower: UInt64;
    upper: Int64;
  end;

  TDuckDBDecimal = record
    width: Byte;
    scale: Byte;
    value: TDuckDBHugeInt;
  end;

  TDuckDBAppenderHandle = type Pointer;

  // Bulk row insertion into a table (DuckDB Appender API). Append the columns
  // of the row in order, then EndRow; Close (or the release of the interface)
  // flushes the pending rows. Errors raise EDuckDBError with the DuckDB
  // message; the release never raises, so call Close explicitly if you need
  // to catch constraint violations.
  IDuckDBAppender = interface
    ['{E3B7C9A1-2F4D-4B86-9A0E-5C1D8F7A2B64}']
    procedure AppendNull;
    procedure AppendBoolean(Value: Boolean);
    procedure AppendInt(Value: Int64);
    procedure AppendDouble(Value: Double);
    // exact: the fixed point value is transmitted as text, never as a float
    procedure AppendCurrency(const Value: Currency);
    procedure AppendString(const Value: string);
    procedure AppendBlob(const Value: TBytes); overload;
    procedure AppendBlob(Data: Pointer; Len: Integer); overload;
    procedure AppendBlob(Stream: TStream); overload;
    procedure AppendDateTime(Value: TDateTime);         // TIMESTAMP column
    procedure AppendDate(Value: TDateTime);             // DATE column
    procedure AppendTime(Value: TDateTime);             // TIME column
    procedure AppendTimestampMillis(JavaMillis: Int64); // TIMESTAMP column, java milliseconds (TDBDateTime convention)
    // dispatch on the superobject type (stNull, stBoolean, stInt, stDouble,
    // stCurrency, stString, IDBBlob, IDBDateTime; stObject/stArray as JSON)
    procedure Append(const Value: ISuperObject);
    procedure AppendRow(const Row: ISuperObject); overload;   // stArray: Append each item + EndRow
    procedure AppendRow(const Row: array of const); overload;
    procedure EndRow;
    procedure Flush;
    procedure Close;
  end;

  // A DuckDB database must be opened once per process; connections attach to
  // this shared instance. The holder keeps the handle alive as long as a pool
  // or a connection references it.
  IDuckDBDatabaseHolder = interface
    ['{9C63A1D4-5B7E-4A0F-8D2C-6E1F0B3A7D52}']
    function Handle: TDuckDBDatabase;
  end;

  TDBDuckDBConnection = class;

  TDBDuckDBConnectionPool = class(TInterfacedObject, IDBConnectionPool)
  private
    FCriticalSection: TCriticalSection;
    FMax: Integer;
    FDatabase: IDuckDBDatabaseHolder;
    FPool: TList<IDBConnection>;
  protected
    function Connection: IDBConnection;
    function Size: Integer;
    procedure ClearPool;
    procedure Lock;
    procedure Unlock;
  public
    constructor Create(Max: Integer; const Database: string;
      const Config: ISuperObject = nil); reintroduce;
    destructor Destroy; override;

    function RefCount: Integer;
  end;

  TDBDuckDBConnection = class(TDBConnection)
  private
    FDatabase: IDuckDBDatabaseHolder;
    FConHandle: TDuckDBConnectionHandle;
    procedure RawExecute(const Sql: string);
  protected
    function Transaction(const Options: ISuperObject = nil): IDBTransaction; overload; override;
    function Transaction(const OtherConnections: array of IDBConnection;
      const Options: ISuperObject = nil): IDBTransaction; overload; override;
    function Query(const Sql: string): IDBQuery; override;
    procedure ExecuteImmediate(const Sql: string); override;
  public
    // Config options are passed to duckdb_set_config, eg:
    //   SO(['access_mode', 'READ_ONLY', 'threads', 4])
    constructor Create(const Database: string; const Config: ISuperObject = nil); overload; virtual;
    constructor Create(const Database: IDuckDBDatabaseHolder); overload; virtual;
    destructor Destroy; override;

    function Appender(const Table: string; const Schema: string = ''): IDuckDBAppender;
  end;

  TDBDuckDBTransaction = class(TDBTransaction)
  private
    FConnection: IDBConnection;
    FDbConnection: TDBDuckDBConnection;
    FRollback: Boolean;
  protected
    procedure ExecuteImmediate(const Sql: string); override;
    function Query(const Sql: string; const Connection: IDBConnection = nil): IDBQuery; override;
    procedure Rollback(value: boolean); override;
    function GetConnection: IDBConnection; override;
  public
    constructor Create(const Connection: TDBDuckDBConnection;
      const Options: ISuperObject); reintroduce;
    destructor Destroy; override;
  end;

  TDBDuckDBQuery = class(TDBQuery)
  private
    FConnection: IDBConnection;
    FDbConnection: TDBDuckDBConnection;
    FStHandle: TDuckDBPreparedStatement;
    FSql: string;
  protected
    function Execute(const Params: ISuperObject; Options: TQueryOptions;
      const Transaction: IDBTransaction): ISuperObject; overload; override;

    procedure Execute(const Params: ISuperObject; const callback: TExecuteCallback;
      Options: TQueryOptions; const Transaction: IDBTransaction); overload; override;

    function GetInputMeta: ISuperObject; override;
    function GetOutputMeta(byindex: Boolean): ISuperObject; override;
  public
    constructor Create(const Connection: TDBDuckDBConnection;
      const Trans: TDBDuckDBTransaction; const Sql: string); reintroduce;
    destructor Destroy; override;
  end;

  function DuckDBVersion: string;

implementation

const
{$IFDEF MSWINDOWS}
  DuckDBLib = 'duckdb.dll';
{$ELSE}
  {$IFDEF MACOS}
  DuckDBLib = 'libduckdb.dylib';
  {$ELSE}
  DuckDBLib = 'libduckdb.so';
  {$ENDIF}
{$ENDIF}

  DuckDBSuccess = 0;
  DuckDBError   = 1;

  DUCKDB_TYPE_INVALID      = 0;
  DUCKDB_TYPE_BOOLEAN      = 1;
  DUCKDB_TYPE_TINYINT      = 2;
  DUCKDB_TYPE_SMALLINT     = 3;
  DUCKDB_TYPE_INTEGER      = 4;
  DUCKDB_TYPE_BIGINT       = 5;
  DUCKDB_TYPE_UTINYINT     = 6;
  DUCKDB_TYPE_USMALLINT    = 7;
  DUCKDB_TYPE_UINTEGER     = 8;
  DUCKDB_TYPE_UBIGINT      = 9;
  DUCKDB_TYPE_FLOAT        = 10;
  DUCKDB_TYPE_DOUBLE       = 11;
  DUCKDB_TYPE_TIMESTAMP    = 12;
  DUCKDB_TYPE_DATE         = 13;
  DUCKDB_TYPE_TIME         = 14;
  DUCKDB_TYPE_INTERVAL     = 15;
  DUCKDB_TYPE_HUGEINT      = 16;
  DUCKDB_TYPE_VARCHAR      = 17;
  DUCKDB_TYPE_BLOB         = 18;
  DUCKDB_TYPE_DECIMAL      = 19;
  DUCKDB_TYPE_TIMESTAMP_S  = 20;
  DUCKDB_TYPE_TIMESTAMP_MS = 21;
  DUCKDB_TYPE_TIMESTAMP_NS = 22;
  DUCKDB_TYPE_ENUM         = 23;
  DUCKDB_TYPE_LIST         = 24;
  DUCKDB_TYPE_STRUCT       = 25;
  DUCKDB_TYPE_MAP          = 26;
  DUCKDB_TYPE_UUID         = 27;
  DUCKDB_TYPE_UNION        = 28;
  DUCKDB_TYPE_BIT          = 29;
  DUCKDB_TYPE_TIME_TZ      = 30;
  DUCKDB_TYPE_TIMESTAMP_TZ = 31;
  DUCKDB_TYPE_UHUGEINT     = 32;
  DUCKDB_TYPE_ARRAY        = 33;

  DUCKDB_RESULT_TYPE_INVALID      = 0;
  DUCKDB_RESULT_TYPE_CHANGED_ROWS = 1;
  DUCKDB_RESULT_TYPE_NOTHING      = 2;
  DUCKDB_RESULT_TYPE_QUERY_RESULT = 3;

  DUCKDB_STATEMENT_TYPE_INVALID = 0;
  DUCKDB_STATEMENT_TYPE_SELECT  = 1;
  DUCKDB_STATEMENT_TYPE_INSERT  = 2;
  DUCKDB_STATEMENT_TYPE_UPDATE  = 3;
  DUCKDB_STATEMENT_TYPE_EXPLAIN = 4;
  DUCKDB_STATEMENT_TYPE_DELETE  = 5;
  DUCKDB_STATEMENT_TYPE_COPY    = 11;

function duckdb_open(path: PAnsiChar; out database: TDuckDBDatabase): Integer; cdecl;
  external DuckDBLib;
function duckdb_open_ext(path: PAnsiChar; out database: TDuckDBDatabase;
  config: TDuckDBConfig; out_error: PPAnsiChar): Integer; cdecl;
  external DuckDBLib;
procedure duckdb_close(var database: TDuckDBDatabase); cdecl;
  external DuckDBLib;
function duckdb_connect(database: TDuckDBDatabase; out connection: TDuckDBConnectionHandle): Integer; cdecl;
  external DuckDBLib;
procedure duckdb_disconnect(var connection: TDuckDBConnectionHandle); cdecl;
  external DuckDBLib;
function duckdb_library_version: PAnsiChar; cdecl;
  external DuckDBLib;
procedure duckdb_free(p: Pointer); cdecl;
  external DuckDBLib;

function duckdb_create_config(out config: TDuckDBConfig): Integer; cdecl;
  external DuckDBLib;
function duckdb_set_config(config: TDuckDBConfig; name, option: PAnsiChar): Integer; cdecl;
  external DuckDBLib;
procedure duckdb_destroy_config(var config: TDuckDBConfig); cdecl;
  external DuckDBLib;

function duckdb_query(connection: TDuckDBConnectionHandle; query: PAnsiChar;
  out_result: PDuckDBResult): Integer; cdecl;
  external DuckDBLib;
procedure duckdb_destroy_result(res: PDuckDBResult); cdecl;
  external DuckDBLib;
function duckdb_column_name(res: PDuckDBResult; col: UInt64): PAnsiChar; cdecl;
  external DuckDBLib;
function duckdb_column_type(res: PDuckDBResult; col: UInt64): Integer; cdecl;
  external DuckDBLib;
function duckdb_column_count(res: PDuckDBResult): UInt64; cdecl;
  external DuckDBLib;
function duckdb_row_count(res: PDuckDBResult): UInt64; cdecl;
  external DuckDBLib;
function duckdb_rows_changed(res: PDuckDBResult): UInt64; cdecl;
  external DuckDBLib;
function duckdb_result_error(res: PDuckDBResult): PAnsiChar; cdecl;
  external DuckDBLib;
function duckdb_result_return_type(res: TDuckDBResult): Integer; cdecl;
  external DuckDBLib;
function duckdb_result_statement_type(res: TDuckDBResult): Integer; cdecl;
  external DuckDBLib;

function duckdb_value_boolean(res: PDuckDBResult; col, row: UInt64): ByteBool; cdecl;
  external DuckDBLib;
function duckdb_value_int64(res: PDuckDBResult; col, row: UInt64): Int64; cdecl;
  external DuckDBLib;
function duckdb_value_uint64(res: PDuckDBResult; col, row: UInt64): UInt64; cdecl;
  external DuckDBLib;
function duckdb_value_double(res: PDuckDBResult; col, row: UInt64): Double; cdecl;
  external DuckDBLib;
function duckdb_value_varchar(res: PDuckDBResult; col, row: UInt64): PAnsiChar; cdecl;
  external DuckDBLib;
function duckdb_value_blob(res: PDuckDBResult; col, row: UInt64): TDuckDBBlob; cdecl;
  external DuckDBLib;
function duckdb_value_is_null(res: PDuckDBResult; col, row: UInt64): ByteBool; cdecl;
  external DuckDBLib;
function duckdb_value_decimal(res: PDuckDBResult; col, row: UInt64): TDuckDBDecimal; cdecl;
  external DuckDBLib;
// C returns duckdb_timestamp = struct { int64_t micros }: ABI-identical to a
// plain Int64 return on both win32 and win64
function duckdb_value_timestamp(res: PDuckDBResult; col, row: UInt64): Int64; cdecl;
  external DuckDBLib;
// C returns duckdb_date = struct { int32_t days }: ABI-identical to Integer
function duckdb_value_date(res: PDuckDBResult; col, row: UInt64): Integer; cdecl;
  external DuckDBLib;

function duckdb_prepare(connection: TDuckDBConnectionHandle; query: PAnsiChar;
  out prepared: TDuckDBPreparedStatement): Integer; cdecl;
  external DuckDBLib;
procedure duckdb_destroy_prepare(var prepared: TDuckDBPreparedStatement); cdecl;
  external DuckDBLib;
function duckdb_prepare_error(prepared: TDuckDBPreparedStatement): PAnsiChar; cdecl;
  external DuckDBLib;
function duckdb_nparams(prepared: TDuckDBPreparedStatement): UInt64; cdecl;
  external DuckDBLib;
function duckdb_parameter_name(prepared: TDuckDBPreparedStatement; index: UInt64): PAnsiChar; cdecl;
  external DuckDBLib;
function duckdb_param_type(prepared: TDuckDBPreparedStatement; index: UInt64): Integer; cdecl;
  external DuckDBLib;
function duckdb_clear_bindings(prepared: TDuckDBPreparedStatement): Integer; cdecl;
  external DuckDBLib;
function duckdb_bind_parameter_index(prepared: TDuckDBPreparedStatement;
  out param_idx: UInt64; name: PAnsiChar): Integer; cdecl;
  external DuckDBLib;
function duckdb_bind_boolean(prepared: TDuckDBPreparedStatement; index: UInt64;
  value: ByteBool): Integer; cdecl;
  external DuckDBLib;
function duckdb_bind_int64(prepared: TDuckDBPreparedStatement; index: UInt64;
  value: Int64): Integer; cdecl;
  external DuckDBLib;
function duckdb_bind_double(prepared: TDuckDBPreparedStatement; index: UInt64;
  value: Double): Integer; cdecl;
  external DuckDBLib;
function duckdb_bind_decimal(prepared: TDuckDBPreparedStatement; index: UInt64;
  value: TDuckDBDecimal): Integer; cdecl;
  external DuckDBLib;
function duckdb_bind_varchar_length(prepared: TDuckDBPreparedStatement; index: UInt64;
  value: PAnsiChar; len: UInt64): Integer; cdecl;
  external DuckDBLib;
function duckdb_bind_blob(prepared: TDuckDBPreparedStatement; index: UInt64;
  data: Pointer; len: UInt64): Integer; cdecl;
  external DuckDBLib;
function duckdb_bind_null(prepared: TDuckDBPreparedStatement; index: UInt64): Integer; cdecl;
  external DuckDBLib;
function duckdb_execute_prepared(prepared: TDuckDBPreparedStatement;
  out_result: PDuckDBResult): Integer; cdecl;
  external DuckDBLib;

function duckdb_appender_create(connection: TDuckDBConnectionHandle;
  schema, table: PAnsiChar; out appender: TDuckDBAppenderHandle): Integer; cdecl;
  external DuckDBLib;
function duckdb_appender_error(appender: TDuckDBAppenderHandle): PAnsiChar; cdecl;
  external DuckDBLib;
function duckdb_appender_flush(appender: TDuckDBAppenderHandle): Integer; cdecl;
  external DuckDBLib;
function duckdb_appender_close(appender: TDuckDBAppenderHandle): Integer; cdecl;
  external DuckDBLib;
function duckdb_appender_destroy(var appender: TDuckDBAppenderHandle): Integer; cdecl;
  external DuckDBLib;
function duckdb_appender_end_row(appender: TDuckDBAppenderHandle): Integer; cdecl;
  external DuckDBLib;
function duckdb_append_bool(appender: TDuckDBAppenderHandle; value: ByteBool): Integer; cdecl;
  external DuckDBLib;
function duckdb_append_int64(appender: TDuckDBAppenderHandle; value: Int64): Integer; cdecl;
  external DuckDBLib;
function duckdb_append_double(appender: TDuckDBAppenderHandle; value: Double): Integer; cdecl;
  external DuckDBLib;
// C takes duckdb_date = struct { int32_t days } by value: ABI-identical to Integer
function duckdb_append_date(appender: TDuckDBAppenderHandle; days: Integer): Integer; cdecl;
  external DuckDBLib;
// C takes duckdb_time / duckdb_timestamp = struct { int64_t micros } by value:
// ABI-identical to Int64
function duckdb_append_time(appender: TDuckDBAppenderHandle; micros: Int64): Integer; cdecl;
  external DuckDBLib;
function duckdb_append_timestamp(appender: TDuckDBAppenderHandle; micros: Int64): Integer; cdecl;
  external DuckDBLib;
function duckdb_append_varchar_length(appender: TDuckDBAppenderHandle;
  value: PAnsiChar; len: UInt64): Integer; cdecl;
  external DuckDBLib;
function duckdb_append_blob(appender: TDuckDBAppenderHandle;
  data: Pointer; len: UInt64): Integer; cdecl;
  external DuckDBLib;
function duckdb_append_null(appender: TDuckDBAppenderHandle): Integer; cdecl;
  external DuckDBLib;

function DuckDBVersion: string;
begin
  Result := UTF8ToString(duckdb_library_version);
end;

// Currency is a fixed point Int64 scaled by 10000: the conversion is exact
// when scale <= 4 and the rescaled value cannot overflow (width - scale <= 14
// integer digits, ie. rescaled < 10^18). Otherwise the caller falls back to
// Double.
function TryDecimalToCurrency(const dec: TDuckDBDecimal; out cur: Currency): Boolean;
const
  Mul: array[0..4] of Int64 = (10000, 1000, 100, 10, 1);
begin
  Result := (dec.scale <= 4) and (Integer(dec.width) - Integer(dec.scale) <= 14);
  if Result then
    // width <= 18 here, so the value fits in the lower 64 bits (upper is only
    // the sign extension)
    PInt64(@cur)^ := Int64(dec.value.lower) * Mul[dec.scale];
end;

function CurrencyToDecimal(const cur: Currency): TDuckDBDecimal;
var
  v: Int64;
begin
  v := PInt64(@cur)^;
  Result.width := 19; // 15 integer digits + 4 decimals
  Result.scale := 4;
  Result.value.lower := UInt64(v);
  if v < 0 then
    Result.value.upper := -1
  else
    Result.value.upper := 0;
end;

function DuckTypeName(t: Integer): string;
begin
  case t of
    DUCKDB_TYPE_BOOLEAN: Result := 'bool';
    DUCKDB_TYPE_TINYINT: Result := 'int8';
    DUCKDB_TYPE_SMALLINT: Result := 'int16';
    DUCKDB_TYPE_INTEGER: Result := 'int32';
    DUCKDB_TYPE_BIGINT: Result := 'int64';
    DUCKDB_TYPE_UTINYINT: Result := 'uint8';
    DUCKDB_TYPE_USMALLINT: Result := 'uint16';
    DUCKDB_TYPE_UINTEGER: Result := 'uint32';
    DUCKDB_TYPE_UBIGINT: Result := 'uint64';
    DUCKDB_TYPE_FLOAT: Result := 'float';
    DUCKDB_TYPE_DOUBLE: Result := 'double';
    DUCKDB_TYPE_TIMESTAMP, DUCKDB_TYPE_TIMESTAMP_S, DUCKDB_TYPE_TIMESTAMP_MS,
    DUCKDB_TYPE_TIMESTAMP_NS, DUCKDB_TYPE_TIMESTAMP_TZ: Result := 'timestamp';
    DUCKDB_TYPE_DATE: Result := 'date';
    DUCKDB_TYPE_TIME, DUCKDB_TYPE_TIME_TZ: Result := 'time';
    DUCKDB_TYPE_INTERVAL: Result := 'interval';
    DUCKDB_TYPE_HUGEINT, DUCKDB_TYPE_UHUGEINT: Result := 'hugeint';
    DUCKDB_TYPE_VARCHAR: Result := 'str';
    DUCKDB_TYPE_BLOB, DUCKDB_TYPE_BIT: Result := 'bin';
    DUCKDB_TYPE_DECIMAL: Result := 'numeric';
    DUCKDB_TYPE_ENUM: Result := 'enum';
    DUCKDB_TYPE_LIST, DUCKDB_TYPE_ARRAY: Result := 'list';
    DUCKDB_TYPE_STRUCT: Result := 'struct';
    DUCKDB_TYPE_MAP: Result := 'map';
    DUCKDB_TYPE_UUID: Result := 'uuid';
    DUCKDB_TYPE_UNION: Result := 'union';
  else
    Result := 'unknown';
  end;
end;

const
  UnixDateDelta = 25569; // days between 1899-12-30 (TDateTime zero) and 1970-01-01

// Exact, locale independent text of a Currency ('1234.5678'), built from its
// internal Int64
function CurrencyToUtf8(const Value: Currency): RawByteString;
var
  v: Int64;
begin
  v := PInt64(@Value)^;
  Result := RawByteString(Format('%d.%.4d', [Abs(v) div 10000, Abs(v) mod 10000]));
  if v < 0 then
    Result := '-' + Result;
end;

type
  TDuckDBAppender = class(TInterfacedObject, IDuckDBAppender)
  private
    FConnection: IDBConnection; // keeps the connection alive
    FAppender: TDuckDBAppenderHandle;
    procedure CheckState(state: Integer);
  protected
    procedure AppendNull;
    procedure AppendBoolean(Value: Boolean);
    procedure AppendInt(Value: Int64);
    procedure AppendDouble(Value: Double);
    procedure AppendCurrency(const Value: Currency);
    procedure AppendString(const Value: string);
    procedure AppendBlob(const Value: TBytes); overload;
    procedure AppendBlob(Data: Pointer; Len: Integer); overload;
    procedure AppendBlob(Stream: TStream); overload;
    procedure AppendDateTime(Value: TDateTime);
    procedure AppendDate(Value: TDateTime);
    procedure AppendTime(Value: TDateTime);
    procedure AppendTimestampMillis(JavaMillis: Int64);
    procedure Append(const Value: ISuperObject);
    procedure AppendRow(const Row: ISuperObject); overload;
    procedure AppendRow(const Row: array of const); overload;
    procedure EndRow;
    procedure Flush;
    procedure Close;
  public
    constructor Create(const Connection: TDBDuckDBConnection; const Schema, Table: string);
    destructor Destroy; override;
  end;

{ TDuckDBAppender }

constructor TDuckDBAppender.Create(const Connection: TDBDuckDBConnection;
  const Schema, Table: string);
var
  sch, tbl: RawByteString;
  psch: PAnsiChar;
  msg: string;
begin
  inherited Create;
  FConnection := Connection;
  FAppender := nil;
  tbl := UTF8Encode(Table);
  if Schema <> '' then
  begin
    sch := UTF8Encode(Schema);
    psch := PAnsiChar(sch);
  end
  else
    psch := nil;
  if duckdb_appender_create(Connection.FConHandle, psch, PAnsiChar(tbl), FAppender) = DuckDBError then
  begin
    if FAppender <> nil then
    begin
      msg := UTF8ToString(duckdb_appender_error(FAppender));
      duckdb_appender_destroy(FAppender);
      FAppender := nil;
    end;
    if msg = '' then
      msg := SysUtils.Format('Cannot create appender on table "%s"', [Table]);
    raise EDuckDBError.Create(msg);
  end;
end;

destructor TDuckDBAppender.Destroy;
begin
  // flushes and closes; errors are lost here, call Close explicitly to
  // surface them
  if FAppender <> nil then
    duckdb_appender_destroy(FAppender);
  inherited;
end;

procedure TDuckDBAppender.CheckState(state: Integer);
var
  msg: string;
begin
  if state = DuckDBError then
  begin
    msg := UTF8ToString(duckdb_appender_error(FAppender));
    if msg = '' then
      msg := 'DuckDB appender error';
    raise EDuckDBError.Create(msg);
  end;
end;

procedure TDuckDBAppender.AppendNull;
begin
  CheckState(duckdb_append_null(FAppender));
end;

procedure TDuckDBAppender.AppendBoolean(Value: Boolean);
begin
  CheckState(duckdb_append_bool(FAppender, Value));
end;

procedure TDuckDBAppender.AppendInt(Value: Int64);
begin
  CheckState(duckdb_append_int64(FAppender, Value));
end;

procedure TDuckDBAppender.AppendDouble(Value: Double);
begin
  CheckState(duckdb_append_double(FAppender, Value));
end;

procedure TDuckDBAppender.AppendCurrency(const Value: Currency);
var
  utf8: RawByteString;
begin
  // as text: DuckDB casts it to the DECIMAL column exactly
  utf8 := CurrencyToUtf8(Value);
  CheckState(duckdb_append_varchar_length(FAppender, PAnsiChar(utf8), Length(utf8)));
end;

procedure TDuckDBAppender.AppendString(const Value: string);
var
  utf8: RawByteString;
begin
  utf8 := UTF8Encode(Value);
  CheckState(duckdb_append_varchar_length(FAppender, PAnsiChar(utf8), Length(utf8)));
end;

procedure TDuckDBAppender.AppendBlob(Data: Pointer; Len: Integer);
begin
  if (Data = nil) or (Len <= 0) then
    CheckState(duckdb_append_blob(FAppender, PAnsiChar(''), 0))
  else
    CheckState(duckdb_append_blob(FAppender, Data, UInt64(Len)));
end;

procedure TDuckDBAppender.AppendBlob(const Value: TBytes);
begin
  if Length(Value) > 0 then
    AppendBlob(@Value[0], Length(Value))
  else
    AppendBlob(nil, 0);
end;

procedure TDuckDBAppender.AppendBlob(Stream: TStream);
var
  bytes: TBytes;
begin
  if (Stream = nil) or (Stream.Size = 0) then
    AppendBlob(nil, 0)
  else
  begin
    SetLength(bytes, Stream.Size);
    Stream.Position := 0;
    Stream.ReadBuffer(bytes[0], Length(bytes));
    AppendBlob(@bytes[0], Length(bytes));
  end;
end;

procedure TDuckDBAppender.AppendDateTime(Value: TDateTime);
begin
  // millisecond precision, enough for tickets
  CheckState(duckdb_append_timestamp(FAppender,
    Round((Value - UnixDateDelta) * MSecsPerDay) * 1000));
end;

procedure TDuckDBAppender.AppendDate(Value: TDateTime);
begin
  CheckState(duckdb_append_date(FAppender, Trunc(Value) - UnixDateDelta));
end;

procedure TDuckDBAppender.AppendTime(Value: TDateTime);
begin
  CheckState(duckdb_append_time(FAppender, Round(Frac(Value) * MSecsPerDay) * 1000));
end;

procedure TDuckDBAppender.AppendTimestampMillis(JavaMillis: Int64);
begin
  CheckState(duckdb_append_timestamp(FAppender, JavaMillis * 1000));
end;

procedure TDuckDBAppender.Append(const Value: ISuperObject);
var
  blob: IDBBlob;
  dt: IDBDateTime;
begin
  if Value = nil then
    AppendNull
  else if Value.QueryInterface(IDBBlob, blob) = 0 then
    AppendBlob(blob.getData)
  else if Value.QueryInterface(IDBDateTime, dt) = 0 then
    AppendTimestampMillis(Value.AsInteger)
  else
    case ObjectGetType(Value) of
      stNull: AppendNull;
      stBoolean: AppendBoolean(Value.AsBoolean);
      stInt: AppendInt(Value.AsInteger);
      stDouble: AppendDouble(Value.AsDouble);
      stCurrency: AppendCurrency(Value.AsCurrency);
      stString: AppendString(Value.AsString);
      stObject, stArray: AppendString(Value.AsJSon(False, False));
    else
      raise EDuckDBError.Create('Appender: unsupported value type');
    end;
end;

procedure TDuckDBAppender.AppendRow(const Row: ISuperObject);
var
  j: Integer;
begin
  if not ObjectIsType(Row, stArray) then
    raise EDuckDBError.Create('Appender: AppendRow expects an array');
  for j := 0 to Row.AsArray.Length - 1 do
    Append(Row.AsArray.O[j]);
  EndRow;
end;

procedure TDuckDBAppender.AppendRow(const Row: array of const);
begin
  AppendRow(SA(Row));
end;

procedure TDuckDBAppender.EndRow;
begin
  CheckState(duckdb_appender_end_row(FAppender));
end;

procedure TDuckDBAppender.Flush;
begin
  CheckState(duckdb_appender_flush(FAppender));
end;

procedure TDuckDBAppender.Close;
begin
  CheckState(duckdb_appender_close(FAppender));
end;

type
  TDuckDBDatabaseHolder = class(TInterfacedObject, IDuckDBDatabaseHolder)
  private
    FDbHandle: TDuckDBDatabase;
  public
    constructor Create(const Database: string; const Config: ISuperObject);
    destructor Destroy; override;
    function Handle: TDuckDBDatabase;
  end;

{ TDuckDBDatabaseHolder }

constructor TDuckDBDatabaseHolder.Create(const Database: string; const Config: ISuperObject);
var
  cfg: TDuckDBConfig;
  err: PAnsiChar;
  path: RawByteString;
  ppath: PAnsiChar;
  f: TSuperObjectIter;
  msg: string;
begin
  inherited Create;
  FDbHandle := nil;
  cfg := nil;

  if (Database = '') or SameText(Database, ':memory:') then
    ppath := nil
  else
  begin
    path := UTF8Encode(Database);
    ppath := PAnsiChar(path);
  end;

  try
    if ObjectIsType(Config, stObject) then
    begin
      if duckdb_create_config(cfg) = DuckDBError then
        raise EDuckDBError.Create('Cannot create DuckDB configuration');
      if ObjectFindFirst(Config, f) then
      repeat
        if duckdb_set_config(cfg, PAnsiChar(UTF8Encode(f.key)),
             PAnsiChar(UTF8Encode(f.val.AsString))) = DuckDBError then
          raise EDuckDBError.CreateFmt('Invalid DuckDB configuration option "%s"', [f.key]);
      until not ObjectFindNext(f);
      ObjectFindClose(f);
    end;

    err := nil;
    if duckdb_open_ext(ppath, FDbHandle, cfg, @err) = DuckDBError then
    begin
      if err <> nil then
      begin
        msg := UTF8ToString(err);
        duckdb_free(err);
      end
      else
        msg := Format('Cannot open database "%s"', [Database]);
      raise EDuckDBError.Create(msg);
    end;
  finally
    if cfg <> nil then
      duckdb_destroy_config(cfg);
  end;
end;

destructor TDuckDBDatabaseHolder.Destroy;
begin
  if FDbHandle <> nil then
    duckdb_close(FDbHandle);
  inherited;
end;

function TDuckDBDatabaseHolder.Handle: TDuckDBDatabase;
begin
  Result := FDbHandle;
end;

{ TDBDuckDBConnection }

constructor TDBDuckDBConnection.Create(const Database: string; const Config: ISuperObject);
begin
  Create(TDuckDBDatabaseHolder.Create(Database, Config) as IDuckDBDatabaseHolder);
end;

constructor TDBDuckDBConnection.Create(const Database: IDuckDBDatabaseHolder);
begin
  inherited Create;
  FDatabase := Database;
  FConHandle := nil;
  if duckdb_connect(FDatabase.Handle, FConHandle) = DuckDBError then
    raise EDuckDBError.Create('Cannot create DuckDB connection');
end;

destructor TDBDuckDBConnection.Destroy;
begin
  if FConHandle <> nil then
    duckdb_disconnect(FConHandle);
  FDatabase := nil;
  inherited;
end;

procedure TDBDuckDBConnection.RawExecute(const Sql: string);
var
  ret: TDuckDBResult;
  msg: string;
begin
  FillChar(ret, SizeOf(ret), 0);
  if duckdb_query(FConHandle, PAnsiChar(UTF8Encode(Sql)), @ret) = DuckDBError then
  begin
    msg := UTF8ToString(duckdb_result_error(@ret));
    duckdb_destroy_result(@ret);
    if msg = '' then
      msg := 'DuckDB error';
    raise EDuckDBError.Create(msg);
  end;
  duckdb_destroy_result(@ret);
end;

procedure TDBDuckDBConnection.ExecuteImmediate(const Sql: string);
begin
  RawExecute(Sql);
end;

function TDBDuckDBConnection.Query(const Sql: string): IDBQuery;
begin
  Result := TDBDuckDBQuery.Create(Self, nil, Sql);
end;

function TDBDuckDBConnection.Appender(const Table: string; const Schema: string): IDuckDBAppender;
begin
  Result := TDuckDBAppender.Create(Self, Schema, Table);
end;

function TDBDuckDBConnection.Transaction(const Options: ISuperObject): IDBTransaction;
begin
  Result := TDBDuckDBTransaction.Create(Self, Options);
end;

function TDBDuckDBConnection.Transaction(const OtherConnections: array of IDBConnection;
  const Options: ISuperObject): IDBTransaction;
begin
  if Length(OtherConnections) > 0 then
    raise EDuckDBError.Create('DuckDB does not support transactions over multiple connections');
  Result := TDBDuckDBTransaction.Create(Self, Options);
end;

{ TDBDuckDBTransaction }

constructor TDBDuckDBTransaction.Create(const Connection: TDBDuckDBConnection;
  const Options: ISuperObject);
begin
  inherited Create;
  FRollback := False;
  FDbConnection := Connection;
  FConnection := Connection;
  if ObjectIsType(Options, stString) then
    FDbConnection.RawExecute('BEGIN TRANSACTION ' + Options.AsString)
  else
    FDbConnection.RawExecute('BEGIN TRANSACTION');
end;

destructor TDBDuckDBTransaction.Destroy;
begin
  if FRollback then
  begin
    TriggerRollbackEvent;
    FDbConnection.RawExecute('ROLLBACK');
  end
  else
  begin
    FDbConnection.RawExecute('COMMIT');
    TriggerCommitEvent;
  end;
  inherited;
end;

procedure TDBDuckDBTransaction.ExecuteImmediate(const Sql: string);
begin
  FDbConnection.RawExecute(Sql);
end;

function TDBDuckDBTransaction.GetConnection: IDBConnection;
begin
  Result := FConnection;
end;

function TDBDuckDBTransaction.Query(const Sql: string; const Connection: IDBConnection): IDBQuery;
begin
  if Connection = nil then
    Result := TDBDuckDBQuery.Create(FDbConnection, Self, Sql)
  else
    Result := TDBDuckDBQuery.Create(Connection as TDBDuckDBConnection, Self, Sql);
end;

procedure TDBDuckDBTransaction.Rollback(value: boolean);
begin
  FRollback := value;
end;

{ TDBDuckDBQuery }

constructor TDBDuckDBQuery.Create(const Connection: TDBDuckDBConnection;
  const Trans: TDBDuckDBTransaction; const Sql: string);
var
  msg: string;
begin
  inherited Create;
  FDbConnection := Connection;
  FConnection := Connection;
  FSql := Sql;
  FStHandle := nil;
  if duckdb_prepare(FDbConnection.FConHandle, PAnsiChar(UTF8Encode(Sql)), FStHandle) = DuckDBError then
  begin
    msg := UTF8ToString(duckdb_prepare_error(FStHandle));
    duckdb_destroy_prepare(FStHandle);
    FStHandle := nil;
    if msg = '' then
      msg := 'Cannot prepare statement';
    if Trans <> nil then
      Trans.FRollback := True;
    raise EDuckDBError.Create(msg);
  end;
end;

destructor TDBDuckDBQuery.Destroy;
begin
  if FStHandle <> nil then
    duckdb_destroy_prepare(FStHandle);
  inherited;
end;

function TDBDuckDBQuery.Execute(const Params: ISuperObject; Options: TQueryOptions;
  const Transaction: IDBTransaction): ISuperObject;
var
  ret: ISuperObject;
begin
  Execute(Params,
    procedure(const item: ISuperObject; const R: TExecuteResult)
    begin
      if R.IsResult then
      begin
        if R.HasAffectedRows then
          ret := TSuperObject.Create(Int64(R.Changed))
        else
          ret := item;
      end
      else
      begin
        if ret = nil then
          ret := TSuperObject.Create(stArray);

        if item <> nil then
          ret.AsArray.Add(item);
      end;
    end,
    Options, Transaction
  );

  Result := ret;
end;

procedure TDBDuckDBQuery.Execute(const Params: ISuperObject; const callback: TExecuteCallback;
  Options: TQueryOptions; const Transaction: IDBTransaction);
var
  ctx: IDBTransaction;

  function GetValue(res: PDuckDBResult; col, row: UInt64): ISuperObject;
  var
    p: PAnsiChar;
    b: TDuckDBBlob;
    blob: IDBBlob;
    dec: TDuckDBDecimal;
    cur: Currency;
  begin
    if duckdb_value_is_null(res, col, row) then
      Exit(nil);
    case duckdb_column_type(res, col) of
      DUCKDB_TYPE_BOOLEAN:
        Result := TSuperObject.Create(Boolean(duckdb_value_boolean(res, col, row)));

      DUCKDB_TYPE_TINYINT, DUCKDB_TYPE_SMALLINT, DUCKDB_TYPE_INTEGER, DUCKDB_TYPE_BIGINT,
      DUCKDB_TYPE_UTINYINT, DUCKDB_TYPE_USMALLINT, DUCKDB_TYPE_UINTEGER:
        Result := TSuperObject.Create(duckdb_value_int64(res, col, row));

      DUCKDB_TYPE_UBIGINT:
        Result := TSuperObject.Create(Int64(duckdb_value_uint64(res, col, row)));

      DUCKDB_TYPE_FLOAT, DUCKDB_TYPE_DOUBLE:
        Result := TSuperObject.Create(duckdb_value_double(res, col, row));

      DUCKDB_TYPE_DECIMAL:
        begin
          dec := duckdb_value_decimal(res, col, row);
          if TryDecimalToCurrency(dec, cur) then
            Result := TSuperObject.CreateCurrency(cur)
          else
            Result := TSuperObject.Create(duckdb_value_double(res, col, row));
        end;

      DUCKDB_TYPE_TIMESTAMP, DUCKDB_TYPE_TIMESTAMP_TZ:
        // DuckDB timestamps are microseconds since epoch, TDBDateTime expects
        // java milliseconds
        Result := TDBDateTime.Create(duckdb_value_timestamp(res, col, row) div 1000);

      DUCKDB_TYPE_DATE:
        // days since epoch -> java milliseconds
        Result := TDBDateTime.Create(Int64(duckdb_value_date(res, col, row)) * MSecsPerDay);

      DUCKDB_TYPE_BLOB:
        begin
          b := duckdb_value_blob(res, col, row);
          blob := TDBBinary.Create;
          if b.data <> nil then
          begin
            if b.size > 0 then
              blob.getData.Write(b.data^, Longint(b.size));
            duckdb_free(b.data);
          end;
          Result := blob as ISuperObject;
        end;
    else
      // VARCHAR and everything without a native mapping (HUGEINT, UUID,
      // INTERVAL, TIME, ENUM, LIST, STRUCT, MAP...) is fetched as text
      p := duckdb_value_varchar(res, col, row);
      if p <> nil then
      begin
        Result := TSuperObject.Create(UTF8ToString(p));
        duckdb_free(p);
      end
      else
        Result := nil;
    end;
  end;

  function GetOne(res: PDuckDBResult; row: UInt64): ISuperObject;
  var
    j, cols: Integer;
  begin
    cols := Integer(duckdb_column_count(res));
    if qoValue in Options then
    begin
      if cols > 0 then
        Result := GetValue(res, 0, row)
      else
        Result := nil;
    end
    else if qoArray in Options then
    begin
      Result := TSuperObject.Create(stArray);
      for j := 0 to cols - 1 do
        Result.AsArray.Add(GetValue(res, UInt64(j), row));
    end
    else
    begin
      Result := TSuperObject.Create(stObject);
      for j := 0 to cols - 1 do
        Result.AsObject.O[UTF8ToString(duckdb_column_name(res, UInt64(j)))] :=
          GetValue(res, UInt64(j), row);
    end;
  end;

  procedure SetParam(index: UInt64; const value: ISuperObject);

    procedure CheckBind(state: Integer);
    begin
      if state = DuckDBError then
        raise EDuckDBError.CreateFmt('Cannot bind parameter %d', [index]);
    end;

  var
    blob: IDBBlob;
    stream: TStream;
    bytes: TBytes;
    size: Longint;
    utf8: RawByteString;
  begin
    if (value <> nil) and (value.QueryInterface(IDBBlob, blob) = 0) then
    begin
      stream := blob.getData;
      size := stream.Size;
      stream.Position := 0;
      if size > 0 then
      begin
        SetLength(bytes, size);
        stream.ReadBuffer(bytes[0], size);
        CheckBind(duckdb_bind_blob(FStHandle, index, @bytes[0], UInt64(size)));
      end
      else
        CheckBind(duckdb_bind_blob(FStHandle, index, PAnsiChar(''), 0));
    end
    else
      case ObjectGetType(value) of
        stNull:
          CheckBind(duckdb_bind_null(FStHandle, index));
        stBoolean:
          CheckBind(duckdb_bind_boolean(FStHandle, index, value.AsBoolean));
        stInt:
          CheckBind(duckdb_bind_int64(FStHandle, index, value.AsInteger));
        stDouble:
          CheckBind(duckdb_bind_double(FStHandle, index, value.AsDouble));
        stCurrency:
          // bound as DECIMAL(19,4) to keep the exact fixed point value
          CheckBind(duckdb_bind_decimal(FStHandle, index, CurrencyToDecimal(value.AsCurrency)));
        stString:
          begin
            utf8 := UTF8Encode(value.AsString);
            CheckBind(duckdb_bind_varchar_length(FStHandle, index, PAnsiChar(utf8), Length(utf8)));
          end;
        stObject, stArray:
          begin
            // bound as JSON text, handy for DuckDB JSON columns
            utf8 := UTF8Encode(value.AsJSon(False, False));
            CheckBind(duckdb_bind_varchar_length(FStHandle, index, PAnsiChar(utf8), Length(utf8)));
          end;
      else
        raise EDuckDBError.CreateFmt('Parameter %d: unsupported value type', [index]);
      end;
  end;

  procedure Process;
  var
    ret: TDuckDBResult;
    r: TExecuteResult;
    rows, row: Int64;
    changed: UInt64;
    msg: string;
  begin
    FillChar(ret, SizeOf(ret), 0);
    if duckdb_execute_prepared(FStHandle, @ret) = DuckDBError then
    begin
      msg := UTF8ToString(duckdb_result_error(@ret));
      duckdb_destroy_result(@ret);
      if msg = '' then
        msg := 'DuckDB error';
      raise EDuckDBError.Create(msg);
    end;
    try
      if duckdb_result_return_type(ret) = DUCKDB_RESULT_TYPE_QUERY_RESULT then
      begin
        rows := Int64(duckdb_row_count(@ret));
        if qoSingleton in Options then
        begin
          r := TExecuteResult.Create(True, False);
          if rows > 0 then
            callback(GetOne(@ret, 0), r)
          else
            callback(nil, r);
        end
        else
        begin
          r := TExecuteResult.Create(False, False);
          callback(nil, r); { prepare an empty result object, even if we fetch no data }
          for row := 0 to rows - 1 do
            callback(GetOne(@ret, UInt64(row)), r);
        end;
      end
      else
      begin
        r := TExecuteResult.Create(True, True);
        changed := duckdb_rows_changed(@ret);
        case duckdb_result_statement_type(ret) of
          DUCKDB_STATEMENT_TYPE_INSERT, DUCKDB_STATEMENT_TYPE_COPY:
            r.Inserted := Cardinal(changed);
          DUCKDB_STATEMENT_TYPE_DELETE:
            r.Deleted := Cardinal(changed);
        else
          r.Updated := Cardinal(changed);
        end;
        callback(nil, r);
      end;
    finally
      duckdb_destroy_result(@ret);
    end;
  end;

var
  count: Integer;
  j: Integer;
  idx: UInt64;
  f: TSuperObjectIter;
begin
  ctx := Transaction;
  count := Integer(duckdb_nparams(FStHandle));
  try
    if count > 0 then
    begin
      duckdb_clear_bindings(FStHandle);
      case ObjectGetType(Params) of
        stArray:
          with Params.AsArray do
          begin
            if Length <> count then
              raise EDuckDBError.Create('Missing parameters.');

            for j := 0 to Length - 1 do
              SetParam(UInt64(j + 1), O[j]);

            Process;
          end;
        stObject:
          begin
            if Params.AsObject.count <> count then
              raise EDuckDBError.Create('Missing parameters.');

            if ObjectFindFirst(Params, f) then
            repeat
              if duckdb_bind_parameter_index(FStHandle, idx,
                   PAnsiChar(UTF8Encode(f.key))) = DuckDBError then
                raise EDuckDBError.CreateFmt('Unknown parameter "%s"', [f.key]);
              SetParam(idx, f.val);
            until not ObjectFindNext(f);
            ObjectFindClose(f);

            Process;
          end;
      else
        raise EDuckDBError.Create('Unexpected parameter');
      end;
    end
    else
      Process;
  except
    if ctx <> nil then
      ctx.Rollback(True);
    raise;
  end;
end;

function TDBDuckDBQuery.GetInputMeta: ISuperObject;
var
  j, count: Integer;
  rec: ISuperObject;
  name: PAnsiChar;
begin
  count := Integer(duckdb_nparams(FStHandle));
  if count > 0 then
  begin
    Result := TSuperObject.Create(stArray);
    with Result.AsArray do
      for j := 1 to count do
      begin
        rec := TSuperObject.Create(stObject);
        name := duckdb_parameter_name(FStHandle, UInt64(j));
        if name <> nil then
        begin
          rec.S['name'] := UTF8ToString(name);
          duckdb_free(name);
        end;
        rec.S['type'] := DuckTypeName(duckdb_param_type(FStHandle, UInt64(j)));
        Add(rec);
      end;
  end
  else
    Result := nil;
end;

function TDBDuckDBQuery.GetOutputMeta(byindex: Boolean): ISuperObject;
begin
  // The DuckDB C API does not expose the output columns of a prepared
  // statement before its execution
  Result := nil;
end;

{ TDBDuckDBConnectionPool }

constructor TDBDuckDBConnectionPool.Create(Max: Integer; const Database: string;
  const Config: ISuperObject);
begin
  inherited Create;
  FDatabase := TDuckDBDatabaseHolder.Create(Database, Config);
  FPool := TList<IDBConnection>.Create;
  FCriticalSection := TCriticalSection.Create;
  FMax := Max;
end;

destructor TDBDuckDBConnectionPool.Destroy;
begin
  Lock;
  try
    ClearPool;
    FPool.Free;
  finally
    Unlock;
  end;
  FCriticalSection.Free;
  inherited;
end;

procedure TDBDuckDBConnectionPool.ClearPool;
begin
  Lock;
  try
    FPool.Clear;
  finally
    Unlock;
  end;
end;

function TDBDuckDBConnectionPool.Connection: IDBConnection;
var
  cnx: IDBConnection;
  j, k: Integer;
begin
  Result := nil;

  Lock;
  try
    while Result = nil do
    begin
      for j := 0 to FPool.Count - 1 do
      begin
        cnx := FPool[j];
        k := cnx._AddRef;
        try
          if k = 3 then
          begin
            Result := cnx;
            Exit;
          end;
        finally
          cnx._Release;
          cnx := nil;
        end;
      end;
      if (Result = nil) and ((FMax < 1) or (FPool.Count < FMax)) then
      begin
        Result := TDBDuckDBConnection.Create(FDatabase);
        FPool.Add(Result);
        Exit;
      end;
      Sleep(1);
    end;
  finally
    Unlock;
  end;
end;

function TDBDuckDBConnectionPool.Size: Integer;
begin
  Lock;
  try
    Result := FPool.Count;
  finally
    Unlock;
  end;
end;

procedure TDBDuckDBConnectionPool.Lock;
begin
  FCriticalSection.Enter;
end;

procedure TDBDuckDBConnectionPool.Unlock;
begin
  FCriticalSection.Leave;
end;

function TDBDuckDBConnectionPool.RefCount: Integer;
begin
  Result := FRefCount;
end;

end.
