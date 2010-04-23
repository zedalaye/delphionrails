unit dorUIB;
{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$I uib.inc}

interface
uses
{$IFDEF MSWINDOWS}
  windows,
{$ENDIF}
  dorDB, uibase, uiblib, superobject, syncobjs, dorUtils;

type
  TDBUIBConnectionPool = class(TSuperObject, IDBConnectionPool)
  private
    FCriticalSection: TCriticalSection;
    FMax: Integer;
  protected
    function GetConnection: IDBConnection;
    function GetSize: Integer;
  public
    constructor Create(const Options: ISuperObject; max: Integer); reintroduce; overload;
    constructor Create(const Options: string; max: Integer); reintroduce; overload;
    destructor Destroy; override;
  end;

  TDBUIBConnection = class(TDBConnection)
  private
    FLibrary: TUIBLibrary;
    FDbHandle: IscDbHandle;
    FCharacterSet: TCharacterSet;
  protected
    function newContext(const Options: ISuperObject = nil): IDBContext; override;
  public
    constructor Create(const Options: ISuperObject); reintroduce; overload;
    constructor Create(const Options: string); reintroduce; overload;
    destructor Destroy; override;
  end;

  TDBUIBContext = class(TDBContext)
  private
    FTrHandle: IscTrHandle;
    FConnection: TDBUIBConnection;
  protected
    procedure ExecuteImmediate(const Options: SOString); override;
    function newCommand(const Options: ISuperObject = nil): IDBCommand; override;
  public
    constructor Create(const Connection: TDBUIBConnection; const Options: ISuperObject); reintroduce;
    destructor Destroy; override;
  end;

  TDBUIBCommand = class(TDBCommand)
  private
    FStHandle: IscStmtHandle;
    FConnection: TDBUIBConnection;
    FSQLResult: TSQLResult;
    FSQLParams: TSQLParams;
    FStatementType: TUIBStatementType;
  protected
    function Execute(const params: ISuperObject = nil; const context: IDBContext = nil): ISuperObject; override;
    function GetInputMeta: ISuperObject; override;
    function GetOutputMeta: ISuperObject; override;
  public
    constructor Create(const Connection: TDBUIBConnection; const Context: TDBUIBContext; const Options: ISuperObject); reintroduce;
    destructor Destroy; override;
  end;

implementation
uses sysutils;

{ TDBUIBConnection }

constructor TDBUIBConnection.Create(const Options: ISuperObject);
var
  param: ISuperObject;
  option: string;
begin
  inherited Create(stObject);
  FDbHandle := nil;

  DataPtr := Self;
  Merge(Options, true);

  FLibrary := TUIBLibrary.Create;

  param := O['library'];
  if param <> nil then
    FLibrary.Load(string(param.AsString)) else
    FLibrary.Load(GetClientLibrary);

  option := 'sql_dialect=3';

  param := O['username'];
  if param <> nil then
    option := option + ';user_name=' + param.AsString;

  param := O['password'];
  if param <> nil then
    option := option + ';password=' + param.AsString;

  param := O['characterset'];
  if param <> nil then
    FCharacterSet := StrToCharacterSet(AnsiString(param.AsString)) else
    FCharacterSet := GetSystemCharacterset;
  option := option + ';lc_ctype=' + string(CharacterSetStr[FCharacterSet]);

  param := O['databasename'];
  if param <> nil then
    FLibrary.AttachDatabase(AnsiString(param.AsString), FDbHandle, AnsiString(option)) else
    FDbHandle := nil;
end;

constructor TDBUIBConnection.Create(const Options: string);
begin
  Create(SO(Options));
end;

destructor TDBUIBConnection.Destroy;
begin
  if FDbHandle <> nil then
    FLibrary.DetachDatabase(FDbHandle);
  FLibrary.Free;
  inherited;
end;

function TDBUIBConnection.newContext(const Options: ISuperObject): IDBContext;
begin
  Result := TDBUIBContext.Create(Self, Options);
end;

{ TDBUIBContext }

constructor TDBUIBContext.Create(const Connection: TDBUIBConnection; const Options: ISuperObject);
begin
  inherited Create(stObject);
  DataPtr := Self;
  Merge(Options, true);
  FConnection := Connection;
  AsObject['connection'] := Connection;
  FTrHandle := nil;
  with FConnection, FLibrary do
    TransactionStart(FTrHandle, FDbHandle);
end;

destructor TDBUIBContext.Destroy;
var
  obj: ISuperObject;
begin
  obj := AsObject['rollback'];
  if ObjectIsType(obj, stBoolean) and obj.AsBoolean then
  begin
    TriggerRollbackEvent;
    FConnection.FLibrary.TransactionRollback(FTrHandle);
  end
  else
    begin
      FConnection.FLibrary.TransactionCommit(FTrHandle);
      TriggerCommitEvent;
    end;
  inherited Destroy;
end;

procedure TDBUIBContext.ExecuteImmediate(const Options: SOString);
begin
  with FConnection, FLibrary do
  {$IFDEF UNICODE}
    DSQLExecuteImmediate(FDbHandle, FTrHandle, MBUEncode(Options, CharacterSetCP[FCharacterSet]), 3);
  {$ELSE}
    DSQLExecuteImmediate(FDbHandle, FTrHandle, Options, 3);
  {$ENDIF}
end;

function TDBUIBContext.newCommand(const Options: ISuperObject): IDBCommand;
begin
  Result := TDBUIBCommand.Create(FConnection, Self, Options);
end;

{ TDBUIBCommand }

constructor TDBUIBCommand.Create(const Connection: TDBUIBConnection;
  const Context: TDBUIBContext; const Options: ISuperObject);
begin
  inherited Create(stObject);
  DataPtr := Self;
  if ObjectIsType(Options, stString) then
    O['sql'] := Options else
    Merge(Options, true);
  FConnection := Connection;
  AsObject['connection'] := Connection;

  FSQLResult := TSQLResult.Create(Connection.FCharacterSet, 0, false, true);
  FSQLParams := TSQLParams.Create(Connection.FCharacterSet);

  FStHandle := nil;
  with FConnection, FLibrary do
  begin
    DSQLAllocateStatement(FDbHandle, FStHandle);
    FStatementType := DSQLPrepare(FDbHandle, Context.FTrHandle, FStHandle,
{$IFDEF UNICODE}
      MBUEncode(FSQLParams.Parse(PSOChar(self.S['sql'])), CharacterSetCP[FCharacterSet]), 3, FSQLResult);
{$ELSE}
      AnsiString(FSQLParams.Parse(PSOChar(self.S['sql']))), 3, FSQLResult);
{$ENDIF}
    if (FSQLParams.FieldCount > 0) then
      DSQLDescribeBind(FStHandle, 3, FSQLParams);
  end;
end;

destructor TDBUIBCommand.Destroy;
begin
  FSQLResult.Free;
  FSQLParams.Free;
  FConnection.FLibrary.DSQLFreeStatement(FStHandle, DSQL_drop);
  inherited;
end;

function TDBUIBCommand.Execute(const params: ISuperObject; const context: IDBContext): ISuperObject;
var
  dfFunction, dfArray, dfFirstOne: boolean;
  str: string;
  ctx: IDBContext;

  function getone: ISuperObject;
  var
    i: integer;
    blob: IDBBlob;
  begin
    if dfArray then
    begin
      Result := TSuperObject.Create(stArray);
      for i := 0 to FSQLResult.FieldCount - 1 do
        if FSQLResult.IsNull[i] then
          Result.AsArray.Add(nil) else
        case FSQLResult.FieldType[i] of
          uftChar, uftVarchar, uftCstring:
            if FSQLResult.Data.sqlvar[i].SqlSubType > 1 then
              Result.AsArray.Add(TSuperObject.Create(FSQLResult.AsString[i])) else
              Result.AsArray.Add(TSuperObject.Create(RbsToHex(FSQLResult.AsRawByteString[i])));
          uftSmallint, uftInteger, uftInt64: Result.AsArray.Add(TSuperObject.Create(FSQLResult.AsInteger[i]));
          uftNumeric:
            begin
              if FSQLResult.SQLScale[i] >= -4 then
                Result.AsArray.Add(TSuperObject.CreateCurrency(FSQLResult.AsCurrency[i])) else
                Result.AsArray.Add(TSuperObject.Create(FSQLResult.AsDouble[i]));
            end;
          uftFloat, uftDoublePrecision: Result.AsArray.Add(TSuperObject.Create(FSQLResult.AsDouble[i]));
          uftBlob, uftBlobId:
            begin
              if FSQLResult.Data^.sqlvar[i].SqlSubType = 1 then
              begin
                FSQLResult.ReadBlob(i, str);
                Result.AsArray.Add(TSuperObject.Create(str));
              end else
              begin
                blob := TDBBinary.Create;
                FSQLResult.ReadBlob(i, blob.getData);
                Result.AsArray.Add(blob as ISuperObject);
              end;
            end;
          uftTimestamp, uftDate, uftTime: Result.AsArray.Add(TDBDateTime.Create(DelphiToJavaDateTime(FSQLResult.AsDateTime[i])));
        {$IFDEF IB7_UP}
          uftBoolean: Result.AsArray.Add(TSuperObject.Create(PChar(FSQLResult.AsBoolean[i])));
        {$ENDIF}
         else
           Result.AsArray.Add(nil);
         end;
    end else
    begin
      Result := TSuperObject.Create(stObject);
      for i := 0 to FSQLResult.FieldCount - 1 do
        if FSQLResult.IsNull[i] then
          Result[LowerCase(FSQLResult.AliasName[i])] := nil else
        case FSQLResult.FieldType[i] of
          uftChar, uftVarchar, uftCstring:
             if FSQLResult.Data.sqlvar[i].SqlSubType > 1 then
               Result[LowerCase(FSQLResult.AliasName[i])] := TSuperObject.Create(FSQLResult.AsString[i]) else
               Result[LowerCase(FSQLResult.AliasName[i])] := TSuperObject.Create(RbsToHex(FSQLResult.AsRawByteString[i]));
          uftSmallint, uftInteger, uftInt64: Result[LowerCase(FSQLResult.AliasName[i])] := TSuperObject.Create(FSQLResult.AsInteger[i]);
          uftNumeric:
            begin
              if FSQLResult.SQLScale[i] >= -4 then
                Result[LowerCase(FSQLResult.AliasName[i])] := TSuperObject.CreateCurrency(FSQLResult.AsCurrency[i]) else
                Result[LowerCase(FSQLResult.AliasName[i])] := TSuperObject.Create(FSQLResult.AsDouble[i]);
            end;
          uftFloat, uftDoublePrecision: Result[LowerCase(FSQLResult.AliasName[i])] := TSuperObject.Create(FSQLResult.AsDouble[i]);
          uftBlob, uftBlobId:
            begin
              if FSQLResult.Data^.sqlvar[i].SqlSubType = 1 then
              begin
                FSQLResult.ReadBlob(i, str);
                Result[LowerCase(FSQLResult.AliasName[i])] := TSuperObject.Create(str);
              end else
              begin
                blob := TDBBinary.Create;
                FSQLResult.ReadBlob(i, blob.getData);
                Result[LowerCase(FSQLResult.AliasName[i])] := blob as ISuperObject;
              end;
            end;
          uftTimestamp, uftDate, uftTime: Result[LowerCase(FSQLResult.AliasName[i])] := TDBDateTime.Create(DelphiToJavaDateTime(FSQLResult.AsDateTime[i]));
        {$IFDEF IB7_UP}
          uftBoolean: Result[LowerCase(FSQLResult.AliasName[i])] := TSuperObject.Create(PChar(FSQLResult.AsBoolean[i]));
        {$ENDIF}
         else
           Result[LowerCase(FSQLResult.AliasName[i])] := nil;
         end;
    end;
  end;

  procedure SetParam(index: Integer; value: ISuperObject);
  var
    BlobHandle: IscBlobHandle;
    blob: IDBBlob;
    dt: TDateTime;
    i: Int64;
  begin
    if ObjectIsType(value, stNull) then
      FSQLParams.IsNull[index] := true else
      case FSQLParams.FieldType[index] of
        uftNumeric:
          begin
            if ObjectIsType(value, stCurrency) then
              FSQLParams.AsCurrency[index] := value.AsCurrency else
              FSQLParams.AsDouble[index] := value.AsDouble;
          end;
        uftChar, uftVarchar, uftCstring:
          if FSQLParams.Data.sqlvar[index].SqlSubType > 1 then
            FSQLParams.AsString[index] := value.AsString else
            FSQLParams.AsRawByteString[index] := HexToRbs(value.AsString);

        uftSmallint: FSQLParams.AsSmallint[index] := value.AsInteger;
        uftInteger: FSQLParams.AsInteger[index] := value.AsInteger;
        uftFloat: FSQLParams.AsSingle[index] := value.AsDouble;
        uftDoublePrecision: FSQLParams.AsDouble[index] := value.AsDouble;
        uftDate, uftTime, uftTimestamp:
          case ObjectGetType(value) of
            stInt: FSQLParams.AsDateTime[index] := JavaToDelphiDateTime(value.AsInteger);
            stString:
              if ISO8601DateToJavaDateTime(value.AsString, i) then
                FSQLParams.AsDateTime[index] := JavaToDelphiDateTime(i) else
                if TryStrToDateTime(value.AsString, dt) then
                  FSQLParams.AsDateTime[index] := dt else
                  FSQLParams.IsNull[index] := true;
          end;
        uftInt64: FSQLParams.AsInt64[index] := value.AsInteger;
        uftBlob, uftBlobId:
          with FConnection, FLibrary, TDBUIBContext((ctx as ISuperObject).DataPtr) do
          begin
            BlobHandle := nil;
            FSQLParams.AsQuad[Index] := BlobCreate(FDbHandle, FTrHandle, BlobHandle);
            if value.QueryInterface(IDBBlob, blob) = 0 then
              BlobWriteStream(BlobHandle, blob.getData) else
{$IFDEF UNICODE}
                BlobWriteString(BlobHandle, MBUEncode(value.AsString, CharacterSetCP[FCharacterSet]));
{$ELSE}
                BlobWriteString(BlobHandle, value.AsString);
{$ENDIF}
            BlobClose(BlobHandle);
          end;
      else
        raise Exception.Create('not yet implemented');
      end;
  end;

  procedure Process;
  begin
    with FConnection, FLibrary, TDBUIBContext((ctx as ISuperObject).DataPtr) do
      if FSQLResult.FieldCount > 0 then
      begin
        if not dfFunction then
          DSQLSetCursorName(FStHandle, AnsiChar('C') + AnsiString(IntToStr(PtrInt(FStHandle))));
        try
          if (FStatementType = stExecProcedure) then
          begin
            DSQLExecute2(FTrHandle, FStHandle, 3, FSQLParams, FSQLResult);
            dfFirstOne := true;
          end else
            DSQLExecute(FTrHandle, FStHandle, 3, FSQLParams);

          if dfFunction then
            Result := getone else
          if not dfFirstOne then
          begin
            Result := TSuperObject.Create(stArray);
            while DSQLFetchWithBlobs(FDbHandle, FTrHandle, FStHandle, 3, FSQLResult) do
              Result.AsArray.Add(getone);
          end else
            if DSQLFetchWithBlobs(FDbHandle, FTrHandle, FStHandle, 3, FSQLResult) then
              Result := getone else
              Result := nil;
        finally
          if not dfFunction then
            DSQLFreeStatement(FStHandle, DSQL_close);
        end;
      end else
      begin
        DSQLExecute(FTrHandle, FStHandle, 3, FSQLParams);
        Result := TSuperObject.Create( DSQLInfoRowsAffected(FStHandle, FStatementType));
      end;
  end;
var
  j, affected: integer;
  f: TSuperObjectIter;
begin
  ctx := context;
  dfFirstOne := B['firstone'];
  dfArray := B['array'];
  dfFunction := B['function'];

  if ctx = nil then
    ctx := FConnection.newContext;

  for j := 0 to FSQLParams.FieldCount - 1 do
    FSQLParams.IsNull[j] := true;
  try
    if FSQLParams.FieldCount > 0 then
    begin
      if ObjectIsType(params, stArray) then
      begin
        with params.AsArray do
        begin
          if (Length = FSQLParams.FieldCount) and not(ObjectGetType(O[0]) in [stObject, stArray]) then
          begin
            for j := 0 to Length - 1 do
              SetParam(j, O[j]);
            Process;
          end else
            if FSQLResult.FieldCount > 0 then
            begin
              Result := TSuperObject.Create(stArray);
              for j := 0 to Length - 1 do
                Result.AsArray.Add(Execute(O[j], ctx));
            end else
            begin
              affected := 0;
              for j := 0 to Length - 1 do
                inc(affected, Execute(O[j], ctx).AsInteger);
              Result := TSuperObject.Create(affected);
            end;
        end;
      end else
      if ObjectIsType(params, stObject) then
      begin
        if ObjectFindFirst(params, f) then
        repeat
          SetParam(FSQLParams.GetFieldIndex(AnsiString(f.key)), f.val);
        until not ObjectFindNext(f);
        ObjectFindClose(f);
        Process;
      end else
      begin
        SetParam(0, params);
        Process;
      end;
    end else
      Process;
  except
    (ctx as ISuperObject).B['rollback'] := true;
    raise;
  end;
end;

function TDBUIBCommand.GetInputMeta: ISuperObject;
var
  j: Integer;
  rec: ISuperObject;
  prm: PUIBSQLVar;
begin
  if FSQLParams.FieldCount > 0 then
  begin
    Result := TSuperObject.Create(stArray);
    with Result.AsArray do
      for j := 0 to FSQLParams.FieldCount - 1 do
      begin
        rec := TSuperObject.Create(stObject);
        prm := @FSQLParams.Data.sqlvar[j];
        if prm.ParamNameLength > 0 then
          rec.S['name'] := string(copy(prm.ParamName, 1, prm.ParamNameLength));
        add(rec);
        case FSQLParams.FieldType[j] of
          uftChar, uftVarchar, uftCstring:
          begin
            rec.S['type'] := 'str';
            rec.I['length'] := FSQLParams.SQLLen[j];
          end;
          uftSmallint, uftInteger, uftInt64: rec.S['type'] := 'int';
          uftNumeric, uftFloat, uftDoublePrecision: rec.S['type'] := 'float';
          uftBlob, uftBlobId:
          begin
            if FSQLParams.Data^.sqlvar[j].SqlSubType = 1 then
              rec.S['type'] := 'str' else
              rec.S['type'] := 'bin';
          end;
          uftTimestamp: rec.S['type'] := 'timestamp';
          uftDate: rec.S['type'] := 'date';
          uftTime: rec.S['type'] := 'time';
          {$IFDEF IB7_UP}
          uftBoolean: rec.S['type'] := 'bool';
          {$ENDIF}
        end;
        if not FSQLParams.IsNullable[j] then
          rec.B['notnull'] := true;
      end;
  end else
    Result := nil;
end;

function TDBUIBCommand.GetOutputMeta: ISuperObject;
var
  j: Integer;
  rec: ISuperObject;
  dfArray: Boolean;
begin
  if FSQLResult.FieldCount > 0 then
  begin
    dfArray := B['array'];
    if dfArray then
      Result := TSuperObject.Create(stArray) else
      Result := TSuperObject.Create(stObject);

      for j := 0 to FSQLResult.FieldCount - 1 do
      begin
        rec := TSuperObject.Create(stObject);
        if dfArray then
        begin
          rec.S['name'] := LowerCase(FSQLResult.AliasName[j]);
          Result.asArray.add(rec);
        end else
          Result.AsObject[LowerCase(FSQLResult.AliasName[j])] := rec;

        case FSQLResult.FieldType[j] of
          uftChar, uftVarchar, uftCstring:
          begin
            rec.S['type'] := 'str';
            rec.I['length'] := FSQLResult.SQLLen[j];
          end;
          uftSmallint, uftInteger, uftInt64: rec.S['type'] := 'int';
          uftNumeric, uftFloat, uftDoublePrecision: rec.S['type'] := 'float';
          uftBlob, uftBlobId:
          begin
            if FSQLResult.Data^.sqlvar[j].SqlSubType = 1 then
              rec.S['type'] := 'str' else
              rec.S['type'] := 'bin';
          end;
          uftTimestamp: rec.S['type'] := 'timestamp';
          uftDate: rec.S['type'] := 'date';
          uftTime: rec.S['type'] := 'time';
          {$IFDEF IB7_UP}
          uftBoolean: rec.S['type'] := 'bool';
          {$ENDIF}
        end;
        if not FSQLResult.IsNullable[j] then
          rec.B['notnull'] := true;
      end;
  end else
    Result := nil;
end;

{ TDBUIBConnectionPool }

constructor TDBUIBConnectionPool.Create(const Options: ISuperObject; max: Integer);
begin
  inherited Create(stObject);
  DataPtr := Self;
  AsObject['options'] := Options;
  AsObject['pool'] := TSuperObject.Create(stArray);
  FCriticalSection := TCriticalSection.Create;
  FMax := max;
end;

constructor TDBUIBConnectionPool.Create(const Options: string; max: Integer);
begin
  Create(SO(Options), max);
end;

destructor TDBUIBConnectionPool.Destroy;
begin
  FCriticalSection.Free;
  inherited;
end;

function TDBUIBConnectionPool.GetConnection: IDBConnection;
var
  ar: TSuperArray;
  cnx: ISuperObject;
  j, k: Integer;
begin
  Result := nil;

  FCriticalSection.Enter;
  try
    ar := AsObject['pool'].AsArray;
    while Result = nil do
    begin
      for j := 0 to ar.Length - 1 do
      begin
        cnx := ar.O[j];
        k := cnx._AddRef;
        try
          if k = 3 then
          begin
            Result := cnx as IDBConnection;
            Exit;
          end;
        finally
          cnx._Release;
          cnx := nil;
        end;
      end;
      if (Result = nil) and ((FMax < 1) or (ar.Length < FMax)) then
      begin
        Result := TDBUIBConnection.Create(AsObject['options']);
        ar.Add(Result as ISuperObject);
        Exit;
      end;
    end;
  finally
    FCriticalSection.Leave;
  end;
end;

function TDBUIBConnectionPool.GetSize: Integer;
begin
  FCriticalSection.Enter;
  try
    Result := AsObject['pool'].AsArray.Length;
  finally
    FCriticalSection.Leave;
  end;
end;

end.
