unit dorDB;
{$ifdef FPC}
{$mode ObjFpc}{$H+}
{$endif}
interface
uses superobject, classes, dorUtils;

type
  IDBConnectionPool = interface;
  IDBConnection = interface;
  IDBContext = interface;
  IDBCommand = interface;
  IDBBlob = interface;

  IDBConnectionPool = interface
    ['{27621D9A-AAE9-4E24-82F5-A18D84E415F3}']
    function GetConnection: IDBConnection;
    function GetSize: Integer;
  end;

  IDBConnection = interface
  ['{843E105A-B8E0-42A9-AFA0-CF5AA843DB8B}']
    function newContext(const Options: ISuperObject = nil): IDBContext; overload;
    function newCommand(const Options: ISuperObject = nil): IDBCommand; overload;
    function newContext(const Options: SOString): IDBContext; overload;
    function newCommand(const Options: SOString): IDBCommand; overload;
    function newSelect(const sql: SOString; firstone: boolean = False; asArray: Boolean = False): IDBCommand;
    function newFunction(const sql: SOString): IDBCommand;
    procedure ExecuteImmediate(const Options: SOString); overload;
  end;

  IDBContext = interface
  ['{51992399-2D1A-47EF-9DB1-C5654325F41B}']
    function newCommand(const Options: ISuperObject = nil): IDBCommand; overload;
    function newCommand(const Options: SOString): IDBCommand; overload;
    function newSelect(const sql: SOString; firstone: boolean = False; asArray: Boolean = False): IDBCommand;
    function newFunction(const sql: SOString): IDBCommand;
    procedure ExecuteImmediate(const Options: SOString); overload;
    function Execute(const Command: IDBCommand; const params: ISuperObject = nil): ISuperObject; overload;
    function Execute(const Command: IDBCommand; const params: array of const): ISuperObject; overload;
    function Execute(const Command: IDBCommand; const params: SOString): ISuperObject; overload;
    function Execute(const Command: IDBCommand; const params: Variant): ISuperObject; overload;
  end;

  IDBCommand = interface
  ['{A39B974A-96EA-4047-A57B-A2B3EBE7BABD}']
    function Execute(const params: ISuperObject = nil; const context: IDBContext = nil): ISuperObject; overload;
    function Execute(const params: array of const; const context: IDBContext = nil): ISuperObject; overload;
    function Execute(const params: SOString; const context: IDBContext = nil): ISuperObject; overload;
    function Execute(const params: Variant; const context: IDBContext = nil): ISuperObject; overload;
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
    function newContext(const Options: ISuperObject = nil): IDBContext; overload; virtual; abstract;
    function newContext(const Options: SOString): IDBContext; overload; virtual;
    function newCommand(const Options: ISuperObject = nil): IDBCommand; overload; virtual;
    function newCommand(const Options: SOString): IDBCommand; overload; virtual;
    function newSelect(const sql: SOString; firstone: boolean = False; asArray: Boolean = False): IDBCommand; virtual;
    function newFunction(const sql: SOString): IDBCommand; virtual;
  end;

  TDBContext = class(TSuperObject, IDBContext)
  protected
    procedure ExecuteImmediate(const Options: SOString); virtual; abstract;
    function newCommand(const Options: ISuperObject = nil): IDBCommand; overload; virtual; abstract;
    function newCommand(const Options: SOString): IDBCommand; overload; virtual;
    function newSelect(const sql: SOString; firstone: boolean = False; asArray: Boolean = False): IDBCommand; virtual;
    function newFunction(const sql: SOString): IDBCommand; virtual;
    function Execute(const Command: IDBCommand; const params: ISuperObject = nil): ISuperObject; overload; virtual;
    function Execute(const Command: IDBCommand; const params: array of const): ISuperObject; overload; virtual;
    function Execute(const Command: IDBCommand; const params: SOString): ISuperObject; overload; virtual;
    function Execute(const Command: IDBCommand; const params: Variant): ISuperObject; overload; virtual;
  end;

  TDBCommand = class(TSuperObject, IDBCommand)
  protected
    function Execute(const params: ISuperObject = nil; const context: IDBContext = nil): ISuperObject; overload; virtual; abstract;
    function Execute(const params: array of const; const context: IDBContext = nil): ISuperObject; overload; virtual;
    function Execute(const params: SOString; const context: IDBContext = nil): ISuperObject; overload; virtual;
    function Execute(const params: Variant; const context: IDBContext = nil): ISuperObject; overload; virtual;
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

function TDBConnection.newCommand(const Options: ISuperObject): IDBCommand;
begin
  Result := newContext.newCommand(Options);
end;

procedure TDBConnection.ExecuteImmediate(const Options: SOString);
begin
  newContext.ExecuteImmediate(Options);
end;

function TDBConnection.newCommand(const Options: SOString): IDBCommand;
begin
  Result := newContext.newCommand(Options);
end;

function TDBConnection.newContext(const Options: SOString): IDBContext;
begin
  Result := newContext(TSuperObject.ParseString(PSOChar(Options), false));
end;

function TDBConnection.newFunction(const sql: SOString): IDBCommand;
begin
  Result := newContext.newCommand(SO(['sql', sql, 'function', true]));
end;

function TDBConnection.newSelect(const sql: SOString; firstone: boolean; asArray: Boolean): IDBCommand;
begin
  Result := newContext.newCommand(SO(['sql', sql, 'firstone', firstone, 'array', asArray]));
end;

{ TDBContext }

function TDBContext.Execute(const Command: IDBCommand;
  const params: Variant): ISuperObject;
begin
  Result := Command.Execute(so(params), Self);
end;

function TDBContext.newCommand(const Options: SOString): IDBCommand;
begin
  Result := newCommand(SO(Options));
end;

function TDBContext.newFunction(const sql: SOString): IDBCommand;
begin
  Result := newCommand(SO(['sql', sql, 'function', true]));
end;

function TDBContext.newSelect(const sql: SOString; firstone: boolean; asArray: Boolean): IDBCommand;
begin
  Result := newCommand(SO(['sql', sql, 'firstone', firstone, 'array', asArray]));
end;

function TDBContext.Execute(const Command: IDBCommand;
  const params: SOString): ISuperObject;
begin
  Result := Command.Execute(TSuperObject.ParseString(PSOChar(params), false), Self);
end;

function TDBContext.Execute(const Command: IDBCommand;
  const params: array of const): ISuperObject;
begin
  Result := Command.Execute(SA(params), Self);
end;

function TDBContext.Execute(const Command: IDBCommand;
  const params: ISuperObject = nil): ISuperObject;
begin
  Result := Command.Execute(params, Self);
end;

{ TDBCommand }

function TDBCommand.Execute(const params: Variant;
  const context: IDBContext): ISuperObject;
begin
  Result := Execute(SO(params), context);
end;

function TDBCommand.Execute(const params: SOString;
  const context: IDBContext): ISuperObject;
begin
  Result := Execute(TSuperObject.ParseString(PSOChar(params), false), context);
end;

function TDBCommand.Execute(const params: array of const;
  const context: IDBContext): ISuperObject;
begin
  Result := Execute(SA(params), context);
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
