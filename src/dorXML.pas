unit dorXML;

interface
uses SysUtils, Windows, Classes, Generics.Collections;

type
  IXMLNode = interface;
  TXMLSaveto = reference to procedure(const data: string);
  TXMLNodeType = (ntNull, ntNode, ntText, ntCDATA);
  TXMLNodeList = TList<IXMLNode>;

  IXMLNode = interface
  ['{8A22EC91-113B-48DE-A58B-E920DED5F21D}']
    function GetName: RawByteString;
    function GetAttributes: TDictionary<RawByteString, string>;
    function GetChildNodes: TXMLNodeList;
    function GetText: string;
    procedure SetText(const value: string);
    function FindChildNodes(const name: RawByteString; out ChildNodes: TXMLNodeList): Integer;
    function FirstChild(const name: RawByteString): IXMLNode;
    function GetXML: string;
    procedure SaveToXML(const writer: TXMLSaveto);
    procedure SaveToText(const writer: TXMLSaveto);
    procedure SaveToFile(const filename: string);
    function GetHasAttributes: Boolean;
    function GetHasChildNodes: Boolean;
    function GetDataType: TXMLNodeType;
    procedure SetNullChild(const name: RawByteString; const value: string);
    function GetNullChild(const name: RawByteString): string;
    procedure SetNullAttr(const name: RawByteString; const value: string);
    function GetNullAttr(const name: RawByteString): string;
    function Append(const name: RawByteString): IXMLNode;

    property Name: RawByteString read GetName;
    property Attributes: TDictionary<RawByteString, string> read GetAttributes;
    property ChildNodes: TXMLNodeList read GetChildNodes;
    property Text: string read GetText write SetText;
    property Xml: string read GetXml;
    property HasAttributes: Boolean read GetHasAttributes;
    property HasChildNodes: Boolean read GetHasChildNodes;
    property DataType: TXMLNodeType read GetDataType;
    property NullChild[const name: RawByteString]: string read GetNullChild write SetNullChild;
    property NullAttr[const name: RawByteString]: string read GetNullAttr write SetNullAttr;
  end;

  TXMLNodeNull = class(TInterfacedObject, IXMLNode)
  protected
    class procedure Escape(const str: string; const writer: TXMLSaveto);
    { IXMLNode }
    function GetName: RawByteString; virtual;
    function GetAttributes: TDictionary<RawByteString, string>; virtual;
    function GetChildNodes: TXMLNodeList; virtual;
    function GetText: string; virtual;
    procedure SetText(const value: string); virtual;
    function FindChildNodes(const name: RawByteString; out ChildNodes: TXMLNodeList): Integer; virtual;
    function FirstChild(const name: RawByteString): IXMLNode; virtual;
    function GetXML: string;
    procedure SaveToXML(const writer: TXMLSaveto); virtual;
    procedure SaveToText(const writer: TXMLSaveto); virtual;
    procedure SaveToFile(const filename: string);
    function GetHasAttributes: Boolean; virtual;
    function GetHasChildNodes: Boolean; virtual;
    function GetDataType: TXMLNodeType; virtual;
    procedure SetNullChild(const name: RawByteString; const value: string); virtual;
    function GetNullChild(const name: RawByteString): string; virtual;
    procedure SetNullAttr(const name: RawByteString; const value: string); virtual;
    function GetNullAttr(const name: RawByteString): string; virtual;
    function Append(const name: RawByteString): IXMLNode; virtual;
  public
    property Name: RawByteString read GetName;
    property DataType: TXMLNodeType read GetDataType;
    property Attributes: TDictionary<RawByteString, string> read GetAttributes;
    property ChildNodes: TXMLNodeList read GetChildNodes;
    property Text: string read GetText write SetText;
    property Xml: string read GetXml;
    property HasAttributes: Boolean read GetHasAttributes;
    property HasChildNodes: Boolean read GetHasChildNodes;
    property NullChild[const name: RawByteString]: string read GetNullChild write SetNullChild;
    property NullAttr[const name: RawByteString]: string read GetNullAttr write SetNullAttr;
  end;

  TXMLNodeText = class(TXMLNodeNull)
  private
    FText: string;
  protected
    function GetName: RawByteString; override;
    function GetText: string; override;
    procedure SetText(const value: string); override;
    procedure SaveToXML(const writer: TXMLSaveto); override;
    procedure SaveToText(const writer: TXMLSaveto); override;
    function GetDataType: TXMLNodeType; override;
  public
    constructor Create(const value: string); virtual;
  end;

  TXMLNodeCDATA = class(TXMLNodeNull)
  private
    FCDATA: string;
  protected
    function GetName: RawByteString; override;
    function GetText: string; override;
    procedure SetText(const value: string); override;
    procedure SaveToXML(const writer: TXMLSaveto); override;
    procedure SaveToText(const writer: TXMLSaveto); override;
    function GetDataType: TXMLNodeType; override;
  public
    constructor Create(const value: string); virtual;
  end;

  TXMLNode = class(TXMLNodeNull)
  private
    FName: RawByteString;
    FAttributes: TDictionary<RawByteString, string>;
    FChildNodes: TXMLNodeList;
    FChildIndex: TObjectDictionary<RawByteString, TXMLNodeList>;
    procedure doNotifyChild(Sender: TObject; const Item: IXMLNode;
      Action: TCollectionNotification);
    procedure doNotifyAttr(Sender: TObject; const Item: RawByteString;
      Action: TCollectionNotification);
  protected
    function GetName: RawByteString; override;
    function GetAttributes: TDictionary<RawByteString, string>; override;
    function GetChildNodes: TXMLNodeList; override;
    procedure SetText(const value: string); override;
    function FindChildNodes(const name: RawByteString; out ChildNodes: TXMLNodeList): Integer; override;
    function FirstChild(const name: RawByteString): IXMLNode; override;
    procedure SaveToXML(const writer: TXMLSaveto); override;
    procedure SaveToText(const writer: TXMLSaveto); override;
    function GetHasAttributes: Boolean; override;
    function GetHasChildNodes: Boolean; override;
    function GetDataType: TXMLNodeType; override;
    procedure SetNullChild(const name: RawByteString; const value: string); override;
    function GetNullChild(const name: RawByteString): string; override;
    procedure SetNullAttr(const name: RawByteString; const value: string); override;
    function GetNullAttr(const name: RawByteString): string; override;
    function Append(const name: RawByteString): IXMLNode; override;
  public
    constructor Create(const name: RawByteString); virtual;
    destructor Destroy; override;
  end;

  TXMLNodeClass = class of TXMLNode;

  TXMLReader = reference to function(var c: AnsiChar): Boolean;
  TXMLNodeState = (xtOpen, xtClose, xtAttribute, xtText, xtCData);
  TXMLEvent = reference to function(node: TXMLNodeState; const name: RawByteString; const value: string): Boolean;

  function XMLParseSAX(const reader: TXMLReader; const event: TXMLEvent; cp: Cardinal = CP_ACP): Boolean;
  function XMLParse(const reader: TXMLReader; cp: Cardinal = CP_ACP): IXMLNode;
  function XMLParseStream(stream: TStream; cp: Cardinal = CP_ACP): IXMLNode;
  function XMLParseFile(const filename: TFileName; cp: Cardinal = CP_ACP): IXMLNode;
  function XMLParseString(const str: AnsiString): IXMLNode; overload;
  function XMLParseString(const str: string): IXMLNode; overload;

implementation
uses Math;

function ParseCP(const cp: PAnsiChar): Integer;
var
  len: Integer;
begin
  Result := CP_ACP;
  len := StrLen(cp);
  case len of
    4: if StrLIComp(cp, 'big5', 4) = 0 then Exit(950);
    5: case cp[4] of
         '7': if StrLIComp(cp, 'utf-7', 5) = 0 then Exit(CP_UTF7);
         '8': if StrLIComp(cp, 'utf-8', 5) = 0 then Exit(CP_UTF8);
       end;
    6: case cp[5] of
         'p', 'P': if StrLIComp(cp, 'euc-jp', 6) = 0 then Exit(51932);
         'r', 'R':
           case cp^ of
             'e', 'E': if StrLIComp(cp, 'euc-kr', 6) = 0 then Exit(51949);
             'k', 'K': if StrLIComp(cp, 'koi8-r', 6) = 0 then Exit(20866);
           end;
         '2': if StrLIComp(cp, 'gb2312', 6) = 0 then Exit(936);
         'u', 'U': if StrLIComp(cp, 'koi8-u', 6) = 0 then Exit(21866);
       end;
    8: if StrLIComp(cp, 'us-ascii', 8) = 0 then Exit(20127);
    9: if StrLIComp(cp, 'shift_jis', 9) = 0 then Exit(932);
    10: case cp[9] of
          '1': if StrLIComp(cp, 'iso-8859-1', 10) = 0 then Exit(28591);
          '2': case cp[8] of
                 '-': if StrLIComp(cp, 'iso-8859-2', 10) = 0 then Exit(28592);
                 '1': if StrLIComp(cp, 'hz-gb-2312', 10) = 0 then Exit(52936);
               end;
          '3': if StrLIComp(cp, 'iso-8859-3', 10) = 0 then Exit(28593);
          '4': if StrLIComp(cp, 'iso-8859-4', 10) = 0 then Exit(28594);
          '5': if StrLIComp(cp, 'iso-8859-5', 10) = 0 then Exit(28595);
          '6': if StrLIComp(cp, 'iso-8859-6', 10) = 0 then Exit(28596);
          '7': if StrLIComp(cp, 'iso-8859-7', 10) = 0 then Exit(28597);
          '9': if StrLIComp(cp, 'iso-8859-9', 10) = 0 then Exit(28599);
        end;
    11: case cp[5] of
          '2': if StrLIComp(cp, 'csiso2022jp', 11) = 0 then Exit(50221);
          '0': if StrLIComp(cp, 'iso-2022-jp', 11) = 0 then Exit(50220);
          '8': if StrLIComp(cp, 'iso-8859-15', 11) = 0 then Exit(28605);
          'w', 'W': if StrLIComp(cp, 'windows-874', 11) = 0 then Exit(874);
        end;
    12: case cp[11] of
          '0': if StrLIComp(cp, 'windows-1250', 12) = 0 then Exit(1250);
          '1': if StrLIComp(cp, 'windows-1251', 12) = 0 then Exit(1251);
          '2': if StrLIComp(cp, 'Windows-1252', 12) = 0 then Exit(1252);
          '3': if StrLIComp(cp, 'windows-1253', 12) = 0 then Exit(1253);
          '4': if StrLIComp(cp, 'windows-1254', 12) = 0 then Exit(1254);
          '5': if StrLIComp(cp, 'windows-1255', 12) = 0 then Exit(1255);
          '6': if StrLIComp(cp, 'windows-1256', 12) = 0 then Exit(1256);
          '7': if StrLIComp(cp, 'windows-1257', 12) = 0 then Exit(1257);
          '8': if StrLIComp(cp, 'windows-1258', 12) = 0 then Exit(1258);
          'i', 'I': if StrLIComp(cp, 'iso-8859-8-i', 12) = 0 then Exit(38598);
        end;
    14: if StrLIComp(cp, 'ks_c_5601-1987', 14) = 0 then Exit(949);
  end;
end;

type
  TWriterString = class
  private
    FBuf: PAnsiChar;
    FBPos: integer;
    FSize: integer;
  public
    function Append(buf: PAnsiChar; Size: Integer): Integer; overload;
    function Append(buf: PAnsiChar): Integer; overload;
    procedure Reset;
    procedure TrimRight;
    constructor Create; virtual;
    destructor Destroy; override;
    property Data: PAnsiChar read FBuf;
    property Size: Integer read FSize;
    property Position: integer read FBPos;
  end;

  function TWriterString.Append(buf: PAnsiChar; Size: Integer): Integer;
  begin
    Result := size;
    if Size > 0 then
    begin
      if (FSize - FBPos <= size) then
      begin
        FSize := max(FSize * 2, FBPos + size + 8);
        ReallocMem(FBuf, FSize);
      end;
      // fast move
      case size of
      1: FBuf[FBPos] := buf^;
      2: PWord(@FBuf[FBPos])^ := PWord(buf)^;
      4: PInteger(@FBuf[FBPos])^ := PInteger(buf)^;
      8: PInt64(@FBuf[FBPos])^ := PInt64(buf)^;
      else
        move(buf^, FBuf[FBPos], size);
      end;
      inc(FBPos, size);
      FBuf[FBPos] := #0;
    end;
  end;

  function TWriterString.Append(buf: PAnsiChar): Integer;
  begin
    Result := Append(buf, strlen(buf));
  end;

  procedure TWriterString.Reset;
  begin
    FBuf[0] := #0;
    FBPos := 0;
  end;

  procedure TWriterString.TrimRight;
  begin
    while (FBPos > 0) and (FBuf[FBPos-1] in [#32, #13, #10]) do
    begin
      dec(FBPos);
      FBuf[FBPos] := #0;
    end;
  end;

  constructor TWriterString.Create;
  begin
    FSize := 32;
    FBPos := 0;
    GetMem(FBuf, FSize);
  end;

  destructor TWriterString.Destroy;
  begin
    inherited;
    if FBuf <> nil then
      FreeMem(FBuf)
  end;

(******************************************************************************)
(* XMLParseSAX                                                              *)
(******************************************************************************)

function XMLParseSAX(const reader: TXmlReader; const event: TXMLEvent; cp: Cardinal): Boolean;
const
  spaces = [#32,#9,#10,#13];
  alphas = ['a'..'z', 'A'..'Z', '_', ':', #161..#255];
  nums = ['0'..'9', '.', '-'];
  hex = nums + ['a'..'f','A'..'F'];
  alphanums = alphas + nums;
  publitteral = [#32, #13, #10, 'a'..'z', 'A'..'Z', '0'..'9', '-', '''', '"', '(', ')',
    '+', ',', '.', '/', ':', '=', '?', ';', '!', '*', '#', '@', '$', '_', '%'];

  const
    XML_SPACE : PAnsiChar = #32;
    XML_ARR: PAnsiChar = ']';
    XML_BIG: PAnsiChar = '>';
    XML_LOW: PAnsiChar = '<';
    XML_AMP: PAnsiChar = '&';
    XML_SQU: PAnsiChar = '''';
    XML_DQU: PAnsiChar = '"';

  type
    TXMLState = (xsStart, xsEatSpaces, xsElement, xsElementName, xsAttributes,
      xsAttributeName, xsEqual, xsAttributeValue, xsCloseEmptyElement,
      xsTryCloseElement, xsCloseElementName, xsChildNodes, xsElementString,
      xsElementComment, xsCloseElementComment, xsElementPI, xsElementDataPI,
      xsCloseElementPI, xsElementCDATA, xsClodeElementCDATA, xsEscape,
      xsEscape_lt, xsEscape_gt, xsEscape_amp, xsEscape_apos, xsEscape_quot,
      xsEscape_char, xsEscape_char_num, xsEscape_char_hex, xsEnd);

    TXMLError = (xeSuccess, xeContinue, xeProcessInst, xeError);
    TXMLClass = (xcNone, xcElement, xcComment, xcString, xcCdata, xcDocType, xcProcessInst);

    PXMLStack = ^TXMLStack;
    TXMLStack = record
      state: TXMLState;
      savedstate: TXMLState;
      prev: PXMLStack;
      next: PXMLStack;
      clazz: TXMLClass;
      name: RawByteString;
    end;

var
  Stack: PXMLStack;
  Str: TWriterString;
  Value: TWriterString;
  Position: Integer;
  AChar: AnsiChar;

  procedure StackUp;
  var
    st: PXMLStack;
  begin
    New(st);
    st^.state := xsEatSpaces;
    st^.savedstate := xsStart;
    st^.prev := Stack;
    if st^.prev <> nil then
      st^.prev^.next := st;
    st^.next := nil;
    Stack := st;
  end;

  procedure StackDown;
  var
    prev: PXMLStack;
  begin
    if Stack <> nil then
    begin
      prev := Stack^.prev;
      Dispose(Stack);
      Stack := prev;
      if Stack <> nil then
        Stack^.next := nil;
    end;
  end;

  function hexdigit(const x: AnsiChar): byte;
  begin
    if x <= '9' then
      Result := byte(x) - byte('0') else
      Result := (byte(x) and 7) + 9;
  end;

  function MBUDecode(const str: PAnsiChar): string;
  begin
    if cp > 0 then
    begin
      SetLength(Result, MultiByteToWideChar(cp, 0, str, StrLen(str), nil, 0));
      MultiByteToWideChar(cp, 0, str, StrLen(str), PWideChar(Result), Length(Result));
    end else
      Result := string(str);
  end;

var
  c: AnsiChar;
label
  redo;
begin
  Stack := nil;
  Str := TWriterString.Create;
  Value := TWriterString.Create;
  AChar := #0;
  StackUp;

  try
    while reader(c) do
    begin
redo:
      case Stack^.state of
        xsEatSpaces:
          if (c in spaces) then {nop} else
          begin
            Stack^.state := Stack^.savedstate;
            goto redo;
          end;
        xsStart:
            case c of
              '<': Stack^.state := xsElement;
            else
              Exit(False);
            end;
        xsElement:
          begin
            case c of
              '?':
                begin
                  Stack^.savedstate := xsStart;
                  Stack^.state := xsEatSpaces;
                  StackUp;
                  Str.Reset;
                  Stack^.state := xsElementPI;
                  Stack^.clazz := xcProcessInst;
                end;
              '!':
                begin
                  Position := 0;
                  Stack^.state := xsElementComment;
                  Stack^.clazz := xcComment;
                end;
            else
              if (c in alphas) then
              begin
                Str.Reset;
                Stack^.state := xsElementName;
                Stack^.clazz := xcElement;
                goto redo;
              end else
                Exit(False);
            end;
          end;
        xsElementPI:
          begin
            if (c in alphanums) then
              Str.Append(@c, 1) else
              begin
                Stack^.state := xsEatSpaces;
                if Str.Data = 'xml' then
                  Stack^.savedstate := xsAttributes else
                  begin
                    Value.Reset;
                    Stack^.savedstate := xsElementDataPI;
                  end;
                goto redo;
              end;
          end;
        xsElementDataPI:
          begin
            case c of
              '?': Stack^.state := xsCloseElementPI;
            else
              Value.Append(@c, 1);
            end;
          end;
        xsCloseElementPI:
          begin
            if (c <> '>') then Exit(False);
            StackDown;
          end;
        xsElementName:
          begin
            if (c in alphanums) then
              Str.Append(@c, 1) else
              begin
                Stack.name := Str.Data;
                if not event(xtOpen, Str.Data, '') then Exit(False);
                Stack^.state := xsEatSpaces;
                Stack^.savedstate := xsAttributes;
                goto redo;
              end;
          end;
        xsChildNodes:
          begin
            case c of
              '<': Stack^.state := xsTryCloseElement;
            else
              Value.Reset;
              Stack^.state := xsElementString;
              Stack^.clazz := xcString;
              goto redo;
            end;
          end;
        xsCloseEmptyElement:
          begin
            case c of
              '>':
                begin
                  if not event(xtClose, '', '') then Exit(False);
                  if Stack.prev = nil then
                  begin
                    Stack^.state := xsEnd;
                    Break;
                  end;
                  Stack^.state := xsEatSpaces;
                  Stack^.savedstate := xsEnd;
                end
            else
              Exit(False);
            end;
          end;
        xsTryCloseElement:
          begin
            case c of
              '/': begin
                     Stack^.state := xsCloseElementName;
                     Position := 0;
                     Str.Reset;
                     Str.Append(PAnsiChar(Stack.name));
                   end;
              '!': begin
                     Position := 0;
                     Stack^.state := xsElementComment;
                     Stack^.clazz := xcComment;
                   end;
              '?': begin
                     Stack^.savedstate := xsChildNodes;
                     Stack^.state := xsEatSpaces;
                     StackUp;
                     Str.Reset;
                     Stack^.state := xsElementPI;
                     Stack^.clazz := xcProcessInst;
                   end
            else
              Stack^.state := xsChildNodes;
              StackUp;
              if (c in alphas) then
              begin
                Str.Reset;
                Stack^.state := xsElementName;
                Stack^.clazz := xcElement;
                goto redo;
              end else
                Exit(False);
            end;
          end;
        xsCloseElementName:
          begin
            if Str.Position = Position then
            begin
              Stack^.savedstate := xsCloseEmptyElement;
              Stack^.state := xsEatSpaces;
              goto redo;
            end else
            begin
              if (c <> Str.Data[Position]) then Exit(False);
              inc(Position);
            end;
          end;
        xsAttributes:
          begin
            case c of
              '?': begin
                    if Stack^.clazz <> xcProcessInst then Exit(False);
                    Stack^.state := xsCloseElementPI;
                   end;
              '/': begin
                     Stack^.state := xsCloseEmptyElement;
                   end;
              '>': begin
                     Stack^.state := xsEatSpaces;
                     Stack^.savedstate := xsChildNodes;
                   end
            else
              if (c in alphas) then
              begin
                Str.Reset;
                Str.Append(@c, 1);
                Stack^.state := xsAttributeName;
              end else
                Exit(False);
            end;
          end;
        xsAttributeName:
          begin
            if (c in alphanums) then
              Str.Append(@c, 1) else
            begin
              Stack^.state := xsEatSpaces;
              Stack^.savedstate := xsEqual;
              goto redo;
            end;
          end;
        xsEqual:
          begin
            if c <> '=' then Exit(False);
            Stack^.state := xsEatSpaces;
            Stack^.savedstate := xsAttributeValue;
            Value.Reset;
            Position := 0;
            AChar := #0;
          end;
        xsAttributeValue:
          begin
            if AChar <> #0 then
            begin
              if (c = AChar) then
                begin
                  if Stack.clazz = xcProcessInst then
                  begin
                    if Str.Data = 'encoding' then
                      cp := ParseCP(Value.Data);
                  end else
                    if not event(xtAttribute, Str.Data, MBUDecode(Value.Data)) then Exit(False);
                  Stack^.savedstate := xsAttributes;
                  Stack^.state := xsEatSpaces;
                end else
              case c of
                '&':
                  begin
                    Stack^.state := xsEscape;
                    Stack^.savedstate := xsAttributeValue;
                  end;
                #13, #10:
                  begin
                    Value.TrimRight;
                    Value.Append(XML_SPACE, 1);
                    Stack^.state := xsEatSpaces;
                    Stack^.savedstate := xsAttributeValue;
                  end;
              else
                Value.Append(@c, 1);
              end;
            end else
            begin
              if c in ['"', ''''] then
              begin
                AChar := c;
                inc(Position);
              end else
                Exit(False);
            end;
          end;
        xsElementString:
          begin
            case c of
              '<': begin
                     Value.TrimRight;
                     if not event(xtText, '', MBUDecode(Value.Data)) then Exit(False);
                     Stack^.state := xsTryCloseElement;
                   end;
              #13, #10:
                begin
                  Value.TrimRight;
                  Value.Append(XML_SPACE, 1);
                  Stack^.state := xsEatSpaces;
                  Stack^.savedstate := xsElementString;
                end;
              '&':
                begin
                  Stack^.state := xsEscape;
                  Stack^.savedstate := xsElementString;
                end
            else
              Value.Append(@c, 1);
            end;
          end;
        xsElementComment:
          begin
            case Position of
              0:
                begin
                  case c of
                    '-': Inc(Position);
                    '[':
                      begin
                        Value.Reset;
                        Position := 0;
                        Stack^.state := xsElementCDATA;
                        Stack^.clazz := xcCdata;
                      end;
                  else
                    Exit(False);
                  end;
                end;
              1:
                begin
                  if c <> '-' then Exit(False);
                  Inc(Position);
                end;
            else
              if c = '-' then
              begin
                Position := 0;
                Stack^.state := xsCloseElementComment;
              end;
            end;
          end;
        xsCloseElementComment:
          begin
            case Position of
            0: begin
                 if c <> '-' then
                 begin
                   Position := 2;
                   Stack^.state := xsElementComment;
                 end else
                   Inc(Position);
               end;
            1: begin
                 if c <> '>' then Exit(False);
                 Stack^.state := xsEatSpaces;
                 if Stack^.prev <> nil then
                    Stack^.savedstate := xsChildNodes else
                    Stack^.savedstate := xsStart;
               end;
            end;
          end;
        xsElementCDATA:
          begin
            case Position of
              0: if (c = 'C') then inc(Position) else Exit(False);
              1: if (c = 'D') then inc(Position) else Exit(False);
              2: if (c = 'A') then inc(Position) else Exit(False);
              3: if (c = 'T') then inc(Position) else Exit(False);
              4: if (c = 'A') then inc(Position) else Exit(False);
              5: if (c = '[') then inc(Position) else Exit(False);
            else
              case c of
                ']': begin
                       Position := 0;
                       Stack^.state := xsClodeElementCDATA;
                     end;
              else
                Value.Append(@c, 1);
              end;
            end;
          end;
        xsClodeElementCDATA:
          begin
            case Position of
              0: if (c = ']') then
                   inc(Position) else
                   begin
                     Value.Append(XML_ARR, 1);
                     Value.Append(@c, 1);
                     Position := 6;
                     Stack^.state := xsElementCDATA;
                   end;
              1: case c of
                 '>':
                   begin
                     if not event(xtCData, '', MBUDecode(Value.Data)) then Exit(False);
                     Stack^.state := xsEatSpaces;
                     Stack^.savedstate := xsChildNodes;
                   end;
                 ']':
                   begin
                     Value.Append(@c, 1);
                   end;
              else
                Value.Append(@c, 1);
                Stack^.state := xsElementCDATA;
              end;
            end;
          end;
        xsEscape:
          begin
            Position := 0;
            case c of
              'l': Stack^.state := xsEscape_lt;
              'g': Stack^.state := xsEscape_gt;
              'a': Stack^.state := xsEscape_amp;
              'q': Stack^.state := xsEscape_quot;
              '#': Stack^.state := xsEscape_char;
            else
              Exit(False);
            end;
          end;
        xsEscape_lt:
          begin
            case Position of
              0: begin
                   if c <> 't' then Exit(False);
                   Inc(Position);
                 end;
              1: begin
                   if c <> ';' then Exit(False);
                   Value.Append(XML_LOW, 1);
                   Stack^.state := Stack^.savedstate;
                 end;
            end;
          end;
        xsEscape_gt:
          begin
            case Position of
              0: begin
                   if c <> 't' then Exit(False);
                   Inc(Position);
                 end;
              1: begin
                   if c <> ';' then Exit(False);
                   Value.Append(XML_BIG, 1);
                   Stack^.state := Stack^.savedstate;
                 end;
            end;
          end;
        xsEscape_amp:
          begin
            case Position of
              0: begin
                   case c of
                     'm': Inc(Position);
                     'p': begin
                            Stack^.state := xsEscape_apos;
                            Inc(Position);
                          end;
                   else
                     Exit(False);
                   end;
                 end;
              1: begin
                   if c <> 'p' then Exit(False);
                   Inc(Position);
                 end;
              2: begin
                   if c <> ';' then Exit(False);
                   Value.Append(XML_AMP, 1);
                   Stack^.state := Stack^.savedstate;
                 end;
            end;
          end;
        xsEscape_apos:
          begin
            case Position of
              0: begin
                   case c of
                     'p': Inc(Position);
                     'm': begin
                            Stack^.state := xsEscape_amp;
                            Inc(Position);
                          end;
                   else
                     Exit(False);
                   end;
                 end;
              1: begin
                   if c <> 'o' then Exit(False);
                   Inc(Position);
                 end;
              2: begin
                   if c <> 's' then Exit(False);
                   Inc(Position);
                 end;
              3: begin
                   if c <> ';' then Exit(False);
                   Value.Append(XML_SQU, 1);
                   Stack^.state := Stack^.savedstate;
                 end;
            end;
          end;
        xsEscape_quot:
          begin
            case Position of
              0: begin
                   if c <> 'u' then Exit(False);
                   Inc(Position);
                 end;
              1: begin
                   if c <> 'o' then Exit(False);
                   Inc(Position);
                 end;
              2: begin
                   if c <> 't' then Exit(False);
                   Inc(Position);
                 end;
              3: begin
                   if c <> ';' then Exit(False);
                   Value.Append(XML_DQU, 1);
                   Stack^.state := Stack^.savedstate;
                 end;
            end;
          end;
        xsEscape_char:
          begin
            case AnsiChar(c) of
              '0'..'9':
                begin
                  Position := Ord(c) - 48;
                  Stack^.state := xsEscape_char_num;
                end;
              'x':
                begin
                  Stack^.state := xsEscape_char_hex;
                end
            else
              Exit(False);
            end;
          end;
        xsEscape_char_num:
          begin
            case AnsiChar(c) of
              '0'..'9':Position := (Position * 10) + (Ord(c) - 48);
              ';': begin
                     Value.Append(@Position, 1);
                     Stack^.state := Stack^.savedstate;
                   end;
            else
              Exit(False);
            end;
          end;
        xsEscape_char_hex:
          begin
            if (AnsiChar(c) in hex) then
            begin
              Position := (Position * 16) + hexdigit(c);
            end else
            if c = ';' then
            begin
              Value.Append(@Position, 1);
              Stack^.state := Stack^.savedstate;
            end else
              Exit(False);
          end;
        xsEnd:
          begin
            StackDown;
            goto redo;
          end;
      end;
    end;
    Result := Stack^.state = xsEnd;
  finally
    while Stack <> nil do
      StackDown;
    Str.Free;
    Value.Free;
  end;
end;

function XMLParse(const reader: TXMLReader; cp: Cardinal): IXMLNode;
var
  stack: TStack<IXMLNode>;
  n: IXMLNode;
begin
  stack := TStack<IXMLNode>.Create;
  try
    if XMLParseSAX(
      function (var c: AnsiChar): Boolean
      begin
        Result := reader(c);
      end,
      function(node: TXMLNodeState; const name: RawByteString; const value: string): Boolean
      begin
        Result := True;
        case node of
          xtOpen: stack.Push(TXMLNode.Create(name));
          xtClose:
            begin
              n := stack.Pop;
              if stack.Count > 0 then
                stack.Peek.ChildNodes.Add(n);
            end;
          xtAttribute: stack.Peek.Attributes.AddOrSetValue(name, value);
          xtText:
            stack.Peek.ChildNodes.Add(TXMLNodeText.Create(value));
          xtCData:
            stack.Peek.ChildNodes.Add(TXMLNodeCDATA.Create(value));
        end;
      end, cp) then
      begin
        Assert(n <> nil);
        Result := n;
      end else
        Result := nil;
  finally
    stack.Free;
  end;
end;

function XMLParseStream(stream: TStream; cp: Cardinal): IXMLNode;
begin
  Result := XMLParse(function(var c: AnsiChar): Boolean
    begin
      Result := stream.Read(c, 1) = 1
    end, cp);
end;

function XMLParseFile(const filename: TFileName; cp: Cardinal): IXMLNode;
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
  try
    Result := XMLParseStream(stream, cp);
  finally
    stream.Free;
  end;
end;

function XMLParseString(const str: AnsiString): IXMLNode;
var
  p: PAnsiChar;
begin
  p := PAnsiChar(str);
  Result := XMLParse(function(var c: AnsiChar): Boolean
    begin
      if p^ <> #0 then
      begin
        c := p^;
        inc(p);
        Result := True;
      end else
        Result := False;
    end, StringCodePage(str));
end;

function XMLParseString(const str: string): IXMLNode;
begin
  Result := XMLParseString(AnsiString(UTF8String(str)));
end;

{ TXMLNodeNull }

function TXMLNodeNull.FirstChild(const name: RawByteString): IXMLNode;
begin
  Result := nil;
end;

function TXMLNodeNull.Append(const name: RawByteString): IXMLNode;
begin
  Result := nil;
end;

class procedure TXMLNodeNull.Escape(const str: string; const writer: TXMLSaveto);
var
  p1, p2: PChar;
  procedure push(const data: string);
  begin
    if p2 > p1 then
      writer(Copy(p1, 0, p2-p1));
    Inc(p2);
    p1 := p2;
    if data <> '' then
      writer(data);
  end;
begin
  p1 := PChar(str);
  p2 := p1;

  while True do
    case p2^ of
      '<': push('&lt;');
      '>': push('&gt;');
      '&': push('&amp;');
      '"': push('&quot;');
      #0 :
        begin
          push('');
          Break;
        end;
    else
      inc(p2);
    end;
end;

function TXMLNodeNull.FindChildNodes(const name: RawByteString;
  out ChildNodes: TXMLNodeList): Integer;
begin
  ChildNodes := nil;
  Result := 0;
end;

function TXMLNodeNull.GetAttributes: TDictionary<RawByteString, string>;
begin
  Result := nil;
end;

function TXMLNodeNull.GetChildNodes: TXMLNodeList;
begin
  Result := nil;
end;

function TXMLNodeNull.GetHasAttributes: Boolean;
begin
  Result := False;
end;

function TXMLNodeNull.GetHasChildNodes: Boolean;
begin
  Result := False;
end;

function TXMLNodeNull.GetName: RawByteString;
begin
  Result := '#null';
end;

function TXMLNodeNull.GetDataType: TXMLNodeType;
begin
  Result := ntNull;
end;

function TXMLNodeNull.GetNullAttr(const name: RawByteString): string;
begin
  Result := '';
end;

function TXMLNodeNull.GetNullChild(const name: RawByteString): string;
begin
  Result := '';
end;

function TXMLNodeNull.GetText: string;
var
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  try
    SaveToText(procedure (const data: string)
      begin sb.Append(data) end);
    result := sb.ToString;
  finally
    sb.Free;
  end;
end;

function TXMLNodeNull.GetXML: string;
var
  sb: TStringBuilder;
begin
  sb := TStringBuilder.Create;
  try
    SaveToXML(procedure (const data: string)
      begin sb.Append(data) end);
    result := sb.ToString;
  finally
    sb.Free;
  end;
end;

procedure TXMLNodeNull.SaveToFile(const filename: string);
var
  utf: UTF8String;
  stream: TFileStream;
begin
  utf := '<?xml version="1.0" encoding="utf-8"?>';
  stream := TFileStream.Create(filename, fmCreate);
  try
    stream.Write(PAnsiChar(utf)^, Length(utf));
    SaveToXML(procedure(const data: string) begin
      utf := UTF8String(data);
      stream.Write(PAnsiChar(utf)^, Length(utf));
    end);
  finally
    stream.Free;
  end;
end;

procedure TXMLNodeNull.SaveToText(const writer: TXMLSaveto);
begin

end;

procedure TXMLNodeNull.SaveToXML(const writer: TXMLSaveto);
begin

end;

procedure TXMLNodeNull.SetNullAttr(const name: RawByteString;
  const value: string);
begin

end;

procedure TXMLNodeNull.SetNullChild(const name: RawByteString;
  const value: string);
begin

end;

procedure TXMLNodeNull.SetText(const value: string);
begin

end;

{ TXMLNodeText }

constructor TXMLNodeText.Create(const value: string);
begin
  FText := value;
end;

function TXMLNodeText.GetName: RawByteString;
begin
  Result := '#text';
end;

function TXMLNodeText.GetText: string;
begin
  Result := FText;
end;

function TXMLNodeText.GetDataType: TXMLNodeType;
begin
  Result := ntText;
end;

procedure TXMLNodeText.SaveToText(const writer: TXMLSaveto);
begin
  Writer(FText);
end;

procedure TXMLNodeText.SaveToXML(const writer: TXMLSaveto);
begin
  Escape(FText, writer);
end;

procedure TXMLNodeText.SetText(const value: string);
begin
  FText := value;
end;

{ TXMLNodeCDATA }

constructor TXMLNodeCDATA.Create(const value: string);
begin
  FCDATA := value;
end;

function TXMLNodeCDATA.GetName: RawByteString;
begin
  Result := '#cdata';
end;

function TXMLNodeCDATA.GetText: string;
begin
  Result := FCDATA;
end;

function TXMLNodeCDATA.GetDataType: TXMLNodeType;
begin
  Result := ntCDATA;
end;

procedure TXMLNodeCDATA.SaveToText(const writer: TXMLSaveto);
begin
  writer(FCDATA);
end;

procedure TXMLNodeCDATA.SaveToXML(const writer: TXMLSaveto);
begin
  writer('<![CDATA[');
  SaveToText(writer);
  writer(']]>');
end;

procedure TXMLNodeCDATA.SetText(const value: string);
begin
  FCDATA := value;
end;

{ TXMLNode }

function TXMLNode.Append(const name: RawByteString): IXMLNode;
begin
  if name <> '' then
  begin
    Result := TXMLNode.Create(name);
    ChildNodes.Add(Result);
  end else
    Result := nil;
end;

constructor TXMLNode.Create(const name: RawByteString);
begin
  FName := name;
  FAttributes := nil;
  FChildNodes := nil;
  FChildIndex := nil;
end;

destructor TXMLNode.Destroy;
begin
  inherited;
  if FAttributes <> nil then
  begin
    FAttributes.OnKeyNotify := nil;
    FAttributes.Free;
  end;
  if FChildNodes <> nil then
  begin
    FChildNodes.OnNotify := nil;
    FChildNodes.Free;
  end;
  if FChildIndex <> nil then
    FChildIndex.Free;
end;

function TXMLNode.GetName: RawByteString;
begin
  Result := FName;
end;

function TXMLNode.GetDataType: TXMLNodeType;
begin
  Result := ntNode;
end;

function TXMLNode.GetNullAttr(const name: RawByteString): string;
begin
  if not((FAttributes <> nil) and FAttributes.TryGetValue(name, Result)) then
    Result := '';
end;

function TXMLNode.GetNullChild(const name: RawByteString): string;
var
  node: IXMLNode;
begin
  node := FirstChild(name);
  if node <> nil then
    Result := node.Text else
    Result := '';
end;

function TXMLNode.GetAttributes: TDictionary<RawByteString, string>;
begin
  if FAttributes = nil then
  begin
    FAttributes := TDictionary<RawByteString, string>.Create;
    FAttributes.OnKeyNotify := doNotifyAttr;
  end;
  Result := FAttributes;
end;

procedure TXMLNode.doNotifyAttr(Sender: TObject; const Item: RawByteString;
  Action: TCollectionNotification);
begin
  if FAttributes.Count = 0 then
  begin
    FAttributes.Free;
    FAttributes := nil;
  end;
end;

procedure TXMLNode.doNotifyChild(Sender: TObject; const Item: IXMLNode;
  Action: TCollectionNotification);
var
  lst: TXMLNodeList;
begin
  case Action of
    cnAdded:
      begin
        if not FChildIndex.TryGetValue(Item.Name, lst) then
        begin
          lst := TXMLNodeList.Create;
          FChildIndex.Add(Item.Name, lst);
        end;
        lst.Add(Item)
      end;
    cnRemoved, cnExtracted:
      if FChildIndex.TryGetValue(Item.Name, lst) then
        lst.Remove(Item);
  end;
  if FChildNodes.Count = 0 then
  begin
    FChildNodes.Free;
    FChildNodes := nil;
    FChildIndex.Free;
    FChildIndex := nil;
  end;
end;

function TXMLNode.GetChildNodes: TXMLNodeList;
begin
  if FChildNodes = nil then
  begin
    FChildNodes := TXMLNodeList.Create;
    FChildIndex := TObjectDictionary<RawByteString, TXMLNodeList>.Create([doOwnsValues]);
    FChildNodes.OnNotify := doNotifyChild;
  end;
  Result := FChildNodes;
end;

function TXMLNode.GetHasAttributes: Boolean;
begin
  Result := (FAttributes <> nil) and (FAttributes.Count > 0);
end;

function TXMLNode.GetHasChildNodes: Boolean;
begin
  Result := (FChildNodes <> nil) and (FChildNodes.Count > 0);
end;

procedure TXMLNode.SaveToText(const writer: TXMLSaveto);
var
  node: IXMLNode;
begin
  if HasChildNodes then
    for node in ChildNodes do
      node.SaveToText(writer);
end;

procedure TXMLNode.SaveToXML(const writer: TXMLSaveto);
var
  atr: TPair<RawByteString, string>;
  node: IXMLNode;
begin
  writer('<');
  writer(string(FName));
  if HasAttributes then
    for atr in Attributes do
    begin
      writer(' ');
      writer(string(atr.Key));
      writer('=');
      writer('"');
      escape(atr.Value, writer);
      writer('"');
    end;

  if HasChildNodes then
  begin
    writer('>');
    for node in ChildNodes do
      node.SaveToXML(writer);
    writer('</');
    writer(string(FName));
    writer('>');
  end else
    writer('/>');
end;

procedure TXMLNode.SetNullAttr(const name: RawByteString; const value: string);
begin
  if value <> '' then
    Attributes.AddOrSetValue(name, value) else
    if FAttributes <> nil then
      FAttributes.Remove(name);
end;

procedure TXMLNode.SetNullChild(const name: RawByteString; const value: string);
var
  node: IXMLNode;
begin
  node := FirstChild(name);
  if node <> nil then
  begin
    if value <> '' then
      node.Text := value else
      ChildNodes.Remove(node);
  end else
    if value <> '' then
    begin
      node := TXMLNode.Create(name);
      node.Text := value;
      ChildNodes.Add(node);
    end;
end;


procedure TXMLNode.SetText(const value: string);
begin
  if FChildNodes <> nil then
  begin
    FChildNodes.OnNotify := nil;
    try
      FChildNodes.Clear;
      FChildIndex.Clear;
    finally
      FChildNodes.OnNotify := doNotifyChild;
    end;
  end;
  if value <> '' then
    ChildNodes.Add(TXMLNodeText.Create(value)) else
    begin
      if FChildNodes <> nil then
      begin
        FChildNodes.Free;
        FChildNodes := nil;
        FChildIndex.Free;
        FChildIndex := nil;
      end;
    end;
end;

function TXMLNode.FindChildNodes(const name: RawByteString; out ChildNodes: TXMLNodeList): Integer;
begin
  if (FChildIndex <> nil) and FChildIndex.TryGetValue(name, ChildNodes) then
    Result := ChildNodes.Count else
    Result := 0;
end;

function TXMLNode.FirstChild(const name: RawByteString): IXMLNode;
var
  lst: TXMLNodeList;
begin
  if FindChildNodes(name, lst) > 0 then
    Result := lst[0] else
    Result := nil;
end;

end.
