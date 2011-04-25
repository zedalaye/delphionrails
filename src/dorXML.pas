unit dorXML;

interface
uses Classes, Generics.Collections;

(******************************************************************************)
(* XML Node                                                                   *)
(* Simple XML tree node                                                       *)
(******************************************************************************)

type
  IXMLNode = interface
  ['{8A22EC91-113B-48DE-A58B-E920DED5F21D}']
    function GetName: string;
    function GetAttributes: TDictionary<string, string>;
    function GetChildren: TList<IXMLNode>;
    function GetText: string;
    procedure SetText(const text: string);
    function FindChildren(const name: string; out children: TList<IXMLNode>): Integer;
    function FindChild(const name: string): IXMLNode;
    property Name: string read GetName;
    property Attr: TDictionary<string, string> read GetAttributes;
    property Children: TList<IXMLNode> read GetChildren;
    property Text: string read GetText write SetText;
  end;

  TXMLNode = class(TInterfacedObject, IXMLNode)
  private
    FName: string;
    FText: string;
    FAttributes: TDictionary<string, string>;
    FChildren: TList<IXMLNode>;
    FChildIndex: TObjectDictionary<string, TList<IXMLNode>>;
    procedure doNotify(Sender: TObject; const Item: IXMLNode;
      Action: TCollectionNotification);
  public
    constructor Create(const name: string);
    destructor Destroy; override;
    function GetName: string;
    function GetAttributes: TDictionary<string, string>;
    function GetChildren: TList<IXMLNode>;
    function GetText: string;
    procedure SetText(const text: string);
    function FindChildren(const name: string; out children: TList<IXMLNode>): Integer;
    function FindChild(const name: string): IXMLNode;
  end;

type
  TXMLReader = reference to function(var c: AnsiChar): Boolean;
  TXMLNodeType = (xtOpen, xtClose, xtAttribute, xtText);
  TXMLEvent = reference to function(node: TXMLNodeType; const name, value: string): Boolean;

  function XMLParseSAX(cp: Cardinal; const reader: TXMLReader; const event: TXMLEvent): Boolean;
  function XMLParse(cp: Cardinal; const reader: TXMLReader): IXMLNode;
  function XMLParseStream(cp: Cardinal; stream: TStream): IXMLNode;

implementation
uses Windows, Math, SysUtils;

(******************************************************************************)
(* TWriterString                                                              *)
(* RawByteString Buffer                                                          *)
(******************************************************************************)

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

  constructor TXMLNode.Create(const name: string);
  begin
    FName := name;
    FAttributes := nil;
    FChildren := nil;
  end;

  destructor TXMLNode.Destroy;
  begin
    inherited;
    if FAttributes <> nil then
      FAttributes.Free;
    if FChildren <> nil then
    begin
      FChildren.OnNotify := nil;
      FChildren.Free;
    end;
    if FChildIndex <> nil then
      FChildIndex.Free;
  end;

  function TXMLNode.GetName: string;
  begin
    Result := FName;
  end;

  function TXMLNode.GetAttributes: TDictionary<string, string>;
  begin
    if FAttributes = nil then
      FAttributes := TDictionary<string, string>.Create;
    Result := FAttributes;
  end;

  procedure TXMLNode.doNotify(Sender: TObject; const Item: IXMLNode;
    Action: TCollectionNotification);
  var
    lst: TList<IXMLNode>;
  begin
    case Action of
      cnAdded:
        begin
          if not FChildIndex.TryGetValue(Item.Name, lst) then
          begin
            lst := TList<IXMLNode>.Create;
            FChildIndex.Add(Item.Name, lst);
          end;
          lst.Add(Item)
        end;
      cnRemoved, cnExtracted:
        if FChildIndex.TryGetValue(Item.Name, lst) then
          lst.Remove(Item);
    end;
  end;

  function TXMLNode.GetChildren: TList<IXMLNode>;
  begin
    if FChildren = nil then
    begin
      FChildren := TList<IXMLNode>.Create;
      FChildIndex := TObjectDictionary<string, TList<IXMLNode>>.Create([doOwnsValues]);
      FChildren.OnNotify := doNotify;
    end;
    Result := FChildren;
  end;

  function TXMLNode.GetText: string;
  begin
    Result := FText;
  end;

  procedure TXMLNode.SetText(const text: string);
  begin
    FText := text;
  end;

  function TXMLNode.FindChildren(const name: string; out children: TList<IXMLNode>): Integer;
  begin
    if (FChildIndex <> nil) and FChildIndex.TryGetValue(name, children) then
      Result := children.Count else
      Result := 0;
  end;

  function TXMLNode.FindChild(const name: string): IXMLNode;
  var
    lst: TList<IXMLNode>;
  begin
    if FindChildren(name, lst) > 0 then
      Result := lst[0] else
      Result := nil;
  end;

(******************************************************************************)
(* XMLParseSAX                                                              *)
(******************************************************************************)

function XMLParseSAX(cp: Cardinal; const reader: TXmlReader; const event: TXMLEvent): Boolean;
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
      xsTryCloseElement, xsCloseElementName, xsChildren, xsElementString,
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
                if not event(xtOpen, MBUDecode(Str.Data), '') then Exit(False);
                Stack^.state := xsEatSpaces;
                Stack^.savedstate := xsAttributes;
                goto redo;
              end;
          end;
        xsChildren:
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
                     Stack^.savedstate := xsChildren;
                     Stack^.state := xsEatSpaces;
                     StackUp;
                     Str.Reset;
                     Stack^.state := xsElementPI;
                     Stack^.clazz := xcProcessInst;
                   end
            else
              Stack^.state := xsChildren;
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
                     Stack^.savedstate := xsChildren;
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
                  if Stack.clazz <> xcProcessInst then
                    if not event(xtAttribute, MBUDecode(Str.Data), MBUDecode(Value.Data)) then Exit(False);
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
                    Stack^.savedstate := xsChildren else
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
                     if not event(xtText, '', MBUDecode(Value.Data)) then Exit(False);
                     Stack^.state := xsEatSpaces;
                     Stack^.savedstate := xsChildren;
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

function XMLParse(cp: Cardinal; const reader: TXMLReader): IXMLNode;
var
  stack: TStack<IXMLNode>;
  n: IXMLNode;
begin
  stack := TStack<IXMLNode>.Create;
  try
    if XMLParseSAX(cp,
      function (var c: AnsiChar): Boolean
      begin
        Result := reader(c);
      end,
      function(node: TXMLNodeType; const name, value: string): Boolean
      begin
        Result := True;
        case node of
          xtOpen: stack.Push(TXMLNode.Create(name));
          xtClose:
            begin
              n := stack.Pop;
              stack.Peek.Children.Add(n);
            end;
          xtAttribute: stack.Peek.Attr.AddOrSetValue(name, value);
          xtText:
            with stack.Peek do
              Text := Text + value;
        end;
      end) then
        Result := stack.Peek else
        Result := nil;
  finally
    stack.Free;
  end;
end;

function XMLParseStream(cp: Cardinal; stream: TStream): IXMLNode;
begin
  Result := XMLParse(cp, function(var c: AnsiChar): Boolean
    begin
      Result := stream.Read(c, 1) = 1
    end);
end;

end.
