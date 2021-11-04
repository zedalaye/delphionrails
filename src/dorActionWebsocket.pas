(*
    "The contents of this file are subject to the Mozilla Public License
    Version 1.1 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at
    http://www.mozilla.org/MPL/

    Software distributed under the License is distributed on an "AS IS"
    basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
    License for the specific language governing rights and limitations
    under the License.

    The Initial Developer of the Original Code is
      Henri Gourvest <hgourvest@gmail.com>.
*)

unit dorActionWebsocket;

interface

uses
  Windows, Classes, WinSock2,
  SysUtils,
  superobject, dorSocketStub, dorHTTPStub;

type
  TActionWebsocket = class(TCustomObserver)
  private
    FStub: THTTPStub;
    FCriticalSection: TRTLCriticalSection;
    FParams: ISuperObject;
    FSession: ISuperObject;
    FRequest: THTTPMessage;
    FWebSocketVersion: Integer;
    procedure Output(b: Byte; data: Pointer; len: Int64);
    procedure OutputString(b: Byte; const str: string);
  protected
    class procedure Register;
    function Run: Cardinal; override;
    procedure doOnInternalEvent(const Event: ISuperObject); override;
  public
    procedure OutputMessage(const msg: string);
    procedure OutputPing(const msg: string);
    procedure OutputPong(const msg: string);
    procedure OutputClose(error: Word);
    procedure OutputStream(stream: TStream);

    procedure InputMessage(const msg, source: string); virtual;
    procedure InputStream(stream: TStream; const source: string); virtual;
    procedure InputPing(const msg: string; const source: string); virtual;
    procedure InputPong(const msg: string; const source: string); virtual;
    procedure InputClose(error: Word; const source: string); virtual;

    constructor Create(Version: Integer); reintroduce; virtual;
    destructor Destroy; override;

    property Params: ISuperObject read FParams;
    property Session: ISuperObject read FSession;
    property Request: THTTPMessage read FRequest;
    property WebSocketVersion: Integer read FWebSocketVersion;
  end;

  TActionWebsocketClass = class of TActionWebsocket;

implementation

{ TActionController }

constructor TActionWebsocket.Create(Version: Integer);
begin
  FWebSocketVersion := Version;
  InitializeCriticalSection(FCriticalSection);

  FStub := (CurrentDorThread as THTTPStub);
  FRequest := FStub.Request;
  FParams  := FStub.Params;
  FSession := FStub.Session;

  inherited Create(CurrentDorThread);
end;

destructor TActionWebsocket.Destroy;
begin
  DeleteCriticalSection(FCriticalSection);
  inherited;
end;

procedure TActionWebsocket.doOnInternalEvent(const Event: ISuperObject);
begin
  case ObjectGetType(Event) of
    stString: InputMessage(Event.AsString, '');
    stObject:
      case Event.I['opcode'] of
        $1: InputMessage(Event.AsObject.S['data'], Event.AsObject.S['source']);
        $2: InputStream(TStream(Event.AsObject.I['data']), Event.AsObject.S['source']);
        $8: InputClose(Event.AsObject.I['data'], Event.AsObject.S['source']);
        $9: InputPing(Event.AsObject.S['data'], Event.AsObject.S['source']);
        $A: InputPong(Event.AsObject.S['data'], Event.AsObject.S['source']);
      end;
  end
end;

procedure TActionWebsocket.InputClose(error: Word; const source: string);
begin

end;

procedure TActionWebsocket.InputMessage(const msg: string; const source: string);
begin

end;

procedure TActionWebsocket.InputPing(const msg: string; const source: string);
begin
  OutputPong(msg);
end;

procedure TActionWebsocket.InputPong(const msg: string; const source: string);
begin

end;

procedure TActionWebsocket.InputStream(stream: TStream; const source: string);
begin
  stream.Free;
end;

procedure TActionWebsocket.Output(b: Byte; data: Pointer; len: Int64);
var
  lenarray: array[0..7] of Byte absolute len;
  rw: IReadWrite;
begin
  EnterCriticalSection(FCriticalSection);
  try
    rw := FStub.Source;
    if rw <> nil then
    begin
      rw.Write(b, 1, 0);
      if len < 126 then
        rw.Write(len, 1, 0)
      else if len < High(Word) then
      begin
        b := 126;
        rw.Write(b, 1, 0);
        rw.Write(lenarray[1], 1, 0);
        rw.Write(lenarray[0], 1, 0);
      end
      else
      begin
        b := 127;
        rw.Write(b, 1, 0);
        rw.Write(lenarray[7], 1, 0);
        rw.Write(lenarray[6], 1, 0);
        rw.Write(lenarray[5], 1, 0);
        rw.Write(lenarray[4], 1, 0);
        rw.Write(lenarray[3], 1, 0);
        rw.Write(lenarray[2], 1, 0);
        rw.Write(lenarray[1], 1, 0);
        rw.Write(lenarray[0], 1, 0);
      end;
      if data <> nil then
        rw.Write(data^, len, 0);
      rw.Flush;
    end;
  finally
    LeaveCriticalSection(FCriticalSection);
  end;
end;

procedure TActionWebsocket.OutputClose(error: Word);
begin

end;

procedure TActionWebsocket.OutputMessage(const msg: string);
var
  utf8: UTF8String;
  rw: IReadWrite;
begin
  if FWebSocketVersion <> 0 then
    OutputString($80 or $1, msg)
  else
  begin
    rw := FStub.Source;
    if rw <> nil then
    begin
      utf8 := #0 + UTF8String(msg) + #255;
      EnterCriticalSection(FCriticalSection);
      try
        rw.Write(PAnsiChar(utf8)^, Length(utf8), 0);
        rw.Flush;
      finally
        LeaveCriticalSection(FCriticalSection);
      end;
    end;
  end;
end;

procedure TActionWebsocket.OutputPing(const msg: string);
begin
  if WebSocketVersion > 0 then
    OutputString($80 or $9, msg);
end;

procedure TActionWebsocket.OutputPong(const msg: string);
begin
  if WebSocketVersion > 0 then
    OutputString($80 or $A, msg);
end;

procedure TActionWebsocket.OutputStream(stream: TStream);
var
  len: Int64;
  buffer: array[0..1023] of Byte;
  rw: IReadWrite;
begin
  if WebSocketVersion > 0 then
  begin
    len := stream.Size;
    Output($80 or $2, nil, len);
    stream.Seek(0, soFromBeginning);
    len := stream.Read(buffer, SizeOf(buffer));
    while len > 0 do
    begin
      rw := FStub.Source;
      if rw <> nil then
      begin
        rw.Write(buffer, len, 0);
        len := stream.Read(buffer, SizeOf(buffer));
      end
      else
        Break;
    end;
    rw.Flush;
  end;
end;

procedure TActionWebsocket.OutputString(b: Byte; const str: string);
var
  utf8: UTF8String;
begin
  utf8 := UTF8String(str);
  Output(b, PAnsiChar(utf8), Length(utf8));
end;

class procedure TActionWebsocket.Register;
begin

end;

function TActionWebsocket.Run: Cardinal;
begin
{$if defined(DEBUG)}
  TThread.NameThreadForDebugging(AnsiString(Self.ClassName));
{$ifend}

  Result := 0;
  while not Stopped do
  begin
{$IFDEF SWITCHTOTHREAD}
    if not SwitchToThread then
{$ENDIF}
      sleep(1);
    ProcessEvents;
  end;
end;

end.

