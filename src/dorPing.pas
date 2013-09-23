unit dorPing;

interface
uses Classes;

type
  TPingCallBack = reference to function(success: Boolean): Cardinal;

function Ping(const host: AnsiString; timeout: Cardinal; const callback: TPingCallBack): TThread;

implementation
uses Windows, Winsock2;

type
  TTHreadHack = class(TThread) end;

function Ping(const host: AnsiString; timeout: Cardinal; const callback: TPingCallBack): TThread;
var
  t: TThread;
begin
  t := TThread.CreateAnonymousThread(procedure
  const
    BUFFER_SIZE = 4;
  type
    PIPOptionInformation = ^TIPOptionInformation;
    TIPOptionInformation = record
       TTL:         Byte;
       TOS:         Byte;
       Flags:       Byte;
       OptionsSize: Byte;
       OptionsData: PAnsiChar;
    end;
    TIcmpEchoReply = record
       Address:  LongInt;
       Status:   DWord;
       RTT:      DWord;
       DataSize: Word;
       Reserved: Word;
       Data:     Pointer;
       Options:  TIPOptionInformation;
       Buffer:   array[0..BUFFER_SIZE-1] of AnsiChar;
     end;
  var
    module: HMODULE;
    IcmpCreateFile: function: THandle; stdcall;
    IcmpCloseHandle: function(IcmpHandle: THandle): Boolean; stdcall;
    IcmpSendEcho: function(IcmpHandle: THandle; DestinationAddress: LongInt;
      RequestData: Pointer; RequestSize: Word; RequestOptions: PIPOptionInformation;
      ReplyBuffer: Pointer; ReplySize: DWord; Timeout: DWord): DWord; stdcall;
    handle: THandle;
    options: TIPOptionInformation;
    reply: TIcmpEchoReply;
    data: array[0..BUFFER_SIZE-1] of AnsiChar;
    ret: Integer;
    slp: Cardinal;
    ip: LongInt;
  begin
    ip := inet_addr(PAnsiChar(host));

    if ip = Longint(INADDR_NONE) then
    begin
      callback(False);
      Exit;
    end;
    module := LoadLibrary('icmp.dll');
    IcmpCreateFile  := GetProcAddress(module, 'IcmpCreateFile');
    IcmpCloseHandle := GetProcAddress(module, 'IcmpCloseHandle');
    IcmpSendEcho    := GetProcAddress(module, 'IcmpSendEcho');
    handle := IcmpCreateFile;
    FillChar(options, SizeOf(options), 0);
    FillChar(reply, SizeOf(reply), 0);
    data := 'ping';
    options.TTL := 64;

    repeat
      ret := IcmpSendEcho(handle, ip, @data, SizeOf(data), @options, @reply, SizeOf(reply), timeout);
      slp := callback((ret <> 0) and (reply.Status = 0));
      Sleep(slp);
    until (slp = 0) or TTHreadHack(t).Terminated;
    IcmpCloseHandle(handle);
  end);
  t.FreeOnTerminate := True;
  Result := t;
end;

end.
