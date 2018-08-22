unit dorSynchronizer;

interface

uses
  SysUtils, Windows, Classes, RTLConsts;

type
  TSynchronizer<T> = class
  private type
    PSynchronizeRecord = ^TSynchronizeRecord;
    TSynchronizeRecord = record
      FData: T;
      FProcedure: TProc<T>;
      FSynchronizeException: TObject;
    end;
    PSyncProc = ^TSyncProc;
    TSyncProc = record
      SyncRec: PSynchronizeRecord;
      Queued: Boolean;
      Signal: THandle;
    end;
  private
    FMainThreadID: Cardinal;
    FSyncList: TList;
    FThreadLock: TRTLCriticalSection;
    FSyncEvent: THandle;
    procedure doSynchronize(ASyncRec: PSynchronizeRecord; QueueEvent: Boolean = False);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function CheckSynchronize(Timeout: Integer = 0): Boolean;
    procedure Synchronize(const data: T; const AThreadProc: TProc<T>);
    procedure Queue(const data: T; const AThreadProc: TProc<T>);
  end;

implementation

{$IFNDEF CPUX64}
type
  IntPtr = Integer;
{$ENDIF}

{ TSynchonizer<T> }

function TSynchronizer<T>.CheckSynchronize(Timeout: Integer): Boolean;
var
  SyncProc: PSyncProc;
  LocalSyncList: TList;
begin
  if GetCurrentThreadID <> FMainThreadID then
    raise EThread.CreateResFmt(@SCheckSynchronizeError, [GetCurrentThreadID]);
  if Timeout > 0 then
    WaitForSingleObject(SyncEvent, Timeout);
  ResetEvent(FSyncEvent);

  LocalSyncList := nil;
  EnterCriticalSection(FThreadLock);
  try
    IntPtr(LocalSyncList) := InterlockedExchange(IntPtr(FSyncList), IntPtr(LocalSyncList));
    try
      Result := (LocalSyncList <> nil) and (LocalSyncList.Count > 0);
      if Result then
      begin
        while LocalSyncList.Count > 0 do
        begin
          SyncProc := LocalSyncList[0];
          LocalSyncList.Delete(0);
          LeaveCriticalSection(FThreadLock);
          try
            try
              if Assigned(SyncProc.SyncRec.FProcedure) then
                SyncProc.SyncRec.FProcedure(SyncProc.SyncRec.FData);
            except
              if not SyncProc.Queued then
                SyncProc.SyncRec.FSynchronizeException := AcquireExceptionObject
              else
                raise;
            end;
          finally
            EnterCriticalSection(FThreadLock);
          end;
          if not SyncProc.Queued then
            SetEvent(SyncProc.Signal)
          else
          begin
            Dispose(SyncProc.SyncRec);
            Dispose(SyncProc);
          end;
        end;
      end;
    finally
      LocalSyncList.Free;
    end;
  finally
    LeaveCriticalSection(FThreadLock);
  end;
end;

constructor TSynchronizer<T>.Create;
begin
  FMainThreadID := GetCurrentThreadId;
  InitializeCriticalSection(FThreadLock);
  FSyncEvent := CreateEvent(nil, True, False, '');
  if FSyncEvent = 0 then
    RaiseLastOSError;
end;

destructor TSynchronizer<T>.Destroy;
begin
  DeleteCriticalSection(FThreadLock);
  CloseHandle(FSyncEvent);
  inherited;
end;

procedure TSynchronizer<T>.Synchronize(const data: T; const AThreadProc: TProc<T>);
var
  ASynchronize: TSynchronizeRecord;
begin
  ASynchronize.FSynchronizeException := nil;
  ASynchronize.FData := data;
  ASynchronize.FProcedure := AThreadProc;
  doSynchronize(@ASynchronize);
end;

procedure TSynchronizer<T>.doSynchronize(ASyncRec: PSynchronizeRecord;
  QueueEvent: Boolean);
var
  SyncProc: TSyncProc;
  SyncProcPtr: PSyncProc;
begin
  if GetCurrentThreadID = FMainThreadID then
  begin
    if Assigned(ASyncRec.FProcedure) then
      ASyncRec.FProcedure(AsyncRec.FData);
  end
  else
  begin
    if QueueEvent then
      New(SyncProcPtr)
    else
      SyncProcPtr := @SyncProc;
    if not QueueEvent then
      SyncProcPtr.Signal := CreateEvent(nil, True, False, nil)
    else
      SyncProcPtr.Signal := 0;
    try
      EnterCriticalSection(FThreadLock);
      try
        SyncProcPtr.Queued := QueueEvent;
        if FSyncList = nil then
          FSyncList := TList.Create;
        SyncProcPtr.SyncRec := ASyncRec;
        FSyncList.Add(SyncProcPtr);
        SetEvent(FSyncEvent);
        if not QueueEvent then
        begin
          LeaveCriticalSection(FThreadLock);
          try
            WaitForSingleObject(SyncProcPtr.Signal, INFINITE);
          finally
            EnterCriticalSection(FThreadLock);
          end;
        end;
      finally
        LeaveCriticalSection(FThreadLock);
      end;
    finally
      if not QueueEvent then
        CloseHandle(SyncProcPtr.Signal);
    end;
    if not QueueEvent and Assigned(ASyncRec.FSynchronizeException) then
      raise ASyncRec.FSynchronizeException;
  end;
end;

procedure TSynchronizer<T>.Queue(const data: T; const AThreadProc: TProc<T>);
var
  LSynchronize: PSynchronizeRecord;
begin
  New(LSynchronize);
  try
    LSynchronize.FSynchronizeException := nil;
    LSynchronize.FData := data;
    LSynchronize.FProcedure := AThreadProc;
    doSynchronize(LSynchronize, True);
  finally
    if MainThreadID = GetCurrentThreadId then
      Dispose(LSynchronize);
  end;
end;

end.
