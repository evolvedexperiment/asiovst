unit DAV_Semaphore;

{$I DAV_Compiler.inc}

interface

{.$UNDEF Win32}

uses
  {$IFDEF Win32}Windows, {$ENDIF} Classes;

type
  TSemaphore = class
  private
    {$IFDEF Win32}
    FSemaphore : THandle;
    {$ELSE}
    FSemaphore : Integer;
    {$ENDIF}
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Enter;
    procedure Leave;
  end;

implementation

{ TSemaphore }

constructor TSemaphore.Create;
begin
 {$IFDEF Win32}
 FSemaphore := CreateSemaphore(nil, 1, 1, nil);
 {$ELSE}
 FSemaphore := 0;
 {$ENDIF}
end;

destructor TSemaphore.Destroy;
begin
 {$IFDEF Win32}
 CloseHandle(FSemaphore);
 {$ENDIF}
 inherited;
end;

procedure TSemaphore.Enter;
 {$IFDEF Win32}
var
  ws : DWORD;
begin
 repeat
  ws := WaitForSingleObject(FSemaphore, 0);
//  if ws = WAIT_TIMEOUT then exit;
 until ws = WAIT_OBJECT_0;
 {$ELSE}
begin
 while FSemaphore > 0 do;
 inc(FSemaphore);
 {$ENDIF}
end;

procedure TSemaphore.Leave;
begin
 {$IFDEF Win32}
 ReleaseSemaphore(FSemaphore, 1, nil);
 {$ELSE}
 Dec(FSemaphore);
 {$ENDIF}
end;

end.
