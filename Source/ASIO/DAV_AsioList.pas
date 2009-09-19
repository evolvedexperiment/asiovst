unit DAV_AsioList;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, SysUtils, Registry, Windows;

type
  TDAVAsioDriverDesc = class
  private
    FGuid     : TGUID;
    FName     : string;
    FFilename : string;
  public
    constructor Create(nGuid: TGUID; nName: string; nFilename: string); overload;
    constructor Create(nGuid, nName, nFilename: string); overload;
    property Guid: TGUID read FGuid;
    property Name: string read FName;
    property Filename: string read FFilename;
  end;

  TDAVAsioDriverList = class
  private
    FNameList      : TStrings;
    FIgnoreGuid    : TGuid;
    FHasIgnoreGuid : boolean;
    FList          : TList;
    procedure ClearList;
    procedure LoadList;
    function GetDriverFileName(DrvGuidStr: string): string;
    function GetItem(Index: Integer): TDAVAsioDriverDesc;
    function GetCount: Integer;
  public
    constructor Create(Ignore: TGuid); reintroduce; overload;
    constructor Create; overload;
    destructor Destroy; override;

    procedure UpdateList;
    function DriverNumberByName(DriverName: string): Integer;
    property Items[Index: Integer]: TDAVAsioDriverDesc read GetItem;
    property Count: Integer read GetCount;
    property DriverNames: TStrings read FNameList;
  end;

implementation

const
  CAsioInprocServer = 'InprocServer32';
  CAsioPath         = 'software\asio';
  CAsioComClsId     = 'clsid';
  CAsioDescription  = 'description';

{ TDAVIntAsioDriverDesc }

constructor TDAVAsioDriverDesc.Create(nGuid: TGUID; nName: string; nFilename: string);
begin
 FGuid := nGuid;
 FName := nName;
 FFilename := nFilename;
end;

constructor TDAVAsioDriverDesc.Create(nGuid, nName, nFilename: string);
begin
 FGuid := StringToGUID(nGuid);
 FName := nName;
 FFilename := nFilename;
end;


{ TDAVIntAsioDriverList }

constructor TDAVAsioDriverList.Create;
begin
 inherited;
 FHasIgnoreGuid := False;
 FNameList := TStringList.Create;
 FList := TList.Create;
end;

constructor TDAVAsioDriverList.Create(Ignore: TGuid);
begin
 Create;
 FIgnoreGuid := Ignore;
 FHasIgnoreGuid := True;
end;

destructor TDAVAsioDriverList.Destroy;
begin
  ClearList;
  FreeAndNil(FList);
  FreeAndNil(FNameList);
  inherited;
end;

procedure TDAVAsioDriverList.ClearList;
var
  DriverIndex : Integer;
begin
  for DriverIndex := Count - 1 downto 0
   do Items[DriverIndex].Free;

  FNameList.Clear;
  FList.Clear;
end;

function TDAVAsioDriverList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TDAVAsioDriverList.GetDriverFileName(DrvGuidStr: string): string;
var
  Filename : string;
  DirStr   : PChar;
begin
 Result := '';
 if DrvGuidStr = '' then exit;

 with TRegistry.Create do
  try
   RootKey := HKEY_CLASSES_ROOT;
   if OpenKeyReadOnly(CAsioComClsId + '\' + Lowercase(DrvGuidStr) + '\' + CAsioInprocServer) then
    begin
     Result := ReadString('');
     Filename := ExtractFileName(Result);
     DirStr := StrAlloc(MAX_PATH);

     if not FileExists(Result) and (GetSystemDirectory(DirStr, MAX_PATH) <> 0)
      then Result := StrPas(DirStr) + '\' + Filename;

     if not FileExists(Result) and (GetWindowsDirectory(DirStr, MAX_PATH) <> 0)
      then Result := StrPas(DirStr) + '\' + Filename;

     if not FileExists(Result) then Result := '';

     StrDispose(DirStr);
     CloseKey;
    end;

 finally
  Free;
 end;
end;

function TDAVAsioDriverList.GetItem(Index: Integer): TDAVAsioDriverDesc;
begin
 Result := TDAVAsioDriverDesc(FList.Items[Index]);
end;

procedure TDAVAsioDriverList.LoadList;
var
  SubKeys           : TStringList;
  i                 : Integer;
  DrvName           : string;
  DrvGuidStr        : string;
  DrvFile           : string;
  DrvGuid           : TGuid;
  newAsioDriverItem : TDAVAsioDriverDesc;
begin
 SubKeys := TStringList.Create;
 with TRegistry.Create do
  try
   RootKey := HKEY_LOCAL_MACHINE;
   if OpenKeyReadOnly(CAsioPath) then
    begin
     GetKeyNames(SubKeys);
     CloseKey;
    end;

   for i := 0 to SubKeys.Count-1 do
    begin
     if OpenKeyReadOnly(CAsioPath + '\' + SubKeys[i]) then
      begin
       DrvGuidStr := ReadString(CAsioComClsId);
       DrvGuid := StringToGUID(DrvGuidStr);

       DrvFile := GetDriverFileName(DrvGuidStr);
       if (DrvFile <> '') and not (FHasIgnoreGuid and IsEqualGUID(DrvGuid, FIgnoreGuid)) then
        begin
         DrvName := ReadString(CAsioDescription);
         if DrvName = '' then DrvName := SubKeys[i];

         newAsioDriverItem := TDAVAsioDriverDesc.Create(DrvGuidStr,DrvName,DrvFile);

         FList.Add(newAsioDriverItem);
         FNameList.Add(DrvName);
        end;

       CloseKey;
     end;
   end;
 finally
  Free;
  FreeAndNil(SubKeys);
 end;
end;

procedure TDAVAsioDriverList.UpdateList;
begin
 ClearList;
 LoadList;
end;

function TDAVAsioDriverList.DriverNumberByName(DriverName: string): Integer;
var
  DriverIndex : Integer;
begin
 DriverName := LowerCase(DriverName);
 Result := -1;
 for DriverIndex := 0 to Count-1 do
  if LowerCase(Items[DriverIndex].Name) = DriverName then
   begin
    Result := DriverIndex;
    Break;
   end;
end;

end.
