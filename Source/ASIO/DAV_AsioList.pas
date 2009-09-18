unit DAV_ASIOList;

interface

uses Classes,SysUtils,Registry,Windows;

type
  TDAVAsioDriverDesc = class
  private
    fGuid: TGUID;
    fName: string;
    fFilename: string;
  public
    constructor Create(nGuid: TGUID; nName: string; nFilename: string); overload;
    constructor Create(nGuid, nName, nFilename: string); overload;
    property Guid: TGUID read fGuid;
    property Name: string read fName;
    property Filename: string read fFilename;
  end;

  TDAVAsioDriverList = class
  private
    fNameList: TStrings;
    fIgnoreGuid: TGuid;
    fHasIgnoreGuid: boolean;
    fList: TList;
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
    function DriverNumberByName(s: string): integer;
    property Items[Index: Integer]: TDAVAsioDriverDesc read GetItem;
    property Count: Integer read GetCount;
    property DriverNames: TStrings read fNameList;
  end;

implementation

const
  DAVASIOInprocServer = 'InprocServer32';
  DAVASIOPath         = 'software\asio';
  DAVASIOComClsId     = 'clsid';
  DAVASIODescription  = 'description';

{ TDAVIntAsioDriverDesc }

constructor TDAVAsioDriverDesc.Create(nGuid: TGUID; nName: string; nFilename: string);
begin
  fGuid:=nGuid;
  fName:=nName;
  fFilename:=nFilename;
end;

constructor TDAVAsioDriverDesc.Create(nGuid, nName, nFilename: string);
begin
  fGuid:=StringToGUID(nGuid);
  fName:=nName;
  fFilename:=nFilename;
end;

{ TDAVIntAsioDriverList }

constructor TDAVAsioDriverList.Create;
begin
  fHasIgnoreGuid := false;
  fNameList:=TStringList.Create;
  fList:=TList.Create;
end;

constructor TDAVAsioDriverList.Create(Ignore: TGuid);
begin
  Create;
  fIgnoreGuid := Ignore;
  fHasIgnoreGuid := true;
end;

destructor TDAVAsioDriverList.Destroy;
begin
  ClearList;
  fList.Free;
  fNameList.Free;
  inherited;
end;

procedure TDAVAsioDriverList.ClearList;
var i:integer;
begin
  for i := Count-1 downto 0 do Items[i].Free;

  fNameList.clear;
  fList.Clear;
end;

function TDAVAsioDriverList.GetCount: Integer;
begin
  Result := fList.Count;
end;

function TDAVAsioDriverList.GetDriverFileName(DrvGuidStr: string): string;
var Filename: string;
    DirStr : PChar;
begin
  result := '';
  if DrvGuidStr='' then exit;

  with TRegistry.Create do
  try
    RootKey := HKEY_CLASSES_ROOT;
    if OpenKeyReadOnly(DAVASIOComClsId + '\' + Lowercase(DrvGuidStr) + '\' + DAVASIOInprocServer) then
    begin
      result := ReadString('');
      Filename := ExtractFileName(result);
      DirStr := StrAlloc(MAX_PATH);

      if not FileExists(result) and (GetSystemDirectory(DirStr, MAX_PATH) <> 0) then
        result := StrPas(DirStr) + '\' + Filename;
      
      if not FileExists(result) and (GetWindowsDirectory(DirStr, MAX_PATH) <> 0) then
        result := StrPas(DirStr) + '\' + Filename;

      if not FileExists(result) then
        Result := '';

      StrDispose(DirStr);
      CloseKey;
    end;
  
  finally
    Free;
  end;
end;

function TDAVAsioDriverList.GetItem(Index: Integer): TDAVAsioDriverDesc;
begin
  Result := TDAVAsioDriverDesc(fList.Items[Index]);
end;

procedure TDAVAsioDriverList.LoadList;
var SubKeys: TStringList;
    i: Integer;
    DrvName: string;
    DrvGuidStr: string;
    DrvFile: string;
    DrvGuid: TGuid;
    newAsioDriverItem: TDAVAsioDriverDesc;
begin
  SubKeys := TStringList.Create;
  with TRegistry.Create do
  try
    RootKey:=HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(DAVASIOPath) then
    begin
      GetKeyNames(SubKeys);
      CloseKey;
    end;

    for i:=0 to SubKeys.Count-1 do
    begin
      if OpenKeyReadOnly(DAVASIOPath + '\' + SubKeys[i]) then
      begin
        DrvGuidStr := ReadString(DAVASIOComClsId);
        DrvGuid := StringToGUID(DrvGuidStr);

        DrvFile:=GetDriverFileName(DrvGuidStr);
        if (DrvFile<>'') and not ( fHasIgnoreGuid and IsEqualGUID(DrvGuid,fIgnoreGuid) ) then
        begin
          DrvName := ReadString(DAVASIODescription);
          if DrvName='' then DrvName:=SubKeys[i];

          newAsioDriverItem:=TDAVAsioDriverDesc.Create(DrvGuidStr,DrvName,DrvFile);

          fList.Add(newAsioDriverItem);
          fNameList.Add(DrvName);
        end;

        CloseKey;
      end;
    end;
  finally
    Free;
    SubKeys.Free;
  end;
end;

procedure TDAVAsioDriverList.UpdateList;
begin
  ClearList;
  LoadList;
end;

function TDAVAsioDriverList.DriverNumberByName(s: string): integer;
var i:integer;
begin
  s:=LowerCase(s);
  result := -1;
  for i := 0 to Count-1 do
    if lowercase(Items[i].name) = s then
    begin
      result:=i;
      break;
    end;
end;

end.
