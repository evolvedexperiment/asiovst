unit DAV_DLLResources;

interface

uses
  Windows, Classes, SysUtils, ConTnrs, ImageHlp;

type
  TPEModule = class;
  TResourceDetails = class;
  TResourceDetailsClass = class of TResourceDetails;

  {$IFDEF DELPHI10_UP} {$region 'TResourceModule class'} {$ENDIF}

  ///////////////////////////
  // TResourceModule class //
  ///////////////////////////

  TResourceModule = class
  private
    FDirty  : Boolean;
    FBackup : Boolean;
    function GetDirty: Boolean;
  protected
    function GetResourceCount: Integer; virtual; abstract;
    function GetResourceDetails(idx: Integer): TResourceDetails;
      virtual; abstract;
    procedure ClearDirty;
  public
    procedure DeleteResource(idx: Integer); virtual;
    procedure InsertResource(idx: Integer; details: TResourceDetails); virtual; abstract;
    function AddResource(details: TResourceDetails): Integer; virtual;
    function IndexOfResource(details: TResourceDetails): Integer; virtual; abstract;
    function GetUniqueResourceName(const tp: WideString): WideString;

    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;

    procedure SaveToFile(const FileName: string); virtual;
    procedure LoadFromFile(const FileName: string); virtual;
    procedure SortResources; virtual; abstract;

    function FindResource(const tp, Name: WideString; ALanguage: Integer): TResourceDetails;

    property ResourceCount: Integer read GetResourceCount;
    property ResourceDetails[idx: Integer]: TResourceDetails read GetResourceDetails;
    property Dirty: Boolean read GetDirty write FDirty;
    property Backup: Boolean read FBackup write FBackup;
  end;

  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'TResourceDetails class'} {$ENDIF}
  ////////////////////////////
  // TResourceDetails class //
  ////////////////////////////

  TResourceDetails = class
  private
    FParent                : TResourceModule;
    FData                  : TMemoryStream;
    FCodePage              : Integer;
    FResourceLanguage      : LCID;
    FResourceName          : WideString;
    FResourceType          : WideString;

    FMemoryFlags           : Word;          // Resource memory flags
    FDataVersion, FVersion : DWORD;         // Resource header version info
    FCharacteristics       : DWORD;
    FDirty                 : Boolean;
    FTag                   : Integer;
    procedure SetResourceType(const Value: WideString);
                                           // Resource header characteristics
  protected
    constructor Create(AParent: TResourceModule; ALanguage: Integer; const AName, AType: WideString; ASize: Integer; AData: pointer); virtual;
    procedure InitNew; virtual;
    procedure SetResourceName(const Value: WideString); virtual;
    class function SupportsRCData(const AName: string; Size: Integer; Data: Pointer): Boolean; virtual;
    class function SupportsData(Size: Integer; Data: Pointer): Boolean; virtual;
  public
    class function CreateResourceDetails(AParent: TResourceModule; ALanguage: Integer; const AName, AType: WideString; ASize: Integer; AData: Pointer): TResourceDetails;
    class function GetBaseType: WideString; virtual;

    constructor CreateNew(AParent: TResourceModule; ALanguage: Integer; const AName: WideString); virtual;
    destructor Destroy; override;
    procedure BeforeDelete; virtual;

    procedure ChangeData(newData: TMemoryStream); virtual;

    property Parent: TResourceModule read FParent;
    property Data: TMemoryStream read FData;
    property ResourceName: WideString read FResourceName write SetResourceName;
    property ResourceType: WideString read FResourceType write SetResourceType;
    property ResourceLanguage: LCID read FResourceLanguage write FResourceLanguage;

    property CodePage: Integer read FCodePage write FCodePage;
    property Characteristics: DWORD read FCharacteristics write FCharacteristics;
    property Version: DWORD read FVersion write FDataVersion;
    property DataVersion: DWORD read FDataVersion write FDataVersion;
    property MemoryFlags: WORD read FMemoryFlags write FMemoryFlags;

    property Dirty: Boolean read FDirty write FDirty;
    property Tag: Integer read FTag write FTag;
  end;

  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'TImageSection class'} {$ENDIF}
  /////////////////////////
  // TImageSection class //
  /////////////////////////

  TImageSection = class
  private
    FParent: TPEModule;
    FSectionHeader: TImageSectionHeader;
    FRawData: TMemoryStream;
    FUninitializedDataSize: Integer;

    function GetSectionName: string;
  public
    constructor Create(AParent: TPEModule;
      const AHeader: TImageSectionHeader; rawData: pointer);
    destructor Destroy; override;
    property Parent: TPEModule read FParent;

    property SectionName: string read GetSectionName;
    property SectionHeader: TImageSectionHeader read FSectionHeader;
    property RawData: TMemoryStream read FRawData;
  end;

  TImageImportDirectory = packed record
    Characteristics : DWORD; // This is an RVA to a list of pointers. Each of these points to there function name
    TimeDateStamp   : DWORD; // The time/date stamp indicating when the file was built
    ForwarderChain  : DWORD; // This field relates to forwarding. Forwarding involves one DLL sending on references to one of its functions to another DLL
    Name            : DWORD; // This is an RVA to a NULL-terminated ASCII string containing the imported DLL's name
    FirstThunk      : DWORD; // Another RVA to a list pointers. Each of these points to their function name
  end;
  PImageImportDirectory = ^TImageImportDirectory;
  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'TPEModule class'} {$ENDIF}
  /////////////////////
  // TPEModule class //
  /////////////////////

  TPEModule = class(TResourceModule)
  private
    FDOSHeader      : TImageDosHeader;
    FCOFFHeader     : TImageFileHeader;
    FOptionalHeader : PImageOptionalHeader;
    FSectionList    : TObjectList;           // List of TImageSection objects
    FDOSStub        : TMemoryStream;
    FCommentBlock   : PChar;
    FCommentSize    : Integer;
    FEndComment     : PChar;
    FEndCommentSize : Integer;

    function GetOptionalHeader: TImageOptionalHeader;
    function GetImageSection(index: Integer): TImageSection;
    function GetImageSectionCount: Integer;
    function GetDataDictionary(index: Integer): PImageDataDirectory;
    function GetDataDictionaryCount: Integer;
    function GetDOSHeader: TImageDosHeader;
    function GetCOFFHeader: TImageFileHeader;
    function GetExportCount: Integer;
    function GetImportCount: Integer;
    function GetResourceSection(var offset: Integer): TImageSection;
    function GetImportSection(var offset: Integer): TImageSection;
    function GetExportSection(var offset: Integer): TImageSection;
    function GetImport(idx: Integer): PImageImportDirectory;
    function GetImportSectionData: PChar;
    function GetExportSectionData: PChar;

  protected
    procedure Decode(memory: pointer; exeSize: Integer); virtual;
    procedure Encode; virtual;
    property OptionalHeaderPtr: PImageOptionalHeader read FOptionalHeader;
    function FindDictionaryEntrySection(entryNo: Integer; var offset: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    property DOSHeader: TImageDosHeader read GetDOSHeader;
    property COFFHeader: TImageFileHeader read GetCOFFHeader;
    property OptionalHeader: TImageOptionalHeader read GetOptionalHeader;

    property ImageSectionCount: Integer read GetImageSectionCount;
    property ImageSection[index: Integer]: TImageSection read GetImageSection;

    property DataDictionaryCount: Integer read GetDataDictionaryCount;
    property DataDictionary[index: Integer]: PImageDataDirectory read GetDataDictionary;

    property ImportCount: Integer read GetImportCount;
    property Import[idx: Integer]: PImageImportDirectory read GetImport;
    property ImportSectionData: PChar read GetImportSectionData;
    property ExportSectionData: PChar read GetExportSectionData;
    property ExportCount: Integer read GetExportCount;

    procedure GetExportDetails(idx: Integer; var Name: string; var ordinal: DWORD);

    procedure LoadFromStream(s: TStream); override;
    procedure LoadFromFile(const Name: string); override;

    procedure SaveToStream(Stream: TStream); override;
  end;
  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'TResourceDirectory records'} {$ENDIF}
  ////////////////////////////////////
  // TResourceDirectoryTable record //
  ////////////////////////////////////

  TResourceDirectoryTable = packed record
    characteristics : DWORD; // Resource flags, reserved for future use; currently set to zero.
    timeDateStamp   : DWORD; // Time the resource data was created by the resource compiler.
    versionMajor    : WORD;  // Major version number, set by the user.
    versionMinor    : WORD;  // Minor version number.
    cNameEntries    : WORD;  // Number of directory entries, immediately following the table, that use strings to identify Type, Name, or Language (depending on the level of the table).
    cIDEntries      : WORD;  // Number of directory entries, immediately following the Name entries, that use numeric identifiers for Type, Name, or Language.
  end;
  PResourceDirectoryTable = ^TResourceDirectoryTable;

  //////////////////////
  // TPEModule record //
  //////////////////////

  TResourceDirectoryEntry = packed record
    Name : DWORD; // RVA Address of integer or string that gives the Type, Name, or Language identifier, depending on level of table.
    RVA  : DWORD; // RVA High bit 0. Address of a Resource Data Entry (a leaf).
                  // RVA High bit 1. Lower 31 bits are the address of another Resource Directory Table (the next level down).
  end;
  PResourceDirectoryEntry = ^TResourceDirectoryEntry;

  ////////////////////////////////////
  // TResourceDirectoryEntry record //
  ////////////////////////////////////

  TResourceDataEntry = packed record
    OffsetToData : DWORD;
    Size         : DWORD;
    CodePage     : DWORD;
    Reserved     : DWORD;
  end;
  PResourceDataEntry = ^TResourceDataEntry;
  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  {$IFDEF DELPHI10_UP} {$region 'TPEResourceModule class'} {$ENDIF}
  /////////////////////////////
  // TPEResourceModule class //
  /////////////////////////////

  TPEResourceModule = class(TPEModule)
  private
    FDetailList: TObjectList;             // List of TResourceDetails objects

  protected
    procedure Decode(memory: pointer; exeSize: Integer); override;
    procedure Encode; override;
    function GetResourceCount: Integer; override;
    function GetResourceDetails(idx: Integer): TResourceDetails; override;
  public
    constructor Create;
    destructor Destroy; override;

    property ResourceCount: Integer read GetResourceCount;
    property ResourceDetails[idx: Integer]: TResourceDetails read GetResourceDetails;
    procedure DeleteResource(resourceNo: Integer); override;
    procedure InsertResource(idx: Integer; details: TResourceDetails); override;
    function AddResource(details: TResourceDetails): Integer; override;
    function IndexOfResource(details: TResourceDetails): Integer; override;
    procedure SortResources; override;
  end;
  {$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

  EPEException = class(Exception);

  // Global function definitions
  function ResourceWideCharToStr(var wstr: PWideChar; codePage: Integer): string;
  function ResourceWideCharToWideStr(var wstr: PWideChar): WideString;
  procedure ResourceStrToWideChar(const s: string; var p: PWideChar; codePage: Integer);
  procedure ResourceWideStrToWideChar(const s: WideString; var p: PWideChar);
  function ResourceNameToInt(const s: string): Integer;
  function WideResourceNameToInt(const s: WideString): Integer;
  function CompareDetails(p1, p2: Pointer): Integer;

implementation

{$IFDEF DELPHI10_UP} {$region 'Local Declarations and Functions'} {$ENDIF}

resourcestring
  rstNoBaseType = 'Can''t register resource details class with no base type';
  rstNoStreaming = 'Module doesn''t support streaming';
  rstInvalidDOSSignature = 'Invalid DOS signature';
  rstInvalidCOFFSignature = 'Invalid COFF signature';
  rstInvalidOptionalHeader = 'Invalid Windows Image';
  rstBadDictionaryIndex = 'Index exceeds data dictionary count';
  rstBadLangID = 'Unsupported non-integer language ID in resource';
  rstEncode = 'Error encoding module';

type
  TResourceNode = class
    Count: Integer;
    nodes: array of record
      id: string;
      intID: boolean;
      case leaf: boolean of
        False: (Next: TResourceNode);
        True: (Data: TMemoryStream;
          CodePage: DWORD)
    end;

    constructor Create(const AType, AName: string; ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    constructor CreateNameNode(const AName: string; ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    constructor CreateLangNode(ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    procedure Add(const AType, AName: string; ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    procedure AddName(const AName: string; ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    procedure AddLang(ALang: Integer; aData: TMemoryStream; CodePage: DWORD);
    function IsID(idx: Integer): boolean;
    destructor Destroy; override;
  end;

(*----------------------------------------------------------------------------*
 | procedure ResourceWideCharToStr ()                                         |
 |                                                                            |
 | Convert Pascal-style WideChar array to a string                            |
 |                                                                            |
 | Parameters:                                                                |
 |   WStr : PWChar             The characters                                 |
 |   codePage : Integer        Code page to use in conversion                 |
 *----------------------------------------------------------------------------*)
function ResourceWideCharToStr(var wstr: PWideChar; codePage: Integer): string;
var
  len: word;
begin
  len := word(wstr^);
  SetLength(Result, len);
  Inc(wstr);
  WideCharToMultiByte(codePage, 0, WStr, Len, PChar(Result),
    Len + 1, nil, nil);
  Inc(wstr, len);
  Result := PChar(Result);
end;

(*----------------------------------------------------------------------------*
 | procedure ResourceWideCharToWideStr ()                                     |
 |                                                                            |
 | Convert Pascal-style WideChar array to a WideString                        |
 |                                                                            |
 | Parameters:                                                                |
 |   WStr : PWChar             The characters                                 |
 *----------------------------------------------------------------------------*)
function ResourceWideCharToWideStr(var wstr: PWideChar): WideString;
var
  len: word;
begin
  len := word(wstr^);
  SetLength(Result, len);
  Inc(wstr);
  Move(wstr^, PWideChar(Result)^, len * SizeOf(WideChar));
  Inc(wstr, len);
end;

(*----------------------------------------------------------------------------*
 | procedure ResourceStrToWideChar ()                                         |
 |                                                                            |
 | Convert a string to a Pascal style Wide char array                         |
 |                                                                            |
 | Parameters:                                                                |
 |   s : string                The string                                     |
 |   var p : PWideChar         [in]  Points to the start of the receiving buf |
 |                             [out] Points after the characters.             |
 |   codePage : Integer        Code page to use in conversion                 |
 *----------------------------------------------------------------------------*)
procedure ResourceStrToWideChar(const s: string; var p: PWideChar;
  codePage: Integer);
var
  buffer: PWideChar;
  len, size: word;
begin
  len := Length(s);
  size := (Length(s) + 1) * SizeOf(WideChar);
  GetMem(buffer, size);
   try
    MultiByteToWideChar(codePage, 0, PChar(s), -1, buffer, size);
    p^ := WideChar(len);
    Inc(p);
    Move(buffer^, p^, len * SizeOf(WideChar));
    Inc(p, len)
   finally
    FreeMem(buffer)
   end
end;

(*----------------------------------------------------------------------------*
 | procedure ResourceWideStrToWideChar ()                                     |
 |                                                                            |
 | Convert a wide string to a Pascal style Wide char array                    |
 |                                                                            |
 | Parameters:                                                                |
 |   s : string                The string                                     |
 |   var p : PWideChar         [in]  Points to the start of the receiving buf |
 |                             [out] Points after the characters.             |
 *----------------------------------------------------------------------------*)
procedure ResourceWideStrToWideChar(const s: WideString; var p: PWideChar);
var
  len: word;
begin
  len := Length(s);
  p^ := WideChar(len);
  Inc(p);
  Move(PWideChar(s)^, p^, len * SizeOf(WideChar));
  Inc(p, len)
end;

(*----------------------------------------------------------------------*
 | procedure ResourceNameToInt                                          |
 |                                                                      |
 | Get integer value of resource name (or type).  Return -1 if it's     |
 | not numeric.                                                         |
 *----------------------------------------------------------------------*)
function ResourceNameToInt(const s: string): Integer;
var
  isNumeric: Boolean;
  i: Integer;
begin
  isNumeric := Length(s) > 0;
  for i := 1 to Length(s) do
    if not (s[i] in ['0'..'9']) then
     begin
      isNumeric := False;
      break
     end;

  if isNumeric then
    Result := StrToInt(s)
  else
    Result := -1
end;

function WideResourceNameToInt(const s: WideString): Integer;
begin
  Result := ResourceNameToInt(s);
end;

(*----------------------------------------------------------------------*
 | function CompareDetails                                              |
 |                                                                      |
 | 'Compare' function used when sorting resources.  p1 and p2 must be   |
 | TResourceDetails references.  Returns > 0 if details at p1 are >     |
 | details at p2.                                                       |
 |                                                                      |
 | *  Compare resource types.  If they match then compare names.        |
 | *  'Integer' ids or names must come *after* non integer ids or names.|
 *----------------------------------------------------------------------*)
function CompareDetails(p1, p2: Pointer): Integer;
var
  d1: TResourceDetails;
  d2: TResourceDetails;
  i1, i2: Integer;
begin
  d1 := TResourceDetails(p1);
  d2 := TResourceDetails(p2);

  i1 := ResourceNameToInt(d1.ResourceType);
  i2 := ResourceNameToInt(d2.ResourceType);

  if i1 >= 0 then
    if i2 >= 0 then
      Result := i1 - i2         // Compare two integer ids
    else
      Result := 1               // id1 is int, so it's greater than non-int id2
  else
  if i2 >= 0 then
    Result := -1              // id2 is int, so it's less than non-int id1
  else
                                // Compare two string resource ids
    Result := CompareText(d1.ResourceType, d2.ResourceType);

  if Result = 0 then            // If they match, do the same with the names
   begin
    i1 := ResourceNameToInt(d1.ResourceName);
    i2 := ResourceNameToInt(d2.ResourceName);

    if i1 >= 0 then
      if i2 >= 0 then
        Result := i1 - i2
      else
        Result := 1
    else
    if i2 >= 0 then
      Result := -1
    else
      Result := CompareText(d1.ResourceName, d2.ResourceName)
   end
end;

(*----------------------------------------------------------------------*
 | function LCIDTOCodePage                                              |
 |                                                                      |
 | Get the ANSI code page for a given language ID                       |
 *----------------------------------------------------------------------*)
function LCIDToCodePage(ALcid: LCID): Integer;
var
  Buffer: array [0..6] of Char;
begin
  GetLocaleInfo(ALcid, LOCALE_IDEFAULTANSICODEPAGE, Buffer, SizeOf(Buffer));
  Result := StrToIntDef(Buffer, GetACP);
end;

{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'TResourceDetails implementation'} {$ENDIF}
{ TResourceDetails }

(*----------------------------------------------------------------------*
 | TResourceDetails.BeforeDelete                                        |
 |                                                                      |
 | Can override this to clear up before deleting.  Eg. deleting an      |
 | icon removes it from the icon group it's in.  Deleting an icon group |
 | removes the individual icon resources, etc.                          |
 *----------------------------------------------------------------------*)
procedure TResourceDetails.BeforeDelete;
begin
  // Stub
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.ChangeData                                          |
 |                                                                      |
 | Change all the data.  Handy for implementing 'undo', etc.            |
 *----------------------------------------------------------------------*)
procedure TResourceDetails.ChangeData(newData: TMemoryStream);
begin
  FData.Clear;
  FData.CopyFrom(newData, 0);
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.Create                                              |
 |                                                                      |
 | Raw - protected - constructor for resource details.                  |
 *----------------------------------------------------------------------*)
constructor TResourceDetails.Create(AParent: TResourceModule;
  ALanguage: Integer; const AName, AType: WideString; ASize: Integer;
  AData: pointer);
begin
  FParent := AParent;
  FResourceLanguage := ALanguage;
  FCodePage := LCIDToCodePage(FResourceLanguage);
  FResourceName := AName;
  FResourceType := AType;
  FData := TMemoryStream.Create;
  if AData <> nil then
    FData.Write(AData^, ASize)
  else
    InitNew
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.CreateNew                                           |
 |                                                                      |
 | Constructor to be used when adding new resources to a module.        |
 *----------------------------------------------------------------------*)
constructor TResourceDetails.CreateNew(AParent: TResourceModule;
  ALanguage: Integer; const aName: WideString);
begin
  FParent := AParent;
  FResourceLanguage := ALanguage;
  FCodePage := LCIDToCodePage(FResourceLanguage);
  FResourceName := AName;
  FResourceType := GetBaseType;
  if Assigned(AParent) then
    AParent.AddResource(Self);
  FData := TMemoryStream.Create;
  InitNew
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.CreateResourceDetails                               |
 *----------------------------------------------------------------------*)
class function TResourceDetails.CreateResourceDetails(
  AParent: TResourceModule;
  ALanguage: Integer; const AName, AType: WideString; ASize: Integer;
  AData: pointer): TResourceDetails;
begin
 Result := TResourceDetails.Create(AParent, ALanguage, AName, AType, ASize, AData)
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.Destroy                                             |
 *----------------------------------------------------------------------*)
destructor TResourceDetails.Destroy;
begin
  FData.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.GetBaseType                                         |
 |                                                                      |
 | Return the base type for the resource details.  This is overridden   |
 | in derived classes.                                                  |
 *----------------------------------------------------------------------*)
class function TResourceDetails.GetBaseType: WideString;
begin
  Result := '0';
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.InitNew                                             |
 |                                                                      |
 | Override this to initialize a new resource being added to a module.  |
 *----------------------------------------------------------------------*)
procedure TResourceDetails.InitNew;
begin
// Stub
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.SetResourceName                                     |
 |                                                                      |
 | Set the resource name.                                               |
 *----------------------------------------------------------------------*)
procedure TResourceDetails.SetResourceName(const Value: WideString);
begin
  if FResourceName <> Value then
   begin
    FResourceName := Value;
    FDirty := True
   end
end;

procedure TResourceDetails.SetResourceType(const Value: WideString);
begin
  if FResourceType <> Value then
   begin
    FResourceType := Value;
    FDirty := True
   end
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.SupportsData                                        |
 |                                                                      |
 | Can be overridden to support a custom resource class, where you can  |
 | determine the custom class from the data - eg. RIFF data, etc.       |
 *----------------------------------------------------------------------*)
class function TResourceDetails.SupportsData(Size: Integer;
  Data: Pointer): Boolean;
begin
  Result := False; // stub
end;

(*----------------------------------------------------------------------*
 | TResourceDetails.SupportsData                                        |
 |                                                                      |
 | Can be overridden to support RC data where you can determine the     |
 | type from the data and name - eg. the Delphi splash screen JPEG      |
 *----------------------------------------------------------------------*)
class function TResourceDetails.SupportsRCData(const AName: string;
  Size: Integer; Data: Pointer): Boolean;
begin
  Result := False; // stub
end;

{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'TResourceModule implementation'} {$ENDIF}
{ TResourceModule }

function TResourceModule.AddResource(details: TResourceDetails): Integer;
begin
  Result := -1
 // Stub
end;

procedure TResourceModule.ClearDirty;
var
  i: Integer;
begin
  FDirty := False;
  for i := 0 to ResourceCount - 1 do
    ResourceDetails[i].Dirty := False
end;

(*----------------------------------------------------------------------*
 | TResourceModule.DeleteResource                                       |
 |                                                                      |
 | Must be overridden to remove the resource details object from        |
 | wherever it's stored.  The overriding method must call               |
 | inherited                                                            |
 *----------------------------------------------------------------------*)
procedure TResourceModule.DeleteResource(idx: Integer);
begin
  FDirty := True;
  ResourceDetails[idx].BeforeDelete;
end;

(*----------------------------------------------------------------------*
 | TResourceModule.FindResource                                         |
 |                                                                      |
 | Find a resource with a given type/name                               |
 *----------------------------------------------------------------------*)
function TResourceModule.FindResource(const tp, Name: WideString;
  ALanguage: Integer): TResourceDetails;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to ResourceCount - 1 do
    if (ResourceDetails[i].FResourceType = tp) and
      (ResourceDetails[i].FResourceName = Name) and
      (Integer(ResourceDetails[i].FResourceLanguage) = ALanguage) then
     begin
      Result := ResourceDetails[i];
      break
     end;

  if not Assigned(Result) then
    for i := 0 to ResourceCount - 1 do
      if (ResourceDetails[i].FResourceType = tp) and
        (ResourceDetails[i].FResourceName = Name) and
        (ResourceDetails[i].FResourceLanguage = 0) then
       begin
        Result := ResourceDetails[i];
        break
       end
end;

(*----------------------------------------------------------------------*
 | TResourceModule.GetDirty                                             |
 |                                                                      |
 | Returns true if the module or it's resources are 'dirty'             |
 |                                                                      |
 | nb. FDirty is only set if resources have been deleted.               |
 |     After adding a resource make sure the resource's Dirty is set to |
 |     true.                                                            |
 *----------------------------------------------------------------------*)
function TResourceModule.GetDirty: Boolean;
var
  i: Integer;
begin
  Result := FDirty;
  if not FDirty then
    for i := 0 to ResourceCount - 1 do
      if ResourceDetails[i].Dirty then
       begin
        Result := True;
        break
       end
end;

(*----------------------------------------------------------------------*
 | TResourceModule.GetUniqueResourceName                                |
 |                                                                      |
 | Generate a unique resource name for a given type.  Names start at    |
 | 1 (though string lists downgrade that to '0')                        |
 *----------------------------------------------------------------------*)
function TResourceModule.GetUniqueResourceName(
  const tp: WideString): WideString;
var
  i: Integer;
  n, n1: Integer;
  details: TResourceDetails;
begin
  n := 0;

  for i := 0 to ResourceCount - 1 do
   begin
    details := ResourceDetails[i];
    if details.ResourceType = tp then
     begin
      n1 := ResourceNametoInt(details.ResourceName);
      if n1 > n then
        n := n1
     end
   end;

  Result := IntToStr(n + 1);
end;

(*----------------------------------------------------------------------*
 | TResourceModule.LoadFromFile                                         |
 |                                                                      |
 | Load from file.  This can be overriden but usually isn't as it       |
 | relies on LoadFromStream, which must be.                             |
 *----------------------------------------------------------------------*)
procedure TResourceModule.LoadFromFile(const FileName: string);
var
  s: TFileStream;
begin
  s := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
   try
    LoadFromStream(s);
   finally
    s.Free
   end;
end;

procedure TResourceModule.LoadFromStream(stream: TStream);
begin
  raise Exception.Create(rstNoStreaming);
end;

(*----------------------------------------------------------------------*
 | TResourceModule.SaveToFile                                           |
 |                                                                      |
 | Save to file.  This can be overriden but usually isn't as it         |
 | relies on SaveToStream, which must be.                               |
 *----------------------------------------------------------------------*)
procedure TResourceModule.SaveToFile(const FileName: string);
var
  s: TFileStream;
  oldFileName, ext: string;
  p: PChar;
begin
 if FBackup then
  begin
   // Rename old file to .~ext'
   oldFileName := FileName;
   UniqueString(oldFileName);
   p := StrRScan(PChar(oldFileName), '.');
   if p <> nil then
    begin
     p^ := #0;
     Inc(p);
     ext := p;
     oldFileName := PChar(oldFileName);
    end
   else ext := '';
   ext := '~' + ext;
   oldFileName := oldFileName + '.' + ext;

   if FileExists(oldFileName) then DeleteFile(oldFileName);

   RenameFile(FileName, oldFileName);
  end;

 try
  s := TFileStream.Create(FileName, fmCreate);
   try
    SaveToStream(s);
    ClearDirty;
   finally
    FreeAndNil(s)
   end
 except
  // Failed
  DeleteFile(FileName);

  if FBackup then
   begin
    // Rename old file back.
    RenameFile(oldFileName, FileName);
   end;
  raise
 end
end;

procedure TResourceModule.SaveToStream(Stream: TStream);
begin
  raise Exception.Create(rstNoStreaming);
end;

{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'TPEModule implementation'} {$ENDIF}
(*----------------------------------------------------------------------*
 | constructor TPEModule.Create                                          |
 |                                                                      |
 | Constructor for TPEModule instance.  Create empty section list       |
 *----------------------------------------------------------------------*)
constructor TPEModule.Create;
begin
  inherited Create;
  FSectionList := TObjectList.Create;
  FDOSStub := TMemoryStream.Create;
end;

(*----------------------------------------------------------------------*
 | procedure TPEModule.Decode                                            |
 |                                                                      |
 | Decode the PE file.  Load the DOS header, the COFF header and the    |
 | 'optional' header, then load each section into FSectionList          |
 *----------------------------------------------------------------------*)
procedure TPEModule.Decode(Memory: pointer; exeSize: Integer);
var
  offset: LongInt;
  i: Integer;
  sectionHeader: PImageSectionHeader;
  commentOffset: Integer;
begin
  FSectionList.Clear;

                                // Check it's really a PE file.
  if PWORD(Memory)^ <> IMAGE_DOS_SIGNATURE then
    raise EPEException.Create(rstInvalidDOSSignature);

                                // Load the DOS header
  FDOSHeader := PImageDosHeader(Memory)^;

  offset := FDOSHeader._lfanew;
  FDOSStub.Write((PChar(Memory) + SizeOf(FDOSHeader))^,
    FDOSHeader._lfanew - SizeOf(FDOSHeader));

                                // Check the COFF signature
  if PDWORD(PChar(Memory) + offset)^ <> IMAGE_NT_SIGNATURE then
    raise EPEException.Create(rstInvalidCOFFSignature);

                                // Load the COFF header
  Inc(offset, SizeOf(DWORD));
  FCOFFHeader := PImageFileHEader(PChar(Memory) + offset)^;

  Inc(offset, SizeOf(FCOFFHeader));

                                // Check the Optional Header signature.  nb
                                // the optional header is compulsory for
                                // 32 bit windows modules!
  if PWORD(PChar(Memory) + offset)^ <> IMAGE_NT_OPTIONAL_HDR_MAGIC then
    raise EPEException.Create(rstInvalidOptionalHeader);

                                // Save the 'optional' header
  ReallocMem(FOptionalHeader, FCOFFHeader.SizeOfOptionalHeader);
  Move((PChar(Memory) + Offset)^, FOptionalHeader^,
    FCOFFHeader.SizeOfOptionalHeader);

  Inc(offset, FCOFFHeader.SizeOfOptionalHeader);

  sectionHeader := PImageSectionHeader(PChar(memory) + offset);
  commentOffset := offset + FCOFFHeader.NumberOfSections *
    SizeOf(TImageSectionHeader);

// Save padding between the end of the section headers, and the start of the
// 1st section.  TDump reports this as 'comment', and it seems to be important
// to MS clock.exe...

  FCommentSize := Integer(sectionHeader^.PointerToRawData) - commentOffset;

  if FCommentSize > 0 then
   begin
    GetMem(FCommentBlock, FCommentSize);
    Move((PChar(memory) + commentOffset)^, FCommentBlock^, FCommentSize)
   end;
                                // Now save each image section in the FSectionList
  for i := 0 to FCOFFHeader.NumberOfSections - 1 do
   begin
    sectionHeader := PImageSectionHeader(PChar(memory) + offset);
    FSectionList.Add(TImageSection.Create(self, sectionHeader^,
      PChar(memory) + sectionHeader^.PointertoRawData));
    Inc(offset, SizeOf(TImageSectionHeader));
   end;

  i := sectionHeader^.PointerToRawData + sectionHeader^.SizeOfRawData;

// Save the padding between the last section and the end of the file.
// This appears to hold debug info and things ??

  FEndCommentSize := exeSize - i;
  if FEndCommentSize > 0 then
   begin
    GetMem(FEndComment, FEndCommentSize);
    Move((PChar(memory) + i)^, FEndComment^, FEndCommentSize)
   end
end;

(*----------------------------------------------------------------------*
 | destructor TPEModule.Destroy                                          |
 |                                                                      |
 | Destructor for TPEModule instance.                                   |
 *----------------------------------------------------------------------*)
destructor TPEModule.Destroy;
begin
  ReallocMem(FOptionalHeader, 0);
  FSectionList.Free;
  FDOSStub.Free;
  ReallocMem(FCommentBlock, 0);
  ReallocMem(FEndComment, 0);
  inherited;
end;

(*----------------------------------------------------------------------*
 | procedure TPEModule.Encode                                            |
 |                                                                      |
 | Fix up the data prior to writing to stream.                          |
 |                                                                      |
 | Ensure that the headers match what we've got...                      |
 *----------------------------------------------------------------------*)
procedure TPEModule.Encode;
var
  offset: DWORD;
  i: Integer;
  section: TImageSection;
  align: Integer;
  addrAlign: Integer;
  address: Integer;
  alignedSize, AddrAlignedSize: Integer;
  codeSize, iDataSize, uDataSize, iSize: Integer;
begin
  codeSize := 0;
  iDataSize := 0;
  uDataSize := 0;
                                               // Use the DOS stub from their .EXE
  FDOSHeader._lfanew := SizeOf(FDOSHeader) + FDOSStub.Size;

                                               // Fixup sections count
  FCOFFHeader.NumberOfSections := FSectionList.Count;

  iSize := FDOSHeader._lfanew +
               // File offset for start of sections
    SizeOf(DWORD) +                   // NT signature
    SizeOf(FCOFFHeader) +
    FCOFFHeader.SizeOfOptionalHeader + FSectionList.Count *
    SizeOf(TImageSectionHeader);

  offset := iSize + FCommentSize;

  align := FOptionalHeader^.FileAlignment;
  addrAlign := FOptionalHeader^.SectionAlignment;

  address := addrAlign;
  offset := DWORD((integer(offset) + align - 1) div align * align);

                                                // First section starts at $1000 (when loaded)
                                                // and at 'offset' in file.

  FOptionalHeader^.SizeOfHeaders :=
    DWORD((integer(iSize) + align - 1) div align * align);

  FOptionalHeader^.BaseOfCode := $ffffffff;
  FOptionalHeader^.CheckSum := 0;
               // Calculate it during 'SaveToStream' when
                                                // we've got all the info.

  iSize := DWORD((integer(iSize) + addrAlign - 1) div
    addrAlign * addrAlign);

  for i := 0 to FSectionList.Count - 1 do
      // Recalculate the section offsets
   begin
    section := TImageSection(FSectionList[i]);

    section.FSectionHeader.PointerToRawData := offset;
    section.FSectionHeader.VirtualAddress := address;

// Virtual size is size of data in memory, and is not padded to an 'alignment'.

// SizeOfRawData is size of data in file, padded to (file) alignment.

// 1.  If VirtualSize < SizeOfRawData, that's simply because the raw data is aligned, and virt data isn't.

// 2.  If VirtualSize > SizeOfRawData, the additional memory is filled with zeros when it's loaded.

// Because SizeOfRawData is padded it's impossible to tell how much Virtual Memory is really required.

// We do our best by saving the original difference in '2.' above in fUninitializeDataSize

    section.FSectionHeader.Misc.VirtualSize :=
      section.FRawData.Size + section.FUninitializedDataSize;
    section.FSectionHeader.SizeOfRawData :=
      (section.FRawData.Size + align - 1) div align * align;

    alignedSize := (Integer(section.FSectionHeader.Misc.VirtualSize) +
      align - 1) div align * align;
    addrAlignedSize := (Integer(section.FSectionHeader.Misc.VirtualSize) +
      addrAlign - 1) div addrAlign * addrAlign;

    if (section.FSectionHeader.Characteristics and
      IMAGE_SCN_MEM_EXECUTE) <> 0 then
     begin
      Inc(codeSize, alignedSize);
      if DWORD(address) < FOptionalHeader^.BaseOfCode then
        FOptionalHeader^.BaseOfCode := address
     end
    else
    if (section.FSectionHeader.Characteristics and
      IMAGE_SCN_CNT_INITIALIZED_DATA) <> 0 then
      Inc(iDataSize, alignedSize)
    else
    if (section.FSectionHeader.Characteristics and
      IMAGE_SCN_CNT_UNINITIALIZED_DATA) <> 0 then
      Inc(uDataSize, alignedSize);

    Inc(iSize, addrAlignedSize);
    Inc(offset, section.FSectionHeader.SizeOfRawData);
    Inc(address, (Integer(section.FSectionHeader.Misc.VirtualSize) +
      addrAlign - 1) div addrAlign * addrAlign);
   end;

  FOptionalHeader^.SizeOfCode := codeSize;
  FOptionalHeader^.SizeOfInitializedData := iDataSize;
  FOptionalHeader^.SizeOfUninitializedData := uDataSize;

  i := SizeOf(DWORD) +                   // NT signature
    SizeOf(FCOFFHeader) + FCOFFHeader.SizeOfOptionalHeader +
    codeSize;

  i := (i + addrAlign - 1) div addrAlign * addrAlign;

  // With explorer.exe, codeSize is $14800, i is 148E8, so aligned 'i' is $15000
  // .. so BaseOfData should be $15000 + BaseOfCode ($1000) = $16000.

  // ... but it's not - it's $15000, which means that the last $8e8 bytes of code
  // should be stampled over by the data!

  // But obviously explorer.exe works, so I'm, missing a trick here.  Never mind - it
  // doesn't do any harm making it $16000 instead, and the formula works for everything
  // else I've tested...

  FOptionalHeader^.BaseOfData := FOptionalHeader.BaseOfCode + DWORD(i);

  FOptionalHeader^.SizeOfImage := iSize;
end;

(*----------------------------------------------------------------------*
 | function TPEModule.FindDictionaryEntrySection                         |
 |                                                                      |
 | Return the index of the specified section.  The 'entryNo' to find    |
 | should be a 'IMAGE_DIRECTORY_ENTRY_xxxx' constant defined in         |
 | Windows.pas.                                                         |
 *----------------------------------------------------------------------*)
function TPEModule.FindDictionaryEntrySection(entryNo: Integer;
  var offset: Integer): Integer;
var
  i: Integer;
  p: PImageDataDirectory;
begin
  Result := -1;
  p := DataDictionary[entryNo];
                                // Find section with matching virt address.
  for i := 0 to ImageSectionCount - 1 do
    if (p^.VirtualAddress >=
      ImageSection[i].FSectionHeader.VirtualAddress) and
      (p^.VirtualAddress < ImageSection[i].FSectionHeader.VirtualAddress +
      ImageSection[i].FSectionHeader.Misc.VirtualSize) then
     begin
      Result := i;
      offset := p^.VirtualAddress -
        ImageSection[i].FSectionHeader.VirtualAddress;
      break
     end
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetCOFFHeader                                     |
 |                                                                      |
 | Return COFF header                                                   |
 *----------------------------------------------------------------------*)
function TPEModule.GetCOFFHeader: TImageFileHeader;
begin
  Result := FCOFFHeader;
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetDataDictionary                                 |
 |                                                                      |
 | Return the data dictionary for a specified                           |
 | IMAGE_DIRECTORY_ENTRY_xxxx  index                                    |
 *----------------------------------------------------------------------*)
function TPEModule.GetDataDictionary(index: Integer): PImageDataDirectory;
var
  p: PImageDataDirectory;
begin
  if index < DataDictionaryCount then
   begin
    p := @FOptionalHeader.DataDirectory[0];
    Inc(p, index);
    Result := p
   end
  else
    raise ERangeError.Create(rstBadDictionaryIndex);
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetDataDictionaryCount                            |
 |                                                                      |
 | Return no of entries in the Data Directory                           |
 *----------------------------------------------------------------------*)
function TPEModule.GetDataDictionaryCount: Integer;
begin
  Result := FOptionalHeader^.NumberOfRvaAndSizes
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetDosHeader                                      |
 |                                                                      |
 | Return DOS header                                                    |
 *----------------------------------------------------------------------*)
function TPEModule.GetDOSHeader: TImageDosHeader;
begin
  Result := FDOSHeader;
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetImageSection () : TImageSection                |
 |                                                                      |
 | Get the specified image section                                      |
 *----------------------------------------------------------------------*)
function TPEModule.GetExportCount: Integer;
var
  ExportSection: PImageExportDirectory;
  section: TImageSection;
  offset: Integer;
begin
  section := GetExportSection(offset);
  if Assigned(section) then
   begin
    ExportSection := PImageExportDirectory(
      PChar(section.FRawData.memory) + offset);
    Result := ExportSection^.NumberOfNames
   end
  else
    Result := 0;
end;

procedure TPEModule.GetExportDetails(idx: Integer; var Name: string;
  var ordinal: DWORD);
var
  ExportSection: PImageExportDirectory;
  section: TImageSection;
  offset: Integer;
  po: DWORD;
  pw: PWORD;
  p: PDWORD;
  Data: PChar;
begin
  section := GetExportSection(offset);
  if Assigned(section) then
   begin
    Data := GetExportSectionData;
    ExportSection := PImageExportDirectory(
      PChar(section.FRawData.memory) + offset);
    po := DWORD(ExportSection^.AddressOfNameOrdinals);
    pw := PWORD(Data + po);
    Inc(pw, idx);
    ordinal := pw^;

    po := DWORD(ExportSection^.AddressOfNames);
    p := PDWORD(Data + po);
    Inc(p, idx);
    Name := Data + p^
   end
end;

function TPEModule.GetExportSection(var offset: Integer): TImageSection;
var
  idx: Integer;
begin
  offset := 0;
  idx := FindDictionaryEntrySection(IMAGE_DIRECTORY_ENTRY_EXPORT, offset);
  if idx = -1 then
    Result := nil
  else
    Result := ImageSection[idx]
end;

function TPEModule.GetExportSectionData: PChar;
var
  section: TImageSection;
  offset: Integer;
begin
  section := GetExportSection(offset);
  Result := PChar(section.FRawData.Memory) -
    section.FSectionHeader.VirtualAddress;
end;

function TPEModule.GetImageSection(index: Integer): TImageSection;
begin
  Result := TImageSection(FSectionList[index]);
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetImageSectionCount                              |
 |                                                                      |
 | Return no of image sections                                          |
 *----------------------------------------------------------------------*)
function TPEModule.GetImageSectionCount: Integer;
begin
  Result := FSectionList.Count
end;

function DirValid(dir: PImageImportDirectory): boolean;
begin
  DirValid := (dir^.Characteristics <> 0) or (dir^.TimeDateStamp <> 0) or
    (dir^.ForwarderChain <> 0) or (dir^.Name <> 0) or
    (dir^.FirstThunk <> 0)
end;

(*----------------------------------------------------------------------*
 | function TPEModule.GetImageSectionCount                              |
 |                                                                      |
 | Get the optional header                                              |
 *----------------------------------------------------------------------*)
function TPEModule.GetImport(idx: Integer): PImageImportDirectory;
var
  ImportSection: PImageImportDirectory;
  section: TImageSection;
  offset: Integer;

begin
  section := GetImportSection(offset);
  Result := nil;
  if Assigned(section) then
   begin
    ImportSection := PImageImportDirectory(
      PChar(section.FRawData.memory) + offset);

    while DirValid(ImportSection) and (idx > 0) do
     begin
      Inc(ImportSection);
      Dec(idx)
     end;

    if DirValid(ImportSection) then
      Result := ImportSection
   end
end;

function TPEModule.GetImportCount: Integer;
var
  ImportSection: PImageImportDirectory;
  section: TImageSection;
  offset: Integer;
begin
  section := GetImportSection(offset);
  Result := 0;
  if Assigned(section) then
   begin
    ImportSection := PImageImportDirectory(
      PChar(section.FRawData.memory) + offset);

    while DirValid(ImportSection) do
     begin
      Inc(Result);
      Inc(ImportSection)
     end
   end
end;

function TPEModule.GetImportSection(var offset: Integer): TImageSection;
var
  idx: Integer;
begin
  idx := FindDictionaryEntrySection(IMAGE_DIRECTORY_ENTRY_IMPORT, offset);
  if idx = -1 then
    Result := nil
  else
    Result := ImageSection[idx]
end;

function TPEModule.GetImportSectionData: PChar;
var
  section: TImageSection;
  offset: Integer;
begin
  section := GetImportSection(offset);
  Result := PChar(section.FRawData.Memory) -
    section.FSectionHeader.VirtualAddress;
end;

function TPEModule.GetOptionalHeader: TImageOptionalHeader;
begin
  Result := FOptionalHeader^
end;

function TPEModule.GetResourceSection(var offset: Integer): TImageSection;
var
  idx: Integer;
begin
  idx := FindDictionaryEntrySection(IMAGE_DIRECTORY_ENTRY_RESOURCE, offset);
  if idx = -1 then
    Result := nil
  else
    Result := ImageSection[idx]
end;

(*----------------------------------------------------------------------*
 | procedure TPEModule.LoadFromFile                                     |
 |                                                                      |
 | Load the module from a file                                          |
 *----------------------------------------------------------------------*)
procedure TPEModule.LoadFromFile(const Name: string);
var
  f: TFileStream;
begin
  f := TFileStream.Create(Name, fmOpenRead or fmShareDenyNone);
   try
    LoadFromStream(f)
   finally
    f.Free
   end
end;

(*----------------------------------------------------------------------*
 | procedure TPEModule.LoadFromFile                                     |
 |                                                                      |
 | Load the module from a stream                                        |
 *----------------------------------------------------------------------*)
procedure TPEModule.LoadFromStream(s: TStream);
var
  m: TMemoryStream;
begin
  m := TMemoryStream.Create;
   try
    m.CopyFrom(s, 0);

    Decode(m.memory, m.size)
   finally
    m.Free
   end
end;

(*----------------------------------------------------------------------*
 | procedure TPEModule.SaveToFile                                       |
 |                                                                      |
 | Save the module to a file                                            |
 *----------------------------------------------------------------------*)
(*
procedure TPEModule.SaveToFile(const name: string);
var
  f : TFileStream;
begin
  f := TFileStream.Create (name, fmCreate);
  try
    SaveToStream (f)
  finally
    f.Free
  end
end;
*)
(*----------------------------------------------------------------------*
 | procedure TPEModule.SaveToStream                                     |
 |                                                                      |
 | Save the module to a stream                                          |
 *----------------------------------------------------------------------*)
procedure TPEModule.SaveToStream(Stream: TStream);
var
  NTSignature : DWORD;
  i           : Integer;
  Section     : TImageSection;
  PaddingSize : Integer;
  PaddingLen  : Integer;
  Padding     : PChar;
  OldCheckSum : DWORD;
  NewCheckSum : DWORD;
  NtHeaders   : PImageNtHeaders;
  CkOffset    : DWORD;
begin
 // Encode the data.
 Encode;

 NTSignature := IMAGE_NT_SIGNATURE;

 // Write the DOS stub
 Stream.Write(FDOSHeader, SizeOf(FDOSHeader));
 Stream.CopyFrom(FDOSStub, 0);

 // Write NT sig and COFF header
 Stream.Write(NTSignature, SizeOf(NTSignature));
 Stream.Write(FCOFFHeader, SizeOf(FCOFFHeader));
 CkOffset := Stream.Position + Integer(@FOptionalHeader^.CheckSum) -
   Integer(@FOptionalHeader^);
 Stream.Write(FOptionalHeader^, FCOFFHeader.SizeOfOptionalHeader);

                               // Write the Section headers
 for i := 0 to FSectionList.Count - 1 do
  begin
   Section := TImageSection(FSectionList[i]);
   Stream.Write(Section.FSectionHeader, SizeOf(Section.FSectionHeader))
  end;

 // Save the 'comment' Section.  See 'Decode' for details
 if FCommentSize > 0
  then Stream.Write(FCommentBlock^, FCommentSize);

 // Write the sections
 Padding := nil;
 PaddingLen := 0;
 try
  for i := 0 to FSectionList.Count - 1 do
   begin
    // Write Padding up to file offset of the Section
    Section := TImageSection(FSectionList[i]);
    PaddingSize := Section.FSectionHeader.PointerToRawData - DWORD(Stream.Position);

    if PaddingSize > PaddingLen then
     begin
      PaddingLen := PaddingSize + 65536;
      ReallocMem(Padding, PaddingLen);
      ZeroMemory(Padding, PaddingLen);
     end;

    // Put our signature at the start of the first
    if PaddingSize > 0 then Stream.Write(Padding^, PaddingSize);

    // Write the Section data.
    Stream.CopyFrom(Section.FRawData, 0);

    // Write data
    with Section.FSectionHeader
     do PaddingSize := SizeOfRawData - misc.VirtualSize;

    // Pad data
    if PaddingSize > PaddingLen then
     begin
      PaddingLen := PaddingSize + 65536;
      ReallocMem(Padding, PaddingLen);
      ZeroMemory(Padding, PaddingLen);
     end;

    if PaddingSize > 0
     then Stream.Write(Padding^, PaddingSize)
   end;

  // Save the debug info.
  if FEndCommentSize > 0
   then Stream.Write(FEndComment^, FEndCommentSize)
 finally
  ReallocMem(Padding, 0)
 end;

 // Now calculate the checksum....
 with TMemoryStream.Create do
  try
   Stream.Seek(0, soFromBeginning);
   LoadFromStream(Stream);
   NtHeaders := ChecksumMappedFile(Memory, Size, @OldCheckSum, @NewCheckSum);

   if Assigned(NtHeaders) then
    begin
     Stream.Seek(CkOffset, soFromBeginning);
     Stream.Write(NewCheckSum, SizeOf(NewCheckSum))
    end
  finally
   Free
  end;

 Stream.Seek(0, soFromEnd);
end;
{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'TImageSection implementation'} {$ENDIF}
{ TImageSection }

(*----------------------------------------------------------------------*
 | constructor TImageSection.Create                                     |
 |                                                                      |
 | Constructor for TImageSection.                                       |
 *----------------------------------------------------------------------*)
constructor TImageSection.Create(AParent: TPEModule;
  const AHeader: TImageSectionHeader; rawData: pointer);
begin
  FSectionHeader := AHeader;
  FRawData := TMemoryStream.Create;

//  nb.  SizeOfRawData is usually bigger than VirtualSize because it's padded,
//       and VirtualSize isn't.


  if FSectionHeader.Misc.VirtualSize <= FSectionHeader.SizeOfRawData then
   begin

// Some linkers (?) set VirtualSize to 0 - which isn't correct.  Work round it.
// (Encountered in MCL Link Lite HHT software )

    if FSectionHeader.Misc.VirtualSize = 0 then
      FSectionHeader.Misc.VirtualSize := FSectionHeader.SizeOfRawData;
    FRawData.Write(rawData^, FSectionHeader.Misc.VirtualSize)
   end
  else

// nb.  If VirtualSize is bigger than SizeOfRawData it implies that extra padding is required.
//      Save the amount, so we can get all the COFF header values right.  See 'Encode' above.
   begin
    FRawData.Write(rawData^, FSectionHeader.SizeOfRawData);
    FUninitializedDataSize :=
      FSectionHeader.Misc.VirtualSize - FSectionHeader.SizeOfRawData;
   end;

  FParent := AParent;
end;

(*----------------------------------------------------------------------*
 | function TImageSection.GetSectionName                                |
 |                                                                      |
 | Return the section name - eg. .data                                  |
 *----------------------------------------------------------------------*)
function TImageSection.GetSectionName: string;
begin
  Result := PChar(@FSectionHeader.Name)
end;

(*----------------------------------------------------------------------*
 | destructor TImageSection.Destroy                                     |
 |                                                                      |
 | destructor for TImageSection.                                        |
 *----------------------------------------------------------------------*)
destructor TImageSection.Destroy;
begin
  FRawData.Free;
  inherited;
end;
{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'TPEResourceModule implementation'} {$ENDIF}
{ TPEResourceModule }

(*----------------------------------------------------------------------*
 | procedure TPEResourceModule.DeleteResource                           |
 |                                                                      |
 | Delete the specified resource (by index)                             |
 *----------------------------------------------------------------------*)
procedure TPEResourceModule.DeleteResource(resourceNo: Integer);
var
  res: TResourceDetails;
begin
  res := ResourceDetails[resourceNo];
  inherited;
  resourceNo := IndexOfResource(Res);
  if resourceNo <> -1 then
    FDetailList.Delete(resourceNo);
end;

(*----------------------------------------------------------------------*
 | constructor TPEResourceModule.Create                                 |
 |                                                                      |
 | Constructor for TPEResourceModule                                    |
 *----------------------------------------------------------------------*)
constructor TPEResourceModule.Create;
begin
  inherited Create;
  FDetailList := TObjectList.Create;
end;

(*----------------------------------------------------------------------*
 | destructor TPEResourceModule.Destroy                                 |
 |                                                                      |
 | Destructor for TPEResourceModule                                     |
 *----------------------------------------------------------------------*)
destructor TPEResourceModule.Destroy;
begin
  FDetailList.Free;
  inherited;
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.Decode                                    |
 |                                                                      |
 | Decode the section's resource tree into a list of resource details   |
 *----------------------------------------------------------------------*)
procedure TPEResourceModule.Decode;
var
  section: TImageSection;
  tp, Name: string;
  lang: Integer;
  offset: Integer;

  // Get string resource name
  function GetResourceStr(IdorName: boolean; section: TImageSection;
    n: DWORD): string;
  var
    p: PWideChar;
  begin
    if IdorName then
      Result := IntToStr(n)
    else
     begin
      p := PWideChar(PChar(section.FRawData.Memory) + (n and $7fffffff));
      Result := ResourceWideCharToStr(p, CP_ACP)
     end
  end;

  // (recursively) get resources
  procedure GetResource(offset, level: Integer);
  var
    entry: PResourceDirectoryEntry;
    i, Count: Integer;
    IDorName: boolean;
    dataEntry: PResourceDataEntry;
    table: PResourceDirectoryTable;
    details: TResourceDetails;
  begin
    table := PResourceDirectoryTable(
      PChar(section.FRawData.memory) + offset);
    with table^ do
      Count := cNameEntries + cIDEntries;

    entry := PResourceDirectoryEntry(PChar(section.FRawData.memory) +
      offset + SizeOf(TResourceDirectoryTable));
    for i := 0 to Count - 1 do
     begin
      idOrName := i >= table^.cNameEntries;
      case level of
        0 : tp := GetResourceStr(IDOrName, section, entry^.Name);
        1 :
          Name := GetResourceStr(IDOrName, section, entry^.Name);
        2 :
         begin
          if not IdOrName then
            raise EPEException.Create(rstBadLangID);

          lang := entry^.Name
         end
       end;

      if (entry^.RVA and $80000000) > 0 then
 // Not a leaf node - traverse the tree
        GetResource(entry^.RVA and $7fffffff, level + 1)
      else
       begin
                                             // It's a leaf node - create resource details
        dataEntry := PResourceDataEntry(PChar(section.FRawData.Memory) +
          entry^.RVA);
        details := TResourceDetails.CreateResourceDetails(self,
          lang, Name, tp, dataEntry^.Size, PChar(section.FRawData.Memory) +
          dataEntry^.OffsetToData - section.FSectionHeader.VirtualAddress);
        details.CodePage := dataEntry^.CodePage;
        details.Characteristics := table^.characteristics;
        details.DataVersion :=
          DWORD(table^.versionMajor) * 65536 + DWORD(table^.versionMinor);
        FDetailList.Add(details);

       end;

      Inc(entry)
     end
  end;

begin
  inherited;
  section := GetResourceSection(offset);
  if section <> nil then
    GetResource(offset, 0)
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.GetResourceCount                          |
 |                                                                      |
 | Return the number of resources in the resource section               |
 *----------------------------------------------------------------------*)
function TPEResourceModule.GetResourceCount: Integer;
begin
  Result := FDetailList.Count
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.GetResourceDetails                        |
 |                                                                      |
 | Get the resource details for the specified resource.                 |
 *----------------------------------------------------------------------*)
function TPEResourceModule.GetResourceDetails(idx: Integer):
TResourceDetails;
begin
  Result := TResourceDetails(FDetailList[idx]);
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.IndexOfResource                           |
 |                                                                      |
 | Return the index of the specified resource details in the resource   |
 *----------------------------------------------------------------------*)
function TPEResourceModule.IndexOfResource(details: TResourceDetails): Integer;
begin
  Result := FDetailList.IndexOf(details);
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.InsertResource                            |
 |                                                                      |
 | Insert a resource in the list.                                       |
 *----------------------------------------------------------------------*)
procedure TPEResourceModule.InsertResource(idx: Integer;
  details: TResourceDetails);
begin
  FDetailList.Insert(idx, details);
end;

(*----------------------------------------------------------------------*
 | function TPEResourceModule.Encode                                    |
 |                                                                      |
 | Complicated?  I'll give you complicated ...                          |
 *----------------------------------------------------------------------*)
procedure TPEResourceModule.Encode;
var
  i: Integer;
  details: TResourceDetails;
  section: TImageSection;
  root: TResourceNode;
  versMajor, versMinor: word;
  TimeStamp: DWORD;
  nameSize, nameOffset, namePos, tableOffset: DWORD;
  deOffset, dePos, deSize: DWORD;
  dataOffset, dataPos, dataSize: DWORD;
  offset: Integer;

  nameTable: PChar;
  deTable: PChar;
  Data: PChar;
  zeros: PChar;

  //------------------------------------------------------------------
  // Calculate offset and size of name table and DirectoryEntry table.
  // Calculate size of data

  procedure GetNameTableSize(node: TResourceNode);
  var
    i: Integer;
  begin
    Inc(nameOffset, SizeOf(TResourceDirectoryTable));
    Inc(deOffset, SizeOf(TResourceDirectoryTable));

    for i := 0 to node.Count - 1 do
     begin
      Inc(nameOffset, SizeOf(TResourceDirectoryEntry));
      Inc(deOffset, SizeOf(TResourceDirectoryEntry));

      if not node.nodes[i].intID then
        Inc(nameSize, Length(node.nodes[i].id) * SizeOf(WideChar) +
          SizeOf(word));

      if not node.nodes[i].leaf then
        GetNameTableSize(node.nodes[i].Next)
      else
       begin
        Inc(nameOffset, SizeOf(TResourceDataEntry));
        Inc(deSize, SizeOf(TResourceDataEntry));
        dataSize := (dataSize + DWORD(node.nodes[i].Data.Size) +
          3) div 4 * 4;
       end
     end
  end;

  //------------------------------------------------------------------
  // Save a node to section.FRawData (and save it's child nodes recursively)

  procedure SaveToSection(node: TResourceNode);
  var
    table: TResourceDirectoryTable;
    entry: TResourceDirectoryEntry;
    dataEntry: PResourceDataEntry;
    i, n: Integer;
    w: WideString;
    wl: word;

  //------------------------------------------------------------------
  // Save entry (i), and the child nodes

    procedure SaveNode(i: Integer);
    begin
      if node.nodes[i].intID then      // id is a simple integer
        entry.Name := StrToInt(node.nodes[i].id)
      else
       begin                             // id is an offset to a name in the
                                        // name table.
        entry.Name := nameOffset + namePos + $80000000;
        w := node.nodes[i].id;
        wl := Length(node.nodes[i].id);
        Move(wl, nameTable[namePos], SizeOf(wl));
        Inc(namePos, SizeOf(wl));
        Move(w[1], nameTable[namePos], wl * SizeOf(WideChar));
        Inc(namePos, wl * SizeOf(WideChar))
       end;

      if node.nodes[i].leaf then
       // RVA points to a TResourceDataEntry in the
       begin                             // data entry table.
        entry.RVA := deOffset + dePos;
        dataEntry := PResourceDataEntry(deTable + dePos);
        dataEntry^.CodePage := node.nodes[i].CodePage;
        dataEntry^.Reserved := 0;
        dataEntry^.Size := node.nodes[i].Data.Size;
        dataEntry^.OffsetToData :=
          dataOffset + dataPos + section.FSectionHeader.VirtualAddress;

        Move(node.nodes[i].Data.memory^, Data[dataPos], dataEntry^.Size);

        Inc(dePos, SizeOf(TResourceDataEntry));
        dataPos := (dataPos + dataEntry^.size + 3) div 4 * 4;
        section.FRawData.Write(entry, SizeOf(entry));
       end
      else                              // RVA points to another table.
       begin
        entry.RVA := $80000000 + tableOffset;
        section.FRawData.Write(entry, SizeOf(entry));
        n := section.FRawData.Position;
        SaveToSection(node.nodes[i].Next);
        section.FRawData.Seek(n, soFromBeginning);
       end
    end;

  begin { SaveToSection }
    table.characteristics := 0;
    table.timeDateStamp := TimeStamp;
    table.versionMajor := versMajor;
    table.versionMinor := versMinor;
    table.cNameEntries := 0;
    table.cIDEntries := 0;

                                        // Calculate no of integer and string IDs
    for i := 0 to node.Count - 1 do
      if node.nodes[i].intID then
        Inc(table.cIDEntries)
      else
        Inc(table.cNameEntries);

    section.FRawData.Seek(tableOffset, soFromBeginning);
    section.FRawData.Write(table, SizeOf(table));

    tableOffset := tableOffset + SizeOf(TResourceDirectoryTable) +
      DWORD(node.Count) * SizeOf(TResourceDirectoryEntry);

                                        // The docs suggest that you save the nodes
                                        // with string entries first.  Goodness knows why,
                                        // but play along...
    for i := 0 to node.Count - 1 do
      if not node.nodes[i].intID then
        SaveNode(i);

    for i := 0 to node.Count - 1 do
      if node.nodes[i].intID then
        SaveNode(i);

    section.FRawData.Seek(0, soFromEnd);
  end;


begin { Encode }
 section := GetResourceSection(offset);

 // Get the details in a tree structure
 root := nil;
 Data := nil;
 deTable := nil;
 zeros := nil;

 try
  for i := 0 to FDetailList.Count - 1 do
   begin
    details := TResourceDetails(FDetailList.Items[i]);
    if root = nil
     then root := TResourceNode.Create(details.ResourceType,
            details.ResourceName, details.ResourceLanguage, details.Data,
            details.CodePage)
     else root.Add(details.ResourceType, details.ResourceName,
            details.ResourceLanguage, details.Data, details.CodePage)
   end;

  // Save elements of their original EXE
  versMajor := PResourceDirectoryTable(section.FRawData.Memory)^.versionMajor;
  versMinor := PResourceDirectoryTable(section.FRawData.Memory)^.versionMinor;
  TimeStamp := PResourceDirectoryTable(section.FRawData.Memory)^.timeDateStamp;


  // Clear the data.  We're gonna recreate  it from our resource details.
  Section.FRawData.Clear;

  nameSize := 0;
  nameOffset := offset;
  deSize := 0;
  deOffset := offset;
  dataSize := 0;

  GetNameTableSize(root);              // Calculate sizes and offsets of the
                                       // name table, the data entry table and
                                       // the size of the data.

                                          // Calculate the data offset.  Must be aligned.
  DataOffset := (nameOffset + nameSize + 15) div 16 * 16;

  // Initialize globals...

  // Offset of next entry in the string table
  namePos := 0;
  // Offset of next entry in the data entry table
  dePos := 0;
  // Offset of next data block.
  dataPos := 0;
  // Offset of next TResourceDirectoryTable
  tableOffset := 0;

  GetMem(nameTable, nameSize);         // Allocate buffers for tables
  GetMem(Data, dataSize);
  GetMem(deTable, deSize);

  SaveToSection(root);               // Do the work.

  // Save the tables
  section.FRawData.Write(deTable^, deSize);
  section.FRawData.Write(nameTable^, nameSize);

  // Add padding so the data goes on a
  // 16 byte boundary.
  if DWORD(section.FRawData.Position) < dataOffset then
   begin
    GetMem(zeros, dataOffset - DWORD(section.FRawData.Position));
    ZeroMemory(zeros, dataOffset - DWORD(section.FRawData.Position));
    section.FRawData.Write(zeros^, dataOffset - DWORD(section.FRawData.Position))
   end;

  // Write the data.
  section.FRawData.Write(Data^, dataSize);

  inherited; // **** Must call inherited !

 finally       // Tidy up.
  ReallocMem(zeros, 0);
  FreeMem(nameTable);
  FreeMem(deTable);
  FreeMem(Data);
  FreeAndNil(root);
 end
end;
{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'TResourceNode implementation'} {$ENDIF}
{ TResourceNode }

procedure TResourceNode.Add(const AType, AName: string;
  ALang: Integer; aData: TMemoryStream; codePage: DWORD);
var
  i: Integer;

begin
  for i := 0 to Count - 1 do
    if AType = nodes[i].id then
     begin
      nodes[i].Next.AddName(AName, ALang, aData, codePage);
      exit
     end;

  Inc(Count);
  SetLength(nodes, Count);
  nodes[Count - 1].id := AType;
  nodes[Count - 1].intID := isID(Count - 1);
  nodes[Count - 1].leaf := False;
  nodes[Count - 1].Next :=
    TResourceNode.CreateNameNode(AName, ALang, AData, codePage)
end;

procedure TResourceNode.AddLang(ALang: Integer; aData: TMemoryStream;
  codePage: DWORD);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if IntToStr(ALang) = nodes[i].id then
     begin
      nodes[i].Data := aData;
      exit
     end;

  Inc(Count);
  SetLength(nodes, Count);
  nodes[Count - 1].id := IntToStr(ALang);
  nodes[Count - 1].intId := True;
  nodes[Count - 1].leaf := True;
  nodes[Count - 1].Data := aData;
  nodes[Count - 1].CodePage := codePage;
end;

procedure TResourceNode.AddName(const AName: string; ALang: Integer;
  aData: TMemoryStream; codePage: DWORD);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if AName = nodes[i].id then
     begin
      nodes[i].Next.AddLang(ALang, aData, codePage);
      exit
     end;

  Inc(Count);
  SetLength(nodes, Count);
  nodes[Count - 1].id := AName;
  nodes[Count - 1].intID := isID(Count - 1);
  nodes[Count - 1].leaf := False;
  nodes[Count - 1].Next :=
    TResourceNode.CreateLangNode(ALang, aData, codePage)
end;

constructor TResourceNode.Create(const AType, AName: string;
  ALang: Integer; aData: TMemoryStream; codePage: DWORD);
begin
  Count := 1;
  SetLength(nodes, 1);
  nodes[0].id := AType;
  nodes[Count - 1].intID := isID(Count - 1);
  nodes[0].leaf := False;
  nodes[0].Next := TResourceNode.CreateNameNode(AName, ALang,
    aData, codePage);
end;

constructor TResourceNode.CreateLangNode(ALang: Integer;
  aData: TMemoryStream; codePage: DWORD);
begin
  Count := 1;
  SetLength(nodes, 1);
  nodes[0].id := IntToStr(ALang);
  nodes[Count - 1].intID := True;
  nodes[0].leaf := True;
  nodes[0].Data := aData;
  nodes[0].CodePage := codePage
end;

constructor TResourceNode.CreateNameNode(const AName: string;
  ALang: Integer; aData: TMemoryStream; codePage: DWORD);
begin
  Count := 1;
  SetLength(nodes, 1);
  nodes[0].id := AName;
  nodes[Count - 1].intID := isID(Count - 1);

  nodes[0].leaf := False;
  nodes[0].Next := TResourceNode.CreateLangNode(ALang, aData, codePage)
end;

destructor TResourceNode.Destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    if not nodes[i].leaf then
      nodes[i].Next.Free;

  inherited;
end;

function TResourceNode.IsID(idx: Integer): boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 1 to Length(nodes[idx].id) do
    if not (nodes[idx].id[i] in ['0'..'9']) then
     begin
      Result := False;
      break
     end;

  if Result then
    Result := IntToStr(StrToInt(nodes[idx].id)) = nodes[idx].id;
end;

function TPEResourceModule.AddResource(details: TResourceDetails): Integer;
begin
  Result := FDetailList.Add(details);
end;

procedure TPEResourceModule.SortResources;
begin
  FDetailList.Sort(compareDetails);
end;
{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

end.
