unit DAV_DLLLoader;

interface

uses
  Windows, Classes;

const
  IMPORTED_NAME_OFFSET = $00000002;
  IMAGE_ORDINAL_FLAG32 = $80000000;
  IMAGE_ORDINAL_MASK32 = $0000FFFF;

  IMAGE_REL_BASED_ABSOLUTE = 0;
  IMAGE_REL_BASED_HIGH = 1;
  IMAGE_REL_BASED_LOW = 2;
  IMAGE_REL_BASED_HIGHLOW = 3;
  IMAGE_REL_BASED_HIGHADJ = 4;
  IMAGE_REL_BASED_MIPS_JMPADDR = 5;
  IMAGE_REL_BASED_SECTION = 6;
  IMAGE_REL_BASED_REL32 = 7;

type
  PPLongWord = ^PLongWord;
  PPWord = ^PWord;

  PWordArray = ^TWordArray;
  TWordArray = array[0..(2147483647 div SizeOf(Word)) - 1] of Word;

  PLongWordArray = ^TLongWordArray;
  TLongWordArray = array [0..(2147483647 div SizeOf(LongWord)) - 1] of LongWord;

  PImageDOSHeader = ^TImageDOSHeader;
  TImageDOSHeader = packed record
    Signature : Word;
    PartPag   : Word;
    PageCnt   : Word;
    ReloCnt   : Word;
    HdrSize   : Word;
    MinMem    : Word;
    MaxMem    : Word;
    ReloSS    : Word;
    ExeSP     : Word;
    ChkSum    : Word;
    ExeIP     : Word;
    ReloCS    : Word;
    TablOff   : Word;
    Overlay   : Word;
    Reserved  : packed array [0..3] of Word;
    OEMID     : Word;
    OEMInfo   : Word;
    Reserved2 : packed array [0..9] of Word;
    LFAOffset : LongWord;
  end;

  TISHMisc = packed record
    case Integer of
      0: (PhysicalAddress: LongWord);
      1: (VirtualSize: LongWord);
  end;

  PImageSectionHeaders = ^TImageSectionHeaders;
  TImageSectionHeaders =
    array[0..(2147483647 div SizeOf(TImageSectionHeader)) - 1] of
    TImageSectionHeader;

  PImageImportDescriptor = ^TImageImportDescriptor;
  TImageImportDescriptor = packed record
    OriginalFirstThunk : LongWord;
    TimeDateStamp      : LongWord;
    ForwarderChain     : LongWord;
    Name               : LongWord;
    FirstThunk         : LongWord;
  end;

  PImageBaseRelocation = ^TImageBaseRelocation;
  TImageBaseRelocation = packed record
    VirtualAddress : LongWord;
    SizeOfBlock    : LongWord;
  end;

  PSection = ^TSection;
  TSection = packed record
    Base            : Pointer;
    RVA             : LongWord;
    Size            : LongWord;
    Characteristics : LongWord;
  end;
  TSections = array of TSection;

  TDLLEntryProc = function(hinstDLL: HMODULE; dwReason: LongWord;
    lpvReserved: Pointer): Boolean; StdCall;

  TNameOrID = (niName, niID);

  TExternalLibrary = record
    LibraryName   : string;
    LibraryHandle : HINST;
  end;

  TExternalLibrarys = array of TExternalLibrary;

  PDLLFunctionImport = ^TDLLFunctionImport;

  TDLLFunctionImport = record
    NameOrID : TNameOrID;
    Name     : string;
    ID       : Integer;
  end;

  PDLLImport = ^TDLLImport;

  TDLLImport = record
    LibraryName   : string;
    LibraryHandle : HINST;
    Entries       : array of TDLLFunctionImport;
  end;

  TImports = array of TDLLImport;

  PDLLFunctionExport = ^TDLLFunctionExport;

  TDLLFunctionExport = record
    Name            : string;
    Index           : Integer;
    FunctionPointer : Pointer;
  end;

  TExports = array of TDLLFunctionExport;

  TExportTreeLink = Pointer;

  PExportTreeNode = ^TExportTreeNode;

  TExportTreeNode = record
    TheChar   : Char;
    Link      : TExportTreeLink;
    LinkExist : Boolean;
    Prevoius,
    Next,
    Up, Down  : PExportTreeNode;
  end;

  TExportTree = class
  private
    Root: PExportTreeNode;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Dump;
    function Add(FunctionName: string; Link: TExportTreeLink): Boolean;
    function Delete(FunctionName: string): Boolean;
    function Find(FunctionName: string; var Link: TExportTreeLink): Boolean;
  end;

  TDLLLoader = class
  private
    ImageBase            : Pointer;
    ImageBaseDelta       : Integer;
    DLLProc              : TDLLEntryProc;
    ExternalLibraryArray : TExternalLibrarys;
    ImportArray          : TImports;
    ExportArray          : TExports;
    Sections             : TSections;
    ExportTree           : TExportTree;
    function FindExternalLibrary(LibraryName: string): Integer;
    function LoadExternalLibrary(LibraryName: string): Integer;
    function GetExternalLibraryHandle(LibraryName: string): HInst;
  public
    constructor Create;
    destructor Destroy; override;
    function Load(Stream: TStream): Boolean;
    function Unload: Boolean;
    function FindExport(FunctionName: string): Pointer;
    function FindExportPerIndex(FunctionIndex: Integer): Pointer;
    function GetExportList: TStringList;
  end;

implementation

function CreateExportTreeNode(AChar: Char): PExportTreeNode;
begin
  GetMem(Result, SizeOf(TExportTreeNode));
  with Result^ do
   begin
    TheChar   := AChar;
    Link      := nil;
    LinkExist := False;
    Prevoius  := nil;
    Next      := nil;
    Up        := nil;
    Down      := nil;
   end;
end;

procedure DestroyExportTreeNode(Node: PExportTreeNode);
begin
  if assigned(Node) then
   begin
    DestroyExportTreeNode(Node^.Next);
    DestroyExportTreeNode(Node^.Down);
    FreeMem(Node);
   end;
end;

constructor TExportTree.Create;
begin
  inherited Create;
  Root := nil;
end;

destructor TExportTree.Destroy;
begin
  DestroyExportTreeNode(Root);
  inherited Destroy;
end;

procedure TExportTree.Dump;
var
  Ident: Integer;

  procedure DumpNode(Node: PExportTreeNode);
  var
    SubNode: PExportTreeNode;
    IdentCounter, IdentOld: Integer;
  begin
    for IdentCounter := 1 to Ident do Write(' ');
    Write(Node^.TheChar);
    IdentOld := Ident;
    SubNode := Node^.Next;
    while assigned(SubNode) do
     begin
      Write(SubNode.TheChar);
      if not assigned(SubNode^.Next) then break;
      Inc(Ident);
      SubNode := SubNode^.Next;
     end;
    writeLN;
    Inc(Ident);
    while assigned(SubNode) and (SubNode <> Node) do
     begin
      if assigned(SubNode^.Down) then DumpNode(SubNode^.Down);
      SubNode := SubNode^.Prevoius;
      Dec(Ident);
     end;
    Ident := IdentOld;
    if assigned(Node^.Down) then DumpNode(Node^.Down);
  end;

begin
  Ident := 0;
  DumpNode(Root);
end;

function TExportTree.Add(FunctionName: string; Link: TExportTreeLink): Boolean;
var
  stringLength, Position, PositionCounter: Integer;
  NewNode, LastNode, Node: PExportTreeNode;
  stringChar, NodeChar: Char;
begin
  Result := False;
  stringLength := Length(FunctionName);
  if stringLength > 0 then
   begin
    LastNode := nil;
    Node := Root;
    for Position := 1 to stringLength do
     begin
      stringChar := FunctionName[Position];
      if assigned(Node) then
       begin
        NodeChar := Node^.TheChar;
        if NodeChar = stringChar then
         begin
          LastNode := Node;
          Node := Node^.Next;
         end else
         begin
          while (NodeChar < stringChar) and assigned(Node^.Down) do
           begin
            Node := Node^.Down;
            NodeChar := Node^.TheChar;
           end;
          if NodeChar = stringChar then
           begin
            LastNode := Node;
            Node := Node^.Next;
           end else
           begin
            NewNode := CreateExportTreeNode(stringChar);
            if NodeChar < stringChar then
             begin
              NewNode^.Down := Node^.Down;
              NewNode^.Up := Node;
              if assigned(NewNode^.Down) then
                NewNode^.Down^.Up := NewNode;
              NewNode^.Prevoius := Node^.Prevoius;
              Node^.Down := NewNode;
             end else if NodeChar > stringChar then
             begin
              NewNode^.Down := Node;
              NewNode^.Up := Node^.Up;
              if assigned(NewNode^.Up) then
                NewNode^.Up^.Down := NewNode;
              NewNode^.Prevoius := Node^.Prevoius;
              if not assigned(NewNode^.Up) then
                if assigned(NewNode^.Prevoius)
                 then NewNode^.Prevoius^.Next := NewNode
                 else Root := NewNode;
              Node^.Up := NewNode;
             end;
            LastNode := NewNode;
            Node := LastNode^.Next;
           end;
         end;
       end else
       begin
        for PositionCounter := Position to stringLength do
         begin
          NewNode := CreateExportTreeNode(FunctionName[PositionCounter]);
          if assigned(LastNode) then
           begin
            NewNode^.Prevoius := LastNode;
            LastNode^.Next := NewNode;
            LastNode := LastNode^.Next;
           end else if not assigned(Root) then
           begin
            Root := NewNode;
            LastNode := Root;
           end;
         end;
        break;
       end;
     end;
    if assigned(LastNode) then
      if not LastNode^.LinkExist then
       begin
        LastNode^.Link := Link;
        LastNode^.LinkExist := True;
        Result := True;
       end;
   end;
end;

function TExportTree.Delete(FunctionName: string): Boolean;
var
  stringLength, Position : Integer;
  Node                   : PExportTreeNode;
  stringChar, NodeChar   : Char;
begin
  Result := False;
  stringLength := Length(FunctionName);
  if stringLength > 0 then
   begin
    Node := Root;
    for Position := 1 to stringLength do
     begin
      stringChar := FunctionName[Position];
      if assigned(Node) then
       begin
        NodeChar := Node^.TheChar;
        while (NodeChar <> stringChar) and assigned(Node^.Down) do
         begin
          Node := Node^.Down;
          NodeChar := Node^.TheChar;
         end;
        if NodeChar = stringChar then
         begin
          if (Position = stringLength) and Node^.LinkExist then
           begin
            Node^.LinkExist := False;
            Result := True;
            break;
           end;
          Node := Node^.Next;
         end;
       end else
        break;
     end;
   end;
end;

function TExportTree.Find(FunctionName: string;
  var Link: TExportTreeLink): Boolean;
var
  stringLength, Position : Integer;
  Node                   : PExportTreeNode;
  stringChar, NodeChar   : Char;
begin
  Result := False;
  stringLength := Length(FunctionName);
  if stringLength > 0 then
   begin
    Node := Root;
    for Position := 1 to stringLength do
     begin
      stringChar := FunctionName[Position];
      if assigned(Node) then
       begin
        NodeChar := Node^.TheChar;
        while (NodeChar <> stringChar) and assigned(Node^.Down) do
         begin
          Node := Node^.Down;
          NodeChar := Node^.TheChar;
         end;
        if NodeChar = stringChar then
         begin
          if (Position = stringLength) and Node^.LinkExist then
           begin
            Link := Node^.Link;
            Result := True;
            Break;
           end;
          Node := Node^.Next;
         end;
       end else
        break;
     end;
   end;
end;

constructor TDLLLoader.Create;
begin
  inherited Create;
  ImageBase            := nil;
  DLLProc              := nil;
  ExternalLibraryArray := nil;
  ImportArray          := nil;
  ExportArray          := nil;
  Sections             := nil;
  ExportTree           := nil;
end;

destructor TDLLLoader.Destroy;
begin
  if @DLLProc <> nil then Unload;
  if assigned(ExportTree) then ExportTree.Destroy;
  inherited Destroy;
end;

function TDLLLoader.FindExternalLibrary(LibraryName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Length(ExternalLibraryArray) - 1 do
    if ExternalLibraryArray[I].LibraryName = LibraryName then
     begin
      Result := I;
      exit;
     end;
end;

function TDLLLoader.LoadExternalLibrary(LibraryName: string): Integer;
begin
  Result := FindExternalLibrary(LibraryName);
  if Result < 0 then
   begin
    Result := Length(ExternalLibraryArray);
    SetLength(ExternalLibraryArray, Length(ExternalLibraryArray) + 1);
    ExternalLibraryArray[Result].LibraryName := LibraryName;
    ExternalLibraryArray[Result].LibraryHandle :=
      LoadLibrary(PChar(LibraryName));
   end;
end;

function TDLLLoader.GetExternalLibraryHandle(LibraryName: string): LongWord;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(ExternalLibraryArray) - 1 do
    if ExternalLibraryArray[I].LibraryName = LibraryName then
     begin
      Result := ExternalLibraryArray[I].LibraryHandle;
      exit;
     end;
end;

function TDLLLoader.Load(Stream: TStream): Boolean;
var
  ImageDOSHeader: TImageDOSHeader;
  ImageNTHeaders: TImageNTHeaders;
  OldProtect: LongWord;

  function ConvertPointer(RVA: LongWord): Pointer;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to Length(Sections) - 1 do
      if (RVA < (Sections[I].RVA + Sections[I].Size)) and
        (RVA >= Sections[I].RVA) then
       begin
        Result := Pointer(LongWord((RVA - LongWord(Sections[I].RVA)) +
          LongWord(Sections[I].Base)));
        exit;
       end;
  end;

  function ReadImageHeaders: Boolean;
  begin
    Result := False;
    if Stream.Size > 0 then
     begin
      FillChar(ImageNTHeaders, SizeOf(TImageNTHeaders), #0);
      if Stream.Read(ImageDOSHeader, SizeOf(TImageDOSHeader)) <> SizeOf(TImageDOSHeader) then exit;
      if ImageDOSHeader.Signature <> $5A4D then exit;
      if Stream.Seek(ImageDOSHeader.LFAOffset, soFrombeginning) <> LongInt(ImageDOSHeader.LFAOffset) then exit;
      if Stream.Read(ImageNTHeaders.Signature, SizeOf(LongWord)) <> SizeOf(LongWord) then exit;
      if ImageNTHeaders.Signature <> $00004550 then exit;
      if Stream.Read(ImageNTHeaders.FileHeader, SizeOf(TImageFileHeader)) <> SizeOf(TImageFileHeader) then exit;
      if ImageNTHeaders.FileHeader.Machine <> $14C then exit;
      if Stream.Read(ImageNTHeaders.OptionalHeader, ImageNTHeaders.FileHeader.SizeOfOptionalHeader) <> ImageNTHeaders.FileHeader.SizeOfOptionalHeader then exit;
      Result := True;
     end;
  end;

  function InitializeImage: Boolean;
  var
    SectionBase: Pointer;
    OldPosition: Integer;
  begin
    Result := False;
    with ImageNTHeaders do
      if FileHeader.NumberOfSections > 0 then
       begin
        ImageBase := VirtualAlloc(nil, OptionalHeader.SizeOfImage, MEM_RESERVE, PAGE_NOACCESS);
        ImageBaseDelta := LongWord(ImageBase) - OptionalHeader.ImageBase;
        SectionBase := VirtualAlloc(ImageBase, OptionalHeader.SizeOfHeaders, MEM_COMMIT, PAGE_READWRITE);
        OldPosition := Stream.Position;
        Stream.Seek(0, soFrombeginning);
        Stream.Read(SectionBase^, OptionalHeader.SizeOfHeaders);
        VirtualProtect(SectionBase, OptionalHeader.SizeOfHeaders, PAGE_READONLY, OldProtect);
        Stream.Seek(OldPosition, soFrombeginning);
        Result := True;
       end;
  end;

  function ReadSections: Boolean;
  var
    I              : Integer;
    Section        : TImageSectionHeader;
    SectionHeaders : PImageSectionHeaders;
  begin
    Result := False;
    if ImageNTHeaders.FileHeader.NumberOfSections > 0 then
     begin
      GetMem(SectionHeaders, ImageNTHeaders.FileHeader.NumberOfSections * SizeOf(TImageSectionHeader));
      if Stream.Read(SectionHeaders^, (ImageNTHeaders.FileHeader.NumberOfSections * SizeOf(TImageSectionHeader))) <> (ImageNTHeaders.FileHeader.NumberOfSections * SizeOf(TImageSectionHeader))
       then exit;
      SetLength(Sections, ImageNTHeaders.FileHeader.NumberOfSections);
      for I := 0 to ImageNTHeaders.FileHeader.NumberOfSections - 1 do
       begin
        Section := SectionHeaders^[I];
        Sections[I].RVA := Section.VirtualAddress;
        Sections[I].Size := Section.SizeOfRawData;
        if Sections[I].Size < Section.Misc.VirtualSize
         then Sections[I].Size := Section.Misc.VirtualSize;
        Sections[I].Characteristics := Section.Characteristics;
        Sections[I].Base := VirtualAlloc(Pointer(LongWord(Sections[I].RVA +
          LongWord(ImageBase))), Sections[I].Size, MEM_COMMIT, PAGE_READWRITE);
        FillChar(Sections[I].Base^, Sections[I].Size, #0);
        if Section.PointerToRawData <> 0 then
         begin
          Stream.Seek(Section.PointerToRawData, soFrombeginning);
          if Stream.Read(Sections[I].Base^, Section.SizeOfRawData) <> LONGINT(Section.SizeOfRawData) then exit;
         end;
       end;
      FreeMem(SectionHeaders);
      Result := True;
     end;
  end;

  function ProcessRelocations: Boolean;
  var
    Relocations: PChar;
    Position: LongWord;
    BaseRelocation: PImageBaseRelocation;
    Base: Pointer;
    NumberOfRelocations: LongWord;
    Relocation: PWordArray;
    RelocationCounter: LONGINT;
    RelocationPointer: Pointer;
    RelocationType: LongWord;
  begin
    if ImageNTHeaders.OptionalHeader.DataDirectory[
      IMAGE_DIRECTORY_ENTRY_BASERELOC].VirtualAddress <> 0 then
     begin
      Result := False;
      Relocations := ConvertPointer(
        ImageNTHeaders.OptionalHeader.DataDirectory[
        IMAGE_DIRECTORY_ENTRY_BASERELOC].VirtualAddress);
      Position := 0;
      while assigned(Relocations) and (Position <
          ImageNTHeaders.OptionalHeader.DataDirectory[
          IMAGE_DIRECTORY_ENTRY_BASERELOC].Size) do
       begin
        BaseRelocation := PImageBaseRelocation(Relocations);
        Base := ConvertPointer(BaseRelocation^.VirtualAddress);
        if not assigned(Base) then
          exit;
        NumberOfRelocations :=
          (BaseRelocation^.SizeOfBlock - SizeOf(TImageBaseRelocation)) div
          SizeOf(Word);
        Relocation := Pointer(LongWord(LongWord(BaseRelocation) +
          SizeOf(TImageBaseRelocation)));
        for RelocationCounter := 0 to NumberOfRelocations - 1 do
         begin
          RelocationPointer :=
            Pointer(LongWord(LongWord(Base) +
            (Relocation^[RelocationCounter] and $FFF)));
          RelocationType := Relocation^[RelocationCounter] shr 12;
          case RelocationType of
            IMAGE_REL_BASED_ABSOLUTE : ;
            IMAGE_REL_BASED_HIGH : PWord(RelocationPointer)^ :=
                (LongWord(
                ((LongWord(PWord(RelocationPointer)^ + LongWord(ImageBase) -
                ImageNTHeaders.OptionalHeader.ImageBase)))) shr 16) and $FFFF;
            IMAGE_REL_BASED_LOW : PWord(RelocationPointer)^ :=
                LongWord(((LongWord(PWord(RelocationPointer)^
                + LongWord(ImageBase) - ImageNTHeaders.
                OptionalHeader.ImageBase)))) and $FFFF;
            IMAGE_REL_BASED_HIGHLOW : PPointer(RelocationPointer)^ :=
                Pointer((LongWord(LongWord(PPointer(RelocationPointer)^) +
                LongWord(ImageBase) -
                ImageNTHeaders.OptionalHeader.ImageBase)));
            IMAGE_REL_BASED_HIGHADJ : ; // ???
            IMAGE_REL_BASED_MIPS_JMPADDR : ; // Only for MIPS CPUs ;)
           end;
         end;
        Relocations := Pointer(
          LongWord(LongWord(Relocations) + BaseRelocation^.SizeOfBlock));
        Inc(Position, BaseRelocation^.SizeOfBlock);
       end;
     end;
    Result := True;
  end;

  function ProcessImports: Boolean;
  var
    ImportDescriptor: PImageImportDescriptor;
    ThunkData: PLongWord;
    Name: PChar;
    DLLImport: PDLLImport;
    DLLFunctionImport: PDLLFunctionImport;
    FunctionPointer: Pointer;
  begin
    if ImageNTHeaders.OptionalHeader.DataDirectory[
      IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress <> 0 then
     begin
      ImportDescriptor := ConvertPointer(
        ImageNTHeaders.OptionalHeader.DataDirectory[
        IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress);
      if assigned(ImportDescriptor) then
       begin
        SetLength(ImportArray, 0);
        while ImportDescriptor^.Name <> 0 do
         begin
          Name := ConvertPointer(ImportDescriptor^.Name);
          SetLength(ImportArray, Length(ImportArray) + 1);
          LoadExternalLibrary(Name);
          DLLImport := @ImportArray[Length(ImportArray) - 1];
          DLLImport^.LibraryName := Name;
          DLLImport^.LibraryHandle := GetExternalLibraryHandle(Name);
          DLLImport^.Entries := nil;
          if ImportDescriptor^.TimeDateStamp = 0 then
            ThunkData := ConvertPointer(ImportDescriptor^.FirstThunk)
          else
            ThunkData := ConvertPointer(ImportDescriptor^.OriginalFirstThunk);

          while ThunkData^ <> 0 do
           begin
            SetLength(DLLImport^.Entries, Length(DLLImport^.Entries) + 1);
            DLLFunctionImport :=
              @DLLImport^.Entries[Length(DLLImport^.Entries) - 1];
            if (ThunkData^ and IMAGE_ORDINAL_FLAG32) <> 0 then
             begin
              DLLFunctionImport^.NameOrID := niID;
              DLLFunctionImport^.ID := ThunkData^ and IMAGE_ORDINAL_MASK32;
              DLLFunctionImport^.Name := '';
              FunctionPointer :=
                GetProcAddress(DLLImport^.LibraryHandle,
                PChar(ThunkData^ and IMAGE_ORDINAL_MASK32));
             end
            else
             begin
              Name := ConvertPointer(LongWord(ThunkData^) +
                IMPORTED_NAME_OFFSET);
              DLLFunctionImport^.NameOrID := niName;
              DLLFunctionImport^.ID := 0;
              DLLFunctionImport^.Name := Name;
              FunctionPointer :=
                GetProcAddress(DLLImport^.LibraryHandle, Name);
             end;
            PPointer(Thunkdata)^ := FunctionPointer;
            Inc(ThunkData);
           end;
          Inc(ImportDescriptor);
         end;
       end;
     end;
    Result := True;
  end;

  function ProtectSections: Boolean;
  var
    I               : Integer;
    Characteristics : LongWord;
    Flags           : LongWord;
  begin
    Result := False;
    if ImageNTHeaders.FileHeader.NumberOfSections > 0 then
     begin
      for I := 0 to ImageNTHeaders.FileHeader.NumberOfSections - 1 do
       begin
        Characteristics := Sections[I].Characteristics;
        Flags := 0;
        if (Characteristics and IMAGE_SCN_MEM_EXECUTE) <> 0 then
         begin
          if (Characteristics and IMAGE_SCN_MEM_READ) <> 0 then
           begin
            if (Characteristics and IMAGE_SCN_MEM_write) <> 0
             then Flags := Flags or PAGE_EXECUTE_READwrite
             else Flags := Flags or PAGE_EXECUTE_READ;
           end else if (Characteristics and IMAGE_SCN_MEM_write) <> 0
            then Flags := Flags or PAGE_EXECUTE_writeCOPY
            else Flags := Flags or PAGE_EXECUTE;
         end else if (Characteristics and IMAGE_SCN_MEM_READ) <> 0 then
         begin
          if (Characteristics and IMAGE_SCN_MEM_write) <> 0
           then Flags := Flags or PAGE_READWRITE
           else Flags := Flags or PAGE_READONLY;
         end else
        if (Characteristics and IMAGE_SCN_MEM_write) <> 0
         then Flags := Flags or PAGE_WRITECOPY
         else Flags := Flags or PAGE_NOACCESS;
        if (Characteristics and IMAGE_SCN_MEM_not_CACHED) <> 0
         then Flags := Flags or PAGE_NOCACHE;
        VirtualProtect(Sections[I].Base, Sections[I].Size, Flags, OldProtect);
       end;
      Result := True;
     end;
  end;

  function InitializeLibrary: Boolean;
  begin
    Result := False;
    @DLLProc := ConvertPointer(ImageNTHeaders.OptionalHeader.AddressOfEntryPoint);
    if DLLProc(CARDINAL(ImageBase), DLL_PROCESS_ATTACH, nil)
     then Result := True;
  end;

  function ProcessExports: Boolean;
  var
    I                      : Integer;
    ExportDirectory        : PImageExportDirectory;
    ExportDirectorySize    : LongWord;
    FunctionNamePointer    : Pointer;
    FunctionName           : PChar;
    FunctionIndexPointer   : Pointer;
    FunctionIndex          : LongWord;
    FunctionPointer        : Pointer;
    ForwarderCharPointer   : PChar;
    Forwarderstring        : string;
    ForwarderLibrary       : string;
    ForwarderLibraryHandle : HINST;

    function ParsestringToNumber(Astring: string): LongWord;
    var
      CharCounter: Integer;
    begin
      Result := 0;
      for CharCounter := 0 to Length(Astring) - 1 do
        if Astring[CharCounter] in ['0'..'9'] then
          Result := (Result * 10) +
            Byte(Byte(Astring[CharCounter]) - Byte('0'))
        else
          exit;
    end;

  begin
    if ImageNTHeaders.OptionalHeader.DataDirectory[
      IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress <> 0 then
     begin
      ExportTree := TExportTree.Create;
      ExportDirectory := ConvertPointer(
        ImageNTHeaders.OptionalHeader.DataDirectory[
        IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress);
      if assigned(ExportDirectory) then
       begin
        ExportDirectorySize :=
          ImageNTHeaders.OptionalHeader.DataDirectory[
          IMAGE_DIRECTORY_ENTRY_EXPORT].Size;
        SetLength(ExportArray, ExportDirectory^.NumberOfNames);
        for I := 0 to ExportDirectory^.NumberOfNames - 1 do
         begin
          FunctionNamePointer := ConvertPointer(LongWord(ExportDirectory^.AddressOfNames));
          FunctionNamePointer := ConvertPointer(PLongWordArray(FunctionNamePointer)^[I]);
          FunctionName := FunctionNamePointer;
          FunctionIndexPointer := ConvertPointer(LongWord(ExportDirectory^.AddressOfNameOrdinals));
          FunctionIndex := PWordArray(FunctionIndexPointer)^[I];
          FunctionPointer := ConvertPointer(LongWord(ExportDirectory^.AddressOfFunctions));
          FunctionPointer := ConvertPointer(PLongWordArray(FunctionPointer)^[FunctionIndex]);
          ExportArray[I].Name := FunctionName;
          ExportArray[I].Index := FunctionIndex;
          if (LongWord(ExportDirectory) < LongWord(FunctionPointer)) and
            (LongWord(FunctionPointer) <
            (LongWord(ExportDirectory) + ExportDirectorySize)) then
           begin
            ForwarderCharPointer := FunctionPointer;
            Forwarderstring := ForwarderCharPointer;
            while ForwarderCharPointer^ <> '.' do Inc(ForwarderCharPointer);
            ForwarderLibrary := COPY(Forwarderstring, 1, POS('.', Forwarderstring) - 1);
            LoadExternalLibrary(ForwarderLibrary);
            ForwarderLibraryHandle := GetExternalLibraryHandle(ForwarderLibrary);
            if ForwarderCharPointer^ = '#' then
             begin
              Inc(ForwarderCharPointer);
              Forwarderstring := ForwarderCharPointer;
              ForwarderCharPointer := ConvertPointer(ParsestringToNumber(Forwarderstring));
              Forwarderstring := ForwarderCharPointer;
             end
            else
             begin
              Forwarderstring := ForwarderCharPointer;
              ExportArray[I].FunctionPointer := GetProcAddress(ForwarderLibraryHandle, PChar(Forwarderstring));
             end;
           end else ExportArray[I].FunctionPointer := FunctionPointer;
          ExportTree.Add(ExportArray[I].Name, ExportArray[I].FunctionPointer);
         end
       end;
     end;
    Result := True;
  end;

begin
  Result := False;
  if assigned(Stream) then
   begin
    Stream.Seek(0, soFromBeginning);
    if Stream.Size > 0 then
      if ReadImageHeaders then
        if InitializeImage then
          if ReadSections then
            if ProcessRelocations then
              if ProcessImports then
                if ProtectSections then
                  if InitializeLibrary then
                    if ProcessExports then
                      Result := True;
   end;
end;



function TDLLLoader.Unload: Boolean;
var
  I, J: Integer;
begin
  Result := False;
  if @DLLProc <> nil then
    DLLProc(LongWord(ImageBase), DLL_PROCESS_DETACH, nil);
  for I := 0 to Length(Sections) - 1 do
    if assigned(Sections[I].Base) then
      VirtualFree(Sections[I].Base, 0, MEM_RELEASE);

  SetLength(Sections, 0);
  for I := 0 to Length(ExternalLibraryArray) - 1 do
   begin
    ExternalLibraryArray[I].LibraryName := '';
    FreeLibrary(ExternalLibraryArray[I].LibraryHandle);
   end;

  SetLength(ExternalLibraryArray, 0);
  for I := 0 to Length(ImportArray) - 1 do
   begin
    for J := 0 to Length(ImportArray[I].Entries) - 1 do
      ImportArray[I].Entries[J].Name := '';
    SetLength(ImportArray[I].Entries, 0);
   end;

  SetLength(ImportArray, 0);
  for I := 0 to Length(ExportArray) - 1 do
    ExportArray[I].Name := '';

  SetLength(ExportArray, 0);
  VirtualFree(ImageBase, 0, MEM_RELEASE);
  if assigned(ExportTree) then
   begin
    ExportTree.Destroy;
    ExportTree := nil;
   end;
end;

function TDLLLoader.FindExport(FunctionName: string): Pointer;
var
  I: Integer;
begin
  Result := nil;
  if assigned(ExportTree) then
    ExportTree.Find(FunctionName, Result)
  else
    for I := 0 to Length(ExportArray) - 1 do
      if ExportArray[I].Name = FunctionName then
       begin
        Result := ExportArray[I].FunctionPointer;
        exit;
       end;
end;

function TDLLLoader.FindExportPerIndex(FunctionIndex: Integer): Pointer;
var
  I: Integer;
begin
  Result := nil;
  for i := 0 to Length(ExportArray) - 1 do
    if ExportArray[i].Index = FunctionIndex then
     begin
      Result := ExportArray[i].FunctionPointer;
      exit;
     end;
end;

function TDLLLoader.GetExportList: TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := 0 to Length(ExportArray) - 1 do
    Result.Add(ExportArray[I].Name);
  Result.Sort;
end;

end.
