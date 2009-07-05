unit SEwrapper;

interface

uses
  Windows, Classes, DAV_SECommon, DAV_SEModule, DAV_DLLLoader;

type
  TSEGetModuleProperties = function(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl;
  TSEMakeModule = function(Index, ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl;

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;

implementation

uses
  SysUtils;

var
  ContainedModules : TStringList;
  DLLLoader        : array of TDLLLoader;
  GMP              : array of TSEGetModuleProperties;
  MM               : array of TSEMakeModule;
  PlugCounts       : array of Integer;


function EnumNamesFunc(hModule: THandle; lpType, lpName: PChar; lParam: DWORD): Boolean; stdcall;
begin
 Result := True;
 TStringList(lParam).Add(lpName);
end;

procedure EnumerateModules;
var
  i  : Integer;
  RS : TResourceStream;
  MP : TSEModuleProperties;
begin
 ContainedModules := TStringList.Create;
 try
  EnumResourceNames(HInstance, 'SEM', @EnumNamesFunc, DWord(ContainedModules));
  SetLength(DLLLoader, ContainedModules.Count);
  SetLength(GMP, ContainedModules.Count);
  SetLength(MM, ContainedModules.Count);
  SetLength(PlugCounts, ContainedModules.Count);
  for i := 0 to ContainedModules.Count - 1 do
   begin
    RS := TResourceStream.Create(HInstance, ContainedModules[i], 'SEM');
    try
     DLLLoader[i] := TDLLLoader.Create;
     DLLLoader[i].Load(RS);
     GMP[i] := DLLLoader[i].FindExport('getModuleProperties');
     MM[i] := DLLLoader[i].FindExport('makeModule');

     // scan for module properties contained in the current SEM
     PlugCounts[i] := 0;
     while GMP[i](PlugCounts[i], @MP) do inc(PlugCounts[i]);

    finally
     FreeAndNil(RS);
    end;
   end;
 finally
 end;
end;

procedure FreeDLLLoader;
var
  i : Integer; 
begin
 for i := 0 to Length(DLLLoader) do
  if assigned(DLLLoader) then
   try
    DLLLoader[i].Unload;
   finally
    FreeAndNil(DLLLoader[i]);
   end;
 SetLength(DLLLoader, 0);
 SetLength(GMP, 0);
 SetLength(MM, 0);
end;

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
var
  i, j : Integer;
  ID   : string;
begin
 result := False;
 if Length(DLLLoader) > 0 then
  try
   i := 0;
   j := Index;
   repeat
    if j < PlugCounts[i] then break;
    dec(j, PlugCounts[i]);
    inc(i);
   until (i >= Length(PlugCounts)) or (j < 0);
   if (i < Length(PlugCounts)) and (j >= 0) and assigned(GMP[i])
    then result := GMP[i](j, Properties);
   if result then
    begin
     ID := StrPas(Properties^.ID);
     ID := ID + ' (merged) ' + IntToStr(ContainedModules.Count);
     Properties^.ID := PChar(ID);
    end;
  except
   result := False;
  end;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  i, j : Integer;
begin
 result := nil;
 if Length(DLLLoader) > 0 then
  try
   i := 0;
   j := Index;
   repeat
    if j < PlugCounts[i] then break;
    dec(j, PlugCounts[i]);
    inc(i);
   until (i >= Length(PlugCounts)) or (j < 0);

   if (i < Length(PlugCounts)) and (j >= 0) and assigned(MM[i])
    then result := MM[i](j, ProcessType, SEAudioMaster, Reserved);
  except
   result := nil;
  end;

  if assigned(result) then
   with PSE2ModStructBase(result)^ do
    begin
     Magic := $29A2A826;
     Magic := 2 * Magic;
    end;
end;

initialization
  EnumerateModules;

finalization
//  FreeDLLLoader;

end.
