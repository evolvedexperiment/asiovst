{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2019          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{   SynthEdit is witten by Jef McClintock (see http://www.synthedit.com/       }
{                                                                              }
{******************************************************************************}

unit SEwrapper;

interface

uses
  Windows, Classes, DAV_SECommon, DAV_SEModule, DAV_DLLLoader;

type
  TSEGetModuleProperties = function(Index: Integer;
    Properties: PSEModuleProperties): Boolean; cdecl;
  TSEMakeModule = function(Index, ProcessType: Integer;
    SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl;

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties)
  : Boolean; cdecl; export;
function makeModule(Index: Integer; ProcessType: Integer;
  SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer;
  cdecl; export;

implementation

uses
  SysUtils;

var
  ContainedModules: TStringList;
  DLLLoader: array of TDLLLoader;
  GMP: array of TSEGetModuleProperties;
  MM: array of TSEMakeModule;
  PlugCounts: array of Integer;

function EnumNamesFunc(hModule: THandle; lpType, lpName: PChar; lParam: DWORD)
  : Boolean; stdcall;
begin
  Result := True;
  TStringList(lParam).Add(lpName);
end;

procedure EnumerateModules;
var
  i: Integer;
  RS: TResourceStream;
  MP: TSEModuleProperties;
begin
  ContainedModules := TStringList.Create;
  try
    EnumResourceNames(HInstance, 'SEM', @EnumNamesFunc,
      DWORD(ContainedModules));
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
        while GMP[i](PlugCounts[i], @MP) do
          Inc(PlugCounts[i]);

      finally
        FreeAndNil(RS);
      end;
    end;
  finally
  end;
end;

procedure FreeDLLLoader;
var
  i: Integer;
begin
  for i := 0 to Length(DLLLoader) do
    if Assigned(DLLLoader) then
      try
        DLLLoader[i].Unload;
      finally
        FreeAndNil(DLLLoader[i]);
      end;
  SetLength(DLLLoader, 0);
  SetLength(GMP, 0);
  SetLength(MM, 0);
end;

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties)
  : Boolean; cdecl; export;
var
  i, j: Integer;
  ID: AnsiString;
begin
  Result := False;
  if Length(DLLLoader) > 0 then
    try
      i := 0;
      j := Index;
      repeat
        if j < PlugCounts[i] then
          break;
        Dec(j, PlugCounts[i]);
        Inc(i);
      until (i >= Length(PlugCounts)) or (j < 0);
      if (i < Length(PlugCounts)) and (j >= 0) and Assigned(GMP[i]) then
        Result := GMP[i](j, Properties);
      if Result then
      begin
        ID := StrPas(Properties^.ID);
        if Pos('merged', ContainedModules[Index]) > 0 then
          ID := ID + ' (merged) ' + IntToStr(ContainedModules.Count);
        Properties^.ID := PAnsiChar(ID);
      end;
    except
      Result := False;
    end;
end;

function makeModule(Index: Integer; ProcessType: Integer;
  SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer;
  cdecl; export;
var
  i, j: Integer;
begin
  Result := nil;
  if Length(DLLLoader) > 0 then
    try
      i := 0;
      j := Index;
      repeat
        if j < PlugCounts[i] then
          break;
        Dec(j, PlugCounts[i]);
        Inc(i);
      until (i >= Length(PlugCounts)) or (j < 0);

      if (i < Length(PlugCounts)) and (j >= 0) and Assigned(MM[i]) then
        Result := MM[i](j, ProcessType, SEAudioMaster, Reserved);
    except
      Result := nil;
    end;

  if Assigned(Result) then
    with PSE2ModStructBase(Result)^ do
    begin
      Magic := $29A2A826;
      Magic := 2 * Magic;
    end;
end;

initialization

EnumerateModules;

finalization

// FreeDLLLoader;

end.
