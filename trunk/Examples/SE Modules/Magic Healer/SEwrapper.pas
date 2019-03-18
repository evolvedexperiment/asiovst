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

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
function makeModule(Index: Integer; ProcessType: Integer;
  SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer;
  cdecl; export;

implementation

uses
  SysUtils;

var
  ContainedModules: TStringList;
  DLLLoader: array of TDLLLoader;

function EnumNamesFunc(hModule: THandle; lpType, lpName: PChar; lParam: DWORD): Boolean; stdcall;
begin
  Result := True;
  TStringList(lParam).Add(lpName);
end;

procedure EnumerateModules;
var
  i: Integer;
  RS: TResourceStream;
begin
  ContainedModules := TStringList.Create;
  try
    EnumResourceNames(HInstance, 'SEM', @EnumNamesFunc,
      DWORD(ContainedModules));
    SetLength(DLLLoader, ContainedModules.Count);
    for i := 0 to ContainedModules.Count - 1 do
    begin
      RS := TResourceStream.Create(HInstance, ContainedModules[i], 'SEM');
      try
        DLLLoader[0] := TDLLLoader.Create;
        DLLLoader[0].Load(RS)
      finally
        FreeAndNil(RS);
      end;
    end;
  finally
  end;
end;

function getModuleProperties(Index: Integer;
  Properties: PSEModuleProperties): Boolean; cdecl; export;
var
  GMP: TSEGetModuleProperties;
begin
  if Length(DLLLoader) = 0 then
    Result := False
  else
  begin
    GMP := DLLLoader[0].FindExport('getModuleProperties');
    Result := GMP(Index, Properties);
  end;
end;

function makeModule(Index: Integer; ProcessType: Integer;
  SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer;
  cdecl; export;
var
  MM: TSEMakeModule;
begin
  if Length(DLLLoader) = 0 then
    Result := nil
  else
  begin
    MM := DLLLoader[0].FindExport('makeModule');
    Result := MM(Index, ProcessType, SEAudioMaster, Reserved);
    if Assigned(Result) then
      with PSE2ModStructBase(Result)^ do
      begin
        Magic := $29A2A826;
        Magic := 2 * Magic;

        // comment the following line to let the wrapper run inside the VST as well!
        // assert(Magic = SepMagic);
      end;
  end;
end;

initialization

EnumerateModules;

finalization

end.
