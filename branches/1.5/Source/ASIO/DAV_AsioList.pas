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
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2012          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_AsioList;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, SysUtils, Registry, Windows;

type
  TDavAsioDriverDescription = class
  private
    FGuid: TGUID;
    FName: string;
    FFilename: string;
  public
    constructor Create(nGuid: TGUID; nName: string; nFilename: string);
      overload;
    constructor Create(nGuid, nName, nFilename: string); overload;
    property Guid: TGUID read FGuid;
    property Name: string read FName;
    property Filename: string read FFilename;
  end;

  TDavAsioDriverList = class
  private
    FNameList: TStrings;
    FIgnoreGuid: TGUID;
    FHasIgnoreGuid: boolean;
    FList: TList;
    procedure ClearList;
    procedure LoadList;
    function GetDriverFileName(DrvGuidStr: string): string;
    function GetItem(Index: Integer): TDavAsioDriverDescription;
    function GetCount: Integer;
  public
    constructor Create(Ignore: TGUID); reintroduce; overload;
    constructor Create; overload;
    destructor Destroy; override;

    procedure SetIgnoredDriver(Ignore: TGUID);
    procedure UpdateList;
    function DriverNumberByName(DriverName: string): Integer;
    property Items[Index: Integer]: TDavAsioDriverDescription read GetItem;
    property Count: Integer read GetCount;
    property DriverNames: TStrings read FNameList;
  end;

implementation

const
  CAsioInprocServer = 'InprocServer32';
  CAsioPath = 'software\asio';
  CAsioComClsId = 'clsid';
  CAsioDescription = 'description';

  { TDAVIntAsioDriverDesc }

constructor TDavAsioDriverDescription.Create(nGuid: TGUID; nName: string;
  nFilename: string);
begin
  FGuid := nGuid;
  FName := nName;
  FFilename := nFilename;
end;

constructor TDavAsioDriverDescription.Create(nGuid, nName, nFilename: string);
begin
  FGuid := StringToGUID(nGuid);
  FName := nName;
  FFilename := nFilename;
end;

{ TDAVIntAsioDriverList }

constructor TDavAsioDriverList.Create;
begin
  inherited;
  FHasIgnoreGuid := False;
  FNameList := TStringList.Create;
  FList := TList.Create;
end;

constructor TDavAsioDriverList.Create(Ignore: TGUID);
begin
  Create;
  SetIgnoredDriver(Ignore);
end;

destructor TDavAsioDriverList.Destroy;
begin
  ClearList;
  FreeAndNil(FList);
  FreeAndNil(FNameList);
  inherited;
end;

procedure TDavAsioDriverList.SetIgnoredDriver(Ignore: TGUID);
begin
  if (FIgnoreGuid.D1 <> Ignore.D1) and (FIgnoreGuid.D2 <> Ignore.D2) and
    (FIgnoreGuid.D3 <> Ignore.D3) then
  begin
    FIgnoreGuid := Ignore;
    FHasIgnoreGuid := True;
  end;
end;

procedure TDavAsioDriverList.ClearList;
var
  DriverIndex: Integer;
begin
  for DriverIndex := Count - 1 downto 0 do
    Items[DriverIndex].Free;

  FNameList.Clear;
  FList.Clear;
end;

function TDavAsioDriverList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TDavAsioDriverList.GetDriverFileName(DrvGuidStr: string): string;
var
  Filename: string;
  DirStr: PChar;
begin
  Result := '';
  if DrvGuidStr = '' then
    exit;

  with TRegistry.Create do
    try
      RootKey := HKEY_CLASSES_ROOT;
      if OpenKeyReadOnly(CAsioComClsId + '\' + Lowercase(DrvGuidStr) + '\' +
        CAsioInprocServer) then
      begin
        Result := ReadString('');
        Filename := ExtractFileName(Result);
        DirStr := StrAlloc(MAX_PATH);

        if not FileExists(Result) and (GetSystemDirectory(DirStr, MAX_PATH) <> 0)
        then
          Result := StrPas(DirStr) + '\' + Filename;

        if not FileExists(Result) and
          (GetWindowsDirectory(DirStr, MAX_PATH) <> 0) then
          Result := StrPas(DirStr) + '\' + Filename;

        if not FileExists(Result) then
          Result := '';

        StrDispose(DirStr);
        CloseKey;
      end;

    finally
      Free;
    end;
end;

function TDavAsioDriverList.GetItem(Index: Integer): TDavAsioDriverDescription;
begin
  Result := TDavAsioDriverDescription(FList.Items[Index]);
end;

procedure TDavAsioDriverList.LoadList;
var
  SubKeys: TStringList;
  KeyNo: Integer;
  DrvName: string;
  DrvGuidStr: string;
  DrvFile: string;
  DrvGuid: TGUID;
  DriverItem: TDavAsioDriverDescription;
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

      for KeyNo := 0 to SubKeys.Count - 1 do
        if OpenKeyReadOnly(CAsioPath + '\' + SubKeys[KeyNo]) then
          try
            DrvGuidStr := ReadString(CAsioComClsId);
            if DrvGuidStr <> '' then
            begin
              DrvGuid := StringToGUID(DrvGuidStr);

              DrvFile := GetDriverFileName(DrvGuidStr);
              if (DrvFile <> '') and
                not(FHasIgnoreGuid and IsEqualGUID(DrvGuid, FIgnoreGuid)) then
              begin
                DrvName := ReadString(CAsioDescription);
                if DrvName = '' then
                  DrvName := SubKeys[KeyNo];

                DriverItem := TDavAsioDriverDescription.Create(DrvGuidStr,
                  DrvName, DrvFile);

                FList.Add(DriverItem);
                FNameList.Add(DrvName);
              end;
            end
{$IFNDEF IgnoreBrokenAsioList}
            else
              raise Exception.Create('Error loading GUID from ' +
                SubKeys[KeyNo])
{$ENDIF};
          finally
            CloseKey;
          end;
    finally
      Free;
      FreeAndNil(SubKeys);
    end;
end;

procedure TDavAsioDriverList.UpdateList;
begin
  ClearList;
  LoadList;
end;

function TDavAsioDriverList.DriverNumberByName(DriverName: string): Integer;
var
  DriverIndex: Integer;
begin
  DriverName := Lowercase(DriverName);
  Result := -1;
  for DriverIndex := 0 to Count - 1 do
    if Lowercase(Items[DriverIndex].Name) = DriverName then
    begin
      Result := DriverIndex;
      Break;
    end;
end;

end.
