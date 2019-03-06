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
{******************************************************************************}

unit DAV_VSTChannels;

interface

{$I ..\DAV_Compiler.inc}

uses
{$IFDEF FPC}LCLIntf, {$ELSE}Windows, {$ENDIF} Classes, SysUtils, Forms,
  DAV_VSTEffect, DAV_VSTBasicModule;

type
  TCustomVstChannel = class(TCollectionItem)
  private
    FLabel: string;
    FShortLabel: string;
    FSpeakerArrangement: TVstSpeakerArrangementType;
    FFlags: TVstPinPropertiesFlags;
    FVSTModule: TBasicVSTModule;
    procedure SetShortLabel(const Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;

    procedure SetDisplayName(const AValue: string); override;
    function GetDisplayName: string; override;
  public
{$IFDEF FPC}
    constructor Create(ACollection: TCollection); override;
{$ELSE}
    constructor Create(Collection: TCollection); override;
{$ENDIF}
    destructor Destroy; override;
  published
    property DisplayName{$IFNDEF FPC}: string read GetDisplayName
      write SetDisplayName{$ENDIF};
    property ShortLabel: string read FShortLabel write SetShortLabel;
    property SpeakerArrangement: TVstSpeakerArrangementType
      read FSpeakerArrangement write FSpeakerArrangement;
    property Flags: TVstPinPropertiesFlags read FFlags write FFlags;
    property VSTModule: TBasicVSTModule read FVSTModule write FVSTModule;
  end;

  TCustomVstChannels = class(TOwnedCollection)
  private
    FVSTModule: TBasicVSTModule;
  protected
    function GetItem(Index: Integer): TCustomVstChannel; virtual;
    procedure SetItem(Index: Integer; const Value: TCustomVstChannel); virtual;
    property Items[Index: Integer]: TCustomVstChannel read GetItem
      write SetItem; default;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function Add: TCustomVstChannel;
    function Insert(Index: Integer): TCustomVstChannel;
    procedure Delete(Index: Integer);
    property Count;
    property VSTModule: TBasicVSTModule read FVSTModule write FVSTModule;
  end;

implementation

{ TCustomVstChannel }

{$IFDEF FPC}

constructor TCustomVstChannel.Create(ACollection: TCollection);
begin
  inherited;
  FVSTModule := (ACollection as TCustomVstChannels).VSTModule;
end;
{$ELSE}

constructor TCustomVstChannel.Create(Collection: TCollection);
begin
  inherited;
  FVSTModule := (Collection as TCustomVstChannels).VSTModule;
end;
{$ENDIF}

destructor TCustomVstChannel.Destroy;
begin
  // nothing in here yet!
  inherited;
end;

procedure TCustomVstChannel.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomVstChannel then
    with TCustomVstChannel(Dest) do
      try
        DisplayName := Self.DisplayName;
        FLabel := Self.FLabel;
        FShortLabel := Self.FShortLabel;
        FFlags := Self.FFlags;
      except
        inherited;
      end
  else
    inherited;
end;

function TCustomVstChannel.GetDisplayName: string;
begin
  Result := FLabel;
end;

procedure TCustomVstChannel.SetDisplayName(const AValue: string);
begin
  if FLabel <> AValue then
  begin
    FLabel := AValue;
    inherited;
  end;
end;

procedure TCustomVstChannel.SetShortLabel(const Value: string);
begin
  if FShortLabel <> Value then
  begin
    FShortLabel := Copy(Value, 0, 7);
  end;
end;

{ TCustomVstChannels }

constructor TCustomVstChannels.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TCustomVstChannel);
  FVSTModule := TBasicVSTModule(AOwner);
end;

function TCustomVstChannels.Add: TCustomVstChannel;
begin
  Result := TCustomVstChannel(inherited Add);
end;

procedure TCustomVstChannels.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

destructor TCustomVstChannels.Destroy;
begin
  while Count > 0 do
    Delete(0);
  inherited;
end;

function TCustomVstChannels.GetItem(Index: Integer): TCustomVstChannel;
begin
  Result := TCustomVstChannel(inherited GetItem(Index));
end;

function TCustomVstChannels.Insert(Index: Integer): TCustomVstChannel;
begin
  Result := TCustomVstChannel(inherited Insert(Index));
end;

procedure TCustomVstChannels.SetItem(Index: Integer;
  const Value: TCustomVstChannel);
begin
  inherited SetItem(Index, Value);
end;

end.
