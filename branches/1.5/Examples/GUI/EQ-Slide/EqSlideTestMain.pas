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
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2013          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit EqSlideTestMain;

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC} LCLIntf, LMessages, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, DAV_DspFilter,
  DAV_DspFilterBasics, DAV_GuiCustomControl, DAV_GuiEQSlide;

type
  TFmEqSlideTest = class(TForm)
    GuiEQSlide1: TGuiEQSlide;
    GuiEQSlide2: TGuiEQSlide;
    GuiEQSlide3: TGuiEQSlide;
    GuiEQSlide4: TGuiEQSlide;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    function GuiEQSlideGetColor(Sender: TObject;
      const Frequency: Single): TColor;
    function GuiEQSlideGetColorBW(Sender: TObject;
      const Frequency: Single): TColor;
  private
    FLowpass: TBasicLowpassFilter;
  end;

var
  FmEqSlideTest: TFmEqSlideTest;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

uses
  DAV_Common, DAV_GuiCommon;

procedure TFmEqSlideTest.FormCreate(Sender: TObject);
begin
  FLowpass := TBasicLowpassFilter.Create;
end;

procedure TFmEqSlideTest.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FLowpass);
end;

function TFmEqSlideTest.GuiEQSlideGetColor(Sender: TObject;
  const Frequency: Single): TColor;
var
  Gain: Single;
begin
  Gain := FLowpass.MagnitudeSquared(Frequency);
  Result := HLSToRGB(0.5 - 0.25 * Gain, 0.3 + 0.1 * Gain, 0.3 + 0.3 * Gain);
end;

function TFmEqSlideTest.GuiEQSlideGetColorBW(Sender: TObject;
  const Frequency: Single): TColor;
var
  Gain: Single;
  Col: Byte;
begin
  Gain := FLowpass.MagnitudeSquared(Frequency);
  Col := Round(Limit(250 * Gain, 0, 255));
  Result := Col shl 16 + Col shl 8 + Col;
end;

end.
