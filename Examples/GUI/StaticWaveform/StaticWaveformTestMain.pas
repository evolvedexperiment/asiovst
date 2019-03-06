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

unit StaticWaveformTestMain;

{$I DAV_Compiler.inc}

interface

uses
{$IFDEF FPC} LCLIntf, LMessages, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  DAV_Common, DAV_Types, DAV_GuiBaseControl, DAV_GuiStaticWaveform;

type
  TFmStaticWaveformTest = class(TForm)
    StaticWaveformA: TGuiStaticWaveform;
    StaticWaveformB: TGuiStaticWaveform;
    StaticWaveformC: TGuiStaticWaveform;
    StaticWaveformD: TGuiStaticWaveform;
    CbTransparent: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure CbTransparentClick(Sender: TObject);
  end;

var
  FmStaticWaveformTest: TFmStaticWaveformTest;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmStaticWaveformTest.FormCreate(Sender: TObject);
var
  WaveData: TDAVSingleDynArray;
  Sample: Integer;
begin
  SetLength(WaveData, 256);
  for Sample := 0 to Length(WaveData) - 1 do
    WaveData[Sample] := 2 * random - 1;
  StaticWaveformA.SetWaveForm(WaveData);
  StaticWaveformB.SetWaveForm(WaveData);
  StaticWaveformC.SetWaveForm(WaveData);
  StaticWaveformD.SetWaveForm(WaveData);
end;

procedure TFmStaticWaveformTest.CbTransparentClick(Sender: TObject);
begin
  StaticWaveformA.Transparent := CbTransparent.Checked;
  StaticWaveformB.Transparent := CbTransparent.Checked;
  StaticWaveformC.Transparent := CbTransparent.Checked;
  StaticWaveformD.Transparent := CbTransparent.Checked;
end;

end.
