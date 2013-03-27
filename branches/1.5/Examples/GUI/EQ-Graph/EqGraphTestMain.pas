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

unit EqGraphTestMain;

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC} LCLIntf, LMessages, {$ELSE} Windows, Messages, {$ENDIF}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DAV_GuiEQGraph, DAV_DspFilter, DAV_DspFilterBasics, DAV_GuiCustomControl;

type
  TFmEqGraphTest = class(TForm)
    EqGraphA: TGuiEQGraph;
    EqGraphB: TGuiEQGraph;
    EqGraphC: TGuiEQGraph;
    EqGraphD: TGuiEQGraph;
    function GetFilterGain(Sender: TObject; const Frequency: Single): Single;
    procedure FormCreate(Sender: TObject);
    function GetFilterSubGain(Sender: TObject; const Frequency: Single): Single;
    procedure FormDestroy(Sender: TObject);
  private
    FLowpass: TBasicLowpassFilter;
  end;

var
  FmEqGraphTest: TFmEqGraphTest;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmEqGraphTest.FormCreate(Sender: TObject);
begin
  FLowpass := TBasicLowpassFilter.Create;
end;

procedure TFmEqGraphTest.FormDestroy(Sender: TObject);
begin
  FLowpass.Free;
end;

function TFmEqGraphTest.GetFilterSubGain(Sender: TObject;
  const Frequency: Single): Single;
begin
  Result := FLowpass.MagnitudeLog10(0.5 * Frequency);
end;

function TFmEqGraphTest.GetFilterGain(Sender: TObject;
  const Frequency: Single): Single;
begin
  Result := FLowpass.MagnitudeLog10(Frequency);
end;

end.
