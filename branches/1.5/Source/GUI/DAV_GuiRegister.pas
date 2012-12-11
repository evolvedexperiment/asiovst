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

unit DAV_GuiRegister;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, TypInfo,
  {$IFDEF FPC}
  LCLIntf, LazIDEIntf, PropEdits, ComponentEditors
  {$ELSE}
  {$IFDEF COMPILER6_UP}
  DesignIntf
  {$ELSE}
  DsgnIntf
  {$ENDIF}
  {$ENDIF};

procedure Register;

implementation

{$IFNDEF FPC}{$R ..\..\Resources\DAV_GuiRegister.res}{$ENDIF}

uses
  {$IFDEF FPC} LResources, {$ELSE} DAV_GuiLevelMeter, DAV_GuiBackgrounds,
  DAV_GuiEdit, {$ENDIF} DAV_GuiADSRGraph, DAV_GuiAudioDataDisplay,
  DAV_GuiButton, DAV_GuiCheckBox, DAV_GuiCorrelationMeter, DAV_GuiDial,
  DAV_GuiDialDesign, DAV_GuiDialRenderer, DAV_GuiDynamicWaveform,
  DAV_GuiEQGraph, DAV_GuiEQSlide, DAV_GuiFader, DAV_GuiFont, DAV_GuiFontList,
  DAV_GuiFontDesign, DAV_GuiGroup, DAV_GuiInscription, DAV_GuiImageControl,
  DAV_GuiImageList, DAV_GuiGraphXY, DAV_GuiGraphXYDesign, DAV_GuiLabel,
  DAV_GuiLED, DAV_GuiMediaButton, DAV_GuiMidiKeys, DAV_GuiRadioButton,
  DAV_GuiPaintBox, DAV_GuiPanel, DAV_GuiPixelMap, DAV_GuiPixelMapDesign,
  DAV_GuiPng, DAV_GuiPngList, DAV_GuiPngDesign, DAV_GuiSelectBox,
  DAV_GuiSlider, DAV_GuiStaticWaveform, DAV_GuiStitchedButton,
  DAV_GuiStitchedControls, DAV_GuiStitchedDial, DAV_GuiStitchedDisplay,
  DAV_GuiStitchedImageList, DAV_GuiStitchedRadioSwitch, DAV_GuiStitchedPngList,
  DAV_GuiStitchedSwitch, DAV_GuiVUMeter;

procedure Register;
begin
  RegisterComponents('ASIO/VST GUI', [
    TGuiADSRGraph, TGuiAudioDataDisplay, TGuiButton, TGuiControlsCheckBox,
    TGuiCorrelationMeter, TGuiDial, TGuiDialEx, TGuiDialImageList,
    TGuiDialImageRenderer, TGuiDialMetal, TGuiDynamicWaveform, TGuiEQGraph,
    TGuiEQSlide, TGuiFader, TGuiFontList, TGuiGraphXY, TGuiGroup, TGuiGroupTop,
    TGuiGroupSide, TGuiGroupSimple, TGuiInscription, TGuiIntegerBox,
    TGuiImageList, TGuiLabel, TGuiLED, TGuiMediaButton, TGuiMidiKeys,
    TGuiControlsRadioButton, TGuiPaintBox, TGuiPanel, TGuiPngList,
    TGuiSelectBox, TGuiSlider, TGuiStaticWaveform, TGuiSwitch, TGuiVUMeter
    {$IFNDEF FPC}, TGuiLevelMeter, TGuiColorLevelMeter, TGuiBackground,
    TGuiControlsEdit {$ENDIF}]);

  RegisterComponents('ASIO/VST GUI Stitched', [
    TGuiStitchedButton, TGuiStitchedDial, TGuiStitchedDisplay,
    TGuiStitchedImageList, TGuiStitchedPNGList, TGuiStitchedRadioSwitch,
    TGuiStitchedSwitch]);

  RegisterPropertyEditor(TypeInfo(TGuiCustomPixelMap), nil, '', TPixelMapProperty);
  RegisterPropertyEditor(TypeInfo(TPortableNetworkGraphicPixel32), nil, '', TPngProperty);

  RegisterPropertyEditor(TypeInfo(string), TGuiDialLayerCollectionItem, 'PrimitiveClassName', nil);
  RegisterPropertyEditor(TypeInfo(TCustomGuiDialPrimitive), TGuiDialLayerCollectionItem, 'Primitive', TPrimitiveClassProperty);

  RegisterPropertyEditor(TypeInfo(string), TGuiGraphXYSeriesCollectionItem, 'SeriesClassName', nil);
  RegisterPropertyEditor(TypeInfo(TCustomGuiGraphXYSeries), TGuiGraphXYSeriesCollectionItem, 'Series', TSeriesClassProperty);

  RegisterPropertyEditor(TypeInfo(string), TGuiCustomFontCollectionItem, 'FontClassName', nil);
  RegisterPropertyEditor(TypeInfo(TGuiCustomFont), TGuiCustomFontCollectionItem, 'Font', TFontClassProperty);
end;

{$IFDEF FPC}
initialization
  {$i ..\..\Resources\DAV_GuiRegister.lrs}
{$ENDIF}

end.
