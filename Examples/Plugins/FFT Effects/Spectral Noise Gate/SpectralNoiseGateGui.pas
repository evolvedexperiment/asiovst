unit SpectralNoiseGateGui;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2009             //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_GuiDial, DAV_GuiLabel, Controls, DAV_GuiBaseControl;

type
  TFmSpectralNoiseGate = class(TForm)
    DialSpeed: TGuiDial;
    DialStages: TGuiDial;
    DialDepth: TGuiDial;
    DialMix: TGuiDial;
    LbSpeed: TGuiLabel;
    LbStages: TGuiLabel;
    LbDepth: TGuiLabel;
    LbMix: TGuiLabel;
    LbSpeedValue: TGuiLabel;
    LbStagesValue: TGuiLabel;
    LbDepthValue: TGuiLabel;
    LbMixValue: TGuiLabel;
    DialDrift: TGuiDial;
    LbDrift: TGuiLabel;
    LbDriftValue: TGuiLabel;
    DIL: TGuiDialImageList;
    procedure FormCreate(Sender: TObject);
  end;

implementation

{$R *.DFM}

uses
  Math, PngImage, DAV_VSTModuleWithPrograms, SpectralNoiseGateDM;

procedure TFmSpectralNoiseGate.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  PngBmp : TPngObject;
begin
 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'ChorusKnob', 'PNG');
  try
   with DIL.DialImages.Add do
    begin
     NumGlyphs := 65;
     PngBmp.LoadFromStream(RS);
     DialBitmap.Assign(PngBmp);
    end;
   DialSpeed.DialImageIndex  := 0;
   DialDepth.DialImageIndex  := 0;
   DialStages.DialImageIndex := 0;
   DialMix.DialImageIndex    := 0;
   DialDrift.DialImageIndex  := 0;
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

end.
