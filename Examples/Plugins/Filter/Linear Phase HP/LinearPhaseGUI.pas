unit LinearPhaseGUI;

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

{$I DAV_Compiler.inc}

uses 
  Windows, Messages, SysUtils, Classes, Forms, Controls, DAV_Types,
  DAV_VSTModule, DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiDial;

type
  TFmLinearPhase = class(TForm)
    DialFrequency: TGuiDial;
    LbFrequency: TGuiLabel;
    LbFrequencyValue: TGuiLabel;
    procedure DialFrequencyChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure UpdateFrequency;
  end;

implementation

{$R *.DFM}

uses
  LinearPhaseDM, DAV_VSTModuleWithPrograms;

procedure TFmLinearPhase.FormCreate(Sender: TObject);
var
  RS  : TResourceStream;
begin
 RS := TResourceStream.Create(hInstance, 'YamahaKnob', 'BMP');
 try
  DialFrequency.DialBitmap.LoadFromStream(RS); RS.Position := 0;
 finally
  RS.Free;
 end;
end;

procedure TFmLinearPhase.FormShow(Sender: TObject);
begin
 UpdateFrequency;
end;

procedure TFmLinearPhase.DialFrequencyChange(Sender: TObject);
begin
 with TLinearPhaseDataModule(Owner) do
  begin
   if Parameter[0] <> DialFrequency.Position
    then Parameter[0] := DialFrequency.Position;
  end;
end;

procedure TFmLinearPhase.UpdateFrequency;
var
  Freq : Single;
begin
 with TLinearPhaseDataModule(Owner) do
  begin
   Freq := Parameter[0];
   if DialFrequency.Position <> Freq
    then DialFrequency.Position := Freq;
   if Freq < 1000
    then LbFrequencyValue.Caption := FloatToStrF(Freq, ffGeneral, 3, 3) + ' Hz'
    else LbFrequencyValue.Caption := FloatToStrF(1E-3 * Freq, ffGeneral, 3, 3) + ' kHz';
  end;
end;

end.
