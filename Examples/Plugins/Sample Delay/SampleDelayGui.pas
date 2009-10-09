unit SampleDelayGui;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls,
  DAV_Common, DAV_VSTModule, DAV_GuiBackgrounds;

type
  TFmSampleDelay = class(TForm)
    BrushedMetal: TGuiBackground;
    LbSample: TLabel;
    SbSample: TScrollBar;
    LbSampleValue: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure SbSampleChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure UpdateSampleFrames;  
  end;

implementation

uses
  SampleDelayDM;

{$R *.DFM}

procedure TFmSampleDelay.FormCreate(Sender: TObject);
begin
 BrushedMetal.Active := True;
end;

procedure TFmSampleDelay.FormShow(Sender: TObject);
begin
 UpdateSampleFrames;
end;

procedure TFmSampleDelay.SbSampleChange(Sender: TObject);
begin
 with TSampleDelayDataModule(Owner) do
  begin
   if Round(Parameter[0]) <> SbSample.Position
    then Parameter[0] := SbSample.Position;
  end;
end;

procedure TFmSampleDelay.UpdateSampleFrames;
begin
 with TSampleDelayDataModule(Owner) do
  begin
   if SbSample.Position <> Round(Parameter[0])
    then SbSample.Position := Round(Parameter[0]);
   LbSampleValue.Caption := IntToStr(Round(Parameter[0]));
  end;
end;

end.