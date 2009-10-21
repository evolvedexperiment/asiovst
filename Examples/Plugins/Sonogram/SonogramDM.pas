unit SonogramDM;

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
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_Complex,
  DAV_Classes, DAV_Sonogram, DAV_VSTModule;

type
  TSonogramDataModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure ParameterIntegerDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterOrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOverlapFactorChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterWindowDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterWindowChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
    FSonogram : TSonogram;
  public
    property Sonogram: TSonogram read FSonogram;
  end;

implementation

{$R *.DFM}

uses
  DAV_Approximations, DAV_DspWindowing, SonogramGui;

procedure TSonogramDataModule.VSTModuleCreate(Sender: TObject);
begin
 Assert(numInputs = 1);
end;

procedure TSonogramDataModule.VSTModuleOpen(Sender: TObject);
begin
 FSonogram := TSonogram.Create;
 with FSonogram do
  begin
   SampleRate := Self.SampleRate;
   WindowType := wtBlackman;
   Logarithmic := True;
   MaximumLevel := 6;
   MinimumLevel := -96;
  end;

 Parameter[0] := 10;
 Parameter[1] :=  8;
 Parameter[2] :=  4; 
end;

procedure TSonogramDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FSonogram);
end;

procedure TSonogramDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmSonogram.Create(Self);
end;

procedure TSonogramDataModule.ParameterIntegerDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(Round(Parameter[Index]));
end;

procedure TSonogramDataModule.ParameterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FSonogram.FFTOrder := Round(Value);
end;

procedure TSonogramDataModule.ParameterOverlapFactorChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FSonogram.OverlapFactor := Round(Value);
end;

procedure TSonogramDataModule.ParameterWindowDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'Rectangle';
  1 : PreDefined := 'Triangle';
  2 : PreDefined := 'Hanning';
  3 : PreDefined := 'Hamming';
  4 : PreDefined := 'Blackman';
 end;
end;

procedure TSonogramDataModule.ParameterWindowChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FSonogram.WindowType := TWindowType(Round(Parameter[Index]));
end;

procedure TSonogramDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
begin
 FSonogram.ProcessBlock32(@Inputs[0, 0], SampleFrames);
 Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
end;

end.
