unit ChunkDemoGUI;

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
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_GuiLabel, Controls, DAV_GuiBaseControl, DAV_GuiDial;

type
  TFmChunkDemo = class(TForm)
    DialAlpha: TGuiDial;
    LbAlpha: TGuiLabel;
    LbBeta: TGuiLabel;
    LbGamma: TGuiLabel;
    LbDelta: TGuiLabel;
    DialBeta: TGuiDial;
    DialGamma: TGuiDial;
    DialDelta: TGuiDial;
    DIL: TGuiDialImageList;
    procedure FormShow(Sender: TObject);
    procedure DialAlphaChange(Sender: TObject);
    procedure DialBetaChange(Sender: TObject);
    procedure DialGammaChange(Sender: TObject);
    procedure DialDeltaChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    procedure UpdateAlpha;
    procedure UpdateBeta;
    procedure UpdateGamma;
    procedure UpdateDelta;
  end;

implementation

uses
  ChunkDemoDM, PngImage;

{$R *.DFM}

{ TFmChunkDemo }

procedure TFmChunkDemo.DialAlphaChange(Sender: TObject);
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Parameter[0] := DialAlpha.Position;
  end;
end;

procedure TFmChunkDemo.DialBetaChange(Sender: TObject);
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Parameter[1] := DialBeta.Position;
  end;
end;

procedure TFmChunkDemo.DialGammaChange(Sender: TObject);
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Parameter[2] := DialGamma.Position;
  end;
end;

procedure TFmChunkDemo.DialDeltaChange(Sender: TObject);
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Parameter[3] := DialDelta.Position;
  end;
end;

procedure TFmChunkDemo.FormCreate(Sender: TObject);
var
  RS       : TResourceStream;
{$IFDEF DELPHI2010_UP}
  PngBmp   : TPngImage;
{$ELSE}
  PngBmp   : TPngObject;
{$ENDIF}
(*
  x, y     : Integer;
  ScanLine : array [0..1] of PByteArray;
  b        : Byte;
*)
begin
 {$IFDEF DELPHI2010_UP}
 PngBmp := TPngImage.Create;
 {$ELSE}
 PngBmp := TPngObject.Create;
 {$ENDIF}
 try
  RS := TResourceStream.Create(hInstance, 'ChunkDemoKnob', 'PNG');
  try
   PngBmp.TransparentColor := Self.Color;
   PngBmp.LoadFromStream(RS);

   with DIL.DialImages.Add do
    begin
     DialBitmap.Width := PngBmp.Width;
     DialBitmap.Height := PngBmp.Height;
     PngBmp.DrawUsingPixelInformation(DialBitmap.Canvas, Point(0, 0));
     GlyphCount := 65;
    end;

(*
   for y := 0 to (Height div 2) - 1 do
    begin
     ScanLine[0] := PngBmp.Scanline[y];
     ScanLine[1] := PngBmp.Scanline[(Height div 2)];
     for x := 0 to (3 * Width) - 1 do
      begin
       b := ScanLine[0]^[x];
       ScanLine[0]^[x] := ScanLine[1]^[(3 * Width) - 1 - x];
       ScanLine[1]^[(3 * Width) - 1 - x] := b;
      end;
    end;

   with DIL.DialImages.Add do
    begin
     DialBitmap.Assign(PngBmp);
     GlyphCount := 65;
    end;
*)
   DialAlpha.DialImageIndex := 0;
   DialGamma.DialImageIndex := 0;
   DialBeta.DialImageIndex := 0;
   DialDelta.DialImageIndex := 0;
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmChunkDemo.FormShow(Sender: TObject);
begin
 UpdateAlpha;
 UpdateBeta;
 UpdateGamma;
 UpdateDelta;
end;

procedure TFmChunkDemo.UpdateAlpha;
var
  Alpha : Single;
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Alpha := Parameter[0];
   if DialAlpha.Position <> Alpha
    then DialAlpha.Position := Alpha;
  end;
end;

procedure TFmChunkDemo.UpdateBeta;
var
  Beta : Single;
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Beta := Parameter[1];
   if DialBeta.Position <> Beta
    then DialBeta.Position := Beta;
  end;
end;

procedure TFmChunkDemo.UpdateDelta;
var
  Delta : Single;
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Delta := Parameter[2];
   if DialGamma.Position <> Delta
    then DialGamma.Position := Delta;
  end;
end;

procedure TFmChunkDemo.UpdateGamma;
var
  Gamma : Single;
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Gamma := Parameter[3];
   if DialDelta.Position <> Gamma
    then DialDelta.Position := Gamma;
  end;
end;

end.