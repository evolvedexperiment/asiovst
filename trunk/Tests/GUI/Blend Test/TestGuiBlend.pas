unit TestGuiBlend;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2010             //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

uses
  TestFramework, Windows, Controls, Types, Classes, SysUtils, Messages,
  Graphics, DAV_GuiCommon, DAV_GuiBlend, DAV_ProcessorInfo, DAV_Bindings;

type
  TCustomTestBlendModes = class(TTestCase)
  strict private
    FForeground : PPixel32Array;
    FBackground : PPixel32Array;
  protected
    procedure TestBlend; virtual;
    procedure TestBlendInplace; virtual;
    procedure TestBlendLine; virtual;
    procedure TestCombine; virtual;
    procedure TestCombineInplace; virtual;
    procedure TestCombineLine; virtual;
    procedure TestMerge; virtual;
    procedure TestMergeInplace; virtual;
    procedure TestMergeLine; virtual;
    procedure PerformanceTest; virtual;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  end;

  TTestBlendModesNative = class(TCustomTestBlendModes)
  published
    procedure TestBlend; override;
    procedure TestBlendInplace; override;
    procedure TestBlendLine; override;
    procedure TestCombine; override;
    procedure TestCombineInplace; override;
    procedure TestCombineLine; override;
    procedure TestMerge; override;
    procedure TestMergeInplace; override;
    procedure TestMergeLine; override;
    procedure PerformanceTest; override;
  end;

  TTestBlendModesMMX = class(TCustomTestBlendModes)
  published
    procedure TestBlend; override;
    procedure TestBlendInplace; override;
    procedure TestBlendLine; override;
    procedure TestCombine; override;
    procedure TestCombineInplace; override;
    procedure TestCombineLine; override;
    procedure TestMerge; override;
    procedure TestMergeInplace; override;
    procedure TestMergeLine; override;
    procedure PerformanceTest; override;
  end;

  TTestBlendModesSSE2 = class(TCustomTestBlendModes)
  published
    procedure TestBlend; override;
    procedure TestBlendInplace; override;
    procedure TestBlendLine; override;
    procedure TestCombine; override;
    procedure TestCombineInplace; override;
    procedure TestCombineLine; override;
    procedure TestMerge; override;
    procedure TestMergeInplace; override;
    procedure TestMergeLine; override;
    procedure PerformanceTest; override;
  end;

implementation

{ TTestBlendModes }

procedure TCustomTestBlendModes.SetUp;
begin
 inherited;
 GetMem(FForeground, 256 * SizeOf(TPixel32));
 GetMem(FBackground, 256 * SizeOf(TPixel32));
end;

procedure TCustomTestBlendModes.TearDown;
begin
 inherited;
 Dispose(FForeground);
 Dispose(FBackground);
end;

procedure TCustomTestBlendModes.TestBlend;
var
  BlendColor32    : TPixel32;
  CombinedColor32 : TPixel32;
  ExpectedColor32 : TPixel32;
  Index           : Integer;
begin
  for Index := 0 to High(Byte) do
  begin
    BlendColor32 := pxWhite32;
    BlendColor32.A := Index;
    ExpectedColor32.ARGB := (Index shl 16) or (Index shl 8) or Index;
    CombinedColor32 := BlendPixel(BlendColor32, pxBlack32);
    EMMS;
    CombinedColor32.A := 0;
    CheckEquals(ExpectedColor32.ARGB, CombinedColor32.ARGB,
      'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
      ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
  end;
end;

procedure TCustomTestBlendModes.TestBlendInplace;
var
  BlendColor32    : TPixel32;
  CombinedColor32 : TPixel32;
  ExpectedColor32 : TPixel32;
  Index           : Integer;
begin
  for Index := 0 to High(Byte) do
  begin
    BlendColor32 := pxWhite32;
    BlendColor32.A := Index;
    ExpectedColor32.ARGB := (Index shl 16) or (Index shl 8) or Index;
    CombinedColor32 := pxBlack32;
    BlendPixelInplace(BlendColor32, CombinedColor32);
    EMMS;
    CombinedColor32.A := 0;
    CheckEquals(ExpectedColor32.ARGB, CombinedColor32.ARGB,
      'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
      ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
  end;
end;

procedure TCustomTestBlendModes.TestBlendLine;
var
  CombinedColor32 : TPixel32;
  ExpectedColor32 : TPixel32;
  Index           : Integer;
begin
  for Index := 0 to High(Byte) do
  begin
    FBackground^[Index] := pxBlack32;
    FForeground^[Index] := pxWhite32;
    FForeground^[Index].A := Index;
  end;

  BlendLine(PPixel32(FForeground), PPixel32(FBackground), 256);
  EMMS;

  for Index := 0 to High(Byte) do
  begin
    ExpectedColor32.ARGB := (Index shl 16) or (Index shl 8) or Index;
    FBackground^[Index].A := 0;
    CheckEquals(ExpectedColor32.ARGB, FBackground^[Index].ARGB,
      'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
      ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
  end;
end;

procedure TCustomTestBlendModes.TestCombine;
var
  CombinedColor32 : TPixel32;
  ExpectedColor32 : TPixel32;
  Index           : Integer;
begin
  for Index := 0 to High(Byte) do
  begin
    ExpectedColor32.ARGB := $FF000000 or (Index shl 16) or (Index shl 8) or Index;
    CombinedColor32 := CombinePixel(pxWhite32, pxBlack32, Index);
    EMMS;
    CheckEquals(ExpectedColor32.ARGB, CombinedColor32.ARGB,
      'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
      ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
  end;
end;

procedure TCustomTestBlendModes.TestCombineInplace;
var
  CombinedColor32 : TPixel32;
  ExpectedColor32 : TPixel32;
  Index           : Integer;
begin
  for Index := 0 to High(Byte) do
  begin
    ExpectedColor32.ARGB := $FF000000 or (Index shl 16) or (Index shl 8) or Index;
    CombinedColor32 := pxBlack32;
    CombinePixelInplace(pxWhite32, CombinedColor32, Index);
    EMMS;
    CheckEquals(ExpectedColor32.ARGB, CombinedColor32.ARGB,
      'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
      ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
  end;
end;

procedure TCustomTestBlendModes.TestCombineLine;
var
  CombinedColor32 : TPixel32;
  ExpectedColor32 : TPixel32;
  Index           : Integer;
begin
  for Index := 0 to High(Byte) do
  begin
    FBackground^[Index] := pxBlack32;
    FForeground^[Index] := pxWhite32;
    FForeground^[Index].A := Index;
  end;

  CombineLine(PPixel32(FForeground), PPixel32(FBackground), 256, $FF);

  for Index := 0 to High(Byte) do
  begin
    ExpectedColor32.ARGB := (Index shl 24) or $FFFFFF;
    EMMS;
    CheckEquals(ExpectedColor32.ARGB, FBackground^[Index].ARGB,
      'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
      ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
  end;
end;

procedure TCustomTestBlendModes.TestMerge;
var
  BlendColor32    : TPixel32;
  CombinedColor32 : TPixel32;
  ExpectedColor32 : TPixel32;
  Index           : Integer;
begin
  for Index := 0 to High(Byte) do
  begin
    BlendColor32 := pxWhite32;
    BlendColor32.A := Index;
    ExpectedColor32.ARGB := $FF000000 + (Index shl 16) or (Index shl 8) or Index;
    CombinedColor32 := MergePixel(BlendColor32, pxBlack32);
    EMMS;
    CheckEquals(ExpectedColor32.ARGB, CombinedColor32.ARGB,
      'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
      ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
  end;
end;

procedure TCustomTestBlendModes.TestMergeInplace;
var
  BlendColor32    : TPixel32;
  CombinedColor32 : TPixel32;
  ExpectedColor32 : TPixel32;
  Index           : Integer;
begin
  for Index := 0 to High(Byte) do
  begin
    BlendColor32 := pxWhite32;
    BlendColor32.A := Index;
    ExpectedColor32.ARGB := $FF000000 + (Index shl 16) or (Index shl 8) or Index;
    CombinedColor32 := pxBlack32;
    MergePixelInplace(BlendColor32, CombinedColor32);
    EMMS;
    CheckEquals(ExpectedColor32.ARGB, CombinedColor32.ARGB,
      'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
      ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
  end;
end;

procedure TCustomTestBlendModes.TestMergeLine;
var
  CombinedColor32 : TPixel32;
  ExpectedColor32 : TPixel32;
  Index           : Integer;
begin
  for Index := 0 to High(Byte) do
  begin
    FBackground^[Index] := pxBlack32;
    FForeground^[Index] := pxWhite32;
    FForeground^[Index].A := Index;
  end;

  MergeLine(PPixel32(FForeground), PPixel32(FBackground), 256);

  for Index := 0 to High(Byte) do
  begin
    ExpectedColor32.ARGB := (Index shl 16) or (Index shl 8) or Index;
    EMMS;
    FBackground^[Index].A := 0;
    CheckEquals(ExpectedColor32.ARGB, FBackground^[Index].ARGB,
      'Color should be: ' + IntToHex(ExpectedColor32.ARGB, 8) +
      ', but was: ' + IntToHex(CombinedColor32.ARGB, 8));
  end;
end;

procedure TCustomTestBlendModes.PerformanceTest;
var
  Start, Stop, Freq : Int64;
  BlendColor32      : TPixel32;
  Index             : Integer;
begin
 BlendColor32 := pxWhite32;
 BlendColor32.A := $5A;

 QueryPerformanceFrequency(Freq);
 QueryPerformanceCounter(Start);

 for Index := 0 to $7FFFFFF
  do BlendPixel(BlendColor32, pxBlack32);
 EMMS;

 for Index := 0 to $7FFFFFF do
  begin
   BlendPixel(BlendColor32, pxBlack32);
   EMMS;
  end;

 QueryPerformanceCounter(Stop);

 Fail('Performance: ' + FloatToStr(1000 * (Stop - Start) / Freq));
end;


{ TTestBlendModesNative }

procedure TTestBlendModesNative.TestBlend;
begin
  BindingBlend.Rebind([]);
  inherited;
end;

procedure TTestBlendModesNative.TestBlendInplace;
begin
  BindingBlend.Rebind([]);
  inherited;
end;

procedure TTestBlendModesNative.TestBlendLine;
begin
  BindingBlend.Rebind([]);
  inherited;
end;

procedure TTestBlendModesNative.TestCombine;
begin
  BindingBlend.Rebind([]);
  inherited;
end;

procedure TTestBlendModesNative.TestCombineInplace;
begin
  BindingBlend.Rebind([]);
  inherited;
end;

procedure TTestBlendModesNative.TestCombineLine;
begin
  BindingBlend.Rebind([]);
  inherited;
end;

procedure TTestBlendModesNative.TestMerge;
begin
  BindingBlend.Rebind([]);
  inherited;
end;

procedure TTestBlendModesNative.TestMergeInplace;
begin
  BindingBlend.Rebind([]);
  inherited;
end;

procedure TTestBlendModesNative.TestMergeLine;
begin
  BindingBlend.Rebind([]);
  inherited;
end;

procedure TTestBlendModesNative.PerformanceTest;
begin
  BindingBlend.Rebind([]);
  inherited;
end;


{ TTestBlendModesMMX }

procedure TTestBlendModesMMX.TestBlend;
begin
  BindingBlend.Rebind([pfMMX]);
  inherited;
end;

procedure TTestBlendModesMMX.TestBlendInplace;
begin
  BindingBlend.Rebind([pfMMX]);
  inherited;
end;

procedure TTestBlendModesMMX.TestBlendLine;
begin
  BindingBlend.Rebind([pfMMX]);
  inherited;
end;

procedure TTestBlendModesMMX.TestCombine;
begin
  BindingBlend.Rebind([pfMMX]);
  inherited;
end;

procedure TTestBlendModesMMX.TestCombineInplace;
begin
  BindingBlend.Rebind([pfMMX]);
  inherited;
end;

procedure TTestBlendModesMMX.TestCombineLine;
begin
  BindingBlend.Rebind([pfMMX]);
  inherited;
end;

procedure TTestBlendModesMMX.TestMerge;
begin
  BindingBlend.Rebind([pfMMX]);
  inherited;
end;

procedure TTestBlendModesMMX.TestMergeInplace;
begin
  BindingBlend.Rebind([pfMMX]);
  inherited;
end;

procedure TTestBlendModesMMX.TestMergeLine;
begin
  BindingBlend.Rebind([pfMMX]);
  inherited;
end;

procedure TTestBlendModesMMX.PerformanceTest;
begin
  BindingBlend.Rebind([pfMMX]);
  inherited;
end;


{ TTestBlendModesSSE2 }

procedure TTestBlendModesSSE2.TestBlend;
begin
  BindingBlend.Rebind([pfSSE2]);
  inherited;
end;

procedure TTestBlendModesSSE2.TestBlendInplace;
begin
  BindingBlend.Rebind([pfSSE2]);
  inherited;
end;

procedure TTestBlendModesSSE2.TestBlendLine;
begin
  BindingBlend.Rebind([pfSSE2]);
  inherited;
end;

procedure TTestBlendModesSSE2.TestCombine;
begin
  BindingBlend.Rebind([pfSSE2]);
  inherited;
end;

procedure TTestBlendModesSSE2.TestCombineInplace;
begin
  BindingBlend.Rebind([pfSSE2]);
  inherited;
end;

procedure TTestBlendModesSSE2.TestCombineLine;
begin
  BindingBlend.Rebind([pfSSE2]);
  inherited;
end;

procedure TTestBlendModesSSE2.TestMerge;
begin
  BindingBlend.Rebind([pfSSE2]);
  inherited;
end;

procedure TTestBlendModesSSE2.TestMergeInplace;
begin
  BindingBlend.Rebind([pfSSE2]);
  inherited;
end;

procedure TTestBlendModesSSE2.TestMergeLine;
begin
  BindingBlend.Rebind([pfSSE2]);
  inherited;
end;

procedure TTestBlendModesSSE2.PerformanceTest;
begin
  BindingBlend.Rebind([pfSSE2]);
  inherited;
end;


initialization
  RegisterTest(TTestBlendModesNative.Suite);
  if ProcessorInfo.HasMMX
   then RegisterTest(TTestBlendModesMMX.Suite);
  if ssSSE2 in ProcessorInfo.SupportsSSE
   then RegisterTest(TTestBlendModesSSE2.Suite);

end.
