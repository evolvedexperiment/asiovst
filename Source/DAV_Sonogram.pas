unit DAV_Sonogram;

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

{$I DAV_Compiler.INC}
{$DEFINE Use_IPPS}

uses
  Classes, Graphics, DAV_Common, DAV_Classes, DAV_Complex, DAV_GuiCommon,
  DAV_DspBuildingBlocks, DAV_DspFftReal2Complex
  {$IFDEF Use_IPPS}, DAV_DspFftReal2ComplexIPPS {$ENDIF}
  {$IFDEF Use_CUDA}, DAV_DspFftReal2ComplexCUDA{$ENDIF};

type
  TWindowType = (wtRectangle, wtTriangle, wtHanning, wtHamming, wtBlackman);
  TSonogram = class(TDspSampleRatePersistent)
  private
    FBitmap         : TBitmap;
    FBlockBuilder   : TBuildingBlocksCircular32;
    FBuffer         : PDAVComplexSingleFixedArray;
    FColorScheme    : array [0..255] of TRGB24;
    FCurrentSlice   : Integer;
    FFFTOrder       : Integer;
    FLogarithmic    : Boolean;
    FMagnitude      : PDAVSingleFixedArray;
    FWindowType     : TWindowType;
    FMaximumLevel   : Single;
    FMinimumLevel   : Single;
    FLevelRange     : Single;
    FLevelRangeInv  : Single;
    FMaximumAmp     : Single;
    FMinimumAmp     : Single;
    FOverlapFactor  : Integer;
    FUpperFrequency : Single;
    FLowerFrequency : Single;
    FLowerBin       : Integer;
    FUpperBin       : Integer;
    FBinRange       : Integer;
    {$IFDEF Use_IPPS}
    FFft           : TFftReal2ComplexIPPSFloat32;
    {$ELSE} {$IFDEF Use_CUDA}
    FFft           : TFftReal2ComplexCUDA32;
    {$ELSE}
    FFft           : TFftReal2ComplexNativeFloat32;
    {$ENDIF}{$ENDIF}
    procedure BitmapChangeHandler(Sender: TObject);
    procedure ProcessBlock(Sender: TObject; const Input: PDAVSingleFixedArray);
    procedure SetFFTOrder(const Value: Integer);
    procedure SetLogarithmic(const Value: Boolean);
    function Touch(Input: Single): Single;
    procedure SetMaximumLevel(const Value: Single);
    procedure SetMinimumLevel(const Value: Single);
    procedure SetOverlapFactor(const Value: Integer);
    procedure SetLowerFrequency(const Value: Single);
    procedure SetUpperFrequency(const Value: Single);

    procedure CalculateMaximumAmp;
    procedure CalculateMinimumAmp;
    procedure CalculateLevelRange;
    procedure CalculateOverlap;
    procedure CalculateBinRange;
    procedure CalculateLowerBin;
    procedure CalculateUpperBin;
  protected
    procedure BuildDefaultColorScheme; virtual;
    procedure DrawMagnitudeSlice; virtual;
    procedure FFTOrderChanged; virtual;
    procedure LowerFrequencyChanged; virtual;
    procedure LogarithmicChanged; virtual;
    procedure MaximumLevelChanged; virtual;
    procedure MinimumLevelChanged; virtual;
    procedure OverlapFactorChanged; virtual;
    procedure UpperFrequencyChanged; virtual;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure ProcessSample32(const Input: Single); virtual;
    procedure ProcessBlock32(const Input: PDAVSingleFixedArray; SampleFrames: Integer); overload;

    property FFTOrder: Integer read FFFTOrder write SetFFTOrder;
    property WindowType: TWindowType read FWindowType write FWindowType;
    property Logarithmic: Boolean read FLogarithmic write SetLogarithmic;
    property MinimumLevel: Single read FMinimumLevel write SetMinimumLevel;
    property MaximumLevel: Single read FMaximumLevel write SetMaximumLevel;
    property LowerFrequency: Single read FLowerFrequency write SetLowerFrequency;
    property UpperFrequency: Single read FUpperFrequency write SetUpperFrequency;
    property OverlapFactor: Integer read FOverlapFactor write SetOverlapFactor default 4;

    property Bitmap: TBitmap read FBitmap;
  end;

implementation

uses
  SysUtils, DAV_Approximations, DAV_DspWindowing;

constructor TSonogram.Create;
begin
 inherited;
 FBlockBuilder := TBuildingBlocksCircular32.Create;
 FBlockBuilder.OnProcess := ProcessBlock;

 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat32.Create(10);
 {$ELSE} {$IFDEF Use_CUDA}
 FFft := TFftReal2ComplexCUDA32.Create(10);
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat32.Create(10);
 FFft.DataOrder := doPackedComplex;
 {$ENDIF}{$ENDIF}

 FOverlapFactor  :=     8;
 FLogarithmic    :=  True;
 FMaximumLevel   :=     0;
 FMinimumLevel   :=   -96;
 FLowerFrequency :=    20;
 FUpperFrequency := 20000;

 FFft.AutoScaleType := astDivideBySqrtN;
 FFFTOrder := 10;
 FFTOrderChanged;

 CalculateLowerBin;
 CalculateUpperBin;
 CalculateBinRange;
 CalculateMaximumAmp;
 CalculateMinimumAmp;
 CalculateLevelRange;

 FBitmap := TBitmap.Create;
 with FBitmap do
  begin
   BeginUpdate;
   try
    Width := 256;
    Height := 256;
    PixelFormat := pf24bit;
    with Canvas do
     begin
      Brush.Color := clBlack;
      FillRect(ClipRect);
     end;
   finally
    EndUpdate;
   end;
   OnChange := BitmapChangeHandler;
  end;
 BuildDefaultColorScheme;
end;

destructor TSonogram.Destroy;
begin
 FreeAndNil(FBlockBuilder);
 FreeAndNil(FFft);
end;

procedure TSonogram.SampleRateChanged;
begin
 CalculateLowerBin;
 CalculateUpperBin;
 CalculateBinRange;
 inherited;
end;

procedure TSonogram.SetFFTOrder(const Value: Integer);
begin
 if FFFTOrder <> Value then
  begin
   FFFTOrder := Value;
   FFTOrderChanged;
  end;
end;

procedure TSonogram.SetLogarithmic(const Value: Boolean);
begin
  FLogarithmic := Value;
end;

procedure TSonogram.SetLowerFrequency(const Value: Single);
begin
 if FLowerFrequency <> Value then
  begin
   FLowerFrequency := Value;
   LowerFrequencyChanged;
  end;
end;

procedure TSonogram.SetMaximumLevel(const Value: Single);
begin
 if Value <= FMinimumLevel
  then Exit;

 if FMaximumLevel <> Value then
  begin
   FMaximumLevel := Value;
   MaximumLevelChanged;
  end;
end;

procedure TSonogram.SetMinimumLevel(const Value: Single);
begin
 if Value >= FMaximumLevel
  then Exit;

 if FMinimumLevel <> Value then
  begin
   FMinimumLevel := Value;
   MinimumLevelChanged;
  end;
end;

procedure TSonogram.SetOverlapFactor(const Value: Integer);
begin
 if FOverlapFactor <> Value then
  begin
   FOverlapFactor := Value;
   OverlapFactorChanged;
  end;
end;

procedure TSonogram.SetUpperFrequency(const Value: Single);
begin
 if FUpperFrequency <> Value then
  begin
   FUpperFrequency := Value;
   UpperFrequencyChanged;
  end;
end;

procedure TSonogram.BuildDefaultColorScheme;
var
  Index    : Integer;
  Scale, S : Single;
  R, G, B  : Single;
begin
 S := 0.5 / (Length(FColorScheme) - 1);
 for Index := 0 to Length(FColorScheme) - 1 do
  begin
   Scale := Index * S;
   HLSToRGB(0.6 + Scale, 0.1 + Scale, 0.5 + Scale, R, G, B);
   FColorScheme[Index].R := IntLimit(Round(255 * R), 0, 255) ;
   FColorScheme[Index].G := IntLimit(Round(255 * G), 0, 255);
   FColorScheme[Index].B := IntLimit(Round(255 * B), 0, 255);
  end;
end;

procedure TSonogram.OverlapFactorChanged;
begin
 CalculateOverlap;
end;

procedure TSonogram.LogarithmicChanged;
begin
 Changed;
end;

procedure TSonogram.LowerFrequencyChanged;
begin
 CalculateLowerBin;
 CalculateBinRange;
 Changed;
end;

procedure TSonogram.UpperFrequencyChanged;
begin
 CalculateUpperBin;
 CalculateBinRange;
 Changed;
end;

procedure TSonogram.MaximumLevelChanged;
begin
 CalculateMaximumAmp;
 CalculateLevelRange;
 Changed;
end;

procedure TSonogram.MinimumLevelChanged;
begin
 CalculateMinimumAmp;
 CalculateLevelRange;
 Changed;
end;

procedure TSonogram.CalculateBinRange;
begin
 FBinRange := FUpperBin - FLowerBin;
end;

procedure TSonogram.CalculateLevelRange;
begin
 FLevelRange := FMaximumLevel - FMinimumLevel;
 FLevelRangeInv := 1 / FLevelRange;
end;

procedure TSonogram.CalculateLowerBin;
begin
 FLowerBin := Round(FFft.FFTSize * FLowerFrequency / SampleRate)
end;

procedure TSonogram.CalculateOverlap;
begin
 with FBlockBuilder
  do OverlapSize := BlockSize - (BlockSize div FOverlapFactor);
 Changed;
end;

procedure TSonogram.CalculateUpperBin;
begin
 FUpperBin := Round(FFft.FFTSize * FUpperFrequency / SampleRate)
end;

procedure TSonogram.CalculateMaximumAmp;
begin
 FMaximumAmp := dB_to_Amp(FMaximumLevel);
end;

procedure TSonogram.CalculateMinimumAmp;
begin
 FMinimumAmp := dB_to_Amp(FMinimumLevel);
end;

procedure TSonogram.BitmapChangeHandler(Sender: TObject);
begin
 if FCurrentSlice >= FBitmap.Height
  then FCurrentSlice := 0;
end;

procedure TSonogram.FFTOrderChanged;
begin
 FFft.Order := FFFTOrder;

 ReallocMem(FBuffer, FFft.BinCount * SizeOf(TComplexSingle));
 FillChar(FBuffer^, FFft.BinCount * SizeOf(TComplexSingle), 0);

 FBlockBuilder.BlockSize := FFft.FFTSize;
 CalculateOverlap;

 ReallocMem(FMagnitude, FFft.BinCount * SizeOf(Single));
 FillChar(FMagnitude^, FFft.BinCount * SizeOf(Single), 0);

 Changed;
end;

procedure TSonogram.ProcessBlock32(const Input: PDAVSingleFixedArray;
  SampleFrames: Integer);
begin
 FBlockBuilder.ProcessBlock32(@Input[0], SampleFrames);
end;

procedure TSonogram.ProcessSample32(const Input: Single);
begin
 FBlockBuilder.ProcessSample32(Input);
end;

function TSonogram.Touch(Input: Single): Single;
begin
 if FLogarithmic
  then Result := ((0.5 * FastAmptodBMinError3(Input)) - FMinimumLevel) * FLevelRangeInv
  else Result := Sqrt(Input);
end;

procedure TSonogram.ProcessBlock(Sender: TObject;
  const Input: PDAVSingleFixedArray);
var
  Bin     : Integer;
begin
 case FWindowType of
  wtTriangle : ApplyTriangleWindow(Input, FFft.FFTSize);
  wtHanning  : ApplyHanningWindow(Input, FFft.FFTSize);
  wtHamming  : ApplyHammingWindow(Input, FFft.FFTSize);
  wtBlackman : ApplyBlackmanWindow(Input, FFft.FFTSize);
 end;

 FFft.PerformFFT(FBuffer, Input);

 // calculated log magnitude
 FMagnitude^[0] := Touch(Sqr(FBuffer[0].Re));
 for Bin := 0 to FFft.BinCount - 2
  do FMagnitude^[Bin] := Touch(Sqr(FBuffer[Bin].Re) + Sqr(FBuffer[Bin].Im));
 FMagnitude^[FFft.BinCount - 1] := Touch(Sqr(FBuffer[FFft.BinCount - 1].Re));

 DrawMagnitudeSlice;

 Inc(FCurrentSlice);
 if FCurrentSlice >= FBitmap.Height
  then FCurrentSlice := 0;
end;

procedure TSonogram.DrawMagnitudeSlice;
var
  Pixel, Bin : Integer;
  Scale      : Single;
  ScnLine    : PRGB24Array;
begin
 ScnLine := FBitmap.ScanLine[FCurrentSlice];
 Scale := FBinRange / FBitmap.Width;
 for Pixel := 0 to FBitmap.Width - 1 do
  begin
   Bin := FLowerBin + Round(Pixel * Scale);
   ScnLine^[Pixel] := FColorScheme[IntLimit(Round(255 * FMagnitude^[Bin]), 0, 255)];
  end;
end;

end.
