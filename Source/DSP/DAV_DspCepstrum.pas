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

unit DAV_DspCepstrum;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Complex, DAV_Classes, DAV_DspFftReal2Complex;

type
  TCustomCepstrum = class(TDspPersistent)
  private
    function GetFftOrder: Byte;
    procedure SetFftOrder(const Value: Byte);
  protected
    FFFT: TFftReal2Complex;
    FFFTSize: Integer;
    FFFTSizeHalf: Integer;
    procedure AssignTo(Dest: TPersistent); override;
    procedure FFTOrderChanged; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property FFTOrder: Byte read GetFftOrder write SetFftOrder;
    property FFTSize: Integer read FFFTSize;
  end;

  TCustomPowerCepstrum = TCustomCepstrum;

  TPowerCepstrum32 = class(TCustomPowerCepstrum)
  private
    function GetFft: TFftReal2ComplexNativeFloat32;
  protected
    FSignalFreq: PDAVComplexSingleFixedArray;
    FFreqSignal: PDAVSingleFixedArray;

    procedure AssignTo(Dest: TPersistent); override;
    procedure FFTOrderChanged; override;

    property Fft: TFftReal2ComplexNativeFloat32 read GetFft;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure CalculateCepstrum(const Signal: PDAVSingleFixedArray;
      const Cepstrum: PDAVComplexSingleFixedArray);
  published
    property FFTOrder;
    property FFTSize;
  end;

  TPowerCepstrum64 = class(TCustomCepstrum)
  private
    function GetFft: TFftReal2ComplexNativeFloat64;
  protected
    FSignalFreq: PDAVComplexDoubleFixedArray;
    FFreqSignal: PDAVDoubleFixedArray;

    procedure FFTOrderChanged; override;

    property Fft: TFftReal2ComplexNativeFloat64 read GetFft;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure CalculateCepstrum(const Signal: PDAVDoubleFixedArray;
      const Cepstrum: PDAVComplexDoubleFixedArray);
  published
    property FFTOrder;
  end;

implementation

uses
  SysUtils, DAV_Approximations;

{ TCustomCepstrum }

constructor TCustomCepstrum.Create;
begin
  inherited;
end;

destructor TCustomCepstrum.Destroy;
begin
  FreeAndNil(FFFT);
  inherited;
end;

procedure TCustomCepstrum.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomCepstrum then
    with TCustomCepstrum(Dest) do
    begin
      inherited;
      FFFT.Assign(Self.FFFT);
      FFFTSize := Self.FFFTSize;
      FFFTSizeHalf := Self.FFFTSizeHalf;
    end
  else
    inherited;
end;

procedure TCustomCepstrum.FFTOrderChanged;
begin
  FFFTSize := FFFT.FFTSize;
  FFFTSizeHalf := FFFTSize shr 1;
  FFFT.AutoScaleType := astDivideInvByN;
end;

function TCustomCepstrum.GetFftOrder: Byte;
begin
  Result := FFFT.Order;
end;

procedure TCustomCepstrum.SetFftOrder(const Value: Byte);
begin
  if FFFT.Order <> Value then
  begin
    FFFT.Order := Value;
    FFTOrderChanged;
  end;
end;

{ TPowerCepstrum32 }

constructor TPowerCepstrum32.Create;
begin
  inherited;

  FFFT := TFftReal2ComplexNativeFloat32.Create(6);
  FFFT.DataOrder := doPackedComplex;

  FFTOrderChanged;
end;

destructor TPowerCepstrum32.Destroy;
begin
  FreeAndNil(FFFT);
  inherited;
end;

procedure TPowerCepstrum32.AssignTo(Dest: TPersistent);
begin
  inherited;
  // yet todo!!!
end;

procedure TPowerCepstrum32.FFTOrderChanged;
begin
  inherited;

  ReallocMem(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplex32));
  FillChar(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplex32), 0);

  ReallocMem(FFreqSignal, FFFTSize * SizeOf(Single));
  FillChar(FFreqSignal, FFFTSize * SizeOf(Single), 0);
end;

function TPowerCepstrum32.GetFft: TFftReal2ComplexNativeFloat32;
begin
  Result := TFftReal2ComplexNativeFloat32(FFFT);
end;

procedure TPowerCepstrum32.CalculateCepstrum(const Signal: PDAVSingleFixedArray;
  const Cepstrum: PDAVComplexSingleFixedArray);
var
  i: Integer;
begin
  Fft.PerformFFT(FSignalFreq, Signal);
  FFreqSignal[0] := 2 * FastLog2MinError4(Abs(FSignalFreq[0].Re));
  for i := 1 to FFFTSizeHalf - 1 do
    FFreqSignal[i] := FastLog2MinError4(sqr(FSignalFreq[i].Re) +
      sqr(FSignalFreq[i].Im));
  FFreqSignal[FFFTSizeHalf] := 2 * FastLog2MinError4(Abs(FSignalFreq[0].Re));
  Fft.PerformFFT(Cepstrum, FFreqSignal);
end;

{ TPowerCepstrum64 }

constructor TPowerCepstrum64.Create;
begin
  inherited;

  FFFT := TFftReal2ComplexNativeFloat32.Create(6);
  FFFT.DataOrder := doPackedComplex;
  FFTOrderChanged;
end;

destructor TPowerCepstrum64.Destroy;
begin
  FreeAndNil(FFFT);

  inherited;
end;

procedure TPowerCepstrum64.FFTOrderChanged;
begin
  inherited;

  ReallocMem(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplex64));
  FillChar(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplex64), 0);

  ReallocMem(FFreqSignal, FFFTSize * SizeOf(Double));
  FillChar(FFreqSignal, FFFTSize * SizeOf(Double), 0);
end;

function TPowerCepstrum64.GetFft: TFftReal2ComplexNativeFloat64;
begin
  Result := TFftReal2ComplexNativeFloat64(FFFT);
end;

procedure TPowerCepstrum64.CalculateCepstrum(const Signal: PDAVDoubleFixedArray;
  const Cepstrum: PDAVComplexDoubleFixedArray);
var
  i: Integer;
begin
  Fft.PerformFFT(FSignalFreq, Signal);
  FFreqSignal[0] := 2 * FastLog2MinError4(Abs(FSignalFreq[0].Re));
  for i := 1 to FFFTSizeHalf - 1 do
    FFreqSignal[i] := FastLog2MinError4(sqr(FSignalFreq[i].Re) +
      sqr(FSignalFreq[i].Im));
  FFreqSignal[FFFTSizeHalf] := 2 * FastLog2MinError4(Abs(FSignalFreq[0].Re));
  Fft.PerformFFT(Cepstrum, FFreqSignal);
end;

end.
