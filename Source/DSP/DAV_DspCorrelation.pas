unit DAV_DspCorrelation;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2009-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, DAV_Types, DAV_Complex, DAV_Classes, DAV_DspFftReal2Complex
  {$IFDEF Use_IPPS}, DAV_DspFftReal2ComplexIPPS{$ENDIF}
  {$IFDEF Use_CUDA}, DAV_DspFftReal2ComplexCUDA{$ENDIF};

type
  TCustomCorrelation = class(TDspPersistent)
  private
    function GetFftOrder: Byte;
    procedure SetFftOrder(const Value: Byte);
  protected
    FFFT         : TFftReal2Complex;
    FFFTSize     : Integer;
    FFFTSizeHalf : Integer;
    procedure AssignTo(Dest: TPersistent); override;
    procedure ImpulseResponseChanged; virtual; abstract;
    procedure FFTOrderChanged; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property FFTOrder: Byte read GetFftOrder write SetFftOrder;
    property FFTSize: Integer read FFFTSize;
  end;

  TCorrelation32 = class(TCustomCorrelation)
  private
    {$IFDEF Use_IPPS}
    function GetFft : TFftReal2ComplexIPPSFloat32;
    {$ELSE} {$IFDEF Use_CUDA}
    function GetFft : TFftReal2ComplexCUDA32;
    {$ELSE}
    function GetFft : TFftReal2ComplexNativeFloat32;
    {$ENDIF}{$ENDIF}
  protected
    FSignalFreq      : PDAVComplexSingleFixedArray;
    FCorrelationFreq : PDAVComplexSingleFixedArray;

    procedure AssignTo(Dest: TPersistent); override;
    procedure FFTOrderChanged; override;

    {$IFDEF Use_IPPS}
    property Fft : TFftReal2ComplexIPPSFloat32 read GetFft;
    {$ELSE} {$IFDEF Use_CUDA}
    property Fft : TFftReal2ComplexCUDA32 read GetFft;
    {$ELSE}
    property Fft : TFftReal2ComplexNativeFloat32  read GetFft;
    {$ENDIF}{$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure CrossCorrelation(const SignalA, SignalB, Correlation: PDAVSingleFixedArray); overload; virtual;
    procedure CrossCorrelation(const Signal, SignalCorrelation: PDAVSingleFixedArray); overload; virtual;
    procedure AutoCorrelation(const Signal, Correlation: PDAVSingleFixedArray); overload; virtual;
    procedure AutoCorrelation(const SignalCorrelation: PDAVSingleFixedArray); overload; virtual;
  published
    property FFTOrder;
    property FFTSize;
  end;

  TCorrelation64 = class(TCustomCorrelation)
  private
    {$IFDEF Use_IPPS}
    function GetFft : TFftReal2ComplexIPPSFloat64;
    {$ELSE}
    function GetFft : TFftReal2ComplexNativeFloat64;
    {$ENDIF}
  protected
    FSignalFreq         : PDAVComplexDoubleFixedArray;
    FCorrelationFreq    : PDAVComplexDoubleFixedArray;

    procedure FFTOrderChanged; override;

    {$IFDEF Use_IPPS}
    property Fft : TFftReal2ComplexIPPSFloat64 read GetFft;
    {$ELSE}
    property Fft : TFftReal2ComplexNativeFloat64 read GetFft;
    {$ENDIF}
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure CrossCorrelation(const SignalA, SignalB, Correlation: PDAVDoubleFixedArray); overload; virtual;
    procedure CrossCorrelation(const Signal, SignalCorrelation: PDAVDoubleFixedArray); overload; virtual;
    procedure AutoCorrelation(const Signal, Correlation: PDAVDoubleFixedArray); overload; virtual;
    procedure AutoCorrelation(const SignalCorrelation: PDAVDoubleFixedArray); overload; virtual;
  published
    property FFTOrder;
  end;

implementation

uses
  Math, SysUtils;

resourcestring
  RCStrIRBlockOrderError = 'Maximum IR block order must be larger or equal ' +
    'the minimum IR block order!';

procedure MixBuffers_FPU(InBuffer: PSingle; MixBuffer: PSingle; SampleFrames: Integer); overload;
asm
@Start:
  fld   [eax + 4 * ecx - 4].Single
  fadd  [edx + 4 * ecx - 4].Single
  fstp  [edx + 4 * ecx - 4].Single
  loop @Start
end;

procedure MixBuffers_FPU(InBuffer: PDouble; MixBuffer: PDouble; SampleFrames: Integer); overload;
asm
@Start:
  fld   [eax + 8 * ecx - 8].Double
  fadd  [edx + 8 * ecx - 8].Double
  fstp  [edx + 8 * ecx - 8].Double
  loop @Start
end;

procedure ComplexMultiplyConjugated(const InplaceBuffer, Signal: PDAVComplexSingleFixedArray; const SampleFrames: Integer); overload;
asm
 // DC
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [eax].Single
 add eax, 4
 add edx, 4

 // Nyquist
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [eax].Single
 add eax, 4
 add edx, 4

 dec ecx
@Start:
  fld [eax    ].Single  // A.Re
  fld [eax + 4].Single  // A.Im, A.Re
  fld [edx    ].Single  // B.Re, A.Im, A.Re
  fld [edx + 4].Single  // B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  faddp                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp [eax    ].Single // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch st(2)            // A.Im, B.Re, B.Im, A.Re
  fmulp                 // A.Im * B.Re, B.Im, A.Re
  fxch st(2)            // B.Im, A.Re, A.Im * B.Re
  fmulp                 // B.Im * A.Re, A.Im * B.Re
  fsubp                 // A.Im * B.Re - A.Re * B.Im
  fstp [eax + 4].Single // A.Im := A.Im * B.Re + A.Re * B.Im
  add eax, 8
  add edx, 8
 loop @Start

 // Nyquist
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [eax].Single
end;

procedure ComplexMultiplyConjugated(const InBuffer, Signal: PDAVComplexSingleFixedArray; const SampleFrames: Integer;
  const OutBuffer: PDAVComplexSingleFixedArray); overload;
asm
 push ebx
 mov ebx, OutBuffer

 // DC
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [ebx].Single
 add eax, 4
 add ebx, 4
 add edx, 4

 // Nyquist
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [ebx].Single
 add eax, 4
 add ebx, 4
 add edx, 4

 dec ecx
@Start:
  fld [eax    ].Single  // A.Re
  fld [eax + 4].Single  // A.Im, A.Re
  fld [edx    ].Single  // B.Re, A.Im, A.Re
  fld [edx + 4].Single  // B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  faddp                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp [ebx    ].Single // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch st(2)            // A.Im, B.Re, B.Im, A.Re
  fmulp                 // A.Im * B.Re, B.Im, A.Re
  fxch st(2)            // B.Im, A.Re, A.Im * B.Re
  fmulp                 // B.Im * A.Re, A.Im * B.Re
  fsubp                 // A.Im * B.Re + A.Re * B.Im
  fstp [ebx + 4].Single // A.Im := A.Im * B.Re + A.Re * B.Im
  add eax, 8
  add ebx, 8
  add edx, 8
 loop @Start

 // Nyquist
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [ebx].Single

 pop ebx
end;

procedure ComplexMultiplyConjugated(InplaceBuffer: PDAVComplexDoubleFixedArray; Filter: PDAVComplexDoubleFixedArray; SampleFrames: Integer); overload;
asm
 // DC
 fld   [eax].Double
 fmul  [edx].Double
 fstp  [eax].Double
 add eax, 8
 add edx, 8

 // Nyquist
 fld   [eax].Double
 fmul  [edx].Double
 fstp  [eax].Double
 add eax, 8
 add edx, 8

 dec ecx
@Start:
  fld [eax    ].Double  // A.Re
  fld [eax + 8].Double  // A.Im, A.Re
  fld [edx    ].Double  // B.Re, A.Im, A.Re
  fld [edx + 8].Double  // B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  faddp                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp [eax    ].Double // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch st(2)            // A.Im, B.Re, B.Im, A.Re
  fmulp                 // A.Im * B.Re, B.Im, A.Re
  fxch st(2)            // B.Im, A.Re, A.Im * B.Re
  fmulp                 // B.Im * A.Re, A.Im * B.Re
  fsubp                 // A.Im * B.Re + A.Re * B.Im
  fstp [eax + 8].Double // A.Im := A.Im * B.Re + A.Re * B.Im
  add eax, 16
  add edx, 16
 loop @Start

 // Nyquist
 fld   [eax].Double
 fmul  [edx].Double
 fstp  [eax].Double
end;

procedure ComplexMultiplyConjugated(const InBuffer, Filter: PDAVComplexDoubleFixedArray; const SampleFrames: Integer;
  const OutBuffer: PDAVComplexDoubleFixedArray); overload;
asm
 push ebx
 mov ebx, OutBuffer

 // DC
 fld   [eax].Double
 fmul  [edx].Double
 fstp  [ebx].Double
 add eax, 8
 add ebx, 8
 add edx, 8

 // Nyquist
 fld   [eax].Double
 fmul  [edx].Double
 fstp  [ebx].Double
 add eax, 8
 add ebx, 8
 add edx, 8

 dec ecx
@Start:
  fld [eax    ].Double  // A.Re
  fld [eax + 8].Double  // A.Im, A.Re
  fld [edx    ].Double  // B.Re, A.Im, A.Re
  fld [edx + 8].Double  // B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  faddp                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp [ebx    ].Double // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch st(2)            // A.Im, B.Re, B.Im, A.Re
  fmulp                 // A.Im * B.Re, B.Im, A.Re
  fxch st(2)            // B.Im, A.Re, A.Im * B.Re
  fmulp                 // B.Im * A.Re, A.Im * B.Re
  fsubp                 // A.Im * B.Re + A.Re * B.Im
  fstp [ebx + 8].Double // A.Im := A.Im * B.Re + A.Re * B.Im
  add eax, 16
  add ebx, 16
  add edx, 16
 loop @Start

 // Nyquist
 fld   [eax].Double
 fmul  [edx].Double
 fstp  [ebx].Double

 pop ebx
end;

{ TCustomCorrelation }

procedure TCustomCorrelation.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomCorrelation then
  with TCustomCorrelation(Dest) do
   begin
    inherited;
    FFFT.Assign(Self.FFFT);
    FFFTSize     := Self.FFFTSize;
    FFFTSizeHalf := Self.FFFTSizeHalf;
   end
 else inherited;
end;

constructor TCustomCorrelation.Create;
begin
 inherited;
end;

destructor TCustomCorrelation.Destroy;
begin
 FreeAndNil(FFft);
 inherited;
end;

function TCustomCorrelation.GetFftOrder: Byte;
begin
 result := FFft.Order;
end;

procedure TCustomCorrelation.FFTOrderChanged;
begin
 FFFTSize     := FFft.FFTSize;
 FFFTSizeHalf := FFFTSize shr 1;
 FFft.AutoScaleType := astDivideInvByN;
end;

procedure TCustomCorrelation.SetFftOrder(const Value: Byte);
begin
 if FFft.Order <> Value then
  begin
   FFft.Order := Value;
   FFTOrderChanged;
  end;
end;

{ TCorrelation32 }

constructor TCorrelation32.Create;
begin
 inherited;

 FSignalFreq      := nil;
 FCorrelationFreq := nil;

 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat32.Create(6);
 {$ELSE} {$IFDEF Use_CUDA}
 FFft := TFftReal2ComplexCUDA32.Create(6);
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat32.Create(6);
 FFft.DataOrder := doPackedComplex;
 {$ENDIF}{$ENDIF}
 FFTOrderChanged;
end;

destructor TCorrelation32.Destroy;
begin
 Dispose(FSignalFreq);
 Dispose(FCorrelationFreq);
 FreeAndNil(FFft);
 inherited;
end;

procedure TCorrelation32.FFTOrderChanged;
begin
 inherited;

 ReallocMem(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplexSingle));
 ReallocMem(FCorrelationFreq, (FFFTSizeHalf + 1) * SizeOf(TComplexSingle));

 FillChar(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplexSingle), 0);
 FillChar(FCorrelationFreq, (FFFTSizeHalf + 1) * SizeOf(TComplexSingle), 0);
end;

{$IFDEF Use_IPPS}
function TCorrelation32.GetFft : TFftReal2ComplexIPPSFloat32;
begin
 result := TFftReal2ComplexIPPSFloat32(FFft);
end;

{$ELSE} {$IFDEF Use_CUDA}

function TCorrelation32.GetFft : TFftReal2ComplexCUDA32;
begin
 result := TFftReal2ComplexCUDA32(FFft);
end;

{$ELSE}

function TCorrelation32.GetFft : TFftReal2ComplexNativeFloat32;
begin
 result := TFftReal2ComplexNativeFloat32(FFft);
end;
{$ENDIF}{$ENDIF}

procedure TCorrelation32.AssignTo(Dest: TPersistent);
begin
 inherited;
 // yet todo!!!
end;

procedure TCorrelation32.AutoCorrelation(
  const SignalCorrelation: PDAVSingleFixedArray);
begin
 AutoCorrelation(SignalCorrelation, SignalCorrelation);
end;

procedure TCorrelation32.AutoCorrelation(const Signal,
  Correlation: PDAVSingleFixedArray);
begin
 FFft.PerformFFT(FCorrelationFreq, Signal);
 ComplexMultiplyConjugated(FCorrelationFreq, FCorrelationFreq, FFFTSizeHalf);
 FFft.PerformIFFT(FCorrelationFreq, Correlation);
end;

procedure TCorrelation32.CrossCorrelation(const Signal,
  SignalCorrelation: PDAVSingleFixedArray);
begin
 CrossCorrelation(Signal, Signal, SignalCorrelation);
end;

procedure TCorrelation32.CrossCorrelation(const SignalA, SignalB,
  Correlation: PDAVSingleFixedArray);
begin
 FFft.PerformFFT(FSignalFreq, SignalA);
 FFft.PerformFFT(FCorrelationFreq, SignalB);

 ComplexMultiplyConjugated(FCorrelationFreq, FSignalFreq, FFFTSizeHalf);

 FFft.PerformIFFT(FCorrelationFreq, Correlation);
end;


{ TCorrelation64 }

constructor TCorrelation64.Create;
begin
 inherited;

 FSignalFreq      := nil;
 FCorrelationFreq := nil;

 {$IFDEF Use_IPPS}
 FFft := TFftReal2ComplexIPPSFloat64.Create(6);
 {$ELSE}
 FFft := TFftReal2ComplexNativeFloat64.Create(6);
 FFft.DataOrder := doPackedComplex;
 {$ENDIF}
 FFTOrderChanged;
end;

destructor TCorrelation64.Destroy;
begin
 Dispose(FSignalFreq);
 Dispose(FCorrelationFreq);

 FreeAndNil(FFft);
 inherited;
end;

procedure TCorrelation64.FFTOrderChanged;
begin
 inherited;

 ReallocMem(FSignalFreq, (FFFTSizeHalf + 1) * SizeOf(TComplexDouble));
 ReallocMem(FCorrelationFreq, (FFFTSizeHalf + 1) * SizeOf(TComplexDouble));

 FillChar(FSignalFreq^[0], (FFFTSizeHalf + 1) * SizeOf(TComplexDouble), 0);
 FillChar(FCorrelationFreq^[0], (FFFTSizeHalf + 1) * SizeOf(TComplexDouble), 0);
end;

{$IFDEF Use_IPPS}
function TCorrelation64.GetFft : TFftReal2ComplexIPPSFloat64;
begin
 result := TFftReal2ComplexIPPSFloat64(FFft);
end;

{$ELSE}

function TCorrelation64.GetFft : TFftReal2ComplexNativeFloat64;
begin
 result := TFftReal2ComplexNativeFloat64(FFft);
end;
{$ENDIF}

procedure TCorrelation64.AutoCorrelation(
  const SignalCorrelation: PDAVDoubleFixedArray);
begin
 AutoCorrelation(SignalCorrelation, SignalCorrelation);
end;

procedure TCorrelation64.AutoCorrelation(const Signal,
  Correlation: PDAVDoubleFixedArray);
begin
 FFft.PerformFFT(FCorrelationFreq, Signal);
 ComplexMultiplyConjugated(FCorrelationFreq, FCorrelationFreq, FFFTSizeHalf);
 FFft.PerformIFFT(FCorrelationFreq, Correlation);
end;

procedure TCorrelation64.CrossCorrelation(const Signal,
  SignalCorrelation: PDAVDoubleFixedArray);
begin
 CrossCorrelation(Signal, Signal, SignalCorrelation);
end;

procedure TCorrelation64.CrossCorrelation(const SignalA, SignalB,
  Correlation: PDAVDoubleFixedArray);
begin
 FFft.PerformFFT(FSignalFreq, SignalA);
 FFft.PerformFFT(FCorrelationFreq, SignalB);

 ComplexMultiplyConjugated(FCorrelationFreq, FSignalFreq, FFFTSizeHalf);

 FFft.PerformIFFT(FCorrelationFreq, Correlation);
end;

end.
