unit DAV_BlockRoutines;

interface

{$I DAV_Compiler.INC}

uses
  DAV_Common, DAV_Complex;

procedure MixBuffers_FPU(InBuffer: PSingle; MixBuffer: PSingle; SampleFrames: Integer); overload;
procedure MixBuffers_FPU(InBuffer: PDouble; MixBuffer: PDouble; SampleFrames: Integer); overload;
procedure ComplexMultiply(const InplaceBuffer, Filter: PDAVComplexSingleFixedArray; const SampleFrames: Integer); overload;
procedure ComplexMultiply(const InBuffer, Filter: PDAVComplexSingleFixedArray;
  const SampleFrames: Integer; const OutBuffer: PDAVComplexSingleFixedArray); overload;
procedure ComplexMultiply(const InplaceBuffer, Filter: PDAVComplexDoubleFixedArray;
  const SampleFrames: Integer); overload;
procedure ComplexMultiply(const InBuffer, Filter: PDAVComplexDoubleFixedArray; const SampleFrames: Integer;
  const OutBuffer: PDAVComplexDoubleFixedArray); overload;

implementation

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

procedure ComplexMultiply(const InplaceBuffer, Filter: PDAVComplexSingleFixedArray; const SampleFrames: Integer); overload;
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
  fsubp                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp [eax    ].Single // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch st(2)            // A.Im, B.Re, B.Im, A.Re
  fmulp                 // A.Im * B.Re, B.Im, A.Re
  fxch st(2)            // B.Im, A.Re, A.Im * B.Re
  fmulp                 // B.Im * A.Re, A.Im * B.Re
  faddp                 // A.Im * B.Re + A.Re * B.Im
  fstp [eax + 4].Single // A.Im := A.Im * B.Re + A.Re * B.Im
  add eax, 8
  add edx, 8
 loop @Start

 // Nyquist
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [eax].Single
end;

procedure ComplexMultiply(const InBuffer, Filter: PDAVComplexSingleFixedArray;
  const SampleFrames: Integer; const OutBuffer: PDAVComplexSingleFixedArray); overload;
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
  fsubp                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp [ebx    ].Single // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch st(2)            // A.Im, B.Re, B.Im, A.Re
  fmulp                 // A.Im * B.Re, B.Im, A.Re
  fxch st(2)            // B.Im, A.Re, A.Im * B.Re
  fmulp                 // B.Im * A.Re, A.Im * B.Re
  faddp                 // A.Im * B.Re + A.Re * B.Im
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

procedure ComplexMultiply(const InplaceBuffer, Filter: PDAVComplexDoubleFixedArray;
  const SampleFrames: Integer); overload;
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
  fsubp                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp [eax    ].Double // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch st(2)            // A.Im, B.Re, B.Im, A.Re
  fmulp                 // A.Im * B.Re, B.Im, A.Re
  fxch st(2)            // B.Im, A.Re, A.Im * B.Re
  fmulp                 // B.Im * A.Re, A.Im * B.Re
  faddp                 // A.Im * B.Re + A.Re * B.Im
  fstp [eax + 8].Double // A.Im := A.Im * B.Re + A.Re * B.Im
  add eax, 16
  add edx, 16
 loop @Start

 // Nyquist
 fld   [eax].Double
 fmul  [edx].Double
 fstp  [eax].Double
end;

procedure ComplexMultiply(const InBuffer, Filter: PDAVComplexDoubleFixedArray; const SampleFrames: Integer;
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
  fsubp                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp [ebx    ].Double // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch st(2)            // A.Im, B.Re, B.Im, A.Re
  fmulp                 // A.Im * B.Re, B.Im, A.Re
  fxch st(2)            // B.Im, A.Re, A.Im * B.Re
  fmulp                 // B.Im * A.Re, A.Im * B.Re
  faddp                 // A.Im * B.Re + A.Re * B.Im
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


end.
