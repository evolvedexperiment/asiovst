unit DAV_BufferMathAsm;

(*

well, I did my best... but ASM is not my world...
--- the-real-myco ---

*)

interface

uses
  DAV_Common;


{-------------------------------------------------------------------------------------------
EVERYTHING FOR SINGLE
--------------------------------------------------------------------------------------------}

{TYPE: TAVDSingleDynArray}
procedure AddArrays(const input1, input2, output: TAVDSingleDynArray; const dim2: integer); overload;
procedure SubArrays(const from,   amount, output: TAVDSingleDynArray; const dim2: integer); overload;
procedure MulArrays(const input1, input2, output: TAVDSingleDynArray; const dim2: integer); overload;

procedure AddArrays(const input1: TAVDSingleDynArray; const input2: single; const output: TAVDSingleDynArray; const dim2: integer); overload;
procedure SubArrays(const from:   TAVDSingleDynArray; const amount: single; const output: TAVDSingleDynArray; const dim2: integer); overload;
procedure MulArrays(const input1: TAVDSingleDynArray; const input2: single; const output: TAVDSingleDynArray; const dim2: integer); overload;

procedure AddScaledArrays(const input1, input2: TAVDSingleDynArray; const factor1, factor2: single; const output: TAVDSingleDynArray; const dim2: integer); overload;
procedure AddModulatedArrays(const input1, input2, envelope1, envelope2: TAVDSingleDynArray; const output: TAVDSingleDynArray; const dim2: integer); overload;

procedure MulAddArrays(const factor1, factor2, summand, output: TAVDSingleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1: TAVDSingleDynArray; const factor2: single; const summand, output: TAVDSingleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1, factor2: TAVDSingleDynArray; const summand: single; const output: TAVDSingleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1: TAVDSingleDynArray; const factor2, summand: single; const output: TAVDSingleDynArray; const dim2: integer); overload;

procedure AddMulArrays(const summand1, summand2, factor, output: TAVDSingleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1: TAVDSingleDynArray; const summand2: single; const factor, output: TAVDSingleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1, summand2: TAVDSingleDynArray; const factor: single; const output: TAVDSingleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1: TAVDSingleDynArray; const summand2, factor: single; const output: TAVDSingleDynArray; const dim2: integer); overload;

procedure GetPeaks(const input: TAVDSingleDynArray; var outputmin, outputmax: Single; const dim2: integer); overload;
procedure GetSums(const input: TAVDSingleDynArray; var outputmin, outputmax: Single; const dim2: integer); overload;


{TYPE: TAVDArrayOfSingleDynArray}
procedure AddArrays(const input1, input2, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);  overload;
procedure SubArrays(const from,   amount, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);  overload;
procedure MulArrays(const input1, input2, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);  overload;

procedure AddArrays(const input1: TAVDArrayOfSingleDynArray; const input2:single;
                    const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure SubArrays(const from:   TAVDArrayOfSingleDynArray; const amount:single;
                    const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure MulArrays(const input1: TAVDArrayOfSingleDynArray; const input2:single;
                    const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;
                    

procedure MulAddArrays(const factor1, factor2, summand, output: TAVDArrayOfSingleDynArray;
                       const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1: TAVDArrayOfSingleDynArray; const factor2: single;
                       const summand, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1, factor2: TAVDArrayOfSingleDynArray; const summand: single;
                       const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1: TAVDArrayOfSingleDynArray; const factor2, summand: single;
                       const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;    

procedure AddMulArrays(const summand1, summand2, factor, output: TAVDArrayOfSingleDynArray;
                       const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1: TAVDArrayOfSingleDynArray; const summand2: single;
                       const factor, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1, summand2: TAVDArrayOfSingleDynArray; const factor: single;
                       const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1: TAVDArrayOfSingleDynArray; const summand2, factor: single;
                       const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;

procedure AddScaledArrays(const input1, input2: TAVDArrayOfSingleDynArray; const factor1, factor2: single; output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure AddModulatedArrays(const input1, input2, envelope1, envelope2, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;

procedure ClearArrays(const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure CopyArrays(const input, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;

// when output has no dimensions use this:
procedure CreateArrayCopy(const input: TAVDArrayOfSingleDynArray; out output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;
procedure CreateEmptyArray(out output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer); overload;

procedure GetPeaks(const input: TAVDArrayOfSingleDynArray; const outputmin, outputmax: TAVDSingleDynArray; const dim1, dim2: integer); overload;
procedure GetSums(const input: TAVDArrayOfSingleDynArray; const outputmin, outputmax: TAVDSingleDynArray; const dim1, dim2: integer); overload;


{-------------------------------------------------------------------------------------------
EVERYTHING AGAIN FOR DOUBLE
--------------------------------------------------------------------------------------------}

{TYPE: TAVDDoubleDynArray}
procedure AddArrays(const input1, input2, output: TAVDDoubleDynArray; const dim2: integer); overload;
procedure SubArrays(const from,   amount, output: TAVDDoubleDynArray; const dim2: integer); overload;
procedure MulArrays(const input1, input2, output: TAVDDoubleDynArray; const dim2: integer); overload;

procedure AddArrays(const input1: TAVDDoubleDynArray; const input2: Double; const output: TAVDDoubleDynArray; const dim2: integer); overload;
procedure SubArrays(const from:   TAVDDoubleDynArray; const amount: Double; const output: TAVDDoubleDynArray; const dim2: integer); overload;
procedure MulArrays(const input1: TAVDDoubleDynArray; const input2: Double; const output: TAVDDoubleDynArray; const dim2: integer); overload;

procedure AddScaledArrays(const input1, input2: TAVDDoubleDynArray; const factor1, factor2: Double; const output: TAVDDoubleDynArray; const dim2: integer); overload;
procedure AddModulatedArrays(const input1, input2, envelope1, envelope2: TAVDDoubleDynArray; const output: TAVDDoubleDynArray; const dim2: integer); overload;

procedure MulAddArrays(const factor1, factor2, summand, output: TAVDDoubleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1: TAVDDoubleDynArray; const factor2: Double; const summand, output: TAVDDoubleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1, factor2: TAVDDoubleDynArray; const summand: Double; const output: TAVDDoubleDynArray; const dim2: integer); overload;
procedure MulAddArrays(const factor1: TAVDDoubleDynArray; const factor2, summand: Double; const output: TAVDDoubleDynArray; const dim2: integer); overload;

procedure AddMulArrays(const summand1, summand2, factor, output: TAVDDoubleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1: TAVDDoubleDynArray; const summand2: Double; const factor, output: TAVDDoubleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1, summand2: TAVDDoubleDynArray; const factor: Double; const output: TAVDDoubleDynArray; const dim2: integer); overload;
procedure AddMulArrays(const summand1: TAVDDoubleDynArray; const summand2, factor: Double; const output: TAVDDoubleDynArray; const dim2: integer); overload;

procedure GetPeaks(const input: TAVDDoubleDynArray; var outputmin, outputmax: Double; const dim2: integer); overload;
procedure GetSums(const input: TAVDDoubleDynArray; var outputmin, outputmax: Double; const dim2: integer); overload;


{TYPE: TAVDArrayOfDoubleDynArray}
procedure AddArrays(const input1, input2, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);  overload;
procedure SubArrays(const from,   amount, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);  overload;
procedure MulArrays(const input1, input2, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);  overload;

procedure AddArrays(const input1: TAVDArrayOfDoubleDynArray; const input2:Double;
                    const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure SubArrays(const from:   TAVDArrayOfDoubleDynArray; const amount:Double;
                    const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure MulArrays(const input1: TAVDArrayOfDoubleDynArray; const input2:Double;
                    const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;


procedure MulAddArrays(const factor1, factor2, summand, output: TAVDArrayOfDoubleDynArray;
                       const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1: TAVDArrayOfDoubleDynArray; const factor2: Double;
                       const summand, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1, factor2: TAVDArrayOfDoubleDynArray; const summand: Double;
                       const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1: TAVDArrayOfDoubleDynArray; const factor2, summand: Double;
                       const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;

procedure AddMulArrays(const summand1, summand2, factor, output: TAVDArrayOfDoubleDynArray;
                       const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1: TAVDArrayOfDoubleDynArray; const summand2: Double;
                       const factor, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1, summand2: TAVDArrayOfDoubleDynArray; const factor: Double;
                       const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1: TAVDArrayOfDoubleDynArray; const summand2, factor: Double;
                       const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;

procedure AddScaledArrays(const input1, input2: TAVDArrayOfDoubleDynArray; const factor1, factor2: Double; output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure AddModulatedArrays(const input1, input2, envelope1, envelope2, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;


procedure ClearArrays(const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure CopyArrays(const input, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;

// when output has no dimensions use this:
procedure CreateArrayCopy(const input: TAVDArrayOfDoubleDynArray; out output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;
procedure CreateEmptyArray(out output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer); overload;

procedure GetPeaks(const input: TAVDArrayOfDoubleDynArray; const outputmin, outputmax: TAVDDoubleDynArray; const dim1, dim2: integer); overload;
procedure GetSums(const input: TAVDArrayOfDoubleDynArray; const outputmin, outputmax: TAVDDoubleDynArray; const dim1, dim2: integer); overload;

implementation


{-------------------------------------------------------------------------------------------
EVERYTHING FOR SINGLE
--------------------------------------------------------------------------------------------}


procedure AddArrays(const {eax}   input1,
                          {edx}   input2,
                          {ecx}   output: TAVDSingleDynArray;
                    const {stack} dim2: integer);
asm
  push ebx
  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, 4                  //    dec(dim2);
     fld  [input1 + ebx].Single  // \
     fadd [input2 + ebx].Single  //  > output= input1+input2;
     fstp [output + ebx].Single  // /

     jnz @start                  //    loop until dim2 = 0

  pop ebx
end;

procedure SubArrays(const {eax}   from,
                          {edx}   amount,
                          {ecx}   output: TAVDSingleDynArray;
                    const {stack} dim2: integer);
asm
  push ebx
  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, 4                  //    dec(dim2);
     fld  [from   + ebx].Single  // \
     fsub [amount + ebx].Single  //  > output:= from-amount;
     fstp [output + ebx].Single  // /

     jnz @start                  //    loop until dim2 = 0

  pop ebx
end;

procedure MulArrays(const {eax}   input1,
                          {edx}   input2,
                          {ecx}   output: TAVDSingleDynArray;
                    const {stack} dim2: integer);
asm
  push ebx
  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, 4                  //    dec(dim2);
     fld  [input1 + ebx].Single  // \
     fmul [input2 + ebx].Single  //  > output:= input1*input2;
     fstp [output + ebx].Single  // /

     jnz @start                  //    loop until dim2 = 0

  pop ebx
end;



procedure AddArrays(const {eax}   input1: TAVDSingleDynArray;
                    const {stack} input2: single;
                    const {edx}   output: TAVDSingleDynArray;
                    const {ecx}   dim2: integer);
asm
  shl dim2, 2
  @start:
     sub dim2, 4                  //    dec(dim2);
     fld  [input1 + dim2].Single  // \
     fadd [input2       ].Single  //  > output := input1 + input2;
     fstp [output + dim2].Single  // /

     jnz @start                   //    loop until dim2 = 0
end;

procedure SubArrays(const {eax}   from: TAVDSingleDynArray;
                    const {stack} amount: single;
                    const {edx}   output: TAVDSingleDynArray;
                    const {ecx}   dim2: integer);
asm
  shl dim2, 2
  @start:
     sub dim2, 4                  //    dec(dim2);
     fld  [from   + dim2].Single  // \
     fsub [amount       ].Single  //  > output := from - amount;
     fstp [output + dim2].Single  // /

     jnz @start                   //    loop until dim2 = 0
end;

procedure MulArrays(const {eax}   input1: TAVDSingleDynArray;
                    const {stack} input2: single;
                    const {edx}   output: TAVDSingleDynArray;
                    const {ecx}   dim2: integer);
asm
  shl dim2, 2
  @start:
     sub dim2, 4                  //    dec(dim2);
     fld  [input1 + dim2].Single  // \
     fmul [input2       ].Single  //  > output:= input1*input2;
     fstp [output + dim2].Single  // /

     jnz @start                   //    loop until dim2 = 0
end;




procedure MulAddArrays(const {eax}   factor1,
                             {edx}   factor2,
                             {ecx}   summand,
                             {stack} output: TAVDSingleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx
  push esi

  mov esi, output
  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, 4                   // dec(dim2);
     fld  [factor1 + ebx].Single  // \
     fmul [factor2 + ebx].Single  //  > x      = factor1*factor2
     fadd [summand + ebx].Single  //  > output = x+summand
     fstp [esi + ebx].Single      // /

     jnz @start                   //    loop until dim2 = 0
     
  pop esi
  pop ebx
end;

procedure MulAddArrays(const {eax}   factor1: TAVDSingleDynArray;
                       const {stack} factor2: single;
                       const {edx}   summand,
                             {ecx}   output: TAVDSingleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, 4                   // dec(dim2);
     fld  [factor1 + ebx].Single  // \
     fmul [factor2      ].Single  //  > x      = factor1*factor2
     fadd [summand + ebx].Single  //  > output = x+summand
     fstp [output  + ebx].Single  // /

     jnz @start                   //    loop until dim2 = 0

  pop ebx
end;

procedure MulAddArrays(const {eax}   factor1,
                             {edx}   factor2: TAVDSingleDynArray;
                       const {stack} summand: single;
                       const {ecx}   output: TAVDSingleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, 4                   // dec(dim2);
     fld  [factor1 + ebx].Single  // \
     fmul [factor2 + ebx].Single  //  > x      = factor1*factor2
     fadd [summand      ].Single  //  > output = x+summand
     fstp [output  + ebx].Single  // /

     jnz @start                   //    loop until dim2 = 0

  pop ebx
end;

procedure MulAddArrays(const {eax}   factor1: TAVDSingleDynArray;
                       const {stack} factor2,
                             {stack} summand: single;
                       const {edx}   output: TAVDSingleDynArray;
                       const {ecx}   dim2: integer);
asm
  shl dim2, 2

  @start:
     sub dim2, 4                   // dec(dim2);
     fld  [factor1 + dim2].Single  // \
     fmul [factor2       ].Single  //  > x      = factor1*factor2
     fadd [summand       ].Single  //  > output = x+summand
     fstp [output  + dim2].Single  // /

     jnz @start                //    loop until dim2 = 0
end;






procedure AddMulArrays(const {eax}   summand1,
                             {edx}   summand2,
                             {ecx}   factor,
                             {stack} output: TAVDSingleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx
  push esi

  mov esi, output
  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, 4                // dec(dim2);
     fld  [summand1+ebx].Single  // \
     fadd [summand2+ebx].Single  //  > x      = summand1+summand2
     fmul [factor+ebx].Single    //  > output = x*factor
     fstp [esi+ebx].Single       // /

     jnz @start                  //    loop until dim2 = 0
     
  pop esi
  pop ebx
end;

procedure AddMulArrays(const {eax}   summand1: TAVDSingleDynArray;
                       const {stack} summand2: single;
                       const {edx}   factor,
                             {ecx}   output: TAVDSingleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, 4                // dec(dim2);
     fld  [summand1+ebx].Single  // \
     fadd [summand2].Single      //  > x      = summand1+summand2
     fmul [factor+ebx].Single    //  > output = x*factor
     fstp [output+ebx].Single    // /

     jnz @start                  //    loop until dim2 = 0

  pop ebx
end;

procedure AddMulArrays(const {eax}   summand1,
                             {edx}   summand2: TAVDSingleDynArray;
                       const {stack} factor: single;
                       const {ecx}   output: TAVDSingleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, 4                // dec(dim2);
     fld  [summand1+ebx].Single  // \
     fadd [summand2+ebx].Single  //  > x      = summand1+summand2
     fmul [factor].Single        //  > output = x*factor
     fstp [output+ebx].Single    // /

     jnz @start                  //    loop until dim2 = 0

  pop ebx
end;

procedure AddMulArrays(const {eax}   summand1: TAVDSingleDynArray;
                       const {stack} summand2,
                             {stack} factor: single;
                       const {edx}   output: TAVDSingleDynArray;
                       const {ecx}   dim2: integer);
asm
  shl dim2, 2

  @start:
     sub dim2, 4                // dec(dim2);
     fld  [summand1+dim2].Single  // \
     fadd [summand2].Single       //  > x      = summand1*summand2
     fmul [factor].Single         //  > output = x+factor
     fstp [output+dim2].Single    // /

     jnz @start                   //    loop until dim2 = 0
end;




procedure AddScaledArrays(const {eax}   input1,
                                {edx}   input2: TAVDSingleDynArray;
                          const {stack} factor1,
                                {stack} factor2: single;
                          const {ecx}   output: TAVDSingleDynArray;
                          const {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, 4                // dec(dim2);
     fld  [input1+ebx].Single
     fmul [factor1].Single
     fld  [input2+ebx].Single
     fmul [factor2].Single
     faddp
     fstp [output+ebx].Single

     jnz @start                  //    loop until dim2 = 0

  pop ebx
end;


procedure AddModulatedArrays(const {eax}   input1,
                                   {edx}   input2,
                                   {ecx}   envelope1,
                                   {stack} envelope2: TAVDSingleDynArray;
                             const {stack} output: TAVDSingleDynArray;
                             const {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov edi, envelope2
  mov esi, output
  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, 4                // dec(dim2);
     fld  [input1+ebx].Single
     fmul [envelope1+ebx].Single
     fld  [input2+ebx].Single
     fmul [edi+ebx].Single
     faddp
     fstp [esi+ebx].Single

     jnz @start                  //    loop until dim2 = 0

  pop edi
  pop esi
  pop ebx
end;




procedure AddArrays(const {eax}   input1,
                          {edx}   input2,
                          {ecx}   output: TAVDArrayOfSingleDynArray;
                    const {stack} dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 2

  @outerloop:
    sub ebx, 4               // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov esi,[eax+ebx]
    mov edi,[ecx+ebx]
    push eax
    mov eax,[edx+ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 4             //    dec(dim2);
      fld  [esi+ebx].Single  // \
      fadd [eax+ebx].Single  //  > output:= input1+input2;
      fstp [edi+ebx].Single  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx
    pop eax


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;

procedure SubArrays(const {eax}   from,
                          {edx}   amount,
                          {ecx}   output: TAVDArrayOfSingleDynArray;
                    const {stack} dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 2
  
  @outerloop:
    sub ebx, 4               // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov esi,[eax+ebx]
    mov edi,[ecx+ebx]
    push eax
    mov eax,[edx+ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 4             //    dec(dim2);
      fld  [esi+ebx].Single  // \
      fsub [eax+ebx].Single  //  > output:= from-amount;
      fstp [edi+ebx].Single  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx
    pop eax


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;

procedure MulArrays(const {eax}   input1,
                          {edx}   input2,
                          {ecx}   output: TAVDArrayOfSingleDynArray;
                    const {stack} dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 2
  
  @outerloop:
    sub ebx, 4               // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov esi,[eax+ebx]
    mov edi,[ecx+ebx]
    push eax
    mov eax,[edx+ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 4             //    dec(dim2);
      fld  [esi+ebx].Single  // \
      fmul [eax+ebx].Single  //  > output:= input1*input2;
      fstp [edi+ebx].Single  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx
    pop eax


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;









procedure AddArrays(const {eax}   input1: TAVDArrayOfSingleDynArray;
                    const {stack} input2:single;
                    const {edx}   output: TAVDArrayOfSingleDynArray;
                    const {ecx}   dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 2

  @outerloop:
    sub ebx, 4               // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov esi,[eax+ebx]
    mov edi,[edx+ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 4             //    dec(dim2);
      fld  [esi+ebx].Single  // \
      fadd [input2].Single   //  > output:= input1+input2;
      fstp [edi+ebx].Single  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;

procedure SubArrays(const {eax}   from: TAVDArrayOfSingleDynArray;
                    const {stack} amount:single;
                    const {edx}   output: TAVDArrayOfSingleDynArray;
                    const {ecx}   dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 2

  @outerloop:
    sub ebx, 4               // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov esi,[eax+ebx]
    mov edi,[edx+ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 4             //    dec(dim2);
      fld  [esi+ebx].Single  // \
      fsub [amount].Single   //  > output:= from-amount;
      fstp [edi+ebx].Single  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;

procedure MulArrays(const {eax}   input1: TAVDArrayOfSingleDynArray;
                    const {stack} input2:single;
                    const {edx}   output: TAVDArrayOfSingleDynArray;
                    const {ecx}   dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 2

  @outerloop:
    sub ebx, 4               // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov esi,[eax+ebx]
    mov edi,[edx+ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 4             //    dec(dim2);
      fld  [esi+ebx].Single  // \
      fmul [input2].Single   //  > output:= input1*input2;
      fstp [edi+ebx].Single  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;






procedure MulAddArrays(const factor1, factor2, summand, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do MulAddArrays(factor1[i], factor2[i], summand[i], output[i], dim2);
end;

procedure MulAddArrays(const factor1: TAVDArrayOfSingleDynArray; const factor2: single;
                       const summand, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do MulAddArrays(factor1[i], factor2, summand[i], output[i], dim2);
end;

procedure MulAddArrays(const factor1, factor2: TAVDArrayOfSingleDynArray; const summand: single;
                       const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do MulAddArrays(factor1[i], factor2[i], summand, output[i], dim2);
end;

procedure MulAddArrays(const factor1: TAVDArrayOfSingleDynArray; const factor2, summand: single;
                       const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do MulAddArrays(factor1[i], factor2, summand, output[i], dim2);
end;






procedure AddMulArrays(const summand1, summand2, factor, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddMulArrays(summand1[i], summand2[i], factor[i], output[i], dim2);
end;

procedure AddMulArrays(const summand1: TAVDArrayOfSingleDynArray; const summand2: single; const factor, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddMulArrays(summand1[i], summand2, factor[i], output[i], dim2);
end;

procedure AddMulArrays(const summand1, summand2: TAVDArrayOfSingleDynArray; const factor: single; const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddMulArrays(summand1[i], summand2[i], factor, output[i], dim2);
end;

procedure AddMulArrays(const summand1: TAVDArrayOfSingleDynArray; const summand2, factor: single; const output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddMulArrays(summand1[i], summand2, factor, output[i], dim2);
end;






procedure AddScaledArrays(const input1, input2: TAVDArrayOfSingleDynArray; const factor1, factor2: single; output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddScaledArrays(input1[i], input2[i], factor1, factor2, output[i], dim2);
end;




procedure AddModulatedArrays(const input1, input2, envelope1, envelope2, output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddModulatedArrays(input1[i], input2[i], envelope1[i], envelope2[i], output[i], dim2);
end;



procedure ClearArrays(const {eax} output: TAVDArrayOfSingleDynArray;
                      const {edx} dim1,
                            {ecx} dim2: integer);
asm
  push ebx
  push edi
  push ebp

  shl dim1, 2
  mov ebx, dim2
  mov ebp, eax
  mov eax, 0

  @outerloop:
    sub dim1, 4              // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov edi, [ebp+dim1]
    mov ecx, ebx
    rep stosd

  jmp @outerloop

  @cleanup:
    pop ebp
    pop edi
    pop ebx
end;


procedure CopyArrays(const {eax}   input,
                           {edx}   output: TAVDArrayOfSingleDynArray;
                     const {ecx}   dim1,
                           {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi
  push ebp

  shl dim1, 2
  mov ebx, dim1

  @outerloop:
    sub ebx, 4              // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov ecx, dim2
    mov esi, [eax+ebx]
    mov edi, [edx+ebx]
    rep movsd

  jmp @outerloop

  @cleanup:
    pop ebp
    pop edi
    pop esi
    pop ebx
end;

procedure CreateArrayCopy(const input: TAVDArrayOfSingleDynArray; out output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
begin
  SetLength(output, dim1, dim2);
  CopyArrays(input, output, dim1, dim2);
end;

procedure CreateEmptyArray(out output: TAVDArrayOfSingleDynArray; const dim1, dim2: integer);
begin
  SetLength(output, dim1, dim2);
  ClearArrays(output, dim1, dim2);
end;     








{-------------------------------------------------------------------------------------------
EVERYTHING AGAIN FOR DOUBLE
--------------------------------------------------------------------------------------------}


procedure AddArrays(const {eax}   input1,
                          {edx}   input2,
                          {ecx}   output: TAVDDoubleDynArray;
                    const {stack} dim2: integer);
asm
  push ebx
  mov ebx,dim2
  shl ebx, 3

  @start:
     sub ebx, 8                //    dec(dim2);
     fld  [input1+ebx].Double  // \
     fadd [input2+ebx].Double  //  > output= input1+input2;
     fstp [output+ebx].Double  // /

     jnz @start                //    loop until dim2 = 0

  pop ebx
end;

procedure SubArrays(const {eax}   from,
                          {edx}   amount,
                          {ecx}   output: TAVDDoubleDynArray;
                    const {stack} dim2: integer);
asm
  push ebx
  mov ebx,dim2
  shl ebx, 3

  @start:
     sub ebx, 8                //    dec(dim2);
     fld  [from  +ebx].Double  // \
     fsub [amount+ebx].Double  //  > output:= from-amount;
     fstp [output+ebx].Double  // /

     jnz @start                //    loop until dim2 = 0

  pop ebx
end;

procedure MulArrays(const {eax}   input1,
                          {edx}   input2,
                          {ecx}   output: TAVDDoubleDynArray;
                    const {stack} dim2: integer);
asm
  push ebx
  mov ebx,dim2
  shl ebx, 3

  @start:
     sub ebx, 8                //    dec(dim2);
     fld  [input1+ebx].Double  // \
     fmul [input2+ebx].Double  //  > output:= input1*input2;
     fstp [output+ebx].Double  // /

     jnz @start                //    loop until dim2 = 0

  pop ebx
end;






procedure AddArrays(const {eax}   input1: TAVDDoubleDynArray;
                    const {stack} input2: Double;
                    const {edx}   output: TAVDDoubleDynArray;
                    const {ecx}   dim2: integer);
asm
  shl dim2, 3
  @start:
     sub dim2, 8               //    dec(dim2);
     fld  [input1+dim2].Double  // \
     fadd [input2].Double      //  > output:= input1+input2;
     fstp [output+dim2].Double  // /

     jnz @start                //    loop until dim2 = 0
end;

procedure SubArrays(const {eax}   from: TAVDDoubleDynArray;
                    const {stack} amount: Double;
                    const {edx}   output: TAVDDoubleDynArray;
                    const {ecx}   dim2: integer);
asm
  shl dim2, 3
  @start:
     sub dim2, 8               //    dec(dim2);
     fld  [from+dim2].Double  // \
     fsub [amount].Double       //  > output:= from-amount;
     fstp [output+dim2].Double  // /

     jnz @start                //    loop until dim2 = 0
end;

procedure MulArrays(const {eax}   input1: TAVDDoubleDynArray;
                    const {stack} input2: Double;
                    const {edx}   output: TAVDDoubleDynArray;
                    const {ecx}   dim2: integer);
asm
  shl dim2, 3
  @start:
     sub dim2, 8               //    dec(dim2);
     fld  [input1+dim2].Double  // \
     fmul [input2].Double      //  > output:= input1*input2;
     fstp [output+dim2].Double  // /

     jnz @start                //    loop until dim2 = 0
end;




procedure MulAddArrays(const {eax}   factor1,
                             {edx}   factor2,
                             {ecx}   summand,
                             {stack} output: TAVDDoubleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx
  push esi

  mov esi, output
  mov ebx,dim2
  shl ebx, 3

  @start:
     sub ebx, 8               // dec(dim2);
     fld  [factor1+ebx].Double  // \
     fmul [factor2+ebx].Double  //  > x      = factor1*factor2
     fadd [summand+ebx].Double  //  > output = x+summand
     fstp [esi+ebx].Double      // /

     jnz @start                //    loop until dim2 = 0
     
  pop esi
  pop ebx
end;

procedure MulAddArrays(const {eax}   factor1: TAVDDoubleDynArray;
                       const {stack} factor2: Double;
                       const {edx}   summand,
                             {ecx}   output: TAVDDoubleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 3

  @start:
     sub ebx, 8               // dec(dim2);
     fld  [factor1+ebx].Double  // \
     fmul [factor2].Double      //  > x      = factor1*factor2
     fadd [summand+ebx].Double  //  > output = x+summand
     fstp [output+ebx].Double   // /

     jnz @start                //    loop until dim2 = 0

  pop ebx
end;

procedure MulAddArrays(const {eax}   factor1,
                             {edx}   factor2: TAVDDoubleDynArray;
                       const {stack} summand: Double;
                       const {ecx}   output: TAVDDoubleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 3

  @start:
     sub ebx, 8               // dec(dim2);
     fld  [factor1+ebx].Double  // \
     fmul [factor2+ebx].Double  //  > x      = factor1*factor2
     fadd [summand].Double      //  > output = x+summand
     fstp [output+ebx].Double   // /

     jnz @start                //    loop until dim2 = 0

  pop ebx
end;

procedure MulAddArrays(const {eax}   factor1: TAVDDoubleDynArray;
                       const {stack} factor2,
                             {stack} summand: Double;
                       const {edx}   output: TAVDDoubleDynArray;
                       const {ecx}   dim2: integer);
asm
  shl dim2, 3

  @start:
     sub dim2, 8               // dec(dim2);
     fld  [factor1+dim2].Double  // \
     fmul [factor2].Double       //  > x      = factor1*factor2
     fadd [summand].Double       //  > output = x+summand
     fstp [output+dim2].Double   // /

     jnz @start                //    loop until dim2 = 0
end;






procedure AddMulArrays(const {eax}   summand1,
                             {edx}   summand2,
                             {ecx}   factor,
                             {stack} output: TAVDDoubleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx
  push esi

  mov esi, output
  mov ebx,dim2
  shl ebx, 3

  @start:
     sub ebx, 8                // dec(dim2);
     fld  [summand1+ebx].Double  // \
     fadd [summand2+ebx].Double  //  > x      = summand1+summand2
     fmul [factor+ebx].Double    //  > output = x*factor
     fstp [esi+ebx].Double       // /

     jnz @start                  //    loop until dim2 = 0
     
  pop esi
  pop ebx
end;

procedure AddMulArrays(const {eax}   summand1: TAVDDoubleDynArray;
                       const {stack} summand2: Double;
                       const {edx}   factor,
                             {ecx}   output: TAVDDoubleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 3

  @start:
     sub ebx, 8                // dec(dim2);
     fld  [summand1+ebx].Double  // \
     fadd [summand2].Double      //  > x      = summand1+summand2
     fmul [factor+ebx].Double    //  > output = x*factor
     fstp [output+ebx].Double    // /

     jnz @start                  //    loop until dim2 = 0

  pop ebx
end;

procedure AddMulArrays(const {eax}   summand1,
                             {edx}   summand2: TAVDDoubleDynArray;
                       const {stack} factor: Double;
                       const {ecx}   output: TAVDDoubleDynArray;
                       const {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 3

  @start:
     sub ebx, 8                // dec(dim2);
     fld  [summand1+ebx].Double  // \
     fadd [summand2+ebx].Double  //  > x      = summand1+summand2
     fmul [factor].Double        //  > output = x*factor
     fstp [output+ebx].Double    // /

     jnz @start                  //    loop until dim2 = 0

  pop ebx
end;

procedure AddMulArrays(const {eax}   summand1: TAVDDoubleDynArray;
                       const {stack} summand2,
                             {stack} factor: Double;
                       const {edx}   output: TAVDDoubleDynArray;
                       const {ecx}   dim2: integer);
asm
  shl dim2, 3

  @start:
     sub dim2, 8                // dec(dim2);
     fld  [summand1+dim2].Double  // \
     fadd [summand2].Double       //  > x      = summand1*summand2
     fmul [factor].Double         //  > output = x+factor
     fstp [output+dim2].Double    // /

     jnz @start                   //    loop until dim2 = 0
end;




procedure AddScaledArrays(const {eax}   input1,
                                {edx}   input2: TAVDDoubleDynArray;
                          const {stack} factor1,
                                {stack} factor2: Double;
                          const {ecx}   output: TAVDDoubleDynArray;
                          const {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 3

  @start:
     sub ebx, 8                // dec(dim2);
     fld  [input1+ebx].Double
     fmul [factor1].Double
     fld  [input2+ebx].Double
     fmul [factor2].Double
     faddp
     fstp [output+ebx].Double

     jnz @start                  //    loop until dim2 = 0

  pop ebx
end;


procedure AddModulatedArrays(const {eax}   input1,
                                   {edx}   input2,
                                   {ecx}   envelope1,
                                   {stack} envelope2: TAVDDoubleDynArray;
                             const {stack} output: TAVDDoubleDynArray;
                             const {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov edi, envelope2
  mov esi, output
  mov ebx,dim2
  shl ebx, 3

  @start:
     sub ebx, 8                // dec(dim2);
     fld  [input1+ebx].Double
     fmul [envelope1+ebx].Double
     fld  [input2+ebx].Double
     fmul [edi+ebx].Double
     faddp
     fstp [esi+ebx].Double

     jnz @start                  //    loop until dim2 = 0

  pop edi
  pop esi
  pop ebx
end;




procedure AddArrays(const {eax}   input1,
                          {edx}   input2,
                          {ecx}   output: TAVDArrayOfDoubleDynArray;
                    const {stack} dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 3

  @outerloop:
    sub ebx, 4               // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov esi,[eax+ebx]
    mov edi,[ecx+ebx]
    push eax
    mov eax,[edx+ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 8             //    dec(dim2);
      fld  [esi+ebx].Double  // \
      fadd [eax+ebx].Double  //  > output:= input1+input2;
      fstp [edi+ebx].Double  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx
    pop eax


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;

procedure SubArrays(const {eax}   from,
                          {edx}   amount,
                          {ecx}   output: TAVDArrayOfDoubleDynArray;
                    const {stack} dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 3

  @outerloop:
    sub ebx, 4               // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov esi,[eax+ebx]
    mov edi,[ecx+ebx]
    push eax
    mov eax,[edx+ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 8             //    dec(dim2);
      fld  [esi+ebx].Double  // \
      fsub [eax+ebx].Double  //  > output:= from-amount;
      fstp [edi+ebx].Double  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx
    pop eax


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;

procedure MulArrays(const {eax}   input1,
                          {edx}   input2,
                          {ecx}   output: TAVDArrayOfDoubleDynArray;
                    const {stack} dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 3
  
  @outerloop:
    sub ebx, 4               // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov esi,[eax+ebx]
    mov edi,[ecx+ebx]
    push eax
    mov eax,[edx+ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 8             //    dec(dim2);
      fld  [esi+ebx].Double  // \
      fmul [eax+ebx].Double  //  > output:= input1*input2;
      fstp [edi+ebx].Double  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx
    pop eax


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;









procedure AddArrays(const {eax}   input1: TAVDArrayOfDoubleDynArray;
                    const {stack} input2:Double;
                    const {edx}   output: TAVDArrayOfDoubleDynArray;
                    const {ecx}   dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 3

  @outerloop:
    sub ebx, 4               // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov esi,[eax+ebx]
    mov edi,[edx+ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 8             //    dec(dim2);
      fld  [esi+ebx].Double  // \
      fadd [input2].Double   //  > output:= input1+input2;
      fstp [edi+ebx].Double  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;

procedure SubArrays(const {eax}   from: TAVDArrayOfDoubleDynArray;
                    const {stack} amount:Double;
                    const {edx}   output: TAVDArrayOfDoubleDynArray;
                    const {ecx}   dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 3

  @outerloop:
    sub ebx, 4                 // dec(dim1);
    jc @cleanup                // if dim1 < 0 then exit;

    mov esi,[eax + ebx]
    mov edi,[edx + ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 8               //    dec(dim2);
      fld  [esi + ebx].Double  // \
      fsub [amount   ].Double  //  > output:= from-amount;
      fstp [edi + ebx].Double  // /

      jnz @innerloop           // loop until dim2 = 0

    pop ebx


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;

procedure MulArrays(const {eax}   input1: TAVDArrayOfDoubleDynArray;
                    const {stack} input2:Double;
                    const {edx}   output: TAVDArrayOfDoubleDynArray;
                    const {ecx}   dim1,
                          {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov ebx, dim1
  shl ebx, 2
  shl dim2, 3

  @outerloop:
    sub ebx, 4                 // dec(dim1);
    jc @cleanup                // if dim1 < 0 then exit;

    mov esi, [eax + ebx]
    mov edi, [edx + ebx]
    push ebx

    mov ebx, dim2
    @innerloop:
      sub ebx, 8               //    dec(dim2);
      fld  [esi + ebx].Double  // \
      fmul [input2   ].Double  //  > output:= input1*input2;
      fstp [edi + ebx].Double  // /

      jnz @innerloop           // loop until dim2 = 0

    pop ebx


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;






procedure MulAddArrays(const factor1, factor2, summand, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do MulAddArrays(factor1[i], factor2[i], summand[i], output[i], dim2);
end;

procedure MulAddArrays(const factor1: TAVDArrayOfDoubleDynArray; const factor2: Double;
                       const summand, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do MulAddArrays(factor1[i], factor2, summand[i], output[i], dim2);
end;

procedure MulAddArrays(const factor1, factor2: TAVDArrayOfDoubleDynArray; const summand: Double;
                       const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do MulAddArrays(factor1[i], factor2[i], summand, output[i], dim2);
end;

procedure MulAddArrays(const factor1: TAVDArrayOfDoubleDynArray; const factor2, summand: Double;
                       const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do MulAddArrays(factor1[i], factor2, summand, output[i], dim2);
end;






procedure AddMulArrays(const summand1, summand2, factor, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddMulArrays(summand1[i], summand2[i], factor[i], output[i], dim2);
end;

procedure AddMulArrays(const summand1: TAVDArrayOfDoubleDynArray; const summand2: Double; const factor, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddMulArrays(summand1[i], summand2, factor[i], output[i], dim2);
end;

procedure AddMulArrays(const summand1, summand2: TAVDArrayOfDoubleDynArray; const factor: Double; const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddMulArrays(summand1[i], summand2[i], factor, output[i], dim2);
end;

procedure AddMulArrays(const summand1: TAVDArrayOfDoubleDynArray; const summand2, factor: Double; const output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddMulArrays(summand1[i], summand2, factor, output[i], dim2);
end;






procedure AddScaledArrays(const input1, input2: TAVDArrayOfDoubleDynArray; const factor1, factor2: Double; output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddScaledArrays(input1[i], input2[i], factor1, factor2, output[i], dim2);
end;




procedure AddModulatedArrays(const input1, input2, envelope1, envelope2, output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
var i: integer;
begin
  for i := 0 to dim1 - 1 do AddModulatedArrays(input1[i], input2[i], envelope1[i], envelope2[i], output[i], dim2);
end;



procedure ClearArrays(const {eax} output: TAVDArrayOfDoubleDynArray;
                      const {edx} dim1,
                            {ecx} dim2: integer);
asm
  push ebx
  push edi
  push ebp

  shl dim1, 2
  shl dim2, 1
  mov ebx, dim2
  mov ebp, eax
  mov eax, 0

  @outerloop:
    sub dim1, 4              // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov edi, [ebp+dim1]
    mov ecx, ebx
    rep stosd

  jmp @outerloop

  @cleanup:
    pop ebp
    pop edi
    pop ebx
end;


procedure CopyArrays(const {eax}   input,
                           {edx}   output: TAVDArrayOfDoubleDynArray;
                     const {ecx}   dim1,
                           {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi
  push ebp

  shl dim1, 2
  mov ebx, dim1
  shl dim2, 1

  @outerloop:
    sub ebx, 4              // dec(dim1);
    jc @cleanup              // if dim1 < 0 then exit;

    mov ecx, dim2
    mov esi, [eax+ebx]
    mov edi, [edx+ebx]
    rep movsd

  jmp @outerloop

  @cleanup:
    pop ebp
    pop edi
    pop esi
    pop ebx
end;

procedure CreateArrayCopy(const input: TAVDArrayOfDoubleDynArray; out output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
begin
  SetLength(output, dim1, dim2);
  CopyArrays(input, output, dim1, dim2);
end;

procedure CreateEmptyArray(out output: TAVDArrayOfDoubleDynArray; const dim1, dim2: integer);
begin
  SetLength(output, dim1, dim2);
  ClearArrays(output, dim1, dim2);
end;


procedure GetPeaks(const {eax}   input: TAVDSingleDynArray;
                     var {edx}   outputmin,
                         {ecx}   outputmax: Single;
                   const {stack} dim2: integer);
asm
  push ebx
  push edi
  push esi


  mov esi, input

  shl dim2, 2
  mov ebx, dim2

  mov edi,[input]
  mov [outputmin], edi
  mov [outputmax], edi

  @innerloop:
    sub ebx, 4
    jc @cleanup

    fld [esi+ebx].Single
    fcom [outputmin].Single
    fnstsw ax
    sahf
    jnb @nostoremin
      fstp st
      fld  [esi+ebx].Single
      fstp [outputmin].single
      jmp @innerloop

  @nostoremin:
    fcomp [outputmax].Single
    fnstsw ax
    sahf
    jbe @innerloop
      fld  [esi+ebx].Single
      fstp [outputmax].single

  jmp @innerloop

  @cleanup:
    pop esi
    pop edi
    pop ebx
{
  push ebx
  push edi
  push esi


  mov esi, input

  shl dim2, 2
  mov ebx, dim2

  fld [esi].Single
  fld [esi].Single

  @innerloop:
    sub ebx, 4
    jz @cleanup

    fld [esi+ebx].Single
    fcom st(2)
    fnstsw ax
    sahf

    jbe @nostoremin
    fstp st(2)
    jmp @innerloop

    @nostoremin:
      fcom st(1)
      fnstsw ax
      sahf
      jnb @removefromst
        fstp st(1)
        jmp @innerloop

      @removefromst:
        fstp st
        jmp @innerloop

  @cleanup:
    fstp [outputmin].Single
    fstp [outputmax].Single
    pop esi
    pop edi
    pop ebx }
end;

procedure GetPeaks(const {eax}   input: TAVDDoubleDynArray;
                     var {edx}   outputmin,
                         {ecx}   outputmax: Double;
                   const {stack} dim2: integer);
asm
  push ebx
  push edi
  push esi


  mov esi, input

  shl dim2, 3
  mov ebx, dim2

  fld [esi].Double
  fld [esi].Double

  @innerloop:
    sub ebx, 8
    jz @cleanup

    fld [esi+ebx].Double
    fcom st(2)
    fnstsw ax
    sahf

    jbe @nostoremin
    fstp st(2)
    jmp @innerloop

    @nostoremin:
      fcom st(1)
      fnstsw ax
      sahf
      jnb @removefromst
        fstp st(1)
        jmp @innerloop

      @removefromst:
        fstp st
        jmp @innerloop

  @cleanup:
    fstp [outputmin].Double
    fstp [outputmax].Double
    pop esi
    pop edi
    pop ebx 
end;


procedure GetSums(const {eax}   input: TAVDSingleDynArray;
                    var {edx}   outputmin,
                        {ecx}   outputmax: Single;
                  const {stack} dim2: integer);
asm
  push ebx
  push edi
  push esi
  push eax

  mov esi, input

  shl dim2, 2
  mov ebx, dim2

  fldz
  fldz
  fldz

  @innerloop:
    sub ebx, 4              // dec(dim2);
    jc @cleanup

    fld [esi + ebx].Single
    fcom st(3)
    fnstsw ax
    sahf
    jnb @nostoremin
      faddp st(1),st
      jmp @innerloop

    @nostoremin:
      faddp st(2), st
      jmp @innerloop


  @cleanup:
    fstp [outputmin].Single
    fstp [outputmax].Single
    fstp [esi].Single
    pop eax
    pop esi
    pop edi
    pop ebx
end;



procedure GetSums(const {eax}   input: TAVDDoubleDynArray;
                    var {edx}   outputmin,
                        {ecx}   outputmax: Double;
                  const {stack} dim2: integer);
asm
  push ebx
  push edi
  push esi
  push eax

  mov esi, input

  shl dim2, 3
  mov ebx, dim2

  fldz
  fldz
  fldz

  @innerloop:
    sub ebx, 8              // dec(dim2);
    jc @cleanup

    fld [esi + ebx].Double
    fcom st(3)
    fnstsw ax
    sahf
    jnb @nostoremin
      faddp st(1),st
      jmp @innerloop

    @nostoremin:
      faddp st(2), st
      jmp @innerloop

  
  @cleanup:
    fstp [outputmin].Double
    fstp [outputmax].Double
    fstp [esi].Double
    pop eax
    pop esi
    pop edi
    pop ebx
end;

procedure GetPeaks(const input: TAVDArrayOfSingleDynArray; const outputmin, outputmax: TAVDSingleDynArray; const dim1, dim2: integer);
var
  i: integer;
begin
  for i := 0 to dim1 - 1 do
    GetPeaks(input[i], outputmin[i], outputmax[i], dim2);
end;

procedure GetSums(const input: TAVDArrayOfSingleDynArray; const outputmin, outputmax: TAVDSingleDynArray; const dim1, dim2: integer);
var
  i: integer;
begin
  for i := 0 to dim1 - 1 do
    GetSums(input[i], outputmin[i], outputmax[i], dim2);
end;

procedure GetPeaks(const input: TAVDArrayOfDoubleDynArray; const outputmin, outputmax: TAVDDoubleDynArray; const dim1, dim2: integer);
var
  i: integer;
begin
  for i := 0 to dim1 - 1 do
    GetPeaks(input[i], outputmin[i], outputmax[i], dim2);
end;

procedure GetSums(const input: TAVDArrayOfDoubleDynArray; const outputmin, outputmax: TAVDDoubleDynArray; const dim1, dim2: integer);
var
  i: integer;
begin
  for i := 0 to dim1 - 1 do
    GetSums(input[i], outputmin[i], outputmax[i], dim2);
end;


end.
