unit DAVDBufferMathAsm;

(*

well, I did my best... but ASM is not my world...
--- the-real-myco ---

*)

interface

uses DAVDCommon;

procedure AddArrays(const input1, input2, output: PSingle; dim2: integer); overload;
procedure SubArrays(const from,   amount, output: PSingle; dim2: integer); overload;
procedure MulArrays(const input1, input2, output: PSingle; dim2: integer); overload;

procedure AddArrays(const input1: PSingle; const input2: single; const output: PSingle; dim2: integer); overload;
procedure SubArrays(const from:   PSingle; const amount: single; const output: PSingle; dim2: integer); overload;
procedure MulArrays(const input1: PSingle; const input2: single; const output: PSingle; dim2: integer); overload;

procedure AddArrays(const input1, input2, output: TArrayOfSingleDynArray; dim1, dim2: integer);  overload;
procedure SubArrays(const from,   amount, output: TArrayOfSingleDynArray; dim1, dim2: integer);  overload;
procedure MulArrays(const input1, input2, output: TArrayOfSingleDynArray; dim1, dim2: integer);  overload;

procedure AddScaledArrays(const input1, input2: PSingle; const factor1, factor2: single; const output: PSingle; dim2: integer); overload;
procedure AddModulatedArrays(const input1, input2, envelope1, envelope2: PSingle; const output: PSingle; dim2: integer); overload;

procedure MulAddArrays(const factor1, factor2, summand, output: PSingle; dim2: integer); overload;
procedure MulAddArrays(const factor1: PSingle; const factor2: single; const summand, output: PSingle; dim2: integer); overload;
procedure MulAddArrays(const factor1, factor2: PSingle; const summand: single; const output: PSingle; dim2: integer); overload;
procedure MulAddArrays(const factor1: PSingle; const factor2, summand: single; const output: PSingle; dim2: integer); overload;

procedure AddMulArrays(const summand1, summand2, factor, output: PSingle; dim2: integer); overload;
procedure AddMulArrays(const summand1: PSingle; const summand2: single; const factor, output: PSingle; dim2: integer); overload;
procedure AddMulArrays(const summand1, summand2: PSingle; const factor: single; const output: PSingle; dim2: integer); overload;
procedure AddMulArrays(const summand1: PSingle; const summand2, factor: single; const output: PSingle; dim2: integer); overload;

procedure AddArrays(const input1: TArrayOfSingleDynArray; const input2:single;
                    const output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;
procedure SubArrays(const from:   TArrayOfSingleDynArray; const amount:single;
                    const output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;
procedure MulArrays(const input1: TArrayOfSingleDynArray; const input2:single;
                    const output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;
                    

procedure MulAddArrays(const factor1, factor2, summand, output: TArrayOfSingleDynArray;
                       dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1: TArrayOfSingleDynArray; const factor2: single;
                       const summand, output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1, factor2: TArrayOfSingleDynArray; const summand: single;
                       const output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;
procedure MulAddArrays(const factor1: TArrayOfSingleDynArray; const factor2, summand: single;
                       const output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;    

procedure AddMulArrays(const summand1, summand2, factor, output: TArrayOfSingleDynArray;
                       dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1: TArrayOfSingleDynArray; const summand2: single;
                       const factor, output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1, summand2: TArrayOfSingleDynArray; const factor: single;
                       const output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;
procedure AddMulArrays(const summand1: TArrayOfSingleDynArray; const summand2, factor: single;
                       const output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;

procedure AddScaledArrays(const input1, input2: TArrayOfSingleDynArray; const factor1, factor2: single; output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;
procedure AddModulatedArrays(const input1, input2, envelope1, envelope2, output: TArrayOfSingleDynArray; dim1, dim2: integer); overload;

procedure ClearArrays(const output: TArrayOfSingleDynArray; dim1, dim2: integer);
procedure CopyArrays(const input, output: TArrayOfSingleDynArray; dim1, dim2: integer);

// when output has no dimensions use this:
procedure CreateArrayCopy(const input: TArrayOfSingleDynArray; out output: TArrayOfSingleDynArray; dim1, dim2: integer);
procedure CreateEmptyArray(out output: TArrayOfSingleDynArray; dim1, dim2: integer);

implementation



procedure AddArrays(const {eax}   input1,
                          {edx}   input2,
                          {ecx}   output: PSingle;
                          {stack} dim2: integer);
asm
  push ebx
  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, $04              //    dec(dim2);
     fld  [input1+ebx].single  // \
     fadd [input2+ebx].single  //  > output= input1+input2;
     fstp [output+ebx].single  // /

     jnz @start                //    loop until dim2 = 0

  pop ebx
end;

procedure SubArrays(const {eax}   from,
                          {edx}   amount,
                          {ecx}   output: PSingle;
                          {stack} dim2: integer); 
asm
  push ebx
  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, $04              //    dec(dim2);
     fld  [from  +ebx].single  // \
     fsub [amount+ebx].single  //  > output:= from-amount;
     fstp [output+ebx].single  // /

     jnz @start                //    loop until dim2 = 0

  pop ebx
end;

procedure MulArrays(const {eax}   input1,
                          {edx}   input2,
                          {ecx}   output: PSingle;
                          {stack} dim2: integer); 
asm
  push ebx
  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, $04              //    dec(dim2);
     fld  [input1+ebx].single  // \
     fmul [input2+ebx].single  //  > output:= input1*input2;
     fstp [output+ebx].single  // /

     jnz @start                //    loop until dim2 = 0

  pop ebx
end;






procedure AddArrays(const {eax}   input1: PSingle;
                    const {stack} input2: single;
                    const {edx}   output: PSingle;
                          {ecx}   dim2: integer);
asm
  shl dim2, 2
  @start:
     sub dim2, $04             //    dec(dim2);
     fld  [input1+dim2].single  // \
     fadd [input2].single      //  > output:= input1+input2;
     fstp [output+dim2].single  // /

     jnz @start                //    loop until dim2 = 0
end;

procedure SubArrays(const {eax}   from: PSingle;
                    const {stack} amount: single;
                    const {edx}   output: PSingle;
                          {ecx}   dim2: integer);
asm
  shl dim2, 2
  @start:
     sub dim2, $04             //    dec(dim2);
     fld  [from+dim2].single  // \
     fsub [amount].single       //  > output:= from-amount;
     fstp [output+dim2].single  // /

     jnz @start                //    loop until dim2 = 0
end;

procedure MulArrays(const {eax}   input1: PSingle;
                    const {stack} input2: single;
                    const {edx}   output: PSingle;
                          {ecx}   dim2: integer);
asm
  shl dim2, 2
  @start:
     sub dim2, $04             //    dec(dim2);
     fld  [input1+dim2].single  // \
     fmul [input2].single      //  > output:= input1*input2;
     fstp [output+dim2].single  // /

     jnz @start                //    loop until dim2 = 0
end;




procedure MulAddArrays(const {eax}   factor1,
                             {edx}   factor2,
                             {ecx}   summand,
                             {stack} output: PSingle;
                             {stack} dim2: integer);
asm
  push ebx
  push esi

  mov esi, output
  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, $04               // dec(dim2);
     fld  [factor1+ebx].single  // \
     fmul [factor2+ebx].single  //  > x      = factor1*factor2
     fadd [summand+ebx].single  //  > output = x+summand
     fstp [esi+ebx].single      // /

     jnz @start                //    loop until dim2 = 0
     
  pop esi
  pop ebx
end;

procedure MulAddArrays(const {eax}   factor1: PSingle;
                       const {stack} factor2: single;
                       const {edx}   summand,
                             {ecx}   output: PSingle;
                             {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, $04               // dec(dim2);
     fld  [factor1+ebx].single  // \
     fmul [factor2].single      //  > x      = factor1*factor2
     fadd [summand+ebx].single  //  > output = x+summand
     fstp [output+ebx].single   // /

     jnz @start                //    loop until dim2 = 0

  pop ebx
end;

procedure MulAddArrays(const {eax}   factor1,
                             {edx}   factor2: PSingle;
                       const {stack} summand: single;
                       const {ecx}   output: PSingle;
                             {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, $04               // dec(dim2);
     fld  [factor1+ebx].single  // \
     fmul [factor2+ebx].single  //  > x      = factor1*factor2
     fadd [summand].single      //  > output = x+summand
     fstp [output+ebx].single   // /

     jnz @start                //    loop until dim2 = 0

  pop ebx
end;

procedure MulAddArrays(const {eax}   factor1: PSingle;
                       const {stack} factor2,
                             {stack} summand: single;
                       const {edx}   output: PSingle;
                             {ecx}   dim2: integer);
asm
  shl dim2, 2

  @start:
     sub dim2, $04               // dec(dim2);
     fld  [factor1+dim2].single  // \
     fmul [factor2].single       //  > x      = factor1*factor2
     fadd [summand].single       //  > output = x+summand
     fstp [output+dim2].single   // /

     jnz @start                //    loop until dim2 = 0
end;






procedure AddMulArrays(const {eax}   summand1,
                             {edx}   summand2,
                             {ecx}   factor,
                             {stack} output: PSingle;
                             {stack} dim2: integer);
asm
  push ebx
  push esi

  mov esi, output
  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, $04                // dec(dim2);
     fld  [summand1+ebx].single  // \
     fadd [summand2+ebx].single  //  > x      = summand1+summand2
     fmul [factor+ebx].single    //  > output = x*factor
     fstp [esi+ebx].single       // /

     jnz @start                  //    loop until dim2 = 0
     
  pop esi
  pop ebx
end;

procedure AddMulArrays(const {eax}   summand1: PSingle;
                       const {stack} summand2: single;
                       const {edx}   factor,
                             {ecx}   output: PSingle;
                             {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, $04                // dec(dim2);
     fld  [summand1+ebx].single  // \
     fadd [summand2].single      //  > x      = summand1+summand2
     fmul [factor+ebx].single    //  > output = x*factor
     fstp [output+ebx].single    // /

     jnz @start                  //    loop until dim2 = 0

  pop ebx
end;

procedure AddMulArrays(const {eax}   summand1,
                             {edx}   summand2: PSingle;
                       const {stack} factor: single;
                       const {ecx}   output: PSingle;
                             {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, $04                // dec(dim2);
     fld  [summand1+ebx].single  // \
     fadd [summand2+ebx].single  //  > x      = summand1+summand2
     fmul [factor].single        //  > output = x*factor
     fstp [output+ebx].single    // /

     jnz @start                  //    loop until dim2 = 0

  pop ebx
end;

procedure AddMulArrays(const {eax}   summand1: PSingle;
                       const {stack} summand2,
                             {stack} factor: single;
                       const {edx}   output: PSingle;
                             {ecx}   dim2: integer);
asm
  shl dim2, 2

  @start:
     sub dim2, $04                // dec(dim2);
     fld  [summand1+dim2].single  // \
     fadd [summand2].single       //  > x      = summand1*summand2
     fmul [factor].single         //  > output = x+factor
     fstp [output+dim2].single    // /

     jnz @start                   //    loop until dim2 = 0
end;




procedure AddScaledArrays(const {eax}   input1,
                                {edx}   input2: PSingle;
                          const {stack} factor1,
                                {stack} factor2: single;
                          const {ecx}   output: PSingle;
                                {stack} dim2: integer);
asm
  push ebx

  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, $04                // dec(dim2);
     fld  [input1+ebx].single
     fmul [factor1].single
     fld  [input2+ebx].single
     fmul [factor2].single
     faddp
     fstp [output+ebx].single

     jnz @start                  //    loop until dim2 = 0

  pop ebx
end;


procedure AddModulatedArrays(const {eax}   input1,
                                   {edx}   input2,
                                   {ecx}   envelope1,
                                   {stack} envelope2: PSingle;
                             const {stack} output: PSingle;
                                   {stack} dim2: integer);
asm
  push ebx
  push esi
  push edi

  mov edi, envelope2
  mov esi, output
  mov ebx,dim2
  shl ebx, 2

  @start:
     sub ebx, $04                // dec(dim2);
     fld  [input1+ebx].single
     fmul [envelope1+ebx].single
     fld  [input2+ebx].single
     fmul [edi+ebx].single
     faddp
     fstp [esi+ebx].single

     jnz @start                  //    loop until dim2 = 0

  pop edi
  pop esi
  pop ebx
end;




procedure AddArrays(const {eax}   input1,
                          {edx}   input2,
                          {ecx}   output: TArrayOfSingleDynArray;
                          {stack} dim1,
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
      sub ebx, $04           //    dec(dim2);
      fld  [esi+ebx].single  // \
      fadd [eax+ebx].single  //  > output:= input1+input2;
      fstp [edi+ebx].single  // /

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
                          {ecx}   output: TArrayOfSingleDynArray;
                          {stack} dim1,
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
      sub ebx, $04           //    dec(dim2);
      fld  [esi+ebx].single  // \
      fsub [eax+ebx].single  //  > output:= from-amount;
      fstp [edi+ebx].single  // /

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
                          {ecx}   output: TArrayOfSingleDynArray;
                          {stack} dim1,
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
      sub ebx, $04           //    dec(dim2);
      fld  [esi+ebx].single  // \
      fmul [eax+ebx].single  //  > output:= input1*input2;
      fstp [edi+ebx].single  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx
    pop eax


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;









procedure AddArrays({eax}   const input1: TArrayOfSingleDynArray;
                    {stack} const input2:single;
                    {edx}   const output: TArrayOfSingleDynArray;
                    {ecx}   dim1,
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
      sub ebx, $04           //    dec(dim2);
      fld  [esi+ebx].single  // \
      fadd [input2].single   //  > output:= input1+input2;
      fstp [edi+ebx].single  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;

procedure SubArrays({eax}   const from: TArrayOfSingleDynArray;
                    {stack} const amount:single;
                    {edx}   const output: TArrayOfSingleDynArray;
                    {ecx}   dim1,
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
      sub ebx, $04           //    dec(dim2);
      fld  [esi+ebx].single  // \
      fsub [amount].single   //  > output:= from-amount;
      fstp [edi+ebx].single  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;

procedure MulArrays({eax}   const input1: TArrayOfSingleDynArray;
                    {stack} const input2:single;
                    {edx}   const output: TArrayOfSingleDynArray;
                    {ecx}   dim1,
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
      sub ebx, $04           //    dec(dim2);
      fld  [esi+ebx].single  // \
      fmul [input2].single   //  > output:= input1*input2;
      fstp [edi+ebx].single  // /

      jnz @innerloop         // loop until dim2 = 0

    pop ebx


  jmp @outerloop

  @cleanup:
    pop edi
    pop esi
    pop ebx
end;






procedure MulAddArrays(const factor1, factor2, summand, output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(@factor1[i,0], @factor2[i,0], @summand[i,0], @output[i,0], dim2);
end;

procedure MulAddArrays(const factor1: TArrayOfSingleDynArray; const factor2: single;
                       const summand, output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(@factor1[i,0], factor2, @summand[i,0], @output[i,0], dim2);
end;

procedure MulAddArrays(const factor1, factor2: TArrayOfSingleDynArray; const summand: single;
                       const output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(@factor1[i,0], @factor2[i,0], summand, @output[i,0], dim2);
end;

procedure MulAddArrays(const factor1: TArrayOfSingleDynArray; const factor2, summand: single;
                       const output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do MulAddArrays(@factor1[i,0], factor2, summand, @output[i,0], dim2);
end;






procedure AddMulArrays(const summand1, summand2, factor, output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(@summand1[i,0], @summand2[i,0], @factor[i,0], @output[i,0], dim2);
end;

procedure AddMulArrays(const summand1: TArrayOfSingleDynArray; const summand2: single; const factor, output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(@summand1[i,0], summand2, @factor[i,0], @output[i,0], dim2);
end;

procedure AddMulArrays(const summand1, summand2: TArrayOfSingleDynArray; const factor: single; const output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(@summand1[i,0], @summand2[i,0], factor, @output[i,0], dim2);
end;

procedure AddMulArrays(const summand1: TArrayOfSingleDynArray; const summand2, factor: single; const output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddMulArrays(@summand1[i,0], summand2, factor, @output[i,0], dim2);
end;






procedure AddScaledArrays(const input1, input2: TArrayOfSingleDynArray; const factor1, factor2: single; output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddScaledArrays(@input1[i,0], @input2[i,0], factor1, factor2, @output[i,0], dim2);
end;




procedure AddModulatedArrays(const input1, input2, envelope1, envelope2, output: TArrayOfSingleDynArray; dim1, dim2: integer);
var i: integer;
begin
  for i:=0 to dim1-1 do AddModulatedArrays(@input1[i,0], @input2[i,0], @envelope1[i,0], @envelope2[i,0], @output[i,0], dim2);
end;



procedure ClearArrays(const {eax} output: TArrayOfSingleDynArray;
                            {edx} dim1,
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
                           {edx}   output: TArrayOfSingleDynArray;
                           {ecx}   dim1,
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

procedure CreateArrayCopy(const input: TArrayOfSingleDynArray; out output: TArrayOfSingleDynArray; dim1, dim2: integer);
begin
  SetLength(output, dim1, dim2);
  CopyArrays(input, output, dim1, dim2);
end;

procedure CreateEmptyArray(out output: TArrayOfSingleDynArray; dim1, dim2: integer);
begin
  SetLength(output, dim1, dim2);
  ClearArrays(output, dim1, dim2);
end;

end.
