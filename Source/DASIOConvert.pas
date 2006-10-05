unit DASIOConvert;
{$I JEDI.INC}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses windows;

const fGrdDiv = 0.5;
      MaxShort  : Single = $7F;
      MinShort  : Single = 1/$7F;
      MaxSmall  : Single = $7FFF;
      MinSmall  : Single = 1/$7FFF;
      Max18     : Double = $1FFFF;
      Min18     : Double = 1/$1FFFF;
      Max20     : Double = $7FFFF;
      Min20     : Double = 1/$7FFFF;
      Max24     : Double = $7FFFFF;
      Min24     : Double = 1/$7FFFFF;
      MaxLong   : double = $7FFFFFFF;
      MinLong   : double = 1/$7FFFFFFF;
      mmMaxLong : array[0..3] of single = ($7FFFFFFF, $7FFFFFFF, $7FFFFFFF, $7FFFFFFF);
      mmMinLong : array[0..3] of single = (1/$7FFFFFFF, 1/$7FFFFFFF, 1/$7FFFFFFF, 1/$7FFFFFFF);

type TFPUType = (fpuX87, fpuSSE, fpu3DNow);

procedure Use_x87;
procedure Use_SSE;
procedure Use_3DNow;

var FPUType        : TFPUType;
    ToInt16MSB     : procedure (source: pointer; target: PSingle; frames: longint);
    ToInt24MSB     : procedure (source: pointer; target: PSingle; frames: longint);  // used for 20 bits as well
    ToInt32MSB     : procedure (source: pointer; target: PSingle; frames: longint);
    ToFloat32MSB   : procedure (source: pointer; target: PSingle; frames: longint);  // IEEE 754 32 bit float
    ToFloat64MSB   : procedure (source: pointer; target: PSingle; frames: longint);  // IEEE 754 64 bit double float
    ToInt32MSB16   : procedure (source: pointer; target: PSingle; frames: longint);  // 32 bit data with 16 bit alignment
    ToInt32MSB18   : procedure (source: pointer; target: PSingle; frames: longint);  // 32 bit data with 18 bit alignment
    ToInt32MSB20   : procedure (source: pointer; target: PSingle; frames: longint);  // 32 bit data with 20 bit alignment
    ToInt32MSB24   : procedure (source: pointer; target: PSingle; frames: longint);  // 32 bit data with 24 bit alignment
    ToInt16LSB     : procedure (source: pointer; target: PSingle; frames: longint);
    ToInt24LSB     : procedure (source: pointer; target: PSingle; frames: longint);
    ToInt32LSB     : procedure (source: pointer; target: PSingle; frames: longint);
    ToFloat32LSB   : procedure (source: pointer; target: PSingle; frames: longint);   // IEEE 754 32 bit float
    ToFloat64LSB   : procedure (source: pointer; target: PSingle; frames: longint);   // IEEE 754 64 bit double float
    ToInt32LSB16   : procedure (source: pointer; target: PSingle; frames: longint);  // 32 bit data with 16 bit alignment
    ToInt32LSB18   : procedure (source: pointer; target: PSingle; frames: longint);  // 32 bit data with 18 bit alignment
    ToInt32LSB20   : procedure (source: pointer; target: PSingle; frames: longint);  // 32 bit data with 20 bit alignment
    ToInt32LSB24   : procedure (source: pointer; target: PSingle; frames: longint);  // 32 bit data with 24 bit alignment
    FromInt16MSB   : procedure (source: PSingle; target: pointer; frames: longint);
    FromInt24MSB   : procedure (source: PSingle; target: pointer; frames: longint);  // used for 20 bits as well
    FromInt32MSB   : procedure (source: PSingle; target: pointer; frames: longint);
    FromFloat32MSB : procedure (source: PSingle; target: pointer; frames: longint);   // IEEE 754 32 bit float
    FromFloat64MSB : procedure (source: PSingle; target: pointer; frames: longint);   // IEEE 754 64 bit double float
    FromInt32MSB16 : procedure (source: PSingle; target: pointer; frames: longint);  // 32 bit data with 16 bit alignment
    FromInt32MSB18 : procedure (source: PSingle; target: pointer; frames: longint);  // 32 bit data with 18 bit alignment
    FromInt32MSB20 : procedure (source: PSingle; target: pointer; frames: longint);  // 32 bit data with 20 bit alignment
    FromInt32MSB24 : procedure (source: PSingle; target: pointer; frames: longint);  // 32 bit data with 24 bit alignment
    FromInt16LSB   : procedure (source: PSingle; target: pointer; frames: longint);
    FromInt24LSB   : procedure (source: PSingle; target: pointer; frames: longint);
    FromInt32LSB   : procedure (source: PSingle; target: pointer; frames: longint);
    FromFloat32LSB : procedure (source: PSingle; target: pointer; frames: longint);   // IEEE 754 32 bit float
    FromFloat64LSB : procedure (source: PSingle; target: pointer; frames: longint);   // IEEE 754 64 bit double float
    FromInt32LSB16 : procedure (source: PSingle; target: pointer; frames: longint);  // 32 bit data with 16 bit alignment
    FromInt32LSB18 : procedure (source: PSingle; target: pointer; frames: longint);  // 32 bit data with 18 bit alignment
    FromInt32LSB20 : procedure (source: PSingle; target: pointer; frames: longint);  // 32 bit data with 20 bit alignment
    FromInt32LSB24 : procedure (source: PSingle; target: pointer; frames: longint);  // 32 bit data with 24 bit alignment

var MixBuffers  : procedure(InBuffer:PSingle; MixBuffer:PSingle; Samples:integer);
    Volume      : procedure(InBuffer:PSingle; Volume:Single; Samples:integer);
    ClipDigital : procedure(InBuffer:PSingle; Samples:integer);
    ClipAnalog  : procedure(InBuffer:PSingle; Samples:Integer);
    EnableSSE   : Boolean;

function f_abs(f:single):single;

implementation

uses Math;

function f_abs(f: Single): Single;
asm
 fld f
 fabs
end;

function Saturate2(input, fMax: single): single;
begin
 if input > fMax
  then result := fMax
  else
   if input < -fMax
    then result := -fMax
    else Result := input;
end;

function Saturate(input, fMax: single): single; platform;
const fGrdDiv : Double = 0.5;
asm
 fld input.Single
 fadd fMax
 fabs
 fld input.Single
 fsub fMax
 fabs
 fsubp
 fmul fGrdDiv;
// result := fGrdDiv * (f_abs(input + fMax) - f_abs(input - fMax));
end;

procedure ClipDigital_x86(InBuffer: PSingle; BSize: Integer); platform;
const c1a : Single = 1;
asm
 mov ecx,edx
@Start:
 mov edx, [eax]
 and edx, $7FFFFFFF
 cmp edx, c1a
 jle @Weiter
 mov edx, [eax]
 and edx, $80000000
 add edx, c1a
 mov [eax], edx
@Weiter:
 add eax,4
 loop @Start
end;

procedure ClipAnalog_x87(InBuffer: PSingle; Samples: Integer); platform;
const c3:Single=3;
      c6:Single=6;
asm
 fld c3.Single                      // 3
 fld c6.Single                      // 6, 3
 fld1                               // 1, 6, 3
 fld1                               // 1, 1, 6, 3
 faddp                              // 2, 6, 3
@Start:
 dec edx
 mov ecx,[eax+4*edx].Integer
 and ecx,$7FFFFFFF
 mov [esp-4],ecx
 fld [esp-4].Single                 // abs(input), 2, 6, 3
 fld st(3)                          // 3, abs(input), 2, 6, 3
 fadd st(0),st(1)                   // 3 + abs(input), abs(input), 2, 6, 3
 fld st(0)                          // 3 + abs(input), 3 + abs(input), abs(input), 2, 6, 3
 fmul [eax+4*edx].single            // input*(3 + abs(input)), 3 + abs(input), abs(input), 2, 6, 3
 fxch st(2)                         // abs(input), 3 + abs(input), input*(3 + abs(input)), 2, 6, 3
 fmulp                              // abs(input)* (3 + abs(input)), input*(3 + abs(input)), 2, 6, 3
 fadd st(0),st(3)                   // 6 + abs(input)* (3 + abs(input)), input*(3 + abs(input)), 2, 6, 3
 fdiv                               // 6 + abs(input)* (3 + abs(input)) / input*(3 + abs(input)), 2, 6, 3
 fmul st(0),st(1)                   // 2 * (6 + abs(input)* (3 + abs(input)) / input*(3 + abs(input))), 2, 6, 3
 fstp [eax+4*edx].single            // 2, 6, 3
 test edx,edx
 jg @Start
 fstp st(0)
 fstp st(0)
 fstp st(0)
end;

// ReverseEndian3 : reverts 3-byte entities in place
procedure ReverseEndian3(buffer: pointer; frames: longint); platform;
asm
 mov ecx,edx
@Start:
 mov dh,[eax+2]
 mov dl,[eax  ]
 mov [eax+2],dl
 mov [eax  ],dh
 add  eax,3
 loop @Start
end;

// ReverseEndian4 : reverts 4-byte entities in place
procedure ReverseEndian4(buffer: pointer; frames: longint); platform;
asm
 mov ecx, frames
@Start:
 mov edx,[eax+4*ecx-4]
 bswap edx
 mov [eax+4*ecx-4],edx
 loop @Start
end;

// ReverseEndian8 : reverts 8-byte entities in place
procedure ReverseEndian8(buffer: pointer; frames: longint); platform;
asm
 push ebx
 mov  ecx, frames
@Start:
 mov edx,[eax]
 mov ebx,[eax+4]
 bswap edx
 bswap ebx
 mov [eax+4],edx
 mov [eax],ebx
 add  eax,8
 loop @Start
 pop ebx
end;

////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// x87 ////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

procedure ToInt16LSB_x87(source: pointer; target: PSingle; frames: longint); platform;
asm
  fld   Minsmall        //for speed
 @Start:
  fild  [eax+2*ecx-2].word
  fmul  st(0),st(1)
  fstp  [edx+4*ecx-4].single;
  loop @Start
  ffree st(0)
end;

procedure ToInt24LSB_x87(source: pointer; target: PSingle; frames: longint); platform;
asm
 fld Min24
 push ebx
 mov  ecx, frames
@Start:
 xor ebx,ebx
 mov bx,[eax]
 ror ebx,8
 mov bh,[eax+2]
 rol ebx,8
 mov [esp-4],ebx
 fild [esp-4].Single
 fmul  st(0),st(1)
 fstp [target].Single
 add  eax,3
 add  edx,4
 loop @Start
 pop ebx
 ffree st(0)
end;

procedure ToInt32LSB_x87(source: pointer; target: PSingle; frames: longint); platform;
asm
  fld   minlong         //for speed
 @Start:
  fild  [eax+4*ecx-4].dword
  fmul  st(0),st(1)
  fstp  [edx+4*ecx-4].single;
  loop @Start
  ffree st(0)
end;

procedure ToFloat32LSB_x87(source: pointer; target: PSingle; frames: longint); platform; // IEEE 754 32 bit float
begin
 move(source^, target^, frames);
end;

procedure ToFloat64LSB_x87(source: pointer; target: PSingle; frames: longint); platform; // IEEE 754 64 bit double float
asm
 @Start:
  fld   [eax+8*ecx-8].double
  fstp  [edx+4*ecx-4].single
  loop @Start
end;

procedure ToInt32LSB16_x87(source: pointer; target: PSingle; frames: longint); platform; // 32 bit data with 16 bit alignment
asm
  fld      MinSmall
@Start:
  fild     [eax+4*ecx-4].dword;
  fmul     st(0),st(1)
  fstp     [edx+4*ecx-4].single
  loop @Start
  ffree    st(0)
end;

procedure ToInt32LSB18_x87(source: pointer; target: PSingle; frames: longint); platform; // 32 bit data with 18 bit alignment
asm
  fld      Min18
@Start:
  fild     [eax+4*ecx-4].dword;
  fmul     st(0),st(1)
  fstp     [edx+4*ecx-4].single
  loop     @start
  ffree    st(0)
end;

procedure ToInt32LSB20_x87(source: pointer; target: PSingle; frames: longint); platform; // 32 bit data with 20 bit alignment
asm
  fld      Min20
@Start:
  fild     [eax+4*ecx-4].dword;
  fmul     st(0),st(1)
  fstp     [edx+4*ecx-4].single
  loop     @start
  ffree    st(0)
end;

procedure ToInt32LSB24_x87(source: pointer; target: PSingle; frames: longint); platform; // 32 bit data with 24 bit alignment
asm
  fld      Min24
@Start:
  fild     [eax+4*ecx-4].dword;
  fmul     st(0),st(1)
  fstp     [edx+4*ecx-4].single
  loop     @start
  ffree    st(0)
end;

procedure ToInt16MSB_x87(source: pointer; target: PSingle; frames: longint); platform;
asm
  push ebx
  fld   Minsmall
 @Start:
  mov bx,[eax+2*ecx-2]
  rol bx,$8
  mov [eax+2*ecx-2],bx
  fild  [eax+2*ecx-2].word
  fmul  st(0),st(1)
  fstp  [edx+4*ecx-4].single;
  loop @start
  ffree st(0)
  pop ebx
end;

procedure ToInt24MSB_x87(source: pointer; target: PSingle; frames: longint); platform; // used for 20 bits as well
begin
 ReverseEndian3(source, frames);
 ToInt24LSB_x87(source, target, frames);
end;

procedure ToInt32MSB_x87(source: pointer; target: PSingle; frames: longint); platform;
asm
 push   ebx
 fld    minlong
@Start:
 mov    ebx,[eax+4*ecx-4]
 bswap  ebx
 mov    [eax+4*ecx-4],ebx
 fild   [eax+4*ecx-4].dword
 fmul   st(0),st(1)
 fstp   [edx+4*ecx-4].single;
 loop   @start
 ffree  st(0)
 pop    ebx
end;

procedure ToFloat32MSB_x87(source: pointer; target: PSingle; frames: longint); platform;
begin
 ReverseEndian4(source, frames);
 move(source^, target^, frames);
end;

procedure ToFloat64MSB_x87(source: pointer; target: PSingle; frames: longint); platform;
begin
 ReverseEndian8(source, frames);
 ToFloat64LSB_x87(source, target, frames);
end;

procedure ToInt32MSB16_x87(source: pointer; target: PSingle; frames: longint); platform;
asm
  push     ebx
  fld      MinSmall
@Start:
  mov      ebx,[eax+4*ecx-4]
  bswap    ebx
  mov      [eax+4*ecx-4],ebx
  fild     [eax+4*ecx-4].dword;
  fmul     st(0),st(1)
  fstp     [edx+4*ecx-4].single
  loop     @start
  ffree    st(0)
  pop      ebx
end;

procedure ToInt32MSB18_x87(source: pointer; target: PSingle; frames: longint); platform;
asm
  push     ebx
  fld      Min18
@Start:
  mov      ebx,[eax+4*ecx-4]
  bswap    ebx
  mov      [eax+4*ecx-4],ebx
  fild     [eax+4*ecx-4].dword;
  fmul     st(0),st(1)
  fstp     [edx+4*ecx-4].single
  loop     @start
  ffree    st(0)
  pop      ebx
end;

procedure ToInt32MSB20_x87(source: pointer; target: PSingle; frames: longint); platform;
asm
  push     ebx
  fld      Min20
@Start:
  mov      ebx,[eax+4*ecx-4]
  bswap    ebx
  mov      [eax+4*ecx-4],ebx
  fild     [eax+4*ecx-4].dword;
  fmul     st(0),st(1)
  fstp     [edx+4*ecx-4].single
  loop     @start
  ffree    st(0)
  pop      ebx
end;

procedure ToInt32MSB24_x87(source: pointer; target: PSingle; frames: longint); platform;
asm
  push     ebx
  fld      Min24
@Start:
  mov      ebx,[eax+4*ecx-4]
  bswap    ebx
  mov      [eax+4*ecx-4],ebx
  fild     [eax+4*ecx-4].dword;
  fmul     st(0),st(1)
  fstp     [edx+4*ecx-4].single
  loop     @start
  ffree    st(0)
  pop      ebx
end;

////////////////////////////////////////////////////////////////////////////////

procedure FromInt16LSB_x87(source: PSingle; target: pointer; frames: longint); platform;
asm
  fld      MaxSmall    // move to register for speed
@Start:                // Samplecount already in ecx!
  fld      [eax+4*ecx-4].single;
  fmul     st(0),st(1)
  fistp    word ptr [edx+2*ecx-2]
  loop     @start
  ffree    st(0)       // free after loop has finished
end;

procedure FromInt24LSB_x87(source: PSingle; target: pointer; frames: longint);
asm
  push ebx
  fld   Max24         //for speed
 @Start:
  fld   [eax].dword
  fmul  st(0),st(1)
  fistp [esp-4].dword;
  mov   ebx, [esp-4]
  mov   [edx], bx
  ror   ebx, 8
  mov   [edx+2], bh
  add   eax, 4
  add   edx, 3
  dec   ecx
  jnz   @Start
  ffree st(0)
  pop ebx
end;

procedure FromInt32LSB_x87(source: PSingle; target: pointer; frames: longint);
asm
  fld   MaxLong         //for speed
 @Start:
  fld   [eax+4*ecx-4].single
  fmul  st(0),st(1)
  fistp [edx+4*ecx-4].dword;
  loop @Start
  ffree st(0)
end;

procedure FromFloat32LSB_x87(source: PSingle; target: pointer; frames: longint);   // IEEE 754 32 bit float
begin
 move(source^, target^, frames);
end;

procedure FromFloat64LSB_x87(source: PSingle; target: pointer; frames: longint);   // IEEE 754 64 bit double float
asm
 @Start:
  fld   [eax+4*ecx-4].single
  fstp  [edx+8*ecx-8].double
  loop @Start
end;

procedure FromInt32LSB16_x87(source: PSingle; target: pointer; frames: longint);  // 32 bit data with 16 bit alignment
asm
  fld      MaxSmall
@Start:
  fld      [eax+4*ecx-4].single;
  fmul     st(0),st(1)
  fistp    [edx+4*ecx-4].dword
  loop @Start
  ffree    st(0)
end;

procedure FromInt32LSB18_x87(source: PSingle; target: pointer; frames: longint);  // 32 bit data with 18 bit alignment
asm
  fld   Max18
 @Start:
  fld   [eax+4*ecx-4].single;
  fmul  st(0),st(1)
  fistp [edx+4*ecx-4].dword
  loop @Start
  ffree st(0)
end;

procedure FromInt32LSB20_x87(source: PSingle; target: pointer; frames: longint);  // 32 bit data with 20 bit alignment
asm
  fld   Max20
 @Start:
  fld   [eax+4*ecx-4].single;
  fmul  st(0),st(1)
  fistp [edx+4*ecx-4].dword
  loop @Start
  ffree st(0)
end;

procedure FromInt32LSB24_x87(source: PSingle; target: pointer; frames: longint);  // 32 bit data with 24 bit alignment
asm
  fld   Max24
 @Start:
  fld   [eax+4*ecx-4].single;
  fmul  st(0),st(1)
  fistp [edx+4*ecx-4].dword
  loop @Start
  ffree st(0)
end;

procedure FromInt16MSB_x87(source: PSingle; target: pointer; frames: longint);
asm
   push ebx
   fld      MaxSmall
 @Start:
   fld      [eax+4*ecx-4].single;
   fmul     st(0),st(1)
   fistp    word ptr [edx+2*ecx-2]
   mov      bx,[edx+2*ecx-2]
   rol      bx,$8
   mov      [edx+2*ecx-2],bx
   loop @Start
   ffree    st(0)
   pop ebx
end;

procedure FromInt24MSB_x87(source: PSingle; target: pointer; frames: longint);  // used for 20 bits as well
begin
 FromInt24LSB_x87(source, target, frames);
 ReverseEndian3(target, frames);
end;

procedure FromInt32MSB_x87(source: PSingle; target: pointer; frames: longint);
asm
  push ebx
  fld   MaxLong         //for speed
 @Start:
  fld   [eax+4*ecx-4].single
  fmul  st(0),st(1)
  fistp [edx+4*ecx-4].dword;
  mov ebx,[edx+4*ecx-4]
  bswap ebx
  mov [edx+4*ecx-4],ebx
  loop @Start
  ffree st(0)
  pop ebx
end;

procedure FromFloat32MSB_x87(source: PSingle; target: pointer; frames: longint);   // IEEE 754 32 bit float
begin
 move(source^, target^, frames);
 ReverseEndian4(target, frames);
end;

procedure FromFloat64MSB_x87(source: PSingle; target: pointer; frames: longint);   // IEEE 754 64 bit double float
begin
 FromFloat64LSB_x87(source, target, frames);
 ReverseEndian8(target, frames);
end;

procedure FromInt32MSB16_x87(source: PSingle; target: pointer; frames: longint);  // 32 bit data with 16 bit alignment
asm
  push ebx
  fld   MaxSmall
 @Start:
  fld   [eax+4*ecx-4].single
  fmul  st(0),st(1)
  fistp [edx+4*ecx-4].dword;
  mov ebx,[edx+4*ecx-4]
  bswap ebx
  mov [edx+4*ecx-4],ebx
  loop @Start
  fstp st(0)
  pop ebx
end;

procedure FromInt32MSB18_x87(source: PSingle; target: pointer; frames: longint);  // 32 bit data with 18 bit alignment
asm
  push ebx
  fld   Max18
 @Start:
  fld   [eax+4*ecx-4].single
  fmul  st(0),st(1)
  fistp [edx+4*ecx-4].dword;
  mov ebx,[edx+4*ecx-4]
  bswap ebx
  mov [edx+4*ecx-4],ebx
  loop @Start
  ffree st(0)
  pop ebx
end;

procedure FromInt32MSB20_x87(source: PSingle; target: pointer; frames: longint);  // 32 bit data with 20 bit alignment
asm
  push ebx
  fld   Max20
 @Start:
  fld   [eax+4*ecx-4].single
  fmul  st(0),st(1)
  fistp [edx+4*ecx-4].dword;
  mov ebx,[edx+4*ecx-4]
  bswap ebx
  mov [edx+4*ecx-4],ebx
  loop @Start
  ffree st(0)
  pop ebx
end;

procedure FromInt32MSB24_x87(source: PSingle; target: pointer; frames: longint);  // 32 bit data with 24 bit alignment
asm
  push ebx
  fld   Max24
 @Start:
  fld   [eax+4*ecx-4].single
  fmul  st(0),st(1)
  fistp [edx+4*ecx-4].dword;
  mov ebx,[edx+4*ecx-4]
  bswap ebx
  mov [edx+4*ecx-4],ebx
  loop @Start
  ffree st(0)
  pop ebx
end;

procedure MixBuffers_x87(InBuffer:PSingle; MixBuffer:PSingle; samples:integer);
asm
@Start:
  fld   [eax+4*ecx-4].Single
  fadd  [edx+4*ecx-4].Single
  fstp  [edx+4*ecx-4].Single
  loop @Start
end;

procedure Volume_x87(InBuffer:PSingle; Volume:Single; samples:integer);
asm
 mov ecx,samples
 fld Volume.Single
@Start:
 fld [eax+4*ecx-4].single
 fmul st(0),st(1)
 fstp [eax+4*ecx-4].single
 loop @Start
 fstp st(0)
end;

{$IFNDEF DELPHI5}

////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// SSE ////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

procedure ClipDigital_SSE(InBuffer: PSingle; BSize: Integer);
const c1a : Single = 1;
      mm1pos : array[0..3] of Single = (1, 1, 1, 1);
      mm1neg : array[0..3] of Single = (-1, -1, -1, -1);
asm
 mov ecx,edx
 push ecx
 shr ecx,4  // number of large iterations = number of elements / 16
 jz @SkipLargeAddLoop
 movups xmm0,mm1pos
 movups xmm1,mm1neg
@LargeAddLoop:
 movups xmm2,[eax   ]
 movups xmm3,[eax+$10]
 movups xmm4,[eax+$20]
 movups xmm5,[eax+$30]
 minps xmm2,xmm0
 minps xmm3,xmm0
 minps xmm4,xmm0
 minps xmm5,xmm0
 maxps xmm2,xmm1
 maxps xmm3,xmm1
 maxps xmm4,xmm1
 maxps xmm5,xmm1
 movups [eax    ],xmm2
 movups [eax+$10],xmm3
 movups [eax+$20],xmm4
 movups [eax+$30],xmm5
 add eax,$40
 loop @LargeAddLoop

@SkipLargeAddLoop:
 pop ecx
 and ecx,$0000000F
 jz @EndAdd

@SmallAddLoop:
 mov edx, [eax]
 and edx, $7FFFFFFF
 cmp edx, c1a
 jle @Weiter
 mov edx, [eax]
 and edx, $80000000
 add edx, c1a
 mov [eax], edx
@Weiter:
 add eax,4
 loop @SmallAddLoop

@EndAdd:
end;

procedure ClipAnalog_SSE(InBuffer: PSingle; Samples: Integer);
const c3:Single=3;
      c6:Single=6;
      mm1sgn : array[0..3] of Integer = ($7FFFFFFF,$7FFFFFFF,$7FFFFFFF,$7FFFFFFF);
      mmc2   : array[0..3] of Single = (2,2,2,2);
      mmc3   : array[0..3] of Single = (3,3,3,3);
      mmc6   : array[0..3] of Single = (6,6,6,6);
// a:=f_abs(x); b:=3+a; Result:=(x*b)/(a*b+6);
asm
 mov ecx,edx
 push ecx
 shr ecx,2  // number of large iterations = number of elements / 16
 jz @SkipLargeAddLoop
 movups xmm0,mm1sgn
 movups xmm1,mmc3
 movups xmm2,mmc6
 movups xmm3,mmc2
@LargeAddLoop:
 movups xmm4,[eax]  // xmm3 = x
 movaps xmm5,xmm4   // xmm4 = x
 andps xmm5,xmm0    // xmm4 = |x|
 movaps xmm6,xmm5   // xmm5 = |x|
 addps xmm5,xmm1    // xmm4 = |x|+3
 mulps xmm4,xmm5    // xmm3 = x*(|x|+3)
 mulps xmm6,xmm5    // xmm5 = |x|*(|x|+3)
 addps xmm6,xmm2    // xmm5 = |x|*(|x|+3) + 6
 divps xmm4,xmm6    // xmm4 = x*(|x|+3)/(|x|*(|x|+3)+6)
 mulps xmm4,xmm3    // xmm4 = 2*(x*(|x|+3)/(|x|*(|x|+3)+6))
 movups [eax],xmm4

 add eax,$10
 loop @LargeAddLoop

@SkipLargeAddLoop:
 pop ecx
 and ecx,$00000003
 jz @EndAdd

 fld1
 fld1
 faddp

@SmallAddLoop:
 fld [eax].single
 fabs
 fld c3
 fadd st(0),st(1)
 fld st(0)
 fmul [eax].single
 fxch st(2)
 fmulp
 fadd c6.Single
 fdiv
 fmul st(0),st(1)
 fstp [eax].single
 add eax,4
 loop @SmallAddLoop
 ffree st(0)
@EndAdd:
end;

procedure FromFloat32LSB_SSE(source: PSingle; target: pointer; frames: longint);   // IEEE 754 32 bit float
asm
 push ecx
 shr ecx,5  // number of large iterations = number of elements / 16
 jz @SkipLargeAddLoop
@LargeAddLoop:
 prefetcht0 [eax+$80]
 movups xmm0,[eax    ]
 movups xmm1,[eax+$10]
 movups xmm2,[eax+$20]
 movups xmm3,[eax+$30]
 movups xmm4,[eax+$40]
 movups xmm5,[eax+$50]
 movups xmm6,[eax+$60]
 movups xmm7,[eax+$70]

 // TODO!!!

 movups [edx    ],xmm0
 movups [edx+$10],xmm1
 movups [edx+$20],xmm2
 movups [edx+$30],xmm3
 movups [edx+$40],xmm4
 movups [edx+$50],xmm5
 movups [edx+$60],xmm6
 movups [edx+$70],xmm7

 add eax,$80
 add edx,$80
 loop @LargeAddLoop

@SkipLargeAddLoop:
 pop ecx
 and ecx,$0000001F
 jz @EndAdd

 shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4

@SmallAddLoop:
 movups xmm0,[eax]
 movups [edx],xmm0

 add eax,16
 add edx,16
 dec ecx
 jnz @SmallAddLoop

@EndAdd:
end;

procedure FromInt32LSB_SSE(source: PSingle; target: pointer; frames: longint);
asm
 movups xmm7,mmMaxLong

 push ecx
 shr ecx,4  // number of large iterations = number of elements / 16
 jz @SkipLargeAddLoop
@LargeAddLoop:
 movlps xmm0,[eax]
 prefetcht0 [eax+64]
 mulps xmm0,xmm7
 cvttps2pi mm0,xmm0
 movlps xmm1,[eax+8]
 mulps xmm1,xmm7
 cvttps2pi mm1,xmm1

 movlps xmm2,[eax+16]
 mulps xmm2,xmm7
 cvttps2pi mm2,xmm2
 movlps xmm3,[eax+24]
 mulps xmm3,xmm7
 cvttps2pi mm3,xmm3

 movlps xmm4,[eax+32]
 mulps xmm4,xmm7
 cvttps2pi mm4,xmm4
 movlps xmm5,[eax+40]
 mulps xmm5,xmm7
 cvttps2pi mm5,xmm5

 movlps xmm6,[eax+48]
 mulps xmm6,xmm7
 cvttps2pi mm6,xmm6
 movlps xmm6,[eax+56]
 mulps xmm6,xmm7
 cvttps2pi mm7,xmm6

 movntq [edx],mm0
 movntq [edx+8],mm1
 movntq [edx+16],mm2
 movntq [edx+24],mm3
 movntq [edx+32],mm4
 movntq [edx+40],mm5
 movntq [edx+48],mm6
 movntq [edx+56],mm7

 add eax,64
 add edx,64
 dec ecx
 jnz @LargeAddLoop

@SkipLargeAddLoop:
 pop ecx
 and ecx,$0000000F
 jz @EndAdd

 shr ecx,2 // number of small iterations = (number of elements modulo 16) / 4

@SmallAddLoop:
 movlps xmm0,[eax]
 mulps xmm0,xmm7
 cvttps2pi mm0,xmm0
 movlps xmm1,[eax+8]
 mulps xmm1,xmm7
 cvttps2pi mm1,xmm1

 add eax,16
 add edx,16
 dec ecx
 jnz @SmallAddLoop

@EndAdd:
 emms
end;

procedure MixBuffers_SSE(InBuffer:PSingle; MixBuffer:PSingle; samples:integer);
asm
 push ecx
 shr ecx,4  // number of large iterations = number of elements / 16
 jz @SkipLargeAddLoop
@LargeAddLoop:
  movups xmm0,[eax    ]
  movups xmm1,[eax+$10]
  movups xmm2,[eax+$20]
  movups xmm3,[eax+$30]
  movups xmm4,[edx    ]
  movups xmm5,[edx+$10]
  movups xmm6,[edx+$20]
  movups xmm7,[edx+$30]
  addps xmm0,xmm4
  addps xmm1,xmm5
  addps xmm2,xmm6
  addps xmm3,xmm7
  movups [edx    ],xmm0
  movups [edx+$10],xmm1
  movups [edx+$20],xmm2
  movups [edx+$30],xmm3
  add eax,64
  add edx,64
  dec ecx
  jnz @LargeAddLoop

@SkipLargeAddLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndAdd

@AddLoop:
  fld   [eax+4*ecx-4].Single
  fadd  [edx+4*ecx-4].Single
  fstp  [edx+4*ecx-4].Single
  loop @AddLoop

@EndAdd:
end;

{$IFNDEF FPC}
procedure Volume_SSE(InBuffer:PSingle; Volume:Single; samples:integer);
asm
  mov   ecx,samples
  movss xmm4, Volume.Single
  shufps xmm4, xmm4, 0h

  push ecx
  shr ecx,4
  jz @SkipHugeLoop
@HugeLoop:
  movups  xmm0,[eax]
  mulps  xmm0,xmm4
  movups  xmm1,[eax+16]
  mulps  xmm1,xmm4
  movups  xmm2,[eax+32]
  mulps  xmm2,xmm4
  movups  xmm3,[eax+48]
  mulps  xmm3,xmm4

  movups  [eax],xmm0
  movups  [eax+16],xmm1
  movups  [eax+32],xmm2
  movups  [eax+48],xmm3
  add   eax, 64
  loop  @HugeLoop

@SkipHugeLoop:
  pop ecx
  and ecx,$0000000F
  jz @EndSmallLoop

  push ecx
  shr ecx,2
  jz @SkipLargeLoop
@LargeLoop:
  movups  xmm0,[eax]
  mulps  xmm0,xmm4
  movups  [eax],xmm0
  add   eax, 16
  loop  @LargeLoop

@SkipLargeLoop:
  pop ecx
  and ecx,$00000003
  jz @EndSmallLoop

  fld Volume.Single
@SmallLoop:
  fld [eax].single
  fmul st(0),st(1)
  fstp [eax].single
  add eax,4
  loop  @SmallLoop

@EndSmallLoop:
  ffree st(0)
end;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// 3DNow //////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

const mmInt      : array[0..1] of integer = ($30000000, $30000000);
      mmDivInt   : array[0..1] of integer = ($4F000000, $4F000000);
      mmInt16    : array[0..1] of Single  = (1/32767, 1/32767);
      mmDivInt16 : array[0..1] of Single  = (32767, 32767);
      mmInt18    : array[0..1] of Single  = (1/131071, 1/131071);
      mmDivInt18 : array[0..1] of Single  = (131071, 131071);
      mmInt20    : array[0..1] of Single  = (1/524287, 1/524287);
      mmDivInt20 : array[0..1] of Single  = (524287, 524287);
      mmInt24    : array[0..1] of Single  = (1/8388607, 1/8388607);
      mmDivInt24 : array[0..1] of Single  = (8388607, 8388607);
      mmSmall    : array[0..1] of integer = ($30000000, $30000000);
      mmDivSmall : array[0..1] of integer = ($4F000000, $4F000000);

procedure FromInt16LSB_3DNow(source: PSingle; target: pointer; frames: longint);
var temp64 : array[0..3] of Word;
asm
  femms                     // Fast MMX Enter/Leave
  shr       ecx, 3          // Unroll the loop by 8
  movq      mm4, mmDivSmall // use mm1 as 1/high(integer) divider
  prefetch [eax]            // Holds the total mmx0..7 line in cache
                            // until modified and written
  @Loop2:
  movq      mm0, [eax]      // Spl1 | Spl2
  movq      mm1, [eax + 8]  // Spl3 | Spl4
  movq      mm2, [eax +16]  // Spl5 | Spl7
  movq      mm3, [eax +24]  // Spl7 | Spl8
  pfmul     mm0, mm4        // Multiply by Low(Integer)
  pfmul     mm1, mm4
  pfmul     mm2, mm4
  pfmul     mm3, mm4
  pf2id     mm0, mm0        // convert to FP
  pf2id     mm1, mm1
  pf2id     mm2, mm2
  pf2id     mm3, mm3
  movq      [temp64], mm0
//  mov       [edx], temp64;
  movq      temp64, mm1
  movq      temp64, mm2
  movq      temp64, mm3
{
  movq      [edx]   , mm0   // Store Sample back to RAM
  movq      [edx+ 8], mm1
  movq      [edx+16], mm2
  movq      [edx+24], mm3
}
  add       eax, 32
  add       edx, 32
  prefetch  [eax]           // Inform mmu about next Sample Position
  loop      @Loop2
  femms                     // Fast MMX Enter/Leave
end;

procedure FromInt32LSB_3DNow(source: PSingle; target: pointer; frames: longint);
asm
  femms                     // Fast MMX Enter/Leave
  shr       ecx, 3          // Unroll the loop by 8
  movq      mm4, mmDivInt   // use mm1 as 1/high(integer) divider
  prefetch [eax]            // Holds the total mmx0..7 line in cache
                            // until modified and written
  @Loop2:
  movq      mm0, [eax]      // Spl1 | Spl2
  movq      mm1, [eax + 8]  // Spl3 | Spl4
  movq      mm2, [eax +16]  // Spl5 | Spl7
  movq      mm3, [eax +24]  // Spl7 | Spl8
  pfmul     mm0, mm4        // Multiply by Low(Integer)
  pfmul     mm1, mm4
  pfmul     mm2, mm4
  pfmul     mm3, mm4
  pf2id     mm0, mm0        // convert to FP
  pf2id     mm1, mm1
  pf2id     mm2, mm2
  pf2id     mm3, mm3
  movq      [edx]   , mm0   // Store Sample back to RAM
  movq      [edx+ 8], mm1
  movq      [edx+16], mm2
  movq      [edx+24], mm3
  add       eax, 32
  add       edx, 32
  prefetch  [eax]          // Inform mmu about next Sample Position
  loop      @Loop2
  femms                    // Fast MMX Enter/Leave
end;

procedure FromInt32LSB16_3DNow(source: PSingle; target: pointer; frames: longint);
asm
  femms                     // Fast MMX Enter/Leave
  shr       ecx, 3          // Unroll the loop by 8
  movq      mm4, mmDivInt16 // use mm1 as 1/high(integer) divider
  prefetch [eax]            // Holds the total mmx0..7 line in cache
                            // until modified and written
  @Loop2:
  movq      mm0, [eax]      // Spl1 | Spl2
  movq      mm1, [eax + 8]  // Spl3 | Spl4
  movq      mm2, [eax +16]  // Spl5 | Spl7
  movq      mm3, [eax +24]  // Spl7 | Spl8
  pfmul     mm0, mm4        // Multiply by Low(Integer)
  pfmul     mm1, mm4
  pfmul     mm2, mm4
  pfmul     mm3, mm4
  pf2id     mm0, mm0        // convert to FP
  pf2id     mm1, mm1
  pf2id     mm2, mm2
  pf2id     mm3, mm3
  movq      [edx]   , mm0   // Store Sample back to RAM
  movq      [edx+ 8], mm1
  movq      [edx+16], mm2
  movq      [edx+24], mm3
  add       eax, 32
  add       edx, 32
  prefetch  [eax]          // Inform mmu about next Sample Position
  loop      @Loop2
  femms                    // Fast MMX Enter/Leave
end;

procedure FromInt32LSB18_3DNow(source: PSingle; target: pointer; frames: longint);
asm
  femms                     // Fast MMX Enter/Leave
  shr       ecx, 3          // Unroll the loop by 8
  movq      mm4, mmDivInt18 // use mm1 as 1/high(integer) divider
  prefetch [eax]            // Holds the total mmx0..7 line in cache
                            // until modified and written
  @Loop2:
  movq      mm0, [eax]      // Spl1 | Spl2
  movq      mm1, [eax + 8]  // Spl3 | Spl4
  movq      mm2, [eax +16]  // Spl5 | Spl7
  movq      mm3, [eax +24]  // Spl7 | Spl8
  pfmul     mm0, mm4        // Multiply by Low(Integer)
  pfmul     mm1, mm4
  pfmul     mm2, mm4
  pfmul     mm3, mm4
  pf2id     mm0, mm0        // convert to FP
  pf2id     mm1, mm1
  pf2id     mm2, mm2
  pf2id     mm3, mm3
  movq      [edx]   , mm0   // Store Sample back to RAM
  movq      [edx+ 8], mm1
  movq      [edx+16], mm2
  movq      [edx+24], mm3
  add       eax, 32
  add       edx, 32
  prefetch  [eax]          // Inform mmu about next Sample Position
  loop      @Loop2
  femms                    // Fast MMX Enter/Leave
end;

procedure FromInt32LSB20_3DNow(source: PSingle; target: pointer; frames: longint);
asm
  femms                     // Fast MMX Enter/Leave
  shr       ecx, 3          // Unroll the loop by 8
  movq      mm4, mmDivInt20 // use mm1 as 1/high(integer) divider
  prefetch [eax]            // Holds the total mmx0..7 line in cache
                            // until modified and written
  @Loop2:
  movq      mm0, [eax]      // Spl1 | Spl2
  movq      mm1, [eax + 8]  // Spl3 | Spl4
  movq      mm2, [eax +16]  // Spl5 | Spl7
  movq      mm3, [eax +24]  // Spl7 | Spl8
  pfmul     mm0, mm4        // Multiply by Low(Integer)
  pfmul     mm1, mm4
  pfmul     mm2, mm4
  pfmul     mm3, mm4
  pf2id     mm0, mm0        // convert to FP
  pf2id     mm1, mm1
  pf2id     mm2, mm2
  pf2id     mm3, mm3
  movq      [edx]   , mm0   // Store Sample back to RAM
  movq      [edx+ 8], mm1
  movq      [edx+16], mm2
  movq      [edx+24], mm3
  add       eax, 32
  add       edx, 32
  prefetch  [eax]          // Inform mmu about next Sample Position
  loop      @Loop2
  femms                    // Fast MMX Enter/Leave
end;

procedure FromInt32LSB24_3DNow(source: PSingle; target: pointer; frames: longint);
asm
  femms                     // Fast MMX Enter/Leave
  shr       ecx, 3          // Unroll the loop by 8
  movq      mm4, mmDivInt24 // use mm1 as 1/high(integer) divider
  prefetch [eax]            // Holds the total mmx0..7 line in cache
                            // until modified and written
  @Loop2:
  movq      mm0, [eax]      // Spl1 | Spl2
  movq      mm1, [eax + 8]  // Spl3 | Spl4
  movq      mm2, [eax +16]  // Spl5 | Spl7
  movq      mm3, [eax +24]  // Spl7 | Spl8
  pfmul     mm0, mm4        // Multiply by Low(Integer)
  pfmul     mm1, mm4
  pfmul     mm2, mm4
  pfmul     mm3, mm4
  pf2id     mm0, mm0        // convert to FP
  pf2id     mm1, mm1
  pf2id     mm2, mm2
  pf2id     mm3, mm3
  movq      [edx]   , mm0   // Store Sample back to RAM
  movq      [edx+ 8], mm1
  movq      [edx+16], mm2
  movq      [edx+24], mm3
  add       eax, 32
  add       edx, 32
  prefetch  [eax]          // Inform mmu about next Sample Position
  loop      @Loop2
  femms                    // Fast MMX Enter/Leave
end;

procedure ToInt16LSB_3DNow(source: pointer; target: PSingle; frames: longint);
asm
  femms                    // Fast MMX Enter/Leave
  shr       ecx, 3         // unroll the loop by 8
  movq      mm4, mmSmall   // use mm4 as 1/high(integer) divider
  prefetchw [eax]          // give the mmu a heads-up,
                           // load the total line of mmx0..7 data in the cache
                           // and prepare for modification. (If I understand AMD correctly)
  @Loop2:
  movq      mm0, [eax]     // Spl1 | Spl2
  movq      mm1, [eax+8]   // Spl3 | Spl4
  movq      mm2, [eax+16]  // Spl5 | Spl7
  movq      mm3, [eax+24]  // Spl7 | Spl8
  pi2fd     mm0, mm0       // convert to FP
  pi2fd     mm1, mm1
  pi2fd     mm2, mm2
  pi2fd     mm3, mm3
  pfmul     mm0, mm4       // divide by high(integer)
  pfmul     mm1, mm4
  pfmul     mm2, mm4
  pfmul     mm3, mm4
  movq      [edx], mm0     // Store Sample back to RAM
  movq      [edx+8], mm1
  movq      [edx+16], mm2
  movq      [edx+24], mm3
  add       eax, 32
  add       edx, 32
  prefetchw [eax]          // Inform mmu about next Sample Position
  loop      @Loop2
  femms                    // Fast MMX Enter/Leave
end;

procedure ToInt32LSB_3DNow(source: pointer; target: PSingle; frames: longint);
asm
  femms                    // Fast MMX Enter/Leave
  shr       ecx, 3         // unroll the loop by 8
  movq      mm4, mmInt     // use mm4 as 1/high(integer) divider
  prefetchw [eax]          // give the mmu a heads-up,
                           // load the total line of mmx0..7 data in the cache
                           // and prepare for modification. (If I understand AMD correctly)
  @Loop2:
  movq      mm0, [eax]     // Spl1 | Spl2
  movq      mm1, [eax+8]   // Spl3 | Spl4
  movq      mm2, [eax+16]  // Spl5 | Spl7
  movq      mm3, [eax+24]  // Spl7 | Spl8
  pi2fd     mm0, mm0       // convert to FP
  pi2fd     mm1, mm1
  pi2fd     mm2, mm2
  pi2fd     mm3, mm3
  pfmul     mm0, mm4       // divide by high(integer)
  pfmul     mm1, mm4
  pfmul     mm2, mm4
  pfmul     mm3, mm4
  movq      [edx], mm0     // Store Sample back to RAM
  movq      [edx+8], mm1
  movq      [edx+16], mm2
  movq      [edx+24], mm3
  add       eax, 32
  add       edx, 32
  prefetchw [eax]          // Inform mmu about next Sample Position
  loop      @Loop2
  femms                    // Fast MMX Enter/Leave
end;

procedure ToInt32LSB16_3DNow(source: pointer; target: PSingle; frames: longint);
asm
  femms                    // Fast MMX Enter/Leave
  shr       ecx, 3         // unroll the loop by 8
  movq      mm4, mmInt16   // use mm4 as 1/high(integer) divider
  prefetchw [eax]          // give the mmu a heads-up,
                           // load the total line of mmx0..7 data in the cache
                           // and prepare for modification. (If I understand AMD correctly)
  @Loop2:
  movq      mm0, [eax]     // Spl1 | Spl2
  movq      mm1, [eax+8]   // Spl3 | Spl4
  movq      mm2, [eax+16]  // Spl5 | Spl7
  movq      mm3, [eax+24]  // Spl7 | Spl8
  pi2fd     mm0, mm0       // convert to FP
  pi2fd     mm1, mm1
  pi2fd     mm2, mm2
  pi2fd     mm3, mm3
  pfmul     mm0, mm4       // divide by high(integer)
  pfmul     mm1, mm4
  pfmul     mm2, mm4
  pfmul     mm3, mm4
  movq      [edx], mm0     // Store Sample back to RAM
  movq      [edx+8], mm1
  movq      [edx+16], mm2
  movq      [edx+24], mm3
  add       eax, 32
  add       edx, 32
  prefetchw [eax]          // Inform mmu about next Sample Position
  loop      @Loop2
  femms                    // Fast MMX Enter/Leave
end;

procedure ToInt32LSB18_3DNow(source: pointer; target: PSingle; frames: longint);
asm
  femms                    // Fast MMX Enter/Leave
  shr       ecx, 3         // unroll the loop by 8
  movq      mm4, mmInt18   // use mm4 as 1/high(integer) divider
  prefetchw [eax]          // give the mmu a heads-up,
                           // load the total line of mmx0..7 data in the cache
                           // and prepare for modification. (If I understand AMD correctly)
  @Loop2:
  movq      mm0, [eax]     // Spl1 | Spl2
  movq      mm1, [eax+8]   // Spl3 | Spl4
  movq      mm2, [eax+16]  // Spl5 | Spl7
  movq      mm3, [eax+24]  // Spl7 | Spl8
  pi2fd     mm0, mm0       // convert to FP
  pi2fd     mm1, mm1
  pi2fd     mm2, mm2
  pi2fd     mm3, mm3
  pfmul     mm0, mm4       // divide by high(integer)
  pfmul     mm1, mm4
  pfmul     mm2, mm4
  pfmul     mm3, mm4
  movq      [edx], mm0     // Store Sample back to RAM
  movq      [edx+8], mm1
  movq      [edx+16], mm2
  movq      [edx+24], mm3
  add       eax, 32
  add       edx, 32
  prefetchw [eax]          // Inform mmu about next Sample Position
  loop      @Loop2
  femms                    // Fast MMX Enter/Leave
end;

procedure ToInt32LSB20_3DNow(source: pointer; target: PSingle; frames: longint);
asm
  femms                    // Fast MMX Enter/Leave
  shr       ecx, 3         // unroll the loop by 8
  movq      mm4, mmInt20   // use mm4 as 1/high(integer) divider
  prefetchw [eax]          // give the mmu a heads-up,
                           // load the total line of mmx0..7 data in the cache
                           // and prepare for modification. (If I understand AMD correctly)
  @Loop2:
  movq      mm0, [eax]     // Spl1 | Spl2
  movq      mm1, [eax+8]   // Spl3 | Spl4
  movq      mm2, [eax+16]  // Spl5 | Spl7
  movq      mm3, [eax+24]  // Spl7 | Spl8
  pi2fd     mm0, mm0       // convert to FP
  pi2fd     mm1, mm1
  pi2fd     mm2, mm2
  pi2fd     mm3, mm3
  pfmul     mm0, mm4       // divide by high(integer)
  pfmul     mm1, mm4
  pfmul     mm2, mm4
  pfmul     mm3, mm4
  movq      [edx], mm0     // Store Sample back to RAM
  movq      [edx+8], mm1
  movq      [edx+16], mm2
  movq      [edx+24], mm3
  add       eax, 32
  add       edx, 32
  prefetchw [eax]          // Inform mmu about next Sample Position
  loop      @Loop2
  femms                    // Fast MMX Enter/Leave
end;

procedure ToInt32LSB24_3DNow(source: pointer; target: PSingle; frames: longint);
asm
  femms                    // Fast MMX Enter/Leave
  shr       ecx, 3         // unroll the loop by 8
  movq      mm4, mmInt24   // use mm4 as 1/high(integer) divider
  prefetchw [eax]          // give the mmu a heads-up,
                           // load the total line of mmx0..7 data in the cache
                           // and prepare for modification. (If I understand AMD correctly)
  @Loop2:
  movq      mm0, [eax]     // Spl1 | Spl2
  movq      mm1, [eax+8]   // Spl3 | Spl4
  movq      mm2, [eax+16]  // Spl5 | Spl7
  movq      mm3, [eax+24]  // Spl7 | Spl8
  pi2fd     mm0, mm0       // convert to FP
  pi2fd     mm1, mm1
  pi2fd     mm2, mm2
  pi2fd     mm3, mm3
  pfmul     mm0, mm4       // divide by high(integer)
  pfmul     mm1, mm4
  pfmul     mm2, mm4
  pfmul     mm3, mm4
  movq      [edx], mm0     // Store Sample back to RAM
  movq      [edx+8], mm1
  movq      [edx+16], mm2
  movq      [edx+24], mm3
  add       eax, 32
  add       edx, 32
  prefetchw [eax]          // Inform mmu about next Sample Position
  loop      @Loop2
  femms                    // Fast MMX Enter/Leave
end;

procedure MixBuffers_3DNow(InBuffer:PSingle; MixBuffer:PSingle; samples:integer);
asm
  femms
  shr       ecx, 3
  prefetchw [eax]
@Start:
  movq      mm0, [eax]
  movq      mm1, [eax+8]
  movq      mm2, [eax+16]
  movq      mm3, [eax+24]
  movq      mm4, [edx]
  movq      mm5, [edx+8]
  movq      mm6, [edx+16]
  movq      mm7, [edx+24]
  pfadd     mm4, mm0
  pfadd     mm5, mm1
  pfadd     mm6, mm2
  pfadd     mm7, mm3
  movq      [edx], mm4
  movq      [edx+8], mm5
  movq      [edx+16], mm6
  movq      [edx+24], mm7
  add       eax, 32
  add       edx, 32
  prefetchw [eax]          // Inform mmu about next Sample Position
  loop  @Start
  femms
end;

procedure Volume_3DNow(InBuffer:PSingle; Volume:Single; samples:integer);
var volArray : array[0..1] of Single;
asm
  fld       Volume.Single
  fst       [volArray].Single
  fst       [volArray+4].Single
  mov       ecx, samples
  shr       ecx, 3
  push      ecx
  jz        @SkipLargeLoop

  femms
  movq      mm4, volArray

  prefetchw [eax]
@LargeLoop:
  movq      mm0, [eax]
  movq      mm1, [eax+8]
  movq      mm2, [eax+16]
  movq      mm3, [eax+24]
  pfmul     mm0, mm4
  pfmul     mm1, mm4
  pfmul     mm2, mm4
  pfmul     mm3, mm4
  movq      [eax], mm0
  movq      [eax+8], mm1
  movq      [eax+16], mm2
  movq      [eax+24], mm3
  add       eax, 32
  add       edx, 32
  prefetchw [eax]
  loop      @LargeLoop
  femms
@SkipLargeLoop:
  pop ecx
  and ecx,$00000007
  jz @EndSmallLoop

  fld Volume.Single
@SmallLoop:
  fld [eax].single
  fmul st(0),st(1)
  fstp [eax].single
  add eax,4
  loop  @SmallLoop

@EndSmallLoop:
  ffree st(0)
end;

{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
///////////////////////////////// Selection ////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

procedure Use_x87;
begin
  ToInt16MSB     := ToInt16MSB_x87;
  ToInt24MSB     := ToInt24MSB_x87;
  ToInt32MSB     := ToInt32MSB_x87;
  ToFloat32MSB   := ToFloat32MSB_x87;
  ToFloat64MSB   := ToFloat64MSB_x87;
  ToInt32MSB16   := ToInt32MSB16_x87;
  ToInt32MSB18   := ToInt32MSB18_x87;
  ToInt32MSB20   := ToInt32MSB20_x87;
  ToInt32MSB24   := ToInt32MSB24_x87;
  ToInt16LSB     := ToInt16LSB_x87;
  ToInt24LSB     := ToInt24LSB_x87;
  ToInt32LSB     := ToInt32LSB_x87;
  ToFloat32LSB   := ToFloat32LSB_x87;
  ToFloat64LSB   := ToFloat64LSB_x87;
  ToInt32LSB16   := ToInt32LSB16_x87;
  ToInt32LSB18   := ToInt32LSB18_x87;
  ToInt32LSB20   := ToInt32LSB20_x87;
  ToInt32LSB24   := ToInt32LSB24_x87;
  FromInt16MSB   := FromInt16MSB_x87;
  FromInt24MSB   := FromInt24MSB_x87;
  FromInt32MSB   := FromInt32MSB_x87;
  FromFloat32MSB := FromFloat32MSB_x87;
  FromFloat64MSB := FromFloat64MSB_x87;
  FromInt32MSB16 := FromInt32MSB16_x87;
  FromInt32MSB18 := FromInt32MSB18_x87;
  FromInt32MSB20 := FromInt32MSB20_x87;
  FromInt32MSB24 := FromInt32MSB24_x87;
  FromInt16LSB   := FromInt16LSB_x87;
  FromInt24LSB   := FromInt24LSB_x87;
  FromInt32LSB   := FromInt32LSB_x87;
  FromFloat32LSB := FromFloat32LSB_x87;
  FromFloat64LSB := FromFloat64LSB_x87;
  FromInt32LSB16 := FromInt32LSB16_x87;
  FromInt32LSB18 := FromInt32LSB18_x87;
  FromInt32LSB20 := FromInt32LSB20_x87;
  FromInt32LSB24 := FromInt32LSB24_x87;
  MixBuffers     := MixBuffers_x87;
  Volume         := Volume_x87;
  ClipDigital    := ClipDigital_x86;
  ClipAnalog     := ClipAnalog_x87;
end;

procedure Use_SSE;
begin
 {$IFNDEF DELPHI5}
 FromInt32LSB := FromInt32LSB_SSE;
 MixBuffers   := MixBuffers_SSE;
 ClipDigital  := ClipDigital_SSE;
 ClipAnalog   := ClipAnalog_SSE;
 {$IFNDEF FPC}
 Volume       := Volume_SSE;
 {$ENDIF}
 {$ENDIF}
end;

procedure Use_3DNow;
begin
 {$IFNDEF DELPHI5}
{
 FromInt16LSB := FromInt16LSB_3DNow;
 ToInt16LSB := ToInt16LSB_3DNow;
 FromInt24LSB := FromInt24LSB_3DNow;
 ToInt24LSB := ToInt24LSB_3DNow;
}
 FromInt32LSB := FromInt32LSB_3DNow;
 ToInt32LSB := ToInt32LSB_3DNow;
 FromInt32LSB16 := FromInt32LSB16_3DNow;
 ToInt32LSB16 := ToInt32LSB16_3DNow;
 FromInt32LSB18 := FromInt32LSB18_3DNow;
 ToInt32LSB18 := ToInt32LSB18_3DNow;
 FromInt32LSB20 := FromInt32LSB20_3DNow;
 ToInt32LSB20 := ToInt32LSB20_3DNow;
 FromInt32LSB24 := FromInt32LSB24_3DNow;
 ToInt32LSB24 := ToInt32LSB24_3DNow;
 MixBuffers := MixBuffers_3DNow;
 Volume := Volume_3DNow;
 {$ENDIF}
end;

initialization
 Use_x87;
 try
  FPUType := fpuX87;
  asm
   mov eax, 1
   db $0F,$A2
   test edx,2000000h
   jnz @SSEFound
   mov FPUType,0
   jmp @END_SSE
  @SSEFound:
   mov FPUType,1
  @END_SSE:
  end;
 except
  FPUType := fpuX87;
 end;

 if FPUType = fpuSSE then Use_SSE;
 asm
  mov eax, 80000000h
  db $0F,$A2
  cmp eax, 80000000h
  jbe @NO_EXTENDED
  mov eax, 80000001h
  db $0F, $A2
  test edx, 80000000h
  jz @NO_3DNow
  mov FPUType, 2
  jmp @END_3DNow
 @NO_EXTENDED:
 @NO_3DNow:
  jmp @END_3DNow
 @END_3DNow:
 end;
 if FPUType = fpu3DNow then Use_3DNow;
end.

