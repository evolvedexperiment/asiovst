unit VTModule;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule;

type
  TTimeDomainConvole = procedure(InOutBuffer, IRBuffer: PSingle;
    Samples: Integer; Current: Single);

  TDriveMode = (dmRoasty1 = 1, dmRoasty2 = 2, dmSteamin1 = 3, dmSteamin2 = 4);

  TVTVSTModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcessMono(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessStereo(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure ParamDriveDisplay(Sender: TObject; const Index: Integer; var PreDefined: String);
    procedure ParamChannelDisplay(Sender: TObject; const Index: Integer; var PreDefined: String);
    procedure ParamLowGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHiGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamDriveChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamChannelChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamOutGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamLowBypassChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamHiBypassChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleSampleRateChange(Sender: TObject;
      const SampleRate: Single);
  private
    FDriveMode       : TDriveMode;
    FBufferPos       : Cardinal;
    FCircularBuffer  : array [0..1] of PDAVSingleFixedArray;
    FHistoryBuffer   : array [0..1] of PDAVSingleFixedArray;
    FBassKernel      : PDAVSingleFixedArray;
    FTrebleKernel    : PDAVSingleFixedArray;
    FFilterKernel    : PDAVSingleFixedArray;
    FImpulseResponse : array [1..4, 0..1, 0..47] of PDAVSingleFixedArray;
    FKernelSize      : Cardinal;
    FOutGain         : Single;
    FConvolveIR      : TTimeDomainConvole;
    FModeBypass      : array [0..1] of Boolean;
    FSemaphore       : Integer;
    procedure SetCPUDependant;
    procedure SetKernelSize(const Value: Cardinal);
    procedure KernelSizeChanged;
    procedure BuildBassFilterKernel;
    procedure BuildTrebleFilterKernel;
    procedure BuildCompleteFilterKernel;
  public
    property KernelSize: Cardinal read FKernelSize write SetKernelSize;
  end;

implementation

{$R *.DFM}

uses
  VTGUI, Dialogs, DAV_DspDFT, DAV_VSTCustomModule, DAV_VSTModuleWithPrograms;

const
  CKernelSizes: array [1..4, 0..1] of Integer =
    ((148, 55), (144, 49), (147, 55), (146, 50));

  CKernelResourceNames: array [1..4, 0..1] of string =
    (('Roasty1Bass', 'Roasty1Treble'), ('Roasty2Bass', 'Roasty2Treble'),
     ('Steamin1Bass', 'Steamin1Treble'), ('Steamin2Bass', 'Steamin2Treble'));

procedure ConvolveIR_X87(InOutBuffer, IRBuffer: PSingle;
  SampleFrames: Integer; Current: Single);
asm
    fld   Current.Single
    @SmallLoop:
    fld   [edx].Single
    fmul  st(0),st(1)
    fld   [eax].Single
    faddp

    fstp [eax].Single
    add   eax, 4
    add   edx, 4
    loop  @SmallLoop

    @EndSmallLoop:
    ffree st(0)
end;

procedure ConvolveIR_X87large(InOutBuffer, IRBuffer: PSingle;
  samples: Integer; Current: Single);
asm
    fld   Current.Single

    push ecx
    shr ecx,2
    jz @SkipLargeAddLoop
    @LargeLoop:
    fld   [edx].Single
    fmul  st(0),st(1)
    fld   [eax].Single
    faddp
    fstp [eax].Single
    fld   [edx+4].Single
    fmul  st(0),st(1)
    fld   [eax+4].Single
    faddp
    fstp [eax+4].Single
    fld   [edx+8].Single
    fmul  st(0),st(1)
    fld   [eax+8].Single
    faddp
    fstp [eax+8].Single
    fld   [edx+12].Single
    fmul  st(0),st(1)
    fld   [eax+12].Single
    faddp
    fstp [eax+12].Single

    add   eax, 16
    add   edx, 16
    loop  @LargeLoop

    @SkipLargeAddLoop:
    pop ecx
    and ecx,$00000003
    jz @EndSmallLoop

    @SmallLoop:
    fld   [edx].Single
    fmul  st(0),st(1)
    fld   [eax].Single
    faddp
    fstp [eax].Single

    add   eax, 4
    add   edx, 4
    loop  @SmallLoop

    @EndSmallLoop:
    ffree st(0)
end;

procedure ConvolveIR_X87SSE(InOutBuffer, IRBuffer: PSingle;
  samples: Integer; Current: Single);
asm
    push ecx
    shr ecx,3
    jz @SkipLargeAddLoop

    movss xmm7, Current
    shufps xmm7, xmm7, 0h
    @LargeLoop:
    movups xmm0,[edx]
    mulps xmm0,xmm7
    movups xmm1,[eax]
    addps xmm0,xmm1
    movups [eax],xmm0

    movups xmm2,[edx+16]
    mulps xmm2,xmm7
    movups xmm3,[eax+16]
    addps xmm2,xmm3
    movups [eax+16],xmm2

    add   eax, 32
    add   edx, 32
    loop  @LargeLoop

    @SkipLargeAddLoop:
    pop ecx
    and ecx,$00000007
    jz @EndSmallLoop

    fld   Current.Single
    @SmallLoop:
    fld   [edx].Single
    fmul  st(0),st(1)
    fld   [eax].Single
    faddp
    fstp [eax].Single

    add   eax, 4
    add   edx, 4
    loop  @SmallLoop

    @EndSmallLoop:
    ffree st(0)
end;

procedure TVTVSTModule.VSTModuleOpen(Sender: TObject);
var
  i, m, n, sz : Integer;
begin
  FOutGain := 1;
  FBufferPos := 0;
  FHistoryBuffer[0] := nil;
  FHistoryBuffer[1] := nil;
  FCircularBuffer[0] := nil;
  FCircularBuffer[1] := nil;

  for m := 1 to 4 do
   for n := 0 to 1 do
    with TResourceStream.Create(hInstance, CKernelResourceNames[m, n], RT_RCDATA) do
     try
      for i := 0 to Length(FImpulseResponse[m, n]) - 1 do
       begin
        GetMem(FImpulseResponse[m, n, i], CKernelSizes[m, n] * SizeOf(Single));
        sz := Read(FImpulseResponse[m, n, i]^, CKernelSizes[m, n] * SizeOf(Single));
        assert(sz = CKernelSizes[m, n] * SizeOf(Single));
       end;
     finally
      Free;
     end;
  SetCPUDependant;

  BuildBassFilterKernel;
  BuildTrebleFilterKernel;
  BuildCompleteFilterKernel;
end;

procedure TVTVSTModule.VSTModuleClose(Sender: TObject);
begin
  Dispose(FHistoryBuffer[0]);
  Dispose(FHistoryBuffer[1]);
  Dispose(FCircularBuffer[0]);
  Dispose(FCircularBuffer[1]);
  Dispose(FBassKernel);
  Dispose(FTrebleKernel);
end;

procedure TVTVSTModule.VSTModuleCreate(Sender: TObject);
begin
 FDriveMode := dmRoasty1;
 FBassKernel := nil;
 FTrebleKernel := nil;
 FSemaphore := 0;
end;

procedure TVTVSTModule.VSTEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
// Do not delete this if you are using the editor
begin
  GUI := TFmVT.Create(Self);
end;

procedure TVTVSTModule.SetCPUDependant;
begin
// if not (isFPU in CPU.Instructions) then raise Exception.Create('FPU not found');
  FConvolveIR := ConvolveIR_X87large;
(*
 if (isSSE in CPU.Instructions)
  then FConvolveIR := ConvolveIR_X87SSE;
*)
end;


procedure TVTVSTModule.KernelSizeChanged;
begin
  ReallocMem(FFilterKernel, FKernelSize);
  ReallocMem(FHistoryBuffer[0], FKernelSize * SizeOf(Single));
  ReallocMem(FHistoryBuffer[1], FKernelSize * SizeOf(Single));
  ReallocMem(FCircularBuffer[0], 2 * FKernelSize * SizeOf(Single));
  ReallocMem(FCircularBuffer[1], 2 * FKernelSize * SizeOf(Single));

  FillChar(FFilterKernel^[0], FKernelSize * SizeOf(Single), 0);
  FillChar(FHistoryBuffer[0]^[0], FKernelSize * SizeOf(Single), 0);
  FillChar(FHistoryBuffer[1]^[0], FKernelSize * SizeOf(Single), 0);
  FillChar(FCircularBuffer[0]^[0], 2 * FKernelSize * SizeOf(Single), 0);
  FillChar(FCircularBuffer[1]^[0], 2 * FKernelSize * SizeOf(Single), 0);
end;

procedure TVTVSTModule.SetKernelSize(const Value: Cardinal);
begin
 if FKernelSize <> Value then
  begin
   FKernelSize := Value;
   KernelSizeChanged;
  end;
end;

procedure TVTVSTModule.ParamHiBypassChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FModeBypass[1] := (round(Value) > 0);
 BuildCompleteFilterKernel;
 if EditorForm is TFmVT then
  with TFmVT(EditorForm) do UpdateTrebleBypass;
end;

procedure TVTVSTModule.ParamLowBypassChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FModeBypass[0] := (round(Value) > 0);
 BuildCompleteFilterKernel;
 if EditorForm is TFmVT then
  with TFmVT(EditorForm) do UpdateBassBypass;
end;

procedure TVTVSTModule.ParamDriveDisplay(Sender: TObject; const Index: Integer; var PreDefined: String);
begin
  case round(Parameter[Index]) of
    1 : PreDefined := 'Roasty 1';
    2 : PreDefined := 'Roasty 2';
    3 : PreDefined := 'Steamin'' 1';
    4 : PreDefined := 'Steamin'' 2';
   end;
end;

procedure TVTVSTModule.ParamChannelDisplay(Sender: TObject; const Index: Integer; var PreDefined: String);
begin
  case round(Parameter[Index]) of
    1 : PreDefined := 'Mono';
    2 : PreDefined := 'Stereo';
   end;
end;

procedure TVTVSTModule.BuildBassFilterKernel;
var
  SngleIndex : Single;
  LowerIndex : Integer;
  UpperIndex : Integer;
  i          : Integer;
  Lwr, Upr   : PDAVSingleFixedArray;
begin
 // Calculate Filter Index
 SngleIndex := (Parameter[0] + 12) * 2;
 LowerIndex := round(SngleIndex - 0.5);
 UpperIndex := LowerIndex + 1;
 if LowerIndex <  0 then LowerIndex :=  0;
 if UpperIndex <  0 then UpperIndex :=  0;
 if LowerIndex > 47 then LowerIndex := 47;
 if UpperIndex > 47 then UpperIndex := 47;
 SngleIndex := SngleIndex - LowerIndex;

 // Setup Filter Kernel Pointer
 Lwr := FImpulseResponse[Integer(FDriveMode), 0, LowerIndex];
 Upr := FImpulseResponse[Integer(FDriveMode), 0, UpperIndex];

 // Build Filter Kernel
 ReallocMem(FBassKernel, CKernelSizes[Integer(FDriveMode), 0] * SizeOf(Single));
 for i := 0 to CKernelSizes[Integer(FDriveMode), 0] - 1 do
  begin
   FBassKernel[i] := (1 - SngleIndex) * Lwr^[i] +
                          SngleIndex  * Upr^[i];
  end;
end;

procedure TVTVSTModule.BuildTrebleFilterKernel;
var
  SngleIndex : Single;
  LowerIndex : Integer;
  UpperIndex : Integer;
  i          : Integer;
  Lwr, Upr   : PDAVSingleFixedArray;
begin
 // Calculate Filter Index
 SngleIndex := (Parameter[1] + 12) * 2;
 LowerIndex := round(SngleIndex - 0.5);
 UpperIndex := LowerIndex + 1;
 if LowerIndex <  0 then LowerIndex :=  0;
 if UpperIndex <  0 then UpperIndex :=  0;
 if LowerIndex > 47 then LowerIndex := 47;
 if UpperIndex > 47 then UpperIndex := 47;
 SngleIndex := SngleIndex - LowerIndex;

 // Setup Filter Kernel Pointer
 Lwr := FImpulseResponse[Integer(FDriveMode), 1, LowerIndex];
 Upr := FImpulseResponse[Integer(FDriveMode), 1, UpperIndex];

 // Build Filter Kernel
 ReallocMem(FTrebleKernel, CKernelSizes[Integer(FDriveMode), 1] * SizeOf(Single));
 for i := 0 to CKernelSizes[Integer(FDriveMode), 1] - 1 do
  begin
   FTrebleKernel[i] := (1 - SngleIndex) * Lwr^[i] +
                            SngleIndex  * Upr^[i];
  end;
end;

procedure TVTVSTModule.BuildCompleteFilterKernel;

  procedure SimpleConvolveIR(const OutBuffer: PSingle;
                             const IR1: PSingle;
                             const IR1Size: Integer;
                             const IR2: PSingle;
                             const IR2Size: Integer);
  asm
   push ebx
   push edi
   mov edi, IR2Size
   imul edi, 4
   sub edi, 4
   @OuterLoop:
     mov ebx, IR2
     push ecx
     mov ecx, IR2Size
     fld [edx].Single            // load IR1 sample
     @InnerLoop:
       fld   [ebx].Single        // IR2, IR1
       fmul  st(0), st(1)        // IR1 * IR2, IR1
       fld   [eax].Single        // Out, IR1 * IR2, IR1
       faddp                     // Out + IR1 * IR2, IR1
       fstp [eax].Single         // NewOut := Out + IR1 * IR2, IR1

       add   eax, 4              // inc(Out)
       add   ebx, 4              // inc(IR2)
       loop  @InnerLoop
     pop ecx
     fstp  st(0)
     add   edx, 4                // inc(IR1)
     sub   eax, edi
   loop @OuterLoop

   pop edi
   pop ebx
  end;

var
  TempIR : PDAVSingleFixedArray;
begin
 while FSemaphore > 0 do;
 inc(FSemaphore, 1);
 try
  if FModeBypass[0] and FModeBypass[1] then
   begin
    FFilterKernel^[0] := 1;
    KernelSize := 1;
   end else
  if FModeBypass[0] then
   begin
    KernelSize := CKernelSizes[Integer(FDriveMode), 1];
    Move(FTrebleKernel[0], FFilterKernel^[0], KernelSize * SizeOf(Single));
   end else
  if FModeBypass[1] then
   begin
    KernelSize := CKernelSizes[Integer(FDriveMode), 0];
    Move(FBassKernel[0], FFilterKernel^[0], KernelSize * SizeOf(Single));
   end
  else
   begin
    KernelSize := CKernelSizes[Integer(FDriveMode), 0] + CKernelSizes[Integer(FDriveMode), 1];
    GetMem(TempIR, KernelSize * SizeOf(Single));
    FillChar(TempIR^[0], KernelSize * SizeOf(Single), 0);
    SimpleConvolveIR(@TempIR^[0],
                     @FBassKernel[0], CKernelSizes[Integer(FDriveMode), 0] - 1,
                     @FTrebleKernel[0], CKernelSizes[Integer(FDriveMode), 1] - 1);
    Move(TempIR^[0], FFilterKernel^[0], KernelSize * SizeOf(Single));
   end;
 finally
  dec(FSemaphore, 1);
 end;
end;

procedure TVTVSTModule.ParamLowGainChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 BuildBassFilterKernel;
 BuildCompleteFilterKernel;
 if EditorForm is TFmVT then
  with TFmVT(EditorForm) do UpdateBassGain;
end;

procedure TVTVSTModule.ParamHiGainChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 BuildTrebleFilterKernel;
 BuildCompleteFilterKernel;
 if EditorForm is TFmVT then
  with TFmVT(EditorForm) do UpdateTrebleGain;
end;

procedure TVTVSTModule.ParamDriveChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  NewDriveMode : TDriveMode;
begin
 NewDriveMode := TDriveMode(round(Value));

 if FDriveMode <> NewDriveMode then
  begin
   FDriveMode := NewDriveMode;
   BuildBassFilterKernel;
   BuildTrebleFilterKernel;
   BuildCompleteFilterKernel;
   if EditorForm is TFmVT then
    with TFmVT(EditorForm) do UpdateSelector;
  end;
end;

procedure TVTVSTModule.ParamChannelChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 case Round(Value) of
   1 : OnProcess := VSTModuleProcessMono;
   2 : OnProcess := VSTModuleProcessStereo;
 else OnProcess := nil;
 end;
 OnProcessReplacing := OnProcess;
 if EditorForm is TFmVT then
  with TFmVT(EditorForm) do UpdateChannel;
end;

procedure TVTVSTModule.ParamOutGainChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FOutGain := dB_to_Amp(Value);
 if EditorForm is TFmVT then
  with TFmVT(EditorForm) do UpdateGain;
end;

procedure TVTVSTModule.VSTModuleProcessMono(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i: Cardinal;
begin
 while FSemaphore > 0 do;
 inc(FSemaphore, 1);
 try
  for i := 0 to SampleFrames - 1 do
   begin
    FHistoryBuffer[0, FBufferPos] := Inputs[0, i];
    Outputs[0, i] := FOutGain * (FCircularBuffer[0, FBufferPos] + FHistoryBuffer[0, FBufferPos] * FFilterKernel[0]);
    FCircularBuffer[0, FBufferPos] := 0;
    FConvolveIR(@FCircularBuffer[0, FBufferPos], @FFilterKernel[0], FKernelSize, FHistoryBuffer[0, FBufferPos]);
    Inc(FBufferPos);
    if FBufferPos >= FKernelSize then
     begin
      FBufferPos := 0;
      Move(FCircularBuffer[0, FKernelSize], FCircularBuffer[0, 0], FKernelSize * SizeOf(Single));
      FillChar(FCircularBuffer[0, FKernelSize], FKernelSize * SizeOf(Single), 0);
     end;
   end;
 finally
  dec(FSemaphore);
 end;
end;

procedure TVTVSTModule.VSTModuleProcessStereo(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i: Cardinal;
begin
 while FSemaphore > 0 do;
 inc(FSemaphore, 1);
 try
  for i := 0 to SampleFrames - 1 do
   begin
    FHistoryBuffer[0, FBufferPos] := Inputs[0, i];
    Outputs[0, i] := FOutGain * (FCircularBuffer[0, FBufferPos] + FHistoryBuffer[0, FBufferPos] * FFilterKernel[0]);
    FCircularBuffer[0, FBufferPos] := 0;

    FHistoryBuffer[1, FBufferPos] := Inputs[1, i];
    Outputs[1, i] := FOutGain * (FCircularBuffer[1, FBufferPos] + FHistoryBuffer[1, FBufferPos] * FFilterKernel[0]);
    FCircularBuffer[1, FBufferPos] := 0;

    FConvolveIR(@FCircularBuffer[0, FBufferPos], @FFilterKernel[0], FKernelSize, FHistoryBuffer[0, FBufferPos]);
    FConvolveIR(@FCircularBuffer[1, FBufferPos], @FFilterKernel[0], FKernelSize, FHistoryBuffer[1, FBufferPos]);
    Inc(FBufferPos);

    if FBufferPos >= FKernelSize then
     begin
      FBufferPos := 0;
      Move(FCircularBuffer[0, FKernelSize], FCircularBuffer[0, 0], FKernelSize * SizeOf(Single));
      Move(FCircularBuffer[1, FKernelSize], FCircularBuffer[1, 0], FKernelSize * SizeOf(Single));
      FillChar(FCircularBuffer[0, FKernelSize], FKernelSize * SizeOf(Single), 0);
      FillChar(FCircularBuffer[1, FKernelSize], FKernelSize * SizeOf(Single), 0);
     end;
   end;
 finally
  dec(FSemaphore);
 end;
end;

procedure TVTVSTModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if abs(SampleRate - 44100) > 4000
  then ShowMessage('Samplerates other than 44.1 kHz have not been implemented yet');
end;

end.
