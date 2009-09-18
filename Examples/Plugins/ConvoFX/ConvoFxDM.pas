unit ConvoFxDM;

interface

{$I DAV_Compiler.inc}

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspConvolution, DAV_DSPFilterButterworth;

const
  CNumChannels = 2;

type
  TConvoFxDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterMaximumIROrderChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLatencyChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterIRChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDampingChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FConvolution     : array [0..CNumChannels - 1] of TLowLatencyConvolution32;
    FImpulseResponse : array [0..CNumChannels] of PDAVSingleFixedArray;
    FImpulseLength   : array [0..CNumChannels] of Integer;
    FDirty           : Boolean;
    FGain            : Single;
    FIR              : Integer;
    FSemaphore       : Integer;
    FDampingFilter   : TButterworthLowpassFilter;
    procedure UpdateConvolutionIR;
    procedure RenderEnvelopes(const IR: PDAVSingleFixedArray;
      const Length: Integer);
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_HalfFloat, DAV_DspInterpolation, ConvoFxGUI, DAV_VSTCustomModule;

procedure TConvoFxDataModule.VSTModuleCreate(Sender: TObject);
begin
 FSemaphore := 0;
end;

procedure TConvoFxDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel: Integer;
begin
 for Channel := 0 to Length(FConvolution) - 1 do
  begin
   FConvolution[Channel] := TLowLatencyConvolution32.Create;
   FConvolution[Channel].MinimumIRBlockOrder := max(7, CeilLog2(InitialDelay));
   FConvolution[Channel].MaximumIRBlockOrder := 18;
  end;
 FGain := 1;
 FDampingFilter := TButterworthLowpassFilter.Create(1);
 Parameter[0] := CeilLog2(InitialDelay);
 Parameter[1] := 18;
 Parameter[2] := 1;
 Parameter[3] := 0;
end;

procedure TConvoFxDataModule.VSTModuleClose(Sender: TObject);
var
  Channel: Integer;
begin
 for Channel := 0 to Length(FConvolution) - 1
  do FreeAndNil(FConvolution[Channel]);
end;

procedure TConvoFxDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmConvoFx.Create(Self);
end;

procedure TConvoFxDataModule.ParameterMaximumIROrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel: Integer;
begin
 while FSemaphore > 0 do;
 Inc(FSemaphore);
 try
  for Channel := 0 to Length(FConvolution) - 1 do
   if assigned(FConvolution[Channel]) then
    if Value >= FConvolution[Channel].MinimumIRBlockOrder
     then FConvolution[Channel].MaximumIRBlockOrder := round(Limit(Value, 7, 20))
     else Value := FConvolution[Channel].MinimumIRBlockOrder;
 finally
  Dec(FSemaphore);
 end;
end;

procedure TConvoFxDataModule.ParameterDampingChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FDampingFilter)
  then FDampingFilter.Frequency := Value;
 FDirty := True;

 while FSemaphore > 0 do;
 Inc(FSemaphore);
 try
  UpdateConvolutionIR;
 finally
  Dec(FSemaphore);
 end;
 
 if EditorForm is TFmConvoFx
  then TFmConvoFx(EditorForm).UpdateDamping;
end;

procedure TConvoFxDataModule.ParameterGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FGain := dB_to_Amp(Value);
 FDirty := True;

 while FSemaphore > 0 do;
 Inc(FSemaphore);
 try
  UpdateConvolutionIR;
 finally
  Dec(FSemaphore);
 end;

 if EditorForm is TFmConvoFx
  then TFmConvoFx(EditorForm).UpdateGain;
end;

procedure TConvoFxDataModule.ParameterIRChange(
  Sender: TObject; const Index: Integer; var Value: Single);
const
  CResNames : array [0..39] of string[3] = ('BR2', 'LR1', 'BR1', 'BT1',
    'PB1', 'WR1', 'PT1', 'PT2', 'PT3', 'PT4', 'CP1', 'GV1', 'GV2', 'GV3',
    'GV4', 'GV5', 'HL1', 'MV1', 'NT1', 'OB1', 'RM1', 'SA1', 'SA2', 'SA3',
    'SO1', 'SV1', 'SV2', 'SV3', 'SV4', 'SV5', 'SV6', 'SV7', 'CH1',
    'CT1', 'CT2', 'CV1', 'OS1', 'YS1', 'VS1', 'OC1');
var
  i, c, r : Integer;
  ResName : string;
  HFData  : array [0..2047] of THalfFloat;
  Scale   : Single;
begin
 i := Limit(Integer(round(Value)), 1, 40);
 if FIR <> i then
  begin
   FIR := i;
   ResName := CResNames[i - 1];

   while FSemaphore > 0 do;
   Inc(FSemaphore);
   try
    // load left channel
    with TResourceStream.Create(HInstance, ResName + 'L', 'F16') do
     try
      c := 0;
      FImpulseLength[0] := Size div SizeOf(THalfFloat);
      ReallocMem(FImpulseResponse[0], FImpulseLength[0] * SizeOf(Single));
      while c + Length(HFData) < FImpulseLength[0] do
       begin
        Read(HFData[0], Length(HFData) * 2);
        for i := 0 to Length(HFData) - 1
         do FImpulseResponse[0]^[c + i] := HalfFloatToSingle(HFData[i]);
        Inc(c, Length(HFData));
       end;
      if c < FImpulseLength[0] then
       begin
        r := FImpulseLength[0] - c;
        assert(r > 0);
        assert(r < Length(HFData));
        Read(HFData[0], r * 2);
        Scale := 1 / r;
        for i := 0 to r - 1
         do FImpulseResponse[0]^[c + i] := (r - i) * Scale * HalfFloatToSingle(HFData[i]);
       end;
     finally
      Free;
     end;

    // load right channel
    with TResourceStream.Create(HInstance, ResName + 'R', 'F16') do
     try
      c := 0;
      FImpulseLength[1] := Size div SizeOf(THalfFloat);
      ReallocMem(FImpulseResponse[1], FImpulseLength[1] * SizeOf(Single));
      while c + Length(HFData) < FImpulseLength[1] do
       begin
        Read(HFData[0], Length(HFData) * 2);
        for i := 0 to Length(HFData) - 1
         do FImpulseResponse[1]^[c + i] := HalfFloatToSingle(HFData[i]);
        Inc(c, Length(HFData));
       end;
      if c < FImpulseLength[1] then
       begin
        r := FImpulseLength[1] - c;
        assert(r > 0);
        assert(r < Length(HFData));
        Read(HFData[0], r * 2);
        Scale := 1 / r;
        for i := 0 to r - 1
         do FImpulseResponse[1]^[c + i] := (r - i) * Scale * HalfFloatToSingle(HFData[i]);
       end;
     finally
      Free;
     end;
    UpdateConvolutionIR;
   finally
    Dec(FSemaphore);
   end;
  end;
 if EditorForm is TFmConvoFx
  then TFmConvoFx(EditorForm).UpdateIRSelect;
end;

procedure TConvoFxDataModule.RenderEnvelopes(const IR: PDAVSingleFixedArray;
  const Length: Integer);
var
  Sample   : Integer;
  s, Scale : Single;
begin
 FDampingFilter.ResetStates;
 Scale := 1 / Length;
 for Sample := 0 to Length - 1 do
  begin
   IR[Sample] := FGain * IR[Sample];
   s := 0.1 * sqr(sqr((Length - Sample) * Scale));
   IR[Sample] := s * IR[Sample]  +
                 (1 - s) * FDampingFilter.ProcessSample64(IR[Sample]);
  end;
end;

procedure TConvoFxDataModule.UpdateConvolutionIR;
var
  Channel   : Integer;
  TempIR    : PDAVSingleFixedArray;
  TempSize  : Integer;
  Offset    : Double;
  Pos       : Double;
  Sample, r : Integer;
const
  CDefaultSampleRate: Single = 44100;
begin
 for Channel := 0 to Length(FConvolution) - 1 do
  begin
   if abs(SampleRate - CDefaultSampleRate) < CDenorm32
    then
     begin
      TempSize := FImpulseLength[Channel];
      GetMem(TempIR, TempSize * SizeOf(Single));
      Move(FImpulseResponse[Channel]^[0], TempIR^[0], TempSize * SizeOf(Single));
      RenderEnvelopes(TempIR, TempSize);
      try
       FConvolution[Channel].LoadImpulseResponse(TempIR, TempSize);
      finally
       Dispose(TempIR);
      end;
     end
    else
     begin
      TempSize := round((FImpulseLength[Channel] - 4) * SampleRate / CDefaultSampleRate);
      Offset := CDefaultSampleRate / SampleRate;
      GetMem(TempIR, TempSize * SizeOf(Single));
      try
       Pos := 0;
       for Sample := 0 to TempSize - 1 do
        begin
         r := round(Pos - CHalf32);
         assert(Pos - r >= 0);
         assert(Pos - r <= 1);
         TempIR^[Sample] := Hermite32_asm(Pos - r, @FImpulseResponse[Channel]^[r]);
         Pos := Pos + Offset;
        end;
       RenderEnvelopes(TempIR, TempSize);
       FConvolution[Channel].LoadImpulseResponse(TempIR, TempSize);
      finally
       Dispose(TempIR);
      end;
     end;
  end;
end;

procedure TConvoFxDataModule.ParameterLatencyChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel: Integer;
begin
 while FSemaphore > 0 do;
 Inc(FSemaphore);
 try
  for Channel := 0 to Length(FConvolution) - 1 do
   if assigned(FConvolution[Channel]) then
    begin
     if Value > FConvolution[Channel].MaximumIRBlockOrder
      then Value := FConvolution[Channel].MaximumIRBlockOrder;
     FConvolution[Channel].MinimumIRBlockOrder := round(Value);
    end;
 finally
  Dec(FSemaphore);
 end;
end;

procedure TConvoFxDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel: Integer;
begin
 // lock processing
 while FSemaphore > 0 do;
 Inc(FSemaphore);
 try
  for Channel := 0 to Length(FConvolution) - 1
   do FConvolution[Channel].ProcessBlock(@Inputs[Channel, 0], @Outputs[Channel, 0], SampleFrames);
 finally
  Dec(FSemaphore);
 end;
end;

procedure TConvoFxDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 while FSemaphore > 0 do;
 Inc(FSemaphore);
 try
  UpdateConvolutionIR;
 finally
  Dec(FSemaphore);
 end;
end;

end.
