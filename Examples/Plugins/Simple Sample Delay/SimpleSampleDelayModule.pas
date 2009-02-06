unit SimpleSampleDelayModule;

interface

uses
  Windows, Types, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspInterpolation;

type
  TSimpleSampleDelayVST = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleOpen(Sender: TObject);
    procedure SDDelayLengthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFeedbackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamDryMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterWetMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterInvFBDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterInvFBChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcessDoubleReplacing(const Inputs,
      Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
  private
    FBuffer       : array[0..1] of PDAVSingleFixedArray;
    FMix          : array[0..1] of Single;
    FClearBuffer  : Boolean;
    FFeedbackSign : Single;
    FFeedback     : Single;
    FFeedFactor   : Single;
    FBufferSize   : Integer;
    FBufferPos    : Integer;
    procedure CalculateFeedfactor;
  public
    property ClearBufferOnChange: Boolean read FClearBuffer write FClearBuffer;
  end;

implementation

{$R *.DFM}

uses
  SimpleSampleDelayGUI, DAV_VSTCustomModule;

procedure TSimpleSampleDelayVST.VSTModuleOpen(Sender: TObject);
begin
 FBufferPos := 0;
 FClearBuffer := True;

 Parameter[0] := 441;
 Parameter[1] := 0;
 Parameter[2] := 0;
 Parameter[3] := 0;
 Parameter[4] := 100;
end;

procedure TSimpleSampleDelayVST.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TFmSimpleSampleDelay.Create(Self);
end;

procedure TSimpleSampleDelayVST.SDDelayLengthChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel, newLatency : Integer;
begin
 newLatency := (round(Value) + 1);
 if FBufferSize < newLatency then
  begin
   for Channel := 0 to Length(FBuffer) - 1 do
    begin
     ReallocMem(FBuffer[Channel], newLatency * SizeOf(Single));

     if ClearBufferOnChange
      then FillChar(FBuffer[Channel]^[FBufferSize], (newLatency - FBufferSize) * SizeOf(Single), 0)
      else
       begin
        Move(FBuffer[Channel]^[FBufferPos], FBuffer[Channel]^[(newLatency - FBufferPos)], (FBufferSize - FBufferPos) * SizeOf(Single));
        if (newLatency - 2 * FBufferPos) > 0
         then FillChar(FBuffer[Channel]^[FBufferPos], (newLatency - 2 * FBufferPos) * SizeOf(Single), 0);
       end;
    end;
   FBufferSize := newLatency;
  end else
 if FBufferSize > newLatency then
  begin
   FBufferSize := newLatency;
   for Channel := 0 to Length(FBuffer) - 1 do
    begin
     ReallocMem(FBuffer[Channel], newLatency * SizeOf(Single));
     if not ClearBufferOnChange and (FBufferPos < newLatency)
      then Move(FBuffer[Channel]^[FBufferSize + FBufferPos - newLatency], FBuffer[Channel]^[FBufferPos], (newLatency - FBufferPos) * SizeOf(Single));
    end;
   if FBufferPos >= FBufferSize
    then FBufferPos := 0;
  end;
 if EditorForm is TFmSimpleSampleDelay
  then TFmSimpleSampleDelay(EditorForm).UpdateDelayLength;
end;

procedure TSimpleSampleDelayVST.ParameterFeedbackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFeedback := (0.01 * Value);
 CalculateFeedfactor;

 if EditorForm is TFmSimpleSampleDelay
  then TFmSimpleSampleDelay(EditorForm).UpdateFeedback;
end;

procedure TSimpleSampleDelayVST.ParameterInvFBChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFeedbackSign := 2 * Value - 1;
 CalculateFeedfactor;
 if EditorForm is TFmSimpleSampleDelay
  then TFmSimpleSampleDelay(EditorForm).UpdateFeedbackInvert;
end;

procedure TSimpleSampleDelayVST.CalculateFeedfactor;
begin
 FFeedFactor := FFeedbackSign * abs(FFeedback);
end;

procedure TSimpleSampleDelayVST.ParameterInvFBDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 0.5
  then PreDefined := 'Off'
  else PreDefined := 'On';
end;

procedure TSimpleSampleDelayVST.ParameterWetMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMix[1] := 0.01 * Value;

 if EditorForm is TFmSimpleSampleDelay
  then TFmSimpleSampleDelay(EditorForm).UpdateWetMix;
end;

procedure TSimpleSampleDelayVST.ParamDryMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMix[0] := 0.01 * Value;

 if EditorForm is TFmSimpleSampleDelay
  then TFmSimpleSampleDelay(EditorForm).UpdateDryMix;
end;

procedure TSimpleSampleDelayVST.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  j : Integer;
begin
 for j := 0 to SampleFrames - 1 do
  begin
   FBuffer[0, FBufferPos] := Inputs[0, j] + FFeedFactor * FBuffer[0, FBufferPos];
   FBuffer[1, FBufferPos] := Inputs[1, j] + FFeedFactor * FBuffer[1, FBufferPos];
   Inc(FBufferPos);
   if FBufferPos >= FBufferSize
    then FBufferPos := 0;
   Outputs[0, j] := FMix[0] * Inputs[0, j] + FMix[1] * FBuffer[0, FBufferPos];
   Outputs[1, j] := FMix[0] * Inputs[1, j] + FMix[1] * FBuffer[1, FBufferPos];
   end;
end;

procedure TSimpleSampleDelayVST.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  j : Integer;
begin
 for j := 0 to SampleFrames - 1 do
  begin
   FBuffer[0, FBufferPos] := Inputs[0, j] + FFeedFactor * FBuffer[0, FBufferPos];
   FBuffer[1, FBufferPos] := Inputs[1, j] + FFeedFactor * FBuffer[1, FBufferPos];
   Inc(FBufferPos);
   if FBufferPos >= FBufferSize
    then FBufferPos := 0;
   Outputs[0, j] := FMix[0] * Inputs[0, j] + FMix[1] * FBuffer[0, FBufferPos];
   Outputs[1, j] := FMix[0] * Inputs[1, j] + FMix[1] * FBuffer[1, FBufferPos];
   end;
end;

end.
