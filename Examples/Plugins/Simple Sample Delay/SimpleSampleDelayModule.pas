unit SimpleSampleDelayModule;

interface

uses
  Windows, Types, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule;

type
  TSimpleDelayVST = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure SDDelayLengthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleOpen(Sender: TObject);
    procedure ParameterFeedbackChange(Sender: TObject;
      const Index: Integer; var Value: Single);
    procedure ParamDryMixChange(Sender: TObject;
      const Index: Integer; var Value: Single);
    procedure ParameterWetMixChange(Sender: TObject;
      const Index: Integer; var Value: Single);
    procedure ParameterInvFBDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterInvFBChange(Sender: TObject;
      const Index: Integer; var Value: Single);
  private
    FBuffer     : array[0..1] of PDAVSingleFixedArray;
    FMix        : array[0..1] of Single;
    FFeedback   : Single;
    FBufferSize : Integer;
    FBufferPos  : Integer;
  end;

implementation

{$R *.DFM}

uses
  SimpleSampleDelayGUI, DAV_VSTCustomModule;

procedure TSimpleDelayVST.VSTModuleOpen(Sender: TObject);
begin
 FBufferPos := 0;

 Parameter[0] := 441;
 Parameter[1] := 0;
 Parameter[2] := 0;
 Parameter[3] := 0;
 Parameter[4] := 100;
end;

procedure TSimpleDelayVST.ParameterFeedbackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if FFeedback >= 0
  then FFeedback :=  (0.01 * Value)
  else FFeedback := -(0.01 * Value);

 if EditorForm is TVSTGUI
  then TVSTGUI(EditorForm).UpdateFeedback;
end;

procedure TSimpleDelayVST.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TVSTGUI.Create(Self);
end;

procedure TSimpleDelayVST.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  j : Integer;
begin
 for j := 0 to SampleFrames - 1 do
  begin
   FBuffer[0, FBufferPos] := Inputs[0, j] + FFeedback * FBuffer[0, FBufferPos];
   FBuffer[1, FBufferPos] := Inputs[1, j] + FFeedback * FBuffer[1, FBufferPos];
   Inc(FBufferPos);
   if FBufferPos >= FBufferSize
    then FBufferPos := 0;
   Outputs[0, j] := FMix[0] * Inputs[0, j] + FMix[1] * FBuffer[0, FBufferPos];
   Outputs[1, j] := FMix[0] * Inputs[1, j] + FMix[1] * FBuffer[1, FBufferPos];
   end;
end;

procedure TSimpleDelayVST.SDDelayLengthChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  newLatency : Integer;
begin
 newLatency := (round(Value) + 1);
 if FBufferSize < newLatency then
  begin
   ReallocMem(FBuffer[0], newLatency * SizeOf(Single));
   ReallocMem(FBuffer[1], newLatency * SizeOf(Single));
   FillChar(FBuffer[0]^[FBufferSize], (newLatency - FBufferSize) * SizeOf(Single), 0);
   FillChar(FBuffer[1]^[FBufferSize], (newLatency - FBufferSize) * SizeOf(Single), 0);
   FBufferSize := newLatency;
  end else
 if FBufferSize > newLatency then
  begin
   FBufferSize := newLatency;
   if FBufferPos >= FBufferSize
    then FBufferPos := 0;
   ReallocMem(FBuffer[0], newLatency * SizeOf(Single));
   ReallocMem(FBuffer[1], newLatency * SizeOf(Single));
  end;
 if EditorForm is TVSTGUI
  then TVSTGUI(EditorForm).UpdateDelayLength;
end;

procedure TSimpleDelayVST.ParameterInvFBChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Value > 0
  then FFeedback := -abs(FFeedback)
  else FFeedback := abs(FFeedback);
 if EditorForm is TVSTGUI
  then TVSTGUI(EditorForm).UpdateFeedbackInvert;
end;

procedure TSimpleDelayVST.ParameterInvFBDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 0.5
  then PreDefined := 'Off'
  else PreDefined := 'On';
end;

procedure TSimpleDelayVST.ParameterWetMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMix[1] := 0.01 * Value;

 if EditorForm is TVSTGUI
  then TVSTGUI(EditorForm).UpdateWetMix;
end;

procedure TSimpleDelayVST.ParamDryMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMix[0] := 0.01 * Value;

 if EditorForm is TVSTGUI
  then TVSTGUI(EditorForm).UpdateDryMix;
end;

end.
