unit PluginDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspFilter;

type
  TPluginDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcessLR(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessMS(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
  private
    FEQs : Array [0..1, 0..10] of TSimplePeakFilter;
  public
  end;

implementation

{$R *.DFM}

uses
  EditorFrm;

procedure TPluginDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel, Band : Integer;
const
  CDefaultFrequencies : Array [0..10] of Single = (20, 40, 80, 160, 320, 640,
    1250, 2500, 5000, 10000, 20000);   
begin
 for Channel := 0 to Length(FEQs) - 1 do
  for Band := 0 to Length(FEQs[Channel]) - 1 do
   begin
    if not Assigned(FEQs[Channel, Band])
     then FEQs[Channel, Band] := TSimplePeakFilter.Create;
    with FEQs[Channel, Band] do
     begin
      SampleRate := Self.SampleRate;
      Frequency := CDefaultFrequencies[Band];
      Bandwidth := 1;
     end;
   end;
end;

procedure TPluginDataModule.VSTModuleClose(Sender: TObject);
var
  Channel, Band : Integer;
begin
 for Channel := 0 to Length(FEQs) - 1 do
  for Band := 0 to Length(FEQs[Channel]) - 1 do
   if not Assigned(FEQs[Channel, Band]) then FreeAndNil(FEQs[Channel, Band]);
end;

procedure TPluginDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TEditorForm.Create(Self);
end;

procedure TPluginDataModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  i : Integer;
begin
 FEQs[Index div 11, Index mod 11].Gain := -Value;
 i := Round(Value * 10);
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm) do
   case Index of
     0: if SB20L.Position   <> i then SB20L.Position   := i;
     1: if SB40L.Position   <> i then SB40L.Position   := i;
     2: if SB80L.Position   <> i then SB80L.Position   := i;
     3: if SB160L.Position  <> i then SB160L.Position  := i;
     4: if SB320L.Position  <> i then SB320L.Position  := i;
     5: if SB640L.Position  <> i then SB640L.Position  := i;
     6: if SB1200L.Position <> i then SB1200L.Position := i;
     7: if SB2500L.Position <> i then SB2500L.Position := i;
     8: if SB5kL.Position   <> i then SB5kL.Position   := i;
     9: if SB10kL.Position  <> i then SB10kL.Position  := i;
    10: if SB20kL.Position  <> i then SB20kL.Position  := i;
    11: if SB20R.Position   <> i then SB20R.Position   := i;
    12: if SB40R.Position   <> i then SB40R.Position   := i;
    13: if SB80R.Position   <> i then SB80R.Position   := i;
    14: if SB160R.Position  <> i then SB160R.Position  := i;
    15: if SB320R.Position  <> i then SB320R.Position  := i;
    16: if SB640R.Position  <> i then SB640R.Position  := i;
    17: if SB1200R.Position <> i then SB1200R.Position := i;
    18: if SB2500R.Position <> i then SB2500R.Position := i;
    19: if SB5kR.Position   <> i then SB5kR.Position   := i;
    20: if SB10kR.Position  <> i then SB10kR.Position  := i;
    21: if SB20kR.Position  <> i then SB20kR.Position  := i;
   end;
end;

procedure TPluginDataModule.VSTModuleProcessLR(const inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FEQs[0, 0].ProcessSample(FEQs[0, 1].ProcessSample(
                         FEQs[0, 2].ProcessSample(FEQs[0, 3].ProcessSample(
                         FEQs[0, 4].ProcessSample(FEQs[0, 5].ProcessSample(
                         FEQs[0, 6].ProcessSample(FEQs[0, 7].ProcessSample(
                         FEQs[0, 8].ProcessSample(FEQs[0, 9].ProcessSample(
                         FEQs[0,10].ProcessSample(inputs[0, Sample])))))))))));
   Outputs[1, Sample] := FEQs[1, 0].ProcessSample(FEQs[1, 1].ProcessSample(
                         FEQs[1, 2].ProcessSample(FEQs[1, 3].ProcessSample(
                         FEQs[1, 4].ProcessSample(FEQs[1, 5].ProcessSample(
                         FEQs[1, 6].ProcessSample(FEQs[1, 7].ProcessSample(
                         FEQs[1, 8].ProcessSample(FEQs[1, 9].ProcessSample(
                         FEQs[1,10].ProcessSample(inputs[1, Sample])))))))))));
  end;
end;

procedure TPluginDataModule.VSTModuleProcessMS(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i : Integer;
  d : Double;
const
  CQuarter32 : Single = 0.25;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := FEQs[0, 0].ProcessSample(FEQs[0, 1].ProcessSample(
                    FEQs[0, 2].ProcessSample(FEQs[0, 3].ProcessSample(
                    FEQs[0, 4].ProcessSample(FEQs[0, 5].ProcessSample(
                    FEQs[0, 6].ProcessSample(FEQs[0, 7].ProcessSample(
                    FEQs[0, 8].ProcessSample(FEQs[0, 9].ProcessSample(
                    FEQs[0,10].ProcessSample(inputs[0, i] + inputs[1, i])))))))))));
   Outputs[1, i] := FEQs[1, 0].ProcessSample(FEQs[1, 1].ProcessSample(
                    FEQs[1, 2].ProcessSample(FEQs[1, 3].ProcessSample(
                    FEQs[1, 4].ProcessSample(FEQs[1, 5].ProcessSample(
                    FEQs[1, 6].ProcessSample(FEQs[1, 7].ProcessSample(
                    FEQs[1, 8].ProcessSample(FEQs[1, 9].ProcessSample(
                    FEQs[1,10].ProcessSample(inputs[0, i] - inputs[1, i])))))))))));
   d := CQuarter32 * (Outputs[1, i] + Outputs[0, i]);
   Outputs[1, i] := CQuarter32 * (Outputs[1, i] - Outputs[0, i]);
   Outputs[0, i] := d;
  end;
end;

procedure TPluginDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel, Band : Integer;
begin
 for Channel := 0 to Length(FEQs) - 1 do
  for Band := 0 to Length(FEQs[Channel]) - 1 do
   begin
    if not Assigned(FEQs[Channel, Band])
     then FEQs[Channel, Band] := TSimplePeakFilter.Create;
    FEQs[Channel, Band].SampleRate := SampleRate;
   end;
end;

end.
