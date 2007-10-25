unit DitherNoiseshaperDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, 
  DAVDCommon, DVSTModule, DDitherNoiseshaper;

type
  TDitherNoiseshaperModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm);
    procedure DNTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure DNTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TArrayOfSingleDynArray;
      SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs,
      Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer);
  private
    fDitherNoiseshaper : array [0..1] of TDitherNoiseShaper;
  end;

implementation

{$R *.DFM}

uses
  DitherNoiseshaperGUI;

procedure TDitherNoiseshaperModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm);
begin
 GUI := TFmDitherNoiseshaper.Create(Self);
 with TFmDitherNoiseshaper(GUI)
  do CBNoiseshaperType.ItemIndex := Round(Parameter[0])
end;

procedure TDitherNoiseshaperModule.VSTModuleProcess(const Inputs,
  Outputs: TArrayOfSingleDynArray; SampleFrames: Integer);
var i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := fDitherNoiseshaper[0].ProcessFloat(Inputs[0, i]);
   Outputs[1, i] := fDitherNoiseshaper[1].ProcessFloat(Inputs[1, i]);
  end;
end;

procedure TDitherNoiseshaperModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TArrayOfDoubleDynArray; SampleFrames: Integer);
var i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := fDitherNoiseshaper[0].ProcessFloat(Inputs[0, i]);
   Outputs[1, i] := fDitherNoiseshaper[1].ProcessFloat(Inputs[1, i]);
  end;
end;

procedure TDitherNoiseshaperModule.DNTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fDitherNoiseshaper[0].DitherType := TDitherType(Round(Index));
 fDitherNoiseshaper[1].DitherType := fDitherNoiseshaper[0].DitherType;
 if EditorForm <> nil then
  with TFmDitherNoiseshaper(EditorForm)
   do if CBNoiseshaperType.ItemIndex <> Round(Value)
    then CBNoiseshaperType.ItemIndex := Round(Value)
end;

procedure TDitherNoiseshaperModule.DNTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case TDitherType(Round(Parameter[Index])) of
  dtor9Fc  : PreDefined := '9th Order F-weighting';
  dtor3Fc  : PreDefined := '3rd Order F-weighting';
  dtor2MEc : PreDefined := '2nd Order mod. E-weighting';
  dtor3MEc : PreDefined := '3rd Order mod. E-weighting';
  dtor9MEc : PreDefined := '9th Order mod. E-weighting';
  dtor5IEc : PreDefined := '5th Order improved E-weighting';
  dtor9IEc : PreDefined := '9th Order improved E-weighting';
  dtor2Sc  : PreDefined := '2nd Order simple highpass';
 end;
end;

end.
