unit DitherNoiseshaperDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspDitherNoiseshaper;

type
  TDitherNoiseshaperModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure DNTypeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure DNTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure DNBitDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure DNBitDepthDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FDitherNoiseshaper : array [0..1] of TDitherNoiseShaper;
  end;

implementation

{$R *.DFM}

uses
  DitherNoiseshaperGUI;

procedure TDitherNoiseshaperModule.VSTModuleOpen(Sender: TObject);
begin
 FDitherNoiseshaper[0] := TDitherNoiseShaper.Create;
 FDitherNoiseshaper[1] := TDitherNoiseShaper.Create;
 Parameter[0] := 16;
 Parameter[1] := 0;
end;

procedure TDitherNoiseshaperModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FDitherNoiseshaper[0]);
 FreeAndNil(FDitherNoiseshaper[1]);
end;

procedure TDitherNoiseshaperModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmDitherNoiseshaper.Create(Self);
 with TFmDitherNoiseshaper(GUI)
  do CBNoiseshaperType.ItemIndex := Round(Parameter[1])
end;

procedure TDitherNoiseshaperModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := FDitherNoiseshaper[0].ProcessFloat(Inputs[0, i]);
   Outputs[1, i] := FDitherNoiseshaper[1].ProcessFloat(Inputs[1, i]);
  end;
end;

procedure TDitherNoiseshaperModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := FDitherNoiseshaper[0].ProcessFloat(Inputs[0, i]);
   Outputs[1, i] := FDitherNoiseshaper[1].ProcessFloat(Inputs[1, i]);
  end;
end;

procedure TDitherNoiseshaperModule.DNBitDepthDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := IntToStr(Round(Parameter[Index]));
end;

procedure TDitherNoiseshaperModule.DNBitDepthChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to 1 do
  if assigned(FDitherNoiseshaper[ch])
   then FDitherNoiseshaper[ch].BitDepth := Round(Value);
 if EditorForm is TFmDitherNoiseshaper then
  with TFmDitherNoiseshaper(EditorForm)
   do if SEBitDepth.Value <> Round(Value)
    then SEBitDepth.Value := Round(Value)
end;

procedure TDitherNoiseshaperModule.DNTypeChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  ch : Integer;
begin
 for ch := 0 to 1 do
  if assigned(FDitherNoiseshaper[ch])
   then FDitherNoiseshaper[ch].DitherType := TDitherType(Round(Value));
 if EditorForm is TFmDitherNoiseshaper then
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
