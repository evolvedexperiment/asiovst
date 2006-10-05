unit VUMeterModule;

interface

uses Windows, Messages, SysUtils, Classes, Forms, DDSPBase, DVSTModule;

type
  TVSTVUMeterModule = class(TVSTModule)
    procedure VST_EditOpen(Sender: TObject; var GUI: TForm);
    procedure VSTModuleEditIdle(Sender: TObject);
    procedure VSTModuleProcess(const inputs, outputs: TArrayOfSingleArray;
      sampleframes: Integer);
    procedure VSTModuleProcessDoubleReplacing(const inputs,
      outputs: TArrayOfDoubleArray; sampleframes: Integer);
    procedure VSTModuleParameterChange(Sender: TObject;
      const Index: Integer; var Value: Single);
  private
    fPeak   : array [0..1] of Double;
    fVolume : array [0..1] of Double;
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

uses VUMeterGUI;

procedure TVSTVUMeterModule.VST_EditOpen(Sender: TObject; var GUI: TForm);
// Do not delete this if you are using the editor
begin
 GUI := TVSTVUMeterGUI.Create(nil);
 (GUI As TVSTVUMeterGUI).theModule:=Self;
end;

procedure TVSTVUMeterModule.VSTModuleEditIdle(Sender: TObject);
var tmp : Integer;
begin
 with (EditorForm As TVSTVUMeterGUI) do
  begin
   tmp:=round(300+3*Amp_to_dB(fPeak[0]));
   if tmp>0 then vu_l.Width := tmp else vu_l.Width := 0;
   tmp:=round(300+3*Amp_to_dB(fPeak[1]));
   if tmp>0 then vu_r.Width := tmp else vu_r.Width := 0;
   gain_l.Caption := 'left gain: ' + inttostr(round(Parameter[0])) + ' db(fs)';
   gain_r.Caption := 'right gain: ' + inttostr(round(Parameter[1])) + ' db(fs)';
  end;
end;


////////////////////////////////////////////////////////////////////////////////
// 32 Bit Processing
////////////////////////////////////////////////////////////////////////////////

procedure TVSTVUMeterModule.VSTModuleProcess(const inputs,
  outputs: TArrayOfSingleArray; sampleframes: Integer);
var i: integer;
begin
 // This is usually the most important part of your plugin:
 // Here the samples for each input and output channel can be processed
 // individually. In this example, the volume of the left and right
 // channel is set by variables determined by the parameters 0 and 1,
 // that were pre-calculated and stored in the variables vol_l and vol_r
 // in the parameterChanged procedure.
 // There is also a simple VU meter code here
 for i := 0 to sampleframes - 1 do
  begin
   outputs[0, i] := inputs[0, i] * fVolume[0];
   outputs[1, i] := inputs[1, i] * fVolume[1];

   // simple (but not very efficient) VU meter code:
   fPeak[0]:=fPeak[0]*0.9999;
   fPeak[1]:=fPeak[1]*0.9999;
   if f_abs(outputs[0, i])>fPeak[0] then fPeak[0]:=f_abs(outputs[0, i]);
   if f_abs(outputs[1, i])>fPeak[1] then fPeak[1]:=f_abs(outputs[1, i]);
  end;
end;


////////////////////////////////////////////////////////////////////////////////
// 64 Bit Processing
////////////////////////////////////////////////////////////////////////////////

procedure TVSTVUMeterModule.VSTModuleProcessDoubleReplacing(const inputs,
  outputs: TArrayOfDoubleArray; sampleframes: Integer);
var i: integer;
begin
 // Same as above, but (internally) 64Bit...
 for i := 0 to sampleframes - 1 do
  begin
   outputs[0, i] := inputs[0, i] * fVolume[0];
   outputs[1, i] := inputs[1, i] * fVolume[1];

   // simple (but not very efficient) VU meter code:
   fPeak[0]:=fPeak[0]*0.9999;
   fPeak[1]:=fPeak[1]*0.9999;
   if f_abs(outputs[0, i])>fPeak[0] then fPeak[0]:=f_abs(outputs[0, i]);
   if f_abs(outputs[1, i])>fPeak[1] then fPeak[1]:=f_abs(outputs[1, i]);
  end;
end;

procedure TVSTVUMeterModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 fVolume[Index]:=dB_to_Amp(Value);
 if Amp_to_dB(fVolume[Index])<>Value then
  with (EditorForm As TVSTVUMeterGUI) do
   case Index of
    0 : par0.Position:=Round(Amp_to_dB(fVolume[Index]));
    1 : par1.Position:=Round(Amp_to_dB(fVolume[Index]));
   end;
end;

end.