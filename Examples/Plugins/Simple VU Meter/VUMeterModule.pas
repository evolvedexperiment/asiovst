unit VUMeterModule;

interface

uses
  Windows, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule;

type
  TVSTVUMeterModule = class(TVSTModule)
    procedure VSTModuleEditIdle(Sender: TObject);
    procedure VSTModuleProcess(const inputs, outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const inputs, outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
  private
    fPeak   : T2DoubleArray;
    fVolume : T2DoubleArray;
    function GetPeak(index: Integer): Double;
  public
    property Peak[index : Integer] : Double read GetPeak;
  end;

implementation

{$R *.DFM}

uses VUMeterGUI;

function TVSTVUMeterModule.GetPeak(index: Integer): Double;
begin
 result := fPeak[index];
end;

procedure TVSTVUMeterModule.VSTModuleEditIdle(Sender: TObject);
begin
 if Assigned(EditorForm) then
  with (EditorForm As TVSTVUMeterGUI)
   do TimerTimer(Sender);
end;

procedure TVSTVUMeterModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TVSTVUMeterGUI.Create(Self);
end;

////////////////////////////////////////////////////////////////////////////////
// 32 Bit Processing
////////////////////////////////////////////////////////////////////////////////
procedure TVSTVUMeterModule.VSTModuleProcess(const inputs,
  outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var i: integer;
begin
 // This is usually the most important part of your plugin:
 // Here the samples for each input and output channel can be processed
 // individually. In this example, the volume of the left and right
 // channel is set by variables determined by the parameters 0 and 1,
 // that were pre-calculated and stored in the variables vol_l and vol_r
 // in the parameterChanged procedure.
 // There is also a simple VU meter code here
for i := 0 to SampleFrames - 1 do
  begin
   outputs[0, i] := inputs[0, i] * fVolume[0];
   outputs[1, i] := inputs[1, i] * fVolume[1];

   // simple (but not very efficient) VU meter code:
   fPeak[0] := fPeak[0] * 0.9999;
   fPeak[1] := fPeak[1] * 0.9999;
   if abs(outputs[0, i]) > fPeak[0] then fPeak[0] := abs(outputs[0, i]);
   if abs(outputs[1, i]) > fPeak[1] then fPeak[1] := abs(outputs[1, i]);
  end;
end;


////////////////////////////////////////////////////////////////////////////////
// 64 Bit Processing
////////////////////////////////////////////////////////////////////////////////

procedure TVSTVUMeterModule.VSTModuleProcessDoubleReplacing(const inputs,
  outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer);
var i: integer;
begin
 // Same as above, but (internally) 64Bit...
 for i := 0 to SampleFrames - 1 do
  begin
   outputs[0, i] := inputs[0, i] * fVolume[0];
   outputs[1, i] := inputs[1, i] * fVolume[1];

   // simple (but not very efficient) VU meter code:
   fPeak[0] := fPeak[0] * 0.9999;
   fPeak[1] := fPeak[1] * 0.9999;
   if abs(outputs[0, i]) > fPeak[0] then fPeak[0] := abs(outputs[0, i]);
   if abs(outputs[1, i]) > fPeak[1] then fPeak[1] := abs(outputs[1, i]);
  end;
end;

procedure TVSTVUMeterModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 fVolume[Index] := dB_to_Amp(Value);
 if Amp_to_dB(fVolume[Index])<>Value then
  with (EditorForm As TVSTVUMeterGUI) do
   case Index of
    0 : par0.Position := Round(Amp_to_dB(fVolume[Index]));
    1 : par1.Position := Round(Amp_to_dB(fVolume[Index]));
   end;
end;

end.
