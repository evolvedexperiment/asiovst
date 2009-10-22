unit VUMeterModule;

interface

uses
  Windows, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule;

type
  TVSTVUMeterModule = class(TVSTModule)
    procedure VSTModuleEditIdle(Sender: TObject);
    procedure VSTModuleProcess(const inputs, outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const inputs, outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
  private
    FPeak   : TDAV2DoubleArray;
    FVolume : TDAV2DoubleArray;
    function GetPeak(index: Integer): Double;
  public
    property Peak[index : Integer] : Double read GetPeak;
  end;

implementation

{$R *.DFM}

uses VUMeterGUI;

function TVSTVUMeterModule.GetPeak(index: Integer): Double;
begin
 result := FPeak[index];
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
  outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
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
   outputs[0, i] := inputs[0, i] * FVolume[0];
   outputs[1, i] := inputs[1, i] * FVolume[1];

   // simple (but not very efficient) VU meter code:
   FPeak[0] := FPeak[0] * 0.9999;
   FPeak[1] := FPeak[1] * 0.9999;
   if abs(outputs[0, i]) > FPeak[0] then FPeak[0] := abs(outputs[0, i]);
   if abs(outputs[1, i]) > FPeak[1] then FPeak[1] := abs(outputs[1, i]);
  end;
end;


////////////////////////////////////////////////////////////////////////////////
// 64 Bit Processing
////////////////////////////////////////////////////////////////////////////////

procedure TVSTVUMeterModule.VSTModuleProcessDoubleReplacing(const inputs,
  outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var i: integer;
begin
 // Same as above, but (internally) 64Bit...
 for i := 0 to SampleFrames - 1 do
  begin
   outputs[0, i] := inputs[0, i] * FVolume[0];
   outputs[1, i] := inputs[1, i] * FVolume[1];

   // simple (but not very efficient) VU meter code:
   FPeak[0] := FPeak[0] * 0.9999;
   FPeak[1] := FPeak[1] * 0.9999;
   if abs(outputs[0, i]) > FPeak[0] then FPeak[0] := abs(outputs[0, i]);
   if abs(outputs[1, i]) > FPeak[1] then FPeak[1] := abs(outputs[1, i]);
  end;
end;

procedure TVSTVUMeterModule.VSTModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 FVolume[Index] := dB_to_Amp(Value);
 if Amp_to_dB(FVolume[Index])<>Value then
  with (EditorForm As TVSTVUMeterGUI) do
   case Index of
    0 : SBLeft.Position := Round(Amp_to_dB(FVolume[Index]));
    1 : SBRight.Position := Round(Amp_to_dB(FVolume[Index]));
   end;
end;

end.
