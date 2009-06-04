unit HRTF3DModule;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_Complex,
  DAV_VSTModule, DAV_DspConvolution, DAV_DspHRTF;

type
  TVSTHRTF3DModule = class(TVSTModule)
    procedure VST_EditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VST2ModuleOpen(Sender: TObject);
    procedure VST2ModuleClose(Sender: TObject);
    procedure VST2ModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VST2ModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamAzimuthChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterInterpolationDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterInterpolationChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
    FIR           : array [0..1] of PDAVSingleFixedArray;
    FHRTFs        : THrtfs;
    FLength       : Cardinal;
    FConvolution  : array [0..1] of TLowLatencyConvolution32;
  end;

implementation

{$R *.DFM}

{$R Default.RES}

uses
  Math, HRTF3DGUI;

procedure TVSTHRTF3DModule.VST2ModuleOpen(Sender: TObject);
var
  Channel : Integer;
  RS      : TResourceStream;
begin
  FLength := 512;

  GetMem(FIR[0], FLength * SizeOf(Single));
  GetMem(FIR[1], FLength * SizeOf(Single));

  for Channel := 0 to 1 do
   begin
    FConvolution[Channel] := TLowLatencyConvolution32.Create;
    FConvolution[Channel].MinimumIRBlockOrder :=  6;
    FConvolution[Channel].MaximumIRBlockOrder := 13;
   end;
  FHRTFs := THRTFs.Create;

  RS := TResourceStream.Create(hInstance, 'Default', 'HRTF');
   try
    FHRTFs.LoadFromStream(RS);
   finally
    RS.Free;
   end;
 Parameter[0] := 0;
 Parameter[1] := 0;
 Parameter[2] := 0;
 Parameter[3] := 2;

 // Preset 1
 with Programs[1] do
  begin
   Parameter[0] := 90;
   Parameter[1] := 0;
   Parameter[2] := 0;
   Parameter[3] := 2;
  end;
end;

procedure TVSTHRTF3DModule.VST2ModuleClose(Sender: TObject);
begin
 Dispose(FIR[0]);
 Dispose(FIR[1]);
 FreeAndNil(FConvolution);
 FreeAndNil(FHRTFs);
end;

procedure TVSTHRTF3DModule.VST_EditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
// Do not delete this if you are using the editor
begin
 GUI := TVSTGUI.Create(Self);
end;

procedure TVSTHRTF3DModule.VST2ModuleProcess(
  const Inputs, Outputs: TDAVArrayOfSingleDynArray;
  const SampleFrames: Integer);
var
  Channel : Integer;
begin
 for Channel := 0 to 1
  do FConvolution[Channel].ProcessBlock(@Inputs[Channel, 0], @Outputs[Channel, 0], min(BlockSize, SampleFrames));
end;

procedure TVSTHRTF3DModule.ParameterInterpolationChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FHRTFs) then
  case round(Parameter[Index]) of
   1 : FHRTFs.InterpolationType := itNearest;
   2 : FHRTFs.InterpolationType := itLinear;
   3 : FHRTFs.InterpolationType := itLinear3;
  end;
end;

procedure TVSTHRTF3DModule.ParameterInterpolationDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  1 : PreDefined := 'Nearest';
  2 : PreDefined := 'Linear (2 Points)';
  3 : PreDefined := 'Linear (3 Points)';
 end;
end;

procedure TVSTHRTF3DModule.ParamAzimuthChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if EditorForm is TVSTGUI
  then TVSTGUI(EditorForm).UpdateAzimuth;
end;

procedure TVSTHRTF3DModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 // yet todo!
end;

procedure TVSTHRTF3DModule.VST2ModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
const
  CDeg2Rad = 2 * Pi / 360;
begin
 if assigned(FHRTFs) then
  begin
   FHRTFs.InterpolateHrir(Parameter[0] * CDeg2Rad,
                          Parameter[1] * CDeg2Rad, FLength, FIR[0], FIR[1]);
   FConvolution[0].LoadImpulseResponse(FIR[0], FLength);
   FConvolution[1].LoadImpulseResponse(FIR[1], FLength);
  end;
end;

end.
