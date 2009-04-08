unit HRTF3DModule;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_Complex,
  DAV_VSTModule, DAV_DspConvolution, DHRTF, DAV_DspHRTF;

type
  TVSTHRTF3DModule = class(TVSTModule)
    procedure VST_EditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VST2ModuleOpen(Sender: TObject);
    procedure VST2ModuleClose(Sender: TObject);
    procedure VST2ModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VST2ModuleParameterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
  private
    FIR           : THRTFArray;
    FLength       : Cardinal;
    FConvolution  : array [0..1] of TLowLatencyConvolution32;
  public
    FHRTFs: THRTF;
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

  SetLength(FIR[0], FLength);
  SetLength(FIR[1], FLength);

  for Channel := 0 to 1 do
   begin
    FConvolution[Channel] := TLowLatencyConvolution32.Create;
    FConvolution[Channel].MinimumIRBlockOrder :=  6;
    FConvolution[Channel].MaximumIRBlockOrder := 13;
   end;
  FHRTFs := THRTF.Create;

  RS := TResourceStream.Create(hInstance, 'Default', RT_RCDATA);
   try
    FHRTFs.LoadFromStream(RS);
   finally
    RS.Free;
   end;
 Parameter[0] := 0;
 Parameter[1] := 0;
 Parameter[2] := 0;
end;

procedure TVSTHRTF3DModule.VST2ModuleClose(Sender: TObject);
var
  i: Integer;
begin
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
  i       : Integer;
  t       : Single;
begin
 for Channel := 0 to 1
  do FConvolution[Channel].ProcessBlock(@Inputs[Channel, 0], @Outputs[Channel, 0], min(BlockSize, SampleFrames));
end;

procedure TVSTHRTF3DModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 // yet todo!
end;

procedure TVSTHRTF3DModule.VST2ModuleParameterChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if assigned(FHRTFs) then
  begin
   FHRTFs.GetHRTF(Parameter[0], Parameter[1], 1, FIR);
   FConvolution[0].LoadImpulseResponse(FIR[0]);
   FConvolution[1].LoadImpulseResponse(FIR[1]);
  end;
end;

end.
