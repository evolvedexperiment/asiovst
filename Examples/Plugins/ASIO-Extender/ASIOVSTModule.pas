unit ASIOVSTModule;

interface

uses Windows, Messages, SysUtils, Classes, Forms, DASIOHost,
     DDSPBase, DVSTModule;

type
  TASIOVSTModule = class(TVSTModule)
    procedure VST_EditOpen(Sender: TObject; var GUI: TForm);
    procedure VST2ModuleDestroy(Sender: TObject);
    procedure VST2ModuleProcess(const inputs, outputs: TArrayOfSingleDynArray; sampleframes: Integer);
    procedure ASIOVSTModuleParameterProperties0CustomParameterDisplay(
      Sender: TObject; const Index: Integer; var PreDefined: String);
    procedure VSTModuleCreate(Sender: TObject);
  private
    fASIOHost    : TASIOHost;
    fInBuffer    : TArrayOfSingleDynArray;
    fInBufSize,
    fInWritePos,
    fInReadPos   : Integer;
    procedure AHBufferSwitch(Sender: TObject; const InBuffer, OutBuffer: TArrayOfSingleDynArray);
    procedure AHLatencyChanged(Sender: TObject);
    procedure AHSampleRateChanged(Sender: TObject);
  public
  end;

implementation

{$R *.DFM}

uses Math,ASIOVSTGUI;

procedure TASIOVSTModule.VST_EditOpen(Sender: TObject; var GUI: TForm);
// Do not delete this if you are using the editor
begin
 GUI := TFmASIOVST.Create(nil);
 (GUI As TFmASIOVST).theModule:=Self;
 (GUI As TFmASIOVST).CB_ASIO.Items:=fASIOHost.DriverList;
end;

procedure TASIOVSTModule.VST2ModuleDestroy(Sender: TObject);
begin
 fASIOHost.Free;
end;

procedure TASIOVSTModule.AHBufferSwitch(Sender: TObject; const InBuffer, OutBuffer: TArrayOfSingleDynArray);
var i : Integer;
begin
 // Singlebuffer contains #BufferSize Values
 if Assigned(fInBuffer[0]) and Assigned(fInBuffer[1]) then
  for i:=0 to fASIOHost.Buffersize-1 do
   begin
    OutBuffer[0,i]:=fInBuffer[0,fInReadPos];
    OutBuffer[1,i]:=fInBuffer[1,fInReadPos];
    Inc(fInReadPos); if fInReadPos>fInBufSize then fInReadPos:=0;
   end;
 if fInReadPos>fInWritePos then
  if fInReadPos-fASIOHost.BufferSize<fInWritePos then
   if fInReadPos-max(BlockSize,fASIOHost.BufferSize)>0
    then fInWritePos:=fInReadPos-max(BlockSize,fASIOHost.BufferSize);
end;

procedure TASIOVSTModule.AHLatencyChanged(Sender: TObject);
begin
 fInBufSize:=max(BlockSize,fASIOHost.BufferSize)*2;
 SetLength(fInBuffer[0],fInBufSize);
 SetLength(fInBuffer[1],fInBufSize);
 fInReadPos:=fInBufSize div 2;
 fInWritePos:=0;
end;

procedure TASIOVSTModule.AHSampleRateChanged(Sender: TObject);
begin
 //
end;

procedure TASIOVSTModule.VST2ModuleProcess(const inputs, outputs: TArrayOfSingleDynArray; sampleframes: Integer);
var i : Integer;
begin
 for i:=0 to sampleFrames-1 do
 begin
  fInBuffer[0,fInWritePos]:=inputs[0,i];
  fInBuffer[1,fInWritePos]:=inputs[1,i];
  Inc(fInWritePos); if fInWritePos>fInBufSize then fInWritePos:=0;
  outputs[0,i]:=0;
  outputs[1,i]:=0;
 end;
end;

procedure TASIOVSTModule.ASIOVSTModuleParameterProperties0CustomParameterDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: String);
begin
 PreDefined:=fASIOHost.DriverName;
end;

procedure TASIOVSTModule.VSTModuleCreate(Sender: TObject);
begin
 SetLength(fInBuffer,2);
 fASIOHost := TASIOHost.Create(Self);
 with fASIOHost do
  begin
   Name := 'fASIOHost';
   Active := False;
   PreventClipping := pcDigital;
   PreFillInBuffer := bpfNone;
   PreFillOutBuffer := bpfNone;
   ConvertOptimizations := [coSSE, co3DNow];
   ASIOTime.Speed := 1;
   ASIOTime.SampleRate := 44100;
   ASIOTime.Flags := [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid];
   OnBufferSwitch32:=AHBufferSwitch;
   OnLatencyChanged:=AHLatencyChanged;
   OnSampleRateChanged:=AHSampleRateChanged;
   ParameterProperties[0].Max:=fASIOHost.GetNumDrivers;
  end;
 with ParameterProperties[0] do
  begin
   Min:=-1;
   Max:=fASIOHost.DriverList.Count-1;
  end;
end;

end.
