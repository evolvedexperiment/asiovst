unit ASIOVSTModule;

{$I ASIOVST.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAVASIOHost, DAVDCommon,
  DVSTModule;

type
  TASIOVSTModule = class(TVSTModule)
    procedure AHBufferSwitch(Sender: TObject; const InBuffer, OutBuffer: TAVDArrayOfSingleDynArray);
    procedure AHShortCircuit(Sender: TObject; const InBuffer, OutBuffer: TAVDArrayOfSingleDynArray);
    procedure ASIODriverChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ASIODriverDisplay(Sender: TObject; const Index: Integer; var PreDefined: String);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
  private
    fASIOHost        : TASIOHost;
    fInBuffer        : TAVDArrayOfSingleDynArray;
    fOutBuffer       : TAVDArrayOfSingleDynArray;
    fIntBufSize,
    fIntWritePos,
    fIntReadPos      : Integer;
    fNrOfBuffers     : Integer;
    fBufferUnderruns : Integer;
    procedure AHLatencyChanged(Sender: TObject);
  public
    property BufferUnderruns : Integer read fBufferUnderruns;
    property AsioHost : TASIOHost read fASIOHost;
  end;

implementation

{$R *.DFM}

uses
  Math, ASIOVSTGUI;

procedure TASIOVSTModule.VSTModuleCreate(Sender: TObject);
begin
 fNrOfBuffers := 2;
 fIntWritePos := 1;
 fIntReadPos := 0;
 fIntBufSize := BlockSize * fNrOfBuffers;
 SetLength(fInBuffer, 2);
 SetLength(fOutBuffer, 2);
 SetLength(fInBuffer[0],  fIntBufSize);
 SetLength(fInBuffer[1],  fIntBufSize);
 SetLength(fOutBuffer[0], fIntBufSize);
 SetLength(fOutBuffer[1], fIntBufSize);

 fASIOHost := TASIOHost.Create(Self);
 with fASIOHost do
  begin
   Active := False;
   PreventClipping := pcDigital;
   PreFillInBuffer := bpfNone;
   PreFillOutBuffer := bpfNone;
//   ConvertOptimizations := [coSSE, co3DNow];
   ASIOTime.Speed := 1;
   ASIOTime.SampleRate := 44100;
   ASIOTime.Flags := [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid];
   OnBufferSwitch32 := AHBufferSwitch;
   OnLatencyChanged := AHLatencyChanged;
   InitialDelay := InputLatency + OutputLatency + BufferSize;
  end;
 with ParameterProperties[0] do
  begin
   Min := 0;
   Max := fASIOHost.DriverList.Count - 1;
  end;
end;

procedure TASIOVSTModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TFmASIOVST.Create(Self);
 with (GUI As TFmASIOVST) do
  try
   CB_ASIO.Items := fASIOHost.DriverList;
   DisplayASIOInformation;
  except
  end;
end;

procedure TASIOVSTModule.VSTModuleDestroy(Sender: TObject);
begin
 fASIOHost.Free;
end;

procedure TASIOVSTModule.VSTModuleOpen(Sender: TObject);
begin
 if Assigned(fASIOHost)
  then fASIOHost.Active := True;
end;

procedure TASIOVSTModule.VSTModuleClose(Sender: TObject);
begin
 if Assigned(fASIOHost)
  then fASIOHost.Active := False;
end;

procedure TASIOVSTModule.AHBufferSwitch(Sender: TObject; const InBuffer, OutBuffer: TAVDArrayOfSingleDynArray);
begin
 if (fIntWritePos > fIntReadPos) and (fIntWritePos < fIntReadPos + fASIOHost.BufferSize)
  then inc(fBufferUnderruns);
 Move(fInBuffer[0, fIntReadPos], OutBuffer[0, 0], fASIOHost.BufferSize * SizeOf(Single));
 Move(fInBuffer[1, fIntReadPos], OutBuffer[1, 0], fASIOHost.BufferSize * SizeOf(Single));
 Move(InBuffer[0, 0], fOutBuffer[0, fIntReadPos], fASIOHost.BufferSize * SizeOf(Single));
 Move(InBuffer[1, 0], fOutBuffer[1, fIntReadPos], fASIOHost.BufferSize * SizeOf(Single));
 fIntReadPos := (fIntReadPos + fASIOHost.BufferSize) mod (fNrOfBuffers * fASIOHost.BufferSize);
end;

procedure TASIOVSTModule.AHShortCircuit(Sender: TObject; const InBuffer, OutBuffer: TAVDArrayOfSingleDynArray);
begin
 if (fIntWritePos > fIntReadPos) and (fIntWritePos < fIntReadPos + fASIOHost.BufferSize)
  then inc(fBufferUnderruns);
 Move(fInBuffer[0, fIntReadPos], fOutBuffer[0, fIntReadPos], fASIOHost.BufferSize * SizeOf(Single));
 Move(fInBuffer[1, fIntReadPos], fOutBuffer[0, fIntReadPos], fASIOHost.BufferSize * SizeOf(Single));
 fIntReadPos := (fIntReadPos + fASIOHost.BufferSize) mod (fNrOfBuffers * fASIOHost.BufferSize);
end;

procedure TASIOVSTModule.AHLatencyChanged(Sender: TObject);
begin
 fIntBufSize := fASIOHost.BufferSize * fNrOfBuffers;
 SetLength(fInBuffer[0],  fIntBufSize);
 SetLength(fInBuffer[1],  fIntBufSize);
 SetLength(fOutBuffer[0], fIntBufSize);
 SetLength(fOutBuffer[1], fIntBufSize);
 fIntReadPos := (fNrOfBuffers div 2) * fASIOHost.BufferSize;
 fIntWritePos := 0;
end;

procedure TASIOVSTModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var i, j : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   fInBuffer[0, fIntWritePos] := inputs[0, i];
   fInBuffer[1, fIntWritePos] := inputs[1, i];
   outputs[0, i] := fOutBuffer[0, fIntWritePos];
   outputs[1, i] := fOutBuffer[1, fIntWritePos];
   Inc(fIntWritePos);
   if fIntWritePos >= fIntBufSize then fIntWritePos := 0;
   j := 0;
   while (j < 500) and (fIntWritePos = fIntReadPos) and fASIOHost.Active do
    begin
     inc(j);
     Sleep(1);
    end;
  end;
end;

procedure TASIOVSTModule.ASIODriverDisplay(Sender: TObject; const Index: Integer; var PreDefined: String);
begin
 PreDefined := fASIOHost.DriverName;
end;

procedure TASIOVSTModule.ASIODriverChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if not assigned(fASIOHost) then Exit;
 if fASIOHost.DriverIndex = Round(Value) then Exit;
 fASIOHost.Active := False;
 fASIOHost.DriverIndex := Round(Value);
 InitialDelay := fASIOHost.InputLatency + fASIOHost.OutputLatency + fASIOHost.BufferSize;
 if Assigned(EditorForm) then
  with TFmASIOVST(EditorForm) do
   if CB_ASIO.ItemIndex <> fASIOHost.DriverIndex then
    begin
     CB_ASIO.ItemIndex := fASIOHost.DriverIndex;
     DisplayASIOInformation;
    end;
 fASIOHost.Active := True;
end;

end.
