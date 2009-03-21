unit ASIOVSTModule;

{$I DAV_Compiler.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_ASIOHost, DAV_Common,
  DAV_VSTModule;

type
  TASIOVSTModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure AHBufferSwitch(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleDynArray);
    procedure AHShortCircuit(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleDynArray);
    procedure ASIODriverChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ASIODriverDisplay(Sender: TObject; const Index: Integer; var PreDefined: String);
    procedure VSTModuleBlockSizeChange(Sender: TObject;
      const BlockSize: Integer);
  private
    FASIOHost        : TASIOHost;
    FInBuffer        : array of PDavSingleFixedArray;
    FOutBuffer       : array of PDavSingleFixedArray;
    FIntBufSize,
    FIntWritePos,
    FIntReadPos      : Integer;
    FNrOfBuffers     : Integer;
    FBufferUnderruns : Integer;
    procedure AHLatencyChanged(Sender: TObject);
    procedure InternalBufferSizeChanged;
  public
    property BufferUnderruns : Integer read FBufferUnderruns;
    property AsioHost : TASIOHost read FASIOHost;
  end;

implementation

{$R *.DFM}

uses
  Math, ASIOVSTGUI;

procedure TASIOVSTModule.VSTModuleOpen(Sender: TObject);
begin
 FNrOfBuffers := 2;
 FIntWritePos := 1;
 FIntReadPos := 0;
 SetLength(FInBuffer, 2);
 SetLength(FOutBuffer, 2);

 FASIOHost := TASIOHost.Create(Self);
 with FASIOHost do
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
  end;
 with ParameterProperties[0] do
  begin
   Min := 0;
   Max := FASIOHost.DriverList.Count - 1;
  end;

 AHLatencyChanged(Self);

 if Assigned(FASIOHost)
  then FASIOHost.Active := True;
end;

procedure TASIOVSTModule.VSTModuleClose(Sender: TObject);
begin
 if Assigned(FASIOHost)
  then FASIOHost.Active := False;

 FreeAndNil(FASIOHost);
end;

procedure TASIOVSTModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TFmASIOVST.Create(Self);
 with (GUI As TFmASIOVST) do
  try
   CB_ASIO.Items := FASIOHost.DriverList;
   DisplayASIOInformation;
  except
  end;
end;

procedure TASIOVSTModule.VSTModuleBlockSizeChange(Sender: TObject;
  const BlockSize: Integer);
begin
 InternalBufferSizeChanged;
end;

procedure TASIOVSTModule.InternalBufferSizeChanged;
begin
 if assigned(FASIOHost) then
  with FASIOHost do
   begin
    FIntBufSize := max(Integer(FASIOHost.BufferSize), BlockSize) * FNrOfBuffers;
    InitialDelay := InputLatency + OutputLatency + FIntBufSize;
   end
 else
  begin
   FIntBufSize := Integer(BlockSize) * FNrOfBuffers;
   InitialDelay := FIntBufSize;
  end;
 ReallocMem(FInBuffer[0],  FIntBufSize * SizeOf(Single));
 ReallocMem(FInBuffer[1],  FIntBufSize * SizeOf(Single));
 ReallocMem(FOutBuffer[0], FIntBufSize * SizeOf(Single));
 ReallocMem(FOutBuffer[1], FIntBufSize * SizeOf(Single));
 FIntWritePos := 0;
 FIntReadPos := FIntBufSize div 2;
end;

procedure TASIOVSTModule.AHLatencyChanged(Sender: TObject);
begin
 InternalBufferSizeChanged;
end;

procedure TASIOVSTModule.AHBufferSwitch(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleDynArray);
begin
 if (FIntWritePos > FIntReadPos) and (FIntWritePos < FIntReadPos + Integer(FASIOHost.BufferSize))
  then inc(FBufferUnderruns);
 Move(FInBuffer[0, FIntReadPos], OutBuffer[0, 0], FASIOHost.BufferSize * SizeOf(Single));
 Move(FInBuffer[1, FIntReadPos], OutBuffer[1, 0], FASIOHost.BufferSize * SizeOf(Single));
 Move(InBuffer[0, 0], FOutBuffer[0, FIntReadPos], FASIOHost.BufferSize * SizeOf(Single));
 Move(InBuffer[1, 0], FOutBuffer[1, FIntReadPos], FASIOHost.BufferSize * SizeOf(Single));
 FIntReadPos := (FIntReadPos + Integer(FASIOHost.BufferSize)) mod (FNrOfBuffers * Integer(FASIOHost.BufferSize));
end;

procedure TASIOVSTModule.AHShortCircuit(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleDynArray);
begin
 if (FIntWritePos > FIntReadPos) and (FIntWritePos < FIntReadPos + Integer(FASIOHost.BufferSize))
  then inc(FBufferUnderruns);
 Move(FInBuffer[0, FIntReadPos], FOutBuffer[0, FIntReadPos], FASIOHost.BufferSize * SizeOf(Single));
 Move(FInBuffer[1, FIntReadPos], FOutBuffer[0, FIntReadPos], FASIOHost.BufferSize * SizeOf(Single));
 FIntReadPos := (FIntReadPos + Integer(FASIOHost.BufferSize)) mod (FNrOfBuffers * Integer(FASIOHost.BufferSize));
end;

procedure TASIOVSTModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var i, j : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   FInBuffer[0, FIntWritePos] := Inputs[0, i];
   FInBuffer[1, FIntWritePos] := Inputs[1, i];
   Outputs[0, i] := FOutBuffer[0, FIntWritePos];
   Outputs[1, i] := FOutBuffer[1, FIntWritePos];
   Inc(FIntWritePos);
   if FIntWritePos >= FIntBufSize then FIntWritePos := 0;
   j := 0;
   while (j < 500) and (FIntWritePos = FIntReadPos) and FASIOHost.Active do
    begin
     inc(j);
     Sleep(1);
    end;
  end;
end;

procedure TASIOVSTModule.ASIODriverDisplay(Sender: TObject; const Index: Integer; var PreDefined: String);
begin
 PreDefined := FASIOHost.DriverName;
end;

procedure TASIOVSTModule.ASIODriverChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if not assigned(FASIOHost) then Exit;
 if FASIOHost.DriverIndex = Round(Value) then Exit;
 FASIOHost.Active := False;
 FASIOHost.DriverIndex := Round(Value);
 InitialDelay := FASIOHost.InputLatency + FASIOHost.OutputLatency + Integer(FASIOHost.BufferSize);
 if Assigned(EditorForm) then
  with TFmASIOVST(EditorForm) do
   if CB_ASIO.ItemIndex <> FASIOHost.DriverIndex then
    begin
     CB_ASIO.ItemIndex := FASIOHost.DriverIndex;
     DisplayASIOInformation;
    end;
 FASIOHost.Active := True;
end;

end.
