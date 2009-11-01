unit SortedDisorderDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, SyncObjs, DAV_Types,
  DAV_VSTModule, DAV_DspBlockDistribution;

type
  TSortedDisorderModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray;
      const SampleFrames: Integer);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure ParameterBlocksizeChange(
      Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFilterOrderChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
    FCriticalSection : TCriticalSection;
    FBuildingBuffer  : array of TBlockDisorder32;
  public
  end;

implementation

{$R *.DFM}

uses
  SortedDisorderGui;

procedure TSortedDisorderModule.VSTModuleCreate(Sender: TObject);
begin
 Assert(numInputs = numOutputs);
 FCriticalSection := TCriticalSection.Create;
end;

procedure TSortedDisorderModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TSortedDisorderModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 SetLength(FBuildingBuffer, numInputs);
 for Channel := 0 to Length(FBuildingBuffer) - 1 do
  begin
   FBuildingBuffer[Channel] := TBlockDisorder32.Create;
   FBuildingBuffer[Channel].BlockSize := 1 shl 8;
  end;

 Parameter[0] := 256;
end;

procedure TSortedDisorderModule.ParameterBlocksizeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FBuildingBuffer) - 1
   do FBuildingBuffer[Channel].BlockSize := 2 * Round(0.5 * Value);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSortedDisorderModule.ParameterFilterOrderChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to Length(FBuildingBuffer) - 1
   do FBuildingBuffer[Channel].FilterOrder := Round(Value);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSortedDisorderModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FBuildingBuffer) - 1
  do FreeAndNil(FBuildingBuffer[Channel]);
end;

procedure TSortedDisorderModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmSortedDisorder.Create(Self);
end;

procedure TSortedDisorderModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel : Integer;
  Sample  : Integer;
begin
 FCriticalSection.Enter;
 try
(*
  for Channel := 0 to Length(FBuildingBuffer) - 1
   do FBuildingBuffer[Channel].ProcessBlock32(@Inputs[Channel, 0],
    @Outputs[Channel, 0], SampleFrames * SizeOf(Single));
*)
  for Channel := 0 to Length(FBuildingBuffer) - 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FBuildingBuffer[Channel].ProcessSample32(Inputs[Channel, Sample]);
 finally
  FCriticalSection.Leave;
 end;
end;

end.
