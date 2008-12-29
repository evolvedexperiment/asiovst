unit DelaylaModule;

interface

uses
  Windows, Types, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule;

type
  TSimpleDelayVST = class(TVSTModule)
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure SDDelayLengthChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleOpen(Sender: TObject);
  private
    FBuffer     : array[0..1] of TDAVSingleDynArray;
    FBufferSize : Integer;
    FBufferPos  : Integer;
  end;

implementation

{$R *.DFM}

uses
  DelaylaGUI;

procedure TSimpleDelayVST.VSTModuleOpen(Sender: TObject);
begin
 FBufferPos := 0;
 Parameter[0] := 441;
end;

procedure TSimpleDelayVST.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TVSTGUI.Create(Self);
end;

procedure TSimpleDelayVST.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  j : Integer;
begin
 for j := 0 to SampleFrames - 1 do
  begin
   Outputs[0, j] := Inputs[0, j] + FBuffer[0, FBufferPos];
   Outputs[1, j] := Inputs[1, j] + FBuffer[1, FBufferPos];
   FBuffer[0, FBufferPos] := Inputs[0, j];
   FBuffer[1, FBufferPos] := Inputs[1, j];
   Inc(FBufferPos);
   if FBufferPos>=FBufferSize
    then FBufferPos := 0;
   end;
end;

procedure TSimpleDelayVST.SDDelayLengthChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 FBufferSize := round(Value);
 SetLength(FBuffer[0], FBufferSize);
 SetLength(FBuffer[1], FBufferSize);
 if FBufferPos >= FBufferSize
  then FBufferPos := 0;
end;

end.
