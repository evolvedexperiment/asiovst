unit DelaylaModule;

interface

uses
  Windows, Types, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule;

type
  TSimpleDelayVST = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure SDDelayLengthChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    fBuffer     : array[0..1] of TDAVSingleDynArray;
    fBufferSize : Integer;
    fBufferPos  : Integer;
  end;

implementation

{$R *.DFM}

uses
  DelaylaGUI;

procedure TSimpleDelayVST.VSTModuleCreate(Sender: TObject);
begin
 Parameter[0] := 441;
 fBufferPos := 0;
end;

procedure TSimpleDelayVST.VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  j : Integer;
begin
 for j := 0 to SampleFrames - 1 do
  begin
   Outputs[0, j] := Inputs[0, j] + fBuffer[0, fBufferPos];
   Outputs[1, j] := Inputs[1, j] + fBuffer[1, fBufferPos];
   fBuffer[0, fBufferPos] := Inputs[0, j];
   fBuffer[1, fBufferPos] := Inputs[1, j];
   Inc(fBufferPos);
   if fBufferPos>=fBufferSize
    then fBufferPos := 0;
   end;
end;

procedure TSimpleDelayVST.VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
  ParentWindow: Cardinal);
begin
 GUI := TVSTGUI.Create(Self);
end;

procedure TSimpleDelayVST.SDDelayLengthChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fBufferSize := round(Value);
 SetLength(fBuffer[0], fBufferSize);
 SetLength(fBuffer[1], fBufferSize);
 if fBufferPos >= fBufferSize
  then fBufferPos := 0;
end;

end.
