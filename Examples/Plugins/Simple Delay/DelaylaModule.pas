unit DelaylaModule;

interface

uses Windows, Messages, SysUtils, Classes, Forms, DDSPBase, DVSTModule;

type
  TSimpleDelayVST = class(TVSTModule)
    procedure VST_EditOpen(Sender: TObject; var GUI: TForm);
    procedure VST2ModuleCreate(Sender: TObject);
    procedure VST2ModuleProcess(const inputs, outputs: TArrayOfSingleArray;
      sampleframes: Integer);
    procedure DelaylaVSTParameterProperties0ParameterChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
    fBuffer     : array[0..1] of TSingleArray;
    fBufferSize : Integer;
    fBufferPos  : Integer;
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

uses DelaylaGUI;

procedure TSimpleDelayVST.VST_EditOpen(Sender: TObject; var GUI: TForm);
// Do not delete this if you are using the editor
begin
 GUI := TVSTGUI.Create(nil);
 (GUI As TVSTGUI).theModule:=Self;
end;

procedure TSimpleDelayVST.VST2ModuleCreate(Sender: TObject);
begin
 Parameter[0]:=441;
 fBufferPos:=0;
end;

procedure TSimpleDelayVST.VST2ModuleProcess(const inputs,
  outputs: TArrayOfSingleArray; sampleframes: Integer);
var j : Integer;
begin
 for j:=0 to sampleframes-1 do
  begin
   outputs[0,j]:=inputs[0,j]+fBuffer[0,fBufferPos];
   outputs[1,j]:=inputs[1,j]+fBuffer[1,fBufferPos];
   fBuffer[0,fBufferPos]:=inputs[0,j];
   fBuffer[1,fBufferPos]:=inputs[1,j];
   Inc(fBufferPos);
   if fBufferPos>=fBufferSize
    then fBufferPos:=0;
   end;
end;

procedure TSimpleDelayVST.DelaylaVSTParameterProperties0ParameterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fBufferSize:=round(Value);
 SetLength(fBuffer[0],fBufferSize);
 SetLength(fBuffer[1],fBufferSize);
 if fBufferPos>=fBufferSize
  then fBufferPos:=0;
end;

end.