unit PSDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  DAV_VSTPrograms, uPSRuntime;

type
  TVSTProcessSample = procedure (Channel : Integer; var Data : Double) of object;

  TPascalScriptDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VPSStoreChunk(Sender: TObject; const Index: Integer; const isPreset: Boolean);
    procedure VPSLoadChunk(Sender: TObject; const Index: Integer; const isPreset: Boolean);
  private
    FPascalScriptExecuter : TPSExec;
    FByteCode             : string;
    FScriptCode           : string;
    FVSTProcessSample     : TVSTProcessSample;
    procedure SetByteCode(const Value: string);
  public
    property ByteCode : string read FByteCode write SetByteCode;
    property ScriptCode : string read FScriptCode write FScriptCode;
  end;

implementation

{$R *.DFM}

uses
  PSGUI, DAV_VSTCustomModule;

procedure TPascalScriptDataModule.VSTModuleCreate(Sender: TObject);
begin
 FPascalScriptExecuter := TPSExec.Create;
end;

procedure TPascalScriptDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FPascalScriptExecuter);
end;

procedure TPascalScriptDataModule.VPSLoadChunk(Sender: TObject; const Index: Integer; const isPreset: Boolean);
var
  AtomName : Array [0..3] of Char;
  AtomSize : Integer;
begin
 with (Sender as TVstProgram).Chunk do
  begin
   Position := 0;
   if Size < 8 then exit;
   Read(AtomName, 4);
   Read(AtomSize, 4);
   if AtomSize > 0 then
    begin
     SetLength(FScriptCode, AtomSize);
     Read(FScriptCode[1], AtomSize);
     if Assigned(EditorForm) then
      with TFmPascalScript(EditorForm)
       do SynEdit.Lines.Text := ScriptCode;
    end;
  end;
end;

procedure TPascalScriptDataModule.VPSStoreChunk(Sender: TObject; const Index: Integer; const isPreset: Boolean);
var
  AtomName : Array [0..3] of Char;
  AtomSize : Integer;
begin
 with (Sender as TVstProgram).Chunk do
  begin
   Position := 0;
   AtomName := 'VPSS';
   Write(AtomName, 4);
   AtomSize := Length(FScriptCode);
   Write(AtomSize, 4);
   Write(FScriptCode[1], AtomSize);
  end;
end;

procedure TPascalScriptDataModule.SetByteCode(const Value: string);
begin
 if FByteCode <> Value then
  begin
   FByteCode := Value;
   try
    if FPascalScriptExecuter.LoadData(FByteCode)
     then FVSTProcessSample := TVSTProcessSample(FPascalScriptExecuter.GetProcAsMethodN('VSTProcessSample'));
   except
    FVSTProcessSample := nil;
   end;
  end;
end;

procedure TPascalScriptDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmPascalScript.Create(Self);
 if Length(FScriptCode) > 0 then
  with TFmPascalScript(GUI)
   do SynEdit.LineText := FScriptCode;
end;

procedure TPascalScriptDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i : Integer;
  d : Double;
begin
 if @FVSTProcessSample <> nil then
  for i := 0 to SampleFrames - 1 do
   begin
    d := Inputs[0, i]; FVSTProcessSample(0, d); Outputs[0, i] := d;
    d := Inputs[1, i]; FVSTProcessSample(1, d); Outputs[1, i] := d;
   end
 else
  begin
   Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
   Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
  end;
end;

procedure TPascalScriptDataModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var i : Integer;
begin
 Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
 Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
 if @FVSTProcessSample <> nil then
  for i := 0 to SampleFrames - 1 do
   begin
    FVSTProcessSample(0, Outputs[0, i]);
    FVSTProcessSample(1, Outputs[1, i]);
   end;
end;

end.
