unit PSDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_VSTPrograms, uPSRuntime;

type
  TVSTProcessSample = procedure (Channel : Integer; var Data : Double) of object;

  TPascalScriptDataModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VPSStoreChunk(Sender: TObject; const Index: Integer; const isPreset: Boolean);
    procedure VPSLoadChunk(Sender: TObject; const Index: Integer; const isPreset: Boolean);
  private
    fPascalScriptExecuter : TPSExec;
    fByteCode             : string;
    fScriptCode           : string;
    fVSTProcessSample     : TVSTProcessSample;
    procedure SetByteCode(const Value: string);
  public
    property ByteCode : string read fByteCode write SetByteCode;
    property ScriptCode : string read fScriptCode write fScriptCode;
  end;

implementation

{$R *.DFM}

uses
  PSGUI, DAV_VSTCustomModule;

procedure TPascalScriptDataModule.VSTModuleCreate(Sender: TObject);
begin
 fPascalScriptExecuter := TPSExec.Create;
end;

procedure TPascalScriptDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(fPascalScriptExecuter);
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
     SetLength(fScriptCode, AtomSize);
     Read(fScriptCode[1], AtomSize);
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
   AtomSize := Length(fScriptCode);
   Write(AtomSize, 4);
   Write(fScriptCode[1], AtomSize);
  end;
end;

procedure TPascalScriptDataModule.SetByteCode(const Value: string);
begin
 if fByteCode <> Value then
  begin
   fByteCode := Value;
   try
    if fPascalScriptExecuter.LoadData(fByteCode)
     then fVSTProcessSample := TVSTProcessSample(fPascalScriptExecuter.GetProcAsMethodN('VSTProcessSample'));
   except
    fVSTProcessSample := nil;
   end;
  end;
end;

procedure TPascalScriptDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmPascalScript.Create(Self);
 if Length(fScriptCode) > 0 then
  with TFmPascalScript(GUI)
   do SynEdit.LineText := fScriptCode;
end;

procedure TPascalScriptDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i : Integer;
  d : Double;
begin
 if @fVSTProcessSample <> nil then
  for i := 0 to SampleFrames - 1 do
   begin
    d := Inputs[0, i]; fVSTProcessSample(0, d); Outputs[0, i] := d;
    d := Inputs[1, i]; fVSTProcessSample(1, d); Outputs[1, i] := d;
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
 if @fVSTProcessSample <> nil then
  for i := 0 to SampleFrames - 1 do
   begin
    fVSTProcessSample(0, Outputs[0, i]);
    fVSTProcessSample(1, Outputs[1, i]);
   end;
end;

end.
