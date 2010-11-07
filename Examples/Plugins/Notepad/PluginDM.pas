unit PluginDM;

interface

uses
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Forms, DAV_ChunkClasses, DAV_Types,
  DAV_VSTModule;

type
  TTextChunk = class(TCustomTextChunk)
  public
    class function GetClassChunkName : TChunkName; override;
  end;

  TPluginDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleAfterProgramChange(Sender: TObject);
  private
    FChunk : TTextChunk;
    function GetText: AnsiString;
    procedure SetText(const Value: AnsiString);
  public
    property Text: AnsiString read GetText write SetText;
  end;

implementation

{$R *.DFM}

uses
  Editor;


{ TTextChunk }

class function TTextChunk.GetClassChunkName: TChunkName;
begin
 Result := 'text';
end;


{ TPluginDataModule }

procedure TPluginDataModule.VSTModuleOpen(Sender: TObject);
begin
 // set editor form class
 EditorFormClass := TFmNotepad;

 // create custom chunk
 FChunk := TTextChunk.Create;
end;

procedure TPluginDataModule.VSTModuleClose(Sender: TObject);
begin
 // free custom chunk
 FreeAndNil(FChunk);
end;

function TPluginDataModule.GetText: AnsiString;
begin
 with Programs[CurrentProgram] do
  begin
   // locate the beginning of the chunk
   Chunk.Seek(0, soFromBeginning);

   // check if chunk is valid
   if Chunk.Size > 8
    then FChunk.LoadFromStream(Chunk)
    else FChunk.Text := '';

   // return text
   Result := string(FChunk.Text);
  end;
end;

procedure TPluginDataModule.SetText(const Value: AnsiString);
begin
 with Programs[CurrentProgram] do
  begin
   // locate the beginning of the chunk
   Chunk.Seek(0, soFromBeginning);

   // assign text
   FChunk.Text := Value;

   // save text to stream
   FChunk.SaveToStream(Chunk)
  end;
end;

procedure TPluginDataModule.VSTModuleAfterProgramChange(Sender: TObject);
begin
 // change notepad display
 if EditorForm is TFmNotepad then
  with TFmNotepad(EditorForm)
   do MeNotepad.Lines.Text := Text;
end;

end.
