unit DAV_ChunkPluginGUI;

interface

{$I DAV_Compiler.inc}

uses
  Classes, Graphics, DAV_Common, DAV_ChunkClasses, DAV_GuiBaseControl;

type
  TDAVPluginGuiChunkRecord = packed record
    BackgroundColor  : TColor;
    KnobsPerRow      : Byte;
    FontAntiAliasing : Byte;
    FontSize         : Byte;
    FontColor        : TColor;
  end;

  TDAVPluginGuiChunk = class(TFixedDefinedChunk)
  private
    function GetKnobsPerRow: TGuiAntiAlias;
    procedure SetFontAntiAliasing(const Value: TGuiAntiAlias);
  protected
    FPluginGuiChunkRecord : TDAVPluginGuiChunkRecord;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    class function GetClassChunkSize: Integer; override;
    class function GetClassChunkName : TChunkName; override;
  published
    property BackgroundColor: TColor read FPluginGuiChunkRecord.BackgroundColor write FPluginGuiChunkRecord.BackgroundColor;
    property KnobsPerRow: Byte read FPluginGuiChunkRecord.KnobsPerRow write FPluginGuiChunkRecord.KnobsPerRow;
    property FontAntiAliasing: TGuiAntiAlias read GetKnobsPerRow write SetFontAntiAliasing;
    property FontSize: Byte read FPluginGuiChunkRecord.FontSize write FPluginGuiChunkRecord.FontSize;
    property FontColor: TColor read FPluginGuiChunkRecord.FontColor write FPluginGuiChunkRecord.FontColor;
  end;

implementation

{ TDAVPluginGuiChunk }

constructor TDAVPluginGuiChunk.Create;
begin
 inherited;
 StartAddress := @FPluginGuiChunkRecord;
 with FPluginGuiChunkRecord do
  begin
   BackgroundColor  := $007F7F7F;
   KnobsPerRow      := 6;
   FontAntiAliasing := 0;
   FontSize         := 8;
  end;
end;

procedure TDAVPluginGuiChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TDAVPluginGuiChunk
  then TDAVPluginGuiChunk(Dest).FPluginGuiChunkRecord := FPluginGuiChunkRecord; 
end;

class function TDAVPluginGuiChunk.GetClassChunkName: TChunkName;
begin
 result := 'PGUI';
end;

class function TDAVPluginGuiChunk.GetClassChunkSize: Integer;
begin
 result := SizeOf(TDAVPluginGuiChunkRecord);
end;

function TDAVPluginGuiChunk.GetKnobsPerRow: TGuiAntiAlias;
begin
 result := TGuiAntiAlias(FPluginGuiChunkRecord.FontAntiAliasing);
end;

procedure TDAVPluginGuiChunk.SetFontAntiAliasing(const Value: TGuiAntiAlias);
begin
 FPluginGuiChunkRecord.FontAntiAliasing := Byte(Value);
end;

end.
