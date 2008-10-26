unit DAV_ChunkPluginGUI;

interface

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
    fPluginGuiChunkRecord : TDAVPluginGuiChunkRecord;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    class function GetClassChunkSize: Integer; override;
    class function GetClassChunkName : TChunkName; override;
  published
    property BackgroundColor: TColor read fPluginGuiChunkRecord.BackgroundColor write fPluginGuiChunkRecord.BackgroundColor;
    property KnobsPerRow: Byte read fPluginGuiChunkRecord.KnobsPerRow write fPluginGuiChunkRecord.KnobsPerRow;
    property FontAntiAliasing: TGuiAntiAlias read GetKnobsPerRow write SetFontAntiAliasing;
    property FontSize: Byte read fPluginGuiChunkRecord.FontSize write fPluginGuiChunkRecord.FontSize;
    property FontColor: TColor read fPluginGuiChunkRecord.FontColor write fPluginGuiChunkRecord.FontColor;
  end;

implementation

{ TDAVPluginGuiChunk }

constructor TDAVPluginGuiChunk.Create;
begin
 inherited;
 StartAddress := @fPluginGuiChunkRecord;
 with fPluginGuiChunkRecord do
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
  then TDAVPluginGuiChunk(Dest).fPluginGuiChunkRecord := fPluginGuiChunkRecord; 
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
 result := TGuiAntiAlias(fPluginGuiChunkRecord.FontAntiAliasing);
end;

procedure TDAVPluginGuiChunk.SetFontAntiAliasing(const Value: TGuiAntiAlias);
begin
 fPluginGuiChunkRecord.FontAntiAliasing := Byte(Value);
end;

end.
