unit ChunkDemoGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_GuiLabel, Controls, DAV_GuiBaseControl, DAV_GuiDial;

type
  TFmChunkDemo = class(TForm)
    DialAlpha: TGuiDial;
    LbAlpha: TGuiLabel;
    LbBeta: TGuiLabel;
    LbGamma: TGuiLabel;
    LbDelta: TGuiLabel;
    DialBeta: TGuiDial;
    DialGamma: TGuiDial;
    DialDelta: TGuiDial;
    DIL: TGuiDialImageList;
    procedure FormShow(Sender: TObject);
    procedure DialAlphaChange(Sender: TObject);
    procedure DialBetaChange(Sender: TObject);
    procedure DialGammaChange(Sender: TObject);
    procedure DialDeltaChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  public
    procedure UpdateAlpha;
    procedure UpdateBeta;
    procedure UpdateGamma;
    procedure UpdateDelta;
  end;

implementation

uses
  ChunkDemoDM, PngImage;

{$R *.DFM}

{ TFmChunkDemo }

procedure TFmChunkDemo.DialAlphaChange(Sender: TObject);
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Parameter[0] := DialAlpha.Position;
  end;
end;

procedure TFmChunkDemo.DialBetaChange(Sender: TObject);
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Parameter[1] := DialBeta.Position;
  end;
end;

procedure TFmChunkDemo.DialGammaChange(Sender: TObject);
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Parameter[2] := DialGamma.Position;
  end;
end;

procedure TFmChunkDemo.DialDeltaChange(Sender: TObject);
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Parameter[3] := DialDelta.Position;
  end;
end;

procedure TFmChunkDemo.FormCreate(Sender: TObject);
var
  RS       : TResourceStream;
  PngBmp   : TPngObject;
(*
  x, y     : Integer;
  ScanLine : array [0..1] of PByteArray;
  b        : Byte;
*)
begin
 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'ChunkDemoKnob', 'PNG');
  try

   PngBmp.LoadFromStream(RS);

   with DIL.DialImages.Add do
    begin
     DialBitmap.Assign(PngBmp);
     NumGlyphs := 65;
    end;

(*
   for y := 0 to (Height div 2) - 1 do
    begin
     ScanLine[0] := PngBmp.Scanline[y];
     ScanLine[1] := PngBmp.Scanline[(Height div 2)];
     for x := 0 to (3 * Width) - 1 do
      begin
       b := ScanLine[0]^[x];
       ScanLine[0]^[x] := ScanLine[1]^[(3 * Width) - 1 - x];
       ScanLine[1]^[(3 * Width) - 1 - x] := b;
      end;
    end;

   with DIL.DialImages.Add do
    begin
     DialBitmap.Assign(PngBmp);
     NumGlyphs := 65;
    end;
*)
   DialAlpha.DialImageIndex := 0;
   DialGamma.DialImageIndex := 0;
   DialBeta.DialImageIndex := 0;
   DialDelta.DialImageIndex := 0;
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmChunkDemo.FormShow(Sender: TObject);
begin
 UpdateAlpha;
 UpdateBeta;
 UpdateGamma;
 UpdateDelta;
end;

procedure TFmChunkDemo.UpdateAlpha;
var
  Alpha : Single;
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Alpha := Parameter[0];
   if DialAlpha.Position <> Alpha
    then DialAlpha.Position := Alpha;
  end;
end;

procedure TFmChunkDemo.UpdateBeta;
var
  Beta : Single;
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Beta := Parameter[1];
   if DialBeta.Position <> Beta
    then DialBeta.Position := Beta;
  end;
end;

procedure TFmChunkDemo.UpdateDelta;
var
  Delta : Single;
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Delta := Parameter[2];
   if DialGamma.Position <> Delta
    then DialGamma.Position := Delta;
  end;
end;

procedure TFmChunkDemo.UpdateGamma;
var
  Gamma : Single;
begin
 with TChunkDemoDataModule(Owner) do
  begin
   Gamma := Parameter[3];
   if DialDelta.Position <> Gamma
    then DialDelta.Position := Gamma;
  end;
end;

end.