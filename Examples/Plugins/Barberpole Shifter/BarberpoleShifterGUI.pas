unit BarberpoleShifterGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, Controls, ExtCtrls, StdCtrls,
  Graphics, DAV_Common, DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiDial,
  DAV_GuiLabel, DAV_GuiPanel, DAV_GuiGroup;

type
  TFmBarberpoleShifter = class(TForm)
    GpFrequency: TGuiGroup;
    DialFrequency: TGuiDial;
    PnDisplay: TGuiPanel;
    LbFrequencyValue: TGuiLabel;
    GpMix: TGuiGroup;
    DialMix: TGuiDial;
    PnMix: TGuiPanel;
    LbMixValue: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialFrequencyChange(Sender: TObject);
    procedure DialMixChange(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
  public
    procedure UpdateFrequency;
    procedure UpdateMix;
  end;

implementation

{$R *.DFM}

uses
  PngImage, DAV_GuiCommon, BarberpoleShifterDM, DAV_VSTModuleWithPrograms;

{ TFmBarberpoleShifter }

procedure TFmBarberpoleShifter.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  PngBmp : TPngObject;

begin
 // Create Background Image
 FBackgrounBitmap := TBitmap.Create;
 FBackgrounBitmap.PixelFormat := pf24bit;

 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'BarberpoleShifter', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialFrequency.DialBitmap.Assign(PngBmp);
   DialMix.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmBarberpoleShifter.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array[0..1] of Single;
  h, hr  : Single;
  Line   : PRGB24Array;
begin
 with FBackgrounBitmap do
  begin
   PixelFormat := pf24bit;
   Width := Self.Width;
   Height := Self.Height;
   s[0] := 0;
   s[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     Line := Scanline[y];
     h    := 0.1 * (1 - sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * random;
       s[0] := s[1];

       Line[x].B := round($70 - $34 * (s[1] - h));
       Line[x].G := round($84 - $48 * (s[1] - h));
       Line[x].R := round($8D - $50 * (s[1] - h));
      end;
    end;
  end;
end;

procedure TFmBarberpoleShifter.FormShow(Sender: TObject);
begin
 UpdateFrequency;
 UpdateMix;
end;

procedure TFmBarberpoleShifter.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmBarberpoleShifter.DialFrequencyChange(Sender: TObject);
begin
 with TBarberpoleShifterDataModule(Owner) do
  begin
   if Parameter[0] <> DialFrequency.Position
    then Parameter[0] := DialFrequency.Position;
  end;
end;

procedure TFmBarberpoleShifter.DialMixChange(Sender: TObject);
begin
 with TBarberpoleShifterDataModule(Owner) do
  begin
   if Parameter[1] <> DialMix.Position
    then Parameter[1] := DialMix.Position;
  end;
end;

procedure TFmBarberpoleShifter.UpdateFrequency;
begin
 with TBarberpoleShifterDataModule(Owner) do
  begin
   if DialFrequency.Position <> Parameter[0]
    then DialFrequency.Position := Parameter[0];
   LbFrequencyValue.Caption := ParameterDisplay[0] + 'Hz';
  end;
end;

procedure TFmBarberpoleShifter.UpdateMix;
begin
 with TBarberpoleShifterDataModule(Owner) do
  begin
   if DialMix.Position <> Parameter[1]
    then DialMix.Position := Parameter[1];
   LbMixValue.Caption := ParameterDisplay[1] + '%';
  end;
end;

end.
