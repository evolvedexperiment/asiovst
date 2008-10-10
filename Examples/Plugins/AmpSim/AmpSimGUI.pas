unit AmpSimGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, 
  StdCtrls, DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiSelectBox,
  DAV_GuiLED, ExtCtrls, DAV_GuiPanel;

type
  TFmCombo = class(TForm)
    LbModel: TGuiLabel;
    SBModel: TGuiSelectBox;
    GuiLED: TGuiLED;
    LbStereo: TGuiLabel;
    GuiPanel1: TGuiPanel;
    LbResonanceValue: TLabel;
    LbFrequencyValue: TLabel;
    LbOutputValue: TLabel;
    LbBiasValue: TLabel;
    LbDriveValue: TLabel;
    DialDrive: TGuiDial;
    LbDrive: TGuiLabel;
    DialBias: TGuiDial;
    LbBias: TGuiLabel;
    DialOutput: TGuiDial;
    DialFrequency: TGuiDial;
    DialResonance: TGuiDial;
    LbOutput: TGuiLabel;
    LbFrequency: TGuiLabel;
    LbResonance: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialDriveChange(Sender: TObject);
    procedure DialBiasChange(Sender: TObject);
    procedure DialOutputChange(Sender: TObject);
    procedure DialFreqChange(Sender: TObject);
    procedure DialResoChange(Sender: TObject);
    procedure SBModelChange(Sender: TObject);
    procedure LbStereoClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FBackground : TBitmap
  end;

var
  FmCombo: TFmCombo;

implementation

{$R *.dfm}

uses
  AmpSimDM;

procedure TFmCombo.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  x, y   : Integer;
  s      : array[0..1] of Single;
  h, hr  : Single;
  Line   : PRGB24Array;

begin
 RS := TResourceStream.Create(hInstance, 'AmpKnob', 'BMP');
 try
  DialDrive.DialBitmap.LoadFromStream(RS); RS.Position := 0;
  DialBias.DialBitmap.Assign(DialDrive.DialBitmap);
  DialOutput.DialBitmap.Assign(DialDrive.DialBitmap);
  DialFrequency.DialBitmap.Assign(DialDrive.DialBitmap);
  DialResonance.DialBitmap.Assign(DialDrive.DialBitmap);
 finally
  RS.Free;
 end;

 // Create Background Image
 FBackground := TBitmap.Create;
 with FBackground do
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
       s[1] := 0.97 * s[0] + 0.06 * random - 0.03;
       s[0] := s[1];

       Line[x].B := round($40 - $10 * (s[1] - h));
       Line[x].G := round($80 - $20 * (s[1] - h));
       Line[x].R := round($80 - $20 * (s[1] - h));
      end;
    end;
  end;
end;

procedure TFmCombo.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackground);
end;

procedure TFmCombo.SBModelChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[0] := SBModel.ItemIndex;
end;

procedure TFmCombo.DialBiasChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[2] := DialBias.Position;
end;

procedure TFmCombo.DialDriveChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[1] := DialDrive.Position;
end;

procedure TFmCombo.DialOutputChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[3] := DialOutput.Position;
end;

procedure TFmCombo.DialFreqChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[5] := DialFrequency.Position;
end;

procedure TFmCombo.DialResoChange(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 TComboDataModule(Owner).Parameter[6] := DialResonance.Position;
end;

procedure TFmCombo.FormShow(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 with TComboDataModule(Owner) do
  begin
   SBModel.ItemIndex := round(Parameter[0]);
  end;
end;

procedure TFmCombo.LbStereoClick(Sender: TObject);
begin
 Assert(Owner is TComboDataModule);
 with TComboDataModule(Owner) do
  begin
   Parameter[4] := 1 - Parameter[4];
  end;
end;

end.
