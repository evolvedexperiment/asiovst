unit DitherNoiseshaperGUI;

interface

uses
  Windows, Messages, Classes, Forms, Controls, StdCtrls, Spin, DAV_Common,
  DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiDial;

type
  TFmDitherNoiseshaper = class(TForm)
    CbLimit: TCheckBox;
    LbBit: TLabel;
    LbFinalBitDepth: TLabel;
    LbNoiseshaperType: TLabel;
    SeBitDepth: TSpinEdit;
    LbDitherType: TLabel;
    CbDitherType: TComboBox;
    LbDitherAmp: TLabel;
    DialAmplitude: TGuiDial;
    SbFrequency: TScrollBar;
    procedure FormShow(Sender: TObject);
    procedure CbLimitClick(Sender: TObject);
    procedure SeBitDepthChange(Sender: TObject);
    procedure CbDitherTypeChange(Sender: TObject);
    procedure DialAmplitudeChange(Sender: TObject);
    procedure SbFrequencyChange(Sender: TObject);
  public
    procedure UpdateBitDepth;
    procedure UpdateLimit;
    procedure UpdateDitherType;
    procedure UpdateDitherAmplitude;
    procedure UpdateNoiseShaperFrequency;
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_Approximations, DitherNoiseshaperDM;

procedure TFmDitherNoiseshaper.FormShow(Sender: TObject);
begin
 UpdateBitDepth;
 UpdateLimit;
 UpdateDitherType;
 UpdateDitherAmplitude;
 UpdateNoiseShaperFrequency;
end;

procedure TFmDitherNoiseshaper.SeBitDepthChange(Sender: TObject);
begin
 with TDitherNoiseshaperModule(Owner) do
  begin
   if Parameter[0] <> SeBitDepth.Value
    then Parameter[0] := SeBitDepth.Value;
  end;
end;

procedure TFmDitherNoiseshaper.CbLimitClick(Sender: TObject);
begin
 with TDitherNoiseshaperModule(Owner) do
  begin
   if Parameter[1] <> Integer(CbLimit.Checked)
    then Parameter[1] := Integer(CbLimit.Checked);
  end;
end;

procedure TFmDitherNoiseshaper.CbDitherTypeChange(Sender: TObject);
begin
 with TDitherNoiseshaperModule(Owner) do
  begin
   if Parameter[2] <> CbDitherType.ItemIndex
    then Parameter[2] := CbDitherType.ItemIndex;
  end;
end;

procedure TFmDitherNoiseshaper.DialAmplitudeChange(Sender: TObject);
begin
 with TDitherNoiseshaperModule(Owner) do
  begin
   if Parameter[3] <> DialAmplitude.Position
    then Parameter[3] := DialAmplitude.Position;
  end;
end;

procedure TFmDitherNoiseshaper.SbFrequencyChange(Sender: TObject);
var
  Frequency : Single;
begin
 with TDitherNoiseshaperModule(Owner) do
  begin
   Frequency := (exp(0.001 * SbFrequency.Position * ln(100 + 1)) - 1) * 0.01;
   Frequency := Frequency * 19800 + 200;
   if Parameter[4] <> Frequency
    then Parameter[4] := Frequency;
  end;
end;

procedure TFmDitherNoiseshaper.UpdateBitDepth;
begin
 with TDitherNoiseshaperModule(Owner) do
  begin
   if SeBitDepth.Value <> Round(Parameter[0])
    then SeBitDepth.Value := Round(Parameter[0])
  end;
end;

procedure TFmDitherNoiseshaper.UpdateLimit;
begin
 with TDitherNoiseshaperModule(Owner) do
  begin
   if CbLimit.Checked <> Boolean(Round(Parameter[1]))
    then CbLimit.Checked := Boolean(Round(Parameter[1]))
  end;
end;

procedure TFmDitherNoiseshaper.UpdateDitherType;
begin
 with TDitherNoiseshaperModule(Owner) do
  begin
   if CbDitherType.ItemIndex <> Round(Parameter[2])
    then CbDitherType.ItemIndex := Round(Parameter[2])
  end;
end;

procedure TFmDitherNoiseshaper.UpdateDitherAmplitude;
begin
 with TDitherNoiseshaperModule(Owner) do
  begin
   if DialAmplitude.Position <> Parameter[3]
    then DialAmplitude.Position := Parameter[3]
  end;
end;

procedure TFmDitherNoiseshaper.UpdateNoiseShaperFrequency;
var
  Val   : Single;
  SbPos : Integer;
begin
 with TDitherNoiseshaperModule(Owner) do
  begin
   Val := (Parameter[4] - 200) / 19800;
   Val := log2(100 * Val + 1) / log2(100 + 1);
   SbPos := round(1000 * Val);
   if SbFrequency.Position <> SbPos
    then SbFrequency.Position := SbPos;
  end;
end;

end.