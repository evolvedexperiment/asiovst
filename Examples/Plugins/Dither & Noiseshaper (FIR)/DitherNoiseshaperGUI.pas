unit DitherNoiseshaperGUI;

interface

uses
  Windows, Messages, Classes, Forms, Controls, StdCtrls, Spin, DAV_Common,
  DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiDial;

type
  TFmDitherNoiseshaper = class(TForm)
    CbLimit: TCheckBox;
    CbNoiseshaperType: TComboBox;
    LbBit: TLabel;
    LbFinalBitDepth: TLabel;
    LbNoiseshaperType: TLabel;
    SeBitDepth: TSpinEdit;
    LbDitherType: TLabel;
    CbDitherType: TComboBox;
    LbDitherAmp: TLabel;
    DialAmplitude: TGuiDial;
    procedure FormShow(Sender: TObject);
    procedure CbLimitClick(Sender: TObject);
    procedure CbNoiseshaperTypeChange(Sender: TObject);
    procedure SeBitDepthChange(Sender: TObject);
    procedure CbDitherTypeChange(Sender: TObject);
    procedure DialAmplitudeChange(Sender: TObject);
  public
    procedure UpdateBitDepth;
    procedure UpdateLimit;
    procedure UpdateDitherType;
    procedure UpdateDitherAmplitude;
    procedure UpdateNoiseShaper;
  end;

implementation

{$R *.DFM}

uses
  DitherNoiseshaperDM;

procedure TFmDitherNoiseshaper.FormShow(Sender: TObject);
begin
 UpdateBitDepth;
 UpdateLimit;
 UpdateDitherType;
 UpdateDitherAmplitude;
 UpdateNoiseShaper;
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

procedure TFmDitherNoiseshaper.CbNoiseshaperTypeChange(Sender: TObject);
begin
 with TDitherNoiseshaperModule(Owner) do
  begin
   if Parameter[4] <> CbNoiseshaperType.ItemIndex
    then Parameter[4] := CbNoiseshaperType.ItemIndex;
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

procedure TFmDitherNoiseshaper.UpdateNoiseShaper;
begin
 with TDitherNoiseshaperModule(Owner) do
  begin
   if CbNoiseshaperType.ItemIndex <> Round(Parameter[4])
    then CbNoiseshaperType.ItemIndex := Round(Parameter[4])
  end;
end;

end.
