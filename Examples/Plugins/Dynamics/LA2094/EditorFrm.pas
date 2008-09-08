unit EditorFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule, Controls,
  StdCtrls, DGuiPanel, DGuiLabel, DGuiBaseControl, DGuiDial, DGuiLED;

type
  TEditorForm = class(TForm)
    DialAttack: TGuiDial;
    DialInput: TGuiDial;
    DialKnee: TGuiDial;
    DialOutput: TGuiDial;
    DialRatio: TGuiDial;
    DialRelease: TGuiDial;
    LbAttack: TGuiLabel;
    LbFast: TGuiLabel;
    LbInput: TGuiLabel;
    LbInputValue: TLabel;
    LbKnee: TGuiLabel;
    LbKneeValue: TLabel;
    LbManufacturer: TLabel;
    LbOnOff: TGuiLabel;
    LbOutput: TGuiLabel;
    LbOutputValue: TLabel;
    LbRatioValue: TLabel;
    LbRatioX: TGuiLabel;
    LbRelease: TGuiLabel;
    LbSlow: TGuiLabel;
    LbTitle: TGuiLabel;
    LEDOnOff: TGuiLED;
    PnInputValue: TGuiPanel;
    PnKnee: TGuiPanel;
    PnOutputValue: TGuiPanel;
    PnRatio: TGuiPanel;
    procedure DialInputChange(Sender: TObject);
    procedure DialRatioChange(Sender: TObject);
    procedure DialAttackChange(Sender: TObject);
    procedure DialReleaseChange(Sender: TObject);
    procedure DialKneeChange(Sender: TObject);
    procedure DialOutputChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LEDOnOffClick(Sender: TObject);
  private
  public
    procedure UpdateOnOff;
    procedure UpdateInput;
    procedure UpdateOutput;
    procedure UpdateAttack;
    procedure UpdateRelease;
    procedure UpdateRatio;
    procedure UpdateKnee;
  end;

implementation

{$R *.DFM}

uses
  Math, LA2094DM;

procedure TEditorForm.FormCreate(Sender: TObject);
var
  RS  : TResourceStream;
begin
 RS := TResourceStream.Create(hInstance, 'PanKnob', 'BMP');
 try
  DialInput.DialBitmap.LoadFromStream(RS);   RS.Position := 0;
  DialOutput.DialBitmap.LoadFromStream(RS);  RS.Position := 0;
 finally
  RS.Free;
 end;
 RS := TResourceStream.Create(hInstance, 'RatioKnob', 'BMP');
 try
  DialRatio.DialBitmap.LoadFromStream(RS);   RS.Position := 0;
  DialKnee.DialBitmap.LoadFromStream(RS);  RS.Position := 0;
 finally
  RS.Free;
 end;
 RS := TResourceStream.Create(hInstance, 'AttackKnob', 'BMP');
 try
  DialAttack.DialBitmap.LoadFromStream(RS);
 finally
  RS.Free;
 end;
 RS := TResourceStream.Create(hInstance, 'ReleaseKnob', 'BMP');
 try
  DialRelease.DialBitmap.LoadFromStream(RS);
 finally
  RS.Free;
 end;
end;

procedure TEditorForm.FormShow(Sender: TObject);
begin
 UpdateOnOff;
 UpdateInput;
 UpdateOutput;
 UpdateAttack;
 UpdateRelease;
 UpdateRatio;
 UpdateKnee;
end;

procedure TEditorForm.LEDOnOffClick(Sender: TObject);
begin
 with TLA2094DataModule(Owner) do
  begin
   Parameter[0] := 1 - Parameter[0];
   UpdateOnOff;
  end;
end;

procedure TEditorForm.UpdateOnOff;
begin
 with TLA2094DataModule(Owner) do
  begin
   if Parameter[0] < 0.5
    then LEDOnOff.Brightness_Percent := 100
    else LEDOnOff.Brightness_Percent := 10;
  end;
end;

procedure TEditorForm.UpdateInput;
begin
 with TLA2094DataModule(Owner) do
  begin
   if DialInput.Position <> Parameter[1]
    then DialInput.Position := Parameter[1];
   LbInputValue.Caption := FloatToStrF(DialInput.Position, ffFixed, 3, 1) + ' dB';
  end;
end;

procedure TEditorForm.UpdateKnee;
begin
 with TLA2094DataModule(Owner) do
  begin
   if DialKnee.Position <> Parameter[6]
    then DialKnee.Position := Parameter[6];
   LbKneeValue.Caption := FloatToStrF(DialKnee.Position, ffFixed, 3, 1);
  end;
end;

procedure TEditorForm.UpdateOutput;
begin
 with TLA2094DataModule(Owner) do
  begin
   if DialOutput.Position <> Parameter[2]
    then DialOutput.Position := Parameter[2];
   LbOutputValue.Caption := FloatToStrF(DialOutput.Position, ffFixed, 3, 1) + ' dB';
  end;
end;

procedure TEditorForm.UpdateAttack;
var
  s : Single;
begin
 with TLA2094DataModule(Owner) do
  begin
   s := Log10(Parameter[3]);
   if DialAttack.Position <> s
    then DialAttack.Position := s;
//   LbAttackValue.Caption := FloatToStrF(Parameter[3], ffGeneral, 4, 2) + ' ms';
  end;
end;

procedure TEditorForm.UpdateRelease;
var
  s : Single;
begin
 with TLA2094DataModule(Owner) do
  begin
   s := Log10(Parameter[4]);
   if DialRelease.Position <> s
    then DialRelease.Position := s;
//   LbReleaseValue.Caption := FloatToStrF(Parameter[4], ffGeneral, 4, 5) + ' ms';
  end;
end;

procedure TEditorForm.UpdateRatio;
var
  s : Single;
begin
 with TLA2094DataModule(Owner) do
  begin
   s := Log10(Parameter[5]);
   if DialRatio.Position <> s
    then DialRatio.Position := s;
   LbRatioValue.Caption := '1 : ' + FloatToStrF(Parameter[5], ffFixed, 3, 1);
  end;
end;

procedure TEditorForm.DialInputChange(Sender: TObject);
begin
 with TLA2094DataModule(Owner) do
  begin
   if Parameter[1] <> DialInput.Position
    then Parameter[1] := DialInput.Position;
   UpdateInput;
  end;
end;

procedure TEditorForm.DialOutputChange(Sender: TObject);
begin
 with TLA2094DataModule(Owner) do
  begin
   if Parameter[2] <> DialOutput.Position
    then Parameter[2] := DialOutput.Position;
   UpdateOutput;
  end;
end;

procedure TEditorForm.DialAttackChange(Sender: TObject);
var
  s : Single;
begin
 with TLA2094DataModule(Owner) do
  begin
   s := Power(10, DialAttack.Position);
   Parameter[3] := s;
  end;
end;

procedure TEditorForm.DialReleaseChange(Sender: TObject);
var
  s : Single;
begin
 with TLA2094DataModule(Owner) do
  begin
   s := Power(10, DialRelease.Position);
   Parameter[4] := s;
  end;
end;

procedure TEditorForm.DialRatioChange(Sender: TObject);
var
  s : Single;
begin
 with TLA2094DataModule(Owner) do
  begin
   s := Power(10, DialRatio.Position);
   if abs (Parameter[5] - s) > 1E-3
    then Parameter[5] := s;
   UpdateRatio;
  end;
end;

procedure TEditorForm.DialKneeChange(Sender: TObject);
begin
 with TLA2094DataModule(Owner) do
  begin
   Parameter[6] := DialKnee.Position;
   UpdateKnee;
  end;
end;

end.
