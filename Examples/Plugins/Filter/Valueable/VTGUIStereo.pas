unit VTGUIStereo;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  Controls, ExtCtrls, StdCtrls, DAV_GuiPanel, DAV_GuiBaseControl, DAV_GuiDial,
  DAV_GuiLabel;

type
  TFmVT = class(TForm)
    DialChannel: TGuiDial;
    DialHiBypassLeft: TGuiDial;
    DialHiBypassRight: TGuiDial;
    DialHiGainLeft: TGuiDial;
    DialHiGainRight: TGuiDial;
    DialLowBypassLeft: TGuiDial;
    DialLowBypassRight: TGuiDial;
    DialLowGainLeft: TGuiDial;
    DialLowGainRight: TGuiDial;
    DialOutputGain: TGuiDial;
    DialSelector: TGuiDial;
    GuiPanel: TGuiPanel;
    LbBassFlatLeft: TGuiLabel;
    LbBassFlatRight: TGuiLabel;
    LbBassLeft: TGuiLabel;
    LbBassRight: TGuiLabel;
    LbChannel: TGuiLabel;
    LbDrive: TGuiLabel;
    LbGain: TGuiLabel;
    LbGainValue: TGuiLabel;
    LbMono: TGuiLabel;
    LbRoasty1: TGuiLabel;
    LbRoasty2: TGuiLabel;
    LbSteamin1: TGuiLabel;
    LbSteamin2: TGuiLabel;
    LbStereo: TGuiLabel;
    LbSubTitle: TGuiLabel;
    LbTitle: TGuiLabel;
    LbTrebleFlatLeft: TGuiLabel;
    LbTrebleFlatRight: TGuiLabel;
    LbTrebleLeft: TGuiLabel;
    LbTrebleRight: TGuiLabel;
    LbLowGainLeftLower: TGuiLabel;
    LbHiGainLeftLower: TGuiLabel;
    LbLowGainRightLower: TGuiLabel;
    LbHiGainRightLower: TGuiLabel;
    LbHiGainLeftUpper: TGuiLabel;
    GuiLabel1: TGuiLabel;
    GuiLabel2: TGuiLabel;
    GuiLabel3: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialLowGainLeftChange(Sender: TObject);
    procedure DialHiGainRightChange(Sender: TObject);
    procedure DialLowBypassLeftChange(Sender: TObject);
    procedure DialHiBypassRightChange(Sender: TObject);
    procedure DialSelectorChange(Sender: TObject);
    procedure LbRoasty2Click(Sender: TObject);
    procedure LbRoasty1Click(Sender: TObject);
    procedure LbSteamin1Click(Sender: TObject);
    procedure LbSteamin2Click(Sender: TObject);
    procedure DialOutputGainChange(Sender: TObject);
    procedure DialChannelChange(Sender: TObject);
    procedure DialLowGainRightChange(Sender: TObject);
    procedure DialHiGainLeftChange(Sender: TObject);
    procedure DialHiBypassLeftChange(Sender: TObject);
    procedure DialLowBypassRightChange(Sender: TObject);
  public
    procedure UpdateBassGainLeft;
    procedure UpdateTrebleGainLeft;
    procedure UpdateBassBypassLeft;
    procedure UpdateTrebleBypassLeft;
    procedure UpdateBassGainRight;
    procedure UpdateTrebleGainRight;
    procedure UpdateBassBypassRight;
    procedure UpdateTrebleBypassRight;
    procedure UpdateGain;
    procedure UpdateChannel;
    procedure UpdateSelector;
  end;

implementation

uses
  Math, PngImage, VTModuleStereo;

{$R *.DFM}

procedure TFmVT.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  PngBmp : TPngObject;

begin
 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'ValueableKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialLowGainLeft.DialBitmap.Assign(PngBmp);
   DialLowGainRight.DialBitmap.Assign(PngBmp);
   DialHiGainLeft.DialBitmap.Assign(PngBmp);
   DialHiGainRight.DialBitmap.Assign(PngBmp);
  finally
   FreeAndNil(RS);
  end;

  RS := TResourceStream.Create(hInstance, 'ValueableMiniKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialOutputGain.DialBitmap.Assign(PngBmp);
  finally
   FreeAndNil(RS);
  end;

  RS := TResourceStream.Create(hInstance, 'ValueableSelector', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialSelector.DialBitmap.Assign(PngBmp);
  finally
   FreeAndNil(RS);
  end;

  RS := TResourceStream.Create(hInstance, 'ValueableSwitchYellow', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialChannel.DialBitmap.Assign(PngBmp);
  finally
   FreeAndNil(RS);
  end;

  RS := TResourceStream.Create(hInstance, 'ValueableSwitchRed', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialHiBypassLeft.DialBitmap.Assign(PngBmp);
   DialHiBypassRight.DialBitmap.Assign(PngBmp);
   DialLowBypassLeft.DialBitmap.Assign(PngBmp);
   DialLowBypassRight.DialBitmap.Assign(PngBmp);
  finally
   FreeAndNil(RS);
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmVT.DialLowGainLeftChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[2] <> DialLowGainLeft.Position
    then Parameter[2] := DialLowGainLeft.Position;
   if ssAlt in KeyboardStateToShiftState
    then Parameter[6] := Parameter[2];
  end;
end;

procedure TFmVT.DialLowGainRightChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[6] <> DialLowGainRight.Position
    then Parameter[6] := DialLowGainRight.Position;
   if ssAlt in KeyboardStateToShiftState
    then Parameter[2] := Parameter[6];
  end;
end;

procedure TFmVT.DialHiGainLeftChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[0] <> DialHiGainLeft.Position
    then Parameter[0] := DialHiGainLeft.Position;
   if ssAlt in KeyboardStateToShiftState
    then Parameter[4] := Parameter[0];
  end;
end;

procedure TFmVT.DialHiGainRightChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[4] <> DialHiGainRight.Position
    then Parameter[4] := DialHiGainRight.Position;
   if ssAlt in KeyboardStateToShiftState
    then Parameter[0] := Parameter[4];
  end;
end;

procedure TFmVT.DialLowBypassLeftChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[3] <> 1 - DialLowBypassLeft.Position
    then Parameter[3] := 1 - DialLowBypassLeft.Position;
  end;
end;

procedure TFmVT.DialLowBypassRightChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[7] <> 1 - DialLowBypassRight.Position
    then Parameter[7] := 1 - DialLowBypassRight.Position;
  end;
end;

procedure TFmVT.DialHiBypassLeftChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[1] <> 1 - DialHiBypassLeft.Position
    then Parameter[1] := 1 - DialHiBypassLeft.Position;
  end;
end;

procedure TFmVT.DialHiBypassRightChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[5] <> 1 - DialHiBypassRight.Position
    then Parameter[5] := 1 - DialHiBypassRight.Position;
  end;
end;

procedure TFmVT.DialSelectorChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[8] <> DialSelector.Position
    then Parameter[8] := DialSelector.Position;
  end;
end;

procedure TFmVT.DialChannelChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[9] <> 3 - DialChannel.Position
    then Parameter[9] := 3 - DialChannel.Position;
  end;
end;

procedure TFmVT.DialOutputGainChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[10] <> DialOutputGain.Position
    then Parameter[10] := DialOutputGain.Position;
  end;
end;

procedure TFmVT.FormShow(Sender: TObject);
begin
 UpdateBassGainLeft;
 UpdateTrebleGainLeft;
 UpdateBassBypassLeft;
 UpdateTrebleBypassLeft;
 UpdateBassGainRight;
 UpdateTrebleGainRight;
 UpdateBassBypassRight;
 UpdateTrebleBypassRight;
 UpdateGain;
 UpdateChannel;
 UpdateSelector;
end;

procedure TFmVT.LbRoasty1Click(Sender: TObject);
begin
 TVTVSTModule(Owner).Parameter[8] := 2;
end;

procedure TFmVT.LbRoasty2Click(Sender: TObject);
begin
 TVTVSTModule(Owner).Parameter[8] := 1;
end;

procedure TFmVT.LbSteamin1Click(Sender: TObject);
begin
 TVTVSTModule(Owner).Parameter[8] := 3;
end;

procedure TFmVT.LbSteamin2Click(Sender: TObject);
begin
 TVTVSTModule(Owner).Parameter[8] := 4;
end;

procedure TFmVT.UpdateBassBypassLeft;
var
  TempPosition : Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := 1 - Parameter[3];
   if DialLowBypassLeft.Position <> TempPosition
    then DialLowBypassLeft.Position := TempPosition
  end;
end;

procedure TFmVT.UpdateBassBypassRight;
var
  TempPosition : Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := 1 - Parameter[7];
   if DialLowBypassRight.Position <> TempPosition
    then DialLowBypassRight.Position := TempPosition
  end;
end;

procedure TFmVT.UpdateBassGainLeft;
var
  TempPosition : Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := Parameter[2];
   if DialLowGainLeft.Position <> TempPosition
    then DialLowGainLeft.Position := TempPosition
  end;
end;

procedure TFmVT.UpdateBassGainRight;
var
  TempPosition : Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := Parameter[6];
   if DialLowGainRight.Position <> TempPosition
    then DialLowGainRight.Position := TempPosition
  end;
end;

procedure TFmVT.UpdateChannel;
var
  TempPosition: Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := 3 - Parameter[9];
   if DialChannel.Position <> TempPosition
    then DialChannel.Position := TempPosition
  end;
end;

procedure TFmVT.UpdateGain;
var
  TempGain : Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempGain := Parameter[10];
   if DialOutputGain.Position <> TempGain
    then DialOutputGain.Position := TempGain;
   LbGainValue.Caption := FloatToStrF(RoundTo(TempGain, -1), ffGeneral, 0, 1) + ' dB';
  end;
end;

procedure TFmVT.UpdateSelector;
var
  TempPosition : Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := Parameter[8];
   if DialSelector.Position <> TempPosition
    then DialSelector.Position := TempPosition
  end;
end;

procedure TFmVT.UpdateTrebleBypassLeft;
var
  TempPosition : Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := 1 - Parameter[1];
   if DialHiBypassLeft.Position <> TempPosition
    then DialHiBypassLeft.Position := TempPosition
  end;
end;

procedure TFmVT.UpdateTrebleBypassRight;
var
  TempPosition : Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := 1 - Parameter[5];
   if DialHiBypassRight.Position <> TempPosition
    then DialHiBypassRight.Position := TempPosition
  end;
end;

procedure TFmVT.UpdateTrebleGainLeft;
var
  TempPosition : Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := Parameter[0];
   if DialHiGainLeft.Position <> TempPosition
    then DialHiGainLeft.Position := TempPosition
  end;
end;

procedure TFmVT.UpdateTrebleGainRight;
var
  TempPosition : Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := Parameter[4];
   if DialHiGainRight.Position <> TempPosition
    then DialHiGainRight.Position := TempPosition
  end;
end;

end.
