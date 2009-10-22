unit VTGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule,
  Controls, ExtCtrls, StdCtrls, DAV_GuiPanel, DAV_GuiBaseControl, DAV_GuiDial,
  DAV_GuiLabel;

type
  TFmVT = class(TForm)
    DialLowGain: TGuiDial;
    DialHiGain: TGuiDial;
    DialSelector: TGuiDial;
    GuiPanel: TGuiPanel;
    DialOutputGain: TGuiDial;
    DialChannel: TGuiDial;
    DialLowBypass: TGuiDial;
    DialHiBypass: TGuiDial;
    LbTitle: TGuiLabel;
    LbBass: TGuiLabel;
    LbTreble: TGuiLabel;
    LbSubTitle: TGuiLabel;
    LbBassFLat: TGuiLabel;
    LbTrebleFLat: TGuiLabel;
    LbDrive: TGuiLabel;
    LbGain: TGuiLabel;
    LbChannel: TGuiLabel;
    LbRoasty1: TGuiLabel;
    LbRoasty2: TGuiLabel;
    LbSteamin1: TGuiLabel;
    LbSteamin2: TGuiLabel;
    LbGainValue: TGuiLabel;
    LbMono: TGuiLabel;
    LbStereo: TGuiLabel;
    GuiLabel1: TGuiLabel;
    GuiLabel2: TGuiLabel;
    GuiLabel3: TGuiLabel;
    GuiLabel4: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialLowGainChange(Sender: TObject);
    procedure DialHiGainChange(Sender: TObject);
    procedure DialLowBypassChange(Sender: TObject);
    procedure DialHiBypassChange(Sender: TObject);
    procedure DialSelectorChange(Sender: TObject);
    procedure LbRoasty2Click(Sender: TObject);
    procedure LbRoasty1Click(Sender: TObject);
    procedure LbSteamin1Click(Sender: TObject);
    procedure LbSteamin2Click(Sender: TObject);
    procedure DialOutputGainChange(Sender: TObject);
    procedure DialChannelChange(Sender: TObject);
  public
    procedure UpdateBassGain;
    procedure UpdateTrebleGain;
    procedure UpdateBassBypass;
    procedure UpdateTrebleBypass;
    procedure UpdateGain;
    procedure UpdateChannel;
    procedure UpdateSelector;
  end;

implementation

uses
  Math, PngImage, VTModule;

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
   DialLowGain.DialBitmap.Assign(PngBmp);
   DialHiGain.DialBitmap.Assign(PngBmp);
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
   DialHiBypass.DialBitmap.Assign(PngBmp);
   DialLowBypass.DialBitmap.Assign(PngBmp);
  finally
   FreeAndNil(RS);
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmVT.DialLowGainChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[0] <> DialLowGain.Position
    then Parameter[0] := DialLowGain.Position;
  end;
end;

procedure TFmVT.DialHiGainChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[1] <> DialHiGain.Position
    then Parameter[1] := DialHiGain.Position;
  end;
end;

procedure TFmVT.DialLowBypassChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[2] <> 1 - DialLowBypass.Position
    then Parameter[2] := 1 - DialLowBypass.Position;
  end;
end;

procedure TFmVT.DialHiBypassChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[3] <> 1 - DialHiBypass.Position
    then Parameter[3] := 1 - DialHiBypass.Position;
  end;
end;

procedure TFmVT.DialSelectorChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[4] <> DialSelector.Position
    then Parameter[4] := DialSelector.Position;
  end;
end;

procedure TFmVT.DialChannelChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[5] <> 3 - DialChannel.Position
    then Parameter[5] := 3 - DialChannel.Position;
  end;
end;

procedure TFmVT.DialOutputGainChange(Sender: TObject);
begin
 with TVTVSTModule(Owner) do
  begin
   if Parameter[6] <> DialOutputGain.Position
    then Parameter[6] := DialOutputGain.Position;
  end;
end;

procedure TFmVT.FormShow(Sender: TObject);
begin
 UpdateBassGain;
 UpdateTrebleGain;
 UpdateBassBypass;
 UpdateTrebleBypass;
 UpdateGain;
 UpdateChannel;
 UpdateSelector;
end;

procedure TFmVT.LbRoasty1Click(Sender: TObject);
begin
 TVTVSTModule(Owner).Parameter[4] := 2;
end;

procedure TFmVT.LbRoasty2Click(Sender: TObject);
begin
 TVTVSTModule(Owner).Parameter[4] := 1;
end;

procedure TFmVT.LbSteamin1Click(Sender: TObject);
begin
 TVTVSTModule(Owner).Parameter[4] := 3;
end;

procedure TFmVT.LbSteamin2Click(Sender: TObject);
begin
 TVTVSTModule(Owner).Parameter[4] := 4;
end;

procedure TFmVT.UpdateBassBypass;
var
  TempPosition : Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := 1 - Parameter[2];
   if DialLowBypass.Position <> TempPosition
    then DialLowBypass.Position := TempPosition
  end;
end;

procedure TFmVT.UpdateBassGain;
var
  TempPosition : Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := Parameter[0];
   if DialLowGain.Position <> TempPosition
    then DialLowGain.Position := TempPosition
  end;
end;

procedure TFmVT.UpdateChannel;
var
  TempPosition: Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := 3 - Parameter[5];
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
   TempGain := Parameter[6];
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
   TempPosition := Parameter[4];
   if DialSelector.Position <> TempPosition
    then DialSelector.Position := TempPosition
  end;
end;

procedure TFmVT.UpdateTrebleBypass;
var
  TempPosition : Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := 1 - Parameter[3];
   if DialHiBypass.Position <> TempPosition
    then DialHiBypass.Position := TempPosition
  end;
end;

procedure TFmVT.UpdateTrebleGain;
var
  TempPosition : Single;
begin
 with TVTVSTModule(Owner) do
  begin
   TempPosition := Parameter[1];
   if DialHiGain.Position <> TempPosition
    then DialHiGain.Position := TempPosition
  end;
end;

end.
