unit fReeverbGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, ExtCtrls,
  DAV_Common, DAV_VSTModule, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLabel,
  DAV_GuiPanel, DAV_GuiSelectBox, DAV_GuiButton;

type

  TFmReverb = class(TForm)
    BtAB: TGuiButton;
    BtAbout: TGuiButton;
    CBFreeze: TCheckBox;
    DialDamp: TGuiDial;
    DialDry: TGuiDial;
    DialRoomSize: TGuiDial;
    DialStretch: TGuiDial;
    DialWet: TGuiDial;
    DialWidth: TGuiDial;
    LbPreset: TGuiLabel;
    PnLabel: TGuiPanel;
    PnToolbar: TPanel;
    SBPreset: TGuiSelectBox;
    LbDry: TGuiLabel;
    LbWet: TGuiLabel;
    LbWidth: TGuiLabel;
    LbSize: TGuiLabel;
    LbStretch: TGuiLabel;
    LbDamp: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BtAboutClick(Sender: TObject);
    procedure CBFreezeClick(Sender: TObject);
    procedure DialWetChange(Sender: TObject);
    procedure DialDryChange(Sender: TObject);
    procedure DialWidthChange(Sender: TObject);
    procedure DialRoomSizeChange(Sender: TObject);
    procedure DialStretchChange(Sender: TObject);
    procedure DialDampChange(Sender: TObject);
    procedure SBPresetChange(Sender: TObject);
  public
    procedure UpdateDry;
    procedure UpdateWet;
    procedure UpdateWidth;
    procedure UpdateDamp;
    procedure UpdateSize;
    procedure UpdateStretch;
  end;

implementation

{$R *.DFM}

uses
  Dialogs, fReeverbModule;

procedure TFmReverb.FormCreate(Sender: TObject);
var
  RS  : TResourceStream;
begin
 RS := TResourceStream.Create(hInstance, 'ReverbKnob', 'BMP');
 try
  DialDry.DialBitmap.LoadFromStream(RS);      RS.Position := 0;
  DialWet.DialBitmap.LoadFromStream(RS);      RS.Position := 0;
  DialWidth.DialBitmap.LoadFromStream(RS);    RS.Position := 0;
  DialDamp.DialBitmap.LoadFromStream(RS);     RS.Position := 0;
  DialRoomSize.DialBitmap.LoadFromStream(RS); RS.Position := 0;
  DialStretch.DialBitmap.LoadFromStream(RS);  RS.Position := 0;
 finally
  RS.Free;
 end;
end;

procedure TFmReverb.FormShow(Sender: TObject);
var
  i : Integer;
begin
 with TfReeverbVST(Owner) do
  begin
   SBPreset.Items.Clear;
   for i := 0 to numPrograms - 1
    do SBPreset.Items.Add(Programs[i].DisplayName);
   SBPreset.ItemIndex := CurrentProgram; 
  end;
end;

procedure TFmReverb.SBPresetChange(Sender: TObject);
begin
 with TfReeverbVST(Owner) do
  begin
   CurrentProgram := SBPreset.ItemIndex;
   UpdateDry;
   UpdateWet;
   UpdateWidth;
   UpdateDamp;
   UpdateSize;
   UpdateStretch;
  end;
end;

procedure TFmReverb.UpdateDamp;
begin
 with TfReeverbVST(Owner) do
  if DialDamp.Position <> Parameter[6] then
   begin
    DialDamp.Position := Parameter[6];
   end;
end;

procedure TFmReverb.UpdateDry;
begin
 with TfReeverbVST(Owner) do
  if DialDry.Position <> Parameter[0]  then
   begin
    DialDry.Position := Parameter[0];
   end;
end;

procedure TFmReverb.UpdateSize;
begin
 with TfReeverbVST(Owner) do
  if DialRoomSize.Position <> Parameter[3] then
   begin
    DialRoomSize.Position := Parameter[3];
   end;
end;

procedure TFmReverb.UpdateStretch;
begin
 with TfReeverbVST(Owner) do
  if DialStretch.Position <> Parameter[5] then
   begin
    DialStretch.Position := Parameter[5];
   end;
end;

procedure TFmReverb.UpdateWet;
begin
 with TfReeverbVST(Owner) do
  if DialWet.Position <> Parameter[1]  then
   begin
    DialWet.Position := Parameter[1];
   end;
end;

procedure TFmReverb.UpdateWidth;
begin
 with TfReeverbVST(Owner) do
  if DialWidth.Position <> Parameter[2]  then
   begin
    DialWidth.Position := Parameter[2];
   end;
end;

procedure TFmReverb.DialDryChange(Sender: TObject);
begin
 with TfReeverbVST(Owner) do
  if Parameter[0] <> DialDry.Position then
   begin
    Parameter[0] := DialDry.Position;
   end;
end;

procedure TFmReverb.DialWetChange(Sender: TObject);
begin
 with TfReeverbVST(Owner) do
  if Parameter[1] <> DialWet.Position then
   begin
    Parameter[1] := DialWet.Position;
   end;
end;

procedure TFmReverb.DialWidthChange(Sender: TObject);
begin
 with TfReeverbVST(Owner) do
  if Parameter[2] <> DialWidth.Position then
   begin
    Parameter[2] := DialWidth.Position;
   end;
end;

procedure TFmReverb.DialRoomSizeChange(Sender: TObject);
begin
 with TfReeverbVST(Owner) do
  if Parameter[3] <> DialRoomSize.Position then
   begin
    Parameter[3] := DialRoomSize.Position;
   end;
end;

procedure TFmReverb.BtAboutClick(Sender: TObject);
begin
 MessageDlg('fReeverb example plugin written by Christian Budde' + #13#10 +
            'based on algorithm by Jezar at Dreampoint' + #13#10 +
            'based on GUI by thcilnnahoj', mtInformation, [mbOK], 0);
end;

procedure TFmReverb.CBFreezeClick(Sender: TObject);
begin
  TfReeverbVST(Owner).Parameter[4] := Byte(CBFreeze.Checked);
end;

procedure TFmReverb.DialStretchChange(Sender: TObject);
begin
 with TfReeverbVST(Owner) do
  if Parameter[5] <> DialStretch.Position then
   begin
    Parameter[5] := DialStretch.Position;
   end;
end;

procedure TFmReverb.DialDampChange(Sender: TObject);
begin
 with TfReeverbVST(Owner) do
  if Parameter[6] <> DialDamp.Position then
   begin
    Parameter[6] := DialDamp.Position;
   end;
end;

end.
