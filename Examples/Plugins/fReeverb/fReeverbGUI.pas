unit fReeverbGUI;

interface

uses Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule,
  Controls, StdCtrls, XPStyleActnCtrls, ActnList, ActnMan, ToolWin,
  ActnCtrls, ActnMenus, DGuiBaseControl, DGuiDial, DGuiLabel, DGuiPanel,
  DGuiSelectBox, ExtCtrls, DGuiButton;

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
    LbDamp: TGuiLabel;
    LbDry: TGuiLabel;
    LbPreset: TGuiLabel;
    LbSize: TGuiLabel;
    LbStretch: TGuiLabel;
    LbWet: TGuiLabel;
    LbWidth: TGuiLabel;
    PnLabel: TGuiPanel;
    PnToolbar: TPanel;
    SBPreset: TGuiSelectBox;
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

procedure TFmReverb.DialDryChange(Sender: TObject);
begin
  TfReeverbVST(Owner).Parameter[0] := DialDry.Max - DialDry.Position;
end;

procedure TFmReverb.DialWetChange(Sender: TObject);
begin
  TfReeverbVST(Owner).Parameter[1] := DialWet.Max - DialWet.Position;
end;

procedure TFmReverb.DialWidthChange(Sender: TObject);
begin
  TfReeverbVST(Owner).Parameter[2] := DialWidth.Max - DialWidth.Position;
end;

procedure TFmReverb.DialRoomSizeChange(Sender: TObject);
begin
  TfReeverbVST(Owner).Parameter[3] :=
    (DialRoomSize.Max - DialRoomSize.Position) / DialRoomSize.Max;
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
  TfReeverbVST(Owner).Parameter[5] := (DialStretch.Max - DialStretch.Position);
end;

procedure TFmReverb.DialDampChange(Sender: TObject);
begin
  TfReeverbVST(Owner).Parameter[6] := DialDamp.Max - DialDamp.Position;
end;

end.
