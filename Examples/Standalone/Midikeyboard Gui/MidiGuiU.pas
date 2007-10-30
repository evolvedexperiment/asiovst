unit MidiGuiU;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DGuiBaseControl, DGuiMidiKeys, DGuiMidiKeyZones,
  Buttons;

type
  TKbDemoForm = class(TForm)
    MainKb: TGuiMidiKeys;
    Panel1: TPanel;
    RemoteKeyboard: TGuiMidiKeys;
    Label1: TLabel;
    Label2: TLabel;
    Panel2: TPanel;
    ColorizeAllBtn: TButton;
    ColorizeWhiteBtn: TButton;
    ResetColorBtn: TButton;
    ColorizeCBtn: TButton;
    ColorizeBlackBtn: TButton;
    Panel3: TPanel;
    LogMemo: TMemo;
    Label3: TLabel;
    DeleteZoneBtn: TButton;
    ZoneNameEdit: TEdit;
    ZoneNameBtn: TButton;
    Bevel1: TBevel;
    ClipZoneBtn: TBitBtn;
    procedure RemoteKeyboardNoteOn(Sender: TObject; KeyNr: Byte;
      Velocity: Single);
    procedure RemoteKeyboardNoteOff(Sender: TObject; KeyNr: Byte);
    procedure ResetColorBtnClick(Sender: TObject);
    procedure ColorizeCBtnClick(Sender: TObject);
    procedure ColorizeWhiteBtnClick(Sender: TObject);
    procedure ColorizeAllBtnClick(Sender: TObject);
    procedure ColorizeBlackBtnClick(Sender: TObject);
    procedure DeleteZoneBtnClick(Sender: TObject);
    procedure MainKbMoveZoneBarDragging(Sender: TObject; KeyNr: Integer;
      var DragInfo: TGuiZbMouseDragInfo; Shift: TShiftState; X,
      Y: Integer);
    procedure MainKbZoneSelectionChanged(Sender: TObject;
      Zone: TGuiKeyZoneItem);
    procedure ZoneNameBtnClick(Sender: TObject);
    procedure ClipZoneBtnClick(Sender: TObject);
    procedure MainKbStartZoneBarDragging(Sender: TObject; KeyNr: Integer;
      var DragInfo: TGuiZbMouseDragInfo; Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  KbDemoForm: TKbDemoForm;

implementation

{$R *.dfm}

uses Math;

function RandomColor(Light: Boolean): TColor;
var baseColor: byte;
begin
  if Light then baseColor:=150 else baseColor:=0;
  result:=(Random(100) + baseColor) shl 16
        + (Random(100) + baseColor) shl 8
        + (Random(100) + baseColor);
end;

procedure TKbDemoForm.RemoteKeyboardNoteOn(Sender: TObject; KeyNr: Byte;
  Velocity: Single);
begin
  MainKb.SetKeyPressed(KeyNr);
end;

procedure TKbDemoForm.RemoteKeyboardNoteOff(Sender: TObject; KeyNr: Byte);
begin
  MainKb.ReleaseKey(KeyNr);
end;

procedure TKbDemoForm.ResetColorBtnClick(Sender: TObject);
begin
  MainKb.RemoveKeyColor(0, GUI_KB_HIGHESTKEY);
end;

procedure TKbDemoForm.ColorizeCBtnClick(Sender: TObject);
var i: byte;
begin
  for i:=0 to GUI_KB_MAXOCTAVES do
    MainKb.SetKeyColor(i*12,i*12, RandomColor(true), RandomColor(true), RandomColor(true));
end;

procedure TKbDemoForm.ColorizeWhiteBtnClick(Sender: TObject);
var i: byte;
begin
  for i:=0 to GUI_KB_HIGHESTKEY do
    if not (kfBlackKey in MainKb.Keys[i].Flags) then
      MainKb.SetKeyColor(i,i, RandomColor(true), RandomColor(true), RandomColor(true));
end;

procedure TKbDemoForm.ColorizeAllBtnClick(Sender: TObject);
var i: byte;
begin
  for i:=0 to GUI_KB_HIGHESTKEY do
    if kfBlackKey in MainKb.Keys[i].Flags then
      MainKb.SetKeyColor(i,i, RandomColor(false), RandomColor(false), RandomColor(false))
    else
      MainKb.SetKeyColor(i,i, RandomColor(true), RandomColor(true), RandomColor(true));
end;

procedure TKbDemoForm.ColorizeBlackBtnClick(Sender: TObject);
var i: byte;
begin
  for i:=0 to GUI_KB_HIGHESTKEY do
    if kfBlackKey in MainKb.Keys[i].Flags then
      MainKb.SetKeyColor(i,i, RandomColor(false), RandomColor(false), RandomColor(false));
end;

procedure TKbDemoForm.DeleteZoneBtnClick(Sender: TObject);
begin
  MainKb.KeyZones.DeleteSelected;
  DeleteZoneBtn.Enabled:=false;  
  ZoneNameBtn.Enabled:=false;
  ZoneNameEdit.Enabled:=false;
end;

procedure TKbDemoForm.MainKbMoveZoneBarDragging(Sender: TObject;
  KeyNr: Integer; var DragInfo: TGuiZbMouseDragInfo; Shift: TShiftState;
  X, Y: Integer);
var tmp: integer;
begin
  if (KeyNr<0) or (DragInfo.LastKey<0) then exit;

  if (DragInfo.Zone<>nil) and (KeyNr<>DragInfo.LastKey) then
  begin
    if mptOnLowestBorder in DragInfo.InZonePos then
    begin
      DragInfo.Zone.SetBorders(KeyNr, DragInfo.StartHighestZoneKey);
      LogMemo.Lines.Add('Size Zone: ' + DragInfo.Zone.DisplayName + ', to: '+inttostr(DragInfo.Zone.LowestZoneKey) + ', ' + inttostr(DragInfo.Zone.HighestZoneKey));
    end else if mptOnHighestBorder in DragInfo.InZonePos then
    begin
      if DragInfo.Zone<>nil then DragInfo.Zone.SetBorders(DragInfo.StartLowestZoneKey, KeyNr);
      LogMemo.Lines.Add('Size Zone: ' + DragInfo.Zone.DisplayName + ', to: '+inttostr(DragInfo.Zone.LowestZoneKey) + ', ' + inttostr(DragInfo.Zone.HighestZoneKey));
    end else if mptInZone in DragInfo.InZonePos then
    begin
      tmp:=KeyNr-DragInfo.LastKey;
      DragInfo.Zone.MoveZone(tmp);
      LogMemo.Lines.Add('Move Zone: ' + DragInfo.Zone.DisplayName + ', to: '+inttostr(DragInfo.Zone.LowestZoneKey) + ', ' + inttostr(DragInfo.Zone.HighestZoneKey));
    end else if mptOutside in DragInfo.InZonePos then
    begin
      DragInfo.Zone.SetBorders(KeyNr, DragInfo.StartKey);
      LogMemo.Lines.Add('Size Zone: ' + DragInfo.Zone.DisplayName + ', to: '+inttostr(DragInfo.Zone.LowestZoneKey) + ', ' + inttostr(DragInfo.Zone.HighestZoneKey));    end;
  end;
end;

procedure TKbDemoForm.MainKbZoneSelectionChanged(Sender: TObject;
  Zone: TGuiKeyZoneItem);
begin
  if Zone<>nil then
  begin
    LogMemo.Lines.Add('New selected zone: ' + Zone.DisplayName);
    Zone.BringToFront;
    DeleteZoneBtn.Enabled:=true;
    ZoneNameBtn.Enabled:=true;
    ZoneNameEdit.Enabled:=true;
    ZoneNameEdit.Text:=Zone.DisplayName;
  end else begin
    LogMemo.Lines.Add('No zone selected');
    DeleteZoneBtn.Enabled:=false;
    ZoneNameBtn.Enabled:=false;
    ZoneNameEdit.Enabled:=false;
  end;
end;

procedure TKbDemoForm.ZoneNameBtnClick(Sender: TObject);
begin
  MainKb.KeyZones.Selected.DisplayName := ZoneNameEdit.Text; 
end;

procedure TKbDemoForm.ClipZoneBtnClick(Sender: TObject);
begin
  MainKb.KeyZones.ClipZones;
end;

procedure TKbDemoForm.MainKbStartZoneBarDragging(Sender: TObject;
  KeyNr: Integer; var DragInfo: TGuiZbMouseDragInfo; Shift: TShiftState; X,
  Y: Integer);
begin
  if DragInfo.InZonePos = [mptOutside] then
  begin
    DragInfo.Zone := MainKb.KeyZones.Add;
    with DragInfo.Zone do
    begin
      LowestZoneKey:=KeyNr;
      HighestZoneKey:=KeyNr;
      Select(false);
      DefaultBrushColor:=RandomColor(true);
      HoverBrushColor:=RandomColor(true);
      SelectedBrushColor:=RandomColor(true);
      DisplayName:='Testzone' + inttostr(Random(100));
    end;
    MainKbZoneSelectionChanged(self, DragInfo.Zone);
  end;
end;

end.
