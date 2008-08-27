unit fReeverbGUI;

interface

uses Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule,
  Controls, StdCtrls, XPStyleActnCtrls, ActnList, ActnMan, ToolWin,
  ActnCtrls, ActnMenus, DGuiBaseControl, DGuiDial, DGuiLabel;

type

  TFmReverb = class(TForm)
    CBFreeze: TCheckBox;
    DialDry: TGuiDial;
    DialWet: TGuiDial;
    DialWidth: TGuiDial;
    DialDamp: TGuiDial;
    DialRoomSize: TGuiDial;
    DialStretch: TGuiDial;
    LbDry: TGuiLabel;
    LbWet: TGuiLabel;
    Label1: TGuiLabel;
    LbSize: TGuiLabel;
    LbStretch: TGuiLabel;
    LbDamp: TGuiLabel;
    procedure CBFreezeClick(Sender: TObject);
    procedure DialWetChange(Sender: TObject);
    procedure DialDryChange(Sender: TObject);
    procedure DialWidthChange(Sender: TObject);
    procedure DialRoomSizeChange(Sender: TObject);
    procedure DialStretchChange(Sender: TObject);
    procedure DialDampChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
  end;

implementation

{$R *.DFM}

uses
  fReeverbModule;

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

procedure TFmReverb.DialRoomSizeChange(Sender: TObject);
begin
  TfReeverbVST(Owner).Parameter[3] :=
    (DialRoomSize.Max - DialRoomSize.Position) / DialRoomSize.Max;
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
