unit ConvoFxGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, DAV_Common,
  DAV_VSTModule, DAV_GuiLabel, DAV_GuiBaseControl, DAV_GuiDial;

type
  TFmConvoFx = class(TForm)
    DialIR: TGuiDial;
    DialGain: TGuiDial;
    LbIR: TGuiLabel;
    LbIRSelected: TGuiLabel;
    LbGain: TGuiLabel;
    LbGainValue: TGuiLabel;
    DialDamp: TGuiDial;
    LbDamp: TGuiLabel;
    LbDampValue: TGuiLabel;
    DIL: TGuiDialImageList;
    procedure DialIRChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LbIRSelectedMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure DialGainChange(Sender: TObject);
    procedure DialDampChange(Sender: TObject);
  public
    procedure UpdateIRSelect;
    procedure UpdateGain;
    procedure UpdateDamping;
  end;

implementation

{$R *.DFM}

uses
  Dialogs, Math, PngImage, ConvoFxDM, DAV_VSTModuleWithPrograms;

procedure TFmConvoFx.DialDampChange(Sender: TObject);
begin
 with TConvoFxDataModule(Owner) do
  begin
   Parameter[4] := DialDamp.Position;
  end;
end;

procedure TFmConvoFx.DialGainChange(Sender: TObject);
begin
 with TConvoFxDataModule(Owner) do
  begin
   Parameter[3] := DialGain.Position;
  end;
end;

procedure TFmConvoFx.DialIRChange(Sender: TObject);
begin
 with TConvoFxDataModule(Owner) do
  begin
   if round(Parameter[2]) <> round(DialIR.Position)
    then Parameter[2] := round(DialIR.Position);
  end;
end;

procedure TFmConvoFx.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  PngBmp : TPngObject;
begin
 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'ConvoFxKnob', 'PNG');
  try
   with DIL.DialImages.Add do
    begin
     NumGlyphs := 65;
     PngBmp.LoadFromStream(RS);
     DialBitmap.Assign(PngBmp);
    end;
   DialIR.DialImageIndex := 0;
   DialGain.DialImageIndex := 0;
   DialDamp.DialImageIndex := 0;
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmConvoFx.FormShow(Sender: TObject);
begin
 UpdateIRSelect;
 UpdateGain;
 UpdateDamping;
end;

procedure TFmConvoFx.LbIRSelectedMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 case Button of
  mbLeft  : DialIR.Position := round(DialIR.Position) + 1;
  mbRight : DialIR.Position := round(DialIR.Position) - 1;
 end;
end;

procedure TFmConvoFx.UpdateGain;
begin
 with TConvoFxDataModule(Owner) do
  begin
   if DialGain.Position <> Parameter[3]
    then DialGain.Position := Parameter[3];
   LbGainValue.Caption := FloatToStrF(RoundTo(Parameter[3], -1), ffGeneral, 2, 2) + ' dB';
  end;
end;

procedure TFmConvoFx.UpdateDamping;
begin
 with TConvoFxDataModule(Owner) do
  begin
   if DialDamp.Position <> Parameter[4]
    then DialDamp.Position := Parameter[4];
   if Parameter[4] < 1000
    then LbDampValue.Caption := FloatToStrF(RoundTo(Parameter[4], -1), ffGeneral, 3, 3) + ' Hz'
    else LbDampValue.Caption := FloatToStrF(RoundTo(1E-3 * Parameter[4], -1), ffGeneral, 3, 3) + ' kHz';
  end;
end;

procedure TFmConvoFx.UpdateIRSelect;
begin
 with TConvoFxDataModule(Owner) do
  begin
   if round(DialIR.Position) <> round(Parameter[2])
    then DialIR.Position := round(Parameter[2]);
   LbIRSelected.Caption := IntToStr(round(Parameter[2]));
  end;
end;

end.
