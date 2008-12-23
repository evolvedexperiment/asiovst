unit SimpleChorusGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  Controls, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLabel;

type
  TFmSimpleChorus = class(TForm)
    DialDepth: TGuiDial;
    DialMix: TGuiDial;
    DialSpeed: TGuiDial;
    DialStages: TGuiDial;
    LbDepth: TGuiLabel;
    LbDepthValue: TGuiLabel;
    LbMix: TGuiLabel;
    LbMixValue: TGuiLabel;
    LbSpeed: TGuiLabel;
    LbSpeedValue: TGuiLabel;
    LbStages: TGuiLabel;
    LbStagesValue: TGuiLabel;
    DialDrift: TGuiDial;
    LbDrift: TGuiLabel;
    LbDriftValue: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialSpeedChange(Sender: TObject);
    procedure DialStagesChange(Sender: TObject);
    procedure DialDepthChange(Sender: TObject);
    procedure DialMixChange(Sender: TObject);
    procedure DialDriftChange(Sender: TObject);
  public
    procedure UpdateDepth;
    procedure UpdateDrift;
    procedure UpdateMix;
    procedure UpdateSpeed;
    procedure UpdateStages;
  end;

implementation

{$R *.DFM}

uses
  SimpleChorusDM, PngImage, DAV_VSTModuleWithPrograms;

procedure TFmSimpleChorus.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  PngBmp : TPngObject;
begin
 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'ChorusKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialSpeed.DialBitmap.Assign(PngBmp);
   DialDepth.DialBitmap.Assign(PngBmp);
   DialStages.DialBitmap.Assign(PngBmp);
   DialMix.DialBitmap.Assign(PngBmp);
   DialDrift.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmSimpleChorus.FormShow(Sender: TObject);
begin
 UpdateDepth;
 UpdateDrift;
 UpdateMix;
 UpdateSpeed;
 UpdateStages;
end;

procedure TFmSimpleChorus.DialDepthChange(Sender: TObject);
begin
 with TSimpleChorusModule(Owner) do
  begin
   if Parameter[2] <> DialDepth.Position
    then Parameter[2] := DialDepth.Position;
  end;
end;

procedure TFmSimpleChorus.DialDriftChange(Sender: TObject);
begin
 with TSimpleChorusModule(Owner) do
  begin
   if Parameter[4] <> DialDrift.Position
    then Parameter[4] := DialDrift.Position;
  end;
end;

procedure TFmSimpleChorus.DialMixChange(Sender: TObject);
begin
 with TSimpleChorusModule(Owner) do
  begin
   if Parameter[3] <> DialMix.Position
    then Parameter[3] := DialMix.Position;
  end;
end;

procedure TFmSimpleChorus.DialSpeedChange(Sender: TObject);
begin
 with TSimpleChorusModule(Owner) do
  begin
   if Parameter[0] <> DialSpeed.Position
    then Parameter[0] := DialSpeed.Position;
  end;
end;

procedure TFmSimpleChorus.DialStagesChange(Sender: TObject);
begin
 with TSimpleChorusModule(Owner) do
  begin
   if Parameter[1] <> DialStages.Position
    then Parameter[1] := DialStages.Position;
  end;
end;

procedure TFmSimpleChorus.UpdateDepth;
begin
 with TSimpleChorusModule(Owner) do
  begin
   if DialDepth.Position <> Parameter[2]
    then DialDepth.Position := Parameter[2];
   LbDepthValue.Caption := FloatToStrF(Parameter[2], ffGeneral, 3, 3) + ' %';
  end;
end;

procedure TFmSimpleChorus.UpdateDrift;
begin
 with TSimpleChorusModule(Owner) do
  begin
   if DialDrift.Position <> Parameter[4]
    then DialDrift.Position := Parameter[4];
   LbDriftValue.Caption := FloatToStrF(Parameter[4], ffGeneral, 3, 3) + ' %';
  end;
end;

procedure TFmSimpleChorus.UpdateMix;
begin
 with TSimpleChorusModule(Owner) do
  begin
   if DialMix.Position <> Parameter[3]
    then DialMix.Position := Parameter[3];
   LbMixValue.Caption := FloatToStrF(Parameter[3], ffGeneral, 3, 3) + ' %';
  end;
end;

procedure TFmSimpleChorus.UpdateSpeed;
begin
 with TSimpleChorusModule(Owner) do
  begin
   if DialSpeed.Position <> Parameter[0]
    then DialSpeed.Position := Parameter[0];
   LbSpeedValue.Caption := FloatToStrF(Parameter[0], ffGeneral, 2, 2) + ' Hz';
  end;
end;

procedure TFmSimpleChorus.UpdateStages;
begin
 with TSimpleChorusModule(Owner) do
  begin
   if DialStages.Position <> Parameter[1]
    then DialStages.Position := Parameter[1];
   LbStagesValue.Caption := IntToStr(round(Parameter[1]));
  end;
end;

end.
