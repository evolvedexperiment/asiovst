unit PhaseRotatorGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, StdCtrls, DAV_Types,
  DAV_VSTModule, DAV_GuiDial, DAV_GuiLabel, Controls, DAV_GuiBaseControl;

type
  TFmPhaseRotator = class(TForm)
    DialBandwidth: TGuiDial;
    DialFrequency: TGuiDial;
    DialStages: TGuiDial;
    DIL: TGuiDialImageList;
    LbBandwidth: TGuiLabel;
    LbBandwidthValue: TGuiLabel;
    LbFreq: TGuiLabel;
    LbFrequencyValue: TGuiLabel;
    LbStages: TGuiLabel;
    LbStagesValue: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialFrequencyChange(Sender: TObject);
    procedure DialStagesChange(Sender: TObject);
    procedure DialBandwidthChange(Sender: TObject);
    procedure EdValueKeyPress(Sender: TObject; var Key: Char);
    procedure FormClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DialFrequencyDblClick(Sender: TObject);
    procedure DialStagesDblClick(Sender: TObject);
    procedure DialBandwidthDblClick(Sender: TObject);
  private
    FEdValue: TEdit;
  public
    procedure UpdateFrequency;
    procedure UpdateStages;
    procedure UpdateBandwidth;
  end;

implementation

{$R *.DFM}

uses
  PngImage, PhaseRotatorDSP, DAV_VSTModuleWithPrograms;

procedure TFmPhaseRotator.FormClick(Sender: TObject);
begin
 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

procedure TFmPhaseRotator.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  PngBmp : TPngObject;
begin
 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'PhaseRotatorKnob', 'PNG');
  try
   with DIL.DialImages.Add do
    begin
     NumGlyphs := 65;
     PngBmp.LoadFromStream(RS);
     DialBitmap.Assign(PngBmp);
    end;
   DialFrequency.DialImageIndex  := 0;
   DialStages.DialImageIndex  := 0;
   DialBandwidth.DialImageIndex := 0;
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmPhaseRotator.FormDestroy(Sender: TObject);
begin
 if Assigned(FEdValue)
  then FreeAndNil(FEdValue);
end;

procedure TFmPhaseRotator.FormShow(Sender: TObject);
begin
 UpdateFrequency;
 UpdateBandwidth;
 UpdateStages;
end;

procedure TFmPhaseRotator.DialBandwidthDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent := Self;
   Left := LbBandwidthValue.Left;
   Top := LbBandwidthValue.Top;
   Width := LbBandwidthValue.Width;
   Height := LbBandwidthValue.Height;
   BorderStyle := bsNone;
   Color := Self.Color;
   Text := LbBandwidthValue.Caption;
   Tag := 2;
   OnKeyPress := EdValueKeyPress;
   Font.Assign(LbBandwidthValue.Font);
   SetFocus;
  end;
end;

procedure TFmPhaseRotator.DialFrequencyChange(Sender: TObject);
begin
 with TPhaseRotatorModule(Owner) do
  begin
   if Parameter[0] <> DialFrequency.Position
    then Parameter[0] := DialFrequency.Position;
  end;
end;

procedure TFmPhaseRotator.DialFrequencyDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent := Self;
   Left := LbFrequencyValue.Left;
   Top := LbFrequencyValue.Top;
   Width := LbFrequencyValue.Width;
   Height := LbFrequencyValue.Height;
   BorderStyle := bsNone;
   Color := Self.Color;
   Text := LbFrequencyValue.Caption;
   Tag := 0;
   OnKeyPress := EdValueKeyPress;
   Font.Assign(LbFrequencyValue.Font);
   SetFocus;
  end;
end;

procedure TFmPhaseRotator.DialStagesChange(Sender: TObject);
begin
 with TPhaseRotatorModule(Owner) do
  begin
   if Parameter[1] <> Round(DialStages.Position)
    then Parameter[1] := Round(DialStages.Position);
  end;
end;

procedure TFmPhaseRotator.DialStagesDblClick(Sender: TObject);
begin
 if not Assigned(FEdValue)
  then FEdValue := TEdit.Create(Self);

 with FEdValue do
  begin
   Parent := Self;
   Left := LbStagesValue.Left;
   Top := LbStagesValue.Top;
   Width := LbStagesValue.Width;
   Height := LbStagesValue.Height;
   BorderStyle := bsNone;
   Color := Self.Color;
   Text := LbStagesValue.Caption;
   Tag := 1;
   OnKeyPress := EdValueKeyPress;
   Font.Assign(LbStagesValue.Font);
   SetFocus;
  end;
end;

procedure TFmPhaseRotator.EdValueKeyPress(Sender: TObject; var Key: Char);
begin
 with TPhaseRotatorModule(Owner) do
  if (Key = #13) and Assigned(FEdValue) then
   try
    StringToParameter(FEdValue.Tag, FEdValue.Text);
    FreeAndNil(FEdValue);
   except
   end;
end;

procedure TFmPhaseRotator.DialBandwidthChange(Sender: TObject);
begin
 with TPhaseRotatorModule(Owner) do
  begin
   if Parameter[2] <> DialBandwidth.Position
    then Parameter[2] := DialBandwidth.Position;
  end;
end;

procedure TFmPhaseRotator.UpdateFrequency;
begin
 with TPhaseRotatorModule(Owner) do
  begin
   if DialFrequency.Position <> Parameter[0]
    then DialFrequency.Position := Parameter[0];

   LbFrequencyValue.Caption := ParameterDisplay[0] + ' ' + ParameterLabel[0];
  end;
end;

procedure TFmPhaseRotator.UpdateStages;
begin
 with TPhaseRotatorModule(Owner) do
  begin
   if DialStages.Position <> Parameter[1]
    then DialStages.Position := Parameter[1];

   LbStagesValue.Caption := ParameterDisplay[1];
  end;
end;

procedure TFmPhaseRotator.UpdateBandwidth;
begin
 with TPhaseRotatorModule(Owner) do
  begin
   if DialBandwidth.Position <> Parameter[2]
    then DialBandwidth.Position := Parameter[2];

   LbBandwidthValue.Caption := ParameterDisplay[2] + ' ' + ParameterLabel[2];
  end;
end;

end.