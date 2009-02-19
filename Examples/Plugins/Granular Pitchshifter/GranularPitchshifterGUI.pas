unit GranularPitchShifterGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  Controls, DAV_GuiBaseControl, DAV_GuiDial, DAV_GuiLabel, DAV_GuiSelectBox;

type
  TFmGranularPitchShifter = class(TForm)
    DialStages: TGuiDial;
    LbStages: TGuiLabel;
    LbStagesValue: TGuiLabel;
    DialSemitones: TGuiDial;
    LbSemitones: TGuiLabel;
    LbSemitonesValue: TGuiLabel;
    DialGranularity: TGuiDial;
    LbGranularity: TGuiLabel;
    LbGranularityValue: TGuiLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DialSemitonesChange(Sender: TObject);
    procedure DialStagesChange(Sender: TObject);
    procedure DialGranularityChange(Sender: TObject);
  public
    procedure UpdateSemitones;
    procedure UpdateGranularity;
    procedure UpdateStages;
  end;

implementation

{$R *.DFM}

uses
  Math, PngImage, DAV_VSTModuleWithPrograms, GranularPitchShifterDM;

procedure TFmGranularPitchShifter.FormCreate(Sender: TObject);
var
  RS     : TResourceStream;
  PngBmp : TPngObject;
begin
 PngBmp := TPngObject.Create;
 try
  RS := TResourceStream.Create(hInstance, 'GranularPitchShifterKnob', 'PNG');
  try
   PngBmp.LoadFromStream(RS);
   DialSemitones.DialBitmap.Assign(PngBmp);
   DialStages.DialBitmap.Assign(PngBmp);
   DialGranularity.DialBitmap.Assign(PngBmp);
  finally
   RS.Free;
  end;
 finally
  FreeAndNil(PngBmp);
 end;
end;

procedure TFmGranularPitchShifter.FormShow(Sender: TObject);
begin
 UpdateSemitones;
 UpdateGranularity;
 UpdateStages;
end;

procedure TFmGranularPitchShifter.DialSemitonesChange(Sender: TObject);
begin
 with TGranularPitchShifterModule(Owner) do
  begin
   if Parameter[0] <> DialSemitones.Position
    then Parameter[0] := DialSemitones.Position;
  end;
end;

procedure TFmGranularPitchShifter.DialGranularityChange(Sender: TObject);
begin
 with TGranularPitchShifterModule(Owner) do
  begin
   if Parameter[1] <> DialGranularity.Position
    then Parameter[1] := DialGranularity.Position;
  end;
end;

procedure TFmGranularPitchShifter.DialStagesChange(Sender: TObject);
begin
 with TGranularPitchShifterModule(Owner) do
  begin
   if Parameter[2] <> DialStages.Position
    then Parameter[2] := DialStages.Position;
  end;
end;

procedure TFmGranularPitchShifter.UpdateSemitones;
var
  Semitones : Single;
begin
 with TGranularPitchShifterModule(Owner) do
  begin
   Semitones := Parameter[0];
   if DialSemitones.Position <> Semitones
    then DialSemitones.Position := Semitones;
   LbSemitonesValue.Caption := FloatToStrF(RoundTo(Semitones, -2), ffGeneral, 2, 2);
  end;
end;

procedure TFmGranularPitchShifter.UpdateGranularity;
var
  Granularity : Single;
begin
 with TGranularPitchShifterModule(Owner) do
  begin
   Granularity := Parameter[1];
   if DialGranularity.Position <> Granularity
    then DialGranularity.Position := Granularity;
   LbGranularityValue.Caption := FloatToStrF(RoundTo(Granularity, -2), ffGeneral, 4, 4) + ' ms';
  end;
end;

procedure TFmGranularPitchShifter.UpdateStages;
begin
 with TGranularPitchShifterModule(Owner) do
  begin
   if DialStages.Position <> Parameter[2]
    then DialStages.Position := Parameter[2];
   LbStagesValue.Caption := IntToStr(round(Parameter[2]));
  end;
end;

end.
