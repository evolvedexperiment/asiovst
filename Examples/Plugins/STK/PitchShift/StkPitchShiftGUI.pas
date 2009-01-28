unit StkPitchShiftGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  Controls, StdCtrls;

type
  TFmStkPitchShift = class(TForm)
    LbSemitones: TLabel;
    SBSemitones: TScrollBar;
    LbSemitonesValue: TLabel;
    LbEffectMix: TLabel;
    LbEffectMixValue: TLabel;
    SbEffectMix: TScrollBar;
    procedure SBSemitonesChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SbEffectMixChange(Sender: TObject);
  public
    procedure UpdateDelay;
    procedure UpdateEffectMix;
  end;

implementation

uses
  Math, DAV_VSTModuleWithPrograms, StkPitchShiftDM;

{$R *.DFM}

procedure TFmStkPitchShift.FormShow(Sender: TObject);
begin
 UpdateDelay;
 UpdateEffectMix;
end;

procedure TFmStkPitchShift.SBSemitonesChange(Sender: TObject);
begin
 with TStkPitchShiftModule(Owner) do
  begin
   Parameter[0] := 0.1 * SBSemitones.Position;
  end;
end;

procedure TFmStkPitchShift.SbEffectMixChange(Sender: TObject);
begin
 with TStkPitchShiftModule(Owner) do
  begin
   Parameter[1] := 0.1 * SBEffectMix.Position;
  end;
end;

procedure TFmStkPitchShift.UpdateDelay;
var
  SemiPos : Integer;
begin
 with TStkPitchShiftModule(Owner) do
  begin
   SemiPos := round(10 * Parameter[0]);
   if SBSemitones.Position <> SemiPos
    then SBSemitones.Position := SemiPos;
   LbSemitonesValue.Caption := FloatToStrF(Parameter[0], ffGeneral, 4, 4);
  end;
end;

procedure TFmStkPitchShift.UpdateEffectMix;
var
  EffectMixPos: Integer;
begin
 with TStkPitchShiftModule(Owner) do
  begin
   EffectMixPos := round(10 * Parameter[1]);
   if SBEffectMix.Position <> EffectMixPos
    then SBEffectMix.Position := EffectMixPos;
   LbEffectMixValue.Caption := FloatToStrF(Parameter[1], ffGeneral, 4, 4) + ' %';
  end;
end;

end.
