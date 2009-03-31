unit CTCGui;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  Controls, StdCtrls;

type
  TFmCTC = class(TForm)
    LbAttenuation: TLabel;
    LbAttenuationValue: TLabel;
    LbListenerDistance: TLabel;
    LbListenerDistanceValue: TLabel;
    LbRecursionSteps: TLabel;
    LbRecursionStepsValue: TLabel;
    LbSpeakerDistance: TLabel;
    LbSpeakerDistanceValue: TLabel;
    SbAttenuation: TScrollBar;
    SbListenerDistance: TScrollBar;
    SbRecursionSteps: TScrollBar;
    SbSpeakerDistance: TScrollBar;
    procedure SbRecursionStepsChange(Sender: TObject);
    procedure SbAttenuationChange(Sender: TObject);
    procedure SbListenerDistanceChange(Sender: TObject);
    procedure SbSpeakerDistanceChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  public
    procedure UpdateSpeakerDistance;
    procedure UpdateListenerDistance;
    procedure UpdateRecursionSteps;
    procedure UpdateAttenuation;
  end;

implementation

{$R *.DFM}

uses
  CTCDM, DAV_VSTModuleWithPrograms;

procedure TFmCTC.FormShow(Sender: TObject);
begin
 UpdateSpeakerDistance;
 UpdateListenerDistance;
 UpdateAttenuation;
 UpdateRecursionSteps;
end;

procedure TFmCTC.SbSpeakerDistanceChange(Sender: TObject);
begin
 with TCTCDataModule(Owner) do
  begin
   Parameter[0] := 0.1 * SbSpeakerDistance.Position;
  end;
end;

procedure TFmCTC.SbListenerDistanceChange(Sender: TObject);
begin
 with TCTCDataModule(Owner) do
  begin
   Parameter[1] := 0.1 * SbListenerDistance.Position;
  end;
end;

procedure TFmCTC.SbRecursionStepsChange(Sender: TObject);
begin
 with TCTCDataModule(Owner) do
  begin
   Parameter[2] := SbRecursionSteps.Position;
  end;
end;

procedure TFmCTC.SbAttenuationChange(Sender: TObject);
begin
 with TCTCDataModule(Owner) do
  begin
   Parameter[3] := 0.1 * SbAttenuation.Position;
  end;
end;

procedure TFmCTC.UpdateSpeakerDistance;
begin
 with TCTCDataModule(Owner) do
  begin
   if round(10 * Parameter[0]) <> SbSpeakerDistance.Position
    then SbSpeakerDistance.Position := round(10 * Parameter[0]);
   LbSpeakerDistanceValue.Caption := ParameterDisplay[0] + ' ' + ParameterLabel[0];
  end;
end;

procedure TFmCTC.UpdateListenerDistance;
begin
 with TCTCDataModule(Owner) do
  begin
   if round(10 * Parameter[1]) <> SbListenerDistance.Position
    then SbListenerDistance.Position := round(10 * Parameter[1]);
   LbListenerDistanceValue.Caption := ParameterDisplay[1] + ' ' + ParameterLabel[1];
  end;
end;

procedure TFmCTC.UpdateRecursionSteps;
begin
 with TCTCDataModule(Owner) do
  begin
   if round(Parameter[2]) <> SbRecursionSteps.Position
    then SbRecursionSteps.Position := round(Parameter[2]);
   LbRecursionStepsValue.Caption := IntToStr(round(Parameter[2]));
  end;
end;

procedure TFmCTC.UpdateAttenuation;
begin
 with TCTCDataModule(Owner) do
  begin
   if round(10 * Parameter[3]) <> SbAttenuation.Position
    then SbAttenuation.Position := round(10 * Parameter[3]);
   LbAttenuationValue.Caption := ParameterDisplay[3] + ' dB';
  end;
end;

end.
