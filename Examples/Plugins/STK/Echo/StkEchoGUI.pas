unit StkEchoGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  Controls, StdCtrls;

type
  TFmStkEcho = class(TForm)
    LbDelay: TLabel;
    SBDelay: TScrollBar;
    LbDelayValue: TLabel;
    LbEffectMix: TLabel;
    LbEffectMixValue: TLabel;
    SbEffectMix: TScrollBar;
    procedure SBDelayChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SbEffectMixChange(Sender: TObject);
  public
    procedure UpdateDelay;
    procedure UpdateEffectMix;
  end;

implementation

uses
  Math, DAV_VSTModuleWithPrograms, StkEchoDM;

{$R *.DFM}

procedure TFmStkEcho.FormShow(Sender: TObject);
begin
 UpdateDelay;
 UpdateEffectMix;
end;

procedure TFmStkEcho.SBDelayChange(Sender: TObject);
begin
 with TStkEchoModule(Owner) do
  begin
   Parameter[0] := 0.1 * SBDelay.Position;
  end;
end;

procedure TFmStkEcho.SbEffectMixChange(Sender: TObject);
begin
 with TStkEchoModule(Owner) do
  begin
   Parameter[1] := 0.1 * SBEffectMix.Position;
  end;
end;

procedure TFmStkEcho.UpdateDelay;
var
  DelayPos : Integer;
begin
 with TStkEchoModule(Owner) do
  begin
   DelayPos := round(10 * Parameter[0]);
   if SBDelay.Position <> DelayPos
    then SBDelay.Position := DelayPos;
   LbDelayValue.Caption := FloatToStrF(Parameter[0], ffGeneral, 4, 4) + ' ms';
  end;
end;

procedure TFmStkEcho.UpdateEffectMix;
var
  EffectMixPos: Integer;
begin
 with TStkEchoModule(Owner) do
  begin
   EffectMixPos := round(10 * Parameter[1]);
   if SBEffectMix.Position <> EffectMixPos
    then SBEffectMix.Position := EffectMixPos;
   LbEffectMixValue.Caption := FloatToStrF(Parameter[1], ffGeneral, 4, 4) + ' %';
  end;
end;

end.
