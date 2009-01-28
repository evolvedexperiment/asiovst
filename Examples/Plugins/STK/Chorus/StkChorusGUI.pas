unit StkChorusGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  Controls, StdCtrls;

type
  TFmStkChorus = class(TForm)
    LbModDepth: TLabel;
    SBModDepth: TScrollBar;
    LbModDepthValue: TLabel;
    LbModFreq: TLabel;
    LbModFreqValue: TLabel;
    SBModFreq: TScrollBar;
    LbEffectMix: TLabel;
    LbEffectMixValue: TLabel;
    SbEffectMix: TScrollBar;
    procedure SBModDepthChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SBModFreqChange(Sender: TObject);
    procedure SbEffectMixChange(Sender: TObject);
  public
    procedure UpdateModDepth;
    procedure UpdateModFreq;
    procedure UpdateEffectMix;
  end;

implementation

uses
  Math, DAV_VSTModuleWithPrograms, StkChorusDM;

{$R *.DFM}

procedure TFmStkChorus.FormShow(Sender: TObject);
begin
 UpdateModDepth;
 UpdateModFreq;
 UpdateEffectMix;
end;

procedure TFmStkChorus.SBModDepthChange(Sender: TObject);
begin
 with TStkChorusModule(Owner) do
  begin
   Parameter[0] := 0.1 * SBModDepth.Position;
  end;
end;

procedure TFmStkChorus.SBModFreqChange(Sender: TObject);
begin
 with TStkChorusModule(Owner) do
  begin
   Parameter[1] := 0.01 * SBModFreq.Position;
  end;
end;

procedure TFmStkChorus.SbEffectMixChange(Sender: TObject);
begin
 with TStkChorusModule(Owner) do
  begin
   Parameter[2] := 0.1 * SBEffectMix.Position;
  end;
end;

procedure TFmStkChorus.UpdateModDepth;
var
  ModDepthPos : Integer;
begin
 with TStkChorusModule(Owner) do
  begin
   ModDepthPos := round(10 * Parameter[0]);
   if SBModDepth.Position <> ModDepthPos
    then SBModDepth.Position := ModDepthPos;
   LbModDepthValue.Caption := FloatToStrF(Parameter[0], ffGeneral, 4, 4) + ' ms';
  end;
end;

procedure TFmStkChorus.UpdateModFreq;
var
  ModFreqPos : Integer;
begin
 with TStkChorusModule(Owner) do
  begin
   ModFreqPos := round(100 * Parameter[1]);
   if SBModFreq.Position <> ModFreqPos
    then SBModFreq.Position := ModFreqPos;
   LbModFreqValue.Caption := FloatToStrF(Parameter[1], ffGeneral, 4, 4) + ' Hz';
  end;
end;

procedure TFmStkChorus.UpdateEffectMix;
var
  EffectMixPos: Integer;
begin
 with TStkChorusModule(Owner) do
  begin
   EffectMixPos := round(10 * Parameter[2]);
   if SBEffectMix.Position <> EffectMixPos
    then SBEffectMix.Position := EffectMixPos;
   LbEffectMixValue.Caption := FloatToStrF(Parameter[2], ffGeneral, 4, 4) + ' %';
  end;
end;

end.
