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
   Parameter[0] := SBModDepth.Position;
  end;
end;

procedure TFmStkChorus.UpdateModDepth;
begin
 with TStkChorusModule(Owner) do
  begin
   if SBModDepth.Position <> round(Parameter[0])
    then SBModDepth.Position := round(Parameter[0]);
   LbModDepthValue.Caption := FloatToStrF(Parameter[0], ffGeneral, 4, 4) + ' ms';
  end;
end;

procedure TFmStkChorus.UpdateModFreq;
begin
 with TStkChorusModule(Owner) do
  begin
   if SBModFreq.Position <> round(Parameter[1])
    then SBModFreq.Position := round(Parameter[1]);
   LbModFreqValue.Caption := FloatToStrF(Parameter[0], ffGeneral, 4, 4) + ' ms';
  end;
end;

procedure TFmStkChorus.UpdateEffectMix;
begin
 with TStkChorusModule(Owner) do
  begin
   if SBEffectMix.Position <> round(Parameter[0])
    then SBEffectMix.Position := round(Parameter[0]);
   LbEffectMixValue.Caption := FloatToStrF(Parameter[0], ffGeneral, 4, 4) + ' ms';
  end;
end;

end.
