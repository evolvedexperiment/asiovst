unit StkReverbGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  Controls, StdCtrls;

type
  TFmStkReverb = class(TForm)
    LbT60: TLabel;
    SBT60: TScrollBar;
    LbT60Value: TLabel;
    procedure SBT60Change(Sender: TObject);
  public
    procedure UpdateT60;  
  end;

implementation

uses
  Math, DAV_VSTModuleWithPrograms, StkReverbDM;

{$R *.DFM}

procedure TFmStkReverb.SBT60Change(Sender: TObject);
begin
 with TStkReverbModule(Owner) do
  begin
   Parameter[0] := SBT60.Position;
  end;
end;

procedure TFmStkReverb.UpdateT60;
begin
 with TStkReverbModule(Owner) do
  begin
   if SBT60.Position <> round(Parameter[0])
    then SBT60.Position := round(Parameter[0]);
   LbT60Value.Caption := FloatToStrF(Parameter[0], ffGeneral, 4, 4) + ' ms';  
  end;
end;

end.