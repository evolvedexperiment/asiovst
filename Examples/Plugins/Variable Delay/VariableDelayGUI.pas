unit VariableDelayGUI;

interface

uses
  Windows, SysUtils, Classes, Forms, Controls, StdCtrls, DAV_VSTModule,
  DAV_Common;

type
  TVSTGUI = class(TForm)
    SampleBar: TScrollBar;
    LbSamples: TLabel;
    LbDryMixValue: TLabel;
    SBDryMix: TScrollBar;
    LbWetMixValue: TLabel;
    SBWetMix: TScrollBar;
    procedure FormShow(Sender: TObject);
    procedure SampleBarChange(Sender: TObject);
    procedure SBDryMixChange(Sender: TObject);
    procedure SBWetMixChange(Sender: TObject);
  private
  public
    procedure UpdateDelayLength;
    procedure UpdateDryMix;
    procedure UpdateWetMix;
  end;

implementation

{$R *.DFM}

uses
  VariableDelayModule;

procedure TVSTGUI.FormShow(Sender: TObject);
begin
 UpdateDelayLength;
end;

procedure TVSTGUI.SampleBarChange(Sender: TObject);
begin
 with TVariableDelayVST(Owner) do
  begin
   if Parameter[0] <> 0.01 * SampleBar.Position
    then Parameter[0] := 0.01 * SampleBar.Position;
  end;
end;

procedure TVSTGUI.SBDryMixChange(Sender: TObject);
begin
 with TVariableDelayVST(Owner) do
  begin
   if Parameter[1] <> 0.1 * SBDryMix.Position
    then Parameter[1] := 0.1 * SBDryMix.Position;
  end;
end;

procedure TVSTGUI.SBWetMixChange(Sender: TObject);
begin
 with TVariableDelayVST(Owner) do
  begin
   if Parameter[2] <> 0.1 * SBWetMix.Position
    then Parameter[2] := 0.1 * SBWetMix.Position;
  end;
end;

procedure TVSTGUI.UpdateDelayLength;
begin
 with TVariableDelayVST(Owner) do
  begin
   if round(100 * Parameter[0]) <> SampleBar.Position
    then SampleBar.Position := round(100 * Parameter[0]);
   LbSamples.Caption := 'Delay: ' + IntToStr(round(Parameter[0])) + ' ms';
  end;
end;

procedure TVSTGUI.UpdateDryMix;
begin
 with TVariableDelayVST(Owner) do
  begin
   if round(10 * Parameter[1]) <> SBDryMix.Position
    then SBDryMix.Position := round(10 * Parameter[1]);
   LbDryMixValue.Caption := 'Dry Mix: ' + FloatToStrF(Parameter[1], ffGeneral, 3, 3) + ' %';
  end;
end;

procedure TVSTGUI.UpdateWetMix;
begin
 with TVariableDelayVST(Owner) do
  begin
   if round(10 * Parameter[2]) <> SBWetMix.Position
    then SBWetMix.Position := round(10 * Parameter[2]);
   LbWetMixValue.Caption := 'Wet Mix: ' + FloatToStrF(Parameter[2], ffGeneral, 3, 3) + ' %';
  end;
end;

end.
