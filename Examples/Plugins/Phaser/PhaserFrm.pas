unit PhaserFrm;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, ExtCtrls,
  Graphics, DAV_Common, DAV_VSTModule;

type
  TPhaserForm = class(TForm)
    LbVDepth: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    LbVFeedback: TLabel;
    LbVMinimum: TLabel;
    LbVMaximum: TLabel;
    LbVRate: TLabel;
    SBFeedback: TScrollBar;
    SBMinimum: TScrollBar;
    SBMaximum: TScrollBar;
    SBRate: TScrollBar;
    SBDepth: TScrollBar;
    SBStages: TScrollBar;
    Image1: TImage;
    procedure SBDepthChange(Sender: TObject);
    procedure SBFeedbackChange(Sender: TObject);
    procedure SBMinimumChange(Sender: TObject);
    procedure SBMaximumChange(Sender: TObject);
    procedure SBRateChange(Sender: TObject);
    procedure SBStagesChange(Sender: TObject);
  end;

implementation

{$R *.DFM}

uses PhaserDM;

procedure TPhaserForm.SBDepthChange(Sender: TObject);
begin
  with TPhaserModule(Owner) do
   begin
    Parameter[0] := SBDepth.Position * 0.1;
    LbVDepth.Caption := FloatToStrF(Parameter[0], ffFixed, 2, 2);
   end;
end;

procedure TPhaserForm.SBFeedbackChange(Sender: TObject);
begin
  with TPhaserModule(Owner) do
   begin
    Parameter[1] := SBFeedback.Position * 0.1;
    LbVFeedback.Caption := FloatToStrF(Parameter[1], ffFixed, 3, 1) + '%';
   end;
end;

procedure TPhaserForm.SBMinimumChange(Sender: TObject);
begin
  with TPhaserModule(Owner) do
   begin
    Parameter[2] := FreqLinearToLog(SBMinimum.Position / 20000);
    LbVMinimum.Caption := FloatToStrF(Parameter[2], ffFixed, 6, 0) + 'Hz';
   end;
end;

procedure TPhaserForm.SBMaximumChange(Sender: TObject);
begin
  with TPhaserModule(Owner) do
   begin
    Parameter[3] := FreqLinearToLog(SBMaximum.Position / 20000);
    LbVMaximum.Caption := FloatToStrF(Parameter[3], ffFixed, 6, 0) + 'Hz';
   end;
end;

procedure TPhaserForm.SBRateChange(Sender: TObject);
begin
  with TPhaserModule(Owner) do
   begin
    Parameter[4] := SBRate.Position * 1E-3;
    LbVRate.Caption := FloatToStrF(Parameter[4], ffFixed, 2, 2) + 'Hz';
   end;
end;

procedure TPhaserForm.SBStagesChange(Sender: TObject);
begin
  with TPhaserModule(Owner) do
    Parameter[5] := SBStages.Position;
end;

end.