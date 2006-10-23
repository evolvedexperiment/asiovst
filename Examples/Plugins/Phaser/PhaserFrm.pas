unit PhaserFrm;

interface

uses Windows, Messages, SysUtils, Classes, Forms, DDSPBase, DVSTModule,
  Controls, StdCtrls, ExtCtrls, GraphicEx, Graphics;

type
  TPhaserForm = class(TForm)
    LbVDepth: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LbVFeedback: TLabel;
    LbVMinimum: TLabel;
    LbVMaximum: TLabel;
    LbVRate: TLabel;
    Label6: TLabel;
    Label7: TLabel;
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
  private
  public
    theModule: TVSTModule;
  end;

implementation

{$R *.DFM}

procedure TPhaserForm.SBDepthChange(Sender: TObject);
begin
 theModule.Parameter[0]:=SBDepth.Position/10;
 LbVDepth.Caption:=FloatToStrF(theModule.Parameter[0],ffFixed,2,2);
end;

procedure TPhaserForm.SBFeedbackChange(Sender: TObject);
begin
 theModule.Parameter[1]:=SBFeedback.Position/10;
 LbVFeedback.Caption:=FloatToStrF(theModule.Parameter[1],ffFixed,3,1)+'%';
end;

procedure TPhaserForm.SBMinimumChange(Sender: TObject);
begin
 theModule.Parameter[2]:=FreqLinearToLog(SBMinimum.Position/20000);
 LbVMinimum.Caption:=FloatToStrF(theModule.Parameter[2],ffFixed,6,0)+'Hz';
end;

procedure TPhaserForm.SBMaximumChange(Sender: TObject);
begin
 theModule.Parameter[3]:=FreqLinearToLog(SBMaximum.Position/20000);
 LbVMaximum.Caption:=FloatToStrF(theModule.Parameter[3],ffFixed,6,0)+'Hz';
end;

procedure TPhaserForm.SBRateChange(Sender: TObject);
begin
 theModule.Parameter[4]:=SBRate.Position/1000;
 LbVRate.Caption:=FloatToStrF(theModule.Parameter[4],ffFixed,2,2)+'Hz';
end;

procedure TPhaserForm.SBStagesChange(Sender: TObject);
begin
 theModule.Parameter[5]:=SBStages.Position;
end;

end.