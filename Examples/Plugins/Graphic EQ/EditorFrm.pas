unit EditorFrm;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DDSPBase, DVSTModule, Controls,
  StdCtrls, ExtCtrls;

type
  TEditorForm = class(TForm)
    SB1200L: TScrollBar;
    SB640L: TScrollBar;
    SB320L: TScrollBar;
    SB160L: TScrollBar;
    SB80L: TScrollBar;
    SB40L: TScrollBar;
    SB20L: TScrollBar;
    Lb20: TLabel;
    Lb40: TLabel;
    Lb80: TLabel;
    Lb160: TLabel;
    Lb320: TLabel;
    Lb640: TLabel;
    Lb1200: TLabel;
    SB10kL: TScrollBar;
    SB5kL: TScrollBar;
    SB2500L: TScrollBar;
    Lb2500: TLabel;
    Lb5k: TLabel;
    Lb10k: TLabel;
    Lb20k: TLabel;
    SB20kL: TScrollBar;
    SB1200R: TScrollBar;
    SB640R: TScrollBar;
    SB320R: TScrollBar;
    SB160R: TScrollBar;
    SB80R: TScrollBar;
    SB40R: TScrollBar;
    SB20R: TScrollBar;
    SB10kR: TScrollBar;
    SB5kR: TScrollBar;
    SB2500R: TScrollBar;
    SB20kR: TScrollBar;
    LbLM: TLabel;
    LbRS: TLabel;
    MiddleL: TShape;
    MiddleR: TShape;
    Shape1: TShape;
    procedure SBChange(Sender: TObject);
    procedure LbLMClick(Sender: TObject);
    procedure LbRSClick(Sender: TObject);
  public
    PluginDataModule: TVSTModule;
  end;

implementation

{$R *.DFM}

uses PluginDM;

procedure TEditorForm.LbLMClick(Sender: TObject);
begin
 if LbLM.Caption='L' then
  begin
   LbLM.Caption:='M';
   LbRS.Caption:='S';
   PluginDataModule.OnProcess:=(PluginDataModule As TPluginDataModule).VSTModuleProcessMS;
  end else
 if LbLM.Caption='M' then
  begin
   LbLM.Caption:='L';
   LbRS.Caption:='R';
   PluginDataModule.OnProcess:=(PluginDataModule As TPluginDataModule).VSTModuleProcessLR;
  end;
 PluginDataModule.OnProcessReplacing:=PluginDataModule.OnProcess;
end;

procedure TEditorForm.LbRSClick(Sender: TObject);
begin
 if LbLM.Caption='R' then
  begin
   LbLM.Caption:='M';
   LbRS.Caption:='S';
   PluginDataModule.OnProcess:=(PluginDataModule As TPluginDataModule).VSTModuleProcessMS;
  end else
 if LbLM.Caption='S' then
  begin
   LbLM.Caption:='L';
   LbRS.Caption:='R';
   PluginDataModule.OnProcess:=(PluginDataModule As TPluginDataModule).VSTModuleProcessLR;
  end;
 PluginDataModule.OnProcessReplacing:=PluginDataModule.OnProcess;
end;

procedure TEditorForm.SBChange(Sender: TObject);
begin
 with Sender As TScrollBar
  do PluginDataModule.Parameter[Tag]:=Position*0.1;
end;

end.
