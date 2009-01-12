unit EditorFrm;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, ExtCtrls,
  DAV_Common, DAV_VSTModule;

type
  TEditorForm = class(TForm)
    Lb10k: TLabel;
    Lb1200: TLabel;
    Lb160: TLabel;
    Lb20: TLabel;
    Lb20k: TLabel;
    Lb2500: TLabel;
    Lb320: TLabel;
    Lb40: TLabel;
    Lb5k: TLabel;
    Lb640: TLabel;
    Lb80: TLabel;
    LbLM: TLabel;
    LbRS: TLabel;
    MiddleL: TShape;
    MiddleR: TShape;
    SB10kL: TScrollBar;
    SB10kR: TScrollBar;
    SB1200L: TScrollBar;
    SB1200R: TScrollBar;
    SB160L: TScrollBar;
    SB160R: TScrollBar;
    SB20kL: TScrollBar;
    SB20kR: TScrollBar;
    SB20L: TScrollBar;
    SB20R: TScrollBar;
    SB2500L: TScrollBar;
    SB2500R: TScrollBar;
    SB320L: TScrollBar;
    SB320R: TScrollBar;
    SB40L: TScrollBar;
    SB40R: TScrollBar;
    SB5kL: TScrollBar;
    SB5kR: TScrollBar;
    SB640L: TScrollBar;
    SB640R: TScrollBar;
    SB80L: TScrollBar;
    SB80R: TScrollBar;
    ShBackText: TShape;
    procedure SBChange(Sender: TObject);
    procedure LbLMClick(Sender: TObject);
    procedure LbRSClick(Sender: TObject);
  public
  end;

implementation

{$R *.DFM}

uses PluginDM;

procedure TEditorForm.LbLMClick(Sender: TObject);
begin
 if LbLM.Caption = 'L' then
  begin
   LbLM.Caption := 'M';
   LbRS.Caption := 'S';
   with TPluginDataModule(Owner)
    do OnProcess := VSTModuleProcessMS;
  end else
 if LbLM.Caption = 'M' then
  begin
   LbLM.Caption := 'L';
   LbRS.Caption := 'R';
   with TPluginDataModule(Owner)
    do OnProcess := VSTModuleProcessLR;
  end;
 with TPluginDataModule(Owner)
  do OnProcessReplacing := OnProcess;
end;

procedure TEditorForm.LbRSClick(Sender: TObject);
begin
 if LbRS.Caption = 'R' then
  begin
   LbLM.Caption := 'M';
   LbRS.Caption := 'S';
   with TPluginDataModule(Owner)
    do OnProcess := VSTModuleProcessMS;
  end else
 if LbRS.Caption = 'S' then
  begin
   LbLM.Caption := 'L';
   LbRS.Caption := 'R';
   with TPluginDataModule(Owner)
    do OnProcess := VSTModuleProcessLR;
  end;
 with TPluginDataModule(Owner)
  do OnProcessReplacing := OnProcess;
end;

procedure TEditorForm.SBChange(Sender: TObject);
begin
 with TPluginDataModule(Owner), (Sender As TScrollBar)
  do Parameter[Tag] := Position * 0.1;
end;

end.
