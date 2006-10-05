unit uEditor;

interface
uses Windows, Forms, DAudioEffectX, Controls, Classes, ExtCtrls, ComCtrls, Graphics,
  StdCtrls, Messages, cDIBImageList, cDIBSlider, cDIBControl, cDIBImage,
  cDIBPanel, cDIBFeatures, cDIBTimer, Menus, cDIBStandardCompressors,
  cDIBSettings;
{$I config.inc}

type
  TPluginEditorWindow = class(TForm)
    Ctrl: TLabel;
    updater: TDIBTimer;
    bg: TDIBImageContainer;
    DIBImageList1: TDIBImageList;
    par0: TDIBSlider;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    top: TDIBImage;
    DIBImage1: TDIBImage;
    DIBImage2: TDIBImage;
    par1: TDIBSlider;
    Label5: TLabel;
    par2: TDIBSlider;
    Label6: TLabel;
    Label7: TLabel;
    par3: TDIBSlider;
    Label9: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    DIBImage3: TDIBImage;
    Label8: TLabel;
    par4: TDIBSlider;
    Label10: TLabel;
    par5: TDIBSlider;
    par6: TDIBSlider;
    Label15: TLabel;
    par7: TDIBSlider;
    Label16: TLabel;
    Label17: TLabel;
    par8: TDIBSlider;
    Label18: TLabel;
    par9: TDIBSlider;
    Label19: TLabel;
    par10: TDIBSlider;
    Label20: TLabel;
    par11: TDIBSlider;
    Label21: TLabel;
    presets: TPopupMenu;
    a1: TMenuItem;
    N11: TMenuItem;
    N21: TMenuItem;
    N31: TMenuItem;
    N41: TMenuItem;
    N51: TMenuItem;
    N61: TMenuItem;
    N71: TMenuItem;
    N81: TMenuItem;
    N91: TMenuItem;
    N101: TMenuItem;
    N111: TMenuItem;
    N121: TMenuItem;
    N131: TMenuItem;
    N141: TMenuItem;
    N151: TMenuItem;
    N161: TMenuItem;
    N171: TMenuItem;
    N181: TMenuItem;
    N191: TMenuItem;
    N201: TMenuItem;
    N211: TMenuItem;
    N221: TMenuItem;
    N231: TMenuItem;
    N241: TMenuItem;
    N251: TMenuItem;
    N261: TMenuItem;
    N271: TMenuItem;
    N281: TMenuItem;
    N291: TMenuItem;
    about: TPanel;
    Label2: TLabel;
    Label11: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    DIBSettings1: TDIBSettings;
    procedure UpdaterTimer(Sender: TObject);
    procedure par0Change(Sender: TObject);
    procedure Label12MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure Label21MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure a1Click(Sender: TObject);
    procedure aboutClick(Sender: TObject);
    procedure Label13Click(Sender: TObject);
    procedure Label11Click(Sender: TObject);
    procedure Label22Click(Sender: TObject);
    procedure Label23Click(Sender: TObject);
    procedure Label24Click(Sender: TObject);
    procedure Label25Click(Sender: TObject);
    procedure Label26Click(Sender: TObject);
  private
    FEffect:AudioEffectX;
  public
    manually:boolean;
    fpar:array[0..kNumParams-1] of single;
    property Effect: AudioEffectX read FEffect write FEffect;
  private
    procedure WMEraseBkgnd(var m: TWMEraseBkgnd); message WM_ERASEBKGND;
  end;

implementation
uses sysutils,shellapi;

{$R *.DFM}

const qual:array[0..3] of string=('low','medium','high','excellent');

procedure TPluginEditorWindow.WMEraseBkgnd(var m : TWMEraseBkgnd);
begin m.Result:=LRESULT(False) end;

procedure TPluginEditorWindow.UpdaterTimer(Sender: TObject);
var j:integer;
begin
 manually:=true;
 j:=round(fpar[0]*100);if j<>par0.position then par0.position:=j;
 j:=round(fpar[1]*100);if j<>par1.position then par1.position:=j;
 j:=round(fpar[2]*100);if j<>par2.position then par2.position:=j;
 j:=round(fpar[3]*100);if j<>par3.position then par3.position:=j;
 j:=round(fpar[4]*100);if j<>par4.position then par4.position:=j;
 j:=round(fpar[5]*100);if j<>par5.position then par5.position:=j;
 j:=round(fpar[6]*100);if j<>par6.position then par6.position:=j;
 j:=round(fpar[7]*100);if j<>par7.position then par7.position:=j;
 j:=round(fpar[8]*100);if j<>par8.position then par8.position:=j;
 j:=round(fpar[9]*100);if j<>par9.position then par9.position:=j;
 j:=round(fpar[10]*100);if j<>par10.position then par10.position:=j;
 j:=round(fpar[11]*100);if j<>par11.position then par11.position:=j;
 manually:=false;
 j:=round(fpar[12]*3);
 if label12.caption<>qual[j] then label12.caption:=qual[j];
end;

procedure TPluginEditorWindow.par0Change(Sender: TObject);
var i,j:integer;
begin
 //graphic control is written to parameter
 i:=(sender as TDIBSlider).Tag;
 j:=(sender as TDIBSlider).position;
 case i of
 0:label1.caption:='Room: '+inttostr(round(j*100-10000));
 1:label5.caption:='Room HF: '+inttostr(round(j*100-10000));
 2:label6.caption:='Room Rolloff Factor: '+inttostr(j div 10);
 3:label7.caption:='Decay Time: '+floattostrf(j*0.01*19.9+0.1,fffixed,2,2)+' ms';
 4:label8.caption:='Decay HF Ratio: '+floattostrf(j*0.01*1.9+0.1,fffixed,2,2);
 5:label10.caption:='Reflections: '+inttostr(10000-j*110);
 6:label16.caption:='Reflections Delay: '+floattostrf(j*0.01*0.3,fffixed,2,2)+' ms';
 7:label15.caption:='Reverb: '+inttostr(j*120-10000);
 8:label17.caption:='Reverb Delay: '+floattostrf(j*0.01*0.1,fffixed,2,2)+' ms';
 9:label18.caption:='Diffusion: '+inttostr(j);
 10:label19.caption:='Density: '+inttostr(j);
 11:label20.caption:='HF Reference: '+inttostr(round(j*0.01*(20000-20)+20));
 else
 end;
 
 if manually then exit;
 fpar[i]:=j/100;
 if assigned(effect) then effect.setParameterAutomated(i,fpar[i]);
end;

procedure TPluginEditorWindow.Label12MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i:integer;
begin
 i:=round(fpar[12]*3);
 i:=(i+1) mod 4;
 label12.caption:=qual[i];
 fpar[12]:=i/3;
 if assigned(effect) then effect.setParameterAutomated(12,fpar[12]);
end;

procedure TPluginEditorWindow.FormCreate(Sender: TObject);
begin
 decimalseparator:='.';
 about.top:=48;
 about.left:=32;
end;

procedure TPluginEditorWindow.Label21MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 presets.Popup(mouse.CursorPos.x,mouse.CursorPos.y);
end;

procedure TPluginEditorWindow.a1Click(Sender: TObject);
begin
 if assigned(effect) then effect.setParameter(kpreset,(sender as tmenuitem).tag/50);
end;

procedure TPluginEditorWindow.aboutClick(Sender: TObject);
begin
 about.visible:=false;
end;

procedure TPluginEditorWindow.Label13Click(Sender: TObject);
begin
 about.visible:=not about.visible;
end;

procedure TPluginEditorWindow.Label11Click(Sender: TObject);
begin
 ShellExecute(0, 'open', 'http://www.tobybear.de', nil, nil, SW_SHOWDEFAULT);
end;

procedure TPluginEditorWindow.Label22Click(Sender: TObject);
begin
 ShellExecute(0, 'open', 'http://www.microsoft.com/directx', nil, nil, SW_SHOWDEFAULT);
end;

procedure TPluginEditorWindow.Label23Click(Sender: TObject);
begin
 ShellExecute(0, 'open', 'http://www.progdigy.com', nil, nil, SW_SHOWDEFAULT);
end;

procedure TPluginEditorWindow.Label24Click(Sender: TObject);
begin
 ShellExecute(0, 'open', 'http://dcdspfilter.corecodec.org', nil, nil, SW_SHOWDEFAULT);
end;

procedure TPluginEditorWindow.Label25Click(Sender: TObject);
begin
 ShellExecute(0, 'open', 'http://dsp-worx.de', nil, nil, SW_SHOWDEFAULT);
end;

procedure TPluginEditorWindow.Label26Click(Sender: TObject);
begin
 ShellExecute(0, 'open', 'http://www.steinberg.net', nil, nil, SW_SHOWDEFAULT);
end;

end.

