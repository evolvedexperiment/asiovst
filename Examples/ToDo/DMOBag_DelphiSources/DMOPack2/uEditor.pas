unit uEditor;
// In this file all editor related stuff is managed.
// It can practically be an own program that just has to send
// some data via setparameter/setparameterautomated to the
// main plugin/algorithm.
// For quick access, I created a Delphi project called GUItest.dpr
// in this folder that allows you to design and test the GUI as a
// standalone application

interface
uses Windows, Forms, DAudioEffectX, Controls, Classes, ExtCtrls, ComCtrls, Graphics,
  StdCtrls, Messages, cDIBImageList, cDIBSlider, cDIBControl, cDIBImage,
  cDIBPanel, cDIBFeatures, cDIBTimer, cDIBFadeLabel, cDIBStandardCompressors,
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
    Label2: TLabel;
    par2: TDIBSlider;
    Label6: TLabel;
    Label7: TLabel;
    par3: TDIBSlider;
    Label9: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    about: TPanel;
    Label8: TLabel;
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
    procedure Label11Click(Sender: TObject);
    procedure Label23Click(Sender: TObject);
    procedure Label24Click(Sender: TObject);
    procedure Label22Click(Sender: TObject);
    procedure Label25Click(Sender: TObject);
    procedure Label26Click(Sender: TObject);
    procedure aboutClick(Sender: TObject);
    procedure Label13Click(Sender: TObject);
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

const pshift:array[0..4] of string=('-180°','-90°','0°','90°','180°');
      wave:array[0..1] of string=('sine','triangle');

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
 manually:=false;
 {$IFDEF Gargle}
 j:=round(fpar[1]);
 if label12.caption<>wave[j] then label12.caption:=wave[j];
 {$ENDIF}
end;

procedure TPluginEditorWindow.par0Change(Sender: TObject);
var i,j:integer;
begin
 //graphic control is written to parameter
 i:=(sender as TDIBSlider).Tag;
 j:=(sender as TDIBSlider).position;
 case i of

 {$IFDEF EQ}
 0:label1.caption:='Gain: '+inttostr(round(j*0.01*30-15))+' dB';
 1:label5.caption:='Center Frequency: '+inttostr(round(j*0.01*15920+80))+ 'Hz';
 2:label6.caption:='Bandwidth: '+floattostrf(j*0.01*35+1,fffixed,2,0)+' Hz';
 {$ENDIF}

 {$IFDEF Gargle}
 0:label1.caption:='Rate: '+inttostr(round(j*0.01*999+1))+' Hz';
 {$ENDIF}

 {$IFDEF WAVESREVERB}
 0:label1.caption:='Gain: '+inttostr(round(j*0.01*96-96))+' dB';
 1:label5.caption:='Reverb Mix: '+inttostr(round(j*0.01*96-96))+' dB';
 2:label6.caption:='Reverb Time: '+floattostrf(j*0.01*(3000-0.001)+0.001,fffixed,4,0)+' ms';
 3:label7.caption:='High Frequency Ratio: '+floattostrf(j*0.01*(0.999-0.001)+0.001,fffixed,2,2);
 {$ENDIF}

 {$IFDEF Echo}
 0:label1.caption:='Delay Time Left: '+inttostr(round(j*0.01*1999+1))+' ms';
 1:label5.caption:='Delay Time Right: '+inttostr(round(j*0.01*1999+1))+' ms';
 2:label6.caption:='Feedback: '+inttostr(j)+'%';
 3:label7.caption:='Crossover: '+inttostr(j)+'%';
 {$ENDIF}

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
 i:=round(fpar[1]);
 i:=1-i;
 label12.caption:=wave[i];
 fpar[1]:=i;
 if assigned(effect) then effect.setParameterAutomated(1,fpar[1]);
end;

procedure TPluginEditorWindow.FormCreate(Sender: TObject);
begin
 decimalseparator:='.';
 about.left:=0;
 about.top:=0;
end;

procedure TPluginEditorWindow.Label11Click(Sender: TObject);
begin
 ShellExecute(0, 'open', 'http://www.tobybear.de', nil, nil, SW_SHOWDEFAULT);
end;

procedure TPluginEditorWindow.Label23Click(Sender: TObject);
begin
 ShellExecute(0, 'open', 'http://www.progdigy.com', nil, nil, SW_SHOWDEFAULT);
end;

procedure TPluginEditorWindow.Label24Click(Sender: TObject);
begin
 ShellExecute(0, 'open', 'http://dcdspfilter.corecodec.org', nil, nil, SW_SHOWDEFAULT);
end;

procedure TPluginEditorWindow.Label22Click(Sender: TObject);
begin
 ShellExecute(0, 'open', 'http://www.microsoft.com/directx', nil, nil, SW_SHOWDEFAULT);
end;

procedure TPluginEditorWindow.Label25Click(Sender: TObject);
begin
 ShellExecute(0, 'open', 'http://dsp-worx.de', nil, nil, SW_SHOWDEFAULT);
end;

procedure TPluginEditorWindow.Label26Click(Sender: TObject);
begin
 ShellExecute(0, 'open', 'http://www.steinberg.net', nil, nil, SW_SHOWDEFAULT);
end;

procedure TPluginEditorWindow.aboutClick(Sender: TObject);
begin
 about.Visible:=false;
end;

procedure TPluginEditorWindow.Label13Click(Sender: TObject);
begin
 about.Visible:=true;
end;

end.

