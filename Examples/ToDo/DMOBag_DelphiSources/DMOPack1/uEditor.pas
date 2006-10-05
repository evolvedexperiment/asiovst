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
    DIBImage3: TDIBImage;
    Label2: TLabel;
    par2: TDIBSlider;
    Label6: TLabel;
    Label7: TLabel;
    par3: TDIBSlider;
    Label8: TLabel;
    par4: TDIBSlider;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    par5: TDIBSlider;
    Label15: TLabel;
    about: TPanel;
    Label16: TLabel;
    Label17: TLabel;
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
    procedure Label11MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure Label17Click(Sender: TObject);
    procedure Label22Click(Sender: TObject);
    procedure Label23Click(Sender: TObject);
    procedure Label24Click(Sender: TObject);
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
 j:=round(fpar[4]*100);if j<>par4.position then par4.position:=j;
 {$IFDEF Compressor}
 j:=round(fpar[5]*100);if j<>par5.position then par5.position:=j;
 manually:=false;
 {$ELSE}
 manually:=false;
 j:=round(fpar[5]);
 if label12.caption<>wave[j] then label12.caption:=wave[j];
 j:=round(fpar[6]*4);
 if label11.caption<>pshift[j] then label11.caption:=pshift[j];
 {$ENDIF}
end;

procedure TPluginEditorWindow.par0Change(Sender: TObject);
var i,j:integer;
begin
 //graphic control is written to parameter
 i:=(sender as TDIBSlider).Tag;
 j:=(sender as TDIBSlider).position;
 case i of
 {$IFDEF Distortion}
 0:label1.caption:='Gain: '+inttostr(round(j*0.01*60-60))+' dB';
 1:label5.caption:='Edge: '+inttostr(j)+'%';
 2:label6.caption:='PostEQ Frequency: '+floattostrf(j*79+100,fffixed,4,0)+' Hz';
 3:label7.caption:='PostEQ Bandwidth: '+floattostrf(j*79+100,fffixed,4,0)+' Hz';
 4:label8.caption:='PreLowpass Cutoff: '+floattostrf(j*79+100,fffixed,4,0)+' Hz';
 {$ENDIF}

 {$IFDEF Compressor}
 0:label1.caption:='Gain: '+inttostr(round(j*0.01*120-60))+' dB';
 1:label5.caption:='Attack: '+floattostrf((j*0.01)*(500-0.01)+0.01,fffixed,4,0)+' ms';
 2:label6.caption:='Release: '+floattostrf((j*0.01)*(3000-50)+50,fffixed,4,0)+' ms';
 3:label7.caption:='Threshold: '+floattostrf(j*0.6-60,fffixed,4,0)+' dB';
 4:label8.caption:='Ratio: 1:'+floattostrf(j*0.01*99+1,fffixed,4,0);
 5:label15.caption:='PreDelay: '+floattostrf(j*0.01*4,fffixed,3,1)+' ms';
 {$ENDIF}

 {$IFDEF Flanger}
 0:label1.caption:='Wet/Dry Mix: '+inttostr(j)+':'+inttostr(100-j);
 1:label5.caption:='Depth: '+inttostr(j)+'%';
 2:label6.caption:='Feedback: '+inttostr(j*2-100)+'%';
 3:label7.caption:='Frequency: '+inttostr(j)+'%';
 4:label8.caption:='Delay: '+floattostrf(j/5,fffixed,3,2)+' ms';
 {$ENDIF}

 {$IFDEF Chorus}
 0:label1.caption:='Wet/Dry Mix: '+inttostr(j)+':'+inttostr(100-j);
 1:label5.caption:='Depth: '+inttostr(j)+'%';
 2:label6.caption:='Feedback: '+inttostr(j*2-100)+'%';
 3:label7.caption:='Frequency: '+inttostr(j)+'%';
 4:label8.caption:='Delay: '+floattostrf(j/5,fffixed,3,2)+' ms';
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
 i:=round(fpar[5]);
 i:=1-i;
 label12.caption:=wave[i];
 fpar[5]:=i;
 if assigned(effect) then effect.setParameterAutomated(5,fpar[5]);
end;

procedure TPluginEditorWindow.Label11MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var i:integer;
begin
 i:=round(fpar[6]*4);
 i:=(i+1) mod 5;
 label11.caption:=pshift[i];
 fpar[6]:=i/4;
 if assigned(effect) then effect.setParameterAutomated(6,fpar[6]);
end;

procedure TPluginEditorWindow.FormCreate(Sender: TObject);
begin
 decimalseparator:='.';
 about.left:=32;
 about.top:=45;
end;

procedure TPluginEditorWindow.Label17Click(Sender: TObject);
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

procedure TPluginEditorWindow.aboutClick(Sender: TObject);
begin
 about.visible:=false;
end;

procedure TPluginEditorWindow.Label13Click(Sender: TObject);
begin
 about.visible:=true;
end;

end.

