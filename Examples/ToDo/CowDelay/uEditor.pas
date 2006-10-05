unit uEditor;
interface
uses Windows, Forms,Controls, Classes, ExtCtrls, ComCtrls, DAudioEffectX,
  StdCtrls, Graphics,gauges;

type
  TPluginEditorWindow = class(TForm)
    Updater: TTimer;
    bg: TImage;
    Label01: TLabel;
    Label03: TLabel;
    Label04: TLabel;
    Label05: TLabel;
    Label06: TLabel;
    Vol: TGauge;
    FltType: TImage;
    FltType2: TImage;
    Label1: TLabel;
    Wet: TGauge;
    Pan: TGauge;
    Label02: TLabel;
    DelayLen: TGauge;
    DelayFB: TGauge;
    Cutoff: TGauge;
    Reso: TGauge;
    dir: TLabel;
    thresh: TGauge;
    length: TGauge;
    Label3: TLabel;
    Label4: TLabel;
    procedure UpdaterTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FltTypeClick(Sender: TObject);
    procedure dirClick(Sender: TObject);
  private
    FEffect:AudioEffectX;
    procedure SldMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SldMouseDown(Sender: TObject; Button:TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  public
    manually:boolean;
    fWet,fDelayPan,fDelayLen,fDelayFB,fThresh,fLength,fDir,
    fCutoff,fResonance,fFType,fOutVol:single;
    property Effect: AudioEffectX read FEffect write FEffect;
  private
  end;

implementation

{$R *.DFM}

uses uPlugin;

procedure TPluginEditorWindow.UpdaterTimer(Sender: TObject);
begin
 manually:=true;
 Vol.progress:=round(fOutVol*vol.maxvalue);
 Wet.progress:=round(fWet*wet.maxvalue);
 Pan.progress:=round(fDelayPan*pan.maxvalue);
 if (fDir>0.5)and(dir.caption='Dn') then
  dir.caption:='Up' else
 if (fDir<=0.5)and(dir.caption='Up') then
  dir.caption:='Dn';
 DelayLen.progress:=round(fDelayLen*delaylen.maxvalue);
 DelayFB.progress:=round(fDelayFB*delayfb.maxvalue);
 Cutoff.progress:=round(fCutoff*cutoff.maxvalue);
 Reso.progress:=round(fResonance*reso.maxvalue);
 Thresh.progress:=round(fThresh*Thresh.maxvalue);
 Length.progress:=round(fLength*Length.maxvalue);
 if fFType>0.5 then
  FltType.visible:=true
 else
  FltType.visible:=false;
 manually:=false;
end;

procedure TPluginEditorWindow.SldMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
 if (Shift=[ssLeft])and (not manually) then
 begin
  if y>(Sender as TGauge).height then y:=(Sender as TGauge).height;
  if y<0 then y:=0;
  (Sender as TGauge).progress:=(Sender as TGauge).height-y;
  case (Sender as TGauge).tag of
  kWet:begin
   fWet:=Wet.progress/wet.maxvalue;
   Effect.setParameterAutomated(kWet,fWet);
  end;
  kDelayPan:begin
   fDelayPan:=Pan.progress/pan.maxvalue;
   Effect.setParameterAutomated(kDelayPan,fDelayPan);
  end;
  kOutVol:begin
   fOutVol:=Vol.progress/vol.maxvalue;
   Effect.setParameterAutomated(kOutVol,fOutVol);
  end;
  kThresh:begin
   fThresh:=Thresh.progress/Thresh.maxvalue;
   Effect.setParameterAutomated(kThresh,fThresh);
  end;
  kLength:begin
   fLength:=Length.progress/Length.maxvalue;
   Effect.setParameterAutomated(kLength,fLength);
  end;
  kDelayLen:begin
   fDelayLen:=DelayLen.progress/delaylen.maxvalue;
   Effect.setParameterAutomated(kDelayLen,fDelayLen);
  end;
  kDelayFB:begin
   fDelayFB:=DelayFB.progress/delayfb.maxvalue;
   Effect.setParameterAutomated(kDelayFB,fDelayFB);
  end;
  kCutoff:begin
   fCutoff:=Cutoff.progress/cutoff.maxvalue;
   Effect.setParameterAutomated(kCutoff,fCutoff);
  end;
  kResonance:begin
   fResonance:=Reso.progress/reso.maxvalue;
   Effect.setParameterAutomated(kResonance,fResonance);
  end;
  else
  end;

 end;
end;

procedure TPluginEditorWindow.SldMouseDown(Sender: TObject;
  Button:TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 SldMouseMove(Sender,Shift,X,Y);
end;

procedure TPluginEditorWindow.FormCreate(Sender: TObject);
begin
 Vol.onmousemove:=sldmousemove;Vol.onmousedown:=sldmousedown;
 Pan.onmousemove:=sldmousemove;Pan.onmousedown:=sldmousedown;
 Wet.onmousemove:=sldmousemove;Wet.onmousedown:=sldmousedown;

 DelayLen.onmousemove:=sldmousemove;DelayLen.onmousedown:=sldmousedown;
 DelayFB.onmousemove:=sldmousemove;DelayFB.onmousedown:=sldmousedown;
 Cutoff.onmousemove:=sldmousemove;Cutoff.onmousedown:=sldmousedown;
 Reso.onmousemove:=sldmousemove;Reso.onmousedown:=sldmousedown;
 Length.onmousemove:=sldmousemove;Length.onmousedown:=sldmousedown;
 Thresh.onmousemove:=sldmousemove;Thresh.onmousedown:=sldmousedown;

 vol.maxvalue:=vol.height;
 pan.maxvalue:=pan.height;
 wet.maxvalue:=wet.height;
 delaylen.maxvalue:=delaylen.height;
 delayfb.maxvalue:=delayfb.height;
 cutoff.maxvalue:=cutoff.height;
 reso.maxvalue:=reso.height;
 Length.maxvalue:=Length.height;
 Thresh.maxvalue:=Thresh.height;
end;

procedure TPluginEditorWindow.FltTypeClick(Sender: TObject);
begin
 if FltType.visible then
 begin
  FltType.visible:=false;
  fFType:=0;
 end else
 begin
  FltType.visible:=true;
  fFType:=1;
 end;
 Effect.setParameterAutomated(kFType,fFType);
end;

procedure TPluginEditorWindow.dirClick(Sender: TObject);
begin
 if dir.caption='Up' then
 begin
  dir.caption:='Dn';
  fDir:=0;
 end else
 begin
  fDir:=1;
  dir.caption:='Up';
 end;
 Effect.setParameterAutomated(kDir,fDir);
end;

end.

