unit uEditor;

interface

uses Windows, Forms, DAudioEffectX, Messages, ExtCtrls,
     Classes, Graphics, StdCtrls, Controls, DVstUtils,
     DVstTemplate, daeffectx, DVSTExtra;

type
  TPluginEditorWindow = class(TForm)
    Updater: TTimer;
    bg: TImage;
    vow_a: TImage;
    vow_e: TImage;
    vow_i: TImage;
    vow_o: TImage;
    vow_u: TImage;
    prelease: TImage;
    release: TImage;
    info: TLabel;
    pattack: TImage;
    attack: TImage;
    filterlogo: TImage;
    drywet: TImage;
    pdrywet: TImage;
    morph: TImage;
    pmorph: TImage;
    glide: TImage;
    pglide: TImage;
    noi: TImage;
    saw: TImage;
    squ: TImage;
    vol: TImage;
    pvol: TImage;
    rand: TImage;
    about: TImage;
    aboutscr: TImage;
    version: TLabel;
    procedure vow_aClick(Sender: TObject);
    procedure vow_eClick(Sender: TObject);
    procedure vow_iClick(Sender: TObject);
    procedure vow_oClick(Sender: TObject);
    procedure vow_uClick(Sender: TObject);
    procedure releaseMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure releaseMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure attackMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure attackMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure drywetMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure drywetMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure morphMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure morphMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure glideMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure glideMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure vow_aMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure vow_eMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure vow_iMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure vow_oMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure vow_uMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure UpdaterTimer(Sender: TObject);
    procedure bgMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure sawMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure squMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure noiMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ssnMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure volMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure volMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure randMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure randMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure aboutMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure aboutMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure aboutscrClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
   manually: boolean;
   FEffect : TVstTemplate;
   procedure OnEditorOpen(var Msg: TMessage); message WM_EDITOROPEN;
  public
    property Effect: TVstTemplate read FEffect write FEffect;
  private
  end;

implementation

{$R *.DFM}

uses uPlugin, SysUtils;

procedure TPluginEditorWindow.vow_aClick(Sender: TObject); begin effect.setparameterAutomated(7,0.0); end;
procedure TPluginEditorWindow.vow_eClick(Sender: TObject); begin effect.setparameterAutomated(7,0.1); end;
procedure TPluginEditorWindow.vow_iClick(Sender: TObject); begin effect.setparameterAutomated(7,0.2); end;
procedure TPluginEditorWindow.vow_oClick(Sender: TObject); begin effect.setparameterAutomated(7,0.3); end;
procedure TPluginEditorWindow.vow_uClick(Sender: TObject); begin effect.setparameterAutomated(7,0.4); end;

procedure TPluginEditorWindow.volMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); begin volmousemove(sender,shift,x,y); end;
procedure TPluginEditorWindow.drywetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); begin drywetmousemove(sender,shift,x,y); end;
procedure TPluginEditorWindow.attackMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); begin attackmousemove(sender,shift,x,y); end;
procedure TPluginEditorWindow.releaseMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); begin releasemousemove(sender,shift,x,y); end;
procedure TPluginEditorWindow.glideMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); begin glidemousemove(sender,shift,x,y); end;
procedure TPluginEditorWindow.morphMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); begin morphmousemove(sender,shift,x,y); end;

function f_limit(v:Single;l:Single=-1;u:Single=1):Integer;
begin
 if v<l then result:=round(l)
 else if v>u then result:=round(u) else result:=round(v);
end;

procedure TPluginEditorWindow.volMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
 if manually then exit;
 if (shift=[ssleft]) and assigned(effect) then
  begin
   y:=f_limit(y,0,vol.height);
   pvol.top:=y+vol.top-4;
   Effect.setParameterAutomated(0,y/vol.height);
  end;
 y:=150-round(150*y/vol.height);
 info.caption:='output volume: '+inttostr(y)+'%';
end;

procedure TPluginEditorWindow.drywetMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
 if manually then exit;
 if (shift=[ssleft]) and assigned(effect) then
  begin
   y:=f_limit(y,0,drywet.height);
   pdrywet.top:=y+drywet.top-4;
   Effect.setParameterAutomated(1,y/drywet.height);
  end;
 y:=round(100*y/drywet.height);
 info.caption:='wet/dry mix: '+inttostr(100-y)+':'+inttostr(y);
end;

procedure TPluginEditorWindow.attackMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
 if manually then exit;
 if (shift=[ssleft]) and assigned(effect) then
  begin
   x:=f_limit(x,0,attack.width);
   pattack.left:=x+attack.left-5;
   Effect.setParameterAutomated(3,x/attack.width);
  end;
 info.caption:='attack rate: '+inttostr(round(100*x/attack.width))+'%';
end;

procedure TPluginEditorWindow.releaseMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
 if manually then exit;
 if (shift=[ssleft]) and assigned(effect) then
  begin
   x:=f_limit(x,0,release.width);
   prelease.left:=x+release.left-5;
   Effect.setParameterAutomated(4,x/release.width);
  end;
 info.caption:='release rate: '+inttostr(round(100*x/release.width))+'%';
end;

procedure TPluginEditorWindow.glideMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
 if manually then exit;
 if (shift=[ssleft]) and assigned(effect) then
  begin
   x:=f_limit(x,0,glide.width);
   pglide.left:=x+glide.left-5;
   Effect.setParameterAutomated(5,x/glide.width);
  end;
 info.caption:='glide rate: '+inttostr(round(100*x/glide.width))+'%';
end;

procedure TPluginEditorWindow.morphMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
 if manually then exit;
 if (shift=[ssleft]) and assigned(effect) then
  begin
   x:=f_limit(x,0,morph.width);
   pmorph.left:=x+morph.left-5;
   Effect.setParameterAutomated(6,x/morph.width);
  end;
 info.caption:='morph time (ms): '+inttostr(round(4000*x/morph.width));
end;

procedure TPluginEditorWindow.UpdaterTimer(Sender: TObject);
var j:Integer;
begin
 if (not assigned(Effect)) or(not Effect.editorNeedsUpdate)
  then exit;
 manually := true;
 // parameters are written to the graphic controls
 // for performance reasons, always make sure you update a graphic
 // control only if the actual value it represents has changed!
 with Effect do
 begin
  j := round(getParameter(0)*vol.Height); if j <> pvol.top then pvol.top := j + vol.top-4;
  j := round(getParameter(1)*drywet.Height); if j <> pdrywet.top then pdrywet.top := j + drywet.top-4;
  j := round(getParameter(2)*100);
  if j<30 then begin saw.visible:=true; squ.visible:=false; noi.visible:=false; end else
  if j<60 then begin saw.visible:=false; squ.visible:=true; noi.visible:=false; end else
               begin saw.visible:=false; squ.visible:=false; noi.visible:=true; end;

  j := round(getParameter(3)*attack.width); if j <> pattack.left then pattack.left:=j+attack.left-5;
  j := round(getParameter(4)*release.width); if j <> prelease.left then prelease.left:=j+release.left-5;
  j := round(getParameter(5)*glide.width); if j <> pglide.left then pglide.left:=j+glide.left-5;
  j := round(getParameter(6)*morph.width); if j <> pmorph.left then pmorph.left:=j+morph.left-5;
 end;
 manually := false;
 Effect.editorNeedsUpdate := false;
end;

procedure TPluginEditorWindow.noiMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if manually then exit;
 if assigned(effect) then effect.setParameterAutomated(2,0.0);
 saw.visible:=true;
 squ.visible:=false;
 noi.visible:=false;
end;

procedure TPluginEditorWindow.sawMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if manually then exit;
 if assigned(effect) then effect.setParameterAutomated(2,0.4);
 saw.visible:=false;
 squ.visible:=true;
 noi.visible:=false;
end;

procedure TPluginEditorWindow.squMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 if manually then exit;
 if assigned(effect) then effect.setParameterAutomated(2,0.8);
 saw.visible:=false;
 squ.visible:=false;
 noi.visible:=true;
end;

procedure TPluginEditorWindow.randMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var c:integer;
begin
 for c:=1 to 6 do effect.setparameter(c,random);
end;

procedure TPluginEditorWindow.ssnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); begin info.caption:='choose synth waveform'; end;
procedure TPluginEditorWindow.randMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); begin info.caption:='randomize preset!'; end;
procedure TPluginEditorWindow.aboutMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); begin info.caption:='about this freaky plugin!'; end;
procedure TPluginEditorWindow.bgMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); begin info.caption:=''; end;
procedure TPluginEditorWindow.vow_aMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); begin info.caption:='morph to vowel "a"'; end;
procedure TPluginEditorWindow.vow_eMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); begin info.caption:='morph to vowel "e"'; end;
procedure TPluginEditorWindow.vow_iMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); begin info.caption:='morph to vowel "i"'; end;
procedure TPluginEditorWindow.vow_oMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); begin info.caption:='morph to vowel "o"'; end;
procedure TPluginEditorWindow.vow_uMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer); begin info.caption:='morph to vowel "u"'; end;

procedure TPluginEditorWindow.aboutMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer); begin aboutscr.visible:=not aboutscr.visible; end;
procedure TPluginEditorWindow.aboutscrClick(Sender: TObject); begin aboutscr.visible:=false; end;
procedure TPluginEditorWindow.FormCreate(Sender: TObject); begin aboutscr.left:=0; aboutscr.top:=28; end;

procedure TPluginEditorWindow.OnEditorOpen(var Msg: TMessage);
begin
 Effect := TVstTemplate(Msg.WParam);
end;

end.
