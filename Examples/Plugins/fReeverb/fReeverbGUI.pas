unit fReeverbGUI;

interface

uses Windows, Messages, SysUtils, Classes, Forms, DDSPBase, DVSTModule,
  Controls, StdCtrls, XPStyleActnCtrls, ActnList, ActnMan, ToolWin,
  ActnCtrls, ActnMenus;

type

  TFmReverb = class(TForm)
    SDry: TScrollBar;
    LbDry: TLabel;
    SWet: TScrollBar;
    LbWet: TLabel;
    SWidth: TScrollBar;
    Label1: TLabel;
    SRoomSize: TScrollBar;
    LbSize: TLabel;
    CBFreeze: TCheckBox;
    SStretch: TScrollBar;
    LbStretch: TLabel;
    SDamp: TScrollBar;
    LbDamp: TLabel;
    AMMB1: TActionMainMenuBar;
    AM1: TActionManager;
    procedure SWetChange(Sender: TObject);
    procedure SDryChange(Sender: TObject);
    procedure SWidthChange(Sender: TObject);
    procedure SRoomSizeChange(Sender: TObject);
    procedure CBFreezeClick(Sender: TObject);
    procedure SStretchChange(Sender: TObject);
    procedure SDampChange(Sender: TObject);
  private
  public
    theModule: TVSTModule;
  end;

implementation

{$R *.DFM}

procedure TFmReverb.SDryChange(Sender: TObject);
begin
 theModule.Parameter[0]:=SDry.Max-SDry.Position;
end;

procedure TFmReverb.SWetChange(Sender: TObject);
begin
 theModule.Parameter[1]:=SWet.Max-SWet.Position;
end;

procedure TFmReverb.SWidthChange(Sender: TObject);
begin
 theModule.Parameter[2]:=SWidth.Max-SWidth.Position;
end;

procedure TFmReverb.SRoomSizeChange(Sender: TObject);
begin
 theModule.Parameter[3]:=(SRoomSize.Max-SRoomSize.Position)/SRoomSize.Max;
end;

procedure TFmReverb.CBFreezeClick(Sender: TObject);
begin
 theModule.Parameter[4]:=Byte(CBFreeze.Checked);
end;

procedure TFmReverb.SStretchChange(Sender: TObject);
begin
 theModule.Parameter[5]:=(SStretch.Max-SStretch.Position);
end;

procedure TFmReverb.SDampChange(Sender: TObject);
begin
 theModule.Parameter[6]:=SDamp.Max-SDamp.Position;
end;

end.
