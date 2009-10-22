unit VUMeterGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, StdCtrls, ExtCtrls,
  DAV_Types, DAV_VSTModule;

type
  TVSTVUMeterGUI = class(TForm)
    LbAbout: TLabel;
    LbTrademark: TLabel;
    ShVULeft: TShape;
    ShVURight: TShape;
    LbGainLeft: TLabel;
    LbGainRight: TLabel;
    SBLeft: TScrollBar;
    SBRight: TScrollBar;
    Timer: TTimer;
    procedure ParameterChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  end;

implementation

{$R *.DFM}

uses
  VUMeterModule, Dialogs;

procedure TVSTVUMeterGUI.ParameterChange(Sender: TObject);
begin
 with TVSTVUMeterModule(Owner), (Sender as TScrollbar)
  do Parameter[Tag] := Position;
end;

procedure TVSTVUMeterGUI.TimerTimer(Sender: TObject);
var
  tmp : Integer;
begin
  with (Owner As TVSTVUMeterModule) do
  begin
    tmp := round(300 + 3 * Amp_to_dB(Peak[0]));

    if tmp > 0 then ShVULeft.Width := tmp else ShVULeft.Width := 0;
    tmp := round(300 + 3 * Amp_to_dB(Peak[1]));

    if tmp>0 then ShVURight.Width := tmp else ShVURight.Width := 0;
    LbGainLeft.Caption := 'left gain: ' + IntToStr(round(Parameter[0])) + ' db(fs)';
    LbGainRight.Caption := 'right gain: ' + IntToStr(round(Parameter[1])) + ' db(fs)';
  end;
end;

end.
