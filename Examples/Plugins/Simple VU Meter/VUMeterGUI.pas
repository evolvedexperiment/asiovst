unit VUMeterGUI;

interface

uses Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule,
  Controls, StdCtrls, ExtCtrls;

type
  TVSTVUMeterGUI = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    vu_l: TShape;
    vu_r: TShape;
    gain_l: TLabel;
    gain_r: TLabel;
    par0: TScrollBar;
    par1: TScrollBar;
    Timer: TTimer;
    procedure ParameterChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  end;

implementation

{$R *.DFM}

uses VUMeterModule;

procedure TVSTVUMeterGUI.ParameterChange(Sender: TObject);
begin
 with (Sender as TScrollbar), (Owner As TVSTVUMeterModule)
  do Parameter[Tag]:=Position;
end;

procedure TVSTVUMeterGUI.TimerTimer(Sender: TObject);
var tmp : Integer;
begin
 with (Owner As TVSTVUMeterModule) do
  begin
   tmp := round(300+3*Amp_to_dB(Peak[0]));
   if tmp > 0 then vu_l.Width := tmp else vu_l.Width := 0;
   tmp := round(300+3*Amp_to_dB(Peak[1]));
   if tmp>0 then vu_r.Width := tmp else vu_r.Width := 0;
   gain_l.Caption := 'left gain: ' + inttostr(round(Parameter[0])) + ' db(fs)';
   gain_r.Caption := 'right gain: ' + inttostr(round(Parameter[1])) + ' db(fs)';
  end;
end;

end.