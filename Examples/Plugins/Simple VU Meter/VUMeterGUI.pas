unit VUMeterGUI;

interface

uses Windows, Messages, SysUtils, Classes, Forms, DDSPBase, DVSTModule,
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
    procedure ParameterChange(Sender: TObject);
  end;

implementation

{$R *.DFM}

uses VUMeterModule;

procedure TVSTVUMeterGUI.ParameterChange(Sender: TObject);
begin
 with (Sender as TScrollbar), (Owner As TVSTVUMeterModule)
  do Parameter[Tag]:=Position;
end;

end.