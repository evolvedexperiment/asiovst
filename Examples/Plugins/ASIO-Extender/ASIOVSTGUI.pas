unit ASIOVSTGUI;

interface

uses Windows, Messages, SysUtils, Classes, Forms, DDSPBase, DVSTModule,
  DASIOHost, Controls, StdCtrls;

type
  TFmASIOVST = class(TForm)
    Lb_ASIOOutput: TLabel;
    CB_ASIO: TComboBox;
  private
  public
    theModule: TVSTModule;
  end;

implementation

{$R *.DFM}

end.
