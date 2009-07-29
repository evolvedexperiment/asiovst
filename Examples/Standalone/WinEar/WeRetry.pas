unit WeRetry;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TFmRetry = class(TForm)
    LbWrong: TLabel;
    BtOK: TButton;
    BtRetry: TButton;
  end;

var
  FmRetry: TFmRetry;

implementation

uses
  WeMain;

{$R *.DFM}

end.
