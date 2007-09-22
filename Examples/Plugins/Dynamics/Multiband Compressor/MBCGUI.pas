unit MBCGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DDSPBase, DVSTModule;

type
  TFmMBC = class(TForm)
  public
    MBCDataModule: TVSTModule;
  end;

implementation

{$R *.DFM}

end.