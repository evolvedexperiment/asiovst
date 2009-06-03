unit HMEditor;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DDSPBase, DVSTModule;

type
  TFmHMEditor = class(TForm)
  public
    HMModule: TVSTModule;
  end;

implementation

{$R *.DFM}

end.