unit PhaserFrm;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DDSPBase, DVSTModule;

type
  TPhaserForm = class(TForm)
  public
    PhaserModule: TVSTModule;
  end;

implementation

{$R *.DFM}

end.