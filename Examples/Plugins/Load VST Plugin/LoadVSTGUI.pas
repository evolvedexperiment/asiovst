unit LoadVSTGUI;

interface

uses Windows, Messages, SysUtils, Classes, Forms, DVSTModule;

type
  TVSTGUI = class(TForm)
  private
  public
    theModule: TVSTModule;
  end;

implementation

{$R *.DFM}

end.