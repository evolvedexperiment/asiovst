unit LoadVSTGUI;

interface

uses Windows, Messages, SysUtils, Classes, Forms, DDSPUtils, DVSTModule;

type
  TVSTGUI = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }
    theModule: TVSTModule;
  end;

implementation

{$R *.DFM}

end.