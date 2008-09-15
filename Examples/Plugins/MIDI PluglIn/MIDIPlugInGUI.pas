unit MIDIPlugInGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, ExtCtrls, Controls, StdCtrls,
  DAV_Common, DAV_VSTModule;

type
  TVSTGUI = class(TForm)
    LbInstructions: TLabel;
    LbVSTTrademark: TLabel;
    LbTranspose: TLabel;
    par0: TScrollBar;
    procedure par0Change(Sender: TObject);
  private
  public
  end;

implementation

{$R *.DFM}

uses MIDIPlugInModule;

procedure TVSTGUI.par0Change(Sender: TObject);
begin
 with (Owner As TMIDIModule)
  do Parameter[0]:=(Sender as TScrollbar).Position;
end;

end.