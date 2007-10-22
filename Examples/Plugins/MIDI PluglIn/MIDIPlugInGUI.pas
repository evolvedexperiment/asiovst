unit MIDIPlugInGUI;

interface

uses Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule,
  ExtCtrls, Controls, StdCtrls;

type
  TVSTGUI = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
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