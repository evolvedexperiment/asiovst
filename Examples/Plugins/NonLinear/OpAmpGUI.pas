unit OpAmpGUI;

{$I ASIOVST.inc}

interface

uses {$IFDEF FPC}LCLIntf, LResources, {$ENDIF} Messages, SysUtils, Classes,
     Forms, Controls, StdCtrls, DAV_Common, DAV_VSTModule;

type
  TVSTGUI = class(TForm)
    SBGain: TScrollBar;
    LbGain: TLabel;
    procedure SBGainChange(Sender: TObject);
  private
  public
    theModule: TVSTModule;
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses OpAmpModule;

procedure TVSTGUI.SBGainChange(Sender: TObject);
begin
 TVSTOpAmp(Owner).Parameter[0] := SBGain.Position * 0.1;
end;

{$IFDEF FPC}
initialization
  {$i OpAmpGUI.lrs}
{$ENDIF}

end.