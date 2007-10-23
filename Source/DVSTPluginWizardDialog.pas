unit DVSTPluginWizarDGuiDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls;

type
  TVSTPluginWizarDGuiDialog = class(TForm)
    EdClassName: TEdit;
    LbEffectName: TLabel;
    BtCreate: TButton;
    GBFlags: TGroupBox;
    CBEditor: TCheckBox;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  VSTPluginWizarDGuiDialog: TVSTPluginWizarDGuiDialog;

implementation

{$R *.DFM}

end.
