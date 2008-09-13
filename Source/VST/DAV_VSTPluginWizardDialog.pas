unit DVSTPluginWizardGuiDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls;

type
  TVSTPluginWizardGuiDialog = class(TForm)
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
  VSTPluginWizardGuiDialog: TVSTPluginWizardGuiDialog;

implementation

{$R *.DFM}

end.
