unit DVSTPluginWizardDialog;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls;

type
  TVSTPluginWizardDialog = class(TForm)
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
  VSTPluginWizardDialog: TVSTPluginWizardDialog;

implementation

{$R *.DFM}

end.
