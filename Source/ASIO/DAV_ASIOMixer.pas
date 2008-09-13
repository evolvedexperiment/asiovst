unit DAV_ASIOMixer;

{If this file makes troubles, delete the DEFINE ASIOMixer in DASIOHost}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, DAV_ASIOChannelStrip;

type
  TFmASIOMixer = class(TForm)
    MixerPanel: TPanel;
  private
  public
    ChannelsStrips: array of TFrChannelStrip;
  end;

implementation

{$R *.DFM}

end.
