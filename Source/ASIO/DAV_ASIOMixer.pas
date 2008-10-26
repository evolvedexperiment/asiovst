unit DAV_ASIOMixer;

{$I ..\ASIOVST.inc}

{If this file makes troubles, delete the DEFINE ASIOMixer in DAV_ASIOHost}

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

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

end.
