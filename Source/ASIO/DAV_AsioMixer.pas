unit DAV_ASIOMixer;

interface

{$I ..\DAV_Compiler.inc}

{If this file makes troubles, delete the DEFINE ASIOMixer in DAV_ASIOHost}

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
