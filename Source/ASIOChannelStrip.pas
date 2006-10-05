unit ASIOChannelStrip;

{If this file makes troubles, delete the DEFINE ASIOMixer in DASIOHost}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls;

type
  TFrChannelStrip = class(TFrame)
    TrackBar: TTrackBar;
    CBMute: TCheckBox;
    procedure TrackBarChange(Sender: TObject);
    procedure CBMuteClick(Sender: TObject);
  private
    fChannel: Integer;
    fVolume: Single;
    fMute: Boolean;
    fSolo: Boolean;
    fOnVolumeChange: TNotifyEvent;
    fOnMuteChange: TNotifyEvent;
    fOnSoloChange: TNotifyEvent;
  public
    property Channel: Integer read fChannel write fChannel default -1;
    property Mute: Boolean read fMute Write fMute;
    property Solo: Boolean read fSolo Write fSolo;
    property Volume: Single read fVolume Write fVolume;
    property OnVolumeChange: TNotifyEvent read fOnVolumeChange write fOnVolumeChange;
    property OnMuteChange: TNotifyEvent read fOnMuteChange write fOnMuteChange;
    property OnSoloChange: TNotifyEvent read fOnSoloChange write fOnSoloChange;
  end;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

procedure TFrChannelStrip.TrackBarChange(Sender: TObject);
begin
 fVolume:=(TrackBar.Max-TrackBar.Position)/TrackBar.Max;
 if Assigned(fOnVolumeChange) then fOnVolumeChange(Self);
 TrackBar.Hint:=IntToStr(round(fVolume*100))+'%';
end;

procedure TFrChannelStrip.CBMuteClick(Sender: TObject);
begin
 fMute:=CBMute.Checked;
 if Assigned(fOnMuteChange) then fOnMuteChange(Self);
 if fMute
  then CBMute.Hint:='Mute'
  else CBMute.Hint:='';
end;

end.
