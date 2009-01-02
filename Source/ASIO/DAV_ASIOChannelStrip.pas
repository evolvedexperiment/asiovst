unit DAV_ASIOChannelStrip;

{If this file makes troubles, delete the DEFINE ASIOMixer in DASIOHost}

interface

{$I ..\DAV_Compiler.inc}

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
    FChannel        : Integer;
    FMute           : Boolean;
    FOnMuteChange   : TNotifyEvent;
    FOnSoloChange   : TNotifyEvent;
    FOnVolumeChange : TNotifyEvent;
    FSolo           : Boolean;
    FVolume         : Single;
  public
    property Channel: Integer read FChannel write FChannel default -1;
    property Mute: Boolean read FMute Write FMute;
    property OnMuteChange: TNotifyEvent read FOnMuteChange write FOnMuteChange;
    property OnSoloChange: TNotifyEvent read FOnSoloChange write FOnSoloChange;
    property OnVolumeChange: TNotifyEvent read FOnVolumeChange write FOnVolumeChange;
    property Solo: Boolean read FSolo Write FSolo;
    property Volume: Single read FVolume Write FVolume;
  end;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

procedure TFrChannelStrip.TrackBarChange(Sender: TObject);
begin
 FVolume := (TrackBar.Max - TrackBar.Position) / TrackBar.Max;
 if Assigned(FOnVolumeChange) then FOnVolumeChange(Self);
 TrackBar.Hint := IntToStr(round(FVolume * 100)) + '%';
end;

procedure TFrChannelStrip.CBMuteClick(Sender: TObject);
begin
 FMute := CBMute.Checked;
 if Assigned(FOnMuteChange) then FOnMuteChange(Self);
 if FMute
  then CBMute.Hint := 'Mute'
  else CBMute.Hint := '';
end;

end.
