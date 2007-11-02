unit DAVDProcessingComponent;

interface

uses Classes;

type
  TAVDProcessingComponent = class(TComponent)
  protected
    fBypass: Boolean;
    fEnabled: Boolean;
    fSampleRate: Single;
    fChannels: Integer;

    procedure SetSampleRate(const Value: Single); virtual; abstract;
    procedure SetChannels(const Value: Integer); virtual; abstract;
  public
    procedure Init; virtual; abstract;
    procedure Reset; virtual; abstract;

    property Enabled: Boolean   read fEnabled    write fEnabled      default true;
    property Bypass: Boolean    read fBypass     write fBypass       default true;
    property Channels: Integer  read fChannels   write SetChannels   default 2;
    property SampleRate: Single read fSampleRate write SetSampleRate;
  end;

implementation

end.
