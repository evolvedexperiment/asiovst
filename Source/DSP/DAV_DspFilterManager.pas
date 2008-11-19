unit DAV_DspFilterManager;

interface

uses
  Classes, DAV_Common, DAV_AudioData, DAV_DspFilter;

type
  TDAVFilterChannels = class(TOwnedCollection);
  TDAVFilterChains = class(TOwnedCollection);

  TDAVFilterManager = class(TComponent)
  private
    FFilterChannels: TDAVFilterChannels;
    FFilterChains: TDAVFilterChains;
  published
    property Channels: TDAVFilterChannels read FFilterChannels write FFilterChannels;
    property Chains: TDAVFilterChains read FFilterChains write FFilterChains;
  end;

implementation

end.
