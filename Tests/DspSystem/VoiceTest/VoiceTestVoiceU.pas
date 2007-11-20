unit VoiceTestVoiceU;

interface

uses
  SysUtils, Classes, DDspVoice, DAVDProcessingComponent, DDspBaseComponent,
  DDSPBaseOsc, DDSPOscSaw, DDSPOscSine;

type
  TVoiceTestVoice = class(TDspVoice)
    DspOscSine1: TDspOscSine;
    procedure DspVoiceCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses math;

procedure TVoiceTestVoice.DspVoiceCreate(Sender: TObject);
begin
  DspOscSine1.Frequency:=440*power(2,(VoiceInfo.NoteNr-69)/12)
end;

end.
