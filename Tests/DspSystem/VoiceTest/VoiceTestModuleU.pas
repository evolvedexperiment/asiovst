unit VoiceTestModuleU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms,
  DAVDCommon, DVSTModule, DAVDProcessingComponent, DDspBaseComponent,
  DDspVoiceController, DDspVoice, DDSPBaseOsc, DDSPOscSine;

type
  TVoiceTestModule = class(TVSTModule)
    DspVoiceController1: TDspVoiceController;
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure DspVoiceController1CreateVoice(Sender: TDspVoiceController;
      MidiEvent: TAVDMidiEvent; var NewVoice: TDspVoice);
  private
  public
  end;

implementation

{$R *.DFM}

uses
  VoiceTestFormU, VoiceTestVoiceU;

procedure TVoiceTestModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TVoiceTestForm.Create(Self);
end;

procedure TVoiceTestModule.DspVoiceController1CreateVoice(
  Sender: TDspVoiceController; MidiEvent: TAVDMidiEvent;
  var NewVoice: TDspVoice);
var x: TDspVoiceInfo;
begin
  x := TDspVoiceInfo.Create(MidiEvent);
  NewVoice := TVoiceTestVoice.Create(Sender, x);
end;

end.