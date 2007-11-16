unit VoiceTestModuleU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, 
  DAVDCommon, DVSTModule;

type
  TVoiceTestModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
  private
  public
  end;

implementation

{$R *.DFM}

uses
  VoiceTestFormU;

procedure TVoiceTestModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TVoiceTestForm.Create(Self);
end;

end. 