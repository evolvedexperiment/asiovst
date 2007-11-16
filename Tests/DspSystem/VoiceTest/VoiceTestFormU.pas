unit VoiceTestFormU;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule;

type
  TVoiceTestForm = class(TForm)
  public
    VoiceTestModule: TVSTModule;
  end;

implementation

{$R *.DFM}

end. 