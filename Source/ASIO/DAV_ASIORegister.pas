unit DAV_ASIORegister;

interface

{$I ASIOVST.INC}

procedure Register;

implementation

{$IFNDEF FPC}{$R DAV_ASIOHost.res}{$ENDIF}

uses
  {$IFDEF FPC} LResources, {$ENDIF} Classes, DAV_ASIOHOST, DAV_ASIOGenerator;

procedure Register;
begin
 RegisterComponents('ASIO/VST Basics', [TASIOHost, TASIOHostBasic,
   TASIOHostAudioData, TASIOGeneratorNoise, TASIOGeneratorTone]);
 {$IFDEF DELPHI5}
 {$IFDEF D5CP}
 RegisterComponentEditor(TASIOHost, TASIOControlPanel);
 {$ENDIF}
 {$ENDIF}
end; 

{$IFDEF FPC}
  initialization
  {$i TASIOHost.lrs}
  {$i TASIOHostBasic.lrs}
{$ENDIF}

end.