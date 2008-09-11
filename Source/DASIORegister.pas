unit DASIORegister;

interface

procedure Register;

implementation

{$IFNDEF FPC}{$R DASIOHost.res}{$ENDIF}

uses
  {$IFDEF FPC} LResources, {$ENDIF} Classes, DASIOHOST, DASIOGenerator;

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
