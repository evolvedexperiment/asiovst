unit PulsingGUI;

interface

uses 
  {$IFDEF FPC}LCLIntf, LResources, {$ELSE} Windows, {$ENDIF} SysUtils, Classes, Forms, DAV_Types, DAV_VSTModule;

type
  TFmPulsing = class(TForm)
  end;

implementation

{$R *.DFM}

end.