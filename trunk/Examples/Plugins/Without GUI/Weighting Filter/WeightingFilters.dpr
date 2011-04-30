{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library WeightingFilters;

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  DAV_VSTEffect,
  DAV_VSTBasicModule,
  WeightingFiltersDSP in 'WeightingFiltersDSP.pas' {WeightingFiltersDataModule: TVSTModule};

function VstPluginMain(AudioMasterCallback: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
begin
 Result := VstModuleMain(AudioMasterCallback, TWeightingFiltersDataModule);
end;

exports 
  VstPluginMain name 'main',
  VstPluginMain name 'VSTPluginMain';

end.
