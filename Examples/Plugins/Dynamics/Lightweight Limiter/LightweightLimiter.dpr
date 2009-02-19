{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library LightweightLimiter;

{$R 'Limiter.res' 'Limiter.rc'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  FastMove,
  DAV_VSTBasicModule,
  LightweightLimiterDM in 'LightweightLimiterDM.pas' {LightweightLimiterDataModule: TVSTModule},
  LightweightLimiterGUI in 'LightweightLimiterGUI.pas' {FmLightweightLimiter};

begin
 BasicVSTModuleClass := TLightweightLimiterDataModule;
end.
