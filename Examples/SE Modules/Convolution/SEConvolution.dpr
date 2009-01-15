library SEConvolution;

uses
  SysUtils,
  Classes,
  DAV_SECommon,
  {$IFDEF Use_CUDA}
  DAV_CudaRuntime,
  {$ENDIF}
  DAV_SEModule,
  SEConvolutionModule in 'SEConvolutionModule.pas';

{$E sem}
{$R *.res}

function getModuleProperties(Index: Integer; Properties: PSEModuleProperties): Boolean; cdecl; export;
begin
 result := True;
 case Index of
  0: TSEConvolutionModule.GetModuleProperties(Properties);
  {$IFDEF Use_CUDA}
  1: if CudaRuntimeLoaded then
      try
       TSEConvolutionModuleCUDA.GetModuleProperties(Properties);
      except
       result := False;
      end
     else result := False;
  {$ENDIF}
//  2: TSEConvolutionModule.GetModuleProperties(Properties);
  else result := False;
 end;;
end;

function makeModule(Index: Integer; ProcessType: Integer; SEAudioMaster: TSE2AudioMasterCallback; Reserved: Pointer): Pointer; cdecl; export;
var
  SEModuleBase: TSEModuleBase;
begin
 SEModuleBase := nil;
 if (ProcessType = 1) then
  case Index of
   0: SEModuleBase := TSEConvolutionModule.Create(SEAudioMaster, Reserved);
   {$IFDEF Use_CUDA}
   1: if CudaRuntimeLoaded then
       try
        SEModuleBase := TSEConvolutionModuleCUDA.Create(SEAudioMaster, Reserved);
       except
        SEModuleBase := nil;
       end
      else SEModuleBase := nil;
   {$ENDIF}
  end;
 if assigned(SEModuleBase)
  then result := SEModuleBase.Effect
  else result := nil;
end;

exports makeModule name 'makeModule';
exports getModuleProperties name 'getModuleProperties';

end.
