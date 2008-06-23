unit DVSTModule;

interface

{$I ASIOVST.INC}

uses
  {$IFDEF FPC} LCLIntf, {$ENDIF} Classes, DVSTModuleWithDsp;

type
  TVSTModule = class(TDspVSTModule)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Flags;
    property About;
    property Version;
    property EffectName;
    property ProductName;
    property VendorName;
    property VersionMajor;
    property VersionMinor;
    property VersionRelease;
    property PlugCategory;
    property Tempo;
    property TailSize;
    property CanDos;
    property SampleRate;
    property BlockSize;
    property numInputs;
    property numOutputs;
    property numParams;
    property numPrograms;
    property numCategories;
    property CurrentProgram;
    property CurrentProgramName;
    property ProcessingMode;
    property BlockModeSize;
    property BlockModeOverlap;
    property InitialDelay;
    property KeysRequired;
    property RealQualities;
    property OffQualities;
    property IORatio;
    property UniqueID;
    property ShellPlugins;
    property Programs;
    property ParameterProperties;
    property ProcessPrecisition;
    property OnGetChunkParameter;
    property OnOpen;
    property OnClose;
    property OnEditOpen;
    property OnEditClose;
    property OnEditIdle;
    property OnEditTop;
    property OnEditSleep;
    property OnEditorKeyUp;
    property OnEditorKeyDown;
    property OnEditorKnobMode;
    property OnParameterChange;
    property OnResume;
    property OnSuspend;
    property OnParameterSizeFailed;
    property OnBlockSizeChange;
    property OnSampleRateChange;
    property OnGetVU;
    property OnProcess;
    property OnProcessReplacing;
    property OnProcessDoubleReplacing;
    property OnSoftBypass;
    property OnProcessMidi;
    property OnInConnected;
    property OnOutConnected;
    property OnStartProcess;
    property OnStopProcess;
    property OnOfflineNotify;
    property OnOfflinePrepare;
    property OnOfflineRun;
    property OnProcessVarIO;
    property OnSetPanLaw;
    property OnBeginLoadBank;
    property OnBeginLoadProgram;
    property OnBeginSetProgram;
    property OnEndSetProgram;
    property OnInitialize;
    property OnBeforeProgramChange;
    property OnAfterProgramChange;
    property OnDispatcher;
    property OnVendorSpecific;
    property OnCanDo;
    property OnInputProperties;
    property OnOutputProperties;
    property OnCheckKey;
  end;



{$IFDEF FPC}
function InitResourceComponent(Instance: TComponent; RootAncestor: TClass):Boolean;
{$ENDIF}

implementation

uses
  Forms, RtlConsts;

{ TVSTModule }
    
{$IFNDEF FPC}

constructor TVSTModule.Create(AOwner: TComponent);
begin
 {$IFDEF UseDelphi}
 inherited Create(AOwner);
 if (ClassType <> TVSTModule) and not (csDesigning in ComponentState) then
  try
   if not InitInheritedComponent(Self, TDspVSTModule) then
     raise EResNotFound.CreateFmt('Resource %s not found', [ClassName]);
   try
    if Assigned(OnCreate) and OldCreateOrder then OnCreate(Self);
   except
    Forms.Application.HandleException(Self);
   end;
  except
  end;
 {$ELSE}
 inherited Create(AOwner);
 if Assigned(OnCreate) then OnCreate(Self);
 {$ENDIF}
end;
{$ELSE}
constructor TVSTModule.Create(AOwner: TComponent);
begin
 {$IFDEF UseDelphi}
 inherited Create(AOwner);
 if (ClassType <> TVSTModule) and not (csDesigning in ComponentState) then
  try
   if not InitInheritedComponent(Self, TDspVSTModule)
    then raise EStreamError.CreateFmt('Resource %s not found', [ClassName]);
   if OldCreateOrder then DoCreate;
  except
  end;
 {$ELSE}
 inherited Create(AOwner);
 {$ENDIF}
end;
{$ENDIF}

destructor TVSTModule.Destroy;
begin
 {$IFNDEF UseDelphi}
 if Assigned(fOnDestroy) then fOnDestroy(Self);
 {$ENDIF}
 inherited;
end;



{$IFDEF FPC}
function InitResourceComponent(Instance: TComponent; RootAncestor: TClass): Boolean;
begin
//  Result := InitLazResourceComponent(Instance,RootAncestor);
end;

initialization
//  Set8087CW(Default8087CW or $3F);
  RegisterInitComponentHandler(TVSTModule,@InitResourceComponent);
{$ENDIF}

end.
