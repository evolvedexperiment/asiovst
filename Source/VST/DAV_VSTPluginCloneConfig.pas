unit DAV_VSTPluginCloneConfig;
{******************************************************************************}
{                                                                              }
{ Class used to hold the configuration information entered on the Wizard form  }
{ and enable it to be accessed by the code generation process.                 }
{                                                                              }
{ Part of the VST Plugin Framework by Christian Budde and Tobybear.            }
{                                                                              }
{******************************************************************************}

interface

{$I ..\DAV_Compiler.inc}

uses
  SysUtils, DAV_VSTHost;

type
  TConfig = class(TObject)
  private
    FCloneGui     : Boolean;
    FSaveWhenDone : Boolean;
    FProjectName  : string;
    FProjectPath  : string;
    FVstHost      : TVstHost;
    function FormatToUnitName(Value: string): string;
    function GetClonedPlugin: TFileName;
    function GetContainsGui: Boolean;
    function GetEditorFormName: string;
    function GetEditorUnitName: string;
    function GetPluginFormName: string;
    function GetPluginUnitName: string;
    function GetVSTPlugin: TCustomVstPlugIn;
    procedure SetClonedPlugin(const Value: TFileName);
  public
    constructor Create;
    destructor Destroy; override;
    property ProjectPath: string read FProjectPath write FProjectPath;
    property ProjectName: string read FProjectName write FProjectName;
    property PluginFormName: string read GetPluginFormName;
    property PluginUnitName: string read GetPluginUnitName;
    property EditorFormName: string read GetEditorFormName;
    property EditorUnitName: string read GetEditorUnitName;
    property ClonedPlugin: TFileName read GetClonedPlugin write SetClonedPlugin;
    property CloneGui: Boolean read FCloneGui write FCloneGui;
    property ContainsGui: Boolean read GetContainsGui;
    property SaveWhenDone: Boolean read FSaveWhenDone write FSaveWhenDone;
    property VSTHost: TVstHost read FVstHost;
    property VSTPlugin: TCustomVstPlugIn read GetVSTPlugin;
  end;

implementation

uses
  Math, DAV_VstEffect, DAV_OpenToolsUtils;

constructor TConfig.Create;
begin
 FProjectPath  := GetCurrentDir;
 FProjectName  := GetUniqueProjectName;
 FCloneGui     := True;
 FSaveWhenDone := True;
 FVstHost      := TVstHost.Create(nil);
 FVSTHost.VstPlugIns.Add;
end;

destructor TConfig.Destroy;
begin
 FreeAndNil(FVstHost);
 inherited;
end;

function TConfig.GetClonedPlugin: TFileName;
begin
 result := FVSTHost[0].DLLFileName;
end;

function TConfig.GetContainsGui: Boolean;
begin
 result := CloneGui and (effFlagsHasEditor in VSTPlugin.EffectOptions);
end;

function TConfig.FormatToUnitName(Value: string): string;
var
  i : Integer;
begin
 result := Value;
 i := 1;
 while i < Length(result) do
  if ((result[i] in ['A'..'Z']) or
      (result[i] in ['a'..'z']) or
      (result[i] in ['0'..'9']))
   then inc(i)
   else Delete(result, i, 1);
end;

function TConfig.GetPluginFormName: string;
begin
 result := FormatToUnitName(FVSTHost[0].EffectName) + 'Module';
end;

function TConfig.GetPluginUnitName: string;
begin
 result := FormatToUnitName(FVSTHost[0].EffectName);

 if (result[1] in ['0'..'9'])
  then result := 'DAV' + result;
 result := result + 'Module';
end;

function TConfig.GetEditorFormName: string;
begin
 result := 'Fm' + FormatToUnitName(FVSTHost[0].EffectName) + 'Gui';
end;

function TConfig.GetEditorUnitName: string;
begin
 result := FormatToUnitName(FVSTHost[0].EffectName);

 if (result[1] in ['0'..'9'])
  then result := 'DAV' + result;
 result := result + 'Gui';
end;

function TConfig.GetVSTPlugin: TCustomVstPlugIn;
begin
 result := FVstHost[0];
end;

procedure TConfig.SetClonedPlugin(const Value: TFileName);
begin
 if ClonedPlugin <> Value then
  begin
   if FVSTHost[0].CheckValidPlugin(Value) then
    begin
     FVSTHost[0].LoadFromFile(Value);
     FVSTHost[0].Open;
    end else
   if FVSTHost[0].Active then
    begin
     FVSTHost[0].Close;
     FVSTHost[0].UnLoad;
    end;
  end;
end;

end.

