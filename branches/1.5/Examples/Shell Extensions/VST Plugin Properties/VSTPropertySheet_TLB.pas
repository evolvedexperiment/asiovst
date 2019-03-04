unit VSTPropertySheet_TLB;

{$TYPEDADDRESS OFF}
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}

interface

uses
  Windows, ActiveX, Classes, StdVCL, Variants;
  
const
  VSTPropertySheetMajorVersion = 1;
  VSTPropertySheetMinorVersion = 0;

  LIBID_VSTPropertySheet: TGUID = '{7AD849A6-7662-4436-B0F4-5B84E8825D33}';

  IID_IVSTPluginPropertySheet: TGUID = '{DC4552D5-A08B-47B5-8ECA-20D21DF4F836}';
  CLASS_VSTPluginPropertySheet: TGUID = '{439DF740-4226-49DB-9BD3-CABF3C2CF5DA}';
type
  IVSTPluginPropertySheet = interface;
  VSTPluginPropertySheet = IVSTPluginPropertySheet;

// *********************************************************************//
// Interface: IVSTPluginPropertySheet
// Flags:     (256) OleAutomation
// GUID:      {DC4552D5-A08B-47B5-8ECA-20D21DF4F836}
// *********************************************************************//
  IVSTPluginPropertySheet = interface(IUnknown)
    ['{DC4552D5-A08B-47B5-8ECA-20D21DF4F836}']
  end;

  CoVSTPluginPropertySheet = class
    class function Create: IVSTPluginPropertySheet;
    class function CreateRemote(const MachineName: string): IVSTPluginPropertySheet;
  end;

implementation

uses
  ComObj;

class function CoVSTPluginPropertySheet.Create: IVSTPluginPropertySheet;
begin
  Result := CreateComObject(CLASS_VSTPluginPropertySheet) as IVSTPluginPropertySheet;
end;

class function CoVSTPluginPropertySheet.CreateRemote(const MachineName: string): IVSTPluginPropertySheet;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_VSTPluginPropertySheet) as IVSTPluginPropertySheet;
end;

end.
