unit OversamplePlugin;

interface

uses
  Windows, Classes, SysUtils, Graphics, ExtCtrls, Menus,
  ComServ, ComObj, ActiveX, ShlObj, ShellAPI, Registry;

type
  TContextMenuFactory = class(TComObjectFactory)
  public
    procedure UpdateRegistry(Register: Boolean); override;
  end;

  TContextMenu = class(TComObject, IShellExtInit, IContextMenu)
  private
    function BuildSubMenu(Menu: HMENU; IndexMenu: Integer; var IDCmdFirst: Integer): HMENU;
    procedure SavePlugin;
  protected
    szFile: array[0..MAX_PATH] of Char;

    // Required to disambiguate TComObject.Initialize otherwise a compiler warning will result.
    function IShellExtInit.Initialize = IShellExtInit_Initialize;
  public
    { IShellExtInit members }
    function IShellExtInit_Initialize(pidlFolder: PItemIDList; lpdobj: IDataObject; hKeyProgID: HKEY): HResult; stdcall;

    { IContextMenu }
    function QueryContextMenu(Menu: HMENU; indexMenu, idCmdFirst, idCmdLast, uFlags: UINT): HResult; stdcall;
    function InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult; stdcall;
    function GetCommandString(idCmd, uType: UINT; pwReserved: PUINT; pszName: LPSTR; cchMax: UINT): HResult; stdcall;
  end;

const
  Class_ContextMenu: TGUID = '{E6EE9FD1-5C55-4894-8017-3D97C9BB9110}';

implementation

uses
  DAV_DLLResources, Dialogs;

{ TContextMenuFactory }

procedure TContextMenuFactory.UpdateRegistry(Register: Boolean);
begin
 inherited UpdateRegistry(Register);

 // Register our global context menu handler
 if Register then
  begin
   CreateRegKey('*\ShellEx\ContextMenuHandlers\OversampleVSTPlugin', '', GUIDToString(Class_ContextMenu));
   CreateRegKey('CLSID\' + GUIDToString(ClassID) + '\' + ComServer.ServerKey, 'ThreadingModel', 'Apartment');
  end else DeleteRegKey('*\ShellEx\ContextMenuHandlers\OversampleVSTPlugin');
end;

{ TContextMenu }

{ Build a context menu using the existing Menu handle. If Menu is nil,
  we create a new menu handle and return it in the function's return
  value. Note that this function does not handle nested (recursive)
  menus. This exercise is left to the reader. }
function TContextMenu.BuildSubMenu(Menu: HMENU; IndexMenu: Integer;
  var IDCmdFirst: Integer): HMENU;
var
  menuItemInfo: TMenuItemInfo;
begin
  if Menu = 0
   then Result := CreateMenu
   else Result := Menu;

  // Build the menu items here
  with menuitemInfo do
   begin
    // common
    cbSize        := SizeOf(TMenuItemInfo);
    fMask         := MIIM_CHECKMARKS or MIIM_DATA or MIIM_ID or MIIM_STATE or MIIM_SUBMENU or MIIM_TYPE or MIIM_CHECKMARKS;
    fType         := MFT_STRING;
    fState        := MFS_ENABLED;
    hSubMenu      := 0;
    hbmpChecked   := 0;
    hbmpUnchecked := 0;

    // separator
    wID   := IDCmdFirst;
    fType := MFT_SEPARATOR;
    InsertMenuItem(Result, IndexMenu, True, menuItemInfo);
    Inc(IDCmdFirst);

    // item
    wID   := IDCmdFirst;
    fType := MFT_String;
    dwTypeData := PChar('Oversample VST Plugin');
    InsertMenuItem(Result, IndexMenu + 1, True, menuItemInfo);
    Inc(IDCmdFirst);
   end;
end;

{ IShellExtInit }

procedure TContextMenu.SavePlugin;
var
  RS  : TResourceStream;
  RM  : TPEResourceModule;
  RD  : TResourceDetails;
begin
 RM := TPEResourceModule.Create;
 with RM do
  try

   // load template
   RS := TResourceStream.Create(HInstance, 'OversampleTemplate', 'DLL');
   try
    LoadFromStream(RS);
   finally
    FreeAndNil(RS);
   end;

   // store VST Plugins
   with TMemoryStream.Create do
    try
     LoadFromFile(szFile);
     RD := TResourceDetails.CreateResourceDetails(RM, 0, 'VST1', 'DLL', Size, Memory);
     AddResource(RD);
    finally
     Free;
    end;

   SortResources;
   SaveToFile(ExtractFilePath(szFile) + 'Oversampled ' + ExtractFileName(szFile));
   ShowMessage('Plugin ' + '"Oversampled ' + ExtractFileName(szFile) + '" successfully created!');
  finally
   FreeAndNil(RM);
  end;
end;

function TContextMenu.IShellExtInit_Initialize(pidlFolder: PItemIDList;
  lpdobj: IDataObject; hKeyProgID: HKEY): HResult;
var
  medium : TStgMedium;
  fe     : TFormatEtc;

begin
  with fe do
   begin
    cfFormat := CF_HDROP;
    ptd      := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex   := -1;
    tymed    := TYMED_HGLOBAL;
   end;

  // Fail the call if lpdobj is Nil.
  if lpdobj = nil then
   begin
    Result := E_FAIL;
    Exit;
   end;

  // Render the data referenced by the IDataObject pointer to an HGLOBAL
  // storage medium in CF_HDROP format.
  Result := lpdobj.GetData(fe, medium);
  try
   if Failed(Result) then
     Exit;

   // If only one file is selected, retrieve the file name and store it in
   // szFile. Otherwise fail the call.
   if DragQueryFile(medium.hGlobal, $FFFFFFFF, nil, 0) = 1 then
    try
     DragQueryFile(medium.hGlobal, 0, szFile, SizeOf(szFile));
     Result := NOERROR;
    except
     Result := E_FAIL;
    end
   else Result := E_FAIL;
  finally
   ReleaseStgMedium(medium);
  end;
end;

{ IContextMenu }

function TContextMenu.QueryContextMenu(Menu: HMENU;
  indexMenu, idCmdFirst, idCmdLast, uFlags: UINT): HResult;
var
  extension     : string;
  idLastCommand : Integer;
begin
  Result := E_FAIL;
  idLastCommand := idCmdFirst;

  // extract the filename extension from the file dropped
  Extension := UpperCase(ExtractFileExt(szFile));

  if Extension = '.DLL' then
   begin
    BuildSubMenu(Menu, indexMenu, idLastCommand);
    // Return value is number of items added to context menu
    Result := idLastCommand - idCmdFirst;
   end;
end;

function TContextMenu.InvokeCommand(
  var lpici: TCMInvokeCommandInfo): HResult;
var
  idCmd: UINT;
begin
  if HIWORD(Integer(lpici.lpVerb)) <> 0
   then Result := E_FAIL
   else
    begin
     idCmd := LOWORD(lpici.lpVerb);
     Result := S_OK;

     case idCmd of
       1 : SavePlugin;
     else
       Result := E_FAIL;
      end;
    end;
end;

function TContextMenu.GetCommandString(idCmd, uType: UINT;
  pwReserved: PUINT; pszName: LPSTR; cchMax: UINT): HResult;

begin
  Result := S_OK;
end;

initialization
  { Note that we create an instance of TContextMenuFactory here rather
    than TComObjectFactory. This is necessary so that we can add some
    custom registry entries by overriding the UpdateRegistry virtual
    function. }
  TContextMenuFactory.Create(ComServer, TContextMenu, Class_ContextMenu,
    'ContextMenu', 'Oversample VST Plugin', ciMultiInstance);

end.
