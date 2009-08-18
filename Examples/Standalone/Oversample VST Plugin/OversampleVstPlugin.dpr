program OversampleVstPlugin;

{$R 'OversampleTemplate.res' 'OversampleTemplate.rc'}

uses
  SysUtils, Classes, Dialogs, DAV_DLLResources;

{$R *.res}

var
  Template : TPEResourceModule;
  RS       : TResourceStream;
  RD       : TResourceDetails;
  DLLName  : string;

begin
 with TOpenDialog.Create(nil) do
  try
   DefaultExt := '.DLL';
   Filter := 'VST Plugin (*.dll)|*.dll';
   Options := [ofHideReadOnly, ofAllowMultiSelect, ofFileMustExist, ofEnableSizing];
   if Execute then
    begin
     DLLName := FileName;
     with TSaveDialog.Create(nil) do
      try
       DefaultExt := '.DLL';
       Filter := 'VST Plugin (*.dll)|*.dll';
       if Execute then
        begin
         Template := TPEResourceModule.Create;

         RS := TResourceStream.Create(HInstance, 'Template', 'DLL');
         try
          Template.LoadFromStream(RS);
         finally
          FreeAndNil(RS);
         end;

         try
          with TMemoryStream.Create do
           try
            LoadFromFile(DLLName);
            RD := TResourceDetails.CreateResourceDetails(Template, 0, 'DLL', 'DLL', Size, Memory);
            Template.AddResource(RD);
           finally
            Free;
           end;

          Template.SortResources;
          Template.SaveToFile(FileName);
         finally
          FreeAndNil(Template);
         end;
        end;
     finally
      Free;
     end;
    end;
  finally
   Free;
  end;
end.
