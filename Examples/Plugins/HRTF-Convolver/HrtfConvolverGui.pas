unit HrtfConvolverGui;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  ComCtrls, Controls, Spin, StdCtrls, GLScene, GLObjects, GLVectorFileObjects,
  GLMisc, GLWin32Viewer, DAV_GuiAudioDataDisplay, DAV_AudioData, Dialogs;

type
  TFmHrtfConvolver = class(TForm)
    PCConvolutionSelect: TPageControl;
    TSHrtf: TTabSheet;
    TSReverb: TTabSheet;
    LbHrtfSet: TLabel;
    EdHrtfSet: TEdit;
    BtLoadHrtfFile: TButton;
    GBPosition: TGroupBox;
    LbRadius: TLabel;
    SERadius: TSpinEdit;
    SEElevation: TSpinEdit;
    LbElevation: TLabel;
    SEAzimuth: TSpinEdit;
    LbAzimuth: TLabel;
    GbImpulseResponses: TGroupBox;
    Gb3D: TGroupBox;
    GLSceneViewer: TGLSceneViewer;
    GLScene: TGLScene;
    GLDummyCube: TGLDummyCube;
    GLHead: TGLFreeForm;
    GLLight: TGLLightSource;
    GLHRTFs: TGLPoints;
    GLCamera: TGLCamera;
    AudioDataDisplay: TGuiAudioDataDisplay;
    OpenDialog: TOpenDialog;
    procedure BtLoadHrtfFileClick(Sender: TObject);
    procedure GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GLSceneViewerMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure SEAzimuthChange(Sender: TObject);
  private
    FOldMousePoint : TPoint;
    procedure Zoom(Value: Single);
  end;

implementation

{$R *.DFM}

uses
  Math, VectorGeometry, MeshUtils, Jpeg, TGA, GLFile3DS, GLFileObj,
  GLCrossPlatform, VectorLists, DAV_DspHrtf, HrtfConvolverDM;

procedure TFmHrtfConvolver.FormCreate(Sender: TObject);
var
  rs             : TResourceStream;
  i              : Integer;
  tris, norms    : TAffineVectorList;
  tex, buf       : TAffineVectorList;
  morphTris      : TAffineVectorList;
  morphNorms     : TAffineVectorList;
  indices        : TIntegerList;
  texIndices     : TIntegerList;
  firstRemap     : TIntegerList;
  subdivideRemap : TIntegerList;
  bufRemap       : TIntegerList;
begin
 rs := TResourceStream.Create(hInstance, 'Head', '3DS');
 with rs do
  try
   GLHead.LoadFromStream('Head.3DS',rs);
   for i := 0 to GLHead.MeshObjects.Count-1 do
    begin
     tex := TAffineVectorList.Create;
     try
      with GLHead.MeshObjects[i]
       do tris := ExtractTriangles(tex);
      try
       indices := BuildVectorCountOptimizedIndices(tris);
       try
        firstRemap := TIntegerList(indices.CreateClone);
        RemapAndCleanupReferences(tris, indices);
        norms := BuildNormals(tris, indices);

        // subdivide geometry
        SubdivideTriangles(0.6, tris, indices, norms);
        texIndices := BuildVectorCountOptimizedIndices(tex);
        RemapAndCleanupReferences(tex, texIndices);

        // subdivide texture space
        SubdivideTriangles(0, tex, texIndices);

        // Re-expand everything
        buf := TAffineVectorList.Create;
        try
         ConvertIndexedListToList(tris, indices, buf);
         tris.Assign(buf);
         buf.Count := 0;
         ConvertIndexedListToList(norms, indices, buf);
         norms.Assign(buf);
         buf.Count := 0;
         ConvertIndexedListToList(tex, texIndices, buf);
         tex.Assign(buf);
        finally
         FreeAndNil(buf);
        end;

        // Pack & Optimize the expanded stuff
        FreeAndNil(indices);
        indices := BuildVectorCountOptimizedIndices(tris, norms, tex);
        subdivideRemap := TIntegerList(indices.CreateClone);
        RemapReferences(norms, indices);
        RemapReferences(tex, indices);
        RemapAndCleanupReferences(tris, indices);

        IncreaseCoherency(indices, 13);

        with GLHead.MeshObjects[i] do
         begin
          bufRemap := TIntegerList.Create;
          try
           morphTris := ExtractTriangles;
           try
            bufRemap.Assign(firstRemap);
            RemapAndCleanupReferences(morphTris, bufRemap);

            morphNorms := MeshUtils.BuildNormals(morphTris, bufRemap);
            try
             SubdivideTriangles(0.7, morphTris, bufRemap, morphNorms);
             buf := TAffineVectorList.Create;
             try
              ConvertIndexedListToList(morphTris, bufRemap, buf);
              morphTris.Assign(buf);
              ConvertIndexedListToList(morphNorms, bufRemap, buf);
              morphNorms.Assign(buf);
             finally
              FreeAndNil(buf);
             end;
             RemapReferences(morphTris, subdivideRemap);
             RemapReferences(morphNorms, subdivideRemap);
            finally
             FreeAndNil(morphNorms);
            end;
           finally
            FreeAndNil(morphTris);
           end;
          finally
           FreeAndNil(bufRemap);
          end;

          Vertices := tris;
          Normals := norms;
          TexCoords := tex;
          FaceGroups.Clear;
          with TFGVertexIndexList.CreateOwned(FaceGroups) do
           begin
            VertexIndices := indices;
            Mode := fgmmTriangles;
           end;
         end;
        FreeAndNil(texIndices);
        FreeAndNil(subdivideRemap);
        FreeAndNil(firstRemap);
        FreeAndNil(norms);
       finally
        FreeAndNil(indices);
       end;
      finally
       FreeAndNil(tris);
      end;
     finally
      FreeAndNil(tex);
     end;
    end;
   GLHead.StructureChanged;
  finally
   Free;
  end;

 with THrtfConvolverDataModule(Owner) do
  begin
   AudioDataDisplay.AudioDataCollection := AudioDataCollection;
  end;
end;

procedure TFmHrtfConvolver.BtLoadHrtfFileClick(Sender: TObject);
begin
 with OpenDialog do
  if Execute then
   begin

   end;
end;

procedure TFmHrtfConvolver.GLSceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 FOldMousePoint.X := X;
 FOldMousePoint.Y := Y;
end;

procedure TFmHrtfConvolver.GLSceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
const
  Scale = 1/40;
var
   originalT2C, normalT2C, normalCameraRight, newPos : TVector;
   pitchNow, dist: Single;
begin
 if ssLeft in Shift then
  begin
   with GLSceneViewer.Camera do
    begin
     originalT2C := VectorSubtract(AbsolutePosition, GLDummyCube.AbsolutePosition);
     SetVector(normalT2C, originalT2C);
     dist := VectorLength(normalT2C);
     NormalizeVector(normalT2C);
     normalCameraRight := VectorCrossProduct(AbsoluteUp, normalT2C);
     if VectorLength(normalCameraRight) < 0.001
      then SetVector(normalCameraRight, XVector) // arbitrary vector
      else NormalizeVector(normalCameraRight);
     pitchNow := Math.ArcCos(VectorDotProduct(AbsoluteUp, normalT2C));
     if not (ssAlt in Shift)
      then pitchNow := ClampValue(pitchNow + DegToRad(FOldMousePoint.Y - Y), 0.002, PI - 0.77);
     SetVector(normalT2C, AbsoluteUp);
     RotateVector(normalT2C, normalCameraRight, -pitchNow);
     if not (ssShift in Shift)
      then RotateVector(normalT2C, AbsoluteUp, -DegToRad(FOldMousePoint.X - X));
     ScaleVector(normalT2C, dist);
     newPos := VectorAdd(AbsolutePosition, VectorSubtract(normalT2C, originalT2C));
     if Assigned(Parent) then newPos := Parent.AbsoluteToLocal(newPos);
     Position.AsVector := newPos;

     case GLLight.Position.Style of
      csPoint: GLLight.Position.SetPoint(newPos);
      csVector: GLLight.Position.SetVector(newPos);
     end;
    end;
   FOldMousePoint.X := X;
   FOldMousePoint.Y := Y;
  end else
 if ssRight in Shift then
  begin
   Zoom(Power(0.995, (FOldMousePoint.Y - Y)));
   FOldMousePoint.X := X;
   FOldMousePoint.Y := Y;
  end;
end;

procedure TFmHrtfConvolver.GLSceneViewerMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
const
  Scale = 1/120;
begin
 Zoom(Power(0.9, WheelDelta * Scale));
 Handled := true
end;

procedure TFmHrtfConvolver.SEAzimuthChange(Sender: TObject);
begin
 if SEAzimuth.Value > 180  then SEAzimuth.Value := SEAzimuth.Value - 360 else
 if SEAzimuth.Value < -180 then SEAzimuth.Value := SEAzimuth.Value + 360;

 with THrtfConvolverDataModule(Owner) do
  begin

(*
   FHRTFFile.InterpolateHrir(SEAzimuth.Value * CDegToRad,
     SEPolar.Value * CDegToRad, ADHRIR.SampleFrames,
     ADHRIR[0].ChannelDataPointer, ADHRIR[1].ChannelDataPointer);
   AudioDataDisplay.Invalidate;
*)
  end;
end;

procedure TFmHrtfConvolver.Zoom(Value: Single);
var
  vect : TVector;
begin
 if GLSceneViewer.Camera = GLCamera then
  with GLCamera do
   if Assigned(TargetObject) then
    begin
     vect := VectorSubtract(AbsolutePosition, TargetObject.AbsolutePosition);
     if ((VectorLength(vect) > 1.2) or (Value > 1)) and
        ((VectorLength(vect) < 10)  or (Value < 1)) then
      begin
       ScaleVector(vect, Value - 1);
       AddVector(vect, AbsolutePosition);
       if Assigned(Parent)
        then vect := Parent.AbsoluteToLocal(vect);
       Position.AsVector := vect;
      end;
    end
end;

end.
