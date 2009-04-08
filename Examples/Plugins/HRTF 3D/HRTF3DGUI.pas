unit HRTF3DGUI;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  GLScene, GLObjects, GLMisc, Controls, GLFile3DS, GLWin32Viewer,
  GLVectorFileObjects, Dialogs, StdCtrls;

type
  TVSTGUI = class(TForm)
    GLScene: TGLScene;
    GLSceneViewer: TGLSceneViewer;
    GLCamera: TGLCamera;
    GLDummyCube: TGLDummyCube;
    GLHead: TGLFreeForm;
    GLLight: TGLLightSource;
    procedure FormCreate(Sender: TObject);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure GLSceneViewerMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure GLSceneViewerMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    fOldMousePoint : TPoint;
    procedure Zoom(Value: Single);
  end;

implementation

{$R *.DFM}

uses
  Math, VectorGeometry, MeshUtils, Jpeg, TGA, GLFileObj,
  GLCrossPlatform, VectorLists, HRTF3DModule;

procedure TVSTGUI.FormCreate(Sender: TObject);
var
  rs : TResourceStream;
  i, j : Integer;
  tris, norms, tex, buf, morphTris, morphNorms : TAffineVectorList;
  indices, texIndices : TIntegerlist;
  firstRemap, subdivideRemap, bufRemap : TIntegerList;
  t : Int64;
begin
 rs:=TResourceStream.Create(hInstance, 'Head', RT_RCDATA);
 with rs do
  try
   GLHead.LoadFromStream('Head.3DS',rs);
   for i:=0 to GLHead.MeshObjects.Count-1 do
    begin
     tex:=TAffineVectorList.Create;
     with GLHead.MeshObjects[i] do tris:=ExtractTriangles(tex);
     indices:=BuildVectorCountOptimizedIndices(tris);
     firstRemap:=TIntegerList(indices.CreateClone);
     RemapAndCleanupReferences(tris, indices);
     norms:=BuildNormals(tris, indices);

     // subdivide geometry
     SubdivideTriangles(0.6, tris, indices, norms);
     texIndices:=BuildVectorCountOptimizedIndices(tex);
     RemapAndCleanupReferences(tex, texIndices);

     // subdivide texture space
     SubdivideTriangles(0, tex, texIndices);

     // Re-expand everything
     buf:=TAffineVectorList.Create;
     try
      ConvertIndexedListToList(tris, indices, buf);
      tris.Assign(buf);
      buf.Count:=0;
      ConvertIndexedListToList(norms, indices, buf);
      norms.Assign(buf);
      buf.Count:=0;
      ConvertIndexedListToList(tex, texIndices, buf);
      tex.Assign(buf);
     finally
      buf.Free;
     end;

     // Pack & Optimize the expanded stuff
     indices.Free;
     indices:=BuildVectorCountOptimizedIndices(tris, norms, tex);
     subdivideRemap:=TIntegerList(indices.CreateClone);
     RemapReferences(norms, indices);
     RemapReferences(tex, indices);
     RemapAndCleanupReferences(tris, indices);

     IncreaseCoherency(indices, 13);

      with GLHead.MeshObjects[i] do
       begin
        bufRemap:=TIntegerList.Create;
         morphTris:=ExtractTriangles;
         bufRemap.Assign(firstRemap);
         RemapAndCleanupReferences(morphTris, bufRemap);

         morphNorms:=MeshUtils.BuildNormals(morphTris, bufRemap);
         SubdivideTriangles(0.7, morphTris, bufRemap, morphNorms);
         buf:=TAffineVectorList.Create;
         try
          ConvertIndexedListToList(morphTris, bufRemap, buf);
          morphTris.Assign(buf);
          ConvertIndexedListToList(morphNorms, bufRemap, buf);
          morphNorms.Assign(buf);
         finally
          buf.Free;
         end;
         RemapReferences(morphTris, subdivideRemap);
         RemapReferences(morphNorms, subdivideRemap);
         morphTris.Free;
         morphNorms.Free;
         bufRemap.Free;
         Vertices:=tris;
         Normals:=norms;
         TexCoords:=tex;
         FaceGroups.Clear;
         with TFGVertexIndexList.CreateOwned(FaceGroups) do
          begin
           VertexIndices:=indices;
           Mode:=fgmmTriangles;
         end;
      end;
      texIndices.Free;
      subdivideRemap.Free;
      firstRemap.Free;
      tex.Free;
      indices.Free;
      norms.Free;
      tris.Free;
   end;
   GLHead.StructureChanged;
  finally
   Free;
  end;
end;

procedure TVSTGUI.Zoom(Value:Single);
var vect : TVector;
begin
 if GLSceneViewer.Camera=GLCamera then
  with GLCamera do
   if Assigned(TargetObject) then
    begin
     vect:=VectorSubtract(AbsolutePosition, TargetObject.AbsolutePosition);
     if ((VectorLength(vect)>1.2) or (Value>1)) and ((VectorLength(vect)<10) or (Value<1)) then
      begin
       ScaleVector(vect, Value-1);
       AddVector(vect, AbsolutePosition);
       if Assigned(Parent)
        then vect:=Parent.AbsoluteToLocal(vect);
       Position.AsVector:=vect;
      end;
    end
end;

procedure TVSTGUI.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
const Scale = 1/120;
begin
 Zoom(Power(0.9, WheelDelta*Scale));
 Handled := true
end;

procedure TVSTGUI.GLSceneViewerMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
const Scale = 1/40;
var
   originalT2C, normalT2C, normalCameraRight, newPos : TVector;
   turnNow, pitchNow, dist: Single;
begin
 if ssLeft in Shift then
  begin
   with GLSceneViewer.Camera do
    begin
     originalT2C:=VectorSubtract(AbsolutePosition,GLDummyCube.AbsolutePosition);
     SetVector(normalT2C, originalT2C);
     dist:=VectorLength(normalT2C);
     NormalizeVector(normalT2C);
     normalCameraRight:=VectorCrossProduct(AbsoluteUp, normalT2C);
     if VectorLength(normalCameraRight)<0.001
      then SetVector(normalCameraRight, XVector) // arbitrary vector
      else NormalizeVector(normalCameraRight);
     pitchNow:=Math.ArcCos(VectorDotProduct(AbsoluteUp, normalT2C));
     pitchNow:=ClampValue(pitchNow+DegToRad(fOldMousePoint.Y-Y), 0.002, PI-0.77);
     SetVector(normalT2C, AbsoluteUp);
     RotateVector(normalT2C, normalCameraRight, -pitchNow);
     RotateVector(normalT2C, AbsoluteUp, -DegToRad(fOldMousePoint.X-X));
     ScaleVector(normalT2C, dist);
     newPos:=VectorAdd(AbsolutePosition, VectorSubtract(normalT2C, originalT2C));
     if Assigned(Parent) then newPos:=Parent.AbsoluteToLocal(newPos);
     Position.AsVector:=newPos;
     GLLight.Position.SetVector(newPos);
     TVSTHRTF3DModule(Self.Owner).Parameter[0] := RadToDeg(ArcTan2(Position.Y,Position.X));
     TVSTHRTF3DModule(Self.Owner).Parameter[1] := 90-RadToDeg(pitchNow);
    end;
   fOldMousePoint.X:=X;
   fOldMousePoint.Y:=Y;
  end else
 if ssRight in Shift then
  begin
   Zoom(Power(0.995, (fOldMousePoint.Y-Y)));
   fOldMousePoint.X:=X;
   fOldMousePoint.Y:=Y;
  end;
end;

procedure TVSTGUI.GLSceneViewerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 fOldMousePoint.X:=X;
 fOldMousePoint.Y:=Y;
end;

end.