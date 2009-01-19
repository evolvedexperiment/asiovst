unit DAV_StkSphere;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK TSphere class.

  This class implements a spherical ball with radius, mass, position, and
  velocity parameters.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkVector3D, Windows;

type
  TSphere = class
  private
    myPosition    : TVector3d;
    myVelocity    : TVector3d;
    workingVector : TVector3d;
    myRadius      : Double;
    myMass        : Double;
  public
    // Constructor taking an initial radius value.
    constructor Create(initRadius: Double);

    // Class destructor.
    destructor Destroy;

    // Set the 3D center position of the TSphere.
    procedure setPosition(anX, aY, aZ: Double);

    // Set the 3D velocity of the TSphere.
    procedure setVelocity(anX, aY, aZ: Double);

    // Set the radius of the TSphere.
    procedure setRadius(aRadius: Double);

    // Set the mass of the TSphere.
    procedure setMass(aMass: Double);

    // Get the current position of the TSphere as a 3D vector.
    function getPosition: TVector3d;

    // Get the relative position of the given point to the TSphere as a 3D vector.
    function getRelativePosition(aPosition: TVector3d): TVector3d;

    // Set the velcoity of the TSphere as a 3D vector.
    function getVelocity(aVelocity: TVector3d): Double;

    // Returns the distance from the TSphere boundary to the given position (< 0 if inside).
    function isInside(aPosition: TVector3d): Double;

    // Get the current TSphere radius.
    function getRadius: Double;

    // Get the current TSphere mass.
    function getMass: Double;

    // Increase the current TSphere velocity by the given 3D components.
    procedure addVelocity(anX, aY, aZ: Double);

    // Move the TSphere for the given time increment.
    procedure tick(timeIncrement: Double);
  end;

implementation

constructor TSphere.Create;
begin
  myRadius := initRadius;
  myMass := 1.0;
  myPosition := TVector3d.Create(0, 0, 0);
  myVelocity := TVector3d.Create(0, 0, 0);
end;

destructor TSphere.Destroy;
begin
  myPosition.Free;
  myVelocity.Free;
end;

procedure TSphere.setPosition;
begin
  myPosition.setXYZ(anX, aY, aZ);
end;

procedure TSphere.setVelocity;
begin
  myVelocity.setXYZ(anX, aY, aZ);
end;

procedure TSphere.setRadius;
begin
  myRadius := aRadius;
end;

procedure TSphere.setMass;
begin
  myMass := aMass;
end;

function TSphere.getPosition: TVector3d;
begin
  Result := myPosition;
end;

function TSphere.getRelativePosition(aPosition: TVector3d): TVector3d;
begin
  workingVector.setXYZ(aPosition.getX - myPosition.getX,
    aPosition.getY - myPosition.getY,
    aPosition.getZ - myPosition.getZ);
  Result := workingVector;
end;

function TSphere.getVelocity(aVelocity: TVector3d): Double;
begin
  aVelocity.setXYZ(myVelocity.getX, myVelocity.getY, myVelocity.getZ);
  Result := myVelocity.getLength;
end;

function TSphere.isInside(aPosition: TVector3d): Double;
var
  distance: Double;
  tvector: TVector3d;
begin
 // Return directed distance from aPosition to spherical boundary ( <
 // 0 if inside).
  tVector := getRelativePosition(aPosition);
  distance := tVector.getLength;
  Result := distance - myRadius;
end;

function TSphere.getRadius: Double;
begin
  Result := myRadius;
end;

function TSphere.getMass: Double;
begin
  Result := myMass;
end;

procedure TSphere.addVelocity(anX, aY, aZ: Double);
begin
  myVelocity.setX(myVelocity.getX + anX);
  myVelocity.setY(myVelocity.getY + aY);
  myVelocity.setZ(myVelocity.getZ + aZ);
end;

procedure TSphere.tick(timeIncrement: Double);
begin
  myPosition.setX(myPosition.getX + (timeIncrement * myVelocity.getX));
  myPosition.setY(myPosition.getY + (timeIncrement * myVelocity.getY));
  myPosition.setZ(myPosition.getZ + (timeIncrement * myVelocity.getZ));
end;

end.
