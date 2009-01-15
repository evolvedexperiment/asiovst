unit DAV_StkSphere;

{
/***************************************************/
/*! \class TSphere
    \brief STK TSphere class.

    This class implements a spherical ball with
    radius, mass, position, and velocity parameters.

    by Perry R. Cook, 1995 - 2002.
*/
/***************************************************/
}
interface

uses Vector3D, Windows;

type
  TSphere = class
  public
  //! Constructor taking an initial radius value.
    constructor Create(initRadius: double);

  //! Class destructor.
    destructor Destroy;

  //! Set the 3D center position of the TSphere.
    procedure setPosition(anX, aY, aZ: double);

  //! Set the 3D velocity of the TSphere.
    procedure setVelocity(anX, aY, aZ: double);

  //! Set the radius of the TSphere.
    procedure setRadius(aRadius: double);

  //! Set the mass of the TSphere.
    procedure setMass(aMass: double);

  //! Get the current position of the TSphere as a 3D vector.
    function getPosition: tvector3d;

  //! Get the relative position of the given point to the TSphere as a 3D vector.
    function getRelativePosition(aPosition: tvector3d): tvector3d;

  //! Set the velcoity of the TSphere as a 3D vector.
    function getVelocity(aVelocity: tvector3d): double;

  //! Returns the distance from the TSphere boundary to the given position (< 0 if inside).
    function isInside(aPosition: tvector3d): double;

  //! Get the current TSphere radius.
    function getRadius: double;

  //! Get the current TSphere mass.
    function getMass: double;

  //! Increase the current TSphere velocity by the given 3D components.
    procedure addVelocity(anX, aY, aZ: double);

  //! Move the TSphere for the given time increment.
    procedure tick(timeIncrement: double);

  private
    myPosition, myVelocity: tvector3d;
    workingVector: tvector3d;
    myRadius, myMass: double;
  end;

implementation

constructor TSphere.Create;
begin
  myRadius := initRadius;
  myMass := 1.0;
  myPosition := TVector3D.Create(0, 0, 0);
  myVelocity := TVector3D.Create(0, 0, 0);
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

function TSphere.getPosition: tvector3d;
begin
  Result := myPosition;
end;

function TSphere.getRelativePosition(aPosition: tvector3d): tvector3d;
begin
  workingVector.setXYZ(aPosition.getX - myPosition.getX,
    aPosition.getY - myPosition.getY,
    aPosition.getZ - myPosition.getZ);
  Result := workingVector;
end;

function TSphere.getVelocity(aVelocity: tvector3d): double;
begin
  aVelocity.setXYZ(myVelocity.getX, myVelocity.getY, myVelocity.getZ);
  Result := myVelocity.getLength;
end;

function TSphere.isInside(aPosition: tvector3d): double;
var
  distance: double;
  tvector: tvector3d;
begin
 // Return directed distance from aPosition to spherical boundary ( <
 // 0 if inside).
  tVector := getRelativePosition(aPosition);
  distance := tVector.getLength;
  Result := distance - myRadius;
end;

function TSphere.getRadius: double;
begin
  Result := myRadius;
end;

function TSphere.getMass: double;
begin
  Result := myMass;
end;

procedure TSphere.addVelocity(anX, aY, aZ: double);
begin
  myVelocity.setX(myVelocity.getX + anX);
  myVelocity.setY(myVelocity.getY + aY);
  myVelocity.setZ(myVelocity.getZ + aZ);
end;

procedure TSphere.tick(timeIncrement: double);
begin
  myPosition.setX(myPosition.getX + (timeIncrement * myVelocity.getX));
  myPosition.setY(myPosition.getY + (timeIncrement * myVelocity.getY));
  myPosition.setZ(myPosition.getZ + (timeIncrement * myVelocity.getZ));
end;

end.
