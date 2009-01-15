unit DAV_StkVector3D;

{
/***************************************************/
/*! \class TVector3D
    \brief STK 3D vector class.

    This class implements a three-dimensional vector.

    by Perry R. Cook, 1995 - 2002.
*/
/***************************************************/
}
interface

type
  TVector3D = class
  public
  //! Default constructor taking optional initial X, Y, and Z values.
    constructor Create(initX, initY, initZ: double);

  //! Class destructor.
    destructor Destroy;

  //! Get the current X value.
    function getX: double;

  //! Get the current Y value.
    function getY: double;

  //! Get the current Z value.
    function getZ: double;

  //! Calculate the vector length.
    function getLength: double;

  //! Set the X, Y, and Z values simultaniously.
    procedure setXYZ(anX, aY, aZ: double);

  //! Set the X value.
    procedure setX(aval: double);

  //! Set the Y value.
    procedure setY(aval: double);

  //! Set the Z value.
    procedure setZ(aval: double);

  protected
    myX, myY, myZ: double
  end;

implementation

constructor TVector3D.Create;
begin
  myX := initX;
  myY := initY;
  myZ := initZ;
end;

destructor TVector3D.Destroy;
begin
end;

function TVector3D.getX;
begin
  Result := myX;
end;

function TVector3D.getY;
begin
  Result := myY;
end;

function TVector3D.getZ;
begin
  Result := myZ;
end;

function TVector3D.getLength;
var
  temp: double;
begin
  temp := myX * myX;
  temp := temp + myY * myY;
  temp := temp + myZ * myZ;
  temp := sqrt(temp);
  Result := temp;
end;

procedure TVector3D.setXYZ;
begin
  myX := anX;
  myY := aY;
  myZ := aZ;
end;

procedure TVector3D.setX;
begin
  myX := aval;
end;

procedure TVector3D.setY;
begin
  myY := aval;
end;

procedure TVector3D.setZ;
begin
  myZ := aval;
end;

end.
