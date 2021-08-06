unit U3DCoord;

interface

const infinity = 1e9;

type
  T3DPoint = record  // point in space (x,y, z)
    x, y, z: Double;
  end;

  TCoord3DVector = class // array of coordinates
  private
    Fsize: Integer;
    FCapacity: Integer;
    procedure setSize(aSize: integer);
    procedure setCapacity(aCapacity: Integer);
    function readPoint(idx: Integer): T3DPoint;
    procedure setPoint(idx: Integer; const aValue: T3DPoint);
    function readLastElement(): T3DPoint;
    procedure setLastElement(const aValue: T3DPoint);
    function calcMeanXInternal: Double;
    function calcMeanYInternal: Double;
    function calcMeanPoint: T3DPoint;
    function calcStdev: T3DPoint;
  public
    Points: array of T3DPoint;
    constructor Create(aCapacity: Integer); overload;
    destructor Destroy(); override;
    procedure push_back(aValue: T3DPoint); overload;  // add value
    procedure push_back(ax, ay, az: Double); overload;  // add value
    function pop_back(): T3DPoint;          // returns and deletes last value
    procedure insert(idx: Integer; aValue: T3DPoint);  // insert value into idx-position
    procedure clear();            // deletes all elements in vector
    function empty(): Boolean;    // returns True if vector is empty
    function distance(idx1, idx2: Integer): Double;  // distance between 2 points
    function allPointsDifferent(dDist: Double): Boolean;  // check if all points different (distance is less than distance error)
    procedure shrink_to_fit();    // shrink vector size: free unused memmory
    procedure erase(idx: Integer);  // deletes idx element from vector
    procedure addPoints(const aCoordVec: TCoord3DVector);
    property at[idx: Integer]: T3DPoint read readPoint write setPoint; default;    // access to element with range check
    property size: Integer read Fsize write setSize;                              // set used size, but not size of vector
    property capacity: Integer read FCapacity write setCapacity;                  // set real vector size
    property back: T3DPoint read readLastElement write setLastElement;             // get last element
    property meanX: Double read calcMeanXInternal;                                // mean value of x-coord
    property meanY: Double read calcMeanYInternal;                                // mean value of x-coord
    property meanPoint: T3DPoint read calcMeanPoint;                               // mean point
    property stdevPoint: T3DPoint read calcStdev;
  end;

  function distanceBetween2Points(const aPoint1, aPoint2: T3DPoint): Double;
  procedure getVector(const aPoint1, aPoint2: T3DPoint; R: Double; var u, v, w: Double);

implementation

uses Classes;

{ TCoordVector }

procedure TCoord3DVector.addPoints(const aCoordVec: TCoord3DVector);
var i: Integer;
begin
  if (aCoordVec.size = 0) then Exit;
  setCapacity(Fsize + aCoordVec.size);
  for i:= 0 to aCoordVec.size-1 do
    push_back(aCoordVec[i]);
end;

function TCoord3DVector.allPointsDifferent(dDist: Double): Boolean;
var i, j: Integer;
begin
  Result := True;
  for i:=0 to Fsize-2 do
    for j:=i+1 to Fsize-1 do
      if (distance(i, j) < dDist) then begin
        Result := False;
        Exit;
      end;
end;

function TCoord3DVector.calcMeanPoint: T3DPoint;
var i: Integer;
begin
  Result.x := 0;
  Result.y := 0;
  if (Fsize = 0) then Exit;

  for i := 0 to FSize-1 do begin
    Result.x := Result.x + Points[i].x;
    Result.y := Result.y + Points[i].y;
  end;
  Result.x := Result.x / FSize;
  Result.y := Result.y / FSize;
end;

function TCoord3DVector.calcStdev: T3DPoint;
var i: Integer;
    AveData: T3DPoint;
begin
  Result.x := 0;
  Result.y := 0;
  if (Fsize = 0) then Exit;
  
  AveData := calcMeanPoint();
  for i := 0 to FSize-1 do begin
    Result.x := Result.x + sqr(Points[i].x - AveData.x);
    Result.y := Result.y + sqr(Points[i].y - AveData.y);
  end;
  Result.x := Sqrt(Result.x / FSize);
  Result.y := Sqrt(Result.y / FSize);
end;

function TCoord3DVector.calcMeanXInternal: Double;
var i: Integer;
begin
  Result := 0;
  if (Fsize = 0) then Exit;

  for i := 0 to FSize-1 do
    Result := Result + Points[i].x;
  Result := Result / FSize;
end;

function TCoord3DVector.calcMeanYInternal: Double;
var i: Integer;
begin
  Result := 0;
  if (Fsize = 0) then Exit;
  
  for i := 0 to FSize-1 do
    Result := Result + Points[i].y;
  Result := Result / FSize;
end;

procedure TCoord3DVector.clear;
begin
  setSize(0);
end;

destructor TCoord3DVector.Destroy;
begin
  Points := nil;
  inherited;
end;

function TCoord3DVector.distance(idx1, idx2: Integer): Double;
begin
  Result := Sqrt(sqr(Points[idx1].x - Points[idx2].x) + Sqr(Points[idx1].y - Points[idx2].y));
end;

function TCoord3DVector.empty: Boolean;
begin
  Result := (Fsize = 0);
end;

procedure TCoord3DVector.erase(idx: Integer);
var i: Integer;
begin
  if (idx < 0) and (idx >= FSize) then
    Exit;

  for i:=idx to Fsize-2 do
    Points[i] := Points[i+1];
  setSize(FSize-1);
end;

procedure TCoord3DVector.insert(idx: Integer; aValue: T3DPoint);
var i: Integer;
begin
  if (idx > Fsize) then
    idx := Fsize;

  if (idx = Fsize) then begin
    push_back(aValue);
    Exit;
  end;

  setSize(Fsize + 1);
  for i:= Fsize-2 downto idx do
    Points[i+1] := Points[i];
  Points[idx] := aValue;
end;

function TCoord3DVector.pop_back: T3DPoint;
begin
  Result := Points[Fsize-1];
  setSize(Fsize - 1);
end;

procedure TCoord3DVector.push_back(aValue: T3DPoint);
begin
  setSize(Fsize + 1);
  Points[Fsize - 1] := aValue;
end;

procedure TCoord3DVector.push_back(ax, ay, az: Double);
begin
  setSize(Fsize + 1);
  Points[Fsize - 1].x := ax;
  Points[Fsize - 1].y := ay;
  Points[Fsize - 1].z := az;
end;

function TCoord3DVector.readLastElement: T3DPoint;
begin
  if (FSize > 0) then
    Result := Points[Fsize-1]
  else
    raise EListError.Create('List is empty');
end;

function TCoord3DVector.readPoint(idx: Integer): T3DPoint;
begin
  if (idx >= 0) and (idx < FSize) then
    Result := Points[idx]
  else
    raise EListError.CreateFmt('Index %d is out of bounds', [idx]);
end;
    
procedure TCoord3DVector.setCapacity(aCapacity: Integer);
begin
  if (aCapacity >= 0) then
    FCapacity := aCapacity
  else
    FCapacity := 0;
  SetLength(Points, FCapacity);
end;

procedure TCoord3DVector.setLastElement(const aValue: T3DPoint);
begin
  if (FSize > 0) then
    Points[Fsize-1] := aValue
  else
    raise EListError.Create('List is empty');
end;

procedure TCoord3DVector.setPoint(idx: Integer; const aValue: T3DPoint);
begin
  if (idx >= 0) and (idx < FSize) then
    Points[idx] := aValue
  else
    raise EListError.CreateFmt('Index %d is out of bounds', [idx]);
end;

procedure TCoord3DVector.setSize(aSize: integer);
begin
  if (aSize >= 0) then
    Fsize := aSize
  else
    Fsize := 0;

  if (Fsize > FCapacity) then
    setCapacity(Fsize);
end;

procedure TCoord3DVector.shrink_to_fit;
begin
  if (FCapacity > Fsize) then
    setCapacity(Fsize);
end;



// functions

function distanceBetween2Points(const aPoint1, aPoint2: T3DPoint): Double;
begin
  Result := Sqrt(Sqr(aPoint1.x - aPoint2.x) + Sqr(aPoint1.y - aPoint2.y) + Sqr(aPoint1.z - aPoint2.z));
end;

procedure getVector(const aPoint1, aPoint2: T3DPoint; R: Double; var u, v, w: Double);
begin
  u := (aPoint1.x - aPoint2.x) / R;
  v := (aPoint1.y - aPoint2.y) / R;
  w := (aPoint1.z - aPoint2.z) / R;
end;

constructor TCoord3DVector.Create(aCapacity: Integer);
begin
  setCapacity(aCapacity);
end;

end.
