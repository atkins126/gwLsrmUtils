unit UCoordinates;

interface

type
  TFPoint = record  // point on plain (x,y)
    x, y: Double;
  end;

  TLine = record    // line ax + by + c =0
    a, b, c: Double;
  end;

  TCircle = record
    Center: TFPoint;  // Center coordinates
    R: Double;        // radius
  end;

  TCircleLine = record // окружность и прямая в одной записи, текущая фигура определяется IsCircle: true - окружность, false - прямая 
    IsCircle: Boolean;
    Circle: TCircle;
    Line: TLine;
  end;

  TCoordVector = class // array of coordinates
  private
    Fsize: Integer;
    FCapacity: Integer;
    procedure setSize(aSize: integer);
    procedure setCapacity(aCapacity: Integer);
    function readPoint(idx: Integer): TFPoint;
    procedure setPoint(idx: Integer; const aValue: TFPoint);
    function readLastElement(): TFPoint;
    procedure setLastElement(const aValue: TFPoint);
    function calcMeanXInternal: Double;
    function calcMeanYInternal: Double;
    function calcMeanPoint: TFPoint;
    function calcStdev: TFPoint;
  public
    Points: array of TFPoint;
    destructor Destroy(); override;
    procedure push_back(aValue: TFPoint); overload;  // add value
    procedure push_back(ax, ay: double); overload;  // add value
    function pop_back(): TFPoint;          // returns and deletes last value
    procedure insert(idx: Integer; aValue: TFPoint);  // insert value into idx-position
    procedure clear();            // deletes all elements in vector
    function empty(): Boolean;    // returns True if vector is empty
    function distance(idx1, idx2: Integer): Double;  // distance between 2 points
    function allPointsDifferent(dDist: Double): Boolean;  // check if all points different (distance is less than distance error)
    procedure shrink_to_fit();    // shrink vector size: free unused memmory
    procedure erase(idx: Integer);  // deletes idx element from vector
    procedure addPoints(const aCoordVec: TCoordVector);
    property at[idx: Integer]: TFPoint read readPoint write setPoint; default;    // access to element with range check
    property size: Integer read Fsize write setSize;                              // set used size, but not size of vector
    property capacity: Integer read FCapacity write setCapacity;                  // set real vector size
    property back: TFPoint read readLastElement write setLastElement;             // get last element
    property meanX: Double read calcMeanXInternal;                                // mean value of x-coord
    property meanY: Double read calcMeanYInternal;                                // mean value of x-coord
    property meanPoint: TFPoint read calcMeanPoint;                               // mean point
    property stdevPoint: TFPoint read calcStdev;
  end;

implementation

uses Classes;

{ TCoordVector }

procedure TCoordVector.addPoints(const aCoordVec: TCoordVector);
var i: Integer;
begin
  if (aCoordVec.size = 0) then Exit;
  setCapacity(Fsize + aCoordVec.size);
  for i:= 0 to aCoordVec.size-1 do
    push_back(aCoordVec[i]);
end;

function TCoordVector.allPointsDifferent(dDist: Double): Boolean;
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

function TCoordVector.calcMeanPoint: TFPoint;
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

function TCoordVector.calcStdev: TFPoint;
var i: Integer;
    AveData: TFPoint;
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

function TCoordVector.calcMeanXInternal: Double;
var i: Integer;
begin
  Result := 0;
  if (Fsize = 0) then Exit;

  for i := 0 to FSize-1 do
    Result := Result + Points[i].x;
  Result := Result / FSize;
end;

function TCoordVector.calcMeanYInternal: Double;
var i: Integer;
begin
  Result := 0;
  if (Fsize = 0) then Exit;
  
  for i := 0 to FSize-1 do
    Result := Result + Points[i].y;
  Result := Result / FSize;
end;

procedure TCoordVector.clear;
begin
  setSize(0);
end;

destructor TCoordVector.Destroy;
begin
  Points := nil;
  inherited;
end;

function TCoordVector.distance(idx1, idx2: Integer): Double;
begin
  Result := Sqrt(sqr(Points[idx1].x - Points[idx2].x) + Sqr(Points[idx1].y - Points[idx2].y));
end;

function TCoordVector.empty: Boolean;
begin
  Result := (Fsize = 0);
end;

procedure TCoordVector.erase(idx: Integer);
var i: Integer;
begin
  if (idx < 0) and (idx >= FSize) then
    Exit;

  for i:=idx to Fsize-2 do
    Points[i] := Points[i+1];
  setSize(FSize-1);
end;

procedure TCoordVector.insert(idx: Integer; aValue: TFPoint);
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

function TCoordVector.pop_back: TFPoint;
begin
  Result := Points[Fsize-1];
  setSize(Fsize - 1);
end;

procedure TCoordVector.push_back(aValue: TFPoint);
begin
  setSize(Fsize + 1);
  Points[Fsize - 1] := aValue;
end;

procedure TCoordVector.push_back(ax, ay: double);
begin
  setSize(Fsize + 1);
  Points[Fsize - 1].x := ax;
  Points[Fsize - 1].y := ay;
end;

function TCoordVector.readLastElement: TFPoint;
begin
  if (FSize > 0) then
    Result := Points[Fsize-1]
  else
    raise EListError.Create('List is empty');
end;

function TCoordVector.readPoint(idx: Integer): TFPoint;
begin
  if (idx >= 0) and (idx < FSize) then
    Result := Points[idx]
  else
    raise EListError.CreateFmt('Index %d is out of bounds', [idx]);
end;
    
procedure TCoordVector.setCapacity(aCapacity: Integer);
begin
  if (aCapacity >= 0) then
    FCapacity := aCapacity
  else
    FCapacity := 0;
  SetLength(Points, FCapacity);
end;

procedure TCoordVector.setLastElement(const aValue: TFPoint);
begin
  if (FSize > 0) then
    Points[Fsize-1] := aValue
  else
    raise EListError.Create('List is empty');
end;

procedure TCoordVector.setPoint(idx: Integer; const aValue: TFPoint);
begin
  if (idx >= 0) and (idx < FSize) then
    Points[idx] := aValue
  else
    raise EListError.CreateFmt('Index %d is out of bounds', [idx]);
end;

procedure TCoordVector.setSize(aSize: integer);
begin
  if (aSize >= 0) then
    Fsize := aSize
  else
    Fsize := 0;

  if (Fsize > FCapacity) then
    setCapacity(Fsize);
end;

procedure TCoordVector.shrink_to_fit;
begin
  if (FCapacity > Fsize) then
    setCapacity(Fsize);
end;

end.
