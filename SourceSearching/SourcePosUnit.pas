unit SourcePosUnit;
{#Method description see in Source_seek_n_destroy.odt}
interface

uses
  Math, Matrix, UCoordinates;

const
  EPS = 1e-6;
  TEST_POINT_NUM = 3;
  DERROR = 1;

  {�������� �������, ������������ ��������� ��������� � ����������� ��������� ���������,
  �� ������ ��������� ��������� ��������� � ��������� ����� �� ��������� �� ���������.
  ���� ���������� �������, �� ���������� 0, ���� �� ��������� ������, �� ���������� ����� �������� �� 0 -- ��� ������
  Error Codes
  1: ����� ����� ��������� � ����� ��������� ����� ������
  2: ����� ����� ���������/��������� ���������� ������ 3.
  3: ���� �� ��������� ����� <= 0
  4: ��� ����� �����������, �������� ����������� ������� ���������� ��� �������� �����, ���� ������������� ���������� ������, �������������� ������� ����� ���������� �����. ����� ��������� ����������, �������� ������ �����
  5: 2 ���������� ��������� ��������� �� ������� ������
  6: �� ������ �������� � �������� ������
  9: ���-�� ����� �� ���}
  function SearchSource(const detCoord: TCoordVector; detCoordError: Double; const CountRates: TDoubleVector;
    var SorcePosNum: Integer; var SourceCoord, SourceCoordEr: TCoordVector): Integer; // see description before

  {inner functions}
  // ���������� ���
  function GMTfrom2measure(D1pos, D2pos: TFPoint; n1, n2: Double): TCircleLine; // ������� ������������ ��������� ��� ���������� ��������� ��������� �� 2-� ��������� (���� �����, ���� ����������)
  function GMTfrom2measType(n1, n2: Double): Boolean; // ��������, ����� ������ ������ ��� ���������� �������� ��������� ������������� ���������
  function Circlefrom2meas(D1pos, D2pos: TFPoint; n1, n2: Double): TCircle; // ������� ������������ ��������� ���������� ��� ���������� ��������� ��������� �� 2-� ���������
  function Linefrom2meas(D1pos, D2pos: TFPoint; n1, n2: Double): TLine;  // ������� ������������ ��������� ������ ��� ���������� ��������� ��������� �� 2-� ���������
  // �������������� ��������� � ������ ��������������
  function RelCS2AbsCS(D1pos, D2pos: TFPoint; InP: TFPoint): TFPoint; // ������������ ������� ��������� ����� �� ������������ �� � ��� ��
  function RelCS2AbsCSLine(D1pos, D2pos: TFPoint; InL: TLine): TLine; // ������������ ������� ������������� ������ �� ������������ �� � ��� ��
  function Distance(Point1, Point2: TFPoint): Double; // ���������� ����� 2-�� �������
  function MinCharDistance(const DetPos: TCoordVector): Double; // ����������� ����������� ���������� -- ��� ����������� ���������� ����� �����������
  // ���������� ����� �����������
  function ShapeShapeIntersection(const Shape1, Shape2: TCircleLine; var IntersPoints: TCoordVector): Integer; // ������� ����� ����������� 2-� ����� (����������� ��� ������) � ���������� ����� ����� �����������
  function CircleLineIntersection(const Circle1: TCircle; Line1: TLine; var IntersPoints: TCoordVector): Integer; // ������� ����� ����������� ���������� � ������ � ���������� ����� ����� �����������
  function CircleCircleIntersection(Circle1, Circle2: TCircle; var IntersPoints: TCoordVector): Integer; // ������� ����� ����������� 2-� ����������� � ���������� ����� ����� �����������
  function LineLineIntersection(const Line1, Line2: TLine; var IntersPoints: TCoordVector): Integer;  // ������� ����� ����������� 2-� ������ � ���������� ����� ����� �����������

  
implementation

function SearchSource(const detCoord: TCoordVector; detCoordError: Double; const countRates: TDoubleVector;
    var SorcePosNum: Integer; var SourceCoord, SourceCoordEr: TCoordVector): Integer;
{TODO: �������� ���� ����������� ����� � ���������� � ���������� ������ �������� �����������}
var i, j, k, GMTCount: Integer;
    CirclesLines: array of TCircleLine;
    tempInTPoints, sumIntersecTPoints,                // ������� ��� ����� ����������� ��������� � ���������
    tempSourceCoord, tempSourceCoordEr: TCoordVector;
    pointsNum: array of Integer;  // ����� �����, �������������� ��� ����������
    detCount: Integer;            // counts of detector coordinates or num of points of measure
begin
  // check input data
  detCount := detCoord.size;
  if (detCount <> countRates.size) then
  begin
    Result := 1;
    Exit
  end;
  if (detCount < 3) then
  begin
    Result := 2;
    Exit
  end;
  (*TODO: think about this condition*)
  for i := 0 to detCount - 1 do  // n ��� ������ ���� ������ 0
    if countRates[i] <= 0 then
    begin
      Result := 3;
      Exit
    end;
  if not detCoord.allPointsDifferent(detCoordError) then // ������ �������� 2 ���������� ��������� ���������
  begin
    Result := 5;
    Exit;
  end;
  Result := 9;

  // ���������� ��� ���������� ��������� ��������� (���� �� 3-� ������), ������ �� ������� ����� ������ ����������
  GMTCount := Trunc(detCount * (detCount - 1) / 2); // ����� ��������� ��� = C_n_2
  SetLength(CirclesLines, GMTCount);
  k := 0;
  for i := 0 to detCount - 2 do
    for j := i + 1 to detCount - 1 do
    begin
      CirclesLines[k] := GMTfrom2measure(detCoord[i], detCoord[j], countRates[i], countRates[j]);
      Inc(k);
    end;

  // ���������� ����� �����������
  SumIntersecTPoints := TCoordVector.Create();
  for i := 0 to GMTCount - 2 do
    for j := i + 1 to GMTCount - 1 do
      ShapeShapeIntersection(CirclesLines[i], CirclesLines[j], SumIntersecTPoints);

  // �������� �� ������� ����� �����������
  if (SumIntersecTPoints.size <= 0) then
  begin
    Result := 4;
    Exit;
  end;

  // ��������� ����� �����������  (������ ��������� ������ ��������� � ���� ����� ����� � ������ ����� ��� �������)
  {TODO: ��������� ����� ��������� �����}
  j := 0;
  tempInTPoints := TCoordVector.Create();
  TempSourceCoord := TCoordVector.Create();
  TempSourceCoordEr := TCoordVector.Create();
  while (SumIntersecTPoints.size > 0) do  
  begin
    // ����� ������������� �� ������� SumIntersectPoints ���������������,
    // ���������� �� ��������� ������,
    // ������������ ���������� ����� ������� � ���� ������ ����� ����� ��������,
    // �� ���������� � ������. ����� �� ���� ����� �� ����� ������� ���������� �������
    tempInTPoints.size := 1;
    tempInTPoints[0] := SumIntersecTPoints.pop_back();
    i := 0;
    while (i < SumIntersecTPoints.size) do
    begin
      if Distance(SumIntersecTPoints[i], tempInTPoints[0]) < (MinCharDistance(detCoord) / 10) then
      begin
        tempInTPoints.push_back(SumIntersecTPoints[i]);
        SumIntersecTPoints.erase(i);
      end
      else
        Inc(i);
    end;

    // ����� ������� ����� ����������� � ���������� ����
    TempSourceCoord.push_back(tempInTPoints.meanPoint);
    TempSourceCoordEr.push_back(tempInTPoints.stdevPoint);
    SetLength(PointsNum, j + 1);
    PointsNum[j] := tempInTPoints.size;           // ����� �����, ���������������� ��� ����������

    Inc(j);
  end;

  // ����� ����� � ���������� ������ ����������� � ������ �� � SourceCoord
  k := MaxIntValue(PointsNum);
  SorcePosNum := 0;
  SourceCoord := TCoordVector.Create();
  SourceCoordEr := TCoordVector.Create();
  for i := 0 to Length(PointsNum) - 1 do
  begin
    if PointsNum[i] >= k then
    begin
      try
        SourceCoord.push_back(TempSourceCoord[i]);
        SourceCoordEr.push_back(TempSourceCoordEr[i]);
        Inc(SorcePosNum);
      except
        Result := 6;
        Exit;
      end;
    end;
  end;

  Result := 0;

  SumIntersecTPoints.Free();
  tempInTPoints.Free();
  TempSourceCoord.Free();
  TempSourceCoordEr.Free();
end;

function GMTfrom2measure(D1pos, D2pos: TFPoint; n1, n2: Double): TCircleLine;
begin
  if GMTfrom2measType(n1, n2) then // ���������� ��� ������ ��� � ��������� ��������������� �������
  begin
    Result.IsCircle := True;
    Result.Circle := Circlefrom2meas(D1pos, D2pos, n1, n2);
  end
  else
  begin
    Result.IsCircle := False;
    Result.Line := Linefrom2meas(D1pos, D2pos, n1, n2);
  end;
end;

function GMTfrom2measType(n1, n2: Double): Boolean;
begin
  Result := Abs(n1 - n2) > EPS;
end;

function Circlefrom2meas(D1pos, D2pos: TFPoint; n1, n2: Double): TCircle; 
var R: Double;
begin
  // ���������� ���������� ����� ����������� � ������� ��� �� 2
  R := Sqrt(sqr(D2pos.x - D1pos.x) + sqr(D2pos.y - D1pos.y)) / 2;

  // ���������� ���������� ���������� � ���. ������� ���������: ����� �������� ���������� ����� �����������, ��������� ����������� �� ��� y (��. Source_seek_n_destroy.odt)
  Result.Center.x := 0;
  Result.Center.y := (n1 + n2) / (n1 - n2) * R;
  Result.R := 2 * sqrt(n1 * n2) / Abs(n1 - n2) * R;

  // ������� �� ������������� ������� ��������� � ��, � ������� ������ ��������� ���������
  Result.Center := RelCS2AbsCS(D1pos, D2pos, Result.Center);
end;

function Linefrom2meas(D1pos, D2pos: TFPoint; n1, n2: Double): TLine;
begin
  Result.a := 0;
  Result.b := 1;
  Result.c := 0;

  // ������� �� ������������� ������� ��������� � ��, � ������� ������ ��������� ���������
  Result := RelCS2AbsCSLine(D1pos, D2pos, Result);
end;

function RelCS2AbsCS(D1pos, D2pos: TFPoint; InP: TFPoint): TFPoint;
var x0, y0: Double; // ������������ �������� ��
    cphi, sphi, d1d2dist: Double; // cos(phi), sin(phi) ������� �� � ���������� ����� ����������� ��� �� �������
    TempX: double;
begin
  Result := InP;
  if (D1pos.x = 0) and (D2pos.x = 0) then
    Exit; // �� �� �������, ������ ������ �� �����
 
  //���������� ������ ��������� ������������ �� � ���
  x0 := 0.5 * (D1pos.x + D2pos.x);
  y0 := 0.5 * (D1pos.y + D2pos.y);
  // sin � cos ����
  d1d2dist := Distance(D1pos, D2pos);
  sphi := -(D1pos.x - D2pos.x) / d1d2dist;
  cphi := (D1pos.y - D2pos.y) / d1d2dist;

  // ������� ��
  TempX := Result.x * cphi - Result.y * sphi;
  Result.y := Result.x * sphi + Result.y * cphi;
  Result.x := TempX;
  // ������������ �������� �� (���������� �������)
  Result.x := Result.x + x0;
  Result.y := Result.y + y0;
end;

function RelCS2AbsCSLine(D1pos, D2pos: TFPoint; InL: TLine): TLine;
var x0, y0: Double; // ������������ �������� ��
    cphi, sphi, d1d2dist: Double; // cos(phi), sin(phi) ������� �� � ���������� ����� ����������� ��� �� �������
begin
  Result := InL;
  if (D1pos.x = 0) and (D2pos.x = 0) then Exit; // �� �� �������, ������ ������ �� �����

  //���������� ������ ��������� ������������ �� � ���
  x0 := 0.5 * (D1pos.x + D2pos.x);
  y0 := 0.5 * (D1pos.y + D2pos.y);
  // sin � cos ����
  d1d2dist := Distance(D1pos, D2pos);
  sphi := -(D1pos.x - D2pos.x) / d1d2dist;
  cphi := (D1pos.y - D2pos.y) / d1d2dist;

  // ������� ��
  Result.a := InL.a * cphi - InL.b * sphi;
  Result.b := InL.a * sphi + InL.b * cphi;
  // ������������ ��������
  Result.c := Result.c + Result.a * x0 + Result.b * y0;
end;

function Distance(Point1, Point2: TFPoint): Double;
begin
  Result := Sqrt(sqr(Point1.x - Point2.x) + Sqr(Point1.y - Point2.y));
end;

function MinCharDistance(const DetPos: TCoordVector): Double;
var i, j: Integer;
begin
  Result := DetPos.distance(0, 1);

  for i := 1 to DetPos.size - 2 do
    for j := 2 to DetPos.size-1 do
      if Result < DetPos.distance(i, j) then
        Result := DetPos.distance(i, j);
end;

function ShapeShapeIntersection(const Shape1, Shape2: TCircleLine; var IntersPoints: TCoordVector): Integer;
begin
  if (Shape1.IsCircle and Shape2.IsCircle) then
    Result := CircleCircleIntersection(Shape1.Circle, Shape2.Circle, IntersPoints)
  else if not (Shape1.IsCircle) and Shape2.IsCircle then
    Result := CircleLineIntersection(Shape2.Circle, Shape1.Line, IntersPoints)
  else if Shape1.IsCircle and not (Shape2.IsCircle) then
    Result := CircleLineIntersection(Shape1.Circle, Shape2.Line, IntersPoints)
  else
    Result := LineLineIntersection(Shape1.Line, Shape2.Line, IntersPoints);
end;

function CircleLineIntersection(const Circle1: TCircle; Line1: TLine; var IntersPoints: TCoordVector): Integer;
var NearR, d, mult: Double; // ���������� �� ��������� ����� ������ �� ������; ���������� �� ����.�. �� ����� �����������; ��� ��������� ��������
    NearPoint: TFPoint; // ��������� ����� ������ � ������ �����
begin
  Result := -1;
  // ��������� ����� ����� � ������ ���������
  Line1.c := Line1.c - Line1.a * Circle1.Center.x - Line1.b * Circle1.Center.y;

  // ���������� ��������� �����
  NearR := Abs(Line1.c) / sqrt(Sqr(Line1.a) + Sqr(Line1.b));
  NearPoint.x := -(Line1.a * Line1.c) / (Sqr(Line1.a) + Sqr(Line1.b));
  NearPoint.y := -(Line1.b * Line1.c) / (Sqr(Line1.a) + Sqr(Line1.b));

  // ���������� ����� ����������� � ���������
  if NearR > (Circle1.R + EPS) then
    Result := 0
  else if Abs(NearR - Circle1.R) < EPS then
  begin
    Result := 1;
    IntersPoints.push_back(NearPoint.x + Circle1.Center.x,
                           NearPoint.y + Circle1.Center.y); // ������ ������� �.�������
  end
  else if NearR < (Circle1.R + EPS) then
  begin
    Result := 2;
    d := Sqr(Circle1.R) - Sqr(NearR);
    mult := Sqrt(d / (Sqr(Line1.a) + Sqr(Line1.b)));
    IntersPoints.push_back(NearPoint.x + Line1.b * mult + Circle1.Center.x,
                           NearPoint.y - Line1.a * mult + Circle1.Center.y);
    IntersPoints.push_back(NearPoint.x - Line1.b * mult + Circle1.Center.x,
                           NearPoint.y + Line1.a * mult + Circle1.Center.y);
  end;
end;

function CircleCircleIntersection(Circle1, Circle2: TCircle; var IntersPoints: TCoordVector): Integer;
var LineFromCircle: TLine;
    TempCircle1: TCircle;
    IntPoints: TCoordVector;
begin
  //Result := -1;

  // ���������� ����������� ������, ����� ������ ������ ���������
  if Abs(Circle1.Center.x - Circle2.Center.x) < EPS then
    if Abs(Circle1.R - Circle2.R) < EPS then
    begin
      Result := -2; // ������� ����������
      Exit
    end
    else
    begin
      Result := 0; // ��� �������
      Exit
    end;

  // ��������� ����� ������� ����� � ������ ���������
  Circle2.Center.x := Circle2.Center.x - Circle1.Center.x;
  Circle2.Center.y := Circle2.Center.y - Circle1.Center.y;
  TempCircle1.Center.x := 0;
  TempCircle1.Center.y := 0;
  TempCircle1.R := Circle1.R;

  // �������� �� ��������� ��� 2-�� ����� ��������� ��� 1-��, �������� ��-� ��� ������
  LineFromCircle.a := -2 * Circle2.Center.x;
  LineFromCircle.b := -2 * Circle2.Center.y;
  LineFromCircle.c := Sqr(Circle2.Center.x) + Sqr(Circle2.Center.y) + Sqr(Circle1.R) - Sqr(Circle2.R);

  // solving
  IntPoints := TCoordVector.Create();
  Result := CircleLineIntersection(TempCircle1, LineFromCircle, IntPoints);
  if Result = 1 then
  begin
    IntersPoints.push_back(IntPoints[0].x + Circle1.Center.x,
                           IntPoints[0].y + Circle1.Center.y);
  end
  else if (Result = 2) then
  begin
    IntersPoints.push_back(IntPoints[0].x + Circle1.Center.x,
                           IntPoints[0].y + Circle1.Center.y);
    IntersPoints.push_back(IntPoints[1].x + Circle1.Center.x,
                           IntPoints[1].y + Circle1.Center.y);
  end;
  IntPoints.Free();
end;

function LineLineIntersection(const Line1, Line2: TLine; var IntersPoints: TCoordVector): Integer;
var D0, D1, D2: Double;   // ������������ �� ����� �������
begin
  D0 := Line1.a * Line2.b - Line2.a * Line1.b;
  if Abs(D0) < EPS then
  begin
    Result := 0;
    Exit
  end;

  D1 := Line1.c * Line2.b - Line2.c * Line1.b;
  D2 := Line1.a * Line2.c - Line2.a * Line1.c;

  Result := 1;
  IntersPoints.push_back(-D1/D0, -D2/D0);
end;

end.
