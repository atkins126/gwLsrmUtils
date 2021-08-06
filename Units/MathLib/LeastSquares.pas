unit LeastSquares;

interface

uses
  Matrix {$IFDEF LSRM}, LMatrixDin {$ENDIF};

// МНК
function OLS_StraightLineProp(const xi, yi: array of Double; var a, da: Double): Boolean; // метод МНК для прямой пропорциональности
function OLS_StraightLinePropXY(const xi, zi, yi: array of Double; var a, b: Double;
  IsWeighted: Boolean = False; const dyi: TVector = nil): Boolean; // y = Ax + Bz 
function MNK(const xi, yi: array of Double; ApproxDegree: Integer;
  out Coeffs: Matrix.TVector): Boolean; overload;  // аппроксимация по МНК
function MNK(const xi, yi, dyi: array of Double; ApproxDegree: Integer; IsWeighted, IsCorrectOnDens: Boolean;
             out Coeffs: Matrix.TVector; out dA0, dA1: Double): Boolean;  overload;  // аппроксимация по МНК
procedure CorrectWeightsOnPointsDensity(const xi: array of Double; var wi: Matrix.TVector; NZones: Integer);
procedure LinearMNKError(A1, A0: Double; const xi, yi: array of Double; var dA1, dA0: Double); // y = A1x + A0
function Correlation(const xi, yi: array of Double): Double;
{$IFDEF LSRM}
function MNK_dan(const xi, yi, dyi: array of Double; ApproxDegree: Integer; IsWeighted, IsCorrectOnDens: Boolean;
             out Coeffs: Matrix.TVector; out dA0, dA1: Double): Boolean;
{$ENDIF}

implementation

uses Math, SLAU;

function OLS_StraightLineProp(const xi, yi: array of Double; var a, da: Double): Boolean;
var xxAve, xyAve: Double;
    i, N: Integer;
begin
  Result := False;
  if (Length(xi) <> Length(yi)) then
    Exit;
  N := Length(xi);
  if (N <= 1) then
    Exit;
  xxAve := 0;
  xyAve := 0;

  for i := 0 to N - 1 do
  begin
    xxAve := xxAve + xi[i] * xi[i];
    xyAve := xyAve + xi[i] * yi[i];
  end;
  //xxAve:=xxAve/N;
  //xyAve:=xyAve/N;

  a := xyAve / xxAve;
  da := 0;
  for i := 0 to N - 1 do
    da := da + Sqr(a * xi[i] - yi[i]);
  da := Sqrt(da / (N - 1));

  Result := True;
end;

function OLS_StraightLinePropXY(const xi, zi, yi: array of Double; var a, b: Double;
  IsWeighted: Boolean = False; const dyi: TVector = nil): Boolean;
var x2Sum, z2Sum, xySum, xzSum, yzSum: Double;
    wi: Double;
    i, N: Integer;
begin
  Result:=False;
  if ((Length(xi)<>Length(yi)) or (Length(xi)<>Length(zi))) then Exit;
  N:=Length(xi);
  if (N<=2) then Exit;
  x2Sum:=0; z2Sum:=0;
  xySum:=0; xzSum:=0; yzSum:=0;

  for i:=0 to N-1 do
  begin
    if ((IsWeighted) and (dyi[i]<>0)) then
      wi := 1/sqr(dyi[i])
    else
      wi := 1;
    x2Sum:=x2Sum+xi[i]*xi[i]*wi;
    z2Sum:=z2Sum+zi[i]*zi[i]*wi;
    xySum:=xySum+xi[i]*yi[i]*wi;
    xzSum:=xzSum+xi[i]*zi[i]*wi;
    yzSum:=yzSum+yi[i]*zi[i]*wi;
  end;

  b := x2Sum*z2Sum - Sqr(xzSum); // denominator
  if (b < MinSingle) then Exit;
  
  a := (xySum*z2Sum - yzSum*xzSum)/b;
  b := (yzSum*x2Sum - xySum*xzSum)/b;
  {da:=0;
  for i:=0 to N-1 do
    da:=da + Sqr(a*xi[i] -yi[i]);
  da:=Sqrt(da/(N-1)); }

  Result := True;
end;

function MNK(const xi, yi: array of Double; ApproxDegree: Integer;
  out Coeffs: Matrix.TVector): Boolean; overload;  // аппроксимация по МНК
var C: Matrix.TMatrix;
    B: Matrix.TVector;
    i, j, m, k: Integer;
begin
  // check conditions
  Result := False;
  if (Length(xi) <> Length(yi)) then Exit;
  if (Length(xi) < ApproxDegree + 1) then Exit;
  if (ApproxDegree < 0) then Exit;
  m := ApproxDegree;

  (* creates matrices for least squares
     Aij= Sum(i) (x_i^j * x_i^k)
     Bk= Sum(i) (y_i * x_i^k)  *)

  SetLength(B, m+1);
  SetLength(C, m+1, m+1);
  SetLength(Coeffs, m+1);

  for k:=0 to m do
  begin
    B[k]:=0;
    for i:=0 to High(yi) do
      B[k]:= B[k] +  yi[i]*IntPower(xi[i],k);
    for j:=0 to m do
    begin
      C[j,k]:=0;
      for i:=0 to High(xi) do
        C[j,k]:= C[j,k]+ IntPower(xi[i],j)*IntPower(xi[i],k);
    end;
  end;

  Chol(C, B, Coeffs, m+1);
  Result := True;
end;

function MNK(const xi, yi, dyi: array of Double; ApproxDegree: Integer; IsWeighted, IsCorrectOnDens: Boolean;
             out Coeffs: Matrix.TVector; out dA0, dA1: Double): Boolean; overload;
var C: Matrix.TMatrix;
    B, wi, ki: Matrix.TVector;
    i, j, m, k: Integer;
begin
  // check conditions
  Result := False;
  if (Length(xi) <> Length(yi)) then Exit;
  if (Length(xi) < ApproxDegree + 1) then Exit;
  if (ApproxDegree < 0) then Exit;
  if (IsWeighted) and (Length(dyi) <> Length(yi)) then Exit;
  m := ApproxDegree;

  // weightes
  SetLength(wi, Length(yi));
  SetLength(ki, Length(yi));
  for i:=0 to High(yi) do
    if (IsWeighted) then
      if (dyi[i] > MinSingle) then
        wi[i] := 1/sqr(dyi[i])
      else
        wi[i] := MaxSingle
    else
      wi[i] := 1;

  if (IsWeighted and IsCorrectOnDens) then begin
    CorrectWeightsOnPointsDensity(xi, ki, 10);
    for i:=0 to Length(wi)-1 do
      wi[i] := wi[i] * ki[i];
  end;

  (* creates matrices for least squares
     Aij= Sum(i) (x_i^j * x_i^k)
     Bk= Sum(i) (y_i * x_i^k)  *)

  SetLength(B, m+1);
  SetLength(C, m+1, m+1);
  SetLength(Coeffs, m+1);

  for k:=0 to m do
  begin
    B[k]:=0;
    for i:=0 to High(yi) do
      B[k]:= B[k] +  wi[i]*yi[i]*IntPower(xi[i],k);
    for j:=0 to m do
    begin
      C[j,k]:=0;
      for i:=0 to High(xi) do
        C[j,k]:= C[j,k]+ wi[i]*IntPower(xi[i],j)*IntPower(xi[i],k);
    end;
  end;

  Chol(C, B, Coeffs, m+1);

  // errors for linear MNK
  if ((ApproxDegree = 1) and (not IsWeighted)) then
  begin
    LinearMNKError(Coeffs[1], Coeffs[0], xi, yi, dA1, dA0);
    if (Coeffs[0] <> 0) then
      dA0 := dA0 / Coeffs[0];
    if (Coeffs[1] <> 0) then
      dA1 := dA1 / Coeffs[1];
  end
  else begin
    dA0 := 0;
    dA1 := 0;
  end;

  Result := True;
end;

procedure CorrectWeightsOnPointsDensity(const xi: array of Double; var wi: Matrix.TVector; NZones: Integer);
var zoneLength, zoneWeight, step, leftBorder, rightBorder: Double;
    Np, i: Integer;
  function calcPointsInZone(const xi: array of Double): Integer;
  var j: Integer;
  begin
    Result := 0;
    for j:=0 to Np-1 do
      if (xi[j] > leftBorder) and (xi[j] <= rightBorder) then // last point must be included
        Inc(Result);
  end;
  procedure addWeightToPoints(const xi: array of Double; var wi: Matrix.TVector);
  var j: Integer;
  begin
    for j:=0 to Np-1 do
      if (xi[j] > leftBorder) and (xi[j] <= rightBorder) then
        wi[j] := wi[j] + zoneWeight;
  end;
  procedure zeroWeights();
  var j: Integer;
  begin
    for j:=0 to Np-1 do
      wi[j] := 0;
  end;
begin
  Np := Length(xi);
  zoneLength := xi[Np-1] / NZones;
  step := zoneLength/2;
  zeroWeights(); // Is it needed?
  (* go through range and calculates number of points in each zones.
     Weight for every point in zone = 1/num points
     each shift is half of zone *)
  leftBorder := -step;
  rightBorder := step;
  for i := 0 to 2*NZones do  // each window position
  begin
    zoneWeight := calcPointsInZone(xi);
    if (zoneWeight <> 0) then zoneWeight := 1/zoneWeight;
    addWeightToPoints(xi, wi);
    leftBorder := leftBorder + step;
    rightBorder := rightBorder + step;
  end;
end;

procedure LinearMNKError(A1, A0: Double; const xi, yi: array of Double; var dA1, dA0: Double);
var delta, sum_x, sum_x2, sigma_y2: Double;
    //x_ave, y_ave, sigma_x, sigma_y: Double;
    i, n: Integer;
begin
  n := Length(xi);
  if (n <=2 ) then Exit;

  sum_x := 0; sum_x2 := 0;
  sigma_y2 := 0;

  for i:=0 to n-1 do
  begin
    sum_x := sum_x + xi[i];
    sum_x2 := sum_x2 + Sqr(xi[i]);
    sigma_y2 := sigma_y2 + Sqr(A1*xi[i] + A0 - yi[i]);
  end;

  delta := n*sum_x2 - Sqr(sum_x);
  if (delta = 0) then Exit;
  sigma_y2 := sigma_y2 / (n-2);

  dA1 := Sqrt(n * sigma_y2 / delta);
  dA0 := Sqrt(sum_x2 * sigma_y2 / delta);

  (*x_ave := 0; y_ave := 0;
  for i:=0 to n-1 do
  begin
    x_ave := x_ave + xi[i];
    y_ave := y_ave + yi[i];
  end;
  x_ave := x_ave/n;
  y_ave := y_ave/n;

  sigma_x := 0; sigma_y := 0;
  for i:=0 to n-1 do
  begin
    sigma_x := sigma_x + Sqr(xi[i] - x_ave);
    sigma_y := sigma_y + Sqr(yi[i] - y_ave);
  end;

  dA1 := Sqrt(1/(n-2) * (sigma_y/sigma_x - Sqr(A1)));
  dA0 := dA1 * Sqrt(Sqr(x_ave) + sigma_x/n); *)
end;

function Correlation(const xi, yi: array of Double): Double;
var i, n: Integer;
    x_ave, y_ave: Double;
    sigma_x, sigma_y, cov_xy: Double;
begin
  Result := 0;
  n := Length(xi);
  if (n <=1 ) then Exit;

  // average calculation
  x_ave := 0; y_ave := 0;
  for i:=0 to n-1 do
  begin
    x_ave := x_ave + xi[i];
    y_ave := y_ave + yi[i];
  end;
  x_ave := x_ave/n;
  y_ave := y_ave/n;

  // correlation r_xy = cov_xy / (sigma_x * sigma_y)
  sigma_x := 0; sigma_y := 0; cov_xy := 0;
  for i:=0 to n-1 do
  begin
    sigma_x := sigma_x + Sqr(xi[i] - x_ave);
    sigma_y := sigma_y + Sqr(yi[i] - y_ave);
    cov_xy := cov_xy + (xi[i] - x_ave)*(yi[i] - y_ave);
  end;
  if ((sigma_x = 0) or (sigma_y = 0)) then Exit;
  sigma_x := Sqrt(sigma_x);
  sigma_y := Sqrt(sigma_y);
  Result := cov_xy / (sigma_x * sigma_y); 
end;

{$IFDEF LSRM}
function MNK_dan(const xi, yi, dyi: array of Double; ApproxDegree: Integer; IsWeighted, IsCorrectOnDens: Boolean;
             out Coeffs: Matrix.TVector; out dA0, dA1: Double): Boolean;
var C: LMatrixDin.TMatrix;
    B, Koeffs, dKoeffs: LMatrixDin.TVector; 
    wi, ki: Matrix.TVector;
    i, j, m, k: Integer;
begin
  // check conditions
  Result:=False;
  if (Length(xi)<>Length(yi)) then Exit;
  if (Length(xi)<ApproxDegree+1) then Exit;
  if (ApproxDegree < 0) then Exit;
  if (IsWeighted) and (Length(dyi)<>Length(yi)) then Exit;
  m:=ApproxDegree;

  // weightes
  SetLength(wi, Length(yi));
  SetLength(ki, Length(yi));
  for i:=0 to High(yi) do
    if (IsWeighted) then
      if (dyi[i] > MinSingle) then
        wi[i] := 1/sqr(dyi[i])
      else
        wi[i] := MaxSingle
    else
      wi[i] := 1;

  if (IsWeighted and IsCorrectOnDens) then begin
    CorrectWeightsOnPointsDensity(xi, ki, 10);
    for i:=0 to Length(wi)-1 do
      wi[i] := wi[i] * ki[i];
  end;

  (* creates matrices for least squares
     Aij= Sum(i) (x_i^j * x_i^k)
     Bk= Sum(i) (y_i * x_i^k)  *)

  B := TVector.Create(m+1);
  Koeffs := TVector.Create(m+1);
  dKoeffs := TVector.Create(m+1);
  C := TMatrix.Create(m+2,m+1);
  (*SetLength(B,m+1);
  SetLength(C,m+1,m+1); *)
  SetLength(Coeffs,m+1);

  for k:=0 to m do
  begin
    B[k]:=0;
    for i:=0 to High(yi) do
      B[k]:= B[k] +  wi[i]*yi[i]*IntPower(xi[i],k);
    for j:=0 to m do
    begin
      C[j,k]:=0;
      for i:=0 to High(xi) do
        C[j,k]:=C[j,k]+ wi[i]*IntPower(xi[i],j)*IntPower(xi[i],k);
    end;
  end;

  Cholecky(C,B,Koeffs,dKoeffs,True);

  for i:=0 to m do
    Coeffs[i] := Koeffs[i];
    
  // errors for linear MNK
  dA1 := dKoeffs[1];
  dA0 := dKoeffs[0];

  C.Free;
  B.Free;
  Koeffs.Free;
  dKoeffs.Free;

  Result:=True;
end;
{$ENDIF}

end.
