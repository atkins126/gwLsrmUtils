unit MathLib;

interface

uses Matrix;

const
  PHYS_MIN = 5.0e-100;
  PHYS_MAX = 5.0e100;

  // average and Mean
function average(x, y: Double): Double; overload;
function average(const arr: array of Double): Double; overload;
procedure weightedMean(const A: array of Double; const Er: array of Double;
  var wMean, wError: Double; UseStDev: Boolean = False); // calculation weighted mean with error
procedure weightedMeanWeights(const A: array of Double; const weights: array of Double; var wMean, wError: Double);
function weightsCalculation(const err_arr: array of Double; var wj: array of Double): Boolean;
procedure addSystematicError(var dA: Double; const a_systematic: array of Double);
function RelErrorFromAbs(a, da: Double): Double;
function errorFromPropValue(a, da, b: Double): Double; // b = k*a; rel. errors are equal: db = da / a * b
function sqrtAdd(a, b: Double): Double; overload; // quadratic sum for errors
function sqrtAdd(const arr: array of Double): Double; overload;
  // Min and Max
function MinValueIndex(const Data: array of Double): Integer;
function FindClose(AValue: Int64; const Int64Arr: array of Int64; BeginIndex: Integer = 0): Integer; overload;  // closest element
function FindClose(AValue: Double; const Arr: array of Double; BeginIndex: Integer = 0): Integer; overload;
function FindCloseMin(AValue: Int64; const Int64Arr: array of Int64; BeginIndex: Integer = -1): Integer; // find closest element for sorted array from index i: Arr[i]<AValue
function localChiMin2vars(const A, B, AChi: array of Double; var A0, B0: double): Boolean;  // find value with Chi minimum
function localChiMin2D(const a, b: TDoubleVector; const chi: TDoubleMatrix; var a0, b0: Double): Boolean; // find value with Chi minimum on 2D matrix
function minParabola(x1, x2, x3, y1, y2, y3: Double; var x0: Double): Boolean; overload; // search parabole vertex
function minParabola(x1, x2, x3, y1, y2, y3: Double; var a, b, c: Double): Boolean; overload;
  // statistic
function Poisson(mu: Double): Integer;


function MaxInt16Value(const Data: array of SmallInt): SmallInt;

  // equation solver
function SquareEquationSolver(a, b, c: Double; var x1, x2: Double): Boolean;

implementation

uses Math, SLAU, gwAlgorithm;

function average(x, y: Double): Double;
begin
  Result := (X + Y) / 2;
end;

function average(const arr: array of Double): Double;
var i: Integer;
begin
  Result := 0;
  for i:=Low(arr) to High(arr) do
    Result := Result + arr[i];
  Result := Result/(Length(arr) - Low(arr));
end;

procedure weightedMean(const A: array of Double; const Er: array of Double;
  var wMean, wError: Double; UseStDev: Boolean = False);
var i: Integer;
    weight, weightSum, stanDev: Double;
begin
  wMean := 0; wError := 0;
  weightSum := 0;
  {FORMULA: <x> = Sum(wi*xi)/Sum(wi), Error= 1/Sum(wi)}
  for i := Low(A) to High(A) do
  begin
    if (Er[i] <> 0) then
      weight := 1/sqr(Er[i]) // weight calculating
    else
      weight := MaxSingle;
    wMean := wMean + A[i]*weight;
    weightSum := weightSum + weight;
  end;

  wMean := wMean/weightSum;
  wError := Sqrt(1/weightSum);

  {FORMULA for additional error: dA = sqrt(Sum(wi*(A_ave - Ai))) / sqrt((n-1)*Sum(wi))}
  if (Length(A) > 1) then begin
    stanDev := 0;
    for i := Low(A) to High(A) do begin
      weight := 1/sqr(Er[i]);
      stanDev := stanDev + weight * Sqr(A[i] - wMean);
    end;
    stanDev := Sqrt(stanDev) / Sqrt(weightSum * (Length(A)-1));
    // use max from 2 this errors for A
    wError := Max(wError, stanDev);
  end;
end;

procedure weightedMeanWeights(const A: array of Double; const weights: array of Double; var wMean, wError: Double);
const N_SIGMA = 5;
var i, tmpCounter: Integer;
    oldWMean, weightSum, threshold, stanDev: Double;
begin
  wMean := 0; wError := 0;
  if (Length(A) = 0) or (Length(weights) = 0) then
    Exit;
  oldWMean := 0;
  weightSum:=0;
  {FORMULA: <x> = Sum(wi*xi)/Sum(wi), Error= 1/Sum(wi)}
  for i:=Low(A) to High(A) do
  begin
    oldWMean := oldWMean + A[i]*weights[i];
    weightSum := weightSum + weights[i];
  end;

  oldWMean := oldWMean/weightSum;
  wError := Sqrt(1/weightSum);

  {FORMULA for additional error: dA = sqrt(Sum(wi*(A_ave - Ai))) / sqrt((n-1)*Sum(wi))}
  if (Length(A) > 1) then begin
    stanDev := 0;
    for i := Low(A) to High(A) do begin
      stanDev := stanDev + weights[i] * Sqr(A[i] - oldWMean);
    end;
    // use max from 2 this errors for A
    stanDev := Sqrt(stanDev) / Sqrt(weightSum * (Length(A) - 1));
    wError := Max(wError, stanDev);
  end;

  // set threshold
  threshold := N_SIGMA * wError;
  weightSum := 0;

  // 2-nd run and throw out bad results
  for i:=Low(A) to High(A) do
    if (Abs(A[i] - oldWMean) <= threshold) then begin
      wMean := wMean + A[i]*weights[i];
      weightSum := weightSum + weights[i];
    end;

  if (weightSum > minDouble) then begin
    wMean := wMean/weightSum;
    wError := Sqrt(1/weightSum);
  end
  else
    wMean := oldWMean;

  {FORMULA for additional error: dA = sqrt(Sum(wi*(A_ave - Ai))) / sqrt((n-1)*Sum(wi))}
  stanDev := 0;
  tmpCounter := 0;
  for i := Low(A) to High(A) do
    if (Abs(A[i] - oldWMean) <= threshold) then begin
      stanDev := stanDev + weights[i] * Sqr(A[i] - wMean);
      Inc(tmpCounter);
    end;
  if (tmpCounter > 1) then
    stanDev := Sqrt(stanDev) / Sqrt(weightSum * (tmpCounter - 1))
  else
    stanDev := 0;
  // use max from 2 this errors for A
  wError := Max(wError, stanDev);
end;

function MinValueIndex(const Data: array of Double): Integer;
var
  I: Integer;
  AValue: Double;
begin
  Result := Low(Data);
  AValue := Data[Result];
  for I := Low(Data) + 1 to High(Data) do
    if AValue > Data[I] then begin
      Result := I;
      AValue := Data[I];
    end;
end;

function FindClose(AValue: Int64; const Int64Arr: array of Int64; BeginIndex: Integer = 0): Integer;
var i: Integer;
    diff1, diff2: int64;
begin
  if (Length(Int64Arr)<1) then
  begin
    Result:=-1;
    Exit;
  end;
  Result:= Low(Int64Arr);
  diff1:=Abs(Int64arr[Low(Int64Arr)]-AValue);
  for i:=Low(Int64Arr)+1 to High(Int64Arr) do
  begin
    diff2:=Abs(Int64arr[i]-AValue);
    if (diff1>diff2) then
    begin
      diff1:=diff2;
      Result:=i;
    end;
  end;
end;

function FindClose(AValue: Double; const Arr: array of Double; BeginIndex: Integer = 0): Integer;
var i, j: Integer;
    diff1, diff2: Double;
begin
  if (Length(Arr)<1) then
  begin
    Result:=-1;
    Exit;
  end;
  if (BeginIndex=0) then
    j:=Low(Arr)
  else
    j:=Min(BeginIndex,High(Arr)-1);
  Result:= Low(Arr);
  diff1:=Abs(arr[j]-AValue);
  for i:=j+1 to High(Arr) do
  begin
    diff2:=Abs(arr[i]-AValue);
    if (diff1>diff2) then
    begin
      diff1:=diff2;
      Result:=i;
    end;
  end;
end;
  
function FindCloseMin(AValue: Int64; const Int64Arr: array of Int64; BeginIndex: Integer = -1): Integer;
var i: Integer;
begin
  if (BeginIndex=-1) then
    i:=Low(Int64Arr)
  else
    i:=Min(BeginIndex,High(Int64Arr));
  While (Int64arr[i]<AValue) do
  begin
    Inc(i);
    if (i>High(Int64Arr)-1) then Break;
  end;
  Result:=Max(i-1,Low(Int64Arr));
end;

function MaxInt16Value(const Data: array of SmallInt): SmallInt;
var
  I: Integer;
begin
  Result := Data[Low(Data)];
  for I := Low(Data) + 1 to High(Data) do
    if Result < Data[I] then
      Result := Data[I]
end;

function SquareEquationSolver(a, b, c: Double; var x1, x2: Double): Boolean;
var D: Double;
begin
  Result := False;
  if (a = 0) and (b = 0) then Exit; // not equation
  D := Sqr(b) - 4*a*c;
  if (D<0) then Exit; // there is not real solutions
  if (a=0) then begin
    x1 := -c/b;
    x2 := -c/b;
    Exit;
  end;
  x1 := (-b + Sqrt(D))/(2*a);
  x2 := (-b - Sqrt(D))/(2*a);
  Result := True;
end;

function localChiMin2vars(const A, B, AChi: array of Double; var A0, B0: Double): Boolean;
var i: Integer;
begin
  Result := False;
  i := MinValueIndex(AChi);
  if (i = Low(A)) or (i = High(A)) then
    Exit;

  MinParabola(A[i-1], A[i], A[i+1], AChi[i-1], AChi[i], AChi[i+1], A0);
  MinParabola(B[i-1], B[i], B[i+1], AChi[i-1], AChi[i], AChi[i+1], B0);
  Result := True;
end;

function localChiMin2D(const a, b: TDoubleVector; const chi: TDoubleMatrix; var a0, b0: Double): Boolean;
var i, j: Integer;
    minCol, minRow: TDoubleVector;
begin
  (**
  - find minimal chi2 from matrix: a - row, b - col
  - find min from parabola for row -> a0
  - find min from parabola for col -> b0
  *)
  if (not findMinMtrx(chi, i, j)) or (i = 0) or (j = 0) or (i = chi.size1-1) or
    (j = chi.size2) then begin
    Result := False;
    Exit;
  end;
  minRow := chi.getRow(i);
  minCol := chi.getColumn(j);
  minParabola(a[i-1], a[i], a[i+1], minCol[i-1], minCol[i], minCol[i+1], a0);
  minParabola(b[j-1], b[j], b[j+1], minRow[j-1], minRow[j], minRow[j+1], b0);
  minRow.Free();
  minCol.Free();
  Result := True;
end;

function minParabola(x1, x2, x3, y1, y2, y3: Double; var x0: Double): Boolean;
var z1, z2, r1, r2: Double;
    a, b: Double;
begin
  Result := True;
  // change coordinates to have (0,0)
  z1 := x1 - x2;
  z2 := x3 - x2;
  r1 := y1 - y2;
  r2 := y3 - y2;
  // coeffs calculation for y = a*x^2 + b*x
  a := (r2*z1 - r1*z2) / (z2*z1*(z2 - z1));
  b := (r1 - a*z1*z1) / z1;
  // vertex calculation
  x0 := -b/(2*a);
  x0 := x0 + x2;
end;

function minParabola(x1, x2, x3, y1, y2, y3: Double; var a, b, c: Double): Boolean;
var z1, z2, r1, r2: Double;
begin
  Result := True;
  // change coordinates to have (0,0): r = y - y2, z = x - x2
  z1 := x1 - x2;
  z2 := x3 - x2;
  r1 := y1 - y2;
  r2 := y3 - y2;
  // coeffs calculation for r = a*z^2 + b*z
  a := (r2*z1 - r1*z2) / (z2*z1*(z2 - z1));
  b := (r1 - a*z1*z1) / z1;
  // revert coordinates to x, y
  b := b - 2*a*x2;
  c := a*x2*x2 - b*x2 + y2;
end;

function weightsCalculation(const err_arr: array of Double; var wj: array of Double): Boolean;
var i: Integer;
begin
  Result := False;
  if (Length(err_arr) <> Length(wj)) then
    Exit;

  for i:=0 to Length(err_arr)-1 do
    if (err_arr[i] <> 0) then
      wj[i] := 1 / Sqr(err_arr[i])
    else
      wj[i] := 0;
  Result := True;
end;

procedure addSystematicError(var dA: Double; const a_systematic: array of Double);
begin
  dA := Sqrt(Sqr(dA) + Sqr(MinValue(a_systematic)));
end;

function RelErrorFromAbs(a, da: Double): Double;
begin
  if (Abs(a) > MinDouble) then
    Result := da / a
  else
    Result := 1;
end;

function errorFromPropValue(a, da, b: Double): Double;
begin
  if (Abs(a) > PHYS_MIN) then
    Result := da / a * b
  else
    Result := 0;
end;

function sqrtAdd(a, b: Double): Double; overload;
begin
  Result := Sqrt(Sqr(a) + Sqr(b));
end;

function sqrtAdd(const arr: array of Double): Double; overload;
var i: Integer;
begin
  Result := 0;
  if (Length(arr) = 0) then Exit;

  for i:=Low(arr) to High(arr) do
    Result := Result + Sqr(arr[i]);
  Result := Sqrt(Result);
end;

function Poisson(mu: Double): Integer;
var gamma, prob1, prob2, probSum: Double;
    i, N: Integer;
begin
  // formula p(i) = (mu)^i / i! * exp(-mu)
  gamma := Random;
  prob1 := 1;
  prob2 := Exp(-mu);
  probSum := prob2;
  
  // check for zero counts
  N := Trunc(100*mu);
  if ((probSum > gamma) or (N = 0)) then begin
    Result := 0;
    exit;
  end;
  
  // check for non-zero counts
  for i:=1 to N do begin
    prob1 := prob1 * mu / i;
    probSum := probSum + prob1 * prob2;
    if (probSum > gamma) then begin
      Break;
    end;
  end;

  Result := i;  
end;


end.
