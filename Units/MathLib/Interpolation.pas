unit Interpolation;

interface

uses Math, Matrix;

type TLogState = (lsNone, lsXonly, lsYonly, lsXYBoth);

function InterPol(nPoly: Integer; Xi,Yi: array of Double; X: Double; var Y: Double): Boolean;
function InterPolLog(nPoly: Integer; Xi,Yi: array of Double; X: Double; var Y: Double; XorYLog: TLogState): Boolean;
function Spline(const Xi,Yi: TVector; x: Double; var y: Double): Boolean; // функция строит сплайн 3-й степени по сетке и рассчитывает интерполированное значение ф-ии y(x) для заданного значения x, заданной по сетке {xi,yi} и возвращает true если расчёт удачен
function SplineLog(Xi,Yi: TVector; X: Double; var Y: Double; XorYLog: TLogState): Boolean;

implementation

uses SLAU, gwAlgorithm;

function InterPol(nPoly: Integer; Xi,Yi: array of Double; X: Double; var Y: Double): Boolean;
var i, j, li: Integer;
    A: TMatrix;
    b, Coeffs: TVector;
begin
  // проверка входных данных
  Result := False;
  if nPoly < 0 then Exit;
  if ((nPoly + 1) > Length(Xi)) or ((nPoly + 1) > Length(Yi)) then
    Exit;

  if (Odd(nPoly)) then
    li := low_index(Xi, X) // для нечётных нижняя граница Xi: Xi<=X<Xi+1, либо X<=Xi и равное число точек вправо больше на 1
  else
    li := NearIndex(Xi, X); // для чётных выбирается Xi: Xi наиболее близкий к X и равное число точек вправо и влево
  // отступ влево для того, чтобы провести полином и x был посередине (+корректировка на края)
  li := li - (nPoly div 2);
  if li < Low(Xi) then
    li := Low(Xi);
  if (li + nPoly) > High(Xi) then
    li := High(Xi) - nPoly;

  // Составление СЛАУ для интерполяции yi = Sum(j: from 0 to n) (Coeff_j*xi^j). A
  SetLength(A, nPoly + 1, nPoly + 1);
  SetLength(b, nPoly + 1);
  SetLength(Coeffs, nPoly + 1);
  // заполнение массива
  for i := 0 to nPoly do
  begin
    A[i, 0] := 1;
    for j := 1 to nPoly do
      A[i, j] := A[i, j - 1] * Xi[li + i];   // реккурентная формула для A[i,j]:=IntPower(Xi[li+i],j)
    b[i] := Yi[li + i];
  end;
  // решение СЛАУ
  Gauss(A, b, Coeffs, nPoly + 1);
  Y := Poly(X, Coeffs);
  Result := True;
end;

function InterPolLog(nPoly: Integer; Xi,Yi: array of double; X: Double; var Y: Double; XorYLog: TLogState): Boolean;
var i: Integer;
begin
  Result:=False;
  case XorYLog of
  lsNone: Result:=InterPol(nPoly, Xi, Yi, X, Y);
  lsXonly:  begin
        for i:=Low(Xi) to High(Xi) do
          Xi[i]:=ln(Xi[i]);
        Result:=InterPol(nPoly, Xi, Yi, ln(X), Y);
      end;
  lsYonly:  begin
        for i:=Low(Yi) to High(Yi) do
          if Yi[i]>0 then Yi[i]:=ln(Yi[i]) else Yi[i]:=1e9;
        Result:=InterPol(nPoly, Xi, Yi, X, Y);
        Y:=Exp(Y);
      end;
  lsXYBoth:  begin
        for i:=Low(Yi) to High(Yi) do
        begin
          Xi[i]:=ln(Xi[i]);
          if Yi[i]>0 then Yi[i]:=ln(Yi[i]) else Yi[i]:=1e9;
        end;
        Result:=InterPol(nPoly, Xi, Yi, ln(X), Y);
        Y:=Exp(Y);
      end;
  end;
end;

function Spline(const Xi, Yi: TVector; x: Double; var y: Double): Boolean;
var hi: TVector;
    i, j, N: Integer;
    A: TMatrix;
    b, Coeffs: TVector;
    Ci, Ai, Bi, Di: TVector; // коэффициенты полинома 3-й степени для сплайна f(x)= ai + bi(x-xi-1) + ci(x-xi-1)^2+ di(x-xi-1)^3, решается относительно ci
begin
  // проверка входных данных
  Result := False;
  if (Length(Xi) <> Length(Yi)) then Exit;

  // задание массива шагов сетки
  N := Length(Xi)-1; // N - число промежутков между точками, N+1 - число точек
  SetLength(hi, N);
  for i:=0 to N-1 do begin
    hi[i] := Xi[i+1] - Xi[i];
  end;

  // заполнение матриц для СЛАУ
  SetLength(A,N-1,N-1);
  SetLength(b,N-1);
  SetLength(Ci,N+1); // размерность на 1 больше для удобства записи краевых условий
  SetLength(Ai,N);
  SetLength(Bi,N);
  SetLength(Di,N);

  // предпоследняя и последняя строчка граничные условия
  (*{N-1:}  A[N-1,0]:=1; // C1=0
          b[N-1]:=0;
  {N:}    A[N,N]:=1; // Cn+1=0
          b[N]:=0;
  for j:=0 to N do
  begin
    if (j<>0) and (j<>N) then
    begin
      A[N-1,j]:=0;
      A[N,j]:=0
    end
    else if (j=0) then A[N,j]:=0
    else if (j=N) then A[N-1,j]:=0;
  end; *)
  // остальная часть матрицы i - строка, j - столбец, здесь A[i,0] соответствует С[1], т.е. индекс С смещен на 1 
  for i:=0 to N-2 do
  begin
    for j:=0 to N-2 do
    begin
      if (j = i-1) then A[i,j] := hi[i]
      else if (j = i) then A[i,j] := 2*(hi[i]+hi[i+1])
      else if (j = i+1) then A[i,j] := hi[i+1]
      else A[i,j] := 0;
    end;
  end;
  // правая часть
  for i:=0 to N-2 do
    b[i]:=3*((Yi[i+2] - Yi[i+1])/hi[i+1] - (Yi[i+1] - Yi[i])/hi[i]); // к индексу Yi добавлена 1 относительно формулы в Самарском, т.к. нумерация b, h и A должна начинаться с 1, а Yi с 0

  // решение СЛАУ относительно ci
  Gauss(A, b, Ci ,N-1);

  // возвращение индекса C на место
  Ci[N] := 0; // краевое условие
  for i := N-1 downto 1 do
    Ci[i] := Ci[i-1];
  Ci[0] := 0;

  // вычисление остальных коэффициентов
  for i:=0 to N-1 do
  begin
    Ai[i]:= Yi[i]; // индекс у Yi увеличен на 1
    Di[i]:= (Ci[i+1] - Ci[i])/(3*hi[i]);
    Bi[i]:= (Yi[i+1] - Yi[i])/hi[i] - hi[i]/3*(Ci[i+1] + 2* Ci[i]);
  end;

  {вычисление значения в точке x}
  i := low_index(Xi, x); // i: x[i] < x < x[i+1]
  if (i >= N) then i := N-1;
  // нахождение коэффициентов для полинома для этого участка
  SetLength(Coeffs, 4);
  Coeffs[0] := Ai[i];
  Coeffs[1] := Bi[i];
  Coeffs[2] := Ci[i];
  Coeffs[3] := Di[i];
  // расчёт значения интерполированной функции в т. X
  y := Poly(x-Xi[i], Coeffs);
  Result := True;
end;

function SplineLog(Xi,Yi: TVector; X: Double; var Y: Double; XorYLog: TLogState): Boolean;
var i: Integer;
begin
  Result := False;
  // check boundary cases:
  if (x <= 0) then begin
    if (XorYLog = lsXonly) then XorYLog := lsNone;
    if (XorYLog = lsXYBoth) then XorYLog := lsYonly;
  end;
  if (y <= 0) then begin
    if (XorYLog = lsYonly) then XorYLog := lsNone;
    if (XorYLog = lsXYBoth) then XorYLog := lsXonly;
  end;

  case XorYLog of
  lsNone: Result := Spline(Xi, Yi, X, Y);
  lsXonly:  begin
        for i := Low(Xi) to High(Xi) do begin
          Assert(Xi[i] > 0);
          Xi[i] := ln(Xi[i]);
        end;
        Result := Spline(Xi, Yi, ln(X), Y);
      end;
  lsYonly:  begin
        for i := Low(Yi) to High(Yi) do
          if (Yi[i] > 0) then Yi[i] := ln(Yi[i]) else Yi[i] := 1e9;
        Result := Spline(Xi, Yi, X, Y);
        Y := Exp(Y);
      end;
  lsXYBoth:  begin
        for i := Low(Yi) to High(Yi) do
        begin
          Assert(Xi[i] > 0);
          Xi[i] := ln(Xi[i]);
          if (Yi[i] > 0) then Yi[i] := ln(Yi[i]) else Yi[i] := 1e9;
        end;
        Result := Spline(Xi, Yi, ln(X), Y);
        Y := Exp(Y);
      end;
  end;
end;

end.
