unit gwAlgorithm;

interface

uses Matrix;

  // double array operations
  function FindMax(const b: array of Double): Integer; // returns max element index
  function low_index(const A: array of Double; V: Double): Integer; // return index of closest lower element
  function low_bound(const A: array of Double; V: Double): Integer; // lower bound element >= v
  function upper_bound(const A: array of Double; V: Double): Integer; // upper_bound element >= v
  function high_index(const A: array of Double; V: Double): Integer; // upper_bound element >= v
  function NearIndex(const A: array of Double; V: Double): Integer; // closest element
  function CopyVector(a: TVector): TVector;
  //операции с матрицей
  function IsSquare(const A: TMatrix): Boolean; // is matrix square
  function Determinant(const A: TMatrix): Double; // calculates matrix determinant
  procedure ZeroVector(var V: TVector);
  procedure ZeroMatrix(var A: TMatrix);
  function findMinMtrx(const mtrx: TDoubleMatrix; var row_min, col_min: Integer): Boolean;
  // find
  function find_dbl_vector(vctr: TDoubleVector; aValue: Double; tolerance: Double): Integer;

implementation

uses Math;

function IsSquare(const A: TMatrix): Boolean;
begin
  Result := False;
  if (Length(A) > 0) then
    Result := (Length(A) = Length(A[0]));
end;

function Determinant(const A: TMatrix): Double;
var i,j,k, M: Integer;
    c: TMatrix;
    TempV: TVector;
begin
  Result:=0;
  if IsSquare(A) then
  begin
    M:=Length(A);
    if M=1 then
    begin
      Result:=A[0,0];
      Exit;
    end;
    SetLength(c,M,M);
    for i:=0 to M-1 do
      for j:=0 to M-1 do
        c[i,j]:=a[i,j];
    // приведение к треугольному виду
    {Поиск максимального элемента
    перемещение его на i-е место
    вычитание из всех строк i-й, чтобы i-й у остальных обратился в 0
    т.е. для строки j: (i+1) -- (N-1) получаем для элемента k= i+1 - (N-1): a[j,k]:=a[j,k]- a[j,i]/a[i,i]*a[i,k]
    коэффициент c[j,i]:= - a[j,i]/a[i,i] удобно записать в 0-левые элементы треугольной матрицы: a[j,i]:=c[j,i]
    свободный член b[j]:=b[j] + b[i]*a[j,i]}
    for i:=0 to M-2 do
    begin
      {TODO -o gw: вставить замену строки на ненулевой элемент}
      //FindMax(a?);
      for j:=i+1 to M-1 do
      begin
        if (c[i,i]<>0) then
          c[j,i]:= -c[j,i]/c[i,i] // c можно заменить на a
        else
        begin
          SetLength(TempV,M-i);
          for k:=i+1 to M-1 do
            if (c[k,i]<>0) then
            begin
              TempV:=c[k];
              c[k]:=c[i];
              c[i]:=TempV;
              c[i,i]:=-c[i,i];
            end;
          c[j,i]:= -c[j,i]/c[i,i] // c можно заменить на a
        end;

        for k:=i+1 to M-1 do c[j,k]:=c[j,k] + c[j,i]*c[i,k];
      end;
    end;
    // вычисление детерминанта
    Result:=1;
    for i:=0 to M-1 do
      Result:=Result*c[i,i];
  end;
  TempV:=nil;
  c:=nil;
end;

function FindMax(const b: array of Double): Integer; // возвращает индекс максимального элемента в столбце
var i: Integer;
    M: Double;
begin
  Result := -1;
  if (Length(b) = 0) then Exit;
  M := b[Low(b)];
  for i:=Low(b)+1 to High(b) do
    if (M < b[i]) then
    begin
      M := b[i];
      Result := i;
    end;
end;

function low_index(const A: array of Double; V: Double): Integer;
var i: Integer;
begin
  Result := Low(A);

  for i := Low(A) to High(A) do
  begin
    if A[i] <= V then
      Result := i
    else
      Exit;
  end;
end;

function low_bound(const A: array of Double; V: Double): Integer;
var i: Integer;
begin
  Result := -1;

  for i := Low(A) to High(A) do
  begin
    if A[i] < V then Continue
    else begin
      Result := i;
      Exit;
    end;
  end;
end;

function upper_bound(const A: array of Double; V: Double): Integer;
var i: Integer;
begin
  Result := -1;

  for i := Low(A) to High(A) do
  begin
    if A[i] <= V then Continue
    else begin
      Result := i;
      Exit;
    end;
  end;
end;

function high_index(const A: array of Double; V: Double): Integer;
var i: Integer;
begin
  Result := High(A);

  for i := High(A) downto Low(A) do
  begin
    if A[i] > V then
      Result := i
    else
      Exit;
  end;
end;

function NearIndex(const A: array of Double; V: Double): Integer;
var i: Integer;
begin
  Result := low_index(A, V);
  if Result < High(A) then
  begin
    i := Max(0, Result + sign(V - A[Result])); // следующий либо предыдущий член массива A, такой что V будет между этими членами массива
    if Abs(A[i] - V) < Abs(A[Result] - V) then
      Result := i;
  end;
end;

procedure ZeroVector(var V: TVector);
var i: Integer;
begin
  if (Length(V) < 0) then
    Exit;

  for i:=Low(V) to High(V) do
    V[i] := 0;
end;

procedure ZeroMatrix(var A: TMatrix);
var i, j: Integer;
begin
  if (Length(A) < 0) then
    Exit;

  for i:=Low(A) to High(A) do
    for j:=Low(A[i]) to High(A[i]) do
      A[i,j] := 0;
end;

function CopyVector(a: TVector): TVector;
var i: Integer;
begin
  SetLength(Result, Length(a));
  {$R-}
  for i:=Low(a) to High(a) do
  begin
    Result[i] := a[i];
  end;
  {$R+}
end;

function findMinMtrx(const mtrx: TDoubleMatrix; var row_min, col_min: Integer): Boolean;
var i,j: Integer;
    minValue: Double;
begin
  minValue := 1e10;
  for i:=0 to mtrx.size1-1 do
    for j:=0 to mtrx.size2-1 do
      if (mtrx.getElement(i, j) < minValue) then begin
        row_min := i;
        col_min := j;
        minValue := mtrx.getElement(i, j);
      end;
  if (minValue < 1e10) then
    Result := True
  else
    Result := False;
end;

function find_dbl_vector(vctr: TDoubleVector; aValue: Double; tolerance: Double): Integer;
var i: Integer;
begin
  Result := -1;
  for i:=0 to vctr.size-1 do
    if (Abs(vctr[i] - aValue) <= tolerance) then
    begin
      Result := i;
      Exit;
    end;
end;

end.
