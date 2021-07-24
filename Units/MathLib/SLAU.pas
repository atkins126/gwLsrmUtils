unit SLAU;

interface

uses Matrix;

// OLE solution
function Chol(const a: TMatrix; const b: array of Double; var x: TVector; M: Integer): Boolean; // Cholesky decomposition
function Gauss(const a: TMatrix; b: array of Double; var x: TVector; M: Integer): Boolean; // Gauss solution
function TwoEqSolving(const a: TMatrix; const b: array of Double; var x: TVector; M: Integer): Boolean; // system of 2 linear equations (for testing)

implementation

function Chol(const a: TMatrix; const b: array of Double; var x: TVector; M: Integer): Boolean;
var L: TMatrix;
    i,j,k: Integer;
    y: TVector;
begin
  //Result:=False;
  SetLength(L,M,M); // лева€ диагональна€ матрица, A=L*LT, A*x=b => L*y=b, LT*x=y

  // ѕолучение L -- левой диагональной матрицы
  for i:=0 to M-1 do
    for j:=0 to M-1 do
    begin
      if (j < i) then   // Lij=1/Lii*(Aij - Sum(k=1 to j-1)(Lik*Ljk))
      begin
        L[i,j] := 1/L[j,j] * A[i,j];
        for k:=0 to j-1 do
          L[i,j] := L[i,j] - 1/L[j,j] * L[i,k]*L[j,k];
      end
      else if (i = j) then     // Lii=sqrt(Aii - Sum(k=1 to i-1)(Lik^2))
      begin
        L[i,i] := A[i,i];
        for k:=0 to i-1 do
          L[i,i] := L[i,i] - L[i,k]*L[i,k];
        L[i,i] := Sqrt(L[i,i]);
      end;
    end;

    // решение системы  L*y=b
    SetLength(y, M);
    for i:=0 to M-1 do
    begin
      y[i]:= b[i]/L[i,i];
      for k:=0 to i-1 do
        y[i]:= y[i] - 1/L[i,i]*L[i,k]*y[k];   // yi = bi/Lii - 1/Lii*Sum(k=1 to i-1)(Lik*yk)
    end;

    // решение системы LT*x=y (обратный ход)
    for i:=M-1 downto 0 do
    begin
      x[i] := y[i]/L[i,i];
      for k:=M-1 downto i+1 do
        x[i] := x[i] - 1/L[i,i]*L[k,i]*x[k];
    end;

    Result:=True;
end;

function TwoEqSolving(const a: TMatrix; const b: array of Double; var x: TVector; M: Integer): Boolean;
var D0: Double;
begin
  Result := False;
  if M = 1 then
    if M <> 2 then Exit;
  D0 := a[0, 0] * a[1, 1] - a[0, 1] * a[1, 0];
  if D0 = 0 then Exit;
  x[0] := (b[0] * a[1, 1] - b[1] * a[0, 1]) / D0;
  x[1] := (b[1] * a[0, 0] - b[0] * a[1, 0]) / D0;
  Result := True;
end;

function Gauss(const a: TMatrix; b: array of Double; var x: TVector; M: Integer): Boolean;
var i,j,k: Integer;
    c: TMatrix;
    TempV: TVector;
begin
  //Result:=False;
  //if (det(A)=0) then Exit;

  SetLength(c,M,M);
  for i:=0 to M-1 do
    for j:=0 to M-1 do
      c[i,j]:=a[i,j];
  // приведение к треугольному виду -- пр€мой ход
  {ѕоиск максимального элемента
  перемещение его на i-е место
  вычитание из всех строк i-й, чтобы i-й у остальных обратилс€ в 0
  т.е. дл€ строки j: (i+1) -- (N-1) получаем дл€ элемента k= i+1 - (N-1): a[j,k]:=a[j,k]- a[j,i]/a[i,i]*a[i,k]
  коэффициент c[j,i]:= - a[j,i]/a[i,i] удобно записать в 0-левые элементы треугольной матрицы: a[j,i]:=c[j,i]
  свободный член b[j]:=b[j] + b[i]*a[j,i]}
  for i:=0 to M-2 do
  begin
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
            c[i,i]:=-c[i,i]
          end;
        c[j,i]:= -c[j,i]/c[i,i] // c можно заменить на a
      end;

      for k:=i+1 to M-1 do c[j,k]:=c[j,k] + c[j,i]*c[i,k];
      b[j]:=b[j] + c[j,i]*b[i];
    end;
  end;
  // решение обратным ходом
  {x[i]:=1/a[i,i]*(b[i] - Sum(a[i,j]*x[j]))}
  for i:=M-1 downto 0 do
  begin
    x[i]:=b[i];
    for j:=i+1 to M-1 do
      x[i]:=x[i]-c[i,j]*x[j];
    x[i]:=x[i]/c[i,i];
  end;

  c:=nil;
  TempV:=nil;
  Result:=True;
end;

end.
