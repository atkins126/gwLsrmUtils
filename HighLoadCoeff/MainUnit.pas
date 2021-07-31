unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, Math, JvJCLUtils, Matrix;

const
  NUMBEROFMEAS = 3;

type
  TfrmMain = class(TForm)
    strngrd1: TStringGrid;
    btnCalc: TButton;
    lbl1: TLabel;
    btnShowSLAU: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnCalcClick(Sender: TObject);
    procedure btnShowSLAUClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure strngrd1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    { Private declarations }
    procedure LoadInputFile(FileName: string);
  public
    { Public declarations }
    function ReadValues: Boolean; // чтение значений из таблицы
    procedure ShowResults; // вывод результатов
    procedure CreateSLAU; // процедура создания СЛАУ
  end;

var
  frmMain: TfrmMain;
  n: array of Double; // значения скоростей счёта
  SumY: array of Double; // значения сумм отсчётов для спектров
  SumYi: array of Double; // значение суммы отсчётов, умноженных на канал, для спектра
  A: TMatrix;
  x, dx, b: TVector;
  NMeas: Integer = NUMBEROFMEAS;
  IsCalculated: Boolean = False; // решена ли система уравнений
  AppPath: string;

resourcestring
  rsError1 = 'Wrong values!';
  rsError2 = 'Error in equations solving';

implementation

{$R *.dfm}

uses gwAlgorithm, LeastSquares, SLAU;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  if DecimalSeparator<>'.' then DecimalSeparator:='.';
  AppPath:=ExtractFilePath(ParamStr(0));
  // названия
  strngrd1.Cells[1,0]:='n, cps';
  strngrd1.Cells[2,0]:='r=1/t*Sum(yi)';
  strngrd1.Cells[3,0]:='p=1/t*Sum(yi*i)';
  strngrd1.Cells[4,0]:='RT, s';
  strngrd1.Cells[0,1]:='0: w/o load';
  strngrd1.Cells[0,2]:='1: with load1';
  strngrd1.Cells[0,3]:='2: with load2';
  strngrd1.Cells[0,4]:='---';
  // значения
  strngrd1.Cells[1,1]:='90.8';
  strngrd1.Cells[1,2]:='82.3';
  strngrd1.Cells[1,3]:='79.3';
  strngrd1.Cells[2,2]:='1.0429e4';
  strngrd1.Cells[2,3]:='1.0669e4';
  strngrd1.Cells[3,2]:='1.181e7';
  strngrd1.Cells[3,3]:='6.352e6';
  strngrd1.ColWidths[3]:=74;

  if ParamStr(1)<> '' then LoadInputFile(AppPath+ParamStr(1));
end;

procedure TfrmMain.btnCalcClick(Sender: TObject);
var xi: TVector;
    i: Integer;
begin
  if ReadValues then
  begin
    CreateSLAU;
    if IsSquare(A) and (Abs(Determinant(A))>1e-10) then
      IsCalculated:=Gauss(A,b,x,Length(b))
    else if (Length(A[0])=1) then                 // для переопределённой СЛАУ
    begin
      SetLength(xi, Length(A));
      for i:=0 to Length(A)-1 do
        xi[i]:=A[i,0];
      IsCalculated:= OLS_StraightLineProp(xi, b,x[0], dx[0]);
    end
    else IsCalculated:=False;

    ShowResults;
  end
  else ShowMessage(rsError1);
end;

function TfrmMain.ReadValues: Boolean;
var i: Integer;
begin
  Result:=False;
  SetLength(n, NUMBEROFMEAS); // длина массива скоростей счёта
  SetLength(SumY, NUMBEROFMEAS); // длина массива сумм отсчётов
  SetLength(SumYi, NUMBEROFMEAS); // длина массива сумм отсчёта*канал

  try
    n[0]:=StrToFloat(strngrd1.Cells[1,1]); // считывание незагруженной скорости счёта
    for i:=1 to strngrd1.RowCount-2 do
    begin
      // Увеличием длину массивов, если нужно
      if (i+1>NUMBEROFMEAS) or (i+1>Length(n)) then
      begin
        SetLength(n, i+1);
        SetLength(SumY, i+1);
        SetLength(SumYi, i+1);
      end;
      // считываем значения
      n[i]:=StrToFloatDef(strngrd1.Cells[1,i+1],0);
      SumY[i]:=StrToFloatDef(strngrd1.Cells[2,i+1],0);
      SumYi[i]:=StrToFloatDef(strngrd1.Cells[3,i+1],0);
      // проверяем не нулевые ли они
      if (n[i]<=0) or (SumY[i]<=0) then
      begin
        SetLength(n, i);
        SetLength(SumY, i);
        SetLength(SumYi, i);
        Break; {TODO: убрать после правки расчёта детерминанта и метода Гаусса сделать смешение строки}
      end;
    end;
  except
    n:=nil; SumY:=nil; SumYi:=nil;
  end;
  if Length(n)>1 then Result:=True;
end;

procedure TfrmMain.CreateSLAU; // создание СЛАУ
var i, EqLength, ParamLength: Integer;
    IsZero: Boolean;
begin
  EqLength:=Length(n)-1;
  {TODO: временное решение}
  //if EqLength>2 then EqLength:=2;
  ParamLength:=Min(2,EqLength);

  SetLength(A,EqLength,ParamLength);
  SetLength(x,ParamLength);
  SetLength(dx,ParamLength);
  SetLength(b,EqLength);

  for i:=1 to EqLength do
  begin
    b[i-1]:=1-n[i]/n[0];
    A[i-1,0]:=SumY[i];
    if (ParamLength>1) then A[i-1,1]:=SumYi[i];
  end;

  if EqLength<=0 then Exit;

  // проверка на 0-й столбец Sumi
  IsZero:=True;
  for i:=0 to EqLength-1 do IsZero:=IsZero and (A[i,1]=0);
  if IsZero then
  begin
    SetLength(A,EqLength,1);
    SetLength(x,1);
    SetLength(dx,1);
  end;

  // старый метод
  {if (SumYi[1] = 0) or  (SumYi[2] = 0) then
  begin
    x[0]:=(1-n[1]/n[0])/SumY[1];
    x[1]:=0;
    SolveEquation:=False;
  end
  else
  begin
    for i:=1 to NUMBEROFMEAS-1 do
    begin
      b[i-1]:=1-n[i]/n[0];
      A[i-1,0]:=SumY[i];
      A[i-1,1]:=SumYi[i];
    end;
    SolveEquation:=True;
  end;}
end;

procedure TfrmMain.ShowResults;
//var i: Integer;
begin
  if IsCalculated then
  begin
    lbl1.Caption:='A= '+FormatFloat('0.000E+00',x[0]);
    if (Length(x)>1) then lbl1.Caption:=lbl1.Caption + ', B= '+FormatFloat('0.000E+00',x[1]);
    lbl1.Caption:=lbl1.Caption+#13#10+'dA= '+FormatFloat('0.000E+00',dx[0]);
  end
  else
    lbl1.Caption:='Calculating error';
end;

procedure TfrmMain.btnShowSLAUClick(Sender: TObject);
var s: string;
    i,j, EqLength: Integer;
begin
  EqLength:=Length(b);
  if (EqLength<>0) then
  begin
    S:='b=(';
    for i:=0 to EqLength-1 do
    begin
      S:=S+FormatFloat('0.00#E+00',b[i]);
      if (i<>EqLength-1) then S:=S+', ';
    end;
    S:=S+'); '+#13#10+'A=(';
    for i:=0 to EqLength-1 do
    begin
      for  j:=0 to Length(A[0])-1  do
      begin
        S:=S+FormatFloat('0.00#E+00',A[i,j]);
        if (j<>Length(A[0])-1) then S:=S+', ' else S:=S+')';
      end;
      if (i<>EqLength-1) then S:=S+#13#10+'    (';
    end;

    ShowMessage(S);
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  b:=nil;
  x:=nil;
  n:=nil;
  SumY:=nil;
  SumYi:=nil;
  A:=nil;
end;

procedure TfrmMain.strngrd1SelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  if ARow=strngrd1.RowCount-1 then strngrd1.RowCount:=strngrd1.RowCount+1;  
end;

procedure TfrmMain.LoadInputFile(FileName: string);
var FileList: TStringList;
    i: Integer;
begin
  FileList:=TStringList.Create;
  FileList.LoadFromFile(Filename);

  strngrd1.RowCount:=FileList.Count+1;
  for i:=1 to strngrd1.RowCount-1 do
  begin
    strngrd1.Cells[1,i]:='';
    strngrd1.Cells[2,i]:='';
  end;

  try
    for i:=1 to FileList.Count-1 do
    begin
      strngrd1.Cells[1,i]:=ExtractWord(1,FileList.Strings[i],[#09,' ']); // скорость счёта
      strngrd1.Cells[2,i]:=ExtractWord(2,FileList.Strings[i],[#09,' ']); // r -- загрузка
      strngrd1.Cells[3,i]:=ExtractWord(3,FileList.Strings[i],[#09,' ']); // p -- загрузка * время
    end;
  finally
    FileList.Free;
  end;
end;


end.
