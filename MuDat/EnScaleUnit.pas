unit EnScaleUnit;

interface

type TEnGrid = class
  FEmin,
  FEmax: Double; // ������� �����
  FGridNum: Integer;
  FEnergies: array of Double;
  private

  public
    procedure CalcGrid(Emin, Emax: Double; GridPointsNum: Integer; IsLog: Boolean); // ������� ����� �� �������
    function LoadFromFile(FileName: string): Boolean;
    procedure SaveInFile(FileName: string);
  end;

implementation

uses SysUtils;

{ TEnGrid }

procedure TEnGrid.CalcGrid(Emin, Emax: Double; GridPointsNum: Integer;
  IsLog: Boolean);
var i: Integer;
begin
  // ��������
  if (Emin>0) and (Emax>Emin) and (GridPointsNum>1) then
  begin
    FEmin:=Emin;
    FEmax:=Emax;
    FGridNum:=GridPointsNum;
    SetLength(FEnergies,FGridNum);
  end
  else
  begin
    // exception
    Exit;
  end;

  // ������ ����� �� ������� Ei = Emin*exp((lnEmax-lnEmin)/(n-1)*i)
  if IsLog then
  begin
    for i:=0 to FGridNum-1 do
      FEnergies[i]:=FEmin*exp((ln(FEmax)-ln(FEmin))/(FGridNum-1)*i);
  end
  else  // ������ ����� �� ������� Ei = (Emax-Emin)/(n-1)*i + Emin
  begin
    for i:=0 to FGridNum-1 do
      FEnergies[i]:=(FEmax-FEmin)/(FGridNum-1)*i+FEmin;
  end;
end;

function TEnGrid.LoadFromFile(FileName: string): Boolean;
var F: TextFile;
    CurDecSep: Char;
begin
  Result:=False;
  CurDecSep:=DecimalSeparator;
  DecimalSeparator:='.';
  FGridNum:=0;

  // �������� �����
  AssignFile(F, filename);
  {$I-}Reset(F) {$I+};
  if IOResult<>0 then
    begin
      //ShowMessage('Error openning file '+FileName);
      exit;
    end;

  // ������ ����� � ���� mju
  try
    While not Eof(F) do
    begin
      Inc(FGridNum);
      SetLength(FEnergies,FGridNum);
      Readln(F,FEnergies[FGridNum-1]);
    end;
  finally
    DecimalSeparator:=CurDecSep;
    CloseFile(F);
  end;
  Result:=True;
end;

procedure TEnGrid.SaveInFile(FileName: string);
var F: TextFile;
    CurDecSep: Char;
    i: Integer;
begin
  CurDecSep:=DecimalSeparator;
  DecimalSeparator:='.';

  // �������� �����
  AssignFile(F, filename);
  Rewrite(F);
  for i:=0 to FGridNum-1 do
    Writeln(F,FloatToStrF(FEnergies[i],ffGeneral,5,3));

  CloseFile(F);
  DecimalSeparator:=CurDecSep;
end;

end.
