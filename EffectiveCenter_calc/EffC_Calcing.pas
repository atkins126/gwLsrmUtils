unit EffC_Calcing;

interface

uses
  Controls, SysUtils, DateUtils,
  LibUnit;

type
  TMeasurement = record
    FEnergy: Double; // энергия
    FDistance: Double; // расстояние
    FCount: Double; // счёт
    FDCount: Double; // счёт
    FDate: TDate; // дата
    FNuclide: string; // нуклид
    FHalfTime: Double; // период полураспада этого нуклида
  end;

  TMeasurements = class
    FMeasurement: array of TMeasurement;
    FLib: TLibrary;  // библиотека для пересчёта
    FDistError: Double;
  private
    FMeasCount: Integer;
    procedure setMeasCount(AMeasCount: Integer);
  public
    CalcResult: Boolean; // calculation results writed here
    constructor Create(const libPath: string);
    destructor Destroy; override;
    function ThalfFromNuclideName(i: Integer): Boolean; // получение периода полураспада из нуклида
    function RecalcMeasOnDate(GoalDate: TDate; i: Integer): Boolean; // пересчёт i-го измерения на одну дату
    procedure SortInsert(); // сортировка массива методом вставки
    function RecalcOnDate(GoalDate: TDate): Boolean; // пересчёт всех записей массива на одну дату
    function IsOneDate(): Boolean; // проверка, что все записи с одной датой
    function CalcEffCenter(b,n: Integer; var EffCenterError: Double; IsWriteLog: Boolean = False): Double; // вычисление эффективного центра для одной энергии, b - начальный элемент массива, n- число элементов с одинаковой энергией
    function CalcEffCenterPrecise(b,n: Integer; DetDiam: Double; var EffCenterError: Double; IsWriteLog: Boolean = False): Double; // вычисление эфф. центра в более точной модели (см. EffCenter_description.odt)
    function CalcEffCenterPrec2(b,n: Integer; DetDiam: Double; var EffCenterError: Double; IsWriteLog: Boolean = False): Double; // упрощение более точной модели -- использовалось для отладки
    function CalcEffCenterError(n0, n, r, r0, dn0, dn, dr0, dr: Double): Double; // вычисление погрешности эффективного центра
    property measCount: Integer read FMeasCount write setMeasCount;  
  end;
  
  TEffCenter = record
    FEnergy: Double;
    FEc: Double; // расстояние до эфф. центра
    FEcError: Double; // погрешность расчёта
  end;

  TEffCenters = class     // класс эффективных центров
    FEffCenters: array of TEffCenter;
  private
    FEffCount: Integer;
    procedure setEffCount(ACount: Integer);
  public
    function EffCFromEnergy(Energy: Double; var Effc, dEffc: Double): Boolean; // возращает эффективный центр для заданной энергии
    property effCount: Integer read FEffCount write setEffCount;
  end;

var
  F: TextFile;

  procedure EffCentersCalculating(DMeasurements: TMeasurements; var DEffCenter: TEffCenters;
    IsWriteLog, IsPrecise: Boolean; const LogFileName: string; DetDiam: Double);

implementation

uses MathLib;

procedure EffCentersCalculating(DMeasurements: TMeasurements; var DEffCenter: TEffCenters;
  IsWriteLog, IsPrecise: Boolean; const LogFileName: string; DetDiam: Double);
var b, i, j, M: Integer;
    CurrEnergy, CurrEffCenter, CurrEffCentError: Double;
begin
  b := 0; // начальный элемент с одинаковой энергией
  j := 0; // счётчик записей в DEffCenter
  if (IsWriteLog) then
  begin
    AssignFile(F, LogFileName);
    Rewrite(F);
    //Writeln(F,'E'+#09+'R1'+#09+'R2'+#09+'EffC'+#09+'dEffC');
    (*TODO: так делать нехорошо, нужно либо передавать поток в метод, либо strList *)
  end;
  
  while (b < DMeasurements.measCount-1) do
  begin
    // find group of records with same energy
    i := b;
    CurrEnergy := DMeasurements.FMeasurement[b].FEnergy;
    while (DMeasurements.FMeasurement[i].FEnergy = CurrEnergy) do
    begin
      Inc(i);
      if (i > DMeasurements.measCount - 1) then Break;
    end;
    M := i - b; // число записей с одинаковой энергией
    // calculate EffCenter for them
    if (IsPrecise) then
      CurrEffCenter := DMeasurements.CalcEffCenterPrecise(b, M, detdiam, CurrEffCentError, IsWriteLog) // вычисляем эффективный центр для CurrEnergy
    else
      CurrEffCenter := DMeasurements.CalcEffCenter(b, M, CurrEffCentError, IsWriteLog); // вычисляем эффективный центр для CurrEnergy
    // add to list of effcenters
    DEffCenter.effCount := j + 1;
    DEffCenter.FEffCenters[j].FEnergy := CurrEnergy;
    if (DMeasurements.CalcResult) then // расчёт проведён для этой энергии
    begin
      DEffCenter.FEffCenters[j].Fec := CurrEffCenter;
      DEffCenter.FEffCenters[j].FEcError := CurrEffCentError;
    end
    else
    begin
      DEffCenter.FEffCenters[j].Fec := -1;
      DEffCenter.FEffCenters[j].FEcError := -1;
    end;
    j := j + 1;
    b := i; // find next group of records with same energy from b-position 
  end;
  if (IsWriteLog) then CloseFile(F);
end;

{ TMeasurements }

function TMeasurements.CalcEffCenter(b, n: Integer; var EffCenterError: Double; IsWriteLog: Boolean = False): Double;
var R1, R2, n1, n2: Double; // расстояния и скорости счёта
    i, j, k: Integer;
    EffC, EffE: array of Double; // массивы эффективных центров от разных измерений и погрешности
begin
  CalcResult := False;
  if (n < 2) then
  begin
    Exit;
  end;
  {FORMULA: EffCenter = (R2 - sqrt(n1/n2)*R1)/(sqrt(n1/n2) - 1)}
  k := 0;
  SetLength(EffC, (n*(n-1)) div 2);
  SetLength(EffE, (n*(n-1)) div 2);

  for i:=b to b+n-2 do
    for j:=i+1 to b+n-1 do
    begin
      // use aliases
      R1 := FMeasurement[i].FDistance;
      R2 := FMeasurement[j].FDistance;
      n1 := FMeasurement[i].FCount;
      n2 := FMeasurement[j].FCount;
      // calc effcenter
      if (n1*n2 < 0) then Exit;
      if (n1 <> n2) then begin
        EffC[k] := (R2 - sqrt(n1/n2)*R1)/(sqrt(n1/n2) - 1);
        EffE[k] := CalcEffCenterError(n1, n2, R1, R2, FMeasurement[i].FDCount,
        FMeasurement[j].FDCount, FDistError, FDistError);
        //EffE[k]:=0.1*sqrt(1+(n1/n2))/(sqrt(n1/n2)-1); // примерная формула для погрешности, вбить потом другую
      end
      else begin
        EffC[k] := 0;
        EffE[k] := 1;
      end;
      
      if (IsWriteLog) then
      Write(F, FloatToStrF(FMeasurement[i].FEnergy, ffGeneral, 4, 3) +#09 +
      FloatToStrF(R1, ffGeneral, 3, 2) +#09 + FloatToStrF(R2,ffGeneral, 3, 2) +#09 +
      FloatToStrF(EffC[k], ffGeneral, 3, 3) +#09 + FloatToStrF(EffE[k], ffGeneral, 3, 3) + #09);
      
      Inc(k);
    end;
    
  WeightedMean(EffC, EffE, Result, EffCenterError, True);
  //Result:=Average(EffC);
  if (IsWriteLog) then
    Writeln(F,FloatToStrF(FMeasurement[b].FEnergy,ffGeneral,4,3) + #09 + 'Average'
      +#09 + 'Average' + #09 + FloatToStrF(Result,ffGeneral,3,3) + #09 + FloatToStrF(EffCenterError,ffGeneral,3,3));
  CalcResult := True;
end;

function TMeasurements.CalcEffCenterPrecise(b, n: Integer; DetDiam: Double; var EffCenterError: Double; IsWriteLog: Boolean = False): Double;
var R1,R2,n1,n2, Dover4: Double; // расстояния и скорости счёта
    i, j, k: Integer;
    EffC, EffE: array of Double; // массивы эффективных центров от разных измерений и погрешности
begin
  CalcResult:=False;
  if n<2 then
  begin
    Exit;
  end;
  {FORMULA: D/4 = n1/n2*(r2-r1)^2 - (n1/n2-1)^2 * d^2 /8
            EffCenter = ((R2 - n1/n2*R1)+sqrt(D/4))/((n1/n2)-1)}
  k:=0;
  SetLength(EffC,(n*(n-1)) div 2);
  SetLength(EffE,(n*(n-1)) div 2);

  for i:=b to b+n-2 do
    for j:=i+1 to b+n-1 do
    begin
      R1:=FMeasurement[i].FDistance;
      R2:=FMeasurement[j].FDistance;
      n1:=FMeasurement[i].FCount;
      n2:=FMeasurement[j].FCount;
      Dover4:=n1/n2*sqr(R2-R1) - sqr(n1/n2-1)*sqr(DetDiam)/8;
      if (Dover4>=0) then EffC[k]:=((R2-n1/n2*R1)+sqrt(Dover4))/(n1/n2-1)
      else
      begin
        EffC[k]:=((R2-n1/n2*R1))/(n1/n2-1)
      end;
      EffE[k]:=CalcEffCenterError(n1,n2,R1,R2,FMeasurement[i].FDCount,FMeasurement[j].FDCount,FDistError,FDistError);
      if (IsWriteLog) then Write(F,FloatToStrF(FMeasurement[i].FEnergy,ffGeneral,4,3)+#09+FloatToStrF(R1,ffGeneral,3,2)
      +#09+FloatToStrF(R2,ffGeneral,3,2)+#09+FloatToStrF(EffC[k],ffGeneral,3,3)+#09+FloatToStrF(EffE[k],ffGeneral,3,3)+#09);
      Inc(k);
    end;
  WeightedMean(EffC, EffE, Result, EffCenterError, true);
  //Result:=Average(EffC);
  if (IsWriteLog) then Writeln(F,FloatToStrF(FMeasurement[b].FEnergy,ffGeneral,4,3)+#09+'Average'
      +#09+'Average'+#09+FloatToStrF(Result,ffGeneral,3,3)+#09+FloatToStrF(EffCenterError,ffGeneral,3,3));
  CalcResult:=True;
end;

function TMeasurements.CalcEffCenterPrec2(b, n: Integer; DetDiam: Double; var EffCenterError: Double; IsWriteLog: Boolean = False): Double;
var R1,R2,n1,n2, Temp: Double; // расстояния и скорости счёта
    i, j, k: Integer;
    EffC, EffE: array of Double; // массивы эффективных центров от разных измерений и погрешности
begin
  CalcResult := False;
  if (n < 2) then
  begin
    Exit;
  end;
  {FORMULA: EffCenter = (R2-sqrt(n1/n2)*R1)/(sqrt(n1/n2)-1) - (n1/n2-1)*d^2/(16*n1/n2*(n1/n2-1)*(R2-R1))}
  k:=0;
  SetLength(EffC,(n*(n-1)) div 2);
  SetLength(EffE,(n*(n-1)) div 2);

  for i:=b to b+n-2 do
    for j:=i+1 to b+n-1 do
    begin
      R1 := FMeasurement[i].FDistance;
      R2 := FMeasurement[j].FDistance;
      n1 := FMeasurement[i].FCount;
      n2 := FMeasurement[j].FCount;
      EffC[k] := (R2 - sqrt(n1 / n2) * R1) / (sqrt(n1 / n2) - 1);
      Temp := -(n1 / n2 - 1) * sqr(DetDiam) / (16 * sqrt(n1 / n2) * (R2 - R1));
      EffC[k] := EffC[k] + Temp;
      EffE[k] := CalcEffCenterError(n1, n2, R1, R2, FMeasurement[i].FDCount, FMeasurement[j].FDCount, FDistError, FDistError);
      if (IsWriteLog) then
        Write(F, FloatToStrF(FMeasurement[i].FEnergy, ffGeneral, 4, 3) + #09 + FloatToStrF(R1, ffGeneral, 3, 2) + #09 + FloatToStrF(R2, ffGeneral, 3, 2) + #09 + FloatToStrF(EffC[k], ffGeneral, 3, 3) + #09 + FloatToStrF(EffE[k], ffGeneral, 3, 3) + #09);
      Inc(k);
    end;
  WeightedMean(EffC, EffE, Result, EffCenterError, True);
  if (IsWriteLog) then
    Writeln(F, FloatToStrF(FMeasurement[b].FEnergy, ffGeneral, 4, 3) + #09 + 'Average'
      +#09 + 'Average' + #09 + FloatToStrF(Result,ffGeneral,3,3) + #09 + FloatToStrF(EffCenterError,ffGeneral,3,3));
  CalcResult := True;
end;

constructor TMeasurements.Create(const LibPath: string);
begin
  FLib := TLibrary.Create;
  if not FLib.OpenShortTabLib(libPath) then // загрузка библиотеки из файла
  begin
    FLib.DefaultLibFill();
  end;
end;

destructor TMeasurements.Destroy;
begin
  FreeAndNil(Flib);
  inherited;
end;

function TMeasurements.IsOneDate: Boolean;
var i: Integer;
begin
  Result:= True;
  for i:=0 to FMeasCount-1 do
    if (FMeasurement[i].FDate <> FMeasurement[0].FDate) then
    begin
      Result := False;
      Break;
    end;
end;

function TMeasurements.RecalcMeasOnDate(GoalDate: TDate; i: Integer): Boolean;
var TDiff: Double;
begin
  if not ThalfFromNuclideName(i) then // получение периода полураспад из имени нуклида
  begin
    Result := False;
    Exit;
  end;
  {FORMULA NewCount:=OldCount*exp(ln(2)*(oldDate-NewDate)/365.25/T1/2)}
  TDiff := DaysBetween(GoalDate, FMeasurement[i].FDate); // разность в днях
  if (GoalDate > FMeasurement[i].FDate) then
    TDiff := -TDiff;
  FMeasurement[i].FCount := FMeasurement[i].FCount *
    exp(ln(2) * TDiff / 365.25 / FMeasurement[i].FHalfTime);
  FMeasurement[i].FDate := GoalDate;
  Result := True;
end;

function TMeasurements.RecalcOnDate(GoalDate: TDate): Boolean;
var i: Integer;
begin
  Result := True;
  for i:=0 to FMeasCount-1 do
  begin
    Result := Result and RecalcMeasOnDate(GoalDate, i);
  end;
end;

procedure TMeasurements.SortInsert();
var i, j : Integer;
    Temp: TMeasurement;
begin
  for i:=1 to FMeasCount-1 do
  begin
    Temp := FMeasurement[i];
    j := i - 1;
    while Temp.FEnergy < FMeasurement[j].FEnergy do
    begin
      FMeasurement[j+1] := FMeasurement[j];
      Dec(j);
      if (j < 0) then
        Break;
    end;
    FMeasurement[j+1] := Temp;
  end;
end;

function TMeasurements.ThalfFromNuclideName(i: Integer): Boolean;
begin
  Result := True;
  FMeasurement[i].FHalfTime := FLib.ThalfFromName(FMeasurement[i].FNuclide);
  if (FMeasurement[i].FHalfTime= -1) then
  begin
    FMeasurement[i].FHalfTime := 1e9;
    Result := False;
  end;
end;

function TMeasurements.CalcEffCenterError(n0, n, r, r0, dn0, dn, dr0, dr: Double): Double;
var dE_dr0, dE_dr, dE_dn0, dE_dn: Double; // соответствующие производные
begin
  {вычисление происходит по формуле 9 документа EffCenter_description.odt}
  dE_dr0:= 1/(Sqrt(n/n0) - 1);
  dE_dr:= -Sqrt(n/n0) / (Sqrt(n/n0)-1);
  dE_dn:= (r-r0) / (2*Sqrt(n*n0)*Sqr(Sqrt(n/n0)-1));
  dE_dn0:= (r0-r)/(2*n0*Sqrt(n0/n) * Sqr(Sqrt(n/n0)-1));
  Result:= Sqrt(Sqr(dE_dr0*dr0) + Sqr(dE_dr*dr) + Sqr(dE_dn*dn) + Sqr(dE_dn0*dn0));
end;

procedure TMeasurements.setMeasCount(AMeasCount: Integer);
begin
  if (AMeasCount >= 0) then
    FMeasCount := AMeasCount
  else
    FMeasCount := 0;
  SetLength(FMeasurement, FMeasCount);
end;

{ TEffCenters }

function TEffCenters.EffCFromEnergy(Energy: Double; var Effc, dEffc: Double): Boolean;
var i: Integer;
begin
  {TODO: сделать интерполяцию}
  Result := False;
  if (Energy < 0) then Exit;
  for i:=0 to FEffCount-1 do
  begin
    if Abs(Energy - FEffCenters[i].FEnergy) < 0.01 then
    begin
      Result := True;
      Effc := FEffCenters[i].FEc;
      dEffc := FEffCenters[i].FEcError;
    end;
  end;
end;

procedure TEffCenters.setEffCount(ACount: Integer);
begin
  if (ACount >= 0) then
    FEffCount := ACount
  else
    FEffCount := 0;

  SetLength(FEffCenters, FEffCount);
end;

end.
