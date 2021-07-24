unit MC_Calc;

interface

uses SysUtils, Math, Messages, Windows,
  AlphaDetector;

const
  PROGRESS_POS = WM_USER+1;
  TWOPI = 2 * Pi;

// расчёт эффективности альфа методом Монте-Карло. sourceDiam -- диаметр источника,
// sourceDistance -- расстояние источник-детектор, sourceShift -- сдвиг источника по оси X
// N - число испытаний
function MCAlpha(const aDet: TDet; sourceDiam, sourceDistance, sourceShift: Double;
  N: Int64; mainwndw: HWND; ThreadNum: Integer; var effError: Double): Double;

function minCosThetaCalc(const aDet: TDet; sourceDistance, sourceDiameter, sourceShift: Double): Double;
procedure getRandomPoint(sourceRadius, sourceShift: Double; var x0, y0: Double);
procedure getRandomDirection(maxAngleConst: Double; var phi, cosTheta: Double);
function tallyParticle(const aDet: TDet; cosTheta, phi, x0, y0, sourceDistance: Double): Boolean;

implementation

function MCAlpha(const aDet: TDet; sourceDiam, sourceDistance, sourceShift: Double;
  N: Int64; mainwndw: HWND; ThreadNum: Integer; var effError: Double): Double;
var progress, regCount: Integer; // текущее испытание и число зарегистрированных частиц
    i: Int64;
    x0, y0,    // x0, y0 -- положение рандомной точки на источнике в декартовых координатах
    phi, cosTheta, //t, // направление полёта альфа-частицы и длина пролёта до детектора
    minCosTheta, effError2, sourceRadius, maxAngleConst: Double;
begin
  if (N <= 0) then
  begin
    Result := -1;
    Exit;
  end;
  Randomize();
  regCount := 0;
  // calc minCosTheta
  minCosTheta := minCosThetaCalc(aDet, sourceDistance, sourceDiam, sourceShift);
  maxAngleConst := 1 - minCosTheta;
  sourceRadius := sourceDiam / 2.0;

  progress := 0;
  i := 0;
  while (i < N) do begin
    // get random point
    getRandomPoint(sourceRadius, sourceShift, x0, y0);

    // get random direction
    getRandomDirection(maxAngleConst, phi, cosTheta);

    // get coordinates on detector and its window and tally particle
    if (tallyParticle(aDet, cosTheta, phi, x0, y0, sourceDistance)) then
      Inc(regCount);

    // synchronize results
    if ((Round(100.0 * i / N) - progress) > 0) then
    begin
      Inc(progress);
      PostMessage(mainwndw, PROGRESS_POS, ThreadNum, progress);
    end;
    Inc(i);
  end;

  // calc result efficiency and efficiency error
  Result := regCount / N;
  effError := 200 / Sqrt(regCount);
  effError2 := 200 * Sqrt(Result * (1-Result) * N)/regCount; // M(rC) = eff * N; D(rC) = (eff - eff^2)*N
  Result := Result * (maxAngleConst / 2); // деление на 2 из-за того что, Theta генерится от 0 до Pi/2, т.е. в половину пространства
  effError := Max(effError, effError2);
end;

function minCosThetaCalc(const aDet: TDet; sourceDistance, sourceDiameter, sourceShift: Double): Double;
begin
  if (not aDet.isSquare) then
    Result := (sourceDistance + aDet.sunkZ) / Sqrt(Sqr(sourceDistance + aDet.sunkZ)
      + Sqr(aDet.detDiam + sourceDiameter + sourceShift) / 4)
  else
    Result := (sourceDistance + aDet.sunkZ) / Sqrt(Sqr(sourceDistance + aDet.sunkZ)
      + Sqr(sourceDiameter + sqrt(Sqr(aDet.a) + Sqr(aDet.b) + Sqr(sourceShift))) / 4);
end;

procedure getRandomPoint(sourceRadius, sourceShift: Double; var x0, y0: Double);
var rho, phi0: Double;  // Rho,  Phi -- положение рандомной точки на источнике
begin
  rho := sourceRadius * sqrt(Random);
  phi0 := TWOPI * Random;
  x0 := rho * cos(phi0) + sourceShift;  // add shift to X
  y0 := rho * sin(phi0);
end;

procedure getRandomDirection(maxAngleConst: Double; var phi, cosTheta: Double);
begin
  phi := TWOPI * Random;
  cosTheta := 1 - maxAngleConst * Random;
  // Theta := ArcCos((1 - MinCosTheta) * (Random) + MinCosTheta);
end;

function tallyParticle(const aDet: TDet; cosTheta, phi, x0, y0, sourceDistance: Double): Boolean;
var xS, yS : Double; // положение частицы на плоскости окна детектора
    x, y : Double; // положение частицы на плоскости детектора
    tanTheta: Double;
    isSunken: Boolean;
    tanThcosPhi, tanThsinPhi: Double; // для ускорения расчёта
begin
  if (cosTheta = 0) then begin
    Result := False;
    Exit;
  end;
  tanTheta := Sqrt(1 - Sqr(cosTheta)) / cosTheta;
  tanThcosPhi := tanTheta * cos(phi);
  tanThsinPhi := tanTheta * sin(phi);

  isSunken := (aDet.sunkZ > MinSingle) and (sourceDistance > 0);
  xS := x0 + tanThcosPhi * sourceDistance; // z=R
  yS := y0 + tanThsinPhi * sourceDistance;
  sourceDistance := sourceDistance + aDet.sunkZ;
  x := x0 + tanThcosPhi * sourceDistance;
  y := y0 + tanThsinPhi * sourceDistance;

  // if in window and in detector
  if (not aDet.isSquare) then
  begin
    Result := (Sqr(x) + sqr(y)) <= (Sqr(aDet.detDiam/2));
    if (isSunken) then
      Result := Result and ((Sqr(xS) + sqr(yS)) <= (Sqr(aDet.detDiam/2 + aDet.sunkXY)));
  end
  else begin
    Result := (Abs(x) <= (aDet.a / 2)) and (Abs(y) <= (aDet.b / 2));
    if (isSunken) then
      Result := Result and ((Abs(xS) <= (aDet.a / 2 + aDet.sunkXY)) and (Abs(yS) <= (aDet.b / 2 + aDet.sunkXY)));
  end;
end;

end.
