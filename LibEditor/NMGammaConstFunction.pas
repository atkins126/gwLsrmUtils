unit NMGammaConstFunction;

interface

//uses NMDecay, LCommon;
uses LibUnit;

const
  // ћассовый коэффициент истинного поглощени€
 { Mu: array[0..29, 1..2] of Double = // старый mu_en
  ((0.01, 4.57),
    (0.02, 0.501),
    (0.03, 0.139),
    (0.04, 0.0616),
    (0.05, 0.0374),
    (0.06, 0.0283),
    (0.08, 0.0231),
    (0.1, 0.0227),
    (0.142, 0.0244),
    (0.15, 0.0247),
    (0.2, 0.0265),
    (0.279, 0.0258),
    (0.3, 0.0287),
    (0.4, 0.0294),
    (0.412, 0.0294),
    (0.5, 0.0298),
    (0.6, 0.0295),
    (0.661, 0.0293),
    (0.8, 0.0287),
    (1.0, 0.0278),
    (1.25, 0.0256),
    (1.5, 0.0254),
    (2.0, 0.0234),
    (2.75, 0.0212),
    (3.0, 0.0205),
    (4.0, 0.0187),
    (5.0, 0.0174),
    (6.0, 0.0165),
    (8.0, 0.0153),
    (10.0, 0.0146)); }
    Mu: array[0..28, 0..1] of Double = // mu_en from NIST
  ( (5e-3, 39.31),
    (6e-3, 22.7),
    (8e-3, 9.446),
    (0.01, 4.742),
    (0.015, 1.334),
    (0.02, 0.5389),
    (0.03, 0.1537),
    (0.04, 0.0683),
    (0.05, 0.04098),
    (0.06, 0.03041),
    (0.08, 0.02407),
    (0.1, 0.02325),
    //(0.142, 0.0244),
    (0.15, 0.02496),
    (0.2, 0.02672),
    //(0.279, 0.0258),
    (0.3, 0.02872),
    (0.4, 0.02949),
    //(0.412, 0.0294),
    (0.5, 0.02966),
    (0.6, 0.02953),
    //(0.661, 0.0293),
    (0.8, 0.02882),
    (1.0, 0.02789),
    (1.25, 0.02666),
    (1.5, 0.02547),
    (2.0, 0.02345),
    //(2.75, 0.0212),
    (3.0, 0.02057),
    (4.0, 0.0187),
    (5.0, 0.0174),
    (6.0, 0.01647),
    (8.0, 0.01525),
    (10.0, 0.0145));
 //SortMultiplier = 10e+6;

type
  {единицы дл€ гамма-посто€нной}
  TDoseUnits = (dzExpose,  // перевод в экспозиционную дозу:  м–*см2/(ч*м и)
                dzAbsorb,  // перевод в поглощЄнную дозу в воздухе:  а√р*м2/(с*Ѕк)
                dzEquiv);  // перевод в эквивалентную дозу:  а«в*м2/(с*Ѕк)

function GammaConst(aNuclide:TNuclide): double;
function Pogl(energy: Extended): Extended;
function GammaConstLine(ALine: TLine; GammaUnits: TDoseUnits = dzExpose): Double; // GammaUnits -- единицы гамма-посто€нной, 0 - м–*см2/(ч*м и), 1 - а√р*м2/(с*Ѕк), 2 - а«в*м2/(с*Ѕк)

implementation


// Mu approximation
function Pogl(energy: Extended): Extended;
var i: Integer;
begin
  Result:= 0;
  i:= 0;
  energy := energy / 1000.0; // to MeV
  // find upper bound energy index
  while (i < Length(Mu)) and (Mu[i, 0] < energy) do
    Inc(i);
  // exit if out of bounds
  if (i = 0) or (i = 30) then
    Exit;
  // linear interpolation
  Result:= Ln(Mu[i-1,1]) +
    (Ln(Mu[i,1]) - Ln(Mu[i-1,1])) / (Ln(Mu[i,0]) - Ln(Mu[i-1,0])) *
    (Ln(energy) - Ln(Mu[i-1,0]));
  Result := Exp(Result);
end;

function GammaConst(aNuclide: TNuclide): double;
var
  i: integer;
begin
  Result := 0; // gamma-const
  for i:= 0 to aNuclide.LinesCount - 1 do
    Result:= Result + aNuclide.Lines[i].Energy / 1000 *   // e * I * mu 
             aNuclide.Lines[i].I / 100 *
             Pogl(aNuclide.Lines[i].Energy);
  Result:= 3.7 * 1e7 * Result * 1.6 * 1e-6 * 3600 / 4 / PI / 88;
end;{GammaConst}

function GammaConstLine(ALine: TLine; GammaUnits: TDoseUnits = dzExpose): Double;
begin
  Result:=  ALine.Energy / 1000 * ALine.I / 100 * Pogl(ALine.Energy); // gamma const = e * I * mu
  Result:= 3.7 * 1e7 * Result * 1.6 * 1e-6 * 3600 / 4 / PI / 88;
  if (GammaUnits = dzAbsorb) then {TODO: уточнить коэффициенты}
    Result:= Result / 0.152 // or 0.151
  else if (GammaUnits = dzEquiv) then
    Result:= Result / 0.152 * 1.09;
end;

end.
