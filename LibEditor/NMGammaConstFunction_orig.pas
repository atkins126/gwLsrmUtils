unit NMGammaConstFunction;

interface

uses NMDecay, LCommon, LLibrary;

const
  // ћассовый коэффициент истинного поглощени€
  {Mu: array[0..29, 1..2] of Double =   // старый mu_en
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
  SortMultiplier = 10e+6;
   Mu: array[0..28, 1..2] of Double = // mu_en from NIST
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

type
  {единицы дл€ гамма-посто€нной}
  TDoseUnits = (dzExpose,  // перевод в экспозиционную дозу:  м–*см2/(ч*м и)
                dzAbsorb,  // перевод в поглощЄнную дозу в воздухе:  а√р*м2/(с*Ѕк)
                dzEquiv);  // перевод в эквивалентную дозу:  а«в*м2/(с*Ѕк)

{параметр минимальной энергии добавлен, чтобы пользователь при желании мог исключить
из расчета линии с низкими энерги€ми: из-за того, что Mu_en сильно возрастает при низких энерги€х,
 то линии с низкими энерги€ми дают большой вклад в гамма-посто€нную. –еально при расчЄте доз из активности эти энергии не регистрируютс€ дозиметрами}
function GammaConst(ANuclide:TNuclide; AMinEnergy: double = 0):Integer; overload;
function GammaConst(ANuclide:TLibNuclide; AMinEnergy: double = 0):Integer; overload;
function Pogl(E: Extended): Extended;
function GammaConstLine(ALine: TLine; GammaUnits: TDoseUnits = dzExpose): Integer;

implementation


// јпроксимаци€ Mu
function Pogl(E: Extended): Extended;
var
  i: integer;
//  R:Extended;
begin
  Result:= 0;
  i:= 0;
  while (i < 30) and (Mu[i, 1] < E / 1000) do
    Inc(i);
  if (i = 0) or (i = 30) then
    Exit;
  Result:= Exp(Ln(Mu[i - 1, 2]) + (Ln(Mu[i, 2]) - Ln(Mu[i - 1, 2])) / (Ln(Mu[i, 1]) - Ln(Mu[i - 1, 1])) * (Ln(E / 1000) - Ln(Mu[i - 1, 1])));
  //Result:=0.0283;
end;

function GammaConst(ANuclide:TNuclide; GammaUnits: TDoseUnits = dzExpose; AMinEnergy: double = 0):Integer;
var
  i:integer;
begin
  Result:= -1;
  if not Assigned(ANuclide) then
    exit;
  if not Assigned(ANuclide.Lines) then
    exit;
  Result:= 0;
  ANuclide.Gamma:=0;
  for i:= 0 to ANuclide.Lines.Count - 1 do
    if (ANuclide.Lines[i].LineType in [lnGamma, lnXRay, lnAnnigl]) and (ANuclide.Lines[i].Energy >= AMinEnergy) then
      ANuclide.Gamma:=  ANuclide.Gamma +
                        ANuclide.Lines[i].Energy / 1000 *
                        ANuclide.Lines[i].TableIntensity / 100 *
                        Pogl(ANuclide.Lines[i].Energy);
  ANuclide.Gamma:= 3.7 * 1e7 * ANuclide.Gamma * 1.6 * 1e-6 * 3600 / 4 / PI / 88;

  if GammaUnits=dzAbsorb then {TODO: уточнить коэффициенты}
    ANuclide.Gamma:= ANuclide.Gamma/0.152 // либо 0.151
  else if GammaUnits=dzEquiv then
    ANuclide.Gamma:= ANuclide.Gamma/0.152*1.09;
end;{GammaConst}

function GammaConst(ANuclide:TLibNuclide; GammaUnits: TDoseUnits = dzExpose; AMinEnergy: double = 0):Integer;
var
  i:integer;
begin
  Result:= -1;
  if not Assigned(ANuclide) then
    Exit;
  Result:= 0;
  ANuclide.Gamma:=0;
  for I:= 0 to ANuclide.LineCount - 1 do
    if (ANuclide.Line[I].LineType in [lnGamma, lnXRay]) and (ANuclide.Line[i].Energy >= AMinEnergy) then
      ANuclide.Gamma:=  ANuclide.Gamma +
                        ANuclide.Line[I].Energy / 1000 *
                        ANuclide.Line[I].LibYeld / 100 *
                        Pogl(ANuclide.Line[I].Energy);
  ANuclide.Gamma:= 3.7 * 1e7 * ANuclide.Gamma * 1.6 * 1e-6 * 3600 / 4 / PI / 88;
  
  if GammaUnits=dzAbsorb then 
    ANuclide.Gamma:= ANuclide.Gamma/0.152 // либо 0.151
  else if GammaUnits=dzEquiv then
    ANuclide.Gamma:= ANuclide.Gamma/0.152*1.09;
end;{GammaConst}

{мб попробовать эту функцию}
function GammaConstLine(ALine: TLine; GammaUnits: TDoseUnits = dzExpose): Integer;
begin
  Result:= -1;
  if not Assigned(ALine) then
    Exit;
  Result:= 0;
  
  ALine.Gamma:=  ALine.Gamma +       ALine.Energy / 1000 * ALine.I / 100 * Pogl(ALine.Energy);
  ALine.Gamma:= 3.7 * 1e7 * ALine.Gamma * 1.6 * 1e-6 * 3600 / 4 / PI / 88;
  if GammaUnits=dzAbsorb then {TODO: уточнить коэффициенты}
    ALine.Gamma:= ALine.Gamma/0.152 // либо 0.151
  else if GammaUnits=dzEquiv then
    ALine.Gamma:= ALine.Gamma/0.152*1.09;
end;

end.
