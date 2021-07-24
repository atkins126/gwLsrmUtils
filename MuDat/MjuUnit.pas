unit MjuUnit;

interface

uses
  Classes, Matrix;

type
  TEnergyUnits = (euMeV, eukeV); // units for energy

  TMjuSingle = record
    E,                  // Energy, MeV
    mu: Double;         // mu     
  end;

  TMju = record
    E,                 // Energy, MeV
    CohScat,           // Scattering coherent               (1)
    IncohScat,         // Scat. Incoh                       (2)
    PhotAbs,           // Photoelectric absorption          (3)
    PairInNuc,         // Pair production in nuclear field  (4)
    PairInEl,          // Pair production in electron field (5)
    TotalWithCoh,      // Total with coherent scat          (6)
    TotalWOCoh: Double; // Total without coherent. scat     (0)
  end;

  TElement = class
    Z: Byte;                            // зарядовое число
    ElSymb: string;                     // символ элемента
    ElName, ElNameEn, ElNameRu: string; // имена элемента: латинское, английское, русское
    Rho: Double;                        // плотность материала элемента в г/см3
    Ma: Double;                         // молярная (атомная) масса элемента
    Md: Double;                         // массовая доля в соединении
    Fmju: array of TMju;
  private
    FGridCounts: Integer;               // число линий с mju
    procedure setLengthes(aN: Integer);
    procedure ZElNameFromFileName(const filename: string); // парсинг имени файла в Z и имя элемента
    function strToMju(const S: string; out mju: TMju): Boolean;  // перевод считанной строки в запись для mju
  public
    constructor LoadElementFromFile(const dirname, filename: string);
    constructor CopyElement(const aElement: TElement);
    constructor Create(aZ: Byte; aMd, aMa: Double); overload;
    constructor Create(); overload;
    function getMu(E: Double; AbsorbType: Integer = 0): Double; // возвращает энергию для введённого мю
    function getMuList(aEnergies: TDoubleVector): TDoubleVector;
    function calcAbsorption(aMu, aThick: Double): Double;
    function getAbsorption(aEnergy, aThick: Double): Double;
    property gridCounts: Integer read FGridCounts write setLengthes;
  end;
  //pElement = ^TElement;

  TElementBer = record
    Z: Byte;                             // зарядовое число
    Ma: Double;                          // молярная (атомная) масса элемента
    FGridCountsNP,                       // число линий с mju для образования пар в поле ядра
    FGridCountsEP,                       // число линий с mju для образования пар в поле электрона
    FGridCountsCS,                       // число линий с mju для когерентного рассеяния
    FGridCountsCm,                       // число линий с mju для комптона
    FGridCountsPh,                       // число линий с mju для фотоэффекта
    FGridCountsTotal: Integer;           // число линий с полными mju
    FmjuNP,                              // массив мю для образования пар в поле ядра
    FmjuEP,                              // массив мю для образования пар в поле электрона
    FmjuCS,                              // массив мю для когерентного рассеяния
    FmjuCm,                              // массив мю для комптона
    FmjuPh,                              // массив мю для фотоэффекта
    FmjuTotal: array of TMjuSingle;      // массив полных мю
  end;

  TChemTableMju = class
    FElements: TList;
    FElBerCount: Integer;
    FElementsBer: array of TElementBer;
  private
    function NumFromZ(Z: Byte): Integer; // возвращает элемента нуклида в базе, -1 - нет такого элемента
    function NumFromName(ElSymb: string): Integer; // возвращает номер элемента в базе по имени элемента
    function StrToElProp(S: string; CurEl: TElement): Integer;
    function getElCount(): Integer; // преобразование строки в элемент
  public
    constructor LoadMjusFromTxtFiles(const DBDirName: string); // загружает в память значения mju из БД, возвращает число загруженных элементов
    destructor Destroy(); override;
    function MjuFromEnergy(ElNum: Byte; E: Double; AbsorbType: Integer = 0): Double; overload; // возвращает mju для введённой энергии для конкретного нуклида. AbsorbType - тип мю, 0 - Total without coherent. scat, ...
    function MjuFromEnergy(ElSymb: string; E_MeV: Double; AbsorbType: Integer = 0): Double; overload; // возвращает mju для введённой энергии для конкретного нуклида
    function EnergyFromMu(ElNum: Byte; mu: Double; AbsorbType: Integer = 0): Double; // возвращает энергию для введённого мю
    function LoadElPropFromTxt(const filename: string): Boolean; // считывает из файла инфо о именах элементов, их плотности
    function ElFromZ(Z: Byte): TElement; // возвращает элемент, зная его Z
    function ElFromSymb(Name: string): TElement; // возвращает элемент, зная его символ
    function ElNameFromZ(ElNum: Byte): string; // возвращает имя элемента из его Z
    function ElZFromName(ElName: string): Byte; // возвращает Z элемента из его имени
    function LoadMassesFromBin(const fileName: string): Boolean; // загружает атомные массы из Masses.bin Берлизова
    function LoadAttenuatFromBin(const fileName: string): Boolean; // загружает мю из ATTENUAT.bin Берлизова
    function SaveAttenuatToBin(const filename: string): Boolean; // сохраняет мю в ATTENUAT.bin
    property elCount: Integer read getElCount;
  end;

  TMaterial = class  // материал
  private
    FName: string;  // название материала
    FRho: Double;   // плотность материала в г/см3
    FElNum: Integer;  // кол-во хим элементов в соединении
    FHasMu: Boolean;
    function parseElement(const aStr: string; var aElement: TElement;
      idx: Integer): Integer;
    procedure addInfoFromTable(const chemTableMu: TChemTableMju);
    procedure mdNormalize();
    procedure fillElements(const aStr: string);
  public
    FElements: TList;  // элементы с массовыми долями в соединении
    constructor Create(); overload;
    constructor Create(const chemTableMu: TChemTableMju; const matName: string;
      aRho: Double; const chemFormula: string); overload;
    constructor loadFromString(const aString: string); overload; // matName Rho ElNum [z1 md1 M; z2 md2 M; ...]
    constructor loadFromString(const aString: string; const chemTableMu: TChemTableMju); overload; // matName Rho ElNum [z1 md1 M; z2 md2 M; ...]
    destructor Destroy(); override;
    function asString(): string;
    function getMu(aEnergy: Double): Double;  // E in Mev
    procedure getMuArr(const aEnergyArr: array of Double; var aMu: array of Double); // E in keV
    function calcAbsorption(mu, thick: Double; aRho: Double = -1): Double;
    property name: string read FName write FName;
    property rho: Double read FRho write FRho;
  end;

  TMaterialList = class  // materials database
  private
    function getMatCount: Integer;
    function getMaterial(idx: Integer): TMaterial;
  public
    FMaterials: TList;  // material list
    constructor loadMaterialsFromTxt(const fileName: string); overload;  // load materials from text database
    constructor loadMaterialsFromTxt(const fileName: string; const chemTableMu: TChemTableMju); overload;
    destructor Destroy(); override;
    property matCount: Integer read getMatCount;
    property material[idx: Integer]: TMaterial read getMaterial; default;
  end;

implementation

uses SysUtils, Math, JvJCLUtils, Interpolation;

{ TElement }

function TElement.calcAbsorption(aMu, aThick: Double): Double;
var muRhoT: Double;
begin
  muRhoT := aMu * Rho * aThick;
  if (muRhoT < 300) then
    Result := Exp(-muRhoT)
  else
    Result := 0;
end;

function TElement.getAbsorption(aEnergy, aThick: Double): Double;
var muRhoT: Double;
begin
  muRhoT := getMu(aEnergy/1000) * Rho * aThick;
  if (muRhoT < 300) then
    Result := Exp(-muRhoT)
  else
    Result := 0;
end;

constructor TElement.CopyElement(const aElement: TElement);
var i: Integer;
begin
  Z := aElement.Z;
  ElSymb := aElement.ElSymb;
  ElName := aElement.ElName;
  ElNameEn := aElement.ElNameEn;
  ElNameRu := aElement.ElNameRu;
  Rho := aElement.Rho;
  Ma := aElement.Ma;
  Md := aElement.Md;
  FGridCounts := aElement.GridCounts;
  SetLength(Fmju, FGridCounts);
  for i:=0 to FGridCounts-1 do
    Fmju[i] := aElement.Fmju[i];
end;

constructor TElement.Create(aZ: Byte; aMd, aMa: Double);
begin
  Z := aZ;
  Ma := aMa;
  Md := aMd;
end;

constructor TElement.Create;
begin

end;

function TElement.getMuList(aEnergies: TDoubleVector): TDoubleVector;
var i: Integer;
begin
  if (aEnergies.size = 0) then begin
    Result := nil;
    Exit;
  end;
  Result := TDoubleVector.Create(aEnergies.size);
  for i:=0 to aEnergies.size-1 do begin
    Result.push_back(getMu(aEnergies.at[i]));
  end;
end;

constructor TElement.LoadElementFromFile(const dirname, filename: string);
var F: TextFile;
    s: string;
    CurDecSep: Char;
begin
  // open file with element info
  AssignFile(F, dirname + filename);
  {$I-}Reset(F) {$I+};
  if (IOResult <> 0) then
  begin
    exit;
  end;

  // transform file name to element number and name
  FGridCounts := 0;
  ZElNameFromFileName(filename);

  // reading file to mu element
  CurDecSep := DecimalSeparator;
  DecimalSeparator := '.';
  While not Eof(F) do
  begin
    Readln(F, s);
    // TODO: it's a bad idea for arrays
    SetLengthes(FGridCounts+1);
    strToMju(s, FMju[FGridCounts-1]);
  end;

  CloseFile(F);
  DecimalSeparator := CurDecSep;
end;

function TElement.getMu(E: Double; AbsorbType: Integer): Double;
var i: Integer;
    EnArr, Mju: TVector; // energy and mu grid
begin
  SetLength(EnArr, FGridCounts);
  SetLength(Mju, FGridCounts);
  for i:=0 to Length(Mju)-1 do
  begin
    EnArr[i] := Fmju[i].E;
    case AbsorbType of
    0: Mju[i] := Fmju[i].TotalWOCoh;
    1: Mju[i] := Fmju[i].CohScat;
    2: Mju[i] := Fmju[i].IncohScat;
    3: Mju[i] := Fmju[i].PhotAbs;
    4: Mju[i] := Fmju[i].PairInNuc;
    5: Mju[i] := Fmju[i].PairInEl;
    6: Mju[i] := Fmju[i].TotalWithCoh;
    end;
  end;

  // check boudaries
  if (E < Fmju[0].E) then
  begin
    Result := mju[0];
    Exit;
  end;
  if (E > Fmju[FGridCounts-1].E) then
  begin
    Result := Mju[FGridCounts-1];
    Exit;
  end;

  // проверка на образование пар -- это мю имеет пороговое значение
  if (AbsorbType = 4) and (E <= 1.022) then
  begin
    Result := 0; // нет образования пар
    Exit;
  end;

  if (AbsorbType = 5) and (E <= 2.044) then
  begin
    Result := 0; // нет образования пар
    Exit;
  end;

  InterPolLog(3, EnArr, Mju, E, Result, lsXYBoth);
end;

procedure TElement.setLengthes(aN: Integer);
begin
  FGridCounts := aN;
  if (FGridCounts < 0) then
    FGridCounts := 0;

  SetLength(Fmju, FGridCounts);
end;

function TElement.strToMju(const S: string; out mju: TMju): Boolean;
begin
  if WordCount(S, [' ']) <> 8 then
  begin
    Result := False;
    Exit;
  end
  else
    try
      mju.E := StrToFloat(ExtractWord(1, S, [' ']));
      mju.CohScat := StrToFloat(ExtractWord(2, S, [' ']));
      mju.IncohScat := StrToFloat(ExtractWord(3, S, [' ']));
      mju.PhotAbs := StrToFloat(ExtractWord(4, S, [' ']));
      mju.PairInNuc := StrToFloat(ExtractWord(5, S, [' ']));
      mju.PairInEl := StrToFloat(ExtractWord(6, S, [' ']));
      mju.TotalWithCoh := StrToFloat(ExtractWord(7, S, [' ']));
      mju.TotalWOCoh := StrToFloat(ExtractWord(8, S, [' ']));
    except
      // send exception
    end;
  Result := True;
end;

procedure TElement.ZElNameFromFileName(const filename: string);
var i: Integer;
begin
  // filename: 057La
  z := 0;
  ElSymb := '';
  if Length(filename) < 2 then Exit;
  if (filename[1] < '0') or (filename[1] > '9') then Exit;

  for i:=2 to Length(filename) do
    // find element name
    if (filename[i] >= 'A') and (filename[i] <= 'z') then
    begin
      ElSymb := Copy(filename, i, Length(filename) - i + 1);
      Z := StrToInt(Copy(filename, 1, i - 1));  // exception проверка
      exit;
    end
    else if (filename[i] < '0') or (filename[i] > '9') then
      Exit;
      // raise Exception
end;


{ TChemTableMju }

destructor TChemTableMju.Destroy;
var i: Integer;
begin
  for i:=0 to FElements.Count-1 do
    TElement(FElements[i]).Free();
  FElements.Free();
  inherited;
end;

function TChemTableMju.ElFromSymb(Name: string): TElement;
begin
  if NumFromName(Name) >= 0 then
    Result := TElement(FElements[NumFromName(Name)])
  else
    Result := nil;
end;

function TChemTableMju.ElFromZ(Z: Byte): TElement;
begin
  if NumFromZ(Z) >= 0 then
    Result := TElement(FElements[NumFromZ(Z)])
  else
    Result := nil;
end;

function TChemTableMju.ElNameFromZ(ElNum: Byte): string;
var i: Integer;
begin
  i := NumFromZ(ElNum);
  if (i >= 0) then
    Result := TElement(FElements[i]).ElSymb
  else
    Result := '';
end;

function TChemTableMju.ElZFromName(ElName: string): Byte;
var i: Integer;
begin
  i := NumFromName(ElName);
  if (i >= 0) then
    Result := TElement(FElements[i]).Z
  else
    Result := 0;
end;

function TChemTableMju.EnergyFromMu(ElNum: Byte; mu: Double; AbsorbType: Integer = 0): Double;
var i: Integer;
    E1, E2: Double;
    Mju: array of Double;
    curElement: TElement;
begin
  i := NumFromZ(ElNum);
  if i >= 0 then
    ElNum := i
  else
  begin
    // raise exception,  выводить не в Result, а в отдельную переменную для ошибок
    Result := -1;
    Exit;
  end;

  curElement := TElement(FElements[ElNum]);
  SetLength(Mju, curElement.GridCounts);
  for i:=0 to Length(Mju)-1 do
  begin
    case AbsorbType of
    0: Mju[i] := curElement.Fmju[i].TotalWOCoh;
    1: Mju[i] := curElement.Fmju[i].CohScat;
    2: Mju[i] := curElement.Fmju[i].IncohScat;
    3: Mju[i] := curElement.Fmju[i].PhotAbs;
    4: Mju[i] := curElement.Fmju[i].PairInNuc;
    5: Mju[i] := curElement.Fmju[i].PairInEl;
    6: Mju[i] := curElement.Fmju[i].TotalWithCoh;
    end;
  end;

  // проверка границ
  if mu > Mju[0] then
  begin
    Result := curElement.Fmju[0].E;
    Exit;
  end;
  if mu < Mju[curElement.GridCounts-1] then
  begin
    Result := curElement.Fmju[curElement.GridCounts-1].E;
    Exit;
  end;
  // поиск ближайшей большей энергии
  i:=0;
  while (Mju[i] > mu) do
    Inc(i);

  if (i = 0) then
  begin
    Result := curElement.Fmju[0].E;
    Exit;
  end;
  // расчёт из соединённой прямой между этими 2-мя точками: mx=m1 + (Ex-E1)/(E2-E1)*(m2-m1)
  E1 := curElement.Fmju[i-1].E;
  E2 := curElement.Fmju[i].E;
  Result := E1 + (E2-E1)*(mu-Mju[i-1])/(Mju[i]-Mju[i-1]);
end;

function TChemTableMju.getElCount(): Integer;
begin
  Result := FElements.Count;
end;

function TChemTableMju.LoadAttenuatFromBin(const fileName: string): Boolean;
const
  BERZ = 100; // число элементов у Берлизова в файле (приходиться задавать константой)
  NNP = 55; // число мю для образования пар в поле ядра
  NEP = 51; // число мю для образования пар в поле электрона
  NCS = 80; // число мю для коггерентного рассеяния и комптона
var F: TFileStream;
    TempArrEn, TempArrMu, TempArrMu2: array of Double;
    i, j: Integer;
begin
  Result := False;
  if not FileExists(fileName) then Exit;

  // установка длин массивов
  FElBerCount := BERZ;
  SetLength(FElementsBer, FElBerCount);
  for i:=0 to FElBerCount-1 do
  begin
    FElementsBer[i].Z := i;
    FElementsBer[i].FGridCountsNP := NNP;
    FElementsBer[i].FGridCountsEP := NEP;
    FElementsBer[i].FGridCountsCS := NCS;
    FElementsBer[i].FGridCountsCm := NCS;
    SetLength(FElementsBer[i].FmjuNP, NNP);
    SetLength(FElementsBer[i].FmjuEP, NEP);
    SetLength(FElementsBer[i].FmjuCS, NCS);
    SetLength(FElementsBer[i].FmjuCm, NCS);
  end;
  
  F := TFileStream.Create(fileName, fmOpenRead, fmShareDenyWrite);
  // считываем сечения для образования пар в поле ядра
  SetLength(TempArrEn, NNP);
  SetLength(TempArrMu, NNP);
  F.ReadBuffer(TempArrEn[0], NNP*SizeOf(Double)); // считываем сетку энергий для образования пар в поле ядра
  for i:=0 to FElBerCount-1 do
  begin
    F.ReadBuffer(TempArrMu[0], NNP*SizeOf(Double)); // считываем сетку мю для образования пар в поле ядра для элемента i
    for j:=0 to NNP-1 do
    begin
      FElementsBer[i].FmjuNP[j].E := TempArrEn[j];
      FElementsBer[i].FmjuNP[j].mu := TempArrMu[j];
    end;
  end;

  // считываем сечения для образования пар в поле электрона
  SetLength(TempArrEn, NEP);
  SetLength(TempArrMu, NEP);
  F.ReadBuffer(TempArrEn[0], NEP*SizeOf(Double)); // считываем сетку энергий
  for i:=0 to FElBerCount-1 do
  begin
    F.ReadBuffer(TempArrMu[0], NEP*SizeOf(Double)); // считываем сетку мю
    for j:=0 to NEP-1 do
    begin
      FElementsBer[i].FmjuEP[j].E := TempArrEn[j];
      FElementsBer[i].FmjuEP[j].mu := TempArrMu[j];
    end;
  end;

  // считываем сечения для коггерентного рассеяния и комптона
  SetLength(TempArrEn, NCS);
  SetLength(TempArrMu, NCS);
  SetLength(TempArrMu2, NCS);
  F.ReadBuffer(TempArrEn[0], NCS*SizeOf(Double)); // считываем сетку энергий
  for i:=0 to FElBerCount-1 do
  begin
    F.ReadBuffer(TempArrMu[0], NCS * SizeOf(Double)); // считываем сетку мю
    F.ReadBuffer(TempArrMu2[0], NCS * SizeOf(Double)); // считываем сетку мю
    for j:=0 to NCS-1 do
    begin
      FElementsBer[i].FmjuCS[j].E := TempArrEn[j];
      FElementsBer[i].FmjuCS[j].mu := TempArrMu[j];
      FElementsBer[i].FmjuCm[j].E := TempArrEn[j];
      FElementsBer[i].FmjuCm[j].mu := TempArrMu2[j];
    end;
  end;

  // считываем сечения для фотоэффекта и полного мю
  for i:=0 to FElBerCount-1 do
  begin
    F.ReadBuffer(FElementsBer[i].FGridCountsPh, 1); // считываем число точек в сетке
    FElementsBer[i].FGridCountsTotal := FElementsBer[i].FGridCountsPh;
    SetLength(FElementsBer[i].FmjuPh, FElementsBer[i].FGridCountsPh);
    SetLength(FElementsBer[i].FmjuTotal, FElementsBer[i].FGridCountsTotal);
    SetLength(TempArrEn, FElementsBer[i].FGridCountsPh);
    SetLength(TempArrMu, FElementsBer[i].FGridCountsPh);
    SetLength(TempArrMu2, FElementsBer[i].FGridCountsTotal);

    F.ReadBuffer(TempArrEn[0], FElementsBer[i].FGridCountsPh * SizeOf(Double)); // считываем сетку энергий
    F.ReadBuffer(TempArrMu[0], FElementsBer[i].FGridCountsPh * SizeOf(Double)); // считываем сетку мю
    F.ReadBuffer(TempArrMu2[0], FElementsBer[i].FGridCountsPh * SizeOf(Double)); // считываем сетку мю
    for j:=0 to FElementsBer[i].FGridCountsPh-1 do
    begin
      FElementsBer[i].FmjuPh[j].E := TempArrEn[j];
      FElementsBer[i].FmjuPh[j].mu := TempArrMu[j];
      FElementsBer[i].FmjuTotal[j].E := TempArrEn[j];
      FElementsBer[i].FmjuTotal[j].mu := TempArrMu2[j];
    end;
  end;

  TempArrEn := nil;
  TempArrMu := nil;
  TempArrMu2 := nil;
  F.Free;
end;

function TChemTableMju.LoadElPropFromTxt(const filename: string): Boolean;
var F: TextFile;
    S: string;
    CurEl: TElement;
    i: Integer;
begin
  Result := False;
  // open file
  AssignFile(F, filename);
  {$I-}Reset(F) {$I+};
  if (IOResult <> 0) then
  begin
    //ShowMessage('Error openning file '+FileName);
    exit;
  end;

  // чтение файла и добавление инфо в TElements
  Readln(F, S); // чтение заголовка, пока не используется
  CurEl := TElement.Create();
  While not Eof(F) do
  begin
    Readln(F, S);
    StrToElProp(S, CurEl);
    if (CurEl.Z <= 0) then begin
      CurEl.Free();
      Exit; // error reading
    end;
    
    i := NumFromZ(CurEl.Z);
    if (i >= 0) then
    begin
      TElement(FElements[i]).ElName := CurEl.ElName;
      TElement(FElements[i]).ElNameEn := CurEl.ElNameEn;
      TElement(FElements[i]).ElNameRu := CurEl.ElNameRu;
      TElement(FElements[i]).Rho := CurEl.Rho;
      TElement(FElements[i]).Ma := CurEl.Ma;
    end;
  end;

  Result := True;
  CurEl.Free();
  CloseFile(F);
end;

function TChemTableMju.LoadMassesFromBin(const fileName: string): Boolean;
var F: TFileStream;
    A: array of Single;
    i: Integer;
begin
  Result := False;
  if not FileExists(FileName) then Exit;

  F := TFileStream.Create(FileName, fmOpenRead, fmShareDenyWrite);
  FElBerCount := F.Size div SizeOf(Single);
  SetLength(FElementsBer, FElBerCount);
  SetLength(A, FElBerCount);

  F.ReadBuffer(A[0], FElBerCount*SizeOf(Single));
  
  for i:=0 to FElBerCount-1 do
  begin
    FElementsBer[i].Z := i;
    FElementsBer[i].Ma := A[i];
  end;
  A := nil;
  F.Free;
  Result := True;
end;

constructor TChemTableMju.LoadMjusFromTxtFiles(const DBDirName: string);
var FSearcRec: TSearchRec;
begin
  FElements := TList.Create();
  if FindFirst(DBDirName + '*.*', faAnyFile, FSearcRec) = 0 then
  begin
    if (FSearcRec.Name <> '.') and (FSearcRec.Name <> '..') then
      FElements.Add(TElement.LoadElementFromFile(DBDirName, FSearcRec.Name));
  end;
  while FindNext(FSearcRec) = 0 do
  begin
    if (FSearcRec.Name<>'.') and (FSearcRec.Name<>'..') then
      FElements.Add(TElement.LoadElementFromFile(DBDirName, FSearcRec.Name));
  end;
end;

function TChemTableMju.MjuFromEnergy(ElNum: Byte; E: Double; AbsorbType: Integer = 0): Double;
var i: Integer;
begin
  i := NumFromZ(ElNum);
  if i >= 0 then
    Result := TElement(FElements[ElNum]).getMu(E, AbsorbType)
  else begin
    // raise exception, выводить не в Result, а в отдельную переменную для ошибок
    Result := -1;
    Exit;
  end;
end;

function TChemTableMju.MjuFromEnergy(ElSymb: string; E_MeV: Double; AbsorbType: Integer = 0): Double;
var ElNum: Integer;
begin
  ElNum := NumFromName(ElSymb);
  if (ElNum >= 0) then
    Result := TElement(FElements[ElNum]).getMu(E_MeV, AbsorbType)
  else
  begin
    // raise exception,  выводить не в Result, а в отдельную переменную для ошибок
    Result := -1;
    Exit;
  end;
end;

function TChemTableMju.NumFromName(ElSymb: string): Integer;
var i: Integer;
begin
  Result := -1;
  for i:=0 to FElements.Count-1 do
    if SameText(ElSymb, TElement(FElements[i]).ElSymb) then
    begin
      Result := i;
      Exit;
    end;
end;

function TChemTableMju.NumFromZ(Z: Byte): Integer;
var i: Integer;
begin
  Result := Z-1; // trying to guess
  if (Result < 0) or (Result >= FElements.Count) or  // if we don't guess using dumb search
    (TElement(FElements[Result]).Z <> Z) then
  begin
    Result := -1;
    for i:=0 to FElements.Count-1 do
      if (Z = TElement(FElements[i]).Z) then
      begin
        Result := i;
        Exit;
      end;
  end;
end;

function TChemTableMju.SaveAttenuatToBin(const fileName: string): Boolean;
const
  BERZ = 100; // число элементов у Берлизова в файле (приходиться задавать константой)
  NNP = 55; // число мю для образования пар в поле ядра
  NEP = 51; // число мю для образования пар в поле электрона
  NCS = 80; // число мю для коггерентного рассеяния и комптона
var F: TFileStream;
    TempArrEn, TempArrMu, TempArrMu2: array of Double;
    i, j: Integer;
begin
  Result := False;

  F := TFileStream.Create(fileName, fmCreate, fmshareDenyWrite);

  // записываем сечения для образования пар в поле ядра
  SetLength(TempArrEn, NNP);
  SetLength(TempArrMu, NNP);
  for j:=0 to NNP-1 do TempArrEn[j]:=FElementsBer[0].FmjuNP[j].E;
  F.WriteBuffer(TempArrEn[0], NNP*SizeOf(Double)); // записываем сетку энергий
  for i:=0 to FElBerCount-1 do
  begin
    for j:=0 to NNP-1 do
      TempArrMu[j] := FElementsBer[i].FmjuNP[j].mu; //if FElementsBer[i].FmjuNP[j].mu>0 then TempArrMu[j]:=Log10(FElementsBer[i].FmjuNP[j].mu);//
    F.WriteBuffer(TempArrMu[0], NNP*SizeOf(Double)); // записываем сетку мю
  end;

  // записываем сечения для образования пар в поле электрона
  SetLength(TempArrEn, NEP);
  SetLength(TempArrMu, NEP);
  for j:=0 to NEP-1 do TempArrEn[j] := FElementsBer[0].FmjuEP[j].E;
  F.WriteBuffer(TempArrEn[0], NEP*SizeOf(Double)); // записываем сетку энергий
  for i:=0 to FElBerCount-1 do
  begin
    for j:=0 to NEP-1 do TempArrMu[j] := FElementsBer[i].FmjuEP[j].mu;
    F.WriteBuffer(TempArrMu[0], NEP*SizeOf(Double)); // записываем сетку мю
  end;

  // записываем сечения для коггерентного рассеяния и комптона
  SetLength(TempArrEn,NCS);
  SetLength(TempArrMu,NCS);
  SetLength(TempArrMu2,NCS);
  for j:=0 to NCS-1 do TempArrEn[j]:=FElementsBer[0].FmjuCS[j].E;
  F.WriteBuffer(TempArrEn[0],NCS*SizeOf(Double)); // записываем сетку энергий
  for i:=0 to FElBerCount-1 do 
  begin
    for j:=0 to NCS-1 do
    begin
      TempArrMu[j]:=FElementsBer[i].FmjuCS[j].mu;
      TempArrMu2[j]:=FElementsBer[i].FmjuCm[j].mu;
    end;
    F.WriteBuffer(TempArrMu[0],NCS*SizeOf(Double)); // записываем сетку мю
    F.WriteBuffer(TempArrMu2[0],NCS*SizeOf(Double)); // записываем сетку мю
  end;

  // записываем сечения для фотоэффекта и полного мю
  for i:=0 to FElBerCount-1 do
  begin
    j:=Byte(FElementsBer[i].FGridCountsPh);
    F.WriteBuffer(j,1); // записываем число точек в сетке
    SetLength(TempArrEn,FElementsBer[i].FGridCountsPh);
    SetLength(TempArrMu,FElementsBer[i].FGridCountsPh);
    SetLength(TempArrMu2,FElementsBer[i].FGridCountsTotal);

    for j:=0 to FElementsBer[i].FGridCountsPh-1 do
    begin
      TempArrEn[j] := FElementsBer[i].FmjuPh[j].E;
      TempArrMu[j] := FElementsBer[i].FmjuPh[j].mu;
      TempArrMu2[j] := FElementsBer[i].FmjuTotal[j].mu;
    end;
    F.WriteBuffer(TempArrEn[0], FElementsBer[i].FGridCountsPh*SizeOf(Double)); // считываем сетку энергий
    F.WriteBuffer(TempArrMu[0], FElementsBer[i].FGridCountsPh*SizeOf(Double)); // считываем сетку мю
    F.WriteBuffer(TempArrMu2[0], FElementsBer[i].FGridCountsPh*SizeOf(Double)); // считываем сетку мю
  end;

  TempArrEn:=nil;
  TempArrMu:=nil;
  TempArrMu2:=nil;
  F.Free;
end;

function TChemTableMju.StrToElProp(S: string; CurEl: TElement): Integer;
var i, WC: Integer; // число стобцов в файле
    BR: Boolean;
    CurSep: Char;
begin
  CurSep := DecimalSeparator;
  DecimalSeparator := '.';

  WC := WordCount(S, [#09]);
  if (WC > 0) then
  begin
    BR := TryStrToInt(ExtractWord(1, S, [#09]), i);
    if BR then CurEl.Z := Byte(i) else CurEl.Z := 0;
  end;
  if (WC > 1) then CurEl.ElSymb := ExtractWord(2,S,[#09]) else CurEl.ElSymb := '';
  if (WC > 2) then CurEl.ElName := ExtractWord(3,S,[#09]) else CurEl.ElName := '';
  if (WC > 3) then CurEl.ElNameEn := ExtractWord(4,S,[#09]) else CurEl.ElNameEn := '';
  if (WC > 4) then CurEl.ElNameRu := ExtractWord(5,S,[#09]) else CurEl.ElNameRu := '';
  if (WC > 5) then TryStrToFloat(ExtractWord(6, S, [#09]), CurEl.Rho) else CurEl.Rho := 0;
  if (WC > 6) then TryStrToFloat(ExtractWord(7, S, [#09]), CurEl.Ma) else CurEl.Ma := 0;

  Result := WC;
  DecimalSeparator := CurSep;
end;

{ TMaterial }

procedure TMaterial.addInfoFromTable(const chemTableMu: TChemTableMju);
var i: Integer;
    curElement: TElement;
    temp_md: Double;
begin
  //curElement := nil;
  for i:= 0 to FElements.Count-1 do begin
    temp_md := TElement(FElements[i]).md;
    curElement := chemTableMu.ElFromSymb(TElement(FElements[i]).ElSymb);
    if (curElement <> nil) then begin
      TElement(FElements[i]).Free();
      FElements[i] := TElement.CopyElement(curElement);
      TElement(FElements[i]).Md := temp_md * TElement(FElements[i]).Ma;
    end;
  end;
end;

function TMaterial.asString: string;
var i: Integer;
begin
  Result := FName + #09 + FloatToStr(FRho) + #09 + IntToStr(FElements.Count) + #09 + '[';
  for i:=0 to FElements.Count-1 do begin
    Result := Result + IntToStr(TElement(FElements[i]).Z) + ' '
      + FloatToStr(TElement(FElements[i]).Md) + ' ' + FloatToStr(TElement(FElements[i]).Ma);
    if (i <> FElements.Count - 1) then Result := Result + ';';
  end;
  Result := Result + ']';
end;

constructor TMaterial.Create(const chemTableMu: TChemTableMju; const matName: string;
      aRho: Double; const chemFormula: string);
var i: Integer;
    curElement: TElement;
begin
  // init vars and mem
  FName := matName;
  FRho := aRho;
  FElements := TList.Create();
  //FMDs := TDoubleVector.Create();
  
  // parse Formula = elName_id
  i := 1;
  while (i <= Length(chemFormula)) do begin
    i := parseElement(chemFormula, curElement, i);
    FElements.Add(curElement);
    //FMDs.append(md);
  end;

  // add info from table
  FHasMu := True;
  addInfoFromTable(chemTableMu);
  // recalc weights
  mdNormalize();
end;

constructor TMaterial.Create;
begin
  FHasMu := False;
  FElements := TList.Create();
  //FMDs := TDoubleVector.Create();
end;

destructor TMaterial.Destroy;
var i: Integer;
begin
  for i:=0 to FElements.Count-1 do
    TElement(FElements[i]).Free();
  FElements.Free();
  //FMDs.Free();
  inherited;
end;

procedure TMaterial.fillElements(const aStr: string);
var i: Integer;
    strElement: string;
    Z: Byte;
    Md, Ma: Double;
begin
  for i := 0 to FElNum-1 do
  begin
    // строка с одним элементом и его массовой долею
    strElement := ExtractWord(i+1, aStr, [';']);
    Z := StrToInt(ExtractWord(1, strElement, [' ', ',']));
    Md := StrToFloat(ExtractWord(2, strElement, [' ', ',']));
    Ma := StrToFloat(ExtractWord(3, strElement, [' ', ',']));
    FElements.Add(TElement.Create(Z, Md, Ma));
  end;
end;

constructor TMaterial.loadFromString(const aString: string);
var tmpStr: string;
begin
  FHasMu := False;
  FElements := TList.Create();
  if WordCount(aString, [#09]) < 4 then
    Exit;

  FName := ExtractWord(1, aString, [#09]);
  FRho := StrToFloat(ExtractWord(2, aString, [#09]));
  FElNum := StrToInt(ExtractWord(3, aString, [#09]));
  //setElCount(FElNum);

  // elements
  tmpStr := ExtractWord(4, aString, [#09]);
  if (tmpStr[1] = '[') then Delete(tmpStr, 1, 1);
  if (tmpStr[Length(tmpStr)] = ']') then Delete(tmpStr, Length(tmpStr), 1);
  fillElements(tmpStr); // часть строки с элементами и массовыми долями
end;

function TMaterial.getMu(aEnergy: Double): Double;
var i: Integer;
    sumMuMd, sumMd: Double;
begin
  Result := 0.0;
  if (not FHasMu) then
    Exit;

  sumMuMd := 0.0;
  sumMd := 0;
  for i:=0 to FElements.Count-1 do begin
    sumMuMd := sumMuMd + TElement(FElements[i]).getMu(aEnergy) * TElement(FElements[i]).Md;
    sumMd := sumMd + TElement(FElements[i]).Md;  
  end;

  if (sumMd <> 0) then
    Result := sumMuMd / sumMd;
end;

constructor TMaterial.loadFromString(const aString: string;
  const chemTableMu: TChemTableMju);
begin
  FHasMu := True;
  loadFromString(aString);
  addInfoFromTable(chemTableMu);
end;

procedure TMaterial.mdNormalize;
var i: Integer;
    sumVals: Double; 
begin
  sumVals := 0;
  for i:=0 to FElements.Count-1 do
    sumVals := sumVals + TElement(FElements[i]).Md;

  if (sumVals <> 0) then
    for i:=0 to FElements.Count-1 do
      TElement(FElements[i]).Md := TElement(FElements[i]).Md / sumVals;
end;

function TMaterial.parseElement(const aStr: string; var aElement: TElement;
  idx: Integer): Integer;
var i, n: Integer;
    elName: string;
begin
  elName := '';
  n := 0;
  Result := Length(aStr) + 1;
  for i:= idx to Length(aStr) do begin
    if ((aStr[i] < '0') or (aStr[i] > '9')) and (n = 0) then
      elName := elName + aStr[i]
    else if ((aStr[i] >= '0') and (aStr[i] <= '9')) then
      n := n*10 + StrToInt(aStr[i])
    else begin
      Result := i;
      Break;
    end;
  end;

  if (n = 0) then n := 1;

  aElement := TElement.Create();
  aElement.ElSymb := elName;
  aElement.Md := n;
end;

function TMaterial.calcAbsorption(mu, thick, aRho: Double): Double;
var muRhoT: Double;
begin
  if (aRho = -1) then aRho := FRho;
  muRhoT := mu * aRho * thick;
  if (muRhoT < 300) then
    Result := Exp(-muRhoT)
  else
    Result := 0;
end;

procedure TMaterial.getMuArr(const aEnergyArr: array of Double;
  var aMu: array of Double);
var i: Integer;
begin
  for i:=0 to Length(aEnergyArr)-1 do
    aMu[i] := getMu(aEnergyArr[i]/1000);
end;

{ TMaterialList }

destructor TMaterialList.Destroy;
var i: Integer;
begin
  for i:=0 to FMaterials.Count-1 do
    TMaterial(FMaterials[i]).Free();
  FMaterials.Free();
  inherited;
end;

function TMaterialList.getMatCount: Integer;
begin
  Result := FMaterials.Count;
end;

function TMaterialList.getMaterial(idx: Integer): TMaterial;
begin
  if (idx >= 0) and (idx < FMaterials.Count) then
    Result := TMaterial(FMaterials[idx])
  else
    Result := nil;
end;

constructor TMaterialList.loadMaterialsFromTxt(const fileName: string);
var fileStr: TStringList;
    i: Integer;
begin
  fileStr := TStringList.Create();
  fileStr.LoadFromFile(fileName);
  FMaterials := TList.Create;
  try
    for i:=0 to fileStr.Count-1 do
      if (fileStr[i] <> '') then
        FMaterials.Add(TMaterial.loadFromString(fileStr[i]));
  finally
    fileStr.Free();
  end;
end;

constructor TMaterialList.loadMaterialsFromTxt(const fileName: string;
  const chemTableMu: TChemTableMju);
var fileStr: TStringList;
    i: Integer;
begin
  fileStr := TStringList.Create();
  fileStr.LoadFromFile(fileName);
  FMaterials := TList.Create;
  try
    for i:=0 to fileStr.Count-1 do
      if (fileStr[i] <> '') then
        FMaterials.Add(TMaterial.loadFromString(fileStr[i], chemTableMu));
  finally
    fileStr.Free();
  end;
end;

end.
