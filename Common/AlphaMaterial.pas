unit AlphaMaterial;

interface

uses Classes;

const
  RHO_AIR = 1.2047e-3; // air density g/cm3 at Ò=20C

type
  TElement = record // chemical element 
    Z: Byte;        // charge number
    M: Double;      // atom mass
    Md: Double;     // mass fraction in compound
  end;

  TMaterial = class
  private
    FName: string;    // material name
    FRho: Double;     // material density in g/cm3
    FElNum: Integer;  // chem elements num in material
    procedure setElCount(const Value: Integer);  // elements in array
    procedure fillElements(aStr: string);
  public
    FElements: array of TElement;
    constructor loadFromString(const aString: string); // matName Rho ElNum [z1 md1 M; z2 md2 M; ...]
    constructor CreateAirDefault(atmPressure: Double = 1);
    constructor Copy(aMaterial: TMaterial);
    destructor Destroy(); override;
    function asString(): string;
    property name: string read FName write FName;
    property rho: Double read FRho write FRho;
    property elCount: Integer read FElNum write setElCount;
  end;

  TMaterialList = class  // materials database
  private
    function getMatCount: Integer;
    function getMaterial(idx: Integer): TMaterial;
  public
    FMaterials: TList;  // material list
    constructor LoadMaterialsFromTxt(const fileName: string);  // load materials from text database
    destructor Destroy(); override;
    property matCount: Integer read getMatCount;
    property material[idx: Integer]: TMaterial read getMaterial; default;
  end;
  

implementation

uses SysUtils, JvJCLUtils;

{ TMaterial }

function TMaterial.asString: string;
var i: Integer;
begin
  Result := FName + #09 + FloatToStr(FRho) + #09 + IntToStr(FElNum) + #09 + '[';
  for i:=0 to FElNum-1 do begin
    Result := Result + IntToStr(FElements[i].Z) + ' ' + FloatToStr(FElements[i].Md) +
      ' ' + FloatToStr(FElements[i].M);
    if (i <> FElNum - 1) then Result := Result + ';';
  end;
  Result := Result + ']';
end;

constructor TMaterial.Copy(aMaterial: TMaterial);
var i: Integer;
begin
  if (Assigned(aMaterial)) then begin
    FName := aMaterial.name;
    FRho := aMaterial.rho;
    setElCount(aMaterial.elCount);
    for i:=0 to FElNum-1 do
      FElements[i] := aMaterial.FElements[i];
  end;
end;

constructor TMaterial.CreateAirDefault(atmPressure: Double = 1);
begin
  FName := 'Air';
  FRho := RHO_AIR * atmPressure; // air density corrected on pressure
  setElCount(4);
  FElements[0].Z :=  6; FElements[0].Md := 0.000124; FElements[0].M := 12;
  FElements[1].Z :=  7; FElements[1].Md := 0.755267; FElements[1].M := 14;
  FElements[2].Z :=  8; FElements[2].Md := 0.231781; FElements[2].M := 16;
  FElements[3].Z := 18; FElements[3].Md := 0.012827; FElements[3].M := 39.95;
end;

destructor TMaterial.Destroy;
begin
  FElements := nil;  
  inherited;
end;

procedure TMaterial.fillElements(aStr: string);
var i: Integer;
    strElement: string;
begin
  for i := 0 to FElNum-1 do
  begin
    strElement := ExtractWord(i+1, aStr, [';']);
    FElements[i].Z := StrToInt(ExtractWord(1, strElement, [' ', ',']));
    FElements[i].Md := StrToFloat(ExtractWord(2, strElement, [' ', ',']));
    FElements[i].M := StrToFloat(ExtractWord(3, strElement, [' ', ',']));
  end;
end;

constructor TMaterial.loadFromString(const aString: string);
var tmpStr: string;
begin
  if WordCount(aString, [#09]) < 4 then
    Exit;

  FName := ExtractWord(1, aString, [#09]);
  FRho := StrToFloat(ExtractWord(2, aString, [#09]));
  FElNum := StrToInt(ExtractWord(3, aString, [#09]));
  setElCount(FElNum);

  // elements
  tmpStr := ExtractWord(4, aString, [#09]);
  if (tmpStr[1] = '[') then Delete(tmpStr, 1, 1);
  if (tmpStr[Length(tmpStr)] = ']') then Delete(tmpStr, Length(tmpStr), 1);
  fillElements(tmpStr);
end;

procedure TMaterial.setElCount(const Value: Integer);
begin
  if FElNum >=0 then
    FElNum := Value
  else
    FElNum := 0;

  SetLength(FElements, FElNum);
end;


{ TMaterialList }

destructor TMaterialList.Destroy;
var i: Integer;
begin
  if (FMaterials.Count > 0) then
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

constructor TMaterialList.LoadMaterialsFromTxt(const fileName: string);
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


end.
