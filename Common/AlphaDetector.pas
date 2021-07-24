unit AlphaDetector;

interface

uses AlphaMaterial;

type
  TDet = class
  private
    FIsSquare: Boolean; // round or rectangle detector
    FD,               // detector diameter
    FA,               // dimension À of rectangle detector
    FB,               // dimension B of rectangle detector
    FSunkZ,           // depth to which the detector is sunk
    FSunkXY: Double;  // offset for sunk from detector edges
    FIsThick: Boolean; // is detector thick
    FThick: Double;   // detector thickness
    FDetSelfEfficiency: Double; // detector self efficiency
    FName: String;
    FMaterial: TMaterial; // detector material
    function getSquare: Double;
  public
    constructor openDetector(const fileName: string);
    constructor CopyDetector(const aDetector: TDet);
    procedure saveDetector(const fileName: string);
    procedure setDiamFromSquare(aSquare: Double);
    property isSquare: Boolean read FIsSquare write FIsSquare;
    property detDiam: Double read FD write FD;
    property a: Double read FA write FA;
    property b: Double read FB write FB;
    property sunkZ: Double read FSunkZ write FSunkZ;
    property sunkXY: Double read FSunkXY write FSunkXY;
    property isThick: Boolean read FIsThick write FIsThick;
    property thick: Double read FThick write FThick;
    property name: string read FName write FName;
    property square: Double read getSquare;
    property material: TMaterial read FMaterial write FMaterial;
    property detSelfEfficiency: Double read FDetSelfEfficiency write FDetSelfEfficiency;
  end;

implementation

uses Classes, SysUtils;

{ TDet }

constructor TDet.CopyDetector(const aDetector: TDet);
begin
  FIsSquare := aDetector.isSquare;
  FD := aDetector.detDiam;
  FA := aDetector.a;
  FB := aDetector.b;
  FSunkZ := aDetector.sunkZ;
  FSunkXY := aDetector.sunkXY;
  FIsThick := aDetector.isThick;
  FThick := aDetector.thick;
  FName := aDetector.name;
  FMaterial := TMaterial.Copy(aDetector.FMaterial);
  FDetSelfEfficiency := aDetector.detSelfEfficiency;
end;

function TDet.getSquare: Double;
begin
  if (FIsSquare) then
    Result := FA * FB
  else
    Result := Pi * FD*FD / 4;
end;

constructor TDet.openDetector(const fileName: string);
var CurList: TStringList;
    CurDecSep: Char;
begin
  CurList := TStringList.Create;
  CurDecSep := DecimalSeparator;
  DecimalSeparator := '.';
  try
    CurList.LoadFromFile(fileName);
    FName := CurList.Values['Name'];
    FIsSquare := StrToBoolDef(CurList.Values['IsSquare'], False);
    if not FIsSquare then
    begin
      FD := StrToFloat(CurList.Values['Diameter,mm']);
    end
    else begin
      FA := StrToFloat(CurList.Values['Side A,mm']);
      FB := StrToFloat(CurList.Values['Side B,mm']);
    end;
    FSunkZ := StrToFloatDef(CurList.Values['Sunk Z,mm'], 0);
    FSunkXY := StrToFloatDef(CurList.Values['Sunk XY,mm'], 0);
    FIsThick := StrToBoolDef(CurList.Values['IsThick'], False);
    if FIsThick then
    begin
      FThick := StrToFloat(CurList.Values['Thick,mm']);
      FMaterial := TMaterial.loadFromString(CurList.Values['Material']);
    end;
    FDetSelfEfficiency := StrToFloatDef(CurList.Values['Det efficiency'], 1);
  finally
    CurList.Free;
    DecimalSeparator := CurDecSep;
  end;
end;

procedure TDet.saveDetector(const fileName: string);
var CurList: TStringList;
    CurDecSep: Char;
begin
  CurList := TStringList.Create;
  CurDecSep := DecimalSeparator;
  DecimalSeparator := '.';

  CurList.Add('Name=' + FName);
  CurList.Add('IsSquare=' + BoolToStr(FIsSquare));
  if not FIsSquare then
  begin
    CurList.Add('Diameter,mm=' + FloatToStr(FD));
  end
  else begin
    CurList.Add('Side A,mm=' + FloatToStr(FA));
    CurList.Add('Side B,mm=' + FloatToStr(FB));
  end;
  CurList.Add('Sunk Z,mm=' + FloatToStr(FSunkZ));
  CurList.Add('Sunk XY,mm=' + FloatToStr(FSunkXY));
  CurList.Add('IsThick=' + BoolToStr(FIsThick));
  if FIsThick then
  begin
    CurList.Add('Thick,mm=' + FloatToStr(FThick));
    CurList.Add('Material=' + FMaterial.asString());
  end;
  CurList.Add('Det efficiency=' + FloatToStr(FDetSelfEfficiency));

  CurList.SaveToFile(fileName);
  DecimalSeparator := CurDecSep;
  CurList.Free;
end;

procedure TDet.setDiamFromSquare(aSquare: Double);
begin
  if (not FIsSquare) then
    FD := Sqrt(4 * aSquare / Pi);
end;

end.
