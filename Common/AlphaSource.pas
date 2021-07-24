unit AlphaSource;

interface

uses UMaterial;

type
  TSource = class
  private
    FName: String;    // source name
    FIsSquare: Boolean; // round or rectangle source
    FD,               // source diameter
    FA,               // dimension À of rectangle source
    FB: Double;       // dimension B of rectangle source
    FIsThick: Boolean; // is source thick
    FThick: Double;   // source thickness
    FMaterial: TMaterial;
    function getSquare: Double;
  public
    constructor openSource(const fileName: string);
    constructor CopySource(const aSource: TSource);
    procedure saveSource(const fileName: string);
    procedure setDiamFromSquare(aSquare: Double);
    property isSquare: Boolean read FIsSquare write FIsSquare;
    property d: Double read FD write FD;
    property a: Double read FA write FA;
    property b: Double read FB write FB;
    property isThick: Boolean read FIsThick write FIsThick;
    property thick: Double read FThick write FThick;
    property name: string read FName write FName;
    property material: TMaterial read FMaterial write FMaterial;
    property square: Double read getSquare;
  end;

implementation

uses Classes, SysUtils;

{ TSource }

constructor TSource.CopySource(const aSource: TSource);
begin
  FIsSquare := aSource.isSquare;
  FD := aSource.d;
  FA := aSource.a;
  FB := aSource.b;
  FIsThick := aSource.isThick;
  FThick := aSource.thick;
  FName := aSource.name;
  FMaterial := TMaterial.Copy(aSource.FMaterial);
end;

function TSource.getSquare: Double;
begin
  if (FIsSquare) then
    Result := FA * FB
  else
    Result := Pi * FD*FD / 4;
end;

constructor TSource.openSource(const fileName: string);
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
      FD := StrToFloat(CurList.Values['Radius,mm'])
    else begin
      FA := StrToFloat(CurList.Values['Side A,mm']);
      FB := StrToFloat(CurList.Values['Side B,mm']);
    end;
    FIsThick := StrToBoolDef(CurList.Values['IsThick'], False);
    if FIsThick then
    begin
      FThick := StrToFloat(CurList.Values['Thick,mm']);
      FMaterial := TMaterial.loadFromString(CurList.Values['Material']);
    end;
  finally
    CurList.Free;
    DecimalSeparator := CurDecSep;
  end;
end;

procedure TSource.saveSource(const fileName: string);
var CurList: TStringList;
    CurDecSep: Char;
begin
  CurList := TStringList.Create;
  CurDecSep := DecimalSeparator;
  DecimalSeparator := '.';

  CurList.Add('Name=' + FName);
  CurList.Add('IsSquare=' + BoolToStr(FIsSquare));
  if not FIsSquare then
    CurList.Add('Radius,mm=' + FloatToStr(FD))
  else begin
    CurList.Add('Side A,mm=' + FloatToStr(FA));
    CurList.Add('Side B,mm=' + FloatToStr(FB));
  end;
  CurList.Add('IsThick=' + BoolToStr(FIsThick));
  if FIsThick then
  begin
    CurList.Add('Thick,mm=' + FloatToStr(FThick));
    CurList.Add('Material=' + FMaterial.asString());
  end;
  CurList.SaveToFile(fileName);
  DecimalSeparator := CurDecSep;
  CurList.Free;
end;

procedure TSource.setDiamFromSquare(aSquare: Double);
begin
  if (not FIsSquare) then
    FD := Sqrt(4 * aSquare / Pi);
end;

end.
