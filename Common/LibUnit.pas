unit LibUnit;

interface

uses Classes;

const
  RESWINDOW = 1;
  DAYS_IN_YEAR = 365.25;

type
  TWidth = record // Lorenz width
    NuclideName: string; // for convinient
    LineEnergy,
    NatWidth: Double;  // Natural or Lorenz width
  end;

  TLine = record // nuclide line
    Energy, FdEnergy: Double;
    I, dI: Double;   // x
    LineType: Char;
    IsProcessed: Boolean;
    GammaConst: Double;
    Width: Double;
  end;
  PLine = ^TLine;

  TNuclide = class
    Name: string;       // full nuclide name
    Element: string;    // chemical element name
    MassNum: Integer;   // Mass number
    FHalfLife: Double;  // half life
    FHalfYear: Double;  // half-life in years
    FHLUnits: string;   // half-life units
    GammaConst: Double; // gamma-const -- Radiation exposure dose rate, created by gamma-radiation from point source with given of radiactive nuclide with activity 1 uCi at 1cm distance
    Lines: array of TLine; // lines array
  private
    FLinesCount: Integer;
    procedure setLinesCount(const Value: Integer); // lines count
    procedure deleteFromArray(dIndex: Integer);
  public
    class procedure NameToElementMass(const Name: string; var Element: string; var A: Integer); // преобразует имя нуклида в элемент и массовое число
    procedure convertUnitsToEng();
    procedure sumEqualLines();  // sum equals lines in library
    procedure incLines();
    procedure decLines();
    procedure addLine(const aLine: TLine);
    procedure deleteLine(lineNum: Integer);
    procedure deleteAllLines();
    procedure sortLines();      // sort lines on energy
    procedure deleteLines(const indexes: array of Integer);
    function libNuclLine2Text(lineNumber: Integer; isNewFormat: Boolean): string; // nuclide line -> string
    function activityToMass(act: Double): Double;
    function massToActivity(mass: Double): Double;
    property linesCount: Integer read FLinesCount write setLinesCount;
  end;
  //PNuclide = ^TNuclide;

  TLibrary = class(TObject)
    //Nuclides: array of TNuclide; // список линий
    Widthes: array of TWidth;
  private
    FName: string;    // library name
    FPath: string;    // path to library
    FShapka: string;  // header with library version (1.0 or 1.5)
    FNuclidesCount: Integer; // number of nuclides in library
    FIsNewFormat: Boolean; // library format (with or without uncertainties)
    FNuclides: TList;   // array of TNuclides
    FVersion: Integer;  // library version
    class function unitsFromString(const S: string): string;
    class function AddDash2Name(nuclideName: string): string;
    function OpenLib1(const FileName: string): Boolean;
    function OpenLib15(const FileName: string): Boolean;
    procedure SaveLib1(const FileName: string);
    procedure SaveLib15(const FileName: string);
    procedure AddWidthToLine(ALine: PLine);
    function getNuclide(i: Integer): TNuclide;
    function getNuclideFromName(const nucName: string): TNuclide;
  public
    destructor Destroy; override;
    // save-open
    function OpenLib(const FileName: string): Boolean;
    function OpenTabLib(const FileName: string): Boolean; // open tab-separated library
    function OpenShortTabLib(const FileName: string): Boolean; // open tab-separated library with only half-life times
    procedure SaveLib(const FileName: string); // save library in lsrm-format
    procedure SaveTabLib(const FileName: string); // save tab-separated library
    procedure SaveShortTabLib(const FileName: string); // save tab-separated library with only half-life times
    // serialisation
    function LibNuclNames2Text(): string;  // convert library to text
    // operations with lib
    procedure ConvertLibToEng(); // convert russian hl units to eng
    procedure DefaultLibFill(); 
    procedure GetListOfNuclides(var NucList: TStringList); // returns nuclide list
    // operation for nuclide
    function AddNuclide(const aNucName: string; aHalfTime: Double): integer; // adding nuclide to list, return its number
    procedure DeleteNuclide(NucNum: integer); 
    function IsNuclideInLib(NucName: string): Boolean;
    function ThalfFromName(const nucName: string): Double; // returns halflife time from nuclide name or -1
    function NucNumFromName(const ANuclideName: string): Integer; // returns nuc number from it's name. returns -1 if there is no such nuclide
    // sort
    procedure SortLibName; // sort library by nuclide name
    procedure SortLibA; // sort library by mass number
    procedure SortLibLines; // sort lines by energies
    // operations with nuc lines
    procedure CopyNucLines(NucFromNum, NucToNum: Integer); overload;
    procedure CopyNucLines(NucFromNum, NucToNum: Integer; Factor: Double); overload; // multiply nuclide lines intensities and unc. by factor
    function IntensityFromEnergy(const NucName: string; energy: Double; var dI: Double; dWindow: Double = RESWINDOW): Double;
    procedure mergeNuclideLines(nucNum: Integer; const linesIndexes: array of Integer);
    // other operations
    function RecalcActivityToDate(NuclideName: string; OldAct: Double; OldDate, NewDate: TDateTime): Double;
    function ConvertNuclides2EffmakerString(const ANucIndexes: array of Integer;
      const AActivities: array of Double): string;
    procedure LoadWidthes(const filename: string);
    procedure SetWidthes();
    procedure MergeCloseLines(NucIndex: Integer; F, E: Double); // merge small lines that standes close to big lines
    function activityToMass(nucName: string; act: Double): Double;
    function massToActivity(nucName: string; mass: Double): Double;
    //properties
    property Name: string read FName write FName;
    property Path: string read FPath write FPath;
    property Shapka: string read FShapka;
    property NuclidesCount: Integer read FNuclidesCount;
    property isNewFormat: Boolean read FIsNewFormat write FIsNewFormat;
    property Nuclide[i: Integer]: TNuclide read getNuclide;
  end;

function AddDash2Name(NuclideName: string): string;
function DeleteDashFromName(NuclideName: string): string;
function Ci2Bq(CiAct: Double): Double;
function Bq2Ci(BqAct: Double): Double;

implementation

uses Math, SysUtils, Windows, JvJCLUtils,
  MetrologRound;

{ TNuclide }

function TNuclide.activityToMass(act: Double): Double;
const SEC_PER_YEAR = 31557600;
      NA = 6.022E+023;
begin
  Result := act * (FHalfLife * SEC_PER_YEAR)/ln(2) * MassNum/NA;
end;

procedure TNuclide.addLine(const aLine: TLine);
begin
  incLines();
  Lines[FLinesCount-1] := aLine;
end;

procedure TNuclide.convertUnitsToEng;
begin
  if CompareText(FHLUnits, 'Лет') = 0 then
    FHLUnits := 'Year'
  else if CompareText(FHLUnits, 'Сут') = 0 then
    FHLUnits := 'Day'
  else if CompareText(FHLUnits, 'Час') = 0 then
    FHLUnits := 'Hour'
  else if CompareText(FHLUnits, 'Сек') = 0 then
    FHLUnits := 'Sec';
end;


procedure TNuclide.decLines;
begin
  if (FLinesCount > 0) then begin
    Dec(FLinesCount);
    SetLength(Lines, FLinesCount);
  end;
end;

procedure TNuclide.deleteAllLines;
begin
  Lines := nil;
  FLinesCount := 0;
end;

procedure TNuclide.deleteFromArray(dIndex: Integer);
var i: Integer;
begin
  if (dIndex < FLinesCount-1) then
    for i:=dIndex to FLinesCount-2 do
      Lines[i] := Lines[i+1];

  decLines();
end;

procedure TNuclide.deleteLine(lineNum: Integer);
var i: Integer;
begin
  if (lineNum >= FLinesCount) then Exit;
  
  // if nuclide has only 1 line
  if (FLinesCount <= 1) then begin
    deleteAllLines();
    Exit;
  end;

  // shift array
  for i := lineNum to FLinesCount-2 do
    Lines[i] := Lines[i+1];

  // set new lengthes
  decLines();
end;

procedure TNuclide.deleteLines(const indexes: array of Integer);
var i, j: Integer;
begin
  // delete lines
  for i:=Length(indexes)-1 downto 0 do begin
    // shift array
    for j := indexes[i] to FLinesCount-2 do
      Lines[j] := Lines[j+1];
  end;

  // set new length
  FLinesCount := FLinesCount - Length(indexes);
  if (FLinesCount < 0) then FLinesCount := 0;
  SetLength(Lines, FLinesCount);  
end;

procedure TNuclide.incLines;
begin
  Inc(FLinesCount);
  SetLength(Lines, FLinesCount);
end;

function TNuclide.libNuclLine2Text(lineNumber: Integer; isNewFormat: Boolean): string;
var j: Integer;
    S, St: string;
    CurDecSep: Char;
    FormatSettings: TFormatSettings;
begin
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FormatSettings);
  FormatSettings.DecimalSeparator := '.';
  CurDecSep := DecimalSeparator;
  DecimalSeparator := '.';
  j := lineNumber;

  if (isNewFormat) then
  begin
    // energy
    St := metrolRound(Lines[j].Energy, Lines[j].FdEnergy, FormatSettings);
    S := MakeStr(' ', 6 - Pos('.', St)) + St + MakeStr(' ', 6 - Length(St) + Pos('.', St));
    // energy unc
    St := metrolRound(Lines[j].FdEnergy, FormatSettings);
    S := S + MakeStr(' ', 6 - Pos('.', St)) + St + MakeStr(' ', 6 - Length(St) + Pos('.', St));
  end
  else begin
    // energy
    St := FormatFloat('0.000##', Lines[j].Energy);
    S := MakeStr(' ', 6 - Pos('.', St)) + St + MakeStr(' ', 6 - Length(St) + Pos('.', St));
  end;

  // Intensity
  St := metrolRound(Lines[j].i, Lines[j].dI, FormatSettings);
  S := S + MakeStr(' ', 6 - Pos('.', St)) + St + MakeStr(' ', 6 - Length(St) + Pos('.', St));
  // intensity unc.
  St := metrolRound(Lines[j].dI, FormatSettings);
  S := S + MakeStr(' ', 6 - Pos('.', St)) + St;
  // line type
  if Lines[j].LineType <> #0 then
    S := S + ' ' + Lines[j].LineType;
  // is consider line
  if not (Lines[j].IsProcessed) then
    S := S + ' n';

  Result := S;

  DecimalSeparator := CurDecSep;
end;

function TNuclide.massToActivity(mass: Double): Double;
const SEC_PER_YEAR = 31557600;
      NA = 6.022E+023;
begin
  Result := mass * NA/MassNum * ln(2) / (FHalfLife * SEC_PER_YEAR);
end;

class procedure TNuclide.NameToElementMass(const Name: string;
  var Element: string; var A: Integer);
var S: string;
begin
  if WordCount(Name, ['-']) > 1 then
  begin
    Element := ExtractWord(1, Name, ['-']);
    S := ExtractWord(2, Name, ['-']);
    if (SameText(S[Length(S)], 'm')) then // metastable
      Delete(S, Length(S), 1);

    A := StrToIntDef(S, 999);
  end
  else begin
    Element := Name;
    A := 999;
  end;
end;

procedure TNuclide.setLinesCount(const Value: Integer);
begin
  if (Value >= 0) then
    FLinesCount := Value
  else
    FLinesCount := 0;

  SetLength(Lines, FLinesCount);
end;

procedure TNuclide.sortLines;
var i, j : Integer;
    tmpLine: TLine;
begin
  for i := 1 to FLinesCount-1 do
  begin
    tmpLine := Lines[i];
    j := i - 1;
    while tmpLine.Energy < Lines[j].Energy do
    begin
      Lines[j+1] := Lines[j];
      Dec(j);
      if j < 0 then
        Break;
    end;
    Lines[j+1] := tmpLine;
  end;
end;

procedure TNuclide.sumEqualLines;
var i: Integer;
begin
  for i:=0 to FLinesCount-2 do
    if Abs(Lines[i].Energy - Lines[i+1].Energy) < 0.001 then begin
      Lines[i].I := Lines[i].I + Lines[i+1].I;
      deleteFromArray(i+1);
    end;
end;

{ TLibrary }

function TLibrary.OpenLib(const FileName: string): Boolean;
var CurDecSep: Char;
    F: TextFile;
    S, S1, libVersion: string;
begin
  CurDecSep := DecimalSeparator;
  DecimalSeparator := '.';
  Result := False;
  FNuclidesCount := 0;
  FPath := FileName;

  AssignFile(F, FileName);
  {$I-}Reset(F) {$I+};
  if IOResult <> 0 then
  begin
    exit;
  end;

  readln(F, S);
  S := Trim(S);
  CloseFile(F);
  if (S[1] = '<') then // new library version
  begin
    FShapka := S;
    S1 := ExtractWord(WordCount(S, [' ']), S, [' ']); // try to find version
    if (S1[Length(S1)] = '>') then
      Delete(S1, Length(S1), 1);
    libVersion := ExtractWord(2, S1, ['=']);
    if SameText(libVersion, '1.5') or SameText(libVersion, '1.6') or SameText(libVersion, '1.7') then
      Result := OpenLib15(FileName);
  end
  else 
    Result := OpenLib1(FileName);

  DecimalSeparator := CurDecSep;
end;

function TLibrary.OpenLib1(const FileName: string): Boolean;
var F: TextFile;
    S: string;
    Shift, lastN: Integer;
    TempCh1, TempCh2: Char;
    TempCount: Integer;
    curNuclide: TNuclide;
begin
  Result := False;
  FNuclidesCount := 0;
  FPath := FileName;
  if Assigned(FNuclides) then  FreeAndNil(FNuclides);
  FNuclides := TList.Create();

  AssignFile(F, FileName);
  {$I-}Reset(F) {$I+};
  if IOResult <> 0 then
  begin
      //ShowMessage('Error openning file '+FileName);
    exit;
  end;

  While (not EoF(F)) do
  begin
    readln(F, S);
    S := Trim(S);
    if (S <> '') then
      if S[1] in ['A'..'z'] then // nuclide name
      begin
        Shift := 0;
        Inc(FNuclidesCount);
        TempCount := WordCount(S, [' ']);
        curNuclide := TNuclide.Create();
        curNuclide.Name := ExtractWord(1, S, [' ']);
        TNuclide.NameToElementMass(curNuclide.Name, curNuclide.Element, curNuclide.MassNum);
        if (TempCount > 1) then
        begin
          curNuclide.FHalfLife := JvSafeStrToFloat(ExtractWord(1, ExtractWord(2, S, [' ']), ['(']));
          curNuclide.FHLUnits := UnitsFromString(ExtractWord(2, S, [' ']));
          if curNuclide.FHLUnits = '' then
          begin
            curNuclide.FHLUnits := UnitsFromString(ExtractWord(3, S, [' ']));
            Shift := Shift + 1;
          end;
          if (TempCount >= (3 + Shift)) then
            curNuclide.GammaConst := JvSafeStrToFloat(ExtractWord(3 + Shift, S, [' ']))
          else
            curNuclide.GammaConst := 0;
          curNuclide.linesCount := 0;
        end
        else
        begin
          curNuclide.FHalfLife := JvSafeStrToFloat(ExtractWord(1, ExtractWord(2, S, [' ']), ['(']));
          curNuclide.FHLUnits := UnitsFromString(ExtractWord(2, S, [' ']));
        end;
        FNuclides.Add(curNuclide);
      end
      else if S[1] in ['0'..'9'] then // line of current nuclide
      begin
        if (FNuclidesCount > 0) then 
        begin
          curNuclide.incLines();
          lastN := curNuclide.linesCount-1;
          curNuclide.Lines[lastN].Energy := JvSafeStrToFloat(ExtractWord(1, S, [' ']));
          curNuclide.Lines[lastN].I := JvSafeStrToFloat(ExtractWord(2, S, [' ']));
          curNuclide.Lines[lastN].dI := JvSafeStrToFloat(ExtractWord(3, S, [' ']));
          curNuclide.Lines[lastN].IsProcessed := True;

          if (WordCount(S, [' ']) > 3) then
          begin
            TempCh1 := ExtractWord(4, S, [' '])[1];
            if (TempCh1 = 'n') then
              curNuclide.Lines[lastN].IsProcessed := False
            else
              curNuclide.Lines[lastN].LineType := TempCh1; // X-ray or annihl
            if WordCount(S, [' ']) > 4 then
            begin
              TempCh2 := ExtractWord(5, S, [' '])[1];
              if (TempCh2 = 'n') then
                curNuclide.Lines[lastN].IsProcessed := False
              else
                curNuclide.Lines[lastN].LineType := TempCh2;
            end;
          end;
        end
        else
        begin
          //raise Error
          // exit
        end;
      end
      else
      begin
        // raise Error
        //Exit;
      end;
  end;

  CloseFile(F);
  Result := True;
end;

function TLibrary.OpenLib15(const FileName: string): Boolean;
var F: TextFile;
    S: string;
    Shift, lastN: Integer;
    TempCh1, TempCh2: Char;
    TempCount: Integer;
    curNuclide: TNuclide;
begin
  Result := False;
  FNuclidesCount := 0;
  FPath := FileName;
  IsNewFormat := True;
  if Assigned(FNuclides) then FNuclides.Free;
  FNuclides := TList.Create();

  AssignFile(F, FileName);
  {$I-}Reset(F) {$I+};
  if IOResult <> 0 then
  begin
      //ShowMessage('Error openning file '+FileName);
    exit;
  end;

  readln(F, S); // header reading
  While (not EoF(F)) do
  begin
    readln(F, S);
    S := Trim(S);
    if S <> '' then
      if S[1] in ['A'..'z'] then // nuclide name
      begin
        Shift := 0;
        Inc(FNuclidesCount);
        TempCount := WordCount(S, [' ']);
        curNuclide := TNuclide.Create();
        curNuclide.Name := ExtractWord(1, S, [' ']);
        TNuclide.NameToElementMass(curNuclide.Name, curNuclide.Element, curNuclide.MassNum);
        if TempCount > 1 then
        begin
          curNuclide.FHalfLife := JvSafeStrToFloat(ExtractWord(1, ExtractWord(2, S, [' ']), ['(']));
          curNuclide.FHLUnits := UnitsFromString(ExtractWord(2, S, [' ']));
          if curNuclide.FHLUnits = '' then
          begin
            curNuclide.FHLUnits := UnitsFromString(ExtractWord(3, S, [' ']));
            Shift := Shift + 1;
          end;
          if (TempCount >= (3 + Shift)) then
            curNuclide.GammaConst := JvSafeStrToFloat(ExtractWord(3 + Shift, S, [' ']))
          else
            curNuclide.GammaConst := 0;
          curNuclide.linesCount := 0;
        end
        else
        begin
          curNuclide.FHalfLife := JvSafeStrToFloat(ExtractWord(1, ExtractWord(2, S, [' ']), ['(']));
          curNuclide.FHLUnits := UnitsFromString(ExtractWord(2, S, [' ']));
        end;
        FNuclides.Add(curNuclide);
      end
      else if S[1] in ['0'..'9'] then
      begin
        if (FNuclidesCount > 0) then
        begin
          curNuclide.incLines();
          lastN := curNuclide.linesCount-1;
          curNuclide.Lines[lastN].Energy := JvSafeStrToFloat(ExtractWord(1, S, [' ']));
          curNuclide.Lines[lastN].FdEnergy := JvSafeStrToFloat(ExtractWord(2, S, [' ']));
          curNuclide.Lines[lastN].I := JvSafeStrToFloat(ExtractWord(3, S, [' ']));
          curNuclide.Lines[lastN].dI := JvSafeStrToFloat(ExtractWord(4, S, [' ']));
          curNuclide.Lines[lastN].IsProcessed := True;
          if WordCount(S, [' ']) > 4 then
          begin
            TempCh1 := ExtractWord(5, S, [' '])[1];
            if (TempCh1 = 'n') then
              curNuclide.Lines[lastN].IsProcessed := False
            else
              curNuclide.Lines[lastN].LineType := TempCh1; // X-ray or annihl
            if WordCount(S, [' ']) > 5 then
            begin
              TempCh2 := ExtractWord(6, S, [' '])[1];
              if (TempCh2 = 'n') then
                curNuclide.Lines[lastN].IsProcessed := False
              else
                curNuclide.Lines[lastN].LineType := TempCh2; 
            end;
          end;
        end
        else
        begin
          //raise Error
          // exit
        end;
      end
      else // какая служебная информация, игнорировать
      begin
        // raise Error
        //Exit;
      end;
  end;

  CloseFile(F);
  Result := True;
end;

function TLibrary.OpenTabLib(const FileName: string): Boolean;
var F: TextFile;
    S: string;
    Shift, lastN: Integer;
    TempCh1, TempCh2: Char;
    CurDecSep: Char;
    curNuclide: TNuclide;
begin
  CurDecSep := DecimalSeparator;
  DecimalSeparator := '.';
  Result := False;
  FPath := FileName;
  FNuclidesCount := 0;
  if Assigned(FNuclides) then FNuclides.Free();
  FNuclides := TList.Create();

  AssignFile(F,FileName);
  {$I-}Reset(F) {$I+};
  if IOResult<>0 then
    begin
      //ShowMessage('Error openning file '+FileName);
      exit;
    end;

  FIsNewFormat := True; // always new format
  While (not EoF(F)) do
  begin
    readln(F, S);
    S := Trim(S);
    if S <> '' then
      if S[1] in ['A'..'z'] then
      begin
        Shift := 0;
        Inc(FNuclidesCount);
        curNuclide := TNuclide.Create();
        curNuclide.Name := ExtractWord(1, S, [#9]);
        TNuclide.NameToElementMass(curNuclide.Name, curNuclide.Element, curNuclide.MassNum);
        curNuclide.FHalfLife := JvSafeStrToFloat(ExtractWord(1, ExtractWord(2, S, [#9]), ['(']));
        curNuclide.FHLUnits := UnitsFromString(ExtractWord(2, S, [#9]));
        if curNuclide.FHLUnits = '' then
        begin
          curNuclide.FHLUnits := UnitsFromString(ExtractWord(3, S, [#9]));
          Shift := Shift + 1;
        end;
        if (WordCount(S, [#9]) >= (3 + Shift)) then
          curNuclide.GammaConst := JvSafeStrToFloat(ExtractWord(3 + Shift, S, [#9]));
        if curNuclide.FHLUnits = '' then
          curNuclide.FHLUnits := 'sec';
        FNuclides.Add(curNuclide);
      end
      else if S[1] in ['0'..'9'] then
      begin
        if FNuclidesCount > 0 then
        begin
          curNuclide.incLines();
          lastN := curNuclide.linesCount-1;
          curNuclide.Lines[lastN].Energy := JvSafeStrToFloat(ExtractWord(1, S, [#9]));
          curNuclide.Lines[lastN].FdEnergy := JvSafeStrToFloat(ExtractWord(2, S, [#9]));
          curNuclide.Lines[lastN].I := JvSafeStrToFloat(ExtractWord(3, S, [#9]));
          curNuclide.Lines[lastN].dI := JvSafeStrToFloat(ExtractWord(4, S, [#9]));
          curNuclide.Lines[lastN].IsProcessed := True;
          if WordCount(S, [#9]) > 4 then
          begin
            TempCh1 := ExtractWord(4, S, [#9])[1];
            if (TempCh1 = 'n') then
              curNuclide.Lines[lastN].IsProcessed := False
            else
              curNuclide.Lines[lastN].LineType := TempCh1; // X-ray or Annihl
            if WordCount(S, [#9]) > 4 then
            begin
              TempCh2 := ExtractWord(5, S, [#9])[1];
              if (TempCh2 = 'n') then
                curNuclide.Lines[lastN].IsProcessed := False
              else
                curNuclide.Lines[lastN].LineType := TempCh2; 
            end;
          end;
        end
        else
        begin
          //raise Error
          // exit
        end;
      end
      else
      begin
        // raise Error
        //Exit;
      end;
  end;

  CloseFile(F);
  DecimalSeparator := CurDecSep;
  Result := True;
end;

function TLibrary.OpenShortTabLib(const FileName: string): Boolean;
var F: TextFile;
    S: string;
    CurDecSep: Char;
    curNuclide: TNuclide;
    CurUnit: string;
begin
  CurDecSep := DecimalSeparator;
  DecimalSeparator := '.';
  Result := False;
  FPath := FileName;
  FNuclidesCount := 0;
  if Assigned(FNuclides) then
    FNuclides.Free();
  FNuclides := TList.Create();

  AssignFile(F, FileName);
  {$I-}
  Reset(F) {$I+};
  if IOResult <> 0 then
  begin
      //ShowMessage('Error openning file '+FileName);
    exit;
  end;

  CurUnit := 'year';
  while (not EoF(F)) do
  begin
    readln(F, S);
    S := Trim(S);
    if S <> '' then
      if S[1] in ['A'..'z'] then
      begin
        Inc(FNuclidesCount);
        curNuclide := TNuclide.Create();
        curNuclide.Name := ExtractWord(1, S, [#9]);
        TNuclide.NameToElementMass(curNuclide.Name, curNuclide.Element, curNuclide.MassNum);
        curNuclide.FHalfLife := JvSafeStrToFloat(ExtractWord(2, S, [#9]));
        if (WordCount(S, [#9]) > 2) then
          CurUnit := ExtractWord(3, S, [#9]);
        curNuclide.FHLUnits := CurUnit;
        FNuclides.Add(curNuclide);
      end
      else
      begin
        // raise Error
        Exit;
      end;
  end;
  
  Result := True;
  CloseFile(F);
  DecimalSeparator := CurDecSep;
end;

function TLibrary.LibNuclNames2Text: string;
var i: Integer;
    S, St: string;
    CurDecSep: Char;
    curNuclide: TNuclide;
begin
  CurDecSep := DecimalSeparator;
  DecimalSeparator := '.';
  Result := '';
  for i := 0 to FNuclidesCount - 1 do
  begin
    curNuclide := TNuclide(FNuclides[i]);
    // nuclide header
    // nuclide name
    S := curNuclide.Name + MakeStr(' ', 8 - Length(curNuclide.Name));
    // half-life
    if (curNuclide.FHalfLife > 9999) or (curNuclide.FHalfLife < 0.1) then
      S := S + MakeStr(' ', 4) + FloatToStrF(curNuclide.FHalfLife, ffExponent, 4, 1)
    else begin
      St := FormatFloat('###0.0###', curNuclide.FHalfLife);
      S := S + MakeStr(' ', 6 - Pos('.', St)) + St;
    end;
    // half-life units
    S := S + '(' + curNuclide.FHLUnits + ')';
    // gamma-const
    if curNuclide.GammaConst > 0 then
      S := S + MakeStr(' ', 28 - Length(S)) + FormatFloat('###0.0##', curNuclide.GammaConst);
    Result := Result + S + #13 + #10;
  end;
  DecimalSeparator := CurDecSep;
end;

procedure TLibrary.SaveLib(const FileName: string);
var CurDecSep: Char;
begin
  CurDecSep := DecimalSeparator;
  DecimalSeparator := '.';
  FPath := FileName;

  if IsNewFormat then
    SaveLib15(FileName)
  else
    SaveLib1(FileName);

  DecimalSeparator := CurDecSep;
end;

procedure TLibrary.SaveLib1(const FileName: string);
var F: TextFile;
    i, j: Integer;
    S, St: string;
    curNuclide: TNuclide;
begin
  AssignFile(F, FileName);
  Rewrite(F);
  for i:=0 to FNuclidesCount-1 do
  begin
    curNuclide := TNuclide(FNuclides[i]);
    // header nuclide
    S := curNuclide.Name + MakeStr(' ', 8-Length(curNuclide.Name)); 
    if (curNuclide.FHalfLife > 0) and (curNuclide.FHLUnits <> '') then
    begin
      if (curNuclide.FHalfLife > 9999) or (curNuclide.FHalfLife < 0.1) then              // half-life time
        S:= S+ MakeStr(' ', 4) + FloatToStrF(curNuclide.FHalfLife, ffExponent, 4, 2)
      else
      begin
        St:= FormatFloat('###0.0###', curNuclide.FHalfLife);
        S:= S + MakeStr(' ', 6 - Pos('.', St)) + St;
      end;
      S := S + '(' + curNuclide.FHLUnits + ')'; // half-life time units
      if curNuclide.GammaConst > 0 then 
        S := S + MakeStr(' ', 28 - Length(S)) + FormatFloat('###0.0#####', curNuclide.GammaConst);
    end;
    Writeln(F,S);
    // nuclide lines
    for j:=0 to curNuclide.linesCount-1 do
    begin
      S := curNuclide.libNuclLine2Text(j, FIsNewFormat);
      Writeln(F, S);
    end;
  end;
  CloseFile(F);
end;

procedure TLibrary.SaveLib15(const FileName: string);
var F: TextFile;
    i, j: Integer;
    S, St: string;
    curNuclide: TNuclide;
begin
  AssignFile(F, FileName);
  Rewrite(F);
  if (FIsNewFormat and (FShapka = '')) then
    FShapka := '<LSRM Nuclide library Version=1.5>';
  Writeln(F, FShapka);
  for i := 0 to FNuclidesCount - 1 do
  begin
    curNuclide := TNuclide(FNuclides[i]);
    // шапка нуклида
    S := curNuclide.Name + MakeStr(' ', 8 - Length(curNuclide.Name));
    if (curNuclide.FHalfLife > 0) and (curNuclide.FHLUnits <> '') then
    begin
      if (curNuclide.FHalfLife > 9999) or (curNuclide.FHalfLife < 0.1) then
        S := S + MakeStr(' ', 4) + FloatToStrF(curNuclide.FHalfLife, ffExponent, 4, 2)
      else
      begin
        St := FormatFloat('###0.0###', curNuclide.FHalfLife);
        S := S + MakeStr(' ', 6 - Pos('.', St)) + St;
      end;
      S := S + '(' + curNuclide.FHLUnits + ')';
      if curNuclide.GammaConst > 0 then
        S := S + MakeStr(' ', 28 - Length(S)) + FormatFloat('###0.0#####', curNuclide.GammaConst);
    end;
    Writeln(F, S);
    // nuclide lines
    for j := 0 to curNuclide.linesCount - 1 do
    begin
      S := curNuclide.libNuclLine2Text(j, FIsNewFormat);
      Writeln(F, S);
    end;
  end;
  CloseFile(F);
end;

procedure TLibrary.SaveTabLib(const FileName: string);
var F: TextFile;
    i, j: Integer;
    S: string;
    CurDecSep: Char;
    curNuclide: TNuclide;
begin
  CurDecSep := DecimalSeparator;
  DecimalSeparator := '.';
  FPath := FileName;

  AssignFile(F, FileName);
  Rewrite(F);
  for i := 0 to FNuclidesCount - 1 do
  begin
    curNuclide := TNuclide(FNuclides[i]);
    S := curNuclide.Name + #09;
    if (curNuclide.FHalfLife > 9999) or (curNuclide.FHalfLife < 0.1) then
      S := S + FloatToStrF(curNuclide.FHalfLife, ffExponent, 4, 1)
    else
      S := S + FormatFloat('###0.0###', curNuclide.FHalfLife);
    S := S + '(' + curNuclide.FHLUnits + ')';
    if curNuclide.GammaConst > 0 then
      S := S + #09 + FormatFloat('###0.0##', curNuclide.GammaConst);
    Writeln(F, S);
    // линии нуклида
    for j := 0 to curNuclide.linesCount - 1 do
    begin
      S := FormatFloat('0.000##', curNuclide.Lines[j].Energy);
      if IsNewFormat then
        S := S + #09 + FormatFloat('0.000##', curNuclide.Lines[j].FdEnergy);
      S := S + #09 + FloatToStrF(curNuclide.Lines[j].i, ffGeneral, 6, 2);
      S := S + #09 + FloatToStrF(curNuclide.Lines[j].dI, ffGeneral, 5, 2);
      if curNuclide.Lines[j].LineType <> #0 then
        S := S + #09 + curNuclide.Lines[j].LineType;
      if not (curNuclide.Lines[j].IsProcessed) then
        S := S + #09 + 'n';
      Writeln(F, S);
    end;
  end;
  CloseFile(F);
  DecimalSeparator := CurDecSep;
end;

procedure TLibrary.SaveShortTabLib(const FileName: string);
var F: TextFile;
    i: Integer;
    S: string;
    CurDecSep: Char;
    curNuclide: TNuclide;
begin
  CurDecSep := DecimalSeparator;
  DecimalSeparator := '.';
  FPath := FileName;

  AssignFile(F, FileName);
  Rewrite(F);
  for i := 0 to FNuclidesCount - 1 do
  begin
    curNuclide := TNuclide(FNuclides[i]);
    S := curNuclide.Name + #09;
    if (curNuclide.FHalfLife > 9999) or (curNuclide.FHalfLife < 0.1) then
      S := S + FloatToStrF(curNuclide.FHalfLife, ffExponent, 4, 1)
    else
      S := S + FormatFloat('###0.0###', curNuclide.FHalfLife);
    Writeln(F, S);
  end;
  CloseFile(F);
  DecimalSeparator := CurDecSep;
end;

class function TLibrary.unitsFromString(const S: string): string;
var unitsBegin, unitsEnds: Integer;
begin
  unitsBegin := Pos('(', S);
  unitsEnds := Pos(')', S);
  if (unitsBegin = 0) or (unitsEnds = 0) or (unitsBegin > unitsEnds) or (unitsBegin = Length(S)) then
  begin
    Result := '';
    Exit;
  end;
  result := Copy(S, unitsBegin + 1, unitsEnds - unitsBegin - 1);
end;

function SortDataA(P1, P2: Pointer): Integer;
begin
  if (TNuclide(P1^).MassNum > TNuclide(P2^).MassNum) then
    Result := 1
  else if (TNuclide(P1^).MassNum < TNuclide(P2^).MassNum) then
    Result := -1
  else
    Result := 0;
end;

function SortDataName(P1, P2: Pointer): Integer;
begin
  if (TNuclide(P1^).Name > TNuclide(P2^).Name) then
    Result := 1
  else if (TNuclide(P1^).Name < TNuclide(P2^).Name) then
    Result := -1
  else
    Result := 0;
end;

procedure TLibrary.SortLibA;
(*var i, j : Integer;
    Temp: TNuclide;*)
begin
  FNuclides.Sort(SortDataA);
  (*for i := 1 to FNuclidesCount-1 do
  begin
    Temp := Nuclides[i];
    j := i - 1;
    while Temp.MassNum < Nuclides[j].MassNum do
    begin
      Nuclides[j + 1]:= Nuclides[j];
      Dec (j);
      if j < 0 then
        Break;
    end;
    Nuclides [j + 1] := Temp;
  end;*)
end;

procedure TLibrary.SortLibName;
(*var i, j : Integer;
    Temp: TNuclide;*)
begin
  FNuclides.Sort(SortDataName);
  (*for i := 1 to FNuclidesCount-1 do
  begin
    Temp := Nuclides[i];
    j := i - 1;
    while Temp.Name < Nuclides[j].Name do
    begin
      Nuclides[j + 1]:= Nuclides[j];
      Dec (j);
      if j < 0 then
        Break;
    end;
    Nuclides [j + 1] := Temp;
  end;*)
end;

procedure TLibrary.SortLibLines;
var i : Integer;
begin
  for i:=0 to FNuclidesCount-1 do
    TNuclide(FNuclides[i]).sortLines();
end;

procedure TLibrary.CopyNucLines(NucFromNum, NucToNum: Integer);
var i, oldLinesCount: Integer;
    nucFrom, nucTo: TNuclide;
begin
  if (NucFromNum >= FNuclidesCount) or (NucToNum >= FNuclidesCount) then
    Exit;
  nucFrom := TNuclide(FNuclides[NucFromNum]);
  nucTo := TNuclide(FNuclides[NucToNum]);

  oldLinesCount := nucTo.linesCount;
  nucTo.linesCount := nucTo.linesCount + nucFrom.linesCount;

  for i := 0 to nucFrom.linesCount-1 do
    nucTo.Lines[oldLinesCount + i] := nucFrom.Lines[i];
end;

procedure TLibrary.CopyNucLines(NucFromNum, NucToNum: Integer; Factor: Double);
var i, OldLinesCount: Integer;
    nucFrom, nucTo: TNuclide;
begin
  if (NucFromNum >= FNuclidesCount) or (NucToNum >= FNuclidesCount) then
    Exit;
  nucFrom := TNuclide(FNuclides[NucFromNum]);
  nucTo := TNuclide(FNuclides[NucToNum]);

  OldLinesCount := nucTo.linesCount;
  nucTo.linesCount := nucTo.linesCount + nucFrom.linesCount;

  for i:=0 to nucFrom.linesCount-1 do
  begin
    nucTo.Lines[OldLinesCount+i] := nucFrom.Lines[i];
    nucTo.Lines[OldLinesCount+i].I := nucTo.Lines[OldLinesCount+i].I*Factor;
    nucTo.Lines[OldLinesCount+i].dI := nucTo.Lines[OldLinesCount+i].dI*Factor;
  end;
end;

procedure TLibrary.DeleteNuclide(NucNum: integer);
begin
  if (FNuclidesCount <= 0) or (NucNum >= FNuclidesCount) then
  begin
    // raise error
    Exit;
  end;

  TNuclide(FNuclides[NucNum]).deleteAllLines();
  FNuclides.Delete(NucNum);
  Dec(FNuclidesCount);  
end;

procedure TLibrary.ConvertLibToEng();
var i: Integer;
begin
  for i:=0 to FNuclidesCount-1 do
    TNuclide(FNuclides[i]).convertUnitsToEng();
end;

procedure TLibrary.DefaultLibFill;
var i: Integer;
    curNuclide: TNuclide;
begin
  AddNuclide('Na-22', 2.6029);
  AddNuclide('Ti-44', 60);
  AddNuclide('Mn-54', 0.8545);
  AddNuclide('Co-57', 0.744);
  AddNuclide('Co-60', 5.2712);
  AddNuclide('Zn-65', 0.668);
  AddNuclide('Y-88', 0.2919);
  AddNuclide('Cd-109', 1.2632);
  AddNuclide('Sn-113', 0.3151);
  AddNuclide('Ba-133', 10.536);
  AddNuclide('Cs-134', 2.065);
  AddNuclide('Cs-137', 30.07);
  AddNuclide('Ce-139', 0.3768);
  AddNuclide('Eu-152', 13.537);
  AddNuclide('Th-228', 1.912);
  AddNuclide('Am-241', 432.6);

  for i:=0 to FNuclides.Count-1 do
  begin
    curNuclide := TNuclide(FNuclides[i]);
    TNuclide.NameToElementMass(curNuclide.Name, curNuclide.Element, curNuclide.MassNum);
    curNuclide.FHLUnits := 'year';
    curNuclide.linesCount := 0;
  end;
end;

function TLibrary.ThalfFromName(const nucName: string): Double;
var i: Integer;
begin
  for i:=0 to FNuclidesCount-1 do
    if SameText(TNuclide(FNuclides[i]).Name, NucName) then
    begin
      Result := TNuclide(FNuclides[i]).FHalfLife;
      Exit;
    end;
  Result := -1;
end;

procedure TLibrary.GetListOfNuclides(var NucList: TStringList);
var i: Integer;
begin
  NucList.Clear();
  for i:=0 to FNuclidesCount-1 do
  begin
    NucList.Add(TNuclide(FNuclides[i]).Name);
  end;
end;

function TLibrary.IsNuclideInLib(NucName: string): Boolean;
var i: Integer;
begin
  Result := False;
  for i:=0 to FNuclidesCount-1 do
  begin
    if SameText(NucName, TNuclide(FNuclides[i]).Name) then begin
      Result := True;
      Exit;
    end;
  end;
end;

function TLibrary.ConvertNuclides2EffmakerString(const ANucIndexes: array of Integer;
  const AActivities: array of Double): string;
var i, j, OldLength: Integer;
    TempNuclide, curNuclide: TNuclide;
begin
  Result := '';
  if (Length(ANucIndexes)=0) or (Length(AActivities)=0) or (Length(ANucIndexes) <> Length(AActivities)) then
    Exit;

  TempNuclide := TNuclide.Create();
  TempNuclide.linesCount := 0;
  // creating nuclide with all lines from selectes nuclides
  for i:=0 to Length(ANucIndexes)-1 do
  begin
    curNuclide := TNuclide(FNuclides[ANucIndexes[i]]);
    OldLength := TempNuclide.linesCount;
    TempNuclide.linesCount := TempNuclide.linesCount + curNuclide.linesCount; // Length(TempNuclide.Lines);
    for j:=0 to curNuclide.linesCount-1 do
    begin
      TempNuclide.Lines[OldLength+j] := curNuclide.Lines[j];
      TempNuclide.Lines[OldLength+j].I := TempNuclide.Lines[OldLength+j].I/100 * AActivities[i];  // интенсивность переводит в поток гамма-квантов
    end;
    (*  Result := Result + FloatToStr(Nuclides[ANucIndexes[i]].Lines[j].Energy) + ':' +
                         FloatToStr(Nuclides[ANucIndexes[i]].Lines[j].I/100 * AActivities[i]);
      if SameText(Nuclides[ANucIndexes[i]].Lines[j].LineType,'x') then
        if (Nuclides[ANucIndexes[i]].Lines[j].Width > 0) then
          Result := Result + ':'+FloatToStrF(Nuclides[ANucIndexes[i]].Lines[j].Width,ffFixed,4,1)
        else Result := Result + ':'+'90';
      Result := Result +' ';
    end; *)
  end;

  // form JSON string
  TempNuclide.sortLines();
  TempNuclide.sumEqualLines();
  for j:=0 to TempNuclide.linesCount-1 do begin
    Result := Result + FloatToStr(TempNuclide.Lines[j].Energy) + ':' +
                       FloatToStr(TempNuclide.Lines[j].I);
    if SameText(TempNuclide.Lines[j].LineType,'x') then
      if (TempNuclide.Lines[j].Width > 0) then
        Result := Result + ':'+FloatToStrF(TempNuclide.Lines[j].Width,ffFixed,4,1)
      else Result := Result + ':'+'90';
    Result := Result +' ';
  end;

  TempNuclide.Free();
end;

procedure TLibrary.LoadWidthes(const filename: string);
var FileString: TStringList;
    ANuclideName: string;
    i,j: Integer;
begin
  FileString := TStringList.Create;
  FileString.LoadFromFile(filename);
  ANuclideName := '';
  SetLength(Widthes, FileString.Count);
  j:=0;
  for i:=0 to FileString.Count-1 do
  begin
    if (WordCount(FileString.Strings[i], [#09]) = 1) then
      ANuclideName := Trim(FileString.Strings[i])
    else if (WordCount(FileString.Strings[i], [#09]) >= 2) then begin
      Widthes[j].NuclideName := ANuclideName;
      Widthes[j].LineEnergy := StrToFloatDef(ExtractWord(1, FileString.Strings[i], [#09]), 0);
      Widthes[j].NatWidth := StrToFloatDef(ExtractWord(2, FileString.Strings[i], [#09]), 0);
      Inc(j);
    end;
  end;
  SetLength(Widthes, j);
  
  FileString.Free();
end;

procedure TLibrary.SetWidthes;
var i,j: Integer;
begin
  if Assigned(Widthes) then
    for i:=0 to FNuclidesCount-1 do
      for j:=0 to TNuclide(FNuclides[i]).linesCount-1 do
        if SameText(TNuclide(FNuclides[i]).Lines[j].LineType, 'x') then
          AddWidthToLine(@(TNuclide(FNuclides[i]).Lines[j]));
end;

procedure TLibrary.AddWidthToLine(ALine: PLine);
var i: Integer;
begin
  for i:=0 to Length(Widthes)-1 do
    if (Abs(ALine^.Energy - Widthes[i].LineEnergy) <= 0.01) then begin
      ALine^.Width := Widthes[i].NatWidth;
    end;
end;

function TLibrary.RecalcActivityToDate(NuclideName: string; OldAct: Double; OldDate, NewDate: TDateTime): Double;
var Tdiff, FHalfLife: Double;
begin
  Result := -1;
  NuclideName := AddDash2Name(NuclideName);
  if not IsNuclideInLib(NuclideName) then Exit;
  Tdiff := NewDate - OldDate;
  FHalfLife := ThalfFromName(NuclideName) * DAYS_IN_YEAR;
  if (FHalfLife > 0) then
    Result := OldAct * power(2, -Tdiff/FHalfLife);
end;

class function TLibrary.AddDash2Name(nuclideName: string): string;
var i, NumPos: Integer;
begin
  Result := nuclideName;
  if (Pos('-', nuclideName) > 0) then Exit;
  NumPos := 0;
  for i:=1 to Length(nuclideName) do
    if nuclideName[i] in ['0'..'9'] then begin
      NumPos := i;
      Break;
    end;
  if (NumPos < 2) then Exit;
  Result := Copy(nuclideName, 1, NumPos - 1) + '-'
    + Copy(nuclideName, NumPos, Length(nuclideName) - NumPos + 1);
end;

function AddDash2Name(NuclideName: string): string;
var i, NumPos: Integer;
begin
  Result := NuclideName;
  if (Pos('-',NuclideName)>0) then Exit;
  NumPos:=0;
  for i:=1 to Length(NuclideName) do
    if NuclideName[i] in ['0'..'9'] then begin
      NumPos := i;
      Break;
    end;
  if (NumPos<2) then Exit;
  Result:=Copy(NuclideName,1,NumPos-1)+'-'+Copy(NuclideName,NumPos,Length(NuclideName)-NumPos+1);
end;

function DeleteDashFromName(NuclideName: string): string;
var DashPos: Integer;
begin
  Result := NuclideName;
  DashPos := Pos('-', NuclideName);
  if (DashPos <= 0) then Exit;
  Delete(Result, DashPos, 1);
end;

procedure TLibrary.MergeCloseLines(NucIndex: Integer; F, E: Double);
begin

end;

function TLibrary.getNuclide(i: Integer): TNuclide;
begin
  if ((i >= 0) and (i < FNuclidesCount)) then
    Result := TNuclide(FNuclides[i])
  else
    Result := nil;
end;

destructor TLibrary.Destroy;
var i: Integer;
begin
  for i:=0 to FNuclides.Count-1 do
    TNuclide(FNuclides[i]).Free();
  FNuclides.Free();
  inherited;
end;

function TLibrary.AddNuclide(const aNucName: string; aHalfTime: Double): Integer;
var curNuclide: TNuclide;
begin
  curNuclide := TNuclide.Create();
  curNuclide.Name := aNucName;
  curNuclide.FHalfLife := aHalfTime;
  Result := FNuclidesCount;
  Inc(FNuclidesCount);
  FNuclides.Add(curNuclide);
end;

function Ci2Bq(CiAct: Double): Double;
begin
  Result := 3.7e10 * CiAct;
end;

function Bq2Ci(BqAct: Double): Double;
begin
  Result := BqAct/3.7e10;
end;

function TLibrary.getNuclideFromName(const nucName: string): TNuclide;
var i: Integer;
begin
  Result := nil;
  for i:=0 to FNuclidesCount-1 do
    if SameText(TNuclide(FNuclides[i]).Name, nucName) then begin
      Result := TNuclide(FNuclides[i]);
      Break;
    end;
end;

function TLibrary.IntensityFromEnergy(const NucName: string; energy: Double;
  var dI: Double;  dWindow: Double = RESWINDOW): Double;
var i, j: Integer;
    tmpNuclide: TNuclide;
begin
  Result := -1;

  j := NucNumFromName(NucName);
  if (j < 0) then Exit;

  tmpNuclide := TNuclide(FNuclides[j]);

  for i:=0 to TNuclide(tmpNuclide).linesCount-1 do
    if (Abs(TNuclide(tmpNuclide).Lines[i].Energy - energy) < dWindow) then
    begin
      Result := TNuclide(tmpNuclide).Lines[i].I;
      dI := TNuclide(tmpNuclide).Lines[i].dI;
      Break;
    end;
end;

function TLibrary.NucNumFromName(const ANuclideName: string): Integer;
var i: Integer;
begin
  Result := -1;
  for i:=0 to FNuclidesCount-1 do
    if SameText(TNuclide(FNuclides[i]).Name, ANuclideName) then begin
      Result := i;
      Break;
    end;
end;

procedure TLibrary.mergeNuclideLines(nucNum: Integer;
  const linesIndexes: array of Integer);
var
  TempNuclide: TNuclide;
  linesArray: array of TLine;
  i, tmpLength: Integer;
  mergeLine: TLine;
  stanDev: Double;
begin
  if (Length(linesIndexes) = 0) then Exit;

  TempNuclide := getNuclide(nucNum);

  // find lines for merging
  SetLength(linesArray, Length(linesIndexes));
  tmpLength:=0;
  for i:=0 to Length(linesIndexes)-1 do
  begin
    if (linesIndexes[i] >= TempNuclide.linesCount) then Continue;
    linesArray[tmpLength] := TempNuclide.Lines[linesIndexes[i]];
    Inc(tmpLength);
  end;
  SetLength(linesArray, tmpLength);

  // merge line have weighted energy, weights = intensities. And it has sum intensity
  mergeLine.Energy := 0;
  mergeLine.FdEnergy := 0;
  mergeLine.I := 0;
  mergeLine.dI := 0;
  for i:=0 to tmpLength-1 do
  begin
    mergeLine.Energy := mergeLine.Energy + linesArray[i].Energy*linesArray[i].I;
    mergeLine.I := mergeLine.I + linesArray[i].I;
    mergeLine.FdEnergy := Sqrt(Sqr(mergeLine.FdEnergy) + Sqr(linesArray[i].FdEnergy));
    mergeLine.dI := mergeLine.dI + linesArray[i].dI;
  end;
  mergeLine.Energy := mergeLine.Energy / mergeLine.I;
  mergeLine.LineType := #0;
  mergeLine.IsProcessed := True;
  // correct dE on scatter
  stanDev := 0;
  for i:=0 to tmpLength-1 do
  begin
    stanDev := stanDev + Sqr(linesArray[i].I * (linesArray[i].Energy - mergeLine.Energy));
  end;
  stanDev := Sqrt(stanDev) / mergeLine.I;
  // use max from 2 this errors for A
  mergeLine.FdEnergy := Max(mergeLine.FdEnergy, stanDev);

  // delete lines for merging
  for i:=0 to Length(linesIndexes)-1 do
    TempNuclide.deleteLine(linesIndexes[i]-i);

  // add merged line
  TempNuclide.addLine(mergeLine);
  TempNuclide.sortLines();
end;

function TLibrary.activityToMass(nucName: string; act: Double): Double;
var i: Integer;
begin
  Result := -1;
  nucName := AddDash2Name(nucName);
  if not IsNuclideInLib(nucName) then Exit;

  Result := getNuclideFromName(nucName).activityToMass(act);
end;

function TLibrary.massToActivity(nucName: string; mass: Double): Double;
var i: Integer;
begin
  Result := -1;
  nucName := AddDash2Name(nucName);
  if not IsNuclideInLib(nucName) then Exit;

  Result := getNuclideFromName(nucName).massToActivity(mass);
end;

end.
