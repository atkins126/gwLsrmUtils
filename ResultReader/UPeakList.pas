unit UPeakList;

interface

const
  ARRCOUNT = 21;

  EngOldNames: array[0..ARRCOUNT-1] of AnsiString = (
  'Channel',
  'Channel err.',
  'Energy',
  'Energy err.',
  'FWHM',
  'FWHM (Ch)',
  'Area',
  'Area error',
  'Count rate',
  'Chi square',
  'Activity',
  'Activity err.',
  'Intensity',
  'Intensity error',
  'Nuclide',
  'Step',
  'DCR',
  'LibEnergy',
  'LibIntensity',
  'Gross area',
  'Covell area');

type
  TPeak = record
    Channel,
    DChannel,
    Energy,
    DEnergy,
    FWHM,
    FWHMK,
    Area,
    DArea,
    CountRate,
    DCR,
    GrossArea,
    CovellArea,
    ChiSquare,
    Activity,
    DActivity,
    Intensity,
    DIntensity,
    Step,
    LibEnergy,
    LibIntensity: Double;
    Nuclide: string;
  end;

  TPeakArray = class
  private
    FLength,
    FColumns: Integer;
    FExtendedHead: string;
    FIsLoaded: Boolean;
    FIsCRLoaded, FIsDCRLoaded: Boolean;
    FLiveTime,
    FRealTime: Double;
    procedure setLengthInternal(ALength: Integer);
    procedure internalLoadFromStr(const aStr: string);
    procedure calcCR();
    procedure obtainParams(const S: string);
  public
    peaks: array of TPeak;
    peakNamesNumber: array[1..ARRCOUNT] of Byte;
    constructor openFromResultFile(const FileName: string);
    constructor loadFromStr(const aStr: string);
    constructor copy(const APeakArray: TPeakArray);
    constructor copyForNuclide(const APeakArray: TPeakArray; const NucName: string);
    constructor copyFor2Nuclides(const APeakArray: TPeakArray; const NucName1, NucName2: string);
    constructor copyForNuclides(const APeakArray: TPeakArray; const NucNames: array of string);
    procedure obtainData2Arrays(var E, n, dn, Intens: array of Double; UseLibEnergy: Boolean = True);
    procedure correctOnGross();
    procedure recalcCRFromCovell();
    property lengthPeaks: Integer read FLength write setLengthInternal;
    property columns: Integer read FColumns;
    property extendedHead: string read FExtendedHead write FExtendedHead;
    property isLoaded: Boolean read FIsLoaded write FIsLoaded;
    property liveTime: Double read FLiveTime write FLiveTime;
    property realTime: Double read FRealTime write FRealTime;
    property isCRLoaded: Boolean read FIsCRLoaded;
    property isDCRLoaded: Boolean read FIsDCRLoaded;
  end;

  function ConvertToCurrDC(S: string; DC: Char): string;

implementation

uses Classes, SysUtils, Math, JvJCLUtils;

{ TPeakArray }

procedure TPeakArray.CalcCR;
var i: Integer;
begin
  if ((FLength <= 0) or (FLiveTime <= 0)) then Exit;
  for i:=0 to FLength-1 do
  begin
    if (not FIsCRLoaded) then
      peaks[i].CountRate := peaks[i].Area/FLiveTime;
    if (not FIsDCRLoaded) then begin
      if (peaks[i].CountRate <> 0) then
        peaks[i].DCR := peaks[i].DArea/FLiveTime
      else
        peaks[i].DCR := 0;
    end;
  end;
  FIsCRLoaded := True;
end;

constructor TPeakArray.copy(const APeakArray: TPeakArray);
var i: Integer;
begin
  SetLengthInternal(APeakArray.LengthPeaks);
  FColumns := APeakArray.Columns;
  FExtendedHead := APeakArray.ExtendedHead;
  FIsLoaded := APeakArray.IsLoaded;
  FIsCRLoaded := APeakArray.IsCRLoaded;
  FIsDCRLoaded := APeakArray.IsDCRLoaded;
  FLiveTime := APeakArray.LiveTime;
  FRealTime := APeakArray.RealTime;

  for i:=0 to FLength-1 do
    peaks[i] := APeakArray.peaks[i];
  for i:=1 to ARRCOUNT do
    PeakNamesNumber[i] := APeakArray.PeakNamesNumber[i];
end;

constructor TPeakArray.copyFor2Nuclides(const APeakArray: TPeakArray;
  const NucName1, NucName2: string);
var i, j: Integer;
begin
  SetLengthInternal(APeakArray.LengthPeaks);
  FColumns := APeakArray.Columns;
  FExtendedHead := APeakArray.ExtendedHead;
  FIsLoaded := APeakArray.IsLoaded;
  FIsCRLoaded := APeakArray.IsCRLoaded;
  FIsDCRLoaded := APeakArray.IsDCRLoaded;
  FLiveTime := APeakArray.LiveTime;
  FRealTime := APeakArray.RealTime;

  for i:=1 to ARRCOUNT do
    PeakNamesNumber[i] := APeakArray.PeakNamesNumber[i];
  j := 0;
  for i:=0 to FLength-1 do
    if (SameText(APeakArray.peaks[i].Nuclide, NucName1) or
        SameText(APeakArray.peaks[i].Nuclide, NucName2)) then begin
      peaks[j] := APeakArray.peaks[i];
      Inc(j);
    end;
  SetLengthInternal(j);
end;

constructor TPeakArray.copyForNuclide(const APeakArray: TPeakArray;
  const NucName: string);
var i, j: Integer;
begin
  SetLengthInternal(APeakArray.LengthPeaks);
  FColumns := APeakArray.Columns;
  FExtendedHead := APeakArray.ExtendedHead;
  FIsLoaded := APeakArray.IsLoaded;
  FIsCRLoaded := APeakArray.IsCRLoaded;
  FIsDCRLoaded := APeakArray.IsDCRLoaded;
  FLiveTime := APeakArray.LiveTime;
  FRealTime := APeakArray.RealTime;

  for i:=1 to ARRCOUNT do
    PeakNamesNumber[i] := APeakArray.PeakNamesNumber[i];
  j := 0;
  for i:=0 to FLength-1 do
    if (SameText(APeakArray.peaks[i].Nuclide, NucName)) then begin
      peaks[j] := APeakArray.peaks[i];
      Inc(j);
    end;
  SetLengthInternal(j);
end;

constructor TPeakArray.copyForNuclides(const APeakArray: TPeakArray;
  const NucNames: array of string);
var i, j: Integer;
  function InArray(S: string): Boolean;
  var k: Integer;
  begin
    Result := False;
    for k:=0 to Length(NucNames)-1 do
      if (SameText(NucNames[k], S)) then begin
        Result := True;
        Exit;
      end;
  end;
begin
  SetLengthInternal(APeakArray.LengthPeaks);
  FColumns := APeakArray.Columns;
  FExtendedHead := APeakArray.ExtendedHead;
  FIsLoaded := APeakArray.IsLoaded;
  FIsCRLoaded := APeakArray.IsCRLoaded;
  FIsDCRLoaded := APeakArray.IsDCRLoaded;
  FLiveTime := APeakArray.LiveTime;
  FRealTime := APeakArray.RealTime;

  for i:=1 to ARRCOUNT do
    PeakNamesNumber[i] := APeakArray.PeakNamesNumber[i];
  j := 0;
  for i:=0 to FLength-1 do
    if InArray(APeakArray.peaks[i].Nuclide) then begin
      peaks[j] := APeakArray.peaks[i];
      Inc(j);
    end;
  SetLengthInternal(j);
end;

procedure TPeakArray.correctOnGross;
var i: Integer;
    dGross: Double;
begin
  if ((FLength <= 0) or (FLiveTime <= 0)) then Exit;
  for i:=0 to FLength-1 do begin
    if (peaks[i].GrossArea > 0) then begin
      dGross := Sqrt(2*peaks[i].GrossArea - peaks[i].Area) / FLiveTime;
      if (dGross > peaks[i].DCR) then peaks[i].DCR := dGross;
    end;
  end;  
end;

procedure TPeakArray.internalLoadFromStr(const aStr: string);
var sourceList: TStringList;
    i, iBeg: Integer;
    S: string;
    isResultFile: Boolean;
begin
  isResultFile := False;

  sourceList := TStringList.Create();
  sourceList.Text := aStr;
  // convert decimal separator to current
  sourceList.Text := ConvertToCurrDC(sourceList.Text, DecimalSeparator);

  // Header reading
  for i:=0 to sourceList.Count-1 do
  begin
    S := sourceList.Strings[i];
    if (Length(S) > 0) then begin
      if (S[1]='#') or (S[1]='№') or (S[1]='N') then // columns header start
      begin
        isResultFile := True;   // There is a header in the file
        FColumns := WordCount(S, [#09]); // columns count
        FLength := sourceList.Count - 1 - i; // row count
        obtainParams(S); // obtain parameter's name list
        Break;
      end
      else begin // store header
        FExtendedHead := FExtendedHead + S + #13#10;
        if (Pos('Live=', S) > 0) then
          FLiveTime := StrToFloatDef(ExtractWord(2, S, ['=']), 0)
        else if (Pos('Real=', S) > 0) then
          FRealTime := StrToFloatDef(ExtractWord(2, S, ['=']), 0);
      end;
    end;
  end;

  // peak array reading
  if (isResultFile) then  // if it is a Result-file
  begin
    iBeg := i;
    setLengthInternal(FLength);

    for i := 1 to FLength do
    begin
      S := sourceList.Strings[iBeg + i];

      if (WordCount(S, [#09]) > FColumns) then // если не указана колонка Nuclide, а она есть
      begin
        Inc(FColumns); // колонок реально больше на 1
        PeakNamesNumber[15] := FColumns; // номер колонки "Нуклид" последний
      end;

      if PeakNamesNumber[1]<>0 then
        TryStrToFloat(ExtractDelimited(PeakNamesNumber[1], S,[#09]),peaks[i-1].Channel);
      if PeakNamesNumber[2]<>0 then
        TryStrToFloat(ExtractDelimited(PeakNamesNumber[2], S,[#09]),peaks[i-1].DChannel);
      if PeakNamesNumber[3]<>0 then
        TryStrToFloat(ExtractDelimited(PeakNamesNumber[3], S,[#09]),peaks[i-1].Energy);
      if PeakNamesNumber[4]<>0 then
        TryStrToFloat(ExtractDelimited(PeakNamesNumber[4], S,[#09]),peaks[i-1].DEnergy);
      if PeakNamesNumber[5]<>0 then
        TryStrToFloat(ExtractDelimited(PeakNamesNumber[5], S,[#09]),peaks[i-1].FWHM);
      if PeakNamesNumber[6]<>0 then
        TryStrToFloat(ExtractDelimited(PeakNamesNumber[6], S,[#09]),peaks[i-1].FWHMK);
      if PeakNamesNumber[7]<>0 then
        TryStrToFloat(ExtractDelimited(PeakNamesNumber[7], S,[#09]),peaks[i-1].Area);
      if PeakNamesNumber[8]<>0 then
        TryStrToFloat(ExtractDelimited(PeakNamesNumber[8], S,[#09]),peaks[i-1].DArea);
      if PeakNamesNumber[9]<>0 then begin
        FIsCRLoaded := True;
        TryStrToFloat(ExtractDelimited(PeakNamesNumber[9], S,[#09]),peaks[i-1].CountRate);
      end;
      if PeakNamesNumber[10]<>0 then
        TryStrToFloat(ExtractDelimited(PeakNamesNumber[10], S,[#09]),peaks[i-1].ChiSquare);
      if PeakNamesNumber[11]<>0 then
        TryStrToFloat(ExtractDelimited(PeakNamesNumber[11], S,[#09]),peaks[i-1].Activity);
      if PeakNamesNumber[12]<>0 then
        TryStrToFloat(ExtractDelimited(PeakNamesNumber[12], S,[#09]),peaks[i-1].DActivity);
      if PeakNamesNumber[13]<>0 then
        TryStrToFloat(ExtractDelimited(PeakNamesNumber[13], S,[#09]),peaks[i-1].Intensity);
      if PeakNamesNumber[14]<>0 then
        TryStrToFloat(ExtractDelimited(PeakNamesNumber[14], S,[#09]),peaks[i-1].DIntensity);
      if PeakNamesNumber[15]<>0 then
        peaks[i-1].Nuclide:=ExtractDelimited(PeakNamesNumber[15], S,[#09]);
      if PeakNamesNumber[16]<>0 then
        TryStrToFloat(ExtractDelimited(PeakNamesNumber[16], S,[#09]),peaks[i-1].Step);
      if PeakNamesNumber[17]<>0 then begin
        TryStrToFloat(ExtractDelimited(PeakNamesNumber[17], S,[#09]),peaks[i-1].DCR);
        FIsDCRLoaded := True;
      end;
      if PeakNamesNumber[18]<>0 then
        TryStrToFloat(ExtractDelimited(PeakNamesNumber[18], S,[#09]),peaks[i-1].LibEnergy);
      if PeakNamesNumber[19]<>0 then
        TryStrToFloat(ExtractDelimited(PeakNamesNumber[19], S,[#09]),peaks[i-1].LibIntensity);
      if PeakNamesNumber[20]<>0 then
        TryStrToFloat(ExtractDelimited(PeakNamesNumber[20], S,[#09]),peaks[i-1].GrossArea);
      if PeakNamesNumber[21]<>0 then
        TryStrToFloat(ExtractDelimited(PeakNamesNumber[21], S,[#09]),peaks[i-1].CovellArea);
    end;

    FIsLoaded := True; // файл загружен
  end;

  if (not FIsCRLoaded) or (not FIsDCRLoaded) then
    CalcCR();

  sourceList.Free();
end;

constructor TPeakArray.loadFromStr(const aStr: string);
begin
  InternalLoadFromStr(aStr);
end;

procedure TPeakArray.obtainData2Arrays(var E, n, dn, Intens: array of Double; UseLibEnergy: Boolean = True);
var i, NLength: Integer;
begin
  NLength := Min(FLength, Length(E));
  if (NLength = 0) then Exit;
  
  for i:=0 to NLength-1 do
  begin
    if (UseLibEnergy) then begin
      if (peaks[i].LibEnergy <> 0) then
        E[i] := peaks[i].LibEnergy
      else
        E[i] := peaks[i].Energy;
    end
    else
      E[i] := peaks[i].Energy;
    n[i] := peaks[i].CountRate;
    dn[i] := peaks[i].DCR;
    Intens[i] := peaks[i].LibIntensity;
  end;
end;

procedure TPeakArray.obtainParams(const S: string);
var i: Integer;
    ColName: string;
begin
  for i:=2 to FColumns do
  begin
    ColName := ExtractDelimited(i, S, [#09]);
    if SameText(ColName, 'Channel') or SameText(ColName, 'Канал') then
      PeakNamesNumber[1]:=i
    else if SameText(ColName,'Channel err.') or SameText(ColName,'Погр. канала')
     or SameText(ColName,'Channel unc.') or SameText(ColName,'Неопр. канала') then
      PeakNamesNumber[2] := i
    else if SameText(ColName, 'Energy') or SameText(ColName, 'Энергия') then
      PeakNamesNumber[3] := i
    else if SameText(ColName, 'Energy err.') or SameText(ColName, 'Energy uncert.')
      or SameText(ColName, 'Energy unc.') or SameText(ColName,'Погр. энергии') or
      SameText(ColName, 'Неопр. энергии') then
      PeakNamesNumber[4]:=i
    else if SameText(ColName, 'FWHM') or SameText(ColName,'ПШПВ') or SameText(ColName,'ПШПВ, кэВ') then
      PeakNamesNumber[5]:=i
    else if SameText(ColName, 'FWHM (Ch)') or SameText(ColName,'ПШПВ (К)') then
      PeakNamesNumber[6]:=i
    else if SameText(ColName, 'Area') or SameText(ColName,'Площадь') then
      PeakNamesNumber[7]:=i
    else if SameText(ColName, 'Area error') or SameText(ColName, 'Area uncert.')
     or SameText(ColName, 'Погр. площади') or SameText(ColName, 'Неопр. площади')
     or SameText(ColName, 'Area err.') then
      PeakNamesNumber[8]:=i
    else if SameText(ColName, 'Count rate') or SameText(ColName,'Скорость счета') then
      PeakNamesNumber[9]:=i
    else if SameText(ColName, 'Chi square') or SameText(ColName, 'Хи квадрат') then
      PeakNamesNumber[10]:=i
    else if SameText(ColName, 'Activity') or SameText(ColName, 'Activity, Bq')
     or SameText(ColName, 'Активность, Бк') then
      PeakNamesNumber[11]:=i
    else if SameText(ColName, 'Activity err.') or SameText(ColName, 'Activity uncert., %')
     or SameText(ColName, 'Погр. активности') or SameText(ColName, 'Погр. активности , %')
     or SameText(ColName, 'Неопр. активности , %') then
      PeakNamesNumber[12]:=i
    else if SameText(ColName, 'Intensity') or SameText(ColName, 'Интенсивность') then
      PeakNamesNumber[13]:=i
    else if SameText(ColName, 'Intensity error') or SameText(ColName, 'Intensity uncert.')
     or SameText(ColName, 'Погр. интенсивности') or SameText(ColName, 'Неопр. интенсивности') then
      PeakNamesNumber[14]:=i
    else if SameText(ColName, 'Nuclide') or SameText(ColName, 'Нуклид') then
      PeakNamesNumber[15]:=i
    else if SameText(ColName, 'Step') or SameText(ColName, 'Ступенька') then
      PeakNamesNumber[16]:=i
    else if SameText(ColName, 'Count rate uncert.') or SameText(ColName,'Count rate uncert.') then
      PeakNamesNumber[17]:=i  
    else if SameText(ColName, 'Library energy') or SameText(ColName,'Library energy') then
      PeakNamesNumber[18]:=i
    else if SameText(ColName, 'Library intensity') or SameText(ColName,'Library intensity') then
      PeakNamesNumber[19]:=i
    else if SameText(ColName, 'Gross Area') then
      PeakNamesNumber[20]:=i
    else if SameText(ColName, 'Area by Covell') then
      PeakNamesNumber[21]:=i
  end;
end;

constructor TPeakArray.openFromResultFile(const FileName: string);
var SourceList: TStringList;
begin
  SourceList:=TStringList.Create;
  SourceList.LoadFromFile(FileName);
  InternalLoadFromStr(SourceList.Text);
  
  SourceList.Free;
end;

procedure TPeakArray.recalcCRFromCovell;
var i: Integer;
begin
  if ((FLength <= 0) or (FLiveTime <= 0)) then Exit;
  for i:=0 to FLength-1 do
  begin
    if (peaks[i].CovellArea > 0) then
      peaks[i].CountRate := peaks[i].CovellArea/FLiveTime;
  end;
end;

procedure TPeakArray.setLengthInternal(ALength: Integer);
begin
  if (ALength < 0) then
    ALength := 0;

  FLength := ALength;
  SetLength(peaks, FLength);
end;

function ConvertToCurrDC(S: string; DC: Char): string;
var i: Integer;
begin
  for i:=1 to Length(S) do
    if ((S[i]='.') or (S[i]=',')) and (S[i+1]>='0') and (S[i+1]<='9') then
      S[i]:=DC;
  Result:=S;
end;

end.
