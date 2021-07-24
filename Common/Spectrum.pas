unit Spectrum;

interface

uses  Classes, SysUtils, Dialogs, IniFiles, Math,
      FileStreamLnUnit;

type  TDoubleArray = array of Double;
      PDoubleArray = ^TDoubleArray;

    TSPecCalibration = record // energy and FWHM calibration
      MinEnergy,
      MaxEnergy,
      FWHM: Double;
      Channels: Integer;
    end;
      
  TSpectrum = class(TObject)
    Data: array of Integer; // spectrum data
    BuffSize : Word;
    LT, RT: Double; // live and real times
    MeasureDate, MeasureTime: TDateTime;
    SpectrName, Geometry: String;
    Distance: Double;
    Mass, DMass: Double;
    Volume, DVolume: Double;
    SpectrType, ConfigName, OperatorName, Detector, Material: string;
    EnergyDegree, FWHMDegree: Integer; // Calibrations degree. Energy: E(ch)=Sum(from i=0 to i=EnergyDegree)(EnergyCoeffs[i]*Ch^i)
    EnergyCoeffs, FWHMCoeffs: TDoubleArray; // Calibration coefficients. FWHM: FWHM(E)=Sum(from i=0 to i=FWHMDegree)(FWHMCoeffs[i]*sqrt(E^i))
    IsEnergyLoad, IsFWHMLoad: Boolean;
    CPS: Double; // counts per second
    Comment: string;
    LLD, ULD: Integer;
  private
    function ObtainDateTimeFromString(var outDate, outTime:TDateTime; DTString:string): Boolean;
    procedure StrToMassVol(var Val, DVal: Double; ValStr: String);
    function ObtainCallibration(var EnDegree: Integer; var CalibrCoeffs: TDoubleArray; calibrstr: string): Boolean;
    function CalibrToStr(CalibrDegree: Integer; CalibrCoeffs: TDoubleArray): string;
    function CalcCPS(): Double;
  public
    constructor CopySpectrum(const aSpectr: TSpectrum);
    procedure ChannelReduction(NewBuffSize: Word);
    function TxtOpen(FileName: string): Boolean;
    function csvOpen(FileName: string): Boolean;
    function TxtSave(FileName: string): Boolean;
    function TxtSaveSL(FileName: string): Boolean;
    function SpeOpen(FileName: string): Boolean;
    function SpeOpenSafe(FileName: string): Boolean;
    function SpeSave(FileName: string): Boolean;
    function SpeSaveSafe(const filename: string): Boolean;
    function SpeOldOpen(FileName: string): Boolean;
    function ExtOpen(FileName, Extension: string): Boolean;
    function ExtSave(FileName, Extension: string): Boolean;
    function FileOpen(FileName: string): Boolean;
    function FileSave(FileName: string): Boolean;
    procedure ReadFromIni(IniName: string);
    function Energy(Channel: Word): Double; // returns energy for channel (or -1)
    function FWHM(E: double): Double; // returns FWHM for energy (or -1).
    procedure ClearData();
    function CalcIntegralBetweenLLD(): Integer;
  end;

function ppd2scintil(Spectr: TSpectrum; FWHMscint: Double; Enscint: double; ChNumScint: Integer): TSpectrum;

implementation

uses JvJCLUtils;

{ TSpectrum }

procedure TSpectrum.ChannelReduction(NewBuffSize: Word);
var ChannelScale: Word;
    i, j: Integer;
begin
  ChannelScale:=Trunc(BuffSize/NewBuffSize); // число старых каналов в новом
  if ChannelScale<=0 then Exit;
  for i:=0 to NewBuffSize-1 do
  begin
    Data[i]:=Data[ChannelScale*i];
    for j:=1 to ChannelScale-1 do
    begin
      Data[i]:=Data[i]+Data[ChannelScale*i+j]; // суммирование каналов
    end;
  end;
  if EnergyDegree>1 then
  begin
    EnergyDegree:=2;
    EnergyCoeffs[0]:=EnergyCoeffs[0]+ChannelScale/2*EnergyCoeffs[1];
    EnergyCoeffs[1]:=EnergyCoeffs[1]*ChannelScale;
  end;
  SetLength(Data,NewBuffSize); // сокращение массива
  BuffSize:=NewBuffSize;
end;

function TSpectrum.ExtOpen(FileName, Extension: string): Boolean;
begin
  Result:=False;
  if Extension='txt' then Result:=TxtOpen(FileName)
  else if Extension='spe' then Result:=SpeOpen(FileName);
end;

function TSpectrum.ExtSave(FileName, Extension: string): Boolean;
begin
  Result:=False;
  if Extension='txt' then Result:=TxtSave(FileName)
  else if Extension='spe' then Result:=SpeSave(FileName);
end;

function TSpectrum.ObtainDateTimeFromString(var outDate, outTime: TDateTime; DTString:string): Boolean;
var DStr, TStr: string;
    //DTFormat: TFormatSettings;
    CurrDS: Char;
    TestS: string;
begin
  Result:=False;
  // строка записана в виде DD-MM-YY HH:MM:SS
  // сначала разобъём строку на 2 части
  // а потом переконвертируем каждую строку в дату и время
  DStr:=ExtractWord(1,DTString,[' ']);
  TStr:=ExtractWord(2,DTString,[' ']);
  CurrDS:=DateSeparator;//'-';
  DateSeparator:='-';
  try
    outDate:=StrToDate(DStr);
    outTime:=StrToTime(TStr);
    TestS:=DateToStr(outDate)+' '+ TimeToStr(outTime);
  except
    on EConvertError do ShowMessage('Wrong date format');
  end;
  DateSeparator:=CurrDS;
  Result:=True;
end;

function TSpectrum.SpeOpen(FileName: string): Boolean;
var FileList: TStringList;
    SpeFileStream: TFileStreamLn;
    CurrString,RestString: String; //текущая строка
    BuffInt: Integer;
    i: Integer;
    CurrDecSep: Char;
begin
  Result := False;

  FileList := TStringList.Create;
  try
    FileList.LoadFromFile(FileName);

    // загрузка шапки
    CurrDecSep := DecimalSeparator;
    DecimalSeparator := '.';
    
    SpectrName := FileList.Values['SHIFR'];
    LT := StrToFloatDef(FileList.Values['TLIVE'], -1);
    RT := StrToFloatDef(FileList.Values['TREAL'], -1);
    ObtainDateTimeFromString(MeasureDate, MeasureTime, FileList.Values['MEASBEGIN']);
    Geometry := FileList.Values['GEOMETRY'];
    Distance := StrToFloatDef(FileList.Values['DISTANCE'], 0);
    StrToMassVol(Mass, DMass, FileList.Values['SAMPLEMASS']);
    StrToMassVol(Volume, DVolume, FileList.Values['SAMPLEVOLUME']);
    IsEnergyLoad := ObtainCallibration(EnergyDegree, EnergyCoeffs, FileList.Values['ENERGY']);
    IsFWHMLoad := ObtainCallibration(FWHMDegree, FWHMCoeffs, FileList.Values['FWHM']);
    SpectrType := FileList.Values['TYPE'];
    ConfigName := FileList.Values['CONFIGNAME'];
    OperatorName := FileList.Values['OPERATOR'];
    Detector := FileList.Values['DETECTOR'];
    Material := FileList.Values['MATERIAL'];
    CPS := StrToFloat(FileList.Values['CPS']);
    Comment := FileList.Values['COMMENT'];
    DecimalSeparator := CurrDecSep;

  { Проверки}
  finally
    FileList.Free;
  end;

  // Чтение спектра
  if FileExists(FileName) then // проверка на существование фала
  begin
    SpeFileStream := TFileStreamln.Create(FileName, fmOpenRead or fmShareDenyWrite); // открытие файла spe на чтение

    try
      if SpeFileStream.ReadEqual(CurrString) then SpeFileStream.Readln(RestString); // считывание названия параметра
      While AnsiCompareStr(CurrString, 'SPECTR=')<>0 do // проверка на равенство строки SPEСTR=
      begin
        if SpeFileStream.ReadEqual(CurrString) and (AnsiCompareStr(CurrString,'SPECTR=')<>0) then SpeFileStream.Readln(RestString);; // считывание названия параметра
      end;

      i:=0;
      While SpeFileStream.Read(BuffInt,4)=4 do
      begin
        SetLength(Data,i+1);
        Data[i]:=BuffInt;
        i:=i+1;
      end;
      BuffSize:=Length(Data);
      Result:=true;
    finally
      SpeFileStream.Free();
    end;
  end;
end;

function TSpectrum.SpeSave(FileName: string): Boolean;
var FileList: TStringList;
    SpeFileStream: TFileStreamLn;
    CurrDecSep: Char;
begin
  Result := False;
  // Сохранение шапки в файл
  FileList := TStringList.Create;
  CurrDecSep := DecimalSeparator;
  try
    DecimalSeparator := '.';
    FileList.Add('SHIFR=' + SpectrName);
    FileList.Add('TYPE=' + SpectrType);
    FileList.Add('CONFIGNAME=' + ConfigName);
    FileList.Add('MEASBEGIN=' + FormatDateTime('DD-MM-YY', MeasureDate) + ' ' + FormatDateTime('hh:nn:ss.zz', MeasureTime));
    FileList.Add('TLIVE=' + FloatToStr(LT));
    FileList.Add('TREAL=' + FloatToStr(RT));
    FileList.Add('OPERATOR=' + OperatorName);
    FileList.Add('GEOMETRY=' + Geometry);
    FileList.Add('DETECTOR=' + Detector);
    FileList.Add('MATERIAL=' + Material);
    FileList.Add('DISTANCE=' + FloatToStr(Distance));
    FileList.Add('SAMPLEMASS=' + FloatToStr(Mass) + ';' + FloatToStr(DMass));
    FileList.Add('SAMPLEVOLUME=' + FloatToStr(Volume) + ';' + FloatToStr(DVolume));
    FileList.Add('ENERGY=' + CalibrToStr(EnergyDegree, EnergyCoeffs));
    FileList.Add('FWHM=' + CalibrToStr(FWHMDegree, FWHMCoeffs));
    //FWHMDegree:=ObtainCallibration(FWHMCoeffs,FileList.Values['FWHM']);
    FileList.Add('CPS=' + FloatToStr(CPS));
    FileList.Add('COMMENT=' + Comment);

    FileList.SaveToFile(FileName);
  finally
    FileList.Free;
    DecimalSeparator := CurrDecSep;
  end;

  // Сохранение спектра
  SpeFileStream := TFileStreamLn.Create(FileName, fmOpenReadWrite or fmShareDenyRead);
  try
    SpeFileStream.Seek(0, soFromEnd);
    SpeFileStream.Write('SPECTR=', SizeOf(Char) * Length('SPECTR='));
    SpeFileStream.Write(Data[0], SizeOf(Integer) * BuffSize);
  finally
    SpeFileStream.Free;
  end;
  Result := True;
end;

function TSpectrum.TxtOpen(FileName: string): Boolean;
var F: TextFile;
    i: Integer;
    A: Integer;
    Str: String;
    HeadEnd: Boolean; // проверка на конец шапки
    DTFormat: TFormatSettings;
    MeasureDateStr, MeasureTimeStr: string;
    CurDecSep: Char;
begin
  Result:=False;
  HeadEnd:=False;
  CurDecSep:=DecimalSeparator;
  DecimalSeparator:='.';

  AssignFile(F,FileName); // ввод имени файла
  {$I-}Reset(F) {$I+};
  if IOResult<>0 then
    begin
      ShowMessage('Ошибка открытия файла '+FileName);
      exit;
    end;

  // Считывание шапки
  While (not EoF(F)) and (not HeadEnd) do
  begin
    Readln(F,Str); // считывание DATA=, TIME=, TLIVE=, TREAL=, SPECTRTXT=
    A:=Pos('=', Str);
    if Copy(Str,1,A)='DATE=' then MeasureDateStr:=Copy(Str,A+1,Length(Str));
    if Copy(Str,1,A)='TIME=' then MeasureTimeStr:=Copy(Str,A+1,Length(Str));
    if Copy(Str,1,A)='TLIVE=' then LT:=StrToFloat(Copy(Str,A+1,Length(Str)));
    if Copy(Str,1,A)='TREAL=' then RT:=StrToFloat(Copy(Str,A+1,Length(Str)));
    if Copy(Str,1,A)='GEOMETRY=' then Geometry:=Copy(Str,A+1,Length(Str));
    if Copy(Str,1,A)='DISTANCE=' then Distance:=StrToFloat(Copy(Str,A+1,Length(Str)));
    if Copy(Str,1,A)='MASS=' then Mass:=StrToFloat(Copy(Str,A+1,Length(Str)));
    if Copy(Str,1,A)='VOLUME=' then Volume:=StrToFloat(Copy(Str,A+1,Length(Str)));
    if Copy(Str,1,A)='COMMENT=' then Comment:=Copy(Str,A+1,Length(Str));
    if Copy(Str,1,A)='SPECTRTXT=' then
    begin
      BuffSize:=StrToInt(Copy(Str,A+1,Length(Str)));
      HeadEnd:=true;
    end;
  end;

  DecimalSeparator:=CurDecSep;

  if not HeadEnd then
  begin
    ShowMessage('Неверный формат файла');
    Exit;
  end;

  // вычисление параметров
  SpectrName:=ExtractFileName(FileName);
  SpectrName:=Copy(SpectrName,1,Length(SpectrName)-Length(ExtractFileExt(SpectrName))); // имя спектра = имя файла
  SetLength(Data,BuffSize*sizeof(Integer)); // установка числа каналов в спектре
  try
    DTFormat.ShortDateFormat:='DD-MM-YYYY';
    DTFormat.DateSeparator:='-';
    MeasureDate:=StrToDate(MeasureDateStr,DTFormat);
    MeasureTime:=StrToTime(MeasureTimeStr);
  except
    ShowMessage('Неверный формат времени в спектре '+SpectrName);
  end;

  // Считывание спектра 
  While not EoF(F) do
  begin
   Readln(F,i,A); // чтение из файла в массив
   if (i>0) and (i<BuffSize) then Data[i-1]:=A;
  end;
  CloseFile(F);
  Result:=True;
end;

function TSpectrum.TxtSaveSL(FileName: string): Boolean;
var F: TextFile;
    i: Integer;
    CurDecSep: Char;
begin
  Result:=False;
  AssignFile(F,FileName); // ввод имени файла
  {$I-}Rewrite(F) {$I+};
  if IOResult<>0 then
    begin
      ShowMessage('Ошибка записи файла '+FileName);
      exit;
    end;

  CurDecSep:=DecimalSeparator;
  DecimalSeparator:='.';

  // запись шапки
  Writeln(F, 'DATE=', FormatDateTime('DD-MM-YYYY',MeasureDate));
  Writeln(F, 'TIME=', FormatDateTime('HH:MM:SS',MeasureTime));
  Writeln(F,'TLIVE=', FloatToStr(LT));
  Writeln(F,'TREAL=', FloatToStr(RT));
  Writeln(F,'SPECTRTXT=', IntToStr(BuffSize));

  // запись массива в файл
  for i:=0 to BuffSize-1 do Writeln(F,IntToStr(i+1),#09,IntToStr(Data[i]));
  CloseFile(F);
  DecimalSeparator:=CurDecSep;
  Result:=True;
end;

function TSpectrum.TxtSave(FileName: string): Boolean;
var F: TextFile;
    i: Integer;
    CurDecSep: Char;
begin
  Result:=False;
  AssignFile(F,FileName); // ввод имени файла
  {$I-}Rewrite(F) {$I+};
  if IOResult<>0 then
    begin
      ShowMessage('Ошибка записи файла '+FileName);
      exit;
    end;

  CurDecSep:=DecimalSeparator;
  DecimalSeparator:='.';

  // запись шапки
  Writeln(F, 'DATE=', FormatDateTime('DD-MM-YYYY',MeasureDate));
  Writeln(F, 'TIME=', FormatDateTime('HH:MM:SS',MeasureTime));
  Writeln(F,'TLIVE=', FloatToStr(LT));
  Writeln(F,'TREAL=', FloatToStr(RT));
  Writeln(F,'GEOMETRY=', Geometry);
  Writeln(F,'DISTANCE=', FloatToStr(Distance));
  Writeln(F,'MASS=', FloatToStr(Mass));
  Writeln(F,'VOLUME=', FloatToStr(Volume));
  Writeln(F,'COMMENT=', Comment);
  Writeln(F,'SPECTRTXT=', IntToStr(BuffSize));

  // запись массива в файл
  for i:=0 to BuffSize-1 do Writeln(F,IntToStr(i+1),#09,IntToStr(Data[i]));
  CloseFile(F);
  DecimalSeparator:=CurDecSep;
  Result:=True;
end;

function TSpectrum.SpeOldOpen(FileName: string): Boolean;
var SpeFileStream: TFileStreamLn;
    SourceString, CurParString, ParamName, ParamValue, FileExt: string;
    ParamsCount: Integer;
    HeadChar: PChar;
    i, BuffCount: Integer;
    CurDecSep, CurrDS: Char;
begin
  Result:=False;
  CurDecSep:=DecimalSeparator;
  DecimalSeparator:='.';
  CurrDS:=DateSeparator;//'-';
  DateSeparator:='-';

  if FileExists(FileName) then // проверка на существование фала
  begin
    {$I-} SpeFileStream:=TFileStreamln.Create(FileName,fmOpenRead,fmshareDenyWrite) {$I+}; // открытие файла spe на чтение
    if IOResult<>0 then // проверка на возможность прочитать
    begin
      ShowMessage('Ошибка открытия файла '+FileName);
      exit;
    end;

    // head reading
    GetMem(HeadChar,1024);
    SpeFileStream.ReadBuffer(HeadChar^,1024);
    SourceString := HeadChar;
    FreeMem(HeadChar);

    // head parsing
    SpectrName := ExtractFileName(FileName);
    FileExt := ExtractFileExt(FileName);
    Delete(SpectrName,Pos(FileExt,SpectrName),Length(FileExt));
    ParamsCount := WordCount(SourceString, [' ']);
    EnergyDegree := 4;
    SetLength(EnergyCoeffs, EnergyDegree+1);
    for i:=0 to ParamsCount-1 do
    begin
      CurParString := ExtractWord(i+1,SourceString, [' ']);
      if (SameText(CurParString,'MASS:')) then
        CurParString := CurParString + ExtractWord(i+2,SourceString, [' ']);
      if (WordCount(CurParString, [':'])<=1) then Continue;
      ParamName := ExtractWord(1,CurParString, [':']);
      ParamValue := CurParString;
      Delete(ParamValue,1,Length(ParamName)+1); // because time separator is also ':'
      //ExtractWord(2,CurParString, [':']);

      if SameText(ParamName,'DATE') then
        MeasureDate := StrToDate(ParamValue)
      else if SameText(ParamName,'TBEG') then
        MeasureTime := StrToTime(ParamValue)
      else if SameText(ParamName,'TLIVE') then
        LT := StrToInt(ParamValue)
      else if SameText(ParamName,'TREAL') then
        RT := StrToInt(ParamValue)
      else if SameText(ParamName,'MASS') then
        Mass := StrToFloatDef(ParamValue, 0.0)
      else if SameText(ParamName,'E') then
        EnergyCoeffs[0] := StrToFloatDef(ParamValue, 0.0)
      else if SameText(ParamName,'DE') then
        EnergyCoeffs[1] := StrToFloatDef(ParamValue, 0.0)
      else if SameText(ParamName,'DE1') then
        EnergyCoeffs[2] := StrToFloatDef(ParamValue, 0.0)
      else if SameText(ParamName,'DE2') then
        EnergyCoeffs[3] := StrToFloatDef(ParamValue, 0.0)
      else if SameText(ParamName,'DE3') then
        EnergyCoeffs[4] := StrToFloatDef(ParamValue, 0.0)
      else if SameText(ParamName,'COMMENT') then
        Comment := ParamValue;
    end;

    // spectr data reading
    i:=0;
    While SpeFileStream.Read(BuffCount,4)=4 do
    begin
      SetLength(Data,i+1);
      Data[i]:=BuffCount;
      i:=i+1;
    end;
    BuffSize:=Length(Data);
    SpeFileStream.Free;
  end;

  // проверки на успешность считывания, пока нет
  Result:=true;
  DecimalSeparator := CurDecSep;
  DateSeparator := CurrDS;
end;

procedure TSpectrum.StrToMassVol(var Val, DVal: Double; ValStr: String);
var CurrDecSep: Char;
begin
  CurrDecSep:=DecimalSeparator;
  DecimalSeparator:='.';
  try
    Val:=StrToFloatDef(ExtractWord(1,ValStr,[';']),0);
    DVal:=StrToFloatDef(ExtractWord(1,ValStr,[';']),0);
  finally
    DecimalSeparator:=CurrDecSep;
  end;
end;

function TSpectrum.ObtainCallibration(var EnDegree: Integer; var CalibrCoeffs: TDoubleArray; calibrstr: string): Boolean;
var S: string;
    i: Integer;
    CurrDecSep: Char;
begin
  Result := False;
  S := ExtractWord(1, calibrstr, [',']);
  if (S <> '') then
  begin
    EnDegree := StrToInt(S);
    CurrDecSep := DecimalSeparator;
    DecimalSeparator := '.';
    SetLength(CalibrCoeffs, EnDegree + 1);
    for i := 0 to EnDegree do
      CalibrCoeffs[i] := StrToFloat(ExtractWord(i + 2, calibrstr, [',']));
    DecimalSeparator := CurrDecSep;
  end;
  Result := ((EnDegree+1) = Length(CalibrCoeffs));
end;

function TSpectrum.FileOpen(FileName: string): Boolean;
var Extension: string;
begin
  Result := False;
  Extension := ExtractFileExt(FileName);
  //Delete(Extension,1,1); // удаление точки из расширения
  if SameText(Extension, '.txt') then
    Result := TxtOpen(FileName)
  else if SameText(Extension, '.spe') then
    Result := SpeOpen(FileName)
  else if SameText(Extension, '.csv') then
    Result := csvOpen(FileName);
end;

function TSpectrum.FileSave(FileName: string): Boolean;
var Extension: string;
begin
  Result:=False;
  Extension:=ExtractFileExt(FileName);
  Delete(Extension,1,1); // удаление точки из расширения
  if SameText(Extension,'txt') then Result:=TxtSave(FileName)
  else Result:=SpeSave(FileName);
end;

procedure TSpectrum.ReadFromIni(IniName: string);
var IniFile: TIniFile;
    DateStr, TimeStr: string;
begin
  IniFile:=TIniFile.Create(IniName);
  try
    SpectrName:=IniFile.ReadString('SpectrParams', 'SpeName', '');
    DateStr:=IniFile.ReadString('SpectrParams', 'MeasDate', '');
    TimeStr:=IniFile.ReadString('SpectrParams', 'MeasTime', '');
    LT:= IniFile.ReadFloat('SpectrParams', 'LiveTime', 0);
    RT:= IniFile.ReadFloat('SpectrParams', 'RealTime', 0);
    Geometry:=IniFile.ReadString('SpectrParams', 'Geometry', '');
    Distance:= IniFile.ReadFloat('SpectrParams', 'Distance', 0);
    Mass:= IniFile.ReadFloat('SpectrParams', 'Mass', 0);
    Volume:= IniFile.ReadFloat('SpectrParams', 'Volume', 0);
    Comment:=IniFile.ReadString('SpectrParams', 'Comment', '');
    if DateStr='' then MeasureDate:=Date else MeasureDate:=StrToDate(DateStr);
    if TimeStr='' then MeasureTime:=Time else MeasureTime:=StrToTime(TimeStr);
  finally
    IniFile.Free;
  end;
end;

function TSpectrum.Energy(Channel: Word): Double;
var i: Integer;
begin
  Result:=0;
  if (IsEnergyLoad) then
    for i:=0 to EnergyDegree do
      Result:=Result+EnergyCoeffs[i]*Power(Channel,i)
  else  Result:=-100;
end;

function TSpectrum.FWHM(E: double): Double;
var i: Integer;
begin
  Result:=0;
  if (FWHMDegree>=0) and (E>=0) then
    for i:=0 to FWHMDegree do
      Result:=Result+FWHMCoeffs[i]*Power(E,i/2)
  else  Result:=-100;
end;

function TSpectrum.CalibrToStr(CalibrDegree: Integer;
  CalibrCoeffs: TDoubleArray): string;
var i: Integer;
    CurDecSep: Char;
begin
  if (CalibrDegree>=0) and Assigned(CalibrCoeffs) then
  begin
    CurDecSep:=DecimalSeparator;
    DecimalSeparator:='.';

    Result:=IntToStr(CalibrDegree);
    for i:=0 to CalibrDegree do
      Result:=Result+','+FloatToStr(CalibrCoeffs[i]);

    DecimalSeparator:=CurDecSep;
  end;
end;

function ppd2scintil(Spectr: TSpectrum; FWHMscint: Double; Enscint: double; ChNumScint: Integer): TSpectrum;
var i,j, j1, j2: Integer;
    ChannelScale: Word; // число старых каналов в новом
    TempCount: Double; // рассчитанный счёт для сцинтилляционного спектра неокруглённый
    TempSpectr: TSpectrum; // временный спектр для работы
    FWHMppd: Double; // FWHM для ппд в точке Escint
  function Sigma(k: Integer): Double; // вычисления сигма сцинтиллятора по сложной схеме (заменить потом)
  var E, FW: Double; //текущая энергия и полуширина сцинт
  begin
    E:=Spectr.Energy(k);
    if E>-100 then
    begin
      if E<0 then E:=0;
      FW:=Spectr.FWHM(E);
      if FW>0 then
        Result:=FW*FWHMscint/FWHMppd/2.35482/Spectr.EnergyCoeffs[1] //2.35482 = 2*sqrt(2*ln(2))
      else
        Result:=0;
    end
    else Result:=0;
    //Result:=ChannelScale*((FWHMscint-FWHMppd)/(Chscint-Chppd)*k/ChannelScale+(Chscint*FWHMppd-Chppd*FWHMscint)/(Chscint-Chppd))/2.35482; // 2.35482 = 2*sqrt(2*ln(2)) , a*k+b
  end;
begin
  TempSpectr:=TSpectrum.Create;
  ChannelScale:=Trunc(Spectr.BuffSize/ChNumScint);
  TempSpectr.BuffSize:=Spectr.BuffSize;
  SetLength(TempSpectr.Data,TempSpectr.BuffSize);
  FWHMppd:=Spectr.FWHM(Enscint);
  if FWHMppd<0 then
  begin
    Result:=Spectr;
    Exit;
  end;

  // функция свёртки спектра ппд с гауссом в области 4 сигма сцинтиллятора
  for i:=0 to Spectr.BuffSize-1 do
  begin
    // установка границ свёртки
    j1:=i-4*Trunc(Sigma(i));
    j2:=i+4*Trunc(Sigma(i));
    // проверка граничных условий
    if j1<0 then j1:=0;
    if j2>=Spectr.BuffSize then j2:=Spectr.BuffSize-1;

    TempSpectr.Data[i]:=0;
    TempCount:=0;
    // собственно сама свёртка с гауссом, где сигма должна определяться из полуширины
    for j:=j1 to j2 do
      TempCount:=TempCount+Spectr.Data[j]*exp(-(i-j)*(i-j)/(2*Sigma(j)*Sigma(j)))*1/Sqrt(2*Pi)/Sigma(j);
    TempSpectr.Data[i]:=Round(TempCount);
  end;
  //TempSpectr.ChannelReduction(ChNumScint); // сокращение числа каналов
  //Result:=TSpectrum.Create;
  Result:=Spectr;
  Result.Data:=TempSpectr.Data;
  TempSpectr.Free;

  if ChannelScale>1 then Result.ChannelReduction(ChNumScint); // сокращение числа каналов, преобразование энергетической калибровки
  for i:=0 to Result.FWHMDegree do
    Result.FWHMCoeffs[i]:=Result.FWHMCoeffs[i]*FWHMscint/FWHMppd;
end;

function TSpectrum.CalcCPS: Double;
var i: Integer;
begin
  Result:=0;
  if Assigned(Data) then
    for i:=0 to Length(Data)-1 do
      Result:=Result+Data[i];
  if LT>0 then Result:=Result/LT else Result:=0;
end;

procedure TSpectrum.ClearData;
var i: Integer;
begin
  if (BuffSize>0) then
    for i:=0 to BuffSize-1 do
      Data[i]:=0;
end;

function TSpectrum.CalcIntegralBetweenLLD: Integer;
var i: Integer;
begin
  Result:=0;
  if (LLD<0) then
    LLD := 0;
  if (ULD<=0) or (ULD >= BuffSize) then
    ULD := BuffSize-1;
  for i:=LLD to ULD do
    Result := Result+Data[i];
end;

function TSpectrum.csvOpen(FileName: string): Boolean;
const US_2_S = 1000000;
var F: TextFile;
    i: Integer;
    Str: String;
    HeadEnd: Boolean; // проверка на конец шапки
    CurDecSep: Char;
    EnCal_A1: Double;
begin
  Result := False;
  HeadEnd := False;
  CurDecSep:=DecimalSeparator;
  DecimalSeparator:='.';

  AssignFile(F, FileName); // ввод имени файла
  {$I-}Reset(F) {$I+};
  if IOResult<>0 then
    begin
      //ShowMessage('Ошибка открытия файла '+FileName);
      exit;
    end;

  // Считывание шапки
  While (not EoF(F)) and (not HeadEnd) do
  begin
    Readln(F, Str); // считывание AcqTIMEus, LifeTIMEus, Ch
    //if Copy(Str,1,A)='Samples' then RT:=StrToFloat(Copy(Str,A+1,Length(Str)));
    if (Pos('LifeTIMEus,',Str)>0) then begin
      Str := ExtractWord(2, Str, [',']);
      LT := StrToFloat(Str)/US_2_S;
    end
    else if (Pos('AcqTIMEus,',Str)>0) then begin
      Str := ExtractWord(2, Str, [',']);
      RT := StrToFloat(Str)/US_2_S;
    end
    else if (Pos('Ch,',Str)>0) then
    begin
      //BuffSize:=StrToInt(Copy(Str,A+1,Length(Str)));
      HeadEnd:=true;
      Readln(F,Str);
    end;
  end;

  if (not HeadEnd) then
  begin
    DecimalSeparator:=CurDecSep;
    ShowMessage('Неверный формат файла');
    Exit;
  end;

  // вычисление параметров
  SpectrName := ExtractFileName(FileName);
  SpectrName := Copy(SpectrName,1,Length(SpectrName)-Length(ExtractFileExt(SpectrName))); // имя спектра = имя файла
  BuffSize := 1024;
  SetLength(Data, BuffSize*sizeof(Integer)); // установка числа каналов в спектре

  // Считывание спектра
  EnCal_A1 := 0;
  Readln(F, Str); // чтение из файла в массив
  While (not EoF(F)) and (Str <> '') do
  begin
    i := StrToInt(ExtractWord(1, Str, [',']));
    if (i >= BuffSize) then begin
      BuffSize := i+1;
      SetLength(Data, BuffSize);
    end;
    EnCal_A1 := StrToInt(ExtractWord(2, Str, [',']));
    if (i <> 0) then
      EnCal_A1 := EnCal_A1 / i;
    Data[i] := StrToInt(ExtractWord(3, Str, [',']));
    Readln(F, Str);
  end;

  // energy calibration
  EnergyDegree := 1;
  SetLength(EnergyCoeffs, EnergyDegree+1);
  EnergyCoeffs[0] := 0;
  EnergyCoeffs[1] := EnCal_A1;
  IsEnergyLoad := True;

  DecimalSeparator := CurDecSep;
  CloseFile(F);
  Result := True;
end;

constructor TSpectrum.CopySpectrum(const aSpectr: TSpectrum);
var i: Integer;
begin
  BuffSize := aSpectr.BuffSize;
  SetLength(Data, BuffSize);
  for i:=0 to BuffSize-1 do
    Data[i] := aSpectr.Data[i];
  LT := aSpectr.LT;
  RT := aSpectr.RT;
  //MeasureDate, MeasureTime: TDateTime; // дата и время измерений
  SpectrName := aSpectr.SpectrName;
  Geometry := aSpectr.Geometry;
  Distance := aSpectr.Distance;
  Mass := aSpectr.Mass;
  DMass := aSpectr.DMass;
  Volume := aSpectr.Volume;
  DVolume  := aSpectr.DVolume;
  SpectrType := aSpectr.SpectrType;
  ConfigName := aSpectr.ConfigName;
  OperatorName := aSpectr.OperatorName;
  Detector := aSpectr.Detector;
  Material := aSpectr.Material;
  EnergyDegree := aSpectr.EnergyDegree;
  FWHMDegree := aSpectr.FWHMDegree;
  SetLength(EnergyCoeffs, EnergyDegree+1);
  SetLength(FWHMCoeffs, FWHMDegree+1);
  for i:=0 to EnergyDegree do
    EnergyCoeffs[i] := aSpectr.EnergyCoeffs[i];
  for i:=0 to FWHMDegree do
    FWHMCoeffs[i] := aSpectr.FWHMCoeffs[i];

  IsEnergyLoad := aSpectr.IsEnergyLoad;
  IsFWHMLoad := aSpectr.IsFWHMLoad;
  CPS := aSpectr.CPS;
  Comment := aSpectr.Comment;
  LLD := aSpectr.LLD;
  ULD := aSpectr.ULD;
end;

function TSpectrum.SpeSaveSafe(const filename: string): Boolean;
var SpeFileStream: TFileStreamLn;
    CurrDecSep: Char;
    fileStream: TFileStreamLn;
begin
  Result := False;
  CurrDecSep := DecimalSeparator;
  DecimalSeparator := '.';
  // Сохранение шапки в файл
  fileStream := TFileStreamLn.Create(filename, fmCreate or fmShareDenyRead, fmshareDenyNone);
  try
    fileStream.Writeln('SHIFR=' + SpectrName);
    fileStream.Writeln('TYPE=' + SpectrType);
    fileStream.Writeln('CONFIGNAME=' + ConfigName);
    fileStream.Writeln('MEASBEGIN=' + FormatDateTime('DD-MM-YY', MeasureDate) + ' ' + FormatDateTime('hh:nn:ss.zz', MeasureTime));
    fileStream.Writeln('TLIVE=' + FloatToStr(LT));
    fileStream.Writeln('TREAL=' + FloatToStr(RT));
    fileStream.Writeln('OPERATOR=' + OperatorName);
    fileStream.Writeln('GEOMETRY=' + Geometry);
    fileStream.Writeln('DETECTOR=' + Detector);
    fileStream.Writeln('MATERIAL=' + Material);
    fileStream.Writeln('DISTANCE=' + FloatToStr(Distance));
    fileStream.Writeln('SAMPLEMASS=' + FloatToStr(Mass) + ';' + FloatToStr(DMass));
    fileStream.Writeln('SAMPLEVOLUME=' + FloatToStr(Volume) + ';' + FloatToStr(DVolume));
    fileStream.Writeln('ENERGY=' + CalibrToStr(EnergyDegree, EnergyCoeffs));
    fileStream.Writeln('FWHM=' + CalibrToStr(FWHMDegree, FWHMCoeffs));
    fileStream.Writeln('CPS=' + FloatToStr(CPS));
    fileStream.Writeln('COMMENT=' + Comment);
  finally
    fileStream.Free();
    DecimalSeparator := CurrDecSep;
  end;

  // Сохранение спектра
  SpeFileStream := TFileStreamLn.Create(FileName, fmCreate or fmShareDenyRead);
  try
    SpeFileStream.Seek(0, soFromEnd);
    SpeFileStream.Write('SPECTR=', SizeOf(Char) * Length('SPECTR='));
    SpeFileStream.Write(Data[0], SizeOf(Integer)*BuffSize);
  finally
    SpeFileStream.Free;
  end;
  Result := True;
end;

function TSpectrum.SpeOpenSafe(FileName: string): Boolean;
var fileStream, SpeFileStream: TFileStreamLn;
    currString, restString: String; //текущая строка
    BuffInt: Integer;
    i: Integer;
    CurrDecSep: Char;
begin
  Result := False;

  CurrDecSep := DecimalSeparator;
  DecimalSeparator := '.';
  fileStream := TFileStreamLn.Create(filename, fmOpenRead or fmShareDenyWrite);
  try 
    while (fileStream.ReadToChar(currString, '=')) do begin
      if (SameText(currString, 'SPECTR')) then
        Break;

      fileStream.Readln(restString);
      if (SameText(currString, 'SHIFR')) then
        SpectrName := restString
      else if (SameText(currString, 'TLIVE')) then
        LT := StrToFloatDef(restString, -1)
      else if (SameText(currString, 'TREAL')) then
        RT := StrToFloatDef(restString, -1)
      else if (SameText(currString, 'MEASBEGIN')) then
        ObtainDateTimeFromString(MeasureDate, MeasureTime, restString)
      else if (SameText(currString, 'GEOMETRY')) then
        Geometry := restString
      else if (SameText(currString, 'DISTANCE')) then
        Distance := StrToFloatDef(restString, 0)
      else if (SameText(currString, 'SAMPLEMASS')) then
        StrToMassVol(Mass, DMass, restString)
      else if (SameText(currString, 'SAMPLEVOLUME')) then
        StrToMassVol(Volume, DVolume, restString)
      else if (SameText(currString, 'ENERGY')) then
        IsEnergyLoad := ObtainCallibration(EnergyDegree, EnergyCoeffs, restString)
      else if (SameText(currString, 'FWHM')) then
        IsEnergyLoad := ObtainCallibration(EnergyDegree, EnergyCoeffs, restString)
      else if (SameText(currString, 'TYPE')) then
        SpectrType := restString
      else if (SameText(currString, 'CONFIGNAME')) then
        ConfigName := restString
      else if (SameText(currString, 'OPERATOR')) then
        OperatorName := restString
      else if (SameText(currString, 'DETECTOR')) then
        Detector := restString
      else if (SameText(currString, 'MATERIAL')) then
        Material := restString
      else if (SameText(currString, 'MATERIAL')) then
        CPS := StrToFloatDef(restString, 0)
      else if (SameText(currString, 'COMMENT')) then
        Comment := restString
    end;
  finally
    fileStream.Free();
    DecimalSeparator := CurrDecSep;
  end;

  // Чтение спектра
  if FileExists(FileName) then // проверка на существование фала
  begin
    SpeFileStream := TFileStreamln.Create(FileName, fmOpenRead or fmShareDenyWrite); // открытие файла spe на чтение

    try
      if SpeFileStream.ReadEqual(CurrString) then SpeFileStream.Readln(RestString); // считывание названия параметра
      While AnsiCompareStr(CurrString, 'SPECTR=')<>0 do // проверка на равенство строки SPEСTR=
      begin
        if SpeFileStream.ReadEqual(CurrString) and (AnsiCompareStr(CurrString,'SPECTR=')<>0) then SpeFileStream.Readln(RestString);; // считывание названия параметра
      end;

      i:=0;
      While SpeFileStream.Read(BuffInt,4)=4 do
      begin
        SetLength(Data,i+1);
        Data[i]:=BuffInt;
        i:=i+1;
      end;
      BuffSize:=Length(Data);
      Result:=true;
    finally
      SpeFileStream.Free();
    end;
  end;
end;

end.
