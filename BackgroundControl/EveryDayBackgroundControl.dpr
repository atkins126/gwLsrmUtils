program EveryDayBackgroundControl;
{программа для контроля фона, чтением файлов Result программы SpectraLine}
{$APPTYPE CONSOLE}

uses
  SysUtils, Math,
  StringListUnicodeSupport;  // для поддержки Unicode TStringList'ом;;

var AppPath, BackgndName1, BackgndName2: String;
    BackgndFile1, BackgndFile2: TextFile; // файл Result
    S: String = ' '; // строка куда считывается из файла
    S1: string;
    A: ShortInt; // позиция '='
    LT1, LT2: Real; // живое время
    Integral1, Integral2: Integer; // интеграл
    CR1, CR2, DCR1, DCR2: Real; // countRate1, ...2, DeltaCountRate1, ...2 - скорости счёта и погрешности
    BackgndDif, BackgndDev: Real; // отклонение фона
    ErrorLimit: Real; // систематическая погрешность, которую можно учесть при сравнении фонов в долях 1
    AbsErrorLimit: Double; // абсолютная систематическая погрешность, которую можно учесть при сравнении фонов
    NumFormat: TFormatSettings; // формат для числа в Result.txt

//function max()

begin
  LT1 := 0; LT2 := 0;
  NumFormat.DecimalSeparator := '.';
  Integral1 := 0; Integral2 := 0;
  AppPath := ExtractFilePath(ParamStr(0));

  // чтение командной строки
  if (ParamCount < 2) then
  begin
    Writeln('EveryDayBackgroundControl is programm for everyday control background, version 1.0.5 beta');
    Writeln('Format: EveryDayBackgroundControl.exe <ResultName_from_old_Background.txt> <ResultName_from_new_Background.txt> <ErrorLevel>');
    Writeln('It returns countrate from old and new background spectra, difference between them in percent and Chi. ErrorLevel is systematic error in %, which is taken into account. Default value of ErrorLevel is 0%');
    Readln(S);
    exit;
  end;

  BackgndName1 := ParamStr(1); // возвращает название Result-файла
  BackgndName2 := ParamStr(2);
  if (Pos(':', BackgndName1) = 0) then BackgndName1 := AppPath + BackgndName1;
  if (Pos(':', BackgndName2) = 0) then BackgndName2 := AppPath + BackgndName2;
  
  if (ParamCount >= 3) then
  begin
    try
      ErrorLimit := StrToFloat(ParamStr(3), NumFormat) / 100;
    except
      on EConvertError do
      begin
        Writeln('Input <ErrorLimit> is not numeric value. Used default value');
        ErrorLimit := 0;
      end
    else
      ErrorLimit := 0;
    end;
  end
  else ErrorLimit := 0; // дефолтное значение систем. погрешности 0 %

  // check file existence
  if not (FileExists(BackgndName1) and FileExists(BackgndName2)) then // проверка на существование файлов
  begin
    Writeln('Wrong file names');
    Readln(S);
    exit;
  end;

  // open files
  AssignFile(BackgndFile1, BackgndName1);
  {$I-}Reset(BackgndFile1);{$I+}
  if (IOResult <> 0) then // проверка на возможность прочитать
  begin
    Writeln('Error openning file ', BackgndName1);
    Readln(S);
    exit;
  end;

  // считывание шапки старого фона
  try
    While not (EoF(BackgndFile1)) do
    begin
      Readln(BackgndFile1, S); 
      if (Length(S) = 0) then Continue;
      if (S[1] = '#') then Break; 
      A := Pos('=', S);
      if (A > 0) then
      begin
        S1 := Copy(S, 1, A);
        if (SameText(S1, 'Живое=') or SameText(S1, 'Live=')) then
          LT1 := StrToFloatDef(Copy(S, A+1,Length(S)), 0, NumFormat);
        if (SameText(S1, 'Интеграл=') or SameText(S1, 'Integral=')) then
          Integral1 := Trunc(StrToFloatDef(Copy(S, A+1, Length(S)), 0, NumFormat));
      end;
    end;
  except
    on EConvertError do
      begin
        Writeln('Error converting values ');
        Readln(S);
        Exit;
      end;
  else
    Writeln('Error reading file ',BackgndName1);
    Readln(S);
    exit;
  end;

  CloseFile(BackgndFile1);

  AssignFile(BackgndFile2, BackgndName2);
  {$I-}Reset(BackgndFile2);{$I+}
  if (IOResult <> 0) then // проверка на возможность прочитать
  begin
    Writeln('Error openning file ', BackgndName2);
    Readln(S);
    exit;
  end;

  S:='';
  // считывание шапки нового фона
  try
    While not (EoF(BackgndFile2)) do
    begin
      Readln(BackgndFile2, S); 
      if (Length(S) = 0) then Continue;
      if (S[1] = '#') then Break; 
      A := Pos('=', S);
      if (A > 0) then
      begin
        S1 := Copy(S, 1, A);
        if (SameText(S1, 'Живое=') or SameText(S1, 'Live=')) then
          LT2 := StrToFloatDef(Copy(S, A+1,Length(S)), 0, NumFormat);
        if (SameText(S1, 'Интеграл=') or SameText(S1, 'Integral=')) then
          Integral2 := Trunc(StrToFloatDef(Copy(S, A+1, Length(S)), 0, NumFormat));
      end;
    end;
  except
    on EConvertError do
      begin
        Writeln('Error converting values ');
        Readln(S);
        Exit;
      end;
  else
    Writeln('Error reading file ', BackgndName2);
    Readln(S);
    exit;
  end;

  CloseFile(BackgndFile2);

  // проверка на то, что значения считались
  try
    if (LT1 = 0) or (LT2 = 0) then
    begin
      Writeln('Wrong values of LiveTimes');
      Readln(S);
      exit;
    end;
  except
    Writeln('Wrong file format');
    Readln(S);
    exit;
  end;

  // проверка фона на уход
  CR1 := Integral1 / LT1;  // скорость счёта старого фона
  CR2 := Integral2 / LT2;  // скорость счёта нового фона
  DCR1 := 2 * Sqrt(Integral1) / LT1;  //погрешность ск-ти счёта старого фона
  DCR2 := 2 * Sqrt(Integral2) / LT2;
  BackgndDif := 2 * (CR2 - CR1)/(CR2 + CR1); // разница между старым и новым фоном
  AbsErrorLimit := ErrorLimit * (CR2 + CR1)/2; // абсолютная систематическая погрешность
  if (DCR1 <> 0) or (DCR2 <> 0) then
    BackgndDev := (CR2 - CR1)/Sqrt(DCR1*DCR1 + DCR2*DCR2 + AbsErrorLimit*AbsErrorLimit)
  else
    BackgndDev := 0;

  // Вывод инфо на экран
  Writeln('Old background countrate: ', FormatFloat('0.###', CR1), '+/-', FormatFloat('0.###', DCR1), ' (', FormatFloat('0.##',100*DCR1/CR1),'%)');
  Writeln('New background countrate: ', FormatFloat('0.###', CR2), '+/-', FormatFloat('0.###', DCR2), ' (', FormatFloat('0.##',100*DCR2/CR2),'%)');
  Writeln('Background difference: ', FormatFloat('0.###',BackgndDif*100), ' %');
  Writeln('Background Chi: ', FormatFloat('0.##',BackgndDev));
  
  if (BackgndDev > 5) then Writeln('Your backround is much lager than old, you need find a contamination or write new backround to configuration')
  else if BackgndDev>3 then Writeln('Your backround is lager than old, maybe you need write new backround to configuration')
  else if BackgndDev>2 then Writeln('Your backround is slightly lager than old, maybe you need write new backround to configuration')
  else if (BackgndDev < -5) then Writeln('Now background is much lower than old one')
  else if (BackgndDev < -3) then Writeln('Now background is lower than old one')
  else if (BackgndDev < -2) then Writeln('Now background is slightly lower than old one')
  else Writeln('Background is not changed');

  Readln(S);
end.
