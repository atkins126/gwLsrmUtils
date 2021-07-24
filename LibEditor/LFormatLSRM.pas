unit LFormatLSRM;

interface

uses SysUtils, Math;

type

  TResultFormat = (rfMetrologic, rfScientific, rfSimpleScientific);

//Если число <1E-5, то выводим его в формате Е; в противном случае в формате с точкой
function LSRMFormatFloat(Val: Double): string; overload;
function LSRMFormatFloat(Val: Double; const FormatSettings: TFormatSettings): string; overload;

//Округление, которое зависит от разряда самого числа
function LSRMRoundFormat2(Val: Double): string; overload;
function LSRMRoundFormat2(Val: Double; const FormatSettings: TFormatSettings): string; overload;

{LSRMRoundFormat - функции метрологического округления
Каждая цифра в числе проверяется на значимость:
если цифра <3, то она считается значимой и рассматривается следующая за ней;
если цифра >=3, то она незначимая; округление делается до первой незначимой цифры.
Пример, LSRMRoundFormat(2434,23) = 2400; первая незначимая цифра 4, значит округляем до неё
LSRMRoundFormat с одним параметром предназначена для вывода в метр. формате значения погрешности (dVal) величины.
LSRMRoundFormat с двумя параметрами предназначена для вывода в метр. формате значения самой величины (Val);
Округление величины (Val) определяется положением значищих цифр в погрешности (dVal)}
function LSRMRoundFormat(Val, dVal: Double): string; overload;
function LSRMRoundFormat(Val, dVal: Double; const FormatSettings: TFormatSettings): string; overload;
function LSRMRoundFormat(dVal: Double): string; overload;
function LSRMRoundFormat(dVal: Double; const FormatSettings: TFormatSettings): string; overload;
{В научном формате}
function LSRMRoundFormatScience(Val, dVal: Double; const FormatSettings: TFormatSettings): string; overload;
function LSRMRoundFormatScience(dVal: Double; const FormatSettings: TFormatSettings): string; overload;
{В научном формате без учета метрологического округления (простое округление до Precision знаков)}
function LSRMRoundFormatSimpleScience(Val: Double; Precision: Integer; const FormatSettings: TFormatSettings): string;
{Хелпер}
function LSRMFormatOutput(Val, dVal: Double; const FormatSettings: TFormatSettings; ResultFormat: TResultFormat): string; overload;
function LSRMFormatOutput(dVal: Double; const FormatSettings: TFormatSettings; ResultFormat: TResultFormat): string; overload;

//Проверка соответствия числа и погрешности с учетом уровня значимости
function CheckError(A, dA, ErrorUpperLevel: double): Boolean;
//Получение величины и её погрешности с учетом уровня значимости в метрологическом формате для вывода на экран и в отчет
function GetValueStr(Value, dValue, ErrorUpperLevel: Double): string;
function GetDValueStr(Value, dValue, ErrorUpperLevel: Double): string;

{Округление до 2 значащих цифр}
function RoundTo2Sign(Val: Double): Double;

implementation

const
  MAX_VAL = 10E+17;
  MAX_STR = '10E+18';

  DefaultPrecision = 4;

function LSRMFormatScience(Val: Double; Precision: Integer; const FormatSettings: TFormatSettings): string;
begin
  Result:= FloatToStrF(Val, ffExponent, Precision, 2, FormatSettings);
end;  

function LSRMFormatFloat(Val: Double): string; overload;
var
  fmtString: string;
begin
  if Abs(Val) < 0.00001 then
  begin
    fmtString:= '%.3e';
    Result:= Format(fmtString, [Val]);
  end
  else
    Result:= FormatFloat('0.######', Val);
  if Val = 0 then
    Result:= '0';
end;

function LSRMFormatFloat(Val: Double; const FormatSettings: TFormatSettings): string; overload;
var
  fmtString: string;
begin
  if Abs(Val) < 0.00001 then
  begin
    fmtString:= '%.3e';
    Result:= Format(fmtString, [Val], FormatSettings);
  end
  else
    Result:= FormatFloat('0.######', Val, FormatSettings);
  if Val = 0 then
    Result:= '0';
end;

function LSRMFormatFloat2(Val: Double; Ind: integer): string; overload;
var
  fmtString, fmtFormat: string;
  I: integer;
begin
  fmtFormat:= '0.######';
  if Ind < 0 then
  begin
    fmtFormat:= '0.';
    for I:= -1 downto Ind do
      fmtFormat:= fmtFormat + '0';
  end;
  if Abs(Val) < 0.00001 then
  begin
    fmtString:= '%.3e';
    Result:= Format(fmtString, [Val]);
  end
  else
    Result:= FormatFloat(fmtFormat, Val);
  if Val = 0 then
    Result:= '0';
end;

function LSRMFormatFloat2(Val: Double; Ind: integer; const FormatSettings: TFormatSettings): string; overload;
var
  fmtString, fmtFormat: string;
  I: integer;
begin
  fmtFormat:= '0.######';
  if Ind < 0 then
  begin
    fmtFormat:= '0.';
    for I:= -1 downto Ind do
      fmtFormat:= fmtFormat + '0';
  end;
  if Abs(Val) < 0.00001 then
  begin
    fmtString:= '%.3e';
    Result:= Format(fmtString, [Val], FormatSettings);
  end
  else
    Result:= FormatFloat(fmtFormat, Val, FormatSettings);
  if Val = 0 then
    Result:= '0';
end;

procedure FindFormat(Val: Double; var Ind: integer; var FUse: boolean); overload;
var
  str_work: string;
  i, j, n: integer;
begin
  FUse:= true;
  str_work:= FormatFloat('0.################################', Val);
  j:= 0;
  for i:= 1 to Length(str_work) do
    if (str_work[i] <> ',') and (str_work[i] <> '.') and (str_work[i] <> '-') then
      if StrToInt(str_work[i]) <> 0 then
      begin
        j:= i;
        Break;
      end;
  if j = 0 then
    FUse:= false
  else
  begin
    if StrToInt(str_work[j]) < 3 then
    begin
      j:= j + 1;
      i:=StrToInt(str_work[j-1]);
      if (i=2) and (j<Length(str_work)) then
        if (StrToInt(str_work[j])=9) and (StrToInt(str_work[j+1])>=5) then
          j:=j-1;
    end;

    n:= Pos(DecimalSeparator, str_work);
    if n = 0 then
      Ind:= Length(str_work) - j
    else if j < n + 1 then
      Ind:= n - j - 1
    else
      Ind:= n - j;
  end;
end;

procedure FindFormat(Val: Double; var Ind: integer; var FUse: boolean; const FormatSettings: TFormatSettings); overload;
var
  str_work: string;
  i, j, n: integer;
begin
  FUse:= true;
  str_work:= FormatFloat('0.################################', Val);
  j:= 0;
  for i:= 1 to Length(str_work) do
    if (str_work[i] <> ',') and (str_work[i] <> '.') and (str_work[i] <> '-') then
      if StrToInt(str_work[i]) <> 0 then
      begin
        j:= i;
        Break;
      end;
  if j = 0 then
    FUse:= false
  else
  begin
    if StrToInt(str_work[j]) < 3 then
    begin
      i:=StrToInt(str_work[j]);
      Inc(j);
      if (j < Length(str_work)) then begin
        if (str_work[j] = '.') or (str_work[j] = ',') then
          Inc(j);
        if (i=2) and (j<Length(str_work)) then
          if (StrToInt(str_work[j])=9) and (StrToInt(str_work[j+1])>=5) then
            j:=j-1;
      end;
    end;
    n:= Pos(FormatSettings.DecimalSeparator, str_work);
    if n = 0 then
      Ind:= Length(str_work) - j
    else if j < n + 1 then
      Ind:= n - j - 1
    else
      Ind:= n - j;
  end;
end;

function LSRMRoundFormat(Val, dVal: Double): string; overload;
var
  fmtString: string;
  FUse: Boolean;
  Ind: Integer;
begin
  if (Abs(Val) > MAX_VAL) or (Abs(dVal) > MAX_VAL) then
  begin
    Result:= MAX_STR;
    Exit;
  end;
  FindFormat(dVal, Ind, FUse);
  SetRoundMode(rmNearest);
  if FUse = true then
  try
    fmtString:= LSRMFormatFloat2(RoundTo(Val, Ind), Ind)
  except
    fmtString:= FloatToStr(Round(Val));
  end
  else
    fmtString:= LSRMFormatFloat(Val);
  Result:= fmtString;
end;

function LSRMRoundFormat(Val, dVal: Double; const FormatSettings: TFormatSettings): string; overload;
var
  fmtString: string;
  FUse: Boolean;
  Ind: Integer;
begin
  if (Abs(Val) > MAX_VAL) or (Abs(dVal) > MAX_VAL) then
  begin
    Result:= MAX_STR;
    Exit;
  end;
  FindFormat(dVal, Ind, FUse, FormatSettings);
  SetRoundMode(rmNearest);
  if FUse = true then
  try
    fmtString:= LSRMFormatFloat2(RoundTo(Val, Ind), Ind, FormatSettings)
  except
    fmtString:= FloatToStr(Round(Val), FormatSettings);
  end
  else
    fmtString:= LSRMFormatFloat(Val, FormatSettings);
  Result:= fmtString;
end;

function LSRMRoundFormat(dVal: Double): string; overload;
var
  fmtString: string;
  FUse: Boolean;
  Ind: Integer;
begin
  if Abs(dVal) > MAX_VAL then
  begin
    Result:= '-';
    Exit;
  end;
  FindFormat(dVal, Ind, FUse);
  SetRoundMode(rmNearest);
  if FUse = true then
    fmtString:= LSRMFormatFloat2(RoundTo(dVal, Ind), Ind)
  else
    fmtString:= LSRMFormatFloat(dVal);
  Result:= fmtString;
end;

function LSRMRoundFormat(dVal: Double; const FormatSettings: TFormatSettings): string; overload;
var
  fmtString: string;
  FUse: Boolean;
  Ind: Integer;
begin
  if Abs(dVal) > MAX_VAL then
  begin
    Result:= '-';
    Exit;
  end;
  FindFormat(dVal, Ind, FUse, FormatSettings);
  SetRoundMode(rmNearest);
  if FUse = true then
    fmtString:= LSRMFormatFloat2(RoundTo(dVal, Ind), Ind, FormatSettings)
  else
    fmtString:= LSRMFormatFloat(dVal, FormatSettings);
  Result:= fmtString;
end;

function LSRMRoundFormatScience(Val, dVal: Double; const FormatSettings: TFormatSettings): string; overload;
var
  FUse: Boolean;
  Ind: Integer;
begin
  if (Abs(Val) > MAX_VAL) or (Abs(dVal) > MAX_VAL) then
  begin
    Result:= MAX_STR;
    Exit;
  end;
  FindFormat(dVal, Ind, FUse);
  SetRoundMode(rmNearest);
  if FUse = true then
    Result:= LSRMFormatScience(RoundTo(Val, Ind), DefaultPrecision, FormatSettings)
  else
    Result:= LSRMFormatScience(Val, DefaultPrecision, FormatSettings);
end;

function LSRMRoundFormatScience(dVal: Double; const FormatSettings: TFormatSettings): string; overload;
var
  FUse: Boolean;
  Ind: Integer;
begin
  if Abs(dVal) > MAX_VAL then
  begin
    Result:= '-';
    Exit;
  end;
  FindFormat(dVal, Ind, FUse);
  SetRoundMode(rmNearest);
  if FUse = true then
    Result:= LSRMFormatScience(RoundTo(dVal, Ind), DefaultPrecision, FormatSettings)
  else
    Result:= LSRMFormatScience(dVal, DefaultPrecision, FormatSettings);
end;

function LSRMRoundFormatSimpleScience(Val: Double; Precision: Integer; const FormatSettings: TFormatSettings): string;
begin
  if Abs(Val) > MAX_VAL then
  begin
    Result:= '-';
    Exit;
  end;
  {SetRoundMode(rmNearest);}
  { TODO : Добавить округление в зависимости от Precision }
  Result:= LSRMFormatScience(Val, Precision, FormatSettings);
end;

function LSRMFormatOutput(Val, dVal: Double; const FormatSettings: TFormatSettings; ResultFormat: TResultFormat): string; overload;
begin
  case ResultFormat of
    rfMetrologic: Result:= LSRMRoundFormat(Val, dVal, FormatSettings);
    rfScientific: Result:= LSRMRoundFormatScience(Val, dVal, FormatSettings);
    rfSimpleScientific: Result:= LSRMRoundFormatSimpleScience(Val, DefaultPrecision, FormatSettings);
  end;
end;

function LSRMFormatOutput(dVal: Double; const FormatSettings: TFormatSettings; ResultFormat: TResultFormat): string; overload;
begin
  case ResultFormat of
    rfMetrologic: Result:= LSRMRoundFormat(dVal, FormatSettings);
    rfScientific: Result:= LSRMRoundFormatScience(dVal, FormatSettings);
    rfSimpleScientific: Result:= LSRMRoundFormatSimpleScience(dVal, DefaultPrecision, FormatSettings);
  end;
end;

function LSRMRoundFormat2(Val: Double): string; overload;
var
  fmtString: Double;
begin
  fmtString:= Val;
  if abs(Val) > 0.0001 then
    fmtString:= RoundTo(Val, -6);
  if abs(Val) > 0.001 then
    fmtString:= RoundTo(Val, -5);
  if abs(Val) > 0.01 then
    fmtString:= RoundTo(Val, -4);
  if abs(Val) > 0.1 then
    fmtString:= RoundTo(Val, -3);
  if abs(Val) > 0 then
    fmtString:= RoundTo(Val, -2);
  if abs(Val) > 10 then
    fmtString:= RoundTo(Val, -1);
  if abs(Val) > 100 then
    fmtString:= RoundTo(Val, 0);
  if abs(Val) > 1000 then
    fmtString:= RoundTo(Val, 1);
  if abs(Val) > 10000 then
    fmtString:= RoundTo(Val, 2);
  Result:= FormatFloat('0.##########', fmtString);
end;

function LSRMRoundFormat2(Val: Double; const FormatSettings: TFormatSettings): string; overload;
var
  fmtString: Double;
begin
  fmtString:= Val;
  if abs(Val) > 0.0001 then
    fmtString:= RoundTo(Val, -6);
  if abs(Val) > 0.001 then
    fmtString:= RoundTo(Val, -5);
  if abs(Val) > 0.01 then
    fmtString:= RoundTo(Val, -4);
  if abs(Val) > 0.1 then
    fmtString:= RoundTo(Val, -3);
  if abs(Val) > 0 then
    fmtString:= RoundTo(Val, -2);
  if abs(Val) > 10 then
    fmtString:= RoundTo(Val, -1);
  if abs(Val) > 100 then
    fmtString:= RoundTo(Val, 0);
  if abs(Val) > 1000 then
    fmtString:= RoundTo(Val, 1);
  if abs(Val) > 10000 then
    fmtString:= RoundTo(Val, 2);
  Result:= FormatFloat('0.##########', fmtString, FormatSettings);
end;

function CheckError(A, dA, ErrorUpperLevel: double): Boolean;
begin
  if A > 0 then
    Result:= dA / A * 100 > ErrorUpperLevel
  else
    Result:= True;
end;

function GetValueStr(Value, dValue, ErrorUpperLevel: Double): string;
var
  S: string;
begin
  if Value < MAX_VAL then
  begin
    if CheckError(Value, dValue, ErrorUpperLevel) then
    begin
      S:= LSRMRoundFormat(Abs(Value) + dValue, dValue);
      if S <> '0' then
        Result:= '< ' + S
      else
        Result:= S;
    end
    else
      Result:= LSRMRoundFormat(Value, dValue);
  end
  else
    Result:= '> ' + MAX_STR;
end;

function GetDValueStr(Value, dValue, ErrorUpperLevel: Double): string;
begin
  if not CheckError(Value, dValue, ErrorUpperLevel) then
    Result:= LSRMRoundFormat(dValue)
  else
    Result:= '';
end;

function RoundTo2Sign(Val: Double): Double;
begin
  if Abs(Val) < 0.0001 then
    Result:= RoundTo(Val, -6)
  else if (Abs(Val) > 0.0001) and (Abs(Val) < 0.001) then
    Result:= RoundTo(Val, -5)
  else if (Abs(Val) > 0.001) and (Abs(Val) < 0.01) then
    Result:= RoundTo(Val, -4)
  else if (Abs(Val) > 0.01) and (Abs(Val) < 0.1) then
    Result:= RoundTo(Val, -3)
  else if (Abs(Val) > 0.1) and (Abs(Val) < 1) then
    Result:= RoundTo(Val, -2)
  else if (Abs(Val) > 1) and (Abs(Val) < 10) then
    Result:= RoundTo(Val, -1)
  else if (Abs(Val) > 10) and (Abs(Val) < 100) then
    Result:= RoundTo(Val, 0)
  else if (Abs(Val) > 100) and (Abs(Val) < 1000) then
    Result:= RoundTo(Val, 1)
  else if Abs(Val) > 1000 then
    Result:= RoundTo(Val, 2)
  else
    Result:= Val;
end;  

end.

