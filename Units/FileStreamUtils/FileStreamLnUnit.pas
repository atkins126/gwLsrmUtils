unit FileStreamLnUnit;

interface

uses SysUtils, classes;

type
  TFileStreamLn = class(TFileStream) // class for binary files and can read and write strings (lines)
  public
    procedure Readln(var Str: String); // read text line
    function ReadEqual(var Str: String): Boolean; overload; // read line until '='
    function ReadEqual(var Str: String; var ErrorNumber: Integer): Boolean; overload; // read line until '=', ErrorNumber = 0 - read until =, =1 -- read until enter, =2 -- cannot read
    function ReadToChar(var Str: string; ch: Char): Boolean; //  read line until symbol ch
    function Read2numbersln(var n,m: LongInt): LongInt; // read line with 2 numbers
    function ReadLTRT(var StartTime: String; var RT,LT: Double): LongInt;
    function ReadCollectionTime(var StartTime,StopTime: String; var T: Double): LongInt;
    function ReadRownumbersln(var x: Array of LongInt; var L: Integer): LongInt; // read line with some numbers
    procedure Writeln(const Str: String); // write line and \n
    procedure WriteStr(const Str: String); // write line without \n
  end;

implementation

{ TFileStreamLn }

function TFileStreamLn.Read2numbersln(var n, m: Integer): LongInt; // чтение строки с 2-мя числами и возврат этих чисел
var Buffer: Byte; // переменная куда считывается промежуточный результат
    Str: String; // строка содержащая число
begin
  Str := '';
  Result := FileRead(FHandle, Buffer, 1); // считывание 1-го символа
  if (Result = -1) or (Result = 0) then
  begin
    Result := 0;
    exit;
  end;
  while Buffer <> $0D do
  begin
    if Buffer = $09 then
    begin
      n := StrToInt(Str); // получение 1-го числа
      Str := '';
      Result := FileRead(FHandle, Buffer, 1); // считывание следующего символа
      if Result <= 0 then
        exit;
    end;
    Str := Str + Chr(Buffer); // добавление считанного символа
    Result := FileRead(FHandle, Buffer, 1); // считывание следующего символа
    if Result <= 0 then
      exit;
  end;
  m := StrToInt(Str); // получение последнего числа
  FileRead(FHandle, Buffer, 1); // считывание символа $0A
end;

function TFileStreamLn.ReadCollectionTime(var StartTime, StopTime: String;
  var T: Double): LongInt;
var Buffer: Byte; // переменная куда считывается промежуточный результат
    Str: String; // строка содержащая число
    i: Integer; // i - счётчик
begin
  Str := '';
  i := 0;
  Result := FileRead(FHandle, Buffer, 1); // считывание 1-го символа
  if (Result = -1) or (Result = 0) then
  begin
    Result := 0;
    exit;
  end;
  while Buffer <> $0A do
  begin
    if Buffer = $20 then
    begin
      i := i + 1; // начало следующего блока
      case i of
        1:
          StartTime := Str; // получение даты начала отбора
        2:
          StartTime := StartTime + ' ' + Str; // добавление времени начала отбора
        3:
          StopTime := Str; // получение даты конца отбора
        4:
          StopTime := StopTime + ' ' + Str; // добавление времени конца отбора
        5:
          T := StrToFloat(Str); // получение времени отбора
      end;
      Str := '';
      while Buffer = $20 do // досчитывание пробелов
        Result := FileRead(FHandle, Buffer, 1); // считывание следующего символа
      if Result <= 0 then
        exit;
    end;
    Str := Str + Chr(Buffer); // добавление считанного символа
    Result := FileRead(FHandle, Buffer, 1); // считывание следующего символа
    if Result <= 0 then
      exit;
  end;
end;

function TFileStreamLn.ReadEqual(var Str: String): Boolean; // процедура чтения строки до равно
var Buffer: Byte;
begin
  Str := '';
  Buffer := 0;
  Result := False;
  while (Buffer <> 61) and (Buffer <> $0D) do
  begin
    if FileRead(FHandle, Buffer, 1) < 1 then
      exit; // считывание символа
    Str := Str + Char(Buffer); // дописывание строки этим символом
  end;
  if Buffer = $0D then
  begin
    FileRead(FHandle, Buffer, 1); // считывание символа $0A
  end
  else if Buffer = 61 then
    Result := True;
end;

function TFileStreamLn.ReadEqual(var Str: String;
  var ErrorNumber: Integer): Boolean;
var Buffer: Byte;
begin
  Str := '';
  Buffer := 0;
  Result := False;
  ErrorNumber := 3;
  while (Buffer <> 61) and (Buffer <> $0D) do
  begin
    if FileRead(FHandle, Buffer, 1) < 1 then
    begin
      ErrorNumber := 2;
      exit; // считывание символа
    end;
    if (Buffer <> $0D) then
      Str := Str + Char(Buffer); // дописывание строки этим символом
  end;
  if Buffer = $0D then
  begin
    FileRead(FHandle, Buffer, 1); // считывание символа $0A
    Result := True;
    ErrorNumber := 1;
  end
  else if Buffer = 61 then
  begin
    ErrorNumber := 0;
    Result := True;
  end;
end;

procedure TFileStreamLn.Readln(var Str: String); // процедура чтения строки
var Buffer: Byte; // переменная куда считывается промежуточный результат
begin
  Str := '';
  FileRead(FHandle, Buffer, 1); // считывание 1-го символа
  while Buffer <> $0D do
  begin
    Str := Str + Chr(Buffer); // добавление считанного символа
    FileRead(FHandle, Buffer, 1); // считывание следующего символа
  end;
  FileRead(FHandle, Buffer, 1); // считывание символа $0A
end;

function TFileStreamLn.ReadLTRT(var StartTime: String; var RT,LT: Double): LongInt;
var Buffer: Byte; // переменная куда считывается промежуточный результат
    Str: String; // строка содержащая число
    i: Integer; // i - счётчик
    n: Double; // n - для пробной записи числа,
    Error: Boolean; // есть ли ошибка
begin
  Str := '';
  StartTime := '';
  i := 0;
  n := 0;
  Error := FALSE;
  Result := FileRead(FHandle, Buffer, 1); // считывание 1-го символа
  if (Result = -1) or (Result = 0) then
  begin
    Result := 0;
    exit;
  end;
  while Buffer <> $0A do
  begin
    if Buffer = $20 then
    begin
      try
        n := StrToFloat(Str); // пробное получение числа
      except
        on EConvertError do
        begin
          StartTime := StartTime + Str + ' ';
          Error := TRUE;
        end;
      end;
      if not Error then
      begin
        i := i + 1;
        case i of
          1:
            RT := n;
          2:
            LT := n;
        end;
      end;
      Error := FALSE;
      Str := '';
      while Buffer = $20 do
        Result := FileRead(FHandle, Buffer, 1); // считывание следующего символа
      if Result <= 0 then
        exit;
    end;
    Str := Str + Chr(Buffer); // добавление считанного символа
    Result := FileRead(FHandle, Buffer, 1); // считывание следующего символа
    if Result <= 0 then
      exit;
  end;
  //m:=StrToInt(Str); // получение последнего числа
  //FileRead(FHandle, Buffer, 1); // считывание символа $0A
end;

function TFileStreamLn.ReadRownumbersln(var x: array of Integer;
  var L: Integer): LongInt;
var Buffer: Byte; // переменная куда считывается промежуточный результат
    Str: String; // строка содержащая число
begin
  //SetLength(x, 6);
  Str := '';
  L := 0;
  Result := FileRead(FHandle, Buffer, 1); // считывание 1-го символа
  if (Result = -1) or (Result = 0) or (Buffer = Ord('S')) then
  begin
    Result := 0;
    exit;
  end;
  while Buffer <> $0A do
  begin
    if Buffer = $20 then
    begin
      x[L] := StrToInt(Str); // получение 1-го числа
      Str := '';
      while Buffer = $20 do
        Result := FileRead(FHandle, Buffer, 1); // считывание следующего символа
      if Result <= 0 then
        exit;
      L := L + 1;
    end;
    Str := Str + Chr(Buffer); // добавление считанного символа
    Result := FileRead(FHandle, Buffer, 1); // считывание следующего символа
    if Result <= 0 then
      exit;
  end;
  //FileRead(FHandle, Buffer, 1); // считывание символа за $0A
  L := L - 1;
  //x[L]:=StrToInt(Str); // получение последнего числа
end;

function TFileStreamLn.ReadToChar(var Str: string; ch: Char): Boolean;
var Buffer: Byte;
begin
  Str := '';
  Buffer := 0;
  Result := False;
  while (Buffer <> Ord(ch)) and (Buffer <> $0D) do
  begin
    if FileRead(FHandle, Buffer, 1) < 1 then
      exit; // считывание символа
    if (Buffer <> Ord(ch)) then
      Str := Str + Char(Buffer); // дописывание строки этим символом
  end;
  if Buffer = $0D then
  begin
    FileRead(FHandle, Buffer, 1); // считывание символа $0A
  end
  else if Buffer = Ord(ch) then
    Result := True;
end;

procedure TFileStreamLn.Writeln(const Str: String);
var Buffer: Char;
    BufferB: Byte;
    i: Integer;
begin
  for i := 1 to Length(Str) do
  begin
    Buffer := Str[i]; // присвоение Buffer очередного символа строки, которую нужно записать
    FileWrite(FHandle, Buffer, 1); // запись этого символа в файл
  end;
  BufferB := $0D;
  FileWrite(FHandle, BufferB, 1); // запись конца строки
  BufferB := $0A;
  FileWrite(FHandle, BufferB, 1);
end;

procedure TFileStreamLn.WriteStr(const Str: String);
var Buffer: Char;
    i: Integer;
begin
  for i := 1 to Length(Str) do
  begin
    Buffer := Str[i]; // присвоение Buffer очередного символа строки, которую нужно записать
    FileWrite(FHandle, Buffer, 1); // запись этого символа в файл
  end;
end;

end.

