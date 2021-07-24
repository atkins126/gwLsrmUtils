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

function TFileStreamLn.Read2numbersln(var n, m: Integer): LongInt; // ������ ������ � 2-�� ������� � ������� ���� �����
var Buffer: Byte; // ���������� ���� ����������� ������������� ���������
    Str: String; // ������ ���������� �����
begin
  Str := '';
  Result := FileRead(FHandle, Buffer, 1); // ���������� 1-�� �������
  if (Result = -1) or (Result = 0) then
  begin
    Result := 0;
    exit;
  end;
  while Buffer <> $0D do
  begin
    if Buffer = $09 then
    begin
      n := StrToInt(Str); // ��������� 1-�� �����
      Str := '';
      Result := FileRead(FHandle, Buffer, 1); // ���������� ���������� �������
      if Result <= 0 then
        exit;
    end;
    Str := Str + Chr(Buffer); // ���������� ���������� �������
    Result := FileRead(FHandle, Buffer, 1); // ���������� ���������� �������
    if Result <= 0 then
      exit;
  end;
  m := StrToInt(Str); // ��������� ���������� �����
  FileRead(FHandle, Buffer, 1); // ���������� ������� $0A
end;

function TFileStreamLn.ReadCollectionTime(var StartTime, StopTime: String;
  var T: Double): LongInt;
var Buffer: Byte; // ���������� ���� ����������� ������������� ���������
    Str: String; // ������ ���������� �����
    i: Integer; // i - �������
begin
  Str := '';
  i := 0;
  Result := FileRead(FHandle, Buffer, 1); // ���������� 1-�� �������
  if (Result = -1) or (Result = 0) then
  begin
    Result := 0;
    exit;
  end;
  while Buffer <> $0A do
  begin
    if Buffer = $20 then
    begin
      i := i + 1; // ������ ���������� �����
      case i of
        1:
          StartTime := Str; // ��������� ���� ������ ������
        2:
          StartTime := StartTime + ' ' + Str; // ���������� ������� ������ ������
        3:
          StopTime := Str; // ��������� ���� ����� ������
        4:
          StopTime := StopTime + ' ' + Str; // ���������� ������� ����� ������
        5:
          T := StrToFloat(Str); // ��������� ������� ������
      end;
      Str := '';
      while Buffer = $20 do // ������������ ��������
        Result := FileRead(FHandle, Buffer, 1); // ���������� ���������� �������
      if Result <= 0 then
        exit;
    end;
    Str := Str + Chr(Buffer); // ���������� ���������� �������
    Result := FileRead(FHandle, Buffer, 1); // ���������� ���������� �������
    if Result <= 0 then
      exit;
  end;
end;

function TFileStreamLn.ReadEqual(var Str: String): Boolean; // ��������� ������ ������ �� �����
var Buffer: Byte;
begin
  Str := '';
  Buffer := 0;
  Result := False;
  while (Buffer <> 61) and (Buffer <> $0D) do
  begin
    if FileRead(FHandle, Buffer, 1) < 1 then
      exit; // ���������� �������
    Str := Str + Char(Buffer); // ����������� ������ ���� ��������
  end;
  if Buffer = $0D then
  begin
    FileRead(FHandle, Buffer, 1); // ���������� ������� $0A
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
      exit; // ���������� �������
    end;
    if (Buffer <> $0D) then
      Str := Str + Char(Buffer); // ����������� ������ ���� ��������
  end;
  if Buffer = $0D then
  begin
    FileRead(FHandle, Buffer, 1); // ���������� ������� $0A
    Result := True;
    ErrorNumber := 1;
  end
  else if Buffer = 61 then
  begin
    ErrorNumber := 0;
    Result := True;
  end;
end;

procedure TFileStreamLn.Readln(var Str: String); // ��������� ������ ������
var Buffer: Byte; // ���������� ���� ����������� ������������� ���������
begin
  Str := '';
  FileRead(FHandle, Buffer, 1); // ���������� 1-�� �������
  while Buffer <> $0D do
  begin
    Str := Str + Chr(Buffer); // ���������� ���������� �������
    FileRead(FHandle, Buffer, 1); // ���������� ���������� �������
  end;
  FileRead(FHandle, Buffer, 1); // ���������� ������� $0A
end;

function TFileStreamLn.ReadLTRT(var StartTime: String; var RT,LT: Double): LongInt;
var Buffer: Byte; // ���������� ���� ����������� ������������� ���������
    Str: String; // ������ ���������� �����
    i: Integer; // i - �������
    n: Double; // n - ��� ������� ������ �����,
    Error: Boolean; // ���� �� ������
begin
  Str := '';
  StartTime := '';
  i := 0;
  n := 0;
  Error := FALSE;
  Result := FileRead(FHandle, Buffer, 1); // ���������� 1-�� �������
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
        n := StrToFloat(Str); // ������� ��������� �����
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
        Result := FileRead(FHandle, Buffer, 1); // ���������� ���������� �������
      if Result <= 0 then
        exit;
    end;
    Str := Str + Chr(Buffer); // ���������� ���������� �������
    Result := FileRead(FHandle, Buffer, 1); // ���������� ���������� �������
    if Result <= 0 then
      exit;
  end;
  //m:=StrToInt(Str); // ��������� ���������� �����
  //FileRead(FHandle, Buffer, 1); // ���������� ������� $0A
end;

function TFileStreamLn.ReadRownumbersln(var x: array of Integer;
  var L: Integer): LongInt;
var Buffer: Byte; // ���������� ���� ����������� ������������� ���������
    Str: String; // ������ ���������� �����
begin
  //SetLength(x, 6);
  Str := '';
  L := 0;
  Result := FileRead(FHandle, Buffer, 1); // ���������� 1-�� �������
  if (Result = -1) or (Result = 0) or (Buffer = Ord('S')) then
  begin
    Result := 0;
    exit;
  end;
  while Buffer <> $0A do
  begin
    if Buffer = $20 then
    begin
      x[L] := StrToInt(Str); // ��������� 1-�� �����
      Str := '';
      while Buffer = $20 do
        Result := FileRead(FHandle, Buffer, 1); // ���������� ���������� �������
      if Result <= 0 then
        exit;
      L := L + 1;
    end;
    Str := Str + Chr(Buffer); // ���������� ���������� �������
    Result := FileRead(FHandle, Buffer, 1); // ���������� ���������� �������
    if Result <= 0 then
      exit;
  end;
  //FileRead(FHandle, Buffer, 1); // ���������� ������� �� $0A
  L := L - 1;
  //x[L]:=StrToInt(Str); // ��������� ���������� �����
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
      exit; // ���������� �������
    if (Buffer <> Ord(ch)) then
      Str := Str + Char(Buffer); // ����������� ������ ���� ��������
  end;
  if Buffer = $0D then
  begin
    FileRead(FHandle, Buffer, 1); // ���������� ������� $0A
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
    Buffer := Str[i]; // ���������� Buffer ���������� ������� ������, ������� ����� ��������
    FileWrite(FHandle, Buffer, 1); // ������ ����� ������� � ����
  end;
  BufferB := $0D;
  FileWrite(FHandle, BufferB, 1); // ������ ����� ������
  BufferB := $0A;
  FileWrite(FHandle, BufferB, 1);
end;

procedure TFileStreamLn.WriteStr(const Str: String);
var Buffer: Char;
    i: Integer;
begin
  for i := 1 to Length(Str) do
  begin
    Buffer := Str[i]; // ���������� Buffer ���������� ������� ������, ������� ����� ��������
    FileWrite(FHandle, Buffer, 1); // ������ ����� ������� � ����
  end;
end;

end.

