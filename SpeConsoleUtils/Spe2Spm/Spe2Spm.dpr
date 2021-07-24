program Spe2Spm;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  JvJCLUtils, 
  FileStreamLnUnit;

var SpeFName1, SpeFName2, SpmFName, SpmDirName, CurrParName, CurrParValue, S: string;
    SpeFile1, SpeFile2, SpmFile: TFileStreamLn;
    BuffSize: Integer=0;
    Buffer: array of Byte;
    ErrorNumber: Integer; // 0 - считалось до =, 1 - считалось до энтера, 2 - не считалось, 3 - другие ошибки
    n: Integer;

procedure Spe2SpmCopy(SpeFName: string; var SpeFile:TFileStreamLn);
begin
  // открытие файла на чтение
  if FileExists(SpeFName) then
  begin
    {$I-} SpeFile:=TFileStreamLn.Create(SpeFName,fmOpenRead,fmshareDenyWrite) {$I+};
    if IOResult<>0 then
    begin
      Writeln('Can''t read ',SpeFName);
      exit;
    end;
  end
  else Exit;

  try
    Repeat
      // Считывание шапки и пересохранение её в новый файл, но с заменённой строкой ParamName=ParamValue, вместо ParamName=CurrParValue
      if not SpeFile.ReadEqual(CurrParName, ErrorNumber) then break; // проверка на то, что файл не закончился
      While (AnsiCompareStr(CurrParName,'SPECTR=')<>0) do
      begin
        if ErrorNumber=0 then // строка считалась c '=', она переписывается в spm
        begin
          SpeFile.Readln(CurrParValue);
          SpmFile.Writeln(CurrParName+CurrParValue);
          if (AnsiCompareStr(CurrParName,'SPECTRSIZE=')=0) then BuffSize:=4*StrToIntDef(CurrParValue,0);
        end
        else if ErrorNumber=1 then // считалась строка без '=', она тоже переписывается в spm
          SpmFile.Writeln(CurrParName);
        SpeFile.ReadEqual(CurrParName, ErrorNumber); // читается новая строка
      end;
      if (AnsiCompareStr(CurrParName,'SPECTR=')=0) then
      begin
        SpmFile.Writeln('SPECTRSIZE='+'1024'); // заменить потом на реальный размер буферов
        SpmFile.WriteStr(CurrParName);
      end;

        // переписывание точек спектра в новый файл
      if BuffSize>0 then  // если известен размер, то переписывается блок конкретного размера
      begin
        SetLength(Buffer,BuffSize);
        SpeFile.Read(Buffer[0],BuffSize);
        SpmFile.Write(Buffer[0],BuffSize);
      end
      else               // если неизвестен, то побайтно копируется оставшаяся часть
      begin
        SetLength(Buffer,1);
        While SpeFile.Read(Buffer[0],1)=1 do
        begin
          SpmFile.Write(Buffer[0],1);
        end;
      end;
    until BuffSize=0;

  finally
    SpeFile.Free;
  end;

end;

begin
  { TODO -oUser -cConsole Main : Insert code here }
  // проверка на ввод параметров
  if (ParamStr(1)='') or (ParamStr(2)='') or (ParamStr(3)='') then
  begin
    Writeln('Programm is for merging two spe-files to one spm-file. ver 0.1.0 beta.');
    Writeln('Use: Spe2Spm.exe  <SpeFile1> <SpeFile2> <SpmDir>');
    Writeln('For more informations and examples see Readme.txt');
    Readln(S);
    Exit;
  end;

  // чтение параметров
  SpeFName1:=ParamStr(1);
  SpeFName2:=ParamStr(2);
  SpmDirName:=ParamStr(3);
  if SpmDirName[Length(SpmDirName)]<>'\' then SpmDirName:=SpmDirName+'\';
  n:=WordCount(SpeFName1,['\']);
  S:=ExtractWord(n,SpeFName1,['\']);
  S[Length(S)]:='m'; // переименование в spm
  SpmFName:=SpmDirName+S;

  // копирование 2-х spe в spm
  try
    SpmFile:=TFileStreamLn.Create(SpmFName,fmCreate,fmshareDenyWrite);
    Spe2SpmCopy(SpeFName1, SpeFile1);
    Spe2SpmCopy(SpeFName2, SpeFile2);
  finally
    SpmFile.Free;
  end;
end.
