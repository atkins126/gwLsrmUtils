program SpeTxtConv;

{$APPTYPE CONSOLE}

uses
  SysUtils, Classes, Spectrum;

var FileNameIn, FileNameOut, ExtIn, ExtOut: string; // Имена входящего и исходящего файлов, расширения входящего и исходящего файлов
    S: string; // вспомогательная строка
    Spectr: TSpectrum;
    KeyParamStr: string; // ключ для конвертации -d -- урезанный txt-формат для SpectraLine

function ParseCommandLine(const Param1, Param2: string; const Param3: string = ''): Boolean;
begin
  FileNameIn:=Param1;
  if Length(Param2)<=3 then
  begin
    ExtOut:=Param2;
  end
  else
  begin
    FileNameOut:=Param2;
    ExtOut:=ExtractFileExt(FileNameOut);
    Delete(ExtOut,1,1); // удаление точки из расширения
  end;
  KeyParamStr:=Param3;
  ExtIn:=ExtractFileExt(FileNameIn);
  Delete(ExtIn,1,1); // удаление точки из расширения
  if ((ExtIn<>'txt') and (ExtIn<>'spe') and (ExtIn<>'sps') and (ExtIn<>'csv')) or ((ExtOut<>'txt') and (ExtOut<>'spe')) then
  begin
    Writeln('Wrong file extension');
    Result := False;
    Exit;
  end;
  if Length(Param2)<=3 then
  begin
    if ExtIn <> ExtOut then
    begin
      FileNameOut:=FileNameIn;
      Delete(FileNameOut,Length(FileNameIn)-2,3);
      FileNameOut:=FileNameOut+ExtOut;
    end
    else
    begin
      FileNameOut:=FileNameIn;
      Delete(FileNameOut,Length(FileNameIn)-2,3);
      FileNameOut:=FileNameOut+'new';
    end;
  end;
  Result := True;
end;

begin
  { TODO -oUser -cConsole Main : Insert code here }
  // командная строка FileNameIn FileExtOut
  if (ParamStr(1)='') or (ParamStr(2)='') then
  begin
    Writeln('Programm is for convert spectr file formats version 0.1.2 beta');
    Writeln('Format: SpeTxtConverter <InFileName> <outFileExtension or OutFileName> -d');
    Writeln('-d -- key is for convert to simple txt-format, used in old SL');
    Readln(S);
    Exit;
  end;

  // получение имён файлов и расширений -- парсинг командной строки
  if not ParseCommandLine(ParamStr(1), ParamStr(2), ParamStr(3)) then
  begin
    Exit;
  end;

  // конвертация файла
  Spectr := TSpectrum.Create;
  try
    Spectr.FileOpen(FileNameIn);
    if KeyParamStr<>'-d' then Spectr.ExtSave(FileNameOut,ExtOut) else Spectr.TxtSaveSL(FileNameOut);
  finally
    Spectr.Free;
  end;
end.
