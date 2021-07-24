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
    ErrorNumber: Integer; // 0 - ��������� �� =, 1 - ��������� �� ������, 2 - �� ���������, 3 - ������ ������
    n: Integer;

procedure Spe2SpmCopy(SpeFName: string; var SpeFile:TFileStreamLn);
begin
  // �������� ����� �� ������
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
      // ���������� ����� � �������������� � � ����� ����, �� � ��������� ������� ParamName=ParamValue, ������ ParamName=CurrParValue
      if not SpeFile.ReadEqual(CurrParName, ErrorNumber) then break; // �������� �� ��, ��� ���� �� ����������
      While (AnsiCompareStr(CurrParName,'SPECTR=')<>0) do
      begin
        if ErrorNumber=0 then // ������ ��������� c '=', ��� �������������� � spm
        begin
          SpeFile.Readln(CurrParValue);
          SpmFile.Writeln(CurrParName+CurrParValue);
          if (AnsiCompareStr(CurrParName,'SPECTRSIZE=')=0) then BuffSize:=4*StrToIntDef(CurrParValue,0);
        end
        else if ErrorNumber=1 then // ��������� ������ ��� '=', ��� ���� �������������� � spm
          SpmFile.Writeln(CurrParName);
        SpeFile.ReadEqual(CurrParName, ErrorNumber); // �������� ����� ������
      end;
      if (AnsiCompareStr(CurrParName,'SPECTR=')=0) then
      begin
        SpmFile.Writeln('SPECTRSIZE='+'1024'); // �������� ����� �� �������� ������ �������
        SpmFile.WriteStr(CurrParName);
      end;

        // ������������� ����� ������� � ����� ����
      if BuffSize>0 then  // ���� �������� ������, �� �������������� ���� ����������� �������
      begin
        SetLength(Buffer,BuffSize);
        SpeFile.Read(Buffer[0],BuffSize);
        SpmFile.Write(Buffer[0],BuffSize);
      end
      else               // ���� ����������, �� �������� ���������� ���������� �����
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
  // �������� �� ���� ����������
  if (ParamStr(1)='') or (ParamStr(2)='') or (ParamStr(3)='') then
  begin
    Writeln('Programm is for merging two spe-files to one spm-file. ver 0.1.0 beta.');
    Writeln('Use: Spe2Spm.exe  <SpeFile1> <SpeFile2> <SpmDir>');
    Writeln('For more informations and examples see Readme.txt');
    Readln(S);
    Exit;
  end;

  // ������ ����������
  SpeFName1:=ParamStr(1);
  SpeFName2:=ParamStr(2);
  SpmDirName:=ParamStr(3);
  if SpmDirName[Length(SpmDirName)]<>'\' then SpmDirName:=SpmDirName+'\';
  n:=WordCount(SpeFName1,['\']);
  S:=ExtractWord(n,SpeFName1,['\']);
  S[Length(S)]:='m'; // �������������� � spm
  SpmFName:=SpmDirName+S;

  // ����������� 2-� spe � spm
  try
    SpmFile:=TFileStreamLn.Create(SpmFName,fmCreate,fmshareDenyWrite);
    Spe2SpmCopy(SpeFName1, SpeFile1);
    Spe2SpmCopy(SpeFName2, SpeFile2);
  finally
    SpmFile.Free;
  end;
end.
