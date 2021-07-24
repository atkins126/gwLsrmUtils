program SpmHeadEdit;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  Classes,
  IniFiles,
  FileStreamLnUnit;

const INI_NAME = 'SpmHead.ini';

var FileName, FileNameOld, ParamName, ParamValue, CondParValue, CurrParName, CurrParValue, S, AppPath: string;
    SpeFile, SpeFileNew: TFileStreamLn;
    BuffSize: Integer=0; // ������ ������
    Buffer: array of Byte;
    ErrorNumber: Integer; // 0 - ��������� �� =, 1 - ��������� �� ������, 2 - �� ���������, 3 - ������ ������
    IsSaveBackup: Boolean = False;
    IsReplaced: Boolean = False;
    isCondParValue: Boolean = False;

procedure LoadParamFromIni(const AFileName: string; var AParName, AParValue, CondParValue: string);
var IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(AFileName);
  try
    AParName := IniFile.ReadString('INPUT', 'ParName', '');
    AParValue := IniFile.ReadString('INPUT', 'ParValue', '');
    CondParValue := IniFile.ReadString('INPUT', 'CondParValue', '');
  finally
    IniFile.Free();
  end;
end;

begin
  { TODO -oUser -cConsole Main : Insert code here }
  // �������� �� ���� ����������
  if (ParamStr(1)='') or (ParamStr(2)='') then
  begin
    Writeln('Programm is for changing parameter <ParameterName> in spe or spm-file head' +
            ' to new value <ParameterValue>. ver 0.1.5 beta.');
    Writeln('Use: SpmShapkaEdit.exe  <InputFile> <ParameterName> <ParameterValue> <AdditionalParam>');
    Writeln('if -b specified then backup-file will be saved');
    Writeln('For more informations and examples see ReadMe.txt');
    Readln(S);
    Exit;
  end;

  FileName := ParamStr(1);
  ParamName := ParamStr(2);
  ParamValue := ParamStr(3);
  CondParValue := '';
  FileNameOld := FileName + '.bak';

  (*if (not FileExists(FileName)) then
    Exit; *)

  AppPath := ExtractFilePath(ParamStr(0));

  if (ParamName = INI_NAME) then begin
    LoadParamFromIni(AppPath + INI_NAME, ParamName, ParamValue, CondParValue);
    isCondParValue := not (CondParValue = '');
  end;

  //IsSaveBackup:=ParamStr(4)<>'';
  if (ParamStr(4) <> '') then begin
    IsSaveBackup := (ParamStr(4) = '-b');
  end;

  // ����������� ������
  if FileExists(FileNameOld) then DeleteFile(FileNameOld);
  if not RenameFile(FileName, FileNameOld) then
  begin
    Writeln('Can''t rename ', FileName);
    exit;
  end;

  if FileExists(FileNameOld) then // �������� �� ������������� ����
  begin
    // �������� ����� spe �� ������
    {$I-} SpeFile := TFileStreamLn.Create(FileNameOld, fmOpenRead, fmshareDenyWrite) {$I+};
    if IOResult<>0 then // �������� �� ����������� ���������
    begin
      Writeln('Can''t read ', FileName);
      exit;
    end;
  end
  else Exit;

  SpeFileNew := TFileStreamLn.Create(FileName, fmCreate, fmshareDenyWrite);

  try
    Repeat
      // ���������� ����� � �������������� � � ����� ����,
      // �� � ��������� ������� ParamName=ParamValue, ������ ParamName=CurrParValue
      if not SpeFile.ReadEqual(CurrParName) then break; // �������� �� ��, ��� ���� �� ����������
      While (not AnsiSameStr(CurrParName, 'SPECTR=')) do
      begin
        if (ErrorNumber = 0) then
        begin
          SpeFile.Readln(CurrParValue);
          if (AnsiSameText(CurrParName, ParamName + '=')) then begin
            if (not isCondParValue) or (SameText(CurrParValue, CondParValue)) then
              SpeFileNew.Writeln(CurrParName + ParamValue)
            else
              SpeFileNew.Writeln(CurrParName + CurrParValue);
            IsReplaced := True;
          end
          else
            SpeFileNew.Writeln(CurrParName + CurrParValue);
          if (AnsiSameStr(CurrParName, 'SPECTRSIZE=')) then
            BuffSize := 4 * StrToIntDef(CurrParValue, 0);
        end
        else if (ErrorNumber = 1) then
          SpeFileNew.Writeln(CurrParName); // ������ ��������� ������
        SpeFile.ReadEqual(CurrParName, ErrorNumber);
      end;
      if (AnsiSameText(CurrParName, 'SPECTR=') and IsReplaced) then
        SpeFileNew.WriteStr(CurrParName)
      else begin
        // add new param if it wasn't added
        SpeFileNew.Writeln(ParamName + '=' + ParamValue);
        SpeFileNew.WriteStr(CurrParName);
      end;

      // ������������� ����� ������� � ����� ����
      if (BuffSize > 0) then  // ���� �������� ������, �� �������������� ���� ����������� �������
      begin
        SetLength(Buffer, BuffSize);
        SpeFile.Read(Buffer[0], BuffSize);
        SpeFileNew.Write(Buffer[0], BuffSize);
      end
      else               // ���� ����������, �� �������� ���������� ���������� �����
      begin
        SetLength(Buffer, 1);
        While (SpeFile.Read(Buffer[0], 1) = 1) do
        begin
          SpeFileNew.Write(Buffer[0], 1);
        end;
      end;
    until (BuffSize = 0);
  finally
    SpeFile.Free;
    SpeFileNew.Free;
  end;
  
  if (not IsSaveBackup) then
    DeleteFile(FileNameOld); // ������� �����
end.
