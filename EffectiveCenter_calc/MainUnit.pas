unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, Menus, ComCtrls, ImgList,
  TB2Dock, TB2Toolbar, TB2Item,
  LibUnit, EffC_Calcing;

{$DEFINE PRRECISE}

const
  DDISTANCE = 0.02;

type
  TfrmMain = class(TForm)
    strngrd1: TStringGrid;
    btnCalc: TButton;
    lblResult: TLabel;
    mm1: TMainMenu;
    mniFile: TMenuItem;
    mniLoadfilewithMeas: TMenuItem;
    mniSaveefd: TMenuItem;
    dlgOpen1: TOpenDialog;
    dlgSave: TSaveDialog;
    btnToDate: TButton;
    dtp1: TDateTimePicker;
    tbdck1: TTBDock;
    tbtlbr1: TTBToolbar;
    il1: TImageList;
    tbtmOpenEcf: TTBItem;
    tbtmSaveEfd: TTBItem;
    mniTools: TMenuItem;
    mniSort: TMenuItem;
    mniAbout: TMenuItem;
    mniEdit: TMenuItem;
    mniCopy: TMenuItem;
    mniCopyResults: TMenuItem;
    pmSt: TPopupMenu;
    mniCopyResults1: TMenuItem;
    mniCopyAll1: TMenuItem;
    mniOptions: TMenuItem;
    mniWriteLog: TMenuItem;
    mniPreciseModel: TMenuItem;
    mniSelfTesting: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure btnCalcClick(Sender: TObject);
    procedure mniSaveefdClick(Sender: TObject);
    procedure mniSavefilewithmeasClick(Sender: TObject);
    procedure mniLoadfilewithMeasClick(Sender: TObject);
    procedure btnToDateClick(Sender: TObject);
    procedure strngrd1SelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure mniSortClick(Sender: TObject);
    procedure mniAboutClick(Sender: TObject);
    procedure mniCopyClick(Sender: TObject);
    procedure mniCopyResultsClick(Sender: TObject);
    procedure mniWriteLogClick(Sender: TObject);
    procedure mniPreciseModelClick(Sender: TObject);
    procedure mniSelfTestingClick(Sender: TObject);
  private
    procedure CalculateEffCenter();   // calculates effcenter value and saves to file: general function
    procedure ClearMeasurements;      // очистка значений измерений
    function ReadMeasFromTable(): Integer; // чтение измерений из таблицы, сколько измерений
    procedure WriteMeasToTable;       // запись измерений в таблицу
    function WriteEffCentersToTable(): Boolean; // writes EffCenters to strngrid
    function NonEmptyRawCount: integer; // получение числа непустых строк в StringGrid
    procedure LoadFromEcf(const filename: string); // загрузка файла с входными данными измерений
    procedure SaveToEfd(const fileName: string); // сохранение эфф центра в efd-файл
    procedure SaveToResultFile(const fileName: string); // сохранение эфф центра в efd-файл
    procedure SaveToEcf(const fileName: string); // сохранение измерений в eсf-файл
    function SilentCalc(const fileName: string): Boolean; // при открытии программы с параметром -- имя файла, расчёт и сохранение файла efd, закрытие программы
  public

  end;

var
  frmMain: TfrmMain;
  DMeasurements: TMeasurements; // объект с измерениями
  useDateRecalc: Boolean = False; // пересчитывать ли дату или считать, что все одной даты
  DEffCenter: TEffCenters;      // объект с эффективными центрами
  isReaded: Boolean = False; // считаны ли данные
  isCalced: Boolean = False; // рассчитан ли эфф центр
  isWriteLog: Boolean;     // записывать ли лог-файл
  appPath: string;
  detDiam: Double;  // диаметер детектора для более точной модели
  libName: string;
  dDist: Double = DDISTANCE;

resourcestring
  rsCellStr0 = 'Energy, keV';
  rsCellStr1 = 'Distance, cm';
  rsCellStr2 = 'count rate, cps';
  rsCellStr3 = 'dCR, cps';
  rsCellStr4 = 'Date (opt.)';
  rsCellStr5 = 'Nuclide (opt.)';
  rsCellStr6 = 'EffCenter';
  rsCellStr7 = 'dEffCenter';
  rsError1 = 'Library %s is not loaded correct. Default library will be used';
  rsError2 = 'Error. Wrong reading halftime from library';
  rsError3 = 'Wrong values is written in table';
  rsError4 = 'Должно быть введено по-крайней мере 2 строки с данными. Смотри Readme.txt';
  rsError5 = 'Расчёт не проведён';
  rsInfo1 = 'EffCenter = %2.2f cm';
  rsInfo2 = 'EffCenterCalc ver. 0.1.3 beta';

implementation

{$R *.dfm}

uses Math, JvJCLUtils, Clipbrd;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  strngrd1.Cells[0,0] := rsCellStr0;
  strngrd1.Cells[1,0] := rsCellStr1;
  strngrd1.Cells[2,0] := rsCellStr2;
  strngrd1.Cells[3,0] := rsCellStr3;
  strngrd1.Cells[4,0] := rsCellStr4;
  strngrd1.Cells[5,0] := rsCellStr5;
  strngrd1.Cells[6,0] := rsCellStr6;
  strngrd1.Cells[7,0] := rsCellStr7;
  appPath := ExtractFilePath(Application.ExeName);
  libName := appPath + 'NuclideCalc.tlb';
  if (DecimalSeparator <> '.') then DecimalSeparator := '.';
  dtp1.DateTime := Now;
  isWriteLog := mniWriteLog.Checked;

  if (ParamStr(1) <> '') then
    if SilentCalc(ParamStr(1)) then Application.Terminate();

{$IFDEF PRRECISE}
  mniPreciseModel.Visible := True;
{$ENDIF}
end;

procedure TfrmMain.btnCalcClick(Sender: TObject);
begin
  CalculateEffCenter();
end;

procedure TfrmMain.ClearMeasurements;
begin
  if Assigned(DMeasurements) then FreeAndNil(DMeasurements);
  if Assigned(DEffCenter) then FreeAndNil(DEffCenter);
end;

function TfrmMain.ReadMeasFromTable(): Integer;
var i, errorLevel: Integer;
begin
  errorLevel := 0;
  DMeasurements := TMeasurements.Create(libName);
  DMeasurements.measCount := NonEmptyRawCount();
  DMeasurements.FDistError := dDist;
  useDateRecalc := True;
  for i:=0 to DMeasurements.measCount-1 do
  begin
    try
      DMeasurements.FMeasurement[i].FEnergy := StrToFloat(strngrd1.Cells[0,i+1]); // считать энергию
      DMeasurements.FMeasurement[i].FDistance := StrToFloat(strngrd1.Cells[1,i+1]); // считать расстояние
      DMeasurements.FMeasurement[i].FCount := StrToFloat(strngrd1.Cells[2,i+1]); // считать счёт
      DMeasurements.FMeasurement[i].FDCount := StrToFloatDef(strngrd1.Cells[3,i+1], 0); // считать погрешность счёта
      if (strngrd1.Cells[4,i+1] <> '') then
      begin
        DMeasurements.FMeasurement[i].FDate := StrToDate(strngrd1.Cells[4,i+1]); // считать дату
        DMeasurements.FMeasurement[i].FNuclide := strngrd1.Cells[5,i+1]; // считать нуклид
        DMeasurements.ThalfFromNuclideName(i); // получить для этого нуклида период пулураспада
      end
      else
        useDateRecalc := False;
    except
      errorLevel := 1;
    end;
  end;
  if (errorLevel > 0) then ShowMessage(rsError3);
  Result := DMeasurements.measCount;
end;

procedure TfrmMain.WriteMeasToTable;
var i: Integer;
begin
  for i:=0 to DMeasurements.measCount-1 do
  begin
    strngrd1.Cells[0,i+1] := FloatToStr(DMeasurements.FMeasurement[i].FEnergy);
    strngrd1.Cells[1,i+1] := FloatToStr(DMeasurements.FMeasurement[i].FDistance);
    strngrd1.Cells[2,i+1] := FormatFloat('0.###', DMeasurements.FMeasurement[i].FCount);
    strngrd1.Cells[3,i+1] := FormatFloat('0.###', DMeasurements.FMeasurement[i].FDCount);
    if (DMeasurements.FMeasurement[i].FDate <> 0) then
      strngrd1.Cells[4,i+1] := DateToStr(DMeasurements.FMeasurement[i].FDate);
    strngrd1.Cells[5,i+1] := DMeasurements.FMeasurement[i].FNuclide;
  end;
end;

function TfrmMain.NonEmptyRawCount: integer;
var i,j: Integer;
    Breaking: Boolean;
begin
  Result := 0;
  Breaking := False;
  for i:=1 to strngrd1.RowCount-1 do
  begin
    for j:=0 to 2 do
      if ((strngrd1.Cells[j,i]) = '') then
      begin
        Breaking := True;
        Break;
      end;
    if (not Breaking) then
      Result := i
    else
      Break;
  end;
end;

procedure TfrmMain.LoadFromEcf(const filename: string);
var FileList: TStringList;
    i, beg: Integer;
    S: String;
begin
  FileList := TStringList.Create();
  FileList.LoadFromFile(filename);
  ClearMeasurements();
  isCalced := False;
  strngrd1.RowCount := FileList.Count + 1;
  for i:=1 to strngrd1.RowCount do
  begin
    strngrd1.Rows[i].Text := '';
  end;

  // определение версии
  S := FileList.Strings[0];
  S := Trim(S);
  if ((Length(S) > 0) and (S[1] = '<')) then
    beg := 1
  else
    beg := 0;

  try
    for i:=1 to FileList.Count-1-beg do
    begin
      strngrd1.Cells[0,i] := ExtractWord(1, FileList.Strings[i+beg], [#09,' ']); // energy
      strngrd1.Cells[1,i] := ExtractWord(2, FileList.Strings[i+beg], [#09,' ']); // distance
      strngrd1.Cells[2,i] := ExtractWord(3, FileList.Strings[i+beg], [#09,' ']); // cr
      if (beg = 1) then  begin
        strngrd1.Cells[3,i] := ExtractWord(4, FileList.Strings[i+beg], [#09,' ']); // dcr
        strngrd1.Cells[4,i] := ExtractWord(5, FileList.Strings[i+beg], [#09,' ']); // date
        strngrd1.Cells[5,i] := ExtractWord(6, FileList.Strings[i+beg], [#09,' ']); // nuclides
      end
      else  begin
        strngrd1.Cells[3,i] := '0'; // dcr
        strngrd1.Cells[4,i] := ExtractWord(4,FileList.Strings[i+beg], [#09,' ']); // date
        strngrd1.Cells[5,i] := ExtractWord(5,FileList.Strings[i+beg], [#09,' ']); // nuclides
      end;
    end;
    ReadMeasFromTable();  // чтение значений из таблицы в Measurement
    if (DMeasurements.measCount > 0) and (strngrd1.Cells[4,1] <> '') then
    begin
      dtp1.Date := DMeasurements.FMeasurement[0].FDate;
      btnToDate.Enabled := True;
      dtp1.Enabled := True;
    end
    else begin
      btnToDate.Enabled := False;
      dtp1.Enabled := False;
    end;
  finally
    FileList.Free();
  end;
end;

procedure TfrmMain.mniLoadfilewithMeasClick(Sender: TObject);
begin
  if (dlgOpen1.Execute) then
    LoadFromEcf(dlgOpen1.FileName);
end;

procedure TfrmMain.mniSaveefdClick(Sender: TObject);
begin
  dlgSave.FilterIndex := 1;
  if (dlgSave.Execute) then
  begin
    case dlgSave.FilterIndex of
    1: SaveToEfd(dlgSave.FileName);
    2: SaveToEcf(dlgSave.FileName);
    3: SaveToResultFile(dlgSave.FileName);
    end;
  end;
end;

procedure TfrmMain.SaveToEcf(const fileName: string);
var FileList: TStringList;
    i: Integer;
begin
    // очистка предыдущих данных
  ClearMeasurements();

  // считывание из таблицы значений измерений
  if (ReadMeasFromTable() < 2) then
  begin
    ShowMessage(rsError4);
    isReaded := False;
    Exit;
  end
  else isReaded := True;

  // сохранение измерений в файл
  FileList := TStringList.Create();
  try
    FileList.Add('<Measurements for EffCenter calculating. Version=1>');
    FileList.Add('E' + #09 + 'R' +#09 + 'CR' +#09 + 'DCR' + #09 + 'Date'	+#09 + 'Nucl'); // добавление шапки
    if (DMeasurements.FMeasurement[0].FDate <> 0) then begin
      for i:=1 to DMeasurements.measCount do
      begin
        FileList.Add(strngrd1.Cells[0,i] +#09 + strngrd1.Cells[1,i] +#09 + strngrd1.Cells[2,i]
          +#09 + strngrd1.Cells[3,i] +#09 + strngrd1.Cells[4,i] +#09 + strngrd1.Cells[5,i]);
      end;
    end else begin
      for i:=1 to DMeasurements.measCount do
      begin
        FileList.Add(strngrd1.Cells[0,i] +#09 + strngrd1.Cells[1,i] +#09 + strngrd1.Cells[2,i]
          +#09 + strngrd1.Cells[3,i]);
      end;
    end;
  finally
    FileList.SaveToFile(fileName);
    FileList.Free;
  end;
end;

procedure TfrmMain.SaveToEfd(const fileName: string);
var i: Integer;
    FileList: TStringList;
begin
  FileList := TStringList.Create;
  try
    for i:=0 to DEffCenter.effCount-1 do
      if (DEffCenter.FEffCenters[i].Fec <> -1) then
        FileList.Add(FormatFloat('0.####', DEffCenter.FEffCenters[i].FEnergy/1000) +
          ',   ' + FormatFloat('0.##',DEffCenter.FEffCenters[i].Fec));
    FileList.SaveToFile(fileName);
  finally
    FileList.Free;
  end;
end;

procedure TfrmMain.SaveToResultFile(const fileName: string);
var i: Integer;
    FileList: TStringList;
begin
  FileList:=TStringList.Create;
  try
    FileList.Add('Energy, keV' +#09 + 'EffCenter, cm' +#09 + 'EffCError, cm');
    for i:=0 to DEffCenter.effCount-1 do
      if (DEffCenter.FEffCenters[i].Fec <> -1) then
        FileList.Add(FormatFloat('0.####', DEffCenter.FEffCenters[i].FEnergy) +#09 +
          FormatFloat('0.##', DEffCenter.FEffCenters[i].Fec) +#09 +
          FormatFloat('0.##', DEffCenter.FEffCenters[i].FEcError));
    FileList.SaveToFile(fileName);
  finally
    FileList.Free();
  end;
end;

procedure TfrmMain.mniSavefilewithmeasClick(Sender: TObject);
begin
  dlgSave.FilterIndex := 2;
  if dlgSave.Execute then
  begin
    SaveToEcf(dlgSave.FileName);
  end;
end;

procedure TfrmMain.mniCopyClick(Sender: TObject);
var i, j: Integer;
    S: string;
begin
  S := '';
  for i:=0 to strngrd1.RowCount-1 do
  begin
    for j:=0 to strngrd1.ColCount-1 do
    begin
      S := S + strngrd1.Cells[j,i];
      if (j < strngrd1.ColCount-1) then
        S := S + #09;
    end;
    if (i < strngrd1.RowCount-1) then
      S := S + #13#10;
  end;
  Clipboard.AsText := S;

  //strngrd1.SetTextBuf();
end;

procedure TfrmMain.mniCopyResultsClick(Sender: TObject);
var FileList: TStringList;
    i: Integer;
begin
  if (isCalced) then
  begin
    FileList := TStringList.Create;
    try
      FileList.Add('Energy, keV' +#09 + 'EffCenter, cm' +#09 + 'EffCError, cm');
      for i:=0 to DEffCenter.effCount-1 do
        if (DEffCenter.FEffCenters[i].Fec<>-1) then
          FileList.Add(FormatFloat('0.##', DEffCenter.FEffCenters[i].FEnergy) +#09 +
          FormatFloat('0.##', DEffCenter.FEffCenters[i].Fec) + #09 +
          FormatFloat('0.##', DEffCenter.FEffCenters[i].FEcError));
      Clipboard.AsText := FileList.Text; 
    finally
      FileList.Free;
    end;
  end;
end;

procedure TfrmMain.btnToDateClick(Sender: TObject);
var GoalDate: TDate;
begin
  GoalDate := dtp1.Date;
  ReadMeasFromTable();
  if not DMeasurements.RecalcOnDate(GoalDate) then
    MessageDlg(rsError2, mtError, [mbOK], 0);
  WriteMeasToTable();
end;

procedure TfrmMain.strngrd1SelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  if (ARow = strngrd1.RowCount-1) then
    strngrd1.RowCount := strngrd1.RowCount+1;
end;

function TfrmMain.SilentCalc(const fileName: string): Boolean;
begin
  Result := False;
  if FileExists(FileName) then
  begin
    LoadFromEcf(FileName);
    btnCalcClick(Owner);
  end
  else Exit;
  Result := True;
end;

procedure TfrmMain.mniSortClick(Sender: TObject);
begin
  ReadMeasFromTable(); // чтение из таблицы в массив
  DMeasurements.SortInsert(); // сортировка этого массива
  WriteMeasToTable(); // запись обратно в таблицу
end;

procedure TfrmMain.mniAboutClick(Sender: TObject);
begin
  MessageDlg(rsInfo2, mtInformation, [mbOK], 0);
end;

procedure TfrmMain.mniWriteLogClick(Sender: TObject);
begin
  isWriteLog := mniWriteLog.Checked;
end;

procedure TfrmMain.mniPreciseModelClick(Sender: TObject);
var S: string;
begin
  mniPreciseModel.Checked := not (mniPreciseModel.Checked);
  if (mniPreciseModel.Checked) then
  begin
    InputQuery('Det diameter param', 'Input parameter "detector diameter", cm', S);
    if (not TryStrToFloat(S, detDiam)) then
    begin
      ShowMessage(Format('Wrong value %s',[S]));
      Exit;
    end;
  end;
end;

procedure TfrmMain.mniSelfTestingClick(Sender: TObject);
const
  Rnew = 5;
var i: Integer;
    CurrEnergy, CurR, RefCount, CurEfc, CurEfcError, CurCount, NewCount: Double;
    F: TextFile;
begin
  if (not isCalced) then Exit;
  RefCount := 0;
  AssignFile(F, appPath + 'SelfTesting.log');
  Rewrite(F);

  for i:=0 to DMeasurements.measCount-1 do
  begin
    NewCount := -1;
    CurrEnergy := DMeasurements.FMeasurement[i].FEnergy;
    CurR := DMeasurements.FMeasurement[i].FDistance;
    CurCount := DMeasurements.FMeasurement[i].FCount;
    if (Abs(CurR-Rnew) < 0.1) then
      RefCount := CurCount;
    if DEffCenter.EffCFromEnergy(CurrEnergy, CurEfc, CurEfcError) then
    begin
      if (mniPreciseModel.Checked) then
        NewCount := CurCount * (Sqr(detDiam) + 8*sqr(CurR + CurEfc))/
          (Sqr(detDiam) + 8*sqr(Rnew + CurEfc))
      else
        NewCount := CurCount * sqr(CurR + CurEfc)/sqr(Rnew + CurEfc);
    end;
    strngrd1.Cells[4,i+1]:=FloatToStrF(NewCount, ffGeneral, 4, 3);
    strngrd1.Cells[5,i+1]:=FloatToStrF((NewCount/RefCount-1)*100, ffGeneral, 4, 2);
    Writeln(F, FloatToStrF(CurrEnergy, ffGeneral, 4, 1) +#09 +
      FloatToStrF(CurR, ffGeneral, 4, 1) +#09 + FloatToStrF(CurCount, ffGeneral, 4, 1) +#09 +
      FloatToStrF(CurEfc, ffGeneral, 4, 1) +#09 + FloatToStrF(NewCount, ffGeneral, 4, 1));
  end;
  CloseFile(F);
end;

procedure TfrmMain.CalculateEffCenter;
begin
  // clear prev data
  ClearMeasurements();

  // reading measurement values from grid
  if (ReadMeasFromTable() < 2) then
  begin
    ShowMessage(rsError4);
    isReaded := False;
    Exit;
  end
  else isReaded := True;

  // сортировка этого массива и пересчёт на одну дату и отображение в таблице
  DMeasurements.SortInsert();
  if (not DMeasurements.IsOneDate) and (useDateRecalc) then
  begin
    if (not DMeasurements.RecalcOnDate(dtp1.Date)) then
      MessageDlg(rsError2, mtError, [mbOK], 0);
  end;
  WriteMeasToTable();

  // расчёт эффективной толщины
  DEffCenter := TEffCenters.Create();
  EffCentersCalculating(DMeasurements, DEffCenter, isWriteLog, mniPreciseModel.Checked,
    appPath + 'EffCenter.log', detDiam);

  // вывод результатов в таблицу
  isCalced := WriteEffCentersToTable();
  if (not isCalced) then
  begin
    ShowMessage(rsError5);
    Exit;
  end;

  // сохранение эффективной толщины в файл
  SaveToEfd(appPath + 'EffCenter.efd');
end;

function TfrmMain.WriteEffCentersToTable(): Boolean;
var i, j: Integer;
begin
  i := 0;
  j := 0;
  if (DEffCenter.effCount > 0) then
    begin
      While (i < DMeasurements.measCount) or (j < DEffCenter.effCount) do
      begin
        if (DMeasurements.FMeasurement[i].FEnergy = DEffCenter.FEffCenters[j].FEnergy) then
        begin
          if (DEffCenter.FEffCenters[j].Fec <> -1) then
          begin
            strngrd1.Cells[6,i+1] := FormatFloat('0.##', DEffCenter.FEffCenters[j].Fec);
            strngrd1.Cells[7,i+1] := FormatFloat('0.##', DEffCenter.FEffCenters[j].FEcError);
          end
          else
          begin
            strngrd1.Cells[6,i+1] := 'Error';
            strngrd1.Cells[7,i+1] := 'Error';
          end;
          i := i + 1;
        end
        else
        begin
          j := j + 1;
        end;
      end;
      Result := True;
    end
  else
  begin
    Result := False;
  end;
end;

end.
