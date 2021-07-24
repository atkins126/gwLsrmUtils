unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, Menus,
  JvExStdCtrls, JvEdit, JvValidateEdit,
  LibUnit;

const
  DAYSINYEAR = 365.25;//42198;

type
  TMainForm = class(TForm)
    Label1: TLabel;
    OldActEdt: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    btnCalc: TButton;
    Resultlbl: TLabel;
    TdiffLbl: TLabel;
    ThalfLbl: TLabel;
    NuclCmbBx: TComboBox;
    btnBuffCopy: TButton;
    dtpOldDate: TDateTimePicker;
    dtpNewDate: TDateTimePicker;
    mm1: TMainMenu;
    mniFile: TMenuItem;
    mniLoadLibrary: TMenuItem;
    mniReloadLibrary: TMenuItem;
    dlgOpen1: TOpenDialog;
    mniAbout: TMenuItem;
    mniEdit: TMenuItem;
    mniCopy: TMenuItem;
    mniPaste: TMenuItem;
    jvedtOldAct: TJvValidateEdit;
    mniEditNuclide: TMenuItem;
    mniAddNuclide: TMenuItem;
    mniTools: TMenuItem;
    mniConvertActivity: TMenuItem;
    dtpOldTime: TDateTimePicker;
    dtpNewTime: TDateTimePicker;
    procedure btnCalcClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NuclCmbBxSelect(Sender: TObject);
    procedure OldActEdtKeyPress(Sender: TObject; var Key: Char);
    procedure btnBuffCopyClick(Sender: TObject);
    procedure mniAboutClick(Sender: TObject);
    procedure mniReloadLibraryClick(Sender: TObject);
    procedure mniLoadLibraryClick(Sender: TObject);
    procedure mniPasteClick(Sender: TObject);
    procedure mniCopyClick(Sender: TObject);
    procedure jvedtOldActCustomValidate(Sender: TObject; Key: Char;
      const AText: String; const Pos: Integer; var IsValid: Boolean);
    procedure mniConvertActivityClick(Sender: TObject);
  private
    procedure NewActivityCalc(); // процедура пересчёта активности
    procedure LoadInfoFromBuffer(); // загрузка информация о нуклиде из буфера
    procedure ShowNuclideInfo();
  public
    procedure LoadCurLibrary();
  end;

  //TBufferList = (NUCLIDE, LSRM);  // тип данных, которые будут пытаться считываться из буфера  Nuclide - имя нуклида, LSRM -- строка с данными эталона: Eu-152 A=46880 Бк dA=3% 10-09-2012

const
  L = 16;

var
  MainForm: TMainForm;
  appPath, curLibPath: string;
  oldAct, newAct: Double;  // старая и новая активности
  oldDate, newDate: TDateTime; // старая и новая дата
  Nuclide: String;  // текущий нуклид
  CurLib: TLibrary; // библиотека нуклидов

implementation

{$R *.dfm}

uses Math, DateUtils, Char_Check, Clipbrd, JvJCLUtils, UConvertActivity;

procedure TMainForm.FormCreate(Sender: TObject);
var Nuclides: TStringList; // список нуклидов
begin
  if (DecimalSeparator <> '.') then DecimalSeparator := '.';
  appPath := ExtractFilePath(Application.ExeName);
  curLibPath := appPath + 'NuclideCalc.lib';
  dtpNewDate.Date := Date;
  dtpNewTime.Time := 0;
  dtpOldDate.Date := IncYear(Date, -2); // старая дата по-умолчанию равна текущая -2 года
  dtpOldTime.Time := 0;
  Nuclides := TStringList.Create; // создание списка нуклидов
  CurLib := TLibrary.Create;

  // загрузка библиотеки из файла
  if not CurLib.OpenShortTabLib(curLibPath) then //    LoadLibraryFromFile(curLibPath)
  begin
    MessageDlg('Library ' + curLibPath + ' is not loaded correct. Default library will be used',
      mtError, [mbOK], 0);
    CurLib.DefaultLibFill();
  end;
  // присвоение списка нуклидов комбобоксу
  CurLib.GetListOfNuclides(Nuclides);
  NuclCmbBx.Items.Assign(Nuclides);
  NuclCmbBx.ItemIndex := 0;
  ShowNuclideInfo();
  Nuclides.Free;

  // проверка буфера на нуклид в нём и другую информацию
  LoadInfoFromBuffer();
end;

procedure TMainForm.mniReloadLibraryClick(Sender: TObject);
begin
  LoadCurLibrary();
end;

procedure TMainForm.mniLoadLibraryClick(Sender: TObject);
begin
  if dlgOpen1.Execute then
  begin
    curLibPath := dlgOpen1.FileName;
    LoadCurLibrary();
  end;
end;

procedure TMainForm.LoadCurLibrary;
var Nuclides: TStringList; // список нуклидов
begin
  if not CurLib.OpenShortTabLib(curLibPath) then // загрузка библиотеки из файла
  begin
    MessageDlg('Library ' + curLibPath + ' is not loaded correct. Default library will be used',
      mtError, [mbOK], 0);
    CurLib.DefaultLibFill();
  end;
  // присвоение списка нуклидов комбобоксу
  Nuclides:=TStringList.Create;
  CurLib.GetListOfNuclides(Nuclides);
  NuclCmbBx.Items.Assign(Nuclides);
  NuclCmbBx.ItemIndex := 0;
  ShowNuclideInfo();
  Nuclides.Free();
end;

procedure TMainForm.btnCalcClick(Sender: TObject);
begin
  NewActivityCalc();
  //ShowMessage(FloatToStr(jvedtOldAct.Value));
end;

procedure TMainForm.NuclCmbBxSelect(Sender: TObject);
begin
  ShowNuclideInfo();
end;

procedure TMainForm.OldActEdtKeyPress(Sender: TObject; var Key: Char);
begin
  if not IsFloat(Key, OldActEdt.Text) then Key:=#0;
end;

procedure TMainForm.btnBuffCopyClick(Sender: TObject);
var Clip: TClipboard;
begin
  Clip := TClipboard.Create(); // создание объекта
  Clip.SetTextBuf(PChar(FloatToStrF(NewAct, ffGeneral, 4, 2))); // копирование в буфер значение новой активности
  Clip.Free(); // удаление объекта
end;

procedure TMainForm.NewActivityCalc;
var Tdiff: Double;   // разность времени пересчёта
begin
  // считывание данных
  TdiffLbl.Caption := '';
  ResultLbl.Caption := '';
  (*if (not TryStrToFloat(oldActEdt.Text, oldAct)) then
  begin
    ShowMessage('Wrong old activity value');
    Exit;
  end; *)
  oldAct := jvedtOldAct.Value;
  oldDate := dtpOldDate.Date + dtpOldTime.Time;
  newDate := dtpNewDate.Date + dtpNewTime.Time;
  Nuclide := NuclCmbBx.Text;

  // расчёт активности и вспом параметров
  if Assigned(CurLib) then
  begin
    NewAct := CurLib.RecalcActivityToDate(Nuclide, oldAct, oldDate, newDate);
    if (NewAct < 0) then
      ShowMessage('Ошибка расчёта новой активности, проверьте введённые данные');

    // вывод результатов
    Tdiff := newDate - oldDate; //DaysBetween(newDate, oldDate);

    ShowNuclideInfo();
    TdiffLbl.Caption := 'Интервал времени: ' + FloatToStrF(Tdiff / DAYSINYEAR, ffGeneral, 5, 3) + ' лет';
    ResultLbl.Caption := 'Новая активность= ' + FloatToStrF(NewAct, ffGeneral, 4, 2) +
      ' на дату ' + DatetoStr(newDate);
  end;
end;

procedure TMainForm.mniAboutClick(Sender: TObject);
begin
  ShowMessage('NuclideCalc is programm for activity recalculating to other date. Ver. 0.1.7');
end;

procedure TMainForm.LoadInfoFromBuffer;
var Clip: TClipboard;
    ClipInfo, ClipNuc, ClipAct, ClipDate: string;
    CurFormatSettings: TFormatSettings;
begin
  ClipAct := '';
  ClipDate := '';

  // проверка буфера на нуклид в нём и другую информацию
  Clip := TClipboard.Create;
  ClipInfo := Clip.asText;
  Clip.Free();

  // проверка такой строки: "Eu-152 A=46880 Бк dA=3% 10-09-2012" либо "Co-60 A=4200 Bq on 04.05.2015"
  if (WordCount(ClipInfo,[' '])>1) then
  begin
    ClipNuc := ExtractWord(1, ClipInfo, [' ']);
    ClipAct := ExtractWord(2, ClipInfo, [' ']);
    if (WordCount(ClipAct, ['=']) > 1) then
      ClipAct := ExtractWord(2, ClipAct, ['='])
    else
      ClipAct := '';
    if (WordCount(ClipInfo, [' ']) > 4) then
      ClipDate := ExtractWord(5, ClipInfo, [' ']);
  end
  else if (WordCount(ClipInfo,['='])>1) then  // предполагаем такую строчку Eu-152=46880,3
  begin
    ClipNuc := ExtractWord(1, ClipInfo, ['=']);
    ClipAct := ExtractWord(2, ClipInfo, ['=']);
    if (WordCount(ClipAct, [',']) > 1) then
      ClipAct := ExtractWord(1, ClipAct, [',']);
  end
  else
  begin
    {TODO: здесь может стоять и дата, сделать проверку на то, что это за информация}
    ClipNuc := ClipInfo;
    ClipAct := ClipInfo;
    ClipDate := ClipInfo;
  end;
  // выбор нуклида из списка нуклидов
  if CurLib.IsNuclideInLib(ClipNuc) then
  begin
    NuclCmbBx.ItemIndex := NuclCmbBx.Items.IndexOf(ClipNuc);
    ShowNuclideInfo();
  end;
  // установка активности
  if (ClipAct <> '') then
    if (TryStrToFloat(ClipAct, oldAct)) then
      jvedtOldAct.Text := ClipAct; //OldActEdt.Text:=ClipAct;
  // установка даты
  if (ClipDate <> '') then
  begin
    if (TryStrToDate(ClipDate, oldDate)) then
      dtpOldDate.DateTime := oldDate;
    CurFormatSettings.DateSeparator := '-';
    CurFormatSettings.ShortDateFormat := 'DD-MM-YYYY';
    CurFormatSettings.LongDateFormat := 'DD-MM-YYYY';
    if (TryStrToDate(ClipDate, oldDate, CurFormatSettings)) then
      dtpOldDate.DateTime := oldDate;
  end;
    // если всё установлено, то нажать на расчёт
  if (ClipAct <> '') and (ClipDate <> '') then
    btnCalcClick(Self);
end;

procedure TMainForm.mniPasteClick(Sender: TObject);
begin
  LoadInfoFromBuffer();
end;

procedure TMainForm.mniCopyClick(Sender: TObject);
begin
  btnBuffCopyClick(Sender);
end;

procedure TMainForm.jvedtOldActCustomValidate(Sender: TObject; Key: Char;
  const AText: String; const Pos: Integer; var IsValid: Boolean);
begin
  IsValid := (Key in ['0'..'9', 'e', 'E', '.']);
  IsValid := IsValid or ((Key in ['-']) and (Pos <= 1));
end;

procedure TMainForm.mniConvertActivityClick(Sender: TObject);
begin
  frmConvert := TfrmConvert.Create(Self);
  try
    frmConvert.jvedtAct1.Value := jvedtOldAct.Value;
    frmConvert.nucLib := CurLib;
    frmConvert.ShowModal();
  finally
    FreeAndNil(frmConvert);
  end;
end;

procedure TMainForm.ShowNuclideInfo;
begin
  ThalfLbl.Caption := 'T1/2= ' + FloatToStr(CurLib.ThalfFromName(NuclCmbBx.Items[NuclCmbBx.ItemIndex]))
    + ' лет';
  ThalfLbl.Hint := 'T1/2= ' + FloatToStr(CurLib.ThalfFromName(NuclCmbBx.Items[NuclCmbBx.ItemIndex])
    * DAYS_IN_YEAR) + ' сут.';
end;

end.
