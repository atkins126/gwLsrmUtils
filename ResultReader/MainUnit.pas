unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, ComCtrls, IniFiles, ImgList,
  TB2Item, TB2Dock, TB2Toolbar, JvJCLUtils, RusClipboard,
  UPeakList;

type
  TfrmMain = class(TForm)
    dlgOpen1: TOpenDialog;
    dlgSave1: TSaveDialog;
    grp1: TGroupBox;
    chk1: TCheckBox;
    chk2: TCheckBox;
    chk3: TCheckBox;
    chk4: TCheckBox;
    chk5: TCheckBox;
    chk6: TCheckBox;
    chk7: TCheckBox;
    chk8: TCheckBox;
    chk9: TCheckBox;
    chk10: TCheckBox;
    chk11: TCheckBox;
    chk12: TCheckBox;
    chk13: TCheckBox;
    chk14: TCheckBox;
    chk15: TCheckBox;
    mm1: TMainMenu;
    mniFile: TMenuItem;
    mniOpen: TMenuItem;
    mniSave: TMenuItem;
    mniEdit: TMenuItem;
    mniCopy: TMenuItem;
    mniPaste: TMenuItem;
    lstNuclides: TListBox;
    stat1: TStatusBar;
    mniOptions: TMenuItem;
    mniHead: TMenuItem;
    pm1: TPopupMenu;
    mniCopy1: TMenuItem;
    mniPaste1: TMenuItem;
    tbdck1: TTBDock;
    tbtlbr1: TTBToolbar;
    tbtmOpen: TTBItem;
    tbtmSave: TTBItem;
    il1: TImageList;
    mniAbout: TMenuItem;
    mniExtendedHead: TMenuItem;
    N1: TMenuItem;
    mniLoadState: TMenuItem;
    mniClearResults: TMenuItem;
    chk16: TCheckBox;
    chk17: TCheckBox;
    procedure mniOpenClick(Sender: TObject);
    procedure mniSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mniPasteClick(Sender: TObject);
    procedure mniCopyClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure mniCopy1Click(Sender: TObject);
    procedure mniPaste1Click(Sender: TObject);
    procedure tbtmOpenClick(Sender: TObject);
    procedure tbtmSaveClick(Sender: TObject);
    procedure mniAboutClick(Sender: TObject);
    procedure mniLoadStateClick(Sender: TObject);
    procedure mniClearResultsClick(Sender: TObject);
  private
    { Private declarations }
    procedure ResultToColumns(Str: string); // общая процедура разбиение текстового файла/буфера обмена на колонки
    function GenerateListForSave:string; // функция, которая формирует список строк для сохранения
    procedure ShowChecks; // делает checkbox enabled or disabled
    function GetSaveChkState: Integer; // возвращает число, соответствуеющее состоянию checkbox'ов
    procedure FillListBox; // заполнение листбокса списком нуклидов
    function InList(S: string): Boolean; // проверка, что нуклиды есть в листбоксе
    function InSelList(S: string): Boolean; // проверка, что нуклиды принадлежат выделенным в листбоксе
    procedure ShowStatusMenu; // процедура, показывающая текст в statusbar'e и активирующая пункты меню
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;
  chk: array[0..ARRCOUNT-1] of TCheckBox;
  DPeak: TPeakArray; // массив пиков
  IsLoaded: Boolean = False; // загружены ли данные
  FClipboard: TRusClipboard;
  CurrDir: string; // текущая директория для сохранения файлов

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
const
  CHK_HEIGHT = 16;
  CHK_SPACE = 8;
var CurrPath: string;
    IniFile: TIniFile;
    FileName: string;
    FileList: TStringList; 
    i, j: Integer;
begin
  CurrPath:=ExtractFileDir(ParamStr(0));
  dlgOpen1.InitialDir:=CurrPath;
  dlgSave1.InitialDir:=CurrPath;

  // chk creation in 2 columns
  for i:=0 to ARRCOUNT div 2 do
  begin
    chk[i] := TCheckBox.Create(frmMain);
    chk[i].Parent := grp1;
    chk[i].Left := 8;
    chk[i].Width := 80;
    chk[i].Top := CHK_HEIGHT*i + CHK_SPACE*(i+1);
    chk[i].Height := 16;
    chk[i].Visible := true;
    chk[i].Caption := EngOldNames[i];
  end;
  for i:=(ARRCOUNT div 2)+1 to ARRCOUNT-1 do
  begin
    j := i - ((ARRCOUNT div 2)+1);
    chk[i] := TCheckBox.Create(frmMain);
    chk[i].Parent := grp1;
    chk[i].Left := 96;
    chk[i].Width := 80;
    chk[i].Top := CHK_HEIGHT*j + CHK_SPACE*(j+1);
    chk[i].Height := 16;
    chk[i].Visible := true;
    chk[i].Caption := EngOldNames[i];
  end;

  IniFile:=TIniFile.Create(ExtractFilePath(Application.ExeName)+'ResultReader.ini');
  try
    for i:=0 to ARRCOUNT-1 do
      chk[i].Checked := IniFile.ReadBool('CHECKBOX_STATE', 'chk'+IntToStr(i), True);

    mniHead.Checked:=IniFile.ReadBool('MENU_STATE', 'mniHead', False);
    mniExtendedHead.Checked:=IniFile.ReadBool('MENU_STATE', 'mniExtendedHead', False);
  finally
    IniFile.Free;
  end;

  if ParamStr(1)<>'' then
  begin
    FileList:=TStringList.Create;
    FileName:=ParamStr(1);
    try
      FileList.LoadFromFile(FileName);
      ResultToColumns(FileList.Text);
    finally
      ShowStatusMenu;
      FileList.Free;
    end;
  end;

  if ParamStr(2)<>'' then
  begin
    FileList:=TStringList.Create;
    FileName:=ParamStr(2);
    try
      FileList.Text:=GenerateListForSave;
      FileList.SaveToFile(FileName);
    finally
      FileList.Free;
    end;
  end;

  FClipboard:=TRusClipboard.Create;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var IniFile: TIniFile;
    i: Integer;
begin
  FClipboard.Free;
  IniFile:=TIniFile.Create(ExtractFilePath(Application.ExeName)+'ResultReader.ini');
  try
    for i:=0 to ARRCOUNT-1 do
      IniFile.WriteBool('CHECKBOX_STATE', 'chk'+IntToStr(i), chk[i].Checked);

    IniFile.WriteBool('MENU_STATE', 'mniHead', mniHead.Checked);
    IniFile.WriteBool('MENU_STATE', 'mniExtendedHead', mniExtendedHead.Checked);
  finally
    IniFile.Free;
  end;
end;

procedure TfrmMain.ShowChecks;
var i: Integer;
begin
  for i:=0 to ARRCOUNT-1 do
  begin
    chk[i].Enabled:=not (DPeak.PeakNamesNumber[i+1]=0);
    if (DPeak.PeakNamesNumber[i+1]=0) then chk[i].Checked:=False;
  end;
end;

procedure TfrmMain.mniOpenClick(Sender: TObject);
var ResultName: string;
    FileList: TStringList;
begin
  if dlgOpen1.Execute then
  begin
    ResultName:=dlgOpen1.FileName;
    FileList:=TStringList.Create;
    try
      FileList.LoadFromFile(ResultName); // загрузка файла результатов в список
      ResultToColumns(FileList.Text);
    finally
      ShowStatusMenu; // указать, что файл загружен
      FileList.Free;
    end;
  end;
end;

procedure TfrmMain.ResultToColumns(Str: string);
begin
  if (Assigned(DPeak)) then
    DPeak.Free();
  DPeak := TPeakArray.LoadFromStr(Str);
  IsLoaded := DPeak.IsLoaded;
  
  ShowChecks; // дезактивирование Checkbox'ов, параметров которых нет в result.txt
  FillListBox; // заполнение списка нуклидов
end;

procedure TfrmMain.mniSaveClick(Sender: TObject);
var FileList: TStringList;
begin
  if (dlgOpen1.FileName <> '') then
    dlgSave1.FileName := dlgOpen1.FileName
  else
    dlgSave1.InitialDir := CurrDir;
  if dlgSave1.Execute then
  begin
    FileList := TStringList.Create;
    try
      FileList.Text := GenerateListForSave;
      FileList.SaveToFile(dlgSave1.FileName);
    finally
      FileList.Free;
    end;
    CurrDir:=ExtractFileDir(dlgSave1.FileName);
  end;
end;

function TfrmMain.GenerateListForSave: string;
var ResultList: TStringList;
    i: Integer;
    S: string;
begin
  ResultList:=TStringList.Create;
  // запись шапки
  if mniExtendedHead.Checked then
    ResultList.Text := DPeak.ExtendedHead;
  if mniHead.Checked then
  begin
    S:='';
    for i:=0 to ARRCOUNT-1 do
      if chk[i].Checked then S:=S+ EngOldNames[i] +#09;
    if S<>'' then ResultList.Add(S);
  end;
  for i:=0 to DPeak.LengthPeaks-1 do // сохранение только отмеченных колонок
    if InSelList(DPeak.Peaks[i].Nuclide) then
    begin
      S:='';
      if chk[0].Checked then S:=S+FloatToStr(DPeak.Peaks[i].Channel) +#09;
      if chk[1].Checked then S:=S+FloatToStr(DPeak.Peaks[i].DChannel) +#09;
      if chk[2].Checked then S:=S+FloatToStr(DPeak.Peaks[i].Energy) +#09;
      if chk[3].Checked then S:=S+FloatToStr(DPeak.Peaks[i].DEnergy) +#09;
      if chk[4].Checked then S:=S+FloatToStr(DPeak.Peaks[i].FWHM) +#09;
      if chk[5].Checked then S:=S+FloatToStr(DPeak.Peaks[i].FWHMK) +#09;
      if chk[6].Checked then S:=S+FloatToStr(DPeak.Peaks[i].Area) +#09;
      if chk[7].Checked then S:=S+FloatToStr(DPeak.Peaks[i].DArea) +#09;
      if chk[8].Checked then S:=S+FloatToStr(DPeak.Peaks[i].CountRate) +#09;
      if chk[9].Checked then S:=S+FloatToStrF(DPeak.Peaks[i].CountRate*DPeak.Peaks[i].DArea/DPeak.Peaks[i].Area,ffFixed,3,4) +#09;
      if chk[16].Checked then S:=S+FloatToStr(DPeak.Peaks[i].ChiSquare) +#09;
      if chk[10].Checked then S:=S+FloatToStr(DPeak.Peaks[i].Activity) +#09;
      if chk[11].Checked then S:=S+FloatToStr(DPeak.Peaks[i].DActivity) +#09;
      if chk[12].Checked then S:=S+FloatToStr(DPeak.Peaks[i].Intensity) +#09;
      if chk[13].Checked then S:=S+FloatToStr(DPeak.Peaks[i].DIntensity) +#09;
      if chk[15].Checked then S:=S+FloatToStr(DPeak.Peaks[i].Step) +#09;
      if chk[14].Checked then S:=S+DPeak.Peaks[i].Nuclide;
      if S<>'' then ResultList.Add(S);
    end;
  // обрезание лишних табов в конце строк
  for i:=0 to ResultList.Count-1 do
  begin
    S := ResultList[i];
    if (S <> '') then
      if (S[Length(S)] = #09) then ResultList[i] := Copy(S, 1, Length(S)-1);
  end;
  Result := ResultList.Text;
end;

function TfrmMain.GetSaveChkState: Integer;
var i: Integer;
begin
  Result := 0;
  for i:=0 to ARRCOUNT-1 do
    Result := Result + Integer(chk[i].Checked)*IntPower(2,i);
end;

procedure TfrmMain.FillListBox;
var i: Integer;
begin
  lstNuclides.Clear; // очистить список от предыдущих нуклидов
  for i:=0 to DPeak.LengthPeaks-1 do
    if not InList(DPeak.Peaks[i].Nuclide) then //lstNuclides.Items.IndexOf(DPeak[i].Nuclide)<0 then
      lstNuclides.Items.Add(DPeak.Peaks[i].Nuclide);
  for i:=0 to lstNuclides.Count-1 do lstNuclides.Selected[i]:=True; // выделение всех нуклидов
end;

function TfrmMain.InList(S: string): Boolean;
var i: Integer;
begin
  Result:=False;
  for i:=0 to lstNuclides.Items.Count-1 do
    if (S = lstNuclides.Items[i]) then Result:=True;
end;

function TfrmMain.InSelList(S: string): Boolean;
var i: Integer;
begin
  Result:=False;
  for i:=0 to lstNuclides.Items.Count-1 do
    if (lstNuclides.Selected[i]) and (S = lstNuclides.Items[i]) then Result:=True;
end;

procedure TfrmMain.mniPasteClick(Sender: TObject);
var BuffList: TStringList;
begin
  if FClipboard.HasFormat(CF_TEXT)then
  begin
    BuffList:=TStringList.Create;
    try
      //LoadKeyboardLayout(PChar('00000419'), KLF_ACTIVATE); // шаманство с переключением языка на русский при вставлении из буфера
      FClipboard.Open;
      BuffList.Text:=(FClipboard.AsText);
      FClipboard.Close;
      ResultToColumns(BuffList.Text); // обработать текст из буфера
      BuffList.SaveToFile('temp.txt');
    finally
      ShowStatusMenu;
      BuffList.Free;
    end;
  end;
end;

procedure TfrmMain.mniCopyClick(Sender: TObject);
var S:string;
begin
  S:=GenerateListForSave;
  FClipboard.Open;
  FClipBoard.SetTextBuf(PChar(S));
  FClipboard.Close;
end;

procedure TfrmMain.ShowStatusMenu;
begin
  if IsLoaded then stat1.Panels[0].Text:='Data is loaded' else stat1.Panels[0].Text:='Data isn''t loaded';
  mniSave.Enabled:=IsLoaded;
  mniCopy.Enabled:=IsLoaded;
  mniCopy1.Enabled:=IsLoaded;
  tbtmSave.Enabled:=IsLoaded;
end;

procedure TfrmMain.mniCopy1Click(Sender: TObject);
begin
  mniCopyClick(mniCopy1);
end;

procedure TfrmMain.mniPaste1Click(Sender: TObject);
begin
  mniPasteClick(mniPaste1);
end;

procedure TfrmMain.tbtmOpenClick(Sender: TObject);
begin
  mniOpenClick(tbtmOpen);
end;

procedure TfrmMain.tbtmSaveClick(Sender: TObject);
begin
  mniSaveClick(tbtmSave);
end;

procedure TfrmMain.mniAboutClick(Sender: TObject);
begin
  ShowMessage('Программа для чтения и сохранения файлов result.txt версия 0.1.4 бета');
end;

procedure TfrmMain.mniLoadStateClick(Sender: TObject);
var IniFile: TIniFile;
    i: Integer;
begin
  IniFile:=TIniFile.Create(ExtractFilePath(Application.ExeName)+'ResultReader.ini');
  try
    for i:=0 to ARRCOUNT-1 do
      chk[i].Checked:=IniFile.ReadBool('CHECKBOX_STATE', 'chk'+IntToStr(i), True);
    mniHead.Checked:=IniFile.ReadBool('MENU_STATE', 'mniHead', False);
    mniExtendedHead.Checked:=IniFile.ReadBool('MENU_STATE', 'mniExtendedHead', False);
  finally
    IniFile.Free;
  end;
end;

procedure TfrmMain.mniClearResultsClick(Sender: TObject);
var i: Integer;
begin
  if (Assigned(DPeak)) then
    FreeAndNil(DPeak);
  lstNuclides.Clear;
  ShowStatusMenu;

  for i:=0 to ARRCOUNT-1 do
    chk[i].Enabled := True;
end;

end.


