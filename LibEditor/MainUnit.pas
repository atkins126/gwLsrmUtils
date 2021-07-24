unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, ImgList, ComCtrls,
  TB2Item, TB2Dock, TB2Toolbar, 
  LibUnit;

type
  TfrmMain = class(TForm)
    dlgOpen1: TOpenDialog;
    dlgSave1: TSaveDialog;
    mm1: TMainMenu;
    mniFile: TMenuItem;
    mniOpen: TMenuItem;
    mniSave: TMenuItem;
    tbdck1: TTBDock;
    tbtlbr1: TTBToolbar;
    tbtmSave: TTBItem;
    tbtmOpen: TTBItem;
    il1: TImageList;
    lstNuclideNames: TListBox;
    lstEnLines: TListBox;
    mniTools: TMenuItem;
    mniSortbyA: TMenuItem;
    mniSortByName: TMenuItem;
    pmNuclSort: TPopupMenu;
    mniSortbynamep: TMenuItem;
    mniSortbymassnumber: TMenuItem;
    pmLinesSort: TPopupMenu;
    mniEnSort: TMenuItem;
    mniEnSortAll: TMenuItem;
    mniEnergySortAll: TMenuItem;
    mniCloselib: TMenuItem;
    mniMergeNuclides: TMenuItem;
    mniConvertToEng: TMenuItem;
    mniAbout: TMenuItem;
    stat1: TStatusBar;
    mniGammaConst: TMenuItem;
    mniConvert2EffmakerString: TMenuItem;
    mniLoadWidth: TMenuItem;
    tbtmGammaConst: TTBItem;
    mniMergeLines: TMenuItem;
    mniDeleteLine: TMenuItem;
    procedure mniOpenClick(Sender: TObject);
    procedure mniSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lstNuclideNamesClick(Sender: TObject);
    procedure mniSortbyAClick(Sender: TObject);
    procedure mniSortByNameClick(Sender: TObject);
    procedure mniEnSortClick(Sender: TObject);
    procedure mniCloselibClick(Sender: TObject);
    procedure mniEnergySortAllClick(Sender: TObject);
    procedure mniMergeNuclidesClick(Sender: TObject);
    procedure mniConvertToEngClick(Sender: TObject);
    procedure mniAboutClick(Sender: TObject);
    procedure mniGammaConstClick(Sender: TObject);
    procedure mniConvert2EffmakerStringClick(Sender: TObject);
    procedure mniLoadWidthClick(Sender: TObject);
    procedure tbtmGammaConstClick(Sender: TObject);
    procedure mniMergeLinesClick(Sender: TObject);
    procedure mniDeleteLineClick(Sender: TObject);
  private
    { Private declarations }
    procedure ShowNuclides();
    procedure EnableItems();
    procedure renewFormCaption(const filename: string);
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;
  CurLib: TLibrary;
  IsLibLoaded: Boolean = false;
  lastFileName: string = '';

implementation

uses Math, JvJCLUtils,
  MergedNuclidesUnit, NMGammaConstFunction, UGammaConst, Matrix, gwFileUtils;

{$R *.dfm}

procedure TfrmMain.mniOpenClick(Sender: TObject);
var FileName, FileExt: string;
begin
  dlgOpen1.FilterIndex := 1;
  dlgOpen1.FileName := lastFileName;
  if dlgOpen1.Execute then
  begin
    if IsLibLoaded then mniCloselibClick(mniOpen);
    if not Assigned(CurLib) then CurLib := TLibrary.Create;
    FileName := dlgOpen1.FileName;
    FileExt := ExtractFileExt(FileName);
    lastFileName := FileName;
    if AnsiSameText(FileExt, '.lib') then
      IsLibLoaded := CurLib.OpenLib(FileName)
    else if AnsiSameText(FileExt, '.tlb') then
      IsLibLoaded := CurLib.OpenShortTabLib(FileName)
    else
      IsLibLoaded := CurLib.OpenTabLib(FileName);
    {TODO: добавить проверку на то, что загрузка прошла успешно}
    EnableItems();
    ShowNuclides();
    renewFormCaption(FileName);
  end;
end;

procedure TfrmMain.mniSaveClick(Sender: TObject);
var FileName, FileExt: string;
begin
  dlgSave1.FileName := lastFileName;
  if dlgSave1.Execute then
  begin
    FileName := dlgSave1.FileName;
    FileExt := ExtractFileExt(FileName);
    lastFileName := FileName;
    if AnsiSameText(FileExt, '.lib') then
      CurLib.SaveLib(FileName)
    else if AnsiSameText(FileExt, '.tlb') then
      CurLib.SaveShortTabLib(FileName)
    else
      CurLib.SaveTabLib(FileName);
    renewFormCaption(FileName);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var FileName, FileExt: string;
begin
  // 1-й параметр имя библиотеки, которая нужно открыть автоматически, 2-й параметр = имя библиотеки, в которую нужно сохранить автоматически
  // 1-й параметр
  if ParamCount >= 1 then
  begin
    CurLib := TLibrary.Create;
    FileName := ParamStr(1);
    FileExt := ExtractFileExt(FileName);
    if AnsiSameText(FileExt, '.lib') then
      IsLibLoaded := CurLib.OpenLib(FileName)
    else
      IsLibLoaded := CurLib.OpenTabLib(FileName);
    {TODO: добавить проверку на то, что загрузка прошла успешно}
    // 2-й параметр
    if IsLibLoaded and (ParamStr(2) <> '') then
    begin
      FileName := ParamStr(2);
      FileExt := ExtractFileExt(FileName);
      if AnsiSameText(FileExt, '.lib') then
        CurLib.SaveLib(FileName)
      else
        CurLib.SaveTabLib(FileName);
      Application.Terminate();
    end;

    EnableItems();
    ShowNuclides();
  end
  else
    EnableItems;
end;

procedure TfrmMain.ShowNuclides;
begin
  lstNuclideNames.Clear();
  lstNuclideNames.Items.Text := CurLib.LibNuclNames2Text;
  {for i:=0 to CurLib.NuclidesCount-1 do
    lstNuclideNames.Items.Add(CurLib.Nuclides[i].Name+'    '+FloatToStrF(CurLib.Nuclides[i].THalf,ffGeneral,4,2)+' '+Curlib.Nuclides[i].TUnits);}
end;

procedure TfrmMain.EnableItems;
begin
  mniSave.Enabled := IsLibLoaded;
  tbtmSave.Enabled := IsLibLoaded;
  mniSortbyA.Enabled := IsLibLoaded;
  mniSortByName.Enabled := IsLibLoaded;
  mniSortbynamep.Enabled := IsLibLoaded;
  mniSortbymassnumber.Enabled := IsLibLoaded;
  mniEnSort.Enabled := IsLibLoaded;
  mniEnSortAll.Enabled := IsLibLoaded;
  mniEnergySortAll.Enabled := IsLibLoaded;
  mniCloselib.Enabled := IsLibLoaded;
  mniConvertToEng.Enabled := IsLibLoaded;
  mniGammaConst.Enabled := IsLibLoaded;
end;

procedure TfrmMain.lstNuclideNamesClick(Sender: TObject);
var i,j: Integer;
    GC, diff: Double;
begin
  // показать список линий
  i := lstNuclideNames.ItemIndex;
  lstEnLines.Clear();
  for j:=0 to CurLib.Nuclide[i].LinesCount-1 do
    lstEnLines.Items.Add(CurLib.Nuclide[i].libNuclLine2Text(j, CurLib.isNewFormat));

  // расчёт гамма-постоянной
  diff := 0;
  i := lstNuclideNames.ItemIndex;
  if (i >= 0) then
  begin
    GC := GammaConst(CurLib.Nuclide[i]);
    if (CurLib.Nuclide[i].GammaConst > MinSingle) then
      diff := GC / CurLib.Nuclide[i].GammaConst - 1;
    stat1.Panels[0].Text := Format('GammaConst=%2.3f, diff=%f%%', [GC, diff * 100]);
  end;
end;

procedure TfrmMain.mniSortbyAClick(Sender: TObject);
begin
  CurLib.SortLibA();
  ShowNuclides();
end;

procedure TfrmMain.mniSortByNameClick(Sender: TObject);
begin
  CurLib.SortLibName();
  ShowNuclides();
end;

procedure TfrmMain.mniEnSortClick(Sender: TObject);
begin
  CurLib.Nuclide[lstNuclideNames.ItemIndex].sortLines();
  lstNuclideNamesClick(mniEnSort);
end;

procedure TfrmMain.mniCloselibClick(Sender: TObject);
begin
  FreeAndNil(CurLib);
  IsLibLoaded := False;
  lstNuclideNames.Clear();
  lstEnLines.Clear();
  EnableItems();
  renewFormCaption('');
end;

procedure TfrmMain.mniEnergySortAllClick(Sender: TObject);
begin
  CurLib.SortLibLines();
  ShowNuclides();
  lstNuclideNamesClick(mniEnergySortAll);
end;

procedure TfrmMain.mniMergeNuclidesClick(Sender: TObject);
var i,k, MerNucNum : Integer;
    SelIndex: array of Integer; // массив с индексами выделенных строк
    //S: string;
    Factor: Double;
begin
  Factor := 1;
  // проверяем, что выделенных > 1
  if (lstNuclideNames.SelCount < 2) then Exit;

  // создаём массив с индексами выделенных колонок
  k := 0;
  SetLength(SelIndex, lstNuclideNames.SelCount);
  for i:=0 to lstNuclideNames.Count-1 do
    if lstNuclideNames.Selected[i] then begin
      SelIndex[k] := i;
      Inc(k);
    end;

  // получаем имя нуклида, которое будет носить объединённый нуклид и запоминаем номер этого нуклида
  {InputQuery('Merged nuclides name', 'Enter nuclide name',S);
  MerNucNum:=SelIndex[0];
  for k:=0 to lstNuclideNames.SelCount-1 do
    if CompareText(CurLib.Nuclides[SelIndex[k]].Name,S)=0 then MerNucNum:=SelIndex[k];}

  frmMergedNuclide := TfrmMergedNuclide.Create(Self);
  try
    for i:=0 to lstNuclideNames.SelCount-1 do
      frmMergedNuclide.cbbNucName.Items.Add(CurLib.Nuclide[SelIndex[i]].Name);
    frmMergedNuclide.cbbNucName.ItemIndex := 0;
    if (frmMergedNuclide.ShowModal = mrOk) then
    begin
      MerNucNum := SelIndex[frmMergedNuclide.cbbNucName.ItemIndex];
      Factor := frmMergedNuclide.jvedtFactor.Value;
    end
    else
      MerNucNum := -100;
  finally
    FreeAndNil(frmMergedNuclide);
  end;

  if MerNucNum = -100 then Exit;

  // добавляем в MerNucNum-ю запись линии остальных
  for k:=0 to lstNuclideNames.SelCount-1 do
    if (SelIndex[k] <> MerNucNum) then
    begin
      if (Factor = 1) then
        CurLib.CopyNucLines(SelIndex[k], MerNucNum)
      else
        CurLib.CopyNucLines(SelIndex[k], MerNucNum, Factor);
    end;
  // удаляем остальные записи
  for k := lstNuclideNames.SelCount-1 downto 0 do
    if (SelIndex[k] <> MerNucNum) then CurLib.DeleteNuclide(SelIndex[k]);
    
  // обновляем список
  ShowNuclides();
end;

procedure TfrmMain.mniMergeLinesClick(Sender: TObject);
var tmpLineIndexes: array of Integer;
    i, j, firstIndex: Integer;
begin
  // check lines for merge
  if (lstEnLines.SelCount < 2) then Exit;

  // search line indexes to merge
  j := 0;
  firstIndex := -1;
  SetLength(tmpLineIndexes, lstEnLines.SelCount);
  for i:=0 to lstEnLines.Items.Count-1 do
    if (lstEnLines.Selected[i]) then begin
      if (firstIndex = -1) then firstIndex := i;
      tmpLineIndexes[j] := i;
      Inc(j);
    end;

  // merge lines
  CurLib.mergeNuclideLines(lstNuclideNames.ItemIndex, tmpLineIndexes);

  // show
  lstNuclideNamesClick(self);
  lstEnLines.ItemIndex := firstIndex;
end;

procedure TfrmMain.mniConvertToEngClick(Sender: TObject);
begin
  CurLib.ConvertLibToEng();
  ShowNuclides;
end;

procedure TfrmMain.mniAboutClick(Sender: TObject);
begin
  ShowMessage('LibEditor ver. 0.1.6 beta');
end;

procedure TfrmMain.mniGammaConstClick(Sender: TObject);
begin
  frmGammaConst := TfrmGammaConst.Create(Self);
  try
    UGammaConst.CurNuclide := CurLib.Nuclide[lstNuclideNames.ItemIndex];
    frmGammaConst.ShowModal();
  finally
    frmGammaConst.Free();
  end;
end;

procedure TfrmMain.mniConvert2EffmakerStringClick(Sender: TObject);
var i, k: Integer;
    SelIndex: array of Integer; // массив с индексами выделенных строк
    Activities: array of Double; // массив с активностями
    S: string;
begin
  if (lstNuclideNames.SelCount < 1) then Exit;

  // создаём массив с индексами выделенных колонок
  k := 0;
  SetLength(SelIndex, lstNuclideNames.SelCount);
  for i:=0 to lstNuclideNames.Count-1 do
    if lstNuclideNames.Selected[i] then
    begin
      SelIndex[k] := i;
      Inc(k);
    end;

  // create array with activities
  SetLength(Activities, Length(SelIndex));
  InputQuery('Activities prompt', 'Input ' + IntToStr(Length(Activities))
    + ' comma-separated activities', S);

  for i:=0 to Length(Activities)-1 do
  begin
    if ((i+1) <= WordCount(S, [','])) then
      Activities[i] := StrToFloat(ExtractWord(i+1, S, [',']))
    else
      Activities[i] := 1;
  end;

  // получение строки EffMaker
  S := CurLib.ConvertNuclides2EffmakerString(SelIndex, Activities);
  MessageBox(Application.Handle, PAnsiChar(S), 'EffMaker string', MB_OK);
end;

procedure TfrmMain.mniLoadWidthClick(Sender: TObject);
begin
  dlgOpen1.FilterIndex := 2;
  if (dlgOpen1.Execute) then
    if (Assigned(CurLib)) then begin
      CurLib.LoadWidthes(dlgOpen1.FileName);
      CurLib.SetWidthes();
    end;
end;

procedure TfrmMain.tbtmGammaConstClick(Sender: TObject);
begin
  mniGammaConstClick(Self);
end;

procedure TfrmMain.mniDeleteLineClick(Sender: TObject);
var tmpLineIndexes: TIntegerVector;
    i: Integer;
begin
  // TODO: сделать удаление нескольких линий
  if (lstEnLines.SelCount < 1) then Exit;

  // delete line
  if (lstEnLines.SelCount = 1) then
    CurLib.Nuclide[lstNuclideNames.ItemIndex].deleteLine(lstEnLines.ItemIndex)
  else begin
    // search line indexes to delete
    tmpLineIndexes := TIntegerVector.Create(lstEnLines.SelCount);
    for i:=0 to lstEnLines.Items.Count-1 do
      if (lstEnLines.Selected[i]) then
        tmpLineIndexes.push_back(i);
    CurLib.Nuclide[lstNuclideNames.ItemIndex].deleteLines(tmpLineIndexes.x);
  end;

  // show
  lstNuclideNamesClick(self);
end;

procedure TfrmMain.renewFormCaption(const filename: string);
var frmCaption: string;
begin
  if (filename <> '') then begin
    frmCaption := filenameFromPath(filename);
    frmCaption := 'Library editor -- ' + frmCaption;
  end
  else begin
    frmCaption := 'Library editor';
  end;
  frmMain.Caption := frmCaption;
end;

end.
