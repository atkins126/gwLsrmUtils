unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, TB2Dock, TB2Toolbar, ImgList, TB2Item,
  RusClipboard, UZones;

type
  TfrmMain = class(TForm)
    mm1: TMainMenu;
    pm1: TPopupMenu;
    dlgOpen1: TOpenDialog;
    dlgSave1: TSaveDialog;
    mniFile: TMenuItem;
    mniOpen: TMenuItem;
    mniSave: TMenuItem;
    mniClearResults: TMenuItem;
    mniEdit: TMenuItem;
    mniCopy: TMenuItem;
    mniPaste: TMenuItem;
    mniHelpParent: TMenuItem;
    tbdck1: TTBDock;
    tbtlbr1: TTBToolbar;
    tbtmOpen: TTBItem;
    tbtmSave: TTBItem;
    il1: TImageList;
    edtHeaderName: TEdit;
    lstZones: TListBox;
    mmoResult: TMemo;
    mniAddNuclide: TMenuItem;
    mniTools: TMenuItem;
    mniCalcRatio: TMenuItem;
    tbtmCalcRatio: TTBItem;
    mniHelp: TMenuItem;
    mniAbout: TMenuItem;
    mniOptions: TMenuItem;
    mniAbsRatio: TMenuItem;
    procedure mniOpenClick(Sender: TObject);
    procedure tbtmOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mniClearResultsClick(Sender: TObject);
    procedure mniAddNuclideClick(Sender: TObject);
    procedure mniCalcRatioClick(Sender: TObject);
    procedure mniPasteClick(Sender: TObject);
    procedure tbtmCalcRatioClick(Sender: TObject);
    procedure mniAboutClick(Sender: TObject);
    procedure mniHelpClick(Sender: TObject);
    procedure mniCopyClick(Sender: TObject);
    procedure lstZonesClick(Sender: TObject);
  private
    { Private declarations }
    procedure ShowStatus();
    procedure FillListBox();
    procedure FillMemo();
    function CalcRatio(var Ratio: TVector; IsAbs: Boolean = False): Boolean;
    procedure loadFromClipBoard();
  public
    { Public declarations }
  end;

resourcestring
  rsAbout = 'ZoneReader is for reading txt-file from zone info from SL. Version 0.1.2';

var
  frmMain: TfrmMain;
  appPath: string;
  curZones: TZoneList;
  Ratio: TVector;
  FClipboard: TRusClipboard;
  IsLoaded: Boolean = False;
  IsCalc: Boolean = False;

implementation

{$R *.dfm}

uses JvJCLUtils;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  if (DecimalSeparator <> '.') then DecimalSeparator := '.';
  appPath := ExtractFilePath(ParamStr(0));
  FClipboard := TRusClipboard.Create();
  loadFromClipBoard();
end;

procedure TfrmMain.mniOpenClick(Sender: TObject);
var ZoneName: string;
begin
  if (dlgOpen1.Execute) then
  begin
    ZoneName := dlgOpen1.FileName;
    if (Assigned(curZones)) then FreeAndNil(curZones);
    curZones := TZoneList.loadFromFile(ZoneName);
    IsLoaded := curZones.loadResult;
    ShowStatus();   // указать, что файл загружен
    FillListBox();  // заполнить листбокс
  end;
end;

procedure TfrmMain.mniPasteClick(Sender: TObject);
begin
  loadFromClipBoard();
end;

procedure TfrmMain.tbtmOpenClick(Sender: TObject);
begin
  mniOpenClick(Self);
end;

procedure TfrmMain.ShowStatus;
begin
  mniSave.Enabled := IsLoaded;
  mniAddNuclide.Enabled := IsLoaded;
  mniCalcRatio.Enabled := IsLoaded;
  tbtmCalcRatio.Enabled := IsLoaded;
  if IsLoaded then
  begin
    edtHeaderName.Text := curZones.Head;
    edtHeaderName.Font.Color := clBlue;
  end
  else
  begin
    edtHeaderName.Text := 'not loaded';
    edtHeaderName.Font.Color := clRed;
  end;
  mniCopy.Enabled := IsCalc;
  mniSave.Enabled := IsCalc;
end;

procedure TfrmMain.FillListBox;
var i: Integer;
begin
  lstZones.Clear();
  if IsLoaded then
  begin
    lstZones.Items.Add('##  Begin  End  Peaks count  Area  Area err.  Count rate');
    for i := 0 to curZones.zonesCount - 1 do
      lstZones.Items.Add(Zone2Str(curZones.Zones[i]));
    if (curZones.ZonesCount > 0) then
      lstZones.ItemIndex := 0;
  end;
end;

procedure TfrmMain.FillMemo;
begin
  {mmoZones.Clear;
  if IsLoaded then
  begin
    mmoZones.Lines.Add('##  Begin  End  Peaks count  Area  Area err.  Count rate');
    for i:=0 to CurZones.ZonesCount-1 do
      mmoZones.Lines.Add(Zone2Str(CurZones.Zones[i]));
  end;}
end;

procedure TfrmMain.mniClearResultsClick(Sender: TObject);
begin
  if (Assigned(curZones)) then FreeAndNil(curZones);
  IsLoaded := False;
  ShowStatus();
  FillListBox();
  mmoResult.Clear();
end;

procedure TfrmMain.mniAddNuclideClick(Sender: TObject);
var S: string;
begin
  if InputQuery('Insert nuclide','Enter nuclide name', S) then
  begin
    if lstZones.ItemIndex > 0 then
    begin
      CurZones.Zones[lstZones.ItemIndex - 1].Nuclide := S;
      lstZones.Items[lstZones.ItemIndex] := Zone2Str(CurZones.Zones[lstZones.ItemIndex - 1]);
    end;
  end;
end;

procedure TfrmMain.mniCalcRatioClick(Sender: TObject);
var i: Integer;
    S1, S2: string;
begin
  SetLength(Ratio, CurZones.ZonesCount);
  IsCalc := CalcRatio(Ratio, mniAbsRatio.Checked);
  mmoResult.Clear;
  S1 := '';
  S2 := '';
  for i := 0 to CurZones.ZonesCount - 1 do
  begin
    S1 := S1 + CurZones.Zones[i].Nuclide + #09;
    S2 := S2 + Format('%3.3f', [Ratio[i]]) + #09;
  end;
  mmoResult.Lines.Add(S1);
  mmoResult.Lines.Add(S2);
  ShowStatus();
end;

function TfrmMain.CalcRatio(var Ratio: TVector; IsAbs: Boolean = False): Boolean;
var Refer, i: Integer;
    AreaSumm: Double;
begin
  Result := False;
  AreaSumm := 0;
  Refer := lstZones.ItemIndex - 1;
  if (Refer < 0) or (Refer > CurZones.ZonesCount - 1) then
    Refer := 0;

  for i := 0 to CurZones.ZonesCount - 1 do
    Ratio[i] := 0;
  for i := 0 to CurZones.ZonesCount - 1 do
  begin
    if CurZones.Zones[i].Area = 0 then  Exit;
  end;

  if IsAbs then
    for i := 0 to CurZones.ZonesCount - 1 do
      AreaSumm := AreaSumm + CurZones.Zones[i].Area;

  for i := 0 to CurZones.ZonesCount - 1 do
  begin
    if IsAbs then
      Ratio[i] := CurZones.Zones[i].Area / AreaSumm
    else
      Ratio[i] := CurZones.Zones[i].Area / CurZones.Zones[Refer].Area
  end;
  Result := True;
end;

procedure TfrmMain.lstZonesClick(Sender: TObject);
begin
  if (lstZones.ItemIndex > 0) then
    mniCalcRatioClick(Self);
end;

procedure TfrmMain.tbtmCalcRatioClick(Sender: TObject);
begin
  mniCalcRatioClick(Self);
end;

procedure TfrmMain.mniAboutClick(Sender: TObject);
begin
  ShowMessage(rsAbout);
end;

procedure TfrmMain.mniHelpClick(Sender: TObject);
begin
  try
    Exec('notepad.exe', AppPath + 'readme.txt', AppPath);
  except
    raise Exception.Create('Wrong help-file path');
  end;
end;

procedure TfrmMain.mniCopyClick(Sender: TObject);
var i: Integer;
    //BuffList: TStringList;
    S: string;
begin
  //BuffList:=TStringList.Create;

  for i:=0 to High(Ratio) do
  begin
    S := S + FloatToStr(Ratio[i]) + #13#10;
  end;

  FClipboard.Open;
  FClipBoard.SetTextBuf(PChar(S));
  FClipboard.Close;
end;

procedure TfrmMain.loadFromClipBoard;
var BuffList: TStringList;
begin
  if FClipboard.HasFormat(CF_TEXT)then
  begin
    BuffList := TStringList.Create;
    if (Assigned(curZones)) then FreeAndNil(curZones);
    FClipboard.Open;
    BuffList.Text := (FClipboard.AsText);
    FClipboard.Close;
    curZones := TZoneList.loadFromList(BuffList);
    IsLoaded := curZones.loadResult;
    try
      //LoadKeyboardLayout(PChar('00000419'), KLF_ACTIVATE); // шаманство с переключением языка на русский при вставлении из буфера
      BuffList.SaveToFile(appPath + 'temp.txt');
    finally
      ShowStatus();
      FillListBox(); // заполнить листбокс
      BuffList.Free();
    end;
  end;
end;



end.
