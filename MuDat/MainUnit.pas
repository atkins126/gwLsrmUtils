unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvExStdCtrls, JvEdit, JvValidateEdit, Menus, IniFiles,
  MjuUnit, EnScaleUnit, Matrix;

type
  TOutPutResults = record
    Energy,
    mu,
    absorbtion: Double;
  end;
  TMatSelection = (msElNum, msElName, msMatName);
  TfrmMain = class(TForm)
    grp1: TGroupBox;
    rbElNum: TRadioButton;
    rbElName: TRadioButton;
    edtElName: TEdit;
    jvedtElNum: TJvValidateEdit;
    lbl2: TLabel;
    btnShowMju: TButton;
    mmoEnergies: TMemo;
    mm1: TMainMenu;
    mniOptions: TMenuItem;
    mniPreferences: TMenuItem;
    mniAbout: TMenuItem;
    mmoOutput: TMemo;
    mniFile: TMenuItem;
    mniLoadGrid: TMenuItem;
    mniSaveGrid: TMenuItem;
    grp2: TGroupBox;
    lbl3: TLabel;
    lbl4: TLabel;
    lbl5: TLabel;
    btnSetScale: TButton;
    chkLogScale: TCheckBox;
    jvedtEmin: TJvValidateEdit;
    jvedtEmax: TJvValidateEdit;
    jvedtPointsNum: TJvValidateEdit;
    dlgOpen1: TOpenDialog;
    dlgSave1: TSaveDialog;
    mniSaveMju: TMenuItem;
    lbl6: TLabel;
    mniAction: TMenuItem;
    mniShowElInfo: TMenuItem;
    lbl7: TLabel;
    jvedtThick: TJvValidateEdit;
    rbMaterial: TRadioButton;
    edtMatName: TEdit;
    lblRho: TLabel;
    jvedtRho: TJvValidateEdit;
    procedure FormCreate(Sender: TObject);
    procedure rbElNumNameClick(Sender: TObject);
    procedure btnShowMjuClick(Sender: TObject);
    procedure mniPreferencesClick(Sender: TObject);
    procedure mniAboutClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnSetScaleClick(Sender: TObject);
    procedure mniLoadGridClick(Sender: TObject);
    procedure mniSaveGridClick(Sender: TObject);
    procedure mniSaveMjuClick(Sender: TObject);
    procedure mniShowElInfoClick(Sender: TObject);
    procedure edtElNameExit(Sender: TObject);
    procedure jvedtElNumExit(Sender: TObject);
  private
    procedure getEnergies(var outputLst: array of TOutPutResults); overload;
    procedure getEnergies(aEnergies: TDoubleVector); overload;
    (*procedure getMuAbsorption(var outputLst: array of TOutPutResults;
      const aElement: TElement); overload;
    procedure getMuAbsorption(var outputLst: array of TOutPutResults;
      const aMaterial: TMaterial); overload;*)
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;
  curPath: string;
  isClear: Boolean = True; // очищать ли вывод
  muType: Integer = 0; // тип выводимого мю: полное, для комптона и т.д.
  curMatSel: TMatSelection = msElNum;
  chemTableMju: TChemTableMju;
  enUnits: TEnergyUnits = euMeV; // 0 - MeV, 1 - keV
  enGrid: TEnGrid;
  curEnergies: TDoubleVector;
  outPutList: array of TOutPutResults;
  //CurEl: TElement;

resourcestring
  InfoStr1 = 'MuDat is offline XCOM programm ver 0.2.1 beta';
  rsElPropFileName = 'ElementsProperties.txt';
  rsError1 = 'DB-file %s is absent';
  rsError2 = 'Wrong thick value or energy value';

implementation

uses UnitPreferences, UnitElInfo;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var i: Integer;
    iniFile: TIniFile;
begin
  if (DecimalSeparator <> '.') then DecimalSeparator := '.';

  curPath := ExtractFilePath(ParamStr(0));
  chemTableMju := TChemTableMju.LoadMjusFromTxtFiles(curPath + 'XCOM\');
  i := chemTableMju.elCount;
  if (i < 1) then
    ShowMessage('DB is not loaded');
  if not chemTableMju.LoadElPropFromTxt(curPath + rsElPropFileName) then
  begin
    ShowMessage(Format(rsError1, [rsElPropFileName]));
  end;

  enGrid := TEnGrid.Create();
  iniFile := TIniFile.Create(curPath + 'mudat.ini');
  try
    isClear:=iniFile.ReadBool('PREFERENCES', 'isClear', True);
    enUnits := TEnergyUnits(iniFile.ReadInteger('PREFERENCES', 'Energy_units', 0));
    jvedtElNum.Value:=iniFile.ReadInteger('VALUES', 'Element_Number', 1);
    edtElName.Text:=iniFile.ReadString('VALUES', 'Element_Symbol', 'H');
    jvedtEmin.Value:=iniFile.ReadFloat('EN_GRID', 'Emin', 0);
    jvedtEmax.Value:=iniFile.ReadFloat('EN_GRID', 'Emax', 0);
    jvedtPointsNum.Value:=iniFile.ReadInteger('EN_GRID', 'Ecount', 2);
  finally
    iniFile.Free();
  end;

  if (enUnits = euMeV) then
    lbl2.Caption := 'Input energies in MeV, 1 per line.' +#13#10 + 'Blank lines will be ignored'
  else
    lbl2.Caption := 'Input energies in keV, 1 per line.' +#13#10 + 'Blank lines will be ignored';
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var iniFile: TIniFile;
begin
  iniFile := TIniFile.Create(curPath + 'mudat.ini');
  try
    iniFile.WriteBool('PREFERENCES', 'isClear', isClear);
    iniFile.WriteInteger('PREFERENCES', 'Energy_units', Ord(enUnits));
    iniFile.WriteInteger('VALUES', 'Element_Number', jvedtElNum.Value);
    iniFile.WriteString('VALUES', 'Element_Symbol', edtElName.Text);
    iniFile.WriteFloat('EN_GRID', 'Emin', jvedtEmin.Value);
    iniFile.WriteFloat('EN_GRID', 'Emax', jvedtEmax.Value);
    iniFile.WriteInteger('EN_GRID', 'Ecount', jvedtPointsNum.Value);
  finally
    iniFile.Free;
    Application.Terminate;
  end;
end;

procedure TfrmMain.rbElNumNameClick(Sender: TObject);
begin
  jvedtElNum.Enabled := rbElNum.Checked;
  edtElName.Enabled := rbElName.Checked;
  edtMatName.Enabled := rbMaterial.Checked;
  if (rbElNum.Checked) then
    curMatSel := msElNum
  else if (rbElName.Checked) then
    curMatSel := msElName
  else if (rbMaterial.Checked) then
    curMatSel := msMatName
end;

procedure TfrmMain.btnShowMjuClick(Sender: TObject);
var i: Integer;
    curElement: TElement;
    curMaterial: TMaterial;
    thick, rho: Double;
begin
  // read input data
  if (curMatSel = msElNum) then begin
    curElement := chemTableMju.ElFromZ(Byte(jvedtElNum.Value));
    curMaterial := nil;
  end
  else if (curMatSel = msElName) then begin
    curElement := chemTableMju.ElFromSymb(edtElName.Text);
    curMaterial := nil;
  end
  else begin
    rho := jvedtRho.Value;
    curMaterial := TMaterial.Create(chemTableMju, edtMatName.Text, rho, edtMatName.Text);
    curElement := nil;
  end;
  thick := jvedtThick.Value;

  // obtain energy list
  SetLength(outPutList, mmoEnergies.Lines.Count);
  getEnergies(outPutList);

  // obtain mu for every energy
  for i:=0 to Length(outPutList)-1 do
  begin
    if (curMatSel = msElNum) or (curMatSel = msElName) then begin
      outPutList[i].mu := curElement.getMu(outPutList[i].Energy, muType);
      outPutList[i].absorbtion := curElement.calcAbsorption(outPutList[i].mu, thick);
    end
    else begin
      outPutList[i].mu := curMaterial.getMu(outPutList[i].Energy);
      outPutList[i].absorbtion := curMaterial.calcAbsorption(outPutList[i].mu, thick);
    end;
  end;

  // output mu to StringList
  if (isClear) then
    mmoOutput.Clear();
  for i:=0 to High(outPutList) do
  begin
    if (enUnits = eukeV) then
      outPutList[i].Energy := outPutList[i].Energy * 1000;  // 1 - keV
    mmoOutput.Lines.Add(FloatToStr(outPutList[i].Energy) + #09 + FloatToStrF(outPutList[i].mu, ffExponent, 4, 2)
      + #09 + FloatToStrF(outPutList[i].absorbtion, ffExponent, 4, 2)); 
  end;
end;

procedure TfrmMain.getEnergies(var outputLst: array of TOutPutResults);
var i, j: Integer;
    energy: Double;
begin
  j:=0;
  for i:=0 to mmoEnergies.Lines.Count-1 do
    if (mmoEnergies.Lines[i] <> '') then
      if (TryStrToFloat(mmoEnergies.Lines[i], energy)) then begin
        if (enUnits = eukeV) then
          outPutList[j].Energy := energy / 1000
        else
          outPutList[j].Energy := energy;
        Inc(j);
      end;
  SetLength(outPutList, j);
end;

procedure TfrmMain.getEnergies(aEnergies: TDoubleVector);
var i: Integer;
    energy: Double;
begin
  aEnergies.capacity := mmoEnergies.Lines.Count;
    for i:=0 to mmoEnergies.Lines.Count-1 do
    if (mmoEnergies.Lines[i] <> '') then
      if (TryStrToFloat(mmoEnergies.Lines[i], energy)) then begin
        if (enUnits = eukeV) then
          aEnergies.push_back(energy / 1000)
        else
          aEnergies.push_back(energy);
      end;
end;

procedure TfrmMain.mniPreferencesClick(Sender: TObject);
begin
  frmPreferences := TfrmPreferences.Create(Self);
  try
    frmPreferences.chkOutput.Checked := isClear;
    frmPreferences.rgEnUnits.ItemIndex := Ord(enUnits); // 0- MeV, 1 - keV
    frmPreferences.cbbMuType.ItemIndex := muType;

    if (frmPreferences.ShowModal = mrOk) then
    begin
      isClear := frmPreferences.chkOutput.Checked;
      enUnits := TEnergyUnits(frmPreferences.rgEnUnits.ItemIndex);
      if (enUnits = euMeV) then
        lbl2.Caption := 'Input energies in MeV, 1 per line.'+#13#10+'Blank lines will be ignored'
      else
        lbl2.Caption := 'Input energies in keV, 1 per line.'+#13#10+'Blank lines will be ignored';
      muType := frmPreferences.cbbMuType.ItemIndex;
    end;
  finally
    frmPreferences.Free;
  end;
end;

procedure TfrmMain.mniAboutClick(Sender: TObject);
begin
  ShowMessage(InfoStr1);
end;

procedure TfrmMain.btnSetScaleClick(Sender: TObject);
var i: Integer;
begin
  enGrid.CalcGrid(jvedtEmin.Value, jvedtEmax.Value, jvedtPointsNum.Value, chkLogScale.Checked);
  mmoEnergies.Clear;
  for i:=0 to enGrid.FGridNum-1 do
    mmoEnergies.Lines.Add(FloatToStrF(enGrid.FEnergies[i], ffGeneral, 5, 3));
end;

procedure TfrmMain.mniLoadGridClick(Sender: TObject);
var i: Integer;
begin
  // load grid from file
  if (dlgOpen1.Execute) then
    enGrid.LoadFromFile(dlgOpen1.FileName);
  // show grid
  mmoEnergies.Clear;
  for i:=0 to enGrid.FGridNum-1 do
    mmoEnergies.Lines.Add(FloatToStrF(enGrid.FEnergies[i], ffGeneral, 5, 3));
end;

procedure TfrmMain.mniSaveGridClick(Sender: TObject);
begin
  dlgSave1.Filter := 'Energy grid (*.enx)|*.enx|All files (*.*)|*.*';
  dlgSave1.DefaultExt := 'enx';
  if dlgSave1.Execute then
    //enGrid.SaveInFile(dlgSave1.FileName);
    mmoEnergies.Lines.SaveToFile(dlgSave1.FileName);
end;

procedure TfrmMain.mniSaveMjuClick(Sender: TObject);
begin
  dlgSave1.Filter := 'Mju txt-files (*.txt)|*.txt|All files (*.*)|*.*';
  dlgSave1.DefaultExt := 'txt';
  if dlgSave1.Execute then
    mmoOutput.Lines.SaveToFile(dlgSave1.FileName);
end;

procedure TfrmMain.mniShowElInfoClick(Sender: TObject);
var S: string;
    Z: Integer;
    CurEl: TElement;
begin
  Z := jvedtElNum.Value;
  CurEl := chemTableMju.ElFromZ(Z);
  S := IntToStr(Z) + CurEl.ElSymb + ':' + #09 +CurEl.ElName + #09 + CurEl.ElNameEn
    + #09 + CurEl.ElNameRu + #13#10 + 'Ma= ' +FloatToStr(CurEl.Ma) + #13#10 + 'Rho= '
    + FloatToStr(CurEl.Rho);

  frmElementInfo := TfrmElementInfo.Create(Self);
  try
    frmElementInfo.mmoElInfo.Lines.Add(S);
    frmElementInfo.ShowModal();
  finally
    frmElementInfo.Free();
  end;
end;

procedure TfrmMain.edtElNameExit(Sender: TObject);
begin
  jvedtElNum.Value := chemTableMju.ElZFromName(edtElName.Text);
end;

procedure TfrmMain.jvedtElNumExit(Sender: TObject);
begin
  edtElName.Text := chemTableMju.ElNameFromZ(jvedtElNum.Value);
end;



end.
