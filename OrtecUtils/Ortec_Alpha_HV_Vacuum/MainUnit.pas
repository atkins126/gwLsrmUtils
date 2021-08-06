unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, OleCtrls, ComCtrls, Menus, ExtCtrls, IniFiles, 
  MCBAPI32, UMCBILib_TLB,  // generated units from ActiveX
  OrtecMCA, UfrmMCA;

const
  INIFILE_NAME = 'OrtecAlphaStart.ini';
  mTorr2mBar = 1.3332e-3;

type
  TfrmMain = class(TForm)
    stat1: TStatusBar;
    pgc1: TPageControl;
    tsDet1: TTabSheet;
    tsDet2: TTabSheet;
    tmr1: TTimer;
    pnlPump: TPanel;
    chkVacMon: TCheckBox;
    lbl9: TLabel;
    lbl10: TLabel;
    edtVacuumTarget: TEdit;
    edtVacuum: TEdit;
    lbl11: TLabel;
    lbl12: TLabel;
    lbl13: TLabel;
    cbbPump: TComboBox;
    btnSet: TButton;
    frmc1: TfrmMCA;
    frmc2: TfrmMCA;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tmr1Timer(Sender: TObject);
    procedure cbbPumpChange(Sender: TObject);
    procedure pgc1Change(Sender: TObject);
    procedure btnSetClick(Sender: TObject);
    procedure frmc1btnRenewClick(Sender: TObject);
    procedure frmc1btnOnClick(Sender: TObject);
    procedure frmc1btnOffClick(Sender: TObject);
    procedure frmc1cbbDeviceChange(Sender: TObject);
    procedure frmc2cbbDeviceChange(Sender: TObject);
    procedure frmc2btnRenewClick(Sender: TObject);
    procedure frmc2btnOnClick(Sender: TObject);
    procedure frmc2btnOffClick(Sender: TObject);
    procedure frmc2edtAliasChange(Sender: TObject);
    procedure frmc1edtAliasChange(Sender: TObject);
  private
    { Private declarations }
    procedure EnableBtns();
  public
    { Public declarations }
    procedure FillDetList();
    function ConnectToADC(var Anal: TOrtecMCA; const frmc: TfrmMCA): Boolean;
    function GetParamsADC(const Anal: TOrtecMCA; var frmc: TfrmMCA; IsFull: Boolean = False): Boolean; // установка параметров ј÷ѕ на указанную вкладку
  end;

var
  frmMain: TfrmMain;
  Anal1, Anal2: TOrtecMCA;
  IsConnected1: Boolean = False;
  IsConnected2: Boolean = False;
  DevName1, DevName2, appPath, AliasName1, AliasName2: string;


resourcestring
  rsVersion = '1.0.2';

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var IniFile: TIniFile;
begin
  // read connections from ini
  DevName1 := '';
  DevName2 := '';
  appPath := ExtractFilePath(ParamStr(0));
  IniFile := TIniFile.Create(appPath + INIFILE_NAME);
  try
    DevName1 := IniFile.ReadString('DEVICE', 'DevName1', '');
    DevName2 := IniFile.ReadString('DEVICE', 'DevName2', '');
    AliasName1 := IniFile.ReadString('DEVICE', 'AliasName1', '');
    AliasName2 := IniFile.ReadString('DEVICE', 'AliasName2', '');
  finally
    IniFile.Free;
  end;

  // obtain device list
  pgc1.ActivePage := tsDet1;
  FillDetList();
  if (DevName1 <> '') then
    frmc1.cbbDevice.ItemIndex := frmc1.cbbDevice.Items.IndexOf(DevName1);
  if (frmc1.cbbDevice.ItemIndex >= 0) then
  begin
    IsConnected1 := ConnectToADC(Anal1, frmc1);
    if (IsConnected1) then GetParamsADC(Anal1, frmc1, true);
    tmr1.Enabled := IsConnected1;
  end;
  if (DevName2 <> '') then
    frmc2.cbbDevice.ItemIndex := frmc2.cbbDevice.Items.IndexOf(DevName2);
  if (frmc2.cbbDevice.ItemIndex >= 0) then
  begin
    IsConnected2 := ConnectToADC(Anal2, frmc2);
    if (IsConnected2) then GetParamsADC(Anal2, frmc2, true);
    tmr1.Enabled := IsConnected2;
  end;
  EnableBtns();
  frmMain.Caption := 'AlphaDUO start manager ver. ' + rsVersion;
  frmc1.edtAlias.Text := AliasName1;
  frmc2.edtAlias.Text := AliasName2;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var IniFile: TIniFile;
begin
  Anal1.Free;
  Anal2.Free;
  IniFile := TIniFile.Create(AppPath + INIFILE_NAME);
  try
    IniFile.WriteString('DEVICE', 'DevName1', DevName1);
    IniFile.WriteString('DEVICE', 'DevName2', DevName2);
    IniFile.WriteString('DEVICE', 'AliasName1', AliasName1);
    IniFile.WriteString('DEVICE', 'AliasName2', AliasName2);
  finally
    IniFile.Free;
  end;
end;

procedure TfrmMain.FillDetList;
var szName: array [0..MIODETNAMEMAX] of Char;
    dwID: DWORD;
    nDet: Integer;
    s: string;
begin
  MIOStartup;
  try
    nDet := 1;
    while MIOGetConfigName(nDet, '', MIODETNAMEMAX, szName, @dwID, nil) do // получаем по очереди название и id детектора
    begin
      s := Format('%.4d %s', [dwID, szName]);
      frmc1.cbbDevice.Items.AddObject(s, TObject(dwID));   // сохран€ем название и id в комбобокс
      frmc2.cbbDevice.Items.AddObject(s, TObject(dwID));   // сохран€ем название и id в комбобокс
      Inc(nDet);
    end;
  finally
    //MIOCleanup;
  end;
end;

function TfrmMain.ConnectToADC(var Anal: TOrtecMCA; const frmc: TfrmMCA): Boolean;
var dwID: DWORD;
    ConfigItem: PMIOConfigItem;
    S: string;
begin
  MIOStartup();
  dwID := DWORD(frmc.cbbDevice.Items.Objects[frmc.cbbDevice.ItemIndex]); // считывание ID
  ConfigItem := AllocMem(SizeOf(TMIOConfigItem)); // выделить пам€ть под указатель на MIOConfigItem
  try
    ConfigItem^.nLength := SizeOf(TMIOConfigItem);
    Result := MIOGetConfigItem(dwID, '', ConfigItem);
    // передать строку с параметрами конфигурации дл€ коннекта: им€, номер MCB, id
    S := Format('%s |%d %d %s', [ConfigItem^.szName, ConfigItem^.nMCB,
      ConfigItem^.nDevSeg, ConfigItem^.szHost]);

    if (Result) then
      Anal := TOrtecMCA.Create(S);
  finally
    FreeMem(ConfigItem);
    MIOCleanup();
  end;
end;

function TfrmMain.GetParamsADC(const Anal: TOrtecMCA; var frmc: TfrmMCA; IsFull: Boolean = False): Boolean;
var S: string;
    V_mode: Integer;
    Vacuum_Mode_List: TStringList;
begin
  // получение состо€ни€ высокого, уставки высокого, текущего значени€ высокого и тока утечки
  Result := Anal.getHVState();
  if (IsFull) then
    Result := Result and (Anal.GetTargetHV);
  Result := Result and (Anal.GetActualHV);
  Result := Result and (Anal.GetHVCurrent);
  Result := Result and (Anal.GetVacuum);
  if (IsFull) then
    Result := Result and (Anal.GetVacuumTarget);

  // obtain and set vacuum modes
  if (IsFull) then
  begin
    cbbPump.Items.Clear;
    Vacuum_Mode_List := TStringList.Create;
    Anal.getVacuumModeList(Vacuum_Mode_List);
    cbbPump.Items.Assign(Vacuum_Mode_List);
    FreeAndNil(Vacuum_Mode_List);
    // получение текущего режима вакуумировани€, режим может хранитьс€ как в виде слова, так и ввиде номера
    S := Anal.getVacuumMode(V_mode);
    if (V_mode > -1) then
      cbbPump.ItemIndex := V_mode
    else if (S <> '') then
      cbbPump.ItemIndex := cbbPump.Items.IndexOf(S);
  end;

  // выставление этих значений на форму
  if (Result) then
  begin
    if (IsFull) then
      frmc.edtHVTarget.Text := IntToStr(Anal.HVTarget div 10);
    frmc.edtHV.Text := IntToStr(Anal.HV div 10);
    frmc.edtCurrent.Text := IntToStr(Anal.HVCurrent);
    if Anal.isHVOn then begin
      frmc.pnlON.Caption := 'On';
      frmc.pnlON.Font.Color := clBlue;
    end
    else begin
      frmc.pnlON.Caption := 'Off';
      frmc.pnlON.Font.Color := clBlack;
    end;
    if Anal.isHVOverload then
      frmc.pnlOver.Caption := 'Overload'
    else
      frmc.pnlOver.Caption := '';
    if (IsFull) then
      edtVacuumTarget.Text := IntToStr(Anal.vacuumTarg);
    edtVacuum.Text := IntToStr(Anal.vacuum);
    edtVacuum.Hint := FloatToStrF(Anal.vacuum * mTorr2mBar, ffGeneral, 4, 3) + ' мбар';
  end;
end;

procedure TfrmMain.tmr1Timer(Sender: TObject);
begin
  if IsConnected1 then
  begin
    try
      IsConnected1 := GetParamsADC(Anal1, frmc1, False);
    except
      IsConnected1 := False;
    end;
  end;
  if IsConnected2 then
  begin
    try
      IsConnected1 := GetParamsADC(Anal2, frmc2, False);
    except
      IsConnected2 := False;
    end;
  end;
end;

procedure TfrmMain.frmc1btnRenewClick(Sender: TObject);
begin
  if (IsConnected1) then GetParamsADC(Anal1, frmc1, true)
  else if (frmc1.cbbDevice.ItemIndex >= 0) then
  begin
    DevName1 := frmc1.cbbDevice.Items[frmc1.cbbDevice.ItemIndex];
    IsConnected1 := ConnectToADC(Anal1, frmc1);
    EnableBtns();
    if (IsConnected1) then GetParamsADC(Anal1, frmc1, true);
    tmr1.Enabled := IsConnected1;
  end;
end;

procedure TfrmMain.frmc1btnOnClick(Sender: TObject);
begin
  if (IsConnected1) then Anal1.SetHVState(True);
end;

procedure TfrmMain.frmc1btnOffClick(Sender: TObject);
begin
  if (IsConnected1) then Anal1.SetHVState(False);
end;

procedure TfrmMain.frmc1cbbDeviceChange(Sender: TObject);
begin
  if (frmc1.cbbDevice.ItemIndex >= 0) then
  begin
    DevName1 := frmc1.cbbDevice.Items[frmc1.cbbDevice.ItemIndex];
    IsConnected1 := ConnectToADC(Anal1, frmc1);
    EnableBtns();
    if (IsConnected1) then GetParamsADC(Anal1, frmc1, true);
    tmr1.Enabled := IsConnected1;
  end;
end;

procedure TfrmMain.frmc2cbbDeviceChange(Sender: TObject);
begin
  if (frmc2.cbbDevice.ItemIndex >= 0) then
  begin
    DevName2 := frmc2.cbbDevice.Items[frmc2.cbbDevice.ItemIndex];
    IsConnected2 := ConnectToADC(Anal2, frmc2);
    EnableBtns();
    if (IsConnected2) then GetParamsADC(Anal2, frmc2, true);
    tmr1.Enabled := IsConnected2;
  end;
end;

procedure TfrmMain.frmc2btnRenewClick(Sender: TObject);
begin
  if IsConnected2 then GetParamsADC(Anal2, frmc2, true)
  else if (frmc2.cbbDevice.ItemIndex >= 0) then
  begin
    DevName2 := frmc2.cbbDevice.Items[frmc2.cbbDevice.ItemIndex];
    IsConnected2 := ConnectToADC(Anal2, frmc2);
    EnableBtns();
    if (IsConnected2) then GetParamsADC(Anal2, frmc2, true);
    tmr1.Enabled := IsConnected2;
  end;
end;

procedure TfrmMain.frmc2btnOnClick(Sender: TObject);
begin
  if (IsConnected2) then Anal2.SetHVState(True);
end;

procedure TfrmMain.frmc2btnOffClick(Sender: TObject);
begin
  if (IsConnected2) then Anal2.SetHVState(False);
end;

procedure TfrmMain.cbbPumpChange(Sender: TObject);
begin
  if (IsConnected1) and (cbbPump.ItemIndex >= 0) then
    Anal1.SetVacuumMode(cbbPump.ItemIndex)
  else if (IsConnected2) and (cbbPump.ItemIndex >= 0) then
    Anal2.SetVacuumMode(cbbPump.ItemIndex);
end;

procedure TfrmMain.EnableBtns();
begin
  // состо€ние подключени€
  if (pgc1.ActivePageIndex=0) then
  begin
    if IsConnected1 then
      stat1.Panels[0].Text := 'Connected'
    else
      stat1.Panels[0].Text := 'Disconnected'
  end
  else
  begin
    if IsConnected2 then
      stat1.Panels[0].Text := 'Connected'
    else
      stat1.Panels[0].Text := 'Disconnected'
  end;
  // Tract 1
  frmc1.btnOn.Enabled := IsConnected1;
  frmc1.btnOff.Enabled := IsConnected1;
  frmc1.edtHVTarget.Enabled := IsConnected1;
  frmc1.edtHV.Enabled := IsConnected1;
  frmc1.edtCurrent.Enabled := IsConnected1;
  // Tract 2
  frmc2.btnOn.Enabled := IsConnected2;
  frmc2.btnOff.Enabled := IsConnected2;
  frmc2.edtHVTarget.Enabled := IsConnected2;
  frmc2.edtHV.Enabled := IsConnected2;
  frmc2.edtCurrent.Enabled := IsConnected2;
  // Vacuum
  btnSet.Enabled := IsConnected1 or IsConnected2;
end;

procedure TfrmMain.pgc1Change(Sender: TObject);
begin
  EnableBtns();
end;

procedure TfrmMain.btnSetClick(Sender: TObject);
var Pressure: Integer;
begin
  Pressure := 19990;
  TryStrToInt(edtVacuumTarget.Text, Pressure);
  Anal1.SetVacuumTarget(Pressure);
end;


procedure TfrmMain.frmc2edtAliasChange(Sender: TObject);
begin
  AliasName2 := frmc2.edtAlias.Text;
end;

procedure TfrmMain.frmc1edtAliasChange(Sender: TObject);
begin
  AliasName1 := frmc1.edtAlias.Text;
end;

end.
