unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, IniFiles, Menus, JvJCLUtils, OrtecMCA;

const
  INIFILE_NAME = 'OrtecHVOn.ini';
  LOW_HV = 40;
  ORT_VERSION = '0.1.2 beta';


type
  TfrmMain = class(TForm)
    lbl1: TLabel;
    cbbDevice: TComboBox;
    btnOn: TButton;
    btnOff: TButton;
    lbl2: TLabel;
    lbl3: TLabel;
    edtTarget: TEdit;
    edtActual: TEdit;
    lbl4: TLabel;
    lbl5: TLabel;
    grp1: TGroupBox;
    cbbShutdown: TComboBox;
    pnlON: TPanel;
    pnlOverload: TPanel;
    tmr1: TTimer;
    btnRenew: TButton;
    lblPolarity: TLabel;
    pm1: TPopupMenu;
    mniAbout_pm: TMenuItem;
    lbl6: TLabel;
    lbl7: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure btnOnClick(Sender: TObject);
    procedure btnOffClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure cbbDeviceChange(Sender: TObject);
    procedure btnRenewClick(Sender: TObject);
    procedure mniAbout_pmClick(Sender: TObject);
  private
    { Private declarations }
    procedure FillDetList();
    function ConnectToADC(): Boolean;
    function GetParamsADC2Frm(const Anal: TOrtecMCA; isFull: Boolean = False): Boolean;
    procedure EnableBtns();
  public

  end;

var
  frmMain: TfrmMain;
  Anal: TOrtecMCA;
  IsConnected: Boolean = False;
  DevName, AppPath: string;
  IsCMD: Boolean = False;   // запущено ли приложении из коммандной строки
  Backgr_HV: Integer;  // значение высокого в выключенном состоянии

implementation

uses
  MCBAPI32;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var IniFile: TIniFile;
begin
  DevName := '';
  AppPath := ExtractFilePath(ParamStr(0));
  IniFile := TIniFile.Create(AppPath + INIFILE_NAME);
  try
    DevName := IniFile.ReadString('DEVICE', 'DevName', '');
  finally
    IniFile.Free;
  end;
  FillDetList();
  if (DevName <> '') then
    cbbDevice.ItemIndex := cbbDevice.Items.IndexOf(DevName);
  if (cbbDevice.ItemIndex >= 0) then
  begin
    IsConnected := ConnectToADC;
    EnableBtns();
    if (IsConnected) then
      GetParamsADC2Frm(Anal, True);
    tmr1.Enabled := IsConnected;
    if (ParamCount > 0) then
      if SameText(ParamStr(1), '-S') then
      begin
        IsCMD := True;
        if (ParamCount > 1) then
          Backgr_HV := StrToIntDef(ParamStr(2), LOW_HV)
        else
          Backgr_HV := LOW_HV;
        Anal.FConn.Comm('DISABLE_HV');
      end;
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var IniFile: TIniFile;
begin
  Anal.Free();
  IniFile := TIniFile.Create(AppPath + INIFILE_NAME);
  try
    IniFile.WriteString('DEVICE', 'DevName', DevName);
  finally
    IniFile.Free();
  end;
end;

function TfrmMain.ConnectToADC(): Boolean;
var dwID: DWORD;
    ConfigItem: PMIOConfigItem;
    S: string;
    //res: Boolean;
    //szName: array [0..MIODETNAMEMAX] of Char;
begin
  MIOStartup();
  dwID := DWORD(cbbDevice.Items.Objects[cbbDevice.ItemIndex]); // считывание ID
  ConfigItem := AllocMem(SizeOf(TMIOConfigItem));   // выделить память под указатель на MIOConfigItem
  try
    ConfigItem^.nLength := SizeOf(TMIOConfigItem);
    Result := MIOGetConfigItem(dwID, '', ConfigItem);
    // передать строку с параметрами конфигурации для коннекта: имя, номер MCB, id
    S := Format('%s |%d %d %s', [ConfigItem^.szName, ConfigItem^.nMCB, ConfigItem^.nDevSeg,
       ConfigItem^.szHost]);
    if Result then Anal := TOrtecMCA.Create(S);
  finally
    FreeMem(ConfigItem);
    MIOCleanup;
  end;
end;

procedure TfrmMain.FillDetList;
var szName: array [0..MIODETNAMEMAX] of Char;
    dwID: DWORD;
    nDet: Integer;
    s: string;
begin
  MIOStartup();
  try
    nDet := 1;
    while MIOGetConfigName(nDet, '', MIODETNAMEMAX, szName, @dwID, nil) do // получаем по очереди название и id детектора
    begin
      s := Format('%.4d %s', [dwID, szName]);
      cbbDevice.Items.AddObject(s, TObject(dwID));   // сохраняем название и id в комбобокс
      s := Format('DetN=%d, detID=%d, detName=%s', [nDet, dwID, szName]);
      Inc(nDet);
    end;
    {if cbbDevice.Items.Count>0 then
    begin
      cbbDevice.ItemIndex:=0;
      ConnectToADC;
      IsConnected:=True;
      tmr1.Enabled:=True;
    end;}
  finally
    //MIOCleanup;
  end;
end;

function TfrmMain.GetParamsADC2Frm(const Anal: TOrtecMCA; isFull: Boolean = False): Boolean;
var HV_down_list: TStringList;
begin
  // получение состояния высокого, текущего значения высокого и уставок
  Result := Anal.getHVState();
  if (isFull) then
    Result := Result and (Anal.GetTargetHV);
  Result := Result and (Anal.GetActualHV);

  if (IsFull) then begin
    // Получение методов shutdown'a
    cbbShutdown.Items.Clear;
    HV_down_list := TStringList.Create;
    Anal.getHVShutdownList(HV_down_list);
    cbbShutdown.Items.Assign(HV_down_list);
    FreeAndNil(HV_down_list);
    // set current shutdown method on form
    cbbShutdown.ItemIndex := cbbShutdown.Items.IndexOf(Anal.getHVSHutdownMethod());
  end;

  // выставление этих значений на форму
  edtActual.Text := IntToStr(Anal.HV);
  if Anal.isHVOn then begin
    pnlON.Caption := 'On';
    pnlON.Font.Color := clBlue;
  end
  else begin
    pnlON.Caption := 'Off';
    pnlON.Font.Color := clBlack;
  end;
  if Anal.isHVOverload then
    pnlOverload.Caption := 'Overload'
  else
    pnlOverload.Caption := '';

  if (IsFull) then begin
    edtTarget.Text := IntToStr(Anal.HVTarget);
    if Anal.isHVPos then
      lblPolarity.Caption := 'Positive'
    else
      lblPolarity.Caption := 'Negative';
  end;

  // если в режиме снятия высокго и выключения, то завершить приложение
  if (IsCMD and (Anal.HV <= Backgr_HV)) then
    frmMain.Close;
end;

procedure TfrmMain.tmr1Timer(Sender: TObject);
begin
  if IsConnected then
  begin
    GetParamsADC2Frm(Anal);
  end;
end;

procedure TfrmMain.btnOnClick(Sender: TObject);
begin
  if IsConnected then
  begin
    Anal.FConn.Comm('ENABLE_HV');
  end;
end;

procedure TfrmMain.btnOffClick(Sender: TObject);
begin
  if IsConnected then
  begin
    Anal.FConn.Comm('DISABLE_HV');
  end;
end;

procedure TfrmMain.cbbDeviceChange(Sender: TObject);
begin
  if (cbbDevice.ItemIndex >= 0) then
  begin
    DevName := cbbDevice.Items[cbbDevice.ItemIndex];
    IsConnected := ConnectToADC;
    EnableBtns;
    GetParamsADC2Frm(Anal, True);
    tmr1.Enabled := IsConnected;
  end;
end;

procedure TfrmMain.btnRenewClick(Sender: TObject);
begin
  if IsConnected then
    GetParamsADC2Frm(Anal, True)
  else if (cbbDevice.ItemIndex >= 0) then
  begin
    DevName := cbbDevice.Items[cbbDevice.ItemIndex];
    IsConnected := ConnectToADC();
    EnableBtns();
    GetParamsADC2Frm(Anal, True);
    tmr1.Enabled := IsConnected;
  end;
end;

procedure TfrmMain.mniAbout_pmClick(Sender: TObject);
begin
  ShowMessage('OrtecHvOn -- программа для поднятия высокого напряжения на спектрометрах' +
    ' Ortec типа DSPec. Version ' + ORT_VERSION);
end;

procedure TfrmMain.EnableBtns;
begin
  btnOn.Enabled := IsConnected;
  btnOff.Enabled := IsConnected;
  cbbShutdown.Enabled := IsConnected;
end;

end.
