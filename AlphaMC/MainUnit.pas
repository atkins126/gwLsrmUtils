unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, IniFiles, Menus, ComCtrls, ExtCtrls, Gauges,
  JvEdit, JvExStdCtrls, JvValidateEdit,
  AlphaCalcThreadUnit, AlphaDetector;

const
  PROGRESS_POS = WM_USER+1;
  CALC_END = WM_USER+2;

type
  TfrmMain = class(TForm)
    lbl2: TLabel;
    lbl3: TLabel;
    lbl4: TLabel;
    lbl5: TLabel;
    lbl6: TLabel;
    lbl7: TLabel;
    jvedtDetDiam: TJvValidateEdit;
    jvedtSource1Diam: TJvValidateEdit;
    jvedtSource2Diam: TJvValidateEdit;
    jvedtSource1ToDetDist: TJvValidateEdit;
    jvedtSource2ToDetDist: TJvValidateEdit;
    jvedtEventsNum: TJvValidateEdit;
    jvedtEffRatio: TJvValidateEdit;
    btnStart: TButton;
    lbl8: TLabel;
    lbl9: TLabel;
    jvedtEff1: TJvValidateEdit;
    jvedtEff2: TJvValidateEdit;
    mm1: TMainMenu;
    mniTools: TMenuItem;
    mniAddExpEff: TMenuItem;
    lbl10: TLabel;
    jvedtDetEff: TJvValidateEdit;
    jvedtRealEff1: TJvValidateEdit;
    jvedtRealEff2: TJvValidateEdit;
    lbl11: TLabel;
    lbl12: TLabel;
    lbl13: TLabel;
    jvedtEff1Error: TJvValidateEdit;
    jvedtEff2Error: TJvValidateEdit;
    jvedtRatioError: TJvValidateEdit;
    btnRecalcReal: TButton;
    mniCalcDiamFromArea: TMenuItem;
    mniFile: TMenuItem;
    mniOpenDet: TMenuItem;
    mniSaveDet: TMenuItem;
    mniOptions: TMenuItem;
    mniDebug: TMenuItem;
    dlgOpen1: TOpenDialog;
    dlgSave1: TSaveDialog;
    mniAbout: TMenuItem;
    mniRecDet: TMenuItem;
    jvedtDetB: TJvValidateEdit;
    pb1: TProgressBar;
    pb2: TProgressBar;
    grpDet: TGroupBox;
    lblDetB: TLabel;
    lblDetA: TLabel;
    grpSource: TGroupBox;
    grpResults: TGroupBox;
    gaug1: TGauge;
    lblSunkXY: TLabel;
    lblDetSunkZ: TLabel;
    jvedtDetSunkZ: TJvValidateEdit;
    jvedtDetSunkXY: TJvValidateEdit;
    lbl1: TLabel;
    jvedtSource1RadShift: TJvValidateEdit;
    jvedtSource2RadShift: TJvValidateEdit;
    lbl14: TLabel;
    mniAddParams: TMenuItem;
    mniRectangleSource: TMenuItem;
    procedure btnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure mniAddExpEffClick(Sender: TObject);
    procedure btnRecalcRealClick(Sender: TObject);
    procedure mniCalcDiamFromAreaClick(Sender: TObject);
    procedure mniOpenDetClick(Sender: TObject);
    procedure mniSaveDetClick(Sender: TObject);
    procedure mniAboutClick(Sender: TObject);
    procedure mniRecDetClick(Sender: TObject);
    procedure mniAddParamsClick(Sender: TObject);
  private
    procedure RecalcExpEff;
    procedure SetProgressPos(var Msg: TMessage); message PROGRESS_POS;
    procedure OutputResults(var Msg: TMessage); message CALC_END; // вывод результатов
    procedure ReadDetFromForm();
    procedure ShowAdditionalParams();
    procedure setDetectorToForm();
  public
    Eff1, Eff2, Eff1err, Eff2err: Double; // эффективность 1 и 2, их погрешности
  end;

var
  frmMain: TfrmMain;
  appPath: string;
  Thread1, Thread2: TAlphaCalcThread;  // потоки для расчёта эффективности для каждого источника
  CurDet: TDet;

resourcestring
  frmCaption = 'Alpha Monte-Carlo';

implementation

uses AddDetEff, DiamFromAreaDlgUnit, MC_Calc, gwFileUtils;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
var IniFile: TIniFile;
    fileName: string;
begin
  if DecimalSeparator <> '.' then DecimalSeparator := '.';
  appPath := ExtractFilePath(ParamStr(0));
  CurDet := TDet.Create();
  IniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'gwAlphaMC.ini');
  try
    CurDet.isSquare := IniFile.ReadBool('DIMENSIONS', 'IsDetSquare', False);
    CurDet.name := IniFile.ReadString('DIMENSIONS', 'DetName', '');
    CurDet.detDiam := IniFile.ReadFloat('DIMENSIONS', 'DetDiam', 0);
    if (CurDet.isSquare) then
    begin
      CurDet.a := CurDet.detDiam;
      CurDet.b := IniFile.ReadFloat('DIMENSIONS', 'DetB', 0);
    end;
    CurDet.detSelfEfficiency := IniFile.ReadFloat('DIMENSIONS', 'DetEff', 1);
    CurDet.sunkXY := IniFile.ReadFloat('DIMENSIONS', 'DetEdge', 0);
    CurDet.sunkZ := IniFile.ReadFloat('DIMENSIONS', 'SunkZ', 0);

    // source and geometry
    jvedtSource1Diam.Value := IniFile.ReadFloat('DIMENSIONS', 'S1Diam', 0);
    jvedtSource2Diam.Value := IniFile.ReadFloat('DIMENSIONS', 'S2Diam', 0);
    jvedtSource1ToDetDist.Value := IniFile.ReadFloat('DIMENSIONS', 'S1DetDist', 0);
    jvedtSource2ToDetDist.Value := IniFile.ReadFloat('DIMENSIONS', 'S2DetDist', 0);
    jvedtEventsNum.Value := IniFile.ReadInteger('DIMENSIONS', 'N', 100);

    // Parameters
    mniAddParams.Checked := IniFile.ReadBool('PARAMETERS', 'Add_Params', False);
  finally
    IniFile.Free();
  end;
  // set det params to form
  setDetectorToForm();  
  ShowAdditionalParams();
  
  if (ParamStr(1) <> '') then begin
    fileName := addPathToFileName(ParamStr(1), appPath);
    if (FileExists(fileName)) then begin
      if (Assigned(CurDet)) then FreeAndNil(CurDet);
      CurDet := TDet.openDetector(fileName);
      setDetectorToForm();
    end;
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var IniFile: TIniFile;
begin
  ReadDetFromForm();
  IniFile := TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'gwAlphaMC.ini');
  try
    // detector
    IniFile.WriteString('DIMENSIONS', 'DetName', CurDet.name);
    IniFile.WriteBool('DIMENSIONS', 'IsDetSquare', CurDet.isSquare);
    IniFile.WriteFloat('DIMENSIONS', 'DetDiam', CurDet.detDiam); // запись размеров в ini
    if (CurDet.isSquare) then
      IniFile.WriteFloat('DIMENSIONS', 'DetB', CurDet.b);
    IniFile.WriteFloat('DIMENSIONS', 'DetEff', CurDet.detSelfEfficiency);
    IniFile.WriteFloat('DIMENSIONS', 'DetEdge', CurDet.sunkXY);
    IniFile.WriteFloat('DIMENSIONS', 'SunkZ', CurDet.sunkZ);
    // source and geometry
    IniFile.WriteFloat('DIMENSIONS', 'S1Diam', jvedtSource1Diam.Value);
    IniFile.WriteFloat('DIMENSIONS', 'S2Diam', jvedtSource2Diam.Value);
    IniFile.WriteFloat('DIMENSIONS', 'S1DetDist', jvedtSource1ToDetDist.Value);
    IniFile.WriteFloat('DIMENSIONS', 'S2DetDist', jvedtSource2ToDetDist.Value);
    IniFile.WriteInteger('DIMENSIONS', 'N', jvedtEventsNum.Value);
    // Parameters
    IniFile.WriteBool('PARAMETERS', 'Add_Params', mniAddParams.Checked);
  except
    ShowMessage('Can''t write settings to ini file');
    IniFile.Free;
    FreeAndNil(CurDet);
    Application.Terminate();
  end;
end;

procedure TfrmMain.btnStartClick(Sender: TObject);
var Ds1, Ds2, R1, R2, RS1, RS2: Double; //Eff1, Eff2
    N: Int64;
begin
  // считывание значений
  ReadDetFromForm();
  Ds1 := jvedtSource1Diam.Value;
  Ds2 := jvedtSource2Diam.Value;
  R1 := jvedtSource1ToDetDist.Value;
  R2 := jvedtSource2ToDetDist.Value;
  RS1 := jvedtSource1RadShift.Value;
  RS2 := jvedtSource2RadShift.Value;
  N := Round(jvedtEventsNum.Value * 1000);
  jvedtEff1.Value := 0;
  jvedtEff2.Value := 0;
  jvedtRealEff1.Value := 0;
  jvedtRealEff2.Value := 0;
  jvedtEff1Error.Value := 0;
  jvedtEff2Error.Value := 0;
  pb1.Position := 0;
  pb2.Position := 0;
  frmMain.Repaint();
  btnStart.Enabled := False;

  // расчёт эффективностей
  Thread1 := TAlphaCalcThread.Create(true);
  Thread2 := TAlphaCalcThread.Create(true);
  Thread1.FreeOnTerminate := True;
  Thread2.FreeOnTerminate := True;

  Thread1.SetParams(CurDet, Ds1, R1, RS1, N, frmMain.Handle, mniDebug.Checked, 1);
  Thread1.Resume();
  Thread2.SetParams(CurDet, Ds2, R2, RS2, N, frmMain.Handle, mniDebug.Checked, 2);
  Thread2.Resume();
end;

procedure TfrmMain.OutputResults(var Msg: TMessage);
var DetEff: Double;
begin
  if (Msg.WParam = 1) then
  begin
    //jvedtEff1.Value:=Eff1;
    jvedtEff1.Text := FloatToStrF(Eff1, ffGeneral, 5, 1);
    jvedtEff1Error.Value := Eff1err;
  end
  else if (Msg.WParam = 2) then
  begin
    //jvedtEff2.Value:=Eff2;
    jvedtEff2.Text := FloatToStrF(Eff2, ffGeneral, 5, 1);
    jvedtEff2Error.Value := Eff2err;
  end;

  btnStart.Enabled := True;
  if (Eff1 <> 0) and (Eff2 <> 0) then
  begin
    DetEff := jvedtDetEff.Value;
    // вывод значений
    if (Eff1 > 0) and (Eff2 >= 0) then
    begin
      jvedtEffRatio.Value := Eff2 / Eff1;
      jvedtRatioError.Value := sqrt(Sqr(Eff1err) + sqr(Eff2err));
      if (DetEff > 0) then
      begin
        //jvedtRealEff1.Value:=DetEff*Eff1;
        //jvedtRealEff2.Value:=DetEff*Eff2;
        jvedtRealEff1.Text := FloatToStrF(DetEff * Eff1, ffGeneral, 4, 1);
        jvedtRealEff2.Text := FloatToStrF(DetEff * Eff2, ffGeneral, 4, 1);
      end;
      mniAddExpEff.Enabled := True;
    end
    else ShowMessage('Calc error!');
  end;
end;

procedure TfrmMain.mniAddExpEffClick(Sender: TObject);
var ExpEff, DetEff, CalcEff: Double;
    GeomNum: Byte;
begin
  frmAddDetEff:= TfrmAddDetEff.Create(Self);
  try
    if (frmAddDetEff.ShowModal = mrOk) then
    begin
      GeomNum := frmAddDetEff.rg1.ItemIndex;
      ExpEff := frmAddDetEff.jvedtExpEff.Value;
      if (GeomNum = 0) then
        CalcEff := jvedtEff1.Value
      else
        CalcEff := jvedtEff2.Value;
      if (CalcEff <> 0) then
        DetEff := ExpEff / CalcEff
      else
        DetEff := 1;
      jvedtDetEff.Value := DetEff;
      RecalcExpEff();
    end;
  finally
    frmAddDetEff.Free;
  end;
end;

procedure TfrmMain.RecalcExpEff;
var Eff1, Eff2, DetEff, RealEff1, RealEff2: Double;
begin
  Eff1 := jvedtEff1.Value;
  Eff2 := jvedtEff2.Value;
  DetEff := jvedtDetEff.Value;
  RealEff1 := Eff1 * DetEff;
  RealEff2 := Eff2 * DetEff;
  jvedtRealEff1.Value := RealEff1;
  jvedtRealEff2.Value := RealEff2;
end;

procedure TfrmMain.btnRecalcRealClick(Sender: TObject);
begin
  RecalcExpEff();
end;

procedure TfrmMain.mniCalcDiamFromAreaClick(Sender: TObject);
var area, D: Double;
    //strArea: string;
begin
  //if InputQuery('Calc diameter from area', 'Input area in mm2', strArea) then
  frmDiamFromAreaDlg := TfrmDiamFromAreaDlg.Create(Self);
  try
    if (frmDiamFromAreaDlg.ShowModal = mrOk) then
    begin
      area := frmDiamFromAreaDlg.jvedtArea.Value;
      if (area >= 0) then
      begin
        D := Sqrt(4 * area / Pi);
        case frmDiamFromAreaDlg.rg1.ItemIndex of
          0:  jvedtDetDiam.Value := D;
          1:  jvedtSource1Diam.Value := D;
          2:  jvedtSource2Diam.Value := D;
        end;
      end
      else ShowMessage('Wrong area');
    end;
  finally
    frmDiamFromAreaDlg.Free();
  end;

end;

procedure TfrmMain.mniOpenDetClick(Sender: TObject);
begin
  if (dlgOpen1.Execute) then
  begin
    if (Assigned(CurDet)) then FreeAndNil(CurDet);
    CurDet := TDet.openDetector(dlgOpen1.FileName);
    setDetectorToForm();
  end;
end;

procedure TfrmMain.mniSaveDetClick(Sender: TObject);
begin
  ReadDetFromForm();
  if (dlgSave1.Execute) then
    CurDet.saveDetector(dlgSave1.FileName);
end;

procedure TfrmMain.setDetectorToForm();
begin
  mniRecDet.Checked := CurDet.isSquare;
  mniRecDetClick(Self);
  if (CurDet.isSquare) then
  begin
    jvedtDetDiam.Value := CurDet.a;
    jvedtDetB.Value := CurDet.b;
  end
  else
    jvedtDetDiam.Value:=CurDet.detDiam;
  jvedtDetEff.Value := CurDet.detSelfEfficiency;
  jvedtDetSunkXY.Value := CurDet.sunkXY;
  jvedtDetSunkZ.Value := CurDet.sunkZ;
  frmMain.Caption := frmCaption + ': ' + CurDet.name;
end;

procedure TfrmMain.mniAboutClick(Sender: TObject);
begin
  ShowMessage('gwAlphaMC ver 1.0.0');
end;

procedure TfrmMain.mniRecDetClick(Sender: TObject);
begin
  CurDet.isSquare := mniRecDet.Checked;
  if mniRecDet.Checked then
  begin
    lblDetA.Caption := 'Detector length, mm:';
    lblDetB.Caption := 'Detector width, mm:';
    jvedtDetB.Visible := True;
  end
  else
  begin
    lblDetA.Caption := 'Detector diameter, mm:';
    lblDetB.Caption := '';
    jvedtDetB.Visible := False;
  end;
end;

procedure TfrmMain.SetProgressPos(var Msg: TMessage);
begin
  if Msg.WParam = 1 then
  begin {pb1.Position:=Msg.LParam;}
    gaug1.Progress := Msg.LParam
  end
  else if Msg.WParam = 2 then
    pb2.Position := Msg.LParam;
end;

procedure TfrmMain.ReadDetFromForm;
begin
  CurDet.detDiam := jvedtDetDiam.Value;
  CurDet.a := jvedtDetDiam.Value;
  CurDet.isSquare := mniRecDet.Checked;
  if (CurDet.isSquare) then
    CurDet.b := jvedtDetB.Value;
  CurDet.detSelfEfficiency := jvedtDetEff.Value;
  CurDet.sunkXY := jvedtDetSunkXY.Value;
  CurDet.sunkZ := jvedtDetSunkZ.Value;
end;

procedure TfrmMain.mniAddParamsClick(Sender: TObject);
begin
  ShowAdditionalParams();
end;

procedure TfrmMain.ShowAdditionalParams;
begin
  lbl1.Visible := mniAddParams.Checked;
  lbl14.Visible := mniAddParams.Checked;
  jvedtSource1RadShift.Visible := mniAddParams.Checked;
  jvedtSource2RadShift.Visible := mniAddParams.Checked;
  jvedtSource1RadShift.Value := 0;
  jvedtSource2RadShift.Value := 0;
end;



end.
