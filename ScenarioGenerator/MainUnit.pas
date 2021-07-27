unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Mask, ComCtrls, JvExMask, JvToolEdit,
  UTemplateGenerator;

type
  TfrmMain = class(TForm)
    mmoTemplate: TMemo;
    jvdiredtSpeDir: TJvDirectoryEdit;
    btnGenerate: TButton;
    btnSave: TButton;
    dlgSave: TSaveDialog;
    dlgOpen: TOpenDialog;
    jvfnedtTemplate: TJvFilenameEdit;
    lbl1: TLabel;
    lbl2: TLabel;
    edtMask: TEdit;
    lbl3: TLabel;
    btnClear: TButton;
    statInfo: TStatusBar;
    procedure btnGenerateClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure jvfnedtTemplateChange(Sender: TObject);
    procedure jvdiredtSpeDirChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnClearClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;
  AppPath, resultLscName, mask: string;
  scenarioGenerator: TTemplGenerator;

implementation

{$R *.dfm}

uses IniFiles;

procedure TfrmMain.FormCreate(Sender: TObject);
var IniFile: TIniFile;
begin
  AppPath := ExtractFilePath(ParamStr(0));
  IniFile := TIniFile.Create(AppPath + 'ScenarioGenerator.ini');
  try
    jvfnedtTemplate.FileName := IniFile.ReadString('PATH', 'TemplateName', '');
    jvdiredtSpeDir.Directory := IniFile.ReadString('PATH', 'SpeDir', '');
    resultLscName := IniFile.ReadString('PATH', 'LscName', '');
    edtMask.Text := IniFile.ReadString('PREFERENCE', 'Mask', '*.spe');
  finally
    IniFile.Free;
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(AppPath + 'ScenarioGenerator.ini');
  try
    IniFile.WriteString('PATH', 'TemplateName', jvfnedtTemplate.FileName);
    IniFile.WriteString('PATH', 'SpeDir', jvdiredtSpeDir.Directory);
    IniFile.WriteString('PATH', 'LscName', resultLscName);
    IniFile.WriteString('PREFERENCE', 'Mask', edtMask.Text);
  finally
    IniFile.Free;
  end;
end;

procedure TfrmMain.btnGenerateClick(Sender: TObject);
begin
  if (FileExists(jvfnedtTemplate.FileName) and DirectoryExists(jvdiredtSpeDir.Directory)) then begin
    if (Assigned(scenarioGenerator)) then FreeAndNil(scenarioGenerator);

    scenarioGenerator := TTemplGenerator.start(jvfnedtTemplate.FileName, jvdiredtSpeDir.Directory + '\'
      , edtMask.Text);
    btnSave.Enabled := True;
    mmoTemplate.Lines.Clear;
    mmoTemplate.Lines.AddStrings(scenarioGenerator.ScenarioText);
    statInfo.Panels[0].Text := IntToStr(scenarioGenerator.spectrumCount);
  end;
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
begin
  dlgSave.FileName := resultLscName;
  if (dlgSave.Execute) then begin
    resultLscName := dlgSave.FileName;
    scenarioGenerator.saveScenarioToFile(dlgSave.FileName);
  end;
end;

procedure TfrmMain.jvfnedtTemplateChange(Sender: TObject);
begin
  if (FileExists(jvfnedtTemplate.FileName)) then
    jvfnedtTemplate.Font.Color := clBlack
  else
    jvfnedtTemplate.Font.Color := clRed;
end;

procedure TfrmMain.jvdiredtSpeDirChange(Sender: TObject);
begin
  if (DirectoryExists(jvdiredtSpeDir.Directory)) then
    jvdiredtSpeDir.Font.Color := clBlack
  else
    jvdiredtSpeDir.Font.Color := clRed;
end;

procedure TfrmMain.btnClearClick(Sender: TObject);
begin
  mmoTemplate.Clear();
  statInfo.Panels[0].Text := '';
  FreeAndNil(scenarioGenerator);
end;

end.
