program NuclideCalc;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {MainForm},
  LibUnit in '..\Common\LibUnit.pas',
  UConvertActivity in 'UConvertActivity.pas' {frmConvert};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
