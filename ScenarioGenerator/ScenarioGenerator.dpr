program ScenarioGenerator;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {frmMain},
  UTemplateGenerator in 'UTemplateGenerator.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
