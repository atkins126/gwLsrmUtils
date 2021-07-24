program gwAlphaMC;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {frmMain},
  MC_Calc in 'MC_Calc.pas',
  AddDetEff in 'AddDetEff.pas' {frmAddDetEff},
  DiamFromAreaDlgUnit in 'DiamFromAreaDlgUnit.pas' {frmDiamFromAreaDlg},
  AlphaCalcThreadUnit in 'AlphaCalcThreadUnit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
