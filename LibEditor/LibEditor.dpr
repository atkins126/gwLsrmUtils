program LibEditor;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {frmMain},
  MergedNuclidesUnit in 'MergedNuclidesUnit.pas' {frmMergedNuclide},
  NMGammaConstFunction in 'NMGammaConstFunction.pas',
  UGammaConst in 'UGammaConst.pas' {frmGammaConst},
  LibUnit in '..\Common\LibUnit.pas',
  MetrologRound in '..\Common\MetrologRound.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
