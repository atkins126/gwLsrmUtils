program EffCenterCalc;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {frmMain},
  EffC_Calcing in 'EffC_Calcing.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
