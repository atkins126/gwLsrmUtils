program MuDat;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {frmMain},
  MjuUnit in 'MjuUnit.pas',
  UnitPreferences in 'UnitPreferences.pas' {frmPreferences},
  EnScaleUnit in 'EnScaleUnit.pas',
  UnitElInfo in 'UnitElInfo.pas' {frmElementInfo};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
