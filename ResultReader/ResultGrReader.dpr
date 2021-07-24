program ResultGrReader;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {frmMain},
  RusClipboard in '..\Units\RusClipboard\RusClipboard.pas',
  UPeakList in 'UPeakList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
