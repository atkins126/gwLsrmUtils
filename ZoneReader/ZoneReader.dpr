program ZoneReader;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {frmMain},
  RusClipboard in '..\Units\RusClipboard\RusClipboard.pas',
  UZones in 'UZones.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
