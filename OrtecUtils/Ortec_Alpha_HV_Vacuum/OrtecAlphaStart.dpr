program OrtecAlphaStart;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {frmMain},
  UfrmMCA in 'UfrmMCA.pas' {frmMCA: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
