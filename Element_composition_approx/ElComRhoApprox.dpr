program ElComRhoApprox;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
