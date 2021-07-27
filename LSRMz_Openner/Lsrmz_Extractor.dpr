program Lsrmz_Extractor;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {frmMain},
  JvZlibMultiple in '..\..\..\..\..\Projects\Jvcl\jvcl-master\jvcl\run\JvZlibMultiple.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
