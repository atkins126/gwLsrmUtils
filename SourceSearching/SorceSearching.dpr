program SorceSearching;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {frmMain},
  SourcePosUnit in 'SourcePosUnit.pas',
  UCoordinates in 'UCoordinates.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
