unit MergedNuclidesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, MainUnit, StdCtrls, JvExStdCtrls, JvEdit, JvValidateEdit;

type
  TfrmMergedNuclide = class(TForm)
    btnOk: TButton;
    btnCancel: TButton;
    cbbNucName: TComboBox;
    jvedtFactor: TJvValidateEdit;
    lbl1: TLabel;
    chkFactor: TCheckBox;
    procedure chkFactorClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMergedNuclide: TfrmMergedNuclide;

implementation

{$R *.dfm}

procedure TfrmMergedNuclide.chkFactorClick(Sender: TObject);
begin
  jvedtFactor.Enabled:=chkFactor.Checked;
  if jvedtFactor.Enabled=False then jvedtFactor.Value:=1;
end;

end.
