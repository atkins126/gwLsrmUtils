unit AddDetEff;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, JvExStdCtrls, JvEdit, JvValidateEdit;

type
  TfrmAddDetEff = class(TForm)
    rg1: TRadioGroup;
    lbl1: TLabel;
    jvedtExpEff: TJvValidateEdit;
    btnOk: TButton;
    btnCancel: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAddDetEff: TfrmAddDetEff;

implementation

{$R *.dfm}

end.
