unit DiamFromAreaDlgUnit;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls, 
  Buttons, ExtCtrls, JvExStdCtrls, JvEdit, JvValidateEdit;

type
  TfrmDiamFromAreaDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    lbl1: TLabel;
    jvedtArea: TJvValidateEdit;
    rg1: TRadioGroup;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDiamFromAreaDlg: TfrmDiamFromAreaDlg;

implementation

{$R *.dfm}

end.
