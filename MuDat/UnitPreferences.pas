unit UnitPreferences;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, MainUnit;

type
  TfrmPreferences = class(TForm)
    chkOutput: TCheckBox;
    rgEnUnits: TRadioGroup;
    btnOk: TButton;
    btnCancel: TButton;
    cbbMuType: TComboBox;
    lbl1: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPreferences: TfrmPreferences;

implementation

{$R *.dfm}

end.
