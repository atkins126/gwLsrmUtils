unit UnitElInfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TfrmElementInfo = class(TForm)
    btnOk: TButton;
    mmoElInfo: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmElementInfo: TfrmElementInfo;

implementation

{$R *.dfm}

end.
