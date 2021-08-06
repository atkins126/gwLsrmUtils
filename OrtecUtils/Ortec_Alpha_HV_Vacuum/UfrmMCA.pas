unit UfrmMCA;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls,
  OrtecMCA;

type
  TfrmMCA = class(TFrame)
    btnRenew: TButton;
    pnlOver: TPanel;
    pnlOn: TPanel;
    edtCurrent: TEdit;
    edtHV: TEdit;
    edtHVTarget: TEdit;
    btnOff: TButton;
    btnOn: TButton;
    cbbDevice: TComboBox;
    lbl8: TLabel;
    lbl7: TLabel;
    lbl6: TLabel;
    lbl5: TLabel;
    lbl4: TLabel;
    lbl3: TLabel;
    lbl1: TLabel;
    lbl2: TLabel;
    lbl9: TLabel;
    lbl10: TLabel;
    edtAlias: TEdit;
    pnl1: TPanel;
  private
    { Private declarations }
  public
    procedure selectDeviceFromList(const devName: string);
    function ConnectToADC(var Anal: TOrtecMCA): Boolean;
  end;

implementation

{$R *.dfm}

uses MCBAPI32, UMCBILib_TLB;

{ TfrmMCA }

function TfrmMCA.ConnectToADC(var Anal: TOrtecMCA): Boolean;
var dwID: DWORD;
    ConfigItem: PMIOConfigItem;
    S: string;
begin
  MIOStartup();
  dwID := DWORD(cbbDevice.Items.Objects[cbbDevice.ItemIndex]); // считывание ID
  ConfigItem := AllocMem(SizeOf(TMIOConfigItem)); // выделить память под указатель на MIOConfigItem
  try
    ConfigItem^.nLength := SizeOf(TMIOConfigItem);
    Result := MIOGetConfigItem(dwID, '', ConfigItem);
    // передать строку с параметрами конфигурации для коннекта: имя, номер MCB, id
    S := Format('%s |%d %d %s', [ConfigItem^.szName, ConfigItem^.nMCB,
      ConfigItem^.nDevSeg, ConfigItem^.szHost]);

    if (Result) then
      Anal := TOrtecMCA.Create(S);
  finally
    FreeMem(ConfigItem);
    MIOCleanup();
  end;
end;

procedure TfrmMCA.selectDeviceFromList(const devName: string);
begin
  if (devName <> '') then
    cbbDevice.ItemIndex := cbbDevice.Items.IndexOf(devName);
end;

end.
