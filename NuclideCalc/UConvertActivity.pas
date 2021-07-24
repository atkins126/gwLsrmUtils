unit UConvertActivity;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  JvExStdCtrls, JvEdit, JvValidateEdit,
  LibUnit;

type
  TfrmConvert = class(TForm)
    lbl1: TLabel;
    jvedtAct1: TJvValidateEdit;
    jvedtAct2: TJvValidateEdit;
    lbl2: TLabel;
    btnConvert: TButton;
    cbbUnits1: TComboBox;
    cbbUnits2: TComboBox;
    lbl3: TLabel;
    cbbNuclides: TComboBox;
    procedure btnConvertClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FLibrary: TLibrary;
    function act_to_mass(act: Double): Double;
    function mass_to_act(mass: Double): Double;
    procedure setLibrary(const Value: TLibrary);
  public
    property nucLib: TLibrary read FLibrary write setLibrary;
  end;

var
  frmConvert: TfrmConvert;

implementation

{$R *.dfm}

function TfrmConvert.act_to_mass(act: Double): Double;
var nuc_string: string;
begin
  // get nuclide string
  nuc_string := cbbNuclides.Items[cbbNuclides.ItemIndex];

  // convert mass to activity
  Result := FLibrary.activityToMass(nuc_string, act);
end;

function TfrmConvert.mass_to_act(mass: Double): Double;
var nuc_string: string;
begin
  // get nuclide string
  nuc_string := cbbNuclides.Items[cbbNuclides.ItemIndex];

  // convert mass to activity
  Result := FLibrary.massToActivity(nuc_string, mass);
end;

procedure TfrmConvert.btnConvertClick(Sender: TObject);
var A1: Double;
begin
  A1 := jvedtAct1.Value;   // activity in 1-st units
  // convert to Bq
  case cbbUnits1.ItemIndex of
  0:;                 // Bq
  1: A1 := A1*1000;   // kBq
  2: A1 := A1*1e6;    // MBq
  3: A1 := A1*3.7e10; // Ci
  4: A1 := A1*3.7e7; // mCi
  5: A1 := A1*3.7e4; // uCi
  6: A1 := mass_to_act(A1); // g
  end;

  case cbbUnits2.ItemIndex of
  0:;                 // Bq
  1: A1 := A1/1000;   // kBq
  2: A1 := A1/1e6;    // MBq
  3: A1 := A1/3.7e10; // Ci
  4: A1 := A1/3.7e7; // mCi
  5: A1 := A1/3.7e4; // uCi
  6: A1 := act_to_mass(A1); // g
  end;
  jvedtAct2.Value := A1;
end;

procedure TfrmConvert.FormCreate(Sender: TObject);
begin
  FLibrary := nil;
end;

procedure TfrmConvert.setLibrary(const Value: TLibrary);
var Nuclides: TStringList;
begin
  FLibrary := Value;
  if (not Assigned(FLibrary)) then Exit;

  Nuclides := TStringList.Create();
  FLibrary.GetListOfNuclides(Nuclides);
  cbbNuclides.Items.AddStrings(Nuclides);

  if (cbbNuclides.Items.Count > 1) then
    cbbNuclides.ItemIndex := 0;
end;

end.
