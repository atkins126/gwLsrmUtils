unit MjuUnit;

interface

uses
  SysUtils, JvJCLUtils, Classes, Math, Interpolation;

type
  TEnergyUnits = (euMeV, eukeV); // units for energy

  TMjuSingle = record
    E,                  // Energy, MeV
    mu: Double;         // mu     
  end;

  TMju = record
    E,                 // Energy, MeV
    CohScat,           // Scattering coherent               (1)
    IncohScat,         // Scat. Incoh                       (2)
    PhotAbs,           // Photoelectric absorption          (3)
    PairInNuc,         // Pair production in nuclear field  (4)
    PairInEl,          // Pair production in electron field (5)
    TotalWithCoh,      // Total with coherent scat          (6)
    TotalWOCoh: Double; // Total without coherent. scat     (0)
  end;

  TElement = class
    Z: Byte;                            // ��������� �����
    ElSymb: string;                     // ������ ��������
    ElName, ElNameEn, ElNameRu: string; // ����� ��������: ���������, ����������, �������
    Rho: Double;                        // ��������� ��������� �������� � �/��3
    Ma: Double;                         // �������� (�������) ����� ��������
    Md: Double;                         // �������� ���� � ����������
    Fmju: array of TMju;
    private
      FGridCounts: Integer;               // ����� ����� � mju
      procedure SetLengthes(aN: Integer);
    public
      function mu(E: Double; AbsorbType: Integer = 0): Double; // ���������� ������� ��� ��������� ��
      function calcAbsorbtion(aMu, aThick: Double): Double;
      property GridCounts: Integer read FGridCounts write SetLengthes;
  end;
  //pElement = ^TElement;

  TElementBer = record
    Z: Byte;                             // ��������� �����
    Ma: Double;                          // �������� (�������) ����� ��������
    FGridCountsNP,                       // ����� ����� � mju ��� ����������� ��� � ���� ����
    FGridCountsEP,                       // ����� ����� � mju ��� ����������� ��� � ���� ���������
    FGridCountsCS,                       // ����� ����� � mju ��� ������������ ���������
    FGridCountsCm,                       // ����� ����� � mju ��� ��������
    FGridCountsPh,                       // ����� ����� � mju ��� �����������
    FGridCountsTotal: Integer;           // ����� ����� � ������� mju
    FmjuNP,                              // ������ �� ��� ����������� ��� � ���� ����
    FmjuEP,                              // ������ �� ��� ����������� ��� � ���� ���������
    FmjuCS,                              // ������ �� ��� ������������ ���������
    FmjuCm,                              // ������ �� ��� ��������
    FmjuPh,                              // ������ �� ��� �����������
    FmjuTotal: array of TMjuSingle;      // ������ ������ ��
  end;

  TChemTableMju = class
    FElements: array of TElement;
    FElBerCount: Integer;
    FElementsBer: array of TElementBer;
  private
    FElCount: Integer;
    function AddMjuElementFromTxt(dirname, filename: string): Boolean; // ��������� Mju ��� ������ �������� �� ���������� �����
    procedure ZElNameFromFileName(FileName: string; var z: Byte; var ElSymb: string); // ������� ����� ����� � Z � ��� ��������
    function StrToMju(S: string; out mju: TMju): Boolean; // ������� ��������� ������ � ������ ��� mju
    function NumFromZ(Z: Byte): Integer; // ���������� �������� ������� � ����, -1 - ��� ������ ��������
    function NumFromName(ElSymb: string): Integer; // ���������� ����� �������� � ���� �� ����� ��������
    function StrToElProp(S: string; var CurEl: TElement): Integer; // �������������� ������ � �������
    procedure incElementCount(); // set new elements count
    procedure decElementCount(); // decrease elements count
  public
    constructor LoadMjusFromTxtFiles(DBDirName: string); // ��������� � ������ �������� mju �� ��, ���������� ����� ����������� ���������
    function MjuFromEnergy(ElNum: Byte; E: Double; AbsorbType: Integer = 0): Double; overload; // ���������� mju ��� �������� ������� ��� ����������� �������. AbsorbType - ��� ��, 0 - Total without coherent. scat, ...
    function MjuFromEnergy(ElSymb: string; E_MeV: Double; AbsorbType: Integer = 0): Double; overload; // ���������� mju ��� �������� ������� ��� ����������� �������
    function EnergyFromMu(ElNum: Byte; mu: Double; AbsorbType: Integer = 0): Double; // ���������� ������� ��� ��������� ��
    function LoadElPropFromTxt(filename: string): Boolean; // ��������� �� ����� ���� � ������ ���������, �� ���������
    function ElFromZ(Z: Byte): TElement; // ���������� �������, ���� ��� Z
    function ElFromSymb(Name: string): TElement; // ���������� �������, ���� ��� ������
    function LoadMassesFromBin(FileName: string): Boolean; // ��������� ������� ����� �� Masses.bin ���������
    function LoadAttenuatFromBin(FileName: string): Boolean; // ��������� �� �� ATTENUAT.bin ���������
    function SaveAttenuatToBin(Filename: string): Boolean; // ��������� �� � ATTENUAT.bin
    function ElNameFromZ(ElNum: Byte): string; // ���������� ��� �������� �� ��� Z
    function ElZFromName(ElName: string): Byte; // ���������� Z �������� �� ��� �����
    property elCount: Integer read FElCount;
  end;

  TMaterial = class  // ��������
  private
    FName: string;  // �������� ���������
    FRho: Double;  // ��������� ��������� � �/��3
    FElNum: Integer;  // ���-�� ��� ��������� � ����������
  public
    FMDs: array of Double;
    FElements: array of TElement;  // �������� � ��������� ������ � ����������
    Fmju: array of TMju;
    property Name: string read FName write FName;
    property Rho: Double read FRho write FRho;
    procedure ObtainMju(AChemTable: TChemTableMju);
  end;

  TMaterials = class  // ���� ����������
    FMatNum: Integer;  // ���-�� ���������� � ����
    FMaterials: array of TMaterial;  // ������ ����������
  private
    function StrToMat(S: string; var CurMat: TMaterial): Integer; // �������������� � ������ � ��������
    function StrToEl(S: string; var CurEl: TElement): Integer; // �������������� � ������ � �������
  public
    function LoadMaterialsFromTxt(FileName: string): Boolean;  // �������� ���������� �� ��������� ����
    function MaterialAsString(CurMat: TMaterial): string; // ������ ����� ��������� � ������
    function StringAsMaterial(S: string): TMaterial; // ����������� ����� ��������� �� ������
  end;

implementation

uses Matrix;

{ TChemTableMju }

function TChemTableMju.ElFromSymb(Name: string): TElement;
begin
  if NumFromName(Name) >= 0 then
    Result := FElements[NumFromName(Name)]
  else
    Result := nil;
end;

function TChemTableMju.ElFromZ(Z: Byte): TElement;
begin
  if NumFromZ(Z) >= 0 then
    Result := FElements[NumFromZ(Z)]
  else
    Result := nil;
end;

function TChemTableMju.ElNameFromZ(ElNum: Byte): string;
var i: Integer;
begin
  i := NumFromZ(ElNum);
  if (i >= 0) then
    Result := FElements[i].ElSymb
  else
    Result := '';
end;

function TChemTableMju.ElZFromName(ElName: string): Byte;
var i: Integer;
begin
  i := NumFromName(ElName);
  if (i >= 0) then
    Result := FElements[i].Z
  else
    Result := 0;
end;

function TChemTableMju.EnergyFromMu(ElNum: Byte; mu: Double; AbsorbType: Integer = 0): Double;
var i: Integer;
    E1, E2: Double;
    Mju: array of Double;
begin
  i := NumFromZ(ElNum);
  if i >= 0 then
    ElNum := i
  else
  begin
    // raise exception,  �������� �� � Result, � � ��������� ���������� ��� ������
    Result := -1;
    Exit;
  end;

  SetLength(Mju, FElements[ElNum].GridCounts);
  for i:=0 to Length(Mju)-1 do
  begin
    case AbsorbType of
    0: Mju[i] := FElements[ElNum].Fmju[i].TotalWOCoh;
    1: Mju[i] := FElements[ElNum].Fmju[i].CohScat;
    2: Mju[i] := FElements[ElNum].Fmju[i].IncohScat;
    3: Mju[i] := FElements[ElNum].Fmju[i].PhotAbs;
    4: Mju[i] := FElements[ElNum].Fmju[i].PairInNuc;
    5: Mju[i] := FElements[ElNum].Fmju[i].PairInEl;
    6: Mju[i] := FElements[ElNum].Fmju[i].TotalWithCoh;
    end;
  end;

  // �������� ������
  if mu > Mju[0] then
  begin
    Result := FElements[ElNum].Fmju[0].E;
    Exit;
  end;
  if mu < Mju[FElements[ElNum].GridCounts-1] then
  begin
    Result := FElements[ElNum].Fmju[FElements[ElNum].GridCounts-1].E;
    Exit;
  end;
  // ����� ��������� ������� �������
  i:=0;
  while Mju[i]>mu do
    Inc(i);

  if i=0 then
  begin
    Result:=FElements[ElNum].Fmju[0].E;
    Exit;
  end;
  // ������ �� ���������� ������ ����� ����� 2-�� �������: mx=m1 + (Ex-E1)/(E2-E1)*(m2-m1)
  E1 := FElements[ElNum].Fmju[i-1].E;
  E2 := FElements[ElNum].Fmju[i].E;
  {TODO -o gw: �������� ����� ������������ ���������}
  Result:=E1 + (E2-E1)*(mu-Mju[i-1])/(Mju[i]-Mju[i-1]);
end;

function TChemTableMju.LoadAttenuatFromBin(FileName: string): Boolean;
const
  BERZ = 100; // ����� ��������� � ��������� � ����� (����������� �������� ����������)
  NNP = 55; // ����� �� ��� ����������� ��� � ���� ����
  NEP = 51; // ����� �� ��� ����������� ��� � ���� ���������
  NCS = 80; // ����� �� ��� ������������� ��������� � ��������
var F: TFileStream;
    TempArrEn, TempArrMu, TempArrMu2: array of Double;
    i,j: Integer;
begin
  Result:=False;
  if not FileExists(FileName) then Exit;

  // ��������� ���� ��������
  FElBerCount := BERZ;
  SetLength(FElementsBer,FElBerCount);
  for i:=0 to FElBerCount-1 do
  begin
    FElementsBer[i].Z:=i;
    FElementsBer[i].FGridCountsNP:=NNP;
    FElementsBer[i].FGridCountsEP:=NEP;
    FElementsBer[i].FGridCountsCS:=NCS;
    FElementsBer[i].FGridCountsCm:=NCS;
    SetLength(FElementsBer[i].FmjuNP,NNP);
    SetLength(FElementsBer[i].FmjuEP,NEP);
    SetLength(FElementsBer[i].FmjuCS,NCS);
    SetLength(FElementsBer[i].FmjuCm,NCS);
  end;
  
  F:=TFileStream.Create(FileName,fmOpenRead,fmsharecompat);
  // ��������� ������� ��� ����������� ��� � ���� ����
  SetLength(TempArrEn,NNP);
  SetLength(TempArrMu,NNP);
  F.ReadBuffer(TempArrEn[0],NNP*SizeOf(Double)); // ��������� ����� ������� ��� ����������� ��� � ���� ����
  for i:=0 to FElBerCount-1 do
  begin
    F.ReadBuffer(TempArrMu[0],NNP*SizeOf(Double)); // ��������� ����� �� ��� ����������� ��� � ���� ���� ��� �������� i
    for j:=0 to NNP-1 do
    begin
      FElementsBer[i].FmjuNP[j].E:=TempArrEn[j];
      FElementsBer[i].FmjuNP[j].mu:=TempArrMu[j];
    end;
  end;

  // ��������� ������� ��� ����������� ��� � ���� ���������
  SetLength(TempArrEn,NEP);
  SetLength(TempArrMu,NEP);
  F.ReadBuffer(TempArrEn[0],NEP*SizeOf(Double)); // ��������� ����� �������
  for i:=0 to FElBerCount-1 do
  begin
    F.ReadBuffer(TempArrMu[0],NEP*SizeOf(Double)); // ��������� ����� ��
    for j:=0 to NEP-1 do
    begin
      FElementsBer[i].FmjuEP[j].E:=TempArrEn[j];
      FElementsBer[i].FmjuEP[j].mu:=TempArrMu[j];
    end;
  end;

  // ��������� ������� ��� ������������� ��������� � ��������
  SetLength(TempArrEn,NCS);
  SetLength(TempArrMu,NCS);
  SetLength(TempArrMu2,NCS);
  F.ReadBuffer(TempArrEn[0],NCS*SizeOf(Double)); // ��������� ����� �������
  for i:=0 to FElBerCount-1 do
  begin
    F.ReadBuffer(TempArrMu[0],NCS*SizeOf(Double)); // ��������� ����� ��
    F.ReadBuffer(TempArrMu2[0],NCS*SizeOf(Double)); // ��������� ����� ��
    for j:=0 to NCS-1 do
    begin
      FElementsBer[i].FmjuCS[j].E:=TempArrEn[j];
      FElementsBer[i].FmjuCS[j].mu:=TempArrMu[j];
      FElementsBer[i].FmjuCm[j].E:=TempArrEn[j];
      FElementsBer[i].FmjuCm[j].mu:=TempArrMu2[j];
    end;
  end;

  // ��������� ������� ��� ����������� � ������� ��

  for i:=0 to FElBerCount-1 do
  begin
    F.ReadBuffer(FElementsBer[i].FGridCountsPh,1); // ��������� ����� ����� � �����
    FElementsBer[i].FGridCountsTotal:=FElementsBer[i].FGridCountsPh;
    SetLength(FElementsBer[i].FmjuPh,FElementsBer[i].FGridCountsPh);
    SetLength(FElementsBer[i].FmjuTotal,FElementsBer[i].FGridCountsTotal);
    SetLength(TempArrEn,FElementsBer[i].FGridCountsPh);
    SetLength(TempArrMu,FElementsBer[i].FGridCountsPh);
    SetLength(TempArrMu2,FElementsBer[i].FGridCountsTotal);

    F.ReadBuffer(TempArrEn[0],FElementsBer[i].FGridCountsPh*SizeOf(Double)); // ��������� ����� �������
    F.ReadBuffer(TempArrMu[0],FElementsBer[i].FGridCountsPh*SizeOf(Double)); // ��������� ����� ��
    F.ReadBuffer(TempArrMu2[0],FElementsBer[i].FGridCountsPh*SizeOf(Double)); // ��������� ����� ��
    for j:=0 to FElementsBer[i].FGridCountsPh-1 do
    begin
      FElementsBer[i].FmjuPh[j].E:=TempArrEn[j];
      FElementsBer[i].FmjuPh[j].mu:=TempArrMu[j];
      FElementsBer[i].FmjuTotal[j].E:=TempArrEn[j];
      FElementsBer[i].FmjuTotal[j].mu:=TempArrMu2[j];
    end;
  end;

  TempArrEn:=nil;
  TempArrMu:=nil;
  TempArrMu2:=nil;
  F.Free;
end;

function TChemTableMju.LoadElPropFromTxt(filename: string): Boolean;
var F: TextFile;
    S: string;
    CurEl: TElement;
    i: Integer;
begin
  Result := False;
  // open file
  AssignFile(F, filename);
  {$I-}Reset(F) {$I+};
  if IOResult<>0 then
  begin
    //ShowMessage('Error openning file '+FileName);
    exit;
  end;

  // ������ ����� � ���������� ���� � TElements
  Readln(F, S); // ������ ���������, ���� �� ������������
  CurEl := TElement.Create();
  While not Eof(F) do
  begin
    Readln(F, S);
    StrToElProp(S, CurEl);
    if (CurEl.Z <= 0) then Exit; // error reading
    
    i := NumFromZ(CurEl.Z);
    if (i >= 0) then
    begin
      FElements[i].ElName := CurEl.ElName;
      FElements[i].ElNameEn := CurEl.ElNameEn;
      FElements[i].ElNameRu := CurEl.ElNameRu;
      FElements[i].Rho := CurEl.Rho;
      FElements[i].Ma := CurEl.Ma;
    end;
  end;

  Result := True;
  CurEl.Free();
  CloseFile(F);
end;

function TChemTableMju.LoadMassesFromBin(FileName: string): Boolean;
var F: TFileStream;
    A: array of Single;
    i: Integer;
begin
  Result:=False;
  if not FileExists(FileName) then Exit;

  F:=TFileStream.Create(FileName,fmOpenRead,fmsharecompat);
  FElBerCount:=F.Size div SizeOf(Single);
  SetLength(FElementsBer,FElBerCount);
  SetLength(A,FElBerCount);

  F.ReadBuffer(A[0],FElBerCount*SizeOf(Single));
  
  for i:=0 to FElBerCount-1 do
  begin
    FElementsBer[i].Z:=i;
    FElementsBer[i].Ma:=A[i];
  end;
  A:=nil;
  F.Free;
  Result:=True;
end;

function TChemTableMju.AddMjuElementFromTxt(dirname, filename: string): Boolean;
var F: TextFile;
    curEl: Integer; // number of current load element
    S: string;
begin
  Result := False;
  // increase elements number in array
  incElementCount();
  curEl := FElCount - 1;

  // open file with element info
  AssignFile(F, dirname + filename);
  {$I-}Reset(F) {$I+};
  if (IOResult <> 0) then
  begin
    //ShowMessage('Error openning file '+FileName);
    exit;
  end;

  // transform file name o element number and name
  FElements[curEl] := TElement.Create();
  FElements[curEl].GridCounts := 0;
  ZElNameFromFileName(filename, FElements[curEl].Z, FElements[curEl].ElSymb);

  // reading file to mu element
  While not Eof(F) do
  begin
    Readln(F, S);
    FElements[curEl].GridCounts := FElements[curEl].GridCounts + 1;
    StrToMju(S, FElements[curEl].FMju[FElements[curEl].GridCounts-1]);
  end;

  Result := True;
  CloseFile(F);
end;

constructor TChemTableMju.LoadMjusFromTxtFiles(DBDirName: string);
var FSearcRec: TSearchRec;
begin
  if FindFirst(DBDirName + '*.*', faAnyFile, FSearcRec) = 0 then
  begin
    if (FSearcRec.Name <> '.') and (FSearcRec.Name <> '..') then
    begin
      if not (AddMjuElementFromTxt(DBDirName, FSearcRec.Name)) then
        decElementCount();
    end;
  end;
  while FindNext(FSearcRec) = 0 do
  begin
    if (FSearcRec.Name<>'.') and (FSearcRec.Name<>'..') then
    begin
      if not (AddMjuElementFromTxt(DBDirName, FSearcRec.Name)) then
        decElementCount();
    end;
  end;
end;

function TChemTableMju.MjuFromEnergy(ElNum: Byte; E: Double; AbsorbType: Integer = 0): Double;
var i: Integer;
begin
  i := NumFromZ(ElNum);
  if i >= 0 then
    Result := FElements[ElNum].mu(E, AbsorbType)
  else begin
    // raise exception, �������� �� � Result, � � ��������� ���������� ��� ������
    Result := -1;
    Exit;
  end;
end;

function TChemTableMju.MjuFromEnergy(ElSymb: string; E_MeV: Double; AbsorbType: Integer = 0): Double;
var ElNum: Integer;
begin
  ElNum := NumFromName(ElSymb);
  if (ElNum >= 0) then
    Result := FElements[ElNum].mu(E_MeV, AbsorbType)
  else
  begin
    // raise exception,  �������� �� � Result, � � ��������� ���������� ��� ������
    Result := -1;
    Exit;
  end;
end;

function TChemTableMju.NumFromName(ElSymb: string): Integer;
var i: Integer;
begin
  Result:=-1;
  for i:=0 to FElCount-1 do
    if SameText(ElSymb,FElements[i].ElSymb) then
    begin
      Result:=i;
      Exit;
    end;
end;

function TChemTableMju.NumFromZ(Z: Byte): Integer;
var i: Integer;
begin
  Result := Z-1; // trying to guess
  if (Result < 0) or (Result >= FElCount) or  // if we don't guess using dumb search
    (FElements[Result].Z <> Z) then
  begin
    Result := -1;
    for i:=0 to FElCount-1 do
      if (Z = FElements[i].Z) then
      begin
        Result := i;
        Exit;
      end;
  end;
end;

function TChemTableMju.SaveAttenuatToBin(Filename: string): Boolean;
const
  BERZ = 100; // ����� ��������� � ��������� � ����� (����������� �������� ����������)
  NNP = 55; // ����� �� ��� ����������� ��� � ���� ����
  NEP = 51; // ����� �� ��� ����������� ��� � ���� ���������
  NCS = 80; // ����� �� ��� ������������� ��������� � ��������
var F: TFileStream;
    TempArrEn, TempArrMu, TempArrMu2: array of Double;
    i,j: Integer;
begin
  Result:=False;

  F:=TFileStream.Create(FileName,fmCreate,fmshareDenyWrite);

  // ���������� ������� ��� ����������� ��� � ���� ����
  SetLength(TempArrEn,NNP);
  SetLength(TempArrMu,NNP);
  for j:=0 to NNP-1 do TempArrEn[j]:=FElementsBer[0].FmjuNP[j].E;
  F.WriteBuffer(TempArrEn[0],NNP*SizeOf(Double)); // ���������� ����� �������
  for i:=0 to FElBerCount-1 do
  begin
    for j:=0 to NNP-1 do
      TempArrMu[j]:=FElementsBer[i].FmjuNP[j].mu; //if FElementsBer[i].FmjuNP[j].mu>0 then TempArrMu[j]:=Log10(FElementsBer[i].FmjuNP[j].mu);//
    F.WriteBuffer(TempArrMu[0],NNP*SizeOf(Double)); // ���������� ����� ��
  end;

  // ���������� ������� ��� ����������� ��� � ���� ���������
  SetLength(TempArrEn,NEP);
  SetLength(TempArrMu,NEP);
  for j:=0 to NEP-1 do TempArrEn[j]:=FElementsBer[0].FmjuEP[j].E;
  F.WriteBuffer(TempArrEn[0],NEP*SizeOf(Double)); // ���������� ����� �������
  for i:=0 to FElBerCount-1 do
  begin
    for j:=0 to NEP-1 do TempArrMu[j]:=FElementsBer[i].FmjuEP[j].mu;
    F.WriteBuffer(TempArrMu[0],NEP*SizeOf(Double)); // ���������� ����� ��
  end;

  // ���������� ������� ��� ������������� ��������� � ��������
  SetLength(TempArrEn,NCS);
  SetLength(TempArrMu,NCS);
  SetLength(TempArrMu2,NCS);
  for j:=0 to NCS-1 do TempArrEn[j]:=FElementsBer[0].FmjuCS[j].E;
  F.WriteBuffer(TempArrEn[0],NCS*SizeOf(Double)); // ���������� ����� �������
  for i:=0 to FElBerCount-1 do 
  begin
    for j:=0 to NCS-1 do
    begin
      TempArrMu[j]:=FElementsBer[i].FmjuCS[j].mu;
      TempArrMu2[j]:=FElementsBer[i].FmjuCm[j].mu;
    end;
    F.WriteBuffer(TempArrMu[0],NCS*SizeOf(Double)); // ���������� ����� ��
    F.WriteBuffer(TempArrMu2[0],NCS*SizeOf(Double)); // ���������� ����� ��
  end;

  // ���������� ������� ��� ����������� � ������� ��
  for i:=0 to FElBerCount-1 do
  begin
    j:=Byte(FElementsBer[i].FGridCountsPh);
    F.WriteBuffer(j,1); // ���������� ����� ����� � �����
    SetLength(TempArrEn,FElementsBer[i].FGridCountsPh);
    SetLength(TempArrMu,FElementsBer[i].FGridCountsPh);
    SetLength(TempArrMu2,FElementsBer[i].FGridCountsTotal);

    for j:=0 to FElementsBer[i].FGridCountsPh-1 do
    begin
      TempArrEn[j]:=FElementsBer[i].FmjuPh[j].E;
      TempArrMu[j]:=FElementsBer[i].FmjuPh[j].mu;
      TempArrMu2[j]:=FElementsBer[i].FmjuTotal[j].mu;
    end;
    F.WriteBuffer(TempArrEn[0],FElementsBer[i].FGridCountsPh*SizeOf(Double)); // ��������� ����� �������
    F.WriteBuffer(TempArrMu[0],FElementsBer[i].FGridCountsPh*SizeOf(Double)); // ��������� ����� ��
    F.WriteBuffer(TempArrMu2[0],FElementsBer[i].FGridCountsPh*SizeOf(Double)); // ��������� ����� ��
  end;

  TempArrEn:=nil;
  TempArrMu:=nil;
  TempArrMu2:=nil;
  F.Free;
end;

function TChemTableMju.StrToElProp(S: string;
  var CurEl: TElement): Integer;
var i,WC: Integer; // ����� ������� � �����
    BR: Boolean;
    CurSep: Char;
begin
  CurSep:=DecimalSeparator;
  DecimalSeparator:='.';

  WC:=WordCount(S,[#09]);
  if WC>0 then
  begin
    BR:=TryStrToInt(ExtractWord(1,S,[#09]),i);
    if BR then CurEl.Z:=Byte(i) else CurEl.Z:=0;
  end;
  if WC>1 then CurEl.ElSymb:=ExtractWord(2,S,[#09]) else CurEl.ElSymb:='';
  if WC>2 then CurEl.ElName:=ExtractWord(3,S,[#09]) else CurEl.ElName:='';
  if WC>3 then CurEl.ElNameEn:=ExtractWord(4,S,[#09]) else CurEl.ElNameEn:='';
  if WC>4 then CurEl.ElNameRu:=ExtractWord(5,S,[#09]) else CurEl.ElNameRu:='';
  if WC>5 then TryStrToFloat(ExtractWord(6,S,[#09]),CurEl.Rho) else CurEl.Rho:=0;
  if WC>6 then TryStrToFloat(ExtractWord(7,S,[#09]),CurEl.Ma) else CurEl.Ma:=0;

  Result:=WC;
  DecimalSeparator:=CurSep;
end;

function TChemTableMju.StrToMju(S: string; out mju: TMju): Boolean;
var CurDecSep: Char;
begin
  CurDecSep:=DecimalSeparator;
  DecimalSeparator:='.';

  if WordCount(S,[' '])<>8 then
  begin
    Result:=False;
    Exit;
  end
  else
    try
      mju.E:=StrToFloat(ExtractWord(1,S,[' ']));
      mju.CohScat:=StrToFloat(ExtractWord(2,S,[' ']));
      mju.IncohScat:=StrToFloat(ExtractWord(3,S,[' ']));
      mju.PhotAbs:=StrToFloat(ExtractWord(4,S,[' ']));
      mju.PairInNuc:=StrToFloat(ExtractWord(5,S,[' ']));
      mju.PairInEl:=StrToFloat(ExtractWord(6,S,[' ']));
      mju.TotalWithCoh:=StrToFloat(ExtractWord(7,S,[' ']));
      mju.TotalWOCoh:=StrToFloat(ExtractWord(8,S,[' ']));
    except
      // send exception
    end;
  DecimalSeparator:=CurDecSep;
  Result:=True;
end;

procedure TChemTableMju.ZElNameFromFileName(FileName: string; var z: Byte;
  var ElSymb: string);
var i: Integer;
begin
  z:=0;
  ElSymb:='';
  if Length(FileName)<2 then Exit;
  if (FileName[1]<'0') or (FileName[1]>'9') then Exit;

  for i:=2 to Length(FileName) do
    if (FileName[i]>='A') and (FileName[i]<='z') then // ������� �������
    begin
      ElSymb:=Copy(FileName,i,Length(FileName)-i+1);
      z:=StrToInt(Copy(FileName,1,i-1));  // exception ��������
      exit;
    end
    else if (FileName[i]<'0') or (FileName[i]>'9') then
      Exit;
      // raise Exception
end;

procedure TChemTableMju.incElementCount();
begin
  Inc(FElCount);
  //TODO: constructor for nwe element
  SetLength(FElements, FElCount);
end;

procedure TChemTableMju.decElementCount;
begin
  if (FElCount > 0) then begin
    Dec(FElCount);
    FElements[FElCount].Free();
    SetLength(FElements, FElCount);
  end;
end;



{ TMaterials }
function TMaterials.LoadMaterialsFromTxt(FileName: string): Boolean;  // �������� ���������� �� ��������� ����
var F: TextFile;
    S: string;
    CurMat: TMaterial;
    CurEl: TElement;
    i: Integer;
begin
  Result:=False;
  FMatNum:=0;
  // �������� �����
  AssignFile(F, filename);
  {$I-}Reset(F) {$I+};
  if IOResult<>0 then
  begin
    //ShowMessage('Error openning file '+FileName);
    exit;
  end;

  // ������ ����� � ���������� ���� � TElements
  While not Eof(F) do
  begin
    Readln(F,S);
    if S[1]<>#09 then // ������ � ����������
      if StrToMat(S,CurMat)=1 then
      begin
        for i:=0 to CurMat.FElNum-1 do  // ������ ��������� � ��������� ������ �� ��
        begin
          Readln(F,S);
          if StrToEl(S,CurEl)=1 then CurMat.FElements[i]:=CurEl;
        end;
        Inc(FMatNum);
        SetLength(FMaterials,FMatNum);
        FMaterials[FMatNum-1]:=CurMat;
      end;
  end;
  Result:=True;
  CloseFile(F);
end;

function TMaterials.MaterialAsString(CurMat: TMaterial): string;
var i: Integer;
begin
  Result:=CurMat.FName+#09+FloatToStr(CurMat.FRho)+#09+IntToStr(CurMat.FElNum)+#09;
  for i:=0 to CurMat.FElNum-1 do Result:=Result+IntToStr(Curmat.FElements[i].Z)+','+FloatToStr(Curmat.FElements[i].Md)+','+FloatToStr(Curmat.FElements[i].Ma)+';';
end;

function TMaterials.StringAsMaterial(S: string): TMaterial;
var i: Integer;
    TempS, ElemS: string;
begin
  if WordCount(S,[#09]) < 4 then begin
    //Result := nil;
    Exit;
  end;
  Result.FName := ExtractWord(1,S,[#09]);
  Result.FRho := StrToFloat(ExtractWord(2, S, [#09]));
  Result.FElNum := StrToInt(ExtractWord(3, S, [#09]));
  SetLength(Result.FElements,Result.FElNum);
  TempS := ExtractWord(4, S, [#09]); // ����� ������ � ���������� � ��������� ������
  for i:=0 to Result.FElNum-1 do
  begin
    ElemS := ExtractWord(i+1, TempS, [';']); // ������ � ����� ��������� � ��� �������� �����
    Result.FElements[i].Z := StrToInt(ExtractWord(1, ElemS,[',']));
    Result.FElements[i].Md := StrToFloat(ExtractWord(2, ElemS,[',']));
    Result.FElements[i].Ma := StrToFloat(ExtractWord(3, ElemS,[',']));
  end;
end;

function TMaterials.StrToEl(S: string; var CurEl: TElement): Integer;
begin
  if WordCount(S,[#09])>=3 then
  try
    CurEl.Z:=Byte(StrToInt(ExtractWord(1, S,[#09])));
    CurEl.Md:=StrToFloat(ExtractWord(2, S,[#09]));
    CurEl.Ma:=StrToFloat(ExtractWord(3, S,[#09]));
    Result:=1;
  except
    Result:=-1;
  end
  else
    Result:=-1;
end;

function TMaterials.StrToMat(S: string; var CurMat: TMaterial): Integer;
begin
  if WordCount(S,[#09])>=3 then
  try
    CurMat.FName:=ExtractWord(1, S, [#09]);
    CurMat.FRho:=StrToFloat(ExtractWord(2, S,[#09]));
    CurMat.FElNum:=StrToInt(ExtractWord(3, S,[#09]));
    if CurMat.FElNum>0 then SetLength(CurMat.FElements,CurMat.FElNum);
    Result:=1;
  except
    CurMat.FElNum:=0;
    Result:=-1
  end
  else
  begin
    CurMat.FElNum:=0;
    Result:=-1
  end;

end;

{ TMaterial }

procedure TMaterial.ObtainMju(AChemTable: TChemTableMju);
var i, j: Integer;
begin
  if Length(FElements)=0 then Exit;

  SetLength(Fmju, Length(FElements[0].Fmju));
  for j:=0 to Length(Fmju)-1 do
  begin
    Fmju[j].E:=FElements[0].Fmju[j].E;
    Fmju[j].CohScat:=0;
    for i:=0 to Length(FElements)-1 do
    begin
      Fmju[j].CohScat:=Fmju[j].CohScat+FElements[i].Fmju[j].CohScat*FElements[i].Md;
    end;
  end;
end;

{ TElement }

function TElement.calcAbsorbtion(aMu, aThick: Double): Double;
begin
  Result := Exp(-aMu * Rho * aThick);
end;

function TElement.mu(E: Double; AbsorbType: Integer): Double;
var i: Integer;
    EnArr, Mju: TVector; // energy and mu grid
begin
  SetLength(EnArr, FGridCounts);
  SetLength(Mju, FGridCounts);
  for i:=0 to Length(Mju)-1 do
  begin
    EnArr[i] := Fmju[i].E;
    case AbsorbType of
    0: Mju[i] := Fmju[i].TotalWOCoh;
    1: Mju[i] := Fmju[i].CohScat;
    2: Mju[i] := Fmju[i].IncohScat;
    3: Mju[i] := Fmju[i].PhotAbs;
    4: Mju[i] := Fmju[i].PairInNuc;
    5: Mju[i] := Fmju[i].PairInEl;
    6: Mju[i] := Fmju[i].TotalWithCoh;
    end;
  end;

  // check boudaries
  if (E < Fmju[0].E) then
  begin
    Result := mju[0];
    Exit;
  end;
  if (E > Fmju[FGridCounts-1].E) then
  begin
    Result := Mju[FGridCounts-1];
    Exit;
  end;

  // �������� �� ����������� ��� -- ��� �� ����� ��������� ��������
  if (AbsorbType = 4) and (E <= 1.022) then
  begin
    Result := 0; // ��� ����������� ���
    Exit;
  end;

  if (AbsorbType = 5) and (E <= 2.044) then
  begin
    Result := 0; // ��� ����������� ���
    Exit;
  end;

  InterPolLog(3, EnArr, Mju, E, Result, lsXYBoth);
end;

procedure TElement.SetLengthes(aN: Integer);
begin
  FGridCounts := aN;
  if (FGridCounts < 0) then
    FGridCounts := 0;

  SetLength(Fmju, FGridCounts);
end;

end.
