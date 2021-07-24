unit Matrix;

interface

type
  // symple types
  TVector = array of Double;
  TMatrix = array of TVector; // A[i,j], where i - row number, j - col number
  TMatrix3 = array of TMatrix;
  PVector = ^TVector;

  TDoubleVector = class
  private
    Fsize: Integer;
    FCapacity: Integer;
    procedure setSize(aSize: integer);
    procedure setCapacity(aCapacity: Integer);
    function readElement(idx: Integer): Double;
    procedure setElement(idx: Integer; const aValue: Double);
    function readLastElement(): Double;
    procedure setLastElement(const aValue: Double);
  public
    x: array of Double;                   // call shrink before use this array
    constructor Create(aCapacity: Integer); overload;
    constructor CreateFromArray(arr: array of Double);
    constructor Copy(const aVector: TDoubleVector); overload;
    constructor Copy(const aArray: array of Double); overload;
    constructor CopyFromNum(const aVector: TDoubleVector; initNum: Integer; elemCount: Integer = 0);
    constructor CreateSized(aSize: Integer);
    destructor Destroy(); override;
    procedure push_back(aValue: Double);  // add value
    procedure append(aValue: Double);     // add value
    function pop_back(): Double;          // returns and deletes last value
    procedure insert(idx: Integer; aValue: Double);  // insert value into idx-position
    procedure clear();            // deletes all elements in vector
    function empty(): Boolean;    // returns True if vector is empty
    procedure multNumber(aNumber: Double);  // multiplates all values on number
    class function multiplyVectors(v1, v2: TDoubleVector): TDoubleVector;
    function indexFromValue(aValue: Double; threshold: Double = 0.0): Integer; // return -1 if value not in vector
    procedure shrink_to_fit();    // shrink vector size: free unused memmory
    procedure erase(idx: Integer);  // deletes idx element from vector
    procedure saveToFile(const filename: string);
    procedure normalize();
    function getElementsSum(): Double;
    property at[idx: Integer]: Double read readElement write setElement; default;  // access to element with range check
    property size: Integer read Fsize write setSize;                               // set used size, but not size of vector
    property capacity: Integer read FCapacity write setCapacity;                   // set real vector size
    property back: Double read readLastElement write setLastElement;               // get last element
  end;

  TIntegerVector = class
  private
    Fsize: Integer;
    FCapacity: Integer;
    procedure setSize(aSize: integer);
    procedure setCapacity(aCapacity: Integer);
    function readElement(idx: Integer): Integer;
    procedure setElement(idx: Integer; const aValue: Integer);
    function readLastElement(): Integer;
    procedure setLastElement(const aValue: Integer);
  public
    x: array of Integer;                   // call shrink before use this array
    constructor Create(aCapacity: Integer); overload;
    constructor CreateFromArray(arr: array of Integer);
    constructor Copy(const aVector: TIntegerVector); overload;
    constructor Copy(const aArray: array of Integer); overload;
    constructor CopyFromNum(const aVector: TIntegerVector; initNum: Integer; elemCount: Integer = 0);
    constructor CreateSized(aSize: Integer);
    destructor Destroy(); override;
    procedure push_back(aValue: Integer);  // add value
    procedure append(aValue: Integer);     // add value
    function pop_back(): Integer;          // returns and deletes last value
    procedure insert(idx: Integer; aValue: Integer);  // insert value into idx-position
    procedure clear();            // deletes all elements in vector
    function empty(): Boolean;    // returns True if vector is empty
    procedure multNumber(aNumber: Integer);  // multiplates all values on number
    class function multiplyVectors(v1, v2: TIntegerVector): TIntegerVector;
    function indexFromValue(aValue: Integer): Integer; // return -1 if value not in vector
    procedure shrink_to_fit();    // shrink vector size: free unused memmory
    procedure erase(idx: Integer);  // deletes idx element from vector
    procedure saveToFile(const filename: string);
    function getElementsSum(): Integer;
    property at[idx: Integer]: Integer read readElement write setElement; default;  // access to element with range check
    property size: Integer read Fsize write setSize;                               // set used size, but not size of vector
    property capacity: Integer read FCapacity write setCapacity;                   // set real vector size
    property back: Integer read readLastElement write setLastElement;               // get last element
  end;

  TDoubleMatrix = class
  private
    Fsize1, FSize2: Integer;
    FCapacity1, FCapacity2: Integer;
  public
    x: array of array of Double;
    constructor Create(aCapacity: Integer); overload;
    constructor Create(aCapacity1, aCapacity2: Integer); overload;
    constructor CreateSquareSize(aSize: Integer);
    constructor CreateSized(aSize1, aSize2: Integer);
    destructor Destroy(); override;
    procedure setSize(aSize1, aSize2: integer);
    procedure setCapacity(aCapacity1, aCapacity2: Integer);
    function getElement(row_idx, col_idx: Integer): Double;
    procedure setElement(row_idx, col_idx: Integer; const aValue: Double);
    procedure clear();            // deletes all elements in vector
    function empty(): Boolean;    // returns True if vector is empty
    procedure shrink_to_fit();    // shrink vector size: free unused memmory
    function getColumn(idx: Integer): TDoubleVector;
    function getRow(idx: Integer): TDoubleVector;
    //property at[idx, idx2: Integer]: Double read readElement write setElement; default;  // access to element with range check
    procedure saveToFile(const filename: string);
    property size1: Integer read Fsize1;                               // set used size, but not size of vector
    property size2: Integer read Fsize2;
    property capacity1: Integer read FCapacity1;                   // set real vector size
    property capacity2: Integer read FCapacity2;
  end;

implementation

uses
  Math, Classes, SysUtils;

{ TDoubleVector }

procedure TDoubleVector.push_back(aValue: Double);
begin
  if (FCapacity = 0) then
    setCapacity(8)
  else if (FCapacity = Fsize) then
    setCapacity(2*FSize);
  x[Fsize] := aValue;
  Fsize := Fsize + 1;
end;

procedure TDoubleVector.clear;
begin
  setSize(0);
end;

destructor TDoubleVector.Destroy;
begin
  x := nil;
  inherited;
end;

function TDoubleVector.empty: Boolean;
begin
  Result := (Fsize = 0);
end;

procedure TDoubleVector.insert(idx: Integer; aValue: Double);
var i: Integer;
begin
  if (idx > Fsize) then
    idx := Fsize;

  if (idx = Fsize) then begin
    push_back(aValue);
    Exit;
  end;

  setSize(Fsize + 1);
  for i:= Fsize-2 downto idx do
    x[i+1] := x[i];
  x[idx] := aValue;
end;

procedure TDoubleVector.setSize(aSize: integer);
begin
  if (aSize >= 0) then
    Fsize := aSize
  else
    Fsize := 0;

  if (Fsize > FCapacity) then
    setCapacity(Fsize);
end;

procedure TDoubleVector.setCapacity(aCapacity: Integer);
begin
  if (aCapacity >= 0) then
    FCapacity := aCapacity
  else
    FCapacity := 0;
  SetLength(x, FCapacity);
end;

function TDoubleVector.pop_back: Double;
begin
  Result := x[Fsize-1];
  setSize(Fsize - 1);
end;

procedure TDoubleVector.shrink_to_fit;
begin
  if (FCapacity > Fsize) then
    setCapacity(Fsize);
end;

function TDoubleVector.readElement(idx: Integer): Double;
begin
  if (idx >= 0) and (idx < FSize) then
    Result := x[idx]
  else
    raise EListError.CreateFmt('Index %d is out of bounds', [idx]);
end;

procedure TDoubleVector.setElement(idx: Integer; const aValue: Double);
begin
  if (idx >= 0) and (idx < FSize) then
    x[idx] := aValue
  else
    raise EListError.CreateFmt('Index %d is out of bounds', [idx]);
end;

function TDoubleVector.readLastElement: Double;
begin
  if (FSize > 0) then
    Result := x[Fsize-1]
  else
    raise EListError.Create('List is empty');
end;

procedure TDoubleVector.setLastElement(const aValue: Double);
begin
  if (FSize > 0) then
    x[Fsize-1] := aValue
  else
    raise EListError.Create('List is empty');
end;

procedure TDoubleVector.erase(idx: Integer);
var i: Integer;
begin
  if (idx < 0) and (idx >= FSize) then
    Exit;

  for i:=idx to Fsize-2 do
    x[i] := x[i+1];
  setSize(FSize-1);
end;

constructor TDoubleVector.CreateFromArray(arr: array of Double);
var i: Integer;
begin
  if (Length(arr) > 0) then begin
    setCapacity(Length(arr));
    for i:=0 to Length(arr)-1 do
      push_back(arr[i]);
  end;
end;

constructor TDoubleVector.Create(aCapacity: Integer);
begin
  setCapacity(aCapacity);
end;

procedure TDoubleVector.saveToFile(const filename: string);
var i: Integer;
    FStr: TStringList;
begin
  FStr := TStringList.Create();
  if (FileExists(filename)) then
    FStr.LoadFromFile(filename);
  for i:=0 to Fsize-1 do
    FStr.Add(FloatToStrF(x[i], ffGeneral, 4, 2));
  FStr.SaveToFile(filename);
  FStr.Free();
end;

procedure TDoubleVector.multNumber(aNumber: Double);
var i: Integer;
begin
  for i:= 0 to Fsize-1 do
    x[i] := x[i] * aNumber;
end;

constructor TDoubleVector.Copy(const aVector: TDoubleVector);
var i: Integer;
begin
  setCapacity(aVector.capacity);
  setSize(aVector.size);
  for i:=0 to Fsize-1 do
    x[i] := aVector.x[i];
end;

constructor TDoubleVector.Copy(const aArray: array of Double);
var i: Integer;
begin
  setSize(Length(aArray));
  for i:=0 to Fsize-1 do
    x[i] := aArray[i];
end;

procedure TDoubleVector.append(aValue: Double);
begin
  push_back(aValue);
end;

class function TDoubleVector.multiplyVectors(v1,
  v2: TDoubleVector): TDoubleVector;
var i: Integer;
begin
  if (v1 = nil) or (v2 = nil) or (v1.size = 0) or (v1.size <> v2.size) then begin
    Result := nil;
    Exit;
  end;

  Result := TDoubleVector.Create(v1.size);
  Result.setSize(v1.size);
  for i:=0 to v1.size-1 do
    Result.x[i] := v1[i] * v2[i];
end;

constructor TDoubleVector.CreateSized(aSize: Integer);
begin
  setSize(aSize);
end;

constructor TDoubleVector.CopyFromNum(const aVector: TDoubleVector; initNum: Integer;
 elemCount: Integer = 0);
var i, tSize: Integer;
begin
  if ((initNum < 0) or (initNum >= aVector.size)) then
    Exit;

  setCapacity(aVector.capacity);
  tSize := Min(elemCount, aVector.size - initNum);
  setSize(tSize);
  for i:=0 to FSize-1 do
    x[i] := aVector.x[i + initNum];
end;

function TDoubleVector.indexFromValue(aValue, threshold: Double): Integer;
var i: Integer;
begin
  Result := -1;
  if (threshold = 0) then begin
    for i:=0 to Fsize-1 do
      if (aValue = x[i]) then
      begin
        Result := i;
        Break;
      end;
  end
  else
    for i:=0 to Fsize-1 do
      if Abs(aValue - x[i])< threshold then
      begin
        Result := i;
        Break;
      end;
end;

procedure TDoubleVector.normalize;
var i: Integer;
    sumVals: Double; 
begin
  sumVals := 0;
  for i:=0 to Fsize-1 do
    sumVals := sumVals + x[i];

  if (sumVals <> 0) then
    for i:=0 to Fsize-1 do
      x[i] := x[i] / sumVals;
end;

function TDoubleVector.getElementsSum: Double;
var i: Integer;
begin
  Result := 0.0;
  for i:=0 to Fsize-1 do
    Result := Result + x[i];
end;

{ TIntegerVector }

procedure TIntegerVector.append(aValue: Integer);
begin
  push_back(aValue);
end;

procedure TIntegerVector.clear;
begin
  setSize(0);
end;

constructor TIntegerVector.Copy(const aVector: TIntegerVector);
var i: Integer;
begin
  setCapacity(aVector.capacity);
  setSize(aVector.size);
  for i:=0 to Fsize-1 do
    x[i] := aVector.x[i];
end;

constructor TIntegerVector.Copy(const aArray: array of Integer);
var i: Integer;
begin
  setSize(Length(aArray));
  for i:=0 to Fsize-1 do
    x[i] := aArray[i];
end;

constructor TIntegerVector.CopyFromNum(const aVector: TIntegerVector;
  initNum, elemCount: Integer);
var i, tSize: Integer;
begin
  if ((initNum < 0) or (initNum >= aVector.size)) then
    Exit;

  setCapacity(aVector.capacity);
  tSize := Min(elemCount, aVector.size - initNum);
  setSize(tSize);
  for i:=0 to FSize-1 do
    x[i] := aVector.x[i + initNum];
end;

constructor TIntegerVector.Create(aCapacity: Integer);
begin
  setCapacity(aCapacity);
end;

constructor TIntegerVector.CreateFromArray(arr: array of Integer);
var i: Integer;
begin
  if (Length(arr) > 0) then begin
    setCapacity(Length(arr));
    for i:=0 to Length(arr)-1 do
      push_back(arr[i]);
  end;
end;

constructor TIntegerVector.CreateSized(aSize: Integer);
begin
  setSize(aSize);
end;

destructor TIntegerVector.Destroy;
begin
  x := nil;
  inherited;
end;

function TIntegerVector.empty: Boolean;
begin
  Result := (Fsize = 0);
end;

procedure TIntegerVector.erase(idx: Integer);
var i: Integer;
begin
  if (idx < 0) and (idx >= FSize) then
    Exit;

  for i:=idx to Fsize-2 do
    x[i] := x[i+1];
  setSize(FSize-1);
end;

function TIntegerVector.getElementsSum: Integer;
var i: Integer;
begin
  Result := 0;
  for i:=0 to Fsize-1 do
    Result := Result + x[i];
end;

function TIntegerVector.indexFromValue(aValue: Integer): Integer;
var i: Integer;
begin
  Result := -1;
  for i:=0 to Fsize-1 do
    if (aValue = x[i]) then
    begin
      Result := i;
      Break;
    end;
end;

procedure TIntegerVector.insert(idx, aValue: Integer);
var i: Integer;
begin
  if (idx > Fsize) then
    idx := Fsize;

  if (idx = Fsize) then begin
    push_back(aValue);
    Exit;
  end;

  setSize(Fsize + 1);
  for i:= Fsize-2 downto idx do
    x[i+1] := x[i];
  x[idx] := aValue;
end;

class function TIntegerVector.multiplyVectors(v1,
  v2: TIntegerVector): TIntegerVector;
var i: Integer;
begin
  if (v1 = nil) or (v2 = nil) or (v1.size = 0) or (v1.size <> v2.size) then begin
    Result := nil;
    Exit;
  end;

  Result := TIntegerVector.Create(v1.size);
  Result.setSize(v1.size);
  for i:=0 to v1.size-1 do
    Result.x[i] := v1[i] * v2[i];
end;

procedure TIntegerVector.multNumber(aNumber: Integer);
var i: Integer;
begin
  for i:= 0 to Fsize-1 do
    x[i] := x[i] * aNumber;
end;

function TIntegerVector.pop_back: Integer;
begin
  Result := x[Fsize-1];
  setSize(Fsize - 1);
end;

procedure TIntegerVector.push_back(aValue: Integer);
begin
  if (FCapacity = 0) then
    setCapacity(8)
  else if (FCapacity = Fsize) then
    setCapacity(2*FSize);
  x[Fsize] := aValue;
  Fsize := Fsize + 1;
end;

function TIntegerVector.readElement(idx: Integer): Integer;
begin
  if (idx >= 0) and (idx < FSize) then
    Result := x[idx]
  else
    raise EListError.CreateFmt('Index %d is out of bounds', [idx]);
end;

function TIntegerVector.readLastElement: Integer;
begin
  if (FSize > 0) then
    Result := x[Fsize-1]
  else
    raise EListError.Create('List is empty');
end;

procedure TIntegerVector.saveToFile(const filename: string);
var i: Integer;
    FStr: TStringList;
begin
  FStr := TStringList.Create();
  if (FileExists(filename)) then
    FStr.LoadFromFile(filename);
  for i:=0 to Fsize-1 do
    FStr.Add(FloatToStrF(x[i], ffGeneral, 4, 2));
  FStr.SaveToFile(filename);
  FStr.Free();
end;

procedure TIntegerVector.setCapacity(aCapacity: Integer);
begin
  if (aCapacity >= 0) then
    FCapacity := aCapacity
  else
    FCapacity := 0;
  SetLength(x, FCapacity);
end;

procedure TIntegerVector.setElement(idx: Integer; const aValue: Integer);
begin
  if (idx >= 0) and (idx < FSize) then
    x[idx] := aValue
  else
    raise EListError.CreateFmt('Index %d is out of bounds', [idx]);
end;

procedure TIntegerVector.setLastElement(const aValue: Integer);
begin
  if (FSize > 0) then
    x[Fsize-1] := aValue
  else
    raise EListError.Create('List is empty');
end;

procedure TIntegerVector.setSize(aSize: integer);
begin
  if (aSize >= 0) then
    Fsize := aSize
  else
    Fsize := 0;

  if (Fsize > FCapacity) then
    setCapacity(Fsize);
end;

procedure TIntegerVector.shrink_to_fit;
begin
  if (FCapacity > Fsize) then
    setCapacity(Fsize);
end;

{ TDoubleMatrix }

procedure TDoubleMatrix.clear;
begin
   setSize(0, 0);
end;

constructor TDoubleMatrix.Create(aCapacity: Integer);
begin
  setCapacity(aCapacity, aCapacity);
end;

constructor TDoubleMatrix.Create(aCapacity1, aCapacity2: Integer);
begin
  setCapacity(aCapacity1, aCapacity2);
end;

constructor TDoubleMatrix.CreateSized(aSize1, aSize2: Integer);
begin
  setSize(aSize1, aSize2);
end;

constructor TDoubleMatrix.CreateSquareSize(aSize: Integer);
begin
  setSize(aSize, aSize);
end;

destructor TDoubleMatrix.Destroy;
begin
  x := nil;
  inherited;
end;

function TDoubleMatrix.empty: Boolean;
begin
  Result := (Fsize1 = 0) and (Fsize2 = 0);
end;

function TDoubleMatrix.getColumn(idx: Integer): TDoubleVector;
var i: Integer;
begin
  if (idx < 0) or (idx >= FSize2) then begin
    Result := nil;
    Exit;
  end;
  Result := TDoubleVector.Create(Fsize1);
  for i:=0 to FSize1-1 do
    Result.push_back(getElement(i, idx));
end;

function TDoubleMatrix.getElement(row_idx, col_idx: Integer): Double;
begin
  if (row_idx >= 0) and (row_idx < Fsize1) and (col_idx >= 0) and (col_idx < Fsize2) then
    Result := x[row_idx, col_idx]
  else
    raise EListError.CreateFmt('Index %d,%d is out of bounds', [row_idx, col_idx]);
end;

function TDoubleMatrix.getRow(idx: Integer): TDoubleVector;
var i: Integer;
begin
  if (idx < 0) or (idx >= FSize1) then begin
    Result := nil;
    Exit;
  end;
  Result := TDoubleVector.Create(Fsize2);
  for i:=0 to FSize2-1 do
    Result.push_back(getElement(idx, i));
end;

procedure TDoubleMatrix.saveToFile(const filename: string);
var i, j: Integer;
    FStr: TStringList;
    S: string;
begin
  FStr := TStringList.Create();
  if (FileExists(filename)) then
    FStr.LoadFromFile(filename);

  for i:=0 to Fsize1-1 do begin
    S := '';
    for j:=0 to FSize2-1 do begin
      S := S + FloatToStr(x[i, j]);
      if (j < FSize2-1) then S := S + #09;
    end;
    FStr.Add(S);
  end;
  FStr.SaveToFile(filename);
  FStr.Free();
end;

procedure TDoubleMatrix.setCapacity(aCapacity1, aCapacity2: Integer);
begin
  if (aCapacity1 >= 0) then
    FCapacity1 := aCapacity1
  else
    FCapacity1 := 0;
  if (aCapacity2 >= 0) then
    FCapacity2 := aCapacity2
  else
    FCapacity2 := 0;

  SetLength(x, FCapacity1, FCapacity2);
end;

procedure TDoubleMatrix.setElement(row_idx, col_idx: Integer; const aValue: Double);
begin
  if (row_idx >= 0) and (row_idx < Fsize1) and (col_idx >= 0) and (col_idx < Fsize2) then
    x[row_idx, col_idx] := aValue
  else
    raise EListError.CreateFmt('Index %d,%d is out of bounds', [row_idx, col_idx]);
end;

procedure TDoubleMatrix.setSize(aSize1, aSize2: integer);
begin
  if (aSize1 >= 0) then
    Fsize1 := aSize1
  else
    Fsize1 := 0;
  if (aSize2 >= 0) then
    Fsize2 := aSize2
  else
    Fsize2 := 0;

  if (Fsize1 > FCapacity1) or (Fsize2 > FCapacity2) then
    setCapacity(Max(Fsize1, FCapacity1), Max(Fsize2, FCapacity2));
end;

procedure TDoubleMatrix.shrink_to_fit;
begin
  if (FCapacity1 > Fsize1) or (FCapacity2 > Fsize2) then
    setCapacity(Fsize1, Fsize2);
end;

end.
