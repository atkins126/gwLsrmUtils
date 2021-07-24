unit Convolution;

interface

uses Matrix;

procedure ConvolutionLorGauss(sigma, NSigma, sigma_lor, NSigma_lor: Double; var Conv: TVector);
procedure ConvolutionDoppGauss(sigma, NSigma, xm, alpha: Double; var Conv: TVector);
procedure ObtainGauss(sigma, NSigma: Double; var AGauss: TVector);
procedure ObtainLorentz(sigma, NSigma: Double; var ALorentz: TVector);
procedure ObtainDoppler(xm, alpha: Double; NPoints: Integer; var ADoppler: TVector);
function GaussF(x, mu, sigma: Double): Double;
function LorentzF(x, mu, sigma: Double): Double;
function DopplerF(x, xm, alpha: Double): Double;
function SigmaFromFWHM(FWHM: Double): Double;

implementation

uses Math;

procedure ConvolutionLorGauss(sigma, NSigma, sigma_lor, NSigma_lor: Double; var Conv: TVector);
var i, j, n, mm: Integer;
    norm: Double;
begin
  n := Trunc(Max(sigma*NSigma, sigma_lor*NSigma_lor));
  mm := 2*n+1;
  SetLength(Conv, mm);
  norm:=0;
  for i:=-n to n do begin
    Conv[i+n] := 0;
    for j:=-n to n do
      Conv[i+n]:=Conv[i+n] + LorentzF(i-j,0,sigma_lor)*GaussF(j,0,sigma);
    norm := norm+ Conv[i+n];
    //norm := Max(norm,Conv[i+n]);
  end;
  for i:=-n to n do begin
    Conv[i+n]:=Conv[i+n]/norm;
  end;
end;

procedure ConvolutionDoppGauss(sigma, NSigma, xm, alpha: Double; var Conv: TVector);
var i, j, n, mm: Integer;
    norm: Double;
begin
  n := Trunc(Max(sigma*NSigma, 1.2*xm));
  mm := 2*n + 1;
  SetLength(Conv, mm);
  norm:=0;
  for i:=-n to n do begin
    Conv[i+n] := 0;
    for j:=-n to n do
      Conv[i+n] := Conv[i+n] + DopplerF(i-j,xm,alpha)*GaussF(j,0,sigma);
    norm := norm + Conv[i+n];
  end;
  for i:=-n to n do begin
    Conv[i+n]:=Conv[i+n]/norm;
  end;
end;

function GaussF(x, mu, sigma: Double): Double;
begin
  Result:= Exp(- Sqr(x-mu) / (2* Sqr(sigma)));
end;

function LorentzF(x, mu, sigma: Double): Double;
begin
  Result := sigma / (Sqr(sigma) + Sqr(x-mu));
end;

function DopplerF(x, xm, alpha: Double): Double;
begin
  x := Abs(x);
  if (x < xm) then
    Result := (1 + 1/alpha) * (1 - Power(x/xm, alpha))
  else
    Result := 0;
end;

function SigmaFromFWHM(FWHM: Double): Double;
begin
  Result := FWHM / (2*sqrt(2*ln(2))); // ~2.35
end;

procedure ObtainGauss(sigma, NSigma: Double; var AGauss: TVector);
var i, n, mm: Integer;
    sigma_2, norm: Double;
begin
  n := Trunc(sigma*NSigma);
  mm := 2*n+1;
  SetLength(AGauss, mm);
  sigma_2 := Sqr(sigma);
  norm:=0;
  for i:=-n to n do begin
    AGauss[i+n] := Exp(- Sqr(i) / (2* sigma_2));
    norm := norm+ AGauss[i+n];
    //norm := Max(norm,AGauss[i+n]);
  end;
  for i:=-n to n do begin
    AGauss[i+n]:=AGauss[i+n]/norm;
  end;
end;

procedure ObtainLorentz(sigma, NSigma: Double; var ALorentz: TVector);
var i, n, mm: Integer;
    sigma_2, norm: Double;
begin
  n := Trunc(sigma*NSigma);
  mm := 2*n+1;
  SetLength(ALorentz, mm);
  sigma_2 := Sqr(sigma);
  norm:=0;
  for i:=-n to n do begin
    ALorentz[i+n] := sigma / (sigma_2 + Sqr(i));
    norm := norm+ ALorentz[i+n];
    //norm := Max(norm,ALorentz[i+n]);
  end;
  for i:=-n to n do begin
    ALorentz[i+n]:=ALorentz[i+n]/norm;
  end;
end;

procedure ObtainDoppler(xm, alpha: Double; NPoints: Integer; var ADoppler: TVector);
var i, n, mm: Integer;
    norm, x: Double;
begin
  n := Trunc(xm);
  mm := 2*n + 1;
  SetLength(ADoppler, mm);
  norm := 0;
  for i:=-n to n do begin
    x := Abs(i/n*xm);
    if (x < xm) then begin
      ADoppler[i+n] := (1 + 1/alpha) * (1 - Power(x/xm, alpha));
      norm := norm + ADoppler[i+n];
    end
    else
      ADoppler[i+n] := 0;
  end;
  for i:=-n to n do begin
    ADoppler[i+n]:=ADoppler[i+n]/norm;
  end;
end;

end.
