unit gwFileUtils;

interface

function filenameFromPath(const filename: string): string;
function addSmth2FileName(const filename, smthstr: string): string;
function addPathToFileName(const filename, filepath: string): string;
procedure CorrectUnixFilePath(const HeadPath: string; var APath: string);
function getParentDir(const filename: string): string;
function getFileVersionGw(const filename: string): string;

implementation

uses SysUtils, Types, Windows, JvJCLUtils;

function filenameFromPath(const filename: string): string;
var extStr: string;
begin
  Result := ExtractFileName(filename);
  extStr := ExtractFileExt(Result);
  Delete(Result, Pos(extStr, Result), Length(extStr));
end;

function addSmth2FileName(const filename, smthstr: string): string;
var extStr: string;
begin
  extStr := ExtractFileExt(filename);
  Result := Copy(filename, 1, Length(filename) - Length(extStr));
  Result := Result + smthstr + extStr;
end;

function addPathToFileName(const filename, filepath: string): string;
begin
  if (pos(':', filename) = 0) then
    Result := filepath + filename
  else
    Result := filename;
end;

procedure CorrectUnixFilePath(const HeadPath: string; var APath: string);
var i: Integer;
    FHeadPath: string;
begin
  if (Length(APath) = 0) then Exit;
  for i:=1 to Length(APath) do
    if (APath[i] = '/') then
      APath[i] := '\';
  FHeadPath := HeadPath;
  if ((APath[1]='.') and (APath[2]='.') and (APath[3]='\')) then
  begin
    APath := Copy(APath,4,Length(APath)-3);
    FHeadPath := ParentPath(FHeadPath);
  end;

  APath := FHeadPath + APath;
end;

function getParentDir(const filename: string): string;
var i, slash_pos: Integer;
begin
  Result := ExtractFileDir(filename);
  // find last '/' pos
  slash_pos := 0;
  for i:=1 to Length(Result) do begin
    if Result[i] = '\' then
      slash_pos := i;
  end;
  if (slash_pos <> 0) then
    Delete(Result, 1, slash_pos);
  Result := Result;
end;

function getFileVersionGw(const filename: string): string;
type
  PDWORD = ^DWORD;
  PLangAndCodePage = ^TLangAndCodePage;
  TLangAndCodePage = packed record
    wLanguage: WORD;
    wCodePage: WORD;
  end;
  PLangAndCodePageArray = ^TLangAndCodePageArray;
  TLangAndCodePageArray = array[0..0] of TLangAndCodePage;
var
  loc_InfoBufSize: DWORD;
  loc_InfoBuf: PChar;
  loc_VerBufSize: DWORD;
  loc_VerBuf: PChar;
  cbTranslate: DWORD;
  lpTranslate: PDWORD;
  i: DWORD; 
begin
  Result := '';
  if (not FileExists(filename)) then Exit;

  loc_InfoBufSize := GetFileVersionInfoSize(PChar(FileName), loc_InfoBufSize);
  if loc_InfoBufSize > 0 then
  begin
    loc_VerBuf := nil;
    loc_InfoBuf := AllocMem(loc_InfoBufSize);
    try
      if not GetFileVersionInfo(PChar(FileName), 0, loc_InfoBufSize, loc_InfoBuf) then
        exit;
      if not VerQueryValue(loc_InfoBuf, '\\VarFileInfo\\Translation',
        Pointer(lpTranslate), DWORD(cbTranslate)) then
        exit;
      for i := 0 to (cbTranslate div SizeOf(TLangAndCodePage)) - 1 do
      begin
        if VerQueryValue(
          loc_InfoBuf,
          PChar(Format(
          'StringFileInfo\0%x0%x\FileVersion', [
          PLangAndCodePageArray(lpTranslate)[i].wLanguage,
            PLangAndCodePageArray(lpTranslate)[i].wCodePage])),
            Pointer(loc_VerBuf),
          DWORD(loc_VerBufSize)
          ) then
        begin
          Result := loc_VerBuf;
          Break;
        end;
      end;
    finally
      FreeMem(loc_InfoBuf, loc_InfoBufSize);
    end;
  end;
end;

end.
