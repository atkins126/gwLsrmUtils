unit MCBAPI32;

interface

uses Windows;

const

  (*************************************************************************)
  (* Define buffer sizes needed to hold certain response records from MCBs *)
  (*************************************************************************)

  MCBSTATUSSIZE = 36;              (* Bytes in status rec including NULL *)
  MCBTYPESIZE = 9;                 (* Bytes in ID string incl NULL *)
  MCBTIMESIZE = 9;                 (* Bytes in time string incl NULL *)
  MCBDATESIZE = 9;                 (* Bytes in date string incl NULL *)
  MCBFEATUREWORDS = 16;            (* Max number of feature words *)
  MCBIPADDRSIZE = 128;             (* Max chars of Ip address (IPV4 or IPV6)*)

  (*******************************)
  (* Define MCBCIO Buffer sizes. *)
  (*******************************)

  MIOHOSTMAX = 64;                 (* network Host name including NULL *)
  MIOAUTHMAX = 32;                 (* Authorization str including NULL *)
  MIOPASSMAX = 32;                 (* Password string including NULL *)
  MIOOWNERMAX = 64;                (* Det Owner Name str including NULL *)
  MIODETDESCMAX = 128;             (* Det Description str including NULL *)
  MIODETNAMEMAX = 128;             (* Det Name from MIOGetConfigName() *)
  MIOAPPDATANAMEMAX = 32;          (* App Data Name string including NULL *)
  MIOAPPDATAMAX = 128;             (* App Data string including NULL *)
  MIOCONFIGMCBSMAX = 16;           (* max MCBs to report by MIOGetHardwareConf() *)
  MIOCONFIGMCBSMAXEX = 64;         (* max MCBs to report by MIOGetHardwareConfEx() *)
  MIOCONFIGMCBSEX2 = 128;          (* default number of config pairs in MIOHARDCONFEX2 *)
  MIOSECURITYMAX = 32;             (* Length of Server Security String *)
  MIOMAXSERVERDATA = 10;           (* number of pointers in LPSERVERDATA *)
  MIOMAXLANAS = 10;                (* max number of NetBIOS lan adapters concurrently supported *)
  MIOKEYMAX = 48;                  (* Key string including NULL *)

type

  (* Structure used to identify instruments that may be opened. szHost + nMCB + nDevSeg must *)
  (* completely define the address of a detector input that may be opened. nLana identifies  *)
  (* the protocol to use to communicate with the instrument. New for ver 7.0, nLana == -1    *)
  (* indicates that szHost is a DNS resolvable IP address and we should use WinSock instead  *)
  (* of NetBIOS for communication.                                                           *)

  PMIOConfigItem = ^TMIOConfigItem;
  tagMIOCONFIGITEM = packed record
    nLength: Integer;                                         (* set to sizeof(MIOCONFIGITEM) before using *)
    dwID: DWORD;                                              (* det ID number as shown in pick list *)
    nMCB: Integer;                                            (* MCB number on network host *)
    nDevSeg: Integer;                                         (* MCB Device/Segment number *)
    nLana: Integer;                                           (* NetBIOS lana number for host connection or -1 for WinSock *)
    szName: array [0..MIODETNAMEMAX - 1] of AnsiChar;         (* det name as shown in pick list *)
    szHost: array [0..MIOHOSTMAX - 1] of AnsiChar;            (* Network host computer name or DNS resolvable IP Address *)
    szType: array [0..MCBTYPESIZE - 1] of AnsiChar;           (* Detector Type *)
    uFeatures: array [0..MCBFEATUREWORDS - 1] of Cardinal;    (* Feature bits (room for 512 bits) *)
  end;
  TMIOConfigItem = tagMIOCONFIGITEM;

type

  HDET = Pointer;

  TMIOStartup = function: BOOL stdcall;
  TMIOCleanup = function: BOOL stdcall;
  TMIOGetConfigName = function(nDet: Integer; lpszListName: LPCSTR; nNameMax: Integer;
    lpszName: LPSTR; lpdwID: PDWORD; lpbOutDated: PBOOL): BOOL stdcall;
  TMIOOpenDetector = function(nDet: Integer; lpszListName: LPCSTR; lpszAuth: LPCSTR): HDET stdcall;
  TMIOGetDetectorInfo = function(hDet: HDET; lpszDesc: LPSTR; nMaxDesc: Integer;
    lpbDefaultDesc: PBOOL; lpdwID: PDWORD; lpbDefaultID: PBOOL): BOOL stdcall;
  TMIOCloseDetector = function(hDet: HDET): BOOL stdcall;
  TMIOGetType = function(hDet: HDET): LPSTR stdcall;
  TMIOGetConfigItem = function(nDet: Integer; lpszListName: LPCSTR; lpConfigItem: PMIOConfigItem): BOOL stdcall;



function MIOStartup: BOOL;
function MIOCleanup: BOOL;
function MIOGetConfigName(nDet: Integer; lpszListName: LPCSTR; nNameMax: Integer;
    lpszName: LPSTR; lpdwID: PDWORD; lpbOutDated: PBOOL): BOOL;
function MIOOpenDetector(nDet: Integer; lpszListName: LPCSTR; lpszAuth: LPCSTR): HDET;
function MIOGetDetectorInfo(hDet: HDET; lpszDesc: LPSTR; nMaxDesc: Integer;
    lpbDefaultDesc: PBOOL; lpdwID: PDWORD; lpbDefaultID: PBOOL): BOOL;
function MIOCloseDetector(hDet: HDET): BOOL;
function MIOGetType(hDet: HDET): LPSTR;
function MIOGetConfigItem(nDet: Integer; lpszListName: LPCSTR; lpConfigItem: PMIOConfigItem): BOOL;
function MCBLoaded: BOOL;

implementation

var
  mMIOStartup: TMIOStartup;
  mMIOCleanup: TMIOCleanup;
  mMIOGetConfigName: TMIOGetConfigName;
  mMIOOpenDetector: TMIOOpenDetector;
  mMIOGetDetectorInfo: TMIOGetDetectorInfo;
  mMIOCloseDetector: TMIOCloseDetector;
  mMIOGetType: TMIOGetType;
  mMIOGetConfigItem: TMIOGetConfigItem;

  MCBCIO32Handle: THandle;

function MCBLoaded: BOOL;
begin
  if MCBCIO32Handle <> 0 then
    Result:= True
  else
  begin
    MCBCIO32Handle:= LoadLibrary('mcbcio32.dll');
    if MCBCIO32Handle > HINSTANCE_ERROR then
    begin
      Result:= True;
      @mMIOStartup:= GetProcAddress(MCBCIO32Handle, 'MIOStartup');
      @mMIOCleanup:= GetProcAddress(MCBCIO32Handle, 'MIOCleanup');
      @mMIOGetConfigName:= GetProcAddress(MCBCIO32Handle, 'MIOGetConfigName');
      @mMIOOpenDetector:= GetProcAddress(MCBCIO32Handle, 'MIOOpenDetector');
      @mMIOGetDetectorInfo:= GetProcAddress(MCBCIO32Handle, 'MIOGetDetectorInfo');
      @mMIOCloseDetector:= GetProcAddress(MCBCIO32Handle, 'MIOCloseDetector');
      @mMIOGetType:= GetProcAddress(MCBCIO32Handle, 'MIOGetType');
      @mMIOGetConfigItem:= GetProcAddress(MCBCIO32Handle, 'MIOGetConfigItem');
    end
    else
      Result:= False;
  end;
end;

function MIOStartup: BOOL;
begin
  if not MCBLoaded then
    Result:= False
  else if @mMIOStartup <> nil then
    Result:= mMIOStartup
  else
    Result:= False;
end;

function MIOCleanup: BOOL;
begin
  if not MCBLoaded then
    Result:= False
  else if @mMIOCleanup <> nil then
    Result:= mMIOCleanup
  else
    Result:= False;
end;

function MIOGetConfigName(nDet: Integer; lpszListName: LPCSTR; nNameMax: Integer;
    lpszName: LPSTR; lpdwID: PDWORD; lpbOutDated: PBOOL): BOOL;
begin
  if not MCBLoaded then
    Result:= False
  else if @mMIOGetConfigName <> nil then
    Result:= mMIOGetConfigName(nDet, lpszListName, nNameMax, lpszName, lpdwID, lpbOutDated)
  else
    Result:= False;
end;

function MIOOpenDetector(nDet: Integer; lpszListName: LPCSTR; lpszAuth: LPCSTR): HDET;
begin
  if not MCBLoaded then
    Result:= nil
  else if @mMIOOpenDetector <> nil then
    Result:= mMIOOpenDetector(nDet, lpszListName, lpszAuth)
  else
    Result:= nil;
end;

function MIOGetDetectorInfo(hDet: HDET; lpszDesc: LPSTR; nMaxDesc: Integer;
    lpbDefaultDesc: PBOOL; lpdwID: PDWORD; lpbDefaultID: PBOOL): BOOL;
begin
  if not MCBLoaded then
    Result:= False
  else if @mMIOGetDetectorInfo <> nil then
    Result:= mMIOGetDetectorInfo(hDet, lpszDesc, nMaxDesc, lpbDefaultDesc, lpdwID, lpbDefaultID)
  else
    Result:= False;
end;

function MIOCloseDetector(hDet: HDET): BOOL;
begin
  if not MCBLoaded then
    Result:= False
  else if @mMIOCloseDetector <> nil then
    Result:= mMIOCloseDetector(hDet)
  else
    Result:= False;
end;

function MIOGetType(hDet: HDET): LPSTR;
begin
  if not MCBLoaded then
    Result:= ''
  else if @mMIOGetType <> nil then
    Result:= mMIOGetType(hDet)
  else
    Result:= '';
end;

function MIOGetConfigItem(nDet: Integer; lpszListName: LPCSTR; lpConfigItem: PMIOConfigItem): BOOL;
begin
  if not MCBLoaded then
    Result:= False
  else if @mMIOGetConfigItem <> nil then
    Result:= mMIOGetConfigItem(nDet, lpszListName, lpConfigItem)
  else
    Result:= False;
end;

initialization

  MCBCIO32Handle:= 0;

end.
