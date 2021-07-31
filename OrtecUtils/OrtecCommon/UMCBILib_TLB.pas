unit UMCBILib_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 21.11.2013 13:29:18 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Windows\SysWow64\UMCBI.ocx (1)
// LIBID: {41CCC090-1ED5-11D3-A1B1-002078147A97}
// LCID: 0
// Helpfile: C:\Windows\system32\UMCBI.hlp
// HelpString: UMCBI ActiveX Control module
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\SysWOW64\stdole2.tlb)
// Errors:
//   Hint: Symbol 'Type' renamed to 'type_'
//   Hint: Symbol 'Type' renamed to 'type_'
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

uses Windows, ActiveX, Classes, Graphics, OleCtrls, OleServer, StdVCL, Variants;
  


// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  UMCBILibMajorVersion = 6;
  UMCBILibMinorVersion = 1;

  LIBID_UMCBILib: TGUID = '{41CCC090-1ED5-11D3-A1B1-002078147A97}';

  DIID__DUMCBICONN: TGUID = '{D87EF4B0-1ED8-11D3-A1B1-002078147A97}';
  DIID__DUMCBICONNEvents: TGUID = '{D87EF4B1-1ED8-11D3-A1B1-002078147A97}';
  CLASS_UCONN: TGUID = '{D87EF4B2-1ED8-11D3-A1B1-002078147A97}';
  DIID__DUMCBICONN2: TGUID = '{D44C359C-FABB-4703-B695-4522D926CCA4}';
  DIID__DUMCBICONN2Events: TGUID = '{6D97DA5C-7510-4CE5-B512-32B2E40DDD8B}';
  CLASS_UCONN2: TGUID = '{731B1F14-C7BF-46CB-8C20-B03802871E6E}';
  DIID__DUMCBILIST: TGUID = '{D87EF4B4-1ED8-11D3-A1B1-002078147A97}';
  DIID__DUMCBILISTEvents: TGUID = '{D87EF4B5-1ED8-11D3-A1B1-002078147A97}';
  CLASS_ULIST: TGUID = '{D87EF4B6-1ED8-11D3-A1B1-002078147A97}';
  DIID__DUMCBIBOX: TGUID = '{69AA4F60-4685-42CC-BF1F-0CD456E87A51}';
  DIID__DUMCBIBOXEvents: TGUID = '{69AA4F61-4685-42CC-BF1F-0CD456E87A51}';
  CLASS_UBOX: TGUID = '{69AA4F5F-4685-42CC-BF1F-0CD456E87A51}';
  DIID__DUMCBIDROP: TGUID = '{D87EF4B8-1ED8-11D3-A1B1-002078147A97}';
  DIID__DUMCBIDROPEvents: TGUID = '{D87EF4B9-1ED8-11D3-A1B1-002078147A97}';
  CLASS_UDROP: TGUID = '{D87EF4BA-1ED8-11D3-A1B1-002078147A97}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum MIOFEATURES
type
  MIOFEATURES = TOleEnum;
const
  MIOFEAT_CONVGAIN = $00000000;
  MIOFEAT_COARSEGAIN = $00000001;
  MIOFEAT_FINEGAIN = $00000002;
  MIOFEAT_GAINSTAB = $00000003;
  MIOFEAT_ZEROSTAB = $00000004;
  MIOFEAT_PHAMODE = $00000005;
  MIOFEAT_MCSMODE = $00000006;
  MIOFEAT_LISTMODE = $00000007;
  MIOFEAT_SAMPMODE = $00000008;
  MIOFEAT_DIGITALOFF = $00000009;
  MIOFEAT_FINEOFF = $0000000A;
  MIOFEAT_HVPOWER = $0000000B;
  MIOFEAT_HVENHANCED = $0000000C;
  MIOFEAT_HVRANGE = $0000000D;
  MIOFEAT_AUTOPZ = $0000000E;
  MIOFEAT_MANPZ = $0000000F;
  MIOFEAT_CLOCK = $00000010;
  MIOFEAT_SAMPCHANGER = $00000011;
  MIOFEAT_FIELDMODE = $00000012;
  MIOFEAT_NOMADIC = $00000013;
  MIOFEAT_APPDATA = $00000014;
  MIOFEAT_SERIALNUM = $00000015;
  MIOFEAT_POWERMAN = $00000016;
  MIOFEAT_BATTERYSTAT = $00000017;
  MIOFEAT_AMPPOLARITY = $00000018;
  MIOFEAT_OPTIMIZE = $00000019;
  MIOFEAT_STOPPZ = $0000001A;
  MIOFEAT_NETWORK = $0000001B;
  MIOFEAT_MULTIDROP = $0000001C;
  MIOFEAT_DPMADDR = $0000001D;
  MIOFEAT_MULTIDEV = $0000001E;
  MIOFEAT_GATEMODE = $0000001F;
  MIOFEAT_DOWNLOAD = $00000020;
  MIOFEAT_THAMODE = $00000021;
  MIOFEAT_LLD = $00000022;
  MIOFEAT_ULD = $00000023;
  MIOFEAT_SCAINPUT = $00000024;
  MIOFEAT_TTLINPUT = $00000025;
  MIOFEAT_NEGNIMINPUT = $00000026;
  MIOFEAT_DISCINPUT = $00000027;
  MIOFEAT_DISCEDGE = $00000028;
  MIOFEAT_DISCLEVEL = $00000029;
  MIOFEAT_SCAPROG = $0000002A;
  MIOFEAT_INPUTSELECT = $0000002B;
  MIOFEAT_STATPRESET = $0000002C;
  MIOFEAT_VARIFEAT = $0000002D;
  MIOFEAT_SHUTDOWN = $0000002E;
  MIOFEAT_SHAPECONST = $0000002F;
  MIOFEAT_EXPLORESHAPE = $00000030;
  MIOFEAT_ADVANCEDSHAPE = $00000031;
  MIOFEAT_BLR = $00000032;
  MIOFEAT_SHOWSTAT = $00000033;
  MIOFEAT_OVERPRESET = $00000034;
  MIOFEAT_CLICKER = $00000035;
  MIOFEAT_THERMISTOR = $00000036;
  MIOFEAT_FLOATFINE = $00000037;
  MIOFEAT_PUR = $00000038;
  MIOFEAT_ALPHAHV = $00000039;
  MIOFEAT_VACUUM = $0000003A;
  MIOFEAT_ACQALARM = $0000003B;
  MIOFEAT_TRIGGER = $0000003C;
  MIOFEAT_ORDINALSHAP = $0000003D;
  MIOFEAT_LISTGAINS = $0000003E;
  MIOFEAT_ROUTINPUT = $0000003F;
  MIOFEAT_EXTDWELL = $00000040;
  MIOFEAT_SUMREPLACE = $00000041;
  MIOFEAT_EXTSTART = $00000042;
  MIOFEAT_LISTMCS = $00000043;
  MIOFEAT_MDAPRESET = $00000044;
  MIOFEAT_ADCTYPE = $00000045;
  MIOFEAT_DAISY = $00000046;
  MIOFEAT_ZERODT = $00000047;
  MIOFEAT_DSPPTRIG = $00000048;
  MIOFEAT_MULTIINP = $00000049;
  MIOFEAT_COUNTRATE = $0000004A;
  MIOFEAT_MULTIZDT = $0000004B;
  MIOFEAT_MULTIMDA = $0000004C;
  MIOFEAT_MCSRPLSUM = $0000004D;
  MIOFEAT_MCSPRGXDWELL = $0000004E;
  MIOFEAT_INTDIFSHAPE = $0000004F;
  MIOFEAT_PRGPULSE = $00000050;
  MIOFEAT_PRGVCMINTRLK = $00000051;
  MIOFEAT_PRGCURINTRLK = $00000052;
  MIOFEAT_EXPLORESTAB = $00000053;
  MIOFEAT_PRGINPUTIMP = $00000054;
  MIOFEAT_NOCUSP = $00000055;
  MIOFEAT_HVRISE = $00000056;
  MIOFEAT_LISTGATE = $00000057;
  MIOFEAT_MONITORS = $00000058;
  MIOFEAT_SMARTDET = $00000059;
  MIOFEAT_NUCLIDEREPORT = $0000005A;
  MIOFEAT_INTERACTDISPLAY = $0000005B;
  MIOFEAT_ADVSTOREDSPECTRA = $0000005C;
  MIOFEAT_MULTIVIEWDATA = $0000005D;
  MIOFEAT_RS232 = $0000005E;
  MIOFEAT_HVNOSETPOL = $0000005F;
  MIOFEAT_LOWFREQREJ = $00000060;
  MIOFEAT_RESENHANCER = $00000061;
  MIOFEAT_RESENHLIST = $00000062;
  MIOFEAT_SHOWSAMPTIME = $00000063;
  MIOFEAT_SETSAMPTIME = $00000064;
  MIOFEAT_LISTMODEDB = $00000065;
  MIOFEAT_HIGHTHROUGHPUT = $00000066;
  MIOFEAT_LISTMODEPRO = $00000067;
  MIOFEAT_ENHMANPZ = $00000068;
  MIOFEAT_SHAPEREADONLYPPG = $00000069;
  MIOFEAT_HVREADONLYPPG = $0000006A;
  MIOFEAT_AMPGAINREADONLYPPG = $0000006B;
  MIOFEAT_PZREADONLYPPG = $0000006C;
  MIOFEAT_LFRREADONLYPPG = $0000006D;
  MIOFEAT_SYNCHLISTMODE = $0000006E;
  MIOFEAT_DSPECPROAUXENAB = $0000006F;
  MIOFEAT_EXTENDED = $0000007F;
  MIOFEAT_EXTENDED2 = $000000FF;
  MIOFEAT_EXTENDED3 = $0000017F;

// Constants for enum MIOERRORS
type
  MIOERRORS = TOleEnum;
const
  MIOENONE = $000003E8;
  MIOEINVALID = $000003E9;
  MIOEMCB = $000003EA;
  MIOEIO = $000003EB;
  MIOEMEM = $000003EC;
  MIOENOTAUTH = $000003ED;
  MIOENOCONTEXT = $000003F0;
  MIOENOTOPEN = $000003F1;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  _DUMCBICONN = dispinterface;
  _DUMCBICONNEvents = dispinterface;
  _DUMCBICONN2 = dispinterface;
  _DUMCBICONN2Events = dispinterface;
  _DUMCBILIST = dispinterface;
  _DUMCBILISTEvents = dispinterface;
  _DUMCBIBOX = dispinterface;
  _DUMCBIBOXEvents = dispinterface;
  _DUMCBIDROP = dispinterface;
  _DUMCBIDROPEvents = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  UCONN = _DUMCBICONN;
  UCONN2 = _DUMCBICONN2;
  ULIST = _DUMCBILIST;
  UBOX = _DUMCBIBOX;
  UDROP = _DUMCBIDROP;


// *********************************************************************//
// DispIntf:  _DUMCBICONN
// Flags:     (4112) Hidden Dispatchable
// GUID:      {D87EF4B0-1ED8-11D3-A1B1-002078147A97}
// *********************************************************************//
  _DUMCBICONN = dispinterface
    ['{D87EF4B0-1ED8-11D3-A1B1-002078147A97}']
    property Address: WideString dispid 1;
    property type_: WideString readonly dispid 2;
    property MCB: Smallint readonly dispid 3;
    property Input: Smallint readonly dispid 4;
    property Host: WideString readonly dispid 5;
    property Length: Integer readonly dispid 6;
    property DebugLevel: Integer dispid 7;
    property VerMajor: Smallint readonly dispid 8;
    property VerMinor: Smallint readonly dispid 9;
    property StartTime: TDateTime readonly dispid 10;
    property RawStartTime: TDateTime readonly dispid 11;
    property IsActive: WordBool readonly dispid 12;
    property AppDataName: WideString dispid 13;
    property AppDataValue: WideString dispid 14;
    property Password: WideString dispid 15;
    property Authorization: WideString dispid 16;
    property ErrMajor: Integer readonly dispid 17;
    property ErrMinor: Integer readonly dispid 18;
    property Error: Integer readonly dispid 19;
    property DataMask: Integer readonly dispid 20;
    property ROIMask: Integer readonly dispid 21;
    property Description: WideString dispid 22;
    property ID: Integer dispid 23;
    property IsOpen: WordBool readonly dispid 24;
    property IsLocked: WordBool readonly dispid 25;
    property IsReadonly: WordBool readonly dispid 26;
    property IsLocal: WordBool readonly dispid 27;
    property LockOwner: WideString readonly dispid 28;
    property MCBTime: TDateTime dispid 29;
    property ActiveLength: Integer dispid 30;
    property Key: WideString readonly dispid 31;
    property UMCBIHandle: Integer dispid 32;
    property Data[Index: Integer]: Integer readonly dispid 45;
    property RawData[Index: Integer]: Integer readonly dispid 46;
    property ROIData[Index: Integer]: Integer readonly dispid 47;
    property IsFeature[FeatureNumber: MIOFEATURES]: WordBool readonly dispid 48;
    function GetData(Start: Integer; Length: Integer): OleVariant; dispid 33;
    function GetRawData(Start: Integer; Length: Integer): OleVariant; dispid 34;
    function GetROIData(Start: Integer; Length: Integer): OleVariant; dispid 35;
    procedure LockInput(const LockOwner: WideString); dispid 36;
    procedure UnlockInput; dispid 37;
    procedure SetROI(Start: Integer; Length: Integer); dispid 38;
    procedure ClearROI(Start: Integer; Length: Integer); dispid 39;
    procedure Open; dispid 40;
    procedure Close; dispid 41;
    function Comm(const Command: WideString): WideString; dispid 42;
    procedure SignalDevice(const Message: WideString); dispid 43;
    procedure EndSignalDevice; dispid 44;
  end;

// *********************************************************************//
// DispIntf:  _DUMCBICONNEvents
// Flags:     (4096) Dispatchable
// GUID:      {D87EF4B1-1ED8-11D3-A1B1-002078147A97}
// *********************************************************************//
  _DUMCBICONNEvents = dispinterface
    ['{D87EF4B1-1ED8-11D3-A1B1-002078147A97}']
  end;

// *********************************************************************//
// DispIntf:  _DUMCBICONN2
// Flags:     (4112) Hidden Dispatchable
// GUID:      {D44C359C-FABB-4703-B695-4522D926CCA4}
// *********************************************************************//
  _DUMCBICONN2 = dispinterface
    ['{D44C359C-FABB-4703-B695-4522D926CCA4}']
    property Address: WideString dispid 1;
    property type_: WideString readonly dispid 2;
    property MCB: Smallint readonly dispid 3;
    property Input: Smallint readonly dispid 4;
    property Host: WideString readonly dispid 5;
    property Length: Integer readonly dispid 6;
    property DebugLevel: Integer dispid 7;
    property VerMajor: Smallint readonly dispid 8;
    property VerMinor: Smallint readonly dispid 9;
    property StartTime: TDateTime readonly dispid 10;
    property RawStartTime: TDateTime readonly dispid 11;
    property IsActive: WordBool readonly dispid 12;
    property AppDataName: WideString dispid 13;
    property AppDataValue: WideString dispid 14;
    property Password: WideString dispid 15;
    property Authorization: WideString dispid 16;
    property ErrMajor: Integer readonly dispid 17;
    property ErrMinor: Integer readonly dispid 18;
    property Error: Integer readonly dispid 19;
    property DataMask: Integer readonly dispid 20;
    property ROIMask: Integer readonly dispid 21;
    property Description: WideString dispid 22;
    property ID: Integer dispid 23;
    property IsOpen: WordBool readonly dispid 24;
    property IsLocked: WordBool readonly dispid 25;
    property IsReadonly: WordBool readonly dispid 26;
    property IsLocal: WordBool readonly dispid 27;
    property LockOwner: WideString readonly dispid 28;
    property MCBTime: TDateTime dispid 29;
    property ActiveLength: Integer dispid 30;
    property Key: WideString readonly dispid 31;
    property UMCBIHandle: Integer dispid 32;
    property Data[Index: Integer]: Integer readonly dispid 45;
    property RawData[Index: Integer]: Integer readonly dispid 46;
    property ROIData[Index: Integer]: Integer readonly dispid 47;
    property IsFeature[FeatureNumber: MIOFEATURES]: WordBool readonly dispid 48;
    function GetData(Start: Integer; Length: Integer): OleVariant; dispid 33;
    function GetRawData(Start: Integer; Length: Integer): OleVariant; dispid 34;
    function GetROIData(Start: Integer; Length: Integer): OleVariant; dispid 35;
    procedure LockInput(const LockOwner: WideString); dispid 36;
    procedure UnlockInput; dispid 37;
    procedure SetROI(Start: Integer; Length: Integer); dispid 38;
    procedure ClearROI(Start: Integer; Length: Integer); dispid 39;
    procedure Open; dispid 40;
    procedure Close; dispid 41;
    function Comm(const Command: WideString): WideString; dispid 42;
    procedure SignalDevice(const Message: WideString); dispid 43;
    procedure EndSignalDevice; dispid 44;
    procedure SetROIEx(Start: Integer; Length: Integer; View: Integer); dispid 49;
    procedure ClearROIEx(Start: Integer; Length: Integer; View: Integer); dispid 50;
    procedure SetData(Start: Integer; Length: Integer; View: Integer; Data: OleVariant); dispid 51;
  end;

// *********************************************************************//
// DispIntf:  _DUMCBICONN2Events
// Flags:     (4096) Dispatchable
// GUID:      {6D97DA5C-7510-4CE5-B512-32B2E40DDD8B}
// *********************************************************************//
  _DUMCBICONN2Events = dispinterface
    ['{6D97DA5C-7510-4CE5-B512-32B2E40DDD8B}']
  end;

// *********************************************************************//
// DispIntf:  _DUMCBILIST
// Flags:     (4112) Hidden Dispatchable
// GUID:      {D87EF4B4-1ED8-11D3-A1B1-002078147A97}
// *********************************************************************//
  _DUMCBILIST = dispinterface
    ['{D87EF4B4-1ED8-11D3-A1B1-002078147A97}']
    property PersistSel: WordBool dispid 1;
    property SelAddress: WideString dispid 2;
    property SelAddressAlt: WideString dispid 3;
    property SelIndex: Integer dispid 4;
    property ShowIDs: WordBool dispid 5;
    property ShowBuffer: WordBool dispid 6;
    property ListName: WideString dispid 7;
    property OutOfDate: WordBool readonly dispid 8;
    property SelName: WideString readonly dispid 9;
    property SelID: Integer readonly dispid 10;
    property SelMCB: Integer readonly dispid 11;
    property SelInput: Integer readonly dispid 12;
    property SelHost: WideString readonly dispid 13;
    property SelType: WideString readonly dispid 14;
    property FireOnInternalSelChange: WordBool dispid 15;
    property MaxSelection: Integer dispid 16;
    property VerMajor: Smallint dispid 17;
    property VerMinor: Smallint dispid 18;
    function AddSelItem(MasterIndex: Integer): WordBool; dispid 19;
    function DeleteSelItem: WordBool; dispid 20;
    function CreateList: WordBool; dispid 21;
    property SelIsFeature[FeatureNumber: MIOFEATURES]: WordBool readonly dispid 22;
  end;

// *********************************************************************//
// DispIntf:  _DUMCBILISTEvents
// Flags:     (4096) Dispatchable
// GUID:      {D87EF4B5-1ED8-11D3-A1B1-002078147A97}
// *********************************************************************//
  _DUMCBILISTEvents = dispinterface
    ['{D87EF4B5-1ED8-11D3-A1B1-002078147A97}']
    procedure NewSelection; dispid 1;
  end;

// *********************************************************************//
// DispIntf:  _DUMCBIBOX
// Flags:     (4112) Hidden Dispatchable
// GUID:      {69AA4F60-4685-42CC-BF1F-0CD456E87A51}
// *********************************************************************//
  _DUMCBIBOX = dispinterface
    ['{69AA4F60-4685-42CC-BF1F-0CD456E87A51}']
    property PersistSel: WordBool dispid 1;
    property SelAddress: WideString dispid 2;
    property SelAddressAlt: WideString dispid 3;
    property SelIndex: Integer dispid 4;
    property ShowIDs: WordBool dispid 5;
    property ShowBuffer: WordBool dispid 6;
    property ListName: WideString dispid 7;
    property OutOfDate: WordBool readonly dispid 8;
    property SelName: WideString readonly dispid 9;
    property SelID: Integer readonly dispid 10;
    property SelMCB: Integer readonly dispid 11;
    property SelInput: Integer readonly dispid 12;
    property SelHost: WideString readonly dispid 13;
    property SelType: WideString readonly dispid 14;
    property FireOnInternalSelChange: WordBool dispid 15;
    property MaxSelection: Integer dispid 16;
    property VerMajor: Smallint dispid 17;
    property VerMinor: Smallint dispid 18;
    property Border: WordBool dispid 19;
    property LocalOnly: WordBool dispid 20;
    property RemoteOnly: WordBool dispid 21;
    function AddSelItem(MasterIndex: Integer): WordBool; dispid 22;
    function DeleteSelItem: WordBool; dispid 23;
    function CreateList: WordBool; dispid 24;
    property SelIsFeature[FeatureNumber: MIOFEATURES]: WordBool readonly dispid 25;
  end;

// *********************************************************************//
// DispIntf:  _DUMCBIBOXEvents
// Flags:     (4096) Dispatchable
// GUID:      {69AA4F61-4685-42CC-BF1F-0CD456E87A51}
// *********************************************************************//
  _DUMCBIBOXEvents = dispinterface
    ['{69AA4F61-4685-42CC-BF1F-0CD456E87A51}']
    procedure NewSelection; dispid 1;
    procedure DoubleClick; dispid 2;
  end;

// *********************************************************************//
// DispIntf:  _DUMCBIDROP
// Flags:     (4112) Hidden Dispatchable
// GUID:      {D87EF4B8-1ED8-11D3-A1B1-002078147A97}
// *********************************************************************//
  _DUMCBIDROP = dispinterface
    ['{D87EF4B8-1ED8-11D3-A1B1-002078147A97}']
    property PersistSel: WordBool dispid 1;
    property SelAddress: WideString dispid 2;
    property SelAddressAlt: WideString dispid 3;
    property SelIndex: Integer dispid 4;
    property ShowIDs: WordBool dispid 5;
    property ShowBuffer: WordBool dispid 6;
    property ListName: WideString dispid 7;
    property OutOfDate: WordBool readonly dispid 8;
    property SelName: WideString readonly dispid 9;
    property SelID: Integer readonly dispid 10;
    property SelMCB: Integer readonly dispid 11;
    property SelInput: Integer readonly dispid 12;
    property SelHost: WideString readonly dispid 13;
    property SelType: WideString readonly dispid 14;
    property FireOnInternalSelChange: WordBool dispid 15;
    property MaxSelection: Integer dispid 16;
    property VerMajor: Smallint dispid 17;
    property VerMinor: Smallint dispid 18;
    function AddSelItem(MasterIndex: Integer): WordBool; dispid 19;
    function DeleteSelItem: WordBool; dispid 20;
    function CreateList: WordBool; dispid 21;
    property SelIsFeature[FeatureNumber: MIOFEATURES]: WordBool readonly dispid 22;
  end;

// *********************************************************************//
// DispIntf:  _DUMCBIDROPEvents
// Flags:     (4096) Dispatchable
// GUID:      {D87EF4B9-1ED8-11D3-A1B1-002078147A97}
// *********************************************************************//
  _DUMCBIDROPEvents = dispinterface
    ['{D87EF4B9-1ED8-11D3-A1B1-002078147A97}']
    procedure NewSelection; dispid 1;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TUCONN2
// Help String      : UMCBI Connection Control 2
// Default Interface: _DUMCBICONN2
// Def. Intf. DISP? : Yes
// Event   Interface: _DUMCBICONN2Events
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TUCONN2 = class(TOleControl)
  private
    FIntf: _DUMCBICONN2;
    function  GetControlInterface: _DUMCBICONN2;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function Get_Data(Index: Integer): Integer;
    function Get_RawData(Index: Integer): Integer;
    function Get_ROIData(Index: Integer): Integer;
    function Get_IsFeature(FeatureNumber: MIOFEATURES): WordBool;
  public
    function GetData(Start: Integer; Length: Integer): OleVariant;
    function GetRawData(Start: Integer; Length: Integer): OleVariant;
    function GetROIData(Start: Integer; Length: Integer): OleVariant;
    procedure LockInput(const LockOwner: WideString);
    procedure UnlockInput;
    procedure SetROI(Start: Integer; Length: Integer);
    procedure ClearROI(Start: Integer; Length: Integer);
    procedure Open;
    procedure Close;
    function Comm(const Command: WideString): WideString;
    procedure SignalDevice(const Message: WideString);
    procedure EndSignalDevice;
    procedure SetROIEx(Start: Integer; Length: Integer; View: Integer);
    procedure ClearROIEx(Start: Integer; Length: Integer; View: Integer);
    procedure SetData(Start: Integer; Length: Integer; View: Integer; Data: OleVariant);
    property  ControlInterface: _DUMCBICONN2 read GetControlInterface;
    property  DefaultInterface: _DUMCBICONN2 read GetControlInterface;
    property type_: WideString index 2 read GetWideStringProp;
    property MCB: Smallint index 3 read GetSmallintProp;
    property Input: Smallint index 4 read GetSmallintProp;
    property Host: WideString index 5 read GetWideStringProp;
    property Length: Integer index 6 read GetIntegerProp;
    property VerMajor: Smallint index 8 read GetSmallintProp;
    property VerMinor: Smallint index 9 read GetSmallintProp;
    property StartTime: TDateTime index 10 read GetTDateTimeProp;
    property RawStartTime: TDateTime index 11 read GetTDateTimeProp;
    property IsActive: WordBool index 12 read GetWordBoolProp;
    property ErrMajor: Integer index 17 read GetIntegerProp;
    property ErrMinor: Integer index 18 read GetIntegerProp;
    property Error: Integer index 19 read GetIntegerProp;
    property DataMask: Integer index 20 read GetIntegerProp;
    property ROIMask: Integer index 21 read GetIntegerProp;
    property IsOpen: WordBool index 24 read GetWordBoolProp;
    property IsLocked: WordBool index 25 read GetWordBoolProp;
    property IsReadonly: WordBool index 26 read GetWordBoolProp;
    property IsLocal: WordBool index 27 read GetWordBoolProp;
    property LockOwner: WideString index 28 read GetWideStringProp;
    property Key: WideString index 31 read GetWideStringProp;
    property Data[Index: Integer]: Integer read Get_Data;
    property RawData[Index: Integer]: Integer read Get_RawData;
    property ROIData[Index: Integer]: Integer read Get_ROIData;
    property IsFeature[FeatureNumber: MIOFEATURES]: WordBool read Get_IsFeature;
  published
    property Anchors;
    property Address: WideString index 1 read GetWideStringProp write SetWideStringProp stored False;
    property DebugLevel: Integer index 7 read GetIntegerProp write SetIntegerProp stored False;
    property AppDataName: WideString index 13 read GetWideStringProp write SetWideStringProp stored False;
    property AppDataValue: WideString index 14 read GetWideStringProp write SetWideStringProp stored False;
    property Password: WideString index 15 read GetWideStringProp write SetWideStringProp stored False;
    property Authorization: WideString index 16 read GetWideStringProp write SetWideStringProp stored False;
    property Description: WideString index 22 read GetWideStringProp write SetWideStringProp stored False;
    property ID: Integer index 23 read GetIntegerProp write SetIntegerProp stored False;
    property MCBTime: TDateTime index 29 read GetTDateTimeProp write SetTDateTimeProp stored False;
    property ActiveLength: Integer index 30 read GetIntegerProp write SetIntegerProp stored False;
    property UMCBIHandle: Integer index 32 read GetIntegerProp write SetIntegerProp stored False;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TULIST
// Help String      : UMCBI List Control
// Default Interface: _DUMCBILIST
// Def. Intf. DISP? : Yes
// Event   Interface: _DUMCBILISTEvents
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TULIST = class(TOleControl)
  private
    FOnNewSelection: TNotifyEvent;
    FIntf: _DUMCBILIST;
    function  GetControlInterface: _DUMCBILIST;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function Get_SelIsFeature(FeatureNumber: MIOFEATURES): WordBool;
  public
    function AddSelItem(MasterIndex: Integer): WordBool;
    function DeleteSelItem: WordBool;
    function CreateList: WordBool;
    property  ControlInterface: _DUMCBILIST read GetControlInterface;
    property  DefaultInterface: _DUMCBILIST read GetControlInterface;
    property OutOfDate: WordBool index 8 read GetWordBoolProp;
    property SelName: WideString index 9 read GetWideStringProp;
    property SelID: Integer index 10 read GetIntegerProp;
    property SelMCB: Integer index 11 read GetIntegerProp;
    property SelInput: Integer index 12 read GetIntegerProp;
    property SelHost: WideString index 13 read GetWideStringProp;
    property SelType: WideString index 14 read GetWideStringProp;
    property SelIsFeature[FeatureNumber: MIOFEATURES]: WordBool read Get_SelIsFeature;
  published
    property Anchors;
    property PersistSel: WordBool index 1 read GetWordBoolProp write SetWordBoolProp stored False;
    property SelAddress: WideString index 2 read GetWideStringProp write SetWideStringProp stored False;
    property SelAddressAlt: WideString index 3 read GetWideStringProp write SetWideStringProp stored False;
    property SelIndex: Integer index 4 read GetIntegerProp write SetIntegerProp stored False;
    property ShowIDs: WordBool index 5 read GetWordBoolProp write SetWordBoolProp stored False;
    property ShowBuffer: WordBool index 6 read GetWordBoolProp write SetWordBoolProp stored False;
    property ListName: WideString index 7 read GetWideStringProp write SetWideStringProp stored False;
    property FireOnInternalSelChange: WordBool index 15 read GetWordBoolProp write SetWordBoolProp stored False;
    property MaxSelection: Integer index 16 read GetIntegerProp write SetIntegerProp stored False;
    property VerMajor: Smallint index 17 read GetSmallintProp write SetSmallintProp stored False;
    property VerMinor: Smallint index 18 read GetSmallintProp write SetSmallintProp stored False;
    property OnNewSelection: TNotifyEvent read FOnNewSelection write FOnNewSelection;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TUBOX
// Help String      : UMCBI ListBox Control
// Default Interface: _DUMCBIBOX
// Def. Intf. DISP? : Yes
// Event   Interface: _DUMCBIBOXEvents
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TUBOX = class(TOleControl)
  private
    FOnNewSelection: TNotifyEvent;
    FOnDoubleClick: TNotifyEvent;
    FIntf: _DUMCBIBOX;
    function  GetControlInterface: _DUMCBIBOX;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function Get_SelIsFeature(FeatureNumber: MIOFEATURES): WordBool;
  public
    function AddSelItem(MasterIndex: Integer): WordBool;
    function DeleteSelItem: WordBool;
    function CreateList: WordBool;
    property  ControlInterface: _DUMCBIBOX read GetControlInterface;
    property  DefaultInterface: _DUMCBIBOX read GetControlInterface;
    property OutOfDate: WordBool index 8 read GetWordBoolProp;
    property SelName: WideString index 9 read GetWideStringProp;
    property SelID: Integer index 10 read GetIntegerProp;
    property SelMCB: Integer index 11 read GetIntegerProp;
    property SelInput: Integer index 12 read GetIntegerProp;
    property SelHost: WideString index 13 read GetWideStringProp;
    property SelType: WideString index 14 read GetWideStringProp;
    property SelIsFeature[FeatureNumber: MIOFEATURES]: WordBool read Get_SelIsFeature;
  published
    property Anchors;
    property  TabStop;
    property  Align;
    property  DragCursor;
    property  DragMode;
    property  ParentShowHint;
    property  PopupMenu;
    property  ShowHint;
    property  TabOrder;
    property  Visible;
    property  OnDragDrop;
    property  OnDragOver;
    property  OnEndDrag;
    property  OnEnter;
    property  OnExit;
    property  OnStartDrag;
    property PersistSel: WordBool index 1 read GetWordBoolProp write SetWordBoolProp stored False;
    property SelAddress: WideString index 2 read GetWideStringProp write SetWideStringProp stored False;
    property SelAddressAlt: WideString index 3 read GetWideStringProp write SetWideStringProp stored False;
    property SelIndex: Integer index 4 read GetIntegerProp write SetIntegerProp stored False;
    property ShowIDs: WordBool index 5 read GetWordBoolProp write SetWordBoolProp stored False;
    property ShowBuffer: WordBool index 6 read GetWordBoolProp write SetWordBoolProp stored False;
    property ListName: WideString index 7 read GetWideStringProp write SetWideStringProp stored False;
    property FireOnInternalSelChange: WordBool index 15 read GetWordBoolProp write SetWordBoolProp stored False;
    property MaxSelection: Integer index 16 read GetIntegerProp write SetIntegerProp stored False;
    property VerMajor: Smallint index 17 read GetSmallintProp write SetSmallintProp stored False;
    property VerMinor: Smallint index 18 read GetSmallintProp write SetSmallintProp stored False;
    property Border: WordBool index 19 read GetWordBoolProp write SetWordBoolProp stored False;
    property LocalOnly: WordBool index 20 read GetWordBoolProp write SetWordBoolProp stored False;
    property RemoteOnly: WordBool index 21 read GetWordBoolProp write SetWordBoolProp stored False;
    property OnNewSelection: TNotifyEvent read FOnNewSelection write FOnNewSelection;
    property OnDoubleClick: TNotifyEvent read FOnDoubleClick write FOnDoubleClick;
  end;


// *********************************************************************//
// OLE Control Proxy class declaration
// Control Name     : TUDROP
// Help String      : UMCBI DropList Control
// Default Interface: _DUMCBIDROP
// Def. Intf. DISP? : Yes
// Event   Interface: _DUMCBIDROPEvents
// TypeFlags        : (34) CanCreate Control
// *********************************************************************//
  TUDROP = class(TOleControl)
  private
    FOnNewSelection: TNotifyEvent;
    FIntf: _DUMCBIDROP;
    function  GetControlInterface: _DUMCBIDROP;
  protected
    procedure CreateControl;
    procedure InitControlData; override;
    function Get_SelIsFeature(FeatureNumber: MIOFEATURES): WordBool;
  public
    function AddSelItem(MasterIndex: Integer): WordBool;
    function DeleteSelItem: WordBool;
    function CreateList: WordBool;
    property  ControlInterface: _DUMCBIDROP read GetControlInterface;
    property  DefaultInterface: _DUMCBIDROP read GetControlInterface;
    property OutOfDate: WordBool index 8 read GetWordBoolProp;
    property SelName: WideString index 9 read GetWideStringProp;
    property SelID: Integer index 10 read GetIntegerProp;
    property SelMCB: Integer index 11 read GetIntegerProp;
    property SelInput: Integer index 12 read GetIntegerProp;
    property SelHost: WideString index 13 read GetWideStringProp;
    property SelType: WideString index 14 read GetWideStringProp;
    property SelIsFeature[FeatureNumber: MIOFEATURES]: WordBool read Get_SelIsFeature;
  published
    property Anchors;
    property  TabStop;
    property  Align;
    property  DragCursor;
    property  DragMode;
    property  ParentShowHint;
    property  PopupMenu;
    property  ShowHint;
    property  TabOrder;
    property  Visible;
    property  OnDragDrop;
    property  OnDragOver;
    property  OnEndDrag;
    property  OnEnter;
    property  OnExit;
    property  OnStartDrag;
    property PersistSel: WordBool index 1 read GetWordBoolProp write SetWordBoolProp stored False;
    property SelAddress: WideString index 2 read GetWideStringProp write SetWideStringProp stored False;
    property SelAddressAlt: WideString index 3 read GetWideStringProp write SetWideStringProp stored False;
    property SelIndex: Integer index 4 read GetIntegerProp write SetIntegerProp stored False;
    property ShowIDs: WordBool index 5 read GetWordBoolProp write SetWordBoolProp stored False;
    property ShowBuffer: WordBool index 6 read GetWordBoolProp write SetWordBoolProp stored False;
    property ListName: WideString index 7 read GetWideStringProp write SetWideStringProp stored False;
    property FireOnInternalSelChange: WordBool index 15 read GetWordBoolProp write SetWordBoolProp stored False;
    property MaxSelection: Integer index 16 read GetIntegerProp write SetIntegerProp stored False;
    property VerMajor: Smallint index 17 read GetSmallintProp write SetSmallintProp stored False;
    property VerMinor: Smallint index 18 read GetSmallintProp write SetSmallintProp stored False;
    property OnNewSelection: TNotifyEvent read FOnNewSelection write FOnNewSelection;
  end;

procedure Register;

resourcestring
  dtlServerPage = 'ActiveX';

  dtlOcxPage = 'ActiveX';

implementation

uses ComObj;

procedure TUCONN2.InitControlData;
const
  CControlData: TControlData2 = (
    ClassID: '{731B1F14-C7BF-46CB-8C20-B03802871E6E}';
    EventIID: '';
    EventCount: 0;
    EventDispIDs: nil;
    LicenseKey: nil (*HR:$80004005*);
    Flags: $00000000;
    Version: 401);
begin
  ControlData := @CControlData;
end;

procedure TUCONN2.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as _DUMCBICONN2;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TUCONN2.GetControlInterface: _DUMCBICONN2;
begin
  CreateControl;
  Result := FIntf;
end;

function TUCONN2.Get_Data(Index: Integer): Integer;
begin
    Result := DefaultInterface.Data[Index];
end;

function TUCONN2.Get_RawData(Index: Integer): Integer;
begin
    Result := DefaultInterface.RawData[Index];
end;

function TUCONN2.Get_ROIData(Index: Integer): Integer;
begin
    Result := DefaultInterface.ROIData[Index];
end;

function TUCONN2.Get_IsFeature(FeatureNumber: MIOFEATURES): WordBool;
begin
    Result := DefaultInterface.IsFeature[FeatureNumber];
end;

function TUCONN2.GetData(Start: Integer; Length: Integer): OleVariant;
begin
  Result := DefaultInterface.GetData(Start, Length);
end;

function TUCONN2.GetRawData(Start: Integer; Length: Integer): OleVariant;
begin
  Result := DefaultInterface.GetRawData(Start, Length);
end;

function TUCONN2.GetROIData(Start: Integer; Length: Integer): OleVariant;
begin
  Result := DefaultInterface.GetROIData(Start, Length);
end;

procedure TUCONN2.LockInput(const LockOwner: WideString);
begin
  DefaultInterface.LockInput(LockOwner);
end;

procedure TUCONN2.UnlockInput;
begin
  DefaultInterface.UnlockInput;
end;

procedure TUCONN2.SetROI(Start: Integer; Length: Integer);
begin
  DefaultInterface.SetROI(Start, Length);
end;

procedure TUCONN2.ClearROI(Start: Integer; Length: Integer);
begin
  DefaultInterface.ClearROI(Start, Length);
end;

procedure TUCONN2.Open;
begin
  DefaultInterface.Open;
end;

procedure TUCONN2.Close;
begin
  DefaultInterface.Close;
end;

function TUCONN2.Comm(const Command: WideString): WideString;
begin
  Result := DefaultInterface.Comm(Command);
end;

procedure TUCONN2.SignalDevice(const Message: WideString);
begin
  DefaultInterface.SignalDevice(Message);
end;

procedure TUCONN2.EndSignalDevice;
begin
  DefaultInterface.EndSignalDevice;
end;

procedure TUCONN2.SetROIEx(Start: Integer; Length: Integer; View: Integer);
begin
  DefaultInterface.SetROIEx(Start, Length, View);
end;

procedure TUCONN2.ClearROIEx(Start: Integer; Length: Integer; View: Integer);
begin
  DefaultInterface.ClearROIEx(Start, Length, View);
end;

procedure TUCONN2.SetData(Start: Integer; Length: Integer; View: Integer; Data: OleVariant);
begin
  DefaultInterface.SetData(Start, Length, View, Data);
end;

procedure TULIST.InitControlData;
const
  CEventDispIDs: array [0..0] of DWORD = (
    $00000001);
  CControlData: TControlData2 = (
    ClassID: '{D87EF4B6-1ED8-11D3-A1B1-002078147A97}';
    EventIID: '{D87EF4B5-1ED8-11D3-A1B1-002078147A97}';
    EventCount: 1;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil (*HR:$80004005*);
    Flags: $00000000;
    Version: 401);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := Cardinal(@@FOnNewSelection) - Cardinal(Self);
end;

procedure TULIST.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as _DUMCBILIST;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TULIST.GetControlInterface: _DUMCBILIST;
begin
  CreateControl;
  Result := FIntf;
end;

function TULIST.Get_SelIsFeature(FeatureNumber: MIOFEATURES): WordBool;
begin
    Result := DefaultInterface.SelIsFeature[FeatureNumber];
end;

function TULIST.AddSelItem(MasterIndex: Integer): WordBool;
begin
  Result := DefaultInterface.AddSelItem(MasterIndex);
end;

function TULIST.DeleteSelItem: WordBool;
begin
  Result := DefaultInterface.DeleteSelItem;
end;

function TULIST.CreateList: WordBool;
begin
  Result := DefaultInterface.CreateList;
end;

procedure TUBOX.InitControlData;
const
  CEventDispIDs: array [0..1] of DWORD = (
    $00000001, $00000002);
  CControlData: TControlData2 = (
    ClassID: '{69AA4F5F-4685-42CC-BF1F-0CD456E87A51}';
    EventIID: '{69AA4F61-4685-42CC-BF1F-0CD456E87A51}';
    EventCount: 2;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil (*HR:$80004005*);
    Flags: $00000000;
    Version: 401);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := Cardinal(@@FOnNewSelection) - Cardinal(Self);
end;

procedure TUBOX.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as _DUMCBIBOX;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TUBOX.GetControlInterface: _DUMCBIBOX;
begin
  CreateControl;
  Result := FIntf;
end;

function TUBOX.Get_SelIsFeature(FeatureNumber: MIOFEATURES): WordBool;
begin
    Result := DefaultInterface.SelIsFeature[FeatureNumber];
end;

function TUBOX.AddSelItem(MasterIndex: Integer): WordBool;
begin
  Result := DefaultInterface.AddSelItem(MasterIndex);
end;

function TUBOX.DeleteSelItem: WordBool;
begin
  Result := DefaultInterface.DeleteSelItem;
end;

function TUBOX.CreateList: WordBool;
begin
  Result := DefaultInterface.CreateList;
end;

procedure TUDROP.InitControlData;
const
  CEventDispIDs: array [0..0] of DWORD = (
    $00000001);
  CControlData: TControlData2 = (
    ClassID: '{D87EF4BA-1ED8-11D3-A1B1-002078147A97}';
    EventIID: '{D87EF4B9-1ED8-11D3-A1B1-002078147A97}';
    EventCount: 1;
    EventDispIDs: @CEventDispIDs;
    LicenseKey: nil (*HR:$80004005*);
    Flags: $00000000;
    Version: 401);
begin
  ControlData := @CControlData;
  TControlData2(CControlData).FirstEventOfs := Cardinal(@@FOnNewSelection) - Cardinal(Self);
end;

procedure TUDROP.CreateControl;

  procedure DoCreate;
  begin
    FIntf := IUnknown(OleObject) as _DUMCBIDROP;
  end;

begin
  if FIntf = nil then DoCreate;
end;

function TUDROP.GetControlInterface: _DUMCBIDROP;
begin
  CreateControl;
  Result := FIntf;
end;

function TUDROP.Get_SelIsFeature(FeatureNumber: MIOFEATURES): WordBool;
begin
    Result := DefaultInterface.SelIsFeature[FeatureNumber];
end;

function TUDROP.AddSelItem(MasterIndex: Integer): WordBool;
begin
  Result := DefaultInterface.AddSelItem(MasterIndex);
end;

function TUDROP.DeleteSelItem: WordBool;
begin
  Result := DefaultInterface.DeleteSelItem;
end;

function TUDROP.CreateList: WordBool;
begin
  Result := DefaultInterface.CreateList;
end;

procedure Register;
begin
  RegisterComponents(dtlOcxPage, [TUCONN2, TULIST, TUBOX, TUDROP]);
end;

end.
