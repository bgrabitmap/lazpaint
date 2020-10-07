unit Tablet;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

{********************************************}
{  This unit is a part of TTablet component  }
{  Copyright (C) 2001-2002 Mattias Andersson }
{  See COPYING.txt for license information   }
{  Last modified: 2003-10-25; version 1.24   }
{********************************************}
{  Modified by Nelson Chu in 2013 to work    }
{  with Lazarus.                             }
{********************************************}

interface

uses
  Windows, SysUtils, Messages, Classes, WintabConsts, Win32Int;

type
  HCtx = type Integer;
  TFixed = type Integer;
  TWTPkt = type Integer;

  PAxis = ^TAxis;
  TAxis = record
    Min: Integer;
    Max: Integer;
  	Units: Cardinal;
  	Resolution: TFixed;
  end;

  POrientation = ^TOrientation;
  TOrientation = record
    Azimuth: Integer;
    Altitude: Integer;
    Twist: Integer;
  end;

  PRotation = ^TRotation;
  TRotation = record
    Pitch: Integer;
    Roll: Integer;
    Yaw: Integer;
  end;

  PLogContext = ^TLogContext;
  TLogContext = record
    Name: string[39];
    Options: Cardinal;
    Status: Cardinal;
    Locks: Cardinal;
    MsgBase: Cardinal;
    Device: Cardinal;
    PktRate: Cardinal;
    PktData: TWTPkt;
    PktMode: TWTPkt;
    MoveMask: TWTPkt;
    BtnDnMask: Integer;
    BtnUpMask: Integer;
    InOrgX: Integer;
    InOrgY: Integer;
    InOrgZ: Integer;
    InExtX: Integer;
    InExtY: Integer;
    InExtZ: Integer;
    OutOrgX: Integer;
    OutOrgY: Integer;
    OutOrgZ: Integer;
    OutExtX: Integer;
    OutExtY: Integer;
    OutExtZ: Integer;
    SensX: TFixed;
    SensY: TFixed;
    SensZ: TFixed;
    SysMode: Boolean;
    SysOrgX: Integer;
    SysOrgY: Integer;
    SysExtX: Integer;
    SysExtY: Integer;
    SysSensX: TFixed;
    SysSensY: TFixed;
  end;

  TPacketEvent = procedure(Sender: TObject; PacketNumber, ContextHandle: HCtx;
    pPacket: Pointer) of object;
  TProximityEvent = procedure(Sender: TObject; ContextHandle: HCtx;
    Entering: Boolean) of object;
  TInfoChangedEvent = procedure(Sender: TObject; Manager: THandle;
    Category, Index: Word) of object;
  TContextEvent =  procedure(Sender: TObject; ContextHandle, StatusFlags: Integer) of object;

  { TCustomTablet}
  { This component provides access to digitizer hardware }
  TCustomTablet = class(TComponent)
  private
    FOnPacket: TPacketEvent;
    FOnCursorChanged: TPacketEvent;
    FOnProximity: TProximityEvent;
    FOnInfoChanged: TInfoChangedEvent;
    FOnOpenContext: TContextEvent;
    FOnCloseContext: TContextEvent;
    FOnOverlapContext: TContextEvent;
    FOnUpdateContext: TContextEvent;
    FPacketSize: Byte;
    FPresent: Boolean;
    FPPacket: Pointer;
    FWindowHandle: THandle;

    FWin32WidgetSet: TWin32WidgetSet;

    WTHandle: THandle;
    procedure WndProc(var Msg: TMessage);
    procedure SetPacketSize(Size: Byte);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Present: Boolean read FPresent;
    property PacketSize: Byte read FPacketSize write SetPacketSize;
  published
    property OnContextClose: TContextEvent read FOnCloseContext write FOnCloseContext;
    property OnContextOpen: TContextEvent read FOnOpenContext write FOnOpenContext;
    property OnContextOverlap: TContextEvent read FOnOverlapContext write FOnOverlapContext;
    property OnContextUpdate: TContextEvent read FOnUpdateContext write FOnUpdateContext;
    property OnCursorChanged: TPacketEvent read FOnCursorChanged write FOnCursorChanged;
    property OnInfoChanged: TInfoChangedEvent read FOnInfoChanged write FOnInfoChanged;
    property OnPacket: TPacketEvent read FOnPacket write FOnPacket;
    property OnProximity: TProximityEvent read FOnProximity write FOnProximity;
  end;

  { TTablet }
  TTablet = class(TCustomTablet)
  private
    FContextHandle: HCtx;
    FEnabled: Boolean;
    function GetDeviceName: string;
    function GetWintabID: string;
    function GetPressure: TAxis;
    function GetPacketRate: Integer;
    function GetContext: TLogContext;
    procedure SetContext(Context: TLogContext);
    procedure SetEnabled(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Open(Context: TLogContext);
    procedure Close;
    property Context: TLogContext read GetContext write SetContext;
    property ContextHandle: HCtx read FContextHandle write FContextHandle;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Pressure: TAxis read GetPressure;
  published
    property DeviceName: string read GetDeviceName;
    property WintabID: string read GetWintabID;
    property PacketRate: Integer read GetPacketRate;
  end;
  
{ Aliased functions (Can change these to unicode by changing the A to W in Alias name) }
    tWTInfo = function (wCategory: Cardinal; nIndex: Cardinal; lpOutput: Pointer): Cardinal; stdcall;
    { Used to read dispidious pieces of information about the tablet. }

    tWTOpen = function (hWnd: HWnd; var LPLogContext: TLogContext; fEnable: LongBool): Integer; stdcall;
    { Used to begin accessing the Tablet. }

    tWTGet = function (hCtx: HCtx; var LPLogContext: TLogContext): LongBool; stdcall;
    { Fills the supplied structure with the current context attributes for the passed handle. }

    tWTSet = function (hCtx: HCtx; var LPLogContext: TLogContext): LongBool; stdcall;
    { Allows some of the context's attributes to be changed on the fly. }

{ Basic functions }

    tWTClose = function (hCtx: HCtx): LongBool; stdcall;
    { Used to end accessing the Tablet. }

    tWTPacketsGet = function (hCtx: HCtx; cMaxPackets: Integer; lpPkts: Pointer): Integer; stdcall;
    { Used to poll the Tablet for input. }

    tWTPacket = function (hCtx: HCtx; wSerial: Cardinal; lpPkts: Pointer): LongBool; stdcall;
    { Similar to WTPacketsGet but is used in a window function. }

{ Visibility functions }
    tWTEnable = function (hCtx: HCtx; fEnable: LongBool): LongBool; stdcall;
    { Enables and Disables a Tablet Context; temporarily turning on or off the processing of packets. }

    tWTOverlap = function (hCtx: HCtx; fToTop: LongBool): LongBool; stdcall;
    { Sends a tablet context to the top or bottom of the order of overlapping tablet contexts. }

{ Context Editing functions }
    tWTConfig = function (hCtx: HCtx; hWnd: HWnd): LongBool; stdcall;
    { Used to call a requestor which aids in configuring the Tablet }

    tWTExtGet = function (hCtx: HCtx; wExt: Cardinal; lpData: Pointer): Integer; stdcall;
    tWTExtSet = function (hCtx: HCtx; wExt: Cardinal; lpData: Pointer): Integer; stdcall;

    tWTSave = function (hCtx: HCtx; lpSaveInfo: Pointer): Integer; stdcall;
    { Fills the supplied buffer with binary save information that can be used to restore the equivalent context in a subsequent Windows session. }

    tWTRestore = function (hWnd: HWnd; lpSaveInfo: Pointer; fEnable: Integer): Integer; stdcall;
    { Creates a tablet context from the save information returned from the WTSave function. }

{ Advanced Packet and Queue functions }
    tWTPacketsPeek = function (hCtx: HCtx; cMaxPackets: Integer; lpPkts: Pointer): Integer; stdcall;
    tWTDataGet = function (hCtx: HCtx; wBegin: Cardinal; wEnding: Cardinal;
      cMaxPackets: Integer; lpPkts: Pointer; lpNPkts: Pointer): Integer; stdcall;
    tWTDataPeek = function (hCtx: HCtx; wBegin: Cardinal; wEnding: Cardinal;
      cMaxPackets: Integer; lpPkts: Pointer; lpNPkts: Pointer): Integer; stdcall;
    tWTQueuePacketsEx = function (hCtx: HCtx; var lpOld: Pointer; var lpNew: Pointer): Integer; stdcall;
    { Returns the serial numbers of the oldest and newest packets currently in the queue. }

    tWTQueueSizeGet = function (hCtx: Integer): Integer; stdcall;
    tWTQueueSizeSet = function (hCtx: Integer; nPkts: Integer): Integer; stdcall;

{ Manager functions }

    { Manager Handle functions }
        tWTMgrOpen = function (HWnd: Integer; wMsgBase: Cardinal): Integer; stdcall;
        tWTMgrClose = function (hMgr: Integer): LongBool; stdcall;

    { Manager Context functions }
        tWTMgrContextEnum = function (hMgr: Integer; lpEnumFunc: Pointer; lParam: Pointer): Integer; stdcall;
        tWTMgrContextOwner = function (hMgr: Integer; hCtx: HCtx): Integer; stdcall;
        tWTMgrDefContext = function (hMgr: Integer; fSystem: LongBool): Integer; stdcall;
        tWTMgrDefContextEx = function (hMgr: Integer; wDevice: Cardinal; fSystem: LongBool): Integer; stdcall;

    { Manager Configuration functions }
        tWTMgrDeviceConfig = function (hMgr: Integer; var wDevice: Cardinal; HWnd: Integer): Cardinal; stdcall;
        tWTMgrConfigReplaceExA = function (hMgr: Integer; fInstall: LongBool; lpszModule: Pointer; lpszCfgProc: Pointer): Integer; stdcall;
        tWTMgrConfigReplaceExW = function (hMgr: Integer; fInstall: LongBool; lpszModule: Pointer; lpszCfgProc: Pointer): Integer; stdcall;

    { Manager Packet Hook functions }
        tWTMgrPacketHookExA = function (hMgr: Integer; nType: Integer; lpszModule: Pointer; lpszHookProc: Pointer): Integer; stdcall;
        tWTMgrPacketHookExW = function (hMgr: Integer; nType: Integer; lpszModule: Pointer; lpszHookProc: Pointer): Integer; stdcall;
        tWTMgrPacketUnHook = function (hHook: Integer): Integer; stdcall;
        tWTMgrPacketHookNext = function (hHook: Integer; nCode: Integer; wParam: Pointer; var lParam: Pointer): Integer; stdcall;

    { Manager Preference Data functions }
        tWTMgrExt = function (hMgr: Integer; wExt: Cardinal; lpData: Pointer): LongBool; stdcall;
        tWTMgrCsrEnable = function (hMgr: Integer; wCursor: Cardinal; fEnable: LongBool): Integer; stdcall;
        tWTMgrCsrButtonMap = function (hMgr: Integer; wCursor: Cardinal; lpLogBtns: Pointer; lpSysBtns: Pointer): Integer; stdcall;
        tWTMgrCsrPressureBtnMarksEx = function (hMgr: Integer; wCursor: Cardinal; lpNMarks: Pointer; lpTMarks: Pointer): Integer; stdcall;
        tWTMgrCsrPressureResponse = function (hMgr: Integer; wCursor: Cardinal; lpNResp: Pointer; lpTResp: Pointer): Integer; stdcall;
        tWTMgrCsrExt = function (hMgr: Integer; wCursor: Cardinal; wExt: Cardinal; var lpData: Pointer): Integer; stdcall;

  procedure DefaultContext(var Context: TLogContext);
  function GetInfoAsString(Category, Index: Cardinal): string;
  function GetInfoAsInteger(Category, Index: Cardinal): Integer;

  procedure Register;

resourcestring
  OpenFailed = 'Tablet context failed to open.';
  GetProcFailed = 'Procedure %s failed to load properly.';

{ Procedural Variable Definitions }

var
  WTInfo: tWTInfo;
  WTOpen: tWTOpen;
  WTGet: tWTGet;
  WTSet: tWTSet;
  WTClose: tWTClose;
  WTPacketsGet: tWTPacketsGet;
  WTPacket: tWTPacket;
  WTEnable: tWTEnable;
  WTOverlap: tWTOverlap;
  WTConfig: tWTConfig;
  WTExtGet: tWTExtGet;
  WTExtSet: tWTExtSet;
  WTSave: tWTSave;
  WTRestore: tWTRestore;
  WTPacketsPeek: tWTPacketsPeek;
  WTDataGet: tWTDataGet;
  WTDataPeek: tWTDataPeek;
  WTQueuePacketsEx: tWTQueuePacketsEx;
  WTQueueSizeGet: tWTQueueSizeGet;
  WTQueueSizeSet: tWTQueueSizeSet;

  WTMgrOpen: tWTMgrOpen;
  WTMgrClose: tWTMgrClose;

  WTMgrContextEnum: tWTMgrContextEnum;
  WTMgrContextOwner: tWTMgrContextOwner;
  WTMgrDefContext: tWTMgrDefContext;
  WTMgrDefContextEx: tWTMgrDefContextEx;

  WTMgrDeviceConfig: tWTMgrDeviceConfig;
  WTMgrConfigReplaceExA: tWTMgrConfigReplaceExA;
  WTMgrConfigReplaceExW: tWTMgrConfigReplaceExW;

  WTMgrPacketHookExA: tWTMgrPacketHookExA;
  WTMgrPacketHookExW: tWTMgrPacketHookExW;
  WTMgrPacketUnHook: tWTMgrPacketUnHook;
  WTMgrPacketHookNext: tWTMgrPacketHookNext;

  WTMgrExt: tWTMgrExt;
  WTMgrCsrEnable: tWTMgrCsrEnable;
  WTMgrCsrButtonMap: tWTMgrCsrButtonMap;
  WTMgrCsrPressureBtnMarksEx: tWTMgrCsrPressureBtnMarksEx;
  WTMgrCsrPressureResponse: tWTMgrCsrPressureResponse;
  WTMgrCsrExt: tWTMgrCsrExt;

implementation

procedure Register;
begin
  RegisterComponents('System', [TTablet]);
end;

function GetInfoAsString(Category, Index: Cardinal): string;
var
  P: PChar;
begin
  GetMem(P,  WTInfo(Category, Index, nil));
  WTInfo(Category, Index, P);
  Result := StrPas(P);
  FreeMem(P);
end;

function GetInfoAsInteger(Category, Index: Cardinal): Integer;
begin
  WTInfo(Category, Index, @Result);
end;

procedure DefaultContext(var Context: TLogContext);
{ Establish contact and fill context with defaults }
begin
  WTInfo(WTI_DEFCONTEXT, 0, @Context);
end;

//----------------------------------------------------------------------------//

{ TCustomTablet }

procedure TCustomTablet.WndProc(var Msg: TMessage);
begin
  with Msg do
  begin
    case Msg of
      WT_CTXCLOSE:
        if Assigned(FOnCloseContext) then
          FOnCloseContext(Self, WParam, LParam);
      WT_CTXOPEN:
        if Assigned(FOnOpenContext) then
          FOnOpenContext(Self, WParam, LParam);
      WT_CTXOVERLAP:
        if Assigned(FOnOverlapContext) then
          FOnOverlapContext(Self, WParam, LParam);
      WT_CTXUPDATE:
        if Assigned(FOnUpdateContext) then
          FOnUpdateContext(Self, WParam, LParam);
      WT_CSRCHANGE:
        if Assigned(FOnCursorChanged) then
          if WTPacket(LParam, WParam, FPPacket) then
            FOnCursorChanged(Self, WParam, LParam, FPPacket);
      WT_INFOCHANGE:
        if Assigned(FOnInfoChanged) then
          FOnInfoChanged(Self, WParam, LParamLo, LParamHi);
      WT_PACKET:
        if Assigned(FOnPacket)  then
          if WTPacket(LParam, WParam, FPPacket) then
            FOnPacket(Self, WParam, LParam, FPPacket);
      WT_PROXIMITY:
        if Assigned(FOnProximity) then
          FOnProximity(Self, WParam, LParamLo <> 0);
    else
    { Pass any other messages to DefWindowProc }
      Result := DefWindowProc(FWindowHandle, Msg, wParam, lParam);
    end;
  end;
end;

constructor TCustomTablet.Create(AOwner: TComponent);

  procedure ValidateProc(var ProcPtr: Pointer; const ProcName: string);
  begin
    ProcPtr := GetProcAddress(wtHandle, PChar(ProcName));
    if not Assigned(ProcPtr) then
      raise Exception.CreateFmt(GetProcFailed, [ProcName]);
  end;

var
   r : Cardinal;

begin
  inherited Create(AOwner);
  if csDesigning in ComponentState then Exit;

  wtHandle := LoadLibrary('wintab32.dll');
  FPresent := wtHandle <> 0;

  if not FPresent then Exit;

  ValidateProc(@WTInfo,                     'WTInfoA');
  ValidateProc(@WTOpen,                     'WTOpenA');
  ValidateProc(@WTGet,                      'WTGetA');
  ValidateProc(@WTSet,                      'WTSetA');
  ValidateProc(@WTClose,                    'WTClose');
  ValidateProc(@WTPacketsGet,               'WTPacketsGet');
  ValidateProc(@WTPacket,                   'WTPacket');
  ValidateProc(@WTEnable,                   'WTEnable');
  ValidateProc(@WTOverlap,                  'WTOverlap');
  ValidateProc(@WTConfig,                   'WTConfig');
  ValidateProc(@WTExtGet,                   'WTExtGet');
  ValidateProc(@WTExtSet,                   'WTExtSet');
  ValidateProc(@WTSave,                     'WTSave');
  ValidateProc(@WTRestore,                  'WTRestore');
  ValidateProc(@WTPacketsPeek,              'WTPacketsPeek');
  ValidateProc(@WTDataGet,                  'WTDataGet');
  ValidateProc(@WTDataPeek,                 'WTDataPeek');
  ValidateProc(@WTQueuePacketsEx,           'WTQueuePacketsEx');
  ValidateProc(@WTQueueSizeGet,             'WTQueueSizeGet');
  ValidateProc(@WTQueueSizeSet,             'WTQueueSizeSet');
  ValidateProc(@WTMgrOpen,                  'WTMgrOpen');
  ValidateProc(@WTMgrClose,                 'WTMgrClose');
  ValidateProc(@WTMgrContextEnum,           'WTMgrContextEnum');
  ValidateProc(@WTMgrContextOwner,          'WTMgrContextOwner');
  ValidateProc(@WTMgrDefContext,            'WTMgrDefContext');
  ValidateProc(@WTMgrDefContextEx,          'WTMgrDefContextEx');
  ValidateProc(@WTMgrDeviceConfig,          'WTMgrDeviceConfig');
  ValidateProc(@WTMgrConfigReplaceExA,      'WTMgrConfigReplaceExA');
  ValidateProc(@WTMgrConfigReplaceExW,      'WTMgrConfigReplaceExW');
  ValidateProc(@WTMgrPacketHookExA,         'WTMgrPacketHookExA');
  ValidateProc(@WTMgrPacketHookExW,         'WTMgrPacketHookExW');
  ValidateProc(@WTMgrPacketUnhook,          'WTMgrPacketUnhook');
  ValidateProc(@WTMgrPacketHookNext,        'WTMgrPacketHookNext');
  ValidateProc(@WTMgrExt,                   'WTMgrExt');
  ValidateProc(@WTMgrCsrEnable,             'WTMgrCsrEnable');
  ValidateProc(@WTMgrCsrButtonMap,          'WTMgrCsrButtonMap');
  ValidateProc(@WTMgrCsrPressureBtnMarksEx, 'WTMgrCsrPressureBtnMarksEx');
  ValidateProc(@WTMgrCsrPressureResponse,   'WTMgrCsrPressureResponse');
  ValidateProc(@WTMgrCsrExt,                'WTMgrCsrExt');

  r := WTInfo(0, 0, nil);
  FPresent := r <> 0;
  if FPresent then
  begin
    FWin32WidgetSet := TWin32WidgetSet.Create;
    FWindowHandle := FWin32WidgetSet.AllocateHWnd(WndProc);
  end;
end;

destructor TCustomTablet.Destroy;
begin
  if FPresent then
  begin
    FWin32WidgetSet.DeallocateHWnd(FWindowHandle);
    FreeMem(FPPacket, FPacketSize + 1);
    FreeLibrary(wtHandle);
//    FWin32WidgetSet.Free;  // this call causes an exception. What is the proper way to use TWin32WidgetSet?
  end;
  inherited;
end;

procedure TCustomTablet.SetPacketSize(Size: Byte);
begin
  FPacketSize := Size;
  ReallocMem(FPPacket, Size + 1);
end;

//----------------------------------------------------------------------------//

{ TTablet }

procedure TTablet.Close;
begin
  if FPresent and (FContextHandle <> 0) then WTClose(FContextHandle);
end;

constructor TTablet.Create(AOwner: TComponent);
begin
  inherited;
  FEnabled := False;
  FContextHandle := 0;
end;

function TTablet.GetContext: TLogContext;
begin
  if FPresent and (FContextHandle <> 0) then WTGet(FContextHandle, {%H-}Result);
end;

function TTablet.GetDeviceName: string;
begin
  if FPresent then Result := GetInfoAsString(WTI_DEVICES, DVC_NAME);
end;

function TTablet.GetPacketRate: Integer;
begin
  if FPresent then WTInfo(WTI_STATUS, STA_PKTRATE, @Result);
end;

function TTablet.GetPressure: TAxis;
const
  EmptyAxis: TAxis = (Min: 0; Max: 0; Units:0; Resolution: 0);
begin
  Result := EmptyAxis;
  if FPresent then
    WTInfo(WTI_DEVICES, DVC_NPRESSURE, @Result);
end;

function TTablet.GetWintabID: string;
begin
  if FPresent then Result := GetInfoAsString(WTI_INTERFACE, IFC_WINTABID);
end;

procedure TTablet.Open(Context: TLogContext);
begin
  if FPresent then FContextHandle := WTOpen(FWindowHandle, Context, FEnabled);
end;

procedure TTablet.SetContext(Context: TLogContext);
begin
  if FPresent then WTSet(FContextHandle, Context);
end;

procedure TTablet.SetEnabled(Value: Boolean);
begin
  if FPresent then
  begin
    FEnabled := Value;
    if FContextHandle <> 0 then WtEnable(FContextHandle, Value);
  end;
end;

end.
