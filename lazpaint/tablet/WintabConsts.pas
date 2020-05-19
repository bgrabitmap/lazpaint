unit WintabConsts;

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

const
  WT_DEFBASE = $7FF0;
  WT_MAXOFFSET = $F;

  WT_PACKET = WT_DEFBASE + 0;
  WT_CTXOPEN = WT_DEFBASE + 1;
  WT_CTXCLOSE = WT_DEFBASE + 2;
  WT_CTXUPDATE = WT_DEFBASE + 3;
  WT_CTXOVERLAP = WT_DEFBASE + 4;
  WT_PROXIMITY = WT_DEFBASE + 5;
  WT_INFOCHANGE = WT_DEFBASE + 6;
  WT_CSRCHANGE = WT_DEFBASE + 7;
  WT_MAX = WT_DEFBASE + WT_MAXOFFSET;

//Packet constants
  PK_CONTEXT = $1;              // reporting context
  PK_STATUS = $2;               // status bits
  PK_TIME = $4;                 // time stamp
  PK_CHANGED = $8;              // change bit vector
  PK_SERIAL_NUMBER = $10;       // packet serial number
  PK_CURSOR = $20;              // reporting cursor
  PK_BUTTONS = $40;             // button information
  PK_X = $80;                   // x axis
  PK_Y = $100;                  // y axis
  PK_Z = $200;                  // z axis
  PK_NORMAL_PRESSURE = $400;    // normal or tip pressure
  PK_TANGENT_PRESSURE = $800;   // tangential or barrel pressure
  PK_ORIENTATION = $1000;       // orientation info: tilts
  PK_ROTATION = $2000;

//unit specifiers
  TU_NONE = 0;
  TU_INCHES = 1;
  TU_CENTIMETERS = 2;
  TU_CIRCLE = 3;

//System Button Assignment Values
  SBN_NONE = $0;
  SBN_LCLICK = $1;
  SBN_LDBLCLICK = $2;
  SBN_LDRAG = $3;
  SBN_RCLICK = $4;
  SBN_RDBLCLICK = $5;
  SBN_RDRAG = $6;
  SBN_MCLICK = $7;
  SBN_MDBLCLICK = $8;
  SBN_MDRAG = $9;

//Pen Windows Assignments
  SBN_PTCLICK = $10;
  SBN_PTDBLCLICK = $20;
  SBN_PTDRAG = $30;
  SBN_PNCLICK = $40;
  SBN_PNDBLCLICK = $50;
  SBN_PNDRAG = $60;
  SBN_P1CLICK = $70;
  SBN_P1DBLCLICK = $80;
  SBN_P1DRAG = $90;
  SBN_P2CLICK = $A0;
  SBN_P2DBLCLICK = $B0;
  SBN_P2DRAG = $C0;
  SBN_P3CLICK = $D0;
  SBN_P3DBLCLICK = $E0;
  SBN_P3DRAG = $F0;

//Hardware Capabilities
  HWC_INTEGRATED = $1;
  HWC_TOUCH = $2;
  HWC_HARDPROX = $4;
  HWC_PHYSID_CURSORS = $8;

//Cursor Capabilities
  CRC_MULTIMODE = $1;
  CRC_AGGREGATE = $2;
  CRC_INVERT = $4;

//Info Categories
  WTI_INTERFACE = 1;
  WTI_STATUS = 2;
  WTI_DEFCONTEXT = 3;
  WTI_DEFSYSCTX = 4;
  WTI_DEVICES = 100;
  WTI_CURSORS = 200;
  WTI_EXTENSIONS = 300;
  WTI_DDCTXS = 400;
  WTI_DSCTXS = 500;

    //Info Sub Catagories
    //WTI_INTERFACE
      IFC_WINTABID = 1;
      IFC_SPECVERSION = 2;
      IFC_IMPLVERSION = 3;
      IFC_NDEVICES = 4;
      IFC_NCURSORS = 5;
      IFC_NCONTEXTS = 6;
      IFC_CTXOPTIONS = 7;
      IFC_CTXSAVESIZE = 8;
      IFC_NEXTENSIONS = 9;
      IFC_NMANAGERS = 10;
      IFC_MAX = 10;

    //WTI_STATUS
      STA_CONTEXTS = 1;
      STA_SYSCTXS = 2;
      STA_PKTRATE = 3;
      STA_PKTDATA = 4;
      STA_MANAGERS = 5;
      STA_SYSTEM = 6;
      STA_BUTTONUSE = 7;
      STA_SYSBTNUSE = 8;
      STA_MAX = 8;

    //WTI_DEFCONTEXT
      CTX_NAME = 1;
      CTX_OPTIONS = 2;
      CTX_STATUS = 3;
      CTX_LOCKS = 4;
      CTX_MSGBASE = 5;
      CTX_DEVICE = 6;
      CTX_PKTRATE = 7;
      CTX_PKTDATA = 8;
      CTX_PKTMODE = 9;
      CTX_MOVEMASK = 10;
      CTX_BTNDNMASK = 11;
      CTX_BTNUPMASK = 12;
      CTX_INORGX = 13;
      CTX_INORGY = 14;
      CTX_INORGZ = 15;
      CTX_INEXTX = 16;
      CTX_INEXTY = 17;
      CTX_INEXTZ = 18;
      CTX_OUTORGX = 19;
      CTX_OUTORGY = 20;
      CTX_OUTORGZ = 21;
      CTX_OUTEXTX = 22;
      CTX_OUTEXTY = 23;
      CTX_OUTEXTZ = 24;
      CTX_SENSX = 25;
      CTX_SENSY = 26;
      CTX_SENSZ = 27;
      CTX_SYSMODE = 28;
      CTX_SYSORGX = 29;
      CTX_SYSORGY = 30;
      CTX_SYSEXTX = 31;
      CTX_SYSEXTY = 32;
      CTX_SYSSENSX = 33;
      CTX_SYSSENSY = 34;
      CTX_MAX = 34;

    //WTI_DEVICES
      DVC_NAME = 1;
      DVC_HARDWARE = 2;
      DVC_NCSRTYPES = 3;
      DVC_FIRSTCSR = 4;
      DVC_PKTRATE = 5;
      DVC_PKTDATA = 6;
      DVC_PKTMODE = 7;
      DVC_CSRDATA = 8;
      DVC_XMARGIN = 9;
      DVC_YMARGIN = 10;
      DVC_ZMARGIN = 11;
      DVC_X = 12;
      DVC_Y = 13;
      DVC_Z = 14;
      DVC_NPRESSURE = 15;
      DVC_TPRESSURE = 16;
      DVC_ORIENTATION = 17;
      DVC_ROTATION = 18;
      DVC_PNPID = 19;
      DVC_MAX = 19;

    //WTI_CURSORS
      CSR_NAME = 1;
      CSR_ACTIVE = 2;
      CSR_PKTDATA = 3;
      CSR_BUTTONS = 4;
      CSR_BUTTONBITS = 5;
      CSR_BTNNAMES = 6;
      CSR_BUTTONMAP = 7;
      CSR_SYSBTNMAP = 8;
      CSR_NPBUTTON = 9;
      CSR_NPBTNMARKS = 10;
      CSR_NPRESPONSE = 11;
      CSR_TPBUTTON = 12;
      CSR_TPBTNMARKS = 13;
      CSR_TPRESPONSE = 14;
      CSR_PHYSID = 15;
      CSR_MODE = 16;
      CSR_MINPKTDATA = 17;
      CSR_MINBUTTONS = 18;
      CSR_CAPABILITIES = 19;
      CSR_MAX = 19;
      CSR_TYPE = 20;

    //WTI_EXTENSIONS
      EXT_NAME = 1;
      EXT_TAG = 2;
      EXT_MASK = 3;
      EXT_SIZE = 4;
      EXT_AXES = 5;
      EXT_DEFAULT = 6;
      EXT_DEFCONTEXT = 7;
      EXT_DEFSYSCTX = 8;
      EXT_CURSORS = 9;
      EXT_MAX = 109;        // Allow 100 cursors

//context option values
  CXO_SYSTEM = $1;
  CXO_PEN = $2;
  CXO_MESSAGES = $4;
  CXO_MARGIN = $8000;
  CXO_MGNINSIDE = $4000;
  CXO_CSRMESSAGES = $8;

//context status values
  CXS_DISABLED = $1;
  CXS_OBSCURED = $2;
  CXS_ONTOP = $4;

//context lock values
  CXL_INSIZE = $1;
  CXL_INASPECT = $2;
  CXL_SENSITIVITY = $4;
  CXL_MARGIN = $8;
  CXL_SYSOUT = $10;

//Packet Status Values
  TPS_PROXIMITY = $1;
  TPS_QUEUE_ERR = $2;
  TPS_MARGIN = $4;
  TPS_GRAB = $8;
  TPS_INVERT = $10;

//relative buttons
  TBN_NONE = 0;
  TBN_UP = 1;
  TBN_DOWN = 2;


//device configuration constants
  WTDC_NONE = 0;
  WTDC_CANCEL = 1;
  WTDC_OK = 2;
  WTDC_RESTART = 3;

//hook constants
  WTH_PLAYBACK = 1;
  WTH_RECORD = 2;
  WTHC_GETLPLPFN = (-3);
  WTHC_LPLPFNNEXT = (-2);
  WTHC_LPFNNEXT = (-1);
  WTHC_ACTION = 0;
  WTHC_GETNEXT = 1;
  WTHC_SKIP = 2;

implementation

end.
