unit UStateType;

{$mode objfpc}

interface

uses
  Types, Classes, SysUtils, BGRABitmap, BGRABitmapTypes, BGRALayers;

const MinSizeToCompress = 512; //set to 1 if you want always compression
const MinSerializedSize = 16384;

type
  TState = class;

  { TStateDifference }

  TStateDifference = class
    function TryCompress: boolean; virtual;
    procedure ApplyTo(AState: TState); virtual; abstract;
    procedure UnapplyTo(AState: TState); virtual; abstract;
    function UsedMemory: int64; virtual;
  end;

  TState = class
    saved: boolean;
    procedure ApplyDifference(ADifference: TStateDifference); virtual; abstract;
    procedure ReverseDifference(ADifference: TStateDifference); virtual; abstract;
    function Duplicate: TState; virtual; abstract;
  end;

  TImageDifferenceKind = (idkChangeImageAndSelection, idkChangeStack, idkChangeSelection,
                           idkChangeImage, idkChangeLayer);

  { TCustomImageDifference }

  TCustomImageDifference = class(TStateDifference)
  protected
    FSavedBefore, FSavedAfter: boolean;
    function GetIsIdentity: boolean; virtual;
    function GetImageDifferenceKind: TImageDifferenceKind; virtual;
    function GetChangingBounds: TRect; virtual;
    function GetChangingBoundsDefined: boolean; virtual;
  public
    constructor Create(AFromState: TState; AToState: TState);
    constructor Create(AFromState: TState);
    constructor Create(ASavedBefore : boolean; ASavedAfter: boolean);
    procedure ApplyTo(AState: TState); override;
    procedure UnapplyTo(AState: TState); override;
    property SavedBefore: boolean read FSavedBefore write FSavedBefore;
    property SavedAfter: boolean read FSavedAfter write FSavedAfter;
    property Kind: TImageDifferenceKind read GetImageDifferenceKind;
    property ChangingBounds: TRect read GetChangingBounds;
    property ChangingBoundsDefined: boolean read GetChangingBoundsDefined;
    property IsIdentity: boolean read GetIsIdentity;
  end;

{*********** Layer info *************}

type
  TLayerInfo = record
    Id: integer;
    Visible: boolean;
    BlendOp: TBlendOperation;
    Name: string;
    Opactiy: byte;
  end;

procedure ApplyLayerInfo(AInfo: TLayerInfo; ALayeredBitmap: TBGRALayeredBitmap; AIndex: integer);
procedure ApplyLayerInfo(AInfo: TLayerInfo; ALayeredBitmap: TBGRALayeredBitmap);
function GetLayerInfo(ALayeredBitmap: TBGRALayeredBitmap; AIndex: integer): TLayerInfo;

{*********** Inversible **************}

type
  TInversibleAction = (iaHorizontalFlip, iaVerticalFlip, iaRotateCW, iaRotateCCW, iaRotate180, iaSwapRedBlue, iaLinearNegative);

function GetInverseAction(AAction: TInversibleAction): TInversibleAction;
function CanCombineInversibleAction(AAction1, AAction2: TInversibleAction; out
  ACombined: TInversibleAction): boolean;

type
  { TImageDiff }

  TImageDiff = class
  private
    FChangeRect: TRect;
    FUncompressedData: record
      data0,data1,data2,data3: PByte;
      dataLen: PtrUInt;
    end;
    FSavedFilename: string;
    FCompressedData: TMemoryStream;
    procedure DiscardFile;
    function GetIsIdentity: boolean;
    procedure Decompress;
    procedure Init(Image1,Image2: TBGRABitmap; AChangeRect: TRect);
  public
    SizeBefore, SizeAfter: TSize;
    constructor Create(Image1,Image2: TBGRABitmap; AChangeRect: TRect); overload;
    constructor Create(Image1,Image2: TBGRABitmap); overload;
    procedure Apply(ADest: TBGRABitmap; {%H-}AReverse: boolean);
    function Compress: boolean;
    destructor Destroy; override;
    function UsedMemory: int64;
    property IsIdentity: boolean read GetIsIdentity;
    property ChangeRect: TRect read FChangeRect;
  end;

function ComputeFromImageDiff(FromImage: TBGRABitmap; ADiff: TImageDiff; AReverse: boolean): TBGRABitmap;
procedure ApplyImageDiffAndReplace(var AImage: TBGRABitmap; ADiff: TImageDiff; AReverse: boolean);

type
  { TGrayscaleImageDiff }

  TGrayscaleImageDiff = class
  private
    FChangeRect: TRect;
    FUncompressedData: record
      data0: PByte;
      dataLen: PtrUInt;
    end;
    FSavedFilename: string;
    FCompressedData: TMemoryStream;
    procedure DiscardFile;
    function GetIsIdentity: boolean;
    procedure Decompress;
    procedure Init(Image1,Image2: TBGRABitmap; AChangeRect: TRect);
  public
    SizeBefore, SizeAfter: TSize;
    constructor Create(Image1,Image2: TBGRABitmap; AChangeRect: TRect); overload;
    constructor Create(Image1,Image2: TBGRABitmap); overload;
    procedure Apply(ADest: TBGRABitmap; {%H-}AReverse: boolean);
    function Compress: boolean;
    destructor Destroy; override;
    function UsedMemory: int64;
    property IsIdentity: boolean read GetIsIdentity;
    property ChangeRect: TRect read FChangeRect;
  end;

function ComputeFromGrayscaleImageDiff(FromImage: TBGRABitmap; ADiff: TGrayscaleImageDiff; AReverse: boolean): TBGRABitmap;
procedure ApplyGrayscaleImageDiffAndReplace(var AImage: TBGRABitmap; ADiff: TGrayscaleImageDiff; AReverse: boolean);

type
  { TStoredImage }

  TStoredImage = class(TImageDiff)
  public
    constructor Create(ABitmap: TBGRABitmap);
    function GetBitmap: TBGRABitmap;
  end;

implementation

uses Math, BGRALzpCommon;

{ TGrayscaleImageDiff }

procedure TGrayscaleImageDiff.DiscardFile;
begin
  if FSavedFilename <> '' then
  begin
    try
      if FileExists(FSavedFilename) then
        DeleteFile(FSavedFilename);
    except on ex:exception do begin end;
    end;
    FSavedFilename:= '';
  end;
end;

function TGrayscaleImageDiff.GetIsIdentity: boolean;
begin
  result := (SizeBefore.cx = SizeAfter.cx) and (SizeBefore.cy = SizeAfter.cy) and (FUncompressedData.dataLen=0);
end;

procedure TGrayscaleImageDiff.Decompress;
var stream: TFileStream;
begin
  if (FCompressedData = nil) and (FSavedFilename <> '') then
  begin
    FCompressedData := TMemoryStream.Create;
    stream := nil;
    try
      stream := TFileStream.Create(FSavedFilename,fmOpenRead or fmShareDenyWrite);
      FCompressedData.CopyFrom(stream, stream.Size);
    except
    end;
    stream.free;
  end;
  if FCompressedData <> nil then
  begin
    FCompressedData.Position := 0;
    ReAllocMem(FUncompressedData.data0, FUncompressedData.dataLen);
    DecodeLazRLE(FCompressedData,FUncompressedData.data0^,FUncompressedData.dataLen);
    FreeAndNil(FCompressedData);
  end;
end;

procedure TGrayscaleImageDiff.Init(Image1, Image2: TBGRABitmap;
  AChangeRect: TRect);
var tx,ty: integer;
  uncompressedDiff: TBGRABitmap;
  data0,pb: PByte;
  xb,yb: integer;
  dataLen: PtrUInt;
  p: PBGRAPixel;
  uncompressedChangeRect: TRect;
begin
  FUncompressedData.dataLen := 0;
  FChangeRect := EmptyRect;
  if Image1 = nil then
  begin
    SizeBefore.cx := 0;
    SizeBefore.cy := 0;
  end else
  begin
    SizeBefore.cx := Image1.Width;
    SizeBefore.cy := Image1.Height;
  end;

  if Image2 = nil then
  begin
    SizeAfter.cx := 0;
    SizeAfter.cy := 0;
  end else
  begin
    SizeAfter.cx := Image2.Width;
    SizeAfter.cy := Image2.Height;
  end;

  tx := max(SizeBefore.cx,SizeAfter.cx);
  ty := max(SizeBefore.cy,SizeAfter.cy);
  if IntersectRect(AChangeRect, AChangeRect, rect(0,0,tx,ty)) then
  begin
    uncompressedDiff := TBGRABitmap.Create(AChangeRect.Right-AChangeRect.Left,AChangeRect.Bottom-AChangeRect.Top);
    try
      if Image1<>nil then
        uncompressedDiff.PutImage(-AChangeRect.Left,-AChangeRect.Top,Image1,dmXor);
      if Image2<>nil then
        uncompressedDiff.PutImage(-AChangeRect.Left,-AChangeRect.Top,Image2,dmXor);
      uncompressedChangeRect := uncompressedDiff.GetImageBounds(cGreen);
      if not IsRectEmpty(uncompressedChangeRect) then
      begin
        dataLen := (uncompressedChangeRect.Right-uncompressedChangeRect.Left)*
          (uncompressedChangeRect.Bottom-uncompressedChangeRect.top);
        getmem(data0,dataLen);
        pb := data0;
        for yb := uncompressedChangeRect.Top to uncompressedChangeRect.Bottom-1 do
        begin
          p := uncompressedDiff.ScanLine[yb]+uncompressedChangeRect.Left;
          for xb := uncompressedChangeRect.Left to uncompressedChangeRect.Right-1 do
            begin
              pb^ := p^.green;
              inc(p);
              inc(pb);
            end;
        end;
        FreeAndNil(uncompressedDiff);

        FChangeRect := uncompressedChangeRect;
        OffsetRect(FChangeRect, AChangeRect.Left, AChangeRect.Top);
        FUncompressedData.data0 := data0;
        FUncompressedData.dataLen:= dataLen;
      end;
    finally
      uncompressedDiff.free;
    end;
  end;
end;

constructor TGrayscaleImageDiff.Create(Image1, Image2: TBGRABitmap;
  AChangeRect: TRect);
begin
  Init(Image1,Image2,AChangeRect);
end;

constructor TGrayscaleImageDiff.Create(Image1, Image2: TBGRABitmap);
var
  r: TRect;
begin
  r := rect(0,0,0,0);
  if Image1 <> nil then
  begin
    if image1.Width > r.Right then r.Right:= Image1.Width;
    if image1.Height > r.Bottom then r.Bottom:= Image1.Height;
  end;
  if Image2 <> nil then
  begin
    if image2.Width > r.Right then r.Right:= Image2.Width;
    if image2.Height > r.Bottom then r.Bottom:= Image2.Height;
  end;
  Init(Image1,Image2,r);
end;

procedure TGrayscaleImageDiff.Apply(ADest: TBGRABitmap; AReverse: boolean);
var
  pdest: PBGRAPixel;
  data0: PByte;
  r: TRect;
  xb,yb,w,offset: PtrUInt;
  v: NativeUint;
begin
  r := FChangeRect;
  w := FChangeRect.Right-FChangeRect.Left;
  if not IntersectRect(r, r,rect(0,0,ADest.Width,ADest.Height)) then exit;
  if (FUncompressedData.data0 = nil) then
    Decompress;
  data0 := FUncompressedData.data0;
  for yb := r.Top to r.Bottom-1 do
  begin
    pdest := ADest.ScanLine[yb]+r.Left;
    offset := (yb - PtrUInt(FChangeRect.Top))*w + DWord(r.Left-FChangeRect.Left);
    for xb := r.Left to r.right-1 do
    begin
      v := pdest^.green xor (data0+offset)^;
      pdest^.red := v;
      pdest^.green:= v;
      pdest^.blue := v;
      pdest^.alpha := 255;
      inc(pdest);
      inc(offset);
    end;
  end;
end;

function TGrayscaleImageDiff.Compress: boolean;
var
  FSavedFile: TFileStream;
begin
  if (FUncompressedData.data0 <> nil) and
    ((FCompressedData <> nil) or (FSavedFilename <> '')) then
  begin
    ReAllocMem(FUncompressedData.data0,0);
    if FSavedFilename <> '' then FreeAndNil(FCompressedData);
    result := true;
  end else
  if (FSavedFilename <> '') and (FCompressedData <> nil) then
  begin
    FreeAndNil(FCompressedData);
    result := true;
  end else
  if (FUncompressedData.dataLen < MinSizeToCompress) or (FCompressedData <> nil) or (FSavedFilename <> '') then
    result := false
  else
  begin
    FCompressedData := TMemoryStream.create;
    EncodeLazRLE(FUncompressedData.data0^,FUncompressedData.dataLen,FCompressedData);
    ReAllocMem(FUncompressedData.data0,0);
    result := true;

    if FCompressedData.Size >= MinSerializedSize then
    begin
      FSavedFilename := GetTempFileName;
      try
        FSavedFile := TFileStream.Create(FSavedFilename,fmCreate);
        try
          FCompressedData.Position := 0;
          FSavedFile.CopyFrom(FCompressedData, FCompressedData.Size);
          FreeAndNil(FCompressedData);
        finally
          FSavedFile.Free;
        end;
      except
        on ex: exception do
        begin
          if FileExists(FSavedFilename) then DeleteFile(FSavedFilename);
          FSavedFilename := '';
          result := false;
        end;
      end;
    end;
  end;
end;

destructor TGrayscaleImageDiff.Destroy;
begin
  FreeAndnil(FCompressedData);
  ReAllocMem(FUncompressedData.data0,0);
  DiscardFile;
  inherited Destroy;
end;

function TGrayscaleImageDiff.UsedMemory: int64;
begin
  if Assigned(FCompressedData) then
    result := FCompressedData.Size
  else
    result := 0;
  if Assigned(FUncompressedData.data0) then inc(result,FUncompressedData.dataLen);
end;

{ TStateDifference }

function TStateDifference.TryCompress: boolean;
begin
  result := false;
end;

function TStateDifference.UsedMemory: int64;
begin
  result := 0;
end;

{ TCustomImageDifference }

function TCustomImageDifference.GetIsIdentity: boolean;
begin
  result := false;
end;

function TCustomImageDifference.GetImageDifferenceKind: TImageDifferenceKind;
begin
  result := idkChangeImageAndSelection;
end;

function TCustomImageDifference.GetChangingBounds: TRect;
begin
  result := rect(0,0,0,0);
end;

function TCustomImageDifference.GetChangingBoundsDefined: boolean;
begin
  result := false;
end;

constructor TCustomImageDifference.Create(AFromState: TState; AToState: TState);
begin
  FSavedBefore:= AFromState.saved;
  FSavedAfter:= AToState.saved;
end;

constructor TCustomImageDifference.Create(AFromState: TState);
begin
  FSavedBefore:= AFromState.saved;
  FSavedAfter:= false;
end;

constructor TCustomImageDifference.Create(ASavedBefore: boolean;
  ASavedAfter: boolean);
begin
  FSavedBefore := ASavedBefore;
  FSavedAfter := ASavedAfter;
end;

procedure TCustomImageDifference.ApplyTo(AState: TState);
begin
  AState.saved:= FSavedAfter;
end;

procedure TCustomImageDifference.UnapplyTo(AState: TState);
begin
  AState.saved:= FSavedBefore;
end;

{***********************************}

procedure ApplyImageDiffAndReplace(var AImage: TBGRABitmap; ADiff: TImageDiff; AReverse: boolean);
var tempBmp: TBGRABitmap;
begin
  if (ADiff = nil) or ADiff.IsIdentity then exit;
  if (ADiff.SizeAfter.cx <> ADiff.SizeBefore.cx) or
     (ADiff.SizeAfter.cy <> ADiff.SizeBefore.cy) then
  begin
    tempBmp := ComputeFromImageDiff(AImage, ADiff, AReverse);
    FreeAndNil(AImage);
    AImage := tempBmp;
  end else
    ADiff.Apply(AImage, AReverse);
end;

function ComputeFromGrayscaleImageDiff(FromImage: TBGRABitmap;
  ADiff: TGrayscaleImageDiff; AReverse: boolean): TBGRABitmap;
var
  DestSize: TSize;
begin
  if (ADiff = nil) or ADiff.IsIdentity then
  begin
    result := FromImage.Duplicate as TBGRABitmap;
    exit;
  end;
  if AReverse then DestSize := ADiff.SizeBefore else
    DestSize := ADiff.SizeAfter;
  if (DestSize.cx = 0) or (DestSize.cy = 0) then
    result := nil
  else
  begin
    result := TBGRABitmap.Create(Destsize.cx,Destsize.cy,BGRABlack);
    if FromImage <> nil then
      result.PutImage(0,0,FromImage,dmSet);
    ADiff.Apply(result, AReverse);
  end;
end;

procedure ApplyGrayscaleImageDiffAndReplace(var AImage: TBGRABitmap;
  ADiff: TGrayscaleImageDiff; AReverse: boolean);
var tempBmp: TBGRABitmap;
begin
  if (ADiff = nil) or ADiff.IsIdentity then exit;
  if (ADiff.SizeAfter.cx <> ADiff.SizeBefore.cx) or
     (ADiff.SizeAfter.cy <> ADiff.SizeBefore.cy) then
  begin
    tempBmp := ComputeFromGrayscaleImageDiff(AImage, ADiff, AReverse);
    FreeAndNil(AImage);
    AImage := tempBmp;
  end else
    ADiff.Apply(AImage, AReverse);
end;

{*********** Layer info *************}

procedure ApplyLayerInfo(AInfo: TLayerInfo; ALayeredBitmap: TBGRALayeredBitmap; AIndex: integer);
begin
  if (AIndex < 0) or (AIndex >= ALayeredBitmap.NbLayers) then
      raise exception.Create('Out of bounds');
  ALayeredBitmap.LayerUniqueId[AIndex] := AInfo.Id;
  ALayeredBitmap.LayerVisible[AIndex] := AInfo.Visible;
  ALayeredBitmap.BlendOperation[AIndex] := AInfo.BlendOp;
  ALayeredBitmap.LayerName[AIndex] := AInfo.Name;
  ALayeredBitmap.LayerOpacity[AIndex] := AInfo.Opactiy;
end;

procedure ApplyLayerInfo(AInfo: TLayerInfo; ALayeredBitmap: TBGRALayeredBitmap);
var idx: integer;
begin
  idx := ALayeredBitmap.GetLayerIndexFromId(AInfo.Id);
  if idx = -1 then
    raise exception.Create('Layer not found');
  ApplyLayerInfo(AInfo, ALayeredBitmap, idx);
end;

function GetLayerInfo(ALayeredBitmap: TBGRALayeredBitmap; AIndex: integer): TLayerInfo;
begin
  if (AIndex < 0) or (AIndex >= ALayeredBitmap.NbLayers) then
      raise exception.Create('Out of bounds');
  result.Id := ALayeredBitmap.LayerUniqueId[AIndex];
  result.Visible := ALayeredBitmap.LayerVisible[AIndex];
  result.BlendOp := ALayeredBitmap.BlendOperation[AIndex];
  result.Name := ALayeredBitmap.LayerName[AIndex];
  result.Opactiy := ALayeredBitmap.LayerOpacity[AIndex];
end;

{*********** Inversible **************}

function GetInverseAction(AAction: TInversibleAction): TInversibleAction;
begin
  case AAction of
  iaRotateCW: result := iaRotateCCW;
  iaRotateCCW: result := iaRotateCW;
  else
    result := AAction;
  end;
end;

function CanCombineInversibleAction(AAction1, AAction2: TInversibleAction; out
  ACombined: TInversibleAction): boolean;
begin
  result := true;
  ACombined:= iaHorizontalFlip; //some default value
  case AAction1 of
  iaSwapRedBlue: result := false;
  iaRotate180: case AAction2 of
               iaRotateCW: ACombined:= iaRotateCCW;
               iaRotateCCW: ACombined := iaRotateCW;
               iaHorizontalFlip: ACombined := iaVerticalFlip;
               iaVerticalFlip: ACombined:= iaHorizontalFlip;
               else result := false;
               end;
  iaHorizontalFlip: case AAction2 of
                    iaVerticalFlip: ACombined:= iaRotate180;
                    else result := false;
                    end;
  iaVerticalFlip: case AAction2 of
                    iaHorizontalFlip: ACombined:= iaRotate180;
                    else result := false;
                    end;
  iaRotateCW: case AAction2 of
              iaRotateCW: ACombined:= iaRotate180;
              iaRotate180: ACombined:= iaRotateCCW;
              else result := false;
              end;
  iaRotateCCW: case AAction2 of
               iaRotateCCW: ACombined:= iaRotate180;
               iaRotate180: ACombined:= iaRotateCW;
               else result := false;
               end;
  else
    result := false;
  end;
end;

{**************** Image diff ****************}

function ComputeFromImageDiff(FromImage: TBGRABitmap; ADiff: TImageDiff; AReverse: boolean): TBGRABitmap;
var
  DestSize: TSize;
begin
  if (ADiff = nil) or ADiff.IsIdentity then
  begin
    result := FromImage.Duplicate as TBGRABitmap;
    exit;
  end;
  if AReverse then DestSize := ADiff.SizeBefore else
    DestSize := ADiff.SizeAfter;
  if (DestSize.cx = 0) or (DestSize.cy = 0) then
    result := nil
  else
  begin
    result := TBGRABitmap.Create(Destsize.cx,Destsize.cy);
    if FromImage <> nil then
      result.PutImage(0,0,FromImage,dmSet);
    ADiff.Apply(result, AReverse);
  end;
end;

{ TImageDiff }

procedure TImageDiff.DiscardFile;
begin
  if FSavedFilename <> '' then
  begin
    try
      if FileExists(FSavedFilename) then
        DeleteFile(FSavedFilename);
    except on ex:exception do begin end;
    end;
    FSavedFilename:= '';
  end;
end;

function TImageDiff.GetIsIdentity: boolean;
begin
  result := (SizeBefore.cx = SizeAfter.cx) and (SizeBefore.cy = SizeAfter.cy) and (FUncompressedData.dataLen=0);
end;

procedure TImageDiff.Decompress;
var stream: TFileStream;
begin
  if (FCompressedData = nil) and (FSavedFilename <> '') then
  begin
    FCompressedData := TMemoryStream.Create;
    stream := nil;
    try
      stream := TFileStream.Create(FSavedFilename,fmOpenRead or fmShareDenyWrite);
      FCompressedData.CopyFrom(stream, stream.Size);
    except
    end;
    stream.free;
  end;
  if FCompressedData <> nil then
  begin
    FCompressedData.Position := 0;
    ReAllocMem(FUncompressedData.data0, FUncompressedData.dataLen);
    DecodeLazRLE(FCompressedData,FUncompressedData.data0^,FUncompressedData.dataLen);
    ReAllocMem(FUncompressedData.data1, FUncompressedData.dataLen);
    DecodeLazRLE(FCompressedData,FUncompressedData.data1^,FUncompressedData.dataLen);
    ReAllocMem(FUncompressedData.data2, FUncompressedData.dataLen);
    DecodeLazRLE(FCompressedData,FUncompressedData.data2^,FUncompressedData.dataLen);
    ReAllocMem(FUncompressedData.data3, FUncompressedData.dataLen);
    DecodeLazRLE(FCompressedData,FUncompressedData.data3^,FUncompressedData.dataLen);
    FreeAndNil(FCompressedData);
  end;
end;

procedure TImageDiff.Init(Image1, Image2: TBGRABitmap; AChangeRect: TRect);
var tx,ty: integer;
  uncompressedDiff: TBGRABitmap;
  data0,data1,data2,data3: PByte;
  xb,yb: integer;
  dataLen,n: PtrUInt;
  p: PDWord;
  v: DWord;
  uncompressedChangeRect: TRect;
begin
  FUncompressedData.dataLen := 0;
  FChangeRect := EmptyRect;
  if Image1 = nil then
  begin
    SizeBefore.cx := 0;
    SizeBefore.cy := 0;
  end else
  begin
    SizeBefore.cx := Image1.Width;
    SizeBefore.cy := Image1.Height;
  end;

  if Image2 = nil then
  begin
    SizeAfter.cx := 0;
    SizeAfter.cy := 0;
  end else
  begin
    SizeAfter.cx := Image2.Width;
    SizeAfter.cy := Image2.Height;
  end;

  tx := max(SizeBefore.cx,SizeAfter.cx);
  ty := max(SizeBefore.cy,SizeAfter.cy);
  if IntersectRect(AChangeRect, AChangeRect, rect(0,0,tx,ty)) then
  begin
    uncompressedDiff := TBGRABitmap.Create(AChangeRect.Right-AChangeRect.Left,AChangeRect.Bottom-AChangeRect.Top);
    try
      if Image1<>nil then
        uncompressedDiff.PutImage(-AChangeRect.Left,-AChangeRect.Top,Image1,dmXor);
      if Image2<>nil then
        uncompressedDiff.PutImage(-AChangeRect.Left,-AChangeRect.Top,Image2,dmXor);
      uncompressedChangeRect := uncompressedDiff.GetImageBounds([cAlpha,cRed,cGreen,cBlue]);
      if not IsRectEmpty(uncompressedChangeRect) then
      begin
        dataLen := (uncompressedChangeRect.Right-uncompressedChangeRect.Left)*
          (uncompressedChangeRect.Bottom-uncompressedChangeRect.top);
        getmem(data0,dataLen);
        getmem(data1,dataLen);
        getmem(data2,dataLen);
        getmem(data3,dataLen);
        n := 0;
        for yb := uncompressedChangeRect.Top to uncompressedChangeRect.Bottom-1 do
        begin
          p := PDWord(uncompressedDiff.ScanLine[yb]+uncompressedChangeRect.Left);
          for xb := uncompressedChangeRect.Left to uncompressedChangeRect.Right-1 do
            begin
              v := p^;
              (data0+n)^ := v and 255;
              (data1+n)^ := (v shr 8) and 255;
              (data2+n)^ := (v shr 16) and 255;
              (data3+n)^ := (v shr 24) and 255;
              inc(p);
              inc(n);
            end;
        end;
        FreeAndNil(uncompressedDiff);

        FChangeRect := uncompressedChangeRect;
        OffsetRect(FChangeRect, AChangeRect.Left, AChangeRect.Top);
        FUncompressedData.data0 := data0;
        FUncompressedData.data1 := data1;
        FUncompressedData.data2 := data2;
        FUncompressedData.data3 := data3;
        FUncompressedData.dataLen:= dataLen;
      end;
    finally
      uncompressedDiff.free;
    end;
  end;
end;

constructor TImageDiff.Create(Image1, Image2: TBGRABitmap; AChangeRect: TRect);
begin
  Init(Image1,Image2,AChangeRect);
end;

constructor TImageDiff.Create(Image1, Image2: TBGRABitmap);
var
  r: TRect;
begin
  r := rect(0,0,0,0);
  if Image1 <> nil then
  begin
    if image1.Width > r.Right then r.Right:= Image1.Width;
    if image1.Height > r.Bottom then r.Bottom:= Image1.Height;
  end;
  if Image2 <> nil then
  begin
    if image2.Width > r.Right then r.Right:= Image2.Width;
    if image2.Height > r.Bottom then r.Bottom:= Image2.Height;
  end;
  Init(Image1,Image2,r);
end;

procedure TImageDiff.Apply(ADest: TBGRABitmap; AReverse: boolean);
var
  pdest: PDWord;
  data0,data1,data2,data3: PByte;
  r: TRect;
  xb,yb,w,offset: PtrUInt;
begin
  r := FChangeRect;
  w := FChangeRect.Right-FChangeRect.Left;
  if not IntersectRect(r, r,rect(0,0,ADest.Width,ADest.Height)) then exit;
  if (FUncompressedData.data0 = nil) or (FUncompressedData.data1 = nil) or
    (FUncompressedData.data2 = nil) or (FUncompressedData.data3 = nil) then
    Decompress;
  data0 := FUncompressedData.data0;
  data1 := FUncompressedData.data1;
  data2 := FUncompressedData.data2;
  data3 := FUncompressedData.data3;
  for yb := r.Top to r.Bottom-1 do
  begin
    pdest := PDWord(ADest.ScanLine[yb]+r.Left);
    offset := (yb - PtrUInt(FChangeRect.Top))*w + DWord(r.Left-FChangeRect.Left);
    for xb := r.Left to r.right-1 do
    begin
      pdest^ := pdest^ xor ((data0+offset)^ + ((data1+offset)^ shl 8) + ((data2+offset)^ shl 16) + ((data3+offset)^ shl 24));
      inc(pdest);
      inc(offset);
    end;
  end;
end;

function TImageDiff.Compress: boolean;
var
  FSavedFile: TFileStream;
begin
  if ((FUncompressedData.data0 <> nil) or (FUncompressedData.data1 <> nil) or
    (FUncompressedData.data2 <> nil) or (FUncompressedData.data3 <> nil)) and
    ((FCompressedData <> nil) or (FSavedFilename <> '')) then
  begin
    ReAllocMem(FUncompressedData.data0,0);
    ReAllocMem(FUncompressedData.data1,0);
    ReAllocMem(FUncompressedData.data2,0);
    ReAllocMem(FUncompressedData.data3,0);
    if FSavedFilename <> '' then FreeAndNil(FCompressedData);
    result := true;
  end else
  if (FSavedFilename <> '') and (FCompressedData <> nil) then
  begin
    FreeAndNil(FCompressedData);
    result := true;
  end else
  if (FUncompressedData.dataLen < MinSizeToCompress) or (FCompressedData <> nil) or (FSavedFilename <> '') then
    result := false
  else
  begin
    FCompressedData := TMemoryStream.create;
    EncodeLazRLE(FUncompressedData.data0^,FUncompressedData.dataLen,FCompressedData);
    ReAllocMem(FUncompressedData.data0,0);
    EncodeLazRLE(FUncompressedData.data1^,FUncompressedData.dataLen,FCompressedData);
    ReAllocMem(FUncompressedData.data1,0);
    EncodeLazRLE(FUncompressedData.data2^,FUncompressedData.dataLen,FCompressedData);
    ReAllocMem(FUncompressedData.data2,0);
    EncodeLazRLE(FUncompressedData.data3^,FUncompressedData.dataLen,FCompressedData);
    ReAllocMem(FUncompressedData.data3,0);
    result := true;

    if FCompressedData.Size >= MinSerializedSize then
    begin
      FSavedFilename := GetTempFileName;
      try
        FSavedFile := TFileStream.Create(FSavedFilename,fmCreate);
        try
          FCompressedData.Position := 0;
          FSavedFile.CopyFrom(FCompressedData, FCompressedData.Size);
          FreeAndNil(FCompressedData);
        finally
          FSavedFile.Free;
        end;
      except
        on ex: exception do
        begin
          if FileExists(FSavedFilename) then DeleteFile(FSavedFilename);
          FSavedFilename := '';
          result := false;
        end;
      end;
    end;
  end;
end;

destructor TImageDiff.Destroy;
begin
  FreeAndnil(FCompressedData);
  ReAllocMem(FUncompressedData.data0,0);
  ReAllocMem(FUncompressedData.data1,0);
  ReAllocMem(FUncompressedData.data2,0);
  ReAllocMem(FUncompressedData.data3,0);
  DiscardFile;
  inherited Destroy;
end;

function TImageDiff.UsedMemory: int64;
begin
  if Assigned(FCompressedData) then
    result := FCompressedData.Size
  else
    result := 0;
  if Assigned(FUncompressedData.data0) then inc(result,FUncompressedData.dataLen);
  if Assigned(FUncompressedData.data1) then inc(result,FUncompressedData.dataLen);
  if Assigned(FUncompressedData.data2) then inc(result,FUncompressedData.dataLen);
  if Assigned(FUncompressedData.data3) then inc(result,FUncompressedData.dataLen);
end;

{ TStoredImage }

constructor TStoredImage.Create(ABitmap: TBGRABitmap);
begin
  inherited Create(nil,ABitmap,rect(0,0,ABitmap.Width,ABitmap.Height));
end;

function TStoredImage.GetBitmap: TBGRABitmap;
begin
  result := TBGRABitmap.Create(SizeAfter.cx, SizeAfter.cy);
  Apply(result,false);
end;

end.

