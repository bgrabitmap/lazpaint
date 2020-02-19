unit UStateType;

{$mode objfpc}

interface

uses
  Types, Classes, SysUtils, BGRABitmap, BGRABitmapTypes, BGRALayers,
  BGRALayerOriginal, fgl;

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
    function ToString: ansistring; override;
  end;

  TState = class
    saved: boolean;
    procedure ApplyDifference(ADifference: TStateDifference); virtual; abstract;
    procedure ReverseDifference(ADifference: TStateDifference); virtual; abstract;
    function Duplicate: TState; virtual; abstract;
  end;

  TImageDifferenceKind = (idkChangeImageAndSelection, idkChangeStack, idkChangeSelection,
                           idkChangeImage);

  { TCustomImageDifference }

  TCustomImageDifference = class(TStateDifference)
  protected
    FSavedBefore, FSavedAfter: boolean;
    function GetIsIdentity: boolean; virtual;
    function GetImageDifferenceKind: TImageDifferenceKind; virtual;
    function GetChangingBounds: TRect; virtual;
    function GetChangingBoundsDefined: boolean; virtual;
    function GetCost: integer; virtual;
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

  TImageDifferenceList = specialize TFPGObjectList<TCustomImageDifference>;

  { TComposedImageDifference }

  TComposedImageDifference = class(TCustomImageDifference)
  private
    function GetCount: integer;
    function GetItem(AIndex: integer): TCustomImageDifference;
    function GetTotalCount: integer;
  protected
    FDiffs: TImageDifferenceList;
    FAgglutinate: boolean;
    function GetIsIdentity: boolean; override;
    function GetImageDifferenceKind: TImageDifferenceKind; override;
    function GetChangingBounds: TRect; override;
    function GetChangingBoundsDefined: boolean; override;
  public
    constructor Create(AAgglutinate: boolean = false);
    procedure ReleaseDiffs;
    procedure StopAgglutinate;
    destructor Destroy; override;
    function TryCompress: boolean; override;
    function UsedMemory: int64; override;
    procedure Add(ADiff: TCustomImageDifference);
    procedure AddRange(AComposed: TComposedImageDifference);
    procedure ApplyTo(AState: TState); override;
    procedure UnapplyTo(AState: TState); override;
    procedure Clear;
    procedure Delete(AIndex: integer);
    procedure DeleteFrom(AIndex: integer);
    function GetLast: TCustomImageDifference;
    function ToString: ansistring; override;
    property Count: integer read GetCount;
    property TotalCount: integer read GetTotalCount;
    property Agglutinate: boolean read FAgglutinate;
    property Item[AIndex: integer]: TCustomImageDifference read GetItem; default;
  end;

{*********** Layer info *************}

type
  TLayerInfo = record
    Id: integer;
    Visible: boolean;
    BlendOp: TBlendOperation;
    Name: string;
    Opactiy: byte;
    Offset: TPoint;
  end;

procedure ApplyLayerInfo(AInfo: TLayerInfo; ALayeredBitmap: TBGRALayeredBitmap; AIndex: integer);
function GetLayerInfo(ALayeredBitmap: TBGRALayeredBitmap; AIndex: integer): TLayerInfo;

{*********** Inversible **************}

type
  TInversibleAction = (iaHorizontalFlip, iaHorizontalFlipLayer, iaVerticalFlip, iaVerticalFlipLayer, iaRotateCW, iaRotateCCW, iaRotate180, iaSwapRedBlue, iaLinearNegative);

const
  InversibleActionStr : array[TInversibleAction] of string =
    ('HorizontalFlip', 'HorizontalFlipLayer', 'VerticalFlip', 'VerticalFlipLayer', 'RotateCW', 'RotateCCW', 'Rotate180', 'SwapRedBlue', 'LinearNegative');

function GetInverseAction(AAction: TInversibleAction): TInversibleAction;
function CanCombineInversibleAction(AAction1, AAction2: TInversibleAction; out
  ACombined: TInversibleAction): boolean;

type
  TCustomImageDiff = class
  private
    FSavedFilename: string;
    FCompressedData: TMemoryStream;
    procedure DiscardFile;
    function GetIsIdentity: boolean; virtual;
    procedure Init(Image1,Image2: TBGRABitmap; {%H-}AChangeRect: TRect); virtual;
    function SerializeCompressedData: boolean;
    procedure UnserializeCompressedData;
  protected
    function CreateNew(AWidth,AHeight: integer): TBGRABitmap; virtual; abstract;
  public
    SizeBefore, SizeAfter: TSize;
    constructor Create(Image1,Image2: TBGRABitmap; AChangeRect: TRect); overload;
    constructor Create(Image1,Image2: TBGRABitmap); overload;
    procedure ApplyInPlace(ADest: TBGRABitmap; {%H-}AReverse: boolean); virtual; abstract;
    function ApplyInNew(ASource: TBGRABitmap; AReverse: boolean): TBGRABitmap;
    function ApplyCanCreateNew(ASource: TBGRABitmap; AReverse: boolean): TBGRABitmap;
    function Compress: boolean; virtual;
    destructor Destroy; override;
    function UsedMemory: int64;
    property IsIdentity: boolean read GetIsIdentity;
  end;

  { TImageDiff }

  TImageDiff = class(TCustomImageDiff)
  private
    FChangeRect: TRect;
    FUncompressedData: record
      data0,data1,data2,data3: PByte;
      dataLen: PtrUInt;
    end;
    function GetIsIdentity: boolean; override;
    procedure Decompress;
    procedure Init(Image1,Image2: TBGRABitmap; AChangeRect: TRect); override;
  protected
    function CreateNew(AWidth, AHeight: integer): TBGRABitmap; override;
  public
    procedure ApplyInPlace(ADest: TBGRABitmap; {%H-}AReverse: boolean); override;
    function Compress: boolean; override;
    destructor Destroy; override;
    function UsedMemory: int64;
    property ChangeRect: TRect read FChangeRect;
  end;

type
  { TGrayscaleImageDiff }

  TGrayscaleImageDiff = class(TCustomImageDiff)
  private
    FChangeRect: TRect;
    FUncompressedData: record
      data0: PByte;
      dataLen: PtrUInt;
    end;
    function GetIsIdentity: boolean; override;
    procedure Decompress;
    procedure Init(Image1,Image2: TBGRABitmap; AChangeRect: TRect); override;
  protected
    function CreateNew(AWidth, AHeight: integer): TBGRABitmap; override;
  public
    procedure ApplyInPlace(ADest: TBGRABitmap; {%H-}AReverse: boolean); override;
    function Compress: boolean; override;
    destructor Destroy; override;
    function UsedMemory: int64;
    property ChangeRect: TRect read FChangeRect;
  end;

type
  { TStoredImage }

  TStoredImage = class(TImageDiff)
  public
    constructor Create(ABitmap: TBGRABitmap);
    function GetBitmap: TBGRABitmap;
  end;

  { TStoredLayer }

  TStoredLayer = class(TStoredImage)
  private
    function GetId: integer;
    function GetOffset: TPoint;
  protected
    FInfo: TLayerInfo;
    FIndex: integer;
    FOriginalData: TMemoryStream;
    FOriginalBitmapStored: boolean;
    FOriginalRenderStatus: TOriginalRenderStatus;
    FOriginalMatrix: TAffineMatrix;
    FOriginalDraft: boolean;
    FOriginalGuid: TGuid;
    FRegistryData: TMemoryStream;
  public
    constructor Create(ALayeredImage: TBGRALayeredBitmap; AIndex: integer);
    constructor Create(ALayeredImage: TBGRALayeredBitmap; AIndex: integer;
                       AAlwaysStoreBitmap: boolean);
    destructor Destroy; override;
    procedure Restore(ALayeredImage: TBGRALayeredBitmap);
    procedure Replace(ALayeredImage: TBGRALayeredBitmap);
    property LayerIndex: integer read FIndex;
    property LayerId: integer read GetId;
    property Offset: TPoint read GetOffset;
  end;

implementation

uses Math, BGRALzpCommon, UFileSystem;

{ TCustomImageDiff }

procedure TCustomImageDiff.DiscardFile;
begin
  if FSavedFilename <> '' then
  begin
    try
      if FileManager.FileExists(FSavedFilename) then
        FileManager.DeleteFile(FSavedFilename);
    except on ex:exception do begin end;
    end;
    FSavedFilename:= '';
  end;
end;

function TCustomImageDiff.GetIsIdentity: boolean;
begin
  result := (SizeBefore.cx = SizeAfter.cx) and (SizeBefore.cy = SizeAfter.cy);
end;

procedure TCustomImageDiff.Init(Image1, Image2: TBGRABitmap; AChangeRect: TRect);
begin
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
end;

function TCustomImageDiff.SerializeCompressedData: boolean;
var
  savedFile: TStream;
begin
  FSavedFilename := GetTempFileName;
  try
    savedFile := FileManager.CreateFileStream(FSavedFilename,fmCreate);
    try
      FCompressedData.Position := 0;
      savedFile.CopyFrom(FCompressedData, FCompressedData.Size);
      FreeAndNil(FCompressedData);
      result := true;
    finally
      savedFile.Free;
    end;
  except
    on ex: exception do
    begin
      if FileManager.FileExists(FSavedFilename) then FileManager.DeleteFile(FSavedFilename);
      FSavedFilename := '';
      result := false;
    end;
  end;
end;

procedure TCustomImageDiff.UnserializeCompressedData;
var stream: TStream;
begin
  if (FCompressedData = nil) and (FSavedFilename <> '') then
  begin
    FCompressedData := TMemoryStream.Create;
    stream := nil;
    try
      stream := FileManager.CreateFileStream(FSavedFilename,fmOpenRead or fmShareDenyWrite);
      FCompressedData.CopyFrom(stream, stream.Size);
    except
    end;
    stream.free;
  end;
end;

constructor TCustomImageDiff.Create(Image1, Image2: TBGRABitmap;
  AChangeRect: TRect);
begin
  Init(Image1,Image2,AChangeRect);
end;

constructor TCustomImageDiff.Create(Image1, Image2: TBGRABitmap);
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

function TCustomImageDiff.ApplyInNew(ASource: TBGRABitmap; AReverse: boolean): TBGRABitmap;
var
  DestSize: TSize;
begin
  if (self = nil) or IsIdentity then
  begin
    if ASource = nil then
      result := nil
    else
      result := ASource.Duplicate as TBGRABitmap
  end
  else
  begin
    if AReverse then DestSize := SizeBefore else
      DestSize := SizeAfter;

    if (DestSize.cx = 0) or (DestSize.cy = 0) then
      result := nil
    else
    begin
      result := CreateNew(Destsize.cx,Destsize.cy);
      if ASource <> nil then
        result.PutImage(0,0,ASource,dmSet);
      ApplyInPlace(result, AReverse);
    end;
  end;
end;

function TCustomImageDiff.ApplyCanCreateNew(ASource: TBGRABitmap;
  AReverse: boolean): TBGRABitmap;
begin
  if (self = nil) or IsIdentity then exit(ASource); //keep

  if (ASource = nil) or
     ((SizeAfter.cx <> SizeBefore.cx) or
     (SizeAfter.cy <> SizeBefore.cy)) then
     exit(ApplyInNew(ASource, AReverse))
  else
  begin
    ApplyInPlace(ASource, AReverse);
    exit(ASource);
  end;
end;

function TCustomImageDiff.Compress: boolean;
begin
  result := false;
end;

destructor TCustomImageDiff.Destroy;
begin
  FreeAndnil(FCompressedData);
  DiscardFile;
  inherited Destroy;
end;

function TCustomImageDiff.UsedMemory: int64;
begin
  if Assigned(FCompressedData) then
    result := FCompressedData.Size
  else
    result := 0;
end;

{ TComposedImageDifference }

function TComposedImageDifference.GetCount: integer;
begin
  result := FDiffs.Count;
end;

function TComposedImageDifference.GetItem(AIndex: integer): TCustomImageDifference;
begin
  result := FDiffs[AIndex];
end;

function TComposedImageDifference.GetTotalCount: integer;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to FDiffs.Count-1 do
    if FDiffs[i] is TComposedImageDifference then
      inc(result, TComposedImageDifference(FDiffs[i]).TotalCount)
    else
      inc(result, FDiffs[i].GetCost);
end;

function TComposedImageDifference.GetIsIdentity: boolean;
var
  i: Integer;
begin
  if FAgglutinate then exit(false);
  for i := 0 to FDiffs.Count-1 do
    if not FDiffs[i].GetIsIdentity then exit(false);
  exit(true);
end;

function TComposedImageDifference.GetImageDifferenceKind: TImageDifferenceKind;
var
  i: Integer;
begin
  result := idkChangeStack;
  for i := 0 to FDiffs.Count-1 do
    case FDiffs[i].GetImageDifferenceKind of
      idkChangeImageAndSelection: result := idkChangeImageAndSelection;
      idkChangeSelection: if result in[idkChangeImage,idkChangeImageAndSelection] then
                            result := idkChangeImageAndSelection
                          else result := idkChangeSelection;
      idkChangeImage: if result in[idkChangeImageAndSelection,idkChangeSelection] then
                            result := idkChangeImageAndSelection
                          else result := idkChangeImage;
    end;
end;

function TComposedImageDifference.GetChangingBounds: TRect;
var
  i: Integer;
  r: TRect;
begin
  result:= EmptyRect;
  for i := 0 to FDiffs.Count-1 do
  begin
    r := FDiffs[i].GetChangingBounds;
    if not IsRectEmpty(r) then
    begin
      if IsRectEmpty(result) then result:= r
      else UnionRect(result, result,r);
    end;
  end;
end;

function TComposedImageDifference.GetChangingBoundsDefined: boolean;
var
  i: Integer;
begin
  for i := 0 to FDiffs.Count-1 do
    if not FDiffs[i].GetChangingBoundsDefined then exit(false);
  exit(true);
end;

constructor TComposedImageDifference.Create(AAgglutinate: boolean);
begin
  FDiffs := TImageDifferenceList.Create;
  FAgglutinate:= AAgglutinate;
end;

procedure TComposedImageDifference.ReleaseDiffs;
begin
  FDiffs.FreeObjects:= false;
  FDiffs.Clear;
  FDiffs.FreeObjects:= true;
end;

procedure TComposedImageDifference.StopAgglutinate;
begin
  FAgglutinate:= false;
end;

destructor TComposedImageDifference.Destroy;
begin
  FDiffs.Free;
  inherited Destroy;
end;

function TComposedImageDifference.TryCompress: boolean;
var
  i: Integer;
begin
  for i := 0 to FDiffs.Count-1 do
    if FDiffs[i].TryCompress then exit(true);
  exit(false);
end;

function TComposedImageDifference.UsedMemory: int64;
var
  i: Integer;
begin
  result := 0;
  for i := 0 to FDiffs.Count-1 do
    inc(result, FDiffs[i].UsedMemory);
end;

procedure TComposedImageDifference.Add(ADiff: TCustomImageDifference);
begin
  FDiffs.Add(ADiff);
end;

procedure TComposedImageDifference.AddRange(AComposed: TComposedImageDifference);
var
  i: Integer;
begin
  for i:= 0 to AComposed.Count-1 do
    Add(AComposed.FDiffs[i]);
end;

procedure TComposedImageDifference.ApplyTo(AState: TState);
var
  i: Integer;
begin
  for i := 0 to FDiffs.Count-1 do
    FDiffs[i].ApplyTo(AState);
end;

procedure TComposedImageDifference.UnapplyTo(AState: TState);
var
  i: Integer;
begin
  for i := FDiffs.Count-1 downto 0 do
    FDiffs[i].UnapplyTo(AState);
end;

procedure TComposedImageDifference.Clear;
begin
  FDiffs.Clear;
end;

procedure TComposedImageDifference.Delete(AIndex: integer);
begin
  FDiffs.Delete(AIndex);
end;

procedure TComposedImageDifference.DeleteFrom(AIndex: integer);
var
  i: Integer;
begin
  for i := Count-1 downto AIndex do
    Delete(i);
end;

function TComposedImageDifference.GetLast: TCustomImageDifference;
begin
  if Count = 0 then result := nil
  else result := FDiffs[Count-1];
end;

function TComposedImageDifference.ToString: ansistring;
var
  i: Integer;
begin
  Result:= '[';
  for i := 0 to Count-1 do
  begin
    if i <> 0 then result += ', ';
    result += FDiffs[i].ToString;
  end;
  result += ']';
end;

{ TGrayscaleImageDiff }

function TGrayscaleImageDiff.GetIsIdentity: boolean;
begin
  result := inherited GetIsIdentity and (FUncompressedData.dataLen=0);
end;

procedure TGrayscaleImageDiff.Decompress;
begin
  UnserializeCompressedData;
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
  inherited Init(Image1,Image2,AChangeRect);
  FChangeRect := EmptyRect;
  FUncompressedData.dataLen := 0;
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

function TGrayscaleImageDiff.CreateNew(AWidth, AHeight: integer): TBGRABitmap;
begin
  result := TBGRABitmap.Create(AWidth,AHeight, BGRABlack);
end;

procedure TGrayscaleImageDiff.ApplyInPlace(ADest: TBGRABitmap; AReverse: boolean);
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
      SerializeCompressedData;
  end;
end;

destructor TGrayscaleImageDiff.Destroy;
begin
  ReAllocMem(FUncompressedData.data0,0);
  inherited Destroy;
end;

function TGrayscaleImageDiff.UsedMemory: int64;
begin
  result := inherited UsedMemory;
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

function TStateDifference.ToString: ansistring;
begin
  Result:= ClassName;
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

function TCustomImageDifference.GetCost: integer;
begin
  result := 1;
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
  if ALayeredBitmap.LayerOriginalGuid[AIndex] = GUID_NULL then
    ALayeredBitmap.LayerOffset[AIndex] := AInfo.Offset;
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
  result.Offset := ALayeredBitmap.LayerOffset[AIndex];
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

{ TImageDiff }

function TImageDiff.GetIsIdentity: boolean;
begin
  result := inherited GetIsIdentity and (FUncompressedData.dataLen=0);
end;

procedure TImageDiff.Decompress;
begin
  UnserializeCompressedData;
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
  inherited Init(Image1, Image2, ChangeRect);
  FChangeRect := EmptyRect;
  FUncompressedData.dataLen := 0;
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

function TImageDiff.CreateNew(AWidth, AHeight: integer): TBGRABitmap;
begin
  result := TBGRABitmap.Create(AWidth,AHeight);
end;

procedure TImageDiff.ApplyInPlace(ADest: TBGRABitmap; AReverse: boolean);
var
  pdest: PDWord;
  data0,data1,data2,data3: PByte;
  r: TRect;
  xb,yb,w,offset: PtrUInt;
begin
  if ADest = nil then raise exception.Create('Unexpected nil reference');
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
      SerializeCompressedData;
  end;
end;

destructor TImageDiff.Destroy;
begin
  ReAllocMem(FUncompressedData.data0,0);
  ReAllocMem(FUncompressedData.data1,0);
  ReAllocMem(FUncompressedData.data2,0);
  ReAllocMem(FUncompressedData.data3,0);
  inherited Destroy;
end;

function TImageDiff.UsedMemory: int64;
begin
  result := inherited UsedMemory;
  if Assigned(FUncompressedData.data0) then inc(result,FUncompressedData.dataLen);
  if Assigned(FUncompressedData.data1) then inc(result,FUncompressedData.dataLen);
  if Assigned(FUncompressedData.data2) then inc(result,FUncompressedData.dataLen);
  if Assigned(FUncompressedData.data3) then inc(result,FUncompressedData.dataLen);
end;

{ TStoredImage }

constructor TStoredImage.Create(ABitmap: TBGRABitmap);
begin
  if Assigned(ABitmap) then
    inherited Create(nil,ABitmap,rect(0,0,ABitmap.Width,ABitmap.Height))
  else
    inherited Create(nil,ABitmap,EmptyRect)
end;

function TStoredImage.GetBitmap: TBGRABitmap;
begin
  result := TBGRABitmap.Create(SizeAfter.cx, SizeAfter.cy);
  ApplyInPlace(result,false);
end;

{ TStoredLayer }

function TStoredLayer.GetId: integer;
begin
  result := FInfo.Id;
end;

function TStoredLayer.GetOffset: TPoint;
begin
  result := FInfo.Offset;
end;

constructor TStoredLayer.Create(ALayeredImage: TBGRALayeredBitmap;
  AIndex: integer);
var
  {%H-}orig: TBGRALayerCustomOriginal;
  alwaysStoreBitmap: Boolean;
begin
  alwaysStoreBitmap := false;
  if (ALayeredImage.LayerOriginalGuid[AIndex]<>GUID_NULL) and
    ALayeredImage.LayerOriginalKnown[AIndex] then
  begin
    try
      orig := ALayeredImage.LayerOriginal[AIndex];
    except
      on ex:exception do
        alwaysStoreBitmap:= true;
    end;
  end;
  Create(ALayeredImage, AIndex, alwaysStoreBitmap);
end;

constructor TStoredLayer.Create(ALayeredImage: TBGRALayeredBitmap;
  AIndex: integer; AAlwaysStoreBitmap: boolean);
begin
  FIndex := AIndex;
  FInfo := GetLayerInfo(ALayeredImage, AIndex);
  if ALayeredImage.LayerOriginalGuid[AIndex]<>GUID_NULL then
  begin
    FOriginalBitmapStored := AAlwaysStoreBitmap or not ALayeredImage.LayerOriginalKnown[AIndex];
    FOriginalRenderStatus:= ALayeredImage.LayerOriginalRenderStatus[AIndex];

    if not FOriginalBitmapStored then
      inherited Create(nil)
    else
      inherited Create(ALayeredImage.LayerBitmap[AIndex]);

    FOriginalData := TMemoryStream.Create;
    FOriginalGuid := ALayeredImage.LayerOriginalGuid[AIndex];
    ALayeredImage.SaveOriginalToStream(FOriginalGuid, FOriginalData);
    FOriginalMatrix := ALayeredImage.LayerOriginalMatrix[AIndex];
    FOriginalDraft := ALayeredImage.LayerOriginalRenderStatus[AIndex] in[orsDraft,orsPartialDraft];
  end else
  begin
    inherited Create(ALayeredImage.LayerBitmap[AIndex]);
    FOriginalData := nil;
  end;
  FRegistryData := TMemoryStream.Create;
  ALayeredImage.SaveLayerRegistryToStream(AIndex, FRegistryData);
end;

destructor TStoredLayer.Destroy;
begin
  FRegistryData.Free;
  FOriginalData.Free;
  inherited Destroy;
end;

procedure TStoredLayer.Restore(ALayeredImage: TBGRALayeredBitmap);
var
  tempIdx, idxOrig: Integer;
begin
  if Assigned(FOriginalData) then
  begin
    FOriginalData.Position:= 0;
    idxOrig := ALayeredImage.IndexOfOriginal(FOriginalGuid);
    if idxOrig = -1 then
      idxOrig := ALayeredImage.AddOriginalFromStream(FOriginalData, FOriginalGuid, true);

    if not FOriginalBitmapStored then
    begin
      tempIdx := ALayeredImage.AddLayerFromOriginal(ALayeredImage.Original[idxOrig].Guid, FOriginalMatrix);
      ALayeredImage.RenderLayerFromOriginal(tempIdx, FOriginalDraft);
    end else
    begin
      tempIdx := ALayeredImage.AddOwnedLayer(GetBitmap);
      ALayeredImage.LayerOffset[tempIdx] := FInfo.Offset;
      ALayeredImage.LayerOriginalGuid[tempIdx] := ALayeredImage.OriginalGuid[idxOrig];
      ALayeredImage.LayerOriginalMatrix[tempIdx] := FOriginalMatrix;
      ALayeredImage.LayerOriginalRenderStatus[tempIdx] := FOriginalRenderStatus;
    end;
  end else
    tempIdx := ALayeredImage.AddOwnedLayer(GetBitmap);

  ApplyLayerInfo(FInfo,ALayeredImage,tempIdx);
  FRegistryData.Position := 0;
  ALayeredImage.LoadLayerRegistryFromStream(tempIdx, FRegistryData);
  ALayeredImage.InsertLayer(FIndex,tempIdx);
end;

procedure TStoredLayer.Replace(ALayeredImage: TBGRALayeredBitmap);
var
  idxOrig: Integer;
begin
  if Assigned(FOriginalData) then
  begin
    FOriginalData.Position:= 0;
    idxOrig := ALayeredImage.IndexOfOriginal(FOriginalGuid);
    if idxOrig = -1 then
      idxOrig := ALayeredImage.AddOriginalFromStream(FOriginalData, FOriginalGuid, true);
    if not FOriginalBitmapStored then
    begin
      ALayeredImage.LayerOriginalGuid[FIndex] := ALayeredImage.OriginalGuid[idxOrig];
      ALayeredImage.LayerOriginalMatrix[FIndex] := FOriginalMatrix;
      ALayeredImage.RenderLayerFromOriginal(FIndex, FOriginalDraft);
    end else
    begin
      ALayeredImage.SetLayerBitmap(FIndex,GetBitmap,True);
      ALayeredImage.LayerOffset[FIndex] := FInfo.Offset;
      ALayeredImage.LayerOriginalGuid[FIndex] := ALayeredImage.OriginalGuid[idxOrig];
      ALayeredImage.LayerOriginalMatrix[FIndex] := FOriginalMatrix;
      ALayeredImage.LayerOriginalRenderStatus[FIndex] := FOriginalRenderStatus;
    end;
  end else
    ALayeredImage.SetLayerBitmap(FIndex,GetBitmap,True);
  ALayeredImage.RemoveUnusedOriginals;

  ApplyLayerInfo(FInfo,ALayeredImage,FIndex);
  FRegistryData.Position := 0;
  ALayeredImage.LoadLayerRegistryFromStream(FIndex, FRegistryData);
end;

end.

