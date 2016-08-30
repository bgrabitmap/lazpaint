unit BGRAColorQuantization;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRAPalette, BGRABitmapTypes;

type
  TBGRAColorBox = class;
  TBGRAColorTree = class;
  TBGRAApproxPalette = class;
  TBiggestLeafMethod = (blMix, blApparentInterval, blWeight);

  { TDimensionMinMax }

  TDimensionMinMax = object
    Minimum: UInt32;
    Maximum: UInt32;
    function Size: UInt32;
    function Contains(AValue: UInt32): boolean;
    function PointLike: boolean;
    procedure SetAsPoint(AValue: UInt32);
    function GetCenter: UInt32;
    procedure GrowToInclude(AValue: UInt32);
  end;

  TColorDimension = (cdFast,cdRed,cdGreen,cdBlue,cdAlpha,cdRGB,cdRG,cdGB,cdRB,cdRInvG,cdGInvB,cdRInvB,cdRInvGB,cdGInvRB,cdBInvRG,
                     cdSaturation);
  TColorDimensions = set of TColorDimension;

  { TBGRAColorQuantizer }

  TBGRAColorQuantizer = class(TBGRACustomColorQuantizer)
  private
    FColors: ArrayOfWeightedColor;
    FPalette: TBGRAApproxPalette;
    FReductionColorCount: Integer;
    FReductionKeepContrast: boolean;
    FSeparateAlphaChannel: boolean;
    procedure Init(ABox: TBGRAColorBox);
    procedure NormalizeArrayOfColors(AColors: ArrayOfTBGRAPixel; ARedBounds, AGreenBounds, ABlueBounds, AAlphaBounds: TDimensionMinMax; AUniform: boolean);
    procedure NormalizeArrayOfColors(AColors: ArrayOfTBGRAPixel; AColorBounds, AAlphaBounds: TDimensionMinMax);
  protected
    function GetPalette: TBGRACustomApproxPalette; override;
    function GetSourceColor(AIndex: integer): TBGRAPixel; override;
    function GetSourceColorCount: Integer; override;
    function GetReductionColorCount: integer; override;
    procedure SetReductionColorCount(AValue: Integer); override;
  public
    constructor Create(APalette: TBGRACustomPalette; ASeparateAlphaChannel: boolean); override;
    constructor Create(ABitmap: TBGRACustomBitmap; AAlpha: TAlphaChannelPaletteOption); override;
    constructor Create(APalette: TBGRACustomPalette; ASeparateAlphaChannel: boolean; AReductionColorCount: integer); override;
    constructor Create(ABitmap: TBGRACustomBitmap; AAlpha: TAlphaChannelPaletteOption; AReductionColorCount: integer); override;
    destructor Destroy; override;
    procedure ApplyDitheringInplace(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap; ABounds: TRect); override;
    function GetDitheredBitmap(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap; ABounds: TRect): TBGRACustomBitmap; override;
    function GetDitheredBitmapIndexedData(ABitDepth: integer; AByteOrder: TRawImageByteOrder; AAlgorithm: TDitheringAlgorithm;
      ABitmap: TBGRACustomBitmap; out AScanlineSize: PtrInt): Pointer; override;
    procedure SaveBitmapToStream(AAlgorithm: TDitheringAlgorithm;
      ABitmap: TBGRACustomBitmap; AStream: TStream; AFormat: TBGRAImageFormat); override;
  end;

  { TBGRAApproxPalette }

  TBGRAApproxPalette = class(TBGRACustomApproxPalette)
  private
    FTree: TBGRAColorTree;
    FColors: ArrayOfWeightedColor;
  protected
    function GetCount: integer; override;
    function GetColorByIndex(AIndex: integer): TBGRAPixel; override;
    function GetWeightByIndex(AIndex: Integer): UInt32; override;
    procedure Init(const AColors: ArrayOfTBGRAPixel);
  public
    constructor Create(const AColors: ArrayOfTBGRAPixel); overload;
    constructor Create(const AColors: ArrayOfWeightedColor); overload;
    constructor Create(AOwnedSplitTree: TBGRAColorTree); overload;
    destructor Destroy; override;
    function ContainsColor(AValue: TBGRAPixel): boolean; override;
    function IndexOfColor(AValue: TBGRAPixel): integer; override;
    function FindNearestColor(AValue: TBGRAPixel): TBGRAPixel; override;
    function FindNearestColorIndex(AValue: TBGRAPixel): integer; override;
    function GetAsArrayOfColor: ArrayOfTBGRAPixel; override;
    function GetAsArrayOfWeightedColor: ArrayOfWeightedColor; override;
  end;

  { TBGRAApproxPaletteViaLargerPalette }

  TBGRAApproxPaletteViaLargerPalette = class(TBGRAApproxPalette)
  private
    FLarger: TBGRACustomApproxPalette;
    FLargerColors: array of record
      approxColor: TBGRAPixel;
      approxColorIndex: integer;
    end;
    FLargerOwned: boolean;
    FTransparentColorIndex: integer;
  protected
    function FindNearestLargerColorIndex(AValue: TBGRAPixel): integer; virtual;
    function SlowFindNearestColorIndex(AValue: TBGRAPixel): integer;
  public
    constructor Create(const AColors: ArrayOfTBGRAPixel; ALarger: TBGRACustomApproxPalette; ALargerOwned: boolean);
    destructor Destroy; override;
    function FindNearestColor(AValue: TBGRAPixel): TBGRAPixel; override;
    function FindNearestColorIndex(AValue: TBGRAPixel): integer; override;
    function GetAsArrayOfWeightedColor: ArrayOfWeightedColor; override;
  end;

  TIsChannelStrictlyGreaterFunc = TBGRAPixelComparer;
  TIsChannelGreaterThanOrEqualToValueFunc = function (p : PBGRAPixel; v: UInt32): boolean;

  TColorBoxBounds = array[TColorDimension] of TDimensionMinMax;

  { TBGRAColorBox }

  TBGRAColorBox = class
  private
    FBounds: TColorBoxBounds;
    FTotalWeight: UInt32;
    FColors: ArrayOfWeightedColor;
    FDimensions: TColorDimensions;
    FPureTransparentColorCount: integer;
    function GetApparentInterval(ADimension: TColorDimension): UInt32;
    function GetAverageColor: TBGRAPixel;
    function GetAverageColorOrMainColor: TBGRAPixel;
    function GetBounds(ADimension: TColorDimension): TDimensionMinMax;
    function GetColorCount(ACountPureTransparent: boolean): integer;
    function GetHasPureTransparentColor: boolean;
    function GetInferiorColor: TBGRAPixel;
    function GetLargestApparentDimension: TColorDimension;
    function GetLargestApparentInterval: UInt32;
    function GetPointLike: boolean;
    function GetSuperiorColor: TBGRAPixel;
    procedure Init(AColors: ArrayOfWeightedColor; AOwner: boolean);
    procedure SortBy(ADimension: TColorDimension);
    function GetMedianIndex(ADimension : TColorDimension; AMinValue, AMaxValue: UInt32): integer;
  public
    constructor Create(ADimensions: TColorDimensions; AColors: ArrayOfWeightedColor; AOwner: boolean); overload;
    constructor Create(ADimensions: TColorDimensions; const AColors: ArrayOfTBGRAPixel; AAlpha: TAlphaChannelPaletteOption = acFullChannelInPalette); overload;
    constructor Create(ADimensions: TColorDimensions; ABounds: TColorBoxBounds); overload;
    constructor Create(ADimensions: TColorDimensions; APalette: TBGRACustomPalette); overload;
    constructor Create(ADimensions: TColorDimensions; ABitmap: TBGRACustomBitmap; AAlpha: TAlphaChannelPaletteOption); overload;
    constructor Create(ADimensions: TColorDimensions; AColors: PBGRAPixel; ANbPixels: integer; AAlpha: TAlphaChannelPaletteOption); overload;
    function BoundsContain(AColor: TBGRAPixel): boolean;
    function MedianCut(ADimension: TColorDimension; out SuperiorMiddle: UInt32): TBGRAColorBox;
    function Duplicate : TBGRAColorBox;
    property Bounds[ADimension: TColorDimension]: TDimensionMinMax read GetBounds;
    property ApparentInterval[AChannel: TColorDimension]: UInt32 read GetApparentInterval;
    property LargestApparentDimension: TColorDimension read GetLargestApparentDimension;
    property LargestApparentInterval: UInt32 read GetLargestApparentInterval;
    property PointLike: boolean read GetPointLike;
    property AverageColor: TBGRAPixel read GetAverageColor;
    property SuperiorColor: TBGRAPixel read GetSuperiorColor;
    property InferiorColor: TBGRAPixel read GetInferiorColor;
    property AverageColorOrMainColor: TBGRAPixel read GetAverageColorOrMainColor;
    function GetAsArrayOfColors(AIncludePureTransparent: boolean): ArrayOfTBGRAPixel;
    property TotalWeight: UInt32 read FTotalWeight;
    property ColorCount[ACountPureTransparent: boolean]: integer read GetColorCount;
    property HasPureTransparentColor: boolean read GetHasPureTransparentColor;
    property PureTransparentColorCount: integer read FPureTransparentColorCount;
  end;

  TBGRALeafColorMode = (lcAverage, lcCenter, lcExtremum, lcMix);

  { TBGRAColorTree }

  TBGRAColorTree = class
  private
    FLeaf: TBGRAColorBox;
    FIsLeaf: boolean;
    FLargestApparentInterval: integer;
    FWeight: UInt32;

    FLeafColor: TBGRAPixel;
    FLeafColorIndex: integer;
    FLeafColorComputed: boolean;
    FMinBorder, FMaxBorder: array[TColorDimension] of boolean;
    FCenterColor: TBGRAPixel;
    FAverageColor: TBGRAPixel;

    FPureTransparentColorCount: integer;
    FPureTransparentColorIndex: integer;
    FDimension: TColorDimension;
    FPixelValueComparer: TIsChannelGreaterThanOrEqualToValueFunc;
    FSuperiorMiddle: UInt32;
    FInferiorBranch, FSuperiorBranch: TBGRAColorTree;
    function GetApproximatedColorCount: integer;
    function GetHasPureTransparentColor: boolean;
    function GetLeafCount: integer;
    procedure Init(ALeaf: TBGRAColorBox; AOwned: boolean);
    procedure InternalComputeLeavesColor(ALeafColor: TBGRALeafColorMode; var AStartIndex: integer);
    procedure CheckColorComputed;
  public
    constructor Create(ABox: TBGRAColorBox; AOwned: boolean); overload;
    constructor Create(ADimensions: TColorDimensions; APalette: TBGRACustomPalette); overload;
    constructor Create(ADimensions: TColorDimensions; ABitmap: TBGRACustomBitmap; AAlpha: TAlphaChannelPaletteOption); overload;
    destructor Destroy; override;
    procedure FreeLeaves;
    function FindBiggestLeaf(AMethod: TBiggestLeafMethod): TBGRAColorTree;
    property LargestApparentInterval: integer read FLargestApparentInterval;
    property Weight: UInt32 read FWeight;
    property IsLeaf: boolean read FIsLeaf;
    function TrySplitLeaf: boolean;
    procedure ComputeLeavesColor(ALeafColor: TBGRALeafColorMode);
    function ApproximateColor(AColor: TBGRAPixel): TBGRAPixel;
    function ApproximateColorIndex(AColor: TBGRAPixel): integer;
    function GetAsArrayOfApproximatedColors: ArrayOfTBGRAPixel;
    function GetAsArrayOfWeightedColors: ArrayOfWeightedColor;
    procedure SplitIntoPalette(ACount: integer; AMethod: TBiggestLeafMethod;
      ALeafColor: TBGRALeafColorMode);
    function SplitIntoPaletteWithSubPalette(ACount: integer; AMethod: TBiggestLeafMethod;
      ALeafColor: TBGRALeafColorMode; ASubPaletteCount: integer): ArrayOfTBGRAPixel;
    property LeafCount: integer read GetLeafCount;
    property ApproximatedColorCount: integer read GetApproximatedColorCount;
    property HasPureTransparentColor: boolean read GetHasPureTransparentColor;
    property PureTransparentColorCount: integer read FPureTransparentColorCount;
  end;

function GetPixelStrictComparer(ADimension: TColorDimension): TIsChannelStrictlyGreaterFunc;
function GetPixelValueComparer(ADimension: TColorDimension): TIsChannelGreaterThanOrEqualToValueFunc;
function BGRAColorCount(ABitmap: TBGRACustomBitmap; AAlpha: TAlphaChannelPaletteOption): integer;

const AllColorDimensions = [cdRed,cdGreen,cdBlue,cdAlpha,cdRGB,cdRG,cdGB,cdRB,cdRInvG,cdGInvB,cdRInvB,cdRInvGB,cdGInvRB,cdBInvRG,
                            cdSaturation];

implementation

uses BGRADithering, FPimage, FPWriteBMP, BGRAWritePNG;

const MedianMinPercentage = 0.2;

const RedShift = 1;
      GreenShift = 2;
      AlphaShift = 1;
      SaturationShift = 2;

function GetDimensionValue(APixel: TBGRAPixel; ADimension: TColorDimension): UInt32;
var v: UInt32;
begin
  case ADimension of
  cdFast: result := DWord(APixel);
  cdRed: result := GammaExpansionTab[APixel.red] shl RedShift;
  cdGreen: result := GammaExpansionTab[APixel.green] shl GreenShift;
  cdBlue: result := GammaExpansionTab[APixel.blue];
  cdAlpha: result := (APixel.alpha + (APixel.alpha shl 8)) shl AlphaShift;
  cdRGB: result := GammaExpansionTab[APixel.blue] + (GammaExpansionTab[APixel.red] shl RedShift) + (GammaExpansionTab[APixel.green] shl GreenShift);
  cdRG: result := (GammaExpansionTab[APixel.red] shl RedShift) + (GammaExpansionTab[APixel.green] shl GreenShift);
  cdGB: result := GammaExpansionTab[APixel.blue] + (GammaExpansionTab[APixel.green] shl GreenShift);
  cdRB: result := (GammaExpansionTab[APixel.red] shl RedShift) + GammaExpansionTab[APixel.blue];
  cdRInvG: result := (GammaExpansionTab[APixel.red] shl RedShift) + ((not GammaExpansionTab[APixel.green]) shl GreenShift);
  cdGInvB: result := (GammaExpansionTab[APixel.green] shl GreenShift) + (not GammaExpansionTab[APixel.blue]);
  cdRInvB: result := (GammaExpansionTab[APixel.red] shl RedShift) + (not GammaExpansionTab[APixel.blue]);
  cdRInvGB: result := (GammaExpansionTab[APixel.red] shl RedShift) + ((not GammaExpansionTab[APixel.green]) shl GreenShift) + (not GammaExpansionTab[APixel.blue]);
  cdGInvRB: result := (GammaExpansionTab[APixel.green] shl GreenShift) + ((not GammaExpansionTab[APixel.red]) shl RedShift) + (not GammaExpansionTab[APixel.blue]);
  cdBInvRG: result := (GammaExpansionTab[APixel.blue]) + ((not GammaExpansionTab[APixel.red]) shl RedShift) + ((not GammaExpansionTab[APixel.green]) shl GreenShift);
  cdSaturation: with GammaExpansion(APixel) do
    begin
       v := red;
       if green>v then v := green;
       if blue>v then v := blue;
       result := v;
       v := red;
       if green<v then v := green;
       if blue<v then v := blue;
       result -= v;
       result := result shl SaturationShift;
    end
  else raise exception.Create('Unknown dimension');
  end;
end;

function IsRGBGreater(p1, p2: PBGRAPixel): boolean;
begin
  result := ((GammaExpansionTab[p1^.red] shl RedShift)+(GammaExpansionTab[p1^.green] shl GreenShift)+GammaExpansionTab[p1^.blue]) >
     ((GammaExpansionTab[p2^.red] shl RedShift)+(GammaExpansionTab[p2^.green] shl GreenShift)+GammaExpansionTab[p2^.blue]);
end;

function IsRGBGreaterThanValue(p: PBGRAPixel;
  v: UInt32): boolean;
begin
  with p^ do
    result := ((GammaExpansionTab[red] shl RedShift)+(GammaExpansionTab[green] shl GreenShift)+GammaExpansionTab[blue]) >= v;
end;

function IsRGGreater(p1, p2: PBGRAPixel): boolean;
begin
  result := ((GammaExpansionTab[p1^.red] shl RedShift)+(GammaExpansionTab[p1^.green] shl GreenShift)) >
     ((GammaExpansionTab[p2^.red] shl RedShift)+(GammaExpansionTab[p2^.green] shl GreenShift));
end;

function IsRGGreaterThanValue(p: PBGRAPixel;
  v: UInt32): boolean;
begin
  with p^ do
    result := ((GammaExpansionTab[red] shl RedShift)+(GammaExpansionTab[green] shl GreenShift)) >= v;
end;

function IsGBGreater(p1, p2: PBGRAPixel): boolean;
begin
  result := ((GammaExpansionTab[p1^.green] shl GreenShift)+GammaExpansionTab[p1^.blue]) >
  ((GammaExpansionTab[p2^.green] shl GreenShift)+GammaExpansionTab[p2^.blue]);
end;

function IsGBGreaterThanValue(p: PBGRAPixel;
  v: UInt32): boolean;
begin
  with p^ do
    result := ((GammaExpansionTab[green] shl GreenShift)+GammaExpansionTab[blue]) >= v;
end;

function IsRBGreater(p1, p2: PBGRAPixel): boolean;
begin
  result := ((GammaExpansionTab[p1^.red] shl RedShift)+GammaExpansionTab[p1^.blue]) >
  ((GammaExpansionTab[p2^.red] shl RedShift)+GammaExpansionTab[p2^.blue]);
end;

function IsRBGreaterThanValue(p: PBGRAPixel;
  v: UInt32): boolean;
begin
  with p^ do
    result := ((GammaExpansionTab[red] shl RedShift)+GammaExpansionTab[blue]) >= v;
end;

function IsRInvGGreater(p1, p2: PBGRAPixel
  ): boolean;
begin
  result := (GammaExpansionTab[p1^.red]+ ((not GammaExpansionTab[p1^.green]) shl GreenShift)) >
          (GammaExpansionTab[p2^.red]+((not GammaExpansionTab[p2^.green]) shl GreenShift));
end;

function IsRInvGGreaterThanValue(p: PBGRAPixel;
  v: UInt32): boolean;
begin
  with p^ do
    result := (GammaExpansionTab[red]+((not GammaExpansionTab[green]) shl GreenShift)) >= v;
end;

function IsGInvBGreater(p1, p2: PBGRAPixel
  ): boolean;
begin
  result := (GammaExpansionTab[p1^.green] shl GreenShift + not GammaExpansionTab[p1^.blue]) >
     (GammaExpansionTab[p2^.green] shl GreenShift + not GammaExpansionTab[p2^.blue]);
end;

function IsGInvBGreaterThanValue(p: PBGRAPixel;
  v: UInt32): boolean;
begin
  with p^ do
    result := (GammaExpansionTab[green] shl GreenShift + not GammaExpansionTab[blue]) >= v;
end;

function IsRInvBGreater(p1, p2: PBGRAPixel
  ): boolean;
begin
  result := (GammaExpansionTab[p1^.red] shl RedShift + not GammaExpansionTab[p1^.blue]) >
    (GammaExpansionTab[p2^.red] shl RedShift + not GammaExpansionTab[p2^.blue]);
end;

function IsRInvBGreaterThanValue(p: PBGRAPixel;
  v: UInt32): boolean;
begin
  with p^ do
    result := (GammaExpansionTab[red] shl RedShift + not GammaExpansionTab[blue]) >= v;
end;

function IsRInvGBGreater(p1, p2: PBGRAPixel
  ): boolean;
begin
  result := (GammaExpansionTab[p1^.red] shl RedShift + ((not GammaExpansionTab[p1^.green]) shl GreenShift) + not GammaExpansionTab[p1^.blue]) >
          (GammaExpansionTab[p2^.red] shl RedShift + ((not GammaExpansionTab[p2^.green]) shl GreenShift) + not GammaExpansionTab[p2^.blue]);
end;

function IsRInvGBGreaterThanValue(p: PBGRAPixel;
  v: UInt32): boolean;
begin
  with p^ do
    result := (GammaExpansionTab[red] shl RedShift + ((not GammaExpansionTab[green]) shl GreenShift) + not GammaExpansionTab[blue]) >= v;
end;

function IsGInvRBGreater(p1, p2: PBGRAPixel
  ): boolean;
begin
  result := (GammaExpansionTab[p1^.green] shl GreenShift + ((not GammaExpansionTab[p1^.red]) shl RedShift) + not GammaExpansionTab[p1^.blue]) >
     (GammaExpansionTab[p2^.green] shl GreenShift + ((not GammaExpansionTab[p2^.red]) shl RedShift) + not GammaExpansionTab[p2^.blue]);
end;

function IsGInvRBGreaterThanValue(p: PBGRAPixel;
  v: UInt32): boolean;
begin
  with p^ do
    result := (GammaExpansionTab[green] shl GreenShift + ((not GammaExpansionTab[red]) shl RedShift) + not GammaExpansionTab[blue]) >= v;
end;

function IsBInvRGGreater(p1, p2: PBGRAPixel
  ): boolean;
begin
  result := (GammaExpansionTab[p1^.blue] + ((not GammaExpansionTab[p1^.red]) shl RedShift) + ((not GammaExpansionTab[p1^.green]) shl GreenShift)) >
    (GammaExpansionTab[p2^.blue] + ((not GammaExpansionTab[p2^.red]) shl RedShift) + ((not GammaExpansionTab[p2^.green]) shl GreenShift));
end;

function IsBInvRGGreaterThanValue(p: PBGRAPixel;
  v: UInt32): boolean;
begin
  with p^ do
    result := (GammaExpansionTab[blue] + ((not GammaExpansionTab[red]) shl RedShift) + ((not GammaExpansionTab[green]) shl GreenShift)) >= v;
end;

function IsSaturationGreater(p1, p2: PBGRAPixel): boolean;
begin
  result := GetDimensionValue(p1^,cdSaturation) > GetDimensionValue(p2^,cdSaturation);
end;

function IsSaturationGreaterThanValue(p: PBGRAPixel;
  v: UInt32): boolean;
begin
  result := GetDimensionValue(p^,cdSaturation) >= v;
end;

function IsRedGreater(p1, p2: PBGRAPixel): boolean;
begin
  result := p1^.red > p2^.red;
end;

function IsRedGreaterThanValue(p: PBGRAPixel;
  v: UInt32): boolean;
begin
  result := GammaExpansionTab[p^.red] shl RedShift >= v;
end;

function IsGreenGreater(p1, p2: PBGRAPixel
  ): boolean;
begin
  result := p1^.green > p2^.green;
end;

function IsGreenGreaterThanValue(p: PBGRAPixel;
  v: UInt32): boolean;
begin
  result := GammaExpansionTab[p^.green] shl GreenShift >= v;
end;

function IsBlueGreater(p1, p2: PBGRAPixel
  ): boolean;
begin
  result := p1^.blue > p2^.blue;
end;

function IsBlueGreaterThanValue(p: PBGRAPixel;
  v: UInt32): boolean;
begin
  result := GammaExpansionTab[p^.blue] >= v;
end;

function IsAlphaGreater(p1, p2: PBGRAPixel
  ): boolean;
begin
  result := p1^.alpha > p2^.alpha;
end;

function IsAlphaGreaterThanValue(p: PBGRAPixel;
  v: UInt32): boolean;
begin
  result := (p^.alpha + p^.alpha shl 8) shl AlphaShift >= v;
end;

function IsDWordGreater(p1, p2: PBGRAPixel
  ): boolean;
begin
  result := DWord(p1^) > DWord(p2^);
end;

function IsDWordGreaterThanValue(p: PBGRAPixel;
  v: UInt32): boolean;
begin
  result := DWord(p^) >= v;
end;

function GetPixelStrictComparer(ADimension: TColorDimension
  ): TIsChannelStrictlyGreaterFunc;
begin
  case ADimension of
  cdFast: result := @IsDWordGreater;
  cdRed: result := @IsRedGreater;
  cdGreen: result := @IsGreenGreater;
  cdBlue: result := @IsBlueGreater;
  cdAlpha: result := @IsAlphaGreater;
  cdRGB: result := @IsRGBGreater;
  cdRG: result := @IsRGGreater;
  cdGB: result := @IsGBGreater;
  cdRB: result := @IsRBGreater;
  cdRInvG: result := @IsRInvGGreater;
  cdGInvB: result := @IsGInvBGreater;
  cdRInvB: result := @IsRInvBGreater;
  cdRInvGB: result := @IsRInvGBGreater;
  cdGInvRB: result := @IsGInvRBGreater;
  cdBInvRG: result := @IsBInvRGGreater;
  cdSaturation: result := @IsSaturationGreater;
  else raise Exception.Create('Unknown dimension');
  end;
end;

function GetPixelValueComparer(ADimension: TColorDimension
  ): TIsChannelGreaterThanOrEqualToValueFunc;
begin
  case ADimension of
  cdFast: result := @IsDWordGreaterThanValue;
  cdRed: result := @IsRedGreaterThanValue;
  cdGreen: result := @IsGreenGreaterThanValue;
  cdBlue: result := @IsBlueGreaterThanValue;
  cdAlpha: result := @IsAlphaGreaterThanValue;
  cdRGB: result := @IsRGBGreaterThanValue;
  cdRG: result := @IsRGGreaterThanValue;
  cdGB: result := @IsGBGreaterThanValue;
  cdRB: result := @IsRBGreaterThanValue;
  cdRInvG: result := @IsRInvGGreaterThanValue;
  cdGInvB: result := @IsGInvBGreaterThanValue;
  cdRInvB: result := @IsRInvBGreaterThanValue;
  cdRInvGB: result := @IsRInvGBGreaterThanValue;
  cdGInvRB: result := @IsGInvRBGreaterThanValue;
  cdBInvRG: result := @IsBInvRGGreaterThanValue;
  cdSaturation: result := @IsSaturationGreaterThanValue;
  else raise Exception.Create('Unknown dimension');
  end;
end;

function BGRAColorCount(ABitmap: TBGRACustomBitmap;
  AAlpha: TAlphaChannelPaletteOption): integer;
var
  box: TBGRAColorBox;
begin
  box := TBGRAColorBox.Create(AllColorDimensions,ABitmap,AAlpha);
  result := box.ColorCount[True];
  box.Free;
end;

const
  ApproxPaletteDimensions = [cdAlpha,cdRInvG,cdGInvB,cdRInvB,cdRInvGB,cdGInvRB,cdBInvRG,cdRGB];

{ TBGRAApproxPaletteViaLargerPalette }

function TBGRAApproxPaletteViaLargerPalette.FindNearestLargerColorIndex(
  AValue: TBGRAPixel): integer;
begin
  result := FLarger.FindNearestColorIndex(AValue);
end;

function TBGRAApproxPaletteViaLargerPalette.SlowFindNearestColorIndex(
  AValue: TBGRAPixel): integer;
var diff,curDiff: NativeInt;
  i: NativeInt;
begin
  if AValue.alpha = 0 then
  begin
    result := FTransparentColorIndex;
    exit;
  end;
  diff := BGRAWordDiff(AValue, FColors[0].Color);
  result := 0;
  for i := 0 to high(FColors) do
  begin
    curDiff := BGRAWordDiff(AValue, FColors[i].Color);
    if curDiff < diff then
    begin
      result := i;
      diff := curDiff;
    end;
  end;
end;

constructor TBGRAApproxPaletteViaLargerPalette.Create(
  const AColors: ArrayOfTBGRAPixel; ALarger: TBGRACustomApproxPalette; ALargerOwned: boolean);
var i: integer;
  largeWeighted: ArrayOfWeightedColor;
begin
  inherited Create(AColors);
  FTransparentColorIndex:= -1;
  for i := 0 to high(FColors) do
  begin
    FColors[i].Weight := 0;
    if FColors[i].Color.alpha = 0 then FTransparentColorIndex:= i;
  end;
  FLarger := ALarger;
  FLargerOwned := ALargerOwned;
  largeWeighted := FLarger.GetAsArrayOfWeightedColor;
  setlength(FLargerColors, length(largeWeighted));
  for i := 0 to high(FLargerColors) do
  with FLargerColors[i] do
  begin
    approxColorIndex := SlowFindNearestColorIndex(largeWeighted[i].Color);
    if approxColorIndex = -1 then
      approxColor := BGRAPixelTransparent
    else
    begin
      approxColor := FColors[approxColorIndex].Color;
      inc(FColors[approxColorIndex].Weight, largeWeighted[i].Weight);
    end;
  end;
end;

destructor TBGRAApproxPaletteViaLargerPalette.Destroy;
begin
  if FLargerOwned then FreeAndNil(FLarger);
  inherited Destroy;
end;

function TBGRAApproxPaletteViaLargerPalette.FindNearestColor(AValue: TBGRAPixel
  ): TBGRAPixel;
var index: integer;
begin
  index := FindNearestLargerColorIndex(AValue);
  if index = -1 then
    result := BGRAPixelTransparent
  else
    Result:= FLargerColors[index].approxColor;
end;

function TBGRAApproxPaletteViaLargerPalette.FindNearestColorIndex(
  AValue: TBGRAPixel): integer;
var index: integer;
begin
  index := FindNearestLargerColorIndex(AValue);
  if index = -1 then
    result := -1
  else
    Result:= FLargerColors[index].approxColorIndex;
end;

function TBGRAApproxPaletteViaLargerPalette.GetAsArrayOfWeightedColor: ArrayOfWeightedColor;
var
  i: Integer;
begin
  setlength(result, length(FColors));
  for i := 0 to high(FColors) do
    result[i] := FColors[i];
end;

{ TBGRAApproxPalette }

function TBGRAApproxPalette.GetCount: integer;
begin
  result := length(FColors);
end;

function TBGRAApproxPalette.GetColorByIndex(AIndex: integer): TBGRAPixel;
begin
  if (AIndex < 0) or (AIndex >= length(FColors)) then
    raise ERangeError.Create('Index out of bounds');
  result := FColors[AIndex].Color;
end;

function TBGRAApproxPalette.GetWeightByIndex(AIndex: Integer): UInt32;
begin
  if (AIndex < 0) or (AIndex >= length(FColors)) then
    raise ERangeError.Create('Index out of bounds');
  result := FColors[AIndex].Weight;
end;

procedure TBGRAApproxPalette.Init(const AColors: ArrayOfTBGRAPixel);
var
  weightedColors: ArrayOfWeightedColor;
  i: NativeInt;
begin
  setlength(weightedColors, length(AColors));
  for i := 0 to high(weightedColors) do
  with weightedColors[i] do
  begin
    Color := AColors[i];
    Weight := 1;
  end;
  FTree := TBGRAColorTree.Create(TBGRAColorBox.Create(ApproxPaletteDimensions,weightedColors,True),True);
  FTree.SplitIntoPalette(length(AColors),blApparentInterval,lcAverage);

  FColors := FTree.GetAsArrayOfWeightedColors;
end;

constructor TBGRAApproxPalette.Create(const AColors: ArrayOfTBGRAPixel);
begin
  Init(AColors);
end;

constructor TBGRAApproxPalette.Create(const AColors: ArrayOfWeightedColor);
begin
  FTree := TBGRAColorTree.Create(TBGRAColorBox.Create(ApproxPaletteDimensions,AColors,True),True);
  FTree.SplitIntoPalette(length(AColors),blApparentInterval,lcAverage);

  FColors := FTree.GetAsArrayOfWeightedColors;
end;

constructor TBGRAApproxPalette.Create(AOwnedSplitTree: TBGRAColorTree);
begin
  FTree := AOwnedSplitTree;
  FColors := FTree.GetAsArrayOfWeightedColors;
end;

destructor TBGRAApproxPalette.Destroy;
begin
  FreeAndNil(FTree);
  inherited Destroy;
end;

function TBGRAApproxPalette.ContainsColor(AValue: TBGRAPixel): boolean;
begin
  result := (IndexOfColor(AValue)<>-1);
end;

function TBGRAApproxPalette.IndexOfColor(AValue: TBGRAPixel): integer;
begin
  result := FTree.ApproximateColorIndex(AValue);
  if (result <> -1) and not (DWord(FColors[result].Color) = DWord(AValue)) then result := -1;
end;

function TBGRAApproxPalette.FindNearestColor(AValue: TBGRAPixel): TBGRAPixel;
begin
  result := FTree.ApproximateColor(AValue);
end;

function TBGRAApproxPalette.FindNearestColorIndex(AValue: TBGRAPixel): integer;
begin
  result := FTree.ApproximateColorIndex(AValue);
end;

function TBGRAApproxPalette.GetAsArrayOfColor: ArrayOfTBGRAPixel;
var
  i: NativeInt;
begin
  setlength(result, length(FColors));
  for i := 0 to high(result) do
    result[i] := FColors[i].Color;
end;

function TBGRAApproxPalette.GetAsArrayOfWeightedColor: ArrayOfWeightedColor;
var
  i: NativeInt;
begin
  if Assigned(FTree) then
    result := FTree.GetAsArrayOfWeightedColors
  else
  begin
    setlength(result, length(FColors));
    for i := 0 to high(result) do
      result[i] := FColors[i];
  end;
end;

{ TBGRAColorQuantizer }

procedure TBGRAColorQuantizer.Init(ABox: TBGRAColorBox);
begin
  FColors := ABox.FColors;
  if ABox.HasPureTransparentColor then
  begin
    setlength(FColors,length(FColors)+1);
    with FColors[high(FColors)] do
    begin
      Color := BGRAPixelTransparent;
      Weight:= ABox.PureTransparentColorCount;
    end;
  end;
  ABox.FColors := nil;
  ABox.Free;
  FReductionColorCount := 256;
  FReductionKeepContrast := true;
end;

procedure TBGRAColorQuantizer.SetReductionColorCount(AValue: Integer);
begin
  if AValue < 1 then AValue := 1;
  if FReductionColorCount=AValue then Exit;
  FReductionColorCount:=AValue;
  FreeAndNil(FPalette);
end;

procedure TBGRAColorQuantizer.NormalizeArrayOfColors(
  AColors: ArrayOfTBGRAPixel; ARedBounds, AGreenBounds, ABlueBounds,
  AAlphaBounds: TDimensionMinMax; AUniform: boolean);
var
  curRedBounds, curGreenBounds, curBlueBounds, curAlphaBounds: TDimensionMinMax;
  RedSub,RedMul,RedDiv,RedAdd: NativeUInt;
  GreenSub,GreenMul,GreenDiv,GreenAdd: NativeUInt;
  BlueSub,BlueMul,BlueDiv,BlueAdd: NativeUInt;
  AlphaSub,AlphaMul,AlphaDiv,AlphaAdd: NativeUInt;
  i: NativeInt;
  colorBounds: TDimensionMinMax;
begin
  if length(AColors)=0 then exit;
  if AUniform then
  begin
    colorBounds := ABlueBounds;
    colorBounds.GrowToInclude(AGreenBounds.Minimum shr GreenShift);
    colorBounds.GrowToInclude(AGreenBounds.Maximum shr GreenShift);
    colorBounds.GrowToInclude(ARedBounds.Minimum shr RedShift);
    colorBounds.GrowToInclude(ARedBounds.Maximum shr RedShift);
    NormalizeArrayOfColors(AColors, colorBounds, AAlphaBounds);
    exit;
  end;
  curRedBounds.SetAsPoint(GetDimensionValue(AColors[0],cdRed));
  curGreenBounds.SetAsPoint(GetDimensionValue(AColors[0],cdGreen));
  curBlueBounds.SetAsPoint(GetDimensionValue(AColors[0],cdBlue));
  curAlphaBounds.SetAsPoint(GetDimensionValue(AColors[0],cdAlpha));
  for i := 1 to high(AColors) do
  with AColors[i] do
  begin
    curRedBounds.GrowToInclude(GetDimensionValue(AColors[i],cdRed));
    curGreenBounds.GrowToInclude(GetDimensionValue(AColors[i],cdGreen));
    curBlueBounds.GrowToInclude(GetDimensionValue(AColors[i],cdBlue));
    curAlphaBounds.GrowToInclude(GetDimensionValue(AColors[i],cdAlpha));
  end;
  RedSub := curRedBounds.Minimum shr RedShift;
  RedMul := ARedBounds.Size shr RedShift;
  RedDiv := curRedBounds.Size shr RedShift;
  RedAdd := ARedBounds.Minimum shr RedShift;
  if RedDiv = 0 then RedDiv := 1;
  GreenSub := curGreenBounds.Minimum shr GreenShift;
  GreenMul := AGreenBounds.Size shr GreenShift;
  GreenDiv := curGreenBounds.Size shr GreenShift;
  GreenAdd := AGreenBounds.Minimum shr GreenShift;
  if GreenDiv = 0 then GreenDiv := 1;
  BlueSub := curBlueBounds.Minimum;
  BlueMul := ABlueBounds.Size;
  BlueDiv := curBlueBounds.Size;
  BlueAdd := ABlueBounds.Minimum;
  if BlueDiv = 0 then BlueDiv := 1;
  AlphaSub := curAlphaBounds.Minimum shr (AlphaShift+8);
  AlphaMul := AAlphaBounds.Size shr (AlphaShift+8);
  AlphaDiv := curAlphaBounds.Size shr (AlphaShift+8);
  AlphaAdd := AAlphaBounds.Minimum shr (AlphaShift+8);
  if AlphaDiv = 0 then AlphaDiv := 1;
  for i := 0 to high(AColors) do
  with AColors[i] do
  begin
    red := GammaCompressionTab[((GammaExpansionTab[red]-RedSub)*RedMul+(RedDiv shr 1)) div RedDiv + RedAdd];
    green := GammaCompressionTab[((GammaExpansionTab[green]-GreenSub)*GreenMul+(GreenDiv shr 1)) div GreenDiv + GreenAdd];
    blue := GammaCompressionTab[((GammaExpansionTab[blue]-BlueSub)*BlueMul+(BlueDiv shr 1)) div BlueDiv + BlueAdd];
    alpha := ((alpha-AlphaSub)*AlphaMul+(AlphaDiv shr 1)) div AlphaDiv + AlphaAdd;
  end;
end;

procedure TBGRAColorQuantizer.NormalizeArrayOfColors(
  AColors: ArrayOfTBGRAPixel; AColorBounds, AAlphaBounds: TDimensionMinMax);
var
  curColorBounds, curAlphaBounds: TDimensionMinMax;
  ColorSub,ColorMul,ColorDiv,ColorAdd: NativeUInt;
  AlphaSub,AlphaMul,AlphaDiv,AlphaAdd: NativeUInt;
  i: NativeInt;
begin
  if length(AColors)=0 then exit;
  curColorBounds.SetAsPoint(GammaExpansionTab[AColors[0].red]);
  curColorBounds.GrowToInclude(GammaExpansionTab[AColors[0].green]);
  curColorBounds.GrowToInclude(GammaExpansionTab[AColors[0].blue]);
  curAlphaBounds.SetAsPoint(AColors[0].alpha);
  for i := 1 to high(AColors) do
  with AColors[i] do
  begin
    curColorBounds.GrowToInclude(GammaExpansionTab[red]);
    curColorBounds.GrowToInclude(GammaExpansionTab[green]);
    curColorBounds.GrowToInclude(GammaExpansionTab[blue]);
    curAlphaBounds.GrowToInclude(alpha);
  end;
  ColorSub := curColorBounds.Minimum;
  ColorMul := AColorBounds.Size;
  ColorDiv := curColorBounds.Size;
  ColorAdd := AColorBounds.Minimum;
  if ColorDiv = 0 then ColorDiv := 1;
  AlphaSub := curAlphaBounds.Minimum;
  AlphaMul := AAlphaBounds.Size shr 8;
  AlphaDiv := curAlphaBounds.Size;
  AlphaAdd := AAlphaBounds.Minimum shr 8;
  if AlphaDiv = 0 then AlphaDiv := 1;
  for i := 0 to high(AColors) do
  with AColors[i] do
  begin
    red := GammaCompressionTab[((GammaExpansionTab[red]-ColorSub)*ColorMul+(ColorDiv shr 1)) div ColorDiv + ColorAdd];
    green := GammaCompressionTab[((GammaExpansionTab[green]-ColorSub)*ColorMul+(ColorDiv shr 1)) div ColorDiv + ColorAdd];
    blue := GammaCompressionTab[((GammaExpansionTab[blue]-ColorSub)*ColorMul+(ColorDiv shr 1)) div ColorDiv + ColorAdd];
    alpha := ((alpha-AlphaSub)*AlphaMul+(AlphaDiv shr 1)) div AlphaDiv + AlphaAdd;
  end;
end;

function TBGRAColorQuantizer.GetSourceColorCount: Integer;
begin
  result := length(FColors);
end;

function TBGRAColorQuantizer.GetReductionColorCount: integer;
begin
  result := FReductionColorCount;
end;

function TBGRAColorQuantizer.GetPalette: TBGRACustomApproxPalette;
var
  tree: TBGRAColorTree;

  procedure MakeTreeErrorDiffusionFriendly;
  var moreColors: ArrayOfWeightedColor;
    box: TBGRAColorBox;
  begin
    moreColors := tree.GetAsArrayOfWeightedColors;
    tree.free;
    box := TBGRAColorBox.Create([cdRed,cdGreen,cdBlue,cdAlpha],moreColors,True);
    tree := TBGRAColorTree.Create(box,True);
    tree.SplitIntoPalette(box.ColorCount[true], blApparentInterval, lcAverage);
  end;

var
  originalBox: TBGRAColorBox;
  colors: ArrayOfTBGRAPixel;
  bounds: array[TColorDimension] of TDimensionMinMax;
  nbLarge,nbOriginal: integer;

begin
  if not Assigned(FPalette) then
  if FReductionColorCount >= length(FColors) then
  begin
    originalBox := TBGRAColorBox.Create([cdRed,cdGreen,cdBlue,cdAlpha],FColors, False);
    tree := TBGRAColorTree.Create(originalBox,True);
    tree.SplitIntoPalette(originalBox.ColorCount[true], blApparentInterval, lcAverage);
    FPalette := TBGRAApproxPalette.Create(tree);
  end else
  begin
    originalBox := TBGRAColorBox.Create(AllColorDimensions, FColors, False);
    bounds[cdRed] := originalBox.Bounds[cdRed];
    bounds[cdGreen] := originalBox.Bounds[cdGreen];
    bounds[cdBlue] := originalBox.Bounds[cdBlue];
    bounds[cdAlpha] := originalBox.Bounds[cdAlpha];
    if originalBox.HasPureTransparentColor then bounds[cdAlpha].Minimum := 0;
    if FReductionColorCount = 1 then
    begin
      setlength(colors,1);
      colors[0] := originalBox.AverageColor;
      originalBox.Free;
      FPalette := TBGRAApproxPalette.Create(colors);
    end else
    begin
      tree := TBGRAColorTree.Create(originalBox,True);
      if FReductionColorCount <= 64 then
      begin
        nbLarge := 128;
        nbOriginal := originalBox.ColorCount[True];
        if nbOriginal < 128 then nbLarge:= nbOriginal;
        colors := tree.SplitIntoPaletteWithSubPalette(nbLarge, blMix,lcMix, FReductionColorCount);
        MakeTreeErrorDiffusionFriendly;
        if FReductionColorCount <= 4 then
          NormalizeArrayOfColors(colors, bounds[cdRed],bounds[cdGreen],bounds[cdBlue],bounds[cdAlpha],true);
        FPalette := TBGRAApproxPaletteViaLargerPalette.Create(colors, TBGRAApproxPalette.Create(tree), True);
      end else
      begin
        tree.SplitIntoPalette(FReductionColorCount, blMix,lcMix);
        MakeTreeErrorDiffusionFriendly;
        FPalette := TBGRAApproxPalette.Create(tree);
      end;
    end;
  end;
  result := FPalette;
end;

function TBGRAColorQuantizer.GetSourceColor(AIndex: integer): TBGRAPixel;
begin
  if (AIndex < 0) or (AIndex >= length(FColors)) then
    raise ERangeError.Create('Index out of bounds');
  result := FColors[AIndex].Color;
end;

constructor TBGRAColorQuantizer.Create(APalette: TBGRACustomPalette; ASeparateAlphaChannel: boolean);
begin
  FSeparateAlphaChannel:= ASeparateAlphaChannel;
  Init(TBGRAColorBox.Create(AllColorDimensions, APalette));
end;

constructor TBGRAColorQuantizer.Create(ABitmap: TBGRACustomBitmap; AAlpha: TAlphaChannelPaletteOption);
begin
  FSeparateAlphaChannel:= (AAlpha = acIgnore);
  Init(TBGRAColorBox.Create(AllColorDimensions, ABitmap, AAlpha));
end;

constructor TBGRAColorQuantizer.Create(APalette: TBGRACustomPalette;
  ASeparateAlphaChannel: boolean; AReductionColorCount: integer);
begin
  FSeparateAlphaChannel:= ASeparateAlphaChannel;
  Init(TBGRAColorBox.Create(AllColorDimensions, APalette));
  ReductionColorCount := AReductionColorCount;
end;

constructor TBGRAColorQuantizer.Create(ABitmap: TBGRACustomBitmap;
  AAlpha: TAlphaChannelPaletteOption; AReductionColorCount: integer);
begin
  FSeparateAlphaChannel:= (AAlpha = acIgnore);
  Init(TBGRAColorBox.Create(AllColorDimensions, ABitmap, AAlpha));
  ReductionColorCount := AReductionColorCount;
end;

destructor TBGRAColorQuantizer.Destroy;
begin
  FreeAndNil(FPalette);
  inherited Destroy;
end;

procedure TBGRAColorQuantizer.ApplyDitheringInplace(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap;
  ABounds: TRect);
var task: TDitheringTask;
begin
  task := CreateDitheringTask(AAlgorithm, ABitmap, ReducedPalette, FSeparateAlphaChannel, ABounds);
  task.Destination := ABitmap;
  task.Execute;
  task.Free;
end;

function TBGRAColorQuantizer.GetDitheredBitmap(AAlgorithm: TDitheringAlgorithm;
  ABitmap: TBGRACustomBitmap; ABounds: TRect): TBGRACustomBitmap;
var task: TDitheringTask;
begin
  task := CreateDitheringTask(AAlgorithm, ABitmap, ReducedPalette, FSeparateAlphaChannel, ABounds);
  result := task.Execute;
  task.Free;
end;

function TBGRAColorQuantizer.GetDitheredBitmapIndexedData(
  ABitDepth: integer; AByteOrder: TRawImageByteOrder; AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap;
  out AScanlineSize: PtrInt): Pointer;
var
  indexer: TDitheringToIndexedImage;
begin
  indexer := TDitheringToIndexedImage.Create(ReducedPalette, FSeparateAlphaChannel, ABitDepth, AByteOrder);
  indexer.DefaultTransparentColorIndex := ReducedPalette.IndexOfColor(BGRAPixelTransparent);
  AScanlineSize:= indexer.ComputeMinimumScanlineSize(ABitmap.Width);
  result := indexer.DitherImage(AAlgorithm, ABitmap, AScanlineSize);
  indexer.Free;
end;

procedure TBGRAColorQuantizer.SaveBitmapToStream(AAlgorithm: TDitheringAlgorithm; ABitmap: TBGRACustomBitmap;
  AStream: TStream; AFormat: TBGRAImageFormat);
var
  dithered: TBGRACustomBitmap;
  hasTransp: boolean;
  writer: TFPCustomImageWriter;
  depth: integer;
begin
  dithered := GetDitheredBitmap(AAlgorithm, ABitmap);
  try
    ReducedPalette.AssignTo(dithered);
    hasTransp := dithered.HasTransparentPixels;
    writer := CreateBGRAImageWriter(AFormat, hasTransp);
    try
      if writer is TBGRAWriterPNG then TBGRAWriterPNG(writer).Indexed := true else
      if writer is TFPWriterBMP then
      begin
        if not hasTransp then
        begin
          depth := BGRARequiredBitDepth(ReducedPalette);
          if depth < 8 then
          begin
            if depth > 4 then
              depth := 8
            else if depth > 1 then
              depth := 4;
          end;
          TFPWriterBMP(writer).BitsPerPixel := depth;
        end;
      end;
      dithered.SaveToStream(AStream, writer);
    finally
      writer.Free;
    end;
  finally
    dithered.Free;
  end;
end;

{ TBGRAColorTree }

function TBGRAColorTree.TrySplitLeaf: boolean;
var
  dim: TColorDimension;
  box2: TBGRAColorBox;
  mid: UInt32;
begin
  result := false;
  if IsLeaf and Assigned(FLeaf) and not FLeaf.PointLike then
  begin
    dim := FLeaf.LargestApparentDimension;
    box2 := FLeaf.MedianCut(dim,mid);
    if box2 <> nil then
    begin
      FInferiorBranch := TBGRAColorTree.Create(FLeaf,True);
      FSuperiorBranch := TBGRAColorTree.Create(box2,True);

      FInferiorBranch.FMinBorder := FMinBorder;
      FInferiorBranch.FMaxBorder := FMaxBorder;
      FSuperiorBranch.FMinBorder := FMinBorder;
      FSuperiorBranch.FMaxBorder := FMaxBorder;
      FInferiorBranch.FMaxBorder[dim] := false;
      FSuperiorBranch.FMinBorder[dim] := false;

      FLeaf := nil;
      FIsLeaf:= false;
      FDimension := dim;
      FPixelValueComparer := GetPixelValueComparer(FDimension);
      FSuperiorMiddle := mid;
      result := true;
    end;
  end;
end;

procedure TBGRAColorTree.ComputeLeavesColor(ALeafColor: TBGRALeafColorMode);
var index: integer;
begin
  index := 0;
  if HasPureTransparentColor then
  begin
    FPureTransparentColorIndex:= index;
    inc(index);
  end;
  InternalComputeLeavesColor(ALeafColor,{%H-}index);
end;

procedure TBGRAColorTree.InternalComputeLeavesColor(
  ALeafColor: TBGRALeafColorMode; var AStartIndex: integer);
var nbMin,nbMax: NativeInt;
  c: TColorDimension;
  extremumColor: TBGRAPixel;
  extremumColorRelevant: Boolean;
begin
  if IsLeaf then
  begin
    FLeafColorIndex := AStartIndex;
    inc(AStartIndex);
    if Assigned(FLeaf) then
    begin
      if not FLeafColorComputed then
      begin
        FLeafColorComputed := true;
        FCenterColor.alpha:= FLeaf.FBounds[cdAlpha].GetCenter shr AlphaShift;
        FCenterColor.red:= GammaCompressionTab[FLeaf.FBounds[cdRed].GetCenter shr RedShift];
        FCenterColor.green:= GammaCompressionTab[FLeaf.FBounds[cdGreen].GetCenter shr GreenShift];
        FCenterColor.blue:= GammaCompressionTab[FLeaf.FBounds[cdBlue].GetCenter];
        FAverageColor := FLeaf.AverageColorOrMainColor;
        extremumColor := FAverageColor;

        if ALeafColor in [lcMix,lcExtremum] then
        begin
          nbMax := 0;
          nbMin := 0;
          for c := succ(low(TColorDimension)) to high(TColorDimension) do
          begin
            if FMinBorder[c] then inc(nbMin);
            if FMaxBorder[c] then inc(nbMax);
          end;

          if nbMin > nbMax then
            extremumColor := FLeaf.InferiorColor
          else if nbMax > nbMin then
            extremumColor := FLeaf.SuperiorColor;
        end;

        case ALeafColor of
        lcAverage,lcMix: FLeafColor := FAverageColor;
        lcExtremum: FLeafColor := extremumColor;
        else FLeafColor := FCenterColor;
        end;

        if ALeafColor = lcMix then
        begin
          extremumColorRelevant := false;
          for c := succ(low(TColorDimension)) to high(TColorDimension) do
            if UInt32(abs(GetDimensionValue(extremumColor,c) - GetDimensionValue(FLeafColor,c))) >
               FLeaf.FBounds[c].Size div 7 then
            begin
              extremumColorRelevant := true;
              break;
            end;
          if extremumColorRelevant then FLeafColor := extremumColor;
        end;
      end;
    end else
    begin
      FLeafColor := BGRAPixelTransparent;
      FCenterColor := BGRAPixelTransparent;
    end;
  end else
  begin
    if Assigned(FInferiorBranch) then FInferiorBranch.InternalComputeLeavesColor(ALeafColor, AStartIndex);
    if Assigned(FSuperiorBranch) then FSuperiorBranch.InternalComputeLeavesColor(ALeafColor, AStartIndex);
  end;
end;

procedure TBGRAColorTree.CheckColorComputed;
begin
  if not FLeafColorComputed then
    raise exception.Create('Color not computed. Call ComputeLeavesColor first.');
end;

function TBGRAColorTree.ApproximateColor(AColor: TBGRAPixel): TBGRAPixel;
var branch: TBGRAColorTree;
begin
  if AColor.alpha = 0 then
  begin
    result := BGRAPixelTransparent;
    exit;
  end;
  if IsLeaf then
  begin
    CheckColorComputed;
    result := FLeafColor;
  end else
  begin
    if FPixelValueComparer(@AColor,FSuperiorMiddle) then
      branch := FSuperiorBranch else branch := FInferiorBranch;
    if Assigned(branch) then
      result := branch.ApproximateColor(AColor)
    else
      result := BGRAPixelTransparent;
  end;
end;

function TBGRAColorTree.ApproximateColorIndex(AColor: TBGRAPixel): integer;
var branch: TBGRAColorTree;
begin
  if AColor.alpha = 0 then
  begin
    result := FPureTransparentColorIndex;
    exit;
  end;
  if IsLeaf then
  begin
    CheckColorComputed;
    result := FLeafColorIndex;
  end else
  begin
    if FPixelValueComparer(@AColor,FSuperiorMiddle) then
      branch := FSuperiorBranch else branch := FInferiorBranch;
    if Assigned(branch) then
      result := branch.ApproximateColorIndex(AColor)
    else
      result := FPureTransparentColorIndex;
  end;
end;

function TBGRAColorTree.GetAsArrayOfApproximatedColors: ArrayOfTBGRAPixel;
var a,b: ArrayOfTBGRAPixel;
  idx,i: integer;
begin
  if IsLeaf then
  begin
    CheckColorComputed;
    setlength(result,1+byte(HasPureTransparentColor));
    idx := 0;
    if HasPureTransparentColor then
    begin
      result[idx] := BGRAPixelTransparent;
      inc(idx);
    end;
    result[idx] := FLeafColor;
  end else
  begin
    a := FInferiorBranch.GetAsArrayOfApproximatedColors;
    b := FSuperiorBranch.GetAsArrayOfApproximatedColors;
    setlength(result, length(a)+length(b)+byte(HasPureTransparentColor));
    idx := 0;
    if HasPureTransparentColor then
    begin
      result[idx] := BGRAPixelTransparent;
      inc(idx);
    end;
    for i := 0 to high(a) do
    begin
      result[idx] := a[i];
      inc(idx);
    end;
    for i := 0 to high(b) do
    begin
      result[idx] := b[i];
      inc(idx);
    end;
  end;
end;

function TBGRAColorTree.GetAsArrayOfWeightedColors: ArrayOfWeightedColor;
var a,b: ArrayOfWeightedColor;
  idx,i: integer;
begin
  if IsLeaf then
  begin
    CheckColorComputed;
    setlength(result,1+byte(HasPureTransparentColor));
    idx := 0;
    if HasPureTransparentColor then
    begin
      result[idx].Color := BGRAPixelTransparent;
      result[idx].Weight := PureTransparentColorCount;
      inc(idx);
    end;
    result[idx].Color := FLeafColor;
    result[idx].Weight := Weight;
  end else
  begin
    a := FInferiorBranch.GetAsArrayOfWeightedColors;
    b := FSuperiorBranch.GetAsArrayOfWeightedColors;
    setlength(result, length(a)+length(b)+byte(HasPureTransparentColor));
    idx := 0;
    if HasPureTransparentColor then
    begin
      result[idx].Color := BGRAPixelTransparent;
      result[idx].Weight := PureTransparentColorCount;
      inc(idx);
    end;
    for i := 0 to high(a) do
    begin
      result[idx] := a[i];
      inc(idx);
    end;
    for i := 0 to high(b) do
    begin
      result[idx] := b[i];
      inc(idx);
    end;
  end;
end;

procedure TBGRAColorTree.SplitIntoPalette(ACount: integer;
  AMethod: TBiggestLeafMethod; ALeafColor: TBGRALeafColorMode);
var nbColors: integer;
  leaf: TBGRAColorTree;
begin
  nbColors := ApproximatedColorCount;
  while nbColors < ACount do
  begin
    leaf := FindBiggestLeaf(AMethod);
    if not leaf.TrySplitLeaf then break;
    inc(nbColors);
  end;
  ComputeLeavesColor(ALeafColor);
  FreeLeaves;
end;

function TBGRAColorTree.SplitIntoPaletteWithSubPalette(ACount: integer;
  AMethod: TBiggestLeafMethod; ALeafColor: TBGRALeafColorMode;
  ASubPaletteCount: integer): ArrayOfTBGRAPixel;
var nbColors: integer;
  leaf: TBGRAColorTree;
begin
  result := nil;
  nbColors := ApproximatedColorCount;
  if ASubPaletteCount > ACount then ASubPaletteCount:= ACount;
  if nbColors = ASubPaletteCount then
  begin
    ComputeLeavesColor(ALeafColor);
    result := GetAsArrayOfApproximatedColors;
  end;
  while nbColors < ACount do
  begin
    leaf := FindBiggestLeaf(AMethod);
    if not leaf.TrySplitLeaf then break;
    inc(nbColors);
    if nbColors = ASubPaletteCount then
    begin
      ComputeLeavesColor(ALeafColor);
      result := GetAsArrayOfApproximatedColors;
    end;
  end;
  ComputeLeavesColor(ALeafColor);
  FreeLeaves;
end;

function TBGRAColorTree.GetLeafCount: integer;
begin
  if IsLeaf then
    result := 1
  else
  begin
    result := 0;
    if Assigned(FInferiorBranch) then result += FInferiorBranch.LeafCount;
    if Assigned(FSuperiorBranch) then result += FSuperiorBranch.LeafCount;
  end;
end;

function TBGRAColorTree.GetApproximatedColorCount: integer;
begin
  if IsLeaf then
    result := 1
  else
  begin
    result := 0;
    if Assigned(FInferiorBranch) then result += FInferiorBranch.ApproximatedColorCount;
    if Assigned(FSuperiorBranch) then result += FSuperiorBranch.ApproximatedColorCount;
  end;
  if HasPureTransparentColor then inc(result);
end;

function TBGRAColorTree.GetHasPureTransparentColor: boolean;
begin
  result := FPureTransparentColorCount > 0;
end;

procedure TBGRAColorTree.Init(ALeaf: TBGRAColorBox; AOwned: boolean);
var
  c: TColorDimension;
begin
  if not AOwned then
    FLeaf := ALeaf.Duplicate
  else
    FLeaf := ALeaf;
  FLargestApparentInterval:= FLeaf.LargestApparentInterval;
  FWeight := FLeaf.TotalWeight;
  FIsLeaf:= true;
  for c := low(TColorDimension) to high(TColorDimension) do
  begin
    FMinBorder[c] := true;
    FMaxBorder[c] := true;
  end;
  FPureTransparentColorCount:= FLeaf.PureTransparentColorCount;
  FPureTransparentColorIndex:= -1;
end;

constructor TBGRAColorTree.Create(ABox: TBGRAColorBox; AOwned: boolean);
begin
  Init(ABox,AOwned);
end;

constructor TBGRAColorTree.Create(ADimensions: TColorDimensions; APalette: TBGRACustomPalette);
begin
  Init(TBGRAColorBox.Create(ADimensions, APalette),True);
end;

constructor TBGRAColorTree.Create(ADimensions: TColorDimensions; ABitmap: TBGRACustomBitmap; AAlpha: TAlphaChannelPaletteOption);
begin
  Init(TBGRAColorBox.Create(ADimensions, ABitmap, AAlpha),True);
end;

destructor TBGRAColorTree.Destroy;
begin
  FreeAndNil(FInferiorBranch);
  FreeAndNil(FSuperiorBranch);
  FreeAndNil(FLeaf);
  inherited Destroy;
end;

procedure TBGRAColorTree.FreeLeaves;
begin
  if IsLeaf then
    FreeAndNil(FLeaf)
  else
  begin
    if Assigned(FInferiorBranch) then FInferiorBranch.FreeLeaves;
    if Assigned(FSuperiorBranch) then FSuperiorBranch.FreeLeaves;
  end;
end;

function TBGRAColorTree.FindBiggestLeaf(AMethod: TBiggestLeafMethod
  ): TBGRAColorTree;
var infLeaf,supLeaf: TBGRAColorTree;
begin
  if IsLeaf then
    result := self
  else
  begin
    infLeaf := FInferiorBranch.FindBiggestLeaf(AMethod);
    supLeaf := FSuperiorBranch.FindBiggestLeaf(AMethod);
    case AMethod of
    blApparentInterval:
      if infLeaf.LargestApparentInterval >= supLeaf.LargestApparentInterval then
        result := infLeaf
      else
        result := supLeaf;
    blWeight:
      if (infLeaf.LargestApparentInterval > 0) and (infLeaf.Weight >= supLeaf.Weight) then
        result := infLeaf
      else
        result := supLeaf;
    else{blMix:}
      if (sqrt(infLeaf.Weight/FWeight)*(infLeaf.LargestApparentInterval/LargestApparentInterval) >=
          sqrt(supLeaf.Weight/FWeight)*(supLeaf.LargestApparentInterval/LargestApparentInterval) ) then
        result := infLeaf
      else
        result := supLeaf;
    end;
  end;
end;

{ TDimensionMinMax }

function TDimensionMinMax.Size: UInt32;
begin
  if Maximum>Minimum then
    result := Maximum-Minimum
  else
    result := 0;
end;

function TDimensionMinMax.Contains(AValue: UInt32): boolean;
begin
  result := (AValue >= Minimum) and (AValue <= Maximum);
end;

function TDimensionMinMax.PointLike: boolean;
begin
  result := (Minimum = Maximum);
end;

procedure TDimensionMinMax.SetAsPoint(AValue: UInt32);
begin
  Minimum := AValue;
  Maximum := AValue;
end;

function TDimensionMinMax.GetCenter: UInt32;
begin
  result := (Minimum+Maximum) shr 1;
end;

procedure TDimensionMinMax.GrowToInclude(AValue: UInt32);
begin
  if AValue < Minimum then Minimum := AValue
  else if AValue > Maximum then Maximum := AValue;
end;

{ TBGRAColorBox }

function TBGRAColorBox.GetApparentInterval(ADimension: TColorDimension): UInt32;
var factor: single;
begin
  if not (ADimension in FDimensions) then result := 0
  else
  begin
    factor := 1;
    case ADimension of
    cdRGB: factor := 0.7;
    end;
    result := round(FBounds[ADimension].Size*factor);
  end;
end;

function TBGRAColorBox.GetAverageColor: TBGRAPixel;
var
  n:     integer;
  r, g, b, a: double;
  cura: double;
  w: UInt32;
begin
  a := 0;
  r := 0;
  g := 0;
  b := 0;
  w := 0;
  for n := 0 to high(FColors) do
  with FColors[n].Color do
  begin
    cura := (alpha / 255)*FColors[n].Weight;
    a     += cura;
    r     += GammaExpansionTab[red] * cura;
    g     += GammaExpansionTab[green] * cura;
    b     += GammaExpansionTab[blue] * cura;
    w     += FColors[n].Weight;
  end;
  if w = 0 then
    Result := BGRAPixelTransparent
  else
  begin
    result.alpha := round(a*255/w);
    if result.alpha = 0 then result := BGRAPixelTransparent
    else
    begin
      result.red := GammaCompressionTab[round(r / a)];
      result.green := GammaCompressionTab[round(g / a)];
      result.blue := GammaCompressionTab[round(b / a)];
    end;
  end;
end;

function TBGRAColorBox.GetAverageColorOrMainColor: TBGRAPixel;
var i: integer;
  maxWeight: UInt32;
begin
  result := BGRAPixelTransparent;
  maxWeight:= 0;
  for i := 0 to high(FColors) do
    with FColors[i] do
    begin
      if Weight > maxWeight then
      begin
        maxWeight:= Weight;
        result := Color;
      end;
    end;
  if maxWeight <= 3*FTotalWeight shr 2 then
    result := GetAverageColor;
end;

function TBGRAColorBox.GetBounds(ADimension: TColorDimension): TDimensionMinMax;
begin
  result := FBounds[ADimension];
end;

function TBGRAColorBox.GetColorCount(ACountPureTransparent: boolean): integer;
begin
  result := length(FColors);
  if ACountPureTransparent and HasPureTransparentColor then inc(result);
end;

function TBGRAColorBox.GetHasPureTransparentColor: boolean;
begin
  result := FPureTransparentColorCount > 0;
end;

function TBGRAColorBox.GetInferiorColor: TBGRAPixel;
var
  n:     integer;
  r, g, b, a: double;
  w: UInt32;
  cura: double;
  wantedWeight: UInt32;
begin
  a := 0;
  r := 0;
  g := 0;
  b := 0;
  w := 0;
  wantedWeight:= FTotalWeight div 10;
  for n := 0 to high(FColors) do
  with FColors[n].Color do
  begin
    cura := (alpha / 255)*FColors[n].Weight;
    a     += cura;
    r     += red * cura;
    g     += green * cura;
    b     += blue * cura;
    w     += FColors[n].Weight;
    if w >= wantedWeight then break;
  end;
  if w = 0 then
    Result := BGRAPixelTransparent
  else
  begin
    result.alpha := round(a*255/w);
    if result.alpha = 0 then result := BGRAPixelTransparent
    else
    begin
      result.red := round(r / a);
      result.green := round(g / a);
      result.blue := round(b / a);
    end;
  end;
end;

function TBGRAColorBox.GetLargestApparentDimension: TColorDimension;
var c: TColorDimension;
  curApparentInterval, maxApparentInterval: UInt32;
begin
  c := succ(low(TColorDimension));
  result := c;
  maxApparentInterval:= ApparentInterval[c];
  while c < high(TColorDimension) do
  begin
    inc(c);
    curApparentInterval:= ApparentInterval[c];
    if curApparentInterval > maxApparentInterval then
    begin
      maxApparentInterval:= curApparentInterval;
      result := c;
    end;
  end;
end;

function TBGRAColorBox.GetLargestApparentInterval: UInt32;
var
  curApparentInterval: UInt32;
  c: TColorDimension;
begin
  result:= ApparentInterval[succ(low(TColorDimension))];
  for c := succ(succ(low(TColorDimension))) to high(TColorDimension) do
  begin
    curApparentInterval:= ApparentInterval[c];
    if curApparentInterval > result then
      result := curApparentInterval;
  end;
end;

function TBGRAColorBox.GetPointLike: boolean;
var c: TColorDimension;
begin
  for c := succ(low(TColorDimension)) to high(TColorDimension) do
    if not FBounds[c].PointLike then
    begin
      result := false;
      exit;
    end;
  result := true;
end;

function TBGRAColorBox.GetSuperiorColor: TBGRAPixel;
var
  n:     integer;
  r, g, b, a: double;
  w: UInt32;
  cura: double;
  wantedWeight: UInt32;
begin
  a := 0;
  r := 0;
  g := 0;
  b := 0;
  w := 0;
  wantedWeight:= FTotalWeight div 10;
  for n := high(FColors) downto 0 do
  with FColors[n].Color do
  begin
    cura := (alpha / 255)*FColors[n].Weight;
    a     += cura;
    r     += red * cura;
    g     += green * cura;
    b     += blue * cura;
    w     += FColors[n].Weight;
    if w >= wantedWeight then break;
  end;
  if w = 0 then
    Result := BGRAPixelTransparent
  else
  begin
    result.alpha := round(a*255/w);
    if result.alpha = 0 then result := BGRAPixelTransparent
    else
    begin
      result.red := round(r / a);
      result.green := round(g / a);
      result.blue := round(b / a);
    end;
  end;
end;

procedure TBGRAColorBox.Init(AColors: ArrayOfWeightedColor; AOwner: boolean);
var
  i,idx: NativeInt;
  FirstColor: boolean;
  c: TColorDimension;
begin
  FPureTransparentColorCount:= 0;
  FTotalWeight:= 0;
  for c := low(TColorDimension) to high(TColorDimension) do
    FBounds[c].SetAsPoint(0);
  FirstColor := True;
  if AOwner then
    FColors := AColors
  else
    SetLength(FColors, length(AColors));
  idx := 0;
  for i := 0 to high(AColors) do
  with AColors[i] do
  begin
    if Color.alpha > 0 then
    begin
      if FirstColor then
      begin
        for c := low(TColorDimension) to high(TColorDimension) do
          FBounds[c].SetAsPoint(GetDimensionValue(Color,c));
        FirstColor := false;
      end else
      begin
        for c := low(TColorDimension) to high(TColorDimension) do
          FBounds[c].GrowToInclude(GetDimensionValue(Color,c));
      end;
      inc(FTotalWeight, Weight);
      if not AOwner or (idx <> i) then
        FColors[idx] := AColors[i];
      inc(idx);
    end else
      inc(FPureTransparentColorCount, Weight);
  end;
  setlength(FColors,idx);
end;

procedure TBGRAColorBox.SortBy(ADimension: TColorDimension);
var comparer: TIsChannelStrictlyGreaterFunc;
begin
  comparer := GetPixelStrictComparer(ADimension);
  if comparer = nil then exit;
  ArrayOfWeightedColor_QuickSort(FColors,0,high(FColors),comparer)
end;

function TBGRAColorBox.GetMedianIndex(ADimension: TColorDimension;
    AMinValue, AMaxValue: UInt32
  ): integer;
var i: integer;
  sum,goal: UInt32;
  valueComparer: TIsChannelGreaterThanOrEqualToValueFunc;
  strictComparer: TIsChannelStrictlyGreaterFunc;
  ofs: integer;
begin
  if length(FColors) = 1 then
  begin
    result := 0;
    exit;
  end else
  if length(FColors) = 0 then
  begin
    result := -1;
    exit;
  end;
  valueComparer:= GetPixelValueComparer(ADimension);
  sum := 0;
  goal := (FTotalWeight+1) shr 1;
  result := high(FColors) shr 1;
  for i := 0 to high(FColors) do
  begin
    inc(sum, FColors[i].Weight);
    if (sum>=goal) and (valueComparer(@FColors[i].Color, AMinValue)) then
    begin
      result := i;
      while (result > 0) and (valueComparer(@FColors[result].Color, AMaxValue+1)) do dec(result);
      break;
    end;
  end;
  if result = 0 then inc(result);
  //check that there it is not splitting consecutive colors with the same value
  strictComparer := GetPixelStrictComparer(ADimension);
  ofs := 0;
  while true do
  begin
    if (result-ofs < 1) and (result+ofs > high(FColors)) then break;
    if (result-ofs >= 1) and strictComparer(@FColors[result-ofs].Color,@FColors[result-ofs-1].Color) then
    begin
      result := result-ofs;
      exit;
    end;
    if (result+ofs <= high(FColors)) and strictComparer(@FColors[result+ofs].Color,@FColors[result+ofs-1].Color) then
    begin
      result := result+ofs;
      exit;
    end;
    inc(ofs);
  end;
end;

constructor TBGRAColorBox.Create(ADimensions: TColorDimensions; AColors: ArrayOfWeightedColor; AOwner: boolean);
begin
  FDimensions:= ADimensions;
  Init(AColors,AOwner);
end;

constructor TBGRAColorBox.Create(ADimensions: TColorDimensions;
  const AColors: ArrayOfTBGRAPixel; AAlpha: TAlphaChannelPaletteOption = acFullChannelInPalette);
var weightedColors: ArrayOfWeightedColor;
  i: Integer;
begin
  if AAlpha = acFullChannelInPalette then
  begin
    FDimensions:= ADimensions;
    setlength(weightedColors, length(AColors));
    for i := 0 to high(weightedColors) do
    with weightedColors[i] do
    begin
      color := AColors[i];
      Weight:= 1;
    end;
    Init(weightedColors,True);
  end else
    Create(ADimensions, @AColors[0], length(AColors), AAlpha);
end;

constructor TBGRAColorBox.Create(ADimensions: TColorDimensions; ABounds: TColorBoxBounds);
begin
  FDimensions:= ADimensions;
  FBounds := ABounds;
  FTotalWeight:= 0;
  FPureTransparentColorCount:= 0;
end;

constructor TBGRAColorBox.Create(ADimensions: TColorDimensions; APalette: TBGRACustomPalette);
begin
  FDimensions:= ADimensions;
  Init(APalette.GetAsArrayOfWeightedColor,False);
end;

constructor TBGRAColorBox.Create(ADimensions: TColorDimensions;
  ABitmap: TBGRACustomBitmap; AAlpha: TAlphaChannelPaletteOption);
begin
  Create(ADimensions, ABitmap.Data, ABitmap.NbPixels, AAlpha);
end;

constructor TBGRAColorBox.Create(ADimensions: TColorDimensions; AColors: PBGRAPixel; ANbPixels: integer; AAlpha: TAlphaChannelPaletteOption);
var i,j,prev,idx: integer;
  p: PBGRAPixel;
  skip: boolean;
  alphaMask: DWord;
  transpIndex: integer;
begin
  if AAlpha <> acFullChannelInPalette then
    alphaMask := LEtoN($FF000000)
  else
    alphaMask := 0;
  FDimensions:= ADimensions;
  transpIndex := -1;
  SetLength(FColors,ANbPixels);
  if length(FColors)>0 then
  begin
    p := AColors;
    idx := 0;
    for i := 0 to ANbPixels-1 do
    begin
      if (p^.alpha = 0) or ((AAlpha = acTransparentEntry) and (p^.alpha < 128)) then
      begin
        skip := true;
        if not (AAlpha = acIgnore) then
        begin
          if (transpIndex=-1) then
          begin
            transpIndex := idx;
            with FColors[idx] do
            begin
              Color := BGRAPixelTransparent;
              Weight:= 1;
            end;
            inc(idx);
          end else
            inc(FColors[transpIndex].Weight);
        end;
        if (p^.alpha = 0) then
        begin
          inc(p);
          continue;
        end;
      end;
      skip := false;
      for j := idx-1 downto idx-10 do
        if j < 0 then
          break
        else
        with FColors[j] do
        if DWord(Color)=DWord(p^) or alphaMask then
        begin
          skip := true;
          inc(Weight);
          break;
        end;
      if skip then
      begin
        inc(p);
        continue;
      end;
      with FColors[idx] do
      begin
        Color := p^;
        if AAlpha <> acFullChannelInPalette then Color.alpha := 255;
        Weight := 1;
        inc(p);
        inc(idx);
      end;
    end;
    setLength(FColors, idx);

    ArrayOfWeightedColor_QuickSort(FColors,0,high(FColors),@IsDWordGreater);
    prev := 0;
    for i := 1 to high(FColors) do
    begin
      if DWord(FColors[i].Color)=DWord(FColors[prev].Color) then
        inc(FColors[prev].Weight, FColors[i].Weight)
      else
      begin
        inc(prev);
        if i <> prev then
          FColors[prev] := FColors[i];
      end;
    end;
    setlength(FColors, prev+1);
  end;
  Init(FColors,True);
end;

function TBGRAColorBox.BoundsContain(AColor: TBGRAPixel): boolean;
var c: TColorDimension;
begin
  for c := succ(low(TColorDimension)) to high(TColorDimension) do
    if not FBounds[c].Contains(GetDimensionValue(AColor,c)) then
    begin
      result := false;
      exit;
    end;
  result := true;
end;

function TBGRAColorBox.MedianCut(ADimension: TColorDimension; out SuperiorMiddle: UInt32
  ): TBGRAColorBox;
var idxSplit: NativeInt;
  secondArray: ArrayOfWeightedColor;
  i: NativeInt;
begin
  result := nil;
  SuperiorMiddle := 0;
  if FBounds[ADimension].PointLike then exit;
  if length(FColors) <= 1 then exit;
  SortBy(ADimension);
  idxSplit := GetMedianIndex(ADimension,
    round(FBounds[ADimension].Minimum*(1-MedianMinPercentage)+FBounds[ADimension].Maximum*MedianMinPercentage),
    round(FBounds[ADimension].Minimum*MedianMinPercentage+FBounds[ADimension].Maximum*(1-MedianMinPercentage)));
  if idxSplit = -1 then exit;
  setlength(secondArray, length(FColors)-idxSplit);
  for i := idxSplit to high(FColors) do
    secondArray[i-idxSplit] := FColors[i];
  result := TBGRAColorBox.Create(FDimensions, secondArray,True);
  setlength(FColors, idxSplit);
  Init(FColors,True);
  SuperiorMiddle := (FBounds[ADimension].Maximum + result.FBounds[ADimension].Minimum + 1) shr 1;
end;

function TBGRAColorBox.Duplicate: TBGRAColorBox;
var
  i: NativeInt;
begin
  result := TBGRAColorBox.Create(FDimensions, FBounds);
  result.FTotalWeight := FTotalWeight;
  setlength(result.FColors, length(FColors));
  for i := 0 to high(FColors) do
    result.FColors[i] := FColors[i];
end;

function TBGRAColorBox.GetAsArrayOfColors(AIncludePureTransparent: boolean): ArrayOfTBGRAPixel;
var i,idx: integer;
begin
  if AIncludePureTransparent and HasPureTransparentColor then
  begin
    setlength(result, length(FColors)+1);
    result[0] := BGRAPixelTransparent;
    idx := 1;
  end else
  begin
    setlength(result, length(FColors));
    idx := 0;
  end;
  for i:= 0 to high(FColors) do
  begin
    result[idx] := FColors[i].Color;
    inc(idx);
  end;
end;

end.

