// SPDX-License-Identifier: GPL-3.0-only
unit UTiff;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

const
  TiffTagNewSubFileType = 254;
  TiffTagSubfileType = 255;
  TiffTagImageWidth = 256;
  TiffTagImageLength = 257;
  TiffTagBitsPerSample = 258;
  TiffTagCompression = 259;
  TiffTagPhotometricInterpretation = 262;
  TiffTagThresholding = 263;
  TiffTagCellWidth = 264;
  TiffTagCellLength = 265;
  TiffTagFillOrder = 266;
  TiffTagDocumentName = 269;
  TiffTagImageDescription = 270;
  TiffTagMake = 271;
  TiffTagModel = 272;
  TiffTagStripOffsets = 273;
  TiffTagOrientation = 274;
  TiffTagSamplesPerPixel = 277;
  TiffTagRowsPerStrip = 278;
  TiffTagStripByteCounts = 279;
  TiffTagMinSampleValue = 280;
  TiffTagMaxSampleValue = 281;
  TiffTagXResolution = 282;
  TiffTagYResolution = 283;
  TiffTagPlanarConfiguration = 284;
  TiffTagPageName = 285;
  TiffTagXPosition = 286;
  TiffTagYPosition = 287;
  TiffTagFreeOffsets = 288;
  TiffTagFreeByteCounts = 289;
  TiffTagGrayResponseUnit = 290;
  TiffTagGrayResponseCurve = 291;
  TiffTagT4Options = 292;
  TiffTagT6Options = 293;
  TiffTagResolutionUnit = 296;
  TiffTagPageNumber = 297;
  TiffTagTransferFunction = 301;
  TiffTagSoftware = 305;
  TiffTagDateTime = 306;
  TiffTagArtist = 315;
  TiffTagHostComputer = 316;
  TiffTagPredictor = 317;
  TiffTagWhitePoint = 318;
  TiffTagPrimaryChromacities = 319;
  TiffTagColorMap = 320;
  TiffTagHalftoneHints = 321;
  TiffTagTileWidth = 322;
  TiffTagTileLength = 323;
  TiffTagTileOffsets = 324;
  TiffTagTileByteCounts = 325;
  TiffTagBadFaxLines = 326;
  TiffTagCleanFaxData = 327;
  TiffTagConsecutiveBadFaxLines = 328;
  TiffTagInkSet = 332;
  TiffTakInkNames = 333;
  TiffTagNumberOfInks = 334;
  TiffTagDotRange = 336;
  TiffTagTargetPrinter = 337;
  TiffTagExtraSamples = 338;
  TiffTagSampleFormat = 339;
  TiffTagSMinSampleValue = 340;
  TiffTagSMaxSampleValue = 341;
  TiffTagTransferRange = 342;
  TiffTagJPEGTables = 347;
  TiffTagJPEGProc = 512;
  TiffTagJPEGInterchangeFormat = 513;
  TiffTagJPEGInterchangeFormatLength = 514;
  TiffTagJPEGRestartInterval = 515;
  TiffTagJPEGLosslessPerdictors = 517;
  TiffTagJPEGPointTransforms = 518;
  TiffTagJPEGQTables = 519;
  TiffTagJPEGDCTables = 520;
  TiffTagJPEGACTables = 521;
  TiffTagYCbCrCoefficients = 529;
  TiffTafYCbCrSubSampling = 530;
  TiffTagYCbCrPositioning = 531;
  TiffTagReferenceBlackWhite = 532;
  TiffTagXMLPacket = 700;
  TiffTagCopyright = 33432;
  TiffTagRichTiffIPTC = 33723;
  TiffTagPhotoshopImageResourceBlocks = 34377;
  TiffTagExifIFD = 34665;
  TiffTagICCProfile = 34675;

  TiffTagHylaFaxReceiveParams = 34908;
  TiffTagHylaFaxReceiveTimeSecs = 34910;

  ExifTagColorspace = 40961;
  ExifTagPixelXDimension = 40962;
  ExifTagPixelYDimension = 40963;

type
  TTiffError = (teNone,
                teUnexpectedEndOfStream,
                teInvalidHeader,
                teInvalidStreamOffset,
                teCircularOffset,
                teUnhandledException,
                teUnknownValueType,
                teDuplicateTag);

  { TTiffIO }

  TTiffIO = object
  private
    FStream: TStream;
    FStartPos: int64;
    FLittleEndian: boolean;
    function GetPosition: int64;
    function GetSize: int64;
    procedure SetPosition(AValue: int64);
  public
    procedure Init(AStream: TStream; AStartPos: int64);
    function CopyTo(AStream: TStream; ACount: LongWord): TTiffError;
    procedure CopyFrom(AStream: TStream; ACount: LongWord);
    function ReadBuffer(var ABuffer; ACount: integer): TTiffError;
    procedure WriteBuffer(var ABuffer; ACount: integer);
    function ReadByte(out AValue: byte): TTiffError;
    function ReadWord(out AValue: Word): TTiffError;
    function ReadLong(out AValue: LongWord): TTiffError;
    procedure WriteByte(AValue: byte);
    procedure WriteWord(AValue: Word);
    procedure WriteLong(AValue: LongWord);
    function FixEndian(AValue: Word): Word;
    function FixEndian(AValue: LongWord): LongWord;
    function FixEndian(AValue: QWord): QWord;
    property LittleEndian: boolean read FLittleEndian write FLittleEndian;
    property Position: int64 read GetPosition write SetPosition;
    property Size: int64 read GetSize;
  end;

  TTiffValueType = (tvtUnknown, tvtByte, tvtAscii, tvtWord, tvtLong, tvtRational,
    tvtSignedByte, tvtRawByte, tvtSignedWord, tvtSignedLongWord, tvtSignedRational,
    tvtSingle, tvtDouble);

const
  TiffValueSize : array[TTiffValueType] of Byte =
   (0, 1, 1, 2, 4, 8,
    1, 1, 2, 4, 8, 4, 8);
  TiffValueTypeStr : array[TTiffValueType] of string =
   ('Unknown','Byte','Ascii','Word','Long','Rational',
    'SignedByte','RawByte','SignedWord','SignedLong','SignedRational',
    'Single','Double');

type
  TTiffRawDirEntry = packed record
    Tag: Word;
    ValueType: Word;
    ValueCount: LongWord;
    case boolean of
      false: (ShortData: array[1..4] of Byte);
      true: (ValueOffset: LongWord);
  end;

  { TTiffRational }

  TTiffRational = object
    Numerator, Denominator: LongWord;
    Negative: boolean;
    function AsString: string;
    function AsDouble: double;
  end;

function TiffRational(ANumerator,ADenominator: LongWord): TTiffRational;
function TiffRational(ANumerator,ADenominator: Integer): TTiffRational;

type
  ArrayOfLongWord = array of LongWord;
  ArrayOfWord = array of Word;

  { TTiffDirEntry }
  PTiffDirEntry = ^TTiffDirEntry;
  TTiffDirEntry = object
  private
    FTag: Word;
    FValueType: TTiffValueType;
    FValueCount: LongWord;
    FShortData: array[1..4] of byte;
    FLongData: pointer;
    procedure FixEndianData(AData: Pointer; AIO: TTiffIO);
    function GetData: Pointer;
    function GetDoubleValue(AIndex: LongWord): Double;
    function GetRationalValue(AIndex: LongWord): TTiffRational;
    function GetName: string;
    function GetSignedValue(AIndex: LongWord): Integer;
    function GetStringValue: string;
    function GetUnsignedValue(AIndex: LongWord): LongWord;
  public
    procedure Free;
    procedure Realloc(AValueType: TTiffValueType; AValueCount: LongWord);
    procedure InitNew(ATag: Word);
    function LoadFromInput(AInput: TTiffIO; const ARaw: TTiffRawDirEntry): TTiffError;
    procedure SaveToOutput(AOutput: TTiffIO; out ARaw: TTiffRawDirEntry);
    procedure SetLong(AValue: LongWord);
    procedure SetWord(AValue: Word);
    procedure SetByte(AValue: Byte);
    procedure SetLongArray(AValues: ArrayOfLongWord);
    procedure SetWordArray(AValues: ArrayOfWord);
    property Tag: Word read FTag;
    property ValueType: TTiffValueType read FValueType;
    property ValueCount: LongWord read FValueCount;
    property Data: Pointer read GetData;
    property Name: string read GetName;
    property StringValue: string read GetStringValue;
    property SignedValue[AIndex: LongWord]: Integer read GetSignedValue;
    property UnsignedValue[AIndex: LongWord]: LongWord read GetUnsignedValue;
    property RationalValue[AIndex: LongWord]: TTiffRational read GetRationalValue;
    property DoubleValue[AIndex: LongWord]: Double read GetDoubleValue;
  end;

  TStreamList = specialize TFPGObjectList<TStream>;

  { TTiffDirectory }

  TTiffDirectory = class
  private
    FDirEntries: packed array of TTiffDirEntry;
    FDirEntryCount: integer;
    function GetEntry(AIndex: integer): PTiffDirEntry;
    function LoadChunks(AInput: TTiffIO): TTiffError; virtual; abstract;
    procedure SaveChunks(AOutput: TTiffIO); virtual; abstract;
    function LoadChunkList(AInput: TTiffIO; ATagOffsets, ATagByteCounts: Word; var AList: TStreamList): TTiffError;
    procedure SaveChunkList(AOutput: TTiffIO; ATagOffsets, ATagByteCounts: Word; AList: TStreamList);
    procedure ClearChunkList(var AList: TStreamList);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure SortEntries;
    function AddEntry(const AEntry: TTiffDirEntry): TTiffError;
    function LoadFromInput(AInput: TTiffIO; ADirectoryPos: LongWord; out ANextDirectoryPos: LongWord): TTiffError;
    procedure SaveToOutput(AOutput: TTiffIO; out ADirectoryPos: LongWord; out ANextDirectoryPosStreamPos: int64);
    function ToString: ansistring; override;
    function GetOrCreateTag(ATag: Word): PTiffDirEntry;
    function RemoveTag(ATag: Word): boolean;
    function IndexOfTag(ATag: Word): integer;
    property EntryCount: integer read FDirEntryCount;
    property Entry[AIndex: integer]: PTiffDirEntry read GetEntry;
  end;

  { TTiffExifDirectory }

  TTiffExifDirectory = class(TTiffDirectory)
  private
    function LoadChunks({%H-}AInput: TTiffIO): TTiffError; override;
    procedure SaveChunks({%H-}AOutput: TTiffIO); override;
  public
    constructor Create;
  end;

  { TTiffImageDirectory }

  TTiffImageDirectory = class(TTiffDirectory) //also called IFD
  private
    FExif: TTiffExifDirectory;
    FStripChunks, FTileChunks, FFreeChunks: TStreamList;
    function GetBitDepth: Word;
    function GetExtraBitDepth: Word;
    function GetHeight: LongWord;
    function GetTotalBitDepth: Word;
    function GetWidth: LongWord;
    function LoadChunks(AInput: TTiffIO): TTiffError; override;
    procedure SaveChunks(AOutput: TTiffIO); override;
    function LoadExifChunk(AInput: TTiffIO): TTiffError;
    procedure SaveExifChunk(AOutput: TTiffIO);
  public
    constructor Create;
    destructor Destroy; override;
    property Width: LongWord read GetWidth;
    property Height: LongWord read GetHeight;
    property BitDepth: Word read GetBitDepth;
    property ExtraBitDepth: Word read GetExtraBitDepth;
    property TotalBitDepth: Word read GetTotalBitDepth;
  end;

  TTiffImageDirectoryList = specialize TFPGObjectList<TTiffImageDirectory>;

  { TTiff }

  TTiff = class
  private
    FImageEntries: TTiffImageDirectoryList;
    FLittleEndian: boolean;
    function GetCount: integer;
    function GetEntry(AIndex: integer): TTiffImageDirectory;
  protected
    function ReadHeader(var AInput: TTiffIO; out AFirstImagePos: LongWord): TTiffError;
    procedure WriteHeader(var AOutput: TTiffIO; out AFirstImagePosStreamPos: int64);
    function LoadImageEntries(var AInput: TTiffIO; AFirstImagePos: LongWord): TTiffError;
  public
    constructor Create;
    procedure Clear;
    function LoadFromStream(AStream: TStream): TTiffError;
    procedure SaveToStream(AStream: TStream); overload;
    procedure SaveToStream(AStream: TStream; AEntryIndices: array of integer); overload;
    procedure Delete(AIndex: integer);
    procedure Move(AFromTiff: TTiff; AFromIndex: integer; AToIndex: integer); overload;
    function Move(AFromTiff: TTiff; AFromIndex: integer): integer; overload;
    procedure Move(AFromIndex, AToIndex: integer); overload;
    destructor Destroy; override;
    function ToString: ansistring; override;
    function GetBiggestImage: TTiffImageDirectory;
    function IndexOfImage(AImage: TTiffImageDirectory): integer;
    property Count: integer read GetCount;
    property Entry[AIndex: integer]: TTiffImageDirectory read GetEntry;
    property LittleEndian: boolean read FLittleEndian write FLittleEndian;
  end;

function GetTiffTagName(ATag: Word): string;

implementation

uses math;

function TiffRational(ANumerator, ADenominator: LongWord): TTiffRational;
begin
  result.Numerator := ANumerator;
  result.Denominator:= ADenominator;
  result.Negative := false;
end;

function TiffRational(ANumerator, ADenominator: Integer): TTiffRational;
begin
  result.Numerator := abs(ANumerator);
  result.Denominator:= abs(ADenominator);
  result.Negative := (ANumerator < 0) xor (ADenominator < 0);
end;

function GetTiffTagName(ATag: Word): string;
begin
  case ATag of
  TiffTagNewSubFileType: result := 'NewSubFileType';
  TiffTagSubfileType: result := 'SubfileType';
  TiffTagImageWidth: result := 'ImageWidth';
  TiffTagImageLength: result := 'ImageLength';
  TiffTagBitsPerSample: result := 'BitsPerSample';
  TiffTagCompression: result := 'Compression';
  TiffTagPhotometricInterpretation: result := 'PhotometricInterpretation';
  TiffTagThresholding: result := 'Thresholding';
  TiffTagCellWidth: result := 'CellWidth';
  TiffTagCellLength: result := 'CellLength';
  TiffTagFillOrder: result := 'FillOrder';
  TiffTagDocumentName: result := 'DocumentName';
  TiffTagImageDescription: result := 'ImageDescription';
  TiffTagMake: result := 'Make';
  TiffTagModel: result := 'Model';
  TiffTagStripOffsets: result := 'StripOffsets';
  TiffTagOrientation: result := 'Orientation';
  TiffTagSamplesPerPixel: result := 'SamplesPerPixel';
  TiffTagRowsPerStrip: result := 'RowsPerStrip';
  TiffTagStripByteCounts: result := 'StripByteCounts';
  TiffTagMinSampleValue: result := 'MinSampleValue';
  TiffTagMaxSampleValue: result := 'MaxSampleValue';
  TiffTagXResolution: result := 'XResolution';
  TiffTagYResolution: result := 'YResolution';
  TiffTagPlanarConfiguration: result := 'PlanarConfiguration';
  TiffTagPageName: result := 'PageName';
  TiffTagXPosition: result := 'XPosition';
  TiffTagYPosition: result := 'YPosition';
  TiffTagFreeOffsets: result := 'FreeOffsets';
  TiffTagFreeByteCounts: result := 'FreeByteCounts';
  TiffTagGrayResponseUnit: result := 'GrayResponseUnit';
  TiffTagGrayResponseCurve: result := 'GrayResponseCurve';
  TiffTagT4Options: result := 'T4Options';
  TiffTagT6Options: result := 'T6Options';
  TiffTagResolutionUnit: result := 'ResolutionUnit';
  TiffTagPageNumber: result := 'PageNumber';
  TiffTagTransferFunction: result := 'TransferFunction';
  TiffTagSoftware: result := 'Software';
  TiffTagDateTime: result := 'DateTime';
  TiffTagArtist: result := 'Artist';
  TiffTagHostComputer: result := 'HostComputer';
  TiffTagPredictor: result := 'Predictor';
  TiffTagWhitePoint: result := 'WhitePoint';
  TiffTagPrimaryChromacities: result := 'PrimaryChromacities';
  TiffTagColorMap: result := 'ColorMap';
  TiffTagHalftoneHints: result := 'HalftoneHints';
  TiffTagTileWidth: result := 'TileWidth';
  TiffTagTileLength: result := 'TileLength';
  TiffTagTileOffsets: result := 'TileOffsets';
  TiffTagTileByteCounts: result := 'TileByteCounts';
  TiffTagBadFaxLines: result := 'BadFaxLines';
  TiffTagCleanFaxData: result := 'CleanFaxData';
  TiffTagConsecutiveBadFaxLines: result := 'ConsecutiveBadFaxLines';
  TiffTagInkSet: result := 'InkSet';
  TiffTakInkNames: result := 'InkNames';
  TiffTagNumberOfInks: result := 'NumberOfInks';
  TiffTagDotRange: result := 'DotRange';
  TiffTagTargetPrinter: result := 'TargetPrinter';
  TiffTagExtraSamples: result := 'ExtraSamples';
  TiffTagSampleFormat: result := 'SampleFormat';
  TiffTagSMinSampleValue: result := 'SMinSampleValue';
  TiffTagSMaxSampleValue: result := 'SMaxSampleValue';
  TiffTagTransferRange: result := 'TransferRange';
  TiffTagJPEGTables: result := 'JPEGTables';
  TiffTagJPEGProc: result := 'JPEGProc';
  TiffTagJPEGInterchangeFormat: result := 'JPEGInterchangeFormat';
  TiffTagJPEGInterchangeFormatLength: result := 'JPEGInterchangeFormatLength';
  TiffTagJPEGRestartInterval: result := 'JPEGRestartInterval';
  TiffTagJPEGLosslessPerdictors: result := 'JPEGLosslessPerdictors';
  TiffTagJPEGPointTransforms: result := 'JPEGPointTransforms';
  TiffTagJPEGQTables: result := 'JPEGQTables';
  TiffTagJPEGDCTables: result := 'JPEGDCTables';
  TiffTagJPEGACTables: result := 'JPEGACTables';
  TiffTagYCbCrCoefficients: result := 'YCbCrCoefficients';
  TiffTafYCbCrSubSampling: result := 'YCbCrSubSampling';
  TiffTagYCbCrPositioning: result := 'YCbCrPositioning';
  TiffTagReferenceBlackWhite: result := 'ReferenceBlackWhite';
  TiffTagXMLPacket: result := 'XMLPacket';
  TiffTagCopyright: result := 'Copyright';
  TiffTagRichTiffIPTC: result := 'RichTiffIPTC';
  TiffTagPhotoshopImageResourceBlocks: result := 'PhotoshopImageResourceBlocks';
  TiffTagExifIFD: result := 'ExifIFD';
  TiffTagICCProfile: result := 'ICCProfile';

  TiffTagHylaFaxReceiveParams: result := 'HylaFaxReceiveParams';
  TiffTagHylaFaxReceiveTimeSecs: result := 'HylaFaxReceiveTimeSecs';

  ExifTagColorspace: result := 'Colorspace';
  ExifTagPixelXDimension: result := 'PixelXDimension';
  ExifTagPixelYDimension: result := 'PixelYDimension';
  else
    result := 'Tag'+IntToStr(ATag);
  end;
end;

{ TTiffExifDirectory }

function TTiffExifDirectory.LoadChunks(AInput: TTiffIO): TTiffError;
begin
  result := teNone;
end;

procedure TTiffExifDirectory.SaveChunks(AOutput: TTiffIO);
begin
  //nothing
end;

constructor TTiffExifDirectory.Create;
begin
  inherited Create;
end;

{ TTiffImageDirectory }

function TTiffImageDirectory.LoadChunks(AInput: TTiffIO): TTiffError;
var subError: TTiffError;
begin
  LoadExifChunk(AInput); //ignore error as Exif is optional

  subError := LoadChunkList(AInput, TiffTagStripOffsets, TiffTagStripByteCounts, FStripChunks);
  if subError <> teNone then Exit(subError);

  subError := LoadChunkList(AInput, TiffTagTileOffsets, TiffTagTileByteCounts, FTileChunks);
  if subError <> teNone then Exit(subError);

  subError := LoadChunkList(AInput, TiffTagFreeOffsets, TiffTagFreeByteCounts, FFreeChunks);
  if subError <> teNone then Exit(subError);

  result := teNone;
end;

procedure TTiffImageDirectory.SaveChunks(AOutput: TTiffIO);
begin
  SaveChunkList(AOutput, TiffTagStripOffsets, TiffTagStripByteCounts, FStripChunks);
  SaveChunkList(AOutput, TiffTagTileOffsets, TiffTagTileByteCounts, FTileChunks);
  SaveChunkList(AOutput, TiffTagFreeOffsets, TiffTagFreeByteCounts, FFreeChunks);
  SaveExifChunk(AOutput);
end;

function TTiffImageDirectory.GetWidth: LongWord;
var idxWidth: integer;
begin
  idxWidth := IndexOfTag(TiffTagImageWidth);
  if idxWidth = -1 then result := 0
  else result := Entry[idxWidth]^.UnsignedValue[0];
end;

function TTiffImageDirectory.GetHeight: LongWord;
var idxHeight: integer;
begin
  idxHeight := IndexOfTag(TiffTagImageLength);
  if idxHeight = -1 then result := 0
  else result := Entry[idxHeight]^.UnsignedValue[0];
end;

function TTiffImageDirectory.GetBitDepth: Word;
begin
  result := TotalBitDepth - ExtraBitDepth;
end;

function TTiffImageDirectory.GetExtraBitDepth: Word;
var
  idxDepth, i: Integer;
begin
  idxDepth := IndexOfTag(TiffTagExtraSamples);
  result := 0;
  if idxDepth <> -1 then
  with Entry[idxDepth]^ do
    for i := 0 to ValueCount-1 do
      inc(result, UnsignedValue[i]);
end;

function TTiffImageDirectory.GetTotalBitDepth: Word;
var
  idxDepth, i: Integer;
begin
  idxDepth := IndexOfTag(TiffTagBitsPerSample);
  result := 0;
  if idxDepth <> -1 then
  with Entry[idxDepth]^ do
    for i := 0 to ValueCount-1 do
      inc(result, UnsignedValue[i]);
end;

constructor TTiffImageDirectory.Create;
begin
  inherited Create;
  FExif := nil;
  FStripChunks := nil;
  FTileChunks := nil;
  FFreeChunks := nil;
end;

destructor TTiffImageDirectory.Destroy;
begin
  ClearChunkList(FStripChunks);
  ClearChunkList(FTileChunks);
  ClearChunkList(FFreeChunks);
  FreeAndNil(FExif);
  inherited Destroy;
end;

function TTiffImageDirectory.LoadExifChunk(AInput: TTiffIO): TTiffError;
var idxExif: integer;
  nextExifPos: LongWord;
begin
  idxExif := IndexOfTag(TiffTagExifIFD);
  if (idxExif = -1) then exit;
  with Entry[idxExif]^ do
    if (ValueCount = 1) and (ValueType in[tvtLong,tvtWord,tvtByte]) then
    begin
      FreeAndNil(FExif);
      FExif := TTiffExifDirectory.Create;
      result := FExif.LoadFromInput(AInput, UnsignedValue[0], nextExifPos);
      if result <> teNone then FreeAndNil(FExif);
    end else
      result := teInvalidStreamOffset;
end;

procedure TTiffImageDirectory.SaveExifChunk(AOutput: TTiffIO);
var
  exifPos: LongWord;
  nextExifPosStreamPos: int64;
  exifEntry: PTiffDirEntry;
begin
  if Assigned(FExif) then
    exifEntry := GetOrCreateTag(TiffTagExifIFD)
  else
  begin
    RemoveTag(TiffTagExifIFD);
    exit;
  end;
  FExif.SaveToOutput(AOutput, exifPos, nextExifPosStreamPos);
  exifEntry^.SetLong(exifPos);
end;

{ TTiffRational }

function TTiffRational.AsString: string;
begin
  if Negative then result := '-' else result := '';
  result += IntToStr(Numerator)+'/'+IntToStr(Denominator);
end;

function TTiffRational.AsDouble: double;
begin
  result := Numerator/Denominator;
  if Negative then result := -result;
end;

{ TTiffIO }

function TTiffIO.GetPosition: int64;
begin
  result := FStream.Position - FStartPos;
end;

function TTiffIO.GetSize: int64;
begin
  result := FStream.Size - FStartPos;
end;

procedure TTiffIO.SetPosition(AValue: int64);
begin
  FStream.Position := AValue + FStartPos;
end;

procedure TTiffIO.Init(AStream: TStream; AStartPos: int64);
begin
  FStream := AStream;
  FLittleEndian:= false;
  FStartPos := AStartPos;
  if AStream.Position <> AStartPos then
    AStream.Position:= AStartPos;
end;

function TTiffIO.CopyTo(AStream: TStream; ACount: LongWord): TTiffError;
begin
  if AStream.CopyFrom(FStream, ACount) <> ACount then
    result := teUnexpectedEndOfStream
  else
    result := teNone;
end;

procedure TTiffIO.CopyFrom(AStream: TStream; ACount: LongWord);
begin
  if FStream.CopyFrom(AStream, ACount) <> ACount then
    raise exception.Create('Unexpected end of stream');
end;

function TTiffIO.ReadBuffer(var ABuffer; ACount: integer): TTiffError;
begin
  fillchar(ABuffer, ACount, 0);
  if FStream.Read(ABuffer, ACount) <> ACount then
    exit(teUnexpectedEndOfStream)
  else
    exit(teNone);
end;

procedure TTiffIO.WriteBuffer(var ABuffer; ACount: integer);
begin
  FStream.WriteBuffer(ABuffer, ACount);
end;

function TTiffIO.ReadByte(out AValue: byte): TTiffError;
begin
  AValue := 0;
  result := ReadBuffer(AValue, sizeof(AValue));
end;

function TTiffIO.ReadWord(out AValue: Word): TTiffError;
begin
  AValue := 0;
  result := ReadBuffer(AValue, sizeof(AValue));
  AValue := FixEndian(AValue);
end;

function TTiffIO.ReadLong(out AValue: LongWord): TTiffError;
begin
  AValue := 0;
  result := ReadBuffer(AValue, sizeof(AValue));
  AValue := FixEndian(AValue);
end;

procedure TTiffIO.WriteByte(AValue: byte);
begin
  FStream.WriteByte(AValue);
end;

procedure TTiffIO.WriteWord(AValue: Word);
begin
  AValue := FixEndian(AValue);
  WriteBuffer(AValue, sizeof(AValue));
end;

procedure TTiffIO.WriteLong(AValue: LongWord);
begin
  AValue := FixEndian(AValue);
  WriteBuffer(AValue, sizeof(AValue));
end;

function TTiffIO.FixEndian(AValue: Word): Word;
begin
  If FLittleEndian then
    result := LEtoN(AValue)
  else
    result := BEtoN(AValue);
end;

function TTiffIO.FixEndian(AValue: LongWord): LongWord;
begin
  If FLittleEndian then
    result := LEtoN(AValue)
  else
    result := BEtoN(AValue);
end;

function TTiffIO.FixEndian(AValue: QWord): QWord;
begin
  If FLittleEndian then
    result := LEtoN(AValue)
  else
    result := BEtoN(AValue);
end;

{ TTiffDirEntry }

function TTiffDirEntry.GetData: Pointer;
begin
  if Assigned(FLongData) then
    result := FLongData
  else
    result := @FShortData;
end;

function TTiffDirEntry.GetDoubleValue(AIndex: LongWord): Double;
begin
  case ValueType of
  tvtSingle: result := PSingle(Data)[AIndex];
  tvtDouble: result := PDouble(Data)[AIndex];
  tvtSignedByte,tvtSignedWord,tvtSignedLongWord: result := GetSignedValue(AIndex);
  tvtByte,tvtWord,tvtLong: result := GetUnsignedValue(AIndex);
  tvtRational,tvtSignedRational: result := GetRationalValue(AIndex).AsDouble;
  else
    raise Exception.Create('Incompatible types');
  end;
end;

function TTiffDirEntry.GetRationalValue(AIndex: LongWord): TTiffRational;
begin
  case ValueType of
  tvtRational: result := TiffRational(PLongWord(Data)[AIndex*2],PLongWord(Data)[AIndex*2+1]);
  tvtSignedRational: result := TiffRational(PInteger(Data)[AIndex*2],PInteger(Data)[AIndex*2+1]);
  tvtSignedByte,tvtSignedWord,tvtSignedLongWord: result := TiffRational(GetSignedValue(AIndex),1);
  tvtByte,tvtWord,tvtLong: TiffRational(GetUnsignedValue(AIndex),1);
  else
    raise Exception.Create('Incompatible types');
  end;
end;

function TTiffDirEntry.GetName: string;
begin
  result := GetTiffTagName(Tag);
end;

function TTiffDirEntry.GetSignedValue(AIndex: LongWord): Integer;
begin
  if AIndex >= ValueCount then
    raise ERangeError.Create('Index out of bounds');
  case ValueType of
  tvtSignedByte: result := PShortInt(Data)[AIndex];
  tvtSignedWord: result := PSmallInt(Data)[AIndex];
  tvtSignedLongWord: result := PLongInt(Data)[AIndex];
  else result := GetUnsignedValue(AIndex);
  end;
end;

function TTiffDirEntry.GetStringValue: string;
var
  i: LongWord;
begin
  case ValueType of
  tvtAscii: begin
      setlength(result, ValueCount-1);
      if result <> '' then
        move(Data^, result[1], length(result));
      result := '''' + StringReplace(result, '''', '''''', [rfReplaceAll]) + '''';
    end;
  tvtRawByte: result := '<'+inttostr(ValueCount)+' raw bytes>';
  tvtUnknown: result := '?';
  else
    begin
      if (ValueCount > 40) and (ValueType = tvtByte) then
        result := '<'+inttostr(ValueCount)+' bytes>'
      else
      if (ValueCount > 40) and (ValueType = tvtWord) then
        result := '<'+inttostr(ValueCount)+' words>'
      else
      if (ValueCount > 40) and (ValueType = tvtLong) then
        result := '<'+inttostr(ValueCount)+' longs>'
      else
      begin
        if ValueCount <> 1 then result := '[' else result := '';
        if ValueCount > 0 then
          for i := 0 to ValueCount-1 do
          begin
            if i > 0 then result += ', ';
            case ValueType of
            tvtByte,tvtWord,tvtLong:
              result += IntToStr(UnsignedValue[i]);
            tvtSignedByte,tvtSignedWord,tvtSignedLongWord:
              result += IntToStr(SignedValue[i]);
            tvtRational,tvtSignedRational:
              result += RationalValue[i].AsString;
            tvtSingle,tvtDouble:
              result += FloatToStr(DoubleValue[i]);
            else
              result += '?';
            end;
          end;
        if ValueCount <> 1 then result += ']';
      end;
    end;
  end;
end;

function TTiffDirEntry.GetUnsignedValue(AIndex: LongWord): LongWord;
var
  signed: Integer;
begin
  if AIndex >= ValueCount then
    raise ERangeError.Create('Index out of bounds');
  case ValueType of
  tvtSignedByte,tvtSignedWord,tvtSignedLongWord:
    begin
      signed := GetSignedValue(AIndex);
      if signed < 0 then result := 0
      else result := signed;
    end;
  tvtByte,tvtAscii,tvtRawByte: result := PByte(Data)[AIndex];
  tvtWord: result := PWord(Data)[AIndex];
  tvtLong: result := PLongWord(Data)[AIndex];
  else
    raise Exception.Create('Not implemented');
  end;
end;

procedure TTiffDirEntry.Free;
begin
  ReallocMem(FLongData, 0);
end;

procedure TTiffDirEntry.Realloc(AValueType: TTiffValueType; AValueCount: LongWord);
var dataSize: PtrUInt;
begin
  FValueType := AValueType;
  FValueCount := AValueCount;
  dataSize := PtrUInt(TiffValueSize[AValueType])*AValueCount;
  if dataSize <= 4 then
    Free
  else
    ReallocMem(FLongData, dataSize);
end;

procedure TTiffDirEntry.InitNew(ATag: Word);
begin
  FTag := ATag;
  FValueCount:= 0;
  FValueType := tvtUnknown;
  fillchar(FShortData, sizeof(FShortData), 0);
  FLongData := nil;
end;

procedure TTiffDirEntry.FixEndianData(AData: Pointer; AIO: TTiffIO);
var i: LongWord;
begin
  if FValueCount = 0 then exit;

  if FValueType in[tvtWord, tvtSignedWord] then
  begin
    for i := 0 to FValueCount-1 do
      PWord(AData)[i] := AIO.FixEndian(PWord(AData)[i]);
  end else
  if FValueType in [tvtLong,tvtRational, tvtSignedLongWord,tvtSignedRational, tvtSingle] then
  begin
    for i := 0 to FValueCount-1 do
      PLongWord(AData)[i] := AIO.FixEndian(PLongWord(AData)[i]);
  end else
  if FValueType = tvtDouble then
  begin
    for i := 0 to FValueCount-1 do
      PQWord(AData)[i] := AIO.FixEndian(PQWord(AData)[i]);
  end;
end;

function TTiffDirEntry.LoadFromInput(AInput: TTiffIO; const ARaw: TTiffRawDirEntry): TTiffError;
var dataSize: PtrUInt;
  valueTypeWord: Word;
  valueOffset: LongWord;
begin
  FTag := AInput.FixEndian(ARaw.Tag);
  FValueCount:= AInput.FixEndian(ARaw.ValueCount);
  FValueType := tvtUnknown;
  fillchar(FShortData, sizeof(FShortData), 0);
  FLongData := nil;

  valueTypeWord := AInput.FixEndian(ARaw.ValueType);
  if (valueTypeWord = 0) or (valueTypeWord > ord(high(TTiffValueType))) then
    Exit(teUnknownValueType);
  FValueType := TTiffValueType(valueTypeWord);

  dataSize := PtrUInt(TiffValueSize[FValueType]) * FValueCount;
  if dataSize > 4 then
  begin
    valueOffset := AInput.FixEndian(ARaw.ValueOffset);
    if (valueOffset < 8) or (valueOffset > AInput.Size - dataSize) then
      Exit(teInvalidStreamOffset);
    AInput.Position := valueOffset;

    GetMem(FLongData, dataSize);
    result := AInput.ReadBuffer(FLongData^, dataSize);
    if result <> teNone then
      ReallocMem(FLongData, 0)
    else
      FixEndianData(FLongData, AInput);
  end else
  begin
    move(ARaw.ShortData, FShortData, dataSize);
    FixEndianData(@FShortData, AInput);
    Exit(teNone);
  end;
end;

procedure TTiffDirEntry.SaveToOutput(AOutput: TTiffIO; out
  ARaw: TTiffRawDirEntry);
var
  dataSize: PtrUInt;
begin
  ARaw.Tag := AOutput.FixEndian(Tag);
  ARaw.ValueCount := AOutput.FixEndian(ValueCount);
  ARaw.ValueType := AOutput.FixEndian(Word(ValueType));
  dataSize := PtrUInt(TiffValueSize[ValueType]) * ValueCount;
  if dataSize > 4 then
  begin
    if not Assigned(FLongData) then
      raise exception.Create('Long data not allocated');
    if dataSize > maxLongint then
      raise exception.Create('Data too long');

    {$PUSH}{$RANGECHECKS ON}
    ARaw.ValueOffset:= AOutput.FixEndian(LongWord(AOutput.Position));
    {$POP}
    FixEndianData(FLongData, AOutput);
    AOutput.WriteBuffer(FLongData^, dataSize);
    FixEndianData(FLongData, AOutput);
  end else
  begin
    move(FShortData, ARaw.ShortData, dataSize);
    FixEndianData(@ARaw.ShortData, AOutput);
  end;
end;

procedure TTiffDirEntry.SetLong(AValue: LongWord);
begin
  Realloc(tvtLong, 1);
  PLongWord(Data)[0] := AValue;
end;

procedure TTiffDirEntry.SetWord(AValue: Word);
begin
  Realloc(tvtWord, 1);
  PWord(Data)[0] := AValue;
end;

procedure TTiffDirEntry.SetByte(AValue: Byte);
begin
  Realloc(tvtByte, 1);
  PByte(Data)[0] := AValue;
end;

procedure TTiffDirEntry.SetLongArray(AValues: ArrayOfLongWord);
var i: Integer;
  p : PLongWord;
  wordSized: boolean;
  words: ArrayOfWord;
begin
  wordSized := true;
  for i := 0 to high(AValues) do
    if AValues[i] > high(Word) then
    begin
      wordSized := false;
      break;
    end;
  if wordSized then
  begin
    setlength(words, length(AValues));
    for i := 0 to high(AValues) do
      words[i] := AValues[i];
    SetWordArray(words);
    exit;
  end;

  Realloc(tvtLong, length(AValues));
  if length(AValues)>0 then
  begin
    p := PLongWord(Data);
    for i := 0 to high(AValues) do
      p[i] := AValues[i];
  end;
end;

procedure TTiffDirEntry.SetWordArray(AValues: ArrayOfWord);
var i: Integer;
  p : PWord;
begin
  Realloc(tvtWord, length(AValues));
  if length(AValues)>0 then
  begin
    p := PWord(Data);
    for i := 0 to high(AValues) do
      p[i] := AValues[i];
  end;
end;

{ TTiffDirectory }

function TTiffDirectory.GetEntry(AIndex: integer): PTiffDirEntry;
begin
  result := @FDirEntries[AIndex];
end;

constructor TTiffDirectory.Create;
begin
  FDirEntryCount := 0;
end;

destructor TTiffDirectory.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TTiffDirectory.Clear;
var i: integer;
begin
  for i := 0 to FDirEntryCount-1 do
    FDirEntries[i].Free;
  FDirEntries := nil;
  FDirEntryCount:= 0;
end;

function CompareTagOfDirEntry(elem1, elem2: pointer): Integer;
begin
  result := integer(PTiffDirEntry(elem1)^.Tag) - integer(PTiffDirEntry(elem2)^.Tag);
end;

procedure TTiffDirectory.SortEntries;
type
  TCompareFunc = function (elem1, elem2: pointer): Integer;

  procedure AnyQuickSort(Arr: pointer; idxL, idxH: Integer;
    Stride: Integer; CompareFunc: TCompareFunc;
    SwapBuf : Pointer = nil);
  var
    ls,hs : Integer;
    li,hi : Integer;
    mi    : Integer;
    ms    : Integer;
    pb    : PByte;
    OwnSwapBuf: boolean;
  begin
    if SwapBuf = nil then
    begin
      GetMem(SwapBuf, Stride);
      OwnSwapBuf := true;
    end else
      OwnSwapBuf := false;
    pb:=PByte(Arr);
    li:=idxL;
    hi:=idxH;
    mi:=(li+hi) div 2;
    ls:=li*Stride;
    hs:=hi*Stride;
    ms:=mi*Stride;
    repeat
      while CompareFunc( @pb[ls], @pb[ms] ) < 0 do begin
        inc(ls, Stride);
        inc(li);
      end;
      while CompareFunc( @pb[ms], @pb[hs] ) < 0 do begin
        dec(hs, Stride);
        dec(hi);
      end;
      if ls <= hs then begin
        Move(pb[ls], SwapBuf^, Stride);
        Move(pb[hs], pb[ls], Stride);
        Move(SwapBuf^, pb[hs], Stride);
        inc(ls, Stride); inc(li);
        dec(hs, Stride); dec(hi);
      end;
    until ls>hs;
    if hi>idxL then AnyQuickSort(Arr, idxL, hi, Stride, CompareFunc, SwapBuf);
    if li<idxH then AnyQuickSort(Arr, li, idxH, Stride, CompareFunc, SwapBuf);
    if OwnSwapBuf then FreeMem(SwapBuf);
  end;

begin
  if FDirEntryCount > 0 then
    AnyQuickSort(@FDirEntries[0], 0, FDirEntryCount-1, sizeof(TTiffDirEntry), @CompareTagOfDirEntry);
end;

function TTiffDirectory.AddEntry(const AEntry: TTiffDirEntry): TTiffError;
var
  i: Integer;
begin
  for i := 0 to FDirEntryCount-1 do
    if FDirEntries[i].Tag = AEntry.Tag then
      Exit(teDuplicateTag);

  if length(FDirEntries) = FDirEntryCount then
    setlength(FDirEntries, length(FDirEntries)*2+8);
  FDirEntries[FDirEntryCount] := AEntry;
  Inc(FDirEntryCount);
  Exit(teNone);
end;

function TTiffDirectory.LoadChunkList(AInput: TTiffIO; ATagOffsets, ATagByteCounts: Word;
  var AList: TStreamList): TTiffError;
var i, chunkCount: LongWord;
  idxOffsets, idxByteCounts: Integer;
  offsets, byteCounts: PTiffDirEntry;
  chunkOffset, chunkSize: LongWord;
  mem: TMemoryStream;
  subError: TTiffError;
begin
  FreeAndNil(AList);

  idxOffsets := IndexOfTag(ATagOffsets);
  idxByteCounts := IndexOfTag(ATagByteCounts);

  if (idxOffsets = -1) or (idxByteCounts = -1) then Exit(teNone);

  offsets := Entry[idxOffsets];
  byteCounts := Entry[idxByteCounts];
  chunkCount := min(offsets^.ValueCount, byteCounts^.ValueCount);
  if chunkCount > 0 then
  begin
    AList := TStreamList.Create;
    for i := 0 to chunkCount-1 do
    begin
      chunkOffset := offsets^.UnsignedValue[i];
      chunkSize := byteCounts^.UnsignedValue[i];
      if (chunkOffset < 8) or (chunkOffset > AInput.Size - chunkSize) then
        Exit(teInvalidStreamOffset);

      AInput.Position := chunkOffset;
      mem := TMemoryStream.Create;
      subError := AInput.CopyTo(mem, chunkSize);
      if subError <> teNone then
      begin
        mem.Free;
        Exit(subError);
      end else
        AList.Add(mem);
    end;
  end;
  result := teNone;
end;

procedure TTiffDirectory.SaveChunkList(AOutput: TTiffIO; ATagOffsets,
  ATagByteCounts: Word; AList: TStreamList);
var
  offsets, byteCounts: array of LongWord;
  i: Integer;
begin
  if not Assigned(AList) or (AList.Count = 0) then
  begin
    RemoveTag(ATagOffsets);
    RemoveTag(ATagByteCounts);
    exit;
  end;

  setlength(offsets, AList.Count);
  setlength(byteCounts, AList.Count);

  for i := 0 to AList.Count-1 do
  begin
    {$PUSH}{$RANGECHECKS ON}
    offsets[i] := AOutput.Position;
    byteCounts[i] := AList[i].Size;
    {$POP}
    AList[i].Position := 0;
    AOutput.CopyFrom(AList[i], AList[i].Size);
  end;
  GetOrCreateTag(ATagOffsets)^.SetLongArray(offsets);
  GetOrCreateTag(ATagByteCounts)^.SetLongArray(byteCounts);
end;

procedure TTiffDirectory.ClearChunkList(var AList: TStreamList);
begin
  if Assigned(AList) then
  begin
    AList.Clear;
    FreeAndNil(AList);
  end;
end;

function TTiffDirectory.LoadFromInput(AInput: TTiffIO; ADirectoryPos: LongWord; out
  ANextDirectoryPos: LongWord): TTiffError;
var
  rawEntries: packed array of TTiffRawDirEntry;
  subError: TTiffError;
  dirCount: Word;
  newEntry: TTiffDirEntry;
  i: Word;
begin
  ANextDirectoryPos := 0;

  if (ADirectoryPos < 8) or (ADirectoryPos > AInput.Size - 2) then
    exit(teInvalidStreamOffset);

  AInput.Position := ADirectoryPos;
  subError := AInput.ReadWord(dirCount);
  if subError <> teNone then Exit(subError);

  setlength(rawEntries, dirCount);
  subError := AInput.ReadBuffer(rawEntries[0], dirCount*sizeof(TTiffRawDirEntry));
  if subError <> teNone then Exit(subError);

  subError := AInput.ReadLong(ANextDirectoryPos);
  if subError <> teNone then Exit(subError);

  fillchar({%H-}newEntry, sizeof({%H-}newEntry), 0);
  if dirCount > 0 then
    for i := 0 to dirCount-1 do
    begin
      subError := newEntry.LoadFromInput(AInput, rawEntries[i]);
      if subError = teUnknownValueType then Continue; //skip unknown types
      if subError <> teNone then Exit(subError); //stop on other errors

      subError := AddEntry(newEntry);
      if subError <> teNone then
      begin
        newEntry.Free;
        Exit(subError);
      end;
    end;

  result := LoadChunks(AInput);
end;

procedure TTiffDirectory.SaveToOutput(AOutput: TTiffIO; out
  ADirectoryPos: LongWord; out ANextDirectoryPosStreamPos: int64);
var
  rawEntries: packed array of TTiffRawDirEntry;
  i: Integer;
begin
  SaveChunks(AOutput);

  SortEntries;
  setlength(rawEntries, EntryCount);
  for i := 0 to EntryCount-1 do
    Entry[i]^.SaveToOutput(AOutput, rawEntries[i]);

  if odd(AOutput.Position) then AOutput.WriteByte(0); //word padding

  {$PUSH}{$RANGECHECKS ON}
  ADirectoryPos:= AOutput.Position;
  {$POP}
  AOutput.WriteWord(EntryCount);
  if EntryCount > 0 then
    AOutput.WriteBuffer(rawEntries[0], EntryCount*sizeof(TTiffRawDirEntry));

  ANextDirectoryPosStreamPos:= AOutput.Position;
  AOutput.WriteLong(0);
end;

function TTiffDirectory.ToString: ansistring;
var
  i: Integer;
begin
  result := '';
  for i := 0 to EntryCount-1 do
  with Entry[i]^ do
  begin
    if i > 0 then result += ','+LineEnding;
    result += Name+': '+StringValue;
  end;
end;

function TTiffDirectory.GetOrCreateTag(ATag: Word): PTiffDirEntry;
var newEntry: TTiffDirEntry;
  idx: Integer;
begin
  idx := IndexOfTag(ATag);
  if idx = -1 then
  begin
    newEntry.InitNew(ATag);
    AddEntry(newEntry);
    idx := EntryCount-1;
  end;
  result := Entry[idx];
end;

function TTiffDirectory.RemoveTag(ATag: Word): boolean;
var
  idx, i: Integer;
begin
  idx := IndexOfTag(ATag);
  if idx <> -1 then
  begin
    FDirEntries[idx].Free;
    for i := idx to FDirEntryCount-2 do
      FDirEntries[i] := FDirEntries[i+1];
    FDirEntries[EntryCount-1].InitNew(0);
    dec(FDirEntryCount);
    result := true;
  end else
    result := false;
end;

function TTiffDirectory.IndexOfTag(ATag: Word): integer;
var
  i: Integer;
begin
  for i := 0 to EntryCount-1 do
    if Entry[i]^.Tag = ATag then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

{ TTiff }

function TTiff.GetCount: integer;
begin
  result := FImageEntries.Count;
end;

function TTiff.GetEntry(AIndex: integer): TTiffImageDirectory;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise ERangeError.Create('Index out of bounds');
  result := FImageEntries[AIndex];
end;

function TTiff.ReadHeader(var AInput: TTiffIO; out AFirstImagePos: LongWord): TTiffError;
var ByteOrderMark: array[1..2] of char;
  MeaningUniverse: Word;
  SubError: TTiffError;
begin
  AFirstImagePos:= 0;

  SubError := AInput.ReadBuffer({%H-}ByteOrderMark, 2);
  if SubError <> teNone then Exit(SubError);
  if ByteOrderMark = 'II' then AInput.LittleEndian:= true
  else if ByteOrderMark = 'MM' then AInput.LittleEndian:= false
  else Exit(teInvalidHeader);

  MeaningUniverse := 0;
  SubError := AInput.ReadWord(MeaningUniverse);
  if SubError <> teNone then exit(SubError);
  if MeaningUniverse <> 42 then exit(teInvalidHeader);

  SubError := AInput.ReadLong(AFirstImagePos);
  if SubError <> teNone then exit(SubError);
  if AFirstImagePos < 8 then exit(teInvalidHeader);

  Exit(teNone);
end;

procedure TTiff.WriteHeader(var AOutput: TTiffIO; out
  AFirstImagePosStreamPos: int64);
var ByteOrderMark: array[1..2] of char;
begin
  if AOutput.LittleEndian then
    ByteOrderMark := 'II'
  else
    ByteOrderMark := 'MM';
  AOutput.WriteBuffer(ByteOrderMark, 2);
  AOutput.WriteWord(42);
  AFirstImagePosStreamPos:= AOutput.Position;
  AOutput.WriteLong(0);
end;

function TTiff.LoadImageEntries(var AInput: TTiffIO; AFirstImagePos: LongWord
  ): TTiffError;
type TLongwordList = specialize TFPGList<Longword>;
var
  curImagePos, nextImagePos: LongWord;
  previousPositions: TLongwordList;
  newEntry: TTiffImageDirectory;
  i: Integer;
  subError: TTiffError;
begin
  previousPositions := TLongwordList.Create;
  try
    curImagePos := AFirstImagePos;
    repeat
      previousPositions.Add(curImagePos);

      newEntry := TTiffImageDirectory.Create;
      try
        nextImagePos := 0;
        subError := newEntry.LoadFromInput(AInput, curImagePos, nextImagePos);
      except on ex:exception do
        subError := teUnhandledException;
      end;
      if subError <> teNone then
      begin
        newEntry.Free;
        exit(subError);
      end;

      FImageEntries.Add(newEntry);

      for i := 0 to previousPositions.Count-1 do
        if nextImagePos = previousPositions[i] then
          exit(teCircularOffset);

      curImagePos := nextImagePos;
    until nextImagePos = 0;
  finally
    previousPositions.Free;
  end;
  result := teNone;
end;

constructor TTiff.Create;
begin
  FImageEntries := TTiffImageDirectoryList.Create;
  FLittleEndian := false;
end;

procedure TTiff.Clear;
begin
  FImageEntries.Clear;
end;

function TTiff.LoadFromStream(AStream: TStream): TTiffError;
var
  firstImagePos: LongWord;
  subError: TTiffError;
  input: TTiffIO;
begin
  Clear;

  input.Init(AStream, AStream.Position);
  subError := ReadHeader(input, firstImagePos);
  if subError <> teNone then Exit(subError);
  FLittleEndian:= input.LittleEndian;

  result := LoadImageEntries(input, firstImagePos);
end;

procedure TTiff.SaveToStream(AStream: TStream);
var indices: array of integer;
  i: Integer;
begin
  setlength(indices, Count);
  for i := 0 to high(indices) do
    indices[i] := i;
  SaveToStream(AStream, indices);
end;

procedure TTiff.SaveToStream(AStream: TStream; AEntryIndices: array of integer);
var output: TTiffIO;
  curImagePosStreamPos, nextImagePosStreamPos, nextStreamPos: int64;
  curImagePos: LongWord;
  i: Integer;
begin
  if length(AEntryIndices) = 0 then
    raise exception.Create('File cannot be empty');

  output.Init(AStream, AStream.Position);
  output.LittleEndian := LittleEndian;

  WriteHeader(output, curImagePosStreamPos);
  for i := 0 to high(AEntryIndices) do
  begin
    Entry[AEntryIndices[i]].SaveToOutput(output, curImagePos, nextImagePosStreamPos);

    nextStreamPos := output.Position;
    output.Position:= curImagePosStreamPos;
    output.WriteLong(curImagePos);
    output.Position := nextStreamPos;

    curImagePosStreamPos := nextImagePosStreamPos;
  end;
end;

procedure TTiff.Delete(AIndex: integer);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise ERangeError.Create('Index out of bounds');
  FImageEntries.Delete(AIndex);
end;

procedure TTiff.Move(AFromTiff: TTiff; AFromIndex: integer; AToIndex: integer);
var idx: integer;
begin
  if (AToIndex < 0) or (AToIndex > Count) then
    raise ERangeError.Create('Index out of bounds');

  idx := Move(AFromTiff, AFromIndex);
  Move(idx, AToIndex);
end;

function TTiff.Move(AFromTiff: TTiff; AFromIndex: integer): integer;
var
  otherEntry: TTiffImageDirectory;
begin
  otherEntry := AFromTiff.Entry[AFromIndex];
  AFromTiff.FImageEntries.Extract(otherEntry);
  result := FImageEntries.Add(otherEntry);
end;

procedure TTiff.Move(AFromIndex, AToIndex: integer);
var fromEntry: TTiffImageDirectory;
begin
  if (AFromIndex < 0) or (AFromIndex >= Count) then
    raise ERangeError.Create('Index out of bounds');
  if (AToIndex < 0) or (AToIndex >= Count) then
    raise ERangeError.Create('Index out of bounds');
  if AToIndex = AFromIndex then exit;
  fromEntry := Entry[AFromIndex];
  FImageEntries.Extract(fromEntry);
  if AToIndex > AFromIndex then Inc(AToIndex);
  FImageEntries.Insert(AToIndex, fromEntry);
end;

destructor TTiff.Destroy;
begin
  Clear;
  FImageEntries.Free;
  inherited Destroy;
end;

function TTiff.ToString: ansistring;
var
  i: Integer;
begin
  Result:='Count: '+IntToStr(Count);
  for i := 0 to Count-1 do
    result += ','+LineEnding+'Image'+inttostr(i+1)+': {'+Entry[i].ToString+'}';
end;

function TTiff.GetBiggestImage: TTiffImageDirectory;
var
  i: Integer;
begin
  if Count = 0 then
  begin
    result := nil;
    exit;
  end;
  result := Entry[0];
  for i := 1 to Count-1 do
    if (Entry[i].Width+Entry[i].Height > result.Width+result.Height) or
       ((Entry[i].Width+Entry[i].Height = result.Width+result.Height) and
        (Entry[i].BitDepth > result.BitDepth)) then
      result := Entry[i];
end;

function TTiff.IndexOfImage(AImage: TTiffImageDirectory): integer;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    if Entry[i] = AImage then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

end.

