unit BGRAOpenRaster;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRALayers, zipper, DOM, BGRABitmap, FPImage;

const
  OpenRasterMimeType = 'image/openraster'; //do not change, it's part of the file format

type

  { TBGRAOpenRasterDocument }

  TBGRAOpenRasterDocument = class(TBGRALayeredBitmap)
  private
    FFiles: array of record
      Filename: string;
      Stream: TMemoryStream;
    end;
    FStackXML: TXMLDocument;
    FZipInputStream: TStream;
    procedure SetMimeType(AValue: string);
  protected
    Procedure ZipOnCreateStream(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
    Procedure ZipOnDoneStream(Sender : TObject; var AStream : TStream; AItem : TFullZipFileEntry);
    Procedure ZipOnOpenInputStream(Sender : TObject; var AStream : TStream);
    Procedure ZipOnCloseInputStream(Sender : TObject; var AStream : TStream);
    procedure ClearFiles;
    function GetMemoryStream(AFilename: string): TMemoryStream;
    procedure SetMemoryStream(AFilename: string; AStream: TMemoryStream);
    function AddLayerFromMemoryStream(ALayerFilename: string): integer;
    function CopyLayerToMemoryStream(ALayerIndex: integer; ALayerFilename: string): boolean;
    function CopyBitmapToMemoryStream(ABitmap: TBGRABitmap; AFilename: string): boolean;
    procedure SetMemoryStreamAsString(AFilename: string; AContent: string);
    function GetMemoryStreamAsString(AFilename: string): string;
    procedure UnzipFromStream(AStream: TStream);
    procedure UnzipFromFile(AFilenameUTF8: string);
    procedure ZipToFile(AFilenameUTF8: string);
    procedure ZipToStream(AStream: TStream);
    procedure CopyThumbnailToMemoryStream(AMaxWidth, AMaxHeight: integer);
    procedure AnalyzeZip;
    procedure PrepareZipToSave;
    function GetMimeType: string; override;

  public
    constructor Create; override; overload;
    constructor Create(AWidth, AHeight: integer); override; overload;
    procedure Clear; override;
    function CheckMimeType(AStream: TStream): boolean;
    procedure LoadFromStream(AStream: TStream); override;
    procedure LoadFromFile(const filenameUTF8: string); override;
    procedure SaveToFile(const filenameUTF8: string); override;
    procedure SaveToStream(AStream: TStream); override;
    property MimeType : string read GetMimeType write SetMimeType;
    property StackXML : TXMLDocument read FStackXML;
  end;

  { TFPReaderOpenRaster }

  TFPReaderOpenRaster = class(TFPCustomImageReader)
    private
      FWidth,FHeight,FNbLayers: integer;
    protected
      function InternalCheck(Stream: TStream): boolean; override;
      procedure InternalRead(Stream: TStream; Img: TFPCustomImage); override;
    public
      property Width: integer read FWidth;
      property Height: integer read FHeight;
      property NbLayers: integer read FNbLayers;
  end;

  { TFPWriterOpenRaster }

  TFPWriterOpenRaster = class(TFPCustomImageWriter)
    protected
      procedure InternalWrite (Str:TStream; Img:TFPCustomImage); override;
  end;

procedure RegisterOpenRasterFormat;

implementation

uses XMLRead, XMLWrite, FPReadPNG, BGRABitmapTypes, zstream, BGRAUTF8,
  UnzipperExt;

function IsZipStream(stream: TStream): boolean;
var
  header:  packed array[0..1] of char;
  SavePos: int64;
begin
  Result := False;
  try
    if stream.Position + 2 < Stream.Size then
    begin
      header  := #0#0;
      SavePos := stream.Position;
      stream.Read(header, 2);
      stream.Position := SavePos;
      if (header[0] = 'P') and (header[1] = 'K') then
        Result := True;
    end;
  except
    on ex: Exception do ;
  end;
end;

{ TFPWriterOpenRaster }

procedure TFPWriterOpenRaster.InternalWrite(Str: TStream; Img: TFPCustomImage);
var doc: TBGRAOpenRasterDocument;
  tempBmp: TBGRABitmap;
  x,y: integer;

begin
  doc := TBGRAOpenRasterDocument.Create;
  if Img is TBGRABitmap then doc.AddLayer(Img as TBGRABitmap) else
  begin
    tempBmp := TBGRABitmap.Create(img.Width,img.Height);
    for y := 0 to Img.Height-1 do
      for x := 0 to img.Width-1 do
        tempBmp.SetPixel(x,y, FPColorToBGRA(img.Colors[x,y]));
    doc.AddOwnedLayer(tempBmp);
  end;
  doc.SaveToStream(Str);
  doc.Free;
end;

{ TFPReaderOpenRaster }

function TFPReaderOpenRaster.InternalCheck(Stream: TStream): boolean;
var magic: packed array[0..3] of byte;
  OldPos,BytesRead: Int64;
  doc : TBGRAOpenRasterDocument;
begin
  Result:=false;
  if Stream=nil then exit;
  oldPos := stream.Position;
  {$PUSH}{$HINTS OFF}
  BytesRead := Stream.Read(magic,sizeof(magic));
  {$POP}
  stream.Position:= OldPos;
  if BytesRead<>sizeof(magic) then exit;
  if (magic[0] = $50) and (magic[1] = $4b) and (magic[2] = $03) and (magic[3] = $04) then
  begin
    doc := TBGRAOpenRasterDocument.Create;
    result := doc.CheckMimeType(Stream);
    doc.Free;
  end;
end;

procedure TFPReaderOpenRaster.InternalRead(Stream: TStream; Img: TFPCustomImage);
var
  layeredImage: TBGRAOpenRasterDocument;
  flat: TBGRABitmap;
  x,y: integer;
begin
  FWidth := 0;
  FHeight:= 0;
  FNbLayers:= 0;
  layeredImage := TBGRAOpenRasterDocument.Create;
  try
    layeredImage.LoadFromStream(Stream);
    flat := layeredImage.ComputeFlatImage;
    try
      FWidth:= layeredImage.Width;
      FHeight:= layeredImage.Height;
      FNbLayers:= layeredImage.NbLayers;
      if Img is TBGRACustomBitmap then
        TBGRACustomBitmap(img).Assign(flat)
      else
      begin
        Img.SetSize(flat.Width,flat.Height);
        for y := 0 to flat.Height-1 do
          for x := 0 to flat.Width-1 do
            Img.Colors[x,y] := BGRAToFPColor(flat.GetPixel(x,y));
      end;
    finally
      flat.free;
    end;
    layeredImage.Free;
  except
    on ex: Exception do
    begin
      layeredImage.Free;
      raise Exception.Create('Error while loading OpenRaster file. ' + ex.Message);
    end;
  end;
end;

{ TBGRAOpenRasterDocument }

procedure TBGRAOpenRasterDocument.AnalyzeZip;
var StackStream: TMemoryStream;
  imageNode, stackNode, layerNode, attr, srcAttr: TDOMNode;
  i,j,w,h,idx: integer;
  x,y: integer;
  float: double;
  errPos: integer;
  opstr : string;
  gammastr: string;
begin
  if MimeType <> OpenRasterMimeType then
    raise Exception.Create('Invalid mime type');

  StackStream := GetMemoryStream('stack.xml');
  if StackStream = nil then
    raise Exception.Create('Layer stack not found');

  ReadXMLFile(FStackXML, StackStream);

  imageNode := StackXML.FindNode('image');
  if imagenode = nil then
    raise Exception.Create('Image node not found');

  w := 0;
  h := 0;
  LinearBlend := true;

  if Assigned(imageNode.Attributes) then
    for i:=0 to imageNode.Attributes.Length-1 do
    begin
      attr := imagenode.Attributes[i];
      if lowercase(attr.NodeName) = 'w' then
        w := strToInt(attr.NodeValue) else
      if lowercase(attr.NodeName) = 'h' then
        h := strToInt(attr.NodeValue) else
      if lowercase(attr.NodeName) = 'gamma-correction' then
        linearBlend := (attr.NodeValue = 'no') or (attr.NodeValue = '0');
    end;

  SetSize(w,h);

  stackNode := imageNode.FindNode('stack');
  if stackNode = nil then
    raise Exception.Create('Stack node not found');

  for i := stackNode.ChildNodes.Length-1 downto 0 do
  begin
    OnLayeredBitmapLoadProgress((stackNode.ChildNodes.Length-i)*100 div stackNode.ChildNodes.Length);
    layerNode:= stackNode.ChildNodes[i];
    if (layerNode.NodeName = 'layer') and Assigned(layerNode.Attributes) then
    begin
      srcAttr := layerNode.Attributes.GetNamedItem('src');
      idx := AddLayerFromMemoryStream(UTF8Encode(srcAttr.NodeValue));
      if idx <> -1 then
      begin
        x := 0;
        y := 0;
        gammastr := '';
        for j := 0 to layerNode.Attributes.Length-1 do
        begin
          attr := layerNode.Attributes[j];
          if lowercase(attr.NodeName) = 'opacity' then
          begin
            val(attr.NodeValue, float, errPos);
            if errPos = 0 then
            begin
              if float < 0 then float := 0;
              if float > 1 then float := 1;
              LayerOpacity[idx] := round(float*255);
            end;
          end else
          if lowercase(attr.NodeName) = 'gamma-correction' then
            gammastr := attr.NodeValue else
          if lowercase(attr.NodeName) = 'visibility' then
            LayerVisible[idx] := (attr.NodeValue = 'visible') or (attr.NodeValue = 'yes') or (attr.NodeValue = '1') else
          if (lowercase(attr.NodeName) = 'x') or (lowercase(attr.NodeName) = 'y') then
          begin
            val(attr.NodeValue, float, errPos);
            if errPos = 0 then
            begin
              if float < -(MaxInt shr 1) then float := -(MaxInt shr 1);
              if float > (MaxInt shr 1) then float := (MaxInt shr 1);
              if (lowercase(attr.NodeName) = 'x') then x := round(float);
              if (lowercase(attr.NodeName) = 'y') then y := round(float);
            end;
          end else
          if lowercase(attr.NodeName) = 'name' then
            LayerName[idx] := UTF8Encode(attr.NodeValue) else
          if lowercase(attr.NodeName) = 'composite-op' then
          begin
            opstr := StringReplace(lowercase(attr.NodeValue),'_','-',[rfReplaceAll]);
            if (pos(':',opstr) = 0) and (opstr <> 'xor') then opstr := 'svg:'+opstr;
            //parse composite op
            if (opstr = 'svg:src-over') or (opstr = 'krita:dissolve') then
              BlendOperation[idx] := boTransparent else
            if opstr = 'svg:lighten' then
              BlendOperation[idx] := boLighten else
            if opstr = 'svg:screen' then
              BlendOperation[idx] := boScreen else
            if opstr = 'svg:color-dodge' then
              BlendOperation[idx] := boColorDodge else
            if (opstr = 'svg:color-burn') or (opstr = 'krita:gamma_dark'){approx} then
              BlendOperation[idx] := boColorBurn else
            if opstr = 'svg:darken' then
              BlendOperation[idx] := boDarken else
            if (opstr = 'svg:plus') or (opstr = 'svg:add') or (opstr = 'krita:linear_dodge') then
              BlendOperation[idx] := boLinearAdd else
            if (opstr = 'svg:multiply') or (opstr = 'krita:bumpmap') then
              BlendOperation[idx] := boMultiply else
            if opstr = 'svg:overlay' then
              BlendOperation[idx] := boOverlay else
            if opstr = 'svg:soft-light' then
              BlendOperation[idx] := boSvgSoftLight else
            if opstr = 'svg:hard-light' then
              BlendOperation[idx] := boHardLight else
            if opstr = 'svg:difference' then
              BlendOperation[idx] := boLinearDifference else
            if (opstr = 'krita:inverse-subtract') or (opstr = 'krita:linear-burn') then
              BlendOperation[idx] := boLinearSubtractInverse else
            if opstr = 'krita:subtract' then
              BlendOperation[idx] := boLinearSubtract else
            if (opstr = 'svg:difference') or
              (opstr = 'krita:equivalence') then
              BlendOperation[idx] := boLinearDifference else
            if (opstr = 'svg:exclusion') or
              (opstr = 'krita:exclusion') then
              BlendOperation[idx] := boLinearExclusion else
            if opstr = 'krita:divide' then
              BlendOperation[idx] := boDivide else
            if opstr = 'bgra:soft-light' then
              BlendOperation[idx] := boSoftLight else
            if opstr = 'bgra:nice-glow' then
              BlendOperation[idx] := boNiceGlow else
            if opstr = 'bgra:glow' then
              BlendOperation[idx] := boGlow else
            if opstr = 'bgra:reflect' then
              BlendOperation[idx] := boReflect else
            if opstr = 'bgra:negation' then
              BlendOperation[idx] := boLinearNegation else
            if (opstr = 'bgra:xor') or (opstr = 'xor') then
              BlendOperation[idx] := boXor else
            begin
              //messagedlg('Unknown blend operation : ' + attr.NodeValue,mtInformation,[mbOk],0);
              BlendOperation[idx] := boTransparent;
            end;
          end;
        end;
        LayerOffset[idx] := point(x,y);
        if (gammastr = 'yes') or (gammastr = 'on') then
        begin
          case BlendOperation[idx] of
            boLinearAdd: BlendOperation[idx] := boAdditive;
            boOverlay: BlendOperation[idx] := boDarkOverlay;
            boLinearDifference: BlendOperation[idx] := boDifference;
            boLinearExclusion: BlendOperation[idx] := boExclusion;
            boLinearSubtract: BlendOperation[idx] := boSubtract;
            boLinearSubtractInverse: BlendOperation[idx] := boSubtractInverse;
            boLinearNegation: BlendOperation[idx] := boNegation;
          end;
        end else
        if (gammastr = 'no') or (gammastr = 'off') then
          if BlendOperation[idx] = boTransparent then
            BlendOperation[idx] := boLinearBlend; //explicit linear blending
      end;
    end;
  end;

end;

procedure TBGRAOpenRasterDocument.PrepareZipToSave;
var i: integer;
    imageNode,stackNode,layerNode: TDOMElement;
    layerFilename,strval: string;
    stackStream: TMemoryStream;
begin
  ClearFiles;
  MimeType := OpenRasterMimeType;
  FStackXML := TXMLDocument.Create;
  imageNode := TDOMElement(StackXML.CreateElement('image'));
  StackXML.AppendChild(imageNode);
  imageNode.SetAttribute('w',inttostr(Width));
  imageNode.SetAttribute('h',inttostr(Height));
  if LinearBlend then
    imageNode.SetAttribute('gamma-correction','no')
  else
    imageNode.SetAttribute('gamma-correction','yes');

  stackNode := TDOMElement(StackXML.CreateElement('stack'));
  imageNode.AppendChild(stackNode);
  SetMemoryStreamAsString('stack.xml',''); //to put it before image data

  CopyThumbnailToMemoryStream(256,256);

  for i := NbLayers-1 downto 0 do
  begin
    layerFilename := 'data/layer'+inttostr(i)+'.png';
    if CopyLayerToMemoryStream(i, layerFilename) then
    begin
      layerNode := StackXML.CreateElement('layer');
      stackNode.AppendChild(layerNode);
      layerNode.SetAttribute('name', UTF8Decode(LayerName[i]));
      str(LayerOpacity[i]/255:0:3,strval);
      layerNode.SetAttribute('opacity',strval);
      layerNode.SetAttribute('src',layerFilename);
      if LayerVisible[i] then
        layerNode.SetAttribute('visibility','visible')
      else
        layerNode.SetAttribute('visibility','hidden');
      layerNode.SetAttribute('x',inttostr(LayerOffset[i].x));
      layerNode.SetAttribute('y',inttostr(LayerOffset[i].y));
      strval := '';
      case BlendOperation[i] of
        boLighten: strval := 'svg:lighten';
        boScreen: strval := 'svg:screen';
        boAdditive, boLinearAdd: strval := 'svg:add';
        boColorDodge: strval := 'svg:color-dodge';
        boColorBurn : strval := 'svg:color-burn';
        boDarken: strval := 'svg:darken';
        boMultiply: strval := 'svg:multiply';
        boOverlay, boDarkOverlay: strval := 'svg:overlay';
        boSoftLight: strval := 'bgra:soft-light';
        boHardLight: strval := 'svg:hard-light';
        boDifference,boLinearDifference: strval := 'svg:difference';
        boLinearSubtractInverse, boSubtractInverse: strval := 'krita:inverse_subtract';
        boLinearSubtract, boSubtract: strval := 'krita:subtract';
        boExclusion, boLinearExclusion: strval := 'svg:exclusion';
        boDivide: strval := 'krita:divide';
        boNiceGlow: strval := 'bgra:nice-glow';
        boGlow: strval := 'bgra:glow';
        boReflect: strval := 'bgra:reflect';
        boLinearNegation,boNegation: strval := 'bgra:negation';
        boXor: strval := 'bgra:xor';
        boSvgSoftLight: strval := 'svg:soft-light';
        else strval := 'svg:src-over';
      end;
      layerNode.SetAttribute('composite-op',strval);
      if BlendOperation[i] <> boTransparent then //in 'transparent' case, linear blending depends on general setting
      begin
        if BlendOperation[i] in[boAdditive,boDarkOverlay,boDifference,boSubtractInverse,
             boSubtract,boExclusion,boNegation] then
          strval := 'yes' else strval := 'no';
        layerNode.SetAttribute('gamma-correction',strval);
      end;
    end;
  end;
  StackStream := TMemoryStream.Create;
  WriteXMLFile(StackXML, StackStream);
  SetMemoryStream('stack.xml',StackStream);
end;

procedure TBGRAOpenRasterDocument.LoadFromFile(const filenameUTF8: string);
var AStream: TFileStreamUTF8;
begin
  AStream := TFileStreamUTF8.Create(filenameUTF8,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TBGRAOpenRasterDocument.SaveToFile(const filenameUTF8: string);
begin
  PrepareZipToSave;
  ZipToFile(filenameUTF8);
end;

procedure TBGRAOpenRasterDocument.SaveToStream(AStream: TStream);
begin
  PrepareZipToSave;
  ZipToStream(AStream);
end;

function TBGRAOpenRasterDocument.GetMimeType: string;
begin
  if length(FFiles)=0 then
    result := OpenRasterMimeType
   else
    result := GetMemoryStreamAsString('mimetype');
end;

constructor TBGRAOpenRasterDocument.Create;
begin
  inherited Create;
  RegisterOpenRasterFormat;
end;

constructor TBGRAOpenRasterDocument.Create(AWidth, AHeight: integer);
begin
  inherited Create(AWidth, AHeight);
  RegisterOpenRasterFormat;
end;

function TBGRAOpenRasterDocument.AddLayerFromMemoryStream(ALayerFilename: string): integer;
var stream: TMemoryStream;
  bmp: TBGRABitmap;
  png: TFPReaderPNG;
begin
  stream := GetMemoryStream(ALayerFilename);
  if stream = nil then raise Exception.Create('Layer not found');

  png := TFPReaderPNG.Create;
  bmp := TBGRABitmap.Create;
  try
    bmp.LoadFromStream(stream,png);
  except
    on ex: exception do
    begin
      png.Free;
      bmp.Free;
      raise exception.Create('Layer format error');
    end;
  end;
  png.Free;

  result := AddOwnedLayer(bmp);
  LayerName[result] := ExtractFileName(ALayerFilename);
end;

function TBGRAOpenRasterDocument.CopyLayerToMemoryStream(ALayerIndex: integer;
  ALayerFilename: string): boolean;
var
  bmp: TBGRABitmap;
  mustFreeBmp: boolean;
  p: PBGRAPixel;
  n: integer;
begin
  result := false;
  bmp := LayerBitmap[ALayerIndex];
  if bmp <> nil then mustFreeBmp := false
  else
  begin
    bmp := GetLayerBitmapCopy(ALayerIndex);
    if bmp = nil then exit;
    mustFreeBmp:= true;
  end;
  if bmp.HasTransparentPixels then
  begin
    //avoid png bug with black color
    if not mustFreeBmp then
    begin
      bmp := bmp.Duplicate as TBGRABitmap;
      mustFreeBmp := true;
    end;
    p := bmp.data;
    for n := bmp.NbPixels-1 downto 0 do
    begin
      if (p^.alpha <> 0) and (p^.red = 0) and (p^.green = 0) and (p^.blue = 0) then
        p^.blue := 1;
      inc(p);
    end;
  end;

  result := CopyBitmapToMemoryStream(bmp,ALayerFilename);
  if mustFreeBmp then bmp.Free;
end;

function TBGRAOpenRasterDocument.CopyBitmapToMemoryStream(ABitmap: TBGRABitmap;
  AFilename: string): boolean;
var
  memStream: TMemoryStream;
begin
  result := false;
  memstream := TMemoryStream.Create;
  try
    ABitmap.SaveToStreamAsPng(memStream);
    SetMemoryStream(AFilename,memstream);
    result := true;
  except
    on ex: Exception do
    begin
      memStream.Free;
    end;
  end;
end;

procedure TBGRAOpenRasterDocument.SetMemoryStreamAsString(AFilename: string;
  AContent: string);
var strstream: TStringStream;
  memstream: TMemoryStream;
begin
  strstream:= TStringStream.Create(AContent);
  memstream := TMemoryStream.Create;
  strstream.Position := 0;
  memstream.CopyFrom(strstream, strstream.Size);
  strstream.Free;
  SetMemoryStream(AFilename, memstream);
end;

function TBGRAOpenRasterDocument.GetMemoryStreamAsString(AFilename: string): string;
var stream: TMemoryStream;
  str: TStringStream;
begin
  stream := GetMemoryStream(AFilename);
  str := TStringStream.Create('');
  str.CopyFrom(stream,stream.Size);
  result := str.DataString;
  str.Free;
end;

procedure TBGRAOpenRasterDocument.UnzipFromStream(AStream: TStream);
var unzip: TUnZipper;
begin
  Clear;
  unzip := TUnZipper.Create;
  try
    unzip.OnCreateStream := @ZipOnCreateStream;
    unzip.OnDoneStream := @ZipOnDoneStream;
    unzip.OnOpenInputStream := @ZipOnOpenInputStream;
    unzip.OnCloseInputStream := @ZipOnCloseInputStream;
    FZipInputStream := AStream;
    unzip.UnZipAllFiles;
  finally
    FZipInputStream := nil;
    unzip.Free;
  end;
end;

procedure TBGRAOpenRasterDocument.UnzipFromFile(AFilenameUTF8: string);
var unzip: TUnZipper;
begin
  Clear;
  unzip := TUnZipper.Create;
  try
    unzip.FileName := Utf8ToAnsi(AFilenameUTF8);
    unzip.OnCreateStream := @ZipOnCreateStream;
    unzip.OnDoneStream := @ZipOnDoneStream;
    unzip.UnZipAllFiles;
  finally
    unzip.Free;
  end;
end;

procedure TBGRAOpenRasterDocument.ZipToFile(AFilenameUTF8: string);
var
  stream: TFileStreamUTF8;
begin
  stream := TFileStreamUTF8.Create(AFilenameUTF8, fmCreate);
  try
    ZipToStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TBGRAOpenRasterDocument.ZipToStream(AStream: TStream);
var zip: TZipper;
  i: integer;
begin
  zip := TZipper.Create;
  try
    for i := 0 to high(FFiles) do
    begin
      FFiles[i].Stream.Position:= 0;
      zip.Entries.AddFileEntry(FFiles[i].Stream,FFiles[i].Filename).CompressionLevel := clnone;
    end;
    zip.SaveToStream(AStream);
  finally
    zip.Free;
  end;
end;

procedure TBGRAOpenRasterDocument.CopyThumbnailToMemoryStream(AMaxWidth,AMaxHeight: integer);
var thumbnail: TBGRABitmap;
  w,h: integer;
begin
  if (Width = 0) or (Height = 0) then exit;
  thumbnail := ComputeFlatImage;
  CopyBitmapToMemoryStream(thumbnail,'mergedimage.png');
  if (thumbnail.Width > AMaxWidth) or
   (thumbnail.Height > AMaxHeight) then
  begin
    if thumbnail.Width > AMaxWidth then
    begin
      w := AMaxWidth;
      h := round(thumbnail.Height* (w/thumbnail.Width));
    end else
    begin
      w := thumbnail.Width;
      h := thumbnail.Height;
    end;
    if h > AMaxHeight then
    begin
      h := AMaxHeight;
      w := round(thumbnail.Width* (h/thumbnail.Height));
    end;
    BGRAReplace(thumbnail, thumbnail.Resample(w,h));
  end;
  CopyBitmapToMemoryStream(thumbnail,'Thumbnails\thumbnail.png');
  thumbnail.Free;
end;

procedure TBGRAOpenRasterDocument.Clear;
begin
  ClearFiles;
  inherited Clear;
end;

function TBGRAOpenRasterDocument.CheckMimeType(AStream: TStream): boolean;
var unzip: TUnzipperStreamUtf8;
  mimeTypeFound: string;
  oldPos: int64;
begin
  result := false;
  unzip := TUnzipperStreamUtf8.Create;
  oldPos := AStream.Position;
  try
    unzip.InputStream := AStream;
    mimeTypeFound := unzip.UnzipFileToString('mimetype');
    if mimeTypeFound = OpenRasterMimeType then result := true;
  except
  end;
  unzip.Free;
  astream.Position:= OldPos;
end;

procedure TBGRAOpenRasterDocument.LoadFromStream(AStream: TStream);
begin
  OnLayeredBitmapLoadFromStreamStart;
  try
    UnzipFromStream(AStream);
    AnalyzeZip;
  finally
    OnLayeredBitmapLoaded;
  end;
end;

procedure TBGRAOpenRasterDocument.SetMimeType(AValue: string);
begin
  SetMemoryStreamAsString('mimetype',AValue);
end;

procedure TBGRAOpenRasterDocument.ZipOnCreateStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
var MemStream: TMemoryStream;
begin
  MemStream := TMemoryStream.Create;
  SetMemoryStream(AItem.ArchiveFileName, MemStream);
  AStream := MemStream;
end;

{$hints off}
procedure TBGRAOpenRasterDocument.ZipOnDoneStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  //do nothing, files stay in memory
end;
{$hints on}

procedure TBGRAOpenRasterDocument.ZipOnOpenInputStream(Sender: TObject;
  var AStream: TStream);
begin
  AStream := FZipInputStream;
end;

procedure TBGRAOpenRasterDocument.ZipOnCloseInputStream(Sender: TObject;
  var AStream: TStream);
begin
  AStream := nil; //avoid freeing
end;

procedure TBGRAOpenRasterDocument.ClearFiles;
var i: integer;
begin
  for i := 0 to high(FFiles) do
    ffiles[i].Stream.Free;
  FFiles := nil;
  FreeAndNil(FStackXML);
end;

function TBGRAOpenRasterDocument.GetMemoryStream(AFilename: string): TMemoryStream;
var i: integer;
begin
  for i := 0 to high(FFiles) do
    if ffiles[i].Filename = AFilename then
    begin
      result := FFiles[i].Stream;
      result.Position:= 0;
      exit;
    end;
  result := nil;
end;

procedure TBGRAOpenRasterDocument.SetMemoryStream(AFilename: string;
  AStream: TMemoryStream);
var i: integer;
begin
  for i := 0 to high(FFiles) do
    if ffiles[i].Filename = AFilename then
    begin
      FreeAndNil(FFiles[i].Stream);
      FFiles[i].Stream := AStream;
      exit;
    end;
  setlength(FFiles, length(FFiles)+1);
  FFiles[high(FFiles)].Filename := AFilename;
  FFiles[high(FFiles)].Stream := AStream;
end;

var AlreadyRegistered: boolean;

procedure RegisterOpenRasterFormat;
begin
  if AlreadyRegistered then exit;
  ImageHandlers.RegisterImageReader ('OpenRaster', 'ora', TFPReaderOpenRaster);
  RegisterLayeredBitmapReader('ora', TBGRAOpenRasterDocument);
  RegisterLayeredBitmapWriter('ora', TBGRAOpenRasterDocument);
  //TPicture.RegisterFileFormat('ora', 'OpenRaster', TBGRAOpenRasterDocument);
  DefaultBGRAImageReader[ifOpenRaster] := TFPReaderOpenRaster;
  DefaultBGRAImageWriter[ifOpenRaster] := TFPWriterOpenRaster;
  AlreadyRegistered:= True;
end;

end.

