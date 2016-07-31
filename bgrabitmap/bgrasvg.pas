unit BGRASVG;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, laz2_DOM, BGRAUnits, BGRASVGShapes,
  BGRACanvas2D;

type
  TSVGViewBox = record
    min, size: TPointF;
  end;
  TSVGSize = record
    width, height: TFloatWithCSSUnit;
  end;

  { TSVGUnits }

  TSVGUnits = class(TCSSUnitConverter)
  private
    function GetCustomDpi: TPointF;
    procedure Recompute;
  protected
    FSvg: TDOMElement;
    FViewBox: TSVGViewBox;
    FViewSize: TSVGSize;
    FDefaultUnitHeight, FDefaultUnitWidth: TFloatWithCSSUnit;
    FDefaultDpi: PSingle;
    FUseDefaultDPI: boolean;
    FDpiScaleX,FDpiScaleY: single;
    function GetDefaultUnitHeight: TFloatWithCSSUnit; override;
    function GetDefaultUnitWidth: TFloatWithCSSUnit; override;
    function GetDpiX: single; override;
    function GetDpiY: single; override;
    function GetCustomDpiX: single;
    function GetCustomDpiY: single;
    function GetCustomOrigin: TPointF;
    procedure SetCustomOrigin(AValue: TPointF);
    procedure SetViewBox(AValue: TSVGViewBox);
    procedure SetCustomDpi(ADpi: TPointF);
    function GetDpiScaleX: single; override;
    function GetDpiScaleY: single; override;
    function GetDPIScaled: boolean; override;
  public
    procedure SetDefaultDpiAndOrigin;
    constructor Create(ASvg: TDOMElement; ADefaultDpi: PSingle);
    property ViewBox: TSVGViewBox read FViewBox write SetViewBox;
    property CustomOrigin: TPointF read GetCustomOrigin write SetCustomOrigin;
    property CustomDpiX: single read GetCustomDpiX;
    property CustomDpiY: single read GetCustomDpiY;
    property CustomDpi: TPointF read GetCustomDpi write SetCustomDpi;
  end;

  { TBGRASVG }

  TBGRASVG = class
  private
    function GetAttribute(AName: string): string;
    function GetCustomDpi: TPointF;
    function GetHeight: TFloatWithCSSUnit;
    function GetHeightAsCm: single;
    function GetHeightAsInch: single;
    function GetPreserveAspectRatio: string;
    function GetViewBox: TSVGViewBox;
    function GetViewBox(AUnit: TCSSUnit): TSVGViewBox;
    procedure GetViewBoxIndirect(AUnit: TCSSUnit; out AViewBox: TSVGViewBox);
    function GetWidth: TFloatWithCSSUnit;
    function GetWidthAsCm: single;
    function GetWidthAsInch: single;
    function GetZoomable: boolean;
    procedure SetAttribute(AName: string; AValue: string);
    procedure SetCustomDpi(AValue: TPointF);
    procedure SetDefaultDpi(AValue: single);
    procedure SetHeight(AValue: TFloatWithCSSUnit);
    procedure SetHeightAsCm(AValue: single);
    procedure SetHeightAsInch(AValue: single);
    procedure SetPreserveAspectRatio(AValue: string);
    procedure SetViewBox(AValue: TSVGViewBox);
    procedure SetWidth(AValue: TFloatWithCSSUnit);
    procedure SetWidthAsCm(AValue: single);
    procedure SetWidthAsInch(AValue: single);
    procedure SetZoomable(AValue: boolean);
  protected
    FXml: TXMLDocument;
    FRoot: TDOMElement;
    FUnits: TSVGUnits;
    FDefaultDpi: single;
    FContent: TSVGContent;
    procedure Init(ACreateEmpty: boolean);
    function GetViewBoxAlignment(AHorizAlign: TAlignment; AVertAlign: TTextLayout): TPointF;
  public
    constructor Create; overload;
    constructor Create(AWidth,AHeight: single; AUnit: TCSSUnit); overload;
    constructor Create(AWidth,AHeight: single; AUnit: TCSSUnit; ACustomDPI: single); overload;
    constructor Create(AFilenameUTF8: string); overload;
    constructor Create(AStream: TStream); overload;
    destructor Destroy; override;
    procedure LoadFromFile(AFilenameUTF8: string);
    procedure LoadFromStream(AStream: TStream);
    procedure SaveToFile(AFilenameUTF8: string);
    procedure SaveToStream(AStream: TStream);
    procedure Draw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment; AVertAlign: TTextLayout; x,y: single; AUnit: TCSSUnit = cuPixel); overload;
    procedure Draw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment; AVertAlign: TTextLayout; x,y: single; destDpi: single); overload;
    procedure Draw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment; AVertAlign: TTextLayout; x,y: single; destDpi: TPointF); overload;
    procedure Draw(ACanvas2d: TBGRACanvas2D; x,y: single; AUnit: TCSSUnit = cuPixel); overload;
    procedure Draw(ACanvas2d: TBGRACanvas2D; x,y: single; destDpi: single); overload;
    procedure Draw(ACanvas2d: TBGRACanvas2D; x,y: single; destDpi: TPointF); overload;
    procedure StretchDraw(ACanvas2d: TBGRACanvas2D; x,y,w,h: single); overload;
    procedure StretchDraw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment; AVertAlign: TTextLayout; x,y,w,h: single); overload;
    property Units: TSVGUnits read FUnits;
    property Width: TFloatWithCSSUnit read GetWidth write SetWidth;
    property Height: TFloatWithCSSUnit read GetHeight write SetHeight;
    property WidthAsCm: single read GetWidthAsCm write SetWidthAsCm;
    property HeightAsCm: single read GetHeightAsCm write SetHeightAsCm;
    property WidthAsInch: single read GetWidthAsInch write SetWidthAsInch;
    property HeightAsInch: single read GetHeightAsInch write SetHeightAsInch;
    property Zoomable: boolean read GetZoomable write SetZoomable;
    property ViewBox: TSVGViewBox read GetViewBox write SetViewBox;
    property ViewBoxInUnit[AUnit: TCSSUnit]: TSVGViewBox read GetViewBox;
    property Attribute[AName: string]: string read GetAttribute write SetAttribute;
    property DefaultDpi: single read FDefaultDpi write SetDefaultDpi; //this is not saved in the SVG file
    property CustomDpi: TPointF read GetCustomDpi write SetCustomDpi;
    property Content: TSVGContent read FContent;
    property preserveAspectRatio: string read GetPreserveAspectRatio write SetPreserveAspectRatio;
  end;

implementation

uses laz2_XMLRead, laz2_XMLWrite, BGRAUTF8;

const SvgNamespace = 'http://www.w3.org/2000/svg';

function TSVGUnits.GetCustomDpiX: single;
var pixSize: single;
begin
  pixSize := Convert(FDefaultUnitWidth.value,FDefaultUnitWidth.CSSUnit,cuInch,FDefaultDpi^);
  if pixSize = 0 then
    result := 0
  else
    result := 1/pixSize;
end;

function TSVGUnits.GetCustomDpiY: single;
var pixSize: single;
begin
  pixSize := Convert(FDefaultUnitHeight.value,FDefaultUnitHeight.CSSUnit,cuInch,FDefaultDpi^);
  if pixSize = 0 then
    result := 0
  else
    result := 1/pixSize;
end;

function TSVGUnits.GetCustomOrigin: TPointF;
begin
  result := FViewBox.min;
end;

procedure TSVGUnits.SetCustomOrigin(AValue: TPointF);
var newViewBox: TSVGViewBox;
begin
  newViewBox := ViewBox;
  newViewBox.min := AValue;
  ViewBox := newViewBox;
end;

function TSVGUnits.GetCustomDpi: TPointF;
begin
  result := PointF(CustomDpiX,CustomDpiY);
end;

procedure TSVGUnits.Recompute;
var viewBoxStr: string;

  function parseNextFloat: single;
  var
    idxSpace,{%H-}errPos: integer;
  begin
    idxSpace:= pos(' ',viewBoxStr);
    if idxSpace <> 0 then
      val(copy(viewBoxStr,1,idxSpace-1),result,errPos)
    else
      result := 0;
    delete(viewBoxStr,1,idxSpace);
    while (viewBoxStr <> '') and (viewBoxStr[1] = ' ') do delete(viewBoxStr,1,1);
  end;

begin
  viewBoxStr := trim(FSvg.GetAttribute('viewBox'))+' ';
  FViewBox.min.x := parseNextFloat;
  FViewBox.min.y := parseNextFloat;
  FViewBox.size.x := parseNextFloat;
  FViewBox.size.y := parseNextFloat;

  FViewSize.width := parseValue(FSvg.GetAttribute('width'), FloatWithCSSUnit(FViewBox.size.x, cuPixel));
  if FViewSize.width.CSSUnit = cuCustom then FViewSize.width.CSSUnit := cuPixel;
  FViewSize.height := parseValue(FSvg.GetAttribute('height'), FloatWithCSSUnit(FViewBox.size.y, cuPixel));
  if FViewSize.height.CSSUnit = cuCustom then FViewSize.height.CSSUnit := cuPixel;

  if (FViewBox.size.x <= 0) and (FViewBox.size.y <= 0) then
    begin
      FDefaultUnitWidth.value:= 1/FDefaultDpi^;
      FDefaultUnitWidth.CSSUnit := cuInch;
      FDefaultUnitHeight.value:= 1/FDefaultDpi^;
      FDefaultUnitHeight.CSSUnit := cuInch;
      FUseDefaultDPI := true;
      FDpiScaleX := 1;
      FDpiScaleY := 1;
      FViewBox.min := PointF(0,0);
      FViewBox.size.x := ConvertWidth(FViewSize.width,cuCustom).value;
      FViewBox.size.y := ConvertHeight(FViewSize.height,cuCustom).value;
    end else
    begin
      FDefaultUnitWidth.value := FViewSize.width.value/FViewBox.size.x;
      FDefaultUnitWidth.CSSUnit := FViewSize.width.CSSUnit;
      if FDefaultUnitWidth.CSSUnit = cuCustom then
        begin
          FDefaultUnitWidth.value /= FDefaultDpi^;
          FDefaultUnitWidth.CSSUnit := cuInch;
        end;
      FDefaultUnitHeight.value := FViewSize.height.value/FViewBox.size.y;
      FDefaultUnitHeight.CSSUnit := FViewSize.height.CSSUnit;
      if FDefaultUnitHeight.CSSUnit = cuCustom then
        begin
          FDefaultUnitHeight.value /= FDefaultDpi^;
          FDefaultUnitHeight.CSSUnit := cuInch;
        end;
      FUseDefaultDPI := false;
      FDpiScaleX := CustomDpiX/DpiX;
      FDpiScaleY := CustomDpiY/DpiY;
    end;
end;

procedure TSVGUnits.SetCustomDpi(ADpi: TPointF);
var vb: TSVGViewBox;
  vs: TSVGSize;
begin
  vb := ViewBox;
  vs := FViewSize;
  if (vs.width.value > 0) and (vs.height.value > 0) then
    begin
      vb.size.x := ConvertWidth(vs.width,cuInch).value*ADpi.X;
      vb.size.y := ConvertHeight(vs.height,cuInch).value*ADpi.Y;
    end
  else
    raise exception.Create('The size of the view port is not properly defined. Use Width and Height properties of TBGRASVG object.');
  viewBox := vb;
end;

function TSVGUnits.GetDpiScaleX: single;
begin
  Result:=FDpiScaleX;
end;

function TSVGUnits.GetDpiScaleY: single;
begin
  Result:=FDpiScaleY;
end;

function TSVGUnits.GetDPIScaled: boolean;
begin
  Result:= not FUseDefaultDPI;
end;

procedure TSVGUnits.SetDefaultDpiAndOrigin;
begin
  FSvg.RemoveAttribute('viewBox');
  Recompute;
end;

procedure TSVGUnits.SetViewBox(AValue: TSVGViewBox);
begin
  FSvg.SetAttribute('viewBox', formatValue(AValue.min.x)+' '+
    formatValue(AValue.min.y)+' '+
    formatValue(AValue.size.x)+' '+
    formatValue(AValue.size.y));
  Recompute;
end;

function TSVGUnits.GetDefaultUnitHeight: TFloatWithCSSUnit;
begin
  result := FDefaultUnitHeight;
end;

function TSVGUnits.GetDefaultUnitWidth: TFloatWithCSSUnit;
begin
  result := FDefaultUnitWidth;
end;

function TSVGUnits.GetDpiX: single;
begin
  result := FDefaultDpi^;
end;

function TSVGUnits.GetDpiY: single;
begin
  result := FDefaultDpi^;
end;

constructor TSVGUnits.Create(ASvg: TDOMElement; ADefaultDpi: PSingle);
begin
  FSvg := ASvg;
  FDefaultDpi := ADefaultDpi;
  Recompute;
end;

{ TBGRASVG }

function TBGRASVG.GetAttribute(AName: string): string;
begin
  result := FRoot.GetAttribute(AName);
end;

function TBGRASVG.GetCustomDpi: TPointF;
begin
  result := FUnits.CustomDpi;
end;

function TBGRASVG.GetHeight: TFloatWithCSSUnit;
begin
  result := TCSSUnitConverter.parseValue(Attribute['height'],FloatWithCSSUnit(0,cuCustom));
end;

function TBGRASVG.GetHeightAsCm: single;
begin
  result := FUnits.ConvertHeight(Height,cuCentimeter).value;
end;

function TBGRASVG.GetHeightAsInch: single;
begin
  result := FUnits.ConvertHeight(Height,cuInch).value;
end;

function TBGRASVG.GetPreserveAspectRatio: string;
begin
  result := Attribute['preserveAspectRatio'];
end;

function TBGRASVG.GetViewBox: TSVGViewBox;
begin
  result := FUnits.ViewBox;
end;

function TBGRASVG.GetViewBox(AUnit: TCSSUnit): TSVGViewBox;
begin
  GetViewBoxIndirect(AUnit,result);
end;

procedure TBGRASVG.GetViewBoxIndirect(AUnit: TCSSUnit; out AViewBox: TSVGViewBox);
begin
  with FUnits.ViewBox do
  begin
    AViewBox.min := FUnits.ConvertCoord(min,cuCustom,AUnit);
    AViewBox.size := FUnits.ConvertCoord(size,cuCustom,AUnit);
  end;
end;

function TBGRASVG.GetWidth: TFloatWithCSSUnit;
begin
  result := TCSSUnitConverter.parseValue(Attribute['width'],FloatWithCSSUnit(0,cuCustom));
end;

function TBGRASVG.GetWidthAsCm: single;
begin
  result := FUnits.ConvertWidth(Width,cuCentimeter).value;
end;

function TBGRASVG.GetWidthAsInch: single;
begin
  result := FUnits.ConvertWidth(Width,cuInch).value;
end;

function TBGRASVG.GetZoomable: boolean;
begin
  result := trim(Attribute['zoomAndPan'])<>'disable';
end;

procedure TBGRASVG.SetAttribute(AName: string; AValue: string);
begin
  AName := trim(AName);
  if compareText(AName,'viewBox')= 0 then AName := 'viewBox' else
  if compareText(AName,'width')=0 then AName := 'width' else
  if compareText(AName,'height')=0 then AName := 'height';
  FRoot.SetAttribute(AName,AValue);
  if (AName = 'viewBox') or (AName = 'width') or (AName = 'height') then
    FUnits.Recompute;
end;

procedure TBGRASVG.SetCustomDpi(AValue: TPointF);
begin
  FUnits.CustomDpi := AValue;
  if AValue.x <> AValue.y then
    preserveAspectRatio := 'none';
end;

procedure TBGRASVG.SetDefaultDpi(AValue: single);
begin
  if FDefaultDpi=AValue then Exit;
  FDefaultDpi:=AValue;
  Units.Recompute;
end;

procedure TBGRASVG.SetHeight(AValue: TFloatWithCSSUnit);
begin
  Attribute['height'] := TCSSUnitConverter.formatValue(AValue);
end;

procedure TBGRASVG.SetHeightAsCm(AValue: single);
begin
  Height := FloatWithCSSUnit(AValue,cuCentimeter);
end;

procedure TBGRASVG.SetHeightAsInch(AValue: single);
begin
  Height := FloatWithCSSUnit(AValue,cuInch);
end;

procedure TBGRASVG.SetPreserveAspectRatio(AValue: string);
begin
  Attribute['preserveAspectRatio'] := AValue;
end;

{$PUSH}{$OPTIMIZATION OFF} //avoids Internal error 2012090607
procedure TBGRASVG.SetViewBox(AValue: TSVGViewBox);
begin
  FUnits.ViewBox := AValue;
end;
{$POP}

procedure TBGRASVG.SetWidth(AValue: TFloatWithCSSUnit);
begin
  Attribute['width'] := TCSSUnitConverter.formatValue(AValue);
end;

procedure TBGRASVG.SetWidthAsCm(AValue: single);
begin
  Width := FloatWithCSSUnit(AValue,cuCentimeter);
end;

procedure TBGRASVG.SetWidthAsInch(AValue: single);
begin
  Width := FloatWithCSSUnit(AValue,cuInch);
end;

procedure TBGRASVG.SetZoomable(AValue: boolean);
begin
  if AValue then
    Attribute['zoomAndPan'] := 'magnify'
  else
    Attribute['zoomAndPan'] := 'disable';
end;

procedure TBGRASVG.Init(ACreateEmpty: boolean);
begin
  FDefaultDpi := 96; //web browser default
  if ACreateEmpty then
  begin
    FXml := TXMLDocument.Create;
    FRoot := FXml.CreateElement('svg');
    FUnits := TSVGUnits.Create(FRoot,@FDefaultDpi);
    FContent := TSVGContent.Create(FXml,FRoot,FUnits);
    FXml.AppendChild(FRoot);
  end;
end;

function TBGRASVG.GetViewBoxAlignment(AHorizAlign: TAlignment;
  AVertAlign: TTextLayout): TPointF;
var vb: TSVGViewBox;
begin
  GetViewBoxIndirect(cuPixel, vb);
  with vb do
  begin
    case AHorizAlign of
      taCenter: result.x := -(min.x+size.x*0.5);
      taRightJustify: result.x := -(min.x+size.x);
    else
      {taLeftJustify:} result.x := -min.x;
    end;
    case AVertAlign of
      tlCenter: result.y := -(min.y+size.y*0.5);
      tlBottom: result.y := -(min.y+size.y);
    else
      {tlTop:} result.y := -min.y;
    end;
  end;
end;

constructor TBGRASVG.Create;
begin
  Init(True);
end;

constructor TBGRASVG.Create(AWidth, AHeight: single; AUnit: TCSSUnit);
begin
  Init(True);
  Width := FloatWithCSSUnit(AWidth,AUnit);
  Height := FloatWithCSSUnit(AHeight,AUnit);
  if AUnit in[cuInch,cuPoint,cuPica] then
    CustomDpi := PointF(288,288)
  else if AUnit in[cuCentimeter,cuMillimeter] then
    CustomDpi := PointF(254,254);
end;

constructor TBGRASVG.Create(AWidth,AHeight: single; AUnit: TCSSUnit; ACustomDPI: single);
begin
  Init(True);
  Width := FloatWithCSSUnit(AWidth,AUnit);
  Height := FloatWithCSSUnit(AHeight,AUnit);
  CustomDpi := PointF(ACustomDPI,ACustomDPI);
end;

constructor TBGRASVG.Create(AFilenameUTF8: string);
begin
  Init(False);
  LoadFromFile(AFilenameUTF8);
end;

constructor TBGRASVG.Create(AStream: TStream);
begin
  Init(False);
  LoadFromStream(AStream);
end;

destructor TBGRASVG.Destroy;
begin
  FreeAndNil(FContent);
  FreeAndNil(FUnits);
  FRoot:= nil;
  FreeAndNil(FXml);
  inherited Destroy;
end;

procedure TBGRASVG.LoadFromFile(AFilenameUTF8: string);
var stream: TStream;
begin
  stream := TFileStreamUTF8.Create(AFilenameUTF8,fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(stream);
  finally
    stream.Free;
  end;
end;

procedure TBGRASVG.LoadFromStream(AStream: TStream);
var xml: TXMLDocument;
  root: TDOMNode;
  byteOrderMark: packed array[1..3] of byte;
  startPos: int64;
begin
  //skip utf8 byte order mark
  startPos:= AStream.Position;
  if AStream.Read({%H-}byteOrderMark,sizeof(byteOrderMark)) = 3 then
  begin
    if (byteOrderMark[1] = $ef) and (byteOrderMark[2] = $bb) and (byteOrderMark[3] = $bf) then
      startPos += 3;
  end;
  AStream.Position:= startPos;
  ReadXMLFile(xml,AStream);
  root := xml.FirstChild;
  while (root <> nil) and not (root is TDOMElement) do root := root.NextSibling;
  if root = nil then
  begin
    xml.Free;
    raise exception.Create('Root node not found');
  end;
  FreeAndNil(FContent);
  FreeAndNil(FUnits);
  FreeAndNil(FXml);
  FXml := xml;
  FRoot := root as TDOMElement;
  FUnits := TSVGUnits.Create(FRoot,@FDefaultDpi);
  FContent := TSVGContent.Create(FXml,FRoot,FUnits);
end;

procedure TBGRASVG.SaveToFile(AFilenameUTF8: string);
var stream: TFileStreamUTF8;
begin
  stream := TFileStreamUTF8.Create(AFilenameUTF8,fmCreate);
  try
    SaveToStream(stream);
  finally
    stream.free;
  end;
end;

procedure TBGRASVG.SaveToStream(AStream: TStream);
begin
  if Attribute['xmlns'] = '' then Attribute['xmlns'] := SvgNamespace;
  WriteXMLFile(FXml, AStream);
end;

procedure TBGRASVG.Draw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment;
  AVertAlign: TTextLayout; x, y: single; AUnit: TCSSUnit);
var prevMatrix: TAffineMatrix;
begin
  prevMatrix := ACanvas2d.matrix;
  ACanvas2d.translate(x,y);
  with GetViewBoxAlignment(AHorizAlign,AVertAlign) do ACanvas2d.translate(x,y);
  Draw(ACanvas2d, 0,0, AUnit);
  ACanvas2d.matrix := prevMatrix;
end;

procedure TBGRASVG.Draw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment;
  AVertAlign: TTextLayout; x, y: single; destDpi: single);
begin
  Draw(ACanvas2d, AHorizAlign,AVertAlign, x,y, PointF(destDpi,destDpi));
end;

procedure TBGRASVG.Draw(ACanvas2d: TBGRACanvas2D; AHorizAlign: TAlignment;
  AVertAlign: TTextLayout; x, y: single; destDpi: TPointF);
begin
  ACanvas2d.save;
  ACanvas2d.translate(x,y);
  ACanvas2d.scale(destDpi.x/Units.DpiX,destDpi.y/Units.DpiY);
  ACanvas2d.strokeResetTransform;
  ACanvas2d.strokeScale(destDpi.x/Units.DpiX,destDpi.y/Units.DpiY);
  with GetViewBoxAlignment(AHorizAlign,AVertAlign) do ACanvas2d.translate(x,y);
  Draw(ACanvas2d, 0,0, cuPixel);
  ACanvas2d.restore;
end;

procedure TBGRASVG.Draw(ACanvas2d: TBGRACanvas2D; x, y: single; AUnit: TCSSUnit);
var prevLinearBlend: boolean;
begin
  prevLinearBlend:= ACanvas2d.linearBlend;
  acanvas2d.linearBlend := true;
  ACanvas2d.save;
  ACanvas2d.translate(x,y);
  Content.Draw(ACanvas2d,AUnit);
  ACanvas2d.restore;
  ACanvas2d.linearBlend := prevLinearBlend;
end;

procedure TBGRASVG.Draw(ACanvas2d: TBGRACanvas2D; x, y: single; destDpi: single);
begin
  Draw(ACanvas2d, x,y, PointF(destDpi,destDpi));
end;

procedure TBGRASVG.Draw(ACanvas2d: TBGRACanvas2D; x, y: single; destDpi: TPointF);
begin
  ACanvas2d.save;
  ACanvas2d.translate(x,y);
  ACanvas2d.scale(destDpi.x/Units.DpiX,destDpi.y/Units.DpiY);
  ACanvas2d.strokeResetTransform;
  ACanvas2d.strokeScale(destDpi.x/Units.DpiX,destDpi.y/Units.DpiY);
  Draw(ACanvas2d, 0,0, cuPixel);
  ACanvas2d.restore;
end;

procedure TBGRASVG.StretchDraw(ACanvas2d: TBGRACanvas2D; x, y, w, h: single);
var vb: TSVGViewBox;
begin
  ACanvas2d.save;
  ACanvas2d.translate(x,y);
  ACanvas2d.strokeResetTransform;
  GetViewBoxIndirect(cuPixel,vb);
  with vb do
  begin
    ACanvas2d.translate(-min.x,-min.y);
    if size.x <> 0 then
    begin
      ACanvas2d.scale(w/size.x,1);
      ACanvas2d.strokeScale(w/size.x,1);
    end;
    if size.y <> 0 then
    begin
      ACanvas2d.scale(1,h/size.y);
      ACanvas2d.strokeScale(1,h/size.y);
    end;
  end;
  Draw(ACanvas2d, 0,0);
  ACanvas2d.restore;
end;

procedure TBGRASVG.StretchDraw(ACanvas2d: TBGRACanvas2D;
  AHorizAlign: TAlignment; AVertAlign: TTextLayout; x, y, w, h: single);
var ratio,stretchRatio,zoom: single;
  vb: TSVGViewBox;
  sx,sy,sw,sh: single;
begin
  GetViewBoxIndirect(cuPixel,vb);
  if (h = 0) or (w = 0) or (vb.size.x = 0) or (vb.size.y = 0) then exit;
  ratio := vb.size.x/vb.size.y;
  stretchRatio := w/h;
  if ratio > stretchRatio then
    zoom := w / vb.size.x
  else
    zoom := h / vb.size.y;

  sx := x;
  sy := y;
  sw := vb.size.x*zoom;
  sh := vb.size.y*zoom;

  case AHorizAlign of
    taCenter: sx += (w - sw)/2;
    taRightJustify: sx += w - sw;
  end;
  case AVertAlign of
    tlCenter: sy += (h - sh)/2;
    tlBottom: sy += h - sh;
  end;
  StretchDraw(ACanvas2d, sx,sy,sw,sh);
end;

end.

