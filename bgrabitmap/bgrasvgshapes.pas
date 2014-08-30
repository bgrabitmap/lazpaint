unit BGRASVGShapes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRAUnits, laz2_DOM, BGRAPath, BGRABitmapTypes,
  BGRACanvas2D, BGRASVGType;

type
  { TSVGLine }

  TSVGLine = class(TSVGElement)
    private
      function GetX1: TFloatWithCSSUnit;
      function GetX2: TFloatWithCSSUnit;
      function GetY1: TFloatWithCSSUnit;
      function GetY2: TFloatWithCSSUnit;
      procedure SetX1(AValue: TFloatWithCSSUnit);
      procedure SetX2(AValue: TFloatWithCSSUnit);
      procedure SetY1(AValue: TFloatWithCSSUnit);
      procedure SetY2(AValue: TFloatWithCSSUnit);
    protected
      procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
    public
      constructor Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter); override;
      property x1: TFloatWithCSSUnit read GetX1 write SetX1;
      property y1: TFloatWithCSSUnit read GetY1 write SetY1;
      property x2: TFloatWithCSSUnit read GetX2 write SetX2;
      property y2: TFloatWithCSSUnit read GetY2 write SetY2;
  end;

  { TSVGRectangle }

  TSVGRectangle = class(TSVGElement)
    private
      function GetX: TFloatWithCSSUnit;
      function GetY: TFloatWithCSSUnit;
      function GetWidth: TFloatWithCSSUnit;
      function GetHeight: TFloatWithCSSUnit;
      function GetRX: TFloatWithCSSUnit;
      function GetRY: TFloatWithCSSUnit;
      procedure SetX(AValue: TFloatWithCSSUnit);
      procedure SetY(AValue: TFloatWithCSSUnit);
      procedure SetWidth(AValue: TFloatWithCSSUnit);
      procedure SetHeight(AValue: TFloatWithCSSUnit);
      procedure SetRX(AValue: TFloatWithCSSUnit);
      procedure SetRY(AValue: TFloatWithCSSUnit);
    protected
      procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
    public
      constructor Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter); override;
      property x: TFloatWithCSSUnit read GetX write SetX;
      property y: TFloatWithCSSUnit read GetY write SetY;
      property width: TFloatWithCSSUnit read GetWidth write SetWidth;
      property height: TFloatWithCSSUnit read GetHeight write SetHeight;
      property rx: TFloatWithCSSUnit read GetRX write SetRX;
      property ry: TFloatWithCSSUnit read GetRY write SetRY;
  end;

  { TSVGCircle }

  TSVGCircle = class(TSVGElement)
    private
      function GetCX: TFloatWithCSSUnit;
      function GetCY: TFloatWithCSSUnit;
      function GetR: TFloatWithCSSUnit;
      procedure SetCX(AValue: TFloatWithCSSUnit);
      procedure SetCY(AValue: TFloatWithCSSUnit);
      procedure SetR(AValue: TFloatWithCSSUnit);
    protected
      procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
    public
      constructor Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter); override;
      property cx: TFloatWithCSSUnit read GetCX write SetCX;
      property cy: TFloatWithCSSUnit read GetCY write SetCY;
      property r: TFloatWithCSSUnit read GetR write SetR;
  end;

  { TSVGEllipse }

  TSVGEllipse = class(TSVGElement)
    private
      function GetCX: TFloatWithCSSUnit;
      function GetCY: TFloatWithCSSUnit;
      function GetRX: TFloatWithCSSUnit;
      function GetRY: TFloatWithCSSUnit;
      procedure SetCX(AValue: TFloatWithCSSUnit);
      procedure SetCY(AValue: TFloatWithCSSUnit);
      procedure SetRX(AValue: TFloatWithCSSUnit);
      procedure SetRY(AValue: TFloatWithCSSUnit);
    protected
      procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
    public
      constructor Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter); override;
      property cx: TFloatWithCSSUnit read GetCX write SetCX;
      property cy: TFloatWithCSSUnit read GetCY write SetCY;
      property rx: TFloatWithCSSUnit read GetRX write SetRX;
      property ry: TFloatWithCSSUnit read GetRY write SetRY;
  end;

  { TSVGPath }

  TSVGPath = class(TSVGElement)
    private
      FPath: TBGRAPath;
      function GetPath: TBGRAPath;
      function GetPathLength: TFloatWithCSSUnit;
      function GetData: string;
      procedure SetPathLength(AValue: TFloatWithCSSUnit);
      procedure SetData(AValue: string);
    protected
      function GetDOMElement: TDOMElement; override;
      procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
    public
      constructor Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter); override;
      constructor Create(ADocument: TXMLDocument; AElement: TDOMElement; AUnits: TCSSUnitConverter); override;
      destructor Destroy; override;
      property d: string read GetData write SetData;
      property path: TBGRAPath read GetPath;
      property pathLength: TFloatWithCSSUnit read GetPathLength write SetPathLength;
  end;

  { TSVGPolypoints }

  TSVGPolypoints = class(TSVGElement)
    private
      function GetClosed: boolean;
      function GetPoints: string;
      function GetPointsF: ArrayOfTPointF;
      procedure SetPoints(AValue: string);
      procedure SetPointsF(AValue: ArrayOfTPointF);
    protected
      procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
    public
      constructor Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter; AClosed: boolean); overload;
      destructor Destroy; override;
      property points: string read GetPoints write SetPoints;
      property pointsF: ArrayOfTPointF read GetPointsF write SetPointsF;
      property closed: boolean read GetClosed;
  end;

  { TSVGText }

  TSVGText = class(TSVGElement)
    private
      function GetFontBold: boolean;
      function GetFontFamily: string;
      function GetFontItalic: boolean;
      function GetFontSize: TFloatWithCSSUnit;
      function GetFontStyle: string;
      function GetFontWeight: string;
      function GetSimpleText: string;
      function GetTextDecoration: string;
      function GetX: TFloatWithCSSUnit;
      function GetY: TFloatWithCSSUnit;
      procedure SetFontBold(AValue: boolean);
      procedure SetFontFamily(AValue: string);
      procedure SetFontItalic(AValue: boolean);
      procedure SetFontSize(AValue: TFloatWithCSSUnit);
      procedure SetFontStyle(AValue: string);
      procedure SetFontWeight(AValue: string);
      procedure SetSimpleText(AValue: string);
      procedure SetTextDecoration(AValue: string);
      procedure SetX(AValue: TFloatWithCSSUnit);
      procedure SetY(AValue: TFloatWithCSSUnit);
    protected
      procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
    public
      constructor Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter); override;
      property x: TFloatWithCSSUnit read GetX write SetX;
      property y: TFloatWithCSSUnit read GetY write SetY;
      property SimpleText: string read GetSimpleText write SetSimpleText;
      property fontSize: TFloatWithCSSUnit read GetFontSize write SetFontSize;
      property fontFamily: string read GetFontFamily write SetFontFamily;
      property fontWeight: string read GetFontWeight write SetFontWeight;
      property fontStyle: string read GetFontStyle write SetFontStyle;
      property textDecoration: string read GetTextDecoration write SetTextDecoration;
      property fontBold: boolean read GetFontBold write SetFontBold;
      property fontItalic: boolean read GetFontItalic write SetFontItalic;
  end;

  TSVGContent = class;

  { TSVGGroup }

  TSVGGroup = class(TSVGElement)
  protected
    FContent: TSVGContent;
    procedure InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); override;
  public
    constructor Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter); override;
    constructor Create(ADocument: TXMLDocument; AElement: TDOMElement;
      AUnits: TCSSUnitConverter); override;
    destructor Destroy; override;
    property Content: TSVGContent read FContent;
  end;

  { TSVGContent }

  TSVGContent = class
    protected
      FDomElem: TDOMElement;
      FDoc: TXMLDocument;
      FElements: TList;
      FUnits: TCSSUnitConverter;
      procedure AppendElement(AElement: TSVGElement);
      procedure InsertElementBefore(AElement: TSVGElement; ASuccessor: TSVGElement);
      function GetElement(AIndex: integer): TSVGElement;
      function GetElementCount: integer;
      function GetUnits: TCSSUnitConverter;
    public
      constructor Create(ADocument: TXMLDocument; AElement: TDOMElement; AUnits: TCSSUnitConverter);
      destructor Destroy; override;
      procedure Draw(ACanvas2d: TBGRACanvas2D; x,y: single; AUnit: TCSSUnit); overload;
      procedure Draw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit); overload;
      function AppendLine(x1,y1,x2,y2: single; AUnit: TCSSUnit = cuCustom): TSVGLine; overload;
      function AppendLine(p1,p2: TPointF; AUnit: TCSSUnit = cuCustom): TSVGLine; overload;
      function AppendCircle(cx,cy,r: single; AUnit: TCSSUnit = cuCustom): TSVGCircle; overload;
      function AppendCircle(c: TPointF; r: single; AUnit: TCSSUnit = cuCustom): TSVGCircle; overload;
      function AppendEllipse(cx,cy,rx,ry: single; AUnit: TCSSUnit = cuCustom): TSVGEllipse; overload;
      function AppendEllipse(c,r: TPointF; AUnit: TCSSUnit = cuCustom): TSVGEllipse; overload;
      function AppendPath(data: string; AUnit: TCSSUnit = cuCustom): TSVGPath; overload;
      function AppendPath(path: TBGRAPath; AUnit: TCSSUnit = cuCustom): TSVGPath; overload;
      function AppendPolygon(const points: array of single; AUnit: TCSSUnit = cuCustom): TSVGPolypoints; overload;
      function AppendPolygon(const points: array of TPointF; AUnit: TCSSUnit = cuCustom): TSVGPolypoints; overload;
      function AppendRect(x,y,width,height: single; AUnit: TCSSUnit = cuCustom): TSVGRectangle; overload;
      function AppendRect(origin,size: TPointF; AUnit: TCSSUnit = cuCustom): TSVGRectangle; overload;
      function AppendText(x,y: single; AText: string; AUnit: TCSSUnit = cuCustom): TSVGText; overload;
      function AppendText(origin: TPointF; AText: string; AUnit: TCSSUnit = cuCustom): TSVGText; overload;
      function AppendRoundRect(x,y,width,height,rx,ry: single; AUnit: TCSSUnit = cuCustom): TSVGRectangle; overload;
      function AppendRoundRect(origin,size,radius: TPointF; AUnit: TCSSUnit = cuCustom): TSVGRectangle; overload;
      property ElementCount: integer read GetElementCount;
      property Element[AIndex: integer]: TSVGElement read GetElement;
      property Units: TCSSUnitConverter read GetUnits;
  end;

function GetSVGFactory(ATagName: string): TSVGFactory;
function CreateSVGElementFromNode(ADocument: TXMLDocument;
  AElement: TDOMElement; AUnits: TCSSUnitConverter): TSVGElement;

implementation

uses BGRATransform, Graphics;

function GetSVGFactory(ATagName: string): TSVGFactory;
var tag: string;
begin
  tag := LowerCase(ATagName);
  if tag='line' then
    result := TSVGLine else
  if tag='rect' then
    result := TSVGRectangle else
  if tag='circle' then
    result := TSVGCircle else
  if tag='ellipse' then
    result := TSVGEllipse else
  if tag='path' then
    result := TSVGPath else
  if (tag='polygon') or (tag='polyline') then
    result := TSVGPolypoints else
  if tag='text' then
    result := TSVGText else
  if tag='g' then
    result := TSVGGroup else
    result := TSVGElement;
end;

function CreateSVGElementFromNode(ADocument: TXMLDocument;
  AElement: TDOMElement; AUnits: TCSSUnitConverter): TSVGElement;
var
  factory: TSVGFactory;
begin
  factory := GetSVGFactory(AElement.TagName);
  result := factory.Create(ADocument,AElement,AUnits);
end;

{ TSVGText }

function TSVGText.GetFontBold: boolean;
var valueText: string;
begin
  valueText := trim(fontWeight);
  result := (valueText = 'bold') or (valueText = 'bolder') or
  (valueText = '600') or (valueText = '700') or (valueText = '800') or
   (valueText = '900');
end;

function TSVGText.GetFontFamily: string;
begin
  result := AttributeOrStyle['font-family'];
  if result = '' then result := 'Arial';
end;

function TSVGText.GetFontItalic: boolean;
var valueText: string;
begin
  valueText := trim(fontStyle);
  result := (valueText = 'oblique') or (valueText = 'italic');
end;

function TSVGText.GetFontSize: TFloatWithCSSUnit;
begin
  if AttributeOrStyle['font-size']='' then
    result := FloatWithCSSUnit(12,cuPoint)
  else
    result := VerticalAttributeOrStyleWithUnit['font-size'];
end;

function TSVGText.GetFontStyle: string;
begin
  result := AttributeOrStyle['font-style'];
  if result = '' then result := 'normal';
end;

function TSVGText.GetFontWeight: string;
begin
  result := AttributeOrStyle['font-weight'];
  if result = '' then result := 'normal';
end;

function TSVGText.GetSimpleText: string;
begin
  result := FDomElem.TextContent;
end;

function TSVGText.GetTextDecoration: string;
begin
  result := AttributeOrStyle['text-decoration'];
  if result='' then result := 'none';
end;

function TSVGText.GetX: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['x'];
end;

function TSVGText.GetY: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['y'];
end;

procedure TSVGText.SetFontBold(AValue: boolean);
begin
  if AValue then fontWeight:= 'bold' else fontWeight:= 'normal';
end;

procedure TSVGText.SetFontFamily(AValue: string);
begin
  Attribute['font-family'] := AValue;
  RemoveStyle('font-family');
end;

procedure TSVGText.SetFontItalic(AValue: boolean);
begin
  if AValue then fontStyle:= 'italic' else fontStyle:= 'normal';
end;

procedure TSVGText.SetFontSize(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['font-size'] := AValue;
end;

procedure TSVGText.SetFontStyle(AValue: string);
begin
  Attribute['font-style'] := AValue;
  RemoveStyle('font-style');
end;

procedure TSVGText.SetFontWeight(AValue: string);
begin
  Attribute['font-weight'] := AValue;
  RemoveStyle('font-weight');
end;

procedure TSVGText.SetSimpleText(AValue: string);
begin
  FDomElem.TextContent := AValue;
end;

procedure TSVGText.SetTextDecoration(AValue: string);
begin
  Attribute['text-decoration'] := AValue;
  RemoveStyle('text-decoration');
end;

procedure TSVGText.SetX(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['x'] := AValue;
end;

procedure TSVGText.SetY(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['y'] := AValue;
end;

procedure TSVGText.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
var fs:TFontStyles;
begin
  ACanvas2d.beginPath;
  ACanvas2d.fontEmHeight := Units.ConvertWidth(fontSize,AUnit).value;
  ACanvas2d.fontName := fontFamily;
  fs := [];
  if fontBold then fs += [fsBold];
  if fontItalic then fs += [fsItalic];
  ACanvas2d.fontStyle := fs;
  ACanvas2d.text(SimpleText,Units.ConvertWidth(x,AUnit).value,Units.ConvertWidth(y,AUnit).value);
  if not isFillNone then
  begin
    ACanvas2d.fillStyle(fillColor);
    ACanvas2d.fill;
  end;
  if not isStrokeNone then
  begin
    ACanvas2d.strokeStyle(strokeColor);
    ACanvas2d.stroke;
  end;
end;

constructor TSVGText.Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter);
begin
  Init(ADocument,'text',AUnits);
end;

{ TSVGGroup }

constructor TSVGGroup.Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter);
begin
  inherited Create(ADocument, AUnits);
  FContent := TSVGContent.Create(ADocument,FDomElem,AUnits);
end;

constructor TSVGGroup.Create(ADocument: TXMLDocument; AElement: TDOMElement;
  AUnits: TCSSUnitConverter);
begin
  inherited Create(ADocument, AElement, AUnits);
  FContent := TSVGContent.Create(ADocument,AElement,AUnits);
end;

destructor TSVGGroup.Destroy;
begin
  FreeAndNil(FContent);
  inherited Destroy;
end;

procedure TSVGGroup.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
begin
  FContent.Draw(ACanvas2d, AUnit);
end;

{ TSVGRectangle }

function TSVGRectangle.GetX: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['x'];
end;

function TSVGRectangle.GetY: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['y'];
end;

function TSVGRectangle.GetWidth: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['width'];
end;

function TSVGRectangle.GetHeight: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['height'];
end;

function TSVGRectangle.GetRX: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['rx'];
end;

function TSVGRectangle.GetRY: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['ry'];
end;

procedure TSVGRectangle.SetX(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['x'] := AValue;
end;

procedure TSVGRectangle.SetY(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['y'] := AValue;
end;

procedure TSVGRectangle.SetWidth(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['width'] := AValue;
end;

procedure TSVGRectangle.SetHeight(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['height'] := AValue;
end;

procedure TSVGRectangle.SetRX(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['rx'] := AValue;
end;

procedure TSVGRectangle.SetRY(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['ry'] := AValue;
end;

constructor TSVGRectangle.Create(ADocument: TXMLDocument;
  AUnits: TCSSUnitConverter);
begin
  Init(ADocument,'rect',AUnits);
end;

procedure TSVGRectangle.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
begin
  if not isStrokeNone or not isFillNone then
  begin
    ACanvas2d.beginPath;
    ACanvas2d.roundRect(Units.ConvertWidth(x,AUnit).value,Units.ConvertWidth(y,AUnit).value,
       Units.ConvertWidth(width,AUnit).value,Units.ConvertWidth(height,AUnit).value,
       Units.ConvertWidth(rx,AUnit).value,Units.ConvertWidth(ry,AUnit).value);
    if not isFillNone then
    begin
      ACanvas2d.fillStyle(fillColor);
      ACanvas2d.fill;
    end;
    if not isStrokeNone then
    begin
      ACanvas2d.strokeStyle(strokeColor);
      ACanvas2d.stroke;
    end;
  end;
end;

{ TSVGPolypoints }

function TSVGPolypoints.GetClosed: boolean;
begin
  result := FDomElem.TagName = 'polygon';
end;

function TSVGPolypoints.GetPoints: string;
begin
  result := Attribute['points'];
end;

function TSVGPolypoints.GetPointsF: ArrayOfTPointF;
var parser: TSVGParser;
  nbcoord,i: integer;
begin
  parser:=TSVGParser.Create(points);
  nbcoord := 0;
  repeat
    parser.ParseFloat;
    if not parser.NumberError then
      inc(nbcoord);
  until parser.NumberError or parser.Done;
  parser.ClearError;
  setlength(Result,nbcoord div 2);
  parser.Position := 1;
  for i := 0 to high(result) do
  begin
    result[i].x := parser.ParseFloat;
    result[i].y := parser.ParseFloat;
  end;
end;

procedure TSVGPolypoints.SetPoints(AValue: string);
begin
  Attribute['points'] := AValue;
end;

procedure TSVGPolypoints.SetPointsF(AValue: ArrayOfTPointF);
var s: string;
  i: integer;
begin
  s:= '';
  for i := 0 to high(AValue) do
  begin
    if s <> '' then s += ' ';
    with AValue[i] do
      s += TCSSUnitConverter.formatValue(x)+' '+TCSSUnitConverter.formatValue(y);
  end;
  points := s;
end;

constructor TSVGPolypoints.Create(ADocument: TXMLDocument;
  AUnits: TCSSUnitConverter; AClosed: boolean);
begin
  if AClosed then
    Init(ADocument, 'polygon', AUnits)
  else
    Init(ADocument, 'polyline', AUnits);
end;

destructor TSVGPolypoints.Destroy;
begin
  inherited Destroy;
end;

procedure TSVGPolypoints.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
var
  prevMatrix: TAffineMatrix;
begin
  if isFillNone and isStrokeNone then exit;
  if AUnit <> cuCustom then
  begin
    prevMatrix := ACanvas2d.matrix;
    ACanvas2d.scale(Units.ConvertWidth(1,cuCustom,AUnit),
      Units.ConvertHeight(1,cuCustom,AUnit));
    InternalDraw(ACanvas2d, cuCustom);
    ACanvas2d.matrix:= prevMatrix;
  end else
  begin
    ACanvas2d.beginPath;
    ACanvas2d.polylineTo(pointsF);
    if closed then ACanvas2d.closePath;
    if not isFillNone then
    begin
      ACanvas2d.fillStyle(fillColor);
      ACanvas2d.fill;
    end;
    if not isStrokeNone then
    begin
      ACanvas2d.strokeStyle(strokeColor);
      ACanvas2d.stroke;
    end;
  end;
end;

{ TSVGPath }

function TSVGPath.GetPathLength: TFloatWithCSSUnit;
begin
  result := OrthoAttributeWithUnit['pathLength'];
end;

function TSVGPath.GetPath: TBGRAPath;
begin
  if FPath = nil then
    FPath := TBGRAPath.Create(Attribute['d']);
  result := FPath;
end;

function TSVGPath.GetData: string;
begin
  if FPath = nil then
    result := Attribute['d']
  else
    result := FPath.SvgString;
end;

procedure TSVGPath.SetPathLength(AValue: TFloatWithCSSUnit);
begin
  OrthoAttributeWithUnit['pathLength'] := AValue;
end;

procedure TSVGPath.SetData(AValue: string);
begin
  if FPath = nil then
    Attribute['d'] := AValue
  else
    FPath.SvgString := AValue;
end;

function TSVGPath.GetDOMElement: TDOMElement;
begin
  if FPath <> nil then Attribute['d'] := FPath.SvgString;
  Result:=inherited GetDOMElement;
end;

constructor TSVGPath.Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter);
begin
  Init(ADocument,'path',AUnits);
  FPath := nil;
end;

constructor TSVGPath.Create(ADocument: TXMLDocument; AElement: TDOMElement;
  AUnits: TCSSUnitConverter);
begin
  Init(ADocument, AElement, AUnits);
  FPath := nil;
end;

destructor TSVGPath.Destroy;
begin
  FreeAndNil(FPath);
  inherited Destroy;
end;

procedure TSVGPath.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
var
  prevMatrix: TAffineMatrix;
begin
  if isFillNone and isStrokeNone then exit;
  if AUnit <> cuCustom then
  begin
    prevMatrix := ACanvas2d.matrix;
    ACanvas2d.scale(Units.ConvertWidth(1,cuCustom,AUnit),
      Units.ConvertHeight(1,cuCustom,AUnit));
    InternalDraw(ACanvas2d, cuCustom);
    ACanvas2d.matrix:= prevMatrix;
  end else
  begin
    ACanvas2d.path(path);
    if not isFillNone then
    begin
      ACanvas2d.fillStyle(fillColor);
      ACanvas2d.fill;
    end;
    if not isStrokeNone then
    begin
      ACanvas2d.strokeStyle(strokeColor);
      ACanvas2d.stroke;
    end;
  end;
end;

{ TSVGEllipse }

function TSVGEllipse.GetCX: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['cx'];
end;

function TSVGEllipse.GetCY: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['cy'];
end;

function TSVGEllipse.GetRX: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['rx'];
end;

function TSVGEllipse.GetRY: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['ry'];
end;

procedure TSVGEllipse.SetCX(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['cx'] := AValue;
end;

procedure TSVGEllipse.SetCY(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['cy'] := AValue;
end;

procedure TSVGEllipse.SetRX(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['rx'] := AValue;
end;

procedure TSVGEllipse.SetRY(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['ry'] := AValue;
end;

constructor TSVGEllipse.Create(ADocument: TXMLDocument;
  AUnits: TCSSUnitConverter);
begin
  Init(ADocument,'ellipse',AUnits);
end;

procedure TSVGEllipse.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
begin
  if not isFillNone or not isStrokeNone then
  begin
    ACanvas2d.beginPath;
    ACanvas2d.ellipse(Units.ConvertWidth(cx,AUnit).value,Units.ConvertWidth(cy,AUnit).value,
         Units.ConvertWidth(rx,AUnit).value,Units.ConvertWidth(ry,AUnit).value);
    if not isFillNone then
    begin
      ACanvas2d.fillStyle(fillColor);
      ACanvas2d.fill;
    end;
    if not isStrokeNone then
    begin
      ACanvas2d.strokeStyle(strokeColor);
      ACanvas2d.stroke;
    end;
  end;
end;

{ TSVGCircle }

function TSVGCircle.GetCX: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['cx'];
end;

function TSVGCircle.GetCY: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['cy'];
end;

function TSVGCircle.GetR: TFloatWithCSSUnit;
begin
  result := OrthoAttributeWithUnit['r'];
end;

procedure TSVGCircle.SetCX(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['cx'] := AValue;
end;

procedure TSVGCircle.SetCY(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['cy'] := AValue;
end;

procedure TSVGCircle.SetR(AValue: TFloatWithCSSUnit);
begin
  OrthoAttributeWithUnit['r'] := AValue;
end;

constructor TSVGCircle.Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter);
begin
  Init(ADocument,'circle',AUnits);
end;

procedure TSVGCircle.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
begin
  if not isFillNone or not isStrokeNone then
  begin
    ACanvas2d.beginPath;
    ACanvas2d.circle(Units.ConvertWidth(cx,AUnit).value,Units.ConvertWidth(cy,AUnit).value,
         Units.ConvertWidth(r,AUnit).value);
    if not isFillNone then
    begin
      ACanvas2d.fillStyle(fillColor);
      ACanvas2d.fill;
    end;
    if not isStrokeNone then
    begin
      ACanvas2d.strokeStyle(strokeColor);
      ACanvas2d.stroke;
    end;
  end;
end;

{ TSVGLine }

function TSVGLine.GetX1: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['x1'];
end;

function TSVGLine.GetX2: TFloatWithCSSUnit;
begin
  result := HorizAttributeWithUnit['x2'];
end;

function TSVGLine.GetY1: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['y1'];
end;

function TSVGLine.GetY2: TFloatWithCSSUnit;
begin
  result := VerticalAttributeWithUnit['y2'];
end;

procedure TSVGLine.SetX1(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['x1'] := AValue;
end;

procedure TSVGLine.SetX2(AValue: TFloatWithCSSUnit);
begin
  HorizAttributeWithUnit['x2'] := AValue;
end;

procedure TSVGLine.SetY1(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['y1'] := AValue;
end;

procedure TSVGLine.SetY2(AValue: TFloatWithCSSUnit);
begin
  VerticalAttributeWithUnit['y2'] := AValue;
end;

constructor TSVGLine.Create(ADocument: TXMLDocument; AUnits: TCSSUnitConverter);
begin
  Init(ADocument,'line',AUnits);
end;

procedure TSVGLine.InternalDraw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
begin
  if not isStrokeNone then
  begin
    ACanvas2d.strokeStyle(strokeColor);
    ACanvas2d.beginPath;
    ACanvas2d.moveTo(Units.ConvertWidth(x1,AUnit).value,Units.ConvertWidth(y1,AUnit).value);
    ACanvas2d.lineTo(Units.ConvertWidth(x2,AUnit).value,Units.ConvertWidth(y2,AUnit).value);
    ACanvas2d.stroke;
  end;
end;

{ TSVGContent }

function TSVGContent.GetElement(AIndex: integer): TSVGElement;
begin
  result := TSVGElement(FElements.Items[AIndex]);
end;

function TSVGContent.GetElementCount: integer;
begin
  result := FElements.Count;
end;

function TSVGContent.GetUnits: TCSSUnitConverter;
begin
  result := FUnits;
end;

procedure TSVGContent.AppendElement(AElement: TSVGElement);
begin
  FDomElem.AppendChild(AElement.DOMElement);
  FElements.Add(AElement);
end;

procedure TSVGContent.InsertElementBefore(AElement: TSVGElement;
  ASuccessor: TSVGElement);
var idx: integer;
begin
  idx := FElements.IndexOf(ASuccessor);
  if idx <> -1 then
  begin
    FElements.Insert(idx,AElement);
    FDomElem.InsertBefore(AElement.DOMElement, ASuccessor.DOMElement);
  end
  else
  begin
    FElements.Add(AElement);
    FDomElem.AppendChild(ASuccessor.DOMElement);
  end;
end;

constructor TSVGContent.Create(ADocument: TXMLDocument; AElement: TDOMElement;
  AUnits: TCSSUnitConverter);
var cur: TDOMNode;
begin
  FDoc := ADocument;
  FDomElem := AElement;
  FElements := TList.Create;
  FUnits := AUnits;
  cur := FDomElem.FirstChild;
  while cur <> nil do
  begin
    if cur is TDOMElement then
      FElements.Add(CreateSVGElementFromNode(ADocument,TDOMElement(cur),FUnits));
    cur := cur.NextSibling;
  end;
end;

destructor TSVGContent.Destroy;
var i:integer;
begin
  for i := 0 to ElementCount-1 do
    Element[i].free;
  FreeAndNil(FElements);
  inherited Destroy;
end;

procedure TSVGContent.Draw(ACanvas2d: TBGRACanvas2D; x, y: single; AUnit: TCSSUnit);
var prevMatrix: TAffineMatrix;
begin
  if (x<>0) or (y<>0) then
  begin
    prevMatrix := ACanvas2d.matrix;
    ACanvas2d.translate(x,y);
    Draw(ACanvas2d, AUnit);
    ACanvas2d.matrix := prevMatrix;
  end else
    Draw(ACanvas2d, AUnit);
end;

procedure TSVGContent.Draw(ACanvas2d: TBGRACanvas2D; AUnit: TCSSUnit);
var i: integer;
begin
  for i := 0 to ElementCount-1 do
    Element[i].Draw(ACanvas2d, AUnit);
end;

function TSVGContent.AppendLine(x1, y1, x2, y2: single; AUnit: TCSSUnit
  ): TSVGLine;
begin
  result := TSVGLine.Create(FDoc,Units);
  result.x1 := FloatWithCSSUnit(x1,AUnit);
  result.y1 := FloatWithCSSUnit(y1,AUnit);
  result.x2 := FloatWithCSSUnit(x2,AUnit);
  result.y2 := FloatWithCSSUnit(y2,AUnit);
  AppendElement(result);
end;

function TSVGContent.AppendLine(p1, p2: TPointF; AUnit: TCSSUnit): TSVGLine;
begin
  result := AppendLine(p1.x,p1.y,p2.x,p2.y,AUnit);
end;

function TSVGContent.AppendCircle(cx, cy, r: single; AUnit: TCSSUnit
  ): TSVGCircle;
begin
  if (AUnit <> cuCustom) and (Units.DpiScaleX <> Units.DpiScaleY) then
  begin
    result := TSVGCircle.Create(FDoc,Units);
    result.cx := FloatWithCSSUnit(Units.Convert(cx,AUnit,cuCustom,Units.DpiX),cuCustom);
    result.cy := FloatWithCSSUnit(Units.Convert(cy,AUnit,cuCustom,Units.DpiY),cuCustom);
    result.r := FloatWithCSSUnit(Units.Convert(r,AUnit,cuCustom,Units.DpiX),cuCustom);
    result.transform:= Units.DpiScaleTransform;
    AppendElement(result);
  end else
  begin
    result := TSVGCircle.Create(FDoc,Units);
    result.cx := FloatWithCSSUnit(cx,AUnit);
    result.cy := FloatWithCSSUnit(cy,AUnit);
    result.r := FloatWithCSSUnit(r,AUnit);
    AppendElement(result);
  end;
end;

function TSVGContent.AppendCircle(c: TPointF; r: single; AUnit: TCSSUnit
  ): TSVGCircle;
begin
  result := AppendCircle(c.x,c.y,r,AUnit);
end;

function TSVGContent.AppendEllipse(cx, cy, rx, ry: single; AUnit: TCSSUnit
  ): TSVGEllipse;
begin
  result := TSVGEllipse.Create(FDoc,Units);
  result.cx := FloatWithCSSUnit(cx,AUnit);
  result.cy := FloatWithCSSUnit(cy,AUnit);
  result.rx := FloatWithCSSUnit(rx,AUnit);
  result.ry := FloatWithCSSUnit(ry,AUnit);
  AppendElement(result);
end;

function TSVGContent.AppendEllipse(c, r: TPointF; AUnit: TCSSUnit): TSVGEllipse;
begin
  result := AppendEllipse(c.x,c.y,r.x,r.y,AUnit);
end;

function TSVGContent.AppendPath(data: string; AUnit: TCSSUnit): TSVGPath;
var tempPath: TBGRAPath;
begin
  if AUnit <> cuCustom then
  begin
    tempPath := TBGRAPath.Create(data);
    result := AppendPath(tempPath, AUnit);
    tempPath.Free;
  end else
  begin
    result := TSVGPath.Create(FDoc,Units);
    result.d := data;
    AppendElement(result);
  end;
end;

function TSVGContent.AppendPath(path: TBGRAPath; AUnit: TCSSUnit): TSVGPath;
begin
  if (AUnit <> cuCustom) and (Units.DpiScaleX <> Units.DpiScaleY) then
  begin
    result := TSVGPath.Create(FDoc,Units);
    result.path.scale(Units.Convert(1,AUnit,cuCustom,Units.DpiX));
    path.copyTo(result.path);
    result.transform := Units.DpiScaleTransform;
    AppendElement(result);
  end else
  begin
    result := TSVGPath.Create(FDoc,Units);
    result.path.scale(Units.ConvertWidth(1,AUnit,cuCustom));
    path.copyTo(result.path);
    AppendElement(result);
  end;
end;

function TSVGContent.AppendPolygon(const points: array of single;
  AUnit: TCSSUnit): TSVGPolypoints;
var
  pts: ArrayOfTPointF;
  i: integer;
begin
  result := TSVGPolypoints.Create(FDoc,FUnits,true);
  setlength(pts, length(points) div 2);
  for i := 0 to high(pts) do
    pts[i] := Units.ConvertCoord(PointF(points[i shl 1],points[(i shl 1)+1]),AUnit,cuCustom);
  result.pointsF := pts;
  AppendElement(result);
end;

function TSVGContent.AppendPolygon(const points: array of TPointF;
  AUnit: TCSSUnit): TSVGPolypoints;
var
  pts: ArrayOfTPointF;
  i: integer;
begin
  result := TSVGPolypoints.Create(FDoc,FUnits,true);
  setlength(pts, length(points));
  for i := 0 to high(pts) do
    pts[i] := Units.ConvertCoord(points[i],AUnit,cuCustom);
  result.pointsF := pts;
  AppendElement(result);
end;

function TSVGContent.AppendRect(x, y, width, height: single; AUnit: TCSSUnit
  ): TSVGRectangle;
begin
  result := TSVGRectangle.Create(FDoc,Units);
  result.x := FloatWithCSSUnit(x,AUnit);
  result.y := FloatWithCSSUnit(y,AUnit);
  result.width := FloatWithCSSUnit(width,AUnit);
  result.height := FloatWithCSSUnit(height,AUnit);
  AppendElement(result);
end;

function TSVGContent.AppendRect(origin, size: TPointF; AUnit: TCSSUnit
  ): TSVGRectangle;
begin
  result := AppendRect(origin.x,origin.y,size.x,size.y,AUnit);
end;

function TSVGContent.AppendText(x, y: single; AText: string; AUnit: TCSSUnit
  ): TSVGText;
begin
  result := TSVGText.Create(FDoc,Units);
  result.x := FloatWithCSSUnit(x,AUnit);
  result.y := FloatWithCSSUnit(y,AUnit);
  result.SimpleText:= AText;
  AppendElement(result);
end;

function TSVGContent.AppendText(origin: TPointF; AText: string; AUnit: TCSSUnit
  ): TSVGText;
begin
  result := AppendText(origin.x,origin.y,AText,AUnit);
end;

function TSVGContent.AppendRoundRect(x, y, width, height, rx, ry: single;
  AUnit: TCSSUnit): TSVGRectangle;
begin
  result := TSVGRectangle.Create(FDoc,Units);
  result.x := FloatWithCSSUnit(x,AUnit);
  result.y := FloatWithCSSUnit(y,AUnit);
  result.width := FloatWithCSSUnit(width,AUnit);
  result.height := FloatWithCSSUnit(height,AUnit);
  result.rx := FloatWithCSSUnit(rx,AUnit);
  result.ry := FloatWithCSSUnit(ry,AUnit);
  AppendElement(result);
end;

function TSVGContent.AppendRoundRect(origin, size, radius: TPointF;
  AUnit: TCSSUnit): TSVGRectangle;
begin
  result := AppendRoundRect(origin.x,origin.y,size.x,size.y,radius.x,radius.y,AUnit);
end;

end.

