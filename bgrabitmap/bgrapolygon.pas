unit BGRAPolygon;

{$mode objfpc}{$H+}

{ This unit contains polygon drawing functions and spline functions.

  Shapes are drawn using a TBGRACustomFillInfo object, which calculates the
  intersection of an horizontal line and the polygon.

  Various shapes are handled :
  - TFillPolyInfo : polygon scanned in any order
  - TOnePassFillPolyInfo : polygon scanned from top to bottom
  - TFillEllipseInfo : ellipse
  - TFillBorderEllipseInfo : ellipse border
  - TFillRoundRectangleInfo : round rectangle (or other corners)
  - TFillBorderRoundRectInfo : round rectangle border

  Various fill modes :
  - Alternate : each time there is an intersection, it enters or go out of the polygon
  - Winding : filled when the sum of ascending and descending intersection is non zero
  - Color : fill with a color defined as a TBGRAPixel argument
  - Erase : erase with an alpha in the TBGRAPixel argument
  - Texture : draws a texture with the IBGRAScanner argument

  Various border handling :
  - aliased : one horizontal line intersection is calculated per pixel in the vertical loop
  - antialiased : more lines are calculated and a density is computed by adding them together
  - multi-polygon antialiasing and superposition (TBGRAMultiShapeFiller) : same as above but
    by combining multiple polygons at the same time, and optionally subtracting top polygons
  }

interface

uses
  Classes, SysUtils, BGRAGraphics, BGRABitmapTypes, BGRAFillInfo, BGRAPath;

procedure FillShapeAntialias(bmp: TBGRACustomBitmap; shapeInfo: TBGRACustomFillInfo;
  c: TBGRAPixel; EraseMode: boolean; scan: IBGRAScanner; NonZeroWinding: boolean; LinearBlend: boolean = false);
procedure FillShapeAntialiasWithTexture(bmp: TBGRACustomBitmap; shapeInfo: TBGRACustomFillInfo;
  scan: IBGRAScanner; NonZeroWinding: boolean; LinearBlend: boolean = false);
procedure FillShapeAliased(bmp: TBGRACustomBitmap; shapeInfo: TBGRACustomFillInfo;
  c: TBGRAPixel; EraseMode: boolean; scan: IBGRAScanner; NonZeroWinding: boolean; drawmode: TDrawMode; AliasingIncludeBottomRight: Boolean= false);

type

  { TBGRAMultishapeFiller }

  TBGRAMultishapeFiller = class
  protected
    nbShapes: integer;
    shapes: array of record
        info: TBGRACustomFillInfo;
        internalInfo: boolean;
        texture: IBGRAScanner;
        internalTexture: TObject;
        color: TExpandedPixel;
        bounds: TRect;
      end;
    procedure AddShape(AInfo: TBGRACustomFillInfo; AInternalInfo: boolean; ATexture: IBGRAScanner; AInternalTexture: TObject; AColor: TBGRAPixel);
    function CheckRectangleBorderBounds(var x1, y1, x2, y2: single; w: single): boolean;
    procedure InternalAddStroke(const APoints: array of TPointF; AClosed: boolean; AData: Pointer);
  public
    FillMode : TFillMode;
    PolygonOrder: TPolygonOrder;
    Antialiasing: Boolean;
    AliasingIncludeBottomRight: Boolean;
    constructor Create;
    destructor Destroy; override;
    procedure AddShape(AShape: TBGRACustomFillInfo; AColor: TBGRAPixel);
    procedure AddShape(AShape: TBGRACustomFillInfo; ATexture: IBGRAScanner);
    procedure AddPolygon(const points: array of TPointF; AColor: TBGRAPixel);
    procedure AddPolygon(const points: array of TPointF; ATexture: IBGRAScanner);
    procedure AddPathStroke(APath: TBGRAPath; AColor: TBGRAPixel; AWidth: single; AStroker: TBGRACustomPenStroker);
    procedure AddPathStroke(APath: TBGRAPath; ATexture: IBGRAScanner; AWidth: single; AStroker: TBGRACustomPenStroker);
    procedure AddPathStroke(APath: TBGRAPath; AMatrix: TAffineMatrix; AColor: TBGRAPixel; AWidth: single; AStroker: TBGRACustomPenStroker);
    procedure AddPathStroke(APath: TBGRAPath; AMatrix: TAffineMatrix; ATexture: IBGRAScanner; AWidth: single; AStroker: TBGRACustomPenStroker);
    procedure AddPathFill(APath: TBGRAPath; AColor: TBGRAPixel);
    procedure AddPathFill(APath: TBGRAPath; ATexture: IBGRAScanner);
    procedure AddPathFill(APath: TBGRAPath; AMatrix: TAffineMatrix; AColor: TBGRAPixel);
    procedure AddPathFill(APath: TBGRAPath; AMatrix: TAffineMatrix; ATexture: IBGRAScanner);
    procedure AddPolylineStroke(const points: array of TPointF; AColor: TBGRAPixel; AWidth: single; AStroker: TBGRACustomPenStroker);
    procedure AddPolylineStroke(const points: array of TPointF; ATexture: IBGRAScanner; AWidth: single; AStroker: TBGRACustomPenStroker);
    procedure AddPolygonStroke(const points: array of TPointF; AColor: TBGRAPixel; AWidth: single; AStroker: TBGRACustomPenStroker);
    procedure AddPolygonStroke(const points: array of TPointF; ATexture: IBGRAScanner; AWidth: single; AStroker: TBGRACustomPenStroker);
    procedure AddTriangleLinearColor(pt1, pt2, pt3: TPointF; c1, c2, c3: TBGRAPixel);
    procedure AddTriangleLinearMapping(pt1, pt2, pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF);
    procedure AddQuadLinearColor(pt1, pt2, pt3, pt4: TPointF; c1, c2, c3, c4: TBGRAPixel);
    procedure AddQuadLinearMapping(pt1, pt2, pt3, pt4: TPointF; texture: IBGRAScanner; tex1, tex2, {%H-}tex3, tex4: TPointF;
       ACulling: TFaceCulling = fcNone);
    procedure AddQuadPerspectiveMapping(pt1, pt2, pt3, pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF);
    procedure AddEllipse(x, y, rx, ry: single; AColor: TBGRAPixel);
    procedure AddEllipse(x, y, rx, ry: single; ATexture: IBGRAScanner);
    procedure AddEllipseBorder(x, y, rx, ry, w: single; AColor: TBGRAPixel);
    procedure AddEllipseBorder(x, y, rx, ry, w: single; ATexture: IBGRAScanner);
    procedure AddRoundRectangle(x1, y1, x2, y2, rx, ry: single; AColor: TBGRAPixel; options: TRoundRectangleOptions= []);
    procedure AddRoundRectangle(x1, y1, x2, y2, rx, ry: single; ATexture: IBGRAScanner; options: TRoundRectangleOptions= []);
    procedure AddRoundRectangleBorder(x1, y1, x2, y2, rx, ry, w: single; AColor: TBGRAPixel; options: TRoundRectangleOptions= []);
    procedure AddRoundRectangleBorder(x1, y1, x2, y2, rx, ry, w: single; ATexture: IBGRAScanner; options: TRoundRectangleOptions= []);
    procedure AddRectangle(x1, y1, x2, y2: single; AColor: TBGRAPixel);
    procedure AddRectangle(x1, y1, x2, y2: single; ATexture: IBGRAScanner);
    procedure AddRectangleBorder(x1, y1, x2, y2, w: single; AColor: TBGRAPixel);
    procedure AddRectangleBorder(x1, y1, x2, y2, w: single; ATexture: IBGRAScanner);
    procedure Draw(dest: TBGRACustomBitmap; ADrawMode: TDrawMode = dmDrawWithTransparency);
  end;

procedure FillPolyAliased(bmp: TBGRACustomBitmap; points: array of TPointF;
  c: TBGRAPixel; EraseMode: boolean; NonZeroWinding: boolean; drawmode: TDrawMode);
procedure FillPolyAliasedWithTexture(bmp: TBGRACustomBitmap; points: array of TPointF;
  scan: IBGRAScanner; NonZeroWinding: boolean; drawmode: TDrawMode);
procedure FillPolyAntialias(bmp: TBGRACustomBitmap; points: array of TPointF;
  c: TBGRAPixel; EraseMode: boolean; NonZeroWinding: boolean; LinearBlend: boolean = false);
procedure FillPolyAntialiasWithTexture(bmp: TBGRACustomBitmap; points: array of TPointF;
  scan: IBGRAScanner; NonZeroWinding: boolean; LinearBlend: boolean = false);

procedure FillEllipseAntialias(bmp: TBGRACustomBitmap; x, y, rx, ry: single;
  c: TBGRAPixel; EraseMode: boolean; LinearBlend: boolean = false);
procedure FillEllipseAntialiasWithTexture(bmp: TBGRACustomBitmap; x, y, rx, ry: single;
  scan: IBGRAScanner; LinearBlend: boolean = false);

procedure BorderEllipseAntialias(bmp: TBGRACustomBitmap; x, y, rx, ry, w: single;
  c: TBGRAPixel; EraseMode: boolean; LinearBlend: boolean = false);
procedure BorderEllipseAntialiasWithTexture(bmp: TBGRACustomBitmap; x, y, rx, ry, w: single;
  scan: IBGRAScanner; LinearBlend: boolean = false);

procedure FillRoundRectangleAntialias(bmp: TBGRACustomBitmap; x1, y1, x2, y2, rx, ry: single;
  options: TRoundRectangleOptions; c: TBGRAPixel; EraseMode: boolean; LinearBlend: boolean = false);
procedure FillRoundRectangleAntialiasWithTexture(bmp: TBGRACustomBitmap; x1, y1, x2, y2, rx, ry: single;
  options: TRoundRectangleOptions; scan: IBGRAScanner; LinearBlend: boolean = false);

procedure BorderRoundRectangleAntialias(bmp: TBGRACustomBitmap; x1, y1, x2, y2, rx, ry, w: single;
  options: TRoundRectangleOptions; c: TBGRAPixel; EraseMode: boolean; LinearBlend: boolean = false);
procedure BorderRoundRectangleAntialiasWithTexture(bmp: TBGRACustomBitmap; x1, y1, x2, y2, rx, ry, w: single;
  options: TRoundRectangleOptions; scan: IBGRAScanner; LinearBlend: boolean = false);

procedure BorderAndFillRoundRectangleAntialias(bmp: TBGRACustomBitmap; x1, y1, x2, y2, rx, ry, w: single;
  options: TRoundRectangleOptions; bordercolor,fillcolor: TBGRAPixel; bordertexture,filltexture: IBGRAScanner; EraseMode: boolean);

implementation

uses Math, BGRABlend, BGRAGradientScanner, BGRATransform;

type
  TPathStrokeData = record
    Stroker: TBGRACustomPenStroker;
    Texture: IBGRAScanner;
    Color: TBGRAPixel;
    Width: Single;
  end;

procedure FillShapeAntialias(bmp: TBGRACustomBitmap; shapeInfo: TBGRACustomFillInfo;
  c: TBGRAPixel; EraseMode: boolean; scan: IBGRAScanner; NonZeroWinding: boolean; LinearBlend: boolean);
const oneOver512 = 1/512;
var
  inter:   array of TIntersectionInfo;
  nbInter: integer;

  firstScan, lastScan: record
    inter:   array of TIntersectionInfo;
    nbInter: integer;
    sliceIndex: integer;
  end;

  miny, maxy, minx, maxx,
  densMinX, densMaxX: integer;

  density: PDensity;

  xb, yb, yc, i: integer;
  tempDensity: UInt32or64;

  x1, x2, x1b,x2b: single;
  ix1, ix2: integer;
  pdest:    PBGRAPixel;
  pdens:    PDensity;

  curvedSeg,optimised: boolean;
  ec: TExpandedPixel;
  c2:TBGRAPixel;
  MemScanCopy,pscan: pbgrapixel;
  ScanNextPixelProc: TScanNextPixelFunction;
  temp: Single;

  function GetYScan(num: integer): single; inline;
  begin
    result := yb + (num * 2 + 1) / (AntialiasPrecision * 2);
  end;

  procedure SubTriangleDensity(x1,density1, x2, density2: single);
  var ix1,ix2,n: integer;
      slope: single;
    function densityAt(x: single): single; inline;
    begin
      result := (x-x1)*slope+density1;
    end;
  var
      curdens: single;
      pdens: pdensity;
      newvalue: Int32or64;
  begin
    if (x1 <> x2) and (x1 < maxx + 1) and (x2 >= minx) then
    begin
      slope := (density2-density1)/(x2-x1);
      if x1 < minx then
      begin
        density1 := densityAt(minx);
        x1 := minx;
      end;
      if x2 >= maxx + 1 then
      begin
        density2 := densityAt(maxx+1);
        x2 := maxx + 1;
      end;
      ix1  := floor(x1);
      ix2  := floor(x2);

      if ix1 = ix2 then
      begin
        newValue := (density + (ix1 - minx))^ - round((x2 - x1)*(density1+density2)/2);
        if newValue < 0 then newValue := 0;
        if newValue > 256 then newValue := 256;
        (density + (ix1 - minx))^ := newValue
      end
      else
      begin
        newValue := (density + (ix1 - minx))^ - round((1 - (x1 - ix1))*(density1+densityAt(ix1+1))/2) ;
        if newValue < 0 then newValue := 0;
        if newValue > 256 then newValue := 256;
        (density + (ix1 - minx))^ := newValue;
        if (ix2 <= maxx) then
        begin
          newValue := (density + (ix2 - minx))^ - round((x2 - ix2)*(density2+densityAt(ix2))/2);
          if newValue < 0 then newValue := 0;
          if newValue > 256 then newValue := 256;
          (density + (ix2 - minx))^ := newValue;
        end;
      end;
      if ix2 > ix1 + 1 then
      begin
        curdens := densityAt(ix1+1.5);
        pdens := density + (ix1+1 - minx);
        for n := ix2-1-(ix1+1) downto 0 do
        begin
          newValue := pdens^ - round(curdens);
          if newValue < 0 then newValue := 0;
          if newValue > 256 then newValue := 256;
          pdens^ := newValue;
          curdens += slope;
          inc(pdens);
        end;
      end;
    end;
  end;

begin
  if (scan=nil) and (c.alpha=0) then exit;
  If not BGRAShapeComputeMinMax(shapeInfo,minx,miny,maxx,maxy,bmp) then exit;

  inter := shapeInfo.CreateIntersectionArray;
  getmem(density, (maxx - minx + 2)*sizeof(TDensity)); //more for safety
  ec := GammaExpansion(c);
  c2 := c;

  MemScanCopy := nil;
  ScanNextPixelProc := nil;
  if scan <> nil then
  begin
    if scan.IsScanPutPixelsDefined then
      GetMem(MemScanCopy,(maxx-minx+1)*sizeof(TBGRAPixel));
    ScanNextPixelProc := @scan.ScanNextPixel;
  end;

  curvedSeg := shapeInfo.SegmentsCurved;
  if not curvedSeg then
  begin
    firstScan.inter := shapeInfo.CreateIntersectionArray;
    lastScan.inter := shapeInfo.CreateIntersectionArray;
  end;

  //vertical scan
  for yb := miny to maxy do
  begin
    //mean density
    fillchar(density^,(maxx-minx+1)*sizeof(TDensity),0);

    densMinX := maxx+1;
    densMaxX := minx-1;

    if not curvedSeg then
    begin
      with firstScan do
      begin
        shapeInfo.ComputeAndSort(yb+1/256,inter,nbInter,NonZeroWinding);
        sliceIndex:= shapeInfo.GetSliceIndex;
      end;
      with lastScan do
      begin
        shapeInfo.ComputeAndSort(yb+255/256,inter,nbInter,NonZeroWinding);
        sliceIndex:= shapeInfo.GetSliceIndex;
      end;
      if (firstScan.sliceIndex = lastScan.sliceIndex) and (firstScan.nbInter = lastScan.nbInter) then
      begin
        optimised := true;
        for i := 0 to firstScan.nbInter-1 do
          if firstScan.inter[i].numSegment <> lastScan.inter[i].numSegment then
          begin
            optimised := false;
            break;
          end;
      end else
        optimised := false;

      if optimised then
      begin
        for i := 0 to firstScan.nbinter div 2 - 1 do
        begin
          x1 := firstScan.inter[i+i].interX;
          x1b := lastScan.inter[i+i].interX;
          x2 := firstScan.inter[i+i+1].interX;
          x2b := lastScan.inter[i+i+1].interX;
          if (abs(x1-x1b)<oneOver512) and (abs(x2-x2b)<oneOver512) and
             ((i+i+2 >= firstScan.nbInter) or
              ((firstScan.inter[i+i+2].interX >= x2+1) and
               (lastScan.inter[i+i+2].interX >= x2b+1))) then
          begin
            x1 := (x1+x1b)*0.5;
            x2 := (x2+x2b)*0.5;
            ix1 := floor(x1);
            ix2 := floor(x2);
            if ix1 < minx then ix1 := minx;
            if ix2 > maxx then ix2 := maxx;
            if ix1>ix2 then continue;
            if ix1=ix2 then
            begin
              tempDensity:= round((x2-x1)*256);
              if scan <> nil then //with texture scan
              begin
                scan.ScanMoveTo(ix1,yb);
                c := scan.ScanNextPixel;
                c.alpha := c.alpha*tempDensity shr 8;
                if linearBlend then
                  bmp.DrawPixel(ix1, yb, c, dmLinearBlend)
                else
                  bmp.DrawPixel(ix1, yb, c, dmDrawWithTransparency);
              end else
              if EraseMode then //erase with alpha
                bmp.ErasePixel(ix1,yb,c.alpha*tempDensity shr 8)
              else
              begin  //solid color
                c2.alpha := c.alpha*tempDensity shr 8;
                if linearBlend then
                  bmp.DrawPixel(ix1, yb, c2, dmLinearBlend)
                else
                  bmp.DrawPixel(ix1, yb, c2, dmDrawWithTransparency);
              end;
            end else
            begin
              tempDensity:= round((ix1+1-x1)*256);
              if scan <> nil then scan.ScanMoveTo(ix1,yb);
              if tempDensity < 256 then
              begin
                if scan <> nil then //with texture scan
                begin
                  c := scan.ScanNextPixel;
                  c.alpha := c.alpha*tempDensity shr 8;
                  if linearBlend then
                    bmp.DrawPixel(ix1, yb, c, dmLinearBlend)
                  else
                    bmp.DrawPixel(ix1, yb, c, dmDrawWithTransparency);
                end else
                if EraseMode then //erase with alpha
                  bmp.ErasePixel(ix1,yb, c.alpha*tempDensity shr 8)
                else
                begin  //solid color
                  c2.alpha := c.alpha*tempDensity shr 8;
                  if linearBlend then
                    bmp.DrawPixel(ix1, yb, c2, dmLinearBlend)
                  else
                    bmp.DrawPixel(ix1, yb, c2, dmDrawWithTransparency);
                end;
                inc(ix1);
              end;
              tempDensity:= round((x2-ix2)*256);
              if tempDensity < 256 then dec(ix2);
              if ix2 >= ix1 then
              begin
                if scan <> nil then //with texture scan
                begin
                  if linearBlend then
                    ScannerPutPixels(scan, bmp.ScanLine[yb] + ix1, ix2-ix1+1, dmLinearBlend)
                  else
                    ScannerPutPixels(scan, bmp.ScanLine[yb] + ix1, ix2-ix1+1, dmDrawWithTransparency);
                end else
                if EraseMode then //erase with alpha
                  bmp.EraseLine(ix1,yb,ix2,yb,c.alpha,True)
                else
                begin  //solid color
                  if LinearBlend then
                    bmp.HorizLine(ix1,yb,ix2,c,dmLinearBlend)
                  else
                    bmp.HorizLine(ix1,yb,ix2,c,dmDrawWithTransparency);
                end;
              end;
              if tempDensity < 256 then
              begin
                inc(ix2);
                if scan <> nil then //with texture scan
                begin
                  c := scan.ScanNextPixel;
                  c.alpha := c.alpha*tempDensity shr 8;
                  if linearBlend then
                    bmp.DrawPixel(ix2, yb, c, dmLinearBlend)
                  else
                    bmp.DrawPixel(ix2, yb, c, dmDrawWithTransparency);
                end else
                if EraseMode then //erase with alpha
                  bmp.ErasePixel(ix2,yb,c.alpha*tempDensity shr 8)
                else
                begin  //solid color
                  c2.alpha := c.alpha*tempDensity shr 8;
                  if linearBlend then
                    bmp.DrawPixel(ix2, yb, c2, dmLinearBlend)
                  else
                    bmp.DrawPixel(ix2, yb, c2, dmDrawWithTransparency);
                end;
              end;
            end;
            continue;
          end else
          begin
            if (x1 > x1b) then
            begin
              temp := x1;
              x1 := x1b;
              x1b := temp;
            end;
            if (x2 < x2b) then
            begin
              temp := x2;
              x2 := x2b;
              x2b := temp;
            end;

  	    {$DEFINE INCLUDE_FILLDENSITY}
  	    {$DEFINE PARAM_SINGLESEGMENT}
            {$i density256.inc}
            SubTriangleDensity(x1,256,x1b,0);
            SubTriangleDensity(x2b,0,x2,256);
          end;
        end;
      end else
      begin
        for yc := 0 to AntialiasPrecision - 1 do
        begin
          //find intersections
          shapeInfo.ComputeAndSort(GetYScan(yc),inter,nbInter,NonZeroWinding);

	  {$DEFINE INCLUDE_FILLDENSITY}
          {$i density256.inc}
        end;
      end;
    end else
    begin
      optimised := false;
      //precision scan
      for yc := 0 to AntialiasPrecision - 1 do
      begin
        //find intersections
        shapeInfo.ComputeAndSort(GetYScan(yc),inter,nbInter,NonZeroWinding);

	{$DEFINE INCLUDE_FILLDENSITY}
        {$i density256.inc}
      end;
    end;

    if LinearBlend then
    begin
      if optimised then
	{$DEFINE INCLUDE_RENDERDENSITY}
        {$define PARAM_LINEARANTIALIASING}
        {$i density256.inc}
      else
	{$DEFINE INCLUDE_RENDERDENSITY}
        {$define PARAM_LINEARANTIALIASING}
        {$define PARAM_ANTIALIASINGFACTOR}
        {$i density256.inc}
    end else
    begin
      if optimised then
        {$DEFINE INCLUDE_RENDERDENSITY}
        {$i density256.inc}
      else
        {$DEFINE INCLUDE_RENDERDENSITY}
        {$define PARAM_ANTIALIASINGFACTOR}
        {$i density256.inc}
    end;
  end;

  freemem(MemScanCopy);
  shapeInfo.FreeIntersectionArray(inter);

  if not curvedSeg then
  begin
    with firstScan do
    begin
      for i := 0 to high(inter) do
        inter[i].free;
    end;
    with lastScan do
    begin
      for i := 0 to high(inter) do
        inter[i].free;
    end;
  end;
  freemem(density);

  bmp.InvalidateBitmap;
end;

procedure FillShapeAliased(bmp: TBGRACustomBitmap; shapeInfo: TBGRACustomFillInfo;
  c: TBGRAPixel; EraseMode: boolean; scan: IBGRAScanner; NonZeroWinding: boolean; drawmode: TDrawMode; AliasingIncludeBottomRight: Boolean= false);
var
  inter:    array of TIntersectionInfo;
  nbInter:  integer;

  miny, maxy, minx, maxx: integer;
  xb,yb, i: integer;
  x1, x2: single;
  ix1, ix2: integer;
  pdest: PBGRAPixel;
  AliasingOfs: TPointF;
  ec: TExpandedPixel;

begin
  if (scan=nil) and (c.alpha=0) then exit;
  If not BGRAShapeComputeMinMax(shapeInfo,minx,miny,maxx,maxy,bmp) then exit;
  inter := shapeInfo.CreateIntersectionArray;

  if AliasingIncludeBottomRight then
    AliasingOfs := PointF(0,0) else
    AliasingOfs := PointF(-0.0001,-0.0001);

  ec := GammaExpansion(c);
  if (scan = nil) and (c.alpha = 255) then drawmode := dmSet;

  //vertical scan
  for yb := miny to maxy do
  begin
    //find intersections
    shapeInfo.ComputeAndSort( yb+0.5-AliasingOfs.Y, inter, nbInter, NonZeroWinding);

    for i := 0 to nbinter div 2 - 1 do
    begin
      x1 := inter[i + i].interX+AliasingOfs.X;
      x2 := inter[i + i+ 1].interX+AliasingOfs.X;

      if x1 <> x2 then
      begin
        ComputeAliasedRowBounds(x1,x2, minx,maxx, ix1,ix2);
        if ix1 <= ix2 then
        begin
          //render scanline
          if scan <> nil then //with texture scan
          begin
            pdest := bmp.ScanLine[yb] + ix1;
            scan.ScanMoveTo(ix1,yb);
            ScannerPutPixels(scan,pdest,ix2-ix1+1,drawmode);
          end else
          if EraseMode then //erase with alpha
          begin
            pdest := bmp.ScanLine[yb] + ix1;
            for xb := ix1 to ix2 do
            begin
              ErasePixelInline(pdest, c.alpha);
              Inc(pdest);
            end;
          end
          else
          begin
            case drawmode of
              dmFastBlend: bmp.FastBlendHorizLine(ix1,yb,ix2, c);
              dmDrawWithTransparency: bmp.DrawHorizLine(ix1,yb,ix2, ec);
              dmSet: bmp.SetHorizLine(ix1,yb,ix2, c);
              dmXor: bmp.XorHorizLine(ix1,yb,ix2, c);
            end;
          end;
        end;
      end;
    end;
  end;

  shapeInfo.FreeIntersectionArray(inter);
  bmp.InvalidateBitmap;
end;

procedure FillShapeAntialiasWithTexture(bmp: TBGRACustomBitmap;
  shapeInfo: TBGRACustomFillInfo; scan: IBGRAScanner; NonZeroWinding: boolean; LinearBlend: boolean);
begin
  FillShapeAntialias(bmp,shapeInfo,BGRAPixelTransparent,False,scan,NonZeroWinding,LinearBlend);
end;

procedure FillPolyAliased(bmp: TBGRACustomBitmap; points: array of TPointF;
  c: TBGRAPixel; EraseMode: boolean; NonZeroWinding: boolean; drawmode: TDrawMode);
var
  info: TCustomFillPolyInfo;
begin
  if length(points) < 3 then
    exit;

  info := TOnePassFillPolyInfo.Create(points);
  FillShapeAliased(bmp, info, c, EraseMode, nil, NonZeroWinding, drawmode);
  info.Free;
end;

procedure FillPolyAliasedWithTexture(bmp: TBGRACustomBitmap;
  points: array of TPointF; scan: IBGRAScanner; NonZeroWinding: boolean; drawmode: TDrawMode);
var
  info: TCustomFillPolyInfo;
begin
  if length(points) < 3 then
    exit;

  info := TOnePassFillPolyInfo.Create(points);
  FillShapeAliased(bmp, info, BGRAPixelTransparent,False,scan, NonZeroWinding, drawmode);
  info.Free;
end;

procedure FillPolyAntialias(bmp: TBGRACustomBitmap; points: array of TPointF;
  c: TBGRAPixel; EraseMode: boolean; NonZeroWinding: boolean; LinearBlend: boolean);
var
  info: TCustomFillPolyInfo;
begin
  if length(points) < 3 then
    exit;

  info := TOnePassFillPolyInfo.Create(points);
  FillShapeAntialias(bmp, info, c, EraseMode, nil, NonZeroWinding, LinearBlend);
  info.Free;
end;

procedure FillPolyAntialiasWithTexture(bmp: TBGRACustomBitmap;
  points: array of TPointF; scan: IBGRAScanner; NonZeroWinding: boolean; LinearBlend: boolean);
var
  info: TCustomFillPolyInfo;
begin
  if length(points) < 3 then
    exit;

  info := TOnePassFillPolyInfo.Create(points);
  FillShapeAntialiasWithTexture(bmp, info, scan, NonZeroWinding, LinearBlend);
  info.Free;
end;

procedure FillEllipseAntialias(bmp: TBGRACustomBitmap; x, y, rx, ry: single;
  c: TBGRAPixel; EraseMode: boolean; LinearBlend: boolean);
var
  info: TFillEllipseInfo;
begin
  if (rx = 0) or (ry = 0) or (x = EmptySingle) or (y = EmptySingle) then
    exit;

  info := TFillEllipseInfo.Create(x, y, rx, ry);
  FillShapeAntialias(bmp, info, c, EraseMode, nil, False, LinearBlend);
  info.Free;
end;

procedure FillEllipseAntialiasWithTexture(bmp: TBGRACustomBitmap; x, y, rx,
  ry: single; scan: IBGRAScanner; LinearBlend: boolean);
var
  info: TFillEllipseInfo;
begin
  if (rx = 0) or (ry = 0) or (x = EmptySingle) or (y = EmptySingle) then
    exit;

  info := TFillEllipseInfo.Create(x, y, rx, ry);
  FillShapeAntialiasWithTexture(bmp, info, scan, False, LinearBlend);
  info.Free;
end;

procedure BorderEllipseAntialias(bmp: TBGRACustomBitmap; x, y, rx, ry, w: single;
  c: TBGRAPixel; EraseMode: boolean; LinearBlend: boolean);
var
  info: TFillBorderEllipseInfo;
begin
  if ((rx = 0) and (ry = 0)) or (w=0) or (x = EmptySingle) or (y = EmptySingle) then
    exit;
  info := TFillBorderEllipseInfo.Create(x, y, rx, ry, w);
  FillShapeAntialias(bmp, info, c, EraseMode, nil, False, LinearBlend);
  info.Free;
end;

procedure BorderEllipseAntialiasWithTexture(bmp: TBGRACustomBitmap; x, y, rx,
  ry, w: single; scan: IBGRAScanner; LinearBlend: boolean);
var
  info: TFillBorderEllipseInfo;
begin
  if ((rx = 0) and (ry = 0)) or (w=0) or (x = EmptySingle) or (y = EmptySingle) then
    exit;
  info := TFillBorderEllipseInfo.Create(x, y, rx, ry, w);
  FillShapeAntialiasWithTexture(bmp, info, scan, False, LinearBlend);
  info.Free;
end;

{ TBGRAMultishapeFiller }

procedure TBGRAMultishapeFiller.AddShape(AInfo: TBGRACustomFillInfo; AInternalInfo: boolean; ATexture: IBGRAScanner; AInternalTexture: TObject; AColor: TBGRAPixel);
begin
  if length(shapes) = nbShapes then
    setlength(shapes, (length(shapes)+1)*2);
  with shapes[nbShapes] do
  begin
    info := AInfo;
    internalInfo:= AInternalInfo;
    texture := ATexture;
    internalTexture:= AInternalTexture;
    color := GammaExpansion(AColor);
  end;
  inc(nbShapes);
end;

function TBGRAMultishapeFiller.CheckRectangleBorderBounds(var x1, y1, x2,
  y2: single; w: single): boolean;
var temp: single;
begin
  if x1 > x2 then
  begin
    temp := x1;
    x1 := x2;
    x2 := temp;
  end;
  if y1 > y2 then
  begin
    temp := y1;
    y1 := y2;
    y2 := temp;
  end;
  result := (x2-x1 > w) and (y2-y1 > w);
end;

procedure TBGRAMultishapeFiller.InternalAddStroke(
  const APoints: array of TPointF; AClosed: boolean; AData: Pointer);
var pts: ArrayOfTPointF;
begin
  with TPathStrokeData(AData^) do
  begin
    if AClosed then
      pts := Stroker.ComputePolygon(APoints, Width)
    else
      pts := Stroker.ComputePolylineAutoCycle(APoints, Width);
    if Texture <> nil then
      AddPolygon(pts, Texture)
    else
      AddPolygon(pts, Color);
  end;
end;

constructor TBGRAMultishapeFiller.Create;
begin
  nbShapes := 0;
  shapes := nil;
  PolygonOrder := poNone;
  Antialiasing := True;
  AliasingIncludeBottomRight := False;
end;

destructor TBGRAMultishapeFiller.Destroy;
var
  i: Integer;
begin
  for i := 0 to nbShapes-1 do
  begin
    if shapes[i].internalInfo then shapes[i].info.free;
    shapes[i].texture := nil;
    if shapes[i].internalTexture <> nil then shapes[i].internalTexture.Free;
  end;
  shapes := nil;
  inherited Destroy;
end;

procedure TBGRAMultishapeFiller.AddShape(AShape: TBGRACustomFillInfo; AColor: TBGRAPixel);
begin
  AddShape(AShape,False,nil,nil,AColor);
end;

procedure TBGRAMultishapeFiller.AddShape(AShape: TBGRACustomFillInfo;
  ATexture: IBGRAScanner);
begin
  AddShape(AShape,False,ATexture,nil,BGRAPixelTransparent);
end;

procedure TBGRAMultishapeFiller.AddPolygon(const points: array of TPointF;
  AColor: TBGRAPixel);
begin
  if length(points) <= 2 then exit;
  AddShape(TOnePassFillPolyInfo.Create(points),True,nil,nil,AColor);
end;

procedure TBGRAMultishapeFiller.AddPolygon(const points: array of TPointF;
  ATexture: IBGRAScanner);
begin
  if length(points) <= 2 then exit;
  AddShape(TOnePassFillPolyInfo.Create(points),True,ATexture,nil,BGRAPixelTransparent);
end;

procedure TBGRAMultishapeFiller.AddPathStroke(APath: TBGRAPath;
  AColor: TBGRAPixel; AWidth: single; AStroker: TBGRACustomPenStroker);
begin
  AddPathStroke(APath,AffineMatrixIdentity,AColor,AWidth,AStroker);
end;

procedure TBGRAMultishapeFiller.AddPathStroke(APath: TBGRAPath;
  ATexture: IBGRAScanner; AWidth: single; AStroker: TBGRACustomPenStroker);
begin
  AddPathStroke(APath,AffineMatrixIdentity,ATexture,AWidth,AStroker);
end;

procedure TBGRAMultishapeFiller.AddPathStroke(APath: TBGRAPath;
  AMatrix: TAffineMatrix; AColor: TBGRAPixel; AWidth: single;
  AStroker: TBGRACustomPenStroker);
var data: TPathStrokeData;
begin
  data.Stroker := AStroker;
  data.Color := AColor;
  data.Texture := nil;
  data.Width := AWidth;
  APath.stroke(@InternalAddStroke, AMatrix, 0.1, @data);
end;

procedure TBGRAMultishapeFiller.AddPathStroke(APath: TBGRAPath;
  AMatrix: TAffineMatrix; ATexture: IBGRAScanner; AWidth: single;
  AStroker: TBGRACustomPenStroker);
var data: TPathStrokeData;
begin
  data.Stroker := AStroker;
  data.Color := BGRAPixelTransparent;
  data.Texture := ATexture;
  data.Width := AWidth;
  APath.stroke(@InternalAddStroke, AMatrix, 0.1, @data);
end;

procedure TBGRAMultishapeFiller.AddPathFill(APath: TBGRAPath; AColor: TBGRAPixel);
begin
  AddPolygon(APath.ToPoints, AColor);
end;

procedure TBGRAMultishapeFiller.AddPathFill(APath: TBGRAPath;
  ATexture: IBGRAScanner);
begin
  AddPolygon(APath.ToPoints, ATexture);
end;

procedure TBGRAMultishapeFiller.AddPathFill(APath: TBGRAPath;
  AMatrix: TAffineMatrix; AColor: TBGRAPixel);
begin
  AddPolygon(APath.ToPoints(AMatrix), AColor);
end;

procedure TBGRAMultishapeFiller.AddPathFill(APath: TBGRAPath;
  AMatrix: TAffineMatrix; ATexture: IBGRAScanner);
begin
  AddPolygon(APath.ToPoints(AMatrix), ATexture);
end;

procedure TBGRAMultishapeFiller.AddPolylineStroke(
  const points: array of TPointF; AColor: TBGRAPixel; AWidth: single;
  AStroker: TBGRACustomPenStroker);
begin
  AddPolygon(AStroker.ComputePolyline(points,AWidth,AColor), AColor);
end;

procedure TBGRAMultishapeFiller.AddPolylineStroke(
  const points: array of TPointF; ATexture: IBGRAScanner; AWidth: single;
  AStroker: TBGRACustomPenStroker);
begin
  AddPolygon(AStroker.ComputePolyline(points,AWidth), ATexture);
end;

procedure TBGRAMultishapeFiller.AddPolygonStroke(
  const points: array of TPointF; AColor: TBGRAPixel; AWidth: single;
  AStroker: TBGRACustomPenStroker);
begin
  AddPolygon(AStroker.ComputePolygon(points,AWidth), AColor);
end;

procedure TBGRAMultishapeFiller.AddPolygonStroke(
  const points: array of TPointF; ATexture: IBGRAScanner; AWidth: single;
  AStroker: TBGRACustomPenStroker);
begin
  AddPolygon(AStroker.ComputePolygon(points,AWidth), ATexture);
end;

procedure TBGRAMultishapeFiller.AddTriangleLinearColor(pt1, pt2, pt3: TPointF; c1, c2,
  c3: TBGRAPixel);
var grad: TBGRAGradientTriangleScanner;
begin
  if (c1 = c2) and (c2 = c3) then
    AddPolygon([pt1,pt2,pt3],c1)
  else
  begin
    grad := TBGRAGradientTriangleScanner.Create(pt1,pt2,pt3, c1,c2,c3);
    AddShape(TOnePassFillPolyInfo.Create([pt1,pt2,pt3]),True,grad,grad,BGRAPixelTransparent);
  end;
end;

procedure TBGRAMultishapeFiller.AddTriangleLinearMapping(pt1, pt2,
  pt3: TPointF; texture: IBGRAScanner; tex1, tex2, tex3: TPointF);
var
  mapping: TBGRATriangleLinearMapping;
begin
  mapping := TBGRATriangleLinearMapping.Create(texture, pt1,pt2,pt3, tex1, tex2, tex3);
  AddShape(TOnePassFillPolyInfo.Create([pt1,pt2,pt3]),True,mapping,mapping,BGRAPixelTransparent);
end;

procedure TBGRAMultishapeFiller.AddQuadLinearColor(pt1, pt2, pt3, pt4: TPointF;
  c1, c2, c3, c4: TBGRAPixel);
var
  center: TPointF;
  centerColor: TBGRAPixel;
begin
  if (c1 = c2) and (c2 = c3) and (c3 = c4) then
    AddPolygon([pt1,pt2,pt3,pt4],c1)
  else
  begin
    center := (pt1+pt2+pt3+pt4)*(1/4);
    centerColor := GammaCompression( MergeBGRA(MergeBGRA(GammaExpansion(c1),GammaExpansion(c2)),
                      MergeBGRA(GammaExpansion(c3),GammaExpansion(c4))) );
    AddTriangleLinearColor(pt1,pt2,center, c1,c2,centerColor);
    AddTriangleLinearColor(pt2,pt3,center, c2,c3,centerColor);
    AddTriangleLinearColor(pt3,pt4,center, c3,c4,centerColor);
    AddTriangleLinearColor(pt4,pt1,center, c4,c1,centerColor);
  end;
end;

procedure TBGRAMultishapeFiller.AddQuadLinearMapping(pt1, pt2, pt3,
  pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF;
  ACulling: TFaceCulling);
var
  mapping: TBGRAQuadLinearScanner;
begin
  mapping := TBGRAQuadLinearScanner.Create(texture,
    [tex1,tex2,tex3,tex4],
    [pt1,pt2,pt3,pt4]);
  mapping.Culling := ACulling;
  AddShape(TOnePassFillPolyInfo.Create([pt1,pt2,pt3,pt4]),True,mapping,mapping,BGRAPixelTransparent);
end;

procedure TBGRAMultishapeFiller.AddQuadPerspectiveMapping(pt1, pt2, pt3,
  pt4: TPointF; texture: IBGRAScanner; tex1, tex2, tex3, tex4: TPointF);
var persp: TBGRAPerspectiveScannerTransform;
begin
  persp := TBGRAPerspectiveScannerTransform.Create(texture,[tex1,tex2,tex3,tex4],[pt1,pt2,pt3,pt4]);
  AddShape(TOnePassFillPolyInfo.Create([pt1,pt2,pt3,pt4]),True,persp,persp,BGRAPixelTransparent);
end;

procedure TBGRAMultishapeFiller.AddEllipse(x, y, rx, ry: single; AColor: TBGRAPixel
  );
begin
  AddShape(TFillEllipseInfo.Create(x,y,rx,ry),True,nil,nil,AColor);
end;

procedure TBGRAMultishapeFiller.AddEllipse(x, y, rx, ry: single;
  ATexture: IBGRAScanner);
begin
  AddShape(TFillEllipseInfo.Create(x,y,rx,ry),True,ATexture,nil,BGRAPixelTransparent);
end;

procedure TBGRAMultishapeFiller.AddEllipseBorder(x, y, rx, ry, w: single;
  AColor: TBGRAPixel);
begin
  AddShape(TFillBorderEllipseInfo.Create(x,y,rx,ry,w),True,nil,nil,AColor);
end;

procedure TBGRAMultishapeFiller.AddEllipseBorder(x, y, rx, ry, w: single;
  ATexture: IBGRAScanner);
begin
  AddShape(TFillBorderEllipseInfo.Create(x,y,rx,ry,w),True,ATexture,nil,BGRAPixelTransparent);
end;

procedure TBGRAMultishapeFiller.AddRoundRectangle(x1, y1, x2, y2, rx, ry: single;
  AColor: TBGRAPixel; options: TRoundRectangleOptions);
begin
  AddShape(TFillRoundRectangleInfo.Create(x1, y1, x2, y2, rx, ry,options),True,nil,nil,AColor);
end;

procedure TBGRAMultishapeFiller.AddRoundRectangle(x1, y1, x2, y2, rx, ry: single;
  ATexture: IBGRAScanner; options: TRoundRectangleOptions);
begin
  AddShape(TFillRoundRectangleInfo.Create(x1, y1, x2, y2, rx, ry,options),True,
     ATexture,nil,BGRAPixelTransparent);
end;

procedure TBGRAMultishapeFiller.AddRoundRectangleBorder(x1, y1, x2, y2, rx,
  ry, w: single; AColor: TBGRAPixel; options: TRoundRectangleOptions);
begin
  AddShape(TFillBorderRoundRectInfo.Create(x1, y1, x2, y2, rx, ry,w,options),True,
    nil,nil,AColor);
end;

procedure TBGRAMultishapeFiller.AddRoundRectangleBorder(x1, y1, x2, y2, rx, ry,
  w: single; ATexture: IBGRAScanner; options: TRoundRectangleOptions);
begin
  AddShape(TFillBorderRoundRectInfo.Create(x1, y1, x2, y2, rx, ry,w,options),True,
    ATexture,nil,BGRAPixelTransparent);
end;

procedure TBGRAMultishapeFiller.AddRectangle(x1, y1, x2, y2: single;
  AColor: TBGRAPixel);
begin
  AddPolygon([PointF(x1,y1),PointF(x2,y1),PointF(x2,y2),PointF(x1,y2)],AColor);
end;

procedure TBGRAMultishapeFiller.AddRectangle(x1, y1, x2, y2: single;
  ATexture: IBGRAScanner);
begin
  AddPolygon([PointF(x1,y1),PointF(x2,y1),PointF(x2,y2),PointF(x1,y2)],ATexture);
end;

procedure TBGRAMultishapeFiller.AddRectangleBorder(x1, y1, x2, y2,
  w: single; AColor: TBGRAPixel);
var hw : single;
begin
  hw := w/2;
  if not CheckRectangleBorderBounds(x1,y1,x2,y2,w) then
    AddRectangle(x1-hw,y1-hw,x2+hw,y2+hw,AColor) else
    AddPolygon([PointF(x1-hw,y1-hw),PointF(x2+hw,y1-hw),PointF(x2+hw,y2+hw),PointF(x1-hw,y2+hw),EmptyPointF,
                PointF(x1+hw,y2-hw),PointF(x2-hw,y2-hw),PointF(x2-hw,y1+hw),PointF(x1+hw,y1+hw)],AColor);
end;

procedure TBGRAMultishapeFiller.AddRectangleBorder(x1, y1, x2, y2,
  w: single; ATexture: IBGRAScanner);
var hw : single;
begin
  hw := w/2;
  if not CheckRectangleBorderBounds(x1,y1,x2,y2,w) then
    AddRectangle(x1-hw,y1-hw,x2+hw,y2+hw,ATexture) else
    AddPolygon([PointF(x1-hw,y1-hw),PointF(x2+hw,y1-hw),PointF(x2+hw,y2+hw),PointF(x1-hw,y2+hw),EmptyPointF,
                PointF(x1+hw,y2-hw),PointF(x2-hw,y2-hw),PointF(x2-hw,y1+hw),PointF(x1+hw,y1+hw)],ATexture);
end;

procedure TBGRAMultishapeFiller.Draw(dest: TBGRACustomBitmap; ADrawMode: TDrawMode = dmDrawWithTransparency);
var
  shapeRow: array of record
    density: PDensity;
    densMinx,densMaxx: integer;
    nbInter: integer;
    inter: array of TIntersectionInfo;
  end;
  shapeRowsList: array of integer;
  NbShapeRows: integer;
  miny, maxy, minx, maxx,
  rowminx, rowmaxx: integer;

  procedure SubstractScanlines(src,dest: integer);
  var i: integer;

    procedure SubstractSegment(srcseg: integer);
    var x1,x2, x3,x4: single;
      j: integer;

      procedure AddSegment(xa,xb: single);
      var nb: PInteger;
      begin
        nb := @shapeRow[dest].nbinter;
        if length(shapeRow[dest].inter) < nb^+2 then
          setlength(shapeRow[dest].inter, nb^*2+2);
        with shapeRow[dest] do
        begin
          if inter[nb^] = nil then inter[nb^] := shapes[dest].info.CreateIntersectionInfo;
          inter[nb^].interX := xa;
          if inter[nb^+1] = nil then inter[nb^+1] := shapes[dest].info.CreateIntersectionInfo;
          inter[nb^+1].interX := xb;
        end;
        inc(nb^,2);
      end;

    begin
      x1 := shapeRow[src].inter[(srcseg-1)*2].interX;
      x2 := shapeRow[src].inter[srcseg*2-1].interX;
      for j := shapeRow[dest].nbInter div 2 downto 1 do
      begin
        x3 := shapeRow[dest].inter[(j-1)*2].interX;
        x4 := shapeRow[dest].inter[j*2-1].interX;
        if (x2 <= x3) or (x1 >= x4) then continue; //not overlapping
        if (x1 <= x3) and (x2 >= x4) then
          shapeRow[dest].inter[j*2-1].interX := x3 //empty
        else
        if (x1 <= x3) and (x2 < x4) then
          shapeRow[dest].inter[(j-1)*2].interX := x2 //remove left part
        else
        if (x1 > x3) and (x2 >= x4) then
          shapeRow[dest].inter[j*2-1].interX := x1 else //remove right part
        begin
          //[x1,x2] is inside [x3,x4]
          shapeRow[dest].inter[j*2-1].interX := x1; //left part
          AddSegment(x2,x4);
        end;
      end;
    end;

  begin
    for i := 1 to shapeRow[src].nbInter div 2 do
      SubstractSegment(i);
  end;

var
    AliasingOfs: TPointF;
    useAA: boolean;

  procedure AddOneLineDensity(cury: single);
  var
    i,k: integer;
    ix1,ix2: integer;
    x1,x2: single;
  begin
    for k := 0 to NbShapeRows-1 do
      with shapeRow[shapeRowsList[k]], shapes[shapeRowsList[k]] do
      begin
        //find intersections
        info.ComputeAndSort(cury, inter, nbInter, FillMode=fmWinding);
        nbInter := nbInter and not 1; //even
      end;

      case PolygonOrder of
        poLastOnTop: begin
          for k := 1 to NbShapeRows-1 do
            if shapeRow[shapeRowsList[k]].nbInter > 0 then
              for i := 0 to k-1 do
                SubstractScanlines(shapeRowsList[k],shapeRowsList[i]);
        end;
        poFirstOnTop: begin
          for k := 0 to NbShapeRows-2 do
            if shapeRow[shapeRowsList[k]].nbInter > 0 then
              for i := k+1 to NbShapeRows-1 do
                SubstractScanlines(shapeRowsList[k],shapeRowsList[i]);
        end;
      end;

      for k := 0 to NbShapeRows-1 do
      with shapeRow[shapeRowsList[k]] do
      begin
        //fill density
        if not useAA then
        begin
          for i := 0 to nbinter div 2 - 1 do
          begin
            x1 := inter[i + i].interX;
            x2 := inter[i + i + 1].interX;
            ComputeAliasedRowBounds(x1+AliasingOfs.X,x2+AliasingOfs.X,minx,maxx,ix1,ix2);

            if ix1 < densMinx then densMinx := ix1;
            if ix2 > densMaxx then densMaxx := ix2;

            FillWord(density[ix1-minx],ix2-ix1+1,256);
          end;
        end else
		  {$DEFINE INCLUDE_FILLDENSITY}
          {$i density256.inc}
      end;

      for k := 0 to NbShapeRows-1 do
      with shapeRow[shapeRowsList[k]] do
      begin
        if densMinX < rowminx then rowminx := densMinX;
        if densMaxX > rowmaxx then rowmaxx := densMaxX;
      end;
  end;

type
    TCardinalSum = record
          sumR,sumG,sumB,sumA: cardinal;
        end;

var
  MultiEmpty: boolean;
  bounds: TRect;

  xb, yb, yc, j,k: integer;
  pdest:    PBGRAPixel;

  curSum,nextSum: ^TCardinalSum;
  sums: array of TCardinalSum;

  pdens: PDensity;
  w: cardinal;
  ec: TExpandedPixel;
  count: integer;
  ScanNextFunc: function: TBGRAPixel of object;

begin
  if nbShapes = 0 then exit;
  useAA := Antialiasing and (ADrawMode in [dmDrawWithTransparency,dmLinearBlend]);
  if nbShapes = 1 then
  begin
    if useAA then
      FillShapeAntialias(dest,shapes[0].info,GammaCompression(shapes[0].color),False,shapes[0].texture,FillMode = fmWinding, ADrawMode=dmLinearBlend) else
      FillShapeAliased(dest,shapes[0].info,GammaCompression(shapes[0].color),False,shapes[0].texture,FillMode = fmWinding, ADrawMode,
        AliasingIncludeBottomRight);
    exit;
  end;
  bounds := Rect(0,0,0,0);
  MultiEmpty := True;
  for k := 0 to nbShapes-1 do
  begin
    If BGRAShapeComputeMinMax(shapes[k].info,minx,miny,maxx,maxy,dest) then
    begin
      shapes[k].bounds := rect(minx,miny,maxx+1,maxy+1);
      if MultiEmpty then
      begin
        MultiEmpty := False;
        bounds := shapes[k].bounds;
      end else
      begin
        if minx < bounds.left then bounds.left := minx;
        if miny < bounds.top then bounds.top := miny;
        if maxx >= bounds.right then bounds.right := maxx+1;
        if maxy >= bounds.bottom then bounds.bottom := maxy+1;
      end;
    end else
      shapes[k].bounds := rect(0,0,0,0);
  end;
  if MultiEmpty then exit;
  minx := bounds.left;
  miny := bounds.top;
  maxx := bounds.right-1;
  maxy := bounds.bottom-1;

  setlength(shapeRow, nbShapes);
  for k := 0 to nbShapes-1 do
  begin
    shapeRow[k].inter := shapes[k].info.CreateIntersectionArray;
    getmem(shapeRow[k].density, (maxx - minx + 2)*sizeof(TDensity)); //more for safety
  end;

  if AliasingIncludeBottomRight then
    AliasingOfs := PointF(0,0) else
    AliasingOfs := PointF(-0.0001,-0.0001);

  setlength(sums,maxx-minx+2); //more for safety
  setlength(shapeRowsList, nbShapes);

  //vertical scan
  for yb := miny to maxy do
  begin
    rowminx := maxx+1;
    rowmaxx := minx-1;

    //init shape rows
    NbShapeRows := 0;
    for k := 0 to nbShapes-1 do
    if (yb >= shapes[k].bounds.top) and (yb < shapes[k].bounds.Bottom) then
    begin
      shapeRowsList[NbShapeRows] := k;
      inc(NbShapeRows);

      fillchar(shapeRow[k].density^,(maxx-minx+1)*sizeof(TDensity),0);
      shapeRow[k].densMinx := maxx+1;
      shapeRow[k].densMaxx := minx-1;
    end;

    If useAA then
    begin
      //precision scan
      for yc := 0 to AntialiasPrecision - 1 do
        AddOneLineDensity( yb + (yc * 2 + 1) / (AntialiasPrecision * 2) );
    end else
    begin
      AddOneLineDensity( yb + 0.5 - AliasingOfs.Y );
    end;

    rowminx := minx;
    rowmaxx := maxx;
    if rowminx <= rowmaxx then
    begin
      if rowminx < minx then rowminx := minx;
      if rowmaxx > maxx then rowmaxx := maxx;

      FillChar(sums[rowminx-minx],(rowmaxx-rowminx+1)*sizeof(sums[0]),0);

      if useAA then
        {$define PARAM_ANTIALIASINGFACTOR}
        {$i multishapeline.inc}
      else
        {$i multishapeline.inc};

      pdest := dest.ScanLine[yb] + rowminx;
      xb := rowminx;
      nextSum := @sums[xb-minx];
      case ADrawMode of
        dmDrawWithTransparency:
          while xb <= rowmaxx do
          begin
            curSum := nextSum;
            inc(nextSum);
            with curSum^ do
            begin
              if sumA <> 0 then
              begin
                ec.red := (sumR+sumA shr 1) div sumA;
                ec.green := (sumG+sumA shr 1) div sumA;
                ec.blue := (sumB+sumA shr 1) div sumA;
                if sumA > 255 then sumA := 255;
                ec.alpha := sumA shl 8 + sumA;
                count := 1;
                while (xb < rowmaxx) and (nextSum^.sumA = sumA) and (nextSum^.sumB = sumB)
                  and (nextSum^.sumG = sumG) and (nextSum^.sumR = sumR) do
                begin
                  inc(xb);
                  inc(nextSum);
                  inc(count);
                end;
                if count = 1 then
                  DrawExpandedPixelInlineNoAlphaCheck(pdest,ec,sumA) else
                   DrawExpandedPixelsInline(pdest, ec, count );
                inc(pdest,count-1);
              end;
            end;
            inc(xb);
            inc(pdest);
          end;

        dmLinearBlend:
          while xb <= rowmaxx do
          begin
            curSum := nextSum;
            inc(nextSum);
            with curSum^ do
            begin
              if sumA <> 0 then
              begin
                ec.red := (sumR+sumA shr 1) div sumA;
                ec.green := (sumG+sumA shr 1) div sumA;
                ec.blue := (sumB+sumA shr 1) div sumA;
                if sumA > 255 then sumA := 255;
                ec.alpha := sumA shl 8 + sumA;
                count := 1;
                while (xb < rowmaxx) and (nextSum^.sumA = sumA) and (nextSum^.sumB = sumB)
                  and (nextSum^.sumG = sumG) and (nextSum^.sumR = sumR) do
                begin
                  inc(xb);
                  inc(nextSum);
                  inc(count);
                end;
                if count = 1 then
                  DrawPixelInlineNoAlphaCheck(pdest,GammaCompression(ec)) else
                   DrawPixelsInline(pdest, GammaCompression(ec), count );
                inc(pdest,count-1);
              end;
            end;
            inc(xb);
            inc(pdest);
          end;

        dmXor:
          while xb <= rowmaxx do
          begin
            curSum := nextSum;
            inc(nextSum);
            with curSum^ do
            begin
              if sumA <> 0 then
              begin
                ec.red := (sumR+sumA shr 1) div sumA;
                ec.green := (sumG+sumA shr 1) div sumA;
                ec.blue := (sumB+sumA shr 1) div sumA;
                if sumA > 255 then sumA := 255;
                ec.alpha := sumA shl 8 + sumA;
                count := 1;
                while (xb < rowmaxx) and (nextSum^.sumA = sumA) and (nextSum^.sumB = sumB)
                  and (nextSum^.sumG = sumG) and (nextSum^.sumR = sumR) do
                begin
                  inc(xb);
                  inc(nextSum);
                  inc(count);
                end;
                XorInline(pdest,GammaCompression(ec),count);
                inc(pdest,count-1);
              end;
            end;
            inc(xb);
            inc(pdest);
          end;

        dmSet:
          while xb <= rowmaxx do
          begin
            curSum := nextSum;
            inc(nextSum);
            with curSum^ do
            begin
              if sumA <> 0 then
              begin
                ec.red := (sumR+sumA shr 1) div sumA;
                ec.green := (sumG+sumA shr 1) div sumA;
                ec.blue := (sumB+sumA shr 1) div sumA;
                if sumA > 255 then sumA := 255;
                ec.alpha := sumA shl 8 + sumA;
                count := 1;
                while (xb < rowmaxx) and (nextSum^.sumA = sumA) and (nextSum^.sumB = sumB)
                  and (nextSum^.sumG = sumG) and (nextSum^.sumR = sumR) do
                begin
                  inc(xb);
                  inc(nextSum);
                  inc(count);
                end;
                FillInline(pdest,GammaCompression(ec),count);
                inc(pdest,count-1);
              end;
            end;
            inc(xb);
            inc(pdest);
          end;

        dmSetExceptTransparent:
          while xb <= rowmaxx do
          begin
            curSum := nextSum;
            inc(nextSum);
            with curSum^ do
            begin
              if sumA >= 255 then
              begin
                ec.red := (sumR+sumA shr 1) div sumA;
                ec.green := (sumG+sumA shr 1) div sumA;
                ec.blue := (sumB+sumA shr 1) div sumA;
                if sumA > 255 then sumA := 255;
                ec.alpha := sumA shl 8 + sumA;
                count := 1;
                while (xb < rowmaxx) and (nextSum^.sumA = sumA) and (nextSum^.sumB = sumB)
                  and (nextSum^.sumG = sumG) and (nextSum^.sumR = sumR) do
                begin
                  inc(xb);
                  inc(nextSum);
                  inc(count);
                end;
                FillInline(pdest,GammaCompression(ec),count);
                inc(pdest,count-1);
              end;
            end;
            inc(xb);
            inc(pdest);
          end;

      end;
    end;

  end;

  for k := 0 to nbShapes-1 do
  begin
    freemem(shapeRow[k].density);
    shapes[k].info.FreeIntersectionArray(shapeRow[k].inter);
  end;

  dest.InvalidateBitmap;
end;

procedure FillRoundRectangleAntialias(bmp: TBGRACustomBitmap; x1, y1, x2, y2,
  rx, ry: single; options: TRoundRectangleOptions; c: TBGRAPixel; EraseMode: boolean; LinearBlend: boolean);
var
  info: TFillRoundRectangleInfo;
begin
  if (x1 = x2) or (y1 = y2) then exit;
  info := TFillRoundRectangleInfo.Create(x1, y1, x2, y2, rx, ry, options);
  FillShapeAntialias(bmp, info, c, EraseMode,nil, False, LinearBlend);
  info.Free;
end;

procedure FillRoundRectangleAntialiasWithTexture(bmp: TBGRACustomBitmap; x1,
  y1, x2, y2, rx, ry: single; options: TRoundRectangleOptions;
  scan: IBGRAScanner; LinearBlend: boolean);
var
  info: TFillRoundRectangleInfo;
begin
  if (x1 = x2) or (y1 = y2) then exit;
  info := TFillRoundRectangleInfo.Create(x1, y1, x2, y2, rx, ry, options);
  FillShapeAntialiasWithTexture(bmp, info, scan, False, LinearBlend);
  info.Free;
end;

procedure BorderRoundRectangleAntialias(bmp: TBGRACustomBitmap; x1, y1, x2,
  y2, rx, ry, w: single; options: TRoundRectangleOptions; c: TBGRAPixel;
  EraseMode: boolean; LinearBlend: boolean);
var
  info: TFillShapeInfo;
  oldLinear: boolean;
begin
  if w=0 then exit;
  if ((rx=0) or (ry=0)) and not EraseMode then
  begin
    oldLinear := bmp.LinearAntialiasing;
    bmp.LinearAntialiasing := LinearBlend;
    bmp.RectangleAntialias(x1,y1,x2,y2,c,w);
    bmp.LinearAntialiasing := oldLinear;
    exit;
  end;
  info := TFillBorderRoundRectInfo.Create(x1, y1, x2,y2, rx, ry, w, options);
  FillShapeAntialias(bmp, info, c, EraseMode, nil, False, LinearBlend);
  info.Free;
end;

procedure BorderRoundRectangleAntialiasWithTexture(bmp: TBGRACustomBitmap; x1,
  y1, x2, y2, rx, ry, w: single; options: TRoundRectangleOptions;
  scan: IBGRAScanner; LinearBlend: boolean);
var
  info: TFillBorderRoundRectInfo;
  oldLinear: Boolean;
begin
  if w=0 then exit;
  if (rx=0) or (ry=0) then
  begin
    oldLinear := bmp.LinearAntialiasing;
    bmp.LinearAntialiasing := LinearBlend;
    bmp.RectangleAntialias(x1,y1,x2,y2,scan,w);
    bmp.LinearAntialiasing := oldLinear;
    exit;
  end;
  info := TFillBorderRoundRectInfo.Create(x1, y1, x2,y2, rx, ry, w, options);
  FillShapeAntialiasWithTexture(bmp, info, scan, False, LinearBlend);
  info.Free;
end;

procedure BorderAndFillRoundRectangleAntialias(bmp: TBGRACustomBitmap; x1, y1,
  x2, y2, rx, ry, w: single; options: TRoundRectangleOptions; bordercolor,
  fillcolor: TBGRAPixel; bordertexture,filltexture: IBGRAScanner; EraseMode: boolean);
var
  info: TFillBorderRoundRectInfo;
  multi: TBGRAMultishapeFiller;
begin
  if (rx = 0) or (ry = 0) then exit;
  info := TFillBorderRoundRectInfo.Create(x1, y1, x2,y2, rx, ry, w, options);
  if not EraseMode then
  begin
    multi := TBGRAMultishapeFiller.Create;
    if filltexture <> nil then
      multi.AddShape(info.innerBorder, filltexture) else
      multi.AddShape(info.innerBorder, fillcolor);
    if w<>0 then
    begin
      if bordertexture <> nil then
        multi.AddShape(info, bordertexture) else
        multi.AddShape(info, bordercolor);
    end;
    multi.Draw(bmp);
    multi.Free;
  end else
  begin
    FillShapeAntialias(bmp, info.innerBorder, fillcolor, EraseMode, nil, False, False);
    FillShapeAntialias(bmp, info, bordercolor, EraseMode, nil, False, False);
  end;
  info.Free;
end;

end.
