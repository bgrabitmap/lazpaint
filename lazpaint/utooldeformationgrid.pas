unit utooldeformationgrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, Types, Math, SysUtils, utool, BGRABitmapTypes, BGRABitmap;

type

  { TToolDeformationGrid }

  TToolDeformationGrid = class(TGenericTool)
  private
    FPreviousBounds,FCurrentBounds,FMergedBounds: TRect;
    procedure ReleaseGrid;
    procedure ToolDeformationGridNeeded;
    procedure ValidateDeformationGrid;
  protected
    deformationGridNbX,deformationGridNbY,deformationGridX,deformationGridY: integer;
    deformationGridMoving: boolean;
    deformationOrigin: TPointF;
    deformationGridBackup: TBGRABitmap;
    deformationGrid: array of array of TPointF;
    deformationGridTexCoord: array of array of TPointF;
    function DoToolDown(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF;
      rightBtn: boolean): TRect; override;
    function DoToolMove(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF): Trect;
      override;
    function GetIsSelectingTool: boolean; override;
  public
    function ToolKeyDown(key: Word): TRect; override;
    function ToolUp: TRect; override;
    procedure Render(VirtualScreen: TBGRABitmap; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction); override;
    procedure BeforeGridSizeChange; override;
    procedure AfterGridSizeChange(NewNbX,NewNbY: Integer); override;
    destructor Destroy; override;
  end;

  { TToolTextureMapping }

  TToolTextureMapping = class(TGenericTool)
  private
    FPreviousBounds,FCurrentBounds,FMergedBounds: TRect;
    procedure ToolQuadNeeded;
    procedure ValidateQuad;
    procedure DrawQuad;

  protected
    quadDefined: boolean;
    quad: array[0..3] of TPointF;
    quadMovingIndex: integer;
    quadMoving: boolean;
    quadMovingOrigin: TPointF;
    quadBackup: TBGRABitmap;
    function DoToolDown(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF;
      rightBtn: boolean): TRect; override;
    function DoToolMove(toolDest: TBGRABitmap; pt: TPoint; ptF: TPointF): TRect;
      override;
    function GetIsSelectingTool: boolean; override;
  public
    constructor Create(AManager: TToolManager); override;
    function ToolKeyDown(key: Word): TRect; override;
    function ToolUp: TRect; override;
    procedure Render(VirtualScreen: TBGRABitmap; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction); override;
    destructor Destroy; override;
  end;

implementation

uses LCLType, ugraph, uscaledpi, LazPaintType, BGRAFillInfo;

{ TToolTextureMapping }

procedure TToolTextureMapping.ToolQuadNeeded;
var
  tx,ty: single;
  ratio,temp: single;
begin
  if not quadDefined and (Manager.ToolTexture <> nil) and (Manager.ToolTexture.Width > 1) and (Manager.ToolTexture.Height > 1) then
  begin
    tx := Manager.ToolTexture.Width;
    ty := Manager.ToolTexture.Height;
    ratio := 1;
    if tx > Manager.Image.Width then
      ratio := Manager.Image.Width/tx;
    if ty > Manager.Image.Height then
    begin
      temp := Manager.Image.Height/ty;
      if temp < ratio then ratio := temp;
    end;
    if ratio > 0 then
    begin
      quad[0] := PointF((Manager.Image.Width-tx*ratio)/2,(Manager.Image.Width-ty*ratio)/2);
      quad[1] := PointF((Manager.Image.Width+tx*ratio-1)/2,(Manager.Image.Width-ty*ratio)/2);
      quad[2] := PointF((Manager.Image.Width+tx*ratio-1)/2,(Manager.Image.Width+ty*ratio-1)/2);
      quad[3] := PointF((Manager.Image.Width-tx*ratio)/2,(Manager.Image.Width+ty*ratio-1)/2);
      quadDefined:= true;
      quadBackup := GetToolDrawingLayer.Duplicate as TBGRABitmap;
      DrawQuad;
    end;
  end;
end;

procedure TToolTextureMapping.ValidateQuad;
begin
  if quadDefined then
  begin
    FreeAndNil(quadBackup);
    quadDefined := false;
    Manager.Image.SaveLayerOrSelectionUndo;
  end;
end;

procedure TToolTextureMapping.DrawQuad;
begin
  if quadDefined then
  begin
    FPreviousBounds := FCurrentBounds;
    FCurrentBounds := GetShapeBounds([quad[0],quad[1],quad[2],quad[3]],1);
    FMergedBounds := RectUnion(FPreviousBounds,FCurrentBounds);
    GetToolDrawingLayer.FillQuadPerspectiveMappingAntialias(quad[0],quad[1],quad[2],quad[3],Manager.ToolTexture,PointF(0,0),PointF(manager.ToolTexture.Width-1,0),
      PointF(manager.ToolTexture.Width-1,manager.ToolTexture.Height-1),PointF(0,manager.ToolTexture.Height-1));
    Manager.Image.ImageMayChange(FMergedBounds);
  end;
end;

function TToolTextureMapping.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
var
  n: Integer;
  curDist,minDist: single;
begin
  ToolQuadNeeded;
  if not quadDefined then exit;

  result := EmptyRect;
  minDist := sqr(DoScaleX(10,OriginalDPI));
  for n := 0 to high(quad) do
  begin
    curDist := sqr(ptF.x-quad[n].x)+sqr(ptF.y-quad[n].y);
    if curDist < minDist then
    begin
      minDist := curDist;
      quadMovingIndex := n;
      quadMovingOrigin := ptF;
      quadMoving := True;
    end;
  end;

  if not quadMoving and IsPointInPolygon(quad, ptF, true) then
  begin
    quadMovingIndex := -1;
    quadMovingOrigin := ptF;
    quadMoving := true;
  end;

end;

function TToolTextureMapping.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var n: integer;
begin
  result := EmptyRect;
  if quadMoving then
  begin
    if quadMovingIndex = -1 then
    begin
      for n := 0 to high(quad) do
        quad[n] += ptF-quadMovingOrigin;
    end
    else
      quad[quadMovingIndex] += ptF-quadMovingOrigin;
    quadMovingOrigin := ptF;
    toolDest.PutImage(0,0,quadBackup,dmSet);
    DrawQuad;
    result := FMergedBounds;
  end;
end;

function TToolTextureMapping.GetIsSelectingTool: boolean;
begin
  Result:= false;
end;

constructor TToolTextureMapping.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  FCurrentBounds := EmptyRect;
  ToolQuadNeeded;
end;

function TToolTextureMapping.ToolKeyDown(key: Word): TRect;
begin
  result := EmptyRect;
  if Key = VK_RETURN then
  begin
    if quadDefined then
    begin
      ValidateQuad;
      result := EmptyRect;
      manager.QueryExitTool;
    end;
  end;
end;

function TToolTextureMapping.ToolUp: TRect;
begin
  if quadMoving then quadMoving := false;
  result := EmptyRect;
end;

procedure TToolTextureMapping.Render(VirtualScreen: TBGRABitmap;
  BitmapToVirtualScreen: TBitmapToVirtualScreenFunction);
var curPt,nextPt: TPointF;
    n: Integer;
begin
  if not quadDefined then exit;
  For n := 0 to high(quad) do
  begin
    curPt := BitmapToVirtualScreen(quad[n]);
    nextPt := BitmapToVirtualScreen(quad[(n+1)mod length(quad)]);
    NiceLine(VirtualScreen, curPt.X,curPt.Y,nextPt.x,nextPt.y);
  end;
  For n := 0 to high(quad) do
  begin
    curPt := BitmapToVirtualScreen(quad[n]);
    NicePoint(VirtualScreen, curPt.X,curPt.Y);
  end;
end;

destructor TToolTextureMapping.Destroy;
begin
  ValidateQuad;
  inherited Destroy;
end;

{ TToolDeformationGrid }

procedure TToolDeformationGrid.ToolDeformationGridNeeded;
var xb,yb: integer;
    layer: TBGRABitmap;
begin
  if DeformationGrid = nil then
  begin
    layer := GetToolDrawingLayer;
    setlength(DeformationGrid,Manager.ToolDeformationGridNbY,Manager.ToolDeformationGridNbX);
    setlength(DeformationGridTexCoord,Manager.ToolDeformationGridNbY,Manager.ToolDeformationGridNbX);
    for yb := 0 to Manager.ToolDeformationGridNbY-1 do
      for xb := 0 to Manager.ToolDeformationGridNbX-1 do
      begin
        DeformationGridTexCoord[yb,xb] := PointF(xb/(Manager.ToolDeformationGridNbX-1)*layer.Width-0.5,
                                                     yb/(Manager.ToolDeformationGridNbY-1)*layer.Height-0.5);
        DeformationGrid[yb,xb] :=DeformationGridTexCoord[yb,xb];
      end;
  end;
end;

procedure TToolDeformationGrid.BeforeGridSizeChange;
begin
  ReleaseGrid;
  DeformationGrid := nil;
  DeformationGridTexCoord := nil;
end;

{$hints off}
procedure TToolDeformationGrid.AfterGridSizeChange(NewNbX,NewNbY: Integer);
begin
  //grid will be created when needed
end;
{$hints on}

destructor TToolDeformationGrid.Destroy;
begin
  ValidateDeformationGrid;
  inherited Destroy;
end;

procedure TToolDeformationGrid.ReleaseGrid;
var
  xb,yb: Integer;
begin
  if deformationGridBackup <> nil then
  begin
    Manager.Image.SaveLayerOrSelectionUndo;
    FreeAndNil(deformationGridBackup);
    for yb := 0 to Manager.ToolDeformationGridNbY-2 do
      for xb := 0 to Manager.ToolDeformationGridNbX-2 do
        DeformationGridTexCoord[yb,xb] := DeformationGrid[yb,xb];
  end;
end;

procedure TToolDeformationGrid.ValidateDeformationGrid;
begin
  if deformationGridBackup <> nil then
  begin
    DeformationGrid := nil;
    DeformationGridTexCoord := nil;
    FreeAndNil(deformationGridBackup);
    Manager.Image.ImageMayChange(FMergedBounds);
    Manager.Image.SaveLayerOrSelectionUndo;
  end;
end;

function TToolDeformationGrid.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
var
  yb,xb: Integer;
  curDist,minDist: single;
begin
  result := EmptyRect;
  minDist := sqr(DoScaleX(10,OriginalDPI));
  deformationGridX := 1;
  deformationGridY := 1;
  if DeformationGrid <> nil then
  begin
    for yb := 1 to Manager.ToolDeformationGridNbY-2 do
      for xb := 1 to Manager.ToolDeformationGridNbX-2 do
      begin
        curDist := sqr(ptF.x-DeformationGrid[yb,xb].x)+sqr(ptF.y-DeformationGrid[yb,xb].y);
        if curDist < minDist then
        begin
          minDist := curDist;
          deformationGridX := xb;
          deformationGridY := yb;
          deformationGridMoving := True;
          deformationOrigin := ptF;
        end;
      end;
  end;
end;

function TToolDeformationGrid.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var xb,yb,NbX,NbY: integer;
    gridDone: array of array of boolean;
    layer : TBGRABitmap;
    PreviousClipRect: TRect;

  procedure AddToDeformationArea(xi,yi: integer);
  var ptF: TPointF;
      pix: TRect;
  begin
    if (xi >= 0) and (yi >= 0) and (xi < NbX) and (yi < NbY) then
    begin
      ptF := deformationGrid[yi,xi];
      pix := rect(floor(ptF.X)-1,floor(ptF.Y)-1,ceil(ptF.X)+2,ceil(ptF.Y)+2);
      if IsRectEmpty(FCurrentBounds) then
        FCurrentBounds := pix
      else
        UnionRect(FCurrentBounds,FCurrentBounds,pix);
    end;
  end;

begin
  result := EmptyRect;

  if not deformationGridMoving then exit;
  if Manager.ToolDeformationGridMoveWithoutDeformation then
  begin
    ReleaseGrid;
    DeformationGrid[deformationGridY,deformationGridX] := PointF(
      DeformationGrid[deformationGridY,deformationGridX].X + ptF.X-deformationOrigin.X,
      DeformationGrid[deformationGridY,deformationGridX].Y + ptF.Y-deformationOrigin.Y);
    DeformationGridTexCoord[deformationGridY,deformationGridX] := DeformationGrid[deformationGridY,deformationGridX];
  end else
  begin
    layer := GetToolDrawingLayer;
    NbX := Manager.ToolDeformationGridNbX;
    NbY := Manager.ToolDeformationGridNbY;
    if deformationGridBackup = nil then
    begin
      deformationGridBackup := layer.Duplicate as TBGRABitmap;
      FCurrentBounds := EmptyRect;
    end;

    DeformationGrid[deformationGridY,deformationGridX] := PointF(
      DeformationGrid[deformationGridY,deformationGridX].X + ptF.X-deformationOrigin.X,
      DeformationGrid[deformationGridY,deformationGridX].Y + ptF.Y-deformationOrigin.Y);

    FPreviousBounds := FCurrentBounds;
    FCurrentBounds := EmptyRect;
    for yb := 0 to NbY-1 do
     for xb := 0 to NbX-1 do
       if DeformationGrid[yb,xb] <> deformationGridTexCoord[yb,xb] then
       begin
         AddToDeformationArea(xb,yb);
         AddToDeformationArea(xb-1,yb);
         AddToDeformationArea(xb,yb-1);
         AddToDeformationArea(xb+1,yb);
         AddToDeformationArea(xb,yb+1);
         AddToDeformationArea(xb-1,yb-1);
         AddToDeformationArea(xb-1,yb+1);
         AddToDeformationArea(xb+1,yb+1);
         AddToDeformationArea(xb+1,yb-1);
       end;
    FMergedBounds := RectUnion(FPreviousBounds,FCurrentBounds);

    //progressive drawing of deformation zones
    setlength(gridDone,NbY-1,NbX-1);
    for yb := 0 to NbY-2 do
      for xb := 0 to NbX-2 do
        gridDone[yb,xb] := false;

    if not IsRectEmpty(FMergedBounds) then
    begin
      PreviousClipRect := layer.ClipRect;
      layer.ClipRect := FMergedBounds;
      layer.FillRect(0,0,layer.Width,layer.Height,BGRAPixelTransparent,dmSet);
      //drawing zones that are not deformed
      for yb := 0 to NbY-2 do
        for xb := 0 to NbX-2 do
          if (DeformationGrid[yb,xb] = DeformationGridTexCoord[yb,xb]) and
             (DeformationGrid[yb,xb+1] = DeformationGridTexCoord[yb,xb+1]) and
             (DeformationGrid[yb+1,xb+1] = DeformationGridTexCoord[yb+1,xb+1]) and
             (DeformationGrid[yb+1,xb] = DeformationGridTexCoord[yb+1,xb]) then
          begin
            layer.FillPoly([DeformationGrid[yb,xb],DeformationGrid[yb,xb+1],
                  DeformationGrid[yb+1,xb+1],DeformationGrid[yb+1,xb]],deformationGridBackup,dmDrawWithTransparency);
            gridDone[yb,xb] := true;
          end;
      //drawing zones that are concave
      for yb := 0 to NbY-2 do
        for xb := 0 to NbX-2 do
          if not gridDone[yb,xb] and
             not IsConvex([DeformationGrid[yb,xb],DeformationGrid[yb,xb+1],
                DeformationGrid[yb+1,xb+1],DeformationGrid[yb+1,xb]]) then
          begin
            layer.FillQuadLinearMapping(DeformationGrid[yb,xb],DeformationGrid[yb,xb+1],
                  DeformationGrid[yb+1,xb+1],DeformationGrid[yb+1,xb],deformationGridBackup,
                  DeformationGridTexCoord[yb,xb],DeformationGridTexCoord[yb,xb+1],DeformationGridTexCoord[yb+1,xb+1],
                  DeformationGridTexCoord[yb+1,xb],true);
            gridDone[yb,xb] := true;
          end;
      //drawing convex zones
      for yb := 0 to NbY-2 do
        for xb := 0 to NbX-2 do
          if not gridDone[yb,xb] then
          layer.FillQuadLinearMapping(DeformationGrid[yb,xb],DeformationGrid[yb,xb+1],
                DeformationGrid[yb+1,xb+1],DeformationGrid[yb+1,xb],deformationGridBackup,
                DeformationGridTexCoord[yb,xb],DeformationGridTexCoord[yb,xb+1],DeformationGridTexCoord[yb+1,xb+1],
                DeformationGridTexCoord[yb+1,xb],true);

      layer.ClipRect := PreviousClipRect;
    end;
  end;
  deformationOrigin := ptF;

  result := FMergedBounds;
end;

function TToolDeformationGrid.GetIsSelectingTool: boolean;
begin
  Result:= false;
end;

function TToolDeformationGrid.ToolKeyDown(key: Word): TRect;
begin
  result := EmptyRect;
  if Key = VK_RETURN then
  begin
    if deformationGridBackup <> nil then
    begin
      ValidateDeformationGrid;
      result := EmptyRect;
      manager.QueryExitTool;
    end;
  end;
end;

function TToolDeformationGrid.ToolUp: TRect;
begin
  deformationGridMoving := false;
  Result:=EmptyRect;
end;

procedure TToolDeformationGrid.Render(VirtualScreen: TBGRABitmap; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction);
var curPt,rightPt,downPt: TPointF;
    xb,yb: Integer;

begin
  ToolDeformationGridNeeded;
  for xb := 0 to Manager.ToolDeformationGridNbX-1 do
    for yb := 0 to Manager.ToolDeformationGridNbY-1 do
    begin
      curPt := BitmapToVirtualScreen(DeformationGrid[yb,xb]);
      if (xb < Manager.ToolDeformationGridNbX-1) and (yb > 0) and (yb < Manager.ToolDeformationGridNbY-1) then
      begin
        rightPt := BitmapToVirtualScreen(DeformationGrid[yb,xb+1]);
        NiceLine(VirtualScreen, curPt.X,curPt.Y, rightPt.X,rightPt.Y);
      end;
      if (yb < Manager.ToolDeformationGridNbY-1) and (xb > 0) and (xb < Manager.ToolDeformationGridNbX-1) then
      begin
        downPt := BitmapToVirtualScreen(DeformationGrid[yb+1,xb]);
        NiceLine(VirtualScreen, curPt.X,curPt.Y, downPt.X,downPt.Y);
      end;
    end;
  for xb := 1 to Manager.ToolDeformationGridNbX-2 do
    for yb := 1 to Manager.ToolDeformationGridNbY-2 do
    begin
      curPt := BitmapToVirtualScreen(DeformationGrid[yb,xb]);
      NicePoint(VirtualScreen, curPt.X,curPt.Y);
    end;
end;

initialization

  RegisterTool(ptDeformation, TToolDeformationGrid);
  RegisterTool(ptTextureMapping, TToolTextureMapping);

end.

