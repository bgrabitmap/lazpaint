// SPDX-License-Identifier: GPL-3.0-only
unit UToolDeformationGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, Types, Math, SysUtils, utool, BGRABitmapTypes, BGRABitmap, UImage,
  UImageType, ULayerAction, LCVectorialFill;

type

  { TToolDeformationGrid }

  TToolDeformationGrid = class(TGenericTool)
  private
    FCurrentBounds,FMergedBounds: TRect;
    procedure ReleaseGrid;
    function ToolDeformationGridNeeded: boolean;
    procedure ValidateDeformationGrid;
  protected
    class var ReturnHintShown: boolean;
    deformationGridNbX,deformationGridNbY,deformationGridX,deformationGridY: integer;
    deformationGridMoving: boolean;
    deformationOrigin: TPointF;
    DoingDeformation: boolean;
    deformationGrid: array of array of TPointF;
    deformationGridTexCoord: array of array of TPointF;
    function GetPointAt(const ptF: TPointF; var x,y: integer): boolean;
    function DoToolDown({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF;
      {%H-}rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF): Trect;
      override;
    function DoToolKeyDown(var key: Word): TRect; override;
    function GetIsSelectingTool: boolean; override;
    function DoToolUpdate({%H-}toolDest: TBGRABitmap): TRect; override;
  public
    class procedure ForgetHintShown;
    constructor Create(AManager: TToolManager); override;
    function ToolUp: TRect; override;
    function GetContextualToolbars: TContextualToolbars; override;
    function Render(VirtualScreen: TBGRABitmap; {%H-}VirtualScreenWidth, {%H-}VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect; override;
    function ToolCommand(ACommand: TToolCommand): boolean; override;
    function ToolProvideCommand(ACommand: TToolCommand): boolean; override;
    destructor Destroy; override;
  end;

  { TToolTextureMapping }

  TToolTextureMapping = class(TGenericTool)
  private
    class var ScaleHintShown, ReturnHintShown: boolean;
    FCurrentBounds: TRect;
    FLastTexture: TBGRABitmap;
    FTextureAfterAlpha: TBGRABitmap;
    FAdaptedTexture: TBGRABitmap;
    FCanReadaptTexture: boolean;
    FHighQuality: boolean;
    procedure ToolQuadNeeded;
    procedure ValidateQuad; virtual;
    procedure DrawQuad; virtual;
    function GetAdaptedTexture: TBGRABitmap;
    procedure UpdateBoundsMode(var ARectResult: TRect);

  protected
    boundsMode: boolean;
    quadDefined: boolean;
    definingQuad: boolean;
    quad: array of TPointF;
    boundsPts: array of TPointF;
    quadMovingIndex: integer;
    quadMoving,quadMovingBounds: boolean;
    quadMovingDelta: TPointF;
    function SnapIfNecessary(const ptF: TPointF): TPointF;
    function GetClosestPoint(const ptF: TPointF; out pointFound: TPointF): integer;
    function DoToolDown({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF;
      {%H-}rightBtn: boolean): TRect; override;
    function DoToolMove({%H-}toolDest: TBGRABitmap; {%H-}pt: TPoint; ptF: TPointF): TRect;
      override;
    function DoToolKeyDown(var key: Word): TRect; override;
    function DoToolKeyUp(var key: Word): TRect; override;
    function GetIsSelectingTool: boolean; override;
    function GetTexture: TBGRABitmap; virtual;
    function GetTextureRepetition: TTextureRepetition; virtual;
    procedure OnTryStop({%H-}sender: TCustomLayerAction); override;
    function ComputeBoundsPoints: ArrayOfTPointF;
    procedure PrepareBackground({%H-}toolDest: TBGRABitmap; AFirstTime: boolean); virtual;
    function DefaultTextureCenter: TPointF; virtual;
    function DoToolUpdate({%H-}toolDest: TBGRABitmap): TRect; override;
    function GetStatusText: string; override;
    function GetAllowedBackFillTypes: TVectorialFillTypes; override;
  public
    class procedure ForgetHintShown;
    constructor Create(AManager: TToolManager); override;
    function ToolUp: TRect; override;
    function GetContextualToolbars: TContextualToolbars; override;
    function Render(VirtualScreen: TBGRABitmap; {%H-}VirtualScreenWidth, {%H-}VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction):TRect; override;
    function ToolProvideCommand(ACommand: TToolCommand): boolean; override;
    function ToolCommand(ACommand: TToolCommand): boolean; override;
    destructor Destroy; override;
  end;

  { TToolLayerMapping }

  TToolLayerMapping = class(TToolTextureMapping)
  protected
    FTexture: TBGRABitmap;
    FDefaultTextureCenter: TPointF;
    FAlreadyDrawnOnce: boolean;
    procedure PrepareTexture;
    procedure PrepareBackground(toolDest: TBGRABitmap; {%H-}AFirstTime: boolean); override;
    function GetTexture: TBGRABitmap; override;
    function DefaultTextureCenter: TPointF; override;
    function GetTextureRepetition: TTextureRepetition; override;
    procedure ValidateQuad; override;
    function GetAllowedBackFillTypes: TVectorialFillTypes; override;
  public
    constructor Create(AManager: TToolManager); override;
    function GetContextualToolbars: TContextualToolbars; override;
    destructor Destroy; override;
  end;

implementation

uses LCLType, ugraph, LCScaleDPI, LazPaintType, BGRAFillInfo, BGRATransform, Controls;

{ TToolLayerMapping }

procedure TToolLayerMapping.PrepareTexture;
var src: TBGRABitmap;
    bounds: TRect;
begin
  if FTexture = nil then
  begin
    src := Action.BackupDrawingLayer;
    bounds := src.GetImageBounds;
    if IsRectEmpty(bounds) then
      bounds := rect(0,0,1,1);
    FTexture := src.GetPart(bounds) as TBGRABitmap;
    FDefaultTextureCenter := PointF((bounds.Left+bounds.Right)/2-0.5,(bounds.Top+bounds.Bottom)/2-0.5);
  end;
end;

procedure TToolLayerMapping.PrepareBackground(toolDest: TBGRABitmap;
  AFirstTime: boolean);
var
  r: TRect;
begin
  if not FAlreadyDrawnOnce then
  begin
    FAlreadyDrawnOnce := true;
    r := toolDest.GetImageBounds;
  end else
    r := FCurrentBounds;

  toolDest.FillRect(r, BGRAPixelTransparent, dmSet);
  Action.NotifyChange(toolDest, r);
end;

function TToolLayerMapping.GetTexture: TBGRABitmap;
begin
  PrepareTexture;
  result := FTexture;
end;

function TToolLayerMapping.DefaultTextureCenter: TPointF;
begin
  PrepareTexture;
  result := FDefaultTextureCenter;
end;

function TToolLayerMapping.GetTextureRepetition: TTextureRepetition;
begin
  if poRepeat in Manager.PerspectiveOptions then
    Result:= trRepeatBoth
  else
    result:= trNone;
end;

procedure TToolLayerMapping.ValidateQuad;
begin
  inherited ValidateQuad;
  Manager.QueryExitTool;
end;

function TToolLayerMapping.GetAllowedBackFillTypes: TVectorialFillTypes;
begin
  Result:= [vftSolid,vftGradient,vftTexture];
end;

constructor TToolLayerMapping.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  ToolQuadNeeded;
end;

function TToolLayerMapping.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [ctPerspective];
end;

destructor TToolLayerMapping.Destroy;
begin
  FreeAndNil(FTexture);
  inherited Destroy;
end;

{ TToolTextureMapping }

procedure TToolTextureMapping.ToolQuadNeeded;
var
  tx,ty: single;
  ratio,temp: single;
  center: TPointF;
begin
  if not quadDefined and (GetTexture <> nil) and (GetTexture.Width > 0) and (GetTexture.Height > 0) then
  begin
    tx := GetTexture.Width;
    ty := GetTexture.Height;
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
      setlength(quad,4);
      center := DefaultTextureCenter;
      quad[0] := PointF(round(center.x-tx*ratio/2+0.5)-0.5,round(center.y -ty*ratio/2 + 0.5)-0.5);
      quad[1] := PointF(quad[0].x + tx*ratio,quad[0].y);
      quad[2] := PointF(quad[1].x, quad[1].Y + ty*ratio);
      quad[3] := PointF(quad[0].x, quad[2].y);
      quadDefined:= true;
      PrepareBackground(GetToolDrawingLayer, True);
      DrawQuad;
      Action.NotifyChange(GetToolDrawingLayer, FCurrentBounds);
    end;
  end;
end;

procedure TToolTextureMapping.ValidateQuad;
begin
  if quadDefined then
  begin
    if Manager.Image.Width*Manager.Image.Height <= 786432 then
    begin
      PrepareBackground(GetToolDrawingLayer,False);
      FHighQuality := true;
      FCanReadaptTexture:= true;
      DrawQuad;
      FCanReadaptTexture:= false;
      FHighQuality := false;
      Action.NotifyChange(GetToolDrawingLayer, FCurrentBounds);
    end;
    ValidateAction;
    quadDefined := false;
    quad := nil;
    FLastTexture.FreeReference;
    FLastTexture := nil;
  end;
end;

procedure TToolTextureMapping.DrawQuad;
const OversampleQuality = 2;
var
  tex: TBGRABitmap;
  persp: TBGRAPerspectiveScannerTransform;
  dest: TBGRABitmap;
  quadHQ: array of TPointF;
  i: integer;
  scanRepeat: TBGRABitmapScanner;
  scan: IBGRAScanner;

  function AlmostInt(value: single): boolean;
  begin
    result := (value-round(value)) < 1e-6;
  end;

begin
  if quadDefined then
  begin
    if (quad[1].y = quad[0].y) and (quad[3].x = quad[0].x) and (quad[2].x = quad[1].x) and (quad[3].y = quad[2].y) and
      AlmostInt(quad[0].x+0.5) and AlmostInt(quad[0].y+0.5) and AlmostInt(quad[2].x+0.5) and AlmostInt(quad[2].y+0.5) and
      (round(quad[2].x-quad[0].x) = GetTexture.Width) and (round(quad[2].y-quad[0].y) = GetTexture.Height) then
       FHighQuality := false;

    tex := GetAdaptedTexture;
    if tex <> nil then
    begin

      if GetTextureRepetition <> trNone then
        FCurrentBounds := rect(0,0,Manager.Image.Width,Manager.Image.Height)
      else
        FCurrentBounds := GetShapeBounds([quad[0],quad[1],quad[2],quad[3]],1);

      if FHighQuality then
      begin
        dest := TBGRABitmap.Create(FCurrentBounds.Width*OversampleQuality,FCurrentBounds.Height*OversampleQuality);
        quadHQ := nil;
        setlength(quadHQ, length(quad));
        for i := 0 to high(quad) do quadHQ[i] := (quad[i]+PointF(0.5,0.5))*OversampleQuality - PointF(0.5,0.5) - PointF(FCurrentBounds.TopLeft)*OversampleQuality;
      end
      else
      begin
        dest := GetToolDrawingLayer;
        quadHQ := quad;
        dest.ClipRect := FCurrentBounds;
      end;

      if GetTextureRepetition <> trNone then
      begin
        if GetTextureRepetition <> trRepeatBoth then
        begin
          scanRepeat := TBGRABitmapScanner.Create(tex,
            GetTextureRepetition in [trRepeatX,trRepeatBoth],
            GetTextureRepetition in [trRepeatY,trRepeatBoth], Point(0,0) );
          scan := scanRepeat;
        end else
        begin
          scanRepeat := nil;
          scan := tex;
        end;
        persp := TBGRAPerspectiveScannerTransform.Create(scan,[PointF(-0.5,-0.5),PointF(tex.Width-0.5,-0.5),
          PointF(tex.Width-0.5,tex.Height-0.5),PointF(-0.5,tex.Height-0.5)],quadHQ);
        persp.IncludeOppositePlane := poTwoPlanes in Manager.PerspectiveOptions;
        dest.FillRect(0,0,dest.Width,dest.Height,persp,dmDrawWithTransparency);
        persp.Free;
        scan := nil;
        scanRepeat.Free;
      end else
      begin
        dest.FillQuadPerspectiveMappingAntialias(quadHQ[0],quadHQ[1],quadHQ[2],quadHQ[3],tex,PointF(-0.5,-0.5),PointF(tex.Width-0.5,-0.5),
          PointF(tex.Width-0.5,tex.Height-0.5),PointF(-0.5,tex.Height-0.5), rect(0,0,tex.Width,tex.Height));
      end;

      if FHighQuality then
      begin
        BGRAReplace(dest, dest.Resample(dest.Width div OversampleQuality, dest.Height div OversampleQuality,rmSimpleStretch));
        BGRAReplace(dest, dest.FilterSharpen(96/256));
        GetToolDrawingLayer.PutImage(FCurrentBounds.Left,FCurrentBounds.Top,dest,dmDrawWithTransparency);
        FreeAndNil(dest);
      end else
        dest.NoClip;
    end;
  end
  else
    FCurrentBounds := EmptyRect;
end;

function TToolTextureMapping.GetAdaptedTexture: TBGRABitmap;
var tx,ty: integer;
  precisionFactor: single;
begin
  if GetTextureRepetition <> trNone then //cannot optimize size
  begin
    result := GetTexture;
    exit;
  end;

  if GetTexture = nil then
  begin
    result := nil;
    exit;
  end else
  begin
    if FHighQuality then precisionFactor := 3
      else precisionFactor:= 1.5;
    tx := ceil(Max(VectLen(quad[1]-quad[0]),VectLen(quad[2]-quad[3]))*precisionFactor);
    ty := ceil(Max(VectLen(quad[2]-quad[1]),VectLen(quad[3]-quad[0]))*precisionFactor);
    if tx < 1 then tx := 1;
    if ty < 1 then ty := 1;
    if tx > GetTexture.Width then tx := GetTexture.Width;
    if ty > GetTexture.Height then ty := GetTexture.Height;

    if (tx = GetTexture.Width) and (ty = GetTexture.Height) then
    begin
      result := GetTexture;
      exit;
    end;

    if (FAdaptedTexture = nil) or FCanReadaptTexture then
    begin
      if (FAdaptedTexture <> nil) and ((FAdaptedTexture.Width <> tx) or (FAdaptedTexture.Height <> ty)) then
        FreeAndNil(FAdaptedTexture);
      if FAdaptedTexture = nil then
      begin
        GetTexture.ResampleFilter := rfLinear;
        FAdaptedTexture := GetTexture.Resample(tx,ty,rmFineResample) as TBGRABitmap;
      end;
    end;
    result := FAdaptedTexture;
    exit;
  end;
end;

procedure TToolTextureMapping.UpdateBoundsMode(var ARectResult: TRect);
begin
  if not boundsMode and not quadMoving and ([ssAlt, ssShift]*ShiftState <> []) then
  begin
    boundsMode := true;
    boundsPts := ComputeBoundsPoints;
    if IsRectEmpty(ARectResult) then
      ARectResult := OnlyRenderChange;
  end else
  if boundsMode and not quadMoving and ([ssAlt, ssShift]*ShiftState = [])  then
  begin
    boundsMode := false;
    boundsPts := ComputeBoundsPoints;
    if IsRectEmpty(ARectResult) then
      ARectResult := OnlyRenderChange;
  end;
end;

function TToolTextureMapping.SnapIfNecessary(const ptF: TPointF): TPointF;
begin
  if not (ssSnap in ShiftState) then result := ptF else
    result := PointF(round(ptF.X),round(ptF.Y));
end;

function TToolTextureMapping.GetClosestPoint(const ptF: TPointF; out pointFound: TPointF): integer;
var
  minDist, curDist: single;
  pts: array of TPointF;
  n: Integer;
begin
  if boundsMode then
    pts := boundsPts
  else
    pts := quad;
  result := -1;
  pointFound := EmptyPointF;
  minDist := sqr(DoScaleX(10,OriginalDPI));
  for n := 0 to high(pts) do
  begin
    curDist := sqr(ptF.x-pts[n].x)+sqr(ptF.y-pts[n].y);
    if curDist < minDist then
    begin
      minDist := curDist;
      result := n;
      pointFound := pts[n];
    end;
  end;
end;

function TToolTextureMapping.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
var
  n: Integer;
  selPt: TPointF;
begin
  result := EmptyRect;
  if rightBtn then exit;

  if not quadDefined then
  begin
    if not definingQuad then
    begin
      if GetTexture = nil then
        Manager.ToolPopup(tpmNothingToBeDeformed)
      else
        begin
          definingQuad := true;
          setlength(quad,4);
          quad[0] := ptF;
          quad[1] := ptF;
          quad[2] := ptF;
          quad[3] := ptF;
        end;
    end;
    exit;
  end;

  UpdateBoundsMode(result);

  n := GetClosestPoint(ptF, selPt);
  if n <> -1 then
  begin
    quadMovingIndex := n;
    quadMovingDelta := selPt-PtF;
    quadMoving := True;
    quadMovingBounds  := boundsMode;
  end else
  if IsPointInPolygon(quad, ptF, true) then
  begin
    quadMovingIndex := -1;
    quadMovingDelta := (quad[0]+quad[2])*0.5-ptF;
    quadMoving := true;
    quadMovingBounds  := boundsMode;
  end;
end;

function NonZero(AValue, ADefault: single): single;
begin
  if AValue = 0 then result := ADefault
  else result := AValue;
end;

function TToolTextureMapping.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): TRect;
var n: integer;
  delta,prevSize,newSize,selPt: TPointF;
  curBounds: array of TPointF;
  ratioX,ratioY,ratio: single;
  avgSize: single;
begin
  if definingQuad then
  begin
    if ssShift in ShiftState then
    begin
      if (GetTexture <> nil) and (GetTexture.Height <> 0)
        and (GetTexture.Width <> 0) then
      begin
        ratio := GetTexture.Width/GetTexture.Height;
        newSize := ptF - quad[0];
        avgSize := (abs(newSize.x)+abs(newSize.y))/2;
        ptF.x := quad[0].x+avgSize*NonZero(sign(newSize.x),1)*ratio/((ratio+1)/2);
        ptF.y := quad[0].y+avgSize*NonZero(sign(newSize.y),1)*1/((ratio+1)/2);
      end;
    end;
    quad[2] := ptF;
    quad[1].x := ptF.x;
    quad[3].y := ptF.y;
    result := OnlyRenderChange;
    exit;
  end;

  result := EmptyRect;
  if not ScaleHintShown then
  begin
    Manager.ToolPopup(tpmHoldKeysScaleMode, VK_SHIFT);
    ScaleHintShown:= true;
  end;
  if quadMoving then
  begin
    if quadMovingIndex = -1 then
    begin
      delta := SnapIfNecessary(quadMovingDelta + ptF) - ((quad[0]+quad[2])*0.5);
      for n := 0 to high(quad) do
        quad[n] += delta;
      if quadMovingBounds then boundsPts := ComputeBoundsPoints;
    end
    else
    if quadMovingBounds then
    begin
      boundsPts[quadMovingIndex] := SnapIfNecessary(quadMovingDelta + ptF);
      case quadMovingIndex of
        0:begin
          boundsPts[1].y := boundsPts[quadMovingIndex].y;
          boundsPts[3].x := boundsPts[quadMovingIndex].x;
        end;
        1:begin
          boundsPts[0].y := boundsPts[quadMovingIndex].y;
          boundsPts[2].x := boundsPts[quadMovingIndex].x;
        end;
        2:begin
          boundsPts[3].y := boundsPts[quadMovingIndex].y;
          boundsPts[1].x := boundsPts[quadMovingIndex].x;
        end;
        3:begin
          boundsPts[2].y := boundsPts[quadMovingIndex].y;
          boundsPts[0].x := boundsPts[quadMovingIndex].x;
        end;
      end;
      if ssShift in ShiftState then
      begin
        curBounds := ComputeBoundsPoints;
        prevSize := curBounds[2]-curBounds[0];
        newSize := boundsPts[2]-boundsPts[0];
        if (abs(prevSize.x) > 1e-6) and (abs(prevSize.y) > 1e-6) then
        begin
          ratioX := abs(newSize.X/prevSize.X);
          ratioY := abs(newSize.Y/prevSize.Y);
          ratio := (ratioX+ratioY)/2;
          newSize.X := abs(prevSize.X)*ratio*NonZero(Sign(newSize.X),1);
          newSize.Y := abs(prevSize.Y)*ratio*NonZero(Sign(newSize.Y),1);
          case quadMovingIndex of
          0: boundsPts[0] := boundsPts[2]-newSize;
          1: boundsPts[1] := boundsPts[3]+PointF(newSize.X,-newSize.Y);
          2: boundsPts[2] := boundsPts[0]+newSize;
          3: boundsPts[3] := boundsPts[1]+PointF(-newSize.X,newSize.Y);
          end;
          case quadMovingIndex of
            0:begin
              boundsPts[1].y := boundsPts[quadMovingIndex].y;
              boundsPts[3].x := boundsPts[quadMovingIndex].x;
            end;
            1:begin
              boundsPts[0].y := boundsPts[quadMovingIndex].y;
              boundsPts[2].x := boundsPts[quadMovingIndex].x;
            end;
            2:begin
              boundsPts[3].y := boundsPts[quadMovingIndex].y;
              boundsPts[1].x := boundsPts[quadMovingIndex].x;
            end;
            3:begin
              boundsPts[2].y := boundsPts[quadMovingIndex].y;
              boundsPts[0].x := boundsPts[quadMovingIndex].x;
            end;
          end;
        end;
      end;
    end
    else
      quad[quadMovingIndex] := SnapIfNecessary(quadMovingDelta + ptF);
    PrepareBackground(toolDest,False);
    DrawQuad;
    result := FCurrentBounds;
  end;
  UpdateBoundsMode(result);
  if not quadMoving then
  begin
    if GetClosestPoint(ptF, selPt) <> -1 then
      Cursor := crSizeAll
    else
      Cursor := crDefault;
  end;
end;

function TToolTextureMapping.GetIsSelectingTool: boolean;
begin
  Result:= false;
end;

function TToolTextureMapping.GetTexture: TBGRABitmap;
begin
  if (Manager.BackFill.Texture = nil) or (Manager.BackFill.Texture = FLastTexture) then
  begin
    if FTextureAfterAlpha <> nil then
      result := FTextureAfterAlpha
    else
      result := FLastTexture;
  end
  else
  begin
    if (Manager.BackFill.Texture <> nil) and (Manager.BackFill.TextureOpacity <> 255) then
    begin
      FTextureAfterAlpha := Manager.BackFill.Texture.Duplicate as TBGRABitmap;
      FTextureAfterAlpha.ApplyGlobalOpacity(Manager.BackFill.TextureOpacity);
      result := FTextureAfterAlpha;
    end else
    begin
      result := Manager.BackFill.Texture;
      FreeAndNil(FTextureAfterAlpha);
    end;
    FLastTexture.FreeReference;
    FLastTexture := Manager.BackFill.Texture.NewReference as TBGRABitmap;
  end;
end;

function TToolTextureMapping.GetTextureRepetition: TTextureRepetition;
begin
  if Manager.BackFill.FillType = vftTexture then
    result := Manager.BackFill.TextureRepetition
  else
    result := trNone;
end;

procedure TToolTextureMapping.OnTryStop(sender: TCustomLayerAction);
begin
  //nothing
end;

function TToolTextureMapping.ComputeBoundsPoints: ArrayOfTPointF;
var
  minPt,maxPt: TPointF;
  i: integer;
begin
  if quadDefined then
  begin
    minPt := quad[low(quad)];
    maxPt := quad[low(quad)];
    for i := 1 to high(quad) do
    begin
      if quad[i].x < minPt.X then minPt.x := quad[i].x;
      if quad[i].x > maxPt.X then maxPt.x := quad[i].x;
      if quad[i].y < minPt.y then minPt.y := quad[i].y;
      if quad[i].y > maxPt.y then maxPt.y := quad[i].y;
    end;
    result := PointsF([minPt, PointF(maxPt.X,minPt.Y), maxPt, PointF(MinPt.X,MaxPt.Y)]);
  end else
    result := nil;
end;

procedure TToolTextureMapping.PrepareBackground(toolDest: TBGRABitmap;
  AFirstTime: boolean);
begin
  if not AFirstTime then RestoreBackupDrawingLayer;
end;

function TToolTextureMapping.DefaultTextureCenter: TPointF;
begin
  result := PointF(Manager.Image.Width/2-0.5-LayerOffset.X,Manager.Image.Height/2-0.5-LayerOffset.Y);
end;

function TToolTextureMapping.DoToolUpdate(toolDest: TBGRABitmap): TRect;
begin
  if quadDefined then
  begin
    PrepareBackground(GetToolDrawingLayer,False);
    DrawQuad;
    result := FCurrentBounds;
  end
    else
      result := EmptyRect;
end;

function TToolTextureMapping.GetStatusText: string;
var
  i: Integer;
begin
  result := '';
  for i := 0 to high(quad) do
  begin
    if i > 0 then result += '|';
    result += 'x'+inttostr(i+1)+' = '+inttostr(round(quad[i].x+0.5))+'|'+
       'y'+inttostr(i+1)+' = '+inttostr(round(quad[i].y+0.5));
  end;
end;

function TToolTextureMapping.GetAllowedBackFillTypes: TVectorialFillTypes;
begin
  Result:= [vftTexture];
end;

class procedure TToolTextureMapping.ForgetHintShown;
begin
  ScaleHintShown:= false;
  ReturnHintShown:= false;
end;

constructor TToolTextureMapping.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  FCurrentBounds := EmptyRect;
  FHighQuality:= False;
  FLastTexture := nil;
  quadDefined:= false;
  definingQuad:= false;
end;

function TToolTextureMapping.DoToolKeyDown(var key: Word): TRect;
begin
  result := EmptyRect;

  if Key = VK_RETURN then
  begin
    if quadDefined then
    begin
      ValidateQuad;
      result := EmptyRect;
      key := 0;
    end;
  end else
  if Key = VK_ESCAPE then
  begin
    if quadDefined then
    begin
      CancelActionPartially;
      result := OnlyRenderChange;
      manager.QueryExitTool;
      key := 0;
    end;
  end else
  if (Key = VK_SHIFT) or (Key = VK_MENU) then
    UpdateBoundsMode(result);
end;

function TToolTextureMapping.DoToolKeyUp(var key: Word): TRect;
begin
  Result:= EmptyRect;
  if (Key = VK_SHIFT) or (Key = VK_MENU) then
    UpdateBoundsMode(result);
end;

function TToolTextureMapping.ToolUp: TRect;
var prevSize,newSize: TPointF;
  oldBounds: array of TPointF;
  i: integer;
  redraw: boolean;
begin
  if definingQuad then
  begin
    definingQuad:= false;
    quadDefined:= true;
    PrepareBackground(GetToolDrawingLayer,False);
    FCanReadaptTexture:= true;
    DrawQuad;
    FCanReadaptTexture:= false;
    result := FCurrentBounds;
    if not ReturnHintShown then
    begin
      Manager.ToolPopup(tpmreturnValides);
      ReturnHintShown:= true;
    end;
    exit;
  end;
  if quadMoving then
  begin
    redraw := GetTextureRepetition = trNone;
    if quadMovingBounds then
    begin
      oldBounds := ComputeBoundsPoints;
      prevSize := oldBounds[2]-oldBounds[0];
      newSize := boundsPts[2]-boundsPts[0];
      if (abs(newSize.x) > 1e-6) and (abs(newSize.y) > 1e-6) and
        (abs(prevSize.x) > 1e-6) and (abs(prevSize.y) > 1e-6) then
      begin
        for i := low(quad) to high(quad) do
        begin
          quad[i] -= oldBounds[0];
          quad[i].x *= newSize.X/prevSize.X;
          quad[i].y *= newSize.Y/prevSize.Y;
          quad[i] += boundsPts[0];
        end;
      end;
      quadMovingBounds := false;
      boundsPts := ComputeBoundsPoints;
      redraw := true;
    end;
    if redraw then
    begin
      PrepareBackground(GetToolDrawingLayer,False);
      FCanReadaptTexture:= true;
      DrawQuad;
      FCanReadaptTexture:= false;
      result := FCurrentBounds;
    end else
      result := EmptyRect;
    quadMoving := false;
  end else
    result := EmptyRect;
end;

function TToolTextureMapping.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [ctBackFill,ctPerspective];
end;

function TToolTextureMapping.Render(VirtualScreen: TBGRABitmap;
  VirtualScreenWidth, VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;

  procedure DrawPoints(pts: array of TPointF; alpha: byte);
  var curPt,nextPt: TPointF;
      n: Integer;
  begin
    For n := 0 to high(pts) do
    begin
      curPt := BitmapToVirtualScreen(pts[n]);
      nextPt := BitmapToVirtualScreen(pts[(n+1)mod length(pts)]);
      NiceLine(VirtualScreen, curPt.X,curPt.Y,nextPt.x,nextPt.y,alpha);
    end;
    For n := 0 to high(pts) do
    begin
      curPt := BitmapToVirtualScreen(pts[n]);
      result := RectUnion(result,NicePoint(VirtualScreen, curPt.X,curPt.Y,alpha));
    end;
  end;

begin
  result := EmptyRect;
  if not quadDefined and not definingQuad then exit;
  if boundsMode or quadMovingBounds then
  begin
    DrawPoints(quad,80);
    DrawPoints(boundsPts,192);
  end else
    DrawPoints(quad,192);
end;

function TToolTextureMapping.ToolProvideCommand(ACommand: TToolCommand
  ): boolean;
begin
  case ACommand of
    tcFinish: result := quadDefined;
  else result := false;
  end;
end;

function TToolTextureMapping.ToolCommand(ACommand: TToolCommand): boolean;
begin
  case ACommand of
    tcFinish: if quadDefined then
      begin
        ValidateQuad;
        result := true;
      end  else
        result := false;
  else result := false;
  end;
end;

destructor TToolTextureMapping.Destroy;
begin
  ValidateAction;
  FLastTexture.FreeReference;
  FreeAndNil(FTextureAfterAlpha);
  FreeAndNil(FAdaptedTexture);
  inherited Destroy;
end;

{ TToolDeformationGrid }

function TToolDeformationGrid.ToolDeformationGridNeeded: boolean;
var xb,yb: integer;
    layer: TBGRABitmap;
begin
  if (DeformationGrid = nil) then
  begin
    layer := GetToolDrawingLayer;
    if layer = nil then
    begin
      result := false;
      exit;
    end;
    deformationGridNbX:= Manager.DeformationGridNbX;
    deformationGridNbY:= Manager.DeformationGridNbY;
    SetLength(DeformationGrid, deformationGridNbY, deformationGridNbX);
    SetLength(DeformationGridTexCoord, deformationGridNbY, deformationGridNbX);
    for yb := 0 to deformationGridNbY-1 do
      for xb := 0 to deformationGridNbX-1 do
      begin
        DeformationGridTexCoord[yb,xb] := PointF(xb/(deformationGridNbX-1)*layer.Width-0.5,
                                                     yb/(deformationGridNbY-1)*layer.Height-0.5);
        DeformationGrid[yb,xb] :=DeformationGridTexCoord[yb,xb];
      end;
  end;
  result := true;
end;

function TToolDeformationGrid.ToolCommand(ACommand: TToolCommand): boolean;
begin
  case ACommand of
    tcFinish: if DoingDeformation then
      begin
        ValidateDeformationGrid;
        result := true;
      end
      else result := false;
  else result := false;
  end;
end;

function TToolDeformationGrid.ToolProvideCommand(ACommand: TToolCommand
  ): boolean;
begin
  case ACommand of
    tcFinish: result := DoingDeformation;
  else result := false;
  end;
end;

destructor TToolDeformationGrid.Destroy;
begin
  ValidateDeformationGrid;
  inherited Destroy;
end;

procedure TToolDeformationGrid.ReleaseGrid;
var
  xb,yb: Integer;
begin
  if DoingDeformation then
  begin
    ValidateAction;
    DoingDeformation := false;
    for yb := 0 to deformationGridNbY-2 do
      for xb := 0 to deformationGridNbX-2 do
        DeformationGridTexCoord[yb,xb] := DeformationGrid[yb,xb];
  end;
end;

procedure TToolDeformationGrid.ValidateDeformationGrid;
begin
  if DoingDeformation then
  begin
    DeformationGrid := nil;
    DeformationGridTexCoord := nil;
    ValidateAction;
    DoingDeformation := false;
  end;
end;

function TToolDeformationGrid.GetPointAt(const ptF: TPointF; var x, y: integer): boolean;
var
  yb, xb: Integer;
  curDist, minDist: single;
begin
  result := false;
  minDist := sqr(SelectionMaxPointDistance);
  for yb := 1 to deformationGridNbY-2 do
    for xb := 1 to deformationGridNbX-2 do
    begin
      curDist := sqr(ptF.x-DeformationGrid[yb,xb].x) + sqr(ptF.y-DeformationGrid[yb,xb].y);
      if curDist < minDist then
      begin
        minDist := curDist;
        x := xb;
        y := yb;
        result := True;
      end;
    end;
end;

function TToolDeformationGrid.DoToolDown(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF; rightBtn: boolean): TRect;
begin
  result := EmptyRect;
  deformationGridX := 1;
  deformationGridY := 1;
  if DeformationGrid <> nil then
  begin
    if GetPointAt(ptF, deformationGridX, deformationGridY) then
    begin
      deformationGridMoving := True;
      deformationOrigin := ptF;
    end;
  end;
end;

function TToolDeformationGrid.DoToolMove(toolDest: TBGRABitmap; pt: TPoint;
  ptF: TPointF): Trect;
var xb,yb,NbX,NbY: integer;
    gridDone: array of array of boolean;
    layer,backupLayer : TBGRABitmap;
    PreviousClipRect: TRect;
    previousBounds: TRect;
    gridMinX,gridMinY,gridMaxX,gridMaxY, dummyX, dummyY: integer;

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

  if not deformationGridMoving then
  begin
    dummyX := 1;
    dummyY := 1;
    if GetPointAt(ptF, dummyX, dummyY) then
      Cursor := crHandPoint
      else Cursor := crDefault;
    exit;
  end;
  if Manager.DeformationGridMode = gmMovePointWithoutDeformation then
  begin
    ReleaseGrid;
    DeformationGrid[deformationGridY,deformationGridX] := PointF(
      DeformationGrid[deformationGridY,deformationGridX].X + ptF.X-deformationOrigin.X,
      DeformationGrid[deformationGridY,deformationGridX].Y + ptF.Y-deformationOrigin.Y);
    DeformationGridTexCoord[deformationGridY,deformationGridX] := DeformationGrid[deformationGridY,deformationGridX];
    result := OnlyRenderChange;
  end else
  begin
    if not DoingDeformation then
    begin
      FCurrentBounds := EmptyRect;
      DoingDeformation := True;
    end;

    layer := GetToolDrawingLayer;
    backupLayer := GetBackupLayerIfExists;
    NbX := deformationGridNbX;
    NbY := deformationGridNbY;

    DeformationGrid[deformationGridY,deformationGridX] := PointF(
      DeformationGrid[deformationGridY,deformationGridX].X + ptF.X-deformationOrigin.X,
      DeformationGrid[deformationGridY,deformationGridX].Y + ptF.Y-deformationOrigin.Y);

    previousBounds := FCurrentBounds;
    FCurrentBounds := EmptyRect;
    gridMinX := deformationGridX-1;
    if gridMinX < 0 then gridMinX := 0;
    gridMinY := deformationGridY-1;
    if gridMinY < 0 then gridMinY := 0;
    gridMaxX := deformationGridX+1;
    if gridMaxX > NbX-1 then gridMaxX := NbX-1;
    gridMaxY := deformationGridY+1;
    if gridMaxY > NbY-1 then gridMaxY := NbY-1;
    for yb := gridMinY to gridMaxY do
     for xb := gridMinX to gridMaxX do
         AddToDeformationArea(xb,yb);
    FMergedBounds := RectUnion(previousBounds,FCurrentBounds);

    gridMinX := 0;
    gridMinY := 0;
    gridMaxX := NbX-1;
    gridMaxY := NbY-1;

    //progressive drawing of deformation zones
    gridDone := nil;
    setlength(gridDone,NbY-1,NbX-1);
    for yb := gridMinY to gridMaxY-1 do
      for xb := gridMinX to gridMaxX-1 do
        gridDone[yb,xb] := false;

    if not IsRectEmpty(FMergedBounds) and (backupLayer <>nil) then
    begin

      PreviousClipRect := layer.ClipRect;
      layer.ClipRect := FMergedBounds;
      layer.FillRect(0,0,layer.Width,layer.Height,BGRAPixelTransparent,dmSet);
      //drawing zones that are not deformed
      for yb := gridMinY to gridMaxY-1 do
        for xb := gridMinX to gridMaxX-1 do
          if (DeformationGrid[yb,xb] = DeformationGridTexCoord[yb,xb]) and
             (DeformationGrid[yb,xb+1] = DeformationGridTexCoord[yb,xb+1]) and
             (DeformationGrid[yb+1,xb+1] = DeformationGridTexCoord[yb+1,xb+1]) and
             (DeformationGrid[yb+1,xb] = DeformationGridTexCoord[yb+1,xb]) then
          begin
            layer.FillPoly([DeformationGrid[yb,xb],DeformationGrid[yb,xb+1],
                  DeformationGrid[yb+1,xb+1],DeformationGrid[yb+1,xb]],backupLayer,dmDrawWithTransparency);
            gridDone[yb,xb] := true;
          end;
      //drawing zones that are inverted
      for yb := gridMinY to gridMaxY-1 do
        for xb := gridMinX to gridMaxX-1 do
          if not gridDone[yb,xb] and
             not IsMostlyClockwise([DeformationGrid[yb,xb],DeformationGrid[yb,xb+1],
                DeformationGrid[yb+1,xb+1],DeformationGrid[yb+1,xb]]) then
          begin
            layer.FillQuadLinearMapping(DeformationGrid[yb,xb],DeformationGrid[yb,xb+1],
                  DeformationGrid[yb+1,xb+1],DeformationGrid[yb+1,xb],backupLayer,
                  DeformationGridTexCoord[yb,xb],DeformationGridTexCoord[yb,xb+1],DeformationGridTexCoord[yb+1,xb+1],
                  DeformationGridTexCoord[yb+1,xb],true, fcKeepCW);
            gridDone[yb,xb] := true;
          end;
      //drawing zones that are intersecting
      for yb := gridMinY to gridMaxY-1 do
        for xb := gridMinX to gridMaxX-1 do
          if not gridDone[yb,xb] and
             DoesQuadIntersect(DeformationGrid[yb,xb],DeformationGrid[yb,xb+1],
                DeformationGrid[yb+1,xb+1],DeformationGrid[yb+1,xb]) then
          begin
            layer.FillQuadLinearMapping(DeformationGrid[yb,xb],DeformationGrid[yb,xb+1],
                  DeformationGrid[yb+1,xb+1],DeformationGrid[yb+1,xb],backupLayer,
                  DeformationGridTexCoord[yb,xb],DeformationGridTexCoord[yb,xb+1],DeformationGridTexCoord[yb+1,xb+1],
                  DeformationGridTexCoord[yb+1,xb],true, fcKeepCW);
            gridDone[yb,xb] := true;
          end;
      //drawing zones that are concave
      for yb := gridMinY to gridMaxY-1 do
        for xb := gridMinX to gridMaxX-1 do
          if not gridDone[yb,xb] and
             not IsConvex([DeformationGrid[yb,xb],DeformationGrid[yb,xb+1],
                DeformationGrid[yb+1,xb+1],DeformationGrid[yb+1,xb]]) then
          begin
            layer.FillQuadLinearMapping(DeformationGrid[yb,xb],DeformationGrid[yb,xb+1],
                  DeformationGrid[yb+1,xb+1],DeformationGrid[yb+1,xb],backupLayer,
                  DeformationGridTexCoord[yb,xb],DeformationGridTexCoord[yb,xb+1],DeformationGridTexCoord[yb+1,xb+1],
                  DeformationGridTexCoord[yb+1,xb],true, fcKeepCW);
            gridDone[yb,xb] := true;
          end;
      //drawing convex zones
      for yb := gridMinY to gridMaxY-1 do
        for xb := gridMinX to gridMaxX-1 do
          if not gridDone[yb,xb] and IsClockwise([DeformationGrid[yb,xb],DeformationGrid[yb,xb+1],
                DeformationGrid[yb+1,xb+1],DeformationGrid[yb+1,xb]]) then
          layer.FillQuadLinearMapping(DeformationGrid[yb,xb],DeformationGrid[yb,xb+1],
                DeformationGrid[yb+1,xb+1],DeformationGrid[yb+1,xb],backupLayer,
                DeformationGridTexCoord[yb,xb],DeformationGridTexCoord[yb,xb+1],DeformationGridTexCoord[yb+1,xb+1],
                DeformationGridTexCoord[yb+1,xb],true);

      layer.ClipRect := PreviousClipRect;
    end;
    result := FMergedBounds;
  end;
  deformationOrigin := ptF;
end;

function TToolDeformationGrid.GetIsSelectingTool: boolean;
begin
  Result:= false;
end;

function TToolDeformationGrid.DoToolUpdate(toolDest: TBGRABitmap): TRect;
begin
  if (deformationGridNbX <> Manager.DeformationGridNbX) or
     (deformationGridNbY <> Manager.DeformationGridNbY) then
  begin
    ReleaseGrid;
    DeformationGrid := nil;
    DeformationGridTexCoord := nil;
    deformationGridNbX:= 0;
    deformationGridNbY:= 0;
    Result:= OnlyRenderChange;
  end
  else
    result := EmptyRect;
end;

class procedure TToolDeformationGrid.ForgetHintShown;
begin
  ReturnHintShown := false;
end;

constructor TToolDeformationGrid.Create(AManager: TToolManager);
begin
  inherited Create(AManager);
  deformationGridNbX:= 0;
  deformationGridNbY:= 0;
  DoingDeformation:= false;
  deformationGrid := nil;
  deformationGridTexCoord := nil;
end;

function TToolDeformationGrid.Render(VirtualScreen: TBGRABitmap;
  VirtualScreenWidth, VirtualScreenHeight: integer; BitmapToVirtualScreen: TBitmapToVirtualScreenFunction): TRect;
var curPt,rightPt,downPt: TPointF;
    xb,yb: Integer;
begin
  result := EmptyRect;
  if not ToolDeformationGridNeeded then exit;
  for xb := 0 to deformationGridNbX-1 do
    for yb := 0 to deformationGridNbY-1 do
    begin
      curPt := BitmapToVirtualScreen(DeformationGrid[yb,xb]);
      if not deformationGridMoving or ((xb+1 >= deformationGridX) and (xb <= deformationGridX) and
        (yb >= deformationGridY-1) and (yb <= deformationGridY+1)) then
      begin
        if (xb < deformationGridNbX-1) and (yb > 0) and (yb < deformationGridNbY-1) then
        begin
          rightPt := BitmapToVirtualScreen(DeformationGrid[yb,xb+1]);
          if Assigned(VirtualScreen) then NiceLine(VirtualScreen, curPt.X,curPt.Y, rightPt.X,rightPt.Y);
          result := RectUnion(result,rect(floor(curPt.x)-1,floor(curPt.y)-1,
            ceil(curPt.x)+2,ceil(curPt.y)+2));
          result := RectUnion(result,rect(floor(rightPt.x)-1,floor(rightPt.y)-1,
            ceil(rightPt.x)+2,ceil(rightPt.y)+2));
        end;
      end;
      if not deformationGridMoving or ((xb >= deformationGridX-1) and (xb <= deformationGridX+1) and
        (yb+1 >= deformationGridY) and (yb <= deformationGridY)) then
      begin
        if (yb < deformationGridNbY-1) and (xb > 0) and (xb < deformationGridNbX-1) then
        begin
          downPt := BitmapToVirtualScreen(DeformationGrid[yb+1,xb]);
          if Assigned(virtualScreen) then NiceLine(VirtualScreen, curPt.X,curPt.Y, downPt.X,downPt.Y);
          result := RectUnion(result,rect(floor(curPt.x)-1,floor(curPt.y)-1,
            ceil(curPt.x)+2,ceil(curPt.y)+2));
          result := RectUnion(result,rect(floor(downPt.x)-1,floor(downPt.y)-1,
            ceil(downPt.x)+2,ceil(downPt.y)+2));
        end;
      end;
    end;
  for xb := 1 to deformationGridNbX-2 do
    for yb := 1 to deformationGridNbY-2 do
    begin
      if not deformationGridMoving or ((xb >= deformationGridX-1) and (xb <= deformationGridX+1) and
        (yb >= deformationGridY-1) and (yb <= deformationGridY+1)) then
      begin
        curPt := BitmapToVirtualScreen(DeformationGrid[yb,xb]);
        result := RectUnion(result,NicePoint(VirtualScreen, curPt.X,curPt.Y));
      end;
    end;
end;

function TToolDeformationGrid.DoToolKeyDown(var key: Word): TRect;
begin
  result := EmptyRect;
  if Key = VK_RETURN then
  begin
    if DoingDeformation then
    begin
      ValidateDeformationGrid;
      result := EmptyRect;
      manager.QueryExitTool;
      Key := 0;
    end;
  end else
  if Key = VK_ESCAPE then
  begin
    if DoingDeformation then
      CancelActionPartially;
    result := OnlyRenderChange;
    manager.QueryExitTool;
    Key := 0;
  end;
end;

function TToolDeformationGrid.ToolUp: TRect;
begin
  if deformationGridMoving then
  begin
    result := OnlyRenderChange;
    if not ReturnHintShown then
    begin
      Manager.ToolPopup(tpmreturnValides);
      ReturnHintShown := true;
    end;
  end
  else
    Result:=EmptyRect;
  deformationGridMoving := false;
end;

function TToolDeformationGrid.GetContextualToolbars: TContextualToolbars;
begin
  Result:= [ctDeformation];
end;

initialization

  RegisterTool(ptDeformation, TToolDeformationGrid);
  RegisterTool(ptTextureMapping, TToolTextureMapping);
  RegisterTool(ptLayerMapping, TToolLayerMapping);

end.

