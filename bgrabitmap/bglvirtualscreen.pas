unit BGLVirtualScreen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, BGRABitmapTypes, BGRAOpenGL, OpenGLContext, BGRACanvasGL,
  BGRASpriteGL;

type
  TCustomBGLVirtualScreen = class;
  TBGLRedrawEvent = procedure (Sender: TObject; BGLContext: TBGLContext) of object;
  TBGLLoadTexturesEvent = procedure (Sender: TObject; BGLContext: TBGLContext) of object;
  TBGLElapseEvent = procedure (Sender: TObject; BGLContext: TBGLContext; ElapsedMs: integer) of object;
  TBGLFramesPerSecondEvent = procedure (Sender: TObject; BGLContext: TBGLContext; FramesPerSecond: integer) of object;

  { TCustomBGLVirtualScreen }

  TCustomBGLVirtualScreen = class(TCustomOpenGLControl)
  private
    { Private declarations }
    FOnRedraw: TBGLRedrawEvent;
    FOnLoadTextures: TBGLLoadTexturesEvent;
    FOnUnloadTextures: TBGLLoadTexturesEvent;
    FOnElapse: TBGLElapseEvent;
    FOnFramesPerSecond: TBGLFramesPerSecondEvent;
    FTexturesLoaded: boolean;
    FBevelInner, FBevelOuter: TPanelBevel;
    FBevelWidth:  TBevelWidth;
    FBorderWidth: TBorderWidth;
    FRedrawOnIdle: boolean;
    FSprites: TBGLCustomSpriteEngine;
    FElapseAccumulator, FElapseCount: integer;
    FContextPrepared: boolean;
    FOldSprites: TBGLCustomSpriteEngine;
    FShaderList,FOldShaderList: TStringList;
    function GetCanvas: TBGLCustomCanvas;
    procedure SetBevelInner(const AValue: TPanelBevel);
    procedure SetBevelOuter(const AValue: TPanelBevel);
    procedure SetBevelWidth(const AValue: TBevelWidth);
    procedure SetBorderWidth(const AValue: TBorderWidth);
    procedure SetRedrawOnIdle(AValue: Boolean);
  protected
    class var FToRedrawOnIdle: array of TCustomBGLVirtualScreen;
    { Protected declarations }
    procedure RedrawContent(ctx: TBGLContext); virtual;
    procedure SetEnabled(Value: boolean); override;
    class procedure OnAppIdle(Sender: TObject; var Done: Boolean);
    procedure LoadTextures; virtual;
    function PrepareBGLContext: TBGLContext;
    procedure ReleaseBGLContext(ctx: TBGLContext);
  public
    { Public declarations }
    procedure DoOnPaint; override;
    procedure QueryLoadTextures; virtual;
    procedure UnloadTextures; virtual;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    property Canvas: TBGLCustomCanvas read GetCanvas;
    property Sprites: TBGLCustomSpriteEngine read FSprites;
    property OnLoadTextures: TBGLLoadTexturesEvent Read FOnLoadTextures Write FOnLoadTextures;
    property OnUnloadTextures: TBGLLoadTexturesEvent Read FOnUnloadTextures Write FOnUnloadTextures;
    property OnRedraw: TBGLRedrawEvent Read FOnRedraw Write FOnRedraw;
    property OnElapse: TBGLElapseEvent Read FOnElapse Write FOnElapse;
    property OnFramesPerSecond: TBGLFramesPerSecondEvent Read FOnFramesPerSecond Write FOnFramesPerSecond;
    property RedrawOnIdle: Boolean read FRedrawOnIdle write SetRedrawOnIdle default False;
    property BorderWidth: TBorderWidth Read FBorderWidth Write SetBorderWidth default 0;
    property BevelInner: TPanelBevel Read FBevelInner Write SetBevelInner default bvNone;
    property BevelOuter: TPanelBevel Read FBevelOuter Write SetBevelOuter default bvNone;
    property BevelWidth: TBevelWidth Read FBevelWidth Write SetBevelWidth default 1;
  end;

  TBGLVirtualScreen = class(TCustomBGLVirtualScreen)
  published
    property OnRedraw;
    property Align;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property RedrawOnIdle;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnElapse;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnFramesPerSecond;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnLoadTextures;
    property OnUnloadTextures;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

procedure Register;

implementation

uses Types;

procedure Register;
begin
  {$I bglvirtualscreen_icon.lrs}
  RegisterComponents('OpenGL', [TBGLVirtualScreen]);
end;

{ TCustomBGLVirtualScreen }

procedure TCustomBGLVirtualScreen.SetBevelInner(const AValue: TPanelBevel);
begin
  if FBevelInner = AValue then
    exit;
  FBevelInner := AValue;
  Invalidate;
end;

function TCustomBGLVirtualScreen.GetCanvas: TBGLCustomCanvas;
begin
  result := BGLCanvas;
end;

procedure TCustomBGLVirtualScreen.SetBevelOuter(const AValue: TPanelBevel);
begin
  if FBevelOuter = AValue then
    exit;
  FBevelOuter := AValue;
  Invalidate;
end;

procedure TCustomBGLVirtualScreen.SetBevelWidth(const AValue: TBevelWidth);
begin
  if FBevelWidth = AValue then
    exit;
  FBevelWidth := AValue;
  Invalidate;
end;

procedure TCustomBGLVirtualScreen.SetBorderWidth(const AValue: TBorderWidth);
begin
  if FBorderWidth = AValue then
    exit;
  FBorderWidth := AValue;
  Invalidate;
end;

procedure TCustomBGLVirtualScreen.SetRedrawOnIdle(AValue: Boolean);
var
  i: Integer;
  j: Integer;
begin
  if FRedrawOnIdle=AValue then Exit;
  FRedrawOnIdle:=AValue;

  if FRedrawOnIdle then
  begin
    if length(FToRedrawOnIdle)= 0 then
      Application.AddOnIdleHandler(@OnAppIdle);
    setlength(FToRedrawOnIdle, length(FToRedrawOnIdle)+1);
    FToRedrawOnIdle[high(FToRedrawOnIdle)] := self;
  end
  else
  if length(FToRedrawOnIdle)> 0 then
  begin
    for i := 0 to high(FToRedrawOnIdle) do
    begin
      if FToRedrawOnIdle[i]=self then
      begin
        for j := i to high(FToRedrawOnIdle)-1 do
          FToRedrawOnIdle[j] := FToRedrawOnIdle[j+1];
        setlength(FToRedrawOnIdle, length(FToRedrawOnIdle)-1);
        break;
      end;
    end;
    if length(FToRedrawOnIdle) = 0 then
       Application.RemoveOnIdleHandler(@OnAppIdle);
  end;
end;

procedure TCustomBGLVirtualScreen.DoOnPaint;
var
  ctx: TBGLContext;
begin
  if not FTexturesLoaded then LoadTextures;

  ctx := PrepareBGLContext;
  if Color = clNone then
    BGLViewPort(ClientWidth,ClientHeight)
  else
  if Color = clDefault then
    BGLViewPort(ClientWidth,ClientHeight,ColorToBGRA(ColorToRGB(clWindow)))
  else
    BGLViewPort(ClientWidth,ClientHeight,ColorToBGRA(ColorToRGB(Color)));

  RedrawContent(ctx);
  inherited DoOnPaint;
  SwapBuffers;

  FElapseAccumulator += FrameDiffTimeInMSecs;
  Inc(FElapseCount);
  if FElapseAccumulator >= 2000 then
  begin
    if Assigned(FOnFramesPerSecond) then
      FOnFramesPerSecond(self, ctx, 1000*FElapseCount div FElapseAccumulator);
    FElapseAccumulator := 0;
    FElapseCount := 0;
  end;

  If Assigned(FOnElapse) then
    FOnElapse(self, ctx, FrameDiffTimeInMSecs);

  ReleaseBGLContext(ctx);
end;

procedure TCustomBGLVirtualScreen.QueryLoadTextures;
begin
  FTexturesLoaded := false;
end;

procedure TCustomBGLVirtualScreen.LoadTextures;
var ctx: TBGLContext;
begin
  if MakeCurrent then
  begin
    if Assigned(FOnLoadTextures) then
    begin
      ctx := PrepareBGLContext;
      FOnLoadTextures(self, ctx);
      ReleaseBGLContext(ctx);
    end;
    FTexturesLoaded:= true;
  end;
end;

function TCustomBGLVirtualScreen.PrepareBGLContext: TBGLContext;
begin
  if FContextPrepared then
    raise exception.Create('Context already prepared');
  FOldSprites := BGRASpriteGL.BGLSpriteEngine;
  BGRASpriteGL.BGLSpriteEngine := FSprites;
  FOldShaderList := BGLCanvas.Lighting.ShaderList;
  BGLCanvas.Lighting.ShaderList := FShaderList;
  result.Canvas := BGLCanvas;
  result.Sprites := FSprites;
  FContextPrepared := true;
end;

procedure TCustomBGLVirtualScreen.ReleaseBGLContext(ctx: TBGLContext);
begin
  if not FContextPrepared then
    raise exception.Create('Context not prepared');
  ctx.Canvas.Lighting.ShaderList := FOldShaderList;
  BGRASpriteGL.BGLSpriteEngine := FOldSprites;
  FContextPrepared := false;
end;

procedure TCustomBGLVirtualScreen.UnloadTextures;
var ctx: TBGLContext;
begin
  if MakeCurrent then
  begin
    ctx := PrepareBGLContext;
    if Assigned(FOnUnloadTextures) then FOnUnloadTextures(self, ctx);
    FSprites.Clear;
    ctx.Canvas.Lighting.FreeShaders;
    ReleaseBGLContext(ctx);
    FTexturesLoaded := false;
  end;
end;

procedure TCustomBGLVirtualScreen.RedrawContent(ctx: TBGLContext);
var
  ARect: TRect;
  w: integer;
begin
  ARect := rect(0,0,ctx.Canvas.Width,ctx.Canvas.Height);
  w := BevelWidth;
  if w = 0 then w := 1;

  // if BevelOuter is set then draw a frame with BevelWidth
  if (BevelOuter <> bvNone) and (w > 0) then
    ctx.Canvas.Frame3d(ARect, w, BevelOuter); // Note: Frame3D inflates ARect

  InflateRect(ARect, -BorderWidth, -BorderWidth);

  // if BevelInner is set then skip the BorderWidth and draw a frame with BevelWidth
  if (BevelInner <> bvNone) and (w > 0) then
    ctx.Canvas.Frame3d(ARect, w, BevelInner); // Note: Frame3D inflates ARect

  if Assigned(FOnRedraw) then
    FOnRedraw(self, ctx);
end;

procedure TCustomBGLVirtualScreen.SetEnabled(Value: boolean);
begin
  if Value <> Enabled then Invalidate;
  inherited SetEnabled(Value);
end;

class procedure TCustomBGLVirtualScreen.OnAppIdle(Sender: TObject; var Done: Boolean);
var
  i: Integer;
begin
  if length(FToRedrawOnIdle) > 0 then
  begin
    for i := 0 to high(FToRedrawOnIdle) do
      FToRedrawOnIdle[i].Invalidate;
    Done:=false;
  end;
end;

constructor TCustomBGLVirtualScreen.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FTexturesLoaded:= False;
  AutoResizeViewport := true;
  FSprites := TBGLDefaultSpriteEngine.Create;
  FShaderList:= TStringList.Create;
end;

destructor TCustomBGLVirtualScreen.Destroy;
var
  i: Integer;
begin
  for i := 0 to FShaderList.Count-1 do
    FShaderList.Objects[i].Free;
  FShaderList.Free;
  RedrawOnIdle := false;
  FSprites.Free;
  inherited Destroy;
end;

end.

