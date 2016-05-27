unit umain;

{$mode objfpc}{$H+}

interface

{ This unit provides a user interface for showing the scenes, create the
  scene objects with different parameters, and handle mouse interaction. 
  
  It also show information about rendering counters and speed. 
  
  Scene 5 is handled differently in BGRASurfaceMouseMove because it is
  a first-person view, whereas in other scenes, it is the viewed object
  that gets rotated. }

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Spin, StdCtrls, BGRAVirtualScreen, BCButton, BCPanel, BGRABitmap, BGRAScene3D,
  BGLVirtualScreen, EpikTimer, BGRAOpenGL, BGRAOpenGL3D;

type

  { TForm1 }

  TForm1 = class(TForm)
    BCButton1: TBCButton;
    BCButton10: TBCButton;
    BCButton2: TBCButton;
    BCButton3: TBCButton;
    BCButton4: TBCButton;
    BCButton5: TBCButton;
    BCButton6: TBCButton;
    BCButton7: TBCButton;
    BCButton8: TBCButton;
    BCButton9: TBCButton;
    BGLSurface: TBGLVirtualScreen;
    BGRASurface: TBGRAVirtualScreen;
    ComboBox_Render: TComboBox;
    Label1: TLabel;
    SpinEdit_AA: TSpinEdit;
    Timer1: TTimer;
    vsToolbar: TBCPanel;
    procedure BCButton10Click(Sender: TObject);
    procedure BCButton1Click(Sender: TObject);
    procedure BCButton2Click(Sender: TObject);
    procedure BCButton3Click(Sender: TObject);
    procedure BCButton4Click(Sender: TObject);
    procedure BCButton5Click(Sender: TObject);
    procedure BCButton6Click(Sender: TObject);
    procedure BCButton7Click(Sender: TObject);
    procedure BCButton8Click(Sender: TObject);
    procedure BCButton9Click(Sender: TObject);
    procedure BGLSurfaceMouseEnter(Sender: TObject);
    procedure BGLSurfaceRedraw(Sender: TObject; BGLContext: TBGLContext);
    procedure BGRASurfaceMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure BGRASurfaceMouseEnter(Sender: TObject);
    procedure BGRASurfaceMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
    procedure BGRASurfaceMouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure BGRASurfaceRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure ComboBox_RenderChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit_AAChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure vsToolbarMouseEnter(Sender: TObject);
    procedure vsToolbarRedraw(Sender: TObject; Bitmap: TBGRABitmap);
  private
    { private declarations }
  public
    { public declarations }
    scene: TBGLScene3D;
    moving: boolean;
    moveOrigin: TPoint;
    timer: TEpikTimer;
    glFont: IBGLFont;
    procedure AdjustSceneSize;
    procedure RedrawScene;
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

uses ubgrasamples, ex1, ex2, ex3, ex4, ex5, BGRABitmapTypes;

{ TForm1 }

procedure TForm1.vsToolbarRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  DrawWin7ToolBar(Bitmap,vsToolBar.Align);
end;

procedure TForm1.AdjustSceneSize;
begin
  if ComboBox_Render.Text = 'OpenGL' then
  begin
    if BGRASurface.Visible then
    begin
      BGRASurface.Visible := false;
      BGLSurface.Visible := false;

      BGLSurface.Visible := true;
      BGLSurface.Align := alClient;
    end;
  end else
  begin
    if not BGRASurface.Visible then
    begin
      BGRASurface.Visible := false;
      BGLSurface.Visible := false;

      BGRASurface.Visible := true;
      BGLSurface.Visible := true;
      BGRASurface.Align := alLeft;
      BGRASurface.Width := ClientWidth div 2;
      BGLSurface.Align := alClient;
    end;
  end;
end;

procedure TForm1.RedrawScene;
begin
  if BGRASurface.Visible then BGRASurface.RedrawBitmap;
  if BGLSurface.Visible then BGLSurface.Repaint;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  scene := nil;
  timer := TEpikTimer.Create(nil);
  timer.TimebaseSource := HardwareTimebase;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(scene);
  timer.Free;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = '+' then
  begin
    if scene <> nil then
    begin
      scene.Zoom := scene.Zoom*1.5;
      Key := #0;
    end;
  end;
  if Key = '-' then
  begin
    if scene <> nil then
    begin
      scene.Zoom := scene.Zoom*(1/1.5);
      Key := #0;
    end;
  end;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  AdjustSceneSize;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  AdjustSceneSize;
end;

procedure TForm1.SpinEdit_AAChange(Sender: TObject);
begin

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled := false;
  if assigned(scene) and (scene is TExample2) then
    TExample2(scene).Elapse else
  if assigned(scene) and (scene is TExample4) then
    TExample4(scene).Elapse;
  RedrawScene;
end;

procedure TForm1.vsToolbarMouseEnter(Sender: TObject);
begin
  SpinEdit_AA.Enabled := true;
end;

procedure TForm1.BGRASurfaceRedraw(Sender: TObject; Bitmap: TBGRABitmap);
var h,cury: integer;

  procedure TextLine(str: string);
  var
    c: TBGRAPixel;
  begin
    c := Bitmap.GetPixel(0,cury+h div 2);
    if GetLightness(GammaExpansion(c)) > 32768 then
      c := BGRABlack else c := BGRAWhite;
    Bitmap.TextOut(0,cury,str,c);
    cury += h;
  end;

begin
  if scene <> nil then
  begin
    timer.Clear;
    timer.Start;

    scene.RenderingOptions.AntialiasingMode := am3dResample;
    scene.RenderingOptions.AntialiasingResampleLevel := SpinEdit_AA.Value;
    scene.RenderingOptions.MinZ := 1;

    scene.Surface := Bitmap;
    scene.Render;
    scene.Surface := nil;

    timer.Stop;

    Bitmap.FontFullHeight := 20;
    Bitmap.FontQuality := fqSystemClearType;
    h := Bitmap.FontFullHeight;

    cury := 0;
    TextLine(inttostr(round(timer.Elapsed*1000)) + ' ms');
    TextLine(inttostr(scene.Object3DCount) + ' object(s)');
    TextLine(inttostr(scene.VertexCount) + ' vertices');
    TextLine(inttostr(scene.FaceCount) + ' faces');
    TextLine(inttostr(scene.RenderedFaceCount) + ' rendered');
    TextLine(inttostr(scene.LightCount) + ' light(s)');
    Timer1.Enabled := true;
  end;
end;

procedure TForm1.ComboBox_RenderChange(Sender: TObject);
begin
  AdjustSceneSize;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  BGLSurface.UnloadTextures;
end;

procedure TForm1.BCButton1Click(Sender: TObject);
begin
  FreeAndNil(scene);
  scene := TExample1.Create;
  RedrawScene;
end;

procedure TForm1.BCButton10Click(Sender: TObject);
begin
  if not (scene is TExample4) then
  begin
    FreeAndNil(scene);
    scene := TExample4.Create;
    RedrawScene;
  end;
  TExample4(scene).NextModel;
end;

procedure TForm1.BCButton2Click(Sender: TObject);
begin
  if scene is TExample2 then
    TExample2(scene).Lighting := e2lNone
  else
  begin
    FreeAndNil(scene);
    scene := TExample2.Create(e2lNone);
  end;
  RedrawScene;
end;

procedure TForm1.BCButton3Click(Sender: TObject);
begin
  if scene is TExample2 then
    TExample2(scene).Lighting := e2lLightness
  else
  begin
    FreeAndNil(scene);
    scene := TExample2.Create(e2lLightness);
  end;
  RedrawScene;
end;

procedure TForm1.BCButton4Click(Sender: TObject);
begin
  if scene is TExample2 then
    TExample2(scene).Lighting := e2lColored
  else
  begin
    FreeAndNil(scene);
    scene := TExample2.Create(e2lColored);
  end;
  RedrawScene;
end;

procedure TForm1.BCButton5Click(Sender: TObject);
begin
  FreeAndNil(scene);
  scene := TExample3.Create;
  scene.DefaultLightingNormal := lnFace;
  scene.RenderingOptions.LightingInterpolation := liLowQuality;
  scene.RenderingOptions.AntialiasingMode := am3dMultishape;
  RedrawScene;
end;

procedure TForm1.BCButton6Click(Sender: TObject);
begin
  FreeAndNil(scene);
  scene := TExample3.Create;
  scene.DefaultLightingNormal := lnFaceVertexMix;
  scene.RenderingOptions.LightingInterpolation := liLowQuality;
  scene.RenderingOptions.AntialiasingMode := am3dResample;
  RedrawScene;
end;

procedure TForm1.BCButton7Click(Sender: TObject);
begin
  FreeAndNil(scene);
  scene := TExample3.Create;
  scene.DefaultLightingNormal := lnVertex;
  scene.RenderingOptions.LightingInterpolation := liSpecularHighQuality;
  RedrawScene;
end;

procedure TForm1.BCButton8Click(Sender: TObject);
begin
  if not (scene is TExample4) then
  begin
    FreeAndNil(scene);
    scene := TExample4.Create;
    RedrawScene;
  end;
end;

procedure TForm1.BCButton9Click(Sender: TObject);
begin
  FreeAndNil(scene);
  scene := TExample5.Create;
  RedrawScene;
end;

procedure TForm1.BGLSurfaceMouseEnter(Sender: TObject);
begin
  SpinEdit_AA.Enabled := false;
end;

procedure TForm1.BGLSurfaceRedraw(Sender: TObject; BGLContext: TBGLContext);
var h,cury: integer;

  procedure TextLine(str: string);
  var
    c: TBGRAPixel;
  begin
    {c := Bitmap.GetPixel(0,cury+h div 2);
    if GetLightness(GammaExpansion(c)) > 32768 then
      c := BGRABlack else }c := BGRAWhite;
    glFont.TextOut(0,cury,str,c);
    cury += h;
  end;
begin
  if scene <> nil then
  begin
    timer.Clear;
    timer.Start;

    scene.RenderingOptions.AntialiasingMode := am3dResample;
    scene.RenderingOptions.AntialiasingResampleLevel := SpinEdit_AA.Value;
    scene.RenderingOptions.MinZ := 1;
    scene.RenderGL(BGLContext.Canvas);
    BGLContext.Canvas.WaitForGPU(wfgFinishAllCommands);

    timer.Stop;

    h := 20;
    if glFont = nil then
      glFont := BGLFont('Arial',-h);

    cury := 0;
    TextLine(inttostr(round(timer.Elapsed*1000)) + ' ms');
    TextLine(inttostr(scene.Object3DCount) + ' object(s)');
    TextLine(inttostr(scene.VertexCount) + ' vertices');
    TextLine(inttostr(scene.FaceCount) + ' faces');
    TextLine(inttostr(scene.RenderedFaceCount) + ' rendered');
    TextLine(inttostr(scene.LightCount) + ' light(s)');
    Timer1.Enabled := true;
  end;
end;

procedure TForm1.BGRASurfaceMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (button = mbLeft) and (scene <> nil) then
  begin
    moving := true;
    moveOrigin := point(x,y);
  end;
end;

procedure TForm1.BGRASurfaceMouseEnter(Sender: TObject);
begin
  SpinEdit_AA.Enabled := false;
end;

procedure TForm1.BGRASurfaceMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if moving then
  begin
    if scene is TExample5 then
    begin
      scene.LookRight(X-moveOrigin.X);
      scene.LookDown(Y-moveOrigin.Y);
    end else
    if scene.Object3DCount > 0 then
    begin
      scene.Object3D[0].MainPart.RotateYDeg(-(X-moveOrigin.X),False);
      scene.Object3D[0].MainPart.RotateXDeg(Y-moveOrigin.Y,False);
    end;
    RedrawScene;
    moveOrigin := point(x,y);
  end;
end;

procedure TForm1.BGRASurfaceMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if button = mbLeft then moving := false;
end;

end.

