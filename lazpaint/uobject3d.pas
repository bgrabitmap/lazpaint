unit UObject3D;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ExtCtrls, ComCtrls, BGRAVirtualScreen, BGRAKnob,
  BGRAImageList, BGRABitmap, BGRAScene3D, LazPaintType, BGRABitmapTypes,
  UConfig;

const
  AntialiasingLevelWhenFixed = 2;
  AntialiasingLevelWhenMoving = 1;

type

  { TScene }

  TScene = class(TBGRAScene3D)
  private
    FTextures: array of record
        Name: string;
        Texture: TBGRABitmap;
        Usage: integer;
    end;
    procedure NoTextures;
  public
    TexturePath: string;
    procedure ComputeTexCoord(AFace: IBGRAFace3D; AWidth, AHeight: integer);
    function FetchTexture(AName: string; out texSize: TPointF): IBGRAScanner; override;
    function FetchTextureAsBitmap(AName: string; ARelativePath: boolean): TBGRABitmap;
    procedure ReleaseTextureReference(ref: IBGRAScanner);
    procedure QueryTextureReference(ref: IBGRAScanner);
    procedure Clear; override;
    procedure FreeUnusedTextures;
  end;

  { TFObject3D }

  TFObject3D = class(TForm)
    BGRAImageList1: TBGRAImageList;
    BGRAKnob_Zoom: TBGRAKnob;
    BGRAView3D: TBGRAVirtualScreen;
    Button_LoadTex: TButton;
    Button_NoTex: TButton;
    Button_Cancel: TButton;
    Button_OK: TButton;
    CheckBox_Antialiasing: TCheckBox;
    CheckBox_Biface: TCheckBox;
    CheckBox_TextureInterp: TCheckBox;
    ColorDialog1: TColorDialog;
    ComboBox_Normals: TComboBox;
    GroupBox_SelectedMaterial: TGroupBox;
    GroupBox_SelectedLight: TGroupBox;
    Label_Color1: TLabel;
    Label_Materials: TLabel;
    Label_Lights: TLabel;
    Label_Zoom: TLabel;
    Label_LightingNormals: TLabel;
    Label_SpecularIndex: TLabel;
    Label_Color: TLabel;
    Label_Opacity: TLabel;
    Label_Width: TLabel;
    Label_Height: TLabel;
    ListBox_Materials: TListBox;
    ListBox_Lights: TListBox;
    OpenTextureDialog: TOpenDialog;
    PageControl1: TPageControl;
    PaintBox_LightPos: TPaintBox;
    Shape_MaterialColor: TShape;
    Shape_LightColor: TShape;
    SpinEdit_ColorOpacity: TSpinEdit;
    SpinEdit_Height: TSpinEdit;
    SpinEdit_SpecularIndex: TSpinEdit;
    Rendering: TTabSheet;
    Materials: TTabSheet;
    SpinEdit_Width: TSpinEdit;
    Lights: TTabSheet;
    ToolBar1: TToolBar;
    ToolAddDirectional: TToolButton;
    ToolPointLight: TToolButton;
    ToolRemoveSelectedLight: TToolButton;
    procedure BGRAKnob_ZoomValueChanged(Sender: TObject; Value: single);
    procedure BGRAView3DMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure BGRAView3DMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
    procedure BGRAView3DMouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure BGRAView3DRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure BGRAView3DResize(Sender: TObject);
    procedure Button_LoadTexClick(Sender: TObject);
    procedure Button_NoTexClick(Sender: TObject);
    procedure CheckBox_AntialiasingChange(Sender: TObject);
    procedure CheckBox_BifaceChange(Sender: TObject);
    procedure CheckBox_TextureInterpChange(Sender: TObject);
    procedure ComboBox_NormalsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox_LightsKeyPress(Sender: TObject; var Key: char);
    procedure ListBox_LightsSelectionChange(Sender: TObject; User: boolean);
    procedure ListBox_MaterialsKeyPress(Sender: TObject; var Key: char);
    procedure ListBox_MaterialsSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure PaintBox_LightPosMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure PaintBox_LightPosMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure PaintBox_LightPosMouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure PaintBox_LightPosPaint(Sender: TObject);
    procedure Shape_LightColorMouseUp(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure Shape_MaterialColorMouseDown(Sender: TObject;
      {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure SpinEdit_ColorOpacityChange(Sender: TObject);
    procedure SpinEdit_ColorOpacityKeyPress(Sender: TObject; var Key: char);
    procedure SpinEdit_HeightKeyPress(Sender: TObject; var Key: char);
    procedure SpinEdit_SpecularIndexChange(Sender: TObject);
    procedure SpinEdit_SpecularIndexKeyPress(Sender: TObject; var Key: char);
    procedure SpinEdit_WidthKeyPress(Sender: TObject; var Key: char);
    procedure ToolAddDirectionalClick(Sender: TObject);
    procedure ToolPointLightClick(Sender: TObject);
    procedure ToolRemoveSelectedLightClick(Sender: TObject);
  private
    { private declarations }
    procedure UpdateTabSize;
    procedure CheckKey(var Key: char);
    procedure DoLoadTexture(AFilename: string);
    procedure DoFreeTexture;
    function MaterialHasTexture: boolean;
    procedure UpdateLightList;
    procedure UpdateSelectedLight;
    procedure SetLightPos(x,y: single);
  public
    { public declarations }
    scene : TScene;
    moving,rotating: boolean;
    moveOrigin: TPoint;
    previousAngle: single;
    previousZoom: single;
    materialIndex, lightIndex: integer;
    InnerTabBottomPadding: integer;
    Config: TLazPaintConfig;
  end;

function ShowObject3DDlg({%H-}Instance: TLazPaintCustomInstance; filenameUTF8: string; maxWidth, maxHeight: integer): TBGRABitmap;

implementation

uses ugraph, uscaledpi, umac, ULoadImage;

const PointLightDist = 80;

{ TScene }

procedure TScene.NoTextures;
var i: integer;
begin
  for i := 0 to MaterialCount-1 do
  begin
    ReleaseTextureReference(Material[i].Texture);
    Material[i].Texture := nil;
  end;
end;

procedure TScene.ComputeTexCoord(AFace: IBGRAFace3D; AWidth,AHeight: integer);
var
  j: integer;
  p1,p2,p3,u,v: TPoint3d;
  min,max,pt: TPointF;
  factor: single;
begin
  if AFace.VertexCount < 3 then exit;

  j := 0;
  p1 := AFace.Vertex[j].GetSceneCoord;
  repeat
    inc(j);
    if j >= AFace.VertexCount then exit;
    p2 := AFace.Vertex[j].GetSceneCoord;
    u := p2-p1;
  until u*u <> 0;
  Normalize3D(u);
  repeat
    inc(j);
    if j >= AFace.VertexCount then exit;
    p3 := AFace.Vertex[j].GetSceneCoord;
    v := p3-p2;
    v := v - u*(u*v);
  until v*v <> 0;
  Normalize3D(v);

  for j := 0 to AFace.VertexCount-1 do
  with AFace.Vertex[j] do
  begin
    pt := PointF((GetSceneCoord-p1)*u,(GetSceneCoord-p1)*v);
    if j = 0 then
    begin
      min := pt;
      max := pt;
    end else
    begin
      if pt.x < min.x then min.x := pt.x else
      if pt.x > max.x then max.x := pt.x;
      if pt.y < min.y then min.y := pt.y else
      if pt.y > max.y then max.y := pt.y;
    end;
  end;
  if min.x = max.x then max.x := min.x+1;
  if min.y = max.y then max.y := min.y+1;
  factor := AWidth/(max.x-min.x);
  if AHeight/(max.y-min.y) < factor then factor := AHeight/(max.y-min.y);
  for j := 0 to AFace.VertexCount-1 do
  with AFace.Vertex[j] do
  begin
    pt := PointF((GetSceneCoord-p1)*u,(GetSceneCoord-p1)*v);
    pt := PointF((pt.x-min.x)*factor,(pt.y-min.y)*factor);
    AFace.TexCoord[j] := pt;
  end;
end;

function TScene.FetchTexture(AName: string; out texSize: TPointF): IBGRAScanner;
var bmp: TBGRABitmap;
begin
  bmp := FetchTextureAsBitmap(AName, True);
  result := bmp;
  texSize := PointF(bmp.Width,bmp.Height);
end;

function TScene.FetchTextureAsBitmap(AName: string; ARelativePath: boolean): TBGRABitmap;
  function AddTexture(AFilename: string): TBGRABitmap;
  var bmp: TBGRABitmap;
    dummy: string;
  begin
    bmp := nil;
    try
      bmp := LoadFlatImageUTF8(AFilename,dummy,'');
    except
      on ex:exception do ShowMessage(ex.Message);
    end;
    setlength(FTextures, length(FTextures)+1);
    FTextures[high(FTextures)].Name:= AName;
    FTextures[high(FTextures)].Texture:= bmp;
    result := bmp;
  end;

var i: integer;
begin
  for i := 0 to High(FTextures) do
    if FTextures[i].Name = AName then
    begin
      result := FTextures[i].Texture;
      exit;
    end;
  if ARelativePath and FileExistsUTF8(AppendPathDelim(TexturePath) + AName) then
    result := AddTexture(AppendPathDelim(TexturePath) + AName)
  else if not ARelativePath and FileExistsUTF8(AName) then
    result := AddTexture(AName)
  else
    result := AddTexture('');
end;

procedure TScene.ReleaseTextureReference(ref: IBGRAScanner);
var i: integer;
  comp: IBGRAScanner;
begin
  for i := 0 to high(FTextures) do
  begin
    comp := FTextures[i].Texture;
    if comp = ref then
    begin
      dec(FTextures[i].Usage);
      exit;
    end;
  end;
end;

procedure TScene.QueryTextureReference(ref: IBGRAScanner);
var i: integer;
  comp: IBGRAScanner;
begin
  for i := 0 to high(FTextures) do
  begin
    comp := FTextures[i].Texture;
    if comp = ref then
    begin
      inc(FTextures[i].Usage);
      exit;
    end;
  end;
end;

procedure TScene.Clear;
begin
  NoTextures;
  FreeUnusedTextures;
  inherited Clear;
end;

procedure TScene.FreeUnusedTextures;
var i,j,usage: integer;
begin
  for i:= high(FTextures) downto 0 do
  begin
    if FTextures[i].Texture <> nil then
    begin
      usage := FTextures[i].Usage;
      if usage = 0 then
      begin
        FreeAndNil(FTextures[i].Texture);
        for j := i to high(FTextures)-1 do
          FTextures[j] := FTextures[j+1];
        setlength(FTextures,length(FTextures)-1);
      end;
    end;
  end;
end;

{ TFObject3D }

procedure TFObject3D.BGRAView3DRedraw(Sender: TObject;
  Bitmap: TBGRABitmap);
begin
  DrawCheckers(Bitmap,rect(0,0,Bitmap.Width,Bitmap.Height));
  scene.Surface := Bitmap;
  scene.Render;
end;

procedure TFObject3D.BGRAView3DResize(Sender: TObject);
begin
  BGRAView3D.DiscardBitmap;
end;

procedure TFObject3D.Button_LoadTexClick(Sender: TObject);
begin
  if materialIndex <> -1 then
  begin
    OpenTextureDialog.InitialDir := Config.DefaultTextureDirectory;
    if OpenTextureDialog.Execute then
    begin
      DoLoadTexture(OpenTextureDialog.Filename);
      scene.FreeUnusedTextures;
      BGRAView3D.DiscardBitmap;
    end;
  end;
end;

procedure TFObject3D.Button_NoTexClick(Sender: TObject);
begin
  if MaterialHasTexture then
  begin
    DoFreeTexture;
    scene.FreeUnusedTextures;
    BGRAView3D.DiscardBitmap;
  end;
end;

procedure TFObject3D.CheckBox_AntialiasingChange(Sender: TObject);
begin
  if CheckBox_Antialiasing.Checked then
    scene.RenderingOptions.AntialiasingMode := am3dResample
  else
    scene.RenderingOptions.AntialiasingMode := am3dNone;
  BGRAView3D.RedrawBitmap;
end;

procedure TFObject3D.CheckBox_BifaceChange(Sender: TObject);
begin
  if scene.Object3DCount > 0 then
  begin
    scene.Object3D[0].SetBiface(CheckBox_Biface.Checked);
    BGRAView3D.RedrawBitmap;
  end;
end;

procedure TFObject3D.BGRAView3DMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if not rotating and (button = mbLeft) and (scene <> nil) then
  begin
    moving := true;
    moveOrigin := point(x,y);
  end else
  if not moving and (button = mbRight) and (scene <> nil) then
  begin
    rotating := true;
    previousAngle := ComputeAngle(x-BGRAView3D.Width/2,y-BGRAView3D.Height/2);
  end;
end;

procedure TFObject3D.BGRAKnob_ZoomValueChanged(Sender: TObject; Value: single);
begin
  if scene.Object3DCount > 0 then
  begin
    scene.Object3D[0].MainPart.Scale(Value/previousZoom,false);
    BGRAView3D.DiscardBitmap;
  end;

  previousZoom := Value;
end;

procedure TFObject3D.BGRAView3DMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var angle: single;
begin
  if moving then
  begin
    if scene.Object3DCount > 0 then
    begin
      scene.RenderingOptions.AntialiasingResampleLevel := AntialiasingLevelWhenMoving;
      scene.Object3D[0].MainPart.RotateYDeg(-(X-moveOrigin.X),False);
      scene.Object3D[0].MainPart.RotateXDeg(Y-moveOrigin.Y,False);
      BGRAView3D.RedrawBitmap;
    end;
    moveOrigin := point(x,y);
  end else
  if rotating then
  begin
    angle := ComputeAngle(x-BGRAView3D.Width/2,y-BGRAView3D.Height/2);
    if scene.Object3DCount > 0 then
    begin
      scene.RenderingOptions.AntialiasingResampleLevel := AntialiasingLevelWhenMoving;
      scene.Object3D[0].MainPart.RotateZDeg(angle-previousAngle,False);
      BGRAView3D.RedrawBitmap;
    end;
    previousAngle := angle;
  end;
end;

procedure TFObject3D.BGRAView3DMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (button = mbLeft) and moving then
  begin
    moving := false;
    scene.RenderingOptions.AntialiasingResampleLevel := AntialiasingLevelWhenFixed;
    BGRAView3D.RedrawBitmap;
  end
  else if (button = mbRight) and rotating then
  begin
    rotating := false;
    scene.RenderingOptions.AntialiasingResampleLevel := AntialiasingLevelWhenFixed;
    BGRAView3D.RedrawBitmap;
  end;
end;

procedure TFObject3D.CheckBox_TextureInterpChange(Sender: TObject);
begin
  scene.RenderingOptions.TextureInterpolation := checkbox_textureinterp.Checked;
  BGRAView3D.RedrawBitmap;
end;

procedure TFObject3D.ComboBox_NormalsChange(Sender: TObject);
begin
  scene.DefaultLightingNormal := TLightingNormal3D(ComboBox_Normals.ItemIndex);
  BGRAView3D.RedrawBitmap;
end;

procedure TFObject3D.FormCreate(Sender: TObject);
begin
  ScaleDPI(Self,OriginalDPI);

  CheckOKCancelBtns(Button_OK,Button_Cancel);
  CheckSpinEdit(SpinEdit_ColorOpacity);
  CheckSpinEdit(SpinEdit_SpecularIndex);
  CheckSpinEdit(SpinEdit_Width);
  CheckSpinEdit(SpinEdit_Height);

  scene := TScene.Create;
  scene.DefaultLightingNormal := lnFaceVertexMix;
  with scene.RenderingOptions do
  begin
    LightingInterpolation := liAlwaysHighQuality;
    AntialiasingMode := am3dResample;
    AntialiasingResampleLevel := AntialiasingLevelWhenFixed;
    PerspectiveMode := pmZBuffer;
    TextureInterpolation := True;
  end;
  previousZoom := BGRAKnob_Zoom.Value;
  ComboBox_Normals.Items.Add('None');
  ComboBox_Normals.Items.Add('Flat faces');
  ComboBox_Normals.Items.Add('Rounded faces');
  ComboBox_Normals.Items.Add('Intermediate');
  materialIndex:= -1;

  InnerTabBottomPadding := PageControl1.Height - (GroupBox_SelectedMaterial.Top+GroupBox_SelectedMaterial.Height);
  UpdateTabSize;
end;

procedure TFObject3D.FormDestroy(Sender: TObject);
begin
  scene.Free;
end;

procedure TFObject3D.FormKeyPress(Sender: TObject; var Key: char);
begin
  CheckKey(Key);
end;

procedure TFObject3D.FormResize(Sender: TObject);
begin
  UpdateTabSize;
end;

procedure TFObject3D.FormShow(Sender: TObject);
var i: integer;
begin
  ListBox_Materials.Clear;
  for i := 0 to scene.MaterialCount-1 do
  begin
    if scene.Material[i] = scene.DefaultMaterial then
      ListBox_Materials.Items.Add('<default>')
    else
      ListBox_Materials.Items.Add(scene.Material[i].Name);
  end;
  if ListBox_Materials.Items.Count > 0 then
    ListBox_Materials.ItemIndex := 0;
  if ComboBox_Normals.Items.Count > ord(scene.DefaultLightingNormal) then
    ComboBox_Normals.ItemIndex := ord(scene.DefaultLightingNormal);
  if scene.LightCount > 0 then
    lightIndex := 0
  else
    lightIndex := -1;
  UpdateLightList;
  UpdateSelectedLight;
end;

procedure TFObject3D.ListBox_LightsKeyPress(Sender: TObject; var Key: char);
begin
  CheckKey(Key);
end;

procedure TFObject3D.ListBox_LightsSelectionChange(Sender: TObject;
  User: boolean);
begin
  if User then UpdateSelectedLight;
end;

procedure TFObject3D.ListBox_MaterialsKeyPress(Sender: TObject; var Key: char);
begin
  CheckKey(Key);
end;

procedure TFObject3D.ListBox_MaterialsSelectionChange(Sender: TObject;
  User: boolean);
begin
  if ListBox_Materials.ItemIndex <> -1 then
  begin
    materialIndex := ListBox_Materials.ItemIndex;
    SpinEdit_SpecularIndex.Enabled := false;
    if scene.Material[materialIndex].SpecularOn then
      SpinEdit_SpecularIndex.Value := scene.Material[materialIndex].SpecularIndex
    else
      SpinEdit_SpecularIndex.Value := 0;
    SpinEdit_SpecularIndex.Enabled := true;
    Shape_MaterialColor.Brush.Color := BGRAToColor(scene.Material[materialIndex].SimpleColor);
    Shape_MaterialColor.Enabled := true;
    SpinEdit_ColorOpacity.Enabled := false;
    SpinEdit_ColorOpacity.Value := scene.Material[materialIndex].SimpleAlpha;
    SpinEdit_ColorOpacity.Enabled := true;
  end;
end;

procedure TFObject3D.PaintBox_LightPosMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then SetLightPos(X/PaintBox_LightPos.Width,Y/PaintBox_LightPos.Height);
end;

procedure TFObject3D.PaintBox_LightPosMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then
    SetLightPos(X/PaintBox_LightPos.Width,Y/PaintBox_LightPos.Height);
end;

procedure TFObject3D.PaintBox_LightPosMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    scene.RenderingOptions.AntialiasingResampleLevel := AntialiasingLevelWhenFixed;
    BGRAView3D.RedrawBitmap;
  end;
end;

procedure TFObject3D.PaintBox_LightPosPaint(Sender: TObject);
var x,y: integer;
  pt: TPointF;
  light: IBGRALight3D;
begin
  if lightIndex <> -1 then
  begin
    light := scene.Light[lightIndex];
    if light.IsDirectional then
    begin
      with (light as IBGRADirectionalLight3D).Direction do
        pt := PointF(-x/2+0.5,-y/2+0.5);
    end else
    begin
      with (light as IBGRAPointLight3D).Vertex.GetSceneCoord do
        pt := PointF(x/2/PointLightDist+0.5,y/2/PointLightDist+0.5);
    end;
    with PaintBox_LightPos do
    begin
      x := round(pt.X*Width);
      y := round(pt.Y*Height);
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.Color:= clBtnFace;
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Color := clWindowText;
      Canvas.Rectangle(0,0,Width,Height);
      Canvas.Pen.Color := clBlack;
      Canvas.Brush.Style := bsSolid;
      Canvas.Brush.Color := clWhite;
      Canvas.Ellipse(x-3,y-3,x+4,y+4);
    end;
  end;
end;

procedure TFObject3D.Shape_LightColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if lightIndex <> -1 then
  begin
    ColorDialog1.Color := BGRAToColor(scene.Light[materialIndex].Color);
    if ColorDialog1.Execute then
    begin
      Shape_LightColor.Brush.Color := ColorDialog1.Color;
      scene.Light[lightIndex].Color := ColorToBGRA(ColorDialog1.Color);
      BGRAView3D.DiscardBitmap;
    end;
  end;
end;

procedure TFObject3D.Shape_MaterialColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if materialIndex <> -1 then
  begin
    ColorDialog1.Color := BGRAToColor(scene.Material[materialIndex].SimpleColor);
    if ColorDialog1.Execute then
    begin
      Shape_MaterialColor.Brush.Color := ColorDialog1.Color;
      scene.Material[materialIndex].SimpleColor := ColorToBGRA(ColorDialog1.Color,scene.Material[materialIndex].SimpleAlpha);
      BGRAView3D.DiscardBitmap;
    end;
  end;
end;

procedure TFObject3D.SpinEdit_ColorOpacityChange(Sender: TObject);
begin
  if SpinEdit_ColorOpacity.Enabled and (materialIndex <> -1) then
  begin
    scene.Material[materialIndex].SimpleAlpha := SpinEdit_ColorOpacity.Value;
    BGRAView3D.DiscardBitmap;
  end;
end;

procedure TFObject3D.SpinEdit_ColorOpacityKeyPress(Sender: TObject;
  var Key: char);
begin
  CheckKey(Key);
end;

procedure TFObject3D.SpinEdit_HeightKeyPress(Sender: TObject; var Key: char);
begin
   CheckKey(Key);
end;

procedure TFObject3D.SpinEdit_SpecularIndexChange(Sender: TObject);
begin
  if SpinEdit_SpecularIndex.Enabled and (materialIndex <> -1) then
  begin
    scene.Material[materialIndex].SpecularIndex := SpinEdit_SpecularIndex.Value;
    BGRAView3D.DiscardBitmap;
  end;
end;

procedure TFObject3D.SpinEdit_SpecularIndexKeyPress(Sender: TObject;
  var Key: char);
begin
  CheckKey(Key);
end;

procedure TFObject3D.SpinEdit_WidthKeyPress(Sender: TObject; var Key: char);
begin
  CheckKey(Key);
end;

procedure TFObject3D.ToolAddDirectionalClick(Sender: TObject);
begin
  if scene.LightCount < 10 then
  begin
    scene.AddDirectionalLight(Point3D(1,1,1),BGRAWhite,-0.5); //add a directional light from top-left
    lightIndex:= scene.LightCount-1;
    UpdateLightList;
    UpdateSelectedLight;
    BGRAView3D.DiscardBitmap;
  end;
end;

procedure TFObject3D.ToolPointLightClick(Sender: TObject);
const OneOverSqrt3 = 0.57735026918962576450914878050196;
begin
  if scene.LightCount < 10 then
  begin
    scene.AddPointLight(scene.CreateObject.MainPart.Add(-OneOverSqrt3*PointLightDist,-OneOverSqrt3*PointLightDist,-OneOverSqrt3*PointLightDist),PointLightDist,BGRAWhite);
    lightIndex:= scene.LightCount-1;
    UpdateLightList;
    UpdateSelectedLight;
    BGRAView3D.DiscardBitmap;
  end;
end;

procedure TFObject3D.ToolRemoveSelectedLightClick(Sender: TObject);
begin
  if lightIndex <> -1 then
  begin
    scene.RemoveLight(scene.Light[lightIndex]);
    if lightIndex >= scene.LightCount then dec(lightIndex);
    UpdateLightList;
    UpdateSelectedLight;
    BGRAView3D.DiscardBitmap;
  end;
end;

procedure TFObject3D.UpdateTabSize;
begin
  GroupBox_SelectedMaterial.Top := PageControl1.Height - InnerTabBottomPadding - GroupBox_SelectedMaterial.Height;
  ListBox_Materials.Height := GroupBox_SelectedMaterial.Top-2-ListBox_Materials.Top;
  GroupBox_SelectedLight.Top := PageControl1.Height - InnerTabBottomPadding - GroupBox_SelectedLight.Height;
  ListBox_Lights.Height := GroupBox_SelectedLight.Top-2-ListBox_Lights.Top;
end;

procedure TFObject3D.CheckKey(var Key: char);
begin
  if (Key = '+') or (Key = '-') then
  begin
    if Key = '+' then
      scene.Object3D[0].MainPart.Scale(1.1,false) else
      scene.Object3D[0].MainPart.Scale(1/1.1,false);
    Key := #0;
    BGRAView3D.DiscardBitmap;
  end;
end;

procedure TFObject3D.DoLoadTexture(AFilename: string);
var
  mat: IBGRAMaterial3D;
  i,j: integer;
  bmp: TBGRABitmap;
begin
  if materialIndex = -1 then exit;

  Config.SetDefaultTextureDirectory(ExtractFilePath(AFilename));
  mat := scene.Material[materialIndex];
  bmp := scene.FetchTextureAsBitmap(OpenTextureDialog.FileName,False);
  scene.ReleaseTextureReference(mat.Texture);
  scene.QueryTextureReference(bmp);
  mat.Texture := bmp;
  mat.TextureZoom := PointF(1,1);
  mat.SimpleColor := BGRAWhite;
  Shape_MaterialColor.Brush.Color := BGRAToColor(mat.SimpleColor);
  for i := 0 to scene.Object3DCount-1 do
    with scene.Object3D[i] do
    begin
      for j := 0 to FaceCount-1 do
        if Face[j].Material = mat then
          self.scene.ComputeTexCoord(Face[j],bmp.Width,bmp.Height);
    end;
end;

procedure TFObject3D.DoFreeTexture;
var
  mat: IBGRAMaterial3D;
begin
  if materialIndex = -1 then exit;

  mat := scene.Material[materialIndex];
  scene.ReleaseTextureReference(mat.Texture);
  mat.Texture := nil;
  mat.SimpleColor := scene.UnknownColor;
  Shape_MaterialColor.Brush.Color := BGRAToColor(mat.SimpleColor);
end;

function TFObject3D.MaterialHasTexture: boolean;
begin
  if materialIndex <> -1 then
    result := scene.Material[materialIndex].Texture <> nil
  else
    result := false;
end;

procedure TFObject3D.UpdateLightList;
var
  i: Integer;
begin
  ListBox_Lights.Clear;
  for i := 0 to scene.LightCount-1 do
  begin
    if scene.Light[i].IsDirectional then
      ListBox_Lights.Items.Add('#'+inttostr(i+1)+' Directional')
    else
      ListBox_Lights.Items.Add('#'+inttostr(i+1)+' Point');
  end;
  ListBox_Lights.ItemIndex := lightIndex;
end;

procedure TFObject3D.UpdateSelectedLight;
begin
  lightIndex:= ListBox_Lights.ItemIndex;
  if lightIndex <> -1 then
  begin
    GroupBox_SelectedLight.Enabled := true;
    Shape_LightColor.Brush.Color := BGRAToColor(scene.Light[lightIndex].Color);
    Shape_LightColor.Enabled := true;
    PaintBox_LightPos.Repaint;
  end else
  begin
    GroupBox_SelectedLight.Enabled := false;
  end;
end;

procedure TFObject3D.SetLightPos(x, y: single);
var light: IBGRALight3D;
  xy,z: single;
begin
  if lightIndex <> -1 then
  begin
    light := scene.Light[lightIndex];
    x := (x-0.5)*2;
    y := (y-0.5)*2;
    xy := sqrt(sqr(x)+sqr(y));
    if xy >= 1 then
    begin
      x /= xy;
      y /= xy;
      z := 0;
    end else
      z := sqrt(1-sqr(xy));

    if light.IsDirectional then
      (light as IBGRADirectionalLight3D).Direction := Point3D(-x,-y,z)
    else
      (light as IBGRAPointLight3D).Vertex.SceneCoord := Point3D(x*PointLightDist,y*PointLightDist,-z*PointLightDist);
    PaintBox_LightPos.Repaint;
    scene.RenderingOptions.AntialiasingResampleLevel := AntialiasingLevelWhenMoving;
    BGRAView3D.DiscardBitmap;
  end;
end;

function DoLoadObject(scene: TScene; filenameUTF8: string): boolean;
var
  obj: IBGRAObject3D;
  r: single;
  matFile: string;
  i: integer;
begin
  scene.TexturePath := ExtractFilePath(filenameUTF8);
  result := false;

  matFile := ChangeFileExt(filenameUTF8,'.mtl');
  if FileExistsUTF8(matFile) then
  begin
    scene.LoadMaterialsFromFileUTF8(matFile);
    for i := 0 to scene.MaterialCount-1 do
      scene.QueryTextureReference(scene.Material[i].Texture);
  end;
  obj := scene.LoadObjectFromFileUTF8(filenameUTF8);
  if obj <> nil then
  begin
    scene.DefaultLightingNormal := obj.LightingNormal;
    obj.ParentLighting := true; //set for the whole scene with the dialog box
    with obj do
    begin
      with MainPart.BoundingBox do
        MainPart.Translate((min+max)*(-1/2), False);
      r := MainPart.Radius;
      if r <> 0 then MainPart.Scale(50/r, False);
      MainPart.RotateXDeg(180-20, False);
      MainPart.RotateYDeg(-20, False);
    end;
    with scene do
    begin
      //set ambiant lightness to dark (1 is normal lightness)
      AmbiantLightness := 0.5;
      AddDirectionalLight(Point3D(1,1,1),BGRAWhite,-0.5); //add a directional light from top-left
    end;
    result := true;
  end;
end;

var
  f: TFObject3D;
function ShowObject3DDlg(Instance: TLazPaintCustomInstance; filenameUTF8: string;
  maxWidth, maxHeight: integer): TBGRABitmap;
begin
  if f = nil then
  begin
    f:= TFObject3D.Create(nil);
    f.Config := Instance.Config;
  end;
  result := nil;
  try
    if DoLoadObject(f.scene,filenameUTF8) then
    begin
      f.BGRAView3D.DiscardBitmap;
      f.SpinEdit_Width.MaxValue := maxWidth;
      f.SpinEdit_Width.Value := maxWidth;
      f.SpinEdit_Height.MaxValue := maxHeight;
      f.SpinEdit_Height.Value := maxHeight;
      if f.ShowModal = mrOK then
      begin
        result := TBGRABitmap.Create(f.SpinEdit_Width.Value,f.SpinEdit_Height.Value);
        f.scene.Surface := result;
        f.scene.RenderingOptions.AntialiasingResampleLevel := 5;
        f.scene.Render;
      end;
    end;
  except
    on ex:Exception do
      Instance.ShowError('ShowObject3DDlg',ex.Message);
  end;
  f.scene.Clear;
end;

{$R *.lfm}

finalization

  FreeAndNil(f);

end.

