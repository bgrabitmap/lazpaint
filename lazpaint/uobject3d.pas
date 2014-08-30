unit UObject3D;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ExtCtrls, ComCtrls, BGRAVirtualScreen, BGRAKnob, BGRABitmap,
  BGRAScene3D, LazPaintType, BGRABitmapTypes, UConfig;

type

  { TScene }

  TScene = class(TBGRAScene3D)
  public
    FCustomMaterials: array of record
        Name: string;
        Material3D: IBGRAMaterial3D;
        Color: TBGRAPixel;
        Texture: TBGRABitmap;
      end;
    FCustomMaterialCount: integer;
    procedure ClearCoordFlag(AVertex: IBGRAVertex3D);
    procedure SetCoordFlag(AVertex: IBGRAVertex3D);
    function GetCoordFlag(AVertex: IBGRAVertex3D): boolean;
    procedure UseMaterial(AMaterialName: string; AFace: IBGRAFace3D); override;
    procedure UpdateMaterials; override;
    procedure UpdateMaterial(AMaterialName: string); override;
    destructor Destroy; override;
  end;

  { TFObject3D }

  TFObject3D = class(TForm)
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
    Label_Materials: TLabel;
    Label_Zoom: TLabel;
    Label_LightingNormals: TLabel;
    Label_SpecularIndex: TLabel;
    Label_Color: TLabel;
    Label_Opacity: TLabel;
    Label_Width: TLabel;
    Label_Height: TLabel;
    ListBox_Materials: TListBox;
    OpenTextureDialog: TOpenDialog;
    PageControl1: TPageControl;
    Shape_MaterialColor: TShape;
    SpinEdit_ColorOpacity: TSpinEdit;
    SpinEdit_Height: TSpinEdit;
    SpinEdit_SpecularIndex: TSpinEdit;
    Rendering: TTabSheet;
    Materials: TTabSheet;
    SpinEdit_Width: TSpinEdit;
    procedure BGRAKnob_ZoomValueChanged(Sender: TObject; Value: single);
    procedure BGRAView3DMouseDown(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure BGRAView3DMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
    procedure BGRAView3DMouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure BGRAView3DRedraw(Sender: TObject; Bitmap: TBGRABitmap);
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
    procedure ListBox_MaterialsKeyPress(Sender: TObject; var Key: char);
    procedure ListBox_MaterialsSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure Shape_MaterialColorMouseDown(Sender: TObject;
      {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure SpinEdit_ColorOpacityChange(Sender: TObject);
    procedure SpinEdit_ColorOpacityKeyPress(Sender: TObject; var Key: char);
    procedure SpinEdit_HeightKeyPress(Sender: TObject; var Key: char);
    procedure SpinEdit_SpecularIndexChange(Sender: TObject);
    procedure SpinEdit_SpecularIndexKeyPress(Sender: TObject; var Key: char);
    procedure SpinEdit_WidthKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
    procedure UpdateMaterialsTab;
    procedure CheckKey(var Key: char);
  public
    { public declarations }
    scene : TScene;
    moving,rotating: boolean;
    moveOrigin: TPoint;
    previousAngle: single;
    previousZoom: single;
    materialIndex: integer;
    MaterialsClientHeight: integer;
    Config: TLazPaintConfig;
  end;

function ShowObject3DDlg({%H-}Instance: TLazPaintCustomInstance; filenameUTF8: string; maxWidth, maxHeight: integer): TBGRABitmap;

implementation

uses ugraph, uscaledpi, umac, ULoadImage;

{ TScene }

procedure TScene.ClearCoordFlag(AVertex: IBGRAVertex3D);
begin
  AVertex.CustomFlags:= AVertex.CustomFlags and not 1;
end;

procedure TScene.SetCoordFlag(AVertex: IBGRAVertex3D);
begin
  AVertex.CustomFlags:= AVertex.CustomFlags or 1;
end;

function TScene.GetCoordFlag(AVertex: IBGRAVertex3D): boolean;
begin
  result := (AVertex.CustomFlags and 1) <> 0;
end;

procedure TScene.UseMaterial(AMaterialName: string; AFace: IBGRAFace3D);

  function ParseColor(text: string): TBGRAPixel;
  var
    color,tempColor: TBGRAPixel;
  begin
    color := BGRA(0,128,255);
    if CompareText(text,'bone')=0 then color := BGRA(255,255,240);
    if CompareText(text,'skin')=0 then color := BGRA(239,208,207);
    if CompareText(text,'bronze')=0 then color := BGRA(205, 127, 50);

    if copy(text,1,2) = 'dk' then
    begin
      tempcolor := ParseColor(copy(text,3,length(text)-2));
      tempcolor := MergeBGRA(tempcolor,3,BGRABlack,1);
      color := StrToBGRA('dark'+copy(text,3,length(text)-2),tempcolor);
    end;
    if copy(text,1,2) = 'lt' then
    begin
      tempcolor := ParseColor(copy(text,3,length(text)-2));
      tempcolor := MergeBGRA(tempcolor,3,BGRAWhite,1);
      color := StrToBGRA('light'+copy(text,3,length(text)-2),tempcolor);
    end;
    Color := StrToBGRA(StringReplace(text,'deep','dark',[]),Color);
    Color := StrToBGRA(StringReplace(text,'dark','deep',[]),Color);
    Color := StrToBGRA(text,Color);
    result := color;
  end;

  procedure ComputeTexCoord(AWidth,AHeight: integer);
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

var i: integer;
begin
  for i := 0 to FCustomMaterialCount -1 do
    if FCustomMaterials[i].Name = AMaterialName then
    begin
      if FCustomMaterials[i].Texture = nil then
      begin
        AFace.SetColor(FCustomMaterials[i].Color);
        AFace.Texture := nil;
      end else
      begin
        AFace.SetColor(BGRAWhite);
        AFace.Texture := FCustomMaterials[i].Texture;
        ComputeTexCoord(FCustomMaterials[i].Texture.Width,FCustomMaterials[i].Texture.Height);
      end;
      AFace.Material := FCustomMaterials[i].Material3D;
      exit;
    end;

  if length(FCustomMaterials) = FCustomMaterialCount then
    setlength(FCustomMaterials, length(FCustomMaterials)*2+1);
  with FCustomMaterials[FCustomMaterialCount] do
  begin
    Texture := nil;
    Color := ParseColor(AMaterialName);
    Name := AMaterialName;
    Material3D := CreateMaterial;
    AFace.Material := Material3D;
    AFace.SetColor(Color);
  end;
  inc(FCustomMaterialCount);
end;

procedure TScene.UpdateMaterials;
var i: integer;
begin
  for i := 0 to FCustomMaterialCount-1 do
    UpdateMaterial(FCustomMaterials[i].Name);
end;

procedure TScene.UpdateMaterial(AMaterialName: string);
begin
  ForEachVertex(@ClearCoordFlag);
  inherited UpdateMaterial(AMaterialName);
end;

destructor TScene.Destroy;
var i: integer;
begin
  for i := 0 to FCustomMaterialCount-1 do
    FreeAndNil(FCustomMaterials[i].Texture);
  inherited Destroy;
end;

{ TFObject3D }

procedure TFObject3D.BGRAView3DRedraw(Sender: TObject;
  Bitmap: TBGRABitmap);
begin
  DrawCheckers(Bitmap,rect(0,0,Bitmap.Width,Bitmap.Height));
  scene.Surface := Bitmap;
  scene.Render;
end;

procedure TFObject3D.Button_LoadTexClick(Sender: TObject);
var prevTex,tex: TBGRABitmap;
  final: string;
begin
  if materialIndex <> -1 then
  begin
    OpenTextureDialog.InitialDir := Config.DefaultTextureDirectory;
    if OpenTextureDialog.Execute then
    begin
      prevTex := nil;
      try
        tex := LoadFlatImageUTF8(OpenTextureDialog.FileName,final,'');
        prevTex := scene.FCustomMaterials[materialIndex].Texture;
        scene.FCustomMaterials[materialIndex].Texture := tex;
        Config.SetDefaultTextureDirectory(ExtractFilePath(OpenTextureDialog.Filename));
      except
        on ex:exception do ShowMessage(ex.Message);
      end;
      scene.UpdateMaterial(scene.FCustomMaterials[materialIndex].Name);
      prevTex.Free;
      BGRAView3D.DiscardBitmap;
    end;
  end;
end;

procedure TFObject3D.Button_NoTexClick(Sender: TObject);
begin
  if materialIndex <> -1 then
  begin
    if scene.FCustomMaterials[materialIndex].Texture <> nil then
    begin
      FreeAndNil(scene.FCustomMaterials[materialIndex].Texture);
      scene.UpdateMaterial(scene.FCustomMaterials[materialIndex].Name);
      BGRAView3D.DiscardBitmap;
    end;
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
      scene.Object3D[0].MainPart.RotateZDeg(angle-previousAngle,False);
      BGRAView3D.RedrawBitmap;
    end;
    previousAngle := angle;
  end;
end;

procedure TFObject3D.BGRAView3DMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (button = mbLeft) and moving then moving := false
  else if (button = mbRight) and rotating then rotating := false;
end;

procedure TFObject3D.CheckBox_TextureInterpChange(Sender: TObject);
begin
  scene.RenderingOptions.TextureInterpolation := checkbox_textureinterp.Checked;
  BGRAView3D.RedrawBitmap;
end;

procedure TFObject3D.ComboBox_NormalsChange(Sender: TObject);
begin
  scene.DefaultLightingNormal := TLightingNormal3D(ComboBox_Normals.ItemIndex+1);
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
    AntialiasingResampleLevel := 1;
    PerspectiveMode := pmZBuffer;
    TextureInterpolation := True;
  end;
  previousZoom := BGRAKnob_Zoom.Value;
  ComboBox_Normals.Items.Add('Flat faces');
  ComboBox_Normals.Items.Add('Rounded faces');
  ComboBox_Normals.Items.Add('Intermediate');
  ComboBox_Normals.ItemIndex := ord(scene.DefaultLightingNormal)-1;
  materialIndex:= -1;

  MaterialsClientHeight := GroupBox_SelectedMaterial.Top+GroupBox_SelectedMaterial.Height;
  UpdateMaterialsTab;
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
  UpdateMaterialsTab;
end;

procedure TFObject3D.FormShow(Sender: TObject);
var i: integer;
begin
  ListBox_Materials.Clear;
  for i := 0 to scene.FCustomMaterialCount-1 do
    ListBox_Materials.Items.Add(scene.FCustomMaterials[i].Name);
  if ListBox_Materials.Items.Count > 0 then
    ListBox_Materials.ItemIndex := 0;
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
    if scene.FCustomMaterials[materialIndex].Material3D.SpecularOn then
      SpinEdit_SpecularIndex.Value := scene.FCustomMaterials[materialIndex].Material3D.SpecularIndex
    else
      SpinEdit_SpecularIndex.Value := 0;
    SpinEdit_SpecularIndex.Enabled := true;
    Shape_MaterialColor.Brush.Color := BGRAToColor(scene.FCustomMaterials[materialIndex].Color);
    Shape_MaterialColor.Enabled := true;
    SpinEdit_ColorOpacity.Enabled := false;
    SpinEdit_ColorOpacity.Value := scene.FCustomMaterials[materialIndex].Color.alpha;
    SpinEdit_ColorOpacity.Enabled := true;
  end;
end;

procedure TFObject3D.Shape_MaterialColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if materialIndex <> -1 then
  begin
    ColorDialog1.Color := BGRAToColor(scene.FCustomMaterials[materialIndex].Color);
    if ColorDialog1.Execute then
    begin
      Shape_MaterialColor.Brush.Color := ColorDialog1.Color;
      scene.FCustomMaterials[materialIndex].Color := ColorToBGRA(ColorDialog1.Color,scene.FCustomMaterials[materialIndex].Color.alpha);
      scene.UpdateMaterial(scene.FCustomMaterials[materialIndex].Name);
      BGRAView3D.DiscardBitmap;
    end;
  end;
end;

procedure TFObject3D.SpinEdit_ColorOpacityChange(Sender: TObject);
begin
  if SpinEdit_ColorOpacity.Enabled and (materialIndex <> -1) then
  begin
    scene.FCustomMaterials[materialIndex].Color.alpha := SpinEdit_ColorOpacity.Value;
    scene.UpdateMaterial(scene.FCustomMaterials[materialIndex].Name);
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
    scene.FCustomMaterials[materialIndex].Material3D.SpecularIndex := SpinEdit_SpecularIndex.Value;
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

procedure TFObject3D.UpdateMaterialsTab;
begin
  GroupBox_SelectedMaterial.Top := MaterialsClientHeight - GroupBox_SelectedMaterial.Height;
  ListBox_Materials.Height := GroupBox_SelectedMaterial.Top-2-ListBox_Materials.Top;
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

function ShowObject3DDlg(Instance: TLazPaintCustomInstance; filenameUTF8: string;
  maxWidth, maxHeight: integer): TBGRABitmap;
var
  f: TFObject3D;
  obj: IBGRAObject3D;
  r: single;
begin
  f:= TFObject3D.Create(nil);
  f.Config := Instance.Config;
  result := nil;
  try
    obj := f.scene.LoadObjectFromFileUTF8(filenameUTF8);
    if obj <> nil then
    begin
      with obj do
      begin
        with MainPart.BoundingBox do
          MainPart.Translate((min+max)*(-1/2), False);
        r := MainPart.Radius;
        if r <> 0 then MainPart.Scale(50/r, False);
        MainPart.RotateXDeg(180-20, False);
        MainPart.RotateYDeg(-20, False);
      end;
      with f.scene do
      begin
        //set ambiant lightness to dark (1 is normal lightness)
        AmbiantLightness := 0.5;
        AddDirectionalLight(Point3D(1,1,1),1,-0.5); //add a directional light from top-left, maximum lightness will be 0.5 + 1 = 1.5
      end;
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
  f.Free;
end;

{$R *.lfm}

end.

