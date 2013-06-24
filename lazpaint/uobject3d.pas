unit UObject3D;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin, ExtCtrls, ComCtrls, BGRAVirtualScreen, BGRAKnob, BGRABitmap,
  BGRAScene3D, LazPaintType, BGRABitmapTypes;

type

  { TScene }

  TScene = class(TBGRAScene3D)
    FCustomMaterials: array of record
        Name: string;
        Material3D: IBGRAMaterial3D;
        Color: TBGRAPixel;
        Texture: TBGRABitmap;
      end;
    FCustomMaterialCount: integer;
    procedure UseMaterial(AMaterialName: string; AFace: IBGRAFace3D); override;
  end;

  { TFObject3D }

  TFObject3D = class(TForm)
    BGRAKnob_Zoom: TBGRAKnob;
    BGRAView3D: TBGRAVirtualScreen;
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
    procedure CheckBox_AntialiasingChange(Sender: TObject);
    procedure CheckBox_BifaceChange(Sender: TObject);
    procedure CheckBox_TextureInterpChange(Sender: TObject);
    procedure ComboBox_NormalsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListBox_MaterialsSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure Shape_MaterialColorMouseDown(Sender: TObject;
      {%H-}Button: TMouseButton; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure SpinEdit_ColorOpacityChange(Sender: TObject);
    procedure SpinEdit_SpecularIndexChange(Sender: TObject);
  private
    { private declarations }
    procedure UpdateMaterialsTab;
  public
    { public declarations }
    scene : TScene;
    moving,rotating: boolean;
    moveOrigin: TPoint;
    previousAngle: single;
    previousZoom: single;
    materialIndex: integer;
    MaterialsClientHeight: integer;
  end;

function ShowObject3DDlg({%H-}Instance: TLazPaintCustomInstance; filename: string; maxWidth, maxHeight: integer): TBGRABitmap;

implementation

uses ugraph, uscaledpi, umac;

{ TScene }

procedure TScene.UseMaterial(AMaterialName: string; AFace: IBGRAFace3D);
var i: integer;

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

{ TFObject3D }

procedure TFObject3D.BGRAView3DRedraw(Sender: TObject;
  Bitmap: TBGRABitmap);
begin
  DrawCheckers(Bitmap);
  scene.Surface := Bitmap;
  scene.Render;
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
    AntialiasingResampleLevel := 2;
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
  if SpinEdit_ColorOpacity.Enabled then
  begin
    scene.FCustomMaterials[materialIndex].Color.alpha := SpinEdit_ColorOpacity.Value;
    scene.UpdateMaterial(scene.FCustomMaterials[materialIndex].Name);
    BGRAView3D.DiscardBitmap;
  end;
end;

procedure TFObject3D.SpinEdit_SpecularIndexChange(Sender: TObject);
begin
  if SpinEdit_SpecularIndex.Enabled then
  begin
    scene.FCustomMaterials[materialIndex].Material3D.SpecularIndex := SpinEdit_SpecularIndex.Value;
    BGRAView3D.DiscardBitmap;
  end;
end;

procedure TFObject3D.UpdateMaterialsTab;
begin
  GroupBox_SelectedMaterial.Top := MaterialsClientHeight - GroupBox_SelectedMaterial.Height;
  ListBox_Materials.Height := GroupBox_SelectedMaterial.Top-2-ListBox_Materials.Top;
end;

function ShowObject3DDlg(Instance: TLazPaintCustomInstance; filename: string;
  maxWidth, maxHeight: integer): TBGRABitmap;
var
  f: TFObject3D;
  obj: IBGRAObject3D;
  r: single;
begin
  f:= TFObject3D.Create(nil);
  result := nil;
  try
    obj := f.scene.LoadObjectFromFile(filename);
    if obj <> nil then
    begin
      with obj do
      begin
        with MainPart.BoundingBox do
          MainPart.Translate((min+max)*(-1/2), False);
        r := MainPart.Radius;
        if r <> 0 then MainPart.Scale(40/r, False);
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
      MessageDlg(ex.Message,mtError,[mbOk],0);
  end;
  f.Free;
end;

initialization
  {$I uobject3d.lrs}

end.

