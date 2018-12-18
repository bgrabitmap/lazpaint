unit LCVectorialFillInterface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types,
  Controls, ComCtrls, Menus, Dialogs, ExtDlgs, ExtCtrls,
  BGRAImageList, BCTrackbarUpdown,
  BGRABitmap, BGRABitmapTypes, LCVectorialFill, LCVectorOriginal,
  BGRAGradientScanner, Graphics, BGRAGraphics;

const
  GradRepetitionToStr : array[TBGRAGradientRepetition] of string = ('Pad', 'Repeat', 'Reflect', 'Sine');
  ColorInterpToStr : array[TBGRAColorInterpolation] of string = ('sRGB', 'RGB', 'HSL CW', 'HSL CCW', 'Corr. HSL CW', 'Corr. HSL CCW');
  TextureRepetitionToStr: array[TTextureRepetition] of string = ('No repetition', 'Repeat X', 'Repeat Y', 'Repeat both');

type

  { TVectorialFillInterface }

  TVectorialFillInterface = class(TComponent)
  protected
    FFillType: TVectorialFillType;
    FSolidColor: TBGRAPixel;

    FGradStartColor, FGradEndColor: TBGRAPixel;
    FGradType: TGradientType;
    FGradRepetition: TBGRAGradientRepetition;
    FGradInterp: TBGRAColorInterpolation;

    FTexRepetition: TTextureRepetition;
    FTexture: TBGRABitmap;
    FTexOpacity: byte;

    //interface
    FContainer: TWinControl;

    FButtonFillNone, FButtonFillSolid,
    FButtonFillGradient, FButtonFillTexture: TToolButton;
    FOnFillChange, FOnFillTypeChange: TNotifyEvent;

    FSolidColorInterfaceCreated: boolean;
    FShapeSolidColor: TShape;
    FUpDownSolidAlpha: TBCTrackbarUpdown;
    FSolidColorChange: TNotifyEvent;

    FTextureInterfaceCreated: boolean;
    FCanAdjustToShape: boolean;
    FButtonAdjustToTexture, FButtonTexRepeat, FButtonLoadTexture: TToolButton;
    FUpDownTexAlpha: TBCTrackbarUpdown;
    FTexturePreview: TImage;
    FOnAdjustToShape, FOnTextureChange: TNotifyEvent;

    FGradientInterfaceCreated: boolean;
    FShapeStartColor, FShapeEndColor: TShape;
    FUpDownStartAlpha, FUpDownEndAlpha: TBCTrackbarUpdown;
    FButtonSwapColor, FButtonGradRepetition, FButtonGradInterp: TToolButton;
    FGradTypeMenu, FGradRepetitionMenu, FGradInterpMenu: TPopupMenu;

    FColorDlg: TColorDialog;
    FOpenPictureDlg: TOpenPictureDialog;
    FTexRepetitionMenu: TPopupMenu;

    FToolbar: TToolBar;
    FImageList: TBGRAImageList;
    FImageListLoaded: boolean;
    FImageListSize: TSize;

    procedure AdjustToShapeClick(Sender: TObject);
    procedure ButtonFillChange(Sender: TObject);
    procedure ButtonFillGradClick(Sender: TObject);
    procedure ButtonFillTexClick(Sender: TObject);
    procedure ButtonGradInterpClick(Sender: TObject);
    procedure ButtonGradRepetitionClick(Sender: TObject);
    procedure ButtonLoadTextureClick(Sender: TObject);
    procedure ButtonSwapColorClick(Sender: TObject);
    procedure ButtonTexRepeatClick(Sender: TObject);
    procedure Changed;
    procedure OnClickBackGradType(ASender: TObject);
    procedure OnClickBackTexRepeat(ASender: TObject);
    procedure OnClickGradInterp(ASender: TObject);
    procedure OnClickGradRepeat(ASender: TObject);
    function GetPreferredSize: TSize;
    procedure SetCanAdjustToShape(AValue: boolean);
    procedure SetContainer(AValue: TWinControl);
    procedure SetFillType(AValue: TVectorialFillType);
    procedure SetSolidColor(AValue: TBGRAPixel);
    procedure SetGradientType(AValue: TGradientType);
    procedure SetGradEndColor(AValue: TBGRAPixel);
    procedure SetGradStartColor(AValue: TBGRAPixel);
    procedure SetGradRepetition(AValue: TBGRAGradientRepetition);
    procedure SetGradInterpolation(AValue: TBGRAColorInterpolation);
    procedure SetImageListSize(AValue: TSize);
    procedure SetTexture(AValue: TBGRABitmap);
    procedure SetTextureRepetition(AValue: TTextureRepetition);
    procedure SetTextureOpacity(AValue: byte);
    procedure ShapeEndColorMouseUp({%H-}Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure ShapeSolidColorMouseUp({%H-}Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure ShapeStartColorMouseUp({%H-}Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure UpdateAccordingToFillType;
    procedure UpdateShapeSolidColor;
    procedure UpdateTextureParams;
    procedure UpdateTextureThumbnail;
    procedure UpdateGradientParams;
    procedure UpDownEndAlphaChange(Sender: TObject; AByUser: boolean);
    procedure UpDownSolidAlphaChange(Sender: TObject; AByUser: boolean);
    procedure UpDownStartAlphaChange(Sender: TObject; AByUser: boolean);
    procedure UpDownTexAlphaChange(Sender: TObject; AByUser: boolean);
    function ChooseColor(AColor: TColor): TColor;
    procedure CreateSolidColorInterface;
    procedure CreateGradientInterface;
    procedure CreateTextureInterface;
    procedure HideSolidColorInterface;
    procedure HideGradientInterface;
    procedure HideTextureInterface;
    procedure Init(AImageListWidth,AImageListHeight: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; AImageListWidth,AImageListHeight: Integer);
    destructor Destroy; override;
    procedure LoadTexture;
    procedure LoadImageList;
    procedure ContainerSizeChanged;
    function GetTextureThumbnail(AWidth, AHeight: integer; ABackColor: TColor): TBitmap;
    procedure AssignFill(AFill: TVectorialFill);
    function CreateShapeFill(AShape: TVectorShape): TVectorialFill;
    procedure UpdateShapeFill(AShape: TVectorShape; ABackFill: boolean);
    property FillType: TVectorialFillType read FFillType write SetFillType;
    property SolidColor: TBGRAPixel read FSolidColor write SetSolidColor;
    property GradientType: TGradientType read FGradType write SetGradientType;
    property GradStartColor: TBGRAPixel read FGradStartColor write SetGradStartColor;
    property GradEndColor: TBGRAPixel read FGradEndColor write SetGradEndColor;
    property GradRepetition: TBGRAGradientRepetition read FGradRepetition write SetGradRepetition;
    property GradInterpolation: TBGRAColorInterpolation read FGradInterp write SetGradInterpolation;
    property Texture: TBGRABitmap read FTexture write SetTexture;
    property TextureRepetition: TTextureRepetition read FTexRepetition write SetTextureRepetition;
    property TextureOpacity: byte read FTexOpacity write SetTextureOpacity;
    property CanAdjustToShape: boolean read FCanAdjustToShape write SetCanAdjustToShape;
    property OnFillChange: TNotifyEvent read FOnFillChange write FOnFillChange;
    property OnTextureChange: TNotifyEvent read FOnTextureChange write FOnTextureChange;
    property OnAdjustToShape: TNotifyEvent read FOnAdjustToShape write FOnAdjustToShape;
    property OnFillTypeChange: TNotifyEvent read FOnFillTypeChange write FOnFillTypeChange;
    property Container: TWinControl read FContainer write SetContainer;
    property ImageListSize: TSize read FImageListSize write SetImageListSize;
    property PreferredSize: TSize read GetPreferredSize;
  end;

implementation

uses LCToolbars, Toolwin, BGRAThumbnail, LResources,
  LCVectorShapes, LCVectorPolyShapes,
  BGRAGradientOriginal, BGRATransform;

{ TVectorialFillInterface }

procedure TVectorialFillInterface.LoadImageList;
var
  i: Integer;
  lst: TStringList;
begin
  if FImageList = nil then FImageList := TBGRAImageList.Create(self);
  if FImageListLoaded and (FImageList.Width=FImageListSize.cx) and (FImageList.Height=FImageListSize.cy) then exit;
  FImageList.Clear;
  FImageList.Width := FImageListSize.cx;
  FImageList.Height := FImageListSize.cy;

  lst := TStringList.Create;
  lst.CommaText := GetResourceString('fillimages.lst');
  for i := 0 to lst.Count-1 do
    LoadToolbarImage(FImageList, i, lst[i]);
  lst.Free;

  FImageListLoaded := true;
  if Assigned(FToolbar) then
  begin
    SetToolbarImages(FToolbar, FImageList);
    for i := 0 to FToolbar.ControlCount-1 do
      if FToolbar.Controls[i] is TBCTrackbarUpdown then
        FToolbar.Controls[i].Width := FToolbar.ButtonWidth*2
      else if FToolbar.Controls[i] is TShape then
        FToolbar.Controls[i].Width := FToolbar.ButtonWidth;
  end;
end;

procedure TVectorialFillInterface.Changed;
begin
  if Assigned(FOnFillChange) then
    FOnFillChange(self);
end;

procedure TVectorialFillInterface.OnClickBackGradType(ASender: TObject);
begin
  GradientType:= TGradientType((ASender as TMenuItem).Tag);
  FillType := vftGradient;
end;

procedure TVectorialFillInterface.OnClickBackTexRepeat(ASender: TObject);
begin
  TextureRepetition := TTextureRepetition((ASender as TMenuItem).Tag);
end;

procedure TVectorialFillInterface.OnClickGradInterp(ASender: TObject);
begin
  GradInterpolation:= TBGRAColorInterpolation((ASender as TMenuItem).Tag);
end;

procedure TVectorialFillInterface.OnClickGradRepeat(ASender: TObject);
begin
  GradRepetition:= TBGRAGradientRepetition((ASender as TMenuItem).Tag);
end;

procedure TVectorialFillInterface.SetTexture(AValue: TBGRABitmap);
begin
  if FTexture=AValue then Exit;

  if Assigned(FTexture) then
  begin
    FTexture.FreeReference;
    FTexture := nil;
  end;
  if Assigned(AValue) then
    FTexture := AValue.NewReference as TBGRABitmap;

  UpdateTextureThumbnail;
  if Assigned(FOnTextureChange) then FOnTextureChange(self);
  if FFillType = vftTexture then Changed;
end;

procedure TVectorialFillInterface.LoadTexture;
var
  newTex: TBGRABitmap;
begin
  if FOpenPictureDlg.Execute then
  begin
    try
      newTex := TBGRABitmap.Create(FOpenPictureDlg.FileName, true);
      Texture := newTex;
      newTex.FreeReference;
      FillType:= vftTexture;
    except
      on ex: exception do
        ShowMessage(ex.Message);
    end;
  end;
end;

procedure TVectorialFillInterface.ContainerSizeChanged;
begin
  FToolbar.Align:= alTop;
  FToolbar.Height := FContainer.Height;
end;

procedure TVectorialFillInterface.SetFillType(AValue: TVectorialFillType);
begin
  if FFillType=AValue then Exit;
  FFillType:=AValue;
  UpdateAccordingToFillType;
  if Assigned(FOnFillTypeChange) then FOnFillTypeChange(self);
  Changed;
end;

procedure TVectorialFillInterface.ShapeSolidColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  newColor: TColor;
begin
  newColor := ChooseColor(FShapeSolidColor.Brush.Color);
  if newColor <> clNone then
  begin
    if SolidColor.alpha <> 0 then
      SolidColor := ColorToBGRA(newColor, SolidColor.alpha)
    else
      SolidColor := newColor;
  end;
end;

procedure TVectorialFillInterface.ShapeStartColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  newColor: TColor;
begin
  newColor := ChooseColor(FShapeStartColor.Brush.Color);
  if newColor <> clNone then
  begin
    if GradStartColor.alpha <> 0 then
      GradStartColor := ColorToBGRA(newColor, GradStartColor.alpha)
    else
      GradStartColor := newColor;
  end;
end;

procedure TVectorialFillInterface.UpdateAccordingToFillType;
begin
  FButtonFillNone.Down := FillType = vftNone;
  FButtonFillSolid.Down := FillType = vftSolid;
  FButtonFillGradient.Down := FillType = vftGradient;
  FButtonFillTexture.Down := FillType = vftTexture;

  if FillType <> vftSolid then HideSolidColorInterface;
  if FillType <> vftGradient then HideGradientInterface;
  if FillType <> vftTexture then HideTextureInterface;

  case FillType of
    vftSolid: begin
      CreateSolidColorInterface;
      UpdateShapeSolidColor;
      ShowAppendToolButtons([FShapeSolidColor,FUpDownSolidAlpha]);
    end;
    vftGradient: begin
      CreateGradientInterface;
      UpdateGradientParams;
      ShowAppendToolButtons([FShapeStartColor,FUpDownStartAlpha,FButtonSwapColor,
                           FShapeEndColor,FUpDownEndAlpha,FButtonGradRepetition,FButtonGradInterp]);
    end;
    vftTexture: begin
      CreateTextureInterface;
      UpdateTextureParams;
      ShowAppendToolButtons([FButtonAdjustToTexture,FButtonTexRepeat,FUpDownTexAlpha,
                           FButtonLoadTexture,FTexturePreview]);
    end;
  end;
end;

procedure TVectorialFillInterface.UpdateShapeSolidColor;
var
  c: TBGRAPixel;
begin
  c := SolidColor;
  c.alpha := 255;
  if Assigned(FShapeSolidColor) then FShapeSolidColor.Brush.Color := c;
  if Assigned(FUpDownSolidAlpha) then FUpDownSolidAlpha.Value := SolidColor.alpha;
end;

procedure TVectorialFillInterface.UpdateTextureParams;
begin
  if Assigned(FButtonTexRepeat) then FButtonTexRepeat.ImageIndex := 17 + ord(TextureRepetition);
  if Assigned(FUpDownTexAlpha) then FUpDownTexAlpha.Value := TextureOpacity;
end;

procedure TVectorialFillInterface.UpdateTextureThumbnail;
var
  bmpThumb: TBitmap;
begin
  if not Assigned(FTexturePreview) then exit;
  if Assigned(Texture) then
  begin
    bmpThumb := GetTextureThumbnail(FTexturePreview.Width,FTexturePreview.Height,clBtnFace);
    FTexturePreview.Picture.Assign(bmpThumb);
    bmpThumb.Free;
  end else
  begin
    FTexturePreview.Picture.Clear;
  end;
end;

procedure TVectorialFillInterface.UpdateGradientParams;
var
  c: TBGRAPixel;
begin
  c := GradStartColor;
  c.alpha := 255;
  if Assigned(FShapeStartColor) then FShapeStartColor.Brush.Color := c;
  if Assigned(FUpDownStartAlpha) then FUpDownStartAlpha.Value := GradStartColor.alpha;
  c := GradEndColor;
  c.alpha := 255;
  if Assigned(FShapeEndColor) then FShapeEndColor.Brush.Color := c;
  if Assigned(FUpDownEndAlpha) then FUpDownEndAlpha.Value := GradEndColor.alpha;

  if Assigned(FButtonGradRepetition) then FButtonGradRepetition.ImageIndex := 7+ord(FGradRepetition);
  if Assigned(FButtonGradInterp) then FButtonGradInterp.ImageIndex := 11+ord(FGradInterp);
end;

procedure TVectorialFillInterface.UpDownEndAlphaChange(Sender: TObject;
  AByUser: boolean);
begin
  if AByUser then
    GradEndColor:= ColorToBGRA(FShapeEndColor.Brush.Color, FUpDownEndAlpha.Value);
end;

procedure TVectorialFillInterface.UpDownSolidAlphaChange(Sender: TObject;
  AByUser: boolean);
begin
  if AByUser then
    SolidColor:= ColorToBGRA(FShapeSolidColor.Brush.Color, FUpDownSolidAlpha.Value);
end;

procedure TVectorialFillInterface.UpDownStartAlphaChange(Sender: TObject;
  AByUser: boolean);
begin
  if AByUser then
    GradStartColor:= ColorToBGRA(FShapeStartColor.Brush.Color, FUpDownStartAlpha.Value);
end;

procedure TVectorialFillInterface.UpDownTexAlphaChange(Sender: TObject;
  AByUser: boolean);
begin
  if AByUser then
  begin
    FTexOpacity:= FUpDownTexAlpha.Value;
    if FillType = vftTexture then Changed;
  end;
end;

function TVectorialFillInterface.ChooseColor(AColor: TColor): TColor;
begin
  FColorDlg.Color := AColor;
  if FColorDlg.Execute then
    result := FColorDlg.Color
  else
    result := clNone;
end;

procedure TVectorialFillInterface.CreateSolidColorInterface;
begin
  if FSolidColorInterfaceCreated then exit;
  FSolidColorInterfaceCreated := true;

  //solid color interface
  FShapeSolidColor := TShape.Create(FToolbar);
  FShapeSolidColor.Width := FToolbar.ButtonWidth;
  FShapeSolidColor.Height := FToolbar.ButtonHeight;
  FShapeSolidColor.OnMouseUp:= @ShapeSolidColorMouseUp;
  AddToolbarControl(FToolbar, FShapeSolidColor);
  FUpDownSolidAlpha := TBCTrackbarUpdown.Create(FToolbar);
  FUpDownSolidAlpha.Width := FToolbar.ButtonWidth*2;
  FUpDownSolidAlpha.Height := FToolbar.ButtonHeight;
  FUpDownSolidAlpha.MinValue := 0;
  FUpDownSolidAlpha.MaxValue := 255;
  FUpDownSolidAlpha.OnChange:=@UpDownSolidAlphaChange;
  AddToolbarControl(FToolbar, FUpDownSolidAlpha);
end;

procedure TVectorialFillInterface.CreateGradientInterface;
var
  gr: TBGRAGradientRepetition;
  ci: TBGRAColorInterpolation;
  item: TMenuItem;
begin
  if FGradientInterfaceCreated then exit;
  FGradientInterfaceCreated := true;

  FShapeStartColor := TShape.Create(FToolbar);
  FShapeStartColor.Width := FToolbar.ButtonWidth;
  FShapeStartColor.Height := FToolbar.ButtonHeight;
  FShapeStartColor.OnMouseUp:=@ShapeStartColorMouseUp;
  AddToolbarControl(FToolbar, FShapeStartColor);
  FUpDownStartAlpha := TBCTrackbarUpdown.Create(FToolbar);
  FUpDownStartAlpha.Width := FToolbar.ButtonWidth*2;
  FUpDownStartAlpha.Height := FToolbar.ButtonHeight;
  FUpDownStartAlpha.MinValue := 0;
  FUpDownStartAlpha.MaxValue := 255;
  FUpDownStartAlpha.OnChange:=@UpDownStartAlphaChange;
  AddToolbarControl(FToolbar, FUpDownStartAlpha);
  FButtonSwapColor := AddToolbarButton(FToolbar, 'Swap colors', 23, @ButtonSwapColorClick);
  FShapeEndColor := TShape.Create(FToolbar);
  FShapeEndColor.Width := FToolbar.ButtonWidth;
  FShapeEndColor.Height := FToolbar.ButtonHeight;
  FShapeEndColor.OnMouseUp:=@ShapeEndColorMouseUp;
  AddToolbarControl(FToolbar, FShapeEndColor);
  FUpDownEndAlpha := TBCTrackbarUpdown.Create(FToolbar);
  FUpDownEndAlpha.Width := FToolbar.ButtonWidth*2;
  FUpDownEndAlpha.Height := FToolbar.ButtonHeight;
  FUpDownEndAlpha.MinValue := 0;
  FUpDownEndAlpha.MaxValue := 255;
  FUpDownEndAlpha.OnChange:=@UpDownEndAlphaChange;
  AddToolbarControl(FToolbar, FUpDownEndAlpha);
  FButtonGradRepetition := AddToolbarButton(FToolbar, 'Gradient repetition...', 7+ord(FGradRepetition), @ButtonGradRepetitionClick);
  FButtonGradInterp := AddToolbarButton(FToolbar, 'Color interpolation...', 11+ord(FGradInterp), @ButtonGradInterpClick);

  FGradRepetitionMenu := TPopupMenu.Create(self);
  FGradRepetitionMenu.Images := FImageList;
  for gr := low(TBGRAGradientRepetition) to high(TBGRAGradientRepetition) do
  begin
    item := TMenuItem.Create(FGradRepetitionMenu);  item.Caption := GradRepetitionToStr[gr];
    item.OnClick:=@OnClickGradRepeat;               item.Tag := ord(gr);
    item.ImageIndex:= 7+ord(gr);
    FGradRepetitionMenu.Items.Add(item);
  end;

  FGradInterpMenu := TPopupMenu.Create(self);
  FGradInterpMenu.Images := FImageList;
  for ci := low(TBGRAColorInterpolation) to high(TBGRAColorInterpolation) do
  begin
    item := TMenuItem.Create(FGradInterpMenu);  item.Caption := ColorInterpToStr[ci];
    item.OnClick:=@OnClickGradInterp;           item.Tag := ord(ci);
    item.ImageIndex:= 11+ord(ci);
    FGradInterpMenu.Items.Add(item);
  end;
end;

procedure TVectorialFillInterface.CreateTextureInterface;
var
  tr: TTextureRepetition;
  item: TMenuItem;
begin
  if FTextureInterfaceCreated then exit;
  FTextureInterfaceCreated := true;

  FButtonAdjustToTexture := AddToolbarButton(FToolbar, 'Adjust to shape', 21, @AdjustToShapeClick);
  FButtonAdjustToTexture.Enabled := FCanAdjustToShape;
  FButtonTexRepeat := AddToolbarButton(FToolbar, 'Texture repetition...', -1, @ButtonTexRepeatClick);
  FUpDownTexAlpha := TBCTrackbarUpdown.Create(FToolbar);
  FUpDownTexAlpha.Width := FToolbar.ButtonWidth*2;
  FUpDownTexAlpha.Height := FToolbar.ButtonHeight;
  FUpDownTexAlpha.MinValue := 0;
  FUpDownTexAlpha.MaxValue := 255;
  FUpDownTexAlpha.OnChange:=@UpDownTexAlphaChange;
  AddToolbarControl(FToolbar, FUpDownTexAlpha);
  FButtonLoadTexture := AddToolbarButton(FToolbar, 'Load texture...', 22, @ButtonLoadTextureClick);
  FTexturePreview := TImage.Create(FToolbar);
  FTexturePreview.Width := FToolbar.ButtonWidth;
  FTexturePreview.Height := FToolbar.ButtonHeight;
  UpdateTextureThumbnail;
  AddToolbarControl(FToolbar, FTexturePreview);

  FTexRepetitionMenu := TPopupMenu.Create(self);
  FTexRepetitionMenu.Images := FImageList;
  for tr := low(TTextureRepetition) to high(TTextureRepetition) do
  begin
    item := TMenuItem.Create(FTexRepetitionMenu);  item.Caption := TextureRepetitionToStr[tr];
    item.OnClick:=@OnClickBackTexRepeat;           item.Tag := ord(tr);
    item.ImageIndex:= 17+ord(tr);
    FTexRepetitionMenu.Items.Add(item);
  end;
end;

procedure TVectorialFillInterface.HideSolidColorInterface;
begin
  if not FSolidColorInterfaceCreated then exit;
  FShapeSolidColor.Visible := false;
  FUpDownSolidAlpha.Visible := false;
end;

procedure TVectorialFillInterface.HideGradientInterface;
begin
  if not FGradientInterfaceCreated then exit;
  FShapeStartColor.Visible := false;
  FUpDownStartAlpha.Visible := false;
  FButtonSwapColor.Visible := false;
  FShapeEndColor.Visible := false;
  FUpDownEndAlpha.Visible := false;
  FButtonGradRepetition.Visible := false;
  FButtonGradInterp.Visible := false;
end;

procedure TVectorialFillInterface.HideTextureInterface;
begin
  if not FTextureInterfaceCreated then exit;
  FButtonAdjustToTexture.Visible := false;
  FButtonTexRepeat.Visible := false;
  FUpDownTexAlpha.Visible := false;
  FButtonLoadTexture.Visible := false;
  FTexturePreview.Visible := false;
end;

procedure TVectorialFillInterface.Init(AImageListWidth,
  AImageListHeight: Integer);
var
  gt: TGradientType;
  item: TMenuItem;
begin
  FContainer := nil;

  FFillType:= vftSolid;
  FSolidColor:= BGRAWhite;
  FGradStartColor:= CSSRed;
  FGradEndColor:= CSSYellow;
  FGradType:= gtLinear;
  FGradRepetition:= grPad;
  FGradInterp:= ciLinearRGB;
  FTexture:= nil;
  FTexRepetition:= trRepeatBoth;
  FTexOpacity:= 255;
  FCanAdjustToShape:= true;

  FImageList := TBGRAImageList.Create(self);
  FImageListLoaded:= false;
  FImageListSize := Size(AImageListWidth,AImageListHeight);

  FOpenPictureDlg := TOpenPictureDialog.Create(self);
  FColorDlg:= TColorDialog.Create(self);

  FOnFillChange:= nil;
  FOnTextureChange:= nil;

  FToolbar := CreateToolBar(FImageList);
  FToolbar.Wrapable := false;
  FButtonFillNone := AddToolbarCheckButton(FToolbar, 'No fill', 0, @ButtonFillChange, False, False);
  FButtonFillSolid := AddToolbarCheckButton(FToolbar, 'Solid color', 1, @ButtonFillChange, False, False);
  FButtonFillGradient := AddToolbarButton(FToolbar, 'Gradient fill', 2+ord(FGradType), @ButtonFillGradClick);
  FButtonFillTexture := AddToolbarButton(FToolbar, 'Texture fill', 24, @ButtonFillTexClick);
  FButtonFillTexture.Wrap := true;

  //menu to access gradient interface
  FGradTypeMenu := TPopupMenu.Create(self);
  FGradTypeMenu.Images := FImageList;
  for gt := low(TGradientType) to high(TGradientType) do
  begin
    item := TMenuItem.Create(FGradTypeMenu);  item.Caption := GradientTypeStr[gt];
    item.OnClick:=@OnClickBackGradType;       item.Tag := ord(gt);
    item.ImageIndex:= 2+ord(gt);
    FGradTypeMenu.Items.Add(item);
  end;

  FSolidColorInterfaceCreated := false;
  FGradientInterfaceCreated:= false;
  FTextureInterfaceCreated:= false;

  UpdateAccordingToFillType;
end;

procedure TVectorialFillInterface.SetSolidColor(AValue: TBGRAPixel);
begin
  if FSolidColor=AValue then Exit;
  FSolidColor:=AValue;
  UpdateShapeSolidColor;
  If FillType = vftSolid then Changed;
end;

procedure TVectorialFillInterface.ButtonFillChange(Sender: TObject);
begin
  if Sender = FButtonFillNone then
  begin
    FillType:= vftNone;
    FButtonFillNone.Down := true;
  end
  else if Sender = FButtonFillSolid then
  begin
    FillType:= vftSolid;
    FButtonFillSolid.Down := true;
  end;
end;

procedure TVectorialFillInterface.SetTextureRepetition(
  AValue: TTextureRepetition);
begin
  if FTexRepetition=AValue then Exit;
  FTexRepetition:=AValue;
  UpdateTextureParams;
  If FillType = vftTexture then Changed;
end;

procedure TVectorialFillInterface.SetTextureOpacity(AValue: byte);
begin
  if FTexOpacity=AValue then Exit;
  FTexOpacity:=AValue;
  FUpDownTexAlpha.Value := AValue;
  If FillType = vftTexture then Changed;
end;

procedure TVectorialFillInterface.ShapeEndColorMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  newColor: TColor;
begin
  newColor := ChooseColor(FShapeEndColor.Brush.Color);
  if newColor <> clNone then
  begin
    if GradEndColor.alpha <> 0 then
      GradEndColor := ColorToBGRA(newColor, GradEndColor.alpha)
    else
      GradEndColor := newColor;
  end;
end;

procedure TVectorialFillInterface.SetGradientType(AValue: TGradientType);
begin
  if FGradType=AValue then Exit;
  FGradType:=AValue;
  FButtonFillGradient.ImageIndex := 2+ord(GradientType);
  if FillType = vftGradient then Changed;
end;

procedure TVectorialFillInterface.SetGradEndColor(AValue: TBGRAPixel);
begin
  if CompareMem(@FGradEndColor,@AValue,sizeof(TBGRAPixel)) then Exit;
  FGradEndColor:=AValue;
  UpdateGradientParams;
  if FillType = vftGradient then Changed;
end;

procedure TVectorialFillInterface.SetGradStartColor(AValue: TBGRAPixel);
begin
  if CompareMem(@FGradStartColor,@AValue,sizeof(TBGRAPixel)) then Exit;
  FGradStartColor:=AValue;
  UpdateGradientParams;
  if FillType = vftGradient then Changed;
end;

procedure TVectorialFillInterface.SetGradRepetition(AValue: TBGRAGradientRepetition);
begin
  if FGradRepetition=AValue then Exit;
  FGradRepetition:=AValue;
  UpdateGradientParams;
  if FillType = vftGradient then Changed;
end;

procedure TVectorialFillInterface.SetGradInterpolation(
  AValue: TBGRAColorInterpolation);
begin
  if FGradInterp=AValue then Exit;
  FGradInterp:=AValue;
  UpdateGradientParams;
  if FillType = vftGradient then Changed;
end;

procedure TVectorialFillInterface.SetContainer(AValue: TWinControl);
begin
  if FContainer=AValue then Exit;
  if Assigned(FContainer) then FContainer.RemoveControl(FToolbar);
  FContainer:=AValue;
  if Assigned(FContainer) then
  begin
    FContainer.InsertControl(FToolBar);
    ContainerSizeChanged;
  end;
end;

function TVectorialFillInterface.GetPreferredSize: TSize;
begin
  result := GetToolbarSize(FToolbar,0);
end;

procedure TVectorialFillInterface.SetCanAdjustToShape(AValue: boolean);
begin
  if FCanAdjustToShape=AValue then Exit;
  FCanAdjustToShape:=AValue;
  if FTextureInterfaceCreated then
    FButtonAdjustToTexture.Enabled := AValue;
end;

procedure TVectorialFillInterface.SetImageListSize(AValue: TSize);
begin
  if (FImageListSize.cx=AValue.cx) and (FImageListSize.cy=AValue.cy) then Exit;
  FImageListSize:=AValue;
  if FImageListLoaded then LoadImageList;
end;

procedure TVectorialFillInterface.AdjustToShapeClick(Sender: TObject);
begin
  if Assigned(FOnAdjustToShape) then FOnAdjustToShape(self);
end;

procedure TVectorialFillInterface.ButtonFillGradClick(Sender: TObject);
begin
  if Assigned(FGradTypeMenu) then
    with FButtonFillGradient.ClientToScreen(Point(0,FButtonFillGradient.Height)) do
      FGradTypeMenu.PopUp(X,Y);
  FButtonFillGradient.Down := (FillType = vftGradient);
end;

procedure TVectorialFillInterface.ButtonFillTexClick(Sender: TObject);
begin
  if FFillType = vftTexture then
  begin
    FButtonFillTexture.Down := true;
    exit;
  end;

  if Assigned(FTexture) then FillType := vftTexture
  else LoadTexture;
end;

procedure TVectorialFillInterface.ButtonGradInterpClick(Sender: TObject);
begin
  if Assigned(FGradInterpMenu) then
    with FButtonGradInterp.ClientToScreen(Point(0,FButtonGradInterp.Height)) do
      FGradInterpMenu.PopUp(X,Y);
end;

procedure TVectorialFillInterface.ButtonGradRepetitionClick(Sender: TObject);
begin
  if Assigned(FGradRepetitionMenu) then
    with FButtonGradRepetition.ClientToScreen(Point(0,FButtonGradRepetition.Height)) do
      FGradRepetitionMenu.PopUp(X,Y);
end;

procedure TVectorialFillInterface.ButtonLoadTextureClick(Sender: TObject);
begin
  LoadTexture;
end;

procedure TVectorialFillInterface.ButtonSwapColorClick(Sender: TObject);
var
  temp: TBGRAPixel;
begin
  temp := GradStartColor;
  FGradStartColor := GradEndColor;
  FGradEndColor := temp;
  UpdateGradientParams;
  if FillType = vftGradient then Changed;
end;

procedure TVectorialFillInterface.ButtonTexRepeatClick(Sender: TObject);
begin
  if Assigned(FTexRepetitionMenu) then
    with FButtonTexRepeat.ClientToScreen(Point(0,FButtonTexRepeat.Height)) do
      FTexRepetitionMenu.PopUp(X,Y);
end;

constructor TVectorialFillInterface.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Init(16,16);
end;

constructor TVectorialFillInterface.Create(AOwner: TComponent; AImageListWidth,
  AImageListHeight: Integer);
begin
  inherited Create(AOwner);
  Init(AImageListWidth,AImageListHeight);
end;

destructor TVectorialFillInterface.Destroy;
begin
  FTexture.FreeReference;
  if Assigned(FContainer) then
  begin
    FContainer.RemoveControl(FToolbar);
    FContainer := nil;
  end;
  FToolbar.Free;
  inherited Destroy;
end;

function TVectorialFillInterface.GetTextureThumbnail(AWidth, AHeight: integer; ABackColor: TColor): TBitmap;
var
  thumb: TBGRABitmap;
begin
  if FTexture = nil then exit(nil);
  thumb := GetBitmapThumbnail(FTexture, AWidth,AHeight,BGRAPixelTransparent,true);
  try
    result := thumb.MakeBitmapCopy(ABackColor);
  finally
    thumb.Free;
  end;
end;

procedure TVectorialFillInterface.AssignFill(AFill: TVectorialFill);
begin
  FillType := AFill.FillType;
  case FillType of
    vftTexture:
      begin
        Texture := AFill.Texture;
        TextureOpacity:= AFill.TextureOpacity;
        TextureRepetition:= AFill.TextureRepetition;
      end;
    vftSolid: SolidColor := AFill.SolidColor;
    vftGradient:
      begin
        GradStartColor := AFill.Gradient.StartColor;
        GradEndColor := AFill.Gradient.EndColor;
        GradientType:= AFill.Gradient.GradientType;
        GradRepetition:= AFill.Gradient.Repetition;
        GradInterpolation := AFill.Gradient.ColorInterpolation;
      end;
  end;
end;

function TVectorialFillInterface.CreateShapeFill(AShape: TVectorShape): TVectorialFill;
var
  grad: TBGRALayerGradientOriginal;
  rF: TRectF;
  sx,sy: single;
begin
  if FillType = vftSolid then
    result := TVectorialFill.CreateAsSolid(SolidColor)
  else if (FillType = vftTexture) and Assigned(Texture) then
  begin
    rF := AShape.GetRenderBounds(InfiniteRect,AffineMatrixIdentity,[rboAssumeBackFill]);
    if not (TextureRepetition in [trRepeatX,trRepeatBoth]) and (rF.Width <> 0) and (Texture.Width > 0) then
      sx:= rF.Width/Texture.Width else sx:= 1;
    if not (TextureRepetition in [trRepeatY,trRepeatBoth]) and (rF.Height <> 0) and (Texture.Height > 0) then
      sy:= rF.Height/Texture.Height else sy:= 1;

    result := TVectorialFill.CreateAsTexture(Texture,
                 AffineMatrixTranslation(rF.TopLeft.x,rF.TopLeft.y)*
                 AffineMatrixScale(sx,sy),
                 TextureOpacity, TextureRepetition);
  end
  else if FillType = vftGradient then
  begin
    if Assigned(AShape) then
    begin
      rF := AShape.GetRenderBounds(InfiniteRect,AffineMatrixIdentity,[rboAssumeBackFill]);
      if IsEmptyRectF(rF) then exit(nil);
    end
    else
      exit(nil);
    grad := TBGRALayerGradientOriginal.Create;
    grad.StartColor := GradStartColor;
    grad.EndColor := GradEndColor;
    grad.GradientType:= GradientType;
    grad.Repetition := GradRepetition;
    grad.ColorInterpolation:= GradInterpolation;
    if grad.GradientType = gtLinear then
    begin
      grad.Origin := rF.TopLeft;
      grad.XAxis := rF.BottomRight;
    end else
    begin
      grad.Origin := (rF.TopLeft + rF.BottomRight)*0.5;
      if grad.GradientType = gtReflected then
        grad.XAxis := rF.BottomRight
      else
      begin
        grad.XAxis := PointF(rF.Right,grad.Origin.y);
        grad.YAxis := PointF(grad.Origin.x,rF.Bottom);
      end;
    end;
    result := TVectorialFill.CreateAsGradient(grad, true);
  end
  else result := nil; //none
end;

procedure TVectorialFillInterface.UpdateShapeFill(AShape: TVectorShape; ABackFill: boolean);
var
  vectorFill: TVectorialFill;
  curFill: TVectorialFill;
begin
  if ABackFill then
    curFill:= AShape.BackFill
  else
    curFill:= AShape.PenFill;

  if (FillType = vftTexture) and (TextureOpacity = 0) then
    vectorFill := nil else
  if (FillType = vftTexture) and (curFill.FillType = vftTexture) then
  begin
    vectorFill := TVectorialFill.CreateAsTexture(Texture, curFill.TextureMatrix,
                                                 TextureOpacity, TextureRepetition);
  end
  else if (FillType = vftGradient) and (curFill.FillType = vftGradient) then
  begin
    vectorFill := curFill.Duplicate;
    vectorFill.Gradient.StartColor := GradStartColor;
    vectorFill.Gradient.EndColor := GradEndColor;
    vectorFill.Gradient.GradientType := GradientType;
    vectorFill.Gradient.Repetition := GradRepetition;
    vectorFill.Gradient.ColorInterpolation:= GradInterpolation;
  end else
    vectorFill := CreateShapeFill(AShape);

  if ABackFill then
    AShape.BackFill:= vectorFill
  else
    AShape.PenFill:= vectorFill;
  vectorFill.Free;
end;

begin
  {$i fillimages.lrs}
end.

