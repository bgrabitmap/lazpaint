// SPDX-License-Identifier: GPL-3.0-only
unit USaveOption;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, BGRAVirtualScreen, BGRABitmap, LazPaintType,
  BGRABitmapTypes, BGRAPalette, BGRAColorQuantization;

type

  { TFSaveOption }

  TFSaveOption = class(TForm)
    BGRAPreview: TBGRAVirtualScreen;
    Button_Cancel: TButton;
    Button_OK: TButton;
    CheckBox_Lossless: TCheckBox;
    CheckBox_Dithering: TCheckBox;
    Edit_QualityValue: TEdit;
    Label1: TLabel;
    Label_0: TLabel;
    Label_1: TLabel;
    Label_50: TLabel;
    Label_Size: TLabel;
    Label_ColorDepth: TLabel;
    Panel_Buttons: TPanel;
    Panel_Percent: TPanel;
    Panel_OkCancel: TPanel;
    Panel_Quality: TPanel;
    Panel_BitsPerPixel: TPanel;
    Panel_Option: TPanel;
    RadioButton_32BitsPerPixel: TRadioButton;
    RadioButton_MioMap: TRadioButton;
    RadioButton_2Colors: TRadioButton;
    RadioButton_16Colors: TRadioButton;
    RadioButton_256Colors: TRadioButton;
    RadioButton_24BitsPerPixel: TRadioButton;
    Timer_Update: TTimer;
    TrackBar_Quality: TTrackBar;
    procedure BGRAPreviewRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure Button_OKClick(Sender: TObject);
    procedure CheckBox_DitheringChange(Sender: TObject);
    procedure CheckBox_LosslessChange(Sender: TObject);
    procedure Edit_QualityValueChange(Sender: TObject);
    procedure Edit_QualityValueExit(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure RadioButton_BitsPerPixelChange(Sender: TObject);
    procedure Timer_UpdateTimer(Sender: TObject);
    procedure TrackBar_QualityChange(Sender: TObject);
  private
    { private declarations }
    FInit: boolean;
    FLazPaintInstance: TLazPaintCustomInstance;
    FOutputFilename: string;
    FQualityVisible, FLosslessVisible, FBitsPerPixelVisible: boolean;
    FFlattenedOriginal, FJpegPreview, FWebPPreview: TBGRABitmap;
    FBmpStream, FJpegStream, FWebPStream, FPngStream: TMemoryStream;
    FFormTitle: string;
    FImageFormat: TBGRAImageFormat;
    FQuantizer, FQuantizer1bit: TBGRAColorQuantizer;
    FSizeCaption: string;
    function GetBmpStreamNeeded: boolean;
    procedure BmpQualityChanged;
    function GetLosslessVisible: boolean;
    function GetPngStreamNeeded: boolean;
    function GetWebPLossless: boolean;
    procedure PngQualityChanged;
    function GetBitsPerPixelVisible: boolean;
    function GetColorQuantizer: TBGRAColorQuantizer;
    function GetJpegPreview: TBGRABitmap;
    procedure RequireJpegStream;
    function GetWebPPreview: TBGRABitmap;
    procedure RequireWebPStream;
    procedure MakeBmpStreamIfNeeded;
    function GetWantedBitsPerPixel: integer;
    procedure SetJpegQuality(AValue: integer);
    procedure SetLosslessVisible(AValue: boolean);
    procedure SetWebPLossless(AValue: boolean);
    procedure UpdateFileSize;
    procedure UpdateFileSizeTo(AValue: int64);
    function GetJpegQuality: integer;
    function GetQualityVisible: boolean;
    function GetQuantizerNeeded: boolean;
    procedure SetBitsPerPixelVisible(AValue: boolean);
    procedure SetImageFormat(AValue: TBGRAImageFormat);
    procedure SetLazPaintInstance(AValue: TLazPaintCustomInstance);
    procedure SetOutputFilename(AValue: string);
    procedure SetQualityVisible(AValue: boolean);
    procedure UpdateQualityTextBox;
    procedure NeedBitmapUpdate(AImmediate: boolean);
    function GetDitheringAlgorithm: TDitheringAlgorithm;
    procedure UpdateDitheringCheckbox;
    function GetOriginalBitDepth: integer;
    procedure DoUpdateBitmap;
    procedure JpegQualityChanged;
    procedure WebPQualityChanged;
    procedure LayoutRadioButtonDepth;
    procedure MakePngStreamIfNeeded;
  public
    Exporting: boolean;
    { public declarations }
    property QualityVisible: boolean read GetQualityVisible write SetQualityVisible;
    property LosslessVisible: boolean read GetLosslessVisible write SetLosslessVisible;
    property BitsPerPixelVisible: boolean read GetBitsPerPixelVisible write SetBitsPerPixelVisible;
    property LazPaintInstance: TLazPaintCustomInstance read FLazPaintInstance write SetLazPaintInstance;
    property OutputFilename: string read FOutputFilename write SetOutputFilename;
    property ImageFormat: TBGRAImageFormat read FImageFormat write SetImageFormat;
    property Quantizer: TBGRAColorQuantizer read GetColorQuantizer;
    property QuantizerNeeded: boolean read GetQuantizerNeeded;
    property JpegPreview: TBGRABitmap read GetJpegPreview;
    property JpegQuality: integer read GetJpegQuality write SetJpegQuality;
    property WebPPreview: TBGRABitmap read GetWebPPreview;
    property WebPLossless: boolean read GetWebPLossless write SetWebPLossless;
    property WantedBitsPerPixel: integer read GetWantedBitsPerPixel;
    property BmpStreamNeeded: boolean read GetBmpStreamNeeded;
    property PngStreamNeeded: boolean read GetPngStreamNeeded;
  end;

function ShowSaveOptionDialog(AInstance: TLazPaintCustomInstance; AOutputFilenameUTF8: string;
  ASkipOptions: boolean; AExport: boolean): boolean;

implementation

uses UGraph, FPWriteJPEG, UResourceStrings, FPWriteBMP, BMPcomn,
  UMySLV, BGRAWriteBmpMioMap, BGRADithering, UFileSystem, LCScaleDPI,
  BGRAThumbnail, BGRAIconCursor, BGRAWinResource, BGRAWriteWebP;

function ShowSaveOptionDialog(AInstance: TLazPaintCustomInstance; AOutputFilenameUTF8: string;
  ASkipOptions: boolean; AExport: boolean): boolean;
var f: TFSaveOption;
begin
  result := false;
  if not ASkipOptions and (SuggestImageFormat(AOutputFilenameUTF8) in[ifBmp,ifJpeg,ifWebP,ifPng,ifIco,ifCur]) then
  begin
    f := TFSaveOption.Create(nil);
    try
      f.LazPaintInstance := AInstance;
      f.Exporting := AExport;
      f.OutputFilename := AOutputFilenameUTF8;
      result := f.ShowModal = mrOK;
    except
      on ex:Exception do
        AInstance.ShowError('ShowSaveOptionDialog',ex.Message);
    end;
    f.Free;
  end
  else
  begin
    AInstance.StartSavingImage(AOutputFilenameUTF8);
    try
      AInstance.Image.SaveToFileUTF8(AOutputFilenameUTF8, AExport);
      result := true;
    except
      on ex:Exception do
        AInstance.ShowError('ShowSaveOptionDialog',ex.Message);
    end;
    AInstance.EndSavingImage;
  end;
end;

{ TFSaveOption }

procedure TFSaveOption.FormCreate(Sender: TObject);
begin
  FImageFormat:= ifUnknown;
  UpdateQualityTextBox;
  FFormTitle:= Caption;
  FQualityVisible:= false;
  FLosslessVisible:= true;
  FBitsPerPixelVisible:= false;
  Panel_Quality.Visible := FQualityVisible;
  Panel_BitsPerPixel.Visible := FBitsPerPixelVisible;
  FFlattenedOriginal := nil;
  FSizeCaption:= Label_Size.Caption;
  LayoutRadioButtonDepth;
  BGRAPreview.BitmapAutoScale:= false;
  BGRAPreview.Color := clGray;
end;

procedure TFSaveOption.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FQuantizer);
  FreeAndNil(FQuantizer1bit);
  FreeAndNil(FJpegPreview);
  FreeAndNil(FWebPPreview);
  FreeAndNil(FBmpStream);
  FreeAndNil(FJpegStream);
  FreeAndNil(FWebPStream);
  FreeAndNil(FPngStream);
end;

procedure TFSaveOption.FormHide(Sender: TObject);
begin
  if Assigned(FLazPaintInstance) then
    FLazPaintInstance.Config.SetDefaultSaveOptionDialogMaximized(WindowState = wsMaximized);
end;

procedure TFSaveOption.RadioButton_BitsPerPixelChange(Sender: TObject);
begin
  if FInit then exit;
  BmpQualityChanged;
  PngQualityChanged;
  NeedBitmapUpdate(True);
  UpdateDitheringCheckbox;
end;

procedure TFSaveOption.Timer_UpdateTimer(Sender: TObject);
begin
  Timer_Update.Enabled:= false;
  DoUpdateBitmap;
end;

procedure TFSaveOption.Edit_QualityValueChange(Sender: TObject);
var v,err: integer;
begin
  if FInit then exit;
  try
    val(Edit_QualityValue.Text, v, err);
    if err <> 0 then exit;
  except
    on ex: exception do
      exit;
  end;
  FInit := true;
  TrackBar_Quality.Position := v;
  FInit := false;
  JpegQualityChanged;
end;

procedure DrawHourglass(bmp: TBGRABitmap);
// http://www.openclipart.org/
// Open Clip Art Library
// Frederic Moser
var
  t: single;
begin
  with bmp.Canvas2D do
  begin
    if width < height then
      t := width
    else
      t := height;
    save;
    t /= 2;
    translate((width-t/1.7)/2,(height-t)/2);
    scale(t/1.7/32.4,t/47.2);
    translate(1,-1.5);
    path('M 0.697,4.347 C 0.697,19.895 14.037,17.569 14.037,25.159 C 14.037,32.138 0.70908201,29.332918 0.70908201,46.104918 L 29.565082,46.104918 C 29.565082,29.392918 15.923,32.236 15.923,25.159 C 15.923,17.625 29.528,17.625 29.624,4.347 L 0.697,4.347 L 0.697,4.347 z');
    strokeStyle('#ffffff');
    lineWidth:= 7;
    stroke;
    fillStyle('#dddddd');
    strokeStyle('#000000');
    lineWidth:= 3;
    stroke;
    fill;
    path('M 1.956,44.819 C 4.12,44.886 26.85,44.886 28.541,44.886 C 28.608,44.346 28.406,40.896 26.984,37.513 C 25.428,37.513 3.579,37.446 3.579,37.446 C 1.82,41.099 1.889,44.955 1.956,44.819 z');
    fillStyle('#b5ab93');
    fill;
    path('M 4.797,13.973 C 10.209,14.312 18.665,14.514 24.009,14.04 C 23.873,15.935 15.011,19.385 14.944,23.645 C 14.876,19.385 5.338,16.475 4.797,13.973 z');
    fill;
    path('M 4.003,6.02 L 10.091,6.02 C 10.091,16.952 14.778,17.273 15.264,23.374 C 13.178,16.083 4.689,16.01 4.003,6.02 z');
    fillStyle(BGRA(255,255,255,153));
    fill;
    path('M 3.722,45.108 L 9.81,45.108 C 8.147,33.92 14.497,34.435 14.982,26.906 C 12.896,34.197 2.863,32.046 3.722,45.108 z');
    fill;
    restore;
  end;
end;

procedure TFSaveOption.BGRAPreviewRedraw(Sender: TObject;
  Bitmap: TBGRABitmap);
var ratioX,ratioY,ratio, checkerScale: single;
  picture: TBGRABitmap;
  mustFreePic: boolean;
  x,y,visualWidth,visualHeight,ofs: integer;
  r: TRect;
  thumb: TBGRABitmap;
begin
  Timer_Update.Enabled:= false;
  if QuantizerNeeded then
  begin
    mustFreePic:= true;
    if (ImageFormat in [ifCur,ifIco]) and not PngStreamNeeded then
      picture := BGRADitherIconCursor(FFlattenedOriginal, WantedBitsPerPixel, GetDitheringAlgorithm) as TBGRABitmap
    else
      picture := Quantizer.GetDitheredBitmap(GetDitheringAlgorithm, FFlattenedOriginal) as TBGRABitmap;
  end else
  if ImageFormat = ifJpeg then
  begin
    mustFreePic:= false;
    picture := JpegPreview;
  end else
  if (ImageFormat = ifWebP) and not WebPLossless then
  begin
    mustFreePic:= false;
    picture := WebPPreview;
  end else
  if BmpStreamNeeded and (FBmpStream = nil) then
  begin
    FBmpStream := TMemoryStream.Create;
    if GetDitheringAlgorithm <> daNearestNeighbor then
    begin
      picture := DitherImageTo16Bit(GetDitheringAlgorithm, FFlattenedOriginal) as TBGRABitmap;
      picture.SaveToStreamAs(FBmpStream, ifBmpMioMap);
      picture.Free;
    end else
      FFlattenedOriginal.SaveToStreamAs(FBmpStream, ifBmpMioMap);
    UpdateFileSize;
    mustFreePic:= true;
    FBmpStream.Position:= 0;
    picture := TBGRABitmap.Create(FBmpStream);
  end else
  begin
    mustFreePic:= false;
    picture := FFlattenedOriginal;
  end;
  MakePngStreamIfNeeded;
  if ImageFormat = ifWebP then RequireWebPStream;

  if (Bitmap.Width = 0) or (Bitmap.Height = 0) or (picture.Width = 0) or (picture.Height = 0) then exit;
  ratioX := Bitmap.Width/picture.Width;
  ratioY := Bitmap.Height/picture.Height;
  if ratioX < ratioY then ratio := ratioX else ratio := ratioY;
  if ratio > 1 then ratio := trunc(ratio);
  visualWidth := round(picture.Width*ratio);
  if visualWidth < 1 then visualWidth:= 1;
  visualHeight := round(picture.Height*ratio);
  if visualHeight < 1 then visualHeight:= 1;
  x := (Bitmap.Width-visualWidth) div 2;
  y := (Bitmap.Height-visualHeight) div 2;
  r := RectWithSize(x, y, visualWidth, visualHeight);

  checkerScale := DoScaleX(60, OriginalDPI)/60;
  if not BGRAPreview.BitmapAutoScale then
    checkerScale *= BGRAPreview.GetCanvasScaleFactor;
  ofs := round(4*checkerScale);
  if visualWidth < ofs then ofs := visualWidth;
  if visualHeight < ofs then ofs := visualHeight;
  bitmap.FillRect(rect(x+visualWidth,y+ofs,x+ofs+visualWidth,y+ofs+visualHeight), BGRA(0,0,0,128),dmDrawWithTransparency);
  bitmap.FillRect(rect(x+ofs,y+visualHeight,x+visualWidth,y+ofs+visualHeight), BGRA(0,0,0,128),dmDrawWithTransparency);

  if ImageFormat in[ifIco,ifCur] then
  begin
    thumb := GetBitmapThumbnail(picture, ImageFormat, visualWidth,visualHeight, BGRAPixelTransparent, true);
    Bitmap.PutImage(r.Left,r.Top, thumb, dmSet);
    thumb.Free;
  end else
  begin
    DrawCheckers(Bitmap, r, checkerScale);
    Bitmap.StretchPutImage(r, picture, dmDrawWithTransparency);
  end;
  if mustFreePic then picture.Free;
end;

procedure TFSaveOption.Button_OKClick(Sender: TObject);
  procedure DoneSave(AMultiLayerSave: boolean = false);
  begin
    if not Exporting then
    begin
      if AMultiLayerSave or (FLazPaintInstance.Image.NbLayers = 1) then FLazPaintInstance.Image.SetSavedFlag
        else FLazPaintInstance.Image.OnImageSaving.NotifyObservers;
    end else
      FLazPaintInstance.Image.OnImageExport.NotifyObservers;
  end;

  procedure SavePng;
  var outputStream: TStream;
  begin
    MakePngStreamIfNeeded;
    if assigned(FPngStream) then
    begin
      outputStream := FileManager.CreateFileStream(FOutputFilename,fmCreate);
      try
        FPngStream.Position := 0;
        outputStream.CopyFrom(FPngStream, FPngStream.Size);
        DoneSave;
      finally
        outputStream.Free;
      end;
    end;
  end;

  procedure WriteBmpStream(outputStream: TStream);
  var writer: TFPWriterBMP;
    dithered: TBGRACustomBitmap;
  begin
    MakeBmpStreamIfNeeded;
    if Assigned(FBmpStream) then
    begin
      FBmpStream.Position := 0;
      outputStream.CopyFrom(FBmpStream, FBmpStream.Size);
      DoneSave;
    end else
    if QuantizerNeeded then
    begin
      dithered := Quantizer.GetDitheredBitmap(GetDitheringAlgorithm, FFlattenedOriginal);
      Quantizer.ReducedPalette.AssignTo(dithered);
      writer := TFPWriterBMP.Create;
      writer.BitsPerPixel := WantedBitsPerPixel;
      try
        dithered.SaveToStream(outputStream, writer);
        DoneSave;
      finally
        writer.Free;
        dithered.Free;
      end;
    end else
    begin
      writer := TFPWriterBMP.Create;
      writer.BitsPerPixel := WantedBitsPerPixel;
      try
        FFlattenedOriginal.SaveToStream(outputStream, writer);
        DoneSave;
      finally
        writer.Free;
      end;
    end;
  end;

  procedure SaveBmp;
  var
    outputStream: TStream;
  begin
    outputStream := FileManager.CreateFileStream(FOutputFilename,fmCreate);
    try
      WriteBmpStream(outputStream);
    finally
      outputStream.Free;
    end;
  end;

  procedure SaveIcoCur;
  var inputStream,outputStream: TStream;
    icoCur: TBGRAIconCursor;
    picture: TBGRACustomBitmap;
  begin
    icoCur := TBGRAIconCursor.Create;
    if FileManager.FileExists(FOutputFilename) then
    begin
      inputStream := FileManager.CreateFileStream(FOutputFilename,fmOpenRead or fmShareDenyWrite);
      try
        icoCur.LoadFromStream(inputStream);
      finally
        inputStream.Free;
      end;
    end else
    begin
      icoCur.FileType:= ImageFormat;
    end;
    try
      if PngStreamNeeded then
      begin
        MakePngStreamIfNeeded;
        icoCur.Add(FPngStream,true,false);
      end else
      begin
        picture := BGRADitherIconCursor(FFlattenedOriginal, WantedBitsPerPixel, GetDitheringAlgorithm) as TBGRABitmap;
        try
          icoCur.Add(picture,WantedBitsPerPixel,true);
        finally
          picture.free;
        end;
      end;
      outputStream := FileManager.CreateFileStream(FOutputFilename,fmCreate);
      try
        icoCur.SaveToStream(outputStream);
        DoneSave(true);
      finally
        outputStream.Free;
      end;
    finally
      icoCur.Free;
    end;
  end;

  procedure SaveJpeg;
  var outputStream: TStream;
  begin
    RequireJpegStream;
    outputStream := FileManager.CreateFileStream(FOutputFilename,fmCreate);
    try
      FJpegStream.Position := 0;
      outputStream.CopyFrom(FJpegStream, FJpegStream.Size);
      FLazPaintInstance.Config.SetDefaultJpegQuality(JpegQuality);
      DoneSave;
    finally
      outputStream.Free;
    end;
  end;

  procedure SaveWebP;
  var outputStream: TStream;
  begin
    RequireWebPStream;
    outputStream := FileManager.CreateFileStream(FOutputFilename,fmCreate);
    try
      FWebPStream.Position := 0;
      outputStream.CopyFrom(FWebPStream, FWebPStream.Size);
      FLazPaintInstance.Config.SetDefaultJpegQuality(JpegQuality);
      FLazPaintInstance.Config.SetDefaultWebPLossless(WebPLossless);
      DoneSave;
    finally
      outputStream.Free;
    end;
  end;

begin
  try
    Case ImageFormat of
    ifJpeg: begin
              SaveJpeg;
              ModalResult := mrOK;
            end;
    ifWebP: begin
              SaveWebP;
              ModalResult := mrOK;
            end;
    ifBmp:
      begin
        SaveBmp;
        ModalResult := mrOK;
      end;
    ifPng:
      begin
        SavePng;
        ModalResult := mrOK;
      end;
    ifIco,ifCur:
      begin
        SaveIcoCur;
        ModalResult := mrOK;
      end;
    else
      ModalResult := mrAbort;
    end;
  except
    on ex:Exception do
    begin
      FLazPaintInstance.ShowError(rsSave, ex.Message);
      ModalResult:= mrAbort;
    end;
  end;
end;

procedure TFSaveOption.CheckBox_DitheringChange(Sender: TObject);
begin
  if FInit then exit;
  BmpQualityChanged;
  PngQualityChanged;
  NeedBitmapUpdate(True);
end;

procedure TFSaveOption.CheckBox_LosslessChange(Sender: TObject);
begin
  if FInit then exit;
  WebPQualityChanged;
end;

procedure TFSaveOption.Edit_QualityValueExit(Sender: TObject);
begin
  if FInit then exit;
  FInit := true;
  Edit_QualityValue.Text := IntToStr(TrackBar_Quality.Position);
  FInit := false;
end;

procedure TFSaveOption.TrackBar_QualityChange(Sender: TObject);
begin
  if FInit then exit;
  UpdateQualityTextBox;
  JpegQualityChanged;
end;

procedure TFSaveOption.PngQualityChanged;
begin
  FreeAndNil(FPngStream);
end;

function TFSaveOption.GetBmpStreamNeeded: boolean;
begin
  result := (FImageFormat in [ifBmp,ifBmpMioMap]) and RadioButton_MioMap.Checked;
end;

procedure TFSaveOption.BmpQualityChanged;
begin
  FreeAndNil(FBmpStream);
end;

function TFSaveOption.GetLosslessVisible: boolean;
begin
  result := FLosslessVisible;
end;

function TFSaveOption.GetPngStreamNeeded: boolean;
begin
  result := (ImageFormat = ifPng) or
        ( (ImageFormat in[ifIco,ifCur]) and
          ((FFlattenedOriginal.Width >= 256) or (FFlattenedOriginal.Height >= 256)) and
          ((FFlattenedOriginal.XorMask = nil) or FFlattenedOriginal.XorMask.Empty) );
end;

function TFSaveOption.GetWebPLossless: boolean;
begin
  result := CheckBox_Lossless.Checked;
end;

procedure TFSaveOption.UpdateQualityTextBox;
begin
  FInit := true;
  Edit_QualityValue.Text := IntToStr(TrackBar_Quality.Position);
  FInit := false;
end;

function TFSaveOption.GetQualityVisible: boolean;
begin
  result := FQualityVisible;
end;

function TFSaveOption.GetQuantizerNeeded: boolean;
begin
  result := (FImageFormat in [ifPng,ifBmp,ifIco,ifCur]) and (WantedBitsPerPixel <= 8);
end;

function TFSaveOption.GetBitsPerPixelVisible: boolean;
begin
  result := FBitsPerPixelVisible;
end;

function TFSaveOption.GetColorQuantizer: TBGRAColorQuantizer;
begin
  if WantedBitsPerPixel = 1 then
  begin
    if not Assigned(FQuantizer1bit) then
    begin
      if FFlattenedOriginal.HasTransparentPixels then
        FQuantizer1bit := TBGRAColorQuantizer.Create([BGRABlack,BGRAWhite,BGRAPixelTransparent],false,3)
      else
        FQuantizer1bit := TBGRAColorQuantizer.Create([BGRABlack,BGRAWhite],false,2);
    end;
    result := FQuantizer1bit;
  end else
  begin
    if not Assigned(FQuantizer) then
      FQuantizer := TBGRAColorQuantizer.Create(FFlattenedOriginal,acFullChannelInPalette);
    FQuantizer.ReductionColorCount := 1 shl WantedBitsPerPixel;
    result := FQuantizer;
  end;
end;

function TFSaveOption.GetJpegPreview: TBGRABitmap;
begin
  RequireJpegStream;
  if not Assigned(FJpegPreview) then
  begin
    FJpegPreview := TBGRABitmap.Create;
    FJpegStream.Position := 0;
    FJpegPreview.LoadFromStream(FJpegStream);
  end;
  result := FJpegPreview;
end;

procedure TFSaveOption.RequireJpegStream;
var writer: TFPWriterJPEG;
begin
  if not Assigned(FJpegStream) then
  begin
    FJpegStream := TMemoryStream.Create;
    writer := TFPWriterJPEG.Create;
    writer.CompressionQuality := JpegQuality;
    FFlattenedOriginal.SaveToStream(FJpegStream, writer);
    writer.Free;
    UpdateFileSize;
  end;
end;

function TFSaveOption.GetWebPPreview: TBGRABitmap;
begin
  RequireWebPStream;
  if not Assigned(FWebPPreview) then
  begin
    FWebPPreview := TBGRABitmap.Create;
    FWebPStream.Position := 0;
    FWebPPreview.LoadFromStream(FWebPStream);
  end;
  result := FWebPPreview;
end;

procedure TFSaveOption.RequireWebPStream;
var writer: TBGRAWriterWebP;
begin
  if not Assigned(FWebPStream) then
  begin
    FWebPStream := TMemoryStream.Create;
    writer := TBGRAWriterWebP.Create;
    writer.QualityPercent := JpegQuality;
    writer.Lossless:= WebPLossless;
    FFlattenedOriginal.SaveToStream(FWebPStream, writer);
    writer.Free;
    UpdateFileSize;
  end;
end;

procedure TFSaveOption.MakeBmpStreamIfNeeded;
begin
  if RadioButton_MioMap.Checked then
  begin
    if not Assigned(FBmpStream) then
    begin
      FBmpStream := TMemoryStream.Create;
      FFlattenedOriginal.SaveToStreamAs(FBmpStream, ifBmpMioMap);
      UpdateFileSize;
    end;
  end else
    FreeAndNil(FBmpStream);
end;

function TFSaveOption.GetWantedBitsPerPixel: integer;
begin
  if RadioButton_2Colors.Checked then
    result := 1
  else if RadioButton_16Colors.Checked then
    result := 4
  else if RadioButton_256Colors.Checked then
    result := 8
  else if RadioButton_MioMap.Checked then
    result := 16
  else if RadioButton_24BitsPerPixel.Checked then
    result := 24
  else
    result := 32;
end;

procedure TFSaveOption.SetJpegQuality(AValue: integer);
var oldInit: boolean;
begin
  oldInit := FInit;
  FInit := true;
  Edit_QualityValue.Text := IntToStr(AValue);
  TrackBar_Quality.Position := AValue;
  FInit := oldInit;
end;

procedure TFSaveOption.SetLosslessVisible(AValue: boolean);
begin
  if FLosslessVisible = AValue then exit;
  FLosslessVisible := AValue;
  CheckBox_Lossless.Visible := FLosslessVisible;
end;

procedure TFSaveOption.SetWebPLossless(AValue: boolean);
begin
  CheckBox_Lossless.Checked := AValue;
end;

procedure TFSaveOption.UpdateFileSize;
var size: int64;
begin
  case ImageFormat of
  ifBmp: if BmpStreamNeeded then
    begin
      if FBmpStream = nil then
         UpdateFileSizeTo(-1)
      else
         UpdateFileSizeTo(FBmpStream.Size);
    end else
    begin
      size := int64((FFlattenedOriginal.Width*WantedBitsPerPixel+31) div 32)*4*FFlattenedOriginal.Height;
      if QuantizerNeeded then size += int64(1 shl WantedBitsPerPixel)*4;
      size += sizeof(TBitMapFileHeader)+sizeof(TBitMapInfoHeader);
      UpdateFileSizeTo(size);
    end;
  ifIco,ifCur: if FPngStream = nil then
    begin
      size := int64((FFlattenedOriginal.Width*WantedBitsPerPixel+31) div 32)*4*FFlattenedOriginal.Height;
      if QuantizerNeeded then size += int64(1 shl WantedBitsPerPixel)*4;
      size += sizeof(TIconFileDirEntry)+sizeof(TBitMapInfoHeader);
      UpdateFileSizeTo(size);
    end
    else
       UpdateFileSizeTo(sizeof(TIconFileDirEntry)+FPngStream.Size);
  ifJpeg: if FJpegStream = nil then
             UpdateFileSizeTo(-1)
          else
             UpdateFileSizeTo(FJpegStream.Size);
  ifWebP: if FWebPStream = nil then
             UpdateFileSizeTo(-1)
          else
             UpdateFileSizeTo(FWebPStream.Size);
  ifPng: if FPngStream = nil then
             UpdateFileSizeTo(-1)
          else
             UpdateFileSizeTo(FPngStream.Size);
  else UpdateFileSizeTo(-1);
  end;
end;

procedure TFSaveOption.UpdateFileSizeTo(AValue: int64);
begin
  if AValue < 0 then
    Label_Size.Caption := FSizeCaption + ' ?'
  else
    Label_Size.Caption := FSizeCaption + ' ' + FileSizeToStr(AValue,rsBytes);
end;

function TFSaveOption.GetJpegQuality: integer;
begin
  result := TrackBar_Quality.Position;
  if result < low(TJPEGQualityRange) then result := low(TJPEGQualityRange);
  if result > high(TJPEGQualityRange) then result := high(TJPEGQualityRange);
end;

procedure TFSaveOption.SetBitsPerPixelVisible(AValue: boolean);
begin
  if FBitsPerPixelVisible = AValue then exit;
  FBitsPerPixelVisible := AValue;
  Panel_BitsPerPixel.Visible := FBitsPerPixelVisible;
end;

procedure TFSaveOption.SetImageFormat(AValue: TBGRAImageFormat);
var origBPP: integer;
begin
  if FImageFormat=AValue then Exit;
  FImageFormat:=AValue;
  QualityVisible := FImageFormat in[ifJpeg, ifWebP];
  LosslessVisible := (FImageFormat = ifWebP);
  BitsPerPixelVisible := FImageFormat in[ifPng,ifBmp,ifIco,ifCur];
  if FInit then exit;
  FInit := true;
  origBPP := GetOriginalBitDepth;
  if PngStreamNeeded then
  begin
    RadioButton_2Colors.Enabled := false;
    RadioButton_16Colors.Enabled := false;
    RadioButton_256Colors.Enabled := true;
    RadioButton_MioMap.Enabled := false;
    if FFlattenedOriginal.HasTransparentPixels then
    begin
      RadioButton_24BitsPerPixel.Enabled := false;
      RadioButton_32BitsPerPixel.Enabled := true;
    end
    else
    begin
      RadioButton_24BitsPerPixel.Enabled := true;
      RadioButton_32BitsPerPixel.Enabled := false;
    end;

    if origBPP > 8 then
    begin
      if RadioButton_24BitsPerPixel.Enabled then
        RadioButton_24BitsPerPixel.Checked := true
      else
        RadioButton_32BitsPerPixel.Checked := true;
    end
    else
      RadioButton_256Colors.Checked := true;
  end else
  if FImageFormat in[ifIco,ifCur] then
  begin
    RadioButton_2Colors.Enabled := true;
    RadioButton_16Colors.Enabled := true;
    RadioButton_256Colors.Enabled := true;
    RadioButton_24BitsPerPixel.Enabled := true;
    RadioButton_32BitsPerPixel.Enabled := true;

    if FFlattenedOriginal.HasSemiTransparentPixels then
      RadioButton_32BitsPerPixel.Checked := true else
    if origBPP > 8 then
      RadioButton_24BitsPerPixel.Checked := true else
    if origBPP > 4 then
      RadioButton_256Colors.Checked := true else
    if origBPP > 1 then
      RadioButton_16Colors.Checked := true else
      RadioButton_2Colors.Checked := true;

    RadioButton_MioMap.Enabled := false;
  end else
  if FImageFormat = ifBmp then
  begin
    if FFlattenedOriginal.HasTransparentPixels then
    begin
      RadioButton_2Colors.Enabled := false;
      RadioButton_16Colors.Enabled := false;
      RadioButton_256Colors.Enabled := false;
      RadioButton_24BitsPerPixel.Enabled := false;
      RadioButton_32BitsPerPixel.Enabled := true;
      RadioButton_32BitsPerPixel.Checked := true;
    end else
    begin
      RadioButton_2Colors.Enabled := true;
      RadioButton_16Colors.Enabled := true;
      RadioButton_256Colors.Enabled := true;
      RadioButton_24BitsPerPixel.Enabled := true;
      RadioButton_32BitsPerPixel.Enabled := true;
      if origBPP > 8 then
        RadioButton_24BitsPerPixel.Checked := true else
      if origBPP > 4 then
        RadioButton_256Colors.Checked := true else
      if origBPP > 1 then
        RadioButton_16Colors.Checked := true else
        RadioButton_2Colors.Checked := true;
    end;
    RadioButton_MioMap.Enabled := true;
  end;
  LayoutRadioButtonDepth;
  UpdateDitheringCheckbox;
  UpdateFileSize;
  FInit := false;
end;

procedure TFSaveOption.SetLazPaintInstance(AValue: TLazPaintCustomInstance);
begin
  if FLazPaintInstance=AValue then Exit;
  FLazPaintInstance:=AValue;
  FreeAndNil(FQuantizer);
  FreeAndNil(FQuantizer1bit);
  FreeAndNil(FJpegStream);
  FreeAndNil(FJpegPreview);
  FreeAndNil(FWebPStream);
  FreeAndNil(FWebPPreview);
  JpegQuality := FLazPaintInstance.Config.DefaultJpegQuality;
  WebPLossless := FLazPaintInstance.Config.DefaultWebPLossless;
  FFlattenedOriginal := FLazPaintInstance.Image.RenderedImage;
  UpdateFileSize;
  if LazPaintInstance.Config.DefaultSaveOptionDialogMaximized then
    WindowState := wsMaximized;
end;

procedure TFSaveOption.SetOutputFilename(AValue: string);
begin
  AValue := Trim(AValue);
  if FOutputFilename=AValue then Exit;
  FOutputFilename:=AValue;
  if length(AValue) = 0 then
    Caption := FFormTitle
  else
    Caption := FFormTitle + ' - ' + FOutputFilename;
  if ImageFormat = ifUnknown then
    ImageFormat:= SuggestImageFormat(FOutputFilename);
end;

procedure TFSaveOption.SetQualityVisible(AValue: boolean);
begin
  if FQualityVisible = AValue then exit;
  FQualityVisible := AValue;
  Panel_Quality.Visible := FQualityVisible;
end;

procedure TFSaveOption.NeedBitmapUpdate(AImmediate: boolean);
begin
  Timer_Update.Enabled := false;
  if AImmediate then Timer_Update.Interval := 15
  else Timer_Update.Interval := 500;
  Timer_Update.Enabled := true;
end;

function TFSaveOption.GetDitheringAlgorithm: TDitheringAlgorithm;
begin
  if CheckBox_Dithering.Checked then
    result := daFloydSteinberg
  else
    result := daNearestNeighbor;
end;

procedure TFSaveOption.UpdateDitheringCheckbox;
begin
  CheckBox_Dithering.Enabled := not RadioButton_24BitsPerPixel.Checked and
                                not RadioButton_32BitsPerPixel.Checked;
end;

function TFSaveOption.GetOriginalBitDepth: integer;
begin
  if ImageFormat in[ifIco,ifCur] then
    result := BGRABitDepthIconCursor(FFlattenedOriginal)
  else
    result := BGRARequiredBitDepth(FFlattenedOriginal, acFullChannelInPalette);
end;

procedure TFSaveOption.DoUpdateBitmap;
begin
  UpdateFileSize;
  DrawHourglass(BGRAPreview.Bitmap);
  BGRAPreview.Repaint;
  { $IFDEF LINUX}
  Application.ProcessMessages;
  { $ENDIF}
  BGRAPreview.DiscardBitmap;
end;

procedure TFSaveOption.JpegQualityChanged;
begin
  FreeAndNil(FJpegPreview);
  FreeAndNil(FJpegStream);
  FreeAndNil(FWebPPreview);
  FreeAndNil(FWebPStream);
  UpdateFileSize;
  NeedBitmapUpdate(False);
end;

procedure TFSaveOption.WebPQualityChanged;
begin
  FreeAndNil(FWebPPreview);
  FreeAndNil(FWebPStream);
  UpdateFileSize;
  NeedBitmapUpdate(False);
end;

procedure TFSaveOption.LayoutRadioButtonDepth;
var y: integer;

  procedure LayoutItem(ACtrl: TControl; AShow: boolean);
  begin
    if AShow then
    begin
      ACtrl.Visible := true;
      ACtrl.Top := y;
      y += ACtrl.Height + DoScaleY(3,96);
    end else
    begin
      ACtrl.Visible := false;
    end;
  end;

begin
  y := Label_ColorDepth.Top+Label_ColorDepth.Height+DoScaleY(4,96);
  LayoutItem(RadioButton_2Colors, RadioButton_2Colors.Enabled);
  LayoutItem(RadioButton_16Colors, RadioButton_16Colors.Enabled);
  LayoutItem(RadioButton_256Colors, RadioButton_256Colors.Enabled);
  LayoutItem(RadioButton_MioMap, RadioButton_MioMap.Enabled);
  LayoutItem(RadioButton_24BitsPerPixel, RadioButton_24BitsPerPixel.Enabled);
  LayoutItem(RadioButton_32BitsPerPixel, RadioButton_32BitsPerPixel.Enabled);
  LayoutItem(CheckBox_Dithering, true);
  Panel_BitsPerPixel.Height := y+DoScaleY(1,96);
end;

procedure TFSaveOption.MakePngStreamIfNeeded;
begin
  if PngStreamNeeded and (FPngStream = nil) then
  begin
    FPngStream := TMemoryStream.Create;
    if QuantizerNeeded then
      Quantizer.SaveBitmapToStream(GetDitheringAlgorithm, FFlattenedOriginal, FPngStream, ifPng)
    else
      FFlattenedOriginal.SaveToStreamAsPng(FPngStream);
    UpdateFileSize;
  end;
end;

{$R *.lfm}

end.

