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
    CheckBox_Dithering: TCheckBox;
    Edit_QualityValue: TEdit;
    Label1: TLabel;
    Label_Size: TLabel;
    Label_0: TLabel;
    Label_1: TLabel;
    Label_50: TLabel;
    Label_ColorDepth: TLabel;
    Panel_OkCancel: TPanel;
    Panel_Quality: TPanel;
    Panel_BitsPerPixel: TPanel;
    Panel_Option: TPanel;
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
    FQualityVisible, FBitsPerPixelVisible: boolean;
    FFlattenedOriginal, FJpegPreview: TBGRABitmap;
    FBmpStream, FJpegStream, FPngStream: TMemoryStream;
    FFormTitle: string;
    FImageFormat: TBGRAImageFormat;
    FQuantizer: TBGRAColorQuantizer;
    FSizeCaption: string;
    function GetBmpStreamNeeded: boolean;
    procedure BmpQualityChanged;
    procedure PngQualityChanged;
    function GetBitsPerPixelVisible: boolean;
    function GetColorQuantizer: TBGRAColorQuantizer;
    function GetJpegPreview: TBGRABitmap;
    procedure RequireJpegStream;
    procedure MakeBmpStreamIfNeeded;
    function GetWantedBitsPerPixel: integer;
    procedure SetJpegQuality(AValue: integer);
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
  public
    { public declarations }
    property QualityVisible: boolean read GetQualityVisible write SetQualityVisible;
    property BitsPerPixelVisible: boolean read GetBitsPerPixelVisible write SetBitsPerPixelVisible;
    property LazPaintInstance: TLazPaintCustomInstance read FLazPaintInstance write SetLazPaintInstance;
    property OutputFilename: string read FOutputFilename write SetOutputFilename;
    property ImageFormat: TBGRAImageFormat read FImageFormat write SetImageFormat;
    property Quantizer: TBGRAColorQuantizer read GetColorQuantizer;
    property QuantizerNeeded: boolean read GetQuantizerNeeded;
    property JpegPreview: TBGRABitmap read GetJpegPreview;
    property JpegQuality: integer read GetJpegQuality write SetJpegQuality;
    property WantedBitsPerPixel: integer read GetWantedBitsPerPixel;
    property BmpStreamNeeded: boolean read GetBmpStreamNeeded;
  end;

function ShowSaveOptionDialog(AInstance: TLazPaintCustomInstance; AOutputFilenameUTF8: string): boolean;

implementation

uses UGraph, FPWriteJPEG, UResourceStrings, lazutf8classes, FPWriteBMP, BMPcomn,
  UMySLV, BGRAWriteBmpMioMap, BGRADithering;

function ShowSaveOptionDialog(AInstance: TLazPaintCustomInstance; AOutputFilenameUTF8: string): boolean;
var f: TFSaveOption;
begin
  result := false;
  if SuggestImageFormat(AOutputFilenameUTF8) in[ifBmp,ifJpeg,ifPng] then
  begin
    f := TFSaveOption.Create(nil);
    try
      f.LazPaintInstance := AInstance;
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
    try
      AInstance.Image.SaveToFileUTF8(AOutputFilenameUTF8);
      result := true;
    except
      on ex:Exception do
        AInstance.ShowError('ShowSaveOptionDialog',ex.Message);
    end;
  end;
end;

{ TFSaveOption }

procedure TFSaveOption.FormCreate(Sender: TObject);
begin
  FImageFormat:= ifUnknown;
  UpdateQualityTextBox;
  FFormTitle:= Caption;
  FQualityVisible:= false;
  FBitsPerPixelVisible:= false;
  Panel_Quality.Visible := FQualityVisible;
  Panel_BitsPerPixel.Visible := FBitsPerPixelVisible;
  FFlattenedOriginal := nil;
  FSizeCaption:= Label_Size.Caption;
end;

procedure TFSaveOption.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FFlattenedOriginal);
  FreeAndNil(FQuantizer);
  FreeAndNil(FJpegPreview);
  FreeAndNil(FBmpStream);
  FreeAndNil(FJpegStream);
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
  val(Edit_QualityValue.Text, v, err);
  if err <> 0 then exit;
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
var ratioX,ratioY,ratio: single;
  picture: TBGRABitmap;
  mustFreePic: boolean;
  x,y,visualWidth,visualHeight: integer;
  r: TRect;
begin
  Timer_Update.Enabled:= false;
  if QuantizerNeeded then
  begin
    mustFreePic:= true;
    picture := Quantizer.GetDitheredBitmap(GetDitheringAlgorithm, FFlattenedOriginal) as TBGRABitmap;
    if (ImageFormat = ifPng) and (FPngStream = nil) then
    begin
      FPngStream := TMemoryStream.Create;
      Quantizer.SaveBitmapToStream(GetDitheringAlgorithm, FFlattenedOriginal, FPngStream, ifPng);
      UpdateFileSize;
    end;
  end else
  if ImageFormat = ifJpeg then
  begin
    mustFreePic:= false;
    picture := GetJpegPreview;
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
    if (ImageFormat = ifPng) and (FPngStream = nil) then
    begin
      FPngStream := TMemoryStream.Create;
      FFlattenedOriginal.SaveToStreamAsPng(FPngStream);
      UpdateFileSize;
    end;
  end;

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
  r := rect(x,y,x+visualWidth,y+visualHeight);
  DrawCheckers(Bitmap, r);
  Bitmap.StretchPutImage(r, picture, dmDrawWithTransparency);
  if mustFreePic then picture.Free;
end;

procedure TFSaveOption.Button_OKClick(Sender: TObject);
  procedure SavePng;
  var outputStream: TFileStreamUTF8;
  begin
    if assigned(FPngStream) then
    begin
      outputStream := TFileStreamUTF8.Create(FOutputFilename,fmCreate);
      try
        FPngStream.Position := 0;
        outputStream.CopyFrom(FPngStream, FPngStream.Size);
        if FLazPaintInstance.Image.NbLayers = 1 then FLazPaintInstance.Image.SetSavedFlag;
      finally
        outputStream.Free;
      end;
    end else
    begin
      if QuantizerNeeded then
        Quantizer.SaveBitmapToFile(GetDitheringAlgorithm,FFlattenedOriginal,FOutputFilename,ImageFormat)
      else
        FFlattenedOriginal.SaveToFileUTF8(FOutputFilename);
      if FLazPaintInstance.Image.NbLayers = 1 then FLazPaintInstance.Image.SetSavedFlag;
    end;
  end;

  procedure SaveBmp;
  var writer: TFPWriterBMP;
    dithered: TBGRACustomBitmap;
    outputStream: TFileStreamUTF8;
  begin
    MakeBmpStreamIfNeeded;
    if Assigned(FBmpStream) then
    begin
      outputStream := TFileStreamUTF8.Create(FOutputFilename,fmCreate);
      try
        FBmpStream.Position := 0;
        outputStream.CopyFrom(FBmpStream, FBmpStream.Size);
        if FLazPaintInstance.Image.NbLayers = 1 then FLazPaintInstance.Image.SetSavedFlag;
      finally
        outputStream.Free;
      end;
    end else
    if QuantizerNeeded then
    begin
      dithered := Quantizer.GetDitheredBitmap(GetDitheringAlgorithm, FFlattenedOriginal);
      Quantizer.ReducedPalette.AssignTo(dithered);
      writer := TFPWriterBMP.Create;
      writer.BitsPerPixel := WantedBitsPerPixel;
      try
        dithered.SaveToFileUTF8(FOutputFilename, writer);
        if FLazPaintInstance.Image.NbLayers = 1 then FLazPaintInstance.Image.SetSavedFlag;
      finally
        writer.Free;
        dithered.Free;
      end;
    end else
    begin
      FFlattenedOriginal.SaveToFileUTF8(FOutputFilename);
      if FLazPaintInstance.Image.NbLayers = 1 then FLazPaintInstance.Image.SetSavedFlag;
    end;
  end;

  procedure SaveJpeg;
  var outputStream: TFileStreamUTF8;
  begin
    RequireJpegStream;
    outputStream := TFileStreamUTF8.Create(FOutputFilename,fmCreate);
    try
      FJpegStream.Position := 0;
      outputStream.CopyFrom(FJpegStream, FJpegStream.Size);
      FLazPaintInstance.Config.SetDefaultJpegQuality(JpegQuality);
      if FLazPaintInstance.Image.NbLayers = 1 then FLazPaintInstance.Image.SetSavedFlag;
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
  result := (FImageFormat in [ifPng,ifBmp]) and (WantedBitsPerPixel <= 8);
end;

function TFSaveOption.GetBitsPerPixelVisible: boolean;
begin
  result := FBitsPerPixelVisible;
end;

function TFSaveOption.GetColorQuantizer: TBGRAColorQuantizer;
begin
  if not Assigned(FQuantizer) then
    FQuantizer := TBGRAColorQuantizer.Create(FFlattenedOriginal,acFullChannelInPalette);
  result := FQuantizer;
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
  else
    result := 24;
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
      size := int64((FFlattenedOriginal.Width*WantedBitsPerPixel+7) div 8)*FFlattenedOriginal.Height;
      if QuantizerNeeded then size += int64(1 shl WantedBitsPerPixel)*4;
      size += sizeof(TBitMapFileHeader)+sizeof(TBitMapInfoHeader);
      UpdateFileSizeTo(size);
    end;
  ifJpeg: if FJpegStream = nil then
             UpdateFileSizeTo(-1)
          else
             UpdateFileSizeTo(FJpegStream.Size);
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
end;

procedure TFSaveOption.SetBitsPerPixelVisible(AValue: boolean);
begin
  if FBitsPerPixelVisible = AValue then exit;
  FBitsPerPixelVisible := AValue;
  Panel_BitsPerPixel.Visible := FBitsPerPixelVisible;
end;

procedure TFSaveOption.SetImageFormat(AValue: TBGRAImageFormat);
begin
  if FImageFormat=AValue then Exit;
  FImageFormat:=AValue;
  QualityVisible := FImageFormat = ifJpeg;
  BitsPerPixelVisible := FImageFormat in[ifPng,ifBmp];
  if FInit then exit;
  FInit := true;
  if FImageFormat = ifPng then
  begin
    RadioButton_2Colors.Enabled := false;
    RadioButton_16Colors.Enabled := false;
    RadioButton_256Colors.Enabled := true;
    RadioButton_MioMap.Enabled := false;
    if GetOriginalBitDepth > 8 then
      RadioButton_24BitsPerPixel.Checked := true
    else
      RadioButton_256Colors.Checked := true;
  end else
  if FImageFormat = ifBmp then
  begin
    if FFlattenedOriginal.HasTransparentPixels then
    begin
      RadioButton_2Colors.Enabled := false;
      RadioButton_16Colors.Enabled := false;
      RadioButton_256Colors.Enabled := false;
      RadioButton_24BitsPerPixel.Checked := true;
    end else
    begin
      RadioButton_2Colors.Enabled := true;
      RadioButton_16Colors.Enabled := true;
      RadioButton_256Colors.Enabled := true;
      if GetOriginalBitDepth > 8 then
        RadioButton_24BitsPerPixel.Checked := true else
      if GetOriginalBitDepth > 4 then
        RadioButton_256Colors.Checked := true else
      if GetOriginalBitDepth > 1 then
        RadioButton_16Colors.Checked := true else
        RadioButton_2Colors.Checked := true;
    end;
    RadioButton_MioMap.Enabled := true;
  end;
  UpdateDitheringCheckbox;
  UpdateFileSize;
  FInit := false;
end;

procedure TFSaveOption.SetLazPaintInstance(AValue: TLazPaintCustomInstance);
begin
  if FLazPaintInstance=AValue then Exit;
  FLazPaintInstance:=AValue;
  FreeAndNil(FFlattenedOriginal);
  FreeAndNil(FQuantizer);
  FreeAndNil(FJpegPreview);
  JpegQuality := FLazPaintInstance.Config.DefaultJpegQuality;
  FFlattenedOriginal := FLazPaintInstance.Image.RenderedImage.Duplicate as TBGRABitmap;
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
  If QuantizerNeeded then
    Quantizer.ReductionColorCount := 1 shl WantedBitsPerPixel;
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
  CheckBox_Dithering.Enabled := not RadioButton_24BitsPerPixel.Checked;
end;

function TFSaveOption.GetOriginalBitDepth: integer;
begin
  result := BGRARequiredBitDepth(FFlattenedOriginal, acFullChannelInPalette);
end;

procedure TFSaveOption.DoUpdateBitmap;
begin
  UpdateFileSize;
  DrawHourglass(BGRAPreview.Bitmap);
  BGRAPreview.Repaint;
  BGRAPreview.DiscardBitmap;
end;

procedure TFSaveOption.JpegQualityChanged;
begin
  FreeAndNil(FJpegPreview);
  FreeAndNil(FJpegStream);
  UpdateFileSize;
  NeedBitmapUpdate(False);
end;

initialization
  {$I usaveoption.lrs}

end.

