unit USuperformula;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  LazPaintType, LCSuperformulaOriginal, UScripting, Spin, UStateType;

type
  TFSuperformula = class(TForm)
    Button_Cancel: TButton;
    Button_OK: TButton;
    ColorButton_BackColor: TColorButton;
    ColorButton_PenColor: TColorButton;
    CheckBox_MRational: TCheckBox;
    CheckBox_SpikeOverlap: TCheckBox;
    SpinEdit_Width: TFloatSpinEdit;
    SpinEdit_Size: TFloatSpinEdit;
    SpinEdit_A: TFloatSpinEdit;
    SpinEdit_B: TFloatSpinEdit;
    SpinEdit_M: TFloatSpinEdit;
    SpinEdit_N1: TFloatSpinEdit;
    SpinEdit_N2: TFloatSpinEdit;
    SpinEdit_N3: TFloatSpinEdit;
    Label_Rational: TLabel;
    Label_SpikeOverlap: TLabel;
    Label_BackColor: TLabel;
    Label_PenColor: TLabel;
    Label_Width: TLabel;
    Label_Size: TLabel;
    Label_A: TLabel;
    Label_B: TLabel;
    Label_SpikeCount: TLabel;
    Label_N1: TLabel;
    Label_N2: TLabel;
    Label_N3: TLabel;
    Panel_Buttons: TPanel;
    Panel_Parameters: TPanel;
    TimerDisplay: TTimer;
    procedure Button_CancelClick(Sender: TObject);
    procedure Button_OKClick(Sender: TObject);
    procedure CheckBox_MRationalChange(Sender: TObject);
    procedure CheckBox_SpikeOverlapChange(Sender: TObject);
    procedure ColorButton_BackColorColorChanged(Sender: TObject);
    procedure ColorButton_PenColorColorChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpinEdit_AChange(Sender: TObject);
    procedure SpinEdit_BChange(Sender: TObject);
    procedure SpinEdit_N1Change(Sender: TObject);
    procedure SpinEdit_N2Change(Sender: TObject);
    procedure SpinEdit_N3Change(Sender: TObject);
    procedure SpinEdit_SizeChange(Sender: TObject);
    procedure SpinEdit_MChange(Sender: TObject);
    procedure SpinEdit_WidthChange(Sender: TObject);
    procedure TimerDisplayTimer(Sender: TObject);
  private
    FComposeShape: TComposedImageDifference;
    FInitializing: boolean;
    FInstance: TLazPaintCustomInstance;
    FParameters: TVariableSet;
    FPreviewDone: boolean;
    FUpdateLayer: Boolean;
    function GetCorrespondingOriginal: TSuperformulaOriginal;
    procedure ExtractOriginalParameters(AOriginal: TSuperformulaOriginal; AParameters: TVariableSet);
    procedure NeedPreview;
    procedure UpdateOriginalParameters(AOriginal: TSuperformulaOriginal);

  public
    procedure DisplayShape;
    procedure InitParams;
  end;

function ShowSuperformulaDlg(AInstance: TLazPaintCustomInstance; AParameters: TVariableSet): TScriptResult;

implementation

{$R *.lfm}

uses UMac, LCScaleDPI, UResourceStrings, UImageAction, UImageDiff, BGRABitmapTypes, BGRATransform;

function ShowSuperformulaDlg(AInstance: TLazPaintCustomInstance;
  AParameters: TVariableSet): TScriptResult;
var
  FSuperformula: TFSuperformula;
  doFound, somethingDone: boolean;
begin
  FSuperformula:= TFSuperformula.create(nil);
  FSuperformula.FInstance := AInstance;
  FSuperformula.FParameters := AParameters;
  try
    TImageActions(AInstance.ImageAction).Deselect;

    if Assigned(AParameters) and
      AParameters.Booleans['Validate'] then
    begin
      AInstance.Image.DoBegin;
      FSuperformula.InitParams;
      FSuperformula.DisplayShape;
      AInstance.Image.DoEnd(doFound, somethingDone);
      result := srOk;
    end else
    begin
      if FSuperformula.showModal = mrOk then result := srOk
      else result := srCancelledByUser;
    end;
  finally
    FSuperformula.free;
  end;
end;

{ TFSuperformula }

procedure TFSuperformula.FormCreate(Sender: TObject);
begin
  ScaleControl(Self,OriginalDPI);

  Button_OK.Caption := rsOK;
  Button_Cancel.Caption := rsCancel;

  CheckOKCancelBtns(Button_OK{,Button_Cancel});
  CheckFloatSpinEdit(SpinEdit_Width);
  CheckFloatSpinEdit(SpinEdit_M);
  CheckFloatSpinEdit(SpinEdit_Size);
  CheckFloatSpinEdit(SpinEdit_N1);
  CheckFloatSpinEdit(SpinEdit_N2);
  CheckFloatSpinEdit(SpinEdit_N3);
  CheckFloatSpinEdit(SpinEdit_A);
  CheckFloatSpinEdit(SpinEdit_B);

  FPreviewDone := false;
end;

procedure TFSuperformula.FormHide(Sender: TObject);
begin
  if Assigned(FComposeShape) then
  begin
    If ModalResult <> mrOk then
       FInstance.Image.Undo;
    FComposeShape := nil;
  end;
end;

procedure TFSuperformula.FormShow(Sender: TObject);
begin
  InitParams;
  NeedPreview;
end;

procedure TFSuperformula.SpinEdit_AChange(Sender: TObject);
begin
  NeedPreview;
end;

procedure TFSuperformula.SpinEdit_BChange(Sender: TObject);
begin
  NeedPreview;
end;

procedure TFSuperformula.SpinEdit_N1Change(Sender: TObject);
begin
  NeedPreview;
end;

procedure TFSuperformula.SpinEdit_N2Change(Sender: TObject);
begin
  NeedPreview;
end;

procedure TFSuperformula.SpinEdit_N3Change(Sender: TObject);
begin
  NeedPreview;
end;

procedure TFSuperformula.SpinEdit_SizeChange(Sender: TObject);
begin
  NeedPreview;
end;

procedure TFSuperformula.SpinEdit_MChange(Sender: TObject);
begin
  NeedPreview;
end;

procedure TFSuperformula.SpinEdit_WidthChange(Sender: TObject);
begin
  NeedPreview;
end;

procedure TFSuperformula.TimerDisplayTimer(Sender: TObject);
begin
  TimerDisplay.Enabled:= false;
  DisplayShape;
end;

procedure TFSuperformula.InitParams;
var
  original: TSuperformulaOriginal;
  params: TVariableSet;
begin
  FInitializing := true;

  // default parameters
  if not Assigned(FParameters) then
  begin
    params := TVariableSet.Create('', FInstance.Config.DefaultSuperformulaParameters);
  end else
    params := FParameters.Duplicate;

  // parameters from current layer
  original := GetCorrespondingOriginal;
  if Assigned(original) then
  begin
    ExtractOriginalParameters(original, params);
    FUpdateLayer:= true;
  end
  else
    FUpdateLayer:= false;

  // apply parameters
  if params.IsDefined('PenColor') then
     ColorButton_PenColor.ButtonColor := params.Pixels['PenColor'].ToColor;
  if params.IsDefined('LineWidth') then
     SpinEdit_Width.Value := params.Floats['LineWidth'] * 10;
  if params.IsDefined('BackColor') then
     ColorButton_BackColor.ButtonColor := params.Pixels['BackColor'].ToColor;

  if params.IsDefined('M') then
     SpinEdit_M.Value := params.Floats['M'];
  if params.IsDefined('MRational') then
     CheckBox_MRational.Checked := params.Booleans['MRational'];
  if params.IsDefined('Size') then
     SpinEdit_Size.Value := params.Floats['Size'];

  if params.IsDefined('N1') then
     SpinEdit_N1.Value := params.Floats['N1'];
  if params.IsDefined('N2') then
     SpinEdit_N2.Value := params.Floats['N2'];
  if params.IsDefined('N3') then
     SpinEdit_N3.Value := params.Floats['N3'];

  if params.IsDefined('SpikeOverlap') then
     CheckBox_SpikeOverlap.Checked := params.Booleans['SpikeOverlap'];
  if params.IsDefined('A') then
     SpinEdit_A.Value := params.Floats['A'];
  if params.IsDefined('B') then
     SpinEdit_B.Value := params.Floats['B'];

  params.Free;
  FInitializing := false;
end;

procedure TFSuperformula.NeedPreview;
begin
  TimerDisplay.Enabled := false;
  TimerDisplay.Enabled := true;
  FPreviewDone := false;
  Button_OK.Enabled := false;
end;

procedure TFSuperformula.UpdateOriginalParameters(
  AOriginal: TSuperformulaOriginal);
begin
  AOriginal.PenColor := ColorToBGRA(ColorButton_PenColor.ButtonColor);
  AOriginal.LineWidth := SpinEdit_Width.Value / 10;
  AOriginal.BackColor := ColorToBGRA(ColorButton_BackColor.ButtonColor);

  AOriginal.N1 := SpinEdit_N1.Value;
  AOriginal.N2 := SpinEdit_N2.Value;
  AOriginal.N3 := SpinEdit_N3.Value;

  AOriginal.SpikeOverlap:= CheckBox_SpikeOverlap.Checked;
  AOriginal.A := SpinEdit_A.Value;
  AOriginal.B := SpinEdit_B.Value;

  AOriginal.M := SpinEdit_M.Value;
  AOriginal.mRational:= CheckBox_MRational.Checked;
  // size is the last value to set, because it is computed accoridng to other parameters
  AOriginal.Size:= SpinEdit_Size.Value;
end;

procedure TFSuperformula.DisplayShape;
var
  original: TSuperformulaOriginal;
  curIndex: Integer;
begin
  FPreviewDone := true;

  // cancel the preview
  if Assigned(FComposeShape) then
  begin
    FInstance.Image.Undo;
    FComposeShape := nil;
  end;

  // make a new preview
  FComposeShape := FInstance.Image.DoBegin;
  try
    // if there is already a corresponding layer
    if FUpdateLayer then
    begin
      original := GetCorrespondingOriginal;
      if Assigned(original) then
      begin
        // record changes made inside the original
        FInstance.Image.CurrentState.DiscardOriginalDiff:= false;
        UpdateOriginalParameters(original);
        FInstance.Image.CurrentState.DiscardOriginalDiff:= true;
      end;
    end else
    begin
      // create a new layer
      original := TSuperformulaOriginal.Create;
      UpdateOriginalParameters(original);

      // if current layer is empty, replace it
      if FInstance.Image.CurrentLayerEmpty then
      begin
        curIndex := FInstance.Image.CurrentLayerIndex;
        FInstance.Image.AddUndo(
          TReplaceLayerByCustomOriginalDifference.Create(FInstance.Image.CurrentState,
            FInstance.Image.CurrentLayerIndex, true, original));
        FInstance.Image.ImageMayChangeCompletely;
        FInstance.Image.LayerOriginalMatrix[curIndex] :=
          AffineMatrixTranslation(FInstance.Image.Width/2, FInstance.Image.Height/2);
        FInstance.Image.LayerName[curIndex] := Caption;
      end
      else
      begin
        // otherwise add the new layer
        FInstance.Image.AddNewLayer(original, Caption, boTransparent,
          AffineMatrixTranslation(FInstance.Image.Width/2, FInstance.Image.Height/2));
        curIndex := FInstance.Image.CurrentLayerIndex;
      end;
    end;

    Button_OK.Enabled := true;
  finally
    FInstance.Image.DoEnd(FComposeShape);
  end;
end;

function TFSuperformula.GetCorrespondingOriginal: TSuperformulaOriginal;
var
  curIndex: Integer;
begin
  curIndex := FInstance.Image.CurrentLayerIndex;
  if FInstance.Image.LayerOriginalClass[curIndex] = TSuperformulaOriginal then
    result := TSuperformulaOriginal(FInstance.Image.LayerOriginal[curIndex])
  else
    result := nil;
end;

procedure TFSuperformula.ExtractOriginalParameters(
  AOriginal: TSuperformulaOriginal; AParameters: TVariableSet);
begin
  AParameters.AddPixel('PenColor', AOriginal.PenColor);
  AParameters.AddFloat('LineWidth', AOriginal.LineWidth);
  AParameters.AddPixel('BackColor', AOriginal.BackColor);

  AParameters.AddFloat('M', AOriginal.m);
  AParameters.AddBoolean('MRational', AOriginal.mRational);
  AParameters.AddFloat('Size', AOriginal.Size);

  AParameters.AddFloat('N1', AOriginal.n1);
  AParameters.AddFloat('N2', AOriginal.n2);
  AParameters.AddFloat('N3', AOriginal.n3);

  AParameters.AddBoolean('SpikeOverlap', AOriginal.SpikeOverlap);
  AParameters.AddFloat('A', AOriginal.a);
  AParameters.AddFloat('B', AOriginal.b);
end;

procedure TFSuperformula.Button_OKClick(Sender: TObject);
var
  original: TSuperformulaOriginal;
  params: TVariableSet;
begin
  TimerDisplay.Enabled := false;
  if not FPreviewDone then DisplayShape;
  original := GetCorrespondingOriginal;
  if Assigned(original) then
  begin
    params := TVariableSet.Create('', '');
    try
      ExtractOriginalParameters(original, params);
      FInstance.Config.SetDefaultSuperformulaParameters(params.VariablesAsString);
    finally
      params.Free;
    end;
  end
end;

procedure TFSuperformula.CheckBox_MRationalChange(Sender: TObject);
begin
  NeedPreview;
end;

procedure TFSuperformula.CheckBox_SpikeOverlapChange(Sender: TObject);
begin
  NeedPreview;
end;

procedure TFSuperformula.ColorButton_BackColorColorChanged(Sender: TObject);
begin
  NeedPreview;
end;

procedure TFSuperformula.ColorButton_PenColorColorChanged(Sender: TObject);
begin
  NeedPreview;
end;

procedure TFSuperformula.Button_CancelClick(Sender: TObject);
begin
  TimerDisplay.Enabled := false;
  if Assigned(FComposeShape) then
  begin
    FInstance.Image.Undo;
    FComposeShape := nil;
  end;
end;

end.

