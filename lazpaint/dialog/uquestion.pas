// SPDX-License-Identifier: GPL-3.0-only
unit UQuestion;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, LazPaintType;

type
  TQuestionResult = record
    ButtonResult: TModalResult;
    Remember: boolean;
  end;


function QuestionResult(AValue: TModalResult): TQuestionResult;

type
  { TFQuestion }

  TFQuestion = class(TForm)
    CheckBox_RememberChoice: TCheckBox;
    Label_Message: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FButtons: array of record
                ButtonResult: TModalResult;
                Button: TButton;
              end;
  public
    procedure Init(ATitle, AQuestion: string; AButtons: TMsgDlgButtons; AShowRemember: boolean);
    procedure ClearButtons;
  end;

var
  FQuestion: TFQuestion;

function ShowQuestionDialog(ATitle, AQuestion: string; AButtons: TMsgDlgButtons; AShowRemember: boolean): TQuestionResult;

implementation

uses LCScaleDPI, UResourceStrings;

function QuestionResult(AValue: TModalResult): TQuestionResult;
begin
  result.ButtonResult := AValue;
  result.Remember := false;
end;

function ShowQuestionDialog(ATitle, AQuestion: string; AButtons: TMsgDlgButtons; AShowRemember: boolean): TQuestionResult;
var f: TFQuestion;
begin
  f := TFQuestion.Create(nil);
  f.Init(ATitle, AQuestion, AButtons, AShowRemember);
  result.ButtonResult:= f.ShowModal;
  result.Remember:= f.CheckBox_RememberChoice.Checked;
  f.Free;
end;

{ TFQuestion }

procedure TFQuestion.FormDestroy(Sender: TObject);
begin
  ClearButtons;
end;

procedure TFQuestion.FormCreate(Sender: TObject);
begin
  ScaleControl(self, OriginalDPI);
end;

procedure TFQuestion.Init(ATitle, AQuestion: string; AButtons: TMsgDlgButtons; AShowRemember: boolean);
const CenterButtons = {$IFDEF WINDOWS}true{$ELSE}false{$ENDIF};
  ButtonsFromRight = {$IFDEF WINDOWS}false{$ELSE}true{$ENDIF};
var
  x,y: integer;

  procedure AddButton(ACaption: string; AResult: TModalResult; ACancel, ADefault: boolean);
  begin
    setlength(FButtons, length(FButtons)+1);
    with FButtons[high(FButtons)] do
    begin
      ButtonResult := AResult;
      Button := TButton.Create(self);
      Button.Caption := ACaption;
      Button.ModalResult:= AResult;
      Button.Cancel := ACancel;
      Button.Default := ADefault;
      Button.Width := DoScaleX(120, OriginalDPI);
      self.InsertControl(Button);

      Button.Top := y;
      if ButtonsFromRight then
      begin
        x -= Button.Width;
        Button.Left := x;
        x -= DoScaleX(8, OriginalDPI);
      end
      else
      begin
        Button.Left := x;
        x += Button.Width + DoScaleX(8, OriginalDPI);
      end;
    end;
  end;

var
  margin,i: integer;

begin
  self.Caption := ATitle;
  Label_Message.Caption := AQuestion;
  y := Label_Message.Height + DoScaleY(8, OriginalDPI);

  CheckBox_RememberChoice.Top := y;
  CheckBox_RememberChoice.Visible := AShowRemember;
  if AShowRemember then y += CheckBox_RememberChoice.Height + DoScaleY(8, OriginalDPI);

  ClearButtons;
  if ButtonsFromRight then
    x := ClientWidth - DoScaleX(8, OriginalDPI)
  else
    x := DoScaleX(8, OriginalDPI);

  if mbOk in AButtons then AddButton(rsOkay, mrOk, false, true);
  if mbYes in AButtons then AddButton(rsYes, mrYes, false, not (mbOk in AButtons));
  if mbNo in AButtons then AddButton(rsNo, mrNo, not (mbCancel in AButtons), false);
  if mbCancel in AButtons then AddButton(rsCancel, mrCancel, true, false);

  if CenterButtons and not ButtonsFromRight then
  begin
    margin := (ClientWidth-x) div 2;
    for i := 0 to high(FButtons) do
      FButtons[i].Button.Left := FButtons[i].Button.Left + margin;
  end;

  if length(FButtons)>0 then
    y += FButtons[0].Button.Height + DoScaleY(8, OriginalDPI);
  ClientHeight := y;
end;

procedure TFQuestion.ClearButtons;
var
  i: Integer;
begin
  for i := low(FButtons) to high(FButtons) do
  begin
    self.RemoveControl(FButtons[i].Button);
    FButtons[i].Button.Free;
  end;
  FButtons := nil;
end;

{$R *.lfm}

end.

