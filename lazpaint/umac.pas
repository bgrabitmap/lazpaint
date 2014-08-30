unit UMac;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Menus, Controls, Spin, ActnList;


procedure CheckQuitMenu(MenuItem_Quit : TMenuItem;
                        MenuItem_QuitSeparator : TMenuItem);

procedure CheckOKCancelBtns(OKBtn     : TControl;
                            CancelBtn : TControl; MoveRight: boolean = true);

procedure CheckSpinEdit(SpinEdit: TSpinEdit);
procedure CheckFloatSpinEdit(SpinEdit: TFloatSpinEdit);

procedure CheckActions(List: TActionList);

implementation

uses LCLType;

{$hints off}
procedure CheckQuitMenu(MenuItem_Quit : TMenuItem;
                        MenuItem_QuitSeparator : TMenuItem);
begin
end;
{$hints on}

{$hints off}
procedure CheckOKCancelBtns(OKBtn     : TControl;
                            CancelBtn : TControl; MoveRight: boolean = true);
 {Swap OK and Cancel button positions on Mac.}
{$IFDEF DARWIN}
var
  SaveLeft, Margin : Integer;
{$ENDIF}
begin
  {$IFDEF DARWIN}
  if OKBtn.Left < CancelBtn.Left then
  begin
    if MoveRight then
    begin
      Margin := OKBtn.Parent.ClientHeight-OkBtn.Top-OkBtn.Height;
      OKBtn.Left := OkBtn.Parent.ClientWidth-Margin-OkBtn.Width;
      CancelBtn.Left := OkBtn.Left-Margin-CancelBtn.Width;
      OKBtn.Anchors := OKBtn.Anchors - [akLeft] + [akRight];
      CancelBtn.Anchors := CancelBtn.Anchors - [akLeft] + [akRight];
    end else
    begin
      SaveLeft := OKBtn.Left;
      OKBtn.Left := CancelBtn.Left;
      CancelBtn.Left := SaveLeft;
    end;
  end;
  {$ENDIF}
end;
{$hints on}

{$hints off}
procedure CheckSpinEdit(SpinEdit: TSpinEdit);
begin
   {$IFDEF DARWIN}
   SpinEdit.Left := SpinEdit.Left + 3;
   SpinEdit.Width := SpinEdit.Width - 4;
   {$ENDIF}
end;
{$hints on}

{$hints off}
procedure CheckFloatSpinEdit(SpinEdit: TFloatSpinEdit);
begin
   {$IFDEF DARWIN}
   SpinEdit.Left := SpinEdit.Left + 3;
   SpinEdit.Width := SpinEdit.Width - 4;
   {$ENDIF}
end;
{$hints on}

{$hints off}
procedure CheckActions(List: TActionList);
{$IFDEF DARWIN}
var i: integer;
    action: TAction;
    hasCmd: boolean;
begin
  for i := 0 to List.ActionCount-1 do
  begin
    action := List.Actions[i] as TAction;
    hasCmd := (action.ShortCut and scCtrl) <> 0;
    if hasCmd then
      action.ShortCut := (action.ShortCut and not scCtrl) or scMeta;
  end;
end;
{$ELSE}
begin
end;
{$ENDIF}
{$hints on}

end.

