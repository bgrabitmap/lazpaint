// SPDX-License-Identifier: GPL-3.0-only
unit UTextInput;

{$mode objfpc}{$H+}

{ Input method support (Fcitx, IBus...) for the text tool.

  The text tool draws on a canvas: there is no native text widget to receive
  composed text. This unit hides the widgetset-specific part. The main form
  only calls UpdateTextInput, and receives the committed text through
  OnTextInput. On widgetsets without support, nothing is done. }

interface

uses
  Classes, SysUtils, Controls, LCLType, LazUTF8;

type
  TTextInputEvent = procedure(Sender: TObject; var UTF8Key: TUTF8Char) of object;

{ Call regularly (e.g. on idle). AWanted tells whether text is currently being
  edited and the input method should be active for AForm. AOnTextInput receives
  the committed text, one UTF8 character at a time. }
procedure UpdateTextInput(AForm: TWinControl; AWanted: boolean; AOnTextInput: TTextInputEvent);

implementation

{$IFDEF LCLGTK2}
uses Gtk2, Gdk2, Glib2, Gtk2Def, Gtk2Proc;

var
  GtkIMContext: PGtkIMContext = nil;
  GtkIMWidget: PGtkWidget = nil;
  GtkIMForm: TWinControl = nil;
  GtkIMOnTextInput: TTextInputEvent = nil;
  GtkIMWanted: boolean = false;
  GtkIMFocused: boolean = false;
  GtkIMSnooperId: guint = 0;

{ The LCL GTK2 input method context is reset whenever a key is handled, which
  cancels any composition. So use our own input method context, fed by a key
  snooper that sees the keys before the LCL does. }

procedure GtkIMCommit(context: PGtkIMContext; str: Pgchar; data: gpointer); cdecl;
var s: TUTF8Char;
    p: PChar;
    n: integer;
begin
  if (GtkIMForm = nil) or (str = nil) or not Assigned(GtkIMOnTextInput) then exit;
  // TUTF8Char only holds 7 bytes: send the committed text character by character
  p := str;
  while p^ <> #0 do
  begin
    n := UTF8CodepointSize(p);
    if n < 1 then n := 1;
    SetLength(s, n);
    Move(p^, s[1], n);
    GtkIMOnTextInput(GtkIMForm, s);
    inc(p, n);
  end;
end;

function GtkIMKeySnooper(grab_widget: PGtkWidget; event: PGdkEventKey; func_data: gpointer): gint; cdecl;
begin
  Result := 0;
  if (GtkIMContext = nil) or not GtkIMWanted or not GtkIMFocused or (event = nil) then exit;
  if (event^._type <> GDK_KEY_PRESS) and (event^._type <> GDK_KEY_RELEASE) then exit;
  if (grab_widget = nil) or (gtk_widget_get_toplevel(grab_widget) <> GtkIMWidget) then exit;
  if gtk_im_context_filter_keypress(GtkIMContext, event) then Result := 1;
end;

procedure UpdateTextInput(AForm: TWinControl; AWanted: boolean; AOnTextInput: TTextInputEvent);
var w: PGtkWidget;
    rect: TGdkRectangle;
    p: TPoint;
begin
  if not AForm.HandleAllocated then exit;
  w := PGtkWidget(AForm.Handle);
  if GtkIMContext = nil then
  begin
    GtkIMContext := gtk_im_multicontext_new;
    g_signal_connect(GtkIMContext, 'commit', TGCallback(@GtkIMCommit), nil);
    GtkIMSnooperId := gtk_key_snooper_install(@GtkIMKeySnooper, nil);
  end;
  GtkIMForm := AForm;
  GtkIMOnTextInput := AOnTextInput;
  GtkIMWanted := AWanted;
  if AWanted then
  begin
    if GtkIMWidget <> w then
    begin
      if GtkIMFocused then gtk_im_context_focus_out(GtkIMContext);
      GtkIMFocused := false;
      GtkIMWidget := w;
      gtk_im_context_set_client_window(GtkIMContext, GetControlWindow(w));
    end;
    if not GtkIMFocused then
    begin
      gtk_im_context_focus_in(GtkIMContext);
      GtkIMFocused := true;
    end;
    // approximate caret position: the mouse pointer, in form coordinates
    p := AForm.ScreenToClient(Mouse.CursorPos);
    rect.x := p.x; rect.y := p.y; rect.width := 1; rect.height := 16;
    gtk_im_context_set_cursor_location(GtkIMContext, @rect);
  end else
  if GtkIMFocused then
  begin
    gtk_im_context_focus_out(GtkIMContext);
    gtk_im_context_reset(GtkIMContext);
    GtkIMFocused := false;
  end;
end;

{$ELSE}

procedure UpdateTextInput(AForm: TWinControl; AWanted: boolean; AOnTextInput: TTextInputEvent);
begin
  // no input method handling needed on this widgetset
end;
{$ENDIF}

end.
