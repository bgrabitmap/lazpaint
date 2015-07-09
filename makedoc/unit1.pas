unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    EPath: TEdit;
    Label1: TLabel;
    Memo1: TMemo;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

const
  BoldKeywords : array[1..52] of string = ('var','procedure','function','and',
    'or','xor','not','if','then','case','begin','end','of',
    'exit','new','class','is','const','div','do','downto','to','else','for',
    'in','mod','nil','object','record','repeat','self','shl','shr','string',
    'unit','until','uses','while','array','interface', 'out', 'constructor',
    'property','read','write','default', 'packed', 'operator', 'inline',
    'overload', 'virtual', 'abstract');

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if SelectDirectoryDialog1.Execute then
    EPath.Text := SelectDirectoryDialog1.InitialDir;
end;

procedure HighlightKeywords(var s: string);
const keywordChars = ['a'..'z','A'..'Z'];
  moreKeywordChars = ['a'..'z','A'..'Z','0'..'9','_'];
var
  i,start: Integer;
  w,wlower: string;
  j: Integer;
  found, first: boolean;
begin
  i := 1;
  first := true;
  while i <= length(s) do
  begin
    if s[i] in keywordChars then
      begin
        start := i;
        inc(i);
        while i <= length(s) do
        begin
          if not (s[i] in moreKeywordChars) then break;
          inc(i);
        end;
        w := copy(s,start,i-start);
        wlower := lowercase(w);
        found := false;
        for j := low(BoldKeywords) to high(BoldKeywords) do
          if BoldKeywords[j] = wlower then
            begin
              delete(s, start, length(w));
              dec(i, length(w));
              w := ''''''''+wlower+'''''''';
              insert(w, s, start);
              inc(i, length(w));
              found := true;
              break;
            end;
        if not found and first then
          begin
            delete(s, start, length(w));
            dec(i, length(w));
            w := ''''''+w+'''''';
            insert(w, s, start);
            inc(i, length(w));
          end;
        first := false;
      end else
        inc(i);
  end;
end;

function MakeDocFor(AFilename: string): string;
var
  t: textfile;
  fileoutput,s,bgcolor: String;
  description, element: String;
  comStart,comEnd, idxColor: integer;
  oddRow,indented : boolean;
  docName, colorStr: string;
  tableOpened: boolean;

  procedure openTable;
  begin
    if not tableOpened then
      begin
        fileoutput += '<table style="border-collapse: collapse;">'+lineending;
        oddRow := true;
        tableOpened:= true;
      end;
  end;

  procedure closeTable;
  begin
    if tableOpened then
    begin
      fileoutput += '</table>'+LineEnding;
      tableOpened:= false;
    end;
  end;

  procedure flushOutput;
  var u: textfile;
    path,fullname: string;
    outname,
    currentContent: string;
  begin
    if fileoutput <> '' then
      begin
        closeTable;
        path := ExtractFilePath(AFilename);
        CreateDirUTF8(path+'\doc');
        outname := 'doc\'+docName+'.txt';
        fullname := path+outname;
        fileoutput := '=== ' + docName + ' ===' + LineEnding
                    + fileoutput;
        if FileExistsUTF8(fullname) then
          begin
            currentContent := ReadFileToString(fullname);
            if currentContent <> fileoutput then
              begin
                assignfile(u, SysToUTF8(fullname));
                rewrite(u);
                write(u, fileoutput);
                closefile(u);
                result += outname + ' (updated)' + LineEnding;
              end;
          end else
          begin
            result += outname + ' (created)' + LineEnding;
            assignfile(u, SysToUTF8(fullname));
            rewrite(u);
            write(u, fileoutput);
            closefile(u);
          end;
        fileoutput:= '';
      end;
  end;

begin
  result := '';
  docName := ExtractFileName(AFilename);
  fileoutput := '';
  tableOpened:= false;
  assignfile(t, UTF8ToSys(AFilename));
  reset(t);
  while not eof(t) do
  begin
    readln(t,s);

    comStart:= pos('{====',s);
    if comStart <> 0 then
    begin
      comEnd:= pos('====}',s);
      if comEnd <> 0 then
      begin
        closeTable;
        fileOutput += trim(copy(s,comStart+1,comEnd+3 -(comStart+1)+1)) + LineEnding;
        continue;
      end;
    end;

    comStart:= pos('{===',s);
    if comStart <> 0 then
    begin
      comEnd:= pos('===}',s);
      if comEnd <> 0 then
      begin
        flushOutput;
        docName:= trim(copy(s,comStart+4,comEnd-1 -(comStart+4)+1));
        continue;
      end;
    end;

    comStart:= pos('{* ',s+' ');
    indented:= false;
    if comStart <> 0 then
      comStart += 2
    else
    begin
      comStart := pos('{** ',s+' ');
      if comStart <> 0 then
        comStart += 3;
      indented := true;
    end;
    if comStart<>0 then
      begin
        delete(s,1,comStart-1);
        comStart := 1;
        description := '';
        comEnd := pos('}',s);
        if comEnd = 0 then
          begin
            description += trim(copy(s,comStart,length(s)-comStart+1));
            while not eof(t) do
            begin
              readln(t,s);
              s := trim(s);
              if (length(s) > 0) and (s[1]='*') then
                begin
                  delete(s,1,1);
                  s := trim(s);
                  s := '<br/>'+s;
                end;
              comEnd := pos('}',s);
              if comEnd = 0 then
                description += ' '+s else
                begin
                  description += ' '+trim(copy(s,1,comEnd-1));
                  break;
                end;
            end;
          end
          else
            description += trim(copy(s,comStart,comEnd-comStart));

        while pos('[#',description) <> 0 do
        begin
          idxColor := pos('[#',description);
          colorStr := copy(description, idxColor, 9);
          if (length(colorStr) = 9) and (colorStr[9] = ']') then
            begin
              delete(description, idxColor, length(colorStr));
              insert('<span style="width:8px; height: 8px; display: inline-block; border: 1px solid black; background: '+copy(colorStr,2,7)+';"></span>', description, idxColor);
            end;
        end;

        if not eof(t) then
          readln(t,element) else element := '?';

        HighlightKeywords(element);
        element := trim(element);

        openTable;
        if oddRow then bgcolor := 'white' else bgcolor := '#f0f0ff';

        if indented then
        begin
          fileoutput += '<tr><td width="10%"></td><td colspan="2" style="background: '+bgcolor+';">'+element+'</td></tr>'+LineEnding;
          fileoutput += '<tr><td width="10%"></td><td width="10%" style="background: '+bgcolor+';"></td>'+
             '<td style="border: 1px solid #e0e0a0; background: #ffffe4;">'+description+'</td></tr>'+LineEnding;
        end else
        begin
          fileoutput += '<tr style="background: '+bgcolor+';"><td colspan="3">'+element+'</td></tr>'+LineEnding;
          fileoutput += '<tr style="background: '+bgcolor+';"><td width="10%"></td>'+
             '<td style="border: 1px solid #e0e0a0; background: #ffffe4;" colspan="2">'+description+'</td></tr>'+LineEnding;
        end;

        fileoutput += '<tr style="height: 8px;"><td colspan="3"></td></tr>'+LineEnding;
        oddRow := not oddRow;
      end;
  end;
  closefile(t);
  flushOutput;
end;

procedure TForm1.Button2Click(Sender: TObject);
var sr: TSearchRec;
  output,ext: string;
  path,fileoutput: string;
begin
  memo1.Text := 'Analyzing...';
  memo1.Update;
  path := AppendPathDelim(EPath.Text);
  if FindFirstUTF8(path+'*.*', faAnyFile, sr) = 0 then
    begin
      output := '';
      repeat
        if sr.Attr and (faDirectory or faVolumeId or faSymLink) <> 0 then continue;
        ext := AnsiLowerCase(ExtractFileExt(sr.Name));
        if (ext = '.pas') or (ext = '.inc') then
          begin
            fileoutput:= MakeDocFor(path+sr.Name);
            if fileoutput <> '' then
              begin
                output += fileoutput;
              end;
          end;
      until FindNextUTF8(sr) <> 0;
      FindCloseUTF8(sr);
      if output = '' then
        Memo1.Text := 'No change'
      else
        Memo1.text := output;
    end
  else
    Memo1.Text := 'Nothing to do';
end;

end.

