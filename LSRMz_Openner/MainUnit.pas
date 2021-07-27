unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls,
  JvComponentBase, JvZlibMultiple, StdCtrls, JvBaseDlg, JvSelectDirectory,
  JvBrowseFolder;

type
  TfrmMain = class(TForm)
    btnOpen: TButton;
    dlgOpen1: TOpenDialog;
    jvzlbmltpl1: TJvZlibMultiple;
    jvslctdrctry1: TJvSelectDirectory;
    chkUseRelativePath: TCheckBox;
    stat1: TStatusBar;
    jvbrwflddlg1: TJvBrowseForFolderDialog;
    procedure btnOpenClick(Sender: TObject);
    procedure jvzlbmltpl1DecompressedFile(Sender: TObject;
      const FileName: String; const FileSize: Cardinal);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.btnOpenClick(Sender: TObject);
var ExtractDir: string;
begin
  ExtractDir:='';
  stat1.Panels[0].Text:='';
  if (dlgOpen1.Execute) then
  begin
    if (not chkUseRelativePath.Checked) then
    begin
      jvbrwflddlg1.Directory:=ExtractFilePath(dlgOpen1.FileName);
      if (jvbrwflddlg1.Execute) then
        ExtractDir:=jvbrwflddlg1.Directory
      else
        Exit;
    end;
    jvzlbmltpl1.DecompressFile(dlgOpen1.FileName,ExtractDir,True,chkUseRelativePath.Checked);
  end;
end;

procedure TfrmMain.jvzlbmltpl1DecompressedFile(Sender: TObject;
  const FileName: String; const FileSize: Cardinal);
begin
  stat1.Panels[0].Text:='Done';
end;

end.
