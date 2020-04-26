unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  BCPanel, BCLabel, BCComboBox, fphttpclient, fpjson, uCloneControls, LCLIntF;

resourcestring
  rsCantLoadJSON = 'Can''''t load JSON from server.';

type

  { TfrmMain }

  TfrmMain = class(TForm)
    bccCategory: TBCComboBox;
    bclPackageAuthor: TBCLabel;
    bclPackageAuthorText: TBCLabel;
    bclPackageDescription: TBCLabel;
    bclPackageDescriptionText: TBCLabel;
    bclPackageLicense: TBCLabel;
    bclPackageLicenseText: TBCLabel;
    bclPackageName: TBCLabel;
    bclPackageNameText: TBCLabel;
    bclPackageVersion: TBCLabel;
    bclPackageVersionText: TBCLabel;
    bclTitle: TBCLabel;
    bclPackageTitle: TBCLabel;
    bclPackageCategory: TBCLabel;
    bclDownload: TBCLabel;
    bclHomePage: TBCLabel;
    bcpHeader: TBCPanel;
    bcpPackage: TBCPanel;
    bcpPackageLPK: TPanel;
    bcpSearch: TBCPanel;
    edtSearch: TEdit;
    bcpLinks: TPanel;
    sbMain: TScrollBox;
    tmrSearch: TTimer;
    procedure bccCategoryChange(Sender: TObject);
    procedure bclLinkMouseEnter(Sender: TObject);
    procedure bclLinkMouseLeave(Sender: TObject);
    procedure bclLinkClick(Sender: TObject);
    procedure bcpPackageLPKResize(Sender: TObject);
    procedure edtSearchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tmrSearchTimer(Sender: TObject);
  private
    JSON: TJSONObject;
    procedure LoadJSON;
    procedure DisplayJSON;
    procedure HideAll;
    procedure Search;
    procedure ListCategories;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.bclLinkMouseEnter(Sender: TObject);
begin
  TBCLabel(Sender).FontEx.Style := [fsUnderline];
end;

procedure TfrmMain.bccCategoryChange(Sender: TObject);
begin
  Search;
end;

procedure TfrmMain.bclLinkMouseLeave(Sender: TObject);
begin
  TBCLabel(Sender).FontEx.Style := [];
end;

procedure TfrmMain.bclLinkClick(Sender: TObject);
begin
  OpenURL(TControl(Sender).Hint);
end;

procedure TfrmMain.bcpPackageLPKResize(Sender: TObject);
var
  i: integer;
begin
  for i:=0 to TWinControl(Sender).ControlCount-1 do
    with TBCLabel(TWinControl(Sender).Controls[i]) do
      Constraints.MaxWidth := TWinControl(Sender).Width - Left;
end;

procedure TfrmMain.edtSearchChange(Sender: TObject);
begin
  tmrSearch.Enabled := True;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  LoadJSON;
  ListCategories;
  DisplayJSON;
  Search;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  if Assigned(JSON) then
    JSON.Free;
end;

procedure TfrmMain.tmrSearchTimer(Sender: TObject);
begin
  tmrSearch.Enabled := False;
  Search;
end;

procedure TfrmMain.LoadJSON;
var
  data: string;
begin
  try
    data := TFPHTTPClient.SimpleGet('http://packages.lazarus-ide.org/packagelist.json');
    JSON := TJSONObject(GetJSON(data));
  except
    on e: Exception do
    begin
      ShowMessage(rsCantLoadJSON + LineEnding + e.Message);
      Application.Terminate;
    end;
  end;
end;

procedure TfrmMain.DisplayJSON;
var
  i, j: integer;
  data, fileData: TJSONObject;
  files: TJSONArray;
  ctrl, bcp, ctrlLPK: TControl;
  bcl: TBCLabel;
begin
  i := 0;
  while (JSON.FindPath('PackageData' + IntToStr(i)) <> nil) do
  begin
    data := TJSONObject(JSON.FindPath('PackageData' + IntToStr(i)));
    files := TJSONArray(JSON.FindPath('PackageFiles' + IntToStr(i)));
    CloneControls(bcpPackage, sbMain, i);
    ctrl := sbMain.Controls[i];
    if Assigned(ctrl) then
    begin
      ctrl.Caption := '';

      bcl := TBCLabel(ctrl.FindComponent('CloneOf_bclPackageTitle0'));
      bcl.Caption := data.FindPath('DisplayName').AsString;

      bcl := TBCLabel(ctrl.FindComponent('CloneOf_bclPackageCategory1'));
      bcl.Caption := data.FindPath('Category').AsString;

      bcp := TControl(ctrl.FindComponent('CloneOf_bcpLinks2'));
      bcp.Caption := '';

      bcl := TBCLabel(bcp.FindComponent('CloneOf_bclDownload0'));
      bcl.Hint := 'http://packages.lazarus-ide.org/' + data.findPath('RepositoryFileName').AsString;

      bcl := TBCLabel(bcp.FindComponent('CloneOf_bclHomePage1'));
      bcl.Hint := data.FindPath('HomePageURL').AsString;
      bcl.Visible := bcl.Hint <> '';

      for j := 0 to files.Count-1 do
      begin
        fileData := TJSONObject(files.Objects[j]);
        CloneControls(bcpPackageLPK, TWinControl(ctrl), j);

        ctrlLPK := TControl(ctrl.FindComponent('CloneOf_bcpPackageLPK' + IntToStr(j)));
        ctrlLPK.Caption := '';
        ctrlLPK.Visible := True;

        bcl := TBCLabel(ctrlLPK.FindComponent('CloneOf_bclPackageNameText1'));
        bcl.Caption := fileData.FindPath('Name').AsString;
        bcl.Hint := bcl.Caption;

        bcl := TBCLabel(ctrlLPK.FindComponent('CloneOf_bclPackageAuthorText3'));
        bcl.Caption := fileData.FindPath('Author').AsString;
        bcl.Hint := bcl.Caption;

        bcl := TBCLabel(ctrlLPK.FindComponent('CloneOf_bclPackageDescriptionText5'));
        bcl.Caption := fileData.FindPath('Description').AsString;
        bcl.Hint := bcl.Caption;

        bcl := TBCLabel(ctrlLPK.FindComponent('CloneOf_bclPackageLicenseText7'));
        bcl.Caption := fileData.FindPath('License').AsString;
        bcl.Hint := bcl.Caption;

        bcl := TBCLabel(ctrlLPK.FindComponent('CloneOf_bclPackageVersionText9'));
        bcl.Caption := fileData.FindPath('VersionAsString').AsString;
        bcl.Hint := bcl.Caption;
      end;
    end;
    Inc(i);
  end;
end;

procedure TfrmMain.HideAll;
var
  i: integer;
begin
  for i:=0 to sbMain.ControlCount-1 do
    sbMain.Controls[i].Visible := False;
end;

procedure TfrmMain.Search;

function FindText(source: String): boolean;
begin
  if (edtSearch.Text = '') then
    Result := True
  else
    Result := POS(lowercase(edtSearch.Text), lowercase(source)) > 0;
end;

function FindCategory(categories: string): boolean;
begin
  if (bccCategory.Text = '*') then
    Result := True
  else
    Result := POS(lowercase(bccCategory.Text), lowercase(categories)) > 0;;
end;

var
  i: integer;
  count: integer;
begin
  HideAll;
  sbMain.Visible := False;
  count := 0;
  for i:=0 to sbMain.ControlCount-1 do
  begin
    sbMain.Controls[i].Visible :=
    FindText(TControl(sbMain.Controls[i].FindComponent('CloneOf_bclPackageTitle0')).Caption) and
    FindCategory(TControl(sbMain.Controls[i].FindComponent('CloneOf_bclPackageCategory1')).Caption);
    if sbMain.Controls[i].Visible then
      Inc(count);
    // ToDo: scrollbox limit
    if count >= 10 then
      break;
  end;
  sbMain.Visible := True;
end;

procedure TfrmMain.ListCategories;
var
  i,j: integer;
  categ: string;
  categories: TStringArray;
  data: TJSONObject;
begin
  i := 0;
  bccCategory.Items.Add('*');
  while (JSON.FindPath('PackageData' + IntToStr(i)) <> nil) do
  begin
    data := TJSONObject(JSON.FindPath('PackageData' + IntToStr(i)));
    categ := data.FindPath('Category').AsString;
    categories := categ.Split(',');

    for j:=0 to Length(categories)-1 do
    begin
      categ := trim(categories[j]);
      if bccCategory.Items.IndexOf(categ) = -1 then
        bccCategory.Items.Add(categ);
    end;
    Inc(i);
  end;
  bccCategory.ListBox.Sorted := True;
  bccCategory.ItemIndex := 0;
end;

end.

