unit UTemplateGenerator;

interface

uses
  Classes;

const
  SPE_PATH = '[SPE_PATH]';
  SPE_NAME = '[SPE_NAME]';
  PARENT_DIR = '[PARENT_DIR]';

type
  TTemplGenerator = class
  private
    FSpectrNames: TStringList; // list of spectrum names for scenario generation
    FSpectrPathes: TStringList; // list of spectrum pathes for scenario generation
    FParentDirs: TStringList;  // list of parent directories
    FTemplate: TStringList; // template
    FScenarioText: TStringList;  // result scenario text
    FSpeDir: string;  // directory with spectra
    FMask: string;    // mask for spe file name
    //FConfigDir: string; // relative path from it
    //FScenarioFName: string;
    function getSpeCount(): Integer;
    procedure getSpeList(); // get spectrum list from disc
    procedure obtainSpeList(dirName: string);
    procedure obtainDirectoryList(dirName: string);
    procedure addSpe(aSpeFileName: string); // add record for finded spectrum
    procedure generateScenarioText(); // generate scenario
    procedure createScenarioSection(aSpeName, aSpePath, aParentDir: string); // create section in scenario text using template
  public
    constructor start(templFileName, aSpeDir, aMask: string);
    destructor Destroy(); override;
    procedure saveScenarioToFile(fileName: string);
    property ScenarioText: TStringList read FScenarioText;
    property spectrumCount: Integer read getSpeCount;
  end;

implementation

uses SysUtils, JvJCLUtils, gwFileUtils;

{ TTemplGenerator }

procedure TTemplGenerator.addSpe(aSpeFileName: string);
begin
  FSpectrPathes.Append(aSpeFileName);
  FSpectrNames.Append(gwFileUtils.filenameFromPath(aSpeFileName));
  FParentDirs.Append(gwFileUtils.getParentDir(aSpeFileName));
end;

procedure TTemplGenerator.createScenarioSection(aSpeName, aSpePath, aParentDir: string);
var curSection: TStringList;
    i: Integer;
begin
  curSection := TStringList.Create();
  curSection.AddStrings(FTemplate);
  // replace variables
  for i := 0 to curSection.Count-1 do begin
    if (Pos(SPE_NAME, curSection.Strings[i]) <> 0) then
      curSection.Strings[i] := StringReplace(curSection.Strings[i], SPE_NAME,
        aSpeName, [rfReplaceAll]);
    if (Pos(SPE_PATH, curSection.Strings[i]) <> 0) then
      curSection.Strings[i] := StringReplace(curSection.Strings[i], SPE_PATH,
        aSpePath, [rfReplaceAll]);
    if (Pos(PARENT_DIR, curSection.Strings[i]) <> 0) then
      curSection.Strings[i] := StringReplace(curSection.Strings[i], PARENT_DIR,
        aParentDir, [rfReplaceAll]);
  end;
  // add section to main scenario
  FScenarioText.AddStrings(curSection);
  FScenarioText.Add('');
  curSection.Free();
end;

procedure TTemplGenerator.generateScenarioText();
var i: Integer;
begin
  for i:=0 to FSpectrNames.Count-1 do begin
    // create section from template and add to scenario text
    createScenarioSection(FSpectrNames[i], FSpectrPathes[i], FParentDirs[i]);
  end;  
end;

procedure TTemplGenerator.getSpeList;
begin
  obtainSpeList(FSpeDir);
  obtainDirectoryList(FSpeDir);
end;

procedure TTemplGenerator.obtainSpeList(dirName: string);
var F: TSearchRec;
begin
  if (FindFirst(dirName + Fmask, faAnyFile, F) = 0) then
    if (F.Name <> '.') and (F.Name <> '..') then
      addSpe(dirName + F.Name);
      
  while (FindNext(F) = 0) do
    if (F.Name <> '.') and (F.Name <> '..') then
      addSpe(dirName + F.Name);

  FindClose(F);
end;

procedure TTemplGenerator.obtainDirectoryList(dirName: string);
var F: TSearchRec;
begin
  if (FindFirst(dirName + '*.*', faDirectory, F) = 0) then
    if (F.Name <> '.') and (F.Name <> '..') then begin
      if (F.Attr and faDirectory) = faDirectory then begin
        obtainSpeList(dirName + F.Name + '\');
        obtainDirectoryList(dirName + F.Name + '\');
      end;
    end;
      
  while (FindNext(F) = 0) do
    if (F.Name <> '.') and (F.Name <> '..') then begin
      if (F.Attr and faDirectory) = faDirectory then begin
        obtainSpeList(dirName + F.Name + '\');
        obtainDirectoryList(dirName + F.Name + '\');
      end;
    end;

  FindClose(F);
end;

constructor TTemplGenerator.start(templFileName, aSpeDir, aMask: string);
begin
  // set all fields
  FSpeDir := aSpeDir;
  //FConfigDir := aConfigDir;
  //FScenarioFName := aScenarioName;
  FTemplate := TStringList.Create();
  FTemplate.LoadFromFile(templFileName);
  FScenarioText := TStringList.Create();
  FSpectrNames := TStringList.Create();
  FSpectrPathes := TStringList.Create();
  FParentDirs := TStringList.Create();

  FMask := aMask;
  if (FMask = '') then
    FMask := '*.spe';
    
  //get spectrum list from disc
  getSpeList();
  // generate scenario
  generateScenarioText();
end;

destructor TTemplGenerator.Destroy;
begin
  FTemplate.Free();
  FScenarioText.Free();
  FSpectrNames.Free();
  FSpectrPathes.Free();
  FParentDirs.Free();
  inherited;
end;

procedure TTemplGenerator.saveScenarioToFile(fileName: string);
begin
  FScenarioText.SaveToFile(fileName);
end;

function TTemplGenerator.getSpeCount: Integer;
begin
  Result := FSpectrNames.Count; 
end;

end.
