unit DORAppExpt;

interface
uses
  Classes, SysUtils, Windows, ToolsApi, DCCStrs, superobject;

const
  sCategoryDORNew = 'Borland.Delphi.DOR.New';

type
  TDORAppProjectCreator = class(TInterfacedObject, IOTACreator, IOTAProjectCreator, IOTAProjectCreator50)
  private
    FServicename: string;
    FDisplayname: string;
    FPort: string;
    FAutoJson: Boolean;
  public
    constructor Create(const Servicename, Displayname, Port: string; AutoJson: Boolean); virtual;
    { IOTACreator }
    function  GetCreatorType: String; virtual;
    function  GetExisting: Boolean; virtual;
    function  GetFileSystem: String; virtual;
    function  GetOwner: IOTAModule; virtual;
    function  GetUnnamed: Boolean; virtual;
    { IOTAProjectCreator }
    function  GetFileName: String; virtual;
    function  GetOptionFileName: String; virtual;
    function  GetShowSource: Boolean; virtual;
    procedure NewDefaultModule; virtual;
    function  NewOptionSource(const ProjectName: String): IOTAFile; virtual;
    procedure NewProjectResource(const Project: IOTAProject); virtual;
    function  NewProjectSource(const ProjectName: String): IOTAFile; virtual;
    { IOTAProjectCreator50 }
    procedure NewDefaultProjectModule(const Project: IOTAProject); virtual;
  end;

  TDORAppModuleCreator = class(TInterfacedObject, IOTACreator, IOTAModuleCreator)
  private
    FPort: string;
    FAutoJson: Boolean;
  public
    constructor Create(const Port: string; AutoJson: Boolean); virtual;
    function GetAncestorName: string;
    function GetFormName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent: string;
      const AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent: string; const FormIdent: string;
      const AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent: string; const FormIdent: string;
      const AncestorIdent: string): IOTAFile;
    function GetCreatorType: string;
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

  TDORAppProjectWizard = class(TNotifierObject, IOTAWizard, IOTARepositoryWizard,
    IOTARepositoryWizard60, IOTARepositoryWizard80, IOTAProjectWizard)
  public
    function GetDesigner: string;
    function GetPersonality: string;
    function GetGalleryCategory: IOTAGalleryCategory;
    { IOTAWizard declarations }
    function  GetIDString: String;
    function  GetName: String;
    function  GetState: TWizardState;
    procedure Execute;
    { IOTARepositoryWizard declarations }
    function  GetAuthor: String;
    function  GetComment: String;
    function  GetPage: String;
    function  GetGlyph: Cardinal;
  end;

  TDORProjectSource = class(TInterfacedObject, IOTAFile)
  private
    FProjectName: string;
    FServiceName: string;
    FDisplayName: string;
  public
    constructor Create(const ProjectName, ServiceName, DisplayName: string);
    function GetAge: TDateTime;
    function GetSource: string;
  end;

  TDORModuleSource = class(TInterfacedObject, IOTAFile)
  private
    FModuleIdent: string;
    FPort: string;
    FAutoJson: Boolean;
  public
    constructor Create(const ModuleIdent, Port: string; AutoJson: Boolean);
    function GetAge: TDateTime;
    function GetSource: string;
  end;

procedure Register;

implementation
uses Controls, ProjectOptions;

function FindSource(const res: string): string;
var
  stream: TResourceStream;
  list: TStringList;
begin
  stream := TResourceStream.Create(HInstance, res, RT_RCDATA);
  list := TStringList.Create;
  try
    list.LoadFromStream(stream);
    Result := list.Text;
  finally
    stream.Free;
    list.Free;
  end;
end;

procedure InitModule;
begin
  with (BorlandIDEServices as IOTAGalleryCategoryManager) do
    AddCategory(FindCategory(sCategoryDelphiNew), sCategoryDORNew, 'Delphi on Rails', 0);
end;

procedure DoneModule;
begin
  with (BorlandIDEServices as IOTAGalleryCategoryManager) do
    DeleteCategory(FindCategory(sCategoryDORNew));
end;

{ TDORAppProjectCreator }

constructor TDORAppProjectCreator.Create(const Servicename, Displayname, Port: string;
  AutoJson: Boolean);
begin
  FServicename := Servicename;
  FDisplayname := Displayname;
  FPort        := Port;
  FAutoJson    := AutoJson;
end;

function TDORAppProjectCreator.GetCreatorType: string;
begin
  Result := sConsole;
end;

function TDORAppProjectCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TDORAppProjectCreator.GetFileName: string;
begin
  Result := '';
end;

function TDORAppProjectCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TDORAppProjectCreator.GetOptionFileName: string;
begin
  Result := '';
end;

function TDORAppProjectCreator.GetOwner: IOTAModule;
begin
  Result := nil;
end;

function TDORAppProjectCreator.GetShowSource: Boolean;
begin
  Result := False;
end;

function TDORAppProjectCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

procedure TDORAppProjectCreator.NewDefaultModule;
begin

end;

procedure TDORAppProjectCreator.NewDefaultProjectModule(const Project: IOTAProject);
begin
  with (BorlandIDEServices as IOTAModuleServices) do
    CreateModule(TDORAppModuleCreator.Create(FPort, FAutoJson));
end;

function TDORAppProjectCreator.NewOptionSource(const ProjectName: string): IOTAFile;
begin
  Result := nil;
end;

procedure TDORAppProjectCreator.NewProjectResource(const Project: IOTAProject);
begin
  with (Project.ProjectOptions as IOTAProjectOptionsConfigurations) do
    Configurations[1].InsertValues(sDefine, ['CONSOLEAPP']);
end;

function TDORAppProjectCreator.NewProjectSource(const ProjectName: string): IOTAFile;
begin
  Result := TDORProjectSource.Create(ProjectName, FServicename, FDisplayname);
end;

{ TDORAppProjectWizard }

function TDORAppProjectWizard.GetAuthor: String;
begin
  Result := 'Henri Gourvest';
end;

function TDORAppProjectWizard.GetDesigner: string;
begin
  Result := ToolsApi.dVCL;
end;

function TDORAppProjectWizard.GetGalleryCategory: IOTAGalleryCategory;
begin
  with (BorlandIDEServices as IOTAGalleryCategoryManager) do
    Result := FindCategory(sCategoryDORNew);
end;

function TDORAppProjectWizard.GetGlyph: Cardinal;
begin
  Result := 0;
end;

function TDORAppProjectWizard.GetPage: String;
begin
  Result := 'New';
end;

function TDORAppProjectWizard.GetPersonality: string;
begin
  Result := ToolsApi.sDelphiPersonality;
end;

function TDORAppProjectWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

procedure TDORAppProjectWizard.Execute;
begin
  with TProjOptDlg.Create(nil) do
  begin
    if (ShowModal = mrok) then
      with (BorlandIDEServices as IOTAModuleServices) do
         CreateModule(TDORAppProjectCreator.Create(
           edServiceName.Text,
           edDisplayName.Text,
           edPort.Text,
           chkAutoJson.Checked
           ));
    Free;
  end;
end;

function TDORAppProjectWizard.GetComment: String;
begin
  Result := 'Creates a new DOR Server';
end;

function TDORAppProjectWizard.GetIDString: String;
begin
  Result := 'CodeGear.DORProject';
end;

function TDORAppProjectWizard.GetName: String;
begin
  Result := 'DOR Server';
end;

procedure Register;
begin
  RegisterPackageWizard(TDORAppProjectWizard.Create);
end;

{ TDORProjectSource }

constructor TDORProjectSource.Create(const ProjectName, ServiceName, DisplayName: string);
begin
  FProjectName := ProjectName;
  FDisplayName := DisplayName;
  FServiceName := ServiceName;
end;

function TDORProjectSource.GetAge: TDateTime;
begin
  Result := -1;
end;

function TDORProjectSource.GetSource: string;
begin
    Result :=
      SO(['program',     FProjectName,
          'servicename', FServiceName,
          'displayname', FDisplayName]).Format(FindSource('DORDPR'));
end;

{ TDORAppModuleCreator }

constructor TDORAppModuleCreator.Create(const Port: string; AutoJson: Boolean);
begin
  FPort :=  Port;
  FAutoJson := AutoJson;
end;

procedure TDORAppModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin

end;

function TDORAppModuleCreator.GetAncestorName: string;
begin
  Result := 'Form';
end;

function TDORAppModuleCreator.GetCreatorType: string;
begin
  Result := sForm;
end;

function TDORAppModuleCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TDORAppModuleCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TDORAppModuleCreator.GetFormName: string;
begin
  Result := '';
end;

function TDORAppModuleCreator.GetImplFileName: string;
begin
  Result := '';
end;

function TDORAppModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TDORAppModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TDORAppModuleCreator.GetOwner: IOTAModule;
begin
  Result := GetActiveProject;
end;

function TDORAppModuleCreator.GetShowForm: Boolean;
begin
  Result := False;
end;

function TDORAppModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TDORAppModuleCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

function TDORAppModuleCreator.NewFormFile(const FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TDORAppModuleCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := TDORModuleSource.Create(ModuleIdent, FPort, FAutoJson);
end;

function TDORAppModuleCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

{ TDORModuleSource }

constructor TDORModuleSource.Create(const ModuleIdent, Port: string;
  AutoJson: Boolean);
begin
  FModuleIdent := ModuleIdent;
  FAutoJson := AutoJson;
  FPort := Port;
end;

function TDORModuleSource.GetAge: TDateTime;
begin
  Result := -1
end;

function TDORModuleSource.GetSource: string;
var
  pass: AnsiString;
  c: AnsiChar;
  i: Integer;
  res: string;
begin
  pass := '';
  for i := 1 to 60 do
  begin
    c := AnsiChar(Random(93) + 33);
    pass := pass + c;
    if Ord(c) = 39 then
       pass := pass + AnsiChar(39);
  end;

  if FAutoJson then
    res := 'DORSRVJSON' else
    res := 'DORSRV';

  Result := SO([
    'unit', FModuleIdent,
    'pass', pass,
    'port', FPort]).Format(FindSource(res))
end;

initialization
  InitModule;
finalization
  DoneModule;
end.
