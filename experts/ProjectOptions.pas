unit ProjectOptions;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TProjOptDlg = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    edServiceName: TEdit;
    edDisplayName: TEdit;
    edPort: TEdit;
    lbl1: TLabel;
    lbl3: TLabel;
    lbl2: TLabel;
    chkAutoJson: TCheckBox;
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  ProjOptDlg: TProjOptDlg;

implementation

{$R *.dfm}

end.
