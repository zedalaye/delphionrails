object ProjOptDlg: TProjOptDlg
  Left = 227
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Delphi on Rails project options'
  ClientHeight = 171
  ClientWidth = 384
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 281
    Height = 161
    Shape = bsFrame
  end
  object lbl1: TLabel
    Left = 24
    Top = 29
    Width = 64
    Height = 13
    Caption = 'Service name'
  end
  object lbl3: TLabel
    Left = 25
    Top = 56
    Width = 63
    Height = 13
    Caption = 'Display name'
  end
  object lbl2: TLabel
    Left = 37
    Top = 83
    Width = 51
    Height = 13
    Caption = 'Listen port'
  end
  object OKBtn: TButton
    Left = 300
    Top = 8
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelBtn: TButton
    Left = 301
    Top = 39
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object edServiceName: TEdit
    Left = 94
    Top = 21
    Width = 169
    Height = 21
    TabOrder = 2
    Text = 'DORServer'
  end
  object edDisplayName: TEdit
    Left = 94
    Top = 48
    Width = 169
    Height = 21
    TabOrder = 3
    Text = 'Delphi on Rails Server'
  end
  object edPort: TEdit
    Left = 94
    Top = 75
    Width = 121
    Height = 21
    TabOrder = 4
    Text = '80'
  end
  object chkAutoJson: TCheckBox
    Left = 94
    Top = 102
    Width = 137
    Height = 17
    Caption = 'Automatic Json View'
    Checked = True
    State = cbChecked
    TabOrder = 5
  end
end
