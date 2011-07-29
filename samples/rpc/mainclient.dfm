object Form13: TForm13
  Left = 0
  Top = 0
  Caption = 'Form13'
  ClientHeight = 282
  ClientWidth = 533
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 225
    Height = 282
    Align = alLeft
    TabOrder = 0
  end
  object Button1: TButton
    Left = 248
    Top = 32
    Width = 89
    Height = 25
    Caption = 'Echo'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 352
    Top = 34
    Width = 121
    Height = 21
    TabOrder = 2
    Text = 'hello'
  end
  object Button2: TButton
    Left = 248
    Top = 72
    Width = 89
    Height = 25
    Caption = 'RTTI'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 248
    Top = 112
    Width = 89
    Height = 25
    Caption = 'WebSocket'
    TabOrder = 4
    OnClick = Button3Click
  end
  object Edit2: TEdit
    Left = 352
    Top = 114
    Width = 121
    Height = 21
    TabOrder = 5
    Text = 'websocket'
  end
end
