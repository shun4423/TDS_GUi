' CreateShortcut.vbs
Option Explicit

Dim sh, fso, base, linkPath, iconPath, lnk
Set sh = CreateObject("WScript.Shell")
Set fso = CreateObject("Scripting.FileSystemObject")

' アプリフォルダ（固定）
base = "C:\Apps\TDS_GUi"

' デスクトップに作成
linkPath = sh.SpecialFolders("Desktop") & "\TDS_GUI.lnk"

' アイコン（.ico推奨。なければ exe/dll のアイコンでもOK）
iconPath = base & "\default.ico"

Set lnk = sh.CreateShortcut(linkPath)

' VBSを直接ターゲットにしても良いが、確実に wscript 経由にする（アイコンや引数も安定）
lnk.TargetPath = sh.ExpandEnvironmentStrings("%SystemRoot%\System32\wscript.exe")
lnk.Arguments = """" & base & "\RunTDS.vbs"""
lnk.WorkingDirectory = base
lnk.WindowStyle = 7  ' 7=最小化。0=通常

If fso.FileExists(iconPath) Then
  lnk.IconLocation = iconPath
Else
  ' アイコンファイルが無い場合はVBSそのもの/または任意のexeにする
  lnk.IconLocation = base & "\RunTDS.vbs"
End If

lnk.Description = "TDS GUI"
' もし「ショートカットキー（ホットキー）」も付けたいなら（任意）
' lnk.Hotkey = "CTRL+ALT+T"

lnk.Save
