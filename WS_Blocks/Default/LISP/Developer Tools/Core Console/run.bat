for /R %%f in (C:\Users\Alex\Desktop\RiserLayout*.dwg) do (
    "C:\Program Files\Autodesk\AutoCAD 2016\accoreconsole.exe"  /i "%%f" /s "C:\Users\Alex\Desktop\RiserLayout\code.scr"
    )