(defun C:cc ( / FILE SCR STRINGS)
  (vl-load-com)
	(setq file "C:\\Users\\Alex\\Desktop\\RiserLayout\\RiserLayout.dwg")
	(setq scr "C:\\Users\\Alex\\Desktop\\RiserLayout\\cc.scr")
;;;  	(setq string (strcat "accoreconsole.exe /i " "\"" file "\"" " /s " "\"" scr "\"" " /l en-US"))
;;;  
;;;	(setq stringW (strcat 	"C:/Program Files/Autodesk/AutoCAD 2016/accoreconsole.exe\" /i \""
;;;							file
;;;							" /s \""
;;;							scr
;;;							" /l en-US\n")
;;;			
;;;	)
;;;
;;;  
;;;	(setq file (open scr "w"))
;;;	(write-line stringw  file)
;;;	(close  file)
	(setq stringS (strcat 	"AcCoreConsole.exe /i " file " /s " scr " /l en-US\n"))
  	(setq string5 "AcCoreConsole.exe /i C:\\Users\\Alex\Desktop\\RiserLayout\\RiserLayout.dwg /s C:\\Users\\Alex\\Desktop\\RiserLayout\\Riser-layout-script.scr /l en-US")
	(startapp "accoreconsole" string5)
	(princ)
 )