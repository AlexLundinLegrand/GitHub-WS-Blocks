;;; --- driver---
;;; Notes:
;;; Demonstrates turning command line echo off so return value is not print to command line
;;; Demonstrates returning a list to a calling function
;;; Demonstrates seperating a return list from a sub function
;;; Alex Lundin 06-14-2017
(defun 	c:driver ( / RETURNLIST RETURNLISTITEM1 RETURNLISTITEM2 RETURNLISTITEM3 RETURNLISTITEM4)
	(setvar "CMDECHO" 0)											;turn command line echo off, supressed printing anything returned from sub functions
  	(setq returnList (get-drawing-info))									;set returnlist to the value recieved from get-drawing-info
  	(setq returnListItem1 (nth 0 returnList))
  	(setq returnListItem2 (nth 1 returnList))
  	(setq returnListItem3 (nth 2 returnList))
  	(setq returnListItem4 (nth 3 returnList))

  	(setvar "CMDECHO" 1)											;turn command line echo on to print output
  	(princ (strcat "The drawing name is: " returnListItem1))
  	(terpri)												;newline inside AutoCAD commandline
  	(princ (strcat "The user name is: " returnListItem2))
  	(terpri)
  	(princ (strcat"The date is: " returnListItem3))
  	(terpri)
  	(princ (strcat "The new file name is: " returnListItem4))
  	(princ)													;clean executing always has princ at end
)


;;; --- get-drawing-info---
;;; Notes:
;;; Alex Lundin 06-14-2017
(defun get-drawing-info ( / DATE GET-DRAWING-INFO-RETURNLIST NEWFILENAME ORIGINALFILENAME USERNAME)

  	(setq originalfilename (strcat (getvar "DWGPREFIX" ) (getvar "DWGNAME" )))
  	(setq username (getenv "USERNAME"))
	(setq date (rtos (getvar "CDATE") 2 6))
  	(setq newfilename (strcat "H:\\Drawings\\Room Details Save Location\\" date (getvar "DWGNAME" )))
  	(setq get-drawing-info-returnList (list originalfilename username date newfilename))			;create returnlist, the last line is the only thing ever returned from a subfunction
  														;the return happens through the AutoCAD command line, that's why echo is off
)