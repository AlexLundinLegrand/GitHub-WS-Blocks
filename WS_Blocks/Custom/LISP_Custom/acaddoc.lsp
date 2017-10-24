; --- acaddoc ---
; Alex Lundin 05-12-2017
; Notes to Engineers:
; The two variables that make this file great are "WSCURRENT" and 'osmode
; They are upfront and contain comments on how to set them for your needs
; This file is stored in your customizations folder, once you edit these settings the will be protected from updates
; It's up to you to maintain this file
; Notes to Developers:
; This file runs each time Autocad opens a new drawing and sets the workspace and other variables for the customer
; The syntax needs to look like this, there is no defun at beginning
(vl-load-com)

;;;sets workspace, follow comments to change
(setvar "WSCURRENT" "Drafting & Annotation")						;set current workspace for with setvar, there are options to change below
;;;(setvar "WSCURRENT" "WATTSTOPPER")							;set current workspace for with setvar, there are options to change below
;;;other options for current workspace
;;;just replace the last set of quotes above, with desired space
;;;options for workspace name in quotes, replace the third item on the setvar "WSCURRENT" line:
;;;"Wattstopper"
;;;"Drafting & Annotation"
;;;"AutoCAD Classic"


;;;sets osmode, follow comments to change
(setvar 'osmode 743)								;set osmode to 743
;;;osmode integer is calculated from which osnaps you prefer
;;;add the values for each item you would like
;;;the result is the value of your osmode
;;;options below"
;;;0 NONe
;;;1 ENDpoint
;;;2 MIDpoint
;;;4 CENter
;;;8 NODe
;;;16 QUAdrant
;;;32 INTersection
;;;64 INSertion
;;;128 PERpendicular
;;;256 TANgent
;;;512 NEArest
;;;1024 QUIck
;;;2048 APParent Intersection
;;;4096 EXTension
;;;8192 PARallel

(setvar "PICKADD" 2)
;;;pickadd integer
;;;options below:
;;;0 Turns off PICKADD. The objects most recently selected become the selection set. Previously selected objects are automatically removed from the selection set.
;;;1 Turns on PICKADD. Each object selected, either individually or by windowing, is added to the current selection set
;;;2 Turns on PICKADD. Each object selected, either individually or by windowing, is added to the current selection set. If the SELECT command is used, keeps objects selected after the command ends.

(setvar "PICKFIRST" 1)
;;;pickfirst integer
;;;options below:
;;;0 Off. You can select objects only after you start a command
;;;1 On. You can also select objects before you start a command

(setvar "SAVETIME" 1)								;set automatic save time
(setenv "DefaultFormatForSave" "36")						;set save format, 36 is a 2007 AutoCAD file
(setenv "NoStartUpDialog" "1")							;disable startupdialog
(setvar "rememberfolders" 1)							;set rememberfolders variable to 1 so AutoCAD remembers locations in dialog boxes
;;;options below:
;;;0 Off
;;;1 3D only
;;;2 2D only
;;;3 2D and 3D

(setvar "ISAVEPERCENT" 1)
(setvar "FILEDIA" 1)