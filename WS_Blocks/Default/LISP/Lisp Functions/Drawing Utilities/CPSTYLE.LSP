; --- cpstyle Function ---
; Export all blocks to seperate drawing files in designated location
; Used in combination with defining routine
;http://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/batch-wblock-of-blocks-in-drawing/td-p/789689
; 08-09-2016
(defun c:cpstyle ()
(if (= (getvar "PSTYLEMODE") 0)
 (command "convertpstyles")
)
(command "_.qsave")
)