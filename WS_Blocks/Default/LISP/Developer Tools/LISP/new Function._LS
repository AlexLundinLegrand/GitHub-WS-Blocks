;;; --- new ---
;;; Notes:
;;; Developer Name
;;; XX-XX-XXXX
(defun c:new
       		(
		;arguments
		/
		;local variables
		)
;;; Arguments:
;;;	1
;;;	2


	;Default error handler for all functions at Legrand
	;The syntax for the function name must stay exactly as follows
	;The error function has asterisks on either side and accepts exactly 1 argument, msg
	;The AutoCAD LISP programming environment will look for this function and call it for any unexpected error during runtime
	(defun *error* ( msg )
		;default msg, exit occurs naturally
		((= msg "Function cancelled")
		(princ "\nFunction canceled.")
		(princ "\nThis is most likely caused from terminating a custom command early.")
		(princ "\nExiting now.")
		)
		;default msg, exit occurs naturally
		((= msg "quit / exit abort")
		(princ "\nFunction exit.")
		(princ "\nThis is most likely caused from terminating a custom command early.")
		(princ "\nExiting now.")
		)			
  		;default msg, exit occurs naturally
		(t
		(princ "\nUnexpected error.")
	  	(princ "\nExiting now.")
		)
	)


)