;;; --- new-sub Sub Function---													
;;; Notes:															
;;; Use replace to replace subfunction new-sub name										
;;; This updates the build in error hanlders to give you information and graceful exits when your code terminates for any reason
;;; Developer Name														
;;; XX-XX-XXXX															
;;; Example call														
;;; (new-sub)															
(defun new-sub
       		(
		;arguments
		/
		;local variables
		)
;;; Arguments:		
;;;			
;;; Return:		
;;;			

	

	
  	;can place error hanlder inside any function as desired		
  	;this is useful for files containing multiple functions		
  	;multiple error hanlders will identify the source of the error	
  	;follow instructions above the error hanlder			


  	; function body							



)


;;; Default error handler for all functions at Legrand											
;;; This error function is useful for debugging												
;;; For a single function file, place the error function outside the main function, directly below it.					
;;; For a multi function file, you can place this error hanlding function inside any of the other functions for easy error trapping	
;;; When using multiple error hanlders per file, make sure to edit the messages section to make it clear what function caused the error	
;;; The syntax for the function name must stay exactly as follows									
;;; The error function has asterisks on either side and accepts exactly 1 argument, msg							
;;; The AutoCAD LISP programming environment will look for this function and call it for any unexpected error during runtime
(defun *error* ( msg )
	;default msg, exit occurs naturally
	((= msg "Function cancelled")
	(princ "\nnew-sub: Canceled.")
	(princ "\nThis is most likely caused from terminating a custom command early.")
	(princ "\nExiting now.")
	(print)
	(princ "\nThe following is the error message from the AutoCAD Software.")
	(print)
	;control now passes back to AutoCAD, the client will see what the error message is
	)
	;default msg, exit occurs naturally
	((= msg "quit / exit abort")
	(princ "\nnew-sub: Exit.")
	(princ "\nThis is most likely caused from terminating a custom command early.")
	(princ "\nExiting now.")
	(print)
	(princ "\nThe following is the error message from the AutoCAD Software.")
	(print)
	;control now passes back to AutoCAD, the client will see what the error message is
	)			
	;default msg is not either of the above, exit occurs naturally
	(t
	(princ "\nnew-sub: Unexpected error.")
  	(princ "\nExiting now.")
	(print)
	(princ "\nThe following is the error message from the AutoCAD Software.")
	(print)
	;control now passes back to AutoCAD, the client will see what the error message is
	)
)