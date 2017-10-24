;;; --- DEFAULT-ERROR-HANDLER Sub Function---											
;;; Notes:															
;;;	This function is the operating procedure for error handling								
;;; The wrapper is defined in larger functions for modular code reuse and consistent error trapping				
;;; Alex Lundin															
;;; 09-04-2017															
;;; example call														
;;; (DEFAULT-ERROR-HANDLER theCallingFunctionsName errorMessageFromAutoCAD)							

(defun	DEFAULT-ERROR-HANDLER
		(
		;arguments
		theCallingFunctionsName errorMessageFromAutoCAD
		/
		;local variables
		COUNTER COUNTER2 COUNTERMAX COUNTERMAX2 DXFCODE0 DXFCODE2 DXFCODE66 DXFCODE8 DXFCODECODE-1 ENTITIESTORETURN ENTITY ENTITYDXFCODES *ERROR* RETURNLIST SUPPLIEDTRUENAME TRUENAME
		)
  
;;; Arguments:																
;;;	theCallingFunctionsName 	- name of calling function, which is defined in the error handler inside the caller		
;;; 	errorMessageFromAutoCAD	- error message generated from AutoCAD software environment						
;;; Return:																
;;;	entityList	- None					
  

;;; Default error handler for all functions at Legrand											
;;; This error function is useful for debugging
;;; AutoCAD has built in error messages
;;; They are sent into this function from the calling function
;;; This conditional block prints the calling function and the error message for the client						
	(cond
		;default messsage, exit occurs naturally
		((= errorMessageFromAutoCAD "Function cancelled")
		(princ (strcat "\n" theCallingFunctionsName " --- was canceled."))
		(princ "\nThis is most likely caused from terminating a custom command early.")
		(princ "\nExiting now.")
		(print)
		)
		;default messsage, exit occurs naturally
		((= errorMessageFromAutoCAD "quit / exit abort")
		(princ (strcat "\n" theCallingFunctionsName " --- was quit."))
		(princ "\nThis is most likely caused from terminating a custom command early.")
		(princ "\nExiting now.")
		(print)
		)			
  		;any other error message, other than the two default ones above
		(t
		(princ (strcat "\n" theCallingFunctionsName " --- encountered a unexpected error."))
	  	(princ "\nExiting now.")
		(print)
		(princ "\nThe following is the error message from the AutoCAD Software.")
		(print)
		;control now passes back to AutoCAD, the client will see what the error message is
		)
	)

	;no return value
	(princ)
)



