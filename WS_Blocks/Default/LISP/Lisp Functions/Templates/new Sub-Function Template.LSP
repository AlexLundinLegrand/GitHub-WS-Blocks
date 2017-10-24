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
		*ERROR*
		)
;;; Arguments:		
;;;			
;;; Return:		
;;;			
	
;;; Wrapper for DEFAULT-ERROR-HANDLER															
;;; This error function is useful for debugging														
;;; You can place this error hanlding function inside any of the other functions for easy error trapping						
;;; When using multiple error hanlders per file, make sure to edit the theCallingFunctionsName variable to make it clear what function caused the error	
;;; The syntax for the function name must stay exactly as follows											
;;; The error function has asterisks on either side and accepts exactly 1 argument, msg									
;;; The AutoCAD LISP programming environment will look for this function and call it for any unexpected error during runtime				
;;; Inside the function definition *error* you can get creative with the implementation that makes the most sense					
;;; This one makes use of another sub function to print out the calling functions name and the error message						
	(defun *error* ( msg )
		(setq theCallingFunctionsName "new-sub")
		(DEFAULT-ERROR-HANDLER theCallingFunctionsName msg)
	)

;;; function body																	



)


