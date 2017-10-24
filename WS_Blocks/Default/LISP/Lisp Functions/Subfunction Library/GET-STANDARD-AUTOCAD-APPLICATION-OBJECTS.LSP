;;; --- GET-STANDARD-AUTOCAD-APPLICATION-OBJECTS Sub Function---								
;;; Notes:															
;;; Use replace to replace subfunction GET-STANDARD-AUTOCAD-APPLICATION-OBJECTS name						
;;; This updates the build in error hanlders to give you information and graceful exits when your code terminates for any reason
;;; Developer Name														
;;; XX-XX-XXXX															
;;; Example call														
;;;(setq returnedList (GET-STANDARD-AUTOCAD-APPLICATION-OBJECTS))
;;;(setq AutoCADApplicationObject (nth 0 returnedList))
;;;(setq AutoCADActiveDocumentObject (nth 1 returnedList))
;;;(setq AutoCADBlockTableObject (nth 2 returnedList))

(defun GET-STANDARD-AUTOCAD-APPLICATION-OBJECTS
       		(
		;arguments
		/
		;local variables
		*ERROR* AUTOCADACTIVEDOCUMENTOBJECT AUTOCADAPPLICATIONOBJECT AUTOCADBLOCKTABLEOBJECT RETURNLIST THECALLINGFUNCTIONSNAME
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
		(setq theCallingFunctionsName "GET-STANDARD-AUTOCAD-APPLICATION-OBJECTS")
		(DEFAULT-ERROR-HANDLER theCallingFunctionsName msg)
	)

;;; function body															
	(setq AutoCADApplicationObject (vlax-get-acad-object))
	(setq AutoCADActiveDocumentObject (vla-get-ActiveDocument AutoCADApplicationObject))
	(setq AutoCADBlockTableObject (vla-get-blocks AutoCADActiveDocumentObject))
	
	(setq returnList(list AutoCADApplicationObject AutoCADActiveDocumentObject AutoCADBlockTableObject))


)