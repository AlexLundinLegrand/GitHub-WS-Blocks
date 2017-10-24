;;; --- RELEASE-OBJECTS Sub Function---													
;;; Notes:															
;;; Use replace to replace subfunction RELEASE-OBJECTS name										
;;; This updates the build in error hanlders to give you information and graceful exits when your code terminates for any reason
;;; Developer Name														
;;; XX-XX-XXXX															
;;; Example call														
;;; (RELEASE-OBJECTS)															
(defun RELEASE-OBJECTS
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
	(defun *error* ( msg )
		(setq theCallingFunctionsName "RELEASE-OBJECTS")
		(DEFAULT-ERROR-HANDLER theCallingFunctionsName msg)
	)

;;; function body															



)