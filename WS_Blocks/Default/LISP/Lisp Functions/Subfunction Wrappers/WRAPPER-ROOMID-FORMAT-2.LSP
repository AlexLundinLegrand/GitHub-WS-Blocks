;;; --- WRAPPER-ROOMID-FORMAT-2 Sub Function---												
;;; Notes:																
;;; This is a wrapper function for a few sub functions											
;;; Caller must send in layer to select blocks from											
;;; Developer Name															
;;; 09-01-2017																
;;; Example call															
;;; (setq layer "WATTSTOPPER")														
;;; (WRAPPER-ROOMID-FORMAT-2 layer)													

(defun WRAPPER-ROOMID-FORMAT-2
       		(
		;arguments
		layer
		/
		;local variables
		*ERR0R* RETURNEDSET 
		)	
;;; Arguments:																
;;;	layer	- layer to select objects from												
;;; Return:																
;;;	None																
  
	;;; Wrapper for DEFAULT-ERROR-HANDLER												
	(defun *error* ( msg )
		(setq theCallingFunctionsName "WRAPPER-ROOMID-FORMAT-2")
		(DEFAULT-ERROR-HANDLER theCallingFunctionsName msg)
	)
  
  	(setq returnedSet (SELECT-ENTIRE-LAYER layer))
 	(WRAPPER-ROOMID-FORMAT-3 returnedSet)
  


  
)


