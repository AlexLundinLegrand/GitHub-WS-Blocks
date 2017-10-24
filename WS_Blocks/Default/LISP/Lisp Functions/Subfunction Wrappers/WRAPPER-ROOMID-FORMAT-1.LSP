;;; --- WRAPPER-ROOMID-FORMAT-1 Sub Function---											
;;; Notes:															
;;; Calls a series of functions to format ROOMID attributes of blocks on WATTSTOPPER layer					
;;;																
;;; Alex Lundin															
;;; 09-01-2017															
;;; Example call														
;;; (WRAPPER-ROOMID-FORMAT-1)													

(defun WRAPPER-ROOMID-FORMAT-1
       	(
	/
	*error* layer
	)

;;; Arguments:															
;;;	None															
;;; Return:															
;;;	None															

  
	;;; Wrapper for DEFAULT-ERROR-HANDLER											
	(defun *error* ( msg )
		(setq theCallingFunctionsName "WRAPPER-ROOMID-FORMAT-1")
		(DEFAULT-ERROR-HANDLER theCallingFunctionsName msg)
	)
	
	(setq layer "WATTSTOPPER")													
	(WRAPPER-ROOMID-FORMAT-2 layer)


)