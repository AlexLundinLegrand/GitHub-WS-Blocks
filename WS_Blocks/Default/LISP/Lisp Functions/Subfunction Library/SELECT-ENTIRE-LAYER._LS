;;; --- SELECT-ENTIRE-LAYER Sub Function---												
;;; Notes:																
;;; send layer in through layer variable from calling function										
;;; selection set is returned to caller													
;;;																	
;;; Alex Lundin																
;;; 09-01-2017																
;;; Example call															
;;; (setq layer "WATTSTOPPER"))														
;;; (setq returnedSet (SELECT-ENTIRE-LAYER layer))											

(defun SELECT-ENTIRE-LAYER
       	(
	;arguments
	layer
	/
	;local variables
	*ERROR* RETURNSET SELECTIONSET
	)
  
;;; Arguments:																
;;;	layer - any valid layer in the drawing												
;;; Return:																
;;;	returnSet - selection set of all objects on the entire layer									

	;;; Wrapper for DEFAULT-ERROR-HANDLER											
	(defun *error* ( msg )
		(setq theCallingFunctionsName "SELECT-ENTIRE-LAYER")
		(DEFAULT-ERROR-HANDLER theCallingFunctionsName msg)
	)
  
	(setq selectionSet (ssget "x" (list (cons 8 layer))))			;create selection set with layer variable
	(setq returnSet selectionSet)


  

  
)

