;;; --- WRAPPER-CUSTOM-ENTMOD-1 Sub Function---												
;;; Notes:																
;;;																	
;;; Alex Lundin																
;;; 09-01-2017																
;;; Example call															
;;;(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "WATTSTOPPER" ) (66 . 1 ))))								
;;;(setq trueNameList (list "GENERIC-SQUARE" "GENERIC-CIRCLE" "GENERIC-SUN" "GENERIC-X" "GENERIC-SWITCH"))				
;;;(setq finalSet allBlocks)														
;;;(setq returnedList (SELECTION-WITH-TRUENAME-FILTER finalSet trueNameList))								
;;;(setq entityList returnedList)													
;;;																	
;;; Wrapper																
;;; move blocks that are not part of the DLM Riser from the WATTSTOPPER layer to the 0 layer						
;;;(setq associationListNumber 8)													
;;;(setq associationListValue "0")													
;;;(WRAPPER-CUSTOM-ENTMOD-1 associationListNumber associationListValue entityList)							

(defun WRAPPER-CUSTOM-ENTMOD-1
       	(
	associationListNumber associationListValue entityList
	/
	*error* RETURNEDSET THECALLINGFUNCTIONSNAME
	)
	
	;;; Wrapper for DEFAULT-ERROR-HANDLER											
	(defun *error* ( msg )
		(setq theCallingFunctionsName "WRAPPER-CUSTOM-ENTMOD-1")
		(DEFAULT-ERROR-HANDLER theCallingFunctionsName msg)
	)
  													
	(WRAPPER-CUSTOM-ENTMOD-2 associationListNumber associationListValue entityList)

)


