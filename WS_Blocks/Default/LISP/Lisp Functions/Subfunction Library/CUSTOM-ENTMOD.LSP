;;; --- CUSTOM-ENTMOD Sub Function---													
;;; Notes:																
;;; send selection set in through selectionSet variable from calling function								
;;; nothing is returned to caller													
;;; Alex Lundin																
;;; 09-01-2017																
;;; example call															
;;; (CUSTOM-ENTMOD mainEntity subEntity associationListNumber associationListValue)							

(defun	CUSTOM-ENTMOD
		(
		;arguments
		mainEntity subEntity associationListNumber associationListValue
		/
		;local variables
		ATTRIBUTETAG ATTRIBUTEVALUE COUNTER COUNTERMAX DXFCODE-1 DXFCODE0 DXFCODE2 DXFCODE66 DXFCODE8 mainEntity mainEntityDXFCODES mainEntityNAMEFORDRILLING *ERROR* TRUENAME
		)
  
;;; Arguments:																
;;;	mainEntity 			- any valid mainEntity										
;;;	subEntity			- any valid entity inside the main entity, pass in nil if there is not one			
;;; 	associationListNumber 		- any valid number corresponding to a association item that is part of the entities dxfCodes	
;;;	associationListValue		- value to store in mainEntity dxf codes							
;;; Return:																
;;;	None																
  

	;;; Wrapper for DEFAULT-ERROR-HANDLER											
	(defun *error* ( msg )
		(setq theCallingFunctionsName "CUSTOM-ENTMOD")
		(DEFAULT-ERROR-HANDLER theCallingFunctionsName msg)
	)


  
	(setq mainEntityDxfCodes(entget mainEntity))															;set dxf codes of mainEntity to mainEntityDxfCodes
  	
  


  	(if
	  	;if, subEntity is nil
	  	(= subEntity nil)
	  	(progn
		(setq mainEntityDxfCodes(subst (cons associationListNumber associationListValue)(assoc associationListNumber mainEntityDxfCodes) mainEntityDxfCodes))	;change dxfcodes on mainEntityDxfCodes using the values supplied
		(entmod mainEntityDxfCodes)																;entmod function to modifiy the mainEntity with the new values
		)
	  	;else, subEntity is not nil
	  	(progn
;;;		(setq subEntityDxfCodes(entget subEntity))														;set dxf codes of subEntity to subEntityDxfCodes
;;;		(setq subEntityDxfCodes(subst (cons associationListNumber associationListValue)(assoc associationListNumber subEntityDxfCodes) subEntityDxfCodes))	;change dxfcodes on mainEntityDxfCodes using the values supplied
;;;		(entmod subEntityDxfCodes)																;entmod function to modifiy the mainEntity with the new values
		(setq mainEntityDxfCodes(subst (cons associationListNumber associationListValue)(assoc associationListNumber mainEntityDxfCodes) mainEntityDxfCodes))	;change dxfcodes on mainEntityDxfCodes using the values supplied
		(entmod mainEntityDxfCodes)

		)
	)
	
	
)