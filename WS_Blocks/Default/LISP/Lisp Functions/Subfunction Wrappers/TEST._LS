;;; --- TEST Function---														
;;; Notes:																
;;; Blank space used to test new functions												
;;;																	
;;; Alex Lundin																
;;; 09-01-2017																
;;; Example call															
;;; (c:test)																

(defun c:test
		(
		;arguments
		/
		;local variables
		*error*
		COUNTER COUNTERMAX ENTITY ENTITYLIST LAYER RETURNEDLIST RETURNEDSET SELECTIONSET TRUENAMELIST 
		)
  
;;; Arguments:																
;;;	None																
;;; Return:																
;;;	None																
  
	;;; Wrapper for DEFAULT-ERROR-HANDLER
	(defun *error* ( msg )
		(setq theCallingFunctionsName "c:test")
		(DEFAULT-ERROR-HANDLER theCallingFunctionsName msg)
	)
	

  	;;; error trapper example
  	;;; uncomment these lines
  	;;; they are illegal and will demonstrate the error trapping method
  	;;; (setq layer "WATTSTOPPER")
  	;;; (setq var1 (nth 1 layer))




	;;; set variables to necessary set for the next Wrapper
	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "WATTSTOPPER" ) (66 . 1 ))))
	(setq trueNameList (list "LMSM-201" "LMSM-600" "LMSM-603" "GENERIC-SQUARE" "GENERIC-CIRCLE" "GENERIC-SUN" "GENERIC-X" "GENERIC-SWITCH" "V-EASYTOUCH-II-1" "V-EASYTOUCH-II-2" "V-EASYTOUCH-II-3" "V-EASYTOUCH-II-4" "V-EASYTOUCH-II-5" "V-EQ40TB-TI" "V-EQ73TB-TI" "V-EQ41TB-TI" "V-DMX-DALI-GW" "V-EM-LIGHTSENSOR" "V-LCAP44A" "V-LCAP44L" "V-LVOS" "V-IC-DIN-II-LITE-RF" "V-IC-DIN-II-LITE" "V-STPSRW101" "V-STPSRW201" "V-STPERW101" "V-STPERW201" "V-IC-36" "V-LCAP44HS" "V-LCAP32S" "V-LCAP44S" "V-LCAP44H" "V-LCAP44M" "V-LCAP32M"))
	(setq finalSet allBlocks)
	(setq returnedList (SELECTION-WITH-TRUENAME-FILTER finalSet trueNameList))
	(setq entityList returnedList)

  	;;; Wrapper
  	;;; move blocks that are not part of the DLM Riser from the WATTSTOPPER layer to the 0 layer
	(setq associationListNumber 8)
	(setq associationListValue "0")
	(WRAPPER-CUSTOM-ENTMOD-1 associationListNumber associationListValue entityList)

  


  
	;;; Wrapper Test
	(WRAPPER-ROOMID-FORMAT-1)	
  
	;;; Wrapper Test
;;;	(setq associationListNumber 7)
;;;	(setq associationListValue "ARIAL")
;;;	(setq layer "WATTSTOPPER")															
;;;	(WRAPPER-CUSTOM-ENTMOD-1 associationListNumber associationListValue layer)
;;;  
;;;	(setq layer "WATTSTOPPER")														
;;;	(setq returnedSet (SELECT-ENTIRE-LAYER layer))
;;;	(setq set1 returnedSet)
;;;
;;;	(setq layer "0")														
;;;	(setq returnedSet (SELECT-ENTIRE-LAYER layer))
;;;	(setq set2 returnedSet)
;;;
;;;	(setq trueNameList (list "DA2401" "LMDI-100"))
;;;	(setq finalSet (acet-ss-union (list set1 set2)))
;;;	      
;;;	(setq returnedList (SELECTION-WITH-TRUENAME-FILTER finalSet trueNameList))
;;;	
;;;
;;;	(setq entityList returnedList)
;;;	(setq counter 0)
;;;	(setq counterMax (length entityList))
;;;	(while
;;;			(< counter counterMax)
;;;			(setq entity (nth counter entityList))
;;;			
;;;			(setq returnedList (GET-BLOCK-ATTRIBUTE-TAGS-AND-VALUES-LIST entity))
;;;			(setq attributeAssociationList returnedList)
;;;	  
;;;	  		(setq AuxiliaryData (assoc "VAUXILIARY" attributeAssociationList))
;;;			(setq counter (+ 1 counter))
;;;	)
;;;  
;;;	(setq stfalist(assoc 3(tblsearch "Style" "ROMANC")))
;;;	(setq objname(tblobjname "Style" "Standard"))
;;;	(entmod (subst(cons 3 "Arial.ttf") stfalist (entget objname)))
;;;	(command "regenall")
  
  	(princ)

)


