;;; --- SELECTION-WITH-TRUENAME-FILTER Sub Function---											
;;; Notes:																
;;; send selection set in through selectionSet variable from calling function								
;;; association list of attribute name and attribute value is returned to caller							
;;; Alex Lundin																
;;; 09-04-2017																
;;; example call															
;;; (setq returnedList (SELECTION-WITH-TRUENAME-FILTER selectionSet trueNameList))							

(defun	SELECTION-WITH-TRUENAME-FILTER
		(
		;arguments
		selectionSet trueNameList
		/
		;local variables
		COUNTER COUNTER2 COUNTERMAX COUNTERMAX2 DXFCODE0 DXFCODE2 DXFCODE66 DXFCODE8 DXFCODECODE-1 ENTITIESTORETURN ENTITY ENTITYDXFCODES *ERROR* RETURNLIST SUPPLIEDTRUENAME TRUENAME
		)
  
;;; Arguments:																
;;;	selectionSet 	- any valid selection set											
;;; 	trueNameList	- list of truenames to filter for										
;;; Return:																
;;;	entityList	- list of entities that match one of the supplied trueNames							
  

	;;; Wrapper for DEFAULT-ERROR-HANDLER											
	(defun *error* ( msg )
		(setq theCallingFunctionsName "SELECTION-WITH-TRUENAME-FILTER")
		(DEFAULT-ERROR-HANDLER theCallingFunctionsName msg)
	)	


  
  	(setq counter 0)												;initialize counter to 0 for while loop
  	(if														;if
	  	(/= selectionSet nil)											;set is not nil
		(progn													;progn wrap
  		(setq counterMax (sslength selectionSet))								;set counterMax to length of items in selection
		)													;progn wrap end
	)														;if end
	  
	(while 	(< counter counterMax)											;set loop to continue while the counter varaible counter is less than counterMax
		(setq entity(ssname selectionSet counter))								;set variable entity to the name of the item from the selection that matches the current counter value
		(setq entityDxfCodes(entget entity))									;set the varaible entityDxfCodes to the list of entities from the en varaible

	  	;;; you can use the method here to find any value from a dxfCodecode
	  	(setq dxfCodeCode-1 (cdr (assoc -1 entityDxfCodes )))							;set dxfCodeCode-1 to the second element of the item that has -1 as it's first element, this is the entity name
		(setq dxfCode0 (cdr (assoc 0 entityDxfCodes )))								;set dxfCode0 to the element of the item that has 0 as it's first element, this is the entity type
		(setq dxfCode2 (cdr (assoc 2 entityDxfCodes )))								;set dxfCode8 to the second element of the item that has 8 as it's first element, this is the name, or block name
		(setq dxfCode8 (cdr (assoc 8 entityDxfCodes )))								;set dxfCode8 to the second element of the item that has 8 as it's first element, this is the layer
		(setq dxfCode66 (cdr (assoc 66 entityDxfCodes )))							;set dxfCode66 to the second element of the item that has 66 as it's first element, this is the attribute flag
	  
		;;; ENTITY-TO-BLOCK-TRUENAME sub-function call
	  	;;; AutoCAD is able to change a blocks name for some other functionality used with dynamic blocks
	  	;;; this makes finding blocks based by dxfCodeCode challanging
	  	;;; this truename filter can use some vlax functions to get the effective name, aka truename
		(setq trueName (ENTITY-TO-BLOCK-TRUENAME entity))	  

		(setq counter2 0)
		(setq counterMax2 (length trueNameList))
		(while	(< counter2 counterMax2)
				(setq suppliedTrueName (nth counter2 trueNameList))
				
				(if	
					(= suppliedTrueName trueName)
					(progn
					(setq entitiesToReturn (cons entity entitiesToReturn))
					(setq counter2 counterMax2)
					)
				)
				(setq counter2(+ counter2 1))
		)

	  
		(setq counter(+ counter 1))										;once while loop reaches this point, increment counter varaible by one
	)
	
	(setq returnList entitiesToReturn)
	
)



