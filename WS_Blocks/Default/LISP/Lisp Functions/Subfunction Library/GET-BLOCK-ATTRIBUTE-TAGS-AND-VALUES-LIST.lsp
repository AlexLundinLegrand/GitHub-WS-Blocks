;;; --- GET-BLOCK-ATTRIBUTE-TAGS-AND-VALUES-LIST Sub Function---									
;;; Notes:																
;;; send entity in through entity variable from calling function									
;;; association list of attribute tags and attribute values is returned to caller							
;;; Alex Lundin																
;;; 09-04-2017																
;;; example call															
;;; (setq returnedList (GET-BLOCK-ATTRIBUTE-TAGS-AND-VALUES-LIST entity))								

(defun	GET-BLOCK-ATTRIBUTE-TAGS-AND-VALUES-LIST
		(
		;arguments
		entity
		/
		;local variables
		COUNTER COUNTER2 COUNTERMAX COUNTERMAX2 DXFCODE0 DXFCODE2 DXFCODE66 DXFCODE8 DXFCODECODE-1 ENTITIESTORETURN ENTITY ENTITYDXFCODES *ERROR* RETURNLIST SUPPLIEDTRUENAME TRUENAME
		)
  
;;; Arguments:																
;;;	entity	 	- any valid entity												
;;; Return:																
;;;	returnList	- association list of attribute tags and values in the entity							
  

	;;; Wrapper for DEFAULT-ERROR-HANDLER											
	(defun *error* ( msg )
		(setq theCallingFunctionsName "GET-BLOCK-ATTRIBUTE-TAGS-AND-VALUES-LIST")
		(DEFAULT-ERROR-HANDLER theCallingFunctionsName msg)
	)


  	(setq counter 0)												;initialize counter to 0 for while loop
  	(if														;if
	  	(/= entity nil)												;entity is not nil
		(progn													;progn wrap
		(setq entityDxfCodes(entget entity))									;set the varaible entityDxfCodes to the list of entities from the en varaible

	  	;;; you can use the method here to find any value from a dxfCodecode
	  	(setq dxfCode-1 (cdr (assoc -1 entityDxfCodes )))							;set dxfCode-1 to the second element of the item that has -1 as it's first element, this is the entity name
		(setq dxfCode0 (cdr (assoc 0 entityDxfCodes )))								;set dxfCode0 to the element of the item that has 0 as it's first element, this is the entity type
		(setq dxfCode2 (cdr (assoc 2 entityDxfCodes )))								;set dxfCode8 to the second element of the item that has 8 as it's first element, this is the name, or block name
		(setq dxfCode8 (cdr (assoc 8 entityDxfCodes )))								;set dxfCode8 to the second element of the item that has 8 as it's first element, this is the layer
		(setq dxfCode66 (cdr (assoc 66 entityDxfCodes )))							;set dxfCode66 to the second element of the item that has 66 as it's first element, this is the attribute flag
	 	  
		(setq entityNameForDrilling entity) 

		(if													;if start
		  	(= dxfCode66 1)											;entity attribute flag is 1
		  	(progn												;progn wrap
			  

			(while 	(/= dxfCode0 "SEQEND" )									;while loop to drill to each sub entity in a block
				(setq attributeTag(cdr(assoc 2 entityDxfCodes)))					;set attributeTag to the second element of the second Dxf code (assoc 2) of the entityDxfCodes variable
				(setq attributeValue(cdr(assoc 1 entityDxfCodes)))					;set attributeValue to the second element of the first Dxf code (assoc 1) of the entityDxfCodes variable

				(setq sublist (cons attributeTag attributeValue))
				(setq tagsAndValues (cons sublist TagsAndValues))
			  
				(setq entityNameForDrilling (entnext entityNameForDrilling))
		      		(setq entityDxfCodes (entget entityNameForDrilling))
				(setq dxfCode0 (cdr (assoc 0 entityDxfCodes )))
	    		)
			
			)												;progn wrap end
		)													;if end

		
		)													;progn wrap end
	)														;if end
	  

	
	(setq returnList tagsAndValues)  

	
)



