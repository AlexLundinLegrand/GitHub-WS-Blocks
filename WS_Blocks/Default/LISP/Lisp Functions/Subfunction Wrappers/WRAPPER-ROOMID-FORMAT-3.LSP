;;; --- WRAPPER-ROOMID-FORMAT-3 Sub Function---												
;;; Notes:																
;;; send selection set in through selectionSet variable from calling function								
;;; nothing is returned to caller													
;;; Alex Lundin																
;;; 09-01-2017																
;;; example call															
;;; (WRAPPER-ROOMID-FORMAT-3 selectionSet)												

(defun	WRAPPER-ROOMID-FORMAT-3
		(
		;arguments
		selectionSet
		/
		;local variables
		ATTRIBUTETAG ATTRIBUTEVALUE COUNTER COUNTERMAX DXFCODE-1 DXFCODE0 DXFCODE2 DXFCODE66 DXFCODE8 ENTITY ENTITYDXFCODES ENTITYNAMEFORDRILLING *ERROR* TRUENAME
		)
  
;;; Arguments:																
;;;	selectionSet - any valid selection set												
;;; Return:																
;;;	None																
  

	;;; Wrapper for DEFAULT-ERROR-HANDLER												
	(defun *error* ( msg )
		(setq theCallingFunctionsName "WRAPPER-ROOMID-FORMAT-3")
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
	  	(setq dxfCode-1 (cdr (assoc -1 entityDxfCodes )))							;set dxfCode-1 to the second element of the item that has -1 as it's first element, this is the entity name
		(setq dxfCode0 (cdr (assoc 0 entityDxfCodes )))								;set dxfCode0 to the element of the item that has 0 as it's first element, this is the entity type
		(setq dxfCode2 (cdr (assoc 2 entityDxfCodes )))								;set dxfCode8 to the second element of the item that has 8 as it's first element, this is the name, or block name
		(setq dxfCode8 (cdr (assoc 8 entityDxfCodes )))								;set dxfCode8 to the second element of the item that has 8 as it's first element, this is the layer
		(setq dxfCode66 (cdr (assoc 66 entityDxfCodes )))							;set dxfCode66 to the second element of the item that has 66 as it's first element, this is the attribute flag
	  
		;;; ENTITY-TO-BLOCK-TRUENAME sub-function call
	  	;;; AutoCAD is able to change a blocks name for some other functionality used with dynamic blocks
	  	;;; this makes finding blocks based by dxfCode challanging
	  	;;; this truename filter can use some vlax functions to get the effective name, aka truename
		(setq trueName (ENTITY-TO-BLOCK-TRUENAME entity))	  

 
		(setq entityNameForDrilling entity) 

		(if													;if start
		  	(= dxfCode66 1)											;entity attribute flag is 1
		  	(progn												;progn wrap
			  
			;;; format all ROOMID's on the WATTSTOPPER layer, even blocks that are going to be moved to the 0 layer
			;;; these are the LCAP product line
			(while 	(/= dxfCode0 "SEQEND" )									;while loop to drill to each sub entity in a block
				(setq attributeTag(cdr(assoc 2 entityDxfCodes)))					;set attributeTag to the second element of the second Dxf code (assoc 2) of the entityDxfCodes variable
				(setq attributeValue(cdr(assoc 1 entityDxfCodes)))					;set attributeValue to the second element of the first Dxf code (assoc 1) of the entityDxfCodes variable
					(cond 	((= attributeTag "ROOMID" )						;outer conditional for when the blocks attributeTag is "ROOMID"

															;this if statement formats the attribute value, and we only reach this point if the attribute tage is ROOMID
						(if	(/= attributeValue "")
						  	(progn
					  		(setq attributeValue (ROOMID-STRING-FORMATER attributeValue))	;call ROOMID-STRING-FORMATER subfunction on the attributeValue variable
								(setq entityDxfCodes					;entmod routine to place formated string back into block
								(subst (cons 1 attributeValue)
								(assoc 1 entityDxfCodes)            			; Changes the attribute value group in entityDxfCodes.
								entityDxfCodes                      			; to the formated string now stored in attributeValue
								)
								)
								(entmod entityDxfCodes)
							
							)
					  	)
						 


					)
				)
			(setq entityNameForDrilling (entnext entityNameForDrilling))
	      		(setq entityDxfCodes (entget entityNameForDrilling))
			(setq dxfCode0 (cdr (assoc 0 entityDxfCodes )))
	    		)
			
			)												;progn wrap end
		)													;if end
	  
		(setq counter(+ counter 1))										;once while loop reaches this point, increment counter varaible by one
	)
	
	
	
)