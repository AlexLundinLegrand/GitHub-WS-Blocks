;;; --- WRAPPER-CUSTOM-ENTMOD-2 Sub Function---												
;;; Notes:																
;;; send entities in through entityList variable from calling function									
;;; nothing is returned to caller													
;;; Alex Lundin																
;;; 09-01-2017																
;;; example call															
;;; (WRAPPER-CUSTOM-ENTMOD-2 associationListNumber associationListValue entityList)							

(defun	WRAPPER-CUSTOM-ENTMOD-2
		(
		;arguments
		associationListNumber associationListValue entityList 
		/
		;local variables
		ATTRIBUTETAG ATTRIBUTEVALUE COUNTER COUNTERMAX DXFCODE-1 DXFCODE0 DXFCODE2 DXFCODE66 DXFCODE8 ENTITY ENTITYDXFCODES ENTITYNAMEFORDRILLING *ERROR* TRUENAME
		)
  
;;; Arguments:																
;;;	entityList 		- any valid list of entity names									
;;;	associationListNumber 	- dxf code number to modify										
;;;	associationListValue	- value to put into dxf association list								
;;; Return:																
;;;	None																
  

	;;; Wrapper for DEFAULT-ERROR-HANDLER
	(defun *error* ( msg )
		(setq theCallingFunctionsName "WRAPPER-CUSTOM-ENTMOD-2")
		(DEFAULT-ERROR-HANDLER theCallingFunctionsName msg)
	)


  
  	(setq counter 0)												;initialize counter to 0 for while loop
  	(if														;if
	  	(/= entityList nil)											;set is not nil
		(progn													;progn wrap
  		(setq counterMax (length entityList))									;set counterMax to length of items in entityList
		)													;progn wrap end
	)														;if end
	  
	(while 	(< counter counterMax)											;set loop to continue while the counter varaible counter is less than counterMax
		(setq entity(nth counter entityList))									;set variable entity to the name of the item from the selection that matches the current counter value

	  	(setq entityDxfCodes (entget entity))
	  	;;; you can use the method here to find any value from a dxfCodecode
	  	(setq dxfCode-1 (cdr (assoc -1 entityDxfCodes )))							;set dxfCode-1 to the second element of the item that has -1 as it's first element, this is the entity name
		(setq dxfCode0 (cdr (assoc 0 entityDxfCodes )))								;set dxfCode0 to the element of the item that has 0 as it's first element, this is the entity type
		(setq dxfCode2 (cdr (assoc 2 entityDxfCodes )))								;set dxfCode8 to the second element of the item that has 8 as it's first element, this is the name, or block name
		(setq dxfCode8 (cdr (assoc 8 entityDxfCodes )))								;set dxfCode8 to the second element of the item that has 8 as it's first element, this is the layer
		(setq dxfCode66 (cdr (assoc 66 entityDxfCodes )))							;set dxfCode66 to the second element of the item that has 66 as it's first element, this is the attribute flag
	 	  

		(setq entityNameForDrilling entity) 

		(CUSTOM-ENTMOD entity entityNameForDrilling associationListNumber associationListValue)



	  	;;; area under construction for handling entities inside a block definition
	  
;;;	  	;;; if entity is a block
;;;		(if
;;;		  	(= dxfCode0 "INSERT")
;;;		  	(progn
;;;			
;;;			;;; ENTITY-TO-BLOCK-TRUENAME sub-function call
;;;			(setq trueName (ENTITY-TO-BLOCK-TRUENAME entity))
;;;
;;;			;;; this is where the body of the entmod would go for the block subentities
;;;			(while 	(/= entityNameForDrilling nil )								;while loop to drill to each sub entity in a block
;;;		      		(CUSTOM-ENTMOD entity entityNameForDrilling associationListNumber associationListValue)
;;;				(setq entityNameForDrilling (entnext entityNameForDrilling))
;;;	    		)
;;;			
;;;			)
;;;		  
;;;		  	;;; else entity is not a block
;;;		  	(progn
;;;			  
;;;			(while 	(/= entityNameForDrilling nil )								;while loop to drill to each sub entity in a block
;;;			  	(setq nilEntityForNonBlocks nil)
;;;		      		(CUSTOM-ENTMOD entityNameForDrilling nilEntityForNonBlocks associationListNumber associationListValue)
;;;				(setq entityNameForDrilling (entnext entityNameForDrilling))
;;;	    		)
;;;			
;;;			)
;;;		)


	  
		(setq counter(+ counter 1))										;once while loop reaches this point, increment counter varaible by one
	)
	
	
)