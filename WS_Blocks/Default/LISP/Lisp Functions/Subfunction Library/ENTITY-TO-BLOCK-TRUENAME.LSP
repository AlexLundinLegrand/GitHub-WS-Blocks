;;; --- ENTITY-TO-BLOCK-TRUENAME Sub Function---											
;;; Notes:																
;;; send entity name of block in 													
;;; the truename is returned to the caller												
;;; Alex Lundin 															
;;; 08-29-2017																
;;; Example call															
;;; (setq trueName (ENTITY-TO-BLOCK-TRUENAME entity))											

(defun ENTITY-TO-BLOCK-TRUENAME
       		(
		;argumentitys
		entity
		/
		;local variables
		DXFCODE0 *ERROR* ENTITYDXFCODES RETURNVALUE THECALLINGFUNCTIONSNAME TRUENAME VLAOBJECT
		)
  
;;; Arguments:																
;;;		entity - valid entityity name												
;;; Return:																
;;;		truename - effective name of block											
  
	;;; Wrapper for DEFAULT-ERROR-HANDLER											
	(defun *error* ( msg )
		(setq theCallingFunctionsName "ENTITY-TO-BLOCK-TRUENAME")
		(DEFAULT-ERROR-HANDLER theCallingFunctionsName msg)
	)
  
  	(setq entityDxfCodes (entget entity))											;set entityDxfCodes to the association list from entity
	(setq dxfCode0 (cdr (assoc 0 entityDxfCodes )))										;set dxfCode0 to the second elementity of the item that has 0 as it's first elementity, this is the entityity type
	
	(if															;- if
		(= dxfCode0 "INSERT")
		(progn														;-- progn wrap on if block
	
		(setq vlaobject (vlax-ename->vla-object entity))								;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entityity name stored in entity
		(setq truename(vlax-get-property vlaobject									;set the truename variable to the effective name property
		(if (vlax-property-available-p vlaobject 'effectivename)							;only if the property called effective name exisits inside block 
			'effectivename												;this entityire operation of setting truename fails when if statementity is false
			'name
			)
		)
		) 
		
		)														;-- progn wrap end
	)															;-if end
	
	(setq returnValue truename)												;last variable in function is returned to control



  
)


