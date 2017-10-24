;;; --- RELEASE-OBJECTS Sub Function---												
;;; Notes:															
;;; Use replace to replace subfunction RELEASE-OBJECTS name									
;;; This updates the build in error hanlders to give you information and graceful exits when your code terminates for any reason
;;; Developer Name														
;;; XX-XX-XXXX															
;;; Example call														
;;; (RELEASE-OBJECTS listOfObjectsToRelease)											
(defun RELEASE-OBJECTS
       		(
		;arguments
		listOfObjectsToRelease
		/
		;local variables
		COUNTER COUNTERMAX *ERROR* OBJECTFROMLIST THECALLINGFUNCTIONSNAME
		)
;;; Arguments:		
;;;			
;;; Return:		
;;;			
	
;;; Wrapper for DEFAULT-ERROR-HANDLER
	(defun *error* ( msg )
		(setq theCallingFunctionsName "RELEASE-OBJECTS")
		(DEFAULT-ERROR-HANDLER theCallingFunctionsName msg)
	)

;;; function body

  	(setq counter 0)												;initialize counter to 0 for while loop
	(setq counterMax (length listOfObjectsToRelease))								;set counterMax to length of objectFromLists in listOfObjectsToRelease
	  
	(while 	(< counter counterMax)											;set loop to continue while the counter varaible counter is less than counterMax
		(setq objectFromList(nth counter listOfObjectsToRelease))						;set variable objectFromList to the nth member from the listOfObjectsToRelease that matches the current counter value

	  	(if													;-if start
		  	(= (type objectFromList) "VLA-object")								;variable type is VLA-object
		  	(progn												;--progn start
	  		(vlax-release-object objectFromList)								;release the objectFromList
			)												;--progn end
		)													;-if end
	  
		(setq counter(+ counter 1))										;once while loop reaches this point, increment counter varaible by one
	)

)