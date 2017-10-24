; --- NOID Function ---
; move blocks with empty ROOM ID attribute from Wattstopper to NOID
; then move blocks with non empty ROOM ID attribute from NOID to Wattstopper
; Alex Lundin 05-24-2017
(defun C:NOID ( / blocks cntr en enlist group66 en2 enlist2 attTag attVal)

	(setvar "CMDECHO" 0)
	(command "_.Layer" "_Make" "NOID" "_Color" "30" "" "LType" "Continuous" "" "")					;create NOID LAYER
	(princ)														;supress command line statement
	(setq blocks(ssget "x" '((0 . "INSERT")(8 . "WATTSTOPPER"))))							;create selection set from all objects that are inserted blocks on the WATTSTOPPER layer 
	(setq cntr 0)													;initialize counter to 0 for while loop
  		(cond													;-conditional block
		  	((/= blocks nil)										;-when blocks are not nil
			(while 	(< cntr (sslength blocks))								;set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
				(setq en(ssname blocks cntr))								;set variable en to the name of the block that matches the current counter value
				(setq enlist(entget en))								;set the varaible enlist to the list of entities from the en varaible
				(setq group66(cdr(assoc 66 enlist)))							;set the varaible group66 to the second value of the 66th entitiy of the enlist variable
		  
					(cond	((= group66 1)								;start the conditional statement 1 loop when group66 varaible = 1
						(setq en2(entnext en))							;set en2 to the next sub entity of en
						(setq enlist2(entget en2))						;set enlist2 to the list of entities of en2
						(setq attTag(cdr(assoc 2 enlist2)))					;set attTag varaible to the second, plus all remaining values of the second member of enlist2
						(setq attVal(cdr(assoc 1 enlist2)))					;set attVal varaible to the second, plus all remaining values of the first memeber of enlist2
						 
							(if
							  	(AND							;-start nested and inside if
								(= attTag "ROOMID")(= attVal "")			;if the attTag is ROOMID and the attVal is nothing
								)							;-end nested and inside if
							  
						   		(command "_chprop" en "" "la" "NOID" "")		;change the layer of block en to NOID
							)					 
  															;end if
						) 	 								;end conditional statement 1
	 
					)										;end conditional statement block											
			(setq cntr(+ cntr 1))										;once while loop reaches this point, increment counter varaible by one			
	  		)												;end while loop
			)												;-end when blocks not nil conditional
		)													;-end conditional block

	(setq blocks(ssget "x" '((0 . "INSERT")(8 . "NOID"))))								;create selection set from all objects that are inserted blocks on the NOID layer 
	(setq cntr 0)													;initialize counter to 0 for while loop
  		(cond													;-conditional block
			((/= blocks nil)										;-conditional statement
				(while (< cntr (sslength blocks))							;--same structure as above while loop
					(setq en(ssname blocks cntr))									
					(setq enlist(entget en))									
					(setq group66(cdr(assoc 66 enlist)))								
			  
						(cond	((= group66 1)							;---conditional block									
							(setq en2(entnext en))								
							(setq enlist2(entget en2))							
							(setq attTag(cdr(assoc 2 enlist2)))						
							(setq attVal(cdr(assoc 1 enlist2)))						
						 
							(if
							  	(AND							;-start nested and inside if
								(= attTag "ROOMID")(/= attVal "")			;if the attTag is ROOMID and the attVal has something
								)							;-end nested and inside if
							  
						   		(command "_chprop" en "" "la" "WATTSTOPPER" "")		;change the layer of block en to NOID
							)
							 
							) 	 							;---end conditional statement 1
		 
						)									;---end conditional statement block											
					(setq cntr(+ cntr 1))														
		  		)											;--end while loop
			 )												;-conditional statement
		)													;-conditional block

	(command "clayer" "WATTSTOPPER")										;set current layer to WATTSTOPPER	
	(command "_.purge" "LA" "NOID" "N")										;purge NOID layer
	(prompt "\npurge all complete.")
	(princ)
	(setvar "CMDECHO" 1)   
)															;end NOID function





; --- NOID2 Function ---
; move blocks from NOID to Wattstopper
; won't check for attributes
; Alex Lundin 07-29-2016
(defun C:NOID2 ( / blocks cntr en enlist group66 en2 enlist2 attTag attVal)

	(setvar "CMDECHO" 0)
	(command "_.Layer" "_Make" "NOID" "_Color" "30" "" "LType" "Continuous" "" "")					;create NOID LAYER
	(princ)														;supress command line statement
	(setq blocks(ssget "x" '((0 . "INSERT")(8 . "NOID"))))								;create selection set from all objects that are inserted blocks on the WATTSTOPPER layer 
	(setq cntr 0)													;initialize counter to 0 for while loop
  		(cond													;-conditional block
		  	((/= blocks nil)										;-condtional statement
			(while 	(< cntr (sslength blocks))								;--set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
				(setq en(ssname blocks cntr))
			  	(command "_chprop" en "" "la" "WATTSTOPPER" "")						;change the layer of block en to NOID								
				(setq cntr(+ cntr 1))									;once while loop reaches this point, increment counter varaible by one			
	  		)												;--end while loop
			)												;-end conditional statement
		)													;-end conditional block

										

	(command "clayer" "WATTSTOPPER")										;set current layer to WATTSTOPPER	
	(command "_.purge" "LA" "NOID" "N")										;purge NOID layer
	(prompt "\npurge all complete.")
	(princ)
	(setvar "CMDECHO" 1)   
)															;end NOID2 function


