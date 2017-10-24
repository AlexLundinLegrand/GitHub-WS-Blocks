; --- WSNEW ---
; Copy all attributes from a old title block
; Delete the old title block
; Purge title block definition from drawing
; Import new title block from WATTSTOPPER.dwt
; Paste all values with matching attributes into new title block
; Alex Lundin 03-22-2016
(defun C:WSNEW 	(/)													;declare all variables as global - so WSNEW2 can reach them
(setq tb (car(entsel "\n Select the block with desired atrribute values: ")))						;prompt user to select a title block, store the entity name to tb	
  	
	(setq en2(entnext tb))												;set en2 to ename of the group codes for tb
  	(setq enlist(entget en2))											;set enlist to the group codes of en2
  	(setq enttype 0)												;initialize entttype to 0 (makes while loop false by default)
  	(setq attTag 1)													;initialize attTag to 0 (makes condition false by default)
  	(setq enttype(cdr(assoc 0 enlist)))										;set enttype to the second element of the 0 group code in enlist
  		(while (= enttype "ATTRIB")										;loop while enttype is "ATTRIB"
			(setq attTag(cdr(assoc 2 enlist)))								;set attTag to the second element of the second associative property (a blocks attribute name) of enlist
  			(setq attVal(cdr(assoc 1 enlist)))								;set attVal to the second element of the first associative property (a value associated with the above attribute) of enlist
				(cond 	((/= attTag nil)								;conditional statement only when value is stored in attTag
					(setq Tags(append Tags(list attTag)))						;add the attTag memeber to the list Tags
					(setq Values(append Values(list attVal)))					;add the attVal member to the list Values
					)
				)
  			(setq en2(entnext en2))										;get the codes of the next sub entity of the titleblock (this will drill down farther with each loop)
			(setq enlist(entget en2))									;set enlist to the group codes of this new sub entity
			(setq enttype(cdr(assoc 0 enlist)))								;set the enttype to the second member of the first associative value of the group codes (entity type)
															;loop will repeat until group code is not equal to "ATTRIB"	  			
		)	
  


  	(entdel tb)													;delete the tite block after all values are stored
  	(command "PURGE" "A" "*" "N")											;purge all to remove title block defintion
	(C:WSLAYOUT)													;run WSLAYOUT function to insert title block from WATTSTOPPER.dwt	
)															;END WSNEW

(defun C:WSNEW2 (/)
(setq tb2 (car(entsel "\n Select the block to place values of similar attributes : ")))					;same exact structure as above, for a second title block

	(setq en3(entnext tb2))
	(setq enlist2(entget en3))
  	(setq enttype2 0)
  	(setq attTag2 1)
  	(setq enttype2(cdr(assoc 0 enlist2)))
  
		(while 	(= enttype2 "ATTRIB")
  			(setq attTag2(cdr(assoc 2 enlist2)))
  			(setq attVal2(cdr(assoc 1 enlist2)))
		  
				(cond	((/= attTag2 nil)
					(setq Tags2(append Tags2(list attTag2)))
					(setq Values2(append Values2(list attVal2)))
					)
				)
		  
  			(setq en3(entnext en3))
			(setq enlist2(entget en3))
			(setq enttype2(cdr(assoc 0 enlist2)))
						  		
		)

(setq i 0)														;initialize counter varaibles to 0
(setq i2 0)														
(setq imax (length Tags))												;set imax to the total length (# of members) of the Tags list
(setq i2max (length Tags2))												;set i2max to the total length (# of members) of the Tag2 list
  

(setq en3(entnext tb2))													;priming read for while loop
(setq enlist2(entget en3))												;same structure as above, we are restarting the values from the beginning
(setq attTag2(cdr(assoc 2 enlist2)))											;of tb2 dxf codes
(setq attVal2(cdr(assoc 1 enlist2)))		
  
(while (< i2 i2max)													;outer loop continues until we reach i2max
	  
		(while (< i  imax)											;inner loop continues until we reach imax

		  			(cond	((equal (nth i Tags) attTag2)						;compare the member of Tags, that corresponds to the current i value, to the attTag2 -->  procede to conditional operation if same
						(entmod (subst (cons 1 (nth i Values)) (assoc 1 enlist2) enlist2)) 	;use entmod function to substitute the member of Values, that corresponds to the current i value, in for the attribute value of enlist2
						)
					)										;end conditional operation
	  
			(setq i(1+ i))											;increment i by 1 --> this will move the inner loop to the next value
		)													;repeat inner until reach imax													
	  
	(setq en3(entnext en3))
	(setq enlist2(entget en3))
	(setq attTag2(cdr(assoc 2 enlist2)))
	(setq attVal2(cdr(assoc 1 enlist2)))
  	(setq i 0)													;reset i --> this restarts inner loop every time we reach the outer loop
	(setq i2(1+ i2))												;increment i2 by 1 --> this will move the inner loop to the next value
	)														;repeat outer loop until reach i2max														

)															;END WSNEW2
