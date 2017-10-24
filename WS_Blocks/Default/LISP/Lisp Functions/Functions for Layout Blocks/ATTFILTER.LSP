; --- ATTFILTER Function ---
; Prompt user for string, search for all blocks with that entered in ROOMID
; Alex Lundin 08-09-2016
(defun C:ATTFILTER ( / ALLBLOCKS ATTENTEREDTAG ATTSTOREDVAL ATTTAG ATTVAL BLOCK CMAX CNTR EN EN2 ENLIST ENTTYPE WSBLOCKNAME ALLBLOCKS BLOCK CMAX CNTR)

  	(setq attEnteredTag (getstring "Enter Attribute Tag"))

  	(setq block (car(entsel "\n Select the block with desired atrribute values: ")))				;prompt user to select a title block, store the entity name to tb	
	(setq en2(entnext block))											;set en2 to ename of the group codes for tb
  	(setq enlist(entget en2))											;set enlist to the group codes of en2
  	(setq enttype 0)												;initialize entttype to 0 (makes while loop false by default)
  	(setq attTag 1)													;initialize attTag to 0 (makes condition false by default)
  	(setq enttype(cdr(assoc 0 enlist)))										;set enttype to the second element of the 0 group code in enlist
  		(while (= enttype "ATTRIB")										;loop while enttype is "ATTRIB"
			(setq attTag(cdr(assoc 2 enlist)))								;set attTag to the second element of the second associative property (a blocks attribute name) of enlist
  			(setq attVal(cdr(assoc 1 enlist)))								;set attVal to the second element of the first associative property (a value associated with the above attribute) of enlist
				(cond 	((/= attTag nil)								;conditional statement only when value is stored in attTag
					(if 	(= attTag attEnteredTag)
					  	(progn
					  	(setq attStoredVal attVal)
						)
					)
					)
				)
  			(setq en2(entnext en2))										;get the codes of the next sub entity of the titleblock (this will drill down farther with each loop)
			(setq enlist(entget en2))									;set enlist to the group codes of this new sub entity
			(setq enttype(cdr(assoc 0 enlist)))								;set the enttype to the second member of the first associative value of the group codes (entity type)
															;loop will repeat until group code is not equal to "ATTRIB"	  			
		)
  	

  
	(command "_.Layer" "_Make" "_MATCHES" "_Color" "4" "" "LType" "Continuous" "" "")					;create bridges layer


  	(setq allBlocks (ssget "X" '((0 . "INSERT"))))
	(setq cntr 0)
  
	(setq cntr 0)																	;set counter to 0
  	(setq cmax(sslength allBlocks))															;set counter max to total number of items in allBlocks) selection set
  	(while (< cntr cmax)																;while loop, continue until counter is greater than cmax
		(setq block (ssname allBlocks cntr))													;set block to the entity name (ssname function) of the ssHatchDel seleection set item that corresponds to the current value of the counter
		(cond																	;conditional block
		  	((/= block nil)															;first conditional when block is not nil
  			(setq en(entget block))														;set en variable to the entity name of the block variable
  			(setq wsBlockName (cdr(assoc 2 en)))												;set the wsBlockName variable to the second element (cdr function) of the 2 associative property (dxf code 2) of en (this is the block name)
			(setq en2(entnext block))											;set en2 to ename of the group codes for tb
		  	(setq enlist(entget en2))											;set enlist to the group codes of en2
		  	(setq enttype 0)												;initialize entttype to 0 (makes while loop false by default)
		  	(setq attTag 1)													;initialize attTag to 0 (makes condition false by default)
		  	(setq enttype(cdr(assoc 0 enlist)))										;set enttype to the second element of the 0 group code in enlist
		  		(while (= enttype "ATTRIB")										;loop while enttype is "ATTRIB"
					(setq attTag(cdr(assoc 2 enlist)))								;set attTag to the second element of the second associative property (a blocks attribute name) of enlist
				  		(cond
						  	((= attTag attEnteredTag)
				  			(setq attVal(cdr(assoc 1 enlist)))								;set attVal to the second element of the first associative property (a value associated with the above attribute) of enlist
								(cond
								  	((/= attTag nil)								;conditional statement only when value is stored in attTag
						       				(cond
										  	((= attVal attStoredVal)
											(command "_chprop" EN "" "la" "_MATCHES" "")
											)
										)
									)
								)
																	
							)
						)
				(setq en2(entnext en2))										;get the codes of the next sub entity of the titleblock (this will drill down farther with each loop)
				(setq enlist(entget en2))									;set enlist to the group codes of this new sub entity
				(setq enttype(cdr(assoc 0 enlist)))								;set the enttype to the second member of the first associative value of the group codes (entity type)
				)												;loop will repeat until group code is not equal to "ATTRIB"
			 
			)																	;end conditional
		)																		;end conditional block
	(setq cntr (+ 1 cntr))																	;increment counter at end of loop (moves counter so enHatchItem grabs next item from ssHatchDel set at beginning of loop with ssname function)
	)

  	
	  			
	
)

(defun C:ATTFILTER2 (/)
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "_MATCHES" ))))
	(setq cntr 0)
  
	(setq cntr 0)													;set counter to 0
  	(setq cmax(sslength allBlocks))											;set counter max to total number of items in allBlocks) selection set
  	(while (< cntr cmax)												;while loop, continue until counter is greater than cmax
		(setq block (ssname allBlocks cntr))									;set block to the entity name (ssname function) of the ssHatchDel seleection set item that corresponds to the current value of the counter

		(command "_chprop" block "" "la" "WATTSTOPPER" "")
															;end conditional block
	(setq cntr (+ 1 cntr))												;increment counter at end of loop (moves counter so enHatchItem grabs next item from ssHatchDel set at beginning of loop with ssname function)
	)														;repeat outer loop until reach i2max

      	(command "clayer" "WATTSTOPPER")										;purge command block, sets WATTSTOPPER current layer and purges BRIDGES layer
	(command "_.purge" "LA" "_MATCHES" "N")

)
															;end WSMTB2 function