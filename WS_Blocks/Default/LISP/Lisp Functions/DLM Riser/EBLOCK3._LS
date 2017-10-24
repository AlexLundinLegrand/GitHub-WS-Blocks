; --- EBLOCK3 Function ---
; Moves all network cabinets to 0 layer
; Format the ROOMID attribute in all the blocks on the Wattstopper layer
; Put the formated ROOMID back into the AutoCAD block
; Get SEGMENT and POSITION attribute from the network blocks
; Assemble all information in a dotted list
; Traverse the list and export each item individually
; Export ROOMID to text file
; Export BLOCKNAME to text file
; Export SEGMENT and POSITION to text file
; Alex Lundin 12-08-2016
(defun c:eblock3 (/ A FORMATEDBLOCKCOUNTER ALLBLOCKS ALLBLOCKS2 ATTBRIDGETAG ATTBRIDGEVAL ATTTAG ATTVAL BLOCK BLOCKNAME BRIDGEPOSITION BRIDGESEGMENT BRIDGESUBDXF BRIDGESUBENTNAME CMAX CNTR EN ENTDXF ENTNAME LINEPOSITION LINESEGMENT N NMAX TRUENAME VLAOBJECT WSBLOCKNAME SelSet f1 File#1 Counter blockSublist blockDottedPairs blockDottedPairsAlphabetical lineItem lineAttVal lineBlock)
	(vl-load-com)
  	(setvar "CMDECHO" 0)
  	(setq formatedblockcounter 0)
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "WATTSTOPPER" ))))
	(setq cntr 0)
  
	(setq cntr 0)																	;set counter to 0
  	(setq cmax(sslength allBlocks))															;set counter max to total number of items in allBlocks) selection set
  	(while (< cntr cmax)																;while loop, continue until counter is greater than cmax
		(setq block (ssname allBlocks cntr))													;set block to the entity name (ssname function) of the ssHatchDel seleection set item that corresponds to the current value of the counter
		(cond																	;conditional block
		  	((/= block nil)															;first conditional when block is not nil
  			(setq en(entget block))														;set en variable to the entity name of the block variable
  			(setq wsBlockName (cdr(assoc 2 en)))												;set the wsBlockName variable to the second element (cdr function) of the 2 associative property (dxf code 2) of en (this is the block name)


  			(setq vlaobject (vlax-ename->vla-object block))											;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of block
    			(setq truename(vlax-get-property vlaobject											;set the truename variable  name of block
        			(if (vlax-property-available-p vlaobject 'effectivename)								;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
            			'effectivename														;to the effective name property
            			'name
        			)
    			)
			)																;END IF

;;;this nested if/or block catches all items that are not extracted to the Riser_Extraction text file
;;;this block of code moves them to the 0 layer and the code will move them back at the end
		  		(if															;--nested if
					  	(OR 	(= truename "LMSM-3E")(= truename "LMSM-6E")(= truename "LMNC")					;---or inside the if
						    	(= truename "LMSM-201")(= truename "LMSM-600")(= truename "LMSM-603")
							(= truename "GENERIC-SQUARE")(= truename "GENERIC-CIRCLE")(= truename "GENERIC-SUN")(= truename "GENERIC-X")(= truename "GENERIC-SWITCH")
							(= truename "V-EASYTOUCH-II-1")
							(= truename "V-EASYTOUCH-II-2")
							(= truename "V-EASYTOUCH-II-3")
							(= truename "V-EASYTOUCH-II-4")
							(= truename "V-EASYTOUCH-II-5")
							(= truename "V-EQ40TB-TI")
							(= truename "V-EQ73TB-TI")
							(= truename "V-EQ41TB-TI")
							(= truename "V-DMX-DALI-GW")
							(= truename "V-EM-LIGHTSENSOR")
							(= truename "V-LCAP44A")
							(= truename "V-LCAP44L")
							(= truename "V-LVOS")
							(= truename "V-IC-DIN-II-LITE-RF")
							(= truename "V-IC-DIN-II-LITE")
							(= truename "V-STPSRW101")
							(= truename "V-STPSRW201")
							(= truename "V-STPERW101")
							(= truename "V-STPERW201")
							(= truename "V-IC-36")
							(= truename "V-LCAP44HS")
							(= truename "V-LCAP32S")
							(= truename "V-LCAP44S")
							(= truename "V-LCAP44H")
							(= truename "V-LCAP44M")
							(= truename "V-LCAP32M")
							)
							(progn												;----progn inside the if and or 
									(setq en									;entmod routine to move objects layer
									(subst (cons 8 "0")
									(assoc 8 en)            							; Changes the layer group in en.
									en                      							; to layer "0"
									)
									)
									(entmod en) 
							)
				)															;--end nested if
			 

			)																;end conditional
		)																	;end conditional block
	(setq cntr (+ 1 cntr))																;increment counter at end of loop (moves counter so enHatchItem grabs next item from ssHatchDel set at beginning of loop with ssname function)
	)



	
  
  	(if (setq SelSet (ssget "X" '((0 . "INSERT") (8 . "WATTSTOPPER" ) (66 . 1 )))) 										;create selection set from all blocks on Wattstopper Layter
    	(progn																			;progn to group all following statements into one block
	  	(setq f1 (open (strcat (getvar 'DWGPREFIX) "Riser_Extraction_3.txt")  "a"))									;set f1 to to result of open on the string created from dwgprefix variable plus "Riser_Extraction.txt" open file for appending 
	  	(close f1)																	;close f1
      		(if (setq File#1 (open (strcat (getvar 'DWGPREFIX) "Riser_Extraction_3.txt") "w" ) )								;set File#1 variable to the same thing as f1
        		(progn																	;progn to group all following statements into one block
          		(setq Counter -1 )															;set counter to -1
          		(write-line (strcat (getvar "DWGPREFIX" ) (getvar "DWGNAME" ) " -found " (itoa (sslength SelSet)) " block(s) with attributes" ) File#1)	;write first line of file (this contains information about the drawing we extract from)
          		(repeat (sslength SelSet )														;repeat statement for the entire length of blocks SelectionSet
			(setq Counter (1+ Counter))														;increment counter
			(setq EntName (ssname SelSet Counter))													;set EntName to current Counter value of SelSet
			(setq EntDxf (entget EntName))														;set EntDxf to the Dxf codes of EntName
	    		(setq BlockName (entget EntName))													;set BlockName to the Dxf codes of EntName
  			(setq vlaobject (vlax-ename->vla-object EntName))											;set vlaobject to the EntName converted into a VLA object
    			(setq BlockName(vlax-get-property vlaobject												;get the true name of a block if it is anonymous
        			(if (vlax-property-available-p vlaobject 'effectivename)
            			'effectivename
            			'name
        			)
    			)
			)																	;end of true name sub function

            			(while 	(/= (cdr (assoc 0 EntDxf )) "SEQEND" )											;while loop to drill to each sub entity in a block
	      				(setq attTag(cdr(assoc 2 EntDxf)))											;set attTag to the second element of the second Dxf code (assoc 2) of the EntDxf variable
	      				(setq attVal(cdr(assoc 1 EntDxf)))											;set attVal to the second element of the first Dxf code (assoc 1) of the EntDxf variable
              					(cond 	((= attTag "ROOMID" )											;outer conditional for when the blocks attTag is "ROOMID"

																				;this if statement formats the attribute value, and we only reach this point if the attribute tage is ROOMID
							(if	(/= attVal "")
							  	(progn
						  		(setq attVal (roomnameformat attVal))										;call roomnameformat subfunction on the attVal variable
;;;							  	(setq attVal formattedstring)									;set attVal to formattedstring, which is global from roomnameformat subfunction
									(setq EntDxf										;entmod routine to place formated string back into block
									(subst (cons 1 attVal)
									(assoc 1 EntDxf)            								; Changes the attribute value group in EntDxf.
									EntDxf                      								; to the formated string now stored in attVal
									)
									)
									(entmod EntDxf)
								
								)
						  	)
							 



							(cond	((= BlockName "LMBC-300" )									;inner conditional (1)
								(setq BridgeSubEntName (entnext EntName))							;set BridgeSubEntName to the next entitiy of EntName
								(setq BridgeSubDxf (entget BridgeSubEntName))							;set BridgeSubDxf to the entity codes of BridgeSubEntName
									(while 	(/= (cdr (assoc 0 BridgeSubDxf )) "SEQEND" )					;inner while loop while there are still sub entities in the block to drill to
								
								
	      								(setq attBridgeTag(cdr(assoc 2 BridgeSubDxf)))						;set attBridgeTag to the second element of the second Dxf code (assoc 2) of BridgeSubDxf variable
	      								(setq attBridgeVal(cdr(assoc 1 BridgeSubDxf)))						;set attBridgeVal to the second element of the first Dxf code (assoc 1) of BridgeSubDxf variable
										(if 	(= attBridgeTag "SEGMENT")						;if statement (only store the attribute value if the attribute tage is "SEGMENT")
											(setq bridgeSegment(cdr(assoc 1 BridgeSubDxf)))
										)
										(if 	(= attBridgeTag "POSITION")						;if statement (only store the attribute value if the attribute tage is "POSITION")
											(setq bridgePosition(cdr(assoc 1 BridgeSubDxf)))
										)
								  
								
									(setq BridgeSubEntName (entnext BridgeSubEntName))					;get next sub entity name from the block
									(setq BridgeSubDxf (entget BridgeSubEntName))						;get the Dxf codes from this new sub entity (loop breaks when there are no more)(assoc 0 = "SEQEND")
									)
								(setq blockSublist (list attVal BlockName bridgeSegment bridgePosition))			;create the blockSublist from each value pulled off the LMBC-300
							
								(setq blockDottedPairs (cons blockSublist blockDottedPairs))					;add the blockSublist to the blockDottedPairs
								)

								((= BlockName "LMCP48" )									
								(setq BridgeSubEntName (entnext EntName))							
								(setq BridgeSubDxf (entget BridgeSubEntName))							
									(while 	(/= (cdr (assoc 0 BridgeSubDxf )) "SEQEND" )					
								
								
	      								(setq attBridgeTag(cdr(assoc 2 BridgeSubDxf)))						
	      								(setq attBridgeVal(cdr(assoc 1 BridgeSubDxf)))						
										(if 	(= attBridgeTag "SEGMENT")					
										  	(setq bridgeSegment(cdr(assoc 1 BridgeSubDxf)))
										)
										(if 	(= attBridgeTag "POSITION")						
											(setq bridgePosition(cdr(assoc 1 BridgeSubDxf)))
										)
								  
								
									(setq BridgeSubEntName (entnext BridgeSubEntName))					
									(setq BridgeSubDxf (entget BridgeSubEntName))						
									)
								(setq blockSublist (list attVal BlockName bridgeSegment bridgePosition))			
							
								(setq blockDottedPairs (cons blockSublist blockDottedPairs))				
								)

							

								((= BlockName "LMCP24" )									
								(setq BridgeSubEntName (entnext EntName))							
								(setq BridgeSubDxf (entget BridgeSubEntName))							
									(while 	(/= (cdr (assoc 0 BridgeSubDxf )) "SEQEND" )					
								
								
	      								(setq attBridgeTag(cdr(assoc 2 BridgeSubDxf)))						
	      								(setq attBridgeVal(cdr(assoc 1 BridgeSubDxf)))						
										(if 	(= attBridgeTag "SEGMENT")					
										  	(setq bridgeSegment(cdr(assoc 1 BridgeSubDxf)))
										)
										(if 	(= attBridgeTag "POSITION")						
											(setq bridgePosition(cdr(assoc 1 BridgeSubDxf)))
										)
								  
								
									(setq BridgeSubEntName (entnext BridgeSubEntName))					
									(setq BridgeSubDxf (entget BridgeSubEntName))						
									)
								(setq blockSublist (list attVal BlockName bridgeSegment bridgePosition))			
							
								(setq blockDottedPairs (cons blockSublist blockDottedPairs))				
								)
							

								

							

								((= BlockName "LMCP12" )									
								(setq BridgeSubEntName (entnext EntName))							
								(setq BridgeSubDxf (entget BridgeSubEntName))							
									(while 	(/= (cdr (assoc 0 BridgeSubDxf )) "SEQEND" )					
								
								
	      								(setq attBridgeTag(cdr(assoc 2 BridgeSubDxf)))						
	      								(setq attBridgeVal(cdr(assoc 1 BridgeSubDxf)))						
										(if 	(= attBridgeTag "SEGMENT")					
										  	(setq bridgeSegment(cdr(assoc 1 BridgeSubDxf)))
										)
										(if 	(= attBridgeTag "POSITION")						
											(setq bridgePosition(cdr(assoc 1 BridgeSubDxf)))
										)
								  
								
									(setq BridgeSubEntName (entnext BridgeSubEntName))					
									(setq BridgeSubDxf (entget BridgeSubEntName))						
									)
								(setq blockSublist (list attVal BlockName bridgeSegment bridgePosition))			
							
								(setq blockDottedPairs (cons blockSublist blockDottedPairs))				
								)
							

								

							
						

								((= BlockName "LMCP8" )									
								(setq BridgeSubEntName (entnext EntName))							
								(setq BridgeSubDxf (entget BridgeSubEntName))							
									(while 	(/= (cdr (assoc 0 BridgeSubDxf )) "SEQEND" )					
								
								
	      								(setq attBridgeTag(cdr(assoc 2 BridgeSubDxf)))						
	      								(setq attBridgeVal(cdr(assoc 1 BridgeSubDxf)))						
										(if 	(= attBridgeTag "SEGMENT")					
										  	(setq bridgeSegment(cdr(assoc 1 BridgeSubDxf)))
										)
										(if 	(= attBridgeTag "POSITION")						
											(setq bridgePosition(cdr(assoc 1 BridgeSubDxf)))
										)
								  
								
									(setq BridgeSubEntName (entnext BridgeSubEntName))					
									(setq BridgeSubDxf (entget BridgeSubEntName))						
									)
								(setq blockSublist (list attVal BlockName bridgeSegment bridgePosition))			
							
								(setq blockDottedPairs (cons blockSublist blockDottedPairs))				
								)

								
							

								(t									
								(setq blockSublist (list attVal BlockName))							
								(setq blockDottedPairs (cons blockSublist blockDottedPairs))					
							
								)

							)
						)
              				)
			(setq EntName (entnext EntName))
              		(setq EntDxf (entget EntName))								
            		)
			(setq blockSublist nil)															;reset the sublist when moving to the next block
          	)

  	(setq blockDottedPairsAlphabetical (vl-sort blockDottedPairs (function (lambda (x y)(< (car x)(car y))))))						;custom function with vl-sort and lambda, if x is less than y, put x before y
	(setq n 0)																		;set n to 0
	(setq nmax  (length blockDottedPairsAlphabetical))													;set nmax to length of blockDottedPairsAlphabetical
  	(while (< n nmax)																	;loop for while n is less than n max

	  	(setq lineItem (nth n blockDottedPairsAlphabetical))												;set lineItem to the current n of blockDottedPairsAlphabetical
	  	(setq lineAttVal (car lineItem))														;set lineAttVal to the first element of lineItem
	  	(setq lineBlock (cadr lineItem))														;set lineBlock to the second elemtn of lineItem
;;;	  	format block name in text file only, can't use entmod to change block name in the same way that we can change the attribute value
;;;	  	this only capitalizes the block name in the text file
;;;	  	this will not capitalize the block name in the drawing file
;;;		(stringuppercase lineBlock)
;;;	  	(setq lineBlock (stringuppercase lineBlock))
	  		  	(cond 	((= lineBlock "LMBC-300")												;conditional for LMBC-300
					(setq lineSegment (caddr lineItem))											;set lineSegment to the third element of lineItem
		 			(setq linePosition (cadddr lineItem))											;set linePosition to the fourth element of lineItem
					(write-line (strcat lineAttVal) File#1 )										;write 4 peices of info
	  				(write-line (strcat lineBlock) File#1 )
					(write-line (strcat lineSegment) File#1 )
					(write-line (strcat linePosition) File#1 )
					)

					((= lineBlock "LMCP48")													;same structure as above
					(setq lineSegment (caddr lineItem))											
		 			(setq linePosition (cadddr lineItem))										
					(write-line (strcat lineAttVal) File#1 )										
	  				(write-line (strcat lineBlock) File#1 )
					(write-line (strcat lineSegment) File#1 )
					(write-line (strcat linePosition) File#1 )
					)
					
					((= lineBlock "LMCP24")													
					(setq lineSegment (caddr lineItem))											
		 			(setq linePosition (cadddr lineItem))										
					(write-line (strcat lineAttVal) File#1 )										
	  				(write-line (strcat lineBlock) File#1 )
					(write-line (strcat lineSegment) File#1 )
					(write-line (strcat linePosition) File#1 )
					)

					((= lineBlock "LMCP12")													
					(setq lineSegment (caddr lineItem))											
		 			(setq linePosition (cadddr lineItem))										
					(write-line (strcat lineAttVal) File#1 )										
	  				(write-line (strcat lineBlock) File#1 )
					(write-line (strcat lineSegment) File#1 )
					(write-line (strcat linePosition) File#1 )
					)

					((= lineBlock "LMCP8")													
					(setq lineSegment (caddr lineItem))											
		 			(setq linePosition (cadddr lineItem))										
					(write-line (strcat lineAttVal) File#1 )										
	  				(write-line (strcat lineBlock) File#1 )
					(write-line (strcat lineSegment) File#1 )
					(write-line (strcat linePosition) File#1 )
					)
					
					(t															;else conditional (for all blocks besides LMBC-300's)

					(write-line (strcat lineAttVal) File#1 )										;write 2 peices of info
	  				(write-line (strcat lineBlock) File#1 )
					 
					)
				)
	  	(setq n (+ 1 n))																;increment n

	)


	  (setvar "CMDECHO" 1)
          (close File#1 )																	;close output file
          (princ "Done ! " )
        )
        (princ ". . file not found " )
      )
    )
    (princ "No legal block found " )																;error handling for no blocks
  )

    	(setq allBlocks2 (ssget "X" '((0 . "INSERT")(8 . "0" ))))
	(setq cntr 0)
  
	(if	(/= allBlocks2 nil)
	  	(progn
	  	(setq cmax(sslength allBlocks2))															;set counter max to total number of items in allBlocks) selection set
	  	(while 	(< cntr cmax)																;while loop, continue until counter is greater than cmax
			(setq block (ssname allBlocks2 cntr))													;set block to the entity name (ssname function) of the ssHatchDel seleection set item that corresponds to the current value of the counter
			(cond																	;conditional block
			  	((/= block nil)															;first conditional when block is not nil
	  			(setq en(entget block))														;set en variable to the entity name of the block variable
	  			(setq wsBlockName (cdr(assoc 2 en)))												;set the wsBlockName variable to the second element (cdr function) of the 2 associative property (dxf code 2) of en (this is the block name)


	  			(setq vlaobject (vlax-ename->vla-object block))											;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of block
	    			(setq truename(vlax-get-property vlaobject											;set the truename variable  name of block
	        			(if (vlax-property-available-p vlaobject 'effectivename)								;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
	            			'effectivename														;to the effective name property
	            			'name
	        			)
	    			)
				)																;END IF
			  		(if															;--nested if
						  	(OR 	(= truename "LMSM-3E")(= truename "LMSM-6E")(= truename "LMNC")					;--or inside the if
							    	(= truename "LMSM-201")(= truename "LMSM-600")(= truename "LMSM-603")
								(= truename "GENERIC-SQUARE")(= truename "GENERIC-CIRCLE")(= truename "GENERIC-SUN")(= truename "GENERIC-X")(= truename "GENERIC-SWITCH")
								(= truename "V-EASYTOUCH-II-2")
								(= truename "V-EASYTOUCH-II-3")
								(= truename "V-EASYTOUCH-II-4")
								(= truename "V-EASYTOUCH-II-5")
								(= truename "V-EQ40TB-TI")
								(= truename "V-EQ73TB-TI")
								(= truename "V-EQ41TB-TI")
								(= truename "V-DMX-DALI-GW")
								(= truename "V-EM-LIGHTSENSOR")
								(= truename "V-LCAP44A")
								(= truename "V-LCAP44L")
								(= truename "V-LVOS")
								(= truename "V-IC-DIN-II-LITE-RF")
								(= truename "V-IC-DIN-II-LITE")
								(= truename "V-STPSRW101")
								(= truename "V-STPSRW201")
								(= truename "V-STPERW101")
								(= truename "V-STPERW201")
								(= truename "V-IC-36")
								(= truename "V-LCAP44HS")
								(= truename "V-LCAP32S")
								(= truename "V-LCAP44S")
								(= truename "V-LCAP44H")
								(= truename "V-LCAP44M")
								(= truename "V-LCAP32M")
							)
								(progn												;----progn inside the if and or 
										(setq en									;entmod routine to move objects layer
										(subst (cons 8 "WATTSTOPPER")
										(assoc 8 en)            							; Changes the layer group in en.
										en                      							; to layer WATTSTOPPER.
										)
										)
										(entmod en) 
								)
					)															;--end nested if

				)																;end conditional
			)																	;end conditional block
		(setq cntr (+ 1 cntr))																;increment counter at end of loop (moves counter so enHatchItem grabs next item from ssHatchDel set at beginning of loop with ssname function)
		)

		)																		;end progn to wrap the if
	)																			;end if

	(setq a "\nEBLOCK made changes to your drawing
		\n
		\nIn AutoCAD, ROOMIDs must only contain these characters:
	    	\nCapital letters A thru Z.
	    	\nNumbers 0 thru 9.
	    	\nUnderscores _
	    	\n
	    	\nString formatting functions edited your ROOMIDs that do not match the above criteria.
	    	\nTotal number of blocks fixed:
	")
	  (if	(/= formatedblockcounter 0)
	  	(alert (strcat a "\n" (itoa formatedblockcounter)))
	  )

  (princ)
)

; --- eblockvantage Function ---
; Moves all network cabinets to 0 layer
; Format the ROOMID attribute in all the blocks on the Wattstopper layer
; Put the formated ROOMID back into the AutoCAD block
; Get SEGMENT and POSITION attribute from the network blocks
; Assemble all information in a dotted list
; Traverse the list and export each item individually
; Export ROOMID to text file
; Export BLOCKNAME to text file
; Export SEGMENT and POSITION to text file
; Alex Lundin 12-08-2016
(defun c:eblockvantage (/ A FORMATEDBLOCKCOUNTER ALLBLOCKS ALLBLOCKS2 ATTBRIDGETAG ATTBRIDGEVAL ATTTAG ATTVAL BLOCK BLOCKNAME BRIDGEPOSITION BRIDGESEGMENT BRIDGESUBDXF BRIDGESUBENTNAME CMAX CNTR EN ENTDXF ENTNAME LINEPOSITION LINESEGMENT N NMAX TRUENAME VLAOBJECT WSBLOCKNAME SelSet f1 File#1 Counter blockSublist blockDottedPairs blockDottedPairsAlphabetical lineItem lineAttVal lineBlock)
	(vl-load-com)
  	(setvar "CMDECHO" 0)



    	(setq allBlocks2 (ssget "X" '((0 . "INSERT")(8 . "0" ))))
	(setq cntr 0)
  
	(if	(/= allBlocks2 nil)
	  	(progn
	  	(setq cmax(sslength allBlocks2))															;set counter max to total number of items in allBlocks) selection set
	  	(while 	(< cntr cmax)																;while loop, continue until counter is greater than cmax
			(setq block (ssname allBlocks2 cntr))													;set block to the entity name (ssname function) of the ssHatchDel seleection set item that corresponds to the current value of the counter
			(cond																	;conditional block
			  	((/= block nil)															;first conditional when block is not nil
	  			(setq en(entget block))														;set en variable to the entity name of the block variable
	  			(setq wsBlockName (cdr(assoc 2 en)))												;set the wsBlockName variable to the second element (cdr function) of the 2 associative property (dxf code 2) of en (this is the block name)


	  			(setq vlaobject (vlax-ename->vla-object block))											;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of block
	    			(setq truename(vlax-get-property vlaobject											;set the truename variable  name of block
	        			(if (vlax-property-available-p vlaobject 'effectivename)								;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
	            			'effectivename														;to the effective name property
	            			'name
	        			)
	    			)
				)																;END IF
										(setq en									;entmod routine to move objects layer
										(subst (cons 8 "WATTSTOPPER")
										(assoc 8 en)            							; Changes the layer group in en.
										en                      							; to layer WATTSTOPPER.
										)
										)
										(entmod en) 


				)																;end conditional
			)																	;end conditional block
		(setq cntr (+ 1 cntr))																;increment counter at end of loop (moves counter so enHatchItem grabs next item from ssHatchDel set at beginning of loop with ssname function)
		)

		)																		;end progn to wrap the if
	)




  
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "WATTSTOPPER" ))))
	(setq cntr 0)																	;set counter to 0
  	(setq cmax(sslength allBlocks))															;set counter max to total number of items in allBlocks) selection set
  	(while 	(< cntr cmax)																;while loop, continue until counter is greater than cmax
		(setq block (ssname allBlocks cntr))													;set block to the entity name (ssname function) of the ssHatchDel seleection set item that corresponds to the current value of the counter
		(cond																	;conditional block
		  	((/= block nil)															;first conditional when block is not nil
  			(setq en(entget block))														;set en variable to the entity name of the block variable
  			(setq wsBlockName (cdr(assoc 2 en)))												;set the wsBlockName variable to the second element (cdr function) of the 2 associative property (dxf code 2) of en (this is the block name)


  			(setq vlaobject (vlax-ename->vla-object block))											;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of block
    			(setq truename(vlax-get-property vlaobject											;set the truename variable  name of block
        			(if (vlax-property-available-p vlaobject 'effectivename)								;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
            			'effectivename														;to the effective name property
            			'name
        			)
    				)
				)															;END IF

;;;this section catches all items that on the WATTSTOPPER layer and moves then to the 0 layer
			(setq en															;entmod routine to move objects layer
			(subst (cons 8 "0")
			(assoc 8 en)            													; Changes the layer group in en.
			en                      													; to layer "0"
			)
			)
			(entmod en) 

			 

			)																;end conditional
		)																	;end conditional block
	(setq cntr (+ 1 cntr))																;increment counter at end of loop (moves counter so enHatchItem grabs next item from ssHatchDel set at beginning of loop with ssname function)
	)



	(setq f1 (open (strcat (getvar 'DWGPREFIX) "Vantage_Riser_Extraction.txt")  "w"))								;set f1 to to result of open on the string created from dwgprefix variable plus "Riser_Extraction.txt" open file for appending 
	(write-line (strcat (getvar "DWGPREFIX" ) (getvar "DWGNAME" ) " -found " (itoa (sslength allBlocks)) " block(s) with attributes" ) f1)		;write first line of file (this contains information about the drawing we extract from)
  	(close f1)



  
;;;this is the vantage main panel extraction section
;;;first the code moves the main panels to WATTSTOPPER, then extracts, then moves them back to 0
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "0" ))))
	(setq cntr 0)																	;set counter to 0
  	(setq cmax(sslength allBlocks))															;set counter max to total number of items in allBlocks) selection set
  	(while (< cntr cmax)																;while loop, continue until counter is greater than cmax
		(setq block (ssname allBlocks cntr))													;set block to the entity name (ssname function) of the ssHatchDel seleection set item that corresponds to the current value of the counter
		(cond																	;conditional block
		  	((/= block nil)															;first conditional when block is not nil
  			(setq en(entget block))														;set en variable to the entity name of the block variable
  			(setq wsBlockName (cdr(assoc 2 en)))												;set the wsBlockName variable to the second element (cdr function) of the 2 associative property (dxf code 2) of en (this is the block name)


  			(setq vlaobject (vlax-ename->vla-object block))											;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of block
    			(setq truename(vlax-get-property vlaobject											;set the truename variable  name of block
        			(if (vlax-property-available-p vlaobject 'effectivename)								;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
            			'effectivename														;to the effective name property
            			'name
        			)
    			)
			)																;END IF

;;;this nested if/or block catches all items that are Main Vantage panels
;;;this block of code moves them to the WATTSTOPPER layer
		  		(if															;--nested if
					  	(OR													;---or inside the if
						(= truename "V-LCAP44HS")
						(= truename "V-LCAP32S")
						(= truename "V-LCAP44S")
						(= truename "V-LCAP44H")
						(= truename "V-LCAP44M")
						(= truename "V-LCAP32M")
						(= truename "V-LCAP44A")
						(= truename "V-LCAP44L")
						)
							(progn												;----progn inside the if and or 
									(setq en									;entmod routine to move objects layer
									(subst (cons 8 "WATTSTOPPER")
									(assoc 8 en)            							; Changes the layer group in en.
									en                      							; to layer "0"
									)
									)
									(entmod en) 
							)
				)															;--end nested if
			 

			)																;end conditional
		)																	;end conditional block
	(setq cntr (+ 1 cntr))																;increment counter at end of loop (moves counter so enHatchItem grabs next item from ssHatchDel set at beginning of loop with ssname function)
	)

(if (setq SelSet (ssget "X" '((0 . "INSERT") (8 . "WATTSTOPPER" ) (66 . 1 )))) 										;create selection set from all blocks on Wattstopper Layter
    	(progn																			;progn to group all following statements into one block
      		(if (setq File#1 (open (strcat (getvar 'DWGPREFIX) "Vantage_Riser_Extraction.txt") "a" ) )								;set File#1 variable to the same thing as f1
        		(progn																	;progn to group all following statements into one block
          		(setq Counter -1 )															;set counter to -1
          		(write-line "Vantage Panels:" File#1)
          		(repeat (sslength SelSet )														;repeat statement for the entire length of blocks SelectionSet
			(setq Counter (1+ Counter))														;increment counter
			(setq EntName (ssname SelSet Counter))													;set EntName to current Counter value of SelSet
			(setq EntDxf (entget EntName))														;set EntDxf to the Dxf codes of EntName
	    		(setq BlockName (entget EntName))													;set BlockName to the Dxf codes of EntName
  			(setq vlaobject (vlax-ename->vla-object EntName))											;set vlaobject to the EntName converted into a VLA object
    			(setq BlockName(vlax-get-property vlaobject												;get the true name of a block if it is anonymous
        			(if (vlax-property-available-p vlaobject 'effectivename)
            			'effectivename
            			'name
        			)
    			)
			)																	;end of true name sub function

            			(while 	(/= (cdr (assoc 0 EntDxf )) "SEQEND" )											;while loop to drill to each sub entity in a block
	      				(setq attTag(cdr(assoc 2 EntDxf)))											;set attTag to the second element of the second Dxf code (assoc 2) of the EntDxf variable
	      				(setq attVal(cdr(assoc 1 EntDxf)))											;set attVal to the second element of the first Dxf code (assoc 1) of the EntDxf variable
              					(cond														;outer conditional for when the blocks attTag is "VMAIN"
						  	((= attTag "VMAIN" )											
								(setq main attVal)							
							)
						  	((= attTag "VSECONDARY" )											
								(setq sec attVal)							
							)
						  	((= attTag "VAUXILIARY" )											
								(setq aux attVal)							
							)
							((= attTag "ROOMID" )
								(setq roomid attVal)
							)
						)
				(setq EntName (entnext EntName))
              			(setq EntDxf (entget EntName))								
            			)
			(setq blockSublist (list main sec aux BlockName roomid))							
			(setq blockDottedPairs (cons blockSublist blockDottedPairs)) 
			(setq blockSublist nil)															;reset the sublist when moving to the next block
          	)

  	(setq blockDottedPairsAlphabetical (vl-sort blockDottedPairs (function (lambda (x y)(< (car x)(car y))))))						;custom function with vl-sort and lambda, if x is less than y, put x before y
	(setq n 0)																		;set n to 0
	(setq nmax  (length blockDottedPairsAlphabetical))													;set nmax to length of blockDottedPairsAlphabetical
  	(while (< n nmax)																	;loop for while n is less than n max

	  	(setq lineItem (nth n blockDottedPairsAlphabetical))												;set lineItem to the current n of blockDottedPairsAlphabetical
	  	(setq linemain (nth 0 lineItem))														;set linemain to the first element of lineItem
	  	(setq linesec (nth 1 lineItem))															;set lineblock to the second element of lineItem
	  	(setq lineaux (nth 2 lineItem))															;set lineaux to the third element of lineItem
	  	(setq lineblock (nth 3 lineItem))														;set lineblock to the fourth element of lineItem
	  	(setq lineroomid (nth 4 lineItem))														;set lineroomid to the fifth element of lineItem
;;;	  	format block name in text file only, can't use entmod to change block name in the same way that we can change the attribute value
;;;	  	this only capitalizes the block name in the text file
;;;	  	this will not capitalize the block name in the drawing file
;;;		(stringuppercase lineBlock)
	  	(setq lineBlock (stringuppercase lineBlock))


		(write-line linemain File#1 )															;write 5 peices of info
	  	(write-line linesec File#1 )
	  	(write-line lineaux File#1 )
	  	(write-line lineblock File#1 )
	  	(write-line lineroomid File#1 )

	  	(setq n (+ 1 n))																;increment n

	)


	  (setvar "CMDECHO" 1)
          (close File#1 )																	;close output file
          (princ "Done ! " )
        )
        (princ ". . file not found " )
      )
    )
    (princ "No legal block found " )																;error handling for no blocks
  )
(setq blockDottedPairsAlphabetical nil)
(setq blockDottedPairs nil)


 ;;;this section catches all items that on the WATTSTOPPER layer and moves then to the 0 layer 
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "WATTSTOPPER" ))))
	(setq cntr 0)																	;set counter to 0
  	(setq cmax(sslength allBlocks))															;set counter max to total number of items in allBlocks) selection set
  	(while 	(< cntr cmax)																;while loop, continue until counter is greater than cmax
		(setq block (ssname allBlocks cntr))													;set block to the entity name (ssname function) of the ssHatchDel seleection set item that corresponds to the current value of the counter
		(cond																	;conditional block
		  	((/= block nil)															;first conditional when block is not nil
  			(setq en(entget block))														;set en variable to the entity name of the block variable
  			(setq wsBlockName (cdr(assoc 2 en)))												;set the wsBlockName variable to the second element (cdr function) of the 2 associative property (dxf code 2) of en (this is the block name)
  			(setq vlaobject (vlax-ename->vla-object block))											;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of block
    			(setq truename(vlax-get-property vlaobject											;set the truename variable  name of block
        			(if (vlax-property-available-p vlaobject 'effectivename)								;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
            			'effectivename														;to the effective name property
            			'name
        			)
    				)
				)															;END IF


			(setq en															;entmod routine to move objects layer
			(subst (cons 8 "0")
			(assoc 8 en)            													; Changes the layer group in en.
			en                      													; to layer "0"
			)
			)
			(entmod en) 

			 
			)																;end conditional
		)																	;end conditional block
	(setq cntr (+ 1 cntr))																;increment counter at end of loop (moves counter so enHatchItem grabs next item from ssHatchDel set at beginning of loop with ssname function)
	)




;;;this is the vantage panel component extraction section
;;;first the code moves the panel components to WATTSTOPPER, then extracts, then moves them back to 0
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "0" ))))
	(setq cntr 0)																	;set counter to 0
  	(setq cmax(sslength allBlocks))															;set counter max to total number of items in allBlocks) selection set
  	(while (< cntr cmax)																;while loop, continue until counter is greater than cmax
		(setq block (ssname allBlocks cntr))													;set block to the entity name (ssname function) of the ssHatchDel seleection set item that corresponds to the current value of the counter
		(cond																	;conditional block
		  	((/= block nil)															;first conditional when block is not nil
  			(setq en(entget block))														;set en variable to the entity name of the block variable
  			(setq wsBlockName (cdr(assoc 2 en)))												;set the wsBlockName variable to the second element (cdr function) of the 2 associative property (dxf code 2) of en (this is the block name)


  			(setq vlaobject (vlax-ename->vla-object block))											;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of block
    			(setq truename(vlax-get-property vlaobject											;set the truename variable  name of block
        			(if (vlax-property-available-p vlaobject 'effectivename)								;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
            			'effectivename														;to the effective name property
            			'name
        			)
    			)
			)																;END IF

;;;this nested if/or block catches all items that are Secondary Vantage panels
;;;this block of code moves them to the WATTSTOPPER layer
		  		(if															;--nested if
					  	(OR													;---or inside the if
						(= truename "V-STPSRW101")
						(= truename "V-STPSRW201")
						(= truename "V-STPERW101")
						(= truename "V-STPERW201")
						(= truename "V-IC-36")
						(= truename "V-IC-DIN-II-LITE-RF")
						(= truename "V-IC-DIN-II-LITE")
						)
							(progn												;----progn inside the if and or 
									(setq en									;entmod routine to move objects layer
									(subst (cons 8 "WATTSTOPPER")
									(assoc 8 en)            							; Changes the layer group in en.
									en                      							; to layer "0"
									)
									)
									(entmod en) 
							)
				)															;--end nested if
			 

			)																;end conditional
		)																	;end conditional block
	(setq cntr (+ 1 cntr))																;increment counter at end of loop (moves counter so enHatchItem grabs next item from ssHatchDel set at beginning of loop with ssname function)
	)


(if (setq SelSet (ssget "X" '((0 . "INSERT") (8 . "WATTSTOPPER" ) (66 . 1 )))) 										;create selection set from all blocks on Wattstopper Layter
    	(progn																			;progn to group all following statements into one block
      		(if (setq File#1 (open (strcat (getvar 'DWGPREFIX) "Vantage_Riser_Extraction.txt") "a" ) )								;set File#1 variable to the same thing as f1
        		(progn																	;progn to group all following statements into one block
          		(setq Counter -1 )															;set counter to -1
          		(write-line "Vantage Panel Components:" File#1)
          		(repeat (sslength SelSet )														;repeat statement for the entire length of blocks SelectionSet
			(setq Counter (1+ Counter))														;increment counter
			(setq EntName (ssname SelSet Counter))													;set EntName to current Counter value of SelSet
			(setq EntDxf (entget EntName))														;set EntDxf to the Dxf codes of EntName
	    		(setq BlockName (entget EntName))													;set BlockName to the Dxf codes of EntName
  			(setq vlaobject (vlax-ename->vla-object EntName))											;set vlaobject to the EntName converted into a VLA object
    			(setq BlockName(vlax-get-property vlaobject												;get the true name of a block if it is anonymous
        			(if (vlax-property-available-p vlaobject 'effectivename)
            			'effectivename
            			'name
        			)
    			)
			)																	;end of true name sub function

            			(while 	(/= (cdr (assoc 0 EntDxf )) "SEQEND" )											;while loop to drill to each sub entity in a block
	      				(setq attTag(cdr(assoc 2 EntDxf)))											;set attTag to the second element of the second Dxf code (assoc 2) of the EntDxf variable
	      				(setq attVal(cdr(assoc 1 EntDxf)))											;set attVal to the second element of the first Dxf code (assoc 1) of the EntDxf variable
              					(cond														;outer conditional for when the blocks attTag is "VMAIN"
						  	((= attTag "VMAIN" )											
								(setq main attVal)							
							)
						  	((= attTag "VSECONDARY" )											
								(setq sec attVal)							
							)
						  	((= attTag "VAUXILIARY" )											
								(setq aux attVal)							
							)
							((= attTag "ROOMID" )
								(setq roomid attVal)
							)
						)
				(setq EntName (entnext EntName))
              			(setq EntDxf (entget EntName))								
            			)
			(setq blockSublist (list main sec aux BlockName roomid))							
			(setq blockDottedPairs (cons blockSublist blockDottedPairs)) 
			(setq blockSublist nil)															;reset the sublist when moving to the next block
          	)

  	(setq blockDottedPairsAlphabetical (vl-sort blockDottedPairs (function (lambda (x y)(< (car x)(car y))))))						;custom function with vl-sort and lambda, if x is less than y, put x before y
	(setq n 0)																		;set n to 0
	(setq nmax  (length blockDottedPairsAlphabetical))													;set nmax to length of blockDottedPairsAlphabetical
  	(while (< n nmax)																	;loop for while n is less than n max

	  	(setq lineItem (nth n blockDottedPairsAlphabetical))												;set lineItem to the current n of blockDottedPairsAlphabetical
	  	(setq linemain (nth 0 lineItem))														;set linemain to the first element of lineItem
	  	(setq linesec (nth 1 lineItem))															;set lineblock to the second element of lineItem
	  	(setq lineaux (nth 2 lineItem))															;set lineaux to the third element of lineItem
	  	(setq lineblock (nth 3 lineItem))														;set lineblock to the fourth element of lineItem
	  	(setq lineroomid (nth 4 lineItem))
;;;	  	format block name in text file only, can't use entmod to change block name in the same way that we can change the attribute value
;;;	  	this only capitalizes the block name in the text file
;;;	  	this will not capitalize the block name in the drawing file
;;;		(stringuppercase lineBlock)
	  	(setq lineBlock (stringuppercase lineBlock))


		(write-line linemain File#1 )															;write 5 peices of info
	  	(write-line linesec File#1 )
	  	(write-line lineaux File#1 )
	  	(write-line lineblock File#1 )
	  	(write-line lineroomid File#1 )

	  	(setq n (+ 1 n))																;increment n

	)


	  (setvar "CMDECHO" 1)
          (close File#1 )																	;close output file
          (princ "Done ! " )
        )
        (princ ". . file not found " )
      )
    )
    (princ "No legal block found " )																;error handling for no blocks
  )
(setq blockDottedPairsAlphabetical nil)
(setq blockDottedPairs nil)




 ;;;this section catches all items that on the WATTSTOPPER layer and moves then to the 0 layer 
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "WATTSTOPPER" ))))
	(setq cntr 0)																	;set counter to 0
  	(setq cmax(sslength allBlocks))															;set counter max to total number of items in allBlocks) selection set
  	(while 	(< cntr cmax)																;while loop, continue until counter is greater than cmax
		(setq block (ssname allBlocks cntr))													;set block to the entity name (ssname function) of the ssHatchDel seleection set item that corresponds to the current value of the counter
		(cond																	;conditional block
		  	((/= block nil)															;first conditional when block is not nil
  			(setq en(entget block))														;set en variable to the entity name of the block variable
  			(setq wsBlockName (cdr(assoc 2 en)))												;set the wsBlockName variable to the second element (cdr function) of the 2 associative property (dxf code 2) of en (this is the block name)
  			(setq vlaobject (vlax-ename->vla-object block))											;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of block
    			(setq truename(vlax-get-property vlaobject											;set the truename variable  name of block
        			(if (vlax-property-available-p vlaobject 'effectivename)								;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
            			'effectivename														;to the effective name property
            			'name
        			)
    				)
				)															;END IF


			(setq en															;entmod routine to move objects layer
			(subst (cons 8 "0")
			(assoc 8 en)            													; Changes the layer group in en.
			en                      													; to layer "0"
			)
			)
			(entmod en) 

			 
			)																;end conditional
		)																	;end conditional block
	(setq cntr (+ 1 cntr))																;increment counter at end of loop (moves counter so enHatchItem grabs next item from ssHatchDel set at beginning of loop with ssname function)
	)


  
  

;;;this is the vantage RS232 extraction section
;;;first the code moves the RS232 to WATTSTOPPER, then extracts, then moves them back to 0
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "0" ))))
	(setq cntr 0)																	;set counter to 0
  	(setq cmax(sslength allBlocks))															;set counter max to total number of items in allBlocks) selection set
  	(while (< cntr cmax)																;while loop, continue until counter is greater than cmax
		(setq block (ssname allBlocks cntr))													;set block to the entity name (ssname function) of the ssHatchDel seleection set item that corresponds to the current value of the counter
		(cond																	;conditional block
		  	((/= block nil)															;first conditional when block is not nil
  			(setq en(entget block))														;set en variable to the entity name of the block variable
  			(setq wsBlockName (cdr(assoc 2 en)))												;set the wsBlockName variable to the second element (cdr function) of the 2 associative property (dxf code 2) of en (this is the block name)


  			(setq vlaobject (vlax-ename->vla-object block))											;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of block
    			(setq truename(vlax-get-property vlaobject											;set the truename variable  name of block
        			(if (vlax-property-available-p vlaobject 'effectivename)								;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
            			'effectivename														;to the effective name property
            			'name
        			)
    			)
			)																;END IF

;;;this nested if/or block catches all items that are vantage station bus
;;;this block of code moves them to the WATTSTOPPER layer
		  		(if															;--nested if
					  	(OR													;---or inside the if
						(= truename "LMDI-100")
						)
							(progn												;----progn inside the if and or 
									(setq en									;entmod routine to move objects layer
									(subst (cons 8 "WATTSTOPPER")
									(assoc 8 en)            							; Changes the layer group in en.
									en                      							; to layer "0"
									)
									)
									(entmod en) 
							)
				)															;--end nested if
			 

			)																;end conditional
		)																	;end conditional block
	(setq cntr (+ 1 cntr))																;increment counter at end of loop (moves counter so enHatchItem grabs next item from ssHatchDel set at beginning of loop with ssname function)
	)




(if (setq SelSet (ssget "X" '((0 . "INSERT") (8 . "WATTSTOPPER" ) (66 . 1 )))) 										;create selection set from all blocks on Wattstopper Layter
    	(progn																			;progn to group all following statements into one block
      		(if (setq File#1 (open (strcat (getvar 'DWGPREFIX) "Vantage_Riser_Extraction.txt") "a" ) )								;set File#1 variable to the same thing as f1
        		(progn																	;progn to group all following statements into one block
          		(setq Counter -1 )															;set counter to -1
          		(write-line "Vantage RS232:" File#1)
          		(repeat (sslength SelSet )														;repeat statement for the entire length of blocks SelectionSet
			(setq Counter (1+ Counter))														;increment counter
			(setq EntName (ssname SelSet Counter))													;set EntName to current Counter value of SelSet
			(setq EntDxf (entget EntName))														;set EntDxf to the Dxf codes of EntName
	    		(setq BlockName (entget EntName))													;set BlockName to the Dxf codes of EntName
  			(setq vlaobject (vlax-ename->vla-object EntName))											;set vlaobject to the EntName converted into a VLA object
    			(setq BlockName(vlax-get-property vlaobject												;get the true name of a block if it is anonymous
        			(if (vlax-property-available-p vlaobject 'effectivename)
            			'effectivename
            			'name
        			)
    			)
			)																	;end of true name sub function

            			(while 	(/= (cdr (assoc 0 EntDxf )) "SEQEND" )											;while loop to drill to each sub entity in a block
	      				(setq attTag(cdr(assoc 2 EntDxf)))											;set attTag to the second element of the second Dxf code (assoc 2) of the EntDxf variable
	      				(setq attVal(cdr(assoc 1 EntDxf)))											;set attVal to the second element of the first Dxf code (assoc 1) of the EntDxf variable
              					(cond														;outer conditional for when the blocks attTag is "VMAIN"
						  	((= attTag "VMAIN" )											
								(setq main attVal)							
							)
						  	((= attTag "VSECONDARY" )											
								(setq sec attVal)							
							)
						  	((= attTag "VAUXILIARY" )											
								(setq aux attVal)							
							)
							((= attTag "ROOMID" )
								(setq roomid attVal)
							)
							((= attTag "VRS232POSITION" )
								(setq rs232 attVal)
							)
						)
				(setq EntName (entnext EntName))
              			(setq EntDxf (entget EntName))								
            			)
			(setq blockSublist (list main sec aux BlockName roomid rs232))							
			(setq blockDottedPairs (cons blockSublist blockDottedPairs)) 
			(setq blockSublist nil)															;reset the sublist when moving to the next block
          	)

  	(setq blockDottedPairsAlphabetical (vl-sort blockDottedPairs (function (lambda (x y)(< (car x)(car y))))))						;custom function with vl-sort and lambda, if x is less than y, put x before y
	(setq n 0)																		;set n to 0
	(setq nmax  (length blockDottedPairsAlphabetical))													;set nmax to length of blockDottedPairsAlphabetical
  	(while (< n nmax)																	;loop for while n is less than n max

	  	(setq lineItem (nth n blockDottedPairsAlphabetical))												;set lineItem to the current n of blockDottedPairsAlphabetical
	  	(setq linemain (nth 0 lineItem))														;set linemain to the first element of lineItem
	  	(setq linesec (nth 1 lineItem))															;set lineblock to the second element of lineItem
	  	(setq lineaux (nth 2 lineItem))															;set lineaux to the third element of lineItem
	  	(setq lineblock (nth 3 lineItem))														;set lineblock to the fourth element of lineItem
	  	(setq lineroomid (nth 4 lineItem))														;set lineBlock to the second elemtn of lineItem
	  	(setq liners232 (nth 5 lineItem))														;set lineBlock to the second elemtn of lineItem
;;;	  	format block name in text file only, can't use entmod to change block name in the same way that we can change the attribute value
;;;	  	this only capitalizes the block name in the text file
;;;	  	this will not capitalize the block name in the drawing file
;;;		(stringuppercase lineBlock)
;;;	  	(setq lineBlock (stringuppercase lineBlock))


		(write-line linemain File#1 )															;write 5 peices of info
	  	(write-line linesec File#1 )
	  	(write-line lineaux File#1 )
	  	(write-line lineblock File#1 )
	  	(write-line lineroomid File#1 )
	  	(write-line liners232 File#1 )

	  	(setq n (+ 1 n))																;increment n

	)


	  (setvar "CMDECHO" 1)
          (close File#1 )																	;close output file
          (princ "Done ! " )
        )
        (princ ". . file not found " )
      )
    )
    (princ "No legal block found " )																;error handling for no blocks
  )

(setq blockDottedPairsAlphabetical nil)
(setq blockDottedPairs nil)  

  
 ;;;this section catches all items that on the WATTSTOPPER layer and moves then to the 0 layer 
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "WATTSTOPPER" ))))
	(setq cntr 0)																	;set counter to 0
  	(setq cmax(sslength allBlocks))															;set counter max to total number of items in allBlocks) selection set
  	(while 	(< cntr cmax)																;while loop, continue until counter is greater than cmax
		(setq block (ssname allBlocks cntr))													;set block to the entity name (ssname function) of the ssHatchDel seleection set item that corresponds to the current value of the counter
		(cond																	;conditional block
		  	((/= block nil)															;first conditional when block is not nil
  			(setq en(entget block))														;set en variable to the entity name of the block variable
  			(setq wsBlockName (cdr(assoc 2 en)))												;set the wsBlockName variable to the second element (cdr function) of the 2 associative property (dxf code 2) of en (this is the block name)
  			(setq vlaobject (vlax-ename->vla-object block))											;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of block
    			(setq truename(vlax-get-property vlaobject											;set the truename variable  name of block
        			(if (vlax-property-available-p vlaobject 'effectivename)								;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
            			'effectivename														;to the effective name property
            			'name
        			)
    				)
				)															;END IF


			(setq en															;entmod routine to move objects layer
			(subst (cons 8 "0")
			(assoc 8 en)            													; Changes the layer group in en.
			en                      													; to layer "0"
			)
			)
			(entmod en) 

			 
			)																;end conditional
		)																	;end conditional block
	(setq cntr (+ 1 cntr))																;increment counter at end of loop (moves counter so enHatchItem grabs next item from ssHatchDel set at beginning of loop with ssname function)
	)


  
  

;;;this is the vantage station bus extraction section
;;;first the code moves the station bus to WATTSTOPPER, then extracts, then moves them back to 0
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "0" ))))
	(setq cntr 0)																	;set counter to 0
  	(setq cmax(sslength allBlocks))															;set counter max to total number of items in allBlocks) selection set
  	(while (< cntr cmax)																;while loop, continue until counter is greater than cmax
		(setq block (ssname allBlocks cntr))													;set block to the entity name (ssname function) of the ssHatchDel seleection set item that corresponds to the current value of the counter
		(cond																	;conditional block
		  	((/= block nil)															;first conditional when block is not nil
  			(setq en(entget block))														;set en variable to the entity name of the block variable
  			(setq wsBlockName (cdr(assoc 2 en)))												;set the wsBlockName variable to the second element (cdr function) of the 2 associative property (dxf code 2) of en (this is the block name)


  			(setq vlaobject (vlax-ename->vla-object block))											;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of block
    			(setq truename(vlax-get-property vlaobject											;set the truename variable  name of block
        			(if (vlax-property-available-p vlaobject 'effectivename)								;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
            			'effectivename														;to the effective name property
            			'name
        			)
    			)
			)																;END IF

;;;this nested if/or block catches all items that are vantage station bus
;;;this block of code moves them to the WATTSTOPPER layer
		  		(if															;--nested if
					  	(OR													;---or inside the if
						(= truename "V-EASYTOUCH-II-1")
						(= truename "V-EASYTOUCH-II-2")
						(= truename "V-EASYTOUCH-II-3")
						(= truename "V-EASYTOUCH-II-4")
						(= truename "V-EASYTOUCH-II-5")
						(= truename "V-EQ40TB-TI")
						(= truename "V-EQ73TB-TI")
						(= truename "V-EQ41TB-TI")
						(= truename "V-DMX-DALI-GW")
						(= truename "V-EM-LIGHTSENSOR")
						(= truename "V-LVOS")
						)
							(progn												;----progn inside the if and or 
									(setq en									;entmod routine to move objects layer
									(subst (cons 8 "WATTSTOPPER")
									(assoc 8 en)            							; Changes the layer group in en.
									en                      							; to layer "0"
									)
									)
									(entmod en) 
							)
				)															;--end nested if
			 

			)																;end conditional
		)																	;end conditional block
	(setq cntr (+ 1 cntr))																;increment counter at end of loop (moves counter so enHatchItem grabs next item from ssHatchDel set at beginning of loop with ssname function)
	)




(if (setq SelSet (ssget "X" '((0 . "INSERT") (8 . "WATTSTOPPER" ) (66 . 1 )))) 										;create selection set from all blocks on Wattstopper Layter
    	(progn																			;progn to group all following statements into one block
      		(if (setq File#1 (open (strcat (getvar 'DWGPREFIX) "Vantage_Riser_Extraction.txt") "a" ) )								;set File#1 variable to the same thing as f1
        		(progn																	;progn to group all following statements into one block
          		(setq Counter -1 )															;set counter to -1
          		(write-line "Vantage Station Bus:" File#1)
          		(repeat (sslength SelSet )														;repeat statement for the entire length of blocks SelectionSet
			(setq Counter (1+ Counter))														;increment counter
			(setq EntName (ssname SelSet Counter))													;set EntName to current Counter value of SelSet
			(setq EntDxf (entget EntName))														;set EntDxf to the Dxf codes of EntName
	    		(setq BlockName (entget EntName))													;set BlockName to the Dxf codes of EntName
  			(setq vlaobject (vlax-ename->vla-object EntName))											;set vlaobject to the EntName converted into a VLA object
    			(setq BlockName(vlax-get-property vlaobject												;get the true name of a block if it is anonymous
        			(if (vlax-property-available-p vlaobject 'effectivename)
            			'effectivename
            			'name
        			)
    			)
			)																	;end of true name sub function

            			(while 	(/= (cdr (assoc 0 EntDxf )) "SEQEND" )											;while loop to drill to each sub entity in a block
	      				(setq attTag(cdr(assoc 2 EntDxf)))											;set attTag to the second element of the second Dxf code (assoc 2) of the EntDxf variable
	      				(setq attVal(cdr(assoc 1 EntDxf)))											;set attVal to the second element of the first Dxf code (assoc 1) of the EntDxf variable
              					(cond														;outer conditional for when the blocks attTag is "VMAIN"
						  	((= attTag "VMAIN" )											
								(setq main attVal)							
							)
						  	((= attTag "VSECONDARY" )											
								(setq sec attVal)							
							)
						  	((= attTag "VAUXILIARY" )											
								(setq aux attVal)							
							)
							((= attTag "ROOMID" )
								(setq roomid attVal)
							)
							((= attTag "VSTATIONBUS" )
								(setq stationbus attVal)
							)
							((= attTag "VSTATIONBUSPOSITION" )
								(setq position attVal)
							)
						)
				(setq EntName (entnext EntName))
              			(setq EntDxf (entget EntName))								
            			)
			(setq blockSublist (list main sec aux BlockName roomid stationbus position))							
			(setq blockDottedPairs (cons blockSublist blockDottedPairs)) 
			(setq blockSublist nil)															;reset the sublist when moving to the next block
          	)

  	(setq blockDottedPairsAlphabetical (vl-sort blockDottedPairs (function (lambda (x y)(< (car x)(car y))))))						;custom function with vl-sort and lambda, if x is less than y, put x before y
	(setq n 0)																		;set n to 0
	(setq nmax  (length blockDottedPairsAlphabetical))													;set nmax to length of blockDottedPairsAlphabetical
  	(while (< n nmax)																	;loop for while n is less than n max

	  	(setq lineItem (nth n blockDottedPairsAlphabetical))												;set lineItem to the current n of blockDottedPairsAlphabetical
	  	(setq linemain (nth 0 lineItem))														;set linemain to the first element of lineItem
	  	(setq linesec (nth 1 lineItem))															;set linesec to the second element of lineItem
	  	(setq lineaux (nth 2 lineItem))															;set lineaux to the third element of lineItem
	  	(setq lineblock (nth 3 lineItem))														;set lineblock to the fourth element of lineItem
	  	(setq lineroomid (nth 4 lineItem))
	  	(setq linestationbus (nth 5 lineItem))
	  	(setq lineposition (nth 6 lineItem))
;;;	  	format block name in text file only, can't use entmod to change block name in the same way that we can change the attribute value
;;;	  	this only capitalizes the block name in the text file
;;;	  	this will not capitalize the block name in the drawing file
;;;		(stringuppercase lineBlock)
;;;	  	(setq lineBlock (stringuppercase lineBlock))


		(write-line linemain File#1 )															;write 7 peices of info
	  	(write-line linesec File#1 )
	  	(write-line lineaux File#1 )
	  	(write-line lineblock File#1 )
	  	(write-line lineroomid File#1 )
	  	(write-line linestationbus File#1 )
	  	(write-line lineposition File#1 )
	  	(setq n (+ 1 n))																;increment n

	)


	  (setvar "CMDECHO" 1)
          (close File#1 )																	;close output file
          (princ "Done ! " )
        )
        (princ ". . file not found " )
      )
    )
    (princ "No legal block found " )																;error handling for no blocks
  )

(setq blockDottedPairsAlphabetical nil)
(setq blockDottedPairs nil)


  
 ;;;this section catches all items that on the WATTSTOPPER layer and moves then to the 0 layer 
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "WATTSTOPPER" ))))
	(setq cntr 0)																	;set counter to 0
  	(setq cmax(sslength allBlocks))															;set counter max to total number of items in allBlocks) selection set
  	(while 	(< cntr cmax)																;while loop, continue until counter is greater than cmax
		(setq block (ssname allBlocks cntr))													;set block to the entity name (ssname function) of the ssHatchDel seleection set item that corresponds to the current value of the counter
		(cond																	;conditional block
		  	((/= block nil)															;first conditional when block is not nil
  			(setq en(entget block))														;set en variable to the entity name of the block variable
  			(setq wsBlockName (cdr(assoc 2 en)))												;set the wsBlockName variable to the second element (cdr function) of the 2 associative property (dxf code 2) of en (this is the block name)
  			(setq vlaobject (vlax-ename->vla-object block))											;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of block
    			(setq truename(vlax-get-property vlaobject											;set the truename variable  name of block
        			(if (vlax-property-available-p vlaobject 'effectivename)								;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
            			'effectivename														;to the effective name property
            			'name
        			)
    				)
				)															;END IF


			(setq en															;entmod routine to move objects layer
			(subst (cons 8 "0")
			(assoc 8 en)            													; Changes the layer group in en.
			en                      													; to layer "0"
			)
			)
			(entmod en) 

			 
			)																;end conditional
		)																	;end conditional block
	(setq cntr (+ 1 cntr))																;increment counter at end of loop (moves counter so enHatchItem grabs next item from ssHatchDel set at beginning of loop with ssname function)
	)

  
;;;this is the vantage 318 extraction section
;;;first the code moves the 318 to WATTSTOPPER, then extracts, then moves them back to 0
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "0" ))))
	(setq cntr 0)																	;set counter to 0
  	(setq cmax(sslength allBlocks))															;set counter max to total number of items in allBlocks) selection set
  	(while (< cntr cmax)																;while loop, continue until counter is greater than cmax
		(setq block (ssname allBlocks cntr))													;set block to the entity name (ssname function) of the ssHatchDel seleection set item that corresponds to the current value of the counter
		(cond																	;conditional block
		  	((/= block nil)															;first conditional when block is not nil
  			(setq en(entget block))														;set en variable to the entity name of the block variable
  			(setq wsBlockName (cdr(assoc 2 en)))												;set the wsBlockName variable to the second element (cdr function) of the 2 associative property (dxf code 2) of en (this is the block name)


  			(setq vlaobject (vlax-ename->vla-object block))											;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of block
    			(setq truename(vlax-get-property vlaobject											;set the truename variable  name of block
        			(if (vlax-property-available-p vlaobject 'effectivename)								;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
            			'effectivename														;to the effective name property
            			'name
        			)
    			)
			)																;END IF

;;;this nested if/or block catches all items that are vantage station bus
;;;this block of code moves them to the WATTSTOPPER layer
		  		(if															;--nested if
					  	(OR													;---or inside the if
						(= truename "CI-300")
						(= truename "DT-305")
						(= truename "WT-2255")
						(= truename "CX-105")
						(= truename "CX-100-3")
						(= truename "WPIR")
						(= truename "CI-24")
						(= truename "CX-100-4")
						(= truename "CI-200")
						(= truename "CI-300-1")
						(= truename "UT-305-1")
						(= truename "UT-305-2")
						(= truename "CB-100-3")
						(= truename "DT-205")
						(= truename "WT-2205")
						(= truename "UT-305-3")
						(= truename "UT-300-1")
						(= truename "DT-300")
						(= truename "UT-300-3")
						(= truename "UT-300-2")
						(= truename "DT-200")
						(= truename "WT-2250")
						(= truename "WT-1100")
						(= truename "WT-600")
						(= truename "CI-200-1")
						(= truename "CX-100-1")
						(= truename "CX-100")
						(= truename "CB-100")
						(= truename "W-1000A")
						(= truename "W-500A")
						(= truename "W-2000H")
						(= truename "W-2000A")
						
						)
							(progn												;----progn inside the if and or 
									(setq en									;entmod routine to move objects layer
									(subst (cons 8 "WATTSTOPPER")
									(assoc 8 en)            							; Changes the layer group in en.
									en                      							; to layer "0"
									)
									)
									(entmod en) 
							)
				)															;--end nested if
			 

			)																;end conditional
		)																	;end conditional block
	(setq cntr (+ 1 cntr))																;increment counter at end of loop (moves counter so enHatchItem grabs next item from ssHatchDel set at beginning of loop with ssname function)
	)




(if (setq SelSet (ssget "X" '((0 . "INSERT") (8 . "WATTSTOPPER" ) (66 . 1 )))) 										;create selection set from all blocks on Wattstopper Layter
    	(progn																			;progn to group all following statements into one block
      		(if (setq File#1 (open (strcat (getvar 'DWGPREFIX) "Vantage_Riser_Extraction.txt") "a" ) )								;set File#1 variable to the same thing as f1
        		(progn																	;progn to group all following statements into one block
          		(setq Counter -1 )															;set counter to -1
          		(write-line "Vantage 318:" File#1)
          		(repeat (sslength SelSet )														;repeat statement for the entire length of blocks SelectionSet
			(setq Counter (1+ Counter))														;increment counter
			(setq EntName (ssname SelSet Counter))													;set EntName to current Counter value of SelSet
			(setq EntDxf (entget EntName))														;set EntDxf to the Dxf codes of EntName
	    		(setq BlockName (entget EntName))													;set BlockName to the Dxf codes of EntName
  			(setq vlaobject (vlax-ename->vla-object EntName))											;set vlaobject to the EntName converted into a VLA object
    			(setq BlockName(vlax-get-property vlaobject												;get the true name of a block if it is anonymous
        			(if (vlax-property-available-p vlaobject 'effectivename)
            			'effectivename
            			'name
        			)
    			)
			)																	;end of true name sub function

            			(while 	(/= (cdr (assoc 0 EntDxf )) "SEQEND" )											;while loop to drill to each sub entity in a block
	      				(setq attTag(cdr(assoc 2 EntDxf)))											;set attTag to the second element of the second Dxf code (assoc 2) of the EntDxf variable
	      				(setq attVal(cdr(assoc 1 EntDxf)))											;set attVal to the second element of the first Dxf code (assoc 1) of the EntDxf variable
              					(cond														;outer conditional for when the blocks attTag is "VMAIN"
						  	((= attTag "VMAIN" )											
								(setq main attVal)							
							)
						  	((= attTag "VSECONDARY" )											
								(setq sec attVal)							
							)
						  	((= attTag "VAUXILIARY" )											
								(setq aux attVal)							
							)
							((= attTag "ROOMID" )
								(setq roomid attVal)
							)
							((= attTag "VSTATIONBUS" )
								(setq stationbus attVal)
							)
							((= attTag "VSTATIONBUSPOSITION" )
								(setq position attVal)
							)
							((= attTag "V318POSITION" )
								(setq 318position attVal)
							)
						)
				(setq EntName (entnext EntName))
              			(setq EntDxf (entget EntName))								
            			)
			(setq blockSublist (list main sec aux BlockName roomid stationbus position 318position))							
			(setq blockDottedPairs (cons blockSublist blockDottedPairs)) 
			(setq blockSublist nil)															;reset the sublist when moving to the next block
          	)

  	(setq blockDottedPairsAlphabetical (vl-sort blockDottedPairs (function (lambda (x y)(< (car x)(car y))))))						;custom function with vl-sort and lambda, if x is less than y, put x before y
	(setq n 0)																		;set n to 0
	(setq nmax  (length blockDottedPairsAlphabetical))													;set nmax to length of blockDottedPairsAlphabetical
  	(while (< n nmax)																	;loop for while n is less than n max
		(setq lineItem (nth n blockDottedPairsAlphabetical))
	  	(setq linemain (nth 0 lineItem))														;set linemain to the first element of lineItem
	  	(setq linesec (nth 1 lineItem))															;set linesec to the second element of lineItem
	  	(setq lineaux (nth 2 lineItem))															;set lineaux to the third element of lineItem
	  	(setq lineblock (nth 3 lineItem))														;set lineblock to the fourth element of lineItem
	  	(setq lineroomid (nth 4 lineItem))
	  	(setq linestationbus (nth 5 lineItem))
	  	(setq lineposition (nth 6 lineItem))
	  	(setq line318position (nth 7 lineItem))
;;;	  	format block name in text file only, can't use entmod to change block name in the same way that we can change the attribute value
;;;	  	this only capitalizes the block name in the text file
;;;	  	this will not capitalize the block name in the drawing file
;;;		(stringuppercase lineBlock)
;;;	  	(setq lineBlock (stringuppercase lineBlock))

		
		(write-line linemain File#1 )															;write 7 peices of info
	  	(write-line linesec File#1 )
	  	(write-line lineaux File#1 )
	  	(write-line lineblock File#1 )
	  	(write-line lineroomid File#1 )
	  	(write-line linestationbus File#1 )
	  	(write-line lineposition File#1 )
		(write-line line318position File#1 )
	  	(setq n (+ 1 n))																;increment n

	)


	  (setvar "CMDECHO" 1)
          (close File#1 )																	;close output file
          (princ "Done ! " )
        )
        (princ ". . file not found " )
      )
    )
    (princ "No legal block found " )																;error handling for no blocks
  )

(setq blockDottedPairsAlphabetical nil)
(setq blockDottedPairs nil)


  
 ;;;this section catches all items that on the WATTSTOPPER layer and moves then to the 0 layer 
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "WATTSTOPPER" ))))
	(setq cntr 0)																	;set counter to 0
  	(setq cmax(sslength allBlocks))															;set counter max to total number of items in allBlocks) selection set
  	(while 	(< cntr cmax)																;while loop, continue until counter is greater than cmax
		(setq block (ssname allBlocks cntr))													;set block to the entity name (ssname function) of the ssHatchDel seleection set item that corresponds to the current value of the counter
		(cond																	;conditional block
		  	((/= block nil)															;first conditional when block is not nil
  			(setq en(entget block))														;set en variable to the entity name of the block variable
  			(setq wsBlockName (cdr(assoc 2 en)))												;set the wsBlockName variable to the second element (cdr function) of the 2 associative property (dxf code 2) of en (this is the block name)
  			(setq vlaobject (vlax-ename->vla-object block))											;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of block
    			(setq truename(vlax-get-property vlaobject											;set the truename variable  name of block
        			(if (vlax-property-available-p vlaobject 'effectivename)								;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
            			'effectivename														;to the effective name property
            			'name
        			)
    				)
				)															;END IF


			(setq en															;entmod routine to move objects layer
			(subst (cons 8 "0")
			(assoc 8 en)            													; Changes the layer group in en.
			en                      													; to layer "0"
			)
			)
			(entmod en) 

			 
			)																;end conditional
		)																	;end conditional block
	(setq cntr (+ 1 cntr))																;increment counter at end of loop (moves counter so enHatchItem grabs next item from ssHatchDel set at beginning of loop with ssname function)
	)






  
  


    	(setq allBlocks2 (ssget "X" '((0 . "INSERT")(8 . "0" ))))
	(setq cntr 0)
  
	(if	(/= allBlocks2 nil)
	  	(progn
	  	(setq cmax(sslength allBlocks2))															;set counter max to total number of items in allBlocks) selection set
	  	(while 	(< cntr cmax)																;while loop, continue until counter is greater than cmax
			(setq block (ssname allBlocks2 cntr))													;set block to the entity name (ssname function) of the ssHatchDel seleection set item that corresponds to the current value of the counter
			(cond																	;conditional block
			  	((/= block nil)															;first conditional when block is not nil
	  			(setq en(entget block))														;set en variable to the entity name of the block variable
	  			(setq wsBlockName (cdr(assoc 2 en)))												;set the wsBlockName variable to the second element (cdr function) of the 2 associative property (dxf code 2) of en (this is the block name)


	  			(setq vlaobject (vlax-ename->vla-object block))											;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of block
	    			(setq truename(vlax-get-property vlaobject											;set the truename variable  name of block
	        			(if (vlax-property-available-p vlaobject 'effectivename)								;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
	            			'effectivename														;to the effective name property
	            			'name
	        			)
	    			)
				)																;END IF
										(setq en									;entmod routine to move objects layer
										(subst (cons 8 "WATTSTOPPER")
										(assoc 8 en)            							; Changes the layer group in en.
										en                      							; to layer WATTSTOPPER.
										)
										)
										(entmod en) 


				)																;end conditional
			)																	;end conditional block
		(setq cntr (+ 1 cntr))																;increment counter at end of loop (moves counter so enHatchItem grabs next item from ssHatchDel set at beginning of loop with ssname function)
		)

		)																		;end progn to wrap the if
	)																			;end if

;;;	(setq a "\nEBLOCK made changes to your drawing
;;;		\n
;;;		\nIn AutoCAD, ROOMIDs must only contain these characters:
;;;	    	\nCapital letters A thru Z.
;;;	    	\nNumbers 0 thru 9.
;;;	    	\nUnderscores _
;;;	    	\n
;;;	    	\nString formatting functions edited your ROOMIDs that do not match the above criteria.
;;;	    	\nTotal number of blocks fixed:
;;;	")
;;;	  (if	(/= formatedblockcounter 0)
;;;	  	(alert (strcat a "\n" (itoa formatedblockcounter)))
;;;	  )
	(close File#1)
  (princ)
)




    (defun *error* ( msg )
      	(close File#1)
        (princ)
    )



(defun stringuppercase (string / uppercasestring)
;;;	arguments:
;;;  	string, sent in from EBLOCK
;;;  
;;;  	global variables:
;;;  	formattedstring
;;;  		used in EBLOCK immediatly after this function is called
;;;  		important not to localize this variable
;;;  		this variable is set to nil every time roomnameformat is called
	(setq uppercasestring(strcase string))             														;sets string to uppercase
	(princ uppercasestring)
)

(defun roomnameformat (string / CHARACTER CMAX CNTR FORMATCHECK STRING1 STRING2 STRING2CNTR STRINGCNTR formattedstring)
;;;	arguments:
;;;  	string, sent in from EBLOCK
;;;  
;;;  	global variables:
;;;  	formattedstring
;;;  		used in EBLOCK immediatly after this function is called
;;;  		important not to localize this variable
;;;  		this variable is set to nil every time roomnameformat is called
;;;  	formatedblockcounter
;;;  		accumulator to count number of blocks that have an illegal character
;;;  		this accumulator does not count number of blocks that need the case formatted to uppercase
  
;;;  	(setq formattedstring nil)
  	(setq formatcheck 0)
  	(setq string(strcase string))             														;sets string to uppercase
	(setq cntr 0)
  	(setq cmax (strlen string))																;sets cmax to length of string passed into function
  
  	(while 	(< cntr cmax)
	  	(setq stringcntr (+ 1 cntr))															;strings start at 1, so the stringcntr is 1 more than the loop cntr
	  	(setq string2cntr (+ 2 cntr))															;this string2cntr is used for the second half of the word
  		(setq character (substr string stringcntr 1))													;store character to variable
	  

;;;	  	if and structure to allow for multiple conditions
;;;	  	progn loop must follow the and portion to wrap all statements together
	  	(if
			(AND	
				(= character " ")(= stringcntr cmax)												;if we are at the end of the string and there is a blank space
			)
		  	(progn
			(setq character "")															;set character to nothing, which removes the space
			(setq formatcheck 1)
			)
		)
	  
		(if
			(AND	(/= character "A")(/= character "B")(/= character "C")(/= character "D")(/= character "E")(/= character "F")
				(/= character "G")(/= character "H")(/= character "I")(/= character "J")(/= character "K")(/= character "L")
				(/= character "M")(/= character "N")(/= character "O")(/= character "P")(/= character "Q")(/= character "R")
				(/= character "S")(/= character "T")(/= character "U")(/= character "V")(/= character "W")(/= character "X")
				(/= character "Y")(/= character "Z")(/= character "1")(/= character "2")(/= character "3")(/= character "4")
				(/= character "5")(/= character "6")(/= character "7")(/= character "8")(/= character "9")(/= character "0")
				(/= character "_")(/= character "")
			)
		  	(progn
			(setq character "_")															;if string is not any of the declared values, then set character to _
			(setq formatcheck 1)
			)
		)
	  

;;;string1 and string2 are not used for anything but might be useful for other functions
	  	(cond

		  	((= cntr 0)																;when cntr is 0
	  		(setq string1 nil)															;there is no string1
			(setq string2 (substr string string2cntr cmax))												;string2 is the rest of the string after the character
			(setq formattedstring (strcat character))												;the formated string only has the character value
			)
			
		  	((/= cntr 0)																;when cntr is not 0
	  		(setq string1 (substr string 1 stringcntr))												;string1 is the portion of the string before the character
			(setq string2 (substr string string2cntr cmax))												;string2 is the portion of the string after the character
			(setq formattedstring (strcat  formattedstring character))										;formattedstring is the previous formatted string plus the new character
			)
		)	  
	  	
	  	(setq cntr (+ 1 cntr))
	)
  	(if	(/= formatcheck 0)
		(setq formatedblockcounter (+ formatedblockcounter 1))
	)
  	(princ formattedstring)

  )