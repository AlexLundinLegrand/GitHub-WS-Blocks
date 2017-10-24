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
; Alex Lundin 08-20-2017
(defun c:eblockvantage (/ A FORMATEDBLOCKCOUNTER ALLBLOCKS  panelid ALLBLOCKS2 ATTBRIDGETAG ATTBRIDGEVAL ATTTAG ATTVAL BLOCK BLOCKNAME BRIDGEPOSITION BRIDGESEGMENT BRIDGESUBDXF BRIDGESUBENTNAME CMAX CNTR EN ENTDXF ENTNAME LINEPOSITION LINESEGMENT N NMAX TRUENAME VLAOBJECT WSBLOCKNAME SelSet f1 f1 Counter blockSublist blockDottedPairs blockDottedPairsAlphabetical lineItem lineAttVal lineBlock)
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
	(if	(/= allBlocks nil)
	  	(progn
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




				(setq EntName block) 
				(setq EntDxf en)
	    			(while 	(/= (cdr (assoc 0 EntDxf )) "SEQEND" )											;while loop to drill to each sub entity in a block
	      				(setq attTag(cdr(assoc 2 EntDxf)))											;set attTag to the second element of the second Dxf code (assoc 2) of the EntDxf variable
	      				(setq attVal(cdr(assoc 1 EntDxf)))											;set attVal to the second element of the first Dxf code (assoc 1) of the EntDxf variable
	      					(cond 	((= attTag "ROOMID" )											;outer conditional for when the blocks attTag is "ROOMID"

																				;this if statement formats the attribute value, and we only reach this point if the attribute tage is ROOMID
							(if	(/= attVal "")
							  	(progn
						  		(setq attVal (eblock-vantage-roomnameformat attVal))								;call roomnameformat subfunction on the attVal variable
									(setq EntDxf										;entmod routine to place formated string back into block
									(subst (cons 1 attVal)
									(assoc 1 EntDxf)            								; Changes the attribute value group in EntDxf.
									EntDxf                      								; to the formated string now stored in attVal
									)
									)
									(entmod EntDxf)
								
								)
						  	)
							 
							)
						)
				 	(setq EntName (entnext EntName))
	      				(setq EntDxf (entget EntName))
				)


				 
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

		)
	)



	(setq f1 (open (strcat (getvar 'DWGPREFIX) "Vantage_Riser_Extraction.txt")  "w"))								;set f1 to to result of open on the string created from dwgprefix variable plus "Riser_Extraction.txt" open file for appending 
	(write-line (strcat (getvar "DWGPREFIX" ) (getvar "DWGNAME" ) " -found " (itoa (sslength allBlocks)) " block(s) with attributes" ) f1)		;write first line of file (this contains information about the drawing we extract from)
  	(close f1)



  
;;;this is the vantage main panel extraction section
;;;first the code moves the main panels to WATTSTOPPER, then extracts, then moves them back to 0
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "0" ))))

	(if	(/= allBlocks nil)
	  	(progn

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
		)
	)  


(setq vantagePanels (ssget "X" '((0 . "INSERT") (8 . "WATTSTOPPER" ) (66 . 1 )))) 										;create selection set from all blocks on Wattstopper Layter

(cond	
		((/= vantagePanels nil)
	
      		(if (setq f1 (open (strcat (getvar 'DWGPREFIX) "Vantage_Riser_Extraction.txt") "a" ) )								;set f1 variable to the same thing as f1
        		(progn																	;progn to group all following statements into one block
          		(setq Counter -1 )															;set counter to -1
          		(write-line "Vantage Panels:" f1)
          		(repeat (sslength vantagePanels )														;repeat statement for the entire length of blocks SelectionSet
			(setq Counter (1+ Counter))														;increment counter
			(setq EntName (ssname vantagePanels Counter))													;set EntName to current Counter value of vantagePanels
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
							((= attTag "PANELID" )
								(setq panelid attVal)
							)							
						)
				(setq EntName (entnext EntName))
              			(setq EntDxf (entget EntName))								
            			)
			(setq blockSublist (list main sec aux BlockName roomid panelid))							
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
		  	(setq linepanelid (nth 5 lineItem))														;set lineroomid to the sixth element of lineItem
	;;;	  	format block name in text file only, can't use entmod to change block name in the same way that we can change the attribute value
	;;;	  	this only capitalizes the block name in the text file
	;;;	  	this will not capitalize the block name in the drawing file
	;;;		(stringuppercase lineBlock)
		  	(setq lineBlock (stringuppercase lineBlock))


			(write-line linemain f1 )															;write 5 peices of info
		  	(write-line linesec f1 )
		  	(write-line lineaux f1 )
		  	(write-line lineblock f1 )
		  	(write-line lineroomid f1 )
			(write-line linepanelid f1 )
		  
		  	(setq n (+ 1 n))																;increment n

		)


		  (setvar "CMDECHO" 1)
	          (close f1 )																	;close output file
	          (princ "Done ! " )
	        )
	        (princ ". . file not found " )
	      )
	
		
		)
)
(setq blockDottedPairsAlphabetical nil)
(setq blockDottedPairs nil)
  

 ;;;this section catches all items that on the WATTSTOPPER layer and moves then to the 0 layer 
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "WATTSTOPPER" ))))
	(if	(/= allBlocks nil)
	  	(progn
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

		)
	)



;;;this is the vantage panel component extraction section
;;;first the code moves the panel components to WATTSTOPPER, then extracts, then moves them back to 0
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "0" ))))
	(if	(/= allBlocks nil)
	  	(progn
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
							(= truename "V-FANMOD")
							(= truename "V-MDR8CW301")
							(= truename "V-SDM12-EM")
							(= truename "V-UDM08-EM")
							(= truename "V-IC-36")
							(= truename "V-IC-DIN-II-LITE-RF")
							(= truename "V-IC-DIN-II-LITE")
							(= truename "V-LVOS-0-10-PWM-P-1")
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
				 

				)																	;end conditional
			)																		;end conditional block
		(setq cntr (+ 1 cntr))																	;increment counter at end of loop (moves counter so enHatchItem grabs next item from ssHatchDel set at beginning of loop with ssname function)
		)

		)
	)

(setq vantagePanelComponents (ssget "X" '((0 . "INSERT") (8 . "WATTSTOPPER" ) (66 . 1 )))) 									;create selection set from all blocks on Wattstopper Layter

(cond	
		((/= vantagePanelComponents nil)														;progn to group all following statements into one block
      		(if (setq f1 (open (strcat (getvar 'DWGPREFIX) "Vantage_Riser_Extraction.txt") "a" ) )							;set f1 variable to the same thing as f1
        		(progn																	;progn to group all following statements into one block
          		(setq Counter -1 )															;set counter to -1
          		(write-line "Vantage Panel Components:" f1)
          		(repeat (sslength vantagePanelComponents )												;repeat statement for the entire length of blocks SelectionSet
			(setq Counter (1+ Counter))														;increment counter
			(setq EntName (ssname vantagePanelComponents Counter))											;set EntName to current Counter value of vantagePanelComponents
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
			(setq blockSublist nil)																;reset the sublist when moving to the next block
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


			(write-line linemain f1 )															;write 5 peices of info
		  	(write-line linesec f1 )
		  	(write-line lineaux f1 )
		  	(write-line lineblock f1 )
		  	(write-line lineroomid f1 )

		  	(setq n (+ 1 n))																;increment n

		)


		  (setvar "CMDECHO" 1)
	          (close f1 )																	;close output file
	          (princ "Done ! " )
	        )
	        (princ ". . file not found " )
	      )

	    (princ "No legal block found " )																;error handling for no blocks
	)
)
  
(setq blockDottedPairsAlphabetical nil)
(setq blockDottedPairs nil)




 ;;;this section catches all items that on the WATTSTOPPER layer and moves then to the 0 layer 
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "WATTSTOPPER" ))))

	(if	(/= allBlocks nil)
	  	(progn
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

		)
	)

  
  

;;;this is the vantage RS232 extraction section
;;;first the code moves the RS232 to WATTSTOPPER, then extracts, then moves them back to 0
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "0" ))))

	(if	(/= allBlocks nil)
	  	(progn

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

		)
	)


(setq vantageRS232Components (ssget "X" '((0 . "INSERT") (8 . "WATTSTOPPER" ) (66 . 1 ))))
(cond	
		((/= vantageRS232Components nil)
		 
		      	(if (setq f1 (open (strcat (getvar 'DWGPREFIX) "Vantage_Riser_Extraction.txt") "a" ) )								;set f1 variable to the same thing as f1
        		(progn																	;progn to group all following statements into one block
          		(setq Counter -1 )															;set counter to -1
          		(write-line "Vantage RS232:" f1)
          		(repeat (sslength vantageRS232Components )														;repeat statement for the entire length of blocks SelectionSet
			(setq Counter (1+ Counter))														;increment counter
			(setq EntName (ssname vantageRS232Components Counter))													;set EntName to current Counter value of vantageRS232Components
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
			  	(setq lineroomid (nth 4 lineItem))														;set lineBlock to the second elemtn of lineItem

		;;;	  	format block name in text file only, can't use entmod to change block name in the same way that we can change the attribute value
		;;;	  	this only capitalizes the block name in the text file
		;;;	  	this will not capitalize the block name in the drawing file
		;;;		(stringuppercase lineBlock)
		;;;	  	(setq lineBlock (stringuppercase lineBlock))


				(write-line linemain f1 )															;write 5 peices of info
			  	(write-line linesec f1 )
			  	(write-line lineaux f1 )
			  	(write-line lineblock f1 )
			  	(write-line lineroomid f1 )


			  	(setq n (+ 1 n))																;increment n

			)


				  (setvar "CMDECHO" 1)
			          (close f1 )																	;close output file
			          (princ "Done ! " )
			        )
			        (princ ". . file not found " )
			      )

			    (princ "No legal block found " )																;error handling for no blocks
			)


		

)

(setq blockDottedPairsAlphabetical nil)
(setq blockDottedPairs nil)  

  

  
;;;this is the vantage station bus extraction section
;;;first the code moves the station bus to WATTSTOPPER, then extracts, then moves them back to 0
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "WATTSTOPPER" ))))	
	
	(if	(/= allBlocks nil)
	  	(progn																		;-progn wrap
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

		)																		;-end progn
	)


  
 
	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "0" ))))
	(if	(/= allBlocks nil)																;-if
	  	(progn																		;--progn wrap
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
							(= truename "V-LVOS-0-10-PWM-P-1")
							(= truename "V-LVOS-0-10-PWM-1")
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



		
		)																		;--end progn
	) 																			;-end if





(setq vantageStationBus (ssget "X" '((0 . "INSERT") (8 . "WATTSTOPPER" ) (66 . 1 )))) 										;create selection set from all blocks on Wattstopper Layter
	(cond	
		((/= vantageStationBus nil)
			(if (setq f1 (open (strcat (getvar 'DWGPREFIX) "Vantage_Riser_Extraction.txt") "a" ) )								;set f1 variable to the same thing as f1
        		(progn																	;progn to group all following statements into one block
          		(setq Counter -1 )															;set counter to -1
          		(write-line "Vantage Station Bus:" f1)
          		(repeat (sslength vantageStationBus )														;repeat statement for the entire length of blocks SelectionSet
			(setq Counter (1+ Counter))														;increment counter
			(setq EntName (ssname vantageStationBus Counter))													;set EntName to current Counter value of vantageStationBus
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


		(write-line linemain f1 )															;write 7 peices of info
	  	(write-line linesec f1 )
	  	(write-line lineaux f1 )
	  	(write-line lineblock f1 )
	  	(write-line lineroomid f1 )
	  	(write-line linestationbus f1 )
	  	(write-line lineposition f1 )
	  	(setq n (+ 1 n))																;increment n

	)


	  (setvar "CMDECHO" 1)
          (close f1 )																	;close output file
          (princ "Done ! " )
        )
        (princ ". . file not found " )
      )
		
		
		)
	)

      		
(setq blockDottedPairsAlphabetical nil)
(setq blockDottedPairs nil)


  
 ;;;this section catches all items that on the WATTSTOPPER layer and moves then to the 0 layer 
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "WATTSTOPPER" ))))	
	
	(if	(/= allBlocks nil)
	  	(progn
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

		)
	)


;;;this is the vantage POE extraction section
;;;first the code moves the POE to WATTSTOPPER, then extracts, then moves them back to 0
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "0" ))))
	(if	(/= allBlocks nil)
	  	(progn

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

	;;;this nested if/or block catches all items that are vantage POE
	;;;this block of code moves them to the WATTSTOPPER layer
			  		(if															;--nested if
						  	(OR													;---or inside the if
							(= truename "COM-POE-SWITCH")
							(= truename "DA1458")
							(= truename "DA2401")
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



		)
	)
	
  







(setq vantagepoeSwitches (ssget "X" '((0 . "INSERT") (8 . "WATTSTOPPER" ) (66 . 1 ))))
	(cond	
		((/= vantagepoeSwitches nil)
			(if (setq f1 (open (strcat (getvar 'DWGPREFIX) "Vantage_Riser_Extraction.txt") "a" ) )					;set f1 variable to the same thing as f1
        		(progn																;progn to group all following statements into one block
          		(setq Counter -1 )														;set counter to -1
          		(write-line "Vantage POE Switches:" f1)
          		(repeat (sslength vantagepoeSwitches )											;repeat statement for the entire length of blocks SelectionSet
			(setq Counter (1+ Counter))													;increment counter
			(setq EntName (ssname vantagepoeSwitches Counter))										;set EntName to current Counter value of vantage318Components
			(setq EntDxf (entget EntName))													;set EntDxf to the Dxf codes of EntName
	    		(setq BlockName (entget EntName))												;set BlockName to the Dxf codes of EntName
  			(setq vlaobject (vlax-ename->vla-object EntName))										;set vlaobject to the EntName converted into a VLA object
    			(setq BlockName(vlax-get-property vlaobject											;get the true name of a block if it is anonymous
        			(if (vlax-property-available-p vlaobject 'effectivename)
            			'effectivename
            			'name
        			)
    			)
			)																;end of true name sub function

            			(while 	(/= (cdr (assoc 0 EntDxf )) "SEQEND" )										;while loop to drill to each sub entity in a block
	      				(setq attTag(cdr(assoc 2 EntDxf)))										;set attTag to the second element of the second Dxf code (assoc 2) of the EntDxf variable
	      				(setq attVal(cdr(assoc 1 EntDxf)))										;set attVal to the second element of the first Dxf code (assoc 1) of the EntDxf variable
              					(cond													;outer conditional for when the blocks attTag is "VMAIN"
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
			(setq lineItem (nth n blockDottedPairsAlphabetical))
		  	(setq linemain (nth 0 lineItem))														;set linemain to the first element of lineItem
		  	(setq linesec (nth 1 lineItem))															;set linesec to the second element of lineItem
		  	(setq lineaux (nth 2 lineItem))															;set lineaux to the third element of lineItem
		  	(setq lineblock (nth 3 lineItem))														;set lineblock to the fourth element of lineItem
		  	(setq lineroomid (nth 4 lineItem))
	;;;	  	format block name in text file only, can't use entmod to change block name in the same way that we can change the attribute value
	;;;	  	this only capitalizes the block name in the text file
	;;;	  	this will not capitalize the block name in the drawing file
	;;;		(stringuppercase lineBlock)
	;;;	  	(setq lineBlock (stringuppercase lineBlock))

			
			(write-line linemain f1 )															;write 7 peices of info
		  	(write-line linesec f1 )
		  	(write-line lineaux f1 )
		  	(write-line lineblock f1 )
		  	(write-line lineroomid f1 )
		  	(setq n (+ 1 n))																;increment n

		)


		  (setvar "CMDECHO" 1)
	          (close f1 )																	;close output file
	          (princ "Done ! " )
	        )
	        (princ ". . file not found " )
	      )
		
		
		)																		;--end cond statement
	)																			;-end cond block



  


;;; this section catches all items that on the WATTSTOPPER layer and moves then to the 0 layer 
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "WATTSTOPPER" ))))
	(setq cntr 0)																		;set counter to 0
  	(if
	  	(/= allBlocks  nil)
	  	(progn
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
		)
	)



;;;this is the vantage POE extraction section
;;;first the code moves the POE to WATTSTOPPER, then extracts, then moves them back to 0
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "0" ))))
	(if	(/= allBlocks nil)
	  	(progn

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

	;;;this nested if/or block catches all items that are vantage POE
	;;;this block of code moves them to the WATTSTOPPER layer
			  		(if															;--nested if
						  	(OR													;---or inside the if
							(= truename "V-EQ41TB-TI")
							(= truename "V-EQ73TB-TI")
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



		)
	)
  
(setq vantagepoeDevices (ssget "X" '((0 . "INSERT") (8 . "WATTSTOPPER" ) (66 . 1 ))))
	(cond	
		((/= vantagepoeDevices nil)
			(if (setq f1 (open (strcat (getvar 'DWGPREFIX) "Vantage_Riser_Extraction.txt") "a" ) )					;set f1 variable to the same thing as f1
        		(progn																;progn to group all following statements into one block
          		(setq Counter -1 )														;set counter to -1
          		(write-line "Vantage POE Devices:" f1)
          		(repeat (sslength vantagepoeDevices )											;repeat statement for the entire length of blocks SelectionSet
			(setq Counter (1+ Counter))													;increment counter
			(setq EntName (ssname vantagepoeDevices Counter))										;set EntName to current Counter value of vantage318Components
			(setq EntDxf (entget EntName))													;set EntDxf to the Dxf codes of EntName
	    		(setq BlockName (entget EntName))												;set BlockName to the Dxf codes of EntName
  			(setq vlaobject (vlax-ename->vla-object EntName))										;set vlaobject to the EntName converted into a VLA object
    			(setq BlockName(vlax-get-property vlaobject											;get the true name of a block if it is anonymous
        			(if (vlax-property-available-p vlaobject 'effectivename)
            			'effectivename
            			'name
        			)
    			)
			)																;end of true name sub function

            			(while 	(/= (cdr (assoc 0 EntDxf )) "SEQEND" )										;while loop to drill to each sub entity in a block
	      				(setq attTag(cdr(assoc 2 EntDxf)))										;set attTag to the second element of the second Dxf code (assoc 2) of the EntDxf variable
	      				(setq attVal(cdr(assoc 1 EntDxf)))										;set attVal to the second element of the first Dxf code (assoc 1) of the EntDxf variable
              					(cond													;outer conditional for when the blocks attTag is "VMAIN"
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
			(setq lineItem (nth n blockDottedPairsAlphabetical))
		  	(setq linemain (nth 0 lineItem))														;set linemain to the first element of lineItem
		  	(setq linesec (nth 1 lineItem))															;set linesec to the second element of lineItem
		  	(setq lineaux (nth 2 lineItem))															;set lineaux to the third element of lineItem
		  	(setq lineblock (nth 3 lineItem))														;set lineblock to the fourth element of lineItem
		  	(setq lineroomid (nth 4 lineItem))
	;;;	  	format block name in text file only, can't use entmod to change block name in the same way that we can change the attribute value
	;;;	  	this only capitalizes the block name in the text file
	;;;	  	this will not capitalize the block name in the drawing file
	;;;		(stringuppercase lineBlock)
	;;;	  	(setq lineBlock (stringuppercase lineBlock))

			
			(write-line linemain f1 )															;write 7 peices of info
		  	(write-line linesec f1 )
		  	(write-line lineaux f1 )
		  	(write-line lineblock f1 )
		  	(write-line lineroomid f1 )
		  	(setq n (+ 1 n))																;increment n

		)


		  (setvar "CMDECHO" 1)
	          (close f1 )																	;close output file
	          (princ "Done ! " )
	        )
	        (princ ". . file not found " )
	      )
		
		
		)																		;--end cond statement
	)																			;-end cond block



  


;;; this section catches all items that on the WATTSTOPPER layer and moves then to the 0 layer 
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "WATTSTOPPER" ))))
	(setq cntr 0)																		;set counter to 0
  	(if
	  	(/= allBlocks  nil)
	  	(progn
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
		)
	)
  
;;;
;;;;;;this is the vantage 318 extraction section
;;;;;;first the code moves the 318 to WATTSTOPPER, then extracts, then moves them back to 0
;;;  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "0" ))))
;;;	(setq cntr 0)																		;set counter to 0
;;;  	(if
;;;	  	(/= allBlocks nil)
;;;		(progn
;;;	  	(setq cmax(sslength allBlocks))															;set counter max to total number of items in allBlocks) selection set
;;;	  	(while (< cntr cmax)																;while loop, continue until counter is greater than cmax
;;;			(setq block (ssname allBlocks cntr))													;set block to the entity name (ssname function) of the ssHatchDel seleection set item that corresponds to the current value of the counter
;;;			(cond																	;conditional block
;;;			  	((/= block nil)															;first conditional when block is not nil
;;;	  			(setq en(entget block))														;set en variable to the entity name of the block variable
;;;	  			(setq wsBlockName (cdr(assoc 2 en)))												;set the wsBlockName variable to the second element (cdr function) of the 2 associative property (dxf code 2) of en (this is the block name)
;;;
;;;
;;;	  			(setq vlaobject (vlax-ename->vla-object block))											;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of block
;;;	    			(setq truename(vlax-get-property vlaobject											;set the truename variable  name of block
;;;	        			(if (vlax-property-available-p vlaobject 'effectivename)								;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
;;;	            			'effectivename														;to the effective name property
;;;	            			'name
;;;	        			)
;;;	    			)
;;;				)																;END IF
;;;
;;;	;;;this nested if/or block catches all items that are vantage station bus
;;;	;;;this block of code moves them to the WATTSTOPPER layer
;;;			  		(if															;--nested if
;;;						  	(OR													;---or inside the if
;;;							(= truename "DT-300")
;;;							)
;;;								(progn												;----progn inside the if and or 
;;;										(setq en									;entmod routine to move objects layer
;;;										(subst (cons 8 "WATTSTOPPER")
;;;										(assoc 8 en)            							; Changes the layer group in en.
;;;										en                      							; to layer "0"
;;;										)
;;;										)
;;;										(entmod en) 
;;;								)
;;;					)															;--end nested if
;;;				 
;;;
;;;				)																;end conditional
;;;			)																	;end conditional block
;;;		(setq cntr (+ 1 cntr))																;increment counter at end of loop (moves counter so enHatchItem grabs next item from ssHatchDel set at beginning of loop with ssname function)
;;;		)
;;;		)
;;;	)
;;;	  
;;;  
;;;(setq vantage318 (ssget "X" '((0 . "INSERT") (8 . "WATTSTOPPER" ) (66 . 1 )))) 										;create selection set from all blocks on Wattstopper Layter
;;;    	(cond
;;;			((/= vantage318 nil)
;;;			(if (setq f1 (open (strcat (getvar 'DWGPREFIX) "Vantage_Riser_Extraction.txt") "a" ) )								;set f1 variable to the same thing as f1
;;;			(progn																	;progn to group all following statements into one block
;;;				(setq Counter -1 )															;set counter to -1
;;;				(write-line "Vantage 318:" f1)
;;;				(repeat (sslength vantage318 )														;repeat statement for the entire length of blocks SelectionSet
;;;					(setq Counter (1+ Counter))														;increment counter
;;;					(setq EntName (ssname vantage318 Counter))													;set EntName to current Counter value of vantage318
;;;					(setq EntDxf (entget EntName))														;set EntDxf to the Dxf codes of EntName
;;;					(setq BlockName (entget EntName))													;set BlockName to the Dxf codes of EntName
;;;					(setq vlaobject (vlax-ename->vla-object EntName))											;set vlaobject to the EntName converted into a VLA object
;;;					(setq BlockName(vlax-get-property vlaobject												;get the true name of a block if it is anonymous
;;;						(if (vlax-property-available-p vlaobject 'effectivename)
;;;							'effectivename
;;;							'name
;;;						)
;;;					)
;;;					)																	;end of true name sub function
;;;
;;;							(while 	(/= (cdr (assoc 0 EntDxf )) "SEQEND" )											;while loop to drill to each sub entity in a block
;;;							(setq attTag(cdr(assoc 2 EntDxf)))											;set attTag to the second element of the second Dxf code (assoc 2) of the EntDxf variable
;;;							(setq attVal(cdr(assoc 1 EntDxf)))											;set attVal to the second element of the first Dxf code (assoc 1) of the EntDxf variable
;;;									(cond														;outer conditional for when the blocks attTag is "VMAIN"
;;;								((= attTag "VMAIN" )											
;;;									(setq main attVal)							
;;;								)
;;;								((= attTag "VSECONDARY" )											
;;;									(setq sec attVal)							
;;;								)
;;;								((= attTag "VAUXILIARY" )											
;;;									(setq aux attVal)							
;;;								)
;;;								((= attTag "ROOMID" )
;;;									(setq roomid attVal)
;;;								)
;;;								((= attTag "VSTATIONBUS" )
;;;									(setq stationbus attVal)
;;;								)
;;;								((= attTag "VSTATIONBUSPOSITION" )
;;;									(setq position attVal)
;;;								)
;;;								((= attTag "V318POSITION" )
;;;									(setq 318position attVal)
;;;								)
;;;							)
;;;					(setq EntName (entnext EntName))
;;;							(setq EntDxf (entget EntName))								
;;;							)
;;;					(setq blockSublist (list main sec aux BlockName roomid stationbus position 318position))							
;;;					(setq blockDottedPairs (cons blockSublist blockDottedPairs)) 
;;;					(setq blockSublist nil)															;reset the sublist when moving to the next block
;;;				)
;;;
;;;				(setq blockDottedPairsAlphabetical (vl-sort blockDottedPairs (function (lambda (x y)(< (car x)(car y))))))						;custom function with vl-sort and lambda, if x is less than y, put x before y
;;;				(setq n 0)																		;set n to 0
;;;				(setq nmax  (length blockDottedPairsAlphabetical))													;set nmax to length of blockDottedPairsAlphabetical
;;;				(while (< n nmax)																	;loop for while n is less than n max
;;;					(setq lineItem (nth n blockDottedPairsAlphabetical))
;;;					(setq linemain (nth 0 lineItem))														;set linemain to the first element of lineItem
;;;					(setq linesec (nth 1 lineItem))															;set linesec to the second element of lineItem
;;;					(setq lineaux (nth 2 lineItem))															;set lineaux to the third element of lineItem
;;;					(setq lineblock (nth 3 lineItem))														;set lineblock to the fourth element of lineItem
;;;					(setq lineroomid (nth 4 lineItem))
;;;					(setq linestationbus (nth 5 lineItem))
;;;					(setq lineposition (nth 6 lineItem))
;;;					(setq line318position (nth 7 lineItem))
;;;					;;;	  	format block name in text file only, can't use entmod to change block name in the same way that we can change the attribute value
;;;					;;;	  	this only capitalizes the block name in the text file
;;;					;;;	  	this will not capitalize the block name in the drawing file
;;;					;;;		(stringuppercase lineBlock)
;;;					;;;	  	(setq lineBlock (stringuppercase lineBlock))
;;;
;;;
;;;					(write-line linemain f1 )															;write 7 peices of info
;;;					(write-line linesec f1 )
;;;					(write-line lineaux f1 )
;;;					(write-line lineblock f1 )
;;;					(write-line lineroomid f1 )
;;;					(write-line linestationbus f1 )
;;;					(write-line lineposition f1 )
;;;					(write-line line318position f1 )
;;;					(setq n (+ 1 n))																;increment n
;;;
;;;				)
;;;
;;;
;;;				(setvar "CMDECHO" 1)
;;;				(close f1 )																	;close output file
;;;				(princ "Done ! " )
;;;				)
;;;			(princ ". . file not found " )
;;;			)
;;;			)
;;;			(princ "No legal block found " )																;error handling for no blocks
;;;	)
;;;
;;;(setq blockDottedPairsAlphabetical nil)
;;;(setq blockDottedPairs nil)


  





  
 ;;;this section catches all items that on the WATTSTOPPER layer and moves then to the 0 layer 
  	(setq allBlocks (ssget "X" '((0 . "INSERT")(8 . "WATTSTOPPER" ))))
	(setq cntr 0)																		;set counter to 0
  	(if
	  	(/= allBlocks  nil)
	  	(progn
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
		)
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
	(close f1)
  (princ)
)




    (defun *error* ( msg )
      	(close f1)
        (princ)
    )


(defun eblock-vantage-roomnameformat (string / CHARACTER CMAX CNTR FORMATCHECK STRING1 STRING2 STRING2CNTR STRINGCNTR formattedstring)
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

  	(setq returnString formattedstring )

  )