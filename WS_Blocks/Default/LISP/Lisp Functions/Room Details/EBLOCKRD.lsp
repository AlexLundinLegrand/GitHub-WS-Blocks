; --- eblockrd Function ---
; Similar to EBLOCK
; Create list to export to text file
; The list will hold the ROOMID for each room, followed by each product in the room
; Example [ROOMID.product1 product2 product3 ect]
; The DLM products will be arranged by the Riser Heirarchy sub function
; The remaining products will be arranged alphabetically
; Then combine any rooms with the exact same product list
; Example [ROOM_101 ROOM_102 ROOM_103.product1 product2 product3 ect]
; Moves all network cabinets to 0 layer
; Format the ROOMID attribute in all the blocks on the Wattstopper layer
; Put the formated ROOMID back into the AutoCAD block
; Get SEGMENT and POSITION attribute from the network blocks
; Assemble all information in a dotted list
; Traverse the list and export each item individually
; Export ROOMID to text file
; Export BLOCKNAME to text file
; Export SEGMENT and POSITION to text file
; Alex Lundin 07-24-2017
(defun c:eblockrd (
		 /
		 A FORMATEDBLOCKCOUNTER ALLBLOCKS ALLBLOCKS2 ATTBRIDGETAG ATTBRIDGEVAL ATTTAG ATTVAL BLOCK BLOCKNAME BRIDGEPOSITION BRIDGESEGMENT BRIDGESUBDXF
		 BRIDGESUBENTNAME CMAX CNTR EN ENTDXF ENTNAME LINEPOSITION LINESEGMENT N NMAX TRUENAME VLAOBJECT WSBLOCKNAME SelSet f1 File#1 Counter blockSublist
		 blockDottedPairs blockDottedPairsAlphabetical lineItem lineRoom lineBlock ALLBLOCKS0 ALLBLOCKSMISSING ATTSEGMANTAG ATTSEGMANVAL B SEGMANSEGMENT SEGMANSUBDXF SEGMANSUBENTNAME
		 CABINETTYPE LINECABINETTYPE LINEROUTERNUMBER LINESEGMANSEGMENT LINESEGMENTMANAGER LINESWITCHNUMBER ROUTERNUMBER SEGMENTMANAGER SWITCHNUMBER
BLOCKROOMLIST BLOCKROOMLISTALPHABETICAL INFORMATIONLIST LINEPRODUCTS LISTCHECK M MLINEITEM MLINEPRODUCTS MLINEROOM MMAX
ALLBLOCKSINVALID DLMCONTROLLERS DLMDAYLIGHT DLMINTERFACES DLMNETWORKBRIDGES DLMOCCCORNERMOUNT DLMOCCSENSORS DLMPANELS DLMPLUGCONTROLLERS DLMSEGMENTMANAGERS
			 DLMSPLITTERS DLMSWITCHES DLMZONECONTROLLERS INFORMATIONDOTTEDPAIRS INFORMATIONSUBLIST LINEROOM LISTSORTER NUMBEROFCONTROLLERS PRODUCTLIST PRODUCTS REMAINDER RHRETURNLIST ROOMLOOPCHECK
		DEL LINEPRODUCT LINEROOMS NESTEDLIST STR ROOMNAME	 
		 )
	(vl-load-com)
  	(setvar "CMDECHO" 0)
  
	(*destroyer-final*)
  
	;;;move any blocks on 0 OR WATTSTOPPER_MISSING_SEGMENT to WATTSTOPPER
  	(setq allBlocks0 (ssget "X" '((0 . "INSERT")(8 . "0" ))))
    	(setq allBlocksMissing (ssget "X" '((0 . "INSERT")(8 . "WATTSTOPPER_MISSING_SEGMENT" ))))
  	(setq allBlocksInvalid (ssget "X" '((0 . "INSERT")(8 . "WATTSTOPPER_INVALID_CABINET" ))))

  	(setq allBlocks2 (acet-ss-union (list allBlocks0 allBlocksMissing allBlocksInvalid)))									;add the new [setname] to this line (setq total (acet-ss-union (list blocks cable ohm matchline [setname])))	
	(setq cntr 0)
  
	(if	(/= allBlocks2 nil)
	  	(progn
	  	(setq cmax(sslength allBlocks2))														;set counter max to total number of items in allBlocks) selection set
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

	
  	(*destroyer-selection-sets*)
  
  	(if (setq SelSet (ssget "X" '((0 . "INSERT") (8 . "WATTSTOPPER" ) (66 . 1 )(410 . "MODEL")))) 										;create selection set from all blocks on Wattstopper Layter
    	(progn																			;progn to group all following statements into one block
	  	(setq f1 (open (strcat (getvar 'DWGPREFIX) "Room_Details_Extraction.txt")  "a"))								;set f1 to to result of open on the string created from dwgprefix variable plus "Riser_Extraction.txt" open file for appending 
	  	(close f1)																	;close f1
      		(if (setq File#1 (open (strcat (getvar 'DWGPREFIX) "Room_Details_Extraction.txt") "w" ) )							;set File#1 variable to the same thing as f1
        		(progn																	;progn to group all following statements into one block
          		(setq Counter -1 )															;set counter to -1
          		(write-line (strcat (getvar "DWGPREFIX" ) (getvar "DWGNAME" ) " -found " (itoa (sslength SelSet)) " block(s) with attributes" ) File#1)	;write first line of file (this contains information about the drawing we extract from)
          		(repeat (sslength SelSet )														;repeat statement for the entire length of blocks SelectionSet
			(setq Counter (1+ Counter))														;increment counter
			(setq EntName (ssname SelSet Counter))													;set EntName to current Counter value of SelSet
			(setq en (entget EntName))														;set EntName to current Counter value of SelSet
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
			(setq roomName nil)
            			(while 	(/= (cdr (assoc 0 EntDxf )) "SEQEND" )											;while loop to drill to each sub entity in a block
	      				(setq attTag(cdr(assoc 2 EntDxf)))											;set attTag to the second element of the second Dxf code (assoc 2) of the EntDxf variable
	      				(setq attVal(cdr(assoc 1 EntDxf)))											;set attVal to the second element of the first Dxf code (assoc 1) of the EntDxf variable
              					(cond 	((= attTag "ROOMID" )											;outer conditional for when the blocks attTag is "ROOMID"

																				;this if statement formats the attribute value, and we only reach this point if the attribute tage is ROOMID
							(if	(/= attVal "")
							  	(progn
						  		(setq attVal (EBRDroomnameformat attVal))								;call EBRDroomnameformat subfunction on the attVal variable
									(setq EntDxf										;entmod routine to place formated string back into block
									(subst (cons 1 attVal)
									(assoc 1 EntDxf)            								; Changes the attribute value group in EntDxf.
									EntDxf                      								; to the formated string now stored in attVal
									)
									)
									(entmod EntDxf)
								(setq roomName attVal)
									
								)
							  	(progn
								(setq roomName "")
								)
						  	)
							 

							 
							)
									
              					)
				(setq EntName (entnext EntName))
	              		(setq EntDxf (entget EntName))								
	            		)

			(if
			  	(/= roomName nil)
				(progn
				(setq blockSublist (list roomName BlockName))							
				(setq blockroomlist (cons blockSublist blockroomlist))
				)
			)
			(setq blockSublist nil)															;reset the sublist when moving to the next block
          	)

  	(setq blockroomlistAlphabetical (vl-sort blockroomlist (function (lambda (x y)(< (car x)(car y))))))						;custom function with vl-sort and lambda, if x is less than y, put x before y
	(setq n 0)																		;set n to 0
	(setq nmax  (length blockroomlistAlphabetical))													;set nmax to length of blockroomlistAlphabetical
	;;;priming read

  	(setq lineItem (nth n blockroomlistAlphabetical))												;set lineItem to the current n of blockroomlistAlphabetical
  	(setq lineRoom (car lineItem))															;set lineRoom to the first element of lineItem
  	(setq lineBlock (cadr lineItem))
	(setq roomloopcheck lineRoom)
			
	;;;products list builder
	;;;add product to the list each time we read in from the text file
	;;;the products list will be set to nil when we find a new room
  	(if	(/= lineBlock nil)															;-- if statment to protect list from nil
		(setq products (cons lineBlock products))												;build products list
	)
	(setq n (+ 1 n))
			
	(while 	(< n nmax)																	;loop for while n is less than n max

	  	(setq lineItem (nth n blockroomlistAlphabetical))												;set lineItem to the current n of blockroomlistAlphabetical
	  	(setq lineRoom (car lineItem))															;set lineRoom to the first element of lineItem
	  	(setq lineBlock (cadr lineItem))														;set lineBlock to the second elemtn of lineItem

		
	  
		;;; on the last room, add the last product
	  	(if	(= n (- nmax 1))															;-- if statment to protect list from nil
			(setq products (cons lineBlock products))												;build products list
		)
	  
		(if
		  	(OR (/= roomloopcheck lineRoom)(= n (- nmax 1)))
		  	(progn
			  
			(setq RHreturnlist (EBRDRiserHeirarchy products))

			;;;seperate each item from function return
			(setq DLMSEGMENTMANAGERS (nth 0 RHreturnlist))
			(setq DLMNETWORKBRIDGES(nth 1 RHreturnlist))
			(setq DLMZONECONTROLLERS(nth 2 RHreturnlist))
			(setq DLMPANELS(nth 3 RHreturnlist))
			(setq DLMCONTROLLERS(nth 4 RHreturnlist))
			(setq DLMPLUGCONTROLLERS(nth 5 RHreturnlist))
			(setq NumberofCONTROLLERS(nth 6 RHreturnlist))
			(setq DLMINTERFACES(nth 7 RHreturnlist))
			(setq DLMOCCSENSORS(nth 8 RHreturnlist))
			(setq DLMOCCCORNERMOUNT(nth 9 RHreturnlist))
			(setq DLMDAYLIGHT(nth 10 RHreturnlist))
			(setq DLMSWITCHES(nth 11 RHreturnlist))
			(setq DLMSPLITTERS(nth 12 RHreturnlist))
			(setq REMAINDER(nth 13 RHreturnlist))
			
			
			(setq productlist (append DLMSEGMENTMANAGERS DLMZONECONTROLLERS DLMPANELS DLMNETWORKBRIDGES DLMCONTROLLERS DLMPLUGCONTROLLERS DLMINTERFACES DLMOCCSENSORS DLMOCCCORNERMOUNT DLMDAYLIGHT DLMSWITCHES DLMSPLITTERS REMAINDER))
			(setq RHreturnlist nil)

			;;;store information
			(setq informationSublist roomloopcheck)
			(setq informationSublist (list productlist informationSublist))
			(setq informationlist (cons informationSublist informationlist))
			(setq informationSublist nil)
			(setq products nil)
			)
		)

		;;;products list builder
		;;;add product to the list each time we read in from the text file
		;;;the products list will be set to nil when we find a new room
	  	(if	(/= lineBlock nil)															;-- if statment to protect list from nil
			(setq products (cons lineBlock products))												;build products list
		)
	  
	  	(setq roomloopcheck lineRoom)
	  	
	  	(setq n (+ 1 n))																;increment n

	)

			

	(setq n 0)																		;set n to 0
	(setq nmax  (length informationlist))															;set nmax to length of blockroomlistAlphabetical
	(while 	(< n nmax)																	;loop for while n is less than n max

	  	(setq lineItem (nth n informationlist))														;set lineItem to the current n of blockroomlistAlphabetical
	  	(setq lineProducts (car lineItem))														;set lineProducts to the first element of lineItem
	  	(setq lineRoom (cdr lineItem))															;set lineRoom to the second elemtn of lineItem



		(setq m (+ 1 n))																;set m to n plus 1
		(setq mmax  (length informationlist))														;set mmax to length of blockroomlistAlphabetical
		(while 	(< m mmax)																;loop for while m is less than m max

		  	(setq mlineItem (nth m informationlist))												;set lineItem to the current m of blockroomlistAlphabetical
		  	(setq mlineProducts (car mlineItem))													;set lineProducts to the first element of lineItem
		  	(setq mlineRoom (cdr mlineItem))													;set lineRoom to the second elemtn of lineItem
			(setq listcheck (equal lineProducts mlineProducts))

		  	(cond
			  	((= listcheck T)

				(setq informationlist (EBRDremove-mth-item informationlist m))
				(setq informationlist (EBRDadd-room-to-nth informationlist n mlineRoom))
				)
			)
		  	(setq m (+ 1 m))															;increment m

		)


	  
	  	(setq n (+ 1 n))																;increment n

	)
			



	(setq n 0)																		;set n to 0
	(setq nmax  (length informationlist))															;set nmax to length of blockroomlistAlphabetical
	(while 	(< n nmax)																	;loop for while n is less than n max

	  	(setq lineItem (nth n informationlist))														;set lineItem to the current n of blockroomlistAlphabetical
	  	(setq lineProducts (car lineItem))														;set lineProducts to the first element of lineItem
	  	(setq lineRooms (cdr lineItem))															;set lineRoom to the second elemtn of lineItem



          	(setq del "\\P")
          	(setq m 0)
		(setq lineRoom (nth m lineRooms))
          	;;check if rooms are a nested list (happens when there are multiple)
          	(setq nestedlist  (listp lineRoom))
		(setq str lineRoom)
          	(if
                  	(= nestedlist T)
                  	(progn
                        ;un does the nested list
                        (setq lineRooms lineRoom)
                        (setq m 0)
                        (setq mmax  (length lineRooms))	
                        (setq lineRoom (nth m lineRooms))
                        (setq str lineRoom)
	          	(setq m (+ 1 m))
	          	(while 	(< m mmax)															;loop for while m is less than m max
			  	(setq lineRoom (nth m lineRooms))												;set mproduct to the current m of lineProducts
				(setq str (strcat str del lineRoom))
			  	(setq m (+ 1 m))														;increment m

			)
                        )
               	)
          	(write-line str File#1 )


                        
          	(setq del ",")
		(setq m 0)																	;set m to 0
		(setq mmax  (length lineProducts))														;set mmax to length of lineProducts
		(setq str (itoa mmax))
	  	(write-line str File#1)
          	(while 	(< m mmax)																;loop for while m is less than m max
		  	(setq lineProduct(nth m lineProducts))													;set mproduct to the current m of lineProducts
			;(setq str (strcat str del lineProduct))
		  	(setq str lineProduct)
		  	(write-line str  File#1 )
		  	(setq m (+ 1 m))															;increment m

		)
		;(write-line str  File#1 )
		
		


	  
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
  
																				;end if

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
  
	(setq b "\nEBLOCK found missing attributes:
		\n
		\nIn AutoCAD, Segment Managers must have the SEGMENT attribute filled out:
	    	\nThe only options are 0 and 0A.
	    	\nThe blocks are on the WATTSTOPPER_MISSING_SEGMENT layer.
	    	\nFill the Segment attribute out and use EBLOCK again.
	    	\nTotal number of blocks found missing this information:
	")

  (princ)
)





(defun EBRDadd-room-to-nth (inputlist n inputroom / outputlist roomlist FIRSTITEM NEWITEM SECONDITEM SUBLIST)
  	(setq room (nth 0 inputroom))
    (setq n (1+ n))
    (foreach x inputlist 
		(cond 
			((/= 0 (setq n (1- n)))

			(setq outputlist (cons x outputlist))

			)
			(t
			(progn
			(setq firstitem (nth 0 x))
			(setq seconditem (nth 1 x))

			(if
			  	(= (listp seconditem) T)
			  	(progn

				(foreach z seconditem
				  	(if
					  	(= roomlist nil)
					  
						(setq roomlist z)
					  	(setq roomlist (list z roomlist))
					)

				)
				(setq newitem (list roomlist room))
				)
			  	(progn
				(setq roomlist seconditem)
				(setq newitem (list room roomlist))
				(setq x (list firstitem newitem))
				)
			)

			 
		
			(setq outputlist (cons x outputlist))
			)
			)
		)
		
	)
    (reverse outputlist)
)


(defun EBRDremove-mth-item ( inputlist m / outputlist )
    (setq m (1+ m))
    (foreach x inputlist 
		(if 
			(/= 0 (setq m (1- m))) 
			(setq outputlist (cons x outputlist))
		)
	)
    (reverse outputlist)
)

; --- EBRDRiserHeirarchy Sub Function ---
; accept a list of products in from Main and seperates them into global sublists for other functions
; Alex Lundin 03-17-2017
(defun EBRDRiserHeirarchy (
		       RHproducts

		       /
		       RHCONTROLLERS RHDLMCONTROLLERS RHDLMDAYLIGHT RHDLMINTERFACES RHDLMNETWORKBRIDGES RHDLMOCCCORNERMOUNT
		       RHDLMOCCSENSORS RHDLMPANELS RHDLMPLUGCONTROLLERS RHDLMSEGMENTMANAGERS RHDLMSPLITTERS RHDLMSWITCHES
		       RHDLMZONECONTROLLERS RHREMAINDER RETURNLIST
		       
		       CMAX CNTR PLACEMENTCHECK RHPRODUCT
		       )
;;;  		arguments					
;;;  		GSR:						
;;;		products into RHproducts			
;;;                                                       	
  	(vl-load-com)
  	(setq cntr 0)
  	(setq cmax (length RHproducts))
  	(setq RHControllers 0)

  	;;;while loop to cycle through product list, RHproducts
  	(while	(< cntr cmax)
	  	;variable for later, to determine if there was infact a placement on the DLM rungs
	  	(setq placementcheck 0)

	  	;pull item off list, that corresponds to the current counter value
	  	(setq RHproduct (nth cntr RHproducts))

	  
	  	(if																			;-if
			(OR (= RHproduct "LMSM-3E")(= RHproduct "LMSM-6E")(= RHproduct "LMNC")(= RHproduct "NB-ROUTER"))						;--nested OR in the if
		  	(progn																		;--progn, wraps the lines contained into 1 operation, this is necessary for if's in LISP
			(setq RHDLMSEGMENTMANAGERS (cons RHproduct RHDLMSEGMENTMANAGERS))			
			(setq placementcheck 1)
			)																		;--end progn
		)

	  
	  	(if																			;-if
			(OR (= RHproduct "LMBC-300"))															;--nested OR in the if
		  	(progn																		;--progn, wraps the lines contained into 1 operation, this is necessary for if's in LISP
			(setq RHDLMNETWORKBRIDGES (cons RHproduct RHDLMNETWORKBRIDGES))			
			(setq placementcheck 1)
			)																		;--end progn
		)																			;-end if

	  	(if																			;-if
			(OR (= RHproduct "LMZC-301"))															;--nested OR in the if
		  	(progn																		;--progn, wraps the lines contained into 1 operation, this is necessary for if's in LISP
			(setq RHDLMZONECONTROLLERS (cons RHproduct RHDLMZONECONTROLLERS))			
			(setq placementcheck 1)
			)																		;--end progn
		)																			;-end if
	  
	  	(if																			;-if
			(OR (= RHproduct "LMCP48")(= RHproduct "LMCP24")(= RHproduct "LMCP12")(= RHproduct "LMCP8"))							;--nested OR in the if
		  	(progn																		;--progn, wraps the lines contained into 1 operation, this is necessary for if's in LISP
			(setq RHDLMPANELS (cons RHproduct RHDLMPANELS))			
			(setq placementcheck 1)
			)																		;--end progn
		)																			;-end if
	  
	  	(if
		  	(OR (= RHproduct "LMRC-222")(= RHproduct "LMRC-221")(= RHproduct "LMRC-213-347v")(= RHproduct "LMRC-212-347v")(= RHproduct "LMRC-211-347v")(= RHproduct "LMRC-213")(= RHproduct "LMRC-212")(= RHproduct "LMRC-211")(= RHproduct "LMRC-112-M")(= RHproduct "LMRC-111-M")(= RHproduct "LMRC-112")(= RHproduct "LMRC-111")(= RHproduct "LMRC-102")(= RHproduct "LMRC-101")(= RHproduct "LMFC-011"))
			(progn
		  	(setq RHDLMCONTROLLERS (cons RHproduct RHDLMCONTROLLERS))
			(setq placementcheck 1)
			(setq RHControllers (+ 1 RHControllers))
			)
		)
	  
	  	(if
			(OR (= RHproduct "LMPL-201")(= RHproduct "LMPL-101"))
		  	(progn
			(setq RHDLMPLUGCONTROLLERS (cons RHproduct RHDLMPLUGCONTROLLERS))
			(setq placementcheck 1)
			(setq RHControllers (+ 1 RHControllers))
			)
		)

	  	(if
			(OR (= RHproduct "LMIN-104")(= RHproduct "LMOR-102")(= RHproduct "LMRL-100")(= RHproduct "LMIO-301")(= RHproduct "LMIO-201")(= RHproduct "LMIO-102")(= RHproduct "LMIO-101")(= RHproduct "LMDI-100")(= RHproduct "LMPB-100")(= RHproduct "LMIR-100")(= RHproduct "LMRH-105")(= RHproduct "LMRH-102")(= RHproduct "LMRH-101"))
		  	(progn
			(setq RHDLMINTERFACES (cons RHproduct RHDLMINTERFACES))
			(setq placementcheck 1)
			)
		)
	  
		(if
			(OR (= RHproduct "LMDC-100")(= RHproduct "LMPC-100")(= RHproduct "LMPC-100-1")(= RHproduct "LMPC-100-5")(= RHproduct "LMUC-100-2")(= RHproduct "LMUC-200"))
		  	(progn
			(setq RHDLMOCCSENSORS (cons RHproduct RHDLMOCCSENSORS))
			(setq placementcheck 1)
			)
		)
		  
		(if
			(OR (= RHproduct "LMDX-100")(= RHproduct "LMPX-100")(= RHproduct "LMPX-100-1")(= RHproduct "LMPX-100-3")(= RHproduct "LMPX-100-4"))
		  	(progn
			(setq RHDLMOCCCORNERMOUNT (cons RHproduct RHDLMOCCCORNERMOUNT))
			(setq placementcheck 1)
			)
		)
		  
		(if  
			(OR (= RHproduct "LMLS-105")(= RHproduct "LMLS-305")(= RHproduct "LMLS-400")(= RHproduct "LMLS-500")(= RHproduct "LMLS-600")(= RHproduct "LMPO-200")(= RHproduct "LMPS-6000"))
		  	(progn
			(setq RHDLMDAYLIGHT (cons RHproduct RHDLMDAYLIGHT))
			(setq placementcheck 1)
			)
		)

		(if
			(OR (= RHproduct "LMDM-101")(= RHproduct "LMDW-101")(= RHproduct "LMDW-102")(= RHproduct "LMPW-101")(= RHproduct "LMPW-102")(= RHproduct "LMPS-104")(= RHproduct "LMPW-101")(= RHproduct "LMPW-102")(= RHproduct "LMSW-101")(= RHproduct "LMSW-102")(= RHproduct "LMSW-103")(= RHproduct "LMSW-104")(= RHproduct "LMSW-105")(= RHproduct "LMSW-108")(= RHproduct "LMTS-101-CCT")(= RHproduct "LMSW-105-CCT"))
		  	(progn
			(setq RHDLMSWITCHES (cons RHproduct RHDLMSWITCHES))
			(setq placementcheck 1)
			)
		)

		(if
			(OR (= RHproduct "LMRJ-CS8")(= RHproduct "LMRJ-S8")(= RHproduct "LMRJ-C8"))
		  	(progn
			(setq RHDLMSPLITTERS (cons RHproduct RHDLMSPLITTERS))
			(setq placementcheck 1)
			)
		)

	  	(if	(/= placementcheck 1)
		  	(setq RHREMAINDER (cons RHproduct RHREMAINDER ))
		)
			
	  	(setq cntr (+ 1 cntr))
	)

  
  	(setq returnlist (list RHDLMSEGMENTMANAGERS RHDLMNETWORKBRIDGES RHDLMZONECONTROLLERS RHDLMPANELS RHDLMCONTROLLERS RHDLMPLUGCONTROLLERS RHControllers RHDLMINTERFACES RHDLMOCCSENSORS RHDLMOCCCORNERMOUNT RHDLMDAYLIGHT RHDLMSWITCHES RHDLMSPLITTERS RHREMAINDER))
)





(defun EBRDroomnameformat (string / CHARACTER CMAX CNTR FORMATCHECK STRING1 STRING2 STRING2CNTR STRINGCNTR formattedstring)
;;;	arguments:
;;;  	string, sent in from EBLOCK
;;;  
;;;  	global variables:
;;;  	formattedstring
;;;  		used in EBLOCK immediatly after this function is called
;;;  		important not to localize this variable
;;;  		this variable is set to nil every time EBRDroomnameformat is called
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

