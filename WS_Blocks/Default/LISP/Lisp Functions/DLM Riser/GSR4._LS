;obsolete program

; --- GSR3 Function ---
; Import information from Riser_Extraction_3.txt file created by EBLOCK function
; Manipulate data into lists and sublists
; Create a block for each room based on what products are in the room
; Insert each block by order of network bridge on segment
; Alex Lundin 03-31-2017
(defun c:gsr4 (/ BLOCKNAME BRIDGEPOSITION BRIDGESEGMENT BRIDGESEGMENTDOTTEDPAIRS
	       BRIDGESEGMENTLISTNUMERICAL BRIDGESEGMENTSUBLIST BZ-150 BZ-150COUNT BZ-250 BZ-250-347 BZ-250-347COUNT BZ-250COUNT BZ-50 BZ-50COUNT CB-100-3 CMAX
	       CNTR CNTR2 COLUMNCOUNTER CONDCOUNTER CONDCOUNTERMAX CUSTOMBLOCKSET ELCU-100 ELCU-100COUNT ELCU-200 ELCU-200COUNT EN ENDXF EW-200 F1 FILE1 HB-300-L1
	       HB-300-L1M HB-300-L2 HB-300-L3 HB-300-L3W HB-300-L4 HB-300-L4W HB-350-L1 HB-350-L1M HB-350-L2 HB-350-L3 HB-350-L3W HB-350-L4 HB-350-L4W HB340B-L3
	       INSERTEDROOM INSERTEDROOMS INSERTIONPOINT INSERTIONPOINT2 INSERTMAX LIC24 LIC24COUNT LIC48 LIC48COUNT LIC8 LIC8COUNT LICA24 LICA24COUNT LICA48
	       LICA48COUNT LICA8 LICA8COUNT LILM24 LILM24COUNT LILM48 LILM48COUNT LILM8 LILM8COUNT LIMIT LMBC-300 LMBC-300COUNT LMCP12 LMCP12COUNT LMCP24 LMCP24COUNT
	       LMCP48 LMCP48COUNT LMCP8 LMCP8COUNT LMDC-100 LMDC-100COUNT LMDI-100 LMDI-100COUNT LMDM-101 LMDM-101COUNT LMDW-101 LMDW-101COUNT LMDW-102 LMDW-102COUNT
	       LMDX-100 LMDX-100COUNT LMFC-011 LMFC-011COUNT LMIO-101 LMIO-101COUNT LMIO-102 LMIO-102COUNT LMIO-201 LMIO-201COUNT LMIO-301 LMIO-301COUNT LMIR-100
	       LMIR-100COUNT LMLS-105 LMLS-105COUNT LMLS-305 LMLS-305COUNT LMLS-400 LMLS-400COUNT LMLS-500 LMLS-500COUNT LMLS-600 LMLS-600COUNT LMNC LMNCCOUNT LMPB-100
	       LMPB-100COUNT LMPC-100 LMPC-100-1 LMPC-100-1COUNT LMPC-100-5 LMPC-100-5COUNT LMPC-100COUNT LMPL-101 LMPL-101COUNT LMPL-201 LMPL-201COUNT LMPO-200 LMPO-200COUNT
	       LMPS-104 LMPS-104COUNT LMPS-6000 LMPS-6000COUNT LMPW-101 LMPW-101COUNT LMPW-102 LMPW-102COUNT LMPX-100 LMPX-100-1 LMPX-100-1COUNT LMPX-100-3 LMPX-100-3COUNT
	       LMPX-100-4 LMPX-100-4COUNT LMPX-100COUNT LMRC-100 LMRC-100COUNT LMRC-101 LMRC-101COUNT LMRC-102 LMRC-102COUNT LMRC-111 LMRC-111-M LMRC-111-MCOUNT LMRC-111COUNT
	       LMRC-112 LMRC-112-M LMRC-112-MCOUNT LMRC-112COUNT LMRC-211 LMRC-211-347V LMRC-211-347VCOUNT LMRC-211COUNT LMRC-212 LMRC-212-347V LMRC-212-347VCOUNT LMRC-212COUNT
	       LMRC-213 LMRC-213-347V LMRC-213-347VCOUNT LMRC-213COUNT LMRC-221 LMRC-221COUNT LMRC-222 LMRC-222COUNT LMRH-101 LMRH-101COUNT LMRH-102 LMRH-102COUNT LMRH-105
	       LMRH-105COUNT LMRJ-C8 LMRJ-C8COUNT LMRJ-CS8 LMRJ-CS8COUNT LMRJ-S8 LMRJ-S8COUNT LMRL-100 LMRL-100COUNT LMSM-201 LMSM-201COUNT LMSM-3E LMSM-3ECOUNT LMSM-600 LMSM-600COUNT
	       LMSM-603 LMSM-603COUNT LMSM-6E LMSM-6ECOUNT LMSW-101 LMSW-101COUNT LMSW-102 LMSW-102COUNT LMSW-103 LMSW-103COUNT LMSW-104 LMSW-104COUNT LMSW-105 LMSW-105COUNT LMSW-108
	       LMSW-108COUNT LMUC-100-2 LMUC-100-2COUNT LMUC-200 LMUC-200COUNT LMZC-301 LMZC-301COUNT LVSW-101 LVSW-101COUNT LVSW-102 LVSW-102COUNT LVSW-103 LVSW-103COUNT LVSW-104
	       LVSW-104COUNT LVSW-108 LVSW-108COUNT MATCH NB-ROUTER NB-ROUTERCOUNT NB-SWITCH NB-SWITCHCOUNT POSITIONLISTCURRENT PRODUCTCOUNTERMAX PRODUCTCURRENT
	       PRODUCTFORBUILDING PRODUCTLADDER READFLAG RMAX ROOMCOUNTER ROOMCOUNTERMAX ROOMCURRENT ROOMDOTTEDPAIRS ROOMDOTTEDPAIRSALPHABETICAL ROOMFORBUILDING
	       ROOMFORBUILDINGHIERARCHY ROOMHIERARCHY ROOMHIERARCHYTOGETHER ROOMMAX ROOMPRODUCTCOUNT ROOMPRODUCTCOUNTER ROOMPRODUCTCOUNTERMAX ROOMPRODUCTCURRENT
	       ROOMPRODUCTNUMBER ROOMPRODUCTS ROOMPRODUCTSDOTTEDCOUNTER ROOMPRODUCTSDOTTEDCOUNTS ROOMPRODUCTSDOTTEDCURRENT ROOMPRODUCTSDOTTEDMAX ROOMSTOINSERT
	       ROOMSUBLIST ROOMTEXT ROOMTOINSERT ROWCOUNTER SEGMENT1 SEGMENT10 SEGMENT10A SEGMENT10ABLOCKNAME SEGMENT10ACOUNTER SEGMENT10ALISTCURRENT
	       SEGMENT10AMAX SEGMENT10ANUMERICAL SEGMENT10APOSITIONCURRENT SEGMENT10AROOMCURRENT SEGMENT10AROOMS SEGMENT10ASUBLIST SEGMENT10BLOCKNAME
	       SEGMENT10COUNTER SEGMENT10LISTCURRENT SEGMENT10MAX SEGMENT10NUMERICAL SEGMENT10POSITIONCURRENT SEGMENT10ROOMCURRENT SEGMENT10ROOMS SEGMENT10SUBLIST
	       SEGMENT1A SEGMENT1ABLOCKNAME SEGMENT1ACOUNTER SEGMENT1ALISTCURRENT SEGMENT1AMAX SEGMENT1ANUMERICAL SEGMENT1APOSITIONCURRENT SEGMENT1AROOMCURRENT
	       SEGMENT1AROOMS SEGMENT1ASUBLIST SEGMENT1BLOCKNAME SEGMENT1COUNTER SEGMENT1LISTCURRENT SEGMENT1MAX SEGMENT1NUMERICAL SEGMENT1POSITIONCURRENT
	       SEGMENT1ROOMCURRENT SEGMENT1ROOMS SEGMENT1SUBLIST SEGMENT2 SEGMENT2A SEGMENT2ABLOCKNAME SEGMENT2ACOUNTER SEGMENT2ALISTCURRENT SEGMENT2AMAX SEGMENT2ANUMERICAL
	       SEGMENT2APOSITIONCURRENT SEGMENT2AROOMCURRENT SEGMENT2AROOMS SEGMENT2ASUBLIST SEGMENT2BLOCKNAME SEGMENT2COUNTER SEGMENT2LISTCURRENT SEGMENT2MAX SEGMENT2NUMERICAL
	       SEGMENT2POSITIONCURRENT SEGMENT2ROOMCURRENT SEGMENT2ROOMS SEGMENT2SUBLIST SEGMENT3 SEGMENT3A SEGMENT3ABLOCKNAME SEGMENT3ACOUNTER SEGMENT3ALISTCURRENT SEGMENT3AMAX
	       SEGMENT3ANUMERICAL SEGMENT3APOSITIONCURRENT SEGMENT3AROOMCURRENT SEGMENT3AROOMS SEGMENT3ASUBLIST SEGMENT3BLOCKNAME SEGMENT3COUNTER SEGMENT3LISTCURRENT SEGMENT3MAX SEGMENT3NUMERICAL
	       SEGMENT3POSITIONCURRENT SEGMENT3ROOMCURRENT SEGMENT3ROOMS SEGMENT3SUBLIST SEGMENT4 SEGMENT4A SEGMENT4ABLOCKNAME SEGMENT4ACOUNTER SEGMENT4ALISTCURRENT SEGMENT4AMAX SEGMENT4ANUMERICAL SEGMENT4APOSITIONCURRENT SEGMENT4AROOMCURRENT SEGMENT4AROOMS SEGMENT4ASUBLIST SEGMENT4BLOCKNAME SEGMENT4COUNTER SEGMENT4LISTCURRENT SEGMENT4MAX SEGMENT4NUMERICAL SEGMENT4POSITIONCURRENT SEGMENT4ROOMCURRENT SEGMENT4ROOMS SEGMENT4SUBLIST SEGMENT5 SEGMENT5A SEGMENT5ABLOCKNAME SEGMENT5ACOUNTER SEGMENT5ALISTCURRENT SEGMENT5AMAX SEGMENT5ANUMERICAL SEGMENT5APOSITIONCURRENT SEGMENT5AROOMCURRENT SEGMENT5AROOMS SEGMENT5ASUBLIST SEGMENT5BLOCKNAME SEGMENT5COUNTER SEGMENT5LISTCURRENT SEGMENT5MAX SEGMENT5NUMERICAL SEGMENT5POSITIONCURRENT SEGMENT5ROOMCURRENT SEGMENT5ROOMS SEGMENT5SUBLIST SEGMENT6 SEGMENT6A SEGMENT6ABLOCKNAME SEGMENT6ACOUNTER SEGMENT6ALISTCURRENT SEGMENT6AMAX SEGMENT6ANUMERICAL SEGMENT6APOSITIONCURRENT SEGMENT6AROOMCURRENT SEGMENT6AROOMS SEGMENT6ASUBLIST SEGMENT6BLOCKNAME SEGMENT6COUNTER SEGMENT6LISTCURRENT SEGMENT6MAX SEGMENT6NUMERICAL SEGMENT6POSITIONCURRENT SEGMENT6ROOMCURRENT SEGMENT6ROOMS SEGMENT6SUBLIST SEGMENT7 SEGMENT7A SEGMENT7ABLOCKNAME SEGMENT7ACOUNTER SEGMENT7ALISTCURRENT SEGMENT7AMAX SEGMENT7ANUMERICAL SEGMENT7APOSITIONCURRENT SEGMENT7AROOMCURRENT SEGMENT7AROOMS SEGMENT7ASUBLIST SEGMENT7BLOCKNAME SEGMENT7COUNTER SEGMENT7LISTCURRENT SEGMENT7MAX SEGMENT7NUMERICAL SEGMENT7POSITIONCURRENT SEGMENT7ROOMCURRENT SEGMENT7ROOMS SEGMENT7SUBLIST SEGMENT8 SEGMENT8A SEGMENT8ABLOCKNAME SEGMENT8ACOUNTER SEGMENT8ALISTCURRENT SEGMENT8AMAX SEGMENT8ANUMERICAL SEGMENT8APOSITIONCURRENT SEGMENT8AROOMCURRENT SEGMENT8AROOMS SEGMENT8ASUBLIST SEGMENT8BLOCKNAME SEGMENT8COUNTER SEGMENT8LISTCURRENT SEGMENT8MAX SEGMENT8NUMERICAL SEGMENT8POSITIONCURRENT SEGMENT8ROOMCURRENT SEGMENT8ROOMS SEGMENT8SUBLIST SEGMENT9 SEGMENT9A SEGMENT9ABLOCKNAME SEGMENT9ACOUNTER SEGMENT9ALISTCURRENT SEGMENT9AMAX SEGMENT9ANUMERICAL SEGMENT9APOSITIONCURRENT SEGMENT9AROOMCURRENT SEGMENT9AROOMS SEGMENT9ASUBLIST SEGMENT9BLOCKNAME SEGMENT9COUNTER SEGMENT9LISTCURRENT SEGMENT9MAX SEGMENT9NUMERICAL SEGMENT9POSITIONCURRENT SEGMENT9ROOMCURRENT SEGMENT9ROOMS SEGMENT9SUBLIST SINGLELINEPRODUCTLIST SINGLELINEPRODUCTLISTCURRENT SINGLELINEROOMLIST SINGLELINEROOMLISTCURRENT SINGLELINESEGMENTCURRENT SINGLELINESEGMENTITEM SMAX SSRISERBLOCKS UNIQUEROOM UNIQUEROOMLIST WBT-900-IP WORKINGROOM XCOORD YCOORD ADF-120-277 ADF-120-277COUNT C CB-100 CB-100-3COUNT CB-100COUNT DSW-100 DSW-100COUNT 
		DSW-200 DSW-200COUNT DT-300 DT-300COUNT DT-355 DT-355COUNT DW-100 DW-100COUNT DW-200 
		DW-200COUNT EW-200COUNT HB-300-L1COUNT HB-300-L1MCOUNT HB-300-L2COUNT HB-300-L3COUNT 
		HB-300-L3WCOUNT HB-300-L4COUNT HB-300-L4WCOUNT HB-350-L1COUNT HB-350-L1MCOUNT HB-350-L2COUNT 
		HB-350-L3COUNT HB-350-L3WCOUNT HB-350-L4COUNT HB-350-L4WCOUNT HB340B-L3COUNT HBP HBPCOUNT 
		LVS-1K LVS-1KCOUNT PW-100 PW-100COUNT PW-311 PW-311COUNT R S TS-400 TS-400COUNT UT-300-1 
		UT-300-1COUNT UT-300-2 UT-300-2COUNT UT-355-2 UT-355-2COUNT UUT-300-1COUNT V-EASYTOUCH-II-1 
		V-EASYTOUCH-II-1COUNT V-EASYTOUCH-II-2 V-EASYTOUCH-II-2COUNT V-EASYTOUCH-II-3 V-EASYTOUCH-II-3COUNT 
		V-EASYTOUCH-II-4 V-EASYTOUCH-II-4COUNT 
		V-EASYTOUCH-II-5 V-EASYTOUCH-II-5COUNT WBT-900-IPCOUNT
	       )
	(vl-load-com)																		;open f1 for reading -- similar to oppen in eblock function
	(setvar "ctab" "Model")
	(setq customBlockSet (ssget "X" '((8 . "Riser" ))))													;selection set
	(cond
	  	((/= customBlockSet nil) 
  		(command "erase" "p" "")
		)
  	)
  	(command "-PURGE" "A" "*Z-Riser*" "N")
  	(setq f1 (open (strcat (getvar 'DWGPREFIX) "Riser_Extraction_4.txt")  "r"))
	(close f1)																		;close
  		(cond																		;outer conditional block
		  	((/= f1 nil)																;outer conditional, proceed if f1 is not nil
			(setq file1 (open (strcat (getvar 'DWGPREFIX) "Riser_Extraction_4.txt")  "r"))								;open file for reading
			(setq readFlag (read-line file1))													;set read flag to first line
			(setq roomCurrent "startOfCommand")													;set roomCurrent to startofcommand
				(while	(/= roomCurrent nil)													;while loop to continue while there is a value in roomCurrent variable
				  	(setq roomCurrent (read-line file1))											;read first line into roomCurrent
				  	(setq productCurrent (read-line file1))											;read next line into productCurrent

				  		(cond														;-nested conditional level 1
						  	((/= roomCurrent nil)											;inner conditional, while there is a value in roomCurrent
				  			(setq singleLineRoomList (cons roomCurrent singleLineRoomList))						;set singleLineRoomList to the roomCurrent plus the rest of singleLineRoomList

							 	(if 	(/= productCurrent nil)									;if there is a value stored in productCurrent
								  	(setq singleLineProductList (cons productCurrent singleLineProductList))		;set the singleLineProductList to the productCurrent plus the rest of the singeLineProductList
								)												;end if

								(cond												;--nested conditional level 2
								  	((= productCurrent "LMBC-300")								;conditional for LMBC-300
									(setq bridgeSegment (read-line file1))							;read next line to bridgeSegment
									(setq bridgePosition (read-line file1))							;read next line to bridgePosition
									(setq bridgeSegmentSublist (cons roomCurrent bridgeSegmentSublist))			;create bridgeSegmentSublist with roomCurrent at end, followed by dot and bridgePosition --this line can un do the dot if we structure one element at a time
									(setq bridgeSegmentSublist (cons  bridgePosition bridgeSegmentSublist))
									(setq bridgeSegmentSublist (cons  bridgeSegment bridgeSegmentSublist))			;add bridgeSegment to beginning of bridgeSegmentSublist
									(setq bridgeSegmentDottedPairs (cons bridgeSegmentSublist bridgeSegmentDottedPairs))	;create bridgeSegmentDottedPairs to accumulate all bridgeSegmentSublists
									)
								)												;-- end nested conditional level 2

							 	(cond												;--nested conditional level 2
								  	((= productCurrent "LMCP48")								;conditional for LMCP48
									(setq bridgeSegment (read-line file1))							;read next line to bridgeSegment
									(setq bridgePosition (read-line file1))							;read next line to bridgePosition
									(setq bridgeSegmentSublist (cons roomCurrent bridgeSegmentSublist))			;create bridgeSegmentSublist with roomCurrent at end, followed by dot and bridgePosition --this line can un do the dot if we structure one element at a time
									(setq bridgeSegmentSublist (cons  bridgePosition bridgeSegmentSublist))
									(setq bridgeSegmentSublist (cons  bridgeSegment bridgeSegmentSublist))			;add bridgeSegment to beginning of bridgeSegmentSublist
									(setq bridgeSegmentDottedPairs (cons bridgeSegmentSublist bridgeSegmentDottedPairs))	;create bridgeSegmentDottedPairs to accumulate all bridgeSegmentSublists
									)
								)												;-- end nested conditional level 2

							 	(cond												;--nested conditional level 2
								  	((= productCurrent "LMCP24")								
									(setq bridgeSegment (read-line file1))							
									(setq bridgePosition (read-line file1))							
									(setq bridgeSegmentSublist (cons roomCurrent bridgeSegmentSublist))			
									(setq bridgeSegmentSublist (cons  bridgePosition bridgeSegmentSublist))
									(setq bridgeSegmentSublist (cons  bridgeSegment bridgeSegmentSublist))			
									(setq bridgeSegmentDottedPairs (cons bridgeSegmentSublist bridgeSegmentDottedPairs))	
									)
								)												;-- end nested conditional level 2

							 	(cond												;--nested conditional level 2
								  	((= productCurrent "LMCP12")								
									(setq bridgeSegment (read-line file1))							
									(setq bridgePosition (read-line file1))							
									(setq bridgeSegmentSublist (cons roomCurrent bridgeSegmentSublist))			
									(setq bridgeSegmentSublist (cons  bridgePosition bridgeSegmentSublist))
									(setq bridgeSegmentSublist (cons  bridgeSegment bridgeSegmentSublist))			
									(setq bridgeSegmentDottedPairs (cons bridgeSegmentSublist bridgeSegmentDottedPairs))	
									)
								)												;-- end nested conditional level 2

							 	(cond												;--nested conditional level 2
								  	((= productCurrent "LMCP8")								
									(setq bridgeSegment (read-line file1))							
									(setq bridgePosition (read-line file1))							
									(setq bridgeSegmentSublist (cons roomCurrent bridgeSegmentSublist))			
									(setq bridgeSegmentSublist (cons  bridgePosition bridgeSegmentSublist))
									(setq bridgeSegmentSublist (cons  bridgeSegment bridgeSegmentSublist))			
									(setq bridgeSegmentDottedPairs (cons bridgeSegmentSublist bridgeSegmentDottedPairs))	
									)
								)												;-- end nested conditional level 2


							)													;- end nested conditional level 1

							
						)
					(setq bridgeSegmentSublist nil)												;reset bridgeSegmentSublist
				)																;end while
			)																	;end outer conditional
		)																		;end outer conditional block
  	(close file1)
  	(setq bridgeSegmentListNumerical (vl-sort bridgeSegmentDottedPairs (function (lambda (x y)(< (car x)(car y))))))					;numerical sort of bridgeSegmentDottedPairs, this organizes them by segment number
	(setq singleLineSegmentCurrent 0)															;set singleLineSegmentCurrent to 0 -- we never use 0 as segment number
  	(setq s 0)																		;set s to 0 -- counter to count number of segments in product text file
  	(setq smax (length bridgeSegmentListNumerical))														;set smax to number of bridges in text file -- stored in bridgeSegmentListNumerical
  	(while 	(< s smax)																	;while statement -- loop through each entry in bridgeSegmentListNumerical 																	
		(setq singleLineSegmentItem (nth s bridgeSegmentListNumerical))											;set singleLineSegmentItem to the item in bridgeSegmentListNumerical that corresponds to current s value
	  	(setq singleLineSegmentCurrent (car singleLineSegmentItem))											;set singleLineSegmentCurrent to first entry of singleLineSegmentItem -- segment number
	  	(setq bridgePosition (cadr singleLineSegmentItem))												;set bridgePosition to second entry of singleLineSegmentItem -- bridge position
		(setq roomCurrent (caddr singleLineSegmentItem))												;set roomCurrent to third entry of singleLineSegmentItem -- room name										
	  		(cond																	;conditional block
				
			  	((= singleLineSegmentCurrent "1")												;first conditional statment for when singleLineSegmentCurrent is 1
				(setq segment1Sublist (list bridgePosition singleLineSegmentCurrent roomCurrent))						;--create sublist for segment 1 containing bridgeposition segmentnumber and room
				(setq segment1 (cons segment1Sublist segment1))											;add each sublist to a main list for seperation
				(setq segment1Sublist nil)													;nil the sublist after each room
				)

				((= singleLineSegmentCurrent "2")												;first conditional statment for when singleLineSegmentCurrent is 2
						(setq segment2Sublist (list bridgePosition singleLineSegmentCurrent roomCurrent))				;same structure all the way down
						(setq segment2 (cons segment2Sublist segment2))
						(setq segment2Sublist nil)
				)

				((= singleLineSegmentCurrent "3")
						(setq segment3Sublist (list bridgePosition singleLineSegmentCurrent roomCurrent))
						(setq segment3 (cons segment3Sublist segment3))
						(setq segment3Sublist nil)
				)

				((= singleLineSegmentCurrent "4")
						(setq segment4Sublist (list bridgePosition singleLineSegmentCurrent roomCurrent))
						(setq segment4 (cons segment4Sublist segment4))
						(setq segment4Sublist nil)
				)

				((= singleLineSegmentCurrent "5")
						(setq segment5Sublist (list bridgePosition singleLineSegmentCurrent roomCurrent))
						(setq segment5 (cons segment5Sublist segment5))
						(setq segment5Sublist nil)
				)
				
				((= singleLineSegmentCurrent "6")
						(setq segment6Sublist (list bridgePosition singleLineSegmentCurrent roomCurrent))
						(setq segment6 (cons segment6Sublist segment6))
						(setq segment6Sublist nil)
				)

				((= singleLineSegmentCurrent "7")
						(setq segment7Sublist (list bridgePosition singleLineSegmentCurrent roomCurrent))
						(setq segment7 (cons segment7Sublist segment7))
						(setq segment7Sublist nil)
				)

				((= singleLineSegmentCurrent "8")
						(setq segment8Sublist (list bridgePosition singleLineSegmentCurrent roomCurrent))
						(setq segment8 (cons segment8Sublist segment8))
						(setq segment8Sublist nil)
				)

				((= singleLineSegmentCurrent "9")
						(setq segment9Sublist (list bridgePosition singleLineSegmentCurrent roomCurrent))
						(setq segment9 (cons segment9Sublist segment9))
						(setq segment9Sublist nil)
				)

				((= singleLineSegmentCurrent "10")
						(setq segment10Sublist (list bridgePosition singleLineSegmentCurrent roomCurrent))
						(setq segment10 (cons segment10Sublist segment10))
						(setq segment10Sublist nil)
				)
				
				((= singleLineSegmentCurrent "1A")						
						(setq segment1ASublist (list bridgePosition singleLineSegmentCurrent roomCurrent))												
						(setq segment1A (cons segment1ASublist segment1A))								
						(setq segment1ASublist nil)										
						
				)
				
				((= singleLineSegmentCurrent "2A")
						(setq segment2ASublist (list bridgePosition singleLineSegmentCurrent roomCurrent))
						(setq segment2A (cons segment2ASublist segment2A))
						(setq segment2ASublist nil)
				)

				((= singleLineSegmentCurrent "3A")
						(setq segment3ASublist (list bridgePosition singleLineSegmentCurrent roomCurrent))
						(setq segment3A (cons segment3ASublist segment3A))
						(setq segment3ASublist nil)
				)

				((= singleLineSegmentCurrent "4A")
						(setq segment4ASublist (list bridgePosition singleLineSegmentCurrent roomCurrent))
						(setq segment4A (cons segment4ASublist segment4A))
						(setq segment4ASublist nil)
				)

				((= singleLineSegmentCurrent "5A")
						(setq segment5ASublist (list bridgePosition singleLineSegmentCurrent roomCurrent))
						(setq segment5A (cons segment5ASublist segment5A))
						(setq segment5ASublist nil)
				)
				
				((= singleLineSegmentCurrent "6A")
						(setq segment6ASublist (list bridgePosition singleLineSegmentCurrent roomCurrent))
						(setq segment6A (cons segment6ASublist segment6A))
						(setq segment6ASublist nil)
				)

				((= singleLineSegmentCurrent "7A")
						(setq segment7ASublist (list bridgePosition singleLineSegmentCurrent roomCurrent))
						(setq segment7A (cons segment7ASublist segment7A))
						(setq segment7ASublist nil)
				)

				((= singleLineSegmentCurrent "8A")
						(setq segment8ASublist (list bridgePosition singleLineSegmentCurrent roomCurrent))
						(setq segment8A (cons segment8ASublist segment8A))
						(setq segment8ASublist nil)
				)

				((= singleLineSegmentCurrent "9A")
						(setq segment9ASublist (list bridgePosition singleLineSegmentCurrent roomCurrent))
						(setq segment9A (cons segment9ASublist segment9A))
						(setq segment9ASublist nil)
				)

				((= singleLineSegmentCurrent "10A")
						(setq segment10ASublist (list bridgePosition singleLineSegmentCurrent roomCurrent))
						(setq segment10A (cons segment10ASublist segment10A))
						(setq segment10ASublist nil)
				)

			)
	  	(setq s (+ 1 s))																;increment counter after we add the bridge to correct segement list					
	)																			;end while loop

  	(setq segment1numerical (vl-sort segment1 (function (lambda (x y)(< (car x)(car y))))))									;numerical sort of segment1, this organizes the rooms of segment 1 by bridge position
	(setq segment2numerical (vl-sort segment2 (function (lambda (x y)(< (car x)(car y))))))									;numerical sort of remaining segments
  	(setq segment3numerical (vl-sort segment3 (function (lambda (x y)(< (car x)(car y))))))								
	(setq segment4numerical (vl-sort segment4 (function (lambda (x y)(< (car x)(car y))))))								
  	(setq segment5numerical (vl-sort segment5 (function (lambda (x y)(< (car x)(car y))))))								
  	(setq segment6numerical (vl-sort segment6 (function (lambda (x y)(< (car x)(car y))))))								
  	(setq segment7numerical (vl-sort segment7 (function (lambda (x y)(< (car x)(car y))))))								
  	(setq segment8numerical (vl-sort segment8 (function (lambda (x y)(< (car x)(car y))))))								
  	(setq segment9numerical (vl-sort segment9 (function (lambda (x y)(< (car x)(car y))))))								
  	(setq segment10numerical (vl-sort segment10 (function (lambda (x y)(< (car x)(car y))))))								
	(setq segment1Anumerical (vl-sort segment1A (function (lambda (x y)(< (car x)(car y))))))								
	(setq segment2Anumerical (vl-sort segment2A (function (lambda (x y)(< (car x)(car y))))))								
  	(setq segment3Anumerical (vl-sort segment3A (function (lambda (x y)(< (car x)(car y))))))								
	(setq segment4Anumerical (vl-sort segment4A (function (lambda (x y)(< (car x)(car y))))))								
  	(setq segment5Anumerical (vl-sort segment5A (function (lambda (x y)(< (car x)(car y))))))								
  	(setq segment6Anumerical (vl-sort segment6A (function (lambda (x y)(< (car x)(car y))))))								
  	(setq segment7Anumerical (vl-sort segment7A (function (lambda (x y)(< (car x)(car y))))))								
  	(setq segment8Anumerical (vl-sort segment8A (function (lambda (x y)(< (car x)(car y))))))								
  	(setq segment9Anumerical (vl-sort segment9A (function (lambda (x y)(< (car x)(car y))))))								
  	(setq segment10Anumerical (vl-sort segment10A (function (lambda (x y)(< (car x)(car y))))))
  
  	
    	(setq r 0)  																		;same structure for each segment 												
  	(setq roomCurrent nil)																	;reset roomCurrent before looping	
  	(setq rmax (length segment1numerical))															;set rmax to length of items in segment1
  	(while 	(< r rmax)																	;while r less than rmax
		(setq positionListCurrent (nth r segment1numerical))												;set the positionListCurrent to the nth item of segment1 that corresponds to r
	  	(setq roomCurrent (caddr positionListCurrent))													;pull the roomCurrent from the nth item of segment1
  		(setq segment1Rooms (cons roomCurrent segment1Rooms ))												;construct a list of all roomCurrents -- they will be in reverse order due to cons property
	  	(setq r (+ 1 r))																;increment r
	)
	(setq segment1Rooms (reverse segment1Rooms))														;reverse segment1Rooms to account for cons property										
  
    	(setq r 0)  																		;same structure for each segment                                 
    	(setq roomCurrent nil)                                 
    	(setq rmax (length segment2numerical))                              
    	(while  (< r rmax)                                  
   	(setq positionListCurrent (nth r segment2numerical))                        
      	(setq roomCurrent (caddr positionListCurrent))                          
      	(setq segment2Rooms (cons roomCurrent segment2Rooms ))                       
      	(setq r (+ 1 r))                                
  	)
  	(setq segment2Rooms (reverse segment2Rooms))                            

    	(setq r 0)                                   
    	(setq roomCurrent nil)                                 
    	(setq rmax (length segment3numerical))                              
    	(while  (< r rmax)                                  
   	(setq positionListCurrent (nth r segment3numerical))                        
      	(setq roomCurrent (caddr positionListCurrent))                          
      	(setq segment3Rooms (cons roomCurrent segment3Rooms ))                       
      	(setq r (+ 1 r))                                
  	)
  	(setq segment3Rooms (reverse segment3Rooms))

    	(setq r 0)                                   
    	(setq roomCurrent nil)                                 
    	(setq rmax (length segment4numerical))                              
    	(while  (< r rmax)                                  
   	(setq positionListCurrent (nth r segment4numerical))                        
      	(setq roomCurrent (caddr positionListCurrent))                          
      	(setq segment4Rooms (cons roomCurrent segment4Rooms ))                       
      	(setq r (+ 1 r))                                
  	)
  	(setq segment4Rooms (reverse segment4Rooms))

    	(setq r 0)                                   
    	(setq roomCurrent nil)                                 
    	(setq rmax (length segment5numerical))                              
    	(while  (< r rmax)                                  
   	(setq positionListCurrent (nth r segment5numerical))                        
      	(setq roomCurrent (caddr positionListCurrent))                          
      	(setq segment5Rooms (cons roomCurrent segment5Rooms ))                       
      	(setq r (+ 1 r))                                
  	)
  	(setq segment5Rooms (reverse segment5Rooms))

      	(setq r 0)                                   
    	(setq roomCurrent nil)                                 
    	(setq rmax (length segment6numerical))                              
    	(while  (< r rmax)                                  
   	(setq positionListCurrent (nth r segment6numerical))                        
      	(setq roomCurrent (caddr positionListCurrent))                          
      	(setq segment6Rooms (cons roomCurrent segment6Rooms ))                       
      	(setq r (+ 1 r))                                
  	)
  	(setq segment6Rooms (reverse segment6Rooms))

    	(setq r 0)                                   
    	(setq roomCurrent nil)                                 
    	(setq rmax (length segment7numerical))                              
    	(while  (< r rmax)                                  
   	(setq positionListCurrent (nth r segment7numerical))                        
      	(setq roomCurrent (caddr positionListCurrent))                          
      	(setq segment7Rooms (cons roomCurrent segment7Rooms ))                       
      	(setq r (+ 1 r))                                
  	)
  	(setq segment7Rooms (reverse segment7Rooms))
  
    	(setq r 0)                                   
    	(setq roomCurrent nil)                                 
    	(setq rmax (length segment8numerical))                              
    	(while  (< r rmax)                                  
   	(setq positionListCurrent (nth r segment8numerical))                        
      	(setq roomCurrent (caddr positionListCurrent))                          
      	(setq segment8Rooms (cons roomCurrent segment8Rooms ))                       
      	(setq r (+ 1 r))                                
  	)
  	(setq segment8Rooms (reverse segment8Rooms))

      	(setq r 0)                                   
    	(setq roomCurrent nil)                                 
    	(setq rmax (length segment9numerical))                              
    	(while  (< r rmax)                                  
   	(setq positionListCurrent (nth r segment9numerical))                        
      	(setq roomCurrent (caddr positionListCurrent))                          
      	(setq segment9Rooms (cons roomCurrent segment9Rooms ))                       
      	(setq r (+ 1 r))                                
  	)
  	(setq segment9Rooms (reverse segment9Rooms))

      	(setq r 0)                                   
    	(setq roomCurrent nil)                                 
    	(setq rmax (length segment10numerical))                              
    	(while  (< r rmax)                                  
   	(setq positionListCurrent (nth r segment10numerical))                        
      	(setq roomCurrent (caddr positionListCurrent))                          
      	(setq segment10Rooms (cons roomCurrent segment10Rooms ))                       
      	(setq r (+ 1 r))                                
  	)
  	(setq segment10Rooms (reverse segment10Rooms))

  	(setq r 0)																														
  	(setq roomCurrent nil)																		
  	(setq rmax (length segment1Anumerical))															
  	(while 	(< r rmax)																	
		(setq positionListCurrent (nth r segment1Anumerical))												
	  	(setq roomCurrent (caddr positionListCurrent))													
  		(setq segment1ARooms (cons roomCurrent segment1ARooms ))												
	  	(setq r (+ 1 r))															
	)
	(setq segment1ARooms (reverse segment1ARooms))
  
    	(setq r 0)                                   
    	(setq roomCurrent nil)                                 
    	(setq rmax (length segment2Anumerical))                              
    	(while  (< r rmax)                                  
   	(setq positionListCurrent (nth r segment2Anumerical))                        
      	(setq roomCurrent (caddr positionListCurrent))                          
      	(setq segment2ARooms (cons roomCurrent segment2ARooms ))                       
      	(setq r (+ 1 r))                                
  	)
  	(setq segment2ARooms (reverse segment2ARooms))                            

    	(setq r 0)                                   
    	(setq roomCurrent nil)                                 
    	(setq rmax (length segment3Anumerical))                              
    	(while  (< r rmax)                                  
   	(setq positionListCurrent (nth r segment3Anumerical))                        
      	(setq roomCurrent (caddr positionListCurrent))                          
      	(setq segment3ARooms (cons roomCurrent segment3ARooms ))                       
      	(setq r (+ 1 r))                                
  	)
  	(setq segment3ARooms (reverse segment3ARooms))

    	(setq r 0)                                   
    	(setq roomCurrent nil)                                 
    	(setq rmax (length segment4Anumerical))                              
    	(while  (< r rmax)                                  
   	(setq positionListCurrent (nth r segment4Anumerical))                        
      	(setq roomCurrent (caddr positionListCurrent))                          
      	(setq segment4ARooms (cons roomCurrent segment4ARooms ))                       
      	(setq r (+ 1 r))                                
  	)
  	(setq segment4ARooms (reverse segment4ARooms))

    	(setq r 0)                                   
    	(setq roomCurrent nil)                                 
    	(setq rmax (length segment5Anumerical))                              
    	(while  (< r rmax)                                  
   	(setq positionListCurrent (nth r segment5Anumerical))                        
      	(setq roomCurrent (caddr positionListCurrent))                          
      	(setq segment5ARooms (cons roomCurrent segment5ARooms ))                       
      	(setq r (+ 1 r))                                
  	)
  	(setq segment5ARooms (reverse segment5ARooms))

      	(setq r 0)                                   
    	(setq roomCurrent nil)                                 
    	(setq rmax (length segment6Anumerical))                              
    	(while  (< r rmax)                                  
   	(setq positionListCurrent (nth r segment6Anumerical))                        
      	(setq roomCurrent (caddr positionListCurrent))                          
      	(setq segment6Rooms (cons roomCurrent segment6ARooms ))                       
      	(setq r (+ 1 r))                                
  	)
  	(setq segment6ARooms (reverse segment6ARooms))

    	(setq r 0)                                   
    	(setq roomCurrent nil)                                 
    	(setq rmax (length segment7Anumerical))                              
    	(while  (< r rmax)                                  
   	(setq positionListCurrent (nth r segment7Anumerical))                        
      	(setq roomCurrent (caddr positionListCurrent))                          
      	(setq segment7ARooms (cons roomCurrent segment7ARooms ))                       
      	(setq r (+ 1 r))                                
  	)
  	(setq segment7ARooms (reverse segment7ARooms))
  
    	(setq r 0)                                   
    	(setq roomCurrent nil)                                 
    	(setq rmax (length segment8Anumerical))                              
    	(while  (< r rmax)                                  
   	(setq positionListCurrent (nth r segment8Anumerical))                        
      	(setq roomCurrent (caddr positionListCurrent))                          
      	(setq segment8ARooms (cons roomCurrent segment8ARooms ))                       
      	(setq r (+ 1 r))                                
  	)
  	(setq segment8ARooms (reverse segment8ARooms))

      	(setq r 0)                                   
    	(setq roomCurrent nil)                                 
    	(setq rmax (length segment9Anumerical))                              
    	(while  (< r rmax)                                  
   	(setq positionListCurrent (nth r segment9Anumerical))                        
      	(setq roomCurrent (caddr positionListCurrent))                          
      	(setq segment9ARooms (cons roomCurrent segment9ARooms ))                       
      	(setq r (+ 1 r))                                
  	)
  	(setq segment9ARooms (reverse segment9ARooms))

      	(setq r 0)                                   
    	(setq roomCurrent nil)                                 
    	(setq rmax (length segment10Anumerical))                              
    	(while  (< r rmax)                                  
   	(setq positionListCurrent (nth r segment10Anumerical))                        
      	(setq roomCurrent (caddr positionListCurrent))                          
      	(setq segment10ARooms (cons roomCurrent segment10ARooms ))                       
      	(setq r (+ 1 r))                                
  	)
  	(setq segment10ARooms (reverse segment10ARooms))  

  	(setq rowCounter 0)																	;set rowCounter
	(setq columnCounter 0)																	;set columnCounter
  	(setq roomCounter 0)																	;counter to 0
  	(setq roomCounterMax (length singleLineRoomList))													;max to length of set
  	(setq productCounterMax (length singleLineProductList))										
  	(setq singleLineRoomList (reverse singleLineRoomList))													;reverse to cancel cons property
  	(setq singleLineProductList (reverse singleLineProductList))
	(setq uniqueRoomList nil)

  	(setq roomText (nth roomCounter singleLineRoomList))													;roomText to nth counter of singleLineRoomList -- parrallel list
  	(setq singleLineRoomListCurrent (nth roomCounter singleLineRoomList))											;singleLineRoomListCurrent to same
  	(setq singleLineProductListCurrent (nth roomCounter singleLineProductList))										;singleLineProductListCurrent to nth counter of singleLineProduct list -- parrallel list
  	(setq workingRoom singleLineRoomListCurrent)														;set workingRoom to singleLineRoomListCurrent -- priming read
 	(command "cmdecho" 0)																	;supress command line
	(while	(<= roomCounter roomCounterMax)															;loop until max
	  
  			(setq workingRoom singleLineRoomListCurrent)												;set workingRoom to singleLineRoomListCurrent
	    		(setq singleLineRoomListCurrent (nth roomCounter singleLineRoomList))									;set variables to matching elements in parallel lists
	  		(setq singleLineProductListCurrent (nth roomCounter singleLineProductList))		
	  			(cond																;conditional block
					
					((/= workingRoom singleLineRoomListCurrent)										;second conditional -- while workingRoom and singleLineRoomListCurrent do not match
					 	(setq roomProductNumber (length roomProducts))									;set roomProductNumber to the total number of products stored to roomProducts
						(setq roomProductCounter 0)											;reset counter
						(setq roomSublist nil)												;reset sublist
						(setq roomDottedPairs nil)											;reset dotted pairs
						(setq roomDottedPairsAlphabetical nil)										;reset alphabetical
					 
						(while 	(< roomProductCounter roomProductNumber)								;nested while -- loop through each product in the room
							(setq roomProductCurrent (nth roomProductCounter roomProducts))						;pull nth item off list
							(setq roomSublist (cons roomProductCurrent roomText))							;add the product plus roomname to sublist
						  	(setq roomDottedPairs (cons roomSublist roomDottedPairs))      						;add the sublist to the dotted pairs
							(setq roomProductCounter (+ 1 roomProductCounter))							;increment counter
						)														;end nested while


						(setq roomProductsDottedCounts nil)										;nil roomProductsDottedCounts
						(while roomProducts												;duplicate counter function -- loop for creating list of each component dotted with how many are in each room
        						(setq x (car roomProducts)										;set x to first element of roomProducts
              						c (length roomProducts)											;set c to length of roomProducts list
              						roomProducts (vl-remove x (cdr roomProducts))								;set roomProducts to list minus the current x value -- pulls one off the list each time
              						roomProductsDottedCounts (cons (cons x (- c (length roomProducts))) roomProductsDottedCounts)		;create the list of elements with dotted number of occurances
        						)
    						)
    						(reverse roomProductsDottedCounts)										;reverse to account for cons property
					 
					 	(setq roomProductCurrent nil)											;nil room current
					 	(setq roomProductCount 0)											;set duplicate product counter to 0
						(setq roomProductsDottedCounter 0)										;set counter to 0
						(setq roomProductsDottedMax (length roomProductsDottedCounts))							;set counter max

					 		
					 	(while	(< roomProductsDottedCounter roomProductsDottedMax)							;while loop -- loop through each item in the room and contruct a new product list based on the DLM product hierarchy 
						  	(setq roomProductsDottedCurrent (nth roomProductsDottedCounter roomProductsDottedCounts))		;set roomProductsDottedCurrent to the nth element corresponding to roomProductsDottedCounter of the list roomProductsDottedCounts
						  	(setq roomProductCurrent (car roomProductsDottedCurrent))						;set roomProductCurrent to first entity of roomProductsDottedCurrent -- the DLM product name
						  	(setq roomProductCount (cdr roomProductsDottedCurrent))							;set roomProductCount to second entity of roomProductsDottedCurrent -- number of times DLM product appears in this room -- found from duplicate counter function
						  	(setq roomProductsDottedCounter (+ 1 roomProductsDottedCounter))					;increment counter for each loop
							
						  	(cond													;conditional block
	
								((= roomProductCurrent "LMBC-300")								;first conditional statement -- for LMBC-300's
								(setq LMBC-300count roomProductCount)								;set LMBC-300count to the total number of products -- this gives us that max number of times to check for a LMBC-300 in this room
								(setq condCounter 0)										;set condCounter to 0 (to document number of times we count a LMBC-300
								(setq condCounterMax LMBC-300count)								;set condCounterMax to the LMBC-300count
								(while	(< condCounter condCounterMax)								;while loop -- loop through each element in the list
								  	(setq LMBC-300 (cons roomProductCurrent LMBC-300))					;add a "LMBC-300" text string to the LMBC-300 variable each time we get a match
								  	(setq condCounter (+ 1 condCounter))							;increment condCounter each time we have a match
								)												;end while
								)												;end conditional

								((= roomProductCurrent "LMCP48")								;second conditional statement -- same structure as above
                						(setq LMCP48count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMCP48count)
								(while	(< condCounter condCounterMax)
								  	(setq LMCP48 (cons roomProductCurrent LMCP48))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "LMCP24")								
                						(setq LMCP24count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMCP24count)
								(while	(< condCounter condCounterMax)
								  	(setq LMCP24 (cons roomProductCurrent LMCP24))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "LMCP12")								
                						(setq LMCP12count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMCP12count)
								(while	(< condCounter condCounterMax)
								  	(setq LMCP12 (cons roomProductCurrent LMCP12))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "LMCP8")								
                						(setq LMCP8count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMCP8count)
								(while	(< condCounter condCounterMax)
								  	(setq LMCP8 (cons roomProductCurrent LMCP8))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)


								((= roomProductCurrent "LMNC")								
                						(setq LMNCcount roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMNCcount)
								(while	(< condCounter condCounterMax)
								  	(setq LMNC (cons roomProductCurrent LMNC))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "LMSM-3E")								
                						(setq LMSM-3Ecount roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMSM-3Ecount)
								(while	(< condCounter condCounterMax)
								  	(setq LMSM-3E (cons roomProductCurrent LMSM-3E))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "LMSM-6E")								
                						(setq LMSM-6Ecount roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMSM-6Ecount)
								(while	(< condCounter condCounterMax)
								  	(setq LMSM-6E (cons roomProductCurrent LMSM-6E))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)
								
								((= roomProductCurrent "LIC48")								;second conditional statement -- same structure as above
                						(setq LIC48count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LIC48count)
								(while	(< condCounter condCounterMax)
								  	(setq LIC48 (cons roomProductCurrent LIC48))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "LIC24")								
                						(setq LIC24count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LIC24count)
								(while	(< condCounter condCounterMax)
								  	(setq LIC24 (cons roomProductCurrent LIC24))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)


								((= roomProductCurrent "LIC8")								
                						(setq LIC8count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LIC8count)
								(while	(< condCounter condCounterMax)
								  	(setq LIC8 (cons roomProductCurrent LIC8))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "LICA48")								;second conditional statement -- same structure as above
                						(setq LICA48count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LICA48count)
								(while	(< condCounter condCounterMax)
								  	(setq LICA48 (cons roomProductCurrent LICA48))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "LICA24")								
                						(setq LICA24count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LICA24count)
								(while	(< condCounter condCounterMax)
								  	(setq LICA24 (cons roomProductCurrent LICA24))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)


								((= roomProductCurrent "LICA8")								
                						(setq LICA8count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LICA8count)
								(while	(< condCounter condCounterMax)
								  	(setq LICA8 (cons roomProductCurrent LICA8))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "LILM48")								;second conditional statement -- same structure as above
                						(setq LILM48count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LILM48count)
								(while	(< condCounter condCounterMax)
								  	(setq LILM48 (cons roomProductCurrent LILM48))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "LILM24")								
                						(setq LILM24count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LILM24count)
								(while	(< condCounter condCounterMax)
								  	(setq LILM24 (cons roomProductCurrent LILM24))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "LILM8")								
                						(setq LILM8count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LILM8count)
								(while	(< condCounter condCounterMax)
								  	(setq LILM8 (cons roomProductCurrent LILM8))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)


								((= roomProductCurrent "LIC8")								
                						(setq LILM8count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LILM8count)
								(while	(< condCounter condCounterMax)
								  	(setq LILM8 (cons roomProductCurrent LILM8))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "WBT-900-IP")
                						(setq WBT-900-IPcount roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax WBT-900-IPcount)
								(while	(< condCounter condCounterMax)
								  	(setq WBT-900-IP (cons roomProductCurrent WBT-900-IP))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "NB-ROUTER")
                						(setq NB-ROUTERcount roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax NB-ROUTERcount)
								(while	(< condCounter condCounterMax)
								  	(setq NB-ROUTER (cons roomProductCurrent NB-ROUTER))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "NB-SWITCH")
                						(setq NB-SWITCHcount roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax NB-SWITCHcount)
								(while	(< condCounter condCounterMax)
								  	(setq NB-SWITCH (cons roomProductCurrent NB-SWITCH))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "LMSM-201")
                						(setq LMSM-201count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMSM-201count)
								(while	(< condCounter condCounterMax)
								  	(setq LMSM-201 (cons roomProductCurrent LMSM-201))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "LMSM-603")
                						(setq LMSM-603count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMSM-603count)
								(while	(< condCounter condCounterMax)
								  	(setq LMSM-603 (cons roomProductCurrent LMSM-603))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "LMSM-600")
                						(setq LMSM-600count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMSM-600count)
								(while	(< condCounter condCounterMax)
								  	(setq LMSM-600 (cons roomProductCurrent LMSM-600))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)
								
								((= roomProductCurrent "LMDI-100")
                						(setq LMDI-100count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMDI-100count)
								(while	(< condCounter condCounterMax)
								  	(setq LMDI-100 (cons roomProductCurrent LMDI-100))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)
								
								((= roomProductCurrent "LMIO-301")								
								(setq LMIO-301count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMIO-301count)
								(while	(< condCounter condCounterMax)
								  	(setq LMIO-301 (cons roomProductCurrent LMIO-301))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMIO-201")								
								(setq LMIO-201count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMIO-201count)
								(while	(< condCounter condCounterMax)
								  	(setq LMIO-201 (cons roomProductCurrent LMIO-201))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMIO-102")								
								(setq LMIO-102count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMIO-102count)
								(while	(< condCounter condCounterMax)
								  	(setq LMIO-102 (cons roomProductCurrent LMIO-102))
								  	(setq condCounter (+ 1 condCounter))
								)
								)
								
								((= roomProductCurrent "LMIO-101")								
								(setq LMIO-101count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMIO-101count)
								(while	(< condCounter condCounterMax)
								  	(setq LMIO-101 (cons roomProductCurrent LMIO-101))
								  	(setq condCounter (+ 1 condCounter))
								)
								)
								
                						((= roomProductCurrent "LMRL-100")
                						(setq LMRL-100count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMRL-100count)
								(while	(< condCounter condCounterMax)
								  	(setq LMRL-100 (cons roomProductCurrent LMRL-100))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "LMPB-100")
                						(setq LMPB-100count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMPB-100count)
								(while	(< condCounter condCounterMax)
								  	(setq LMPB-100 (cons roomProductCurrent LMPB-100))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "LMIR-100")
                						(setq LMIR-100count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMIR-100count)
								(while	(< condCounter condCounterMax)
								  	(setq LMIR-100 (cons roomProductCurrent LMIR-100))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "LMRH-101")
                						(setq LMRH-101count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMRH-101count)
								(while	(< condCounter condCounterMax)
								  	(setq LMRH-101 (cons roomProductCurrent LMRH-101))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "LMRH-102")
                						(setq LMRH-102count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMRH-102count)
								(while	(< condCounter condCounterMax)
								  	(setq LMRH-102 (cons roomProductCurrent LMRH-102))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "LMRH-105")
                						(setq LMRH-105count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMRH-105count)
								(while	(< condCounter condCounterMax)
								  	(setq LMRH-105 (cons roomProductCurrent LMRH-105))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "LMRJ-C8")
                						(setq LMRJ-C8count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMRJ-C8count)
								(while	(< condCounter condCounterMax)
								  	(setq LMRJ-C8 (cons roomProductCurrent LMRJ-C8))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "LMRJ-CS8")
                						(setq LMRJ-CS8count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMRJ-CS8count)
								(while	(< condCounter condCounterMax)
								  	(setq LMRJ-CS8 (cons roomProductCurrent LMRJ-CS8))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "LMRJ-S8")
                						(setq LMRJ-S8count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMRJ-S8count)
								(while	(< condCounter condCounterMax)
								  	(setq LMRJ-S8 (cons roomProductCurrent LMRJ-S8))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)
								
                            					((= roomProductCurrent "LMRC-222")
                            					(setq LMRC-222count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMRC-222count)
								(while	(< condCounter condCounterMax)
								  	(setq LMRC-222 (cons roomProductCurrent LMRC-222))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LMRC-221")
                            					(setq LMRC-221count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMRC-221count)
								(while	(< condCounter condCounterMax)
								  	(setq LMRC-221 (cons roomProductCurrent LMRC-221))
								  	(setq condCounter (+ 1 condCounter))
								) 
                            					)

								((= roomProductCurrent "LMRC-213")
                            					(setq LMRC-213count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMRC-213count)
								(while	(< condCounter condCounterMax)
								  	(setq LMRC-213 (cons roomProductCurrent LMRC-213))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LMRC-213-347v")
                            					(setq LMRC-213-347vcount roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMRC-213-347vcount)
								(while	(< condCounter condCounterMax)
								  	(setq LMRC-213-347v (cons roomProductCurrent LMRC-213-347v))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)
								
								((= roomProductCurrent "LMRC-212")
                            					(setq LMRC-212count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMRC-212count)
								(while	(< condCounter condCounterMax)
								  	(setq LMRC-212 (cons roomProductCurrent LMRC-212))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LMRC-212-347v")
                            					(setq LMRC-212-347vcount roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMRC-212-347vcount)
								(while	(< condCounter condCounterMax)
								  	(setq LMRC-212-347v (cons roomProductCurrent LMRC-212-347v))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LMRC-211")
                            					(setq LMRC-211count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMRC-211count)
								(while	(< condCounter condCounterMax)
								  	(setq LMRC-211 (cons roomProductCurrent LMRC-211))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LMRC-211-347v")
                            					(setq LMRC-211-347vcount roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMRC-211-347vcount)
								(while	(< condCounter condCounterMax)
								  	(setq LMRC-211-347v (cons roomProductCurrent LMRC-211-347v))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LMRC-112")
                            					(setq LMRC-112count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMRC-112count)
								(while	(< condCounter condCounterMax)
								  	(setq LMRC-112 (cons roomProductCurrent LMRC-112))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LMRC-112-M")
                            					(setq LMRC-112-Mcount roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMRC-112-Mcount)
								(while	(< condCounter condCounterMax)
								  	(setq LMRC-112-M (cons roomProductCurrent LMRC-112-M))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LMRC-111")
                            					(setq LMRC-111count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMRC-111count)
								(while	(< condCounter condCounterMax)
								  	(setq LMRC-111 (cons roomProductCurrent LMRC-111))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LMRC-111-M")
                            					(setq LMRC-111-Mcount roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMRC-111-Mcount)
								(while	(< condCounter condCounterMax)
								  	(setq LMRC-111-M (cons roomProductCurrent LMRC-111-M))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LMRC-102")								
								(setq LMRC-102count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMRC-102count)
								(while	(< condCounter condCounterMax)
								  	(setq LMRC-102 (cons roomProductCurrent LMRC-102))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMRC-101")								
								(setq LMRC-101count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMRC-101count)
								(while	(< condCounter condCounterMax)
								  	(setq LMRC-101 (cons roomProductCurrent LMRC-101))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMRC-100")								
								(setq LMRC-100count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMRC-100count)
								(while	(< condCounter condCounterMax)
								  	(setq LMRC-100 (cons roomProductCurrent LMRC-100))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMFC-011")								
								(setq LMFC-011count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMFC-011count)
								(while	(< condCounter condCounterMax)
								  	(setq LMFC-011 (cons roomProductCurrent LMFC-011))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMZC-301")								
								(setq LMZC-301count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMZC-301count)
								(while	(< condCounter condCounterMax)
								  	(setq  LMZC-301(cons roomProductCurrent LMZC-301))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMPL-201")								
								(setq LMPL-201count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMPL-201count)
								(while	(< condCounter condCounterMax)
								  	(setq  LMPL-201(cons roomProductCurrent LMPL-201))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMPL-101")								
								(setq LMPL-101count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMPL-101count)
								(while	(< condCounter condCounterMax)
								  	(setq  LMPL-101(cons roomProductCurrent LMPL-101))
								  	(setq condCounter (+ 1 condCounter))
								)
								)
							
								((= roomProductCurrent "LMDC-100")
                            					(setq LMDC-100count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMDC-100count)
								(while	(< condCounter condCounterMax)
								  	(setq LMDC-100 (cons roomProductCurrent LMDC-100))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMDX-100")
								(setq LMDX-100count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMDX-100count)
								(while	(< condCounter condCounterMax)
								  	(setq LMDX-100 (cons roomProductCurrent LMDX-100))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMPC-100")								
								(setq LMPC-100count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMPC-100count)
								(while	(< condCounter condCounterMax)
								  	(setq  LMPC-100(cons roomProductCurrent LMPC-100))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMPC-100-1")								
								(setq LMPC-100-1count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMPC-100-1count)
								(while	(< condCounter condCounterMax)
								  	(setq  LMPC-100-1(cons roomProductCurrent LMPC-100-1))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMPC-100-5")								
								(setq LMPC-100-5count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMPC-100-5count)
								(while	(< condCounter condCounterMax)
								  	(setq  LMPC-100-5(cons roomProductCurrent LMPC-100-5))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMPX-100")								
								(setq LMPX-100count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMPX-100count)
								(while	(< condCounter condCounterMax)
								  	(setq  LMPX-100 (cons roomProductCurrent LMPX-100))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMPX-100-1")								
								(setq LMPX-100-1count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMPX-100-1count)
								(while	(< condCounter condCounterMax)
								  	(setq  LMPX-100-1 (cons roomProductCurrent LMPX-100-1))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMPX-100-3")								
								(setq LMPX-100-3count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMPX-100-3count)
								(while	(< condCounter condCounterMax)
								  	(setq  LMPX-100-3 (cons roomProductCurrent LMPX-100-3))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMPX-100-4")								
								(setq LMPX-100-4count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMPX-100-4count)
								(while	(< condCounter condCounterMax)
								  	(setq  LMPX-100-4 (cons roomProductCurrent LMPX-100-4))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMUC-100-2")								
								(setq LMUC-100-2count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMUC-100-2count)
								(while	(< condCounter condCounterMax)
								  	(setq  LMUC-100-2 (cons roomProductCurrent LMUC-100-2))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMUC-200")								
								(setq LMUC-200count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMUC-200count)
								(while	(< condCounter condCounterMax)
								  	(setq  LMUC-200 (cons roomProductCurrent LMUC-200))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMDM-101")
                            					(setq LMDM-101count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMDM-101count)
								(while	(< condCounter condCounterMax)
								  	(setq LMDM-101 (cons roomProductCurrent LMDM-101))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LMSW-108")
                            					(setq LMSW-108count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMSW-108count)
								(while	(< condCounter condCounterMax)
								  	(setq LMSW-108 (cons roomProductCurrent LMSW-108))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LMSW-105")
                            					(setq LMSW-105count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMSW-105count)
								(while	(< condCounter condCounterMax)
								  	(setq LMSW-105 (cons roomProductCurrent LMSW-105))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LMSW-104")
                            					(setq LMSW-104count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMSW-104count)
								(while	(< condCounter condCounterMax)
								  	(setq LMSW-104 (cons roomProductCurrent LMSW-104))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LMSW-103")
                            					(setq LMSW-103count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMSW-103count)
								(while	(< condCounter condCounterMax)
								  	(setq LMSW-103 (cons roomProductCurrent LMSW-103))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LMSW-102")
                            					(setq LMSW-102count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMSW-102count)
								(while	(< condCounter condCounterMax)
								  	(setq LMSW-102 (cons roomProductCurrent LMSW-102))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LMSW-101")
                            					(setq LMSW-101count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMSW-101count)
								(while	(< condCounter condCounterMax)
								  	(setq LMSW-101 (cons roomProductCurrent LMSW-101))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LMPW-102")								
								(setq LMPW-102count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMPW-102count)
								(while	(< condCounter condCounterMax)
								  	(setq  LMPW-102 (cons roomProductCurrent LMPW-102))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMPW-101")								
								(setq LMPW-101count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMPW-101count)
								(while	(< condCounter condCounterMax)
								  	(setq  LMPW-101 (cons roomProductCurrent LMPW-101))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMDW-102")								
								(setq LMDW-102count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMDW-102count)
								(while	(< condCounter condCounterMax)
								  	(setq  LMDW-102 (cons roomProductCurrent LMDW-102))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMDW-101")								
								(setq LMDW-101count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMDW-101count)
								(while	(< condCounter condCounterMax)
								  	(setq  LMDW-101 (cons roomProductCurrent LMDW-101))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMPS-104")								
								(setq LMPS-104count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMPS-104count)
								(while	(< condCounter condCounterMax)
								  	(setq  LMPS-104 (cons roomProductCurrent LMPS-104))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "LMLS-600")
                            					(setq LMLS-600count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMLS-600count)
								(while	(< condCounter condCounterMax)
								  	(setq LMLS-600 (cons roomProductCurrent LMLS-600))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LMLS-500")
                            					(setq LMLS-500count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMLS-500count)
								(while	(< condCounter condCounterMax)
								  	(setq LMLS-500 (cons roomProductCurrent LMLS-500))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LMLS-400")
                            					(setq LMLS-400count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMLS-400count)
								(while	(< condCounter condCounterMax)
								  	(setq LMLS-400 (cons roomProductCurrent LMLS-400))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LMLS-105")
                            					(setq LMLS-105count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMLS-105count)
								(while	(< condCounter condCounterMax)
								  	(setq LMLS-105 (cons roomProductCurrent LMLS-105))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LMLS-305")
                            					(setq LMLS-305count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMLS-305count)
								(while	(< condCounter condCounterMax)
								  	(setq LMLS-305 (cons roomProductCurrent LMLS-305))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LMPO-200")
                            					(setq LMPO-200count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMPO-200count)
								(while	(< condCounter condCounterMax)
								  	(setq LMPO-200 (cons roomProductCurrent LMPO-200))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LMPS-6000")
                            					(setq LMPS-6000count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LMPS-6000count)
								(while	(< condCounter condCounterMax)
								  	(setq LMPS-6000 (cons roomProductCurrent LMPS-6000))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "ELCU-200")
                						(setq ELCU-200count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax ELCU-200count)
								(while	(< condCounter condCounterMax)
								  	(setq ELCU-200 (cons roomProductCurrent ELCU-200))
								  	(setq condCounter (+ 1 condCounter))
								)
                						)

								((= roomProductCurrent "ELCU-100")								
								(setq ELCU-100count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax ELCU-100count)
								(while	(< condCounter condCounterMax)
								  	(setq ELCU-100 (cons roomProductCurrent ELCU-100))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "BZ-250")
                            					(setq BZ-250count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax BZ-250count)
								(while	(< condCounter condCounterMax)
								  	(setq BZ-250 (cons roomProductCurrent BZ-250))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "BZ-250-347")
                            					(setq BZ-250-347count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax BZ-250-347count)
								(while	(< condCounter condCounterMax)
								  	(setq BZ-250-347 (cons roomProductCurrent BZ-250-347))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "BZ-150")
                            					(setq BZ-150count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax BZ-150count)
								(while	(< condCounter condCounterMax)
								  	(setq BZ-150 (cons roomProductCurrent BZ-150))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "BZ-50")
                            					(setq BZ-50count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax BZ-50count)
								(while	(< condCounter condCounterMax)
								  	(setq BZ-50 (cons roomProductCurrent BZ-50))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "HB-300-L1")								
								(setq HB-300-L1count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax HB-300-L1count)
								(while	(< condCounter condCounterMax)
								  	(setq HB-300-L1 (cons roomProductCurrent HB-300-L1))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "HB-300-L1M")								
								(setq HB-300-L1Mcount roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax HB-300-L1Mcount)
								(while	(< condCounter condCounterMax)
								  	(setq HB-300-L1M (cons roomProductCurrent HB-300-L1M))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "HB-300-L2")								
								(setq HB-300-L2count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax HB-300-L2count)
								(while	(< condCounter condCounterMax)
								  	(setq HB-300-L2 (cons roomProductCurrent HB-300-L2))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "HB-300-L3")								
								(setq HB-300-L3count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax HB-300-L3count)
								(while	(< condCounter condCounterMax)
								  	(setq HB-300-L3(cons roomProductCurrent HB-300-L3))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "HB-300-L3W")								
								(setq HB-300-L3Wcount roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax HB-300-L3Wcount)
								(while	(< condCounter condCounterMax)
								  	(setq HB-300-L3W (cons roomProductCurrent HB-300-L3W))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "HB-300-L4")								
								(setq HB-300-L4count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax HB-300-L4count)
								(while	(< condCounter condCounterMax)
								  	(setq HB-300-L4 (cons roomProductCurrent HB-300-L4))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "HB-300-L4W")								
								(setq HB-300-L4Wcount roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax HB-300-L4Wcount)
								(while	(< condCounter condCounterMax)
								  	(setq HB-300-L4W (cons roomProductCurrent HB-300-L4W))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "HB340B-L3")								
								(setq HB340B-L3count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax HB340B-L3count)
								(while	(< condCounter condCounterMax)
								  	(setq HB340B-L3 (cons roomProductCurrent HB340B-L3))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "HB-350-L1")								
								(setq HB-350-L1count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax HB-350-L1count)
								(while	(< condCounter condCounterMax)
								  	(setq HB-350-L1 (cons roomProductCurrent HB-350-L1))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "HB-350-L1m")								
								(setq HB-350-L1mcount roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax HB-350-L1mcount)
								(while	(< condCounter condCounterMax)
								  	(setq HB-350-L1m (cons roomProductCurrent HB-350-L1m))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "HB-350-L2")								
								(setq HB-350-L2count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax HB-350-L2count)
								(while	(< condCounter condCounterMax)
								  	(setq HB-350-L2 (cons roomProductCurrent HB-350-L2))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "HB-350-L3")								
								(setq HB-350-L3count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax HB-350-L3count)
								(while	(< condCounter condCounterMax)
								  	(setq HB-350-L3 (cons roomProductCurrent HB-350-L3))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "HB-350-L3W")								
								(setq HB-350-L3Wcount roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax HB-350-L3Wcount)
								(while	(< condCounter condCounterMax)
								  	(setq HB-350-L3W (cons roomProductCurrent HB-350-L3W))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "HB-350-L4")								
								(setq HB-350-L4count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax HB-350-L4count)
								(while	(< condCounter condCounterMax)
								  	(setq HB-350-L4 (cons roomProductCurrent HB-350-L4))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "HB-350-L4W")								
								(setq HB-350-L4Wcount roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax HB-350-L4Wcount)
								(while	(< condCounter condCounterMax)
								  	(setq HB-350-L4W (cons roomProductCurrent HB-350-L4W))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "HBP")								
								(setq HBPcount roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax HBPcount)
								(while	(< condCounter condCounterMax)
								  	(setq HBP (cons roomProductCurrent HBP))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								
								((= roomProductCurrent "CB-100")								
								(setq CB-100count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax CB-100count)
								(while	(< condCounter condCounterMax)
								  	(setq CB-100 (cons roomProductCurrent CB-100))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "CB-100-3")								
								(setq CB-100-3count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax CB-100-3count)
								(while	(< condCounter condCounterMax)
								  	(setq CB-100-3 (cons roomProductCurrent CB-100-3))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "EW-200")								
								(setq EW-200count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax EW-200count)
								(while	(< condCounter condCounterMax)
								  	(setq EW-200 (cons roomProductCurrent EW-200))
								  	(setq condCounter (+ 1 condCounter))
								)
								)

								((= roomProductCurrent "DT-300")
                            					(setq DT-300count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax DT-300count)
								(while	(< condCounter condCounterMax)
								  	(setq DT-300 (cons roomProductCurrent DT-300))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "DT-355")
                            					(setq DT-355count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax DT-355count)
								(while	(< condCounter condCounterMax)
								  	(setq DT-355 (cons roomProductCurrent DT-355))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)


								((= roomProductCurrent "UT-355-2")
                            					(setq UT-355-2count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax UT-355-2count)
								(while	(< condCounter condCounterMax)
								  	(setq UT-355-2 (cons roomProductCurrent UT-355-2))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "UT-300-2")
                            					(setq UT-300-2count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax UT-300-2count)
								(while	(< condCounter condCounterMax)
								  	(setq UT-300-2 (cons roomProductCurrent UT-300-2))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "UT-300-1")
                            					(setq UT-300-1count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax UUT-300-1count)
								(while	(< condCounter condCounterMax)
								  	(setq UT-300-1 (cons roomProductCurrent UT-300-1))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LVSW-108")
                            					(setq LVSW-108count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LVSW-108count)
								(while	(< condCounter condCounterMax)
								  	(setq LVSW-108 (cons roomProductCurrent LVSW-108))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LVSW-104")
                            					(setq LVSW-104count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LVSW-104count)
								(while	(< condCounter condCounterMax)
								  	(setq LVSW-104 (cons roomProductCurrent LVSW-104))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LVSW-103")
                            					(setq LVSW-103count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LVSW-103count)
								(while	(< condCounter condCounterMax)
								  	(setq LVSW-103 (cons roomProductCurrent LVSW-103))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LVSW-102")
                            					(setq LVSW-102count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LVSW-102count)
								(while	(< condCounter condCounterMax)
								  	(setq LVSW-102 (cons roomProductCurrent LVSW-102))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LVSW-101")
                            					(setq LVSW-101count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LVSW-101count)
								(while	(< condCounter condCounterMax)
								  	(setq LVSW-101 (cons roomProductCurrent LVSW-101))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)
								
								((= roomProductCurrent "PW-100")
                            					(setq PW-100count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax PW-100count)
								(while	(< condCounter condCounterMax)
								  	(setq PW-100 (cons roomProductCurrent PW-100))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)


								((= roomProductCurrent "DSW-200")
                            					(setq DSW-200count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax DSW-200count)
								(while	(< condCounter condCounterMax)
								  	(setq DSW-200 (cons roomProductCurrent DSW-200))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "DSW-100")
                            					(setq DSW-100count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax DSW-100count)
								(while	(< condCounter condCounterMax)
								  	(setq DSW-100 (cons roomProductCurrent DSW-100))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)
								
								((= roomProductCurrent "DW-200")
                            					(setq DW-200count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax DW-200count)
								(while	(< condCounter condCounterMax)
								  	(setq DW-200 (cons roomProductCurrent DW-200))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "DW-100")
                            					(setq DW-100count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax DW-100count)
								(while	(< condCounter condCounterMax)
								  	(setq DW-100 (cons roomProductCurrent DW-100))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)


								((= roomProductCurrent "PW-100")
                            					(setq PW-100count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax PW-100count)
								(while	(< condCounter condCounterMax)
								  	(setq PW-100 (cons roomProductCurrent PW-100))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "PW-311")
                            					(setq PW-311count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax PW-311count)
								(while	(< condCounter condCounterMax)
								  	(setq PW-311 (cons roomProductCurrent PW-311))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "TS-400")
                            					(setq TS-400count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax TS-400count)
								(while	(< condCounter condCounterMax)
								  	(setq TS-400 (cons roomProductCurrent TS-400))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "ADF-120-277")
                            					(setq ADF-120-277count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax ADF-120-277count)
								(while	(< condCounter condCounterMax)
								  	(setq ADF-120-277 (cons roomProductCurrent ADF-120-277))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "LVS-1k")
                            					(setq LVS-1kcount roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LVS-1kcount)
								(while	(< condCounter condCounterMax)
								  	(setq LVS-1k (cons roomProductCurrent LVS-1k))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)
								((= roomProductCurrent "LVS-1K")
                            					(setq LVS-1kcount roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LVS-1kcount)
								(while	(< condCounter condCounterMax)
								  	(setq LVS-1k (cons roomProductCurrent LVS-1k))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)
								((= roomProductCurrent "V-EASYTOUCH-II-1")
                            					(setq V-EASYTOUCH-II-1count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax V-EASYTOUCH-II-1count)
								(while	(< condCounter condCounterMax)
								  	(setq V-EASYTOUCH-II-1 (cons roomProductCurrent V-EASYTOUCH-II-1))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)
								((= roomProductCurrent "V-EASYTOUCH-II-2")
                            					(setq V-EASYTOUCH-II-2count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax V-EASYTOUCH-II-2count)
								(while	(< condCounter condCounterMax)
								  	(setq V-EASYTOUCH-II-2 (cons roomProductCurrent V-EASYTOUCH-II-2))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)
								((= roomProductCurrent "V-EASYTOUCH-II-3")
                            					(setq V-EASYTOUCH-II-3count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax V-EASYTOUCH-II-3count)
								(while	(< condCounter condCounterMax)
								  	(setq V-EASYTOUCH-II-3 (cons roomProductCurrent V-EASYTOUCH-II-3))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)
								((= roomProductCurrent "V-EASYTOUCH-II-4")
                            					(setq V-EASYTOUCH-II-4count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax V-EASYTOUCH-II-4count)
								(while	(< condCounter condCounterMax)
								  	(setq V-EASYTOUCH-II-4 (cons roomProductCurrent V-EASYTOUCH-II-4))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)
								((= roomProductCurrent "V-EASYTOUCH-II-5")
                            					(setq V-EASYTOUCH-II-5count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax V-EASYTOUCH-II-5count)
								(while	(< condCounter condCounterMax)
								  	(setq V-EASYTOUCH-II-5 (cons roomProductCurrent V-EASYTOUCH-II-5))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)
								((= roomProductCurrent "DSW-301")
                            					(setq DSW-301count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax DSW-301count)
								(while	(< condCounter condCounterMax)
								  	(setq DSW-301 (cons roomProductCurrent DSW-301))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)
								((= roomProductCurrent "DSW-302")
                            					(setq DSW-302count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax DSW-302count)
								(while	(< condCounter condCounterMax)
								  	(setq DSW-302 (cons roomProductCurrent DSW-302))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)
								((= roomProductCurrent "DW-311")
                            					(setq DW-311count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax DW-311count)
								(while	(< condCounter condCounterMax)
								  	(setq DW-311 (cons roomProductCurrent DW-311))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)								
								((= roomProductCurrent "DT-305")
                            					(setq DT-305count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax DT-305count)
								(while	(< condCounter condCounterMax)
								  	(setq DT-305 (cons roomProductCurrent DT-305))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)

								((= roomProductCurrent "PW-301")
                            					(setq PW-301count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax PW-301count)
								(while	(< condCounter condCounterMax)
								  	(setq PW-301 (cons roomProductCurrent PW-301))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)
								((= roomProductCurrent "LVS-1")
                            					(setq LVS-1count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax LVS-1count)
								(while	(< condCounter condCounterMax)
								  	(setq LVS-1 (cons roomProductCurrent LVS-1))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)
								
								((= roomProductCurrent "PW-302-347")
                            					(setq PW-302-347count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax PW-302-347count)
								(while	(< condCounter condCounterMax)
								  	(setq PW-302-347 (cons roomProductCurrent PW-302-347))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)
								
								((= roomProductCurrent "ELCU-200-347")
                            					(setq ELCU-200-347count roomProductCount)
								(setq condCounter 0)
								(setq condCounterMax ELCU-200-347count)
								(while	(< condCounter condCounterMax)
								  	(setq ELCU-200-347 (cons roomProductCurrent ELCU-200-347))
								  	(setq condCounter (+ 1 condCounter))
								)
                            					)
								
							)													;end conditional block for building sublists for hierarchy
						)														;end while loop for each product inside this room


					 
						(setq roomHierarchy (append  	LMBC-300 LMCP48 LMCP24 LMCP12 LMCP8 LMNC LMSM-3E LMSM-6E LIC48 LIC24 LIC8 LICA48 LICA24 LICA8 LILM48 LILM24 LILM8 WBT-900-IP NB-ROUTER NB-SWITCH LMSM-201 LMSM-603 LMSM-600
								      		LMDI-100  LMIO-301  LMIO-201  LMIO-102  LMIO-101  LMRL-100
									     	LMPB-100 LMIR-100 LMRH-105 LMRH-102 LMRH-101 LMRJ-C8 LMRJ-CS8 LMRJ-S8
										LMRC-222  LMRC-221  LMRC-213 LMRC-213-347v  LMRC-212 LMRC-212-347v LMRC-211 LMRC-211-347v  LMRC-112 LMRC-112-M  LMRC-111 LMRC-111-M  LMRC-102 LMRC-101 LMRC-100  LMFC-011  LMZC-301  
										LMPL-201  LMPL-101
								      		LMDC-100  LMDX-100  LMPC-100 LMPC-100-1  LMPC-100-5  LMPX-100  LMPX-100-1  LMPX-100-3  LMPX-100-4  LMUC-100-2 LMUC-200
								      		LMDM-101  LMSW-108  LMSW-105  LMSW-104  LMSW-103  LMSW-102  LMSW-101 LMPW-102  LMPW-101  LMDW-102  LMDW-101  LMPS-104
								      		LMLS-600  LMLS-500  LMLS-400 LMLS-105 LMLS-305 LMPO-200 LMPS-6000
								      		ELCU-200 ELCU-100 ELCU-200-347
								      		DT-305 DT-355 DT-300 DT-200 DT-205 HBP HB-300-L1 HB-300-L1M HB-300-L2 HB-300-L3 HB-300-L3W HB-300-L4 HB-300-L4W HB340B-L3 HB-350-L1 HB-350-L1m HB-350-L2 HB-350-L3 HB-350-L3W HB-350-L4 HB-350-L4W CB-100 CB-100-3 EW-200 UT-355-2 UT-300-2 UT-300-1 LS-301
								      		LVSW-108 LVSW-104 LVSW-103 LVSW-102 LVSW-101 DSW-200 DSW-100 DW-200 DW-100 PW-311 PW-302-347 PW-100 TS-400 ADF-120-277 DSW-301 DSW-302 DW-311 LVS-1k-lowercase LVS-1K PW-301 LVS-1
								      		BZ-250 BZ-250-347 BZ-150 BZ-50
										V-EASYTOUCH-II-1 V-EASYTOUCH-II-2 V-EASYTOUCH-II-3 V-EASYTOUCH-II-4 V-EASYTOUCH-II-5 
										 
										
						))														;set roomListsTogether to the sum of all DLM product lists -- the order which they appear in this line creates the heirarchy
						     
						(setq LMBC-300 nil)												;nil the sublists for the heirarchy after the roomListsTogether is created
						(setq LMCP48 nil)
					 	(setq LMCP24 nil)
						(setq LMCP12 nil)
					 	(setq LMCP8 nil)
					 	(setq LMNC nil)
					 	(setq LMSM-3E nil)
					 	(setq LMSM-6E nil)
					 	(setq LIC48 nil)
					 	(setq LIC24 nil)
					 	(setq LIC8 nil)
					 	(setq LICA48 nil)
					 	(setq LICA24 nil)
					 	(setq LICA8 nil)
					 	(setq LILM48 nil)
					 	(setq LILM24 nil)
					 	(setq LILM8 nil)
					 	(setq LMBC-300 nil)
					 	(setq WBT-900-IP nil)
					 	(setq NB-ROUTER nil)
					 	(setq NB-SWITCH nil)
					 	(setq LMSM-201 nil)
					 	(setq LMSM-603 nil)
					 	(setq LMSM-600 nil)
					 	(setq LMDI-100 nil)
						(setq LMIO-301 nil)
						(setq LMIO-201 nil)
					 	(setq LMIO-102 nil)
					 	(setq LMIO-101 nil)
					 	(setq LMRL-100 nil)
					 	(setq LMPB-100 nil)
					 	(setq LMIR-100 nil)
					 	(setq LMRH-105 nil)
					 	(setq LMRH-102 nil)
					 	(setq LMRH-101 nil)
					 	(setq LMRJ-C8 nil)
					 	(setq LMRJ-CS8 nil)
					 	(setq LMRJ-S8 nil)
					 	(setq LMRC-222 nil)
					 	(setq LMRC-221 nil)
					 	(setq LMRC-213 nil)
					 	(setq LMRC-213-347v nil)
					 	(setq LMRC-212 nil)
					 	(setq LMRC-212-347v nil)
					 	(setq LMRC-211 nil)
					 	(setq LMRC-211-347v nil)
					 	(setq LMRC-112 nil)
					 	(setq LMRC-112-M nil)
					 	(setq LMRC-111 nil)
					 	(setq LMRC-111-M nil)
						(setq LMRC-102 nil)
					 	(setq LMRC-101 nil)
					 	(setq LMRC-100 nil)
					 	(setq LMFC-011 nil)
					 	(setq LMZC-301 nil)
						(setq LMPL-201 nil)
					 	(setq LMPL-101 nil)
					 	(setq LMDC-100 nil)
					 	(setq LMDX-100 nil)
					 	(setq LMPC-100 nil)
					 	(setq LMPC-100-1 nil)
					 	(setq LMPC-100-5 nil)
					 	(setq LMPX-100 nil)
					 	(setq LMPX-100-1 nil)
					 	(setq LMPX-100-3 nil)
					 	(setq LMPX-100-4 nil)
					 	(setq LMUC-100-2 nil)
					 	(setq LMUC-200 nil)
					 	(setq LMDM-101 nil)
					 	(setq LMSW-108 nil)
					 	(setq LMSW-105 nil)
					 	(setq LMSW-104 nil)
						(setq LMSW-103 nil)
					 	(setq LMSW-102 nil)
					 	(setq LMSW-101 nil)
					 	(setq LMPW-102 nil)
					 	(setq LMPW-101 nil)
					 	(setq LMDW-102 nil)
					 	(setq LMDW-101 nil)
					 	(setq LMPS-104 nil)
					 	(setq LMLS-600 nil)
					 	(setq LMLS-500 nil)
					 	(setq LMLS-400 nil)
					 	(setq LMLS-105 nil)
					 	(setq LMLS-305 nil)
					 	(setq LMPO-200 nil)
					 	(setq LMPS-6000 nil)
					 	(setq ELCU-200 nil)
						(setq ELCU-100 nil)
						(setq UT-355-2 nil)
						(setq UT-300-2 nil)
						(setq UT-300-1 nil)
					 	(setq DT-355 nil)
					 	(setq DT-300 nil)
					 	(setq HBP nil)
					 	(setq HB-300-L1 nil)
					 	(setq HB-300-L1M nil)
					 	(setq HB-300-L2 nil)
					 	(setq HB-300-L3 nil)
					 	(setq HB-300-L3W nil)
					 	(setq HB-300-L4 nil)
					 	(setq HB-300-L4W nil)
					 	(setq HB340B-L3 nil)
					 	(setq HB-350-L1 nil)
					 	(setq HB-350-L1m nil)
					 	(setq HB-350-L2 nil)
					 	(setq HB-350-L3 nil)
					 	(setq HB-350-L3W nil)
					 	(setq HB-350-L4 nil)
					 	(setq HB-350-L4W nil)
					 	(setq CB-100-3 nil)
						(setq CB-100 nil)
					 	(setq EW-200 nil)
					 	(setq LVSW-108 nil)
					 	(setq LVSW-104 nil)
					 	(setq LVSW-103 nil)
					 	(setq LVSW-102 nil)
					 	(setq LVSW-101 nil)
					 	(setq DSW-200 nil)
					 	(setq DSW-100 nil)
					 	(setq DW-200 nil)
					 	(setq DW-100 nil)
					 	(setq PW-311 nil)
					 	(setq PW-100 nil)
					 	(setq TS-400 nil)
					 	(setq ADF-120-277 nil)
					 	(setq LVS-1k nil)
					 	(setq BZ-250 nil)
					 	(setq BZ-250-347 nil)
					 	(setq BZ-150 nil)
					 	(setq BZ-50 nil)
						(setq V-EASYTOUCH-II-1 nil)
						(setq V-EASYTOUCH-II-2 nil)
					 	(setq V-EASYTOUCH-II-3 nil)
					 	(setq V-EASYTOUCH-II-4 nil)
					 	(setq V-EASYTOUCH-II-5 nil)
					 	(setq DSW-301 nil)
					 	(setq DSW-302 nil)
					        (setq DW-311 nil)	         
						(setq DT-305 nil)
						(setq PW-301 nil)
						(setq LVS-1 nil)
						(setq LVS-1k-lowercase nil)
						(setq LVS-1K nil)
						(setq DT-200 nil)
						(setq DT-205 nil)
						(setq LS-301 nil)
						(setq PW-302-347 nil)
					 	(setq ELCU-200-347 nil)	  
								      		   
					 	(setq roomProductCounter 0)											;reset counter
						(setq roomProductCounterMax (length roomHierarchy))								;set max
						(setq roomHierarchyTogether (cons roomHierarchy roomHierarchyTogether))						;make a master list including each room organized by the Hierarchy, seperated from each other room
					 	(setq productLadder rowCounter)											;set product ladder to the current row value
					
						)	
				)

			(setq roomText (nth roomCounter singleLineRoomList))											;set roomText to singleLineRoomListCurrent
				(cond 	((/= singleLineProductListCurrent nil)											;nested conditonal -- while singleLineRoomListCurrent is not nil
					(setq roomProducts (cons singleLineProductListCurrent roomProducts))							;add singleLineRoomListCurrent to roomProducts
					)
				)
	  
	  		(setq roomCounter (+ 1 roomCounter))													;move to next room
			
	)																			;end outer conditional
	(setq roomHierarchyTogether (reverse roomHierarchyTogether))												;reverse the master list		
  
      	(while singleLineRoomList																;create a list of unique room names from all the room names in the text file
        	(setq x (car singleLineRoomList)														;set x to first item from all the rooms
              		singleLineRoomList (vl-remove x (cdr singleLineRoomList))										;remove any other instances of x from the entire list
              		uniqueRoomList (cons x uniqueRoomList)													;add x to the new list
        		)
    		)																		;end while
	(setq uniqueRoomList (reverse uniqueRoomList))														;reverse the unique list to account for cons property


	(command "_.Layer" "_Make" "Riser" "_Color" "white" "" "LType" "Continuous" "" "")									;create Riser layer
	(setvar "clayer" "Riser")
  	(command "layer" "_freeze" "_WS_MSTP" "" "")														;freeze _WS_MSTP
	(setq xcoord 0)
  	(setq cntr 0)
	(setq roomMax (length uniqueRoomList))
  		(while	(<= cntr roomMax)															;while loop -- loop through each room
		  	(setq roomForBuilding(nth cntr uniqueRoomList))												;pull first room off list
			(cond																	;conditional block
			  	((/= roomForBuilding nil)													;conditional statement
					(setq xcoord 0)
				 	(setq ycoord 0.20)													;set y coord to 0
;;;				 	this is my old method of inserting text that causes errors
;;;				 	(setq insertionPoint (strcat (rtos xcoord) "," (rtos ycoord)))								;set insertion point for text
;;;		  			(command "-mtext" insertionPoint "J" "MC" "R" "0" "H" ".20" insertionPoint roomForBuilding "")				;insert text
					(setq insertionPoint (list xcoord ycoord))
				  
					;;;http://forums.augi.com/showthread.php?28641-Create-MTEXT-with-Lisp
				    	(entmake
				    	(list
				      	(cons 0 "MTEXT")         	;; Entity Name
				      	(cons 100 "AcDbEntity")  	;; Subclass Marker
				      	(cons 410 "Model")       	;; Space
				      	(cons 8 "Riser")         	;; Layer
				      	(cons 100 "AcDbMText")   	;; Subclass Marker
				      	(cons 10 insertionPoint) 	;; Insertion Point
				      	(cons 40 0.2)            	;; Text Height
				      	(cons 71 5)              	;; Attachment Point (middle-center)
				      	(cons 1 roomForBuilding)     	;; Text Content
				      	(cons 7 "Arial")		;text style
				      	)
				   	)
				 	(setq ycoord (+ ycoord -1.2))												;move y coord down
					(setq roomForBuildingHierarchy (nth cntr roomHierarchyTogether))							;pull the list off products off the total list of lists
		  			(setq cmax (length roomForBuildingHierarchy))										;set counter max to length of roomForBuildingHierarchy
		  			(setq cntr2 0)														;set counter 2 to 0
		  				(while 	(< cntr2 cmax)												;while loop -- loops through products
				  			(setq productForBuilding (nth cntr2 roomForBuildingHierarchy))
						  
;;;						        this is my old method of inserting blocks that causes errors
;;;							(setq insertionPoint (strcat (rtos xcoord) "," (rtos ycoord)))						;set insertion point for block
;;;						  	(setq insertionPoint2 (strcat (rtos xcoord) "," (rtos ycoord)))						;----THIS LINE FIXES THE X AND Y ERROR, making a second insertion point from the same variables does the job
;;;							(command "-insert" productForBuilding insertionPoint2 "1" "1" "0") 					;insert the object



							;;;	before insertion, find out in block is defined in template
							;;; 	if block is not defined, use placeholder and store the block name in the attribute
							(setq blockexists (tblsearch "block" productForBuilding))
							
							(cond																	;conditional block
							  	;;;when block exists
							  	((/= blockexists nil)														;conditional statement
								(setq productForBuilding productForBuilding)
								)																;end conditional statement

								;;;when block does not exist
							  	((= blockexists nil)														;conditional statement
								(setq ConstantRiserPlaceholder "RISERPLACEHOLDERBLOCK")
								(setq productForBuilding ConstantRiserPlaceholder)								 
								)																;end conditional statement

							)

						  
						  	(setq insertionPoint (list xcoord ycoord))
							(entmakex														;entmakex function
							(list															;list of all required items
							(cons 0 "INSERT")													;type of entity
					               	(cons 2 productForBuilding)												;name of block to insert
					               	(cons 10 insertionPoint)												;block insertion point
					              	(cons 41 1)														; Scale Factor
					               	(cons 42 1)														; Scale Factor?
					               	(cons 43 1)														; Scale Factor?
							)
							)	
						  	(setq xcoord 0)
							(setq ycoord (+ ycoord -1.33))	
							(setq cntr2 (+ cntr2 1))										;increment counter to loop through products
						)
					(setq blockName (strcat "Z-Riser-" roomForBuilding))									;name block
		  			(setq customBlockSet (ssget "X" '((8 . "Riser" ))))									;selection set
		  			(command "-block" blockName "0,0" "p" "")										;create the block from previous selection
				 )																;end conditional statement
			)																	;end conditional block
		(setq cntr (+ cntr 1))																;increment counter
		)																		;end while loop



  	(setq rowCounter 0)																	;set rowCounter
	(setq columnCounter 0)																	;set columnCounter
	(setq segment1RoomCurrent 0)
  	(setq segment1ListCurrent 0)
  	(setq segment1max (length segment1))
  	(setq segment1Counter 0)

  	(while 	(< segment1Counter segment1max)
	  	(setq segment1ListCurrent (nth segment1Counter segment1))											;set segment1ListCurrent to the nth member of segment1 that corresponds to the value of segment1Counter
		(setq segment1PositionCurrent (car segment1ListCurrent))
		(setq segment1PositionCurrent (atoi segment1PositionCurrent))
	  	(setq columnCounter (* segment1PositionCurrent 4.5))												;move column after inserting room
	  	(setq segment1RoomCurrent (caddr segment1ListCurrent))												;pull the roomname off segment1ListCurrent (3rd entry)
	  	
;;;	  	(setq insertionPoint (strcat (rtos columnCounter) "," (rtos rowCounter)))									;set new insertion point based on the the column and row counters
;;;		(setq insertionPoint2 (strcat (rtos columnCounter) "," (rtos rowCounter)))									;----THIS LINE FIXES THE X AND Y ERROR, making a second insertion point from the same variables does the job
	  	(setq segment1BlockName (strcat "Z-Riser-" segment1RoomCurrent))
;;;	  	(command "-insert" segment1BlockName insertionPoint2 "1" "1" "0") 										;insert the object
		(setq insertionPoint (list columnCounter rowCounter))
		(entmakex																	;entmakex function
		(list																		;list of all required items
		(cons 0 "INSERT")																;type of entity
		(cons 2 segment1BlockName)															;name of block to insert
		(cons 10 insertionPoint)															;block insertion point
		(cons 41 1)																	; Scale Factor
		(cons 42 1)																	; Scale Factor?
		(cons 43 1)																	; Scale Factor?
		)
		)
	
	  	(setq segment1Counter (+ 1 segment1Counter))
	)
	(setq rowCounter (+ 25 rowCounter))															;move to next after segment1
  	(setq columnCounter 0)																	;reset columnCounter
	
  	(setq segment2RoomCurrent 0)
    	(setq segment2ListCurrent 0)
    	(setq segment2max (length segment2))
    	(setq segment2Counter 0)
  	(while 	(< segment2Counter segment2max)
	  	(setq segment2ListCurrent (nth segment2Counter segment2))											;set segment1ListCurrent to the nth member of segment1 that corresponds to the value of segment1Counter
		(setq segment2PositionCurrent (car segment2ListCurrent))
		(setq segment2PositionCurrent (atoi segment2PositionCurrent))
	  	(setq columnCounter (* segment2PositionCurrent 4.5))												;move column after inserting room
	  	(setq segment2RoomCurrent (caddr segment2ListCurrent))												;pull the roomname off segment1ListCurrent (3rd entry)
;;;	  	(setq insertionPoint (strcat (rtos columnCounter) "," (rtos rowCounter)))									;set new insertion point based on the the column and row counters
;;;		(setq insertionPoint2 (strcat (rtos columnCounter) "," (rtos rowCounter)))
	  	(setq segment2BlockName (strcat "Z-Riser-" segment2RoomCurrent))
;;;	  	(command "-insert" segment2BlockName insertionPoint2 "1" "1" "0") 										;insert the object

	  	(setq insertionPoint (list columnCounter rowCounter))
		(entmakex																	;entmakex function
		(list																		;list of all required items
		(cons 0 "INSERT")																;type of entity
		(cons 2 segment2BlockName)															;name of block to insert
		(cons 10 insertionPoint)															;block insertion point
		(cons 41 1)																	; Scale Factor
		(cons 42 1)																	; Scale Factor?
		(cons 43 1)																	; Scale Factor?
		)
		)
	  	(setq segment2Counter (+ 1 segment2Counter))
	)

  	(setq rowCounter (+ 25 rowCounter))                      											
    	(setq columnCounter 0)                          													

  	(setq segment3RoomCurrent 0)
    	(setq segment3ListCurrent 0)
    	(setq segment3max (length segment3))
    	(setq segment3Counter 0)

    		(while  (< segment3Counter segment3max)
      			(setq segment3ListCurrent (nth segment3Counter segment3))             							
    			(setq segment3PositionCurrent (car segment3ListCurrent))
    			(setq segment3PositionCurrent (atoi segment3PositionCurrent))
      			(setq columnCounter (* segment3PositionCurrent 4.5))                  							
      			(setq segment3RoomCurrent (caddr segment3ListCurrent))                								
;;;      		(setq insertionPoint (strcat (rtos columnCounter) "," (rtos rowCounter)))         						
;;;			(setq insertionPoint2 (strcat (rtos columnCounter) "," (rtos rowCounter)))
		  	(setq segment3BlockName (strcat "Z-Riser-" segment3RoomCurrent))
;;;      		(command "-insert" segment3BlockName insertionPoint2 "1" "1" "0")
		  	(setq insertionPoint (list columnCounter rowCounter))
			(entmakex																	;entmakex function
			(list																		;list of all required items
			(cons 0 "INSERT")																;type of entity
			(cons 2 segment3BlockName)															;name of block to insert
			(cons 10 insertionPoint)															;block insertion point
			(cons 41 1)																	; Scale Factor
			(cons 42 1)																	; Scale Factor?
			(cons 43 1)																	; Scale Factor?
			)
			)
      			(setq segment3Counter (+ 1 segment3Counter))
  		)
  	(setq rowCounter (+ 25 rowCounter))                      												
    	(setq columnCounter 0)    

    	(setq segment4RoomCurrent 0)
    	(setq segment4ListCurrent 0)
    	(setq segment4max (length segment4))
    	(setq segment4Counter 0)

    		(while  (< segment4Counter segment4max)
      			(setq segment4ListCurrent (nth segment4Counter segment4))             							
    			(setq segment4PositionCurrent (car segment4ListCurrent))
    			(setq segment4PositionCurrent (atoi segment4PositionCurrent))
      			(setq columnCounter (* segment4PositionCurrent 4.5))                  							
      			(setq segment4RoomCurrent (caddr segment4ListCurrent))                								
;;;      		(setq insertionPoint (strcat (rtos columnCounter) "," (rtos rowCounter)))         						
;;;			(setq insertionPoint2 (strcat (rtos columnCounter) "," (rtos rowCounter)))
		  	(setq segment4BlockName (strcat "Z-Riser-" segment4RoomCurrent))
;;;      		(command "-insert" segment4BlockName insertionPoint "1" "1" "0")
		  	(setq insertionPoint (list columnCounter rowCounter))
			(entmakex																	;entmakex function
			(list																		;list of all required items
			(cons 0 "INSERT")																;type of entity
			(cons 2 segment4BlockName)															;name of block to insert
			(cons 10 insertionPoint)															;block insertion point
			(cons 41 1)																	; Scale Factor
			(cons 42 1)																	; Scale Factor?
			(cons 43 1)																	; Scale Factor?
			)
			)
      			(setq segment4Counter (+ 1 segment4Counter))
  		)
  	(setq rowCounter (+ 25 rowCounter))                      												
    	(setq columnCounter 0)

  	(setq segment5RoomCurrent 0)
    	(setq segment5ListCurrent 0)
    	(setq segment5max (length segment5))
    	(setq segment5Counter 0)

    		(while  (< segment5Counter segment5max)
      			(setq segment5ListCurrent (nth segment5Counter segment5))             							
    			(setq segment5PositionCurrent (car segment5ListCurrent))
    			(setq segment5PositionCurrent (atoi segment5PositionCurrent))
      			(setq columnCounter (* segment5PositionCurrent 4.5))                  							
      			(setq segment5RoomCurrent (caddr segment5ListCurrent))                								
;;;      		(setq insertionPoint (strcat (rtos columnCounter) "," (rtos rowCounter)))         						
;;;			(setq insertionPoint2 (strcat (rtos columnCounter) "," (rtos rowCounter)))
		  	(setq segment5BlockName (strcat "Z-Riser-" segment5RoomCurrent))
;;;      		(command "-insert" segment5BlockName insertionPoint2 "1" "1" "0")
		  	(setq insertionPoint (list columnCounter rowCounter))
			(entmakex																	;entmakex function
			(list																		;list of all required items
			(cons 0 "INSERT")																;type of entity
			(cons 2 segment5BlockName)															;name of block to insert
			(cons 10 insertionPoint)															;block insertion point
			(cons 41 1)																	; Scale Factor
			(cons 42 1)																	; Scale Factor?
			(cons 43 1)																	; Scale Factor?
			)
			)
      			(setq segment5Counter (+ 1 segment5Counter))
  		)
  	(setq rowCounter (+ 25 rowCounter))                      												
    	(setq columnCounter 0)

  	(setq segment6RoomCurrent 0)
    	(setq segment6ListCurrent 0)
    	(setq segment6max (length segment6))
    	(setq segment6Counter 0)

    		(while  (< segment6Counter segment6max)
      			(setq segment6ListCurrent (nth segment6Counter segment6))             							
    			(setq segment6PositionCurrent (car segment6ListCurrent))
    			(setq segment6PositionCurrent (atoi segment6PositionCurrent))
      			(setq columnCounter (* segment6PositionCurrent 4.5))                  							
      			(setq segment6RoomCurrent (caddr segment6ListCurrent))                								
;;;      		(setq insertionPoint (strcat (rtos columnCounter) "," (rtos rowCounter)))
;;;		  	(setq insertionPoint2 (strcat (rtos columnCounter) "," (rtos rowCounter)))
      			(setq segment6BlockName (strcat "Z-Riser-" segment6RoomCurrent))
;;;      		(command "-insert" segment6BlockName insertionPoint2 "1" "1" "0")
		  	(setq insertionPoint (list columnCounter rowCounter))
			(entmakex																	;entmakex function
			(list																		;list of all required items
			(cons 0 "INSERT")																;type of entity
			(cons 2 segment6BlockName)															;name of block to insert
			(cons 10 insertionPoint)															;block insertion point
			(cons 41 1)																	; Scale Factor
			(cons 42 1)																	; Scale Factor?
			(cons 43 1)																	; Scale Factor?
			)
			)
      			(setq segment6Counter (+ 1 segment6Counter))
  		)
  	(setq rowCounter (+ 25 rowCounter))                      												
    	(setq columnCounter 0)

    	(setq segment7RoomCurrent 0)
    	(setq segment7ListCurrent 0)
    	(setq segment7max (length segment7))
    	(setq segment7Counter 0)

    		(while  (< segment7Counter segment7max)
      			(setq segment7ListCurrent (nth segment7Counter segment7))             							
    			(setq segment7PositionCurrent (car segment7ListCurrent))
    			(setq segment7PositionCurrent (atoi segment7PositionCurrent))
      			(setq columnCounter (* segment7PositionCurrent 4.5))                  							
      			(setq segment7RoomCurrent (caddr segment7ListCurrent))                								
;;;      		(setq insertionPoint (strcat (rtos columnCounter) "," (rtos rowCounter)))
;;;		  	(setq insertionPoint2 (strcat (rtos columnCounter) "," (rtos rowCounter)))
      			(setq segment7BlockName (strcat "Z-Riser-" segment7RoomCurrent))
;;;      		(command "-insert" segment7BlockName insertionPoint2 "1" "1" "0")
		  	(setq insertionPoint (list columnCounter rowCounter))
			(entmakex																	;entmakex function
			(list																		;list of all required items
			(cons 0 "INSERT")																;type of entity
			(cons 2 segment7BlockName)															;name of block to insert
			(cons 10 insertionPoint)															;block insertion point
			(cons 41 1)																	; Scale Factor
			(cons 42 1)																	; Scale Factor?
			(cons 43 1)																	; Scale Factor?
			)
			)
      			(setq segment7Counter (+ 1 segment7Counter))
  		)
  	(setq rowCounter (+ 25 rowCounter))                      												
    	(setq columnCounter 0) 

    	(setq segment8RoomCurrent 0)
    	(setq segment8ListCurrent 0)
    	(setq segment8max (length segment8))
    	(setq segment8Counter 0)

    		(while  (< segment8Counter segment8max)
      			(setq segment8ListCurrent (nth segment8Counter segment8))             							
    			(setq segment8PositionCurrent (car segment8ListCurrent))
    			(setq segment8PositionCurrent (atoi segment8PositionCurrent))
      			(setq columnCounter (* segment8PositionCurrent 4.5))                  							
      			(setq segment8RoomCurrent (caddr segment8ListCurrent))                								
;;;      		(setq insertionPoint (strcat (rtos columnCounter) "," (rtos rowCounter)))
;;;		  	(setq insertionPoint2 (strcat (rtos columnCounter) "," (rtos rowCounter)))
      			(setq segment8BlockName (strcat "Z-Riser-" segment8RoomCurrent))
;;;      		(command "-insert" segment8BlockName insertionPoint2 "1" "1" "0")
		  	(setq insertionPoint (list columnCounter rowCounter))
			(entmakex																	;entmakex function
			(list																		;list of all required items
			(cons 0 "INSERT")																;type of entity
			(cons 2 segment8BlockName)															;name of block to insert
			(cons 10 insertionPoint)															;block insertion point
			(cons 41 1)																	; Scale Factor
			(cons 42 1)																	; Scale Factor?
			(cons 43 1)																	; Scale Factor?
			)
			)
      			(setq segment8Counter (+ 1 segment8Counter))
  		)
  	(setq rowCounter (+ 25 rowCounter))                      												
    	(setq columnCounter 0)

    	(setq segment9RoomCurrent 0)
    	(setq segment9ListCurrent 0)
    	(setq segment9max (length segment9))
    	(setq segment9Counter 0)

    		(while  (< segment9Counter segment9max)
      			(setq segment9ListCurrent (nth segment9Counter segment9))             							
    			(setq segment9PositionCurrent (car segment9ListCurrent))
    			(setq segment9PositionCurrent (atoi segment9PositionCurrent))
      			(setq columnCounter (* segment9PositionCurrent 4.5))                  							
      			(setq segment9RoomCurrent (caddr segment9ListCurrent))                								
;;;      		(setq insertionPoint (strcat (rtos columnCounter) "," (rtos rowCounter)))
;;;		  	(setq insertionPoint2 (strcat (rtos columnCounter) "," (rtos rowCounter)))
      			(setq segment9BlockName (strcat "Z-Riser-" segment9RoomCurrent))
;;;      		(command "-insert" segment9BlockName insertionPoint2 "1" "1" "0")
		  	(setq insertionPoint (list columnCounter rowCounter))
			(entmakex																	;entmakex function
			(list																		;list of all required items
			(cons 0 "INSERT")																;type of entity
			(cons 2 segment9BlockName)															;name of block to insert
			(cons 10 insertionPoint)															;block insertion point
			(cons 41 1)																	; Scale Factor
			(cons 42 1)																	; Scale Factor?
			(cons 43 1)																	; Scale Factor?
			)
			)
      			(setq segment9Counter (+ 1 segment9Counter))
  		)
  	(setq rowCounter (+ 25 rowCounter))                      												
    	(setq columnCounter 0)

    	(setq segment10RoomCurrent 0)
    	(setq segment10ListCurrent 0)
    	(setq segment10max (length segment10))
    	(setq segment10Counter 0)

    		(while  (< segment10Counter segment10max)
      			(setq segment10ListCurrent (nth segment10Counter segment10))             							
    			(setq segment10PositionCurrent (car segment10ListCurrent))
    			(setq segment10PositionCurrent (atoi segment10PositionCurrent))
      			(setq columnCounter (* segment10PositionCurrent 4.5))                  							
      			(setq segment10RoomCurrent (caddr segment10ListCurrent))                								
;;;      		(setq insertionPoint (strcat (rtos columnCounter) "," (rtos rowCounter)))
;;;		  	(setq insertionPoint2 (strcat (rtos columnCounter) "," (rtos rowCounter)))
      			(setq segment10BlockName (strcat "Z-Riser-" segment10RoomCurrent))
;;;      		(command "-insert" segment10BlockName insertionPoint2 "1" "1" "0")            							
		  	(setq insertionPoint (list columnCounter rowCounter))
			(entmakex																	;entmakex function
			(list																		;list of all required items
			(cons 0 "INSERT")																;type of entity
			(cons 2 segment10BlockName)															;name of block to insert
			(cons 10 insertionPoint)															;block insertion point
			(cons 41 1)																	; Scale Factor
			(cons 42 1)																	; Scale Factor?
			(cons 43 1)																	; Scale Factor?
			)
			)
		  	(setq segment10Counter (+ 1 segment10Counter))
  		)
  	(setq rowCounter (+ 35 rowCounter))                      												
    	(setq columnCounter 0)

  	(setq segment1ARoomCurrent 0)
    	(setq segment1AListCurrent 0)
    	(setq segment1Amax (length segment1A))
    	(setq segment1ACounter 0)

    		(while  (< segment1ACounter segment1Amax)
      			(setq segment1AListCurrent (nth segment1ACounter segment1A))             							
    			(setq segment1APositionCurrent (car segment1AListCurrent))
    			(setq segment1APositionCurrent (atoi segment1APositionCurrent))
      			(setq columnCounter (* segment1APositionCurrent 4.5))                  							
      			(setq segment1ARoomCurrent (caddr segment1AListCurrent))                								
;;;      		(setq insertionPoint (strcat (rtos columnCounter) "," (rtos rowCounter)))
;;;		  	(setq insertionPoint2 (strcat (rtos columnCounter) "," (rtos rowCounter)))
      			(setq segment1ABlockName (strcat "Z-Riser-" segment1ARoomCurrent))
;;;      		(command "-insert" segment1ABlockName insertionPoint2 "1" "1" "0")
		  	(setq insertionPoint (list columnCounter rowCounter))
			(entmakex																	;entmakex function
			(list																		;list of all required items
			(cons 0 "INSERT")																;type of entity
			(cons 2 segment1ABlockName)															;name of block to insert
			(cons 10 insertionPoint)															;block insertion point
			(cons 41 1)																	; Scale Factor
			(cons 42 1)																	; Scale Factor?
			(cons 43 1)																	; Scale Factor?
			)
			)
      			(setq segment1ACounter (+ 1 segment1ACounter))
  		)
  	(setq rowCounter (+ 25 rowCounter))                      												
    	(setq columnCounter 0)

    	(setq segment2ARoomCurrent 0)
    	(setq segment2AListCurrent 0)
    	(setq segment2Amax (length segment2A))
    	(setq segment2ACounter 0)

    		(while  (< segment2ACounter segment2Amax)
      			(setq segment2AListCurrent (nth segment2ACounter segment2A))             							
    			(setq segment2APositionCurrent (car segment2AListCurrent))
    			(setq segment2APositionCurrent (atoi segment2APositionCurrent))
      			(setq columnCounter (* segment2APositionCurrent 4.5))                  							
      			(setq segment2ARoomCurrent (caddr segment2AListCurrent))                								
;;;      		(setq insertionPoint (strcat (rtos columnCounter) "," (rtos rowCounter)))
;;;		  	(setq insertionPoint2 (strcat (rtos columnCounter) "," (rtos rowCounter)))
      			(setq segment2ABlockName (strcat "Z-Riser-" segment2ARoomCurrent))
;;;      		(command "-insert" segment2ABlockName insertionPoint2 "1" "1" "0")
		  	(setq insertionPoint (list columnCounter rowCounter))
			(entmakex																	;entmakex function
			(list																		;list of all required items
			(cons 0 "INSERT")																;type of entity
			(cons 2 segment2ABlockName)															;name of block to insert
			(cons 10 insertionPoint)															;block insertion point
			(cons 41 1)																	; Scale Factor
			(cons 42 1)																	; Scale Factor?
			(cons 43 1)																	; Scale Factor?
			)
			)
      			(setq segment2ACounter (+ 1 segment2ACounter))
  		)
  	(setq rowCounter (+ 25 rowCounter))                      												
    	(setq columnCounter 0)

    	(setq segment3ARoomCurrent 0)
    	(setq segment3AListCurrent 0)
    	(setq segment3Amax (length segment3A))
    	(setq segment3ACounter 0)

    		(while  (< segment3ACounter segment3Amax)
      			(setq segment3AListCurrent (nth segment3ACounter segment3A))             							
    			(setq segment3APositionCurrent (car segment3AListCurrent))
    			(setq segment3APositionCurrent (atoi segment3APositionCurrent))
      			(setq columnCounter (* segment3APositionCurrent 4.5))                  							
      			(setq segment3ARoomCurrent (caddr segment3AListCurrent))                								
;;;      		(setq insertionPoint (strcat (rtos columnCounter) "," (rtos rowCounter)))
;;;		  	(setq insertionPoint2 (strcat (rtos columnCounter) "," (rtos rowCounter)))
      			(setq segment3ABlockName (strcat "Z-Riser-" segment3ARoomCurrent))
;;;      		(command "-insert" segment3ABlockName insertionPoint2 "1" "1" "0")
		  	(setq insertionPoint (list columnCounter rowCounter))
			(entmakex																	;entmakex function
			(list																		;list of all required items
			(cons 0 "INSERT")																;type of entity
			(cons 2 segment3ABlockName)															;name of block to insert
			(cons 10 insertionPoint)															;block insertion point
			(cons 41 1)																	; Scale Factor
			(cons 42 1)																	; Scale Factor?
			(cons 43 1)																	; Scale Factor?
			)
			)
      			(setq segment3ACounter (+ 1 segment3ACounter))
  		)
  	(setq rowCounter (+ 25 rowCounter))                      												
    	(setq columnCounter 0)

    	(setq segment4ARoomCurrent 0)
    	(setq segment4AListCurrent 0)
    	(setq segment4Amax (length segment4A))
    	(setq segment4ACounter 0)

    		(while  (< segment4ACounter segment4Amax)
      			(setq segment4AListCurrent (nth segment4ACounter segment4A))             							
    			(setq segment4APositionCurrent (car segment4AListCurrent))
    			(setq segment4APositionCurrent (atoi segment4APositionCurrent))
      			(setq columnCounter (* segment4APositionCurrent 4.5))                  							
      			(setq segment4ARoomCurrent (caddr segment4AListCurrent))                								
;;;      		(setq insertionPoint (strcat (rtos columnCounter) "," (rtos rowCounter)))
;;;		  	(setq insertionPoint2 (strcat (rtos columnCounter) "," (rtos rowCounter)))
      			(setq segment4ABlockName (strcat "Z-Riser-" segment4ARoomCurrent))
;;;      		(command "-insert" segment4ABlockName insertionPoint2 "1" "1" "0")
		  	(setq insertionPoint (list columnCounter rowCounter))
			(entmakex																	;entmakex function
			(list																		;list of all required items
			(cons 0 "INSERT")																;type of entity
			(cons 2 segment4ABlockName)															;name of block to insert
			(cons 10 insertionPoint)															;block insertion point
			(cons 41 1)																	; Scale Factor
			(cons 42 1)																	; Scale Factor?
			(cons 43 1)																	; Scale Factor?
			)
			)
      			(setq segment4ACounter (+ 1 segment4ACounter))
  		)
  	(setq rowCounter (+ 25 rowCounter))                      												
    	(setq columnCounter 0)

    	(setq segment5ARoomCurrent 0)
    	(setq segment5AListCurrent 0)
    	(setq segment5Amax (length segment5A))
    	(setq segment5ACounter 0)

    		(while  (< segment5ACounter segment5Amax)
      			(setq segment5AListCurrent (nth segment5ACounter segment5A))             							
    			(setq segment5APositionCurrent (car segment5AListCurrent))
    			(setq segment5APositionCurrent (atoi segment5APositionCurrent))
      			(setq columnCounter (* segment5APositionCurrent 4.5))                  							
      			(setq segment5ARoomCurrent (caddr segment5AListCurrent))                								
;;;      		(setq insertionPoint (strcat (rtos columnCounter) "," (rtos rowCounter)))
;;;		  	(setq insertionPoint2 (strcat (rtos columnCounter) "," (rtos rowCounter)))
      			(setq segment5ABlockName (strcat "Z-Riser-" segment5ARoomCurrent))
;;;      		(command "-insert" segment5ABlockName insertionPoint2 "1" "1" "0")
		  	(setq insertionPoint (list columnCounter rowCounter))
			(entmakex																	;entmakex function
			(list																		;list of all required items
			(cons 0 "INSERT")																;type of entity
			(cons 2 segment5ABlockName)															;name of block to insert
			(cons 10 insertionPoint)															;block insertion point
			(cons 41 1)																	; Scale Factor
			(cons 42 1)																	; Scale Factor?
			(cons 43 1)																	; Scale Factor?
			)
			)
      			(setq segment5ACounter (+ 1 segment5ACounter))
  		)
  	(setq rowCounter (+ 25 rowCounter))                      												
    	(setq columnCounter 0)

    	(setq segment6ARoomCurrent 0)
    	(setq segment6AListCurrent 0)
    	(setq segment6Amax (length segment6A))
    	(setq segment6ACounter 0)

    		(while  (< segment6ACounter segment6Amax)
      			(setq segment6AListCurrent (nth segment6ACounter segment6A))             							
    			(setq segment6APositionCurrent (car segment6AListCurrent))
    			(setq segment6APositionCurrent (atoi segment6APositionCurrent))
      			(setq columnCounter (* segment6APositionCurrent 4.5))                  							
      			(setq segment6ARoomCurrent (caddr segment6AListCurrent))                								
;;;      		(setq insertionPoint (strcat (rtos columnCounter) "," (rtos rowCounter)))
;;;		  	(setq insertionPoint2 (strcat (rtos columnCounter) "," (rtos rowCounter)))
      			(setq segment6ABlockName (strcat "Z-Riser-" segment6ARoomCurrent))
;;;      		(command "-insert" segment6ABlockName insertionPoint2 "1" "1" "0")
		  	(setq insertionPoint (list columnCounter rowCounter))
			(entmakex																	;entmakex function
			(list																		;list of all required items
			(cons 0 "INSERT")																;type of entity
			(cons 2 segment6ABlockName)															;name of block to insert
			(cons 10 insertionPoint)															;block insertion point
			(cons 41 1)																	; Scale Factor
			(cons 42 1)																	; Scale Factor?
			(cons 43 1)																	; Scale Factor?
			)
			)
      			(setq segment6ACounter (+ 1 segment6ACounter))
  		)
  	(setq rowCounter (+ 25 rowCounter))                      												
    	(setq columnCounter 0)

    	(setq segment7ARoomCurrent 0)
    	(setq segment7AListCurrent 0)
    	(setq segment7Amax (length segment7A))
    	(setq segment7ACounter 0)

    		(while  (< segment7ACounter segment7Amax)
      			(setq segment7AListCurrent (nth segment7ACounter segment7A))             							
    			(setq segment7APositionCurrent (car segment7AListCurrent))
    			(setq segment7APositionCurrent (atoi segment7APositionCurrent))
      			(setq columnCounter (* segment7APositionCurrent 4.5))                  							
      			(setq segment7ARoomCurrent (caddr segment7AListCurrent))                								
;;;      		(setq insertionPoint (strcat (rtos columnCounter) "," (rtos rowCounter)))
;;;		  	(setq insertionPoint2 (strcat (rtos columnCounter) "," (rtos rowCounter)))
      			(setq segment7ABlockName (strcat "Z-Riser-" segment7ARoomCurrent))
;;;      		(command "-insert" segment7ABlockName insertionPoint2 "1" "1" "0")            							
      			(setq segment7ACounter (+ 1 segment7ACounter))
  		)
  	(setq rowCounter (+ 25 rowCounter))                      												
    	(setq columnCounter 0)

    	(setq segment8ARoomCurrent 0)
    	(setq segment8AListCurrent 0)
    	(setq segment8Amax (length segment8A))
    	(setq segment8ACounter 0)

    		(while  (< segment8ACounter segment8Amax)
      			(setq segment8AListCurrent (nth segment8ACounter segment8A))             							
    			(setq segment8APositionCurrent (car segment8AListCurrent))
    			(setq segment8APositionCurrent (atoi segment8APositionCurrent))
      			(setq columnCounter (* segment8APositionCurrent 4.5))                  							
      			(setq segment8ARoomCurrent (caddr segment8AListCurrent))                								
;;;      		(setq insertionPoint (strcat (rtos columnCounter) "," (rtos rowCounter)))
;;;		  	(setq insertionPoint2 (strcat (rtos columnCounter) "," (rtos rowCounter)))
			(setq segment8ABlockName (strcat "Z-Riser-" segment8ARoomCurrent))
;;;      		(command "-insert" segment8ABlockName insertionPoint2 "1" "1" "0")
		  	(setq insertionPoint (list columnCounter rowCounter))
			(entmakex																	;entmakex function
			(list																		;list of all required items
			(cons 0 "INSERT")																;type of entity
			(cons 2 segment8ABlockName)															;name of block to insert
			(cons 10 insertionPoint)															;block insertion point
			(cons 41 1)																	; Scale Factor
			(cons 42 1)																	; Scale Factor?
			(cons 43 1)																	; Scale Factor?
			)
			)
      			(setq segment8ACounter (+ 1 segment8ACounter))
  		)
  	(setq rowCounter (+ 25 rowCounter))                      												
    	(setq columnCounter 0)

    	(setq segment9ARoomCurrent 0)
    	(setq segment9AListCurrent 0)
    	(setq segment9Amax (length segment9A))
    	(setq segment9ACounter 0)

    		(while  (< segment9ACounter segment9Amax)
      			(setq segment9AListCurrent (nth segment9ACounter segment9A))             							
    			(setq segment9APositionCurrent (car segment9AListCurrent))
    			(setq segment9APositionCurrent (atoi segment9APositionCurrent))
      			(setq columnCounter (* segment9APositionCurrent 4.5))                  							
      			(setq segment9ARoomCurrent (caddr segment9AListCurrent))                								
;;;      		(setq insertionPoint (strcat (rtos columnCounter) "," (rtos rowCounter)))
;;;		  	(setq insertionPoint2 (strcat (rtos columnCounter) "," (rtos rowCounter)))
			(setq segment9ABlockName (strcat "Z-Riser-" segment9ARoomCurrent))
;;;      		(command "-insert" segment9ABlockName insertionPoint2 "1" "1" "0")
		  	(setq insertionPoint (list columnCounter rowCounter))
			(entmakex																	;entmakex function
			(list																		;list of all required items
			(cons 0 "INSERT")																;type of entity
			(cons 2 segment9ABlockName)															;name of block to insert
			(cons 10 insertionPoint)															;block insertion point
			(cons 41 1)																	; Scale Factor
			(cons 42 1)																	; Scale Factor?
			(cons 43 1)																	; Scale Factor?
			)
			)
      			(setq segment9ACounter (+ 1 segment9ACounter))
  		)
  	(setq rowCounter (+ 25 rowCounter))                      												
    	(setq columnCounter 0)

   	(setq segment10ARoomCurrent 0)
    	(setq segment10AListCurrent 0)
    	(setq segment10Amax (length segment10A))
    	(setq segment10ACounter 0)

    		(while  (< segment10ACounter segment10Amax)
      			(setq segment10AListCurrent (nth segment10ACounter segment10A))             							
    			(setq segment10APositionCurrent (car segment10AListCurrent))
    			(setq segment10APositionCurrent (atoi segment10APositionCurrent))
      			(setq columnCounter (* segment10APositionCurrent 4.5))                  							
      			(setq segment10ARoomCurrent (caddr segment10AListCurrent))                								
;;;      		(setq insertionPoint (strcat (rtos columnCounter) "," (rtos rowCounter)))
;;;		  	(setq insertionPoint2 (strcat (rtos columnCounter) "," (rtos rowCounter)))
			(setq segment10ABlockName (strcat "Z-Riser-" segment10ARoomCurrent))
;;;      		(command "-insert" segment10ABlockName insertionPoint2 "1" "1" "0")
		  	(setq insertionPoint (list columnCounter rowCounter))
			(entmakex																	;entmakex function
			(list																		;list of all required items
			(cons 0 "INSERT")																;type of entity
			(cons 2 segment10ABlockName)															;name of block to insert
			(cons 10 insertionPoint)															;block insertion point
			(cons 41 1)																	; Scale Factor
			(cons 42 1)																	; Scale Factor?
			(cons 43 1)																	; Scale Factor?
			)
			)
      			(setq segment10ACounter (+ 1 segment10ACounter))
  		)
  	(setq rowCounter (+ 25 rowCounter))                      												
    	(setq columnCounter 0)
  
  	(setq ssRiserBlocks (ssget "x" (list (cons 0 "INSERT")(cons 8 "RISER"))))
  	(cond														;-conditional block
	  	((/= ssRiserBlocks nil)											;-conditional statement
		(setq cntr 0)												;initialize counter to 0 for while loop
  		(setq cmax (sslength ssRiserBlocks))									;set cmax to length of blocks selection set
			(while (< cntr cmax)										;--set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
				(setq en(ssname ssRiserBlocks cntr))							;set variable en to the name of the block that matches the current counter value
				(setq enDXF(entget en))									;set the varaible enlist to the list of entities from the en varaible
				(setq insertedRoom (cdr (assoc 2 enDXF )))
		  		(setq insertedRooms (cons insertedRoom insertedRooms))												
				(setq cntr(+ cntr 1))									;once while loop reaches this point, increment counter varaible by one			
  			)												;--end while loop
		)													;-end conditional statement
	)														;-end conditional block
  
	(setq roomsToInsert nil)
	(setq cntr 0)													;initialize counter to 0 for while loop
  	(setq cmax (length uniqueRoomList))										;set cmax to length of blocks selection set
		(while (< cntr cmax)											;set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
			(setq uniqueRoom(nth cntr uniqueRoomList))							;set variable en to the name of the block that matches the current counter value
			(setq uniqueRoom(strcat "Z-Riser-" uniqueRoom))
		  	(setq match (member uniqueRoom insertedRooms))
		  	(cond
				((= match nil)
				(setq roomsToInsert (cons uniqueRoom roomsToInsert))
				)
			)
		(setq cntr(+ cntr 1))											;once while loop reaches this point, increment counter varaible by one			
  		)													;end while loop
  

	(setq roomsToInsert (reverse roomsToInsert ))


  	(setq rowCounter -50)												;set rowCounter
	(setq columnCounter 4.5)											;set columnCounter
  	(setq insertMax (length roomsToInsert))
  	(setq cntr 0)
  	(setq limit 0)

  	(while 	(< cntr insertMax)
	  	(setq roomToInsert (nth cntr roomsToInsert))								;set segment1ListCurrent to the nth member of segment1 that corresponds to the value of segment1Counter
;;;	  	(setq insertionPoint (strcat (rtos columnCounter) "," (rtos rowCounter)))				;set new insertion point based on the the column and row counters
;;;		(setq insertionPoint2 (strcat (rtos columnCounter) "," (rtos rowCounter)))
;;;	  	(command "-insert" roomToInsert insertionPoint2 "1" "1" "0") 						;insert the object
		(setq insertionPoint (list columnCounter rowCounter))
		(entmakex																	;entmakex function
		(list																		;list of all required items
		(cons 0 "INSERT")																;type of entity
		(cons 2 roomToInsert)																;name of block to insert
		(cons 10 insertionPoint)															;block insertion point
		(cons 41 1)																	; Scale Factor
		(cons 42 1)																	; Scale Factor?
		(cons 43 1)																	; Scale Factor?
		)
		)
		(setq columnCounter (+ columnCounter 4.5))
	  	(setq cntr (+ 1 cntr))
	  	(setq limit (+ 1 limit))
	  	(cond
		  	((= limit 20)
			(setq columnCounter 4.5)
			(setq rowCounter (+  rowCounter -25))
			(setq limit 0)
			)
		)
	)






  	(command "layer" "_thaw" "_WS_MSTP" "" "")									;thaw _WS_MSTP
  	(command "cmdecho" "1" "")											;turn command echo back on

)															;end GSR2