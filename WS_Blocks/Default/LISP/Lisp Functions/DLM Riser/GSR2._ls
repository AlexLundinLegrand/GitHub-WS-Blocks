(defun GSR2-after-room-read-global-destroyer (/)

  (FOREACH X' (

		BLOCKHEIGHT GLOBALBLOCKWIDTH GLOBALCABINETINFORMATIONLIST GLOBALROOMFORGSR2-RungBuilder
		
	       	GLOBALCABINETNAME 
		GLOBALNETWORKSEGMENT  	GLOBALROUTERNUMBER 
		GLOBALSEGMENTMANGER 	GLOBALSEGMENTPOSITION 
		GLOBALSWITCHNUMBER
	       	globalsegmentposition globalnetworksegment
	       
   ) (SET X NIL))
  
	(princ)

)
(defun GSR2-after-room-read-error-destroyer (/)

  (FOREACH X' (
		errormultiplecabinetlist
		errormultiplesegmentmanagerlist
		errormultipledlmpositionlist
	       
   ) (SET X NIL))
  
	(princ)

)
;;; --- GSR2-error-handler-segment-manager-empty-segment ---
;;; Alex Lundin 05-22-2017
;;; when GSR encounters segment manager with empty segment attribute, call this function
;;; this function prompts user to enter the segment and stores it to a global variable so GSR can use it
(defun GSR2-error-handler-segment-manager-empty-segment (/)
	(if
	  		(= errorhandler-empty-segment nil)
			(progn
			(setq errorhandler-empty-segment "1")
			)
			(progn
			(setq errorhandler-empty-segment (atoi errorhandler-empty-segment))
			(setq errorhandler-empty-segment (+ errorhandler-empty-segment 1))
			(setq errorhandler-empty-segment (rtos errorhandler-empty-segment))
			)
	)
	(setq globalnetworksegment errorhandler-empty-segment)

)
;;; --- GSR Function ---
;;; Alex Lundin 05-12-2017
(defun c:GSR2 ( /
		GLOBALDLMCONTROLLERS GLOBALDLMDAYLIGHT GLOBALDLMINTERFACES GLOBALDLMNETWORKDEVICES
	       GLOBALDLMOCCCORNERMOUNT GLOBALDLMOCCSENSORS GLOBALDLMPLUGCONTROLLERS GLOBALDLMSPLITTERS GLOBALDLMSWITCHES GLOBALNUMBEROFRUNG1 GLOBALNUMBEROFRUNG2 GLOBALNUMBEROFRUNG3 GLOBALREMAINDER VERTICALRUNG
		globalSEGMENTDOTTEDPAIRS globalSEGMENTLISTNUMERICAL globalSTANDALONENUMERICAL
		SINGLELINEROOMLIST
		GlobalControllers GLOBALDLMCONTROLLERS GLOBALDLMDAYLIGHT GLOBALDLMINTERFACES GLOBALDLMNETWORKDEVICES GLOBALDLMOCCCORNERMOUNT GLOBALDLMOCCSENSORS GLOBALDLMPLUGCONTROLLERS GLOBALDLMSPLITTERS GLOBALDLMSWITCHES
		GlobalControllers GLOBALNUMBEROFRUNG1 GLOBALNUMBEROFRUNG2 GLOBALNUMBEROFRUNG3 PORTS USEABLEPORTS
		BBPRODUCT BLOCKNAME GLOBALBLOCKWIDTH CMAX CMAX1 CMAX2 CMAX5 CMAX6 CNTR CNTR1
	       CNTR2 CNTR5 CNTR6 COLUMNCHECK GLOBALDLMCONTROLLERS GLOBALDLMDAYLIGHT
	       GLOBALDLMINTERFACES GLOBALDLMNETWORKDEVICES GLOBALDLMOCCCORNERMOUNT GLOBALDLMOCCSENSORS
	       GLOBALDLMPLUGCONTROLLERS GLOBALDLMSPLITTERS GLOBALDLMSWITCHES INSERTIONPOINT NUMBEROFDLMREMAINDERRUNG
	       NUMBEROFREMAINDERRUNG GLOBALNUMBEROFRUNG1 GLOBALNUMBEROFRUNG2 GLOBALNUMBEROFRUNG3
	       RBdlmremainder RBPRODUCT RBPRODUCT2 RBPRODUCTS RBPRODUCTSPASS1 RBPRODUCTSPASS2 RBPRODUCTSPASS3 RBPRODUCTSPASS4 RBPRODUCTSPASS5 GLOBALREMAINDER
	       GLOBALroomforGSR2-RungBuilder RUNGPRODUCT SPACER VERTICALRUNG XCOORD YCOORD products globalSTANDALONEDOTTEDPAIRS
		globalSEGMENTSTOINSERT
		ITEM SS
		SEGMENTSUBLIST STANDALONECHECKER STANDALONESUBLIST globalsegmentposition
	       CUSTOMBLOCKSET DRAWINGLOCATION FILE1 
		PRODUCT ROOM ROOMLOOPCHECK SEGMENTCHECKER CABINETSUBLIST DLMPOSITIONLIST ERRORMULTIPLECABINETLIST ERRORMULTIPLEDLMPOSITIONLIST ERRORMULTIPLESEGMENTMANAGERLIST ERRORROOMNAME
	       )
	(vl-load-com)																		;open f1 for reading -- similar to oppen in eblock function
	(GSR2-destroyerfinal)
	(setq ConstantRiserPlaceholder "RISERPLACEHOLDERBLOCK")
  	(setq ConstantMstpCableIDBlock "_MSTP_CABLE_ID")

  	(command "_.Layer" "_Make" "Riser" "_Color" "7" "" "LType" "Continuous" "" "")
  	(command "_.Layer" "_Make" "Riser-MSTP" "_Color" "160" "" "LType" "Continuous" "" "")
  	(command "_.Layer" "_Make" "Riser-CAT5E" "_Color" "3" "" "LType" "Continuous" "" "")
  	(command "_.Layer" "_Make" "Riser-Segments" "_Color" "7" "" "LType" "Continuous" "" "")
  	(command "_.Layer" "_Make" "Riser-Rooms" "_Color" "160" "" "LType" "Continuous" "" "")
	;;;delete previous blocks on riser layer from any previous risers and purge
	(setq customBlockSet (ssget "X" '((8 . "Riser" ))))													;selection set
	(cond
	  	((/= customBlockSet nil) 
  		(command "erase" "p" "")
		)
  	)
	(setq customBlockSet (ssget "X" '((8 . "Riser-MSTP" ))))												;selection set
	(cond
	  	((/= customBlockSet nil) 
  		(command "erase" "p" "")
		)
  	)
	(setq customBlockSet (ssget "X" '((8 . "Riser-CAT5E" ))))												;selection set
	(cond
	  	((/= customBlockSet nil) 
  		(command "erase" "p" "")
		)
  	)  
  	(command "-PURGE" "A" "*Z-Riser*" "N")

	;;;open text file for reading
  	(setq file1 (open (strcat (getvar 'DWGPREFIX) "Riser_Extraction_2.txt")  "r"))

	;;;let user know if text file cannot be found
  	(cond																			;-cond block
	  	((= file1 nil)																	;-- cond statement if error handler for no text file
		(princ "Riser_Extraction_2.txt not found in folder with SR-1")
		(quit)
		)																		;-- end cond statement
	)																			;- end cond block

	;;;read the first line of the text file in, which is the location of the drawing that was extracted from
	;;;this is the priming read for the following while loop
  	(setq drawingLocation (read-line file1))														;read in first line of Riser_Extraction.txt which will be drawing location
  	(setq globalroom (read-line file1))															;priming read for globalroom so it's not empty
  	(setq roomLoopCheck globalroom)																;roomLoopCheck will always hold the previous rooms value inside the while loop
	(setq singlelineroomlist (cons globalroom singlelineroomlist))												;create a list of all rooms from the extraction
  
  	(while	(/= roomLoopCheck nil)																;- while loop for reading text document, end when roomLoopCheck is nil

		;;;first if statment
		;;;this will call the subfunctions when true
		;;;logic follows:
		;;;if globalroom does not equal roomLoopCheck, call subfunctions to operate on the list of products from the previous room
		;;;globalroom is the current value read from the text file and is compared to roomLoopCheck, the prevous room
		;;;after the subfunctions run, nil list called products and store globalroom to the singlelineroomlist
		(if
		  	(/= globalroom roomLoopCheck)														;-- if statement to identify new room
			(progn																	;---progn wrap

			  
			  		;;;	this conditional creates the segmentmanager block		
			  		;;;									
			  		;;;	error handling conditional statement				
			  		;;;	1. one or multiple cabinets					
			  		;;;	2. multiple segment managers					
			  		;;;	3. multiple network devices in a room				
			  		;;;	ONLY ONE OF THESE MAIN IF CONDITIONALS CAN TAKE PLACE		
			  		;;;	OTHERWISE THERE WILL BE DUPLICATION OF PRODUCTS AND ROOMS	

			  		;;;	there are seperate calls to GSR2-RungBuilder
			  		;;;	(GSR2-RungBuilder-destroyer) must take place after all possible calls otherwise globals will be destroyed to early
			  		(if																;---if
					  	(or
						(>= (length errormultiplecabinetlist) 1)(>= (length errormultiplesegmentmanagerlist) 1)(>= (length errormultipledlmpositionlist) 2)
						)
						(progn															;----progn
						(setq errormultiplecabinetlist (reverse errormultiplecabinetlist))
				  		(setq RHreturnlist (GSR2-RiserHeirarchy products))										;call subfunctions
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
						
						(setq PCreturnlist(GSR2-PortCalculator  products NumberofCONTROLLERS))
						(setq NUMBEROFRUNG1 (nth 0 PCreturnlist))
						(setq NUMBEROFRUNG2 (nth 1 PCreturnlist))
						(setq NUMBEROFRUNG3 (nth 2 PCreturnlist))
						(setq NUMBEROFRUNGFINAL(nth 3 PCreturnlist))
						
						(setq cntrforstring 0)
						(setq blanklist (list nil))
						(setq errorroomname GLOBALroomforGSR2-RungBuilder)
						;repeat loop for network cabinets
						(setq cntr 0)
						(setq cmax (length errormultiplecabinetlist))
						(setq errormultiplecabinetlist (reverse errormultiplecabinetlist))
						(while															;-----while
						  	(< cntr cmax)
						  	;;;make errorroomname after first device
						  	(if														;------if
							  	(/= cntrforstring 0)
							  	(setq errorroomname (strcat GLOBALroomforGSR2-RungBuilder "_" (atoi cntrforstring)))
							)														;-----end if
							(setq individualcabinet (nth cntr errormultiplecabinetlist ))
							(setq globalCABINETNAME (nth 0 individualcabinet))
							(setq globalrouterNumber (nth 1 individualcabinet))
							(setq globalswitchNumber (nth 2 individualcabinet))
							(setq globalsegmentManger (nth 3 individualcabinet))
							(setq globalnetworksegment (nth 4 individualcabinet))
							(setq globalcabinetinformationlist (list globalCABINETNAME globalrouterNumber globalswitchNumber globalsegmentManger))
								
							(setq numberofproductsinroom (length products))
						  	(setq productaslist (list globalCABINETNAME))
						  
				  			(setq RBreturnlist (GSR2-RungBuilder errorroomname globalnetworksegment numberofproductsinroom globalcabinetinformationlist productaslist blanklist blanklist blanklist blanklist blanklist blanklist blanklist blanklist blanklist blanklist blanklist blanklist blanklist 0 0 0 0))
							(setq blockwidth (nth 0 RBreturnlist))
							(setq blockheight (nth 1 RBreturnlist))
						  
						  	(setq cntrforstring (+ cntrforstring 1))
						  	(setq cntr (+ cntr 1))
						)															;-----end while
						  
						;repeat loop for multiple segmans
						(setq cntr 0)
						(setq cmax (length errormultiplesegmentmanagerlist))
						(setq errormultiplesegmentmanagerlist (reverse errormultiplesegmentmanagerlist))
						(while															;-----while
						  	(< cntr cmax)
						  	;;;make errorroomname after first device
						  	(if
							  	(/= cntrforstring 0)
							  	(progn
							  	(setq errorroomname (strcat GLOBALroomforGSR2-RungBuilder "_" (itoa cntrforstring)))
								)
							)						  
							(setq individualsegmentmanager (nth cntr errormultiplesegmentmanagerlist))
							(setq product (nth 0 individualsegmentmanager))
							(setq globalnetworksegment (nth 1 individualsegmentmanager))
						  
							(setq numberofproductsinroom (length products))
						  	(setq productaslist (list product))
				  			(setq RBreturnlist (GSR2-RungBuilder errorroomname globalnetworksegment numberofproductsinroom blanklist productaslist blanklist blanklist blanklist blanklist blanklist blanklist blanklist blanklist blanklist blanklist blanklist blanklist blanklist 0 0 0 0))
							(setq blockwidth (nth 0 RBreturnlist))
							(setq blockheight (nth 1 RBreturnlist))
						  
							(setq cntrforstring (+ cntrforstring 1))
						  	(setq cntr (+ cntr 1))
						)															;-----end while

						
						;repeat loop for multiple position based dlm devices
						;if statement handles situation where there are position based devices
						(if															;-----if
						  	(/= errormultipledlmpositionlist nil)
						  	(progn														;------progn
							(setq errormultipledlmpositionlist(reverse errormultipledlmpositionlist))
							(setq cntr 0)
							(setq cmax (length errormultipledlmpositionlist))
							(while														;-------while
							  	(< cntr cmax)
							  	;;;make errorroomname after first device
							  	(if													;--------if
								  	(/= cntrforstring 0)	
								  	(progn
								  	(setq errorroomname (strcat GLOBALroomforGSR2-RungBuilder "_" (itoa cntrforstring)))
									)
								)													;---------end if
								(setq individualdlmitem (nth cntr errormultipledlmpositionlist))
								(setq product (nth 0 individualdlmitem))
								(setq globalnetworksegment (nth 1 individualdlmitem))
							  	(setq globalsegmentposition (nth 2 individualdlmitem))
							  
								(setq numberofproductsinroom (length products))
								(setq productaslist (list product))
							  
							  	;;;when cntr is 0, then call on the current network product, plus everything
							  	(if
								  	(= cntr 0)
								  	(progn

								  
								  	(if																			;-if
										(OR (= product "LMBC-300"))															;--nested OR in the if
									  	(progn																		;--progn, wraps the lines contained into 1 operation, this is necessary for if's in LISP
										(setq RBreturnlist (GSR2-RungBuilder errorroomname globalnetworksegment numberofproductsinroom blanklist blanklist productaslist blanklist blanklist DLMCONTROLLERS DLMPLUGCONTROLLERS Controllers DLMINTERFACES DLMOCCSENSORS DLMOCCCORNERMOUNT DLMDAYLIGHT DLMSWITCHES DLMSPLITTERS REMAINDER NUMBEROFRUNG1 NUMBEROFRUNG2 NUMBEROFRUNG3 NUMBEROFRUNGFINAL))
										)																		;--end progn
									)																			;-end if

								  	(if																			;-if
										(OR (= product "LMZC-301"))															;--nested OR in the if
									  	(progn																		;--progn, wraps the lines contained into 1 operation, this is necessary for if's in LISP
										(setq RBreturnlist (GSR2-RungBuilder errorroomname globalnetworksegment numberofproductsinroom blanklist blanklist blanklist productaslist blanklist DLMCONTROLLERS DLMPLUGCONTROLLERS Controllers DLMINTERFACES DLMOCCSENSORS DLMOCCCORNERMOUNT DLMDAYLIGHT DLMSWITCHES DLMSPLITTERS REMAINDER NUMBEROFRUNG1 NUMBEROFRUNG2 NUMBEROFRUNG3 NUMBEROFRUNGFINAL))
										)																		;--end progn
									)																			;-end if
								  
								  	(if																			;-if
										(OR (= product "LMCP48")(= product "LMCP24")(= product "LMCP12")(= product "LMCP8"))								;--nested OR in the if
									  	(progn																		;--progn, wraps the lines contained into 1 operation, this is necessary for if's in LISP
										(setq RBreturnlist (GSR2-RungBuilder errorroomname globalnetworksegment numberofproductsinroom blanklist blanklist blanklist blanklist productaslist DLMCONTROLLERS DLMPLUGCONTROLLERS Controllers DLMINTERFACES DLMOCCSENSORS DLMOCCCORNERMOUNT DLMDAYLIGHT DLMSWITCHES DLMSPLITTERS REMAINDER NUMBEROFRUNG1 NUMBEROFRUNG2 NUMBEROFRUNG3 NUMBEROFRUNGFINAL))
										)																		;--end progn
									)																			;-end if
						  			
								      	)
								)

							  	;;;when cntr is not 0, then only call on the product, nil all other lists
							  	(if
								  	(/= cntr 0)
								  	(progn

								  
								  	(if																			;-if
										(OR (= product "LMBC-300"))															;--nested OR in the if
									  	(progn																		;--progn, wraps the lines contained into 1 operation, this is necessary for if's in LISP
										(setq RBreturnlist (GSR2-RungBuilder errorroomname globalnetworksegment numberofproductsinroom blanklist blanklist productaslist blanklist blanklist blanklist blanklist blanklist blanklist blanklist blanklist blanklist blanklist blanklist blanklist 0 0 0 0))
										)																		;--end progn
									)																			;-end if
								  
								  	(if																			;-if
										(OR (= product "LMCP48")(= product "LMCP24")(= product "LMCP12")(= product "LMCP8"))								;--nested OR in the if
									  	(progn																		;--progn, wraps the lines contained into 1 operation, this is necessary for if's in LISP
										(setq RBreturnlist (GSR2-RungBuilder errorroomname globalnetworksegment numberofproductsinroom blanklist blanklist blanklist blanklist productaslist blanklist blanklist blanklist blanklist blanklist blanklist blanklist blanklist blanklist blanklist 0 0 0 0))
										)																		;--end progn
									)																			;-end if
									
								      	)
								)
								  
								(setq blockwidth (nth 0 RBreturnlist))
								(setq blockheight (nth 1 RBreturnlist))
					
								(setq BLOCKHEIGHT(GSR2-RoundUp BLOCKHEIGHT))
								(setq segmentsublist (cons BLOCKHEIGHT segmentsublist))
								(setq segmentsublist (cons blockwidth segmentsublist))
								(setq segmentsublist (cons errorroomname segmentsublist))						;create bridgeSegmentSublist with roomCurrent at end, followed by dot and bridgePosition --this line can un do the dot if we structure one element at a time
								(setq segmentsublist (cons  globalsegmentposition segmentsublist))
								(setq segmentsublist (cons  globalnetworksegment segmentsublist))					;add bridgeSegment to beginning of bridgeSegmentSublist
								(setq globalSEGMENTDOTTEDPAIRS (cons segmentsublist globalSEGMENTDOTTEDPAIRS))				;create bridgeSegmentDottedPairs to accumulate all bridgeSegmentSublists
								(setq segmentsublist nil)

								       
								(setq cntrforstring (+ cntrforstring 1))
							  	(setq cntr (+ cntr 1))
							)														;-------end while
							  

							)														;------end progn
						)															;-----end if
						;if statement handles situation where there are no position based devices
						(if															;-----if
						  	(= errormultipledlmpositionlist nil)
						  	(progn														;------progn
							  	;;;make errorroomname after first device
							  	(if													;--------if
								  	(/= cntrforstring 0)	
								  	(progn
								  	(setq errorroomname (strcat GLOBALroomforGSR2-RungBuilder "_" (itoa cntrforstring)))
									)
								)													;---------end if

							  
							(setq numberofproductsinroom (length products))
							  
				  			(setq RBreturnlist (GSR2-RungBuilder errorroomname globalnetworksegment numberofproductsinroom blanklist blanklist blanklist DLMZONECONTROLLERS blanklist DLMCONTROLLERS DLMPLUGCONTROLLERS Controllers DLMINTERFACES DLMOCCSENSORS DLMOCCCORNERMOUNT DLMDAYLIGHT DLMSWITCHES DLMSPLITTERS REMAINDER NUMBEROFRUNG1 NUMBEROFRUNG2 NUMBEROFRUNG3 NUMBEROFRUNGFINAL))
							(setq blockwidth (nth 0 RBreturnlist))
							(setq blockheight (nth 1 RBreturnlist))
				
							
							(setq BLOCKHEIGHT(GSR2-RoundUp BLOCKHEIGHT))
							(setq standalonesublist (cons BLOCKHEIGHT standalonesublist))
							(setq standalonesublist (cons BLOCKWIDTH standalonesublist))
							(setq standalonesublist (cons errorroomname standalonesublist))
							(setq globalSTANDALONEDOTTEDPAIRS (cons standalonesublist globalSTANDALONEDOTTEDPAIRS))
							(setq standalonesublist nil)

							       
							(setq cntrforstring (+ cntrforstring 1))
							  

							)														;------end progn
						)															;-----end if
						
						;this ensures the other main conidtionals don't catch
						(setq globalnetworksegment nil)
						(setq globalsegmentposition "ZZ")
						(GSR2-RungBuilder-destroyer)





						
						)															;----end progn
					)



			  		;;;	conditional if statement for rooms on any of the numbered segments
			  		;;;	ONLY ONE OF THESE MAIN IF CONDITIONALS CAN TAKE PLACE
			  		;;;	OTHERWISE THERE WILL BE DUPLICATION OF PRODUCTS AND ROOMS

			  		;;;	after builder, the list creation is determined by the globalnetworksegment and globalsegmentposition
			  
			  		(if
					  	(AND
						(/= globalnetworksegment nil)(/= globalsegmentposition nil)
						)
						(progn

				  		(setq RHreturnlist (GSR2-RiserHeirarchy products))										;call subfunctions
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
						
						(setq PCreturnlist(GSR2-PortCalculator  products NumberofCONTROLLERS))
						(setq NUMBEROFRUNG1 (nth 0 PCreturnlist))
						(setq NUMBEROFRUNG2 (nth 1 PCreturnlist))
						(setq NUMBEROFRUNG3 (nth 2 PCreturnlist))
						(setq NUMBEROFRUNGFINAL(nth 3 PCreturnlist))
						
						(setq numberofproductsinroom (length products))

			  			(setq RBreturnlist (GSR2-RungBuilder GLOBALroomforGSR2-RungBuilder globalnetworksegment numberofproductsinroom blanklist DLMSEGMENTMANAGERS DLMNETWORKBRIDGES DLMZONECONTROLLERS DLMPANELS DLMCONTROLLERS DLMPLUGCONTROLLERS Controllers DLMINTERFACES DLMOCCSENSORS DLMOCCCORNERMOUNT DLMDAYLIGHT DLMSWITCHES DLMSPLITTERS REMAINDER NUMBEROFRUNG1 NUMBEROFRUNG2 NUMBEROFRUNG3 NUMBEROFRUNGFINAL))
						(setq blockwidth (nth 0 RBreturnlist))
						(setq blockheight (nth 1 RBreturnlist))
						(GSR2-RungBuilder-destroyer)
						(setq BLOCKHEIGHT(GSR2-RoundUp BLOCKHEIGHT))
						  

						;add to network list if the info exists
						(if
							(and
						  	(/= globalsegmentposition "")(/= globalsegmentposition "")
							)
						  	(progn
							
							(setq segmentsublist (cons BLOCKHEIGHT segmentsublist))
							(setq segmentsublist (cons BLOCKWIDTH segmentsublist))
							(setq segmentsublist (cons GLOBALroomforGSR2-RungBuilder segmentsublist))						;create bridgeSegmentSublist with roomCurrent at end, followed by dot and bridgePosition --this line can un do the dot if we structure one element at a time
							(setq segmentsublist (cons  globalsegmentposition segmentsublist))
							(setq segmentsublist (cons  globalnetworksegment segmentsublist))						;add bridgeSegment to beginning of bridgeSegmentSublist
							(setq globalSEGMENTDOTTEDPAIRS (cons segmentsublist globalSEGMENTDOTTEDPAIRS))				;create bridgeSegmentDottedPairs to accumulate all bridgeSegmentSublists
							(setq segmentsublist nil)
							)
						)
						
						;add to standalone list if the info does not exist
						(if
							(and
						  	(= globalsegmentposition "")(= globalsegmentposition "")
							)
						  	(progn

							(setq standalonesublist (cons BLOCKHEIGHT standalonesublist))
							(setq standalonesublist (cons BLOCKWIDTH standalonesublist))
							(setq standalonesublist (cons GLOBALroomforGSR2-RungBuilder standalonesublist))
							(setq globalSTANDALONEDOTTEDPAIRS (cons standalonesublist globalSTANDALONEDOTTEDPAIRS))		;create bridgeSegmentDottedPairs to accumulate all bridgeSegmentSublists
							(setq standalonesublist nil)
							)
						)
						
						

						)
					)
			  
					;;;	conditional if statement for standalone rooms
			  		;;;	ONLY ONE OF THESE MAIN IF CONDITIONALS CAN TAKE PLACE
			  		;;;	OTHERWISE THERE WILL BE DUPLICATION OF PRODUCTS AND ROOMS
					(if
						(AND
					  	(= globalnetworksegment nil)(= globalsegmentposition nil)
						)
						(progn
				  		(setq RHreturnlist (GSR2-RiserHeirarchy products))										;call subfunctions
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
						
						(setq PCreturnlist(GSR2-PortCalculator  products NumberofCONTROLLERS))
						(setq NUMBEROFRUNG1 (nth 0 PCreturnlist))
						(setq NUMBEROFRUNG2 (nth 1 PCreturnlist))
						(setq NUMBEROFRUNG3 (nth 2 PCreturnlist))
						(setq NUMBEROFRUNGFINAL(nth 3 PCreturnlist))
						
						(setq numberofproductsinroom (length products))
			
			  			(setq RBreturnlist (GSR2-RungBuilder GLOBALroomforGSR2-RungBuilder globalnetworksegment numberofproductsinroom blanklist DLMSEGMENTMANAGERS DLMNETWORKBRIDGES DLMZONECONTROLLERS DLMPANELS DLMCONTROLLERS DLMPLUGCONTROLLERS Controllers DLMINTERFACES DLMOCCSENSORS DLMOCCCORNERMOUNT DLMDAYLIGHT DLMSWITCHES DLMSPLITTERS REMAINDER NUMBEROFRUNG1 NUMBEROFRUNG2 NUMBEROFRUNG3 NUMBEROFRUNGFINAL))
						(setq blockwidth (nth 0 RBreturnlist))
						(setq blockheight (nth 1 RBreturnlist))
						(GSR2-RungBuilder-destroyer)
						
						(setq BLOCKHEIGHT(GSR2-RoundUp BLOCKHEIGHT))
						(setq standalonesublist (cons BLOCKHEIGHT standalonesublist))
						(setq standalonesublist (cons BLOCKWIDTH standalonesublist))
						(setq standalonesublist (cons GLOBALroomforGSR2-RungBuilder standalonesublist))
						(setq globalSTANDALONEDOTTEDPAIRS (cons standalonesublist globalSTANDALONEDOTTEDPAIRS))
						(setq standalonesublist nil)
						)
					)


			  
					(GSR2-after-room-read-global-destroyer)
			  		(GSR2-after-room-read-error-destroyer)
				  	(setq products nil)													;nil the list after
					(if	(/= globalroom nil)
			  			(setq singlelineroomlist (cons globalroom singlelineroomlist))
					)
			)																	;---end progn
		)																		;-- end if

		;;;always read the next line after the room into the product variable	 
	  	(setq product (read-line file1))														;read the next line into product



		;;;this structure is a if statement followed by some or conditions
		;;;this structure will be used throughout the file, in some cases the or conditionals will be in line with eachother rather than one their own lines
		;;;the logic follows:
		;;;true when product = LMNC
	  	(if																		;-- if statment

			(= product "LMNC")
			;;;this progn block wraps all the following statements so the if can execute them
			;;;it's known as artifical wrapping
			;;;this progn block reads the information from the text file when we have a network device
			;;;it also creates a list of all the networked rooms called segmentdotted pairs
			;;;[(networksegment.globalsegmentposition.room.blockwidth.blockheight)(....)(...)]
		  	(progn																	;---progn to wrap if statement
			(setq globalCABINETNAME (read-line file1))												;read the next line in
			(setq globalrouterNumber (read-line file1))												;read the next line in
			(setq globalswitchNumber (read-line file1))												;read the next line in
			(setq globalsegmentManger (read-line file1))												;read the next line in
			(setq globalnetworksegment (read-line file1))												;read the next line into room
			;;;error handling for when designer leaves segment attribute emtpy
			(if	( = globalnetworksegment "")
			  	(progn
				(GSR2-error-handler-segment-manager-empty-segment)
				)
			)
			(setq globalcabinetinformationlist nil)
			(setq globalcabinetinformationlist (list globalCABINETNAME globalrouterNumber globalswitchNumber globalsegmentManger globalnetworksegment))
			(setq errormultiplecabinetlist (cons globalcabinetinformationlist errormultiplecabinetlist))						;list to catch error when designer puts multiple in one room
			(setq globalcabinetinformationlist nil)
			)																	;--- end progn
		)
	  
		;;;this structure is a if statement followed by some or conditions
		;;;this structure will be used throughout the file, in some cases the or conditionals will be in line with eachother rather than one their own lines
		;;;the logic follows:
		;;;true when product = LMSM-3E or LMSM-6E
	  	(if																		;-- if statment
		  	(or
			(= product "LMSM-3E")
			(= product "LMSM-6E")
			(= product "NB-ROUTER")
			)																	;--- end or
			;;;this progn block wraps all the following statements so the if can execute them
			;;;it's known as artifical wrapping
			;;;this progn block reads the information from the text file when we have a network device
			;;;it also creates a list of all the networked rooms called segmentdotted pairs
			;;;[(networksegment.globalsegmentposition.room.blockwidth.blockheight)(....)(...)]
		  	(progn																	;---progn to wrap if statement
			(setq globalnetworksegment (read-line file1))												;read the next line into room
			;;;error handling for when designer leaves segment attribute emtpy
			(if	( = globalnetworksegment "")
			  	(progn
				(GSR2-error-handler-segment-manager-empty-segment)
				)
			)
			(setq segmanlist (list product globalnetworksegment)) 
			(setq errormultiplesegmentmanagerlist (cons segmanlist errormultiplesegmentmanagerlist))
			(setq segmanlist nil)
			)																	;--- end progn
		)
	  
		;;;this structure is a if statement followed by some or conditions
		;;;this structure will be used throughout the file, in some cases the or conditionals will be in line with eachother rather than one their own lines
		;;;the logic follows:
		;;;true when product = any network device
	  	(if																		;-- if statment
			(or																	;--- or statment to catch network products
			(= product "LMBC-300")
			(= product "LMCP48")
			(= product "LMCP24")
			(= product "LMCP12")
			(= product "LMCP8")
			)																	;--- end or
			;;;this progn block wraps all the following statements so the if can execute them
			;;;it's known as artifical wrapping
			;;;this progn block reads the information from the text file when we have a network device
			;;;it also creates a list of all the networked rooms called segmentdotted pairs
			;;;[(networksegment.globalsegmentposition.room.blockwidth.blockheight)(....)(...)]
		  	(progn																	;---progn to wrap if statement
			(setq globalnetworksegment (read-line file1))												;read the next line into room
	  		(setq globalsegmentposition (read-line file1))												;read the next line into product
			;;;error handling for when designer puts multiple devies using the segmentposition attribute under one room
			(setq dlmpositionlist (list product globalnetworksegment globalsegmentposition))
			(setq errormultipledlmpositionlist (cons dlmpositionlist errormultipledlmpositionlist))
			(setq dlmpositionlist nil)
			)																	;--- end progn
		)																		;-- end if

		;;;products list builder
		;;;add product to the list each time we read in from the text file
		;;;the products list will be set to nil when we find a new room
	  	(if	(/= product nil)															;-- if statment to protect list from nil
			(setq products (cons product products))													;build products list
		)																		;-- end if

		;;;save off variables for continueing the loop and for subfunctions
		;;;roomLoopCheck is used on the current pre test loop
		;;;GLOBALroomforGSR2-RungBuilder will be used in the blockbuilder subfunction
	  	(setq roomLoopCheck globalroom)															;store globalroom to roomLoopCheck
	  	(setq GLOBALroomforGSR2-RungBuilder globalroom)

		;;;final step, read next room from text file
		;;;now globalroom has the current room
		;;;roomLoopCheck has the room from the last iteration for testing purposes
		(setq globalroom (read-line file1))
	)																			;- end while


	


  	
  	(setq singlelineroomlist (reverse singlelineroomlist))													;reverse singlelineroomlist to account for cons property which builds list backwards
  	
;;;  	this conditional block handles operations when there are segments on the project
;;;  	the first statement will happen anytime there is more than one networked room
;;;  	it will sort the segments first, then parse them individually
;;;  	the second statement will happen anytime there is one networked room
;;;  	this will only parse the room since it won't need to be numerically sorted because there is only one
	(setq segmentchecker (length globalSEGMENTDOTTEDPAIRS))													;set segmentchecker to the length of singlelineroomlist
  	(cond																			;- cond block
	  	;;;	update notes, used to call sort and parse only greater than 1 item
	  	((>= segmentchecker 1)																;-- cond statement
		(GSR2-segmentsort)
	  	(GSR2-segmentlistparse)
		)																		;-- end cond statement
		
	)																			;-- end cond statement

;;;	reverse the list GSR2-segmentlistparse created if it exists
  	(if	(/= globalSEGMENTSTOINSERT nil)
		(setq globalSEGMENTSTOINSERT (reverse globalSEGMENTSTOINSERT))
	)
  
  	
;;;  	this conditional block handles operations when there are standalone rooms on the project
;;;  	the first statement will happen anytime there is more than one standalone room
;;;  	it will sort the segments first, then parse them individually
;;;  	the second statement will happen anytime there is one standalone room
;;;  	this will only parse the room since it won't need to be numerically sorted because there is only one
  	(setq standalonechecker (length globalSTANDALONEDOTTEDPAIRS))
  	(cond																			;- cond block
	  	((>= standalonechecker 1)															;-- cond statement
		(GSR2-standalonesort)
		)																		;-- end cond statement
	)  




;;;  	if statement to call GSR2-standalone-and-segment-inserter if atleast one of the lists that is used to insert from has items
;;;  	progn wrap is not needed for only one statement on the if, but I included it incase we need to add more lines here
  	(if
	  	(or
		(/= globalSEGMENTSTOINSERT nil) (/= globalSTANDALONENUMERICAL nil)
		)
	  	(progn
	  	(GSR2-standalone-and-segment-inserter)
		)
	)

;;;	burst all blocks into individual products
;;;	this is the only way draw order works
  	(setq ss nil)
  	(setq ss (ssget "x" '((0 . "INSERT")(8 . "Riser-Segments"))))
  	(if
	  	(/= ss nil)
	  	(progn
		(setq cntr 0)
		(setq cmax (sslength ss))
		(while
		  	(< cntr cmax)
		  	(setq item (ssname ss cntr))
		 	(command "_.explode" item)
		  	(setq cntr (+ cntr 1))
		)
		)
  		
	)

  	(setq ss nil)
  	(setq ss (ssget "x" '((0 . "INSERT")(8 . "Riser-Rooms"))))
  	(if
	  	(/= ss nil)
	  	(progn
		(setq cntr 0)
		(setq cmax (sslength ss))
		(while
		  	(< cntr cmax)
		  	(setq item (ssname ss cntr))
			(command "_.explode" item)
		  	(setq cntr (+ cntr 1))
		)
		)
	)
  
  	(GSR2-riserdraworder)
  	(close file1)
  	(princ)

)

;end GSR







; --- GSR2-RiserHeirarchy Sub Function ---
; accept a list of products in from Main and seperates them into RH sublists for other functions
; Alex Lundin 03-17-2017
(defun GSR2-RiserHeirarchy (RHproducts /
RHCONTROLLERS RHDLMCONTROLLERS RHDLMDAYLIGHT RHDLMINTERFACES RHDLMNETWORKBRIDGES RHDLMOCCCORNERMOUNT RHDLMOCCSENSORS RHDLMPANELS RHDLMPLUGCONTROLLERS RHDLMSEGMENTMANAGERS RHDLMSPLITTERS RHDLMSWITCHES RHDLMZONECONTROLLERS RHREMAINDER RETURNLIST

		       CMAX CNTR PLACEMENTCHECK RHPRODUCT)
;;;  		arguments					
;;;  		GSR:						
;;;		products into RHproducts			
;;;                                                       	
;;;  		RH variables used from other functions	
;;;  		None						
;;;								
;;;  		RH variables for other functions		
;;;		GSR2-PortCalculator :					
;;;  		RHControllers				
;;;  		GSR2-RungBuilder:					
;;;  		RHDLMCONTROLLERS RHDLMDAYLIGHT RHDLMINTERFACES RHDLMNETWORKBRIDGES RHDLMOCCCORNERMOUNT RHDLMOCCSENSORS RHDLMPANELS RHDLMPLUGCONTROLLERS RHDLMSPLITTERS RHDLMSWITCHES RHDLMZONECONTROLLERS RHREMAINDER
;;;		GSR2-segmentbuilder:
;;;  		RHDLMSEGMENTMANAGERS
;;;								
;;;  		Local variables					
;;;  		CMAX CNTR PLACEMENTCHECK RHPRODUCT		
;;;								
;;;  	Special Notes:						
;;;  	RHControllers must stay RH, if it is localized, then the port calculation can't take place
;;;  	RHCONTROLLERS RHDLMCONTROLLERS RHDLMDAYLIGHT RHDLMINTERFACES RHDLMNETWORKDEVICES RHDLMNETWORKBRIDGES RHDLMOCCCORNERMOUNT RHDLMOCCSENSORS RHDLMPANELS RHDLMPLUGCONTROLLERS RHDLMSEGMENTMANAGERS RHDLMSPLITTERS RHDLMSWITCHES RHDLMZONECONTROLLERS RHREMAINDER must stay RH, if it is localized, then the rung building can't take place
  
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




; --- GSR2-PortCalculator  Sub Function ---
; accept a list of products in from Main and calculate types of ports avaiable for each item
; this function also calculates the number of each type of rung in PC variables
; Alex Lundin 03-17-2017
(defun GSR2-PortCalculator  (PCproducts PCnumberofcontrollers / PCCONTROLLERS PCNUMBEROFRUNG1 PCNUMBEROFRUNG2 PCNUMBEROFRUNG3 PCNUMBEROFRUNGFINAL RETURNLIST CMAX CNTR PCPRODUCT PORTS PORTSFORCONTROLLERCONNECTION USEABLEPORTS)
;;;  	arguments passed in
;;;  	GSR:
;;;	products into PCproducts
;;;	number of controllers into PCnumberofcontrollers
;;;
;;;  	PC variables used from other functions
;;;	GSR2-RiserHeirarchy:
;;;	PCControllers
;;;
;;;  	PC Variables for other functions
;;;  	GSR2-RungBuilder:
;;;  	PCNUMBEROFRUNG1 PCNUMBEROFRUNG2 PCNUMBEROFRUNG3 
;;;
;;;  	Local variables:
;;;  	CMAX CNTR PCPRODUCT PORTS PORTSFORCONTROLLERCONNECTION USEABLEPORTS
;;;
;;;  	Special Notes:
;;;  	PCNUMBEROFRUNG1 PCNUMBEROFRUNG2 PCNUMBEROFRUNG3 must stay PC, if it is localized, then the rung building can't take place
  
  	(vl-load-com)
  	(setq cntr 0)
  	(setq cmax (length PCproducts))
  	(setq ports 0)
  	(setq PCNUMBEROFRUNG1 0)
  	(setq PCNUMBEROFRUNG2 0)
  	(setq PCNUMBEROFRUNG3 0)
  	(setq PCNUMBEROFRUNGFINAL 0)
  	(while	(< cntr cmax)
	  	(setq PCproduct (nth cntr PCproducts))


	  	(if
			(OR (= PCproduct "LMCP48")(= PCproduct "LMCP24")(= PCproduct "LMCP12")(= PCproduct "LMCP8"))
			(progn
			(setq ports (+ 4 ports))
			(setq PCNUMBEROFRUNG3 (+ 1 PCNUMBEROFRUNG3))
			)
		)

	  
	  	(if
			(OR (= PCproduct "LMRC-222")(= PCproduct "LMRC-221")(= PCproduct "LMRC-213-347v")(= PCproduct "LMRC-212-347v")(= PCproduct "LMRC-211-347v")(= PCproduct "LMRC-213")(= PCproduct "LMRC-212")(= PCproduct "LMRC-211")(= PCproduct "LMPL-201"))
			(progn
			(setq ports (+ 4 ports))
		  	(setq PCNUMBEROFRUNG1 (+ 1 PCNUMBEROFRUNG1))
		  	(setq PCNUMBEROFRUNG2 (+ 1 PCNUMBEROFRUNG2))
			)
		)

	  
	  
	  	(if
			(OR (= PCproduct "LMPL-101")(= PCproduct "LMRC-102")(= PCproduct "LMRC-101"))
		  	(progn
			(setq ports (+ 3 ports))
		  	(setq PCNUMBEROFRUNG3 (+ 1 PCNUMBEROFRUNG3))
			)
		)

	  	(if
			(OR (= PCproduct "LMRC-112-M")(= PCproduct "LMRC-111-M")(= PCproduct "LMRC-112")(= PCproduct "LMRC-111"))
		  	(progn
			(setq ports (+ 2 ports))
			)
		)	
	  	(setq cntr (+ 1 cntr ))

	)
  	(if
	  	(/= PCControllers 0)
		(progn
		(setq portsForControllerConnection (* 2 PCnumberofcontrollers))
		(setq portsForControllerConnection (- portsForControllerConnection 1))
		(setq useablePorts (- ports portsForControllerConnection))
		(setq PCNUMBEROFRUNGFINAL 1)
		)
	)
  	(setq returnlist (list PCNUMBEROFRUNG1 PCNUMBEROFRUNG2 PCNUMBEROFRUNG3 PCNUMBEROFRUNGFINAL))
)



(defun GSR2-segmentsort ( / )
  	;old sorting way, only sorted by entire string, doesn't work for segments with two characters
	;;;	(setq globalSEGMENTLISTNUMERICAL (vl-sort globalSEGMENTDOTTEDPAIRS (function (lambda (x y)(< (car x)(car y))))))
  	(setq globalSEGMENTLISTNUMERICAL (vl-sort globalSEGMENTDOTTEDPAIRS (function (lambda (x y)(< (setq x (atoi (nth 1 x)))(setq y (atoi (nth 1 y))))))))
  	;numerical sort of segmentdottedpairs, this organizes them by first character of segment number
  	;then numerical sort again by second character
	(setq globalSEGMENTLISTNUMERICAL (vl-sort globalSEGMENTLISTNUMERICAL (function (lambda (x y)(< (setq x (substr (car x) 1 1))(setq y (substr (car y) 1 1)))))))
  	(setq globalSEGMENTLISTNUMERICAL (vl-sort globalSEGMENTLISTNUMERICAL (function (lambda (x y)(< (setq x (substr (car x) 2 1))(setq y (substr (car y) 2 1)))))))
)

(defun GSR2-standalonesort ( / )
	(setq globalSTANDALONENUMERICAL (vl-sort globalSTANDALONEDOTTEDPAIRS (function (lambda (x y)(< (car x)(car y))))))					;numerical sort of segmentdottedpairs, this organizes them by segment number
)





; --- GSR2-entmod-blockinsert-attributes Sub Function ---
; releases global variables from memory
; Alex Lundin 03-31-2017
(defun GSR2-GSR2-entmod-blockinsert-attributes-DESTROYER ( / )
;;;arguments
;;;	None
;;;
;;;Global from
;;;	GSR:
;;;	globalcabinetinformationlist
;;;Special notes
;;;  	This function only uses one global, it must be destroyed each time this function is called
  (FOREACH X' (
	globalcabinetinformationlist
   ) (SET X NIL))
  
	(princ)

)


; --- GSR2-entmod-blockinsert-attributes Sub Function ---
; inserts block
; inserts any existing attributes from the block definition table
; file out attributes from list sent in from calling function
; Arguments:
; attributelist		list of attribute values passed in from GSR2-RungBuilder
; Alex Lundin 03-31-2017
(defun GSR2-entmod-blockinsert-attributes
       				(
				attributelist blkinsertionpoint layername riserblockname 
				/
				Attdefs ATTBLK ATTBLKNAME ATTRIBUTEDXF11 ATTRIBUTEINSERTIONPOINT ATTRIBUTEXCOORD ATTRIBUTEYCOORD BLOCKXCOORD
				BLOCKYCOORD DATA ENAME NEWINSERTIONPOINT NEWXCOORD NEWYCOORD NEXTENT GSR2-entmod-blockinsert-attributes-cntr ATTRIBUTEDXF10 ATTRIBUTEVALUE NEWINSERTIONPOINTDXF10 NEWINSERTIONPOINTDXF11
				)
;;;arguments
;;;  	GSR:
;;;  	attributevalue - value to place into block
;;;  	blkinsertionpoint - insertion point for filled out block
;;;	layername - name of layer to insert block on
;;;	riserblockname - name of cabinet passed in from GSR2-RungBuilder
;;;
;;;Global from
;;;	None
;;;Global to
;;;  	None
	
	(setq attblkname riserblockname)
	(setq GSR2-entmod-blockinsert-attributes-cntr 0)
  	(setq blockxcoord (car blkinsertionpoint))
	(setq blockycoord (cadr blkinsertionpoint))
  
	(cond															;-cond block
		((setq Ename (tblobjname "block" attblkname))  									;--cond statement, get Parent entity name, when true continue conditional
		(setq NextEnt (entnext Ename))											;first sub entity

			(while NextEnt												;get ATTDEF subentities
			(setq Data (entget NextEnt))
				(if (= "ATTDEF" (cdr (assoc 0 Data)))
				(setq Attdefs (cons Data Attdefs))
				)
			(setq NextEnt (entnext NextEnt))
			)
		(setq attblk (if (= nil Attdefs) 0 1))										;set attblk variable to determine if block definition has attributes
		(and														;---and for outside items
				(entmake											;---first and, entmake insert for the block
				(list
				'(0 . "INSERT")
				'(100 . "AcDbBlockReference")
				(cons  8 layername)										;layer name
				(cons 66 attblk)										;0 or 1 value, if block has attributes it will be 1
				(cons  2 attblkname)
				(cons 10 blkinsertionpoint  )									;insert point
				(cons 41 1.0)
				(cons 42 1.0)
				(cons 43 1.0)
				)
				)												;---end first and
			
				(foreach x (reverse Attdefs)									;---second and, foreach loop for every item in attdefs

					(setq attributevalue (nth GSR2-entmod-blockinsert-attributes-cntr attributelist))		;get first attribute value from the list of them
					(setq GSR2-entmod-blockinsert-attributes-cntr (+ GSR2-entmod-blockinsert-attributes-cntr 1))	;increase counter for next loop
						
					(setq attributedxf11 (assoc 11 x))							;get 11th dxf code from item x, these next few lines calculate where the attribute is placed based on the block insertion point
					(setq attributeinsertionpoint (cdr attributedxf11))
					(setq attributexcoord (car attributeinsertionpoint))
					(setq attributeycoord (cadr attributeinsertionpoint))
					(setq newxcoord (+ blockxcoord attributexcoord))
					(setq newycoord (+ blockycoord attributeycoord))
					(setq newinsertionpointdxf11 (list newxcoord newycoord))




					(setq attributedxf10 (assoc 10 x))							;get 10th dxf code from item x, these next few lines calculate where the attribute is placed based on the block insertion point
					(setq attributeinsertionpoint (cdr attributedxf10))
					(setq attributexcoord (car attributeinsertionpoint))
					(setq attributeycoord (cadr attributeinsertionpoint))
					(setq newxcoord (+ blockxcoord attributexcoord))
					(setq newycoord (+ blockycoord attributeycoord))
					(setq newinsertionpointdxf10 (list newxcoord newycoord))


					  
					(entmake										;entmake the attribute with all the information associated with x
					(list
					'(0 . "ATTRIB")
					(assoc  8 x)
					(assoc 40 x)
					(cons  1 attributevalue)								;use the attributevalue passed in as a arguement
					(assoc 50 x)
					(assoc 41 x)
					(assoc 51 x)
					(assoc  7 x)
					(assoc 71 x)
					(assoc 72 x)
					(cons 10 newinsertionpointdxf10)							;use the new insertion points to locate the attribute based on where the block is inserted
					(cons 11 newinsertionpointdxf11)
					(assoc  2 x)
					(assoc 70 x)
					(assoc 73 x)
					(assoc 74 x)
					)
					)
				)												;---end second add
				(entmake '((0 . "SEQEND")(8 . "0")))    							;---third add, entmake SEQEND to signify final attribute at end of block
		)
		)														;--cond statement
		(T nil)
	)															;-end cond


)



(defun GSR2-standalone-and-segment-inserter
       			(
		      	/
		      	blockexists
			BLOCKHEIGHT BLOCKWIDTH  BLOCKNAME C CABINETXCOORD CABINETYCOORD CLS CMAX   
			INSERTIONPOINT POINT-LIST POINT1 POINT2 POINT3 POLYLINE-LAYER POLYLINE-LINETYPE 
			POLYLINE-WIDTH ROOMITEM ROOMNAME SEGMENTHEIGHT SEGMENTITEM SEGMENTMANGERBLOCKNAME SEGMENTNAME 
			STANDALONEHEIGHTMAX XCOORD YCOORD undrawn-segments-point-list cabinetxorigin cabinetyorigin ATTRIBUTEVALUELIST C2 C2MAX CABINETXCOORD2 CABINETYCOORD2 CORNER1 CORNER2 CORNER3 CORNER4
			ITEM LAYER SEGMENTNAMEONLY VIEWPORTCREATE VIEWPORTNUMBER VIEWPORTXORIGIN VIEWPORTYORIGIN VPYCOORD XCOORD2 YCOORD2 YCOORDPAGESIZE REFERENCEYCOORD STANDALONEWIDTHMAX VPXCOORD CORNER5
			CHARACTER1 CHARACTER1PREVIOUS CHARACTER2 CHARACTER2PREVIOUS 
			SAMESEGMENTRUNCHECK STRING segmentwidth NUMBEROFSEGMENTCHARACTERS STRINGLENGTH
		      	)

;;;arguments
;;;  	None
;;;
;;;Global from
;;;	GSR2-standalonesort:
;;;	globalSTANDALONENUMERICAL
;;;	GSR2-segmentbuilder:
;;;	globalSEGMENTSTOINSERT
;;;Global to
;;;  	None

	(setq ycoordpagesize 75)
  	
  
  	;;;standalone insert
    	(setq xcoord 0)
  	(setq viewportxorigin (- xcoord 3.0))
	(setq ycoord -30)
  	(setq viewportyorigin (+ ycoord 1.0))
  	(setq insertionPoint (list xcoord ycoord))
  	(setq c 0)
  	(setq viewportnumber 0)
  	(setq cmax (length globalSTANDALONENUMERICAL))
	(setq standaloneheightmax 0)
  	(while 	(< c cmax)
	  	(setq roomItem (nth c globalSTANDALONENUMERICAL))
	  		(if	(/= roomItem nil)
			  	(progn
			  	(setq roomname (car roomItem))
				(setq blockname (strcat "Z-Riser-" roomname))
			  	(setq BLOCKWIDTH (nth 1 roomItem))
			  	(setq BLOCKHEIGHT (nth 2 roomItem))
		  		(entmakex											;entmakex function
				(list												;list of all required items
				(cons 0 "INSERT")										;type of entity
			        (cons 2 blockname)										;name of block to insert
				(cons 8 "Riser-Rooms")
			       	(cons 10 insertionPoint)									;block insertion point
			       	(cons 41 1)											; Scale Factor
			       	(cons 42 1)											; Scale Factor?
			      	(cons 43 1)											; Scale Factor?
				)
				)
				(setq xcoord (+ BLOCKWIDTH xcoord))
			  	(setq insertionPoint (list xcoord ycoord))

				;;; max height calculator
				(if	(> BLOCKHEIGHT standaloneheightmax)
				  	(setq standaloneheightmax BLOCKHEIGHT)
				)
				
				;;; max width calculator
				(if	(> xcoord standalonewidthmax)
				  	(setq standalonewidthmax xcoord)
				)
				
				;;; when code reaches xcoord of 90, move down below the longest block that was inserted on the row above
				(if
				  	(or
					(>= xcoord 90)
					)
				  	(progn
					;;;create new coordinate variables
					(setq vpxcoord xcoord)
					(setq vpycoord (- ycoord standaloneheightmax))						
					(setq vpycoord (+ ycoord 1.0))								;lift the ycoord off the text from the room below
					(setq referenceycoord (+ ycoord 30))							;set reference ycoord to calculate how many pages are available in the space
					;;; calculate the ideal page size with division
					(setq viewportcreate (/ referenceycoord ycoordpagesize))
					(setq viewportcreate (RoundDown viewportcreate))
					(setq viewportcreate (abs viewportcreate))
					;;;when the remainder is greater than the number of pages created
					(if
						(or
					  	(> viewportcreate viewportnumber)
						)
					  	(progn
						(setq corner1 (list viewportxorigin viewportyorigin))
						(setq corner2 (list standalonewidthmax viewportyorigin))
						(setq corner3 (list standalonewidthmax vpycoord))
						(setq corner4 (list viewportxorigin vpycoord))
						(setq corner5 (list viewportxorigin viewportyorigin))


						(setq point-list (list corner1 corner2 corner3 corner4 corner5))
					  	(setq cls 0)
					  	(setq polyline-layer "Riser-MSTP")
					  	(setq polyline-width 0.06)
					  	(setq polyline-linetype "hidden")
				

						(GSR2-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)



						
						(setq viewportnumber (+ viewportnumber 1))
						
					  	(setq viewportyorigin vpycoord)
						(setq standalonewidthmax 0)
						)
					)
						
					;;;move ycoord down
					(setq xcoord 0)
					(setq ycoord (- ycoord standaloneheightmax))

					
					(setq insertionPoint (list xcoord ycoord))
					(setq standaloneheightmax 0)
					
					)
				)
				
				)
			)
	  	(setq c (+ 1 c))
	)

  	(if
	  	(AND
		(/= standalonewidthmax 0)(/= standalonewidthmax nil)
		)
	  	(progn
		;;;create new coordinate variables
		(setq vpxcoord xcoord)
		(setq vpycoord (- ycoord standaloneheightmax))						
		(setq vpycoord (+ vpycoord 1.0))											;lift the ycoord off the text from the room below

		(setq corner1 (list viewportxorigin viewportyorigin))
		(setq corner2 (list standalonewidthmax viewportyorigin))
		(setq corner3 (list standalonewidthmax vpycoord))
		(setq corner4 (list viewportxorigin vpycoord))
		(setq corner5 (list viewportxorigin viewportyorigin))


		(setq point-list (list corner1 corner2 corner3 corner4 corner5))
	  	(setq cls 0)
	  	(setq polyline-layer "Riser-MSTP")
	  	(setq polyline-width 0.06)
	  	(setq polyline-linetype "hidden")


		(GSR2-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)



		
		(setq viewportnumber (+ viewportnumber 1))
		
	  	(setq viewportyorigin vpycoord)
		(setq standalonewidthmax 0)
		)
	)

  	;;;segment insert
  	(if
	  	(/= globalSEGMENTSTOINSERT nil)
	  	(progn
	  	(setq viewportnumber 0)
	    	(setq xcoord 0)
		(setq ycoord 0)
	  	(setq insertionPoint (list xcoord ycoord))
	  	(setq c 0)
	  	(setq cmax (length globalSEGMENTSTOINSERT))
		(setq segmentHeight 0)
	  	(setq segmentItem (nth c globalSEGMENTSTOINSERT))
	  	(setq segmentName (car segmentItem))
	  	(setq segmentHeight (nth 1 segmentItem))
	  	(setq segmentWidth (nth 2 segmentItem))
	  	(setq vpxcoord (+ xcoord segmentWidth))
	  	(setq vpycoord (- ycoord segmentHeight))										;save ycoord for drawing viewport box
	  	(setq ycoord (+ ycoord segmentHeight))
	  	(setq insertionPoint (list xcoord ycoord))
		)
	)
  	;;;segment insert
  	(if
	  	(= globalSEGMENTSTOINSERT nil)
	  	(progn
	  	(setq c 0)
	  	(setq cmax 0)
		)
	)
  
  	(while 	(< c cmax)

		(setq layer "Riser-Segments")
		(setq attributevaluelist nil)
		;;;call GSR2-entmod-blockinsert-attributes to insert the block
		(GSR2-entmod-blockinsert-attributes attributevaluelist insertionPoint layer segmentName)

	  
	  	;;;insert mstp_cable_id block
		(setq layer "Riser")
	  	(setq segmentNameOnly (substr segmentName 17))									;remove first 16 characters from string
		(setq attributevaluelist (list segmentNameOnly))
		;;;call GSR2-entmod-blockinsert-attributes to insert the block
		(GSR2-entmod-blockinsert-attributes attributevaluelist insertionPoint layer ConstantMstpCableIDBlock)


	  
	  	(setq segmentmangerblockname (strcat segmentName "-Segment-Manager"))						;create string of roomname of segment manger block

	  	(setq blockexists (tblsearch "block" segmentmangerblockname))							;use blockexists variable to determine if there is a segment mananger connected to this segment





	  	;;; save previous characters if they exist
		(if
		  	(and
			(/= character1 nil)(/= character2 nil)
			)
		  	(progn
			(setq character1previous character1)
			(setq character2previous character2)
			)
		)
	  
	  	;;; save current characters
	  	(setq stringlength (strlen segmentName ))
	  	(setq numberofsegmentcharacters (- stringlength 16))								;calcualte number of characters after Z-RISER-SEGMENT- prefix
	  	(setq string (substr segmentName 17 numberofsegmentcharacters))							;pull off the postfix string that identifies the segment
		(if
		  	(= numberofsegmentcharacters 1)
		  	(setq character1 (substr string 1 1))
		)
  		(if
		  	(> numberofsegmentcharacters 1)
		  	(progn
		  	(setq character1 (substr string 1 1))
			(setq character2 (substr string 2 1))
			)
		)
	  

		;;;determine if we are on same run of network segments
	  	;;;string compare previous to current
		(if
		  	;;;if string is 1-10 then this is the first segment manager on the job and this step is skiped
		  	(AND
			(/= string nil)(/= string "1")(/= string "2")(/= string "3")(/= string "4")(/= string "5")(/= string "6")(/= string "7")(/= string "8")(/= string "9")(/= string "10")
			)
		  	(progn

			(setq samesegmentruncheck 0)
			;;;conditional statement compares all combinations of matches on either character
			;;;this catches any errors on labeling
			;;;example would be 1A or A1, these conditional statement handle either labeling convetion
			(cond
			  	((= character1 character1previous)
				(setq samesegmentruncheck 1)
				)
				
			  	((= character2 character1previous)
				(setq samesegmentruncheck 1)
				)
				
			  	((= character1 character2previous)
				(setq samesegmentruncheck 1)
				)
				
			  	((= character2 character2previous)
				(setq samesegmentruncheck 1)
				)				
			)

			;;;conditional statement takes action if we are not on the same set of runs
			(cond
			  	((= samesegmentruncheck 0)
				(setq cabinetxcoord nil)
				(setq cabinetycoord nil)
				)				
			)

			)
		)






	  
	  	(cond
		  	((/= blockexists nil)											;if the segment manager is on this segment, then insert it
			
		  	(setq xcoord -5)
			(setq vpxcoord -10)											;set vpxcoord
			(setq insertionPoint (list xcoord ycoord))
			 
			(setq layer "Riser-Segments")
			(setq attributevaluelist nil)
			;;;call GSR2-entmod-blockinsert-attributes to insert the block
			(GSR2-entmod-blockinsert-attributes attributevaluelist insertionPoint layer segmentmangerblockname)

			(setq cabinetxcoord (+ xcoord 1))									;shift xcoord to the right by 1, which will start the cable runs on the right half of the cabinet top
			(setq cabinetycoord (- ycoord 1.13))									;shift ycoord down by 1.13, which will start the cable runs at the top of the cabinet
			(setq cabinetxcoord2 (- cabinetxcoord 2))
			(setq cabinetycoord2 (- cabinetycoord 1.12))
			(setq xcoord 0)

			;;; these three points make up the mstp wire from the segment
			(setq point1 (list xcoord ycoord))
			(setq point2 (list cabinetxcoord ycoord))
			(setq point3 (list cabinetxcoord cabinetycoord))

			(setq point-list (list point1 point2 point3))
		  	(setq cls 0)
		  	(setq polyline-layer "Riser-MSTP")
		  	(setq polyline-width 0.06)
		  	(setq polyline-linetype "hidden")
	

			(GSR2-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)



			 

			;;; shift the cabinetxcoord
			(setq cabinetxcoord (- cabinetxcoord 0.2))								;shift xcoord to the right by .2 to the left

			 
			)

			((= blockexists nil)											;if segment manager is not on this segment, run mstp wire to whenever the last one was

			;;; if the segment manager has not been inserted yet
			;;; save points for connecting after we place the cabinet
			(if
			  	(or
				(= cabinetxcoord nil)(= cabinetycoord nil)
				)
			  	(progn
				(setq point1 (list xcoord ycoord))
				(setq undrawn-segments-point-list (cons point1 undrawn-segments-point-list))

				)
			)

			;;; if the segment manager has been inserted
			;;; run cable down to it
			(if
			  	(and
				(/= cabinetxcoord nil)(/= cabinetycoord nil)
				)
			  	(progn

				;;;start by connecting any undrawn segments, these will be anything below the segment manager
				(if
				  	(/= undrawn-segments-point-list nil)
				  	(progn
					(setq c2 0)
					(setq c2max (length undrawn-segments-point-list))
					(setq undrawn-segments-point-list (reverse undrawn-segments-point-list))		;reverse to account for cons
					(while
					  	(< c2 c2max)
					  	(setq item (nth c2 undrawn-segments-point-list))
					  	(setq xcoord2 (car item))
					  	(setq ycoord2 (cadr item))
					  
						;;; these three points make up the mstp wire from the segment
						(setq point1 (list xcoord2 ycoord2))
						(setq point2 (list cabinetxcoord2 ycoord2))
						(setq point3 (list cabinetxcoord2 cabinetycoord2))

						(setq point-list (list point1 point2 point3))
					  	(setq cls 0)
					  	(setq polyline-layer "Riser-MSTP")
					  	(setq polyline-width 0.06)
					  	(setq polyline-linetype "hidden")
						(GSR2-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)
						
						;;; shift the cabinetxcoord
						(setq cabinetxcoord2 (+ cabinetxcoord2 0.2))					;shift xcoord to the right by .2 to the left

					  	(setq c2 (+ c2 1))
					)
					(setq undrawn-segments-point-list nil)
					  
					)
				)

				;;; now connect the current segment
				;;; these three points make up the mstp wire from the segment
				(setq point1 (list xcoord ycoord))
				(setq point2 (list cabinetxcoord ycoord))
				(setq point3 (list cabinetxcoord cabinetycoord))

				(setq point-list (list point1 point2 point3))
			  	(setq cls 0)
			  	(setq polyline-layer "Riser-MSTP")
			  	(setq polyline-width 0.06)
			  	(setq polyline-linetype "hidden")
				(GSR2-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)
				
				;;; shift the cabinetxcoord
				(setq cabinetxcoord (- cabinetxcoord 0.2))							;shift xcoord to the right by .2 to the left
				)
			)
			 
			)
		)



	  
		(setq c (+ 1 c))



	  	;;;prime the variables for the next iteration
		(if	(/= c cmax)
		  	(progn

			;;; calculate the ideal page size with division
			(setq viewportcreate (/ ycoord ycoordpagesize))
			(setq viewportcreate (RoundDown viewportcreate))
			(setq viewportcreate (abs viewportcreate))
			;;;when the remainder is greater than the number of pages created
			;;;viewport creator
			
;;;			(if
;;;				(or
;;;			  	(> viewportcreate viewportnumber)
;;;				)
;;;			  	(progn
;;;				(setq corner1 (list vpxcoord vpycoord))
;;;				(setq corner2 (list vpxcoord ycoord))
;;;				(setq corner3 (list xcoord ycoord))
;;;				(setq corner4 (list vpxcoord ycoord))
;;;				(setq corner5 (list vpxcoord vpycoord))
;;;
;;;
;;;				(setq point-list (list corner1 corner2 corner3 corner4 corner5))
;;;			  	(setq cls 0)
;;;			  	(setq polyline-layer "0")
;;;			  	(setq polyline-width 0.06)
;;;			  	(setq polyline-linetype "hidden")
;;;		
;;;
;;;				(GSR2-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)
;;;
;;;
;;;
;;;				
;;;				(setq viewportnumber (+ viewportnumber 1))
;;;				
;;;				)
;;;			)
			
	  		(setq segmentItem (nth c globalSEGMENTSTOINSERT))
		  	(setq segmentName (car segmentItem))
	  		(setq segmentHeight (nth 1 segmentItem))
			(setq segmentwidth (nth 2 segmentItem))
	  		(setq segmentHeight (+ segmentHeight 1.3))
		  	(setq ycoord (+ segmentHeight ycoord))
		  	(setq insertionPoint (list xcoord ycoord))


			)

			
		)
		  	
	)



)

(defun GSR2-segmentbuilder 	(

		       	/
		       	blockexists
		       	BLOCKHEIGHT BLOCKNAME BLOCKWIDTH BRIDGEPOSITION CLS COUNTER COUNTERMAX  
			INSERTEDROOMS INSERTIONPOINT POINT-LIST POLYLINE-LAYER POLYLINE-LINETYPE POLYLINE-WIDTH ROOMCURRENT SEGMENTHEIGHT 
			SEGMENTINSERTCURRENT SEGMENTINSERTITEM SEGMENTITEM SEGMENTPOINT1 
			SEGMENTPOINT2 XCOORD XCOORDSEGMENTEND XCOORDSEGMENTSTART YCOORD segmentcurrentrowHeight direction
			 ATTRIBUTEVALUELIST LAYER SEGMENTPOINT3 SEGMENTPOINT4 YCOORDABS segmentwidthmax SEGMENTWIDTH XSHIFTER ifchecker
		       	)

;;;arguments
;;;	None
;;;
;;;Global from
;;;  	GSR2-segmentlistparse:
;;;	GLOBALROOMSONSEGMENT
;;;Global to
;;;  	GSR2-standalone-and-segment-inserter:
;;;  	GLOBALSEGMENTSTOINSERT

  
  	(setq counter 0)
  	(setq countermax (length GLOBALROOMSONSEGMENT))
	(setq SegmentInsertItem (nth counter GLOBALROOMSONSEGMENT))											
	(setq SegmentInsertCurrent (car SegmentInsertItem))
  
  	;BLOCK Header definition:
  	(setq xcoord 0)
	(setq ycoord 0)															;set y coord to 0
  	(setq insertionPoint (list xcoord ycoord))
	(setq segmentHeight 0)
  	(setq segmentcurrentrowHeight 0)
  	(setq segmentwidthmax 0)
  	(setq blockName (strcat "Z-Riser-Segment-" SegmentInsertCurrent))
	(entmake (list (cons 0 "BLOCK")(cons 2 blockName)(cons 8 "Riser-Segments")(cons 70 2)(cons 10 insertionPoint)))  					;begin block definition

	
	(setq segmentpoint1 (list xcoord ycoord))
  	(setq xcoord (+ xcoord 5))
  	(setq segmentpoint2 (list xcoord ycoord))
	(setq point-list (list segmentpoint1 segmentpoint2))
  	(setq cls 0)
  	(setq polyline-layer "Riser-MSTP")
  	(setq polyline-width 0.06)
  	(setq polyline-linetype "hidden")
	(GSR2-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)	
  
  	

  	
  	(setq direction 1)
  	(setq insertionPoint (list xcoord ycoord))
  	(while 	(< counter countermax)
	  	
	  	(setq SegmentInsertItem (nth counter GLOBALROOMSONSEGMENT))
	  	(if	(/= SegmentInsertItem nil)
		  	(progn

			(setq segmentpoint1 (list xcoord ycoord))


			  
		  	(setq SegmentInsertCurrent (car SegmentInsertItem))											
		  	(setq bridgePosition (cadr SegmentInsertItem))												
			(setq roomCurrent (caddr SegmentInsertItem))												
			(setq BLOCKWIDTH (cadddr SegmentInsertItem))
			(setq BLOCKWIDTH (abs BLOCKWIDTH))
			(setq BLOCKHEIGHT (nth 4 SegmentInsertItem))
			(setq BLOCKHEIGHT (abs BLOCKHEIGHT))
		  	(setq roomCurrent (strcat "Z-Riser-" roomCurrent))

			;;;used to caculate where block will land
			
			(setq xshifter (* BLOCKWIDTH direction))
			(setq xcoord (+ xcoord xshifter))
			
			;used to control only one outer if statement per iteration
			(setq ifchecker 0)
			
;;;			;;; when code reaches xcoord of 90, move down below the longest block that was inserted on the row above
			(if
			  	(and
				(> xcoord 80)(/= direction -1)(= ifchecker 0)
				)
			  	(progn
				(setq xcoord (- xcoord xshifter))
				(setq segmentwidth (+ xcoord BLOCKWIDTH))

				(if	(> segmentwidth segmentwidthmax)
				  	(setq segmentwidth segmentwidthmax)
				)

				;;;make right side segment connector during the drop
				(setq segmentpoint1 (list xcoord ycoord))
				(setq xcoord 85)
				(setq segmentpoint2 (list xcoord ycoord))
				(setq ycoord (- ycoord segmentcurrentrowHeight))						;move ycoord for drop
				(setq segmentpoint3 (list xcoord ycoord))
				(setq direction -1)										;set direction variable to negative
				(setq xshifter (* BLOCKWIDTH direction))
				(setq xcoord (+ xcoord xshifter))
				(setq segmentpoint4 (list xcoord ycoord))
				;;;connect points with MSTP
				(setq point-list (list segmentpoint1 segmentpoint2 segmentpoint3 segmentpoint4))
			  	(setq cls 0)
			  	(setq polyline-layer "Riser-MSTP")
			  	(setq polyline-width 0.06)
			  	(setq polyline-linetype "hidden")
				(GSR2-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)


				  
				
				
				(setq segmentcurrentrowHeight 0)								;reset currentrowheight


				(setq insertionPoint (list xcoord ycoord))
				(setq layer "Riser-Rooms")
				(setq attributevaluelist nil)
				;;;call GSR2-entmod-blockinsert-attributes to insert the block
				(GSR2-entmod-blockinsert-attributes attributevaluelist insertionPoint layer roomCurrent)

				(setq segmentpoint1 (list xcoord ycoord))
				
				(setq insertedRooms (cons roomCurrent insertedRooms))


				;;;connect points with MSTP if this is not the last block
				(if	(/= counter (- countermax 1))
				  	(progn
				  	(setq segmentpoint2 (list xcoord ycoord))
					(setq point-list (list segmentpoint1 segmentpoint2))
				  	(setq cls 0)
				  	(setq polyline-layer "Riser-MSTP")
				  	(setq polyline-width 0.06)
				  	(setq polyline-linetype "hidden")
					(GSR2-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)
					)
				)
				;set variable since we entered the outer if statement
				(setq ifchecker 1)
				)
			)
		
			;;; when code reaches xcoord of 10, move down below the longest block that was inserted on the row above
			(if
			  	(and
				(< xcoord 10)(/= direction 1)(= ifchecker 0)
				)
			  	(progn
  				(setq xcoord (- xcoord xshifter))
				;;;connect points with MSTP
			  	(setq segmentpoint2 (list xcoord ycoord))
				(setq point-list (list segmentpoint1 segmentpoint2))
			  	(setq cls 0)
			  	(setq polyline-layer "Riser-MSTP")
			  	(setq polyline-width 0.06)
			  	(setq polyline-linetype "hidden")
				(GSR2-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)

				;;;make left side segment connector during the drop
				(setq segmentpoint1 (list xcoord ycoord))
				(setq segmentpoint2 (list 5 ycoord))
				(setq ycoord (- ycoord segmentcurrentrowHeight))						;move ycoord for drop
				(setq segmentpoint3 (list 5 ycoord))
				(setq xcoord 5)
				(setq xcoord (+ xcoord 5))
				(setq segmentpoint4 (list xcoord ycoord))
				;;;connect points with MSTP
				(setq point-list (list segmentpoint1 segmentpoint2 segmentpoint3 segmentpoint4))
			  	(setq cls 0)
			  	(setq polyline-layer "Riser-MSTP")
			  	(setq polyline-width 0.06)
			  	(setq polyline-linetype "hidden")
				(GSR2-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)
				       
				(setq direction 1)										;set direction variable to negative
				(setq segmentcurrentrowHeight 0)								;reset currentrowheight

				(setq insertionPoint (list xcoord ycoord))
				(setq layer "Riser-Rooms")
				(setq attributevaluelist nil)
				;;;call GSR2-entmod-blockinsert-attributes to insert the block
				(GSR2-entmod-blockinsert-attributes attributevaluelist insertionPoint layer roomCurrent)
				
				(setq segmentpoint1 (list xcoord ycoord))
				
				(setq insertedRooms (cons roomCurrent insertedRooms))
				(setq xshifter (* BLOCKWIDTH direction))
				(setq xcoord (+ xcoord xshifter))
				
				(setq segmentpoint2 (list xcoord ycoord))
				;;;connect points with MSTP even if this is the last block
				;;;allows for drop and then last block connection
				(if
				  	(<= counter countermax )
				  	(progn
				  	(setq segmentpoint2 (list xcoord ycoord))
					(setq point-list (list segmentpoint1 segmentpoint2))
				  	(setq cls 0)
				  	(setq polyline-layer "Riser-MSTP")
				  	(setq polyline-width 0.06)
				  	(setq polyline-linetype "hidden")
					(GSR2-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)
					)
				)				
				;set variable since we entered the outer if statement
				(setq ifchecker 1)
				)
			)


			;;; when code is in the middle and going right
			;;; move xcoord last
			(if
			  	(and
				(> xcoord -50)(< xcoord 500)(= direction 1)(= ifchecker 0)
				)
			  	(progn
				(setq xcoord (- xcoord xshifter))  
				(setq segmentwidth (+ xcoord BLOCKWIDTH))

				(if	(> segmentwidth segmentwidthmax)
				  	(setq segmentwidth segmentwidthmax)
				)
				
				
				(setq insertionPoint (list xcoord ycoord))
				(setq layer "Riser-Rooms")
				(setq attributevaluelist nil)
				;;;call GSR2-entmod-blockinsert-attributes to insert the block
				(GSR2-entmod-blockinsert-attributes attributevaluelist insertionPoint layer roomCurrent)
			
				(setq insertedRooms (cons roomCurrent insertedRooms))
				(setq xshifter (* BLOCKWIDTH direction))
				(setq xcoord (+ xcoord xshifter))

				
				;;;connect points with MSTP if this is not the last block
				(if
				  	(< counter (- countermax 1))
				  	(progn
				  	(setq segmentpoint2 (list xcoord ycoord))
					(setq point-list (list segmentpoint1 segmentpoint2))
				  	(setq cls 0)
				  	(setq polyline-layer "Riser-MSTP")
				  	(setq polyline-width 0.06)
				  	(setq polyline-linetype "hidden")
					(GSR2-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)
					)
				)
				;set variable since we entered the outer if statement
				(setq ifchecker 1)				
				)
			)
			
			;;; when code is in the middle and going left
			;;; move xcoord first
			(if
			  	(and
				(> xcoord -50)(< xcoord 500)(= direction -1)(= ifchecker 0)
				)
			  	(progn

				(setq xcoord (- xcoord xshifter))
				(setq xshifter (* BLOCKWIDTH direction))
				(setq xcoord (+ xcoord xshifter))
				(setq insertionPoint (list xcoord ycoord))
				(setq layer "Riser-Rooms")
				(setq attributevaluelist nil)
				;;;call GSR2-entmod-blockinsert-attributes to insert the block
				(GSR2-entmod-blockinsert-attributes attributevaluelist insertionPoint layer roomCurrent)
				
				(setq insertedRooms (cons roomCurrent insertedRooms))
				
				;;;connect points with MSTP
			  	(setq segmentpoint2 (list xcoord ycoord))
				(setq point-list (list segmentpoint1 segmentpoint2))
			  	(setq cls 0)
			  	(setq polyline-layer "Riser-MSTP")
			  	(setq polyline-width 0.06)
			  	(setq polyline-linetype "hidden")
				(GSR2-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)			
				;set variable since we entered the outer if statement
				(setq ifchecker 1)								
				)
			)
				  
			;;;calculate segmentcurrentrowHeight after inserting block
			(if	(> BLOCKHEIGHT segmentcurrentrowHeight)
				(setq segmentcurrentrowHeight BLOCKHEIGHT)
			)
			

			)
		)
	  	(setq counter (+ counter 1))
	)
  
  	;;;calculate max segment height from ycoord and max block height on the last row
	(setq ycoordabs (abs ycoord))
	(setq segmentHeight (+ ycoordabs segmentcurrentrowHeight))

  
  	;;;subtract last block width from the xcoord, this lines MSTP wire up exactly with last room
	(setq xcoord (- xcoord BLOCKWIDTH))

;;;	(setq xcoordsegmentend xcoord)
;;;	(setq segmentpoint1 (list xcoordsegmentstart ycoord))
;;;  	(setq segmentpoint2 (list xcoordsegmentend ycoord))
;;;
;;;		
;;;	(setq point-list (list segmentpoint1 segmentpoint2))
;;;  	(setq cls 0)
;;;  	(setq polyline-layer "0")
;;;  	(setq polyline-width 0.06)
;;;  	(setq polyline-linetype "hidden")
;;;
;;;	(GSR2-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)
	(entmake
	(list
	(cons 0 "ENDBLK")
	)
	)
  
  	(setq segmentHeight (abs segmentHeight))
;;;  	(setq segmentItem (cons blockName segmentHeight))
  	(setq segmentItem (list blockName segmentHeight segmentwidthmax))
	(setq globalSEGMENTSTOINSERT (cons segmentItem globalSEGMENTSTOINSERT))
		 

		
)

;;;expects 0 as class value
;;;http://www.cadtutor.net/forum/showthread.php?44768-Entmake-Functions
;;;online
(defun GSR2-draw-lwpolyline (point-list cls polyline-layer polyline-width polyline-linetype)
;;;arguments
;;;	point-list
;;;	cls polyline-layer
;;;	polyline-width
;;;	polyline-linetype
;;;	polyine-width
;;;
;;;Global from
;;;	None
;;;
;;;Global to
;;;	None
  	(entmakex
	  		(append
			(list
			(cons 0 "LWPOLYLINE")	      
                       	(cons 100 "AcDbEntity")
                       	(cons 100 "AcDbPolyline")
			(cons 6 polyline-linetype)
			(cons 8 polyline-layer)
			(cons 40 polyline-width)
			(cons 41 polyline-width)
			(cons 43 polyline-width)
                       	(cons 90 (length point-list))
                      	(cons 70 cls))
       			(mapcar (function (lambda (p) (cons 10 p))) point-list))
	)
)



(defun GSR2-segmentlistparse
       			(
		     	/
		     	BRIDGEPOSITION 
 			ROOMCURRENT S S1 SEGMENTHEIGHT SINGLELINESEGMENTCHECK SINGLELINESEGMENTCURRENT SINGLELINESEGMENTITEM SMAX
		     	)

;;;arguments
;;;	None
;;;
;;;Global from
;;;	GSR:
;;;	GLOBALSEGMENTLISTNUMERICAL
;;;
;;;Global to
;;;  	GSR2-segmentbuilder:
;;;	GLOBALROOMSONSEGMENT

  
	(setq segmentHeight 0)
  	(setq s 0)														;set s to 0 -- counter to count number of segments in product text file
	
  	(setq smax (length globalSEGMENTLISTNUMERICAL))										;set smax to number of bridges in text file -- stored in bridgeSegmentListNumerical
;;;	set value for checking if we are on last item
  	(setq s1 (+ smax -1))
  	(setq singleLineSegmentItem (nth s globalSEGMENTLISTNUMERICAL))
	(setq singleLineSegmentCurrent (car singleLineSegmentItem))
	(setq singleLineSegmentCheck singleLineSegmentCurrent)																	
	(while 	(< s smax)
  		(setq singleLineSegmentItem (nth s globalSEGMENTLISTNUMERICAL))							;set singleLineSegmentItem to the item in bridgeSegmentListNumerical that corresponds to current s value
		(if	(/= singleLineSegmentItem nil)
		  	(progn
		  	(setq singleLineSegmentCurrent (car singleLineSegmentItem))						;set singleLineSegmentCurrent to first entry of singleLineSegmentItem -- segment number
		  
		  	(cond
			  	((= singleLineSegmentCurrent singleLineSegmentCheck)
				  	(setq bridgePosition (cadr singleLineSegmentItem))					;set bridgePosition to second entry of singleLineSegmentItem -- bridge position
					(setq roomCurrent (caddr singleLineSegmentItem))					;set roomCurrent to third entry of singleLineSegmentItem -- room name
					(setq GLOBALROOMSONSEGMENT (cons singleLineSegmentItem GLOBALROOMSONSEGMENT))
					(setq singleLineSegmentCheck singleLineSegmentCurrent)
				)
			)
		  	(cond
				((/= singleLineSegmentCurrent singleLineSegmentCheck)
				(setq GLOBALROOMSONSEGMENT (reverse GLOBALROOMSONSEGMENT))
				(GSR2-segmentbuilder)
				(setq GLOBALROOMSONSEGMENT nil)
				(setq GLOBALROOMSONSEGMENT (cons singleLineSegmentItem GLOBALROOMSONSEGMENT))
				(setq singleLineSegmentCheck singleLineSegmentCurrent)
				)
			)
			(if	(and (= s s1) (/= GLOBALROOMSONSEGMENT nil))
			  	(progn
				(setq GLOBALROOMSONSEGMENT (reverse GLOBALROOMSONSEGMENT))
				(GSR2-segmentbuilder)
				(setq GLOBALROOMSONSEGMENT nil)
				)
			)
			)
		)
	  	(setq s (+ s 1))
	)
)
			


(defun GSR2-destroyer2 (/)

  (FOREACH X' (
		globalcabinetinformationlist
   ) (SET X NIL))
  
	(princ)

)


(defun GSR2-destroyerfinal (/)

  (FOREACH X' (
		GLOBALDLMSEGMENTMANAGERS GLOBALDLMNETWORKDEVICES GLOBALDLMCONTROLLERS GLOBALDLMPLUGCONTROLLERS GLOBALNUMBEROFRUNGFINAL
		GLOBALDLMCONTROLLERS GLOBALDLMDAYLIGHT GLOBALDLMINTERFACES GLOBALDLMNETWORKDEVICES
	       GLOBALDLMOCCCORNERMOUNT GLOBALDLMOCCSENSORS GLOBALDLMPLUGCONTROLLERS GLOBALDLMSPLITTERS GLOBALDLMSWITCHES GLOBALNUMBEROFRUNG1 GLOBALNUMBEROFRUNG2 GLOBALNUMBEROFRUNG3 GLOBALREMAINDER VERTICALRUNG
		globalSEGMENTDOTTEDPAIRS globalSEGMENTLISTNUMERICAL
		SINGLELINEROOMLIST
		GlobalControllers GLOBALDLMCONTROLLERS GLOBALDLMDAYLIGHT GLOBALDLMINTERFACES GLOBALDLMNETWORKDEVICES GLOBALDLMOCCCORNERMOUNT GLOBALDLMOCCSENSORS GLOBALDLMPLUGCONTROLLERS GLOBALDLMSPLITTERS GLOBALDLMSWITCHES
		GlobalControllers GLOBALNUMBEROFRUNG1 GLOBALNUMBEROFRUNG2 GLOBALNUMBEROFRUNG3 PORTS USEABLEPORTS
		BBPRODUCT BLOCKNAME GLOBALBLOCKWIDTH CMAX CMAX1 CMAX2 CMAX5 CMAX6 CNTR CNTR1
	       CNTR2 CNTR5 CNTR6 COLUMNCHECK GLOBALDLMCONTROLLERS GLOBALDLMDAYLIGHT
	       GLOBALDLMINTERFACES GLOBALDLMNETWORKDEVICES GLOBALDLMOCCCORNERMOUNT GLOBALDLMOCCSENSORS
	       GLOBALDLMPLUGCONTROLLERS GLOBALDLMSPLITTERS GLOBALDLMSWITCHES INSERTIONPOINT NUMBEROFDLMREMAINDERRUNG
	       NUMBEROFREMAINDERRUNG GLOBALNUMBEROFRUNG1 GLOBALNUMBEROFRUNG2 GLOBALNUMBEROFRUNG3
	       RBdlmremainder RBPRODUCT RBPRODUCT2 RBPRODUCTS RBPRODUCTSPASS1 RBPRODUCTSPASS2 RBPRODUCTSPASS3 RBPRODUCTSPASS4 RBPRODUCTSPASS5 GLOBALREMAINDER
	       GLOBALroomforGSR2-RungBuilder RUNGPRODUCT SPACER VERTICALRUNG XCOORD YCOORD
	       roomsToInsert insertedRooms singlelineroomlist globalSEGMENTSTOINSERT roomLoopCheck builtrooms globalSTANDALONEDOTTEDPAIRS
	       globalSTANDALONENUMERICAL globalcabinetinformationlist
	       GLOBALDLMNETWORKBRIDGES
GLOBALDLMZONECONTROLLERS GLOBALROOMSONSEGMENT
GLOBALDLMPANELS GLOBALDLMNETWORKDEVICES globalSEGMENTDOTTEDPAIRS errorhandler-empty-segment
	       ) (SET X NIL))
  
	(princ)

)











(defun GSR2-RungBuilder (RBroom RBnetworksegment RBnumberofproductsinroom RBcabinetinformationlist RBDLMSEGMENTMANAGERS RBDLMNETWORKBRIDGES RBDLMZONECONTROLLERS RBDLMPANELS RBDLMCONTROLLERS RBDLMPLUGCONTROLLERS
		    RBControllers RBDLMINTERFACES RBDLMOCCSENSORS RBDLMOCCCORNERMOUNT RBDLMDAYLIGHT RBDLMSWITCHES RBDLMSPLITTERS RBREMAINDER
		    RBNUMBEROFRUNG1 RBNUMBEROFRUNG2 RBNUMBEROFRUNG3 RBNUMBEROFRUNGFINAL /
		    blockexists BBPRODUCT BLOCKNAME CMAX CMAX1 CMAX2 CMAX5 CMAX6 CNTR CNTR1 CNTR2 CNTR5 CNTR6 COLUMNCHECK
		    DLMREMAINDERARRAY DLMREMAINDERCOLUMNINDEX DLMREMAINDERCOLUMNLOWERBOUNDARY DLMREMAINDERCOLUMNUPPERBOUNDARY DLMREMAINDERROWINDEX
		    DLMREMAINDERROWLOWERBOUNDARY DLMREMAINDERROWUPPERBOUNDARY NUMBEROFDLMREMAINDERRUNG NUMBEROFREMAINDERRUNG
		    R1ARRAY R1CNTRMAX R1COLUMNINDEX R1COLUMNLOWERBOUNDARY R1COLUMNUPPERBOUNDARY R1ROWINDEX R1ROWLOWERBOUNDARY
		    R1ROWUPPERBOUNDARY R2ARRAY R2CNTRMAX R2COLUMNINDEX R2COLUMNLOWERBOUNDARY R2COLUMNUPPERBOUNDARY R2ROWINDEX R2ROWLOWERBOUNDARY R2ROWUPPERBOUNDARY R3ARRAY
		    R3CNTRMAX R3COLUMNINDEX R3COLUMNLOWERBOUNDARY R3COLUMNUPPERBOUNDARY R3ROWINDEX R3ROWLOWERBOUNDARY R3ROWUPPERBOUNDARY RBdlmremainder RBPRODUCT RBPRODUCT2
		    RBPRODUCTSPASS4 REMAINDERARRAY REMAINDERCOLUMNINDEX REMAINDERCOLUMNLOWERBOUNDARY REMAINDERCOLUMNUPPERBOUNDARY
		    REMAINDERROWINDEX REMAINDERROWLOWERBOUNDARY REMAINDERROWUPPERBOUNDARY RFINALARRAY RFINALCNTRMAX RFINALCOLUMNINDEX RFINALCOLUMNLOWERBOUNDARY RFINALCOLUMNUPPERBOUNDARY
		    RFINALROWINDEX RFINALROWLOWERBOUNDARY RFINALROWUPPERBOUNDARY RUNGPRODUCT SPACER UPPERBOUNDARY
		    XCOORD YCOORD INSERTIONPOINT EXITWHILE

			rFinalcntrmax
		MAXROWREACHED   PLACEMENTROW1MAXNOTREACHED PLACEMENTROW2MAXNOTREACHED PLACEMENTROW3MAXNOTREACHED 
		R1POINT1 R1POINT2 R1XCOORD R1XCOORD2 R1YCOORD R1YCOORD2 R2POINT1 R2POINT2 R2XCOORD R2XCOORD2 R2YCOORD R3POINT1 R3POINT2 R3XCOORD 
		R3XCOORD2 R3YCOORD RUNG1PLACEMENT RUNG2PLACEMENT RUNG3PLACEMENT VERTICALPOINT1 
		VERTICALPOINT2 VXCOORD VYCOORD VYCOORDPREVIOUS XCOORDMAX YCOORDMAX

		RFINALPOINT1 RFINALPOINT2 RFINALXCOORD RFINALXCOORD2 RFINALYCOORD RFINALYCOORD2 RUNGDLMREMAINDERPLACEMENT RUNGFINALPLACEMENT RUNGREMAINDERPLACEMENT
		    LAYER RBPRODUCT1 ATTRIBUTEVALUELIST LINE-LAYER LINE-LINETYPE STARTPOINT ENDPOINT P1 P2 SHIFTER YCOORDBEFORE
		    )

 
;;;arguments
;;;  	GSR:
;;;  	products into RBproducts
;;;	GLOBALroomforGSR2-RungBuilder into room
;;;
;;;Global from
;;;  	GSR2-RiserHeirarchy:
;;;  	GLOBALDLMCONTROLLERS GLOBALDLMDAYLIGHT GLOBALDLMINTERFACES GLOBALDLMNETWORKDEVICES GLOBALDLMOCCCORNERMOUNT GLOBALDLMOCCSENSORS GLOBALDLMPLUGCONTROLLERS GLOBALDLMSPLITTERS GLOBALDLMSWITCHES GLOBALREMAINDER
;;;  	GSR2-PortCalculator :
;;;  	GLOBALNUMBEROFRUNG1 GLOBALNUMBEROFRUNG2 GLOBALNUMBEROFRUNG3
;;;Global to
;;;  	GSR:
;;;  	BLOCKHEIGHT GLOBALBLOCKWIDTH

;;;
;;;  	(setq GLOBALDLMINTERFACES (list "LMIO-101" "LMIO-101" "LMIO-101" "LMIO-201" "LMIO-301"))
;;;  	(setq GLOBALDLMOCCSENSORS (list "LMDC-100" "LMDC-100" "LMDC-100"))
;;;  	(setq GLOBALDLMSWITCHES nil)
;;;  	(setq GLOBALDLMOCCCORNERMOUNT (list "LMDX-100" "LMDX-100" "LMPX-100" "LMPX-100"))
;;;	(setq GLOBALDLMDAYLIGHT (list "LMLS-400" "LMLS-400" "LMLS-500" "LMLS-600" "LMLS-600"))
;;;  	(setq GLOBALDLMSPLITTERS (list "LMRJ-CS8" "LMRJ-S8"))
;;;  	(setq GLOBALREMAINDER (list "LVSW-101" "LVSW-101" "LVSW-101" "LVSW-101" "LVSW-101" "LVSW-101" "LVSW-101" "LVSW-101" "LVSW-101"))
;;;  
;;;  	(setq GLOBALDLMNETWORKDEVICES (list "LMBC-300" "LMCP48"))
;;;  	(setq GLOBALDLMCONTROLLERS (list "LMRC-211" "LMRC-211"))
;;;	(setq GLOBALDLMPLUGCONTROLLERS (list "LMPL-101"))
;;;  	(setq VerticalRung (append GLOBALDLMNETWORKDEVICES GLOBALDLMCONTROLLERS GLOBALDLMPLUGCONTROLLERS))
;;;  	(setq GLOBALroomforGSR2-RungBuilder "Room1")
;;;  	
;;;  
;;;  
;;;	(setq GLOBALNUMBEROFRUNG1 2)
;;;	(setq GLOBALNUMBEROFRUNG2 2)
;;;	(setq GLOBALNUMBEROFRUNG3 1)
  
  	
;;;	here is where the subfunction starts after the values are set
;;;	use the total length of list sent in as the upperBoundary for all arrays, this ensures the arrays are big enough in row and column size to hold the products
	(setq upperBoundary RBnumberofproductsinroom)
  
;;;	create compound lists

	(setq VerticalRung (append RBDLMSEGMENTMANAGERS RBDLMNETWORKBRIDGES RBDLMZONECONTROLLERS RBDLMPANELS RBDLMCONTROLLERS RBDLMPLUGCONTROLLERS))
	(setq listsorter nil)
  	(foreach x VerticalRung
	  	(if
		  	(/= x nil)
			(setq listsorter (cons x listsorter))
		)
	)
  	(setq VerticalRung (reverse listsorter))
  
  	(if
	  	(OR
		(/= RBDLMNETWORKBRIDGES nil)(/= RBDLMZONECONTROLLERS nil)(/= RBDLMPANELS nil)
		)
		(progn
		(setq RBDLMNETWORKDEVICES (append RBDLMNETWORKBRIDGES RBDLMZONECONTROLLERS RBDLMPANELS))
		)
	)
  	(if
	  	(OR
		(/= RBDLMNETWORKBRIDGES nil)(/= RBDLMPANELS nil)
		)
		(progn
		(setq RBDLMMSTPDEVICES (append RBDLMNETWORKBRIDGES RBDLMPANELS))
		)
	)
  
	;;;remove nil lists from compound lists
    	(setq listsorter nil)
  	(foreach x RBDLMNETWORKDEVICES
	  	(if
		  	(/= x nil)
			(setq listsorter (cons x listsorter))
		)
	)
  	(setq RBDLMNETWORKDEVICES (reverse listsorter))
  
    	(setq listsorter nil)
  	(foreach x RBDLMMSTPDEVICES
	  	(if
		  	(/= x nil)
			(setq listsorter (cons x listsorter))
		)
	)
  	(setq RBDLMMSTPDEVICES (reverse listsorter))
  
;;;	set the dimensions for the DLM rung3 array
;;;	create the rung3 array
  	(setq r3RowLowerBoundary 0)
  	(setq r3RowUpperBoundary upperBoundary)
  	(setq r3ColumnLowerBoundary 0)
  	(setq r3ColumnUpperBoundary upperBoundary)
	(setq r3Array (vlax-make-safearray vlax-vbString (cons r3RowLowerBoundary r3RowUpperBoundary)(cons r3ColumnLowerBoundary r3ColumnUpperBoundary)))
  
  	(setq r2RowLowerBoundary 0)
  	(setq r2RowUpperBoundary upperBoundary)
  	(setq r2ColumnLowerBoundary 0)
  	(setq r2ColumnUpperBoundary upperBoundary)
	(setq r2Array (vlax-make-safearray vlax-vbString (cons r2RowLowerBoundary r2RowUpperBoundary)(cons r2ColumnLowerBoundary r2ColumnUpperBoundary)))
  
  	(setq r1RowLowerBoundary 0)
  	(setq r1RowUpperBoundary upperBoundary)
  	(setq r1ColumnLowerBoundary 0)
  	(setq r1ColumnUpperBoundary upperBoundary)
	(setq r1Array (vlax-make-safearray vlax-vbString (cons r1RowLowerBoundary r1RowUpperBoundary)(cons r1ColumnLowerBoundary r1ColumnUpperBoundary)))
  
;;;	create array for the final available slot for DLM products
  	(setq rFinalRowLowerBoundary 0)
  	(setq rFinalRowUpperBoundary upperBoundary)
  	(setq rFinalColumnLowerBoundary 0)
  	(setq rFinalColumnUpperBoundary upperBoundary)
	(setq rFinalArray (vlax-make-safearray vlax-vbString (cons rFinalRowLowerBoundary rFinalRowUpperBoundary)(cons rFinalColumnLowerBoundary rFinalColumnUpperBoundary)))

;;;	create array for remaining DLM products that did not fit in the available rungs.
;;;  	this will be any DLM products that only have one LMRJ port
  	(setq DLMREMAINDERRowLowerBoundary 0)
  	(setq DLMREMAINDERRowUpperBoundary upperBoundary)
  	(setq DLMREMAINDERColumnLowerBoundary 0)
  	(setq DLMREMAINDERColumnUpperBoundary upperBoundary)
	(setq DLMREMAINDERArray (vlax-make-safearray vlax-vbString (cons DLMREMAINDERRowLowerBoundary DLMREMAINDERRowUpperBoundary)(cons DLMREMAINDERColumnLowerBoundary DLMREMAINDERColumnUpperBoundary)))
  
;;;	create array for remaining non DLM products
  	(setq REMAINDERRowLowerBoundary 0)
  	(setq REMAINDERRowUpperBoundary upperBoundary)
  	(setq REMAINDERColumnLowerBoundary 0)
  	(setq REMAINDERColumnUpperBoundary upperBoundary)
	(setq REMAINDERArray (vlax-make-safearray vlax-vbString (cons REMAINDERRowLowerBoundary REMAINDERRowUpperBoundary)(cons REMAINDERColumnLowerBoundary REMAINDERColumnUpperBoundary)))

;;;  	set all the column indexs to 0
	(setq r1ColumnIndex 0)
	(setq r2ColumnIndex 0)
	(setq r3ColumnIndex 0)
	(setq rFinalColumnIndex 0)
  	(setq DLMREMAINDERColumnIndex 0)
	(setq REMAINDERColumnIndex 0)
  
;;;	the counter maxs are used later for calculating the entire block width
  	(setq r1cntrmax 0)
	(setq r2cntrmax 0)
  	(setq r3cntrmax 0)
  	(setq rfinalcntrmax 0)

  	
	(setq RBproductsPass1 (append RBDLMINTERFACES RBDLMOCCSENSORS))
  	;;;sort out nil lists
	(setq listsorter nil)
  	(foreach x RBproductsPass1
	  	(if
		  	(/= x nil)
			(setq listsorter (cons x listsorter))
		)
	)
  	(setq RBproductsPass1 (reverse listsorter))


  
;;;  	set counters and indexs
  	
  	(cond
	  	((/= RBproductsPass1 nil)
  		(setq cntr 0)
		(setq cmax (length RBproductsPass1))
		)
		
		((= RBproductsPass1 nil)
  		(setq cntr 1)
		(setq cmax 0)
		)
	)
  
  	(setq r1RowIndex 0)
  	(setq r2RowIndex 0)
  	(setq r3RowIndex 0)

;;;  	this section uses the number of avaiable rungs to place items in the matrices
;;;  	once we reach the maximum number of rung on each type, we reset the row to 0 and move to the next column
;;;  	there is a rung placement variable that gets set to 1 as soon as we place an item in a matrix
;;;  	this variable allows for calculating the block width when the column is still 0, but we have placed items in the matrix
;;;  	rung1placement rung2placement rung3placement are used to calculate block width when we have not incremented the column
  
;;;  	first pass through the rungs with the products
;;;  	this only includes the 3rd and 1st rungs
;;;  	the repeat places one product per row
;;;  	then the row increments
;;;  	when we reach the last row, the column will increment and the rows will reset
;;;  	all these devices have more than one LMRJ45 port
;;;  	so the row is still open on the end, we can continue to add devices to this row

	(setq rung1placement 0)
  	(setq rung2placement 0)
  	(setq rung3placement 0)
  	(setq RUNGDLMREMAINDERPLACEMENT 0)
	(setq RUNGFINALPLACEMENT 0)
  	(setq RUNGREMAINDERPLACEMENT 0)
  
  	(while	(< cntr cmax)

	  	(cond
			((< r2RowIndex RBNUMBEROFRUNG2)
			(repeat RBNUMBEROFRUNG2
			  	(setq RBproduct (nth cntr RBproductsPass1))
			  	(setq cntr (+ 1 cntr))
			  	(if
				(/= RBProduct nil)
					(progn
					(vlax-safearray-put-element r2Array r2RowIndex r2ColumnIndex RBproduct)
					(setq rung2placement 1)
					(setq r2RowIndex (+ 1 r2RowIndex))
					;calculate max only when we actually store an item
					(if
					  	(> r2RowIndex r2cntrmax)
			    			(setq r2cntrmax r2RowIndex)
					)
					
					)
				)

			  	;;;if we've reached the total number of rung 2 then reset to first
			  	(if
				(= r2RowIndex RBNUMBEROFRUNG2)
					(progn
					(setq r2RowIndex 0)
					(setq r2ColumnIndex (+ 1 r2ColumnIndex))
					)
				)

			)
			)
		)


	  	(cond
			((< r3RowIndex RBNUMBEROFRUNG3)
			(repeat RBNUMBEROFRUNG3
			  	(setq RBproduct (nth cntr RBproductsPass1))
			  	(setq cntr (+ 1 cntr))
			  	(if
				(/= RBProduct nil)
					(progn
					(vlax-safearray-put-element r3Array r3RowIndex r3ColumnIndex RBproduct)
					(setq rung3placement 1)
					(setq r3RowIndex (+ 1 r3RowIndex))
					;calculate max only when we actually store an item
				  	(if
					  	(> r3RowIndex r3cntrmax)
				    		(setq r3cntrmax r3RowIndex)
					)
					
					
					)
				)

			  	(if
				(= r3RowIndex RBNUMBEROFRUNG3)
					(progn
					(setq r3RowIndex 0)
					(setq r3ColumnIndex (+ 1 r3ColumnIndex))
					)
				)

			)
			)
		)
;;;	  	old way of moving passes
	 	(if
			(and(= RBNUMBEROFRUNG3 0)(= RBNUMBEROFRUNG2 0))
		  	(progn
;;;			(setq RBdlmremainder (append RBproductsPass1))
			(setq cntr cmax)
			)
		)
	  
	)


	(setq RBproductsPass2 (append RBDLMSWITCHES))
	(setq listsorter nil)
  	;;;sort out nil lists
  	(foreach x RBproductsPass2
	  	(if
		  	(/= x nil)
			(setq listsorter (cons x listsorter))
		)
	)
  	(setq RBproductsPass2 (reverse listsorter))
  
;;;  	set counters and indexs  
    	(cond
	  	((/= RBproductsPass2 nil)
  		(setq cntr 0)
		(setq cmax (length RBproductsPass2))
		)
		
		((= RBproductsPass2 nil)
  		(setq cntr 1)
		(setq cmax 0)
		)
	)
  
;;;  	second pass through the rungs with the products
;;;  	this only includes the 3rd, 2nd and 1st rungs
;;;  	the repeat places one product per row
;;;  	then the row increments
;;;  	when we reach the last row, the column will increment and the rows will reset
;;;  	all these devices have more than one LMRJ45 port
;;;  	so the row is still open on the end, we can continue to add devices to this row
	(while	(< cntr cmax)

	  	(cond
		  	((< r1RowIndex RBNUMBEROFRUNG1)
			(repeat RBNUMBEROFRUNG1
				(setq RBproduct (nth cntr RBproductsPass2))
			  	(if
				(/= RBProduct nil)
					(progn
					(vlax-safearray-put-element r1Array r1RowIndex r1ColumnIndex RBproduct)
					(setq rung1placement 1)
					(setq r1RowIndex (+ 1 r1RowIndex))
					;calculate max only when we actually store an item
				  	(if
					  	(> r1RowIndex r1cntrmax)
				    		(setq r1cntrmax r1RowIndex)
					)
					
			  		
					)
				)
				(setq cntr (+ 1 cntr))

			  	(if
				(= r1RowIndex RBNUMBEROFRUNG1)
					(progn
					(setq r1RowIndex 0)
					(setq r1ColumnIndex (+ 1 r1ColumnIndex))
					)
				)

			)
			)
		)
	  
	  	(cond
			((< r2RowIndex RBNUMBEROFRUNG2)
			(repeat RBNUMBEROFRUNG2
			  	(setq RBproduct (nth cntr RBproductsPass2))
			  	(setq cntr (+ 1 cntr))
			  	(if
				(/= RBProduct nil)
					(progn
					(vlax-safearray-put-element r2Array r2RowIndex r2ColumnIndex RBproduct)
					(setq rung2placement 1)
					(setq r2RowIndex (+ 1 r2RowIndex))
					;calculate max only when we actually store an item
					(if
					  	(> r2RowIndex r2cntrmax)
				    		(setq r2cntrmax r2RowIndex)
					)
					
					
					)
				)

			  	(if
				(= r2RowIndex RBNUMBEROFRUNG2)
					(progn
					(setq r2RowIndex 0)
					(setq r2ColumnIndex (+ 1 r2ColumnIndex))
					)
				)

			)
			)
		)
	  
	  	(cond
			((< r3RowIndex RBNUMBEROFRUNG3)
			(repeat RBNUMBEROFRUNG3
			  	(setq RBproduct (nth cntr RBproductsPass2))
			  	(if
				(/= RBProduct nil)
					(progn
			  		(vlax-safearray-put-element r3Array r3RowIndex r3ColumnIndex RBproduct)
					(setq rung3placement 1)
					(setq r3RowIndex (+ 1 r3RowIndex))
					;calculate max only when we actually store an item
				 	(if
					  	(> r3RowIndex r3cntrmax)
				    		(setq r3cntrmax r3RowIndex)
					)
					
					
					)
				)
		  		(setq cntr (+ 1 cntr))

			  	(if
				(= r3RowIndex RBNUMBEROFRUNG3)
					(progn
					(setq r3RowIndex 0)
					(setq r3ColumnIndex (+ 1 r3ColumnIndex))
					)
				)

			)
			)
		)

;;;	  	old way of moving passes
	  	(if
			(and(= RBNUMBEROFRUNG3 0)(= RBNUMBEROFRUNG2 0)(= RBNUMBEROFRUNG1 0))
		  	(progn
;;;			(setq RBdlmremainder (append RBproductsPass2))
			(setq cntr cmax)
			)
		)
	  

	)
  
	(setq RBproductsPass3 (append RBDLMOCCCORNERMOUNT RBDLMDAYLIGHT))
 	(setq listsorter nil)
  	;;;sort out nil lists
  	(foreach x RBproductsPass3
	  	(if
		  	(/= x nil)
			(setq listsorter (cons x listsorter))
		)
	)
  	(setq RBproductsPass3 (reverse listsorter))
  
	(setq cntr 0)
  
    	(cond
	  	((/= RBproductsPass3 nil)
		(setq cmax (length RBproductsPass3))
		(setq cntr2 0)
  		(setq cmax2 (length RBDLMSPLITTERS))
		)
		
		((= RBproductsPass3 nil)
		(setq cmax 0)
		)
	)
  
  
	(setq rFinalRowIndex 0)
  
;;;  	third pass through the rungs with the products
;;;  	this only includes the 3rd, 2nd and 1st rungs
;;;  	the repeat places one product per rung
;;;  	then the row increments
;;;  	when we reach the last row, the column will increment and the rows will reset
;;;  	all these devices have only one LMRJ45 port
;;;  	so the row is closed on the end, we can not continue to add devices to this row
;;;  	trying new structure increment column every time, reset row like normal
  	(setq placementrow1maxnotreached 0)
  	(setq placementrow2maxnotreached 0)
  	(setq placementrow3maxnotreached 0)
  	(setq maxrowreached 0)
  	(cond
	  	((/= RBproductsPass3 nil)
  			(repeat RBNUMBEROFRUNG1
				(setq RBproduct (nth cntr RBproductsPass3))
			  	(if
				(/= RBProduct nil)
					(progn
					(vlax-safearray-put-element r1Array r1RowIndex r1ColumnIndex RBproduct)
					(setq rung1placement 1)
					(setq placementrow1maxnotreached 1)
					(setq r1RowIndex (+ 1 r1RowIndex))
					;calculate max only when we actually store an item
				  	(if
					  	(> r1RowIndex r1cntrmax)
				    		(setq r1cntrmax r1RowIndex)
					)
					
			  		
					)
				)
				(setq cntr (+ 1 cntr))

			  	(if
				(= r1RowIndex RBNUMBEROFRUNG1)
					(progn
					(setq maxrowreached 1)
					(setq r1RowIndex 0)
					(setq r1ColumnIndex (+ 1 r1ColumnIndex))
					)
				)

			)

			(repeat RBNUMBEROFRUNG2
			  	(setq RBproduct (nth cntr RBproductsPass3))
			  	(setq cntr (+ 1 cntr))
			  	(if
				(/= RBProduct nil)
					(progn
					(vlax-safearray-put-element r2Array r2RowIndex r2ColumnIndex RBproduct)
					(setq rung2placement 1)
					(setq placementrow2maxnotreached 1)
					(setq r2RowIndex (+ 1 r2RowIndex))
					;calculate max only when we actually store an item
				  	(if
					  	(> r2RowIndex r2cntrmax)
				    		(setq r2cntrmax r2RowIndex)
					)
					
					)
				)

			  	(if
				(= r2RowIndex RBNUMBEROFRUNG2)
					(progn
					(setq maxrowreached 1)
					(setq r2RowIndex 0)
					(setq r2ColumnIndex (+ 1 r2ColumnIndex))
					)
				)
			)
  
  			(repeat RBNUMBEROFRUNG3
			  	(setq RBproduct (nth cntr RBproductsPass3))
			  	(if
				(/= RBProduct nil)
					(progn
			  		(vlax-safearray-put-element r3Array r3RowIndex r3ColumnIndex RBproduct)
					(setq rung3placement 1)
					(setq placementrow3maxnotreached 1)
					(setq r3RowIndex (+ 1 r3RowIndex))
					;calculate max only when we actually store an item
				  	(if	(> r3RowIndex r3cntrmax)
				    		(setq r3cntrmax r3RowIndex)
					)
					)
				)
		  		(setq cntr (+ 1 cntr))

			  	(if
				(= r3RowIndex RBNUMBEROFRUNG3)
					(progn
					(setq maxrowreached 1)
					(setq r3RowIndex 0)
					(setq r3ColumnIndex (+ 1 r3ColumnIndex))
					)
				)
			)




		 
		 )
	)
  

;;;end of passes
;;;move them to RBdlmremainder if there are no avaiabled rungs, ie no controllers
	(if
	(and(= RBNUMBEROFRUNG3 0)(= RBNUMBEROFRUNG2 0)(= RBNUMBEROFRUNG1 0)(= RBNUMBEROFRUNGFINAL 0))
	(progn
	(setq RBdlmremainder (append RBproductsPass1 RBproductsPass2 RBproductsPass3))
	(setq cntr cmax)
	)
	)

  	(setq RBBLOCKWIDTH 0)
;;;  	these 3 if statements, increment the rungcolumn indexs to 1, if we have placed any items into the matrices, but we haven't moved to the next column yet
;;;  	(cond
;;;	  	((/= RBproductsPass3 nil)
;;;		(setq r1ColumnIndex (+ r1ColumnIndex 1))
;;;		(setq r2ColumnIndex (+ r2ColumnIndex 1))
;;;		(setq r3ColumnIndex (+ r3ColumnIndex 1))
;;;		)
;;;	)
		 
  	(if	(and (= rung1placement 1)(= r1ColumnIndex 0))
	  	(setq r1ColumnIndex 1)
	)

	(if	(and (= rung2placement 1)(= r2ColumnIndex 0))
		(setq r2ColumnIndex 1)
	)
	(if	(and (= rung3placement 1)(= r3ColumnIndex 0))
		(setq r3ColumnIndex 1)
	)

;;;  	these 3 if statements calculate the block width from the column index's used to build the matrices
	(if	(> r1cntrmax RBBLOCKWIDTH)
		(setq RBBLOCKWIDTH r1cntrmax)
	)
  	(if	(> r2cntrmax RBBLOCKWIDTH)
		(setq RBBLOCKWIDTH r2cntrmax)
	)
	(if	(> r3cntrmax RBBLOCKWIDTH)
		(setq RBBLOCKWIDTH r3cntrmax)
	)
;;;  	if code placed a device with one LMRJ 45 port, from the 3rd pass, on any rung
;;;  	and we did not reach the max row value for any rung, then make block width 1 wider because the code did not account for it during the 3rd pass
  	(if	(or (= placementrow1maxnotreached 1)(= placementrow2maxnotreached 1)(= placementrow3maxnotreached 1))
		(progn

	  	(setq RBBLOCKWIDTH (+ RBBLOCKWIDTH 1))
	
		)
	)

;;;  	next step for the third pass through the rungs with the products
;;;  	this only includes the final rung
;;;  	the while loop inside the conditional places a splitter then a product from the thrid pass list
;;;  	then the column increments
;;;  	all these devices have only one LMRJ45 port so they are paired with the splitter
;;;  	if no splitters are left, then the row is closed at the end
;;;	the devices remaining get placed into the rung builder final list
(setq rFinalColumnIndex 0)
(setq rFinalRowIndex 0)


;;; place everything from pass 1 and 2 on final rung when there is only the final rung avaiable, ie LMRC-111 and LMRC-112
(if
(and(= RBNUMBEROFRUNG3 0)(= RBNUMBEROFRUNG2 0)(= RBNUMBEROFRUNG1 0)(= RBNUMBEROFRUNGFINAL 1))
(progn
  	(if
	  	(/= RBproductsPass1 nil)
	  	(progn
	  	(setq cntr2 0)
	  	(setq cmax2 (length RBproductsPass1)) 
	  	(while
		  	(< cntr2 cmax2)
		  	(setq RBproduct (nth cntr2 RBproductsPass1))
			(vlax-safearray-put-element rFinalArray rFinalRowIndex rFinalColumnIndex RBproduct)
		  	(setq rungfinalplacement 1)
			(setq rFinalColumnIndex (+ 1 rFinalColumnIndex))
			(setq cntr2 (+ 1 cntr2))
		)
		)
	)

  	(if
	  	(/= RBproductsPass2 nil)
	  	(progn
	  	(setq cntr2 0)
	  	(setq cmax2 (length RBproductsPass2)) 
	  	(while
		  	(< cntr2 cmax2)
		  	(setq RBproduct (nth cntr2 RBproductsPass2))
			(vlax-safearray-put-element rFinalArray rFinalRowIndex rFinalColumnIndex RBproduct)
		  	(setq rungfinalplacement 1)
			(setq rFinalColumnIndex (+ 1 rFinalColumnIndex))
			(setq cntr2 (+ 1 cntr2))
		)
		)
	)
  
)
)

  
;;;	if cntr is not 0, then we placed items during the previous pass
;;;	it's important not to reset cntr at this point, this ensures there is no duplication of the 3rd pass
;;;	left off finishing splitter placement on last rung, need to place all splitters and distribute all lmrj-45 1 port left over
    	(if
	  	(AND (= RBNUMBEROFRUNGFINAL 1)(/= RBproductsPass3 nil))
		(progn
  		(setq cntr2 0)
		
		(setq RBproduct (nth cntr RBproductsPass3))
		 
		;;; place one corner mount on this rung when no splitters
		(if
		  	(AND (= RBDLMSPLITTERS nil)(/= RBproduct nil))
		  	(progn 
			(vlax-safearray-put-element rFinalArray rFinalRowIndex rFinalColumnIndex RBproduct)
			(setq rungfinalplacement 1)
			(setq rFinalColumnIndex (+ 1 rFinalColumnIndex))
			(setq cntr (+ 1 cntr))
			
			)
		)
		 
		;;; set values when there are splitters
		(if
		  	(/= RBDLMSPLITTERS nil)
		  	(progn
			(setq cmax (length RBDLMSPLITTERS))
			(setq RBproduct2 (nth cntr2 RBDLMSPLITTERS))
			)
		)
		 
		(setq exitwhile 0)
	  	;;;set the exitwhile variable to 1 when there is nothing left in the 3rd pass or splitters
	  	(if
		  	(AND (= RBproduct2 nil)(= RBproduct nil))
		  	(progn
		  	(setq exitwhile 1)
			)
		)
		
	  	;;;set the exitwhile variable to 1 when there are no splitters
	  	(if
		  	(AND (= RBDLMSPLITTERS nil))
		  	(progn
		  	(setq exitwhile 1)
			)
		)

		(while
		  	(= exitwhile 0)

			;;;set the exitwhile variable to 1 when there is nothing left in the 3rd pass or splitters
			(if
				(AND (= RBproduct2 nil)(= RBproduct nil))
				(progn
				(setq exitwhile 1)
				)
			)


				(setq RBproduct2 (nth cntr2 RBDLMSPLITTERS))

				(cond
					;;;when there is a splitter, place it on the final rung and grab the next item left over from pass 3
					((/= RBproduct2 nil)
					
					(vlax-safearray-put-element rFinalArray  rFinalRowIndex rFinalColumnIndex  RBproduct2)
					(setq rungfinalplacement 1)
					(setq rFinalColumnIndex (+ 1 rFinalColumnIndex))
					(setq cntr2 (+ 1 cntr2))
					(setq RBproduct (nth cntr RBproductsPass3))

					;;;when there is a item in rbproduct from pass 3
					;;;nested conditional
						(cond	((/= RBproduct nil)
							(vlax-safearray-put-element rFinalArray rFinalRowIndex rFinalColumnIndex RBproduct)
							(setq rungfinalplacement 1)
							(setq rFinalColumnIndex (+ 1 rFinalColumnIndex))
							(setq cntr (+ 1 cntr))
							)		 
						)
					)

					;;;when no more splitters remain, place everything in RBdlmremainder for next pass
					((= RBproduct2 nil)
					 	;;;OLD WAY CHECKED PLACEMENT FIRST
;;;						(if	(= rungfinalplacement 1)
;;;							(progn
							(setq RBproduct (nth cntr RBproductsPass3))
							;;;when there is a item in rbproduct from pass 3
							;;;nested conditional
								(cond	((/= RBproduct nil)
									(setq RBdlmremainder (cons RBproduct RBdlmremainder))
									(setq cntr (+ 1 cntr))
									)		 
								)

;;;							)
;;;						)


					 
						(while	(< cntr cmax)
							(setq RBproduct (nth cntr RBproductsPass3))
							(setq RBdlmremainder (cons RBproduct RBdlmremainder))
							(setq cntr (+ 1 cntr))
						)
					)						
						
				)
				
			

		  
				
		)								;--end while

		

		)								;-end progn
	)									;end if

  	;;;for drawing connection to rFinal from controller
	(setq rFinalcntrmax rFinalColumnIndex)
		
		(if
		(AND (= RBNUMBEROFRUNGFINAL 0)(/= RBproductsPass3 nil))
		(progn
		(setq cmax (length RBproductsPass3))
		
	  	(while	(< cntr cmax)
			(setq RBproduct (nth cntr RBproductsPass3))
			(setq RBdlmremainder (cons RBproduct RBdlmremainder))
			(setq cntr (+ 1 cntr))
		)
		)
		)
		
		(if
		(AND (= RBNUMBEROFRUNGFINAL 1)(/= RBproductsPass3 nil))
		(progn
		(setq cmax (length RBproductsPass3))
		
	  	(while	(< cntr cmax)
			(setq RBproduct (nth cntr RBproductsPass3))
			(setq RBdlmremainder (cons RBproduct RBdlmremainder))
			(setq cntr (+ 1 cntr))
		)

	
		)
		)
  	

	  
	;;;  	fourth pass with the remaining DLM products
	;;;  	this is a collection of any device that could not fit in the available rungs
	;;;  	the while loop places one product then increments the row
	;;;  	the block builder will take care of the spacing later

	  	(setq RBdlmremainder (reverse RBdlmremainder))
	  	(setq RBproductsPass4 (append RBdlmremainder))
  
	  	;;;sort out nil lists
	  	(setq listsorter nil)
	  	(foreach x RBproductsPass4
		  	(if
			  	(/= x nil)
				(setq listsorter (cons x listsorter))
			)
		)
	  	(setq RBproductsPass4 (reverse listsorter))
  
	      	(cond
		  	((/= RBproductsPass4 nil)
	  		(setq cntr 0)
			(setq cmax (length RBproductsPass4))
			(setq NumberofDLMREMAINDERRung 1)
			)
			
			((= RBproductsPass4 nil)
	  		(setq cntr 1)
			(setq cmax 0)
			)
		)
		  

	  	(setq DLMREMAINDERRowIndex 0)
	  
		(while	(< cntr cmax)
			(setq RBproduct (nth cntr RBproductsPass4))
		  	(setq rungdlmremainderplacement 1)
			(vlax-safearray-put-element DLMREMAINDERArray DLMREMAINDERRowIndex DLMREMAINDERColumnIndex RBproduct)
		  	(setq DLMREMAINDERColumnIndex (+ 1 DLMREMAINDERColumnIndex))
			(setq cntr (+ 1 cntr))
		)
  

  
	;;;  	fifth pass with the remaining non DLM products
	;;;  	this is a collection of any device that is not DLM
	;;;  	the while loop places one product then increments the row
	;;;  	the block builder will take care of the spacing later

  	;;;	the RBREMAINDER array holds any blocks are not defined in the RiserHierarchy
  	;;;	these would include analog, LCAP and any new DLM devices that are not created in the Riser Template yet
  	(setq RBproductsPass5 (append RBREMAINDER))
  	;;;sort out nil lists
  	(setq listsorter nil)
  	(foreach x RBproductsPass5
	  	(if
		  	(/= x nil)
			(setq listsorter (cons x listsorter))
		)
	)
  	(setq RBproductsPass5 (reverse listsorter))
  
	      	(cond
		  	((/= RBproductsPass5 nil)
	  		(setq cntr 0)
			(setq cmax (length RBproductsPass5))
			(setq NumberofREMAINDERRung 1)
			)
			
			((= RBproductsPass5 nil)
	  		(setq cntr 1)
			(setq cmax 0)
			)
		)
	  
	  	(setq REMAINDERColumnIndex 0)
	  	(setq REMAINDERRowIndex 0)  
	  	(while	(< cntr cmax)
			(setq RBproduct (nth cntr RBproductsPass5))
		  	(setq rungremainderplacement 1)
			(vlax-safearray-put-element REMAINDERArray REMAINDERRowIndex REMAINDERColumnIndex RBproduct)
			(setq REMAINDERColumnIndex (+ 1 REMAINDERColumnIndex))	  
			(setq cntr (+ 1 cntr))
		)


;;;	this is the next subfunction to build the room 
;;;  	http://autocad.wikia.com/wiki/Entmaking_block_definitions
  

  	(setq xcoord 0)
	(setq ycoord 0)																		;set y coord to 0
  	(setq r1xcoord 0)
  	(setq r2xcoord 0)
  	(setq r3xcoord 0)
  	(setq insertionPoint (list xcoord ycoord))
  
  	(setq listsorter nil)
  	(foreach x RBDLMSEGMENTMANAGERS
	  	(if
		  	(/= x nil)
			(setq listsorter (cons x listsorter))
		)
	)
  	(setq RBDLMSEGMENTMANAGERS (reverse listsorter))

  	(cond
	  	;;; when room contains segment managers
	  	;;; create unique roomname
	  	;;; move xcoord to the left to shift text out of the way of the cables
	  	((/= RBDLMSEGMENTMANAGERS nil)
		(setq blockName (strcat "Z-Riser-Segment-" rbnetworksegment "-Segment-Manager"))									;create string of segmentname of segment manger block

		;;; BLOCK Header definition starts here:
		(entmake (list (cons 0 "BLOCK")(cons 2 blockName)(cons 70 2)(cons 10 insertionPoint)))								;begin block definition
	  	(setq xcoord -5.0)																;shifts xcoord out of way of cables
		(setq ycoord 0.6)
		(setq insertionPoint (list xcoord ycoord))
	  
		;;;http://forums.augi.com/showthread.php?28641-Create-MTEXT-with-Lisp
	    	(entmake
	    	(list
	      	(cons 0 "MTEXT")         		;; Entity Name
	      	(cons 100 "AcDbEntity")  		;; Subclass Marker
	      	(cons 410 "Model")       		;; Space
	      	(cons 8 "Riser")         		;; Layer
	      	(cons 100 "AcDbMText")   		;; Subclass Marker
	      	(cons 10 insertionPoint) 		;; Insertion Point
	      	(cons 40 0.2)            		;; Text Height
	      	(cons 71 5)              		;; Attachment Point (middle-center)
	      	(cons 1 RBroom)    			;; Text Content
	      	(cons 7 "Arial")			;; text style
	      	)
	   	)
	 
		(setq ycoord (+ ycoord -0.4))
		(setq xcoord 0.0)																;shift xcoord back to 0
		)

		;;;when room contains no segment managers
		((= RBDLMSEGMENTMANAGERS nil)
		 
	  	(setq blockName (strcat "Z-Riser-" rbroom))											;create string of roomname
		;;; BLOCK Header definition starts here:
		(entmake (list (cons 0 "BLOCK")(cons 2 blockName)(cons 70 2)(cons 10 insertionPoint)))								;begin block definition
		 
	  	(setq xcoord 0)
		(setq ycoord 0.6)
		(setq insertionPoint (list xcoord ycoord))
	  
		;;;http://forums.augi.com/showthread.php?28641-Create-MTEXT-with-Lisp
	    	(entmake
	    	(list
	      	(cons 0 "MTEXT")         		;; Entity Name
	      	(cons 100 "AcDbEntity")  		;; Subclass Marker
	      	(cons 410 "Model")       		;; Space
	      	(cons 8 "Riser")         		;; Layer
	      	(cons 100 "AcDbMText")   		;; Subclass Marker
	      	(cons 10 insertionPoint) 		;; Insertion Point
	      	(cons 40 0.2)            		;; Text Height
	      	(cons 71 5)              		;; Attachment Point (middle-center)
	      	(cons 1 rbroom)    			;; Text Content
	      	(cons 7 "Arial")			;; text style
	      	)
	   	)
	 
		(setq ycoord (+ ycoord -0.4))
		)
	)
  
  								
	(progn

	
	;;;  	reset indexs and counters
	  
		(setq cmax (length VerticalRung))
		(setq cntr 0)
	  	(setq r1ColumnIndex 0)
		(setq r2ColumnIndex 0)
		(setq r3ColumnIndex 0)
		(setq rFinalColumnIndex 0)
		(setq REMAINDERColumnIndex 0)
	  	(setq r1RowIndex 0)
	  	(setq r2RowIndex 0)
	  	(setq r3RowIndex 0)
		(setq rFinalRowIndex 0)
		(setq REMAINDERRowIndex 0)
	  	(if	(= RBDLMMSTPDEVICES nil)													;drop y coordinate to account for no network block
		  	(setq ycoord (+ ycoord -1.33))
		)
	  

	;used for vertical connection between RBControllers
	(setq vycoordprevious 0)


	;set maxs to 0
	(setq xcoordmax 0)
	(setq ycoordmax 0)
	
	(setq xcoordspacer 3)
	(setq direction 1)
	(setq shifter (* direction xcoordspacer))

	(setq maxblockxcoord 15)
	
	;;;  	while loop for the entire room
	;;;  	if statements to determine which product on the vertical rung
	;;;  	inside the if statements, there are loops to build rungs depending on which type of room controller
		(while	(< cntr cmax)															;while loop -- loop through the network devices and RBControllers

		  
			(setq BBproduct(nth cntr VerticalRung))												;pull first product off list into BBproduct


		  
		  	;;; insert cabinet
		  	;;; move rung down after
		  	(if
				(OR (= BBproduct "LMNC"))
			  	(progn
				(setq insertionPoint (list xcoord ycoord))
				(setq layer "Riser")
				(GSR2-entmod-blockinsert-attributes RBcabinetinformationlist insertionPoint layer BBproduct)  

				;used for vertical connection between RBControllers
				(setq vycoordprevious ycoord)
				(setq xcoord 0)			
				(setq ycoord (+ ycoord -1.33))				  
				
				
				)
			)


		  
		  	;;; insert segment managers
		  	;;; move rung down after
		  	(if
				(OR (= BBproduct "LMSM-3E")(= BBproduct "LMSM-6E")(= BBproduct "NB-ROUTER"))
			  	(progn
				(setq insertionPoint (list xcoord ycoord))
				  
				(setq layer "Riser")
				(setq attributevaluelist nil)
				;;;call GSR2-entmod-blockinsert-attributes to insert the block
				(GSR2-entmod-blockinsert-attributes attributevaluelist insertionPoint layer BBproduct)
				
				;used for vertical connection between RBControllers
				(setq vycoordprevious ycoord)
				(setq xcoord 0)
				(setq ycoord (+ ycoord -1.33))
				
				)
			)

		  

		  
		  
		  	;;; insert primary item, the network bridge, always as first vertical position
		  	;;; move rung down after
		  	(if
				(OR (= BBproduct "LMBC-300"))
			  	(progn
				(setq insertionPoint (list xcoord ycoord))
				  
				(setq layer "Riser")
				(setq attributevaluelist nil)
				;;;call GSR2-entmod-blockinsert-attributes to insert the block
				(GSR2-entmod-blockinsert-attributes attributevaluelist insertionPoint layer BBproduct)
				
				;used for vertical connection between RBControllers
				(setq vycoordprevious ycoord)
				;there are no rungs for LMBC-300's, so drop ycoord now
				(setq xcoord 0)
				(setq ycoord (+ ycoord -1.33))
				
				)
			)



		  	;;; insert secondary items, the panels
		  	;;; they will be below the bridge if there is one
		  	;;; move rung down after
			(if
				(OR (= BBproduct "LMCP48")(= BBproduct "LMCP24")(= BBproduct "LMCP12")(= BBproduct "LMCP8")(= BBproduct "LMZC-301"))
			  	(progn
				(setq insertionPoint (list xcoord ycoord))
				  
				(setq layer "Riser")
				(setq attributevaluelist nil)
				;;;call GSR2-entmod-blockinsert-attributes to insert the block
				(GSR2-entmod-blockinsert-attributes attributevaluelist insertionPoint layer BBproduct)
				
				;used for vertical connection between RBControllers and rungs
				(setq vycoordprevious ycoord)
				;there are no rungs for panels, so drop ycoord later
				(setq xcoord 0)
				(setq ycoord (+ ycoord -1.33))				
				)
			)


		  	;;; insert RBControllers of any type
		  	;;; they will be below the bridge if there is one
		  	;;; move rung down after
			(if
				(OR (= BBproduct "LMRC-222")(= BBproduct "LMRC-221")(= BBproduct "LMRC-213-347v")(= BBproduct "LMRC-212-347v")(= BBproduct "LMRC-211-347v")(= BBproduct "LMRC-213")(= BBproduct "LMRC-212")(= BBproduct "LMRC-211")(= BBproduct "LMPL-201")(= BBproduct "LMPL-101")(= BBproduct "LMRC-102")(= BBproduct "LMRC-101")(= BBproduct "LMRC-112-M")(= BBproduct "LMRC-111-M")(= BBproduct "LMRC-112")(= BBproduct "LMRC-111"))
			  	(progn
				(setq insertionPoint (list xcoord ycoord))
				  
				(setq layer "Riser")
				(setq attributevaluelist nil)
				;;;call GSR2-entmod-blockinsert-attributes to insert the block
				(GSR2-entmod-blockinsert-attributes attributevaluelist insertionPoint layer BBproduct)
								
				;used for vertical connection between RBControllers and rungs
				(setq vycoordprevious ycoord)
				;there are no rungs for controllers, so drop ycoord later
				)
			)



		  	;;; insert the connecting items
		  	;;; they will be below the primary and secondary items
		  	;;; move rung down after
		  	(if
				(OR (= BBproduct "LMZC-301")(= BBproduct "LMRC-222")(= BBproduct "LMRC-221")(= BBproduct "LMRC-213-347v")(= BBproduct "LMRC-212-347v")(= BBproduct "LMRC-211-347v")(= BBproduct "LMRC-213")(= BBproduct "LMRC-212")(= BBproduct "LMRC-211")(= BBproduct "LMPL-201"))
				(progn	
			
				;grab r2
				(if	(/= r2cntrmax 0)
				  	(progn
					(setq direction 1)
					(setq shifter (* direction xcoordspacer))					
					(setq cntr2 0)
					(setq cmax2 upperBoundary)
					;xcoord connection to controller, this variable gets turned into the previous devices xcoord after first inner while loop
					(setq r2xcoord 0.4150)
						(while	(< cntr2 cmax2)
							(setq rungProduct (vlax-safearray-get-element r2Array r2ColumnIndex r2RowIndex))

						  	(if	(/= rungProduct "")
							  	(progn
							  	(setq xcoord (+ xcoord shifter))
								(cond
									((= xcoord maxblockxcoord)
									(setq direction -1)
									(setq shifter (* direction xcoordspacer))
									(setq ycoordbefore ycoord)
									(setq xcoord (+ xcoord shifter))
									(setq ycoord (+ ycoord -1.33))
									(setq xcoordoffset (+ xcoord 2))
							
									;;;connect points with CAT5E
									(setq p1 (list xcoord ycoordbefore))
									(setq p2 (list xcoordoffset ycoordbefore))
									(setq p3 (list xcoordoffset ycoord))
									(setq p4 (list xcoord ycoord))
									(setq point-list (list p1 p2 p3 p4))
								  	(setq cls 0)
								  	(setq polyline-layer "Riser-CAT5E")
								  	(setq polyline-width 0.00)
								  	(setq polyline-linetype "BYLAYER")
									(GSR2-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)
									)
									
									((= xcoord 0)
									(setq direction 1)
									(setq shifter (* direction xcoordspacer))
									(setq ycoordbefore ycoord)
									(setq xcoord (+ xcoord shifter))
									(setq ycoord (+ ycoord -1.33))
									(setq xcoordoffset (- xcoord 2))
									 
									;;;connect points with CAT5E
									(setq p1 (list xcoord ycoordbefore))
									(setq p2 (list xcoordoffset ycoordbefore))
									(setq p3 (list xcoordoffset ycoord))
									(setq p4 (list xcoord ycoord))
									(setq point-list (list p1 p2 p3 p4))
								  	(setq cls 0)
								  	(setq polyline-layer "Riser-CAT5E")
								  	(setq polyline-width 0.00)
								  	(setq polyline-linetype "BYLAYER")
									(GSR2-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)
									)								
								)
							  	(setq insertionPoint (list xcoord ycoord))
								(setq layer "Riser")
								(setq attributevaluelist nil)
								;;;call GSR2-entmod-blockinsert-attributes to insert the block
								(GSR2-entmod-blockinsert-attributes attributevaluelist insertionPoint layer rungProduct)
								(setq r2xcoord2 xcoord) 
								(setq r2ycoord ycoord)
								;calculate maxes
								(if
									(> xcoord xcoordmax)
									(setq xcoordmax xcoord)
								)
								(if
									(> ycoord ycoordmax)
									(setq ycoordmax ycoord)
								)

								;draw CAT5E
								(setq r2point1 (list r2xcoord r2ycoord))
								(setq r2point2 (list r2xcoord2 r2ycoord))
								(setq startpoint r2point1)
								(setq endpoint r2point2)
								(setq line-layer "Riser-CAT5E")
								(setq line-linetype "BYLAYER")
								(GSR2-draw-line startpoint endpoint line-layer line-linetype)
							  	;save xcoord of device for next loop when the connection is made
								(setq r2xcoord xcoord) 
								
								)
							)
     					
							(setq r2RowIndex (+ r2RowIndex 1))
						  	(setq cntr2 (+ cntr2 1))
						)


					(setq r2RowIndex 0)
					(setq r2ColumnIndex (+ r2ColumnIndex 1))
					)
				)
				
				;the coordinate drop is outside the r2 loop, this ensures the ycoord drops even when r2 doesn't exits
				(setq xcoord 0)
				(setq ycoord (+ ycoord -1.33))
				
				;grab r1
				(if	(/= r1cntrmax 0)
				  	(progn
					(setq direction 1)
					(setq shifter (* direction xcoordspacer))
					(setq cntr1 0)
					(setq cmax1 upperBoundary)
					;xcoord connection to controller, this variable gets turned into the previous devices xcoord after first inner while loop
					(setq r1xcoord 0.4150)
					(setq r1ycoord ycoord)
						(while	(< cntr1 cmax1)
							(setq rungProduct (vlax-safearray-get-element r1Array r1ColumnIndex r1RowIndex))

						  	(if	(/= rungProduct "")
							  	(progn

								;;; vertical connection from rung1 to controller
								;;; only happens once per loop
								(if
									(= cntr1 0)
								  	(progn
									(setq r1xcoord 0.4150)
									(setq r1ycoord ycoord)
									(setq r1point1 (list r1xcoord r1ycoord))
									(setq r1ycoord2 vycoordprevious)
									(setq r1point2 (list r1xcoord r1ycoord2))
									(if	(/= r1xcoord 0)
									  	(progn
										(setq startpoint r1point1)
										(setq endpoint r1point2)
										(setq line-layer "Riser-CAT5E")
										(setq line-linetype "BYLAYER")
										(GSR2-draw-line startpoint endpoint line-layer line-linetype)							
										)
									)
									)
								)

								  
							  	(setq xcoord (+ xcoord shifter))
								(cond
									((= xcoord maxblockxcoord)
									(setq direction -1)
									(setq shifter (* direction xcoordspacer))
									(setq ycoordbefore ycoord)
									(setq xcoord (+ xcoord shifter))
									(setq ycoord (+ ycoord -1.33))
									(setq xcoordoffset (+ xcoord 2))
							
									;;;connect points with CAT5E
									(setq p1 (list xcoord ycoordbefore))
									(setq p2 (list xcoordoffset ycoordbefore))
									(setq p3 (list xcoordoffset ycoord))
									(setq p4 (list xcoord ycoord))
									(setq point-list (list p1 p2 p3 p4))
								  	(setq cls 0)
								  	(setq polyline-layer "Riser-CAT5E")
								  	(setq polyline-width 0.00)
								  	(setq polyline-linetype "BYLAYER")
									(GSR2-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)
									)
									
									((= xcoord 0)
									(setq direction 1)
									(setq shifter (* direction xcoordspacer))
									(setq ycoordbefore ycoord)
									(setq xcoord (+ xcoord shifter))
									(setq ycoord (+ ycoord -1.33))
									(setq xcoordoffset (- xcoord 2))
									 
									;;;connect points with CAT5E
									(setq p1 (list xcoord ycoordbefore))
									(setq p2 (list xcoordoffset ycoordbefore))
									(setq p3 (list xcoordoffset ycoord))
									(setq p4 (list xcoord ycoord))
									(setq point-list (list p1 p2 p3 p4))
								  	(setq cls 0)
								  	(setq polyline-layer "Riser-CAT5E")
								  	(setq polyline-width 0.00)
								  	(setq polyline-linetype "BYLAYER")
									(GSR2-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)
									)								
								)

								(setq insertionPoint (list xcoord ycoord)) 
								(setq layer "Riser")
								(setq attributevaluelist nil)
								;;;call GSR2-entmod-blockinsert-attributes to insert the block
								(GSR2-entmod-blockinsert-attributes attributevaluelist insertionPoint layer rungProduct)
								(setq r1xcoord2 xcoord) 
								(setq r1ycoord ycoord)
								;calculate maxes
								(if
									(> xcoord xcoordmax)
									(setq xcoordmax xcoord)
								)
								(if
									(> ycoord ycoordmax)
									(setq ycoordmax ycoord)
								)
								

								
								;draw CAT5E
								(setq r1point1 (list r1xcoord r1ycoord))
								(setq r1point2 (list r1xcoord2 r1ycoord))								
								(setq startpoint r1point1)
								(setq endpoint r1point2)
								(setq line-layer "Riser-CAT5E")
								(setq line-linetype "BYLAYER")
								(GSR2-draw-line startpoint endpoint line-layer line-linetype)
							  	;save xcoord of device for next loop when the connection is made
							  	(setq r1xcoord xcoord)

								
								)
							)

							(setq r1RowIndex (+ r1RowIndex 1))
						  	(setq cntr1 (+ cntr1 1))
						)
					


					
					(setq xcoord 0)
					(setq ycoord (+ ycoord -1.33))
					(setq r1RowIndex 0)
					(setq r1ColumnIndex (+ r1ColumnIndex 1))	
					)
					)
				)
			  


			  
			)

		  	(if
				(OR (= BBproduct "LMPL-101")(= BBproduct "LMRC-102")(= BBproduct "LMRC-101")(= BBproduct "LMCP48")(= BBproduct "LMCP24")(= BBproduct "LMCP12")(= BBproduct "LMCP8")(= BBproduct "LMZC-301"))
			  	(progn	

				;grab r3
				(if	(/= r3cntrmax 0)
				  	(progn
					(setq direction 1)
					(setq shifter (* direction xcoordspacer))
					(setq cntr1 0)
					(setq r3RowIndex 0)
					(setq cmax1 upperBoundary)
					;xcoord connection to controller, this variable gets turned into the previous devices xcoord after first inner while loop
					(setq r3xcoord 0.4150)
						(while	(< cntr1 cmax1)
							(setq rungProduct (vlax-safearray-get-element r3Array r3ColumnIndex r3RowIndex))

						  	(if	(/= rungProduct "")
							  	(progn
							  	(setq xcoord (+ xcoord shifter))
								(cond
									((= xcoord maxblockxcoord)
									(setq direction -1)
									(setq shifter (* direction xcoordspacer))
									(setq ycoordbefore ycoord)
									(setq xcoord (+ xcoord shifter))
									(setq ycoord (+ ycoord -1.33))
									(setq xcoordoffset (+ xcoord 2))
							
									;;;connect points with CAT5E
									(setq p1 (list xcoord ycoordbefore))
									(setq p2 (list xcoordoffset ycoordbefore))
									(setq p3 (list xcoordoffset ycoord))
									(setq p4 (list xcoord ycoord))
									(setq point-list (list p1 p2 p3 p4))
								  	(setq cls 0)
								  	(setq polyline-layer "Riser-CAT5E")
								  	(setq polyline-width 0.00)
								  	(setq polyline-linetype "BYLAYER")
									(GSR2-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)
									)
									
									((= xcoord 0)
									(setq direction 1)
									(setq shifter (* direction xcoordspacer))
									(setq ycoordbefore ycoord)
									(setq xcoord (+ xcoord shifter))
									(setq ycoord (+ ycoord -1.33))
									(setq xcoordoffset (- xcoord 2))
									 
									;;;connect points with CAT5E
									(setq p1 (list xcoord ycoordbefore))
									(setq p2 (list xcoordoffset ycoordbefore))
									(setq p3 (list xcoordoffset ycoord))
									(setq p4 (list xcoord ycoord))
									(setq point-list (list p1 p2 p3 p4))
								  	(setq cls 0)
								  	(setq polyline-layer "Riser-CAT5E")
								  	(setq polyline-width 0.00)
								  	(setq polyline-linetype "BYLAYER")
									(GSR2-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)
									)								
								)
								(setq insertionPoint (list xcoord ycoord))
								(setq layer "Riser")
								(setq attributevaluelist nil)
								;;;call GSR2-entmod-blockinsert-attributes to insert the block
								(GSR2-entmod-blockinsert-attributes attributevaluelist insertionPoint layer rungProduct)
								(setq r3xcoord2 xcoord) 
								(setq r3ycoord ycoord)

								;calculate maxes
								(if
									(> xcoord xcoordmax)
									(setq xcoordmax xcoord)
								)
								(if
									(> ycoord ycoordmax)
									(setq ycoordmax ycoord)
								)
								

								
								;draw CAT5E
								(setq r3point1 (list r3xcoord r3ycoord))
								(setq r3point2 (list r3xcoord2 r3ycoord))								
								(setq startpoint r3point1)
								(setq endpoint r3point2)
								(setq line-layer "Riser-CAT5E")
								(setq line-linetype "BYLAYER")
								(GSR2-draw-line startpoint endpoint line-layer line-linetype)
								;save xcoord of device for next loop when the connection is made
								(setq r3xcoord xcoord)
								
								)
							)				      
							(setq r3RowIndex (+ r3RowIndex 1))
						  	(setq cntr1 (+ cntr1 1))
						)
					


					
					;vertical connection to panel
				  	(if
						(OR (= BBproduct "LMCP48")(= BBproduct "LMCP24")(= BBproduct "LMCP12")(= BBproduct "LMCP8")(= BBproduct "LMZC-301"))
					  	(progn

						(setq r3xcoord 0.4150)
						(setq r3point1 (list r3xcoord r3ycoord))
						(setq r3ycoord vycoordprevious)
						(setq r3point2 (list r3xcoord r3ycoord))
						
						(setq startpoint r3point1)
						(setq endpoint r3point2)
						(setq line-layer "Riser-CAT5E")
						(setq line-linetype "BYLAYER")
						(GSR2-draw-line startpoint endpoint line-layer line-linetype)						  

						)

					)

					

					(setq r3RowIndex 0)
					(setq r3ColumnIndex (+ r3ColumnIndex 1))
					)
				)			
				;the coordinate drop is outside the r3 loop, this ensures the ycoord drops even when r3 doesn't exits
				(setq xcoord 0)
				(setq ycoord (+ ycoord -1.33))



				
				)


			  
			)
		  
	
		  	(if
				(OR (= BBproduct "LMRC-112")(= BBproduct "LMRC-111")(= BBproduct "LMRC-112-M")(= BBproduct "LMRC-111-M"))
			  	(progn	
				;the coordinate drop happens
				(setq xcoord 0)
				(setq ycoord (+ ycoord -1.33))

				)


			  
			)


		  	;;; connect RBControllers
		  	;;; they will be below the bridge if there is one
		  	;;; move rung down after
			(if
				(OR (= BBproduct "LMZC-301")(= BBproduct "LMBC-300")(= BBproduct "LMCP48")(= BBproduct "LMCP24")(= BBproduct "LMCP12")(= BBproduct "LMCP8")(= BBproduct "LMZC-301")(= BBproduct "LMRC-222")(= BBproduct "LMRC-221")(= BBproduct "LMRC-213-347v")(= BBproduct "LMRC-212-347v")(= BBproduct "LMRC-211-347v")(= BBproduct "LMRC-213")(= BBproduct "LMRC-212")(= BBproduct "LMRC-211")(= BBproduct "LMPL-201")(= BBproduct "LMPL-101")(= BBproduct "LMRC-102")(= BBproduct "LMRC-101")(= BBproduct "LMRC-112-M")(= BBproduct "LMRC-111-M")(= BBproduct "LMRC-112")(= BBproduct "LMRC-111"))
			  	(progn

				;;;don't draw cable for last vertical item
				;;;use 1 minus the counter max cmax to tell if last item
				(if
				  	(>  (- cmax 1) cntr)
				  	(progn
					(setq insertionPoint (list xcoord ycoord))
					  
					;make vertical connection for RBControllers
					(setq verticalpoint1 insertionPoint)
					(setq vxcoord xcoord)
					(setq vycoord vycoordprevious)
					(setq verticalpoint2 (list vxcoord vycoord))	

					(setq startpoint verticalpoint1)
					(setq endpoint verticalpoint2)
					(setq line-layer "Riser-CAT5E")
					(setq line-linetype "BYLAYER")
					(GSR2-draw-line startpoint endpoint line-layer line-linetype)					      

					)
				)
				
				)
			)

		  
			(setq cntr (+ cntr 1))															;increment counter

		  
			;;;check boundaries before continuing


			(if	(> r1ColumnIndex upperBoundary)
				(setq r1ColumnIndex 0)
			)
			(if	(> r2ColumnIndex upperBoundary)
				(setq r2ColumnIndex 0)
			)
			(if	(> r3ColumnIndex upperBoundary)
				(setq r3ColumnIndex 0)
			)
			(if	(> rFinalColumnIndex upperBoundary)
				(setq rFinalColumnIndex 0)
			)
			(if	(> DLMREMAINDERColumnIndex upperBoundary)
				(setq DLMREMAINDERColumnIndex 0)
			)
			(if	(> REMAINDERColumnIndex upperBoundary)
				(setq REMAINDERColumnIndex 0)
			)
			(if	(> r1RowIndex upperBoundary)
				(setq r1RowIndex 0)
			)
			(if	(> r2RowIndex upperBoundary)
				(setq r2RowIndex 0)
			)
			(if	(> r3RowIndex upperBoundary)
				(setq r3RowIndex 0)
			)
			(if	(> rFinalRowIndex upperBoundary)
				(setq rFinalRowIndex 0)
			)
			(if	(> DLMREMAINDERRowIndex upperBoundary)
				(setq DLMREMAINDERRowIndex 0)
			)
			(if	(> REMAINDERRowIndex upperBoundary)
				(setq REMAINDERRowIndex 0)
			)

		)


	;;; add 1 to block width to account for vertical rung
	(if	(/= VerticalRung nil)
		(setq RBBLOCKWIDTH (+ RBBLOCKWIDTH 1))
	)
	  
  	(setq xcoord 0)
  	(setq direction 1)
	(setq shifter (* direction xcoordspacer))
	  
		;grab rFinal
	  	(if	(/= RBNUMBEROFRUNGFINAL 0)
		  	(progn

			(setq cntr1 0)
			(setq cmax1 upperBoundary)
		  	(setq rFinalColumnIndex 0)
		  	(setq rFinalRowIndex 0)
		  	(while	(< cntr1 cmax1)
				
				(setq rungProduct (vlax-safearray-get-element rFinalArray rFinalColumnIndex rFinalRowIndex))
					
				;pre increment xcoord when there are vertical products
	;;;			  	(if	(/= VerticalRung nil)
	;;;				  	(progn
	;;;			  		(setq xcoord (+ xcoord 5))
	;;;					)
	;;;				)
			   	
			  	(if	(/= rungProduct "")
				  	(progn



					;;;vertical connection from rFinal to controller
					;;;this connection is inside the product loop to ensure the connection is only drawn when there is actually placement
					(if
					  	(= cntr1 0)
					  	(progn
						(setq rFinalxcoord 0)
						(setq rFinalycoord ycoord)
						(setq rFinalpoint1 (list rFinalxcoord rFinalycoord))
						(setq rFinalycoord2 vycoordprevious)
						(setq rFinalpoint2 (list rFinalxcoord rFinalycoord2))
						(setq startpoint rFinalpoint1)
						(setq endpoint rFinalpoint2)
						(setq line-layer "Riser-CAT5E")
						(setq line-linetype "BYLAYER")
						(GSR2-draw-line startpoint endpoint line-layer line-linetype)
						)
					)


					  
				  	(setq xcoord (+ xcoord shifter))
					(cond
						((= xcoord maxblockxcoord)
						(setq direction -1)
						(setq shifter (* direction xcoordspacer))
						(setq ycoordbefore ycoord)
						(setq xcoord (+ xcoord shifter))
						(setq ycoord (+ ycoord -1.33))
						(setq xcoordoffset (+ xcoord 2))
				
						;;;connect points with CAT5E
						(setq p1 (list xcoord ycoordbefore))
						(setq p2 (list xcoordoffset ycoordbefore))
						(setq p3 (list xcoordoffset ycoord))
						(setq p4 (list xcoord ycoord))
						(setq point-list (list p1 p2 p3 p4))
					  	(setq cls 0)
					  	(setq polyline-layer "Riser-CAT5E")
					  	(setq polyline-width 0.00)
					  	(setq polyline-linetype "BYLAYER")
						(GSR2-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)
						)
						
						((= xcoord 0)
						(setq direction 1)
						(setq shifter (* direction xcoordspacer))
						(setq ycoordbefore ycoord)
						(setq xcoord (+ xcoord shifter))
						(setq ycoord (+ ycoord -1.33))
						(setq xcoordoffset (- xcoord 2))
						 
						;;;connect points with CAT5E
						(setq p1 (list xcoord ycoordbefore))
						(setq p2 (list xcoordoffset ycoordbefore))
						(setq p3 (list xcoordoffset ycoord))
						(setq p4 (list xcoord ycoord))
						(setq point-list (list p1 p2 p3 p4))
					  	(setq cls 0)
					  	(setq polyline-layer "Riser-CAT5E")
					  	(setq polyline-width 0.00)
					  	(setq polyline-linetype "BYLAYER")
						(GSR2-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)
						)								
					)
					(setq insertionPoint (list xcoord ycoord))
					(setq layer "Riser")
					(setq attributevaluelist nil)
					;;;call GSR2-entmod-blockinsert-attributes to insert the block
					(GSR2-entmod-blockinsert-attributes attributevaluelist insertionPoint layer rungProduct)
					(setq rFinalxcoord2 xcoord) 
					(setq rFinalycoord ycoord)

					;calculate maxes
					(if
						(> xcoord xcoordmax)
						(setq xcoordmax xcoord)
					)
					(if
						(> ycoord ycoordmax)
						(setq ycoordmax ycoord)
					)
					

					
					;draw CAT5E
					(setq rFinalpoint1 (list rFinalxcoord rFinalycoord))
					(setq rFinalpoint2 (list rFinalxcoord2 rFinalycoord))								
					(setq startpoint rFinalpoint1)
					(setq endpoint rFinalpoint2)
					(setq line-layer "Riser-CAT5E")
					(setq line-linetype "BYLAYER")
					(GSR2-draw-line startpoint endpoint line-layer line-linetype)
					;save xcoord of device for next loop when the connection is made
					(setq rFinalxcoord xcoord)
					
					)
				)




					
				(setq rFinalRowIndex (+ rFinalRowIndex 1))

			  
			  	;post increment xcoord when there are no vertical products
	;;;			  	(if	(= VerticalRung nil)
	;;;			  		(setq xcoord (+ xcoord 5))
	;;;				)					  



				(setq cntr1 (+ cntr1 1))

			)
		  	(setq rFinalColumnIndex 0)
	  		(setq rFinalRowIndex (+ rFinalRowIndex 1))
			
			

		)
	)
	  
	;;;coordinate drop
	(if
	  	(/= rungfinalplacement 0)
	  	(progn
		;the coordinate drop is outside the rFinal loop, this ensures the ycoord drops only when rFinal exits
		(setq xcoord 0)
		(setq ycoord (+ ycoord -1.33))
		)
	)
	  

	  
	;;; set xcoordmax if it is 0
	(if	(= xcoordmax 0)
		(setq xcoordmax 9)
	)
	  

	;;; set xcoordleftlimit
	(if	(= VerticalRung nil)
		(setq xcoordleftlimit -3)
	)
	  
	;;; set xcoordstart
	(if	(/= VerticalRung nil)
		(setq xcoordleftlimit 0)
	)
	  
  	(setq xcoord xcoordleftlimit)
  	(setq direction 1)	  
	(setq shifter (* direction xcoordspacer))
	  
	;grab DLMREMAINDER
	;at this point the main items from the room are all placed,
	;so the xcoordmax is already set, and will be used from here on out
	(if
	  	(/= rungdlmremainderplacement 0)
	  	(progn
		(setq cntr1 0)
		(setq cmax1 upperBoundary)
	  	(setq DLMREMAINDERColumnIndex 0)
	  	(setq DLMREMAINDERRowIndex 0)
	  	(while	(< cntr1 cmax1)
			
			(setq rungProduct (vlax-safearray-get-element DLMREMAINDERArray DLMREMAINDERColumnIndex DLMREMAINDERRowIndex))
				
		   	
		  	(if	(/= rungProduct "")
			  	(progn
			  	
				(setq xcoord (+ xcoord shifter))
				(cond
					((> xcoord xcoordmax)
					(setq direction 1)
					(setq shifter (* direction xcoordspacer))
					(setq ycoordbefore ycoord)
					(setq xcoord xcoordleftlimit)
					(setq xcoord (+ xcoord shifter))
					(setq ycoord (+ ycoord -1.33))
					(setq xcoordoffset (- xcoord 2))
					)
					
					((= xcoord xcoordleftlimit)
					(setq direction 1)
					(setq shifter (* direction xcoordspacer))
					(setq ycoordbefore ycoord)
					(setq xcoord (+ xcoord shifter))
					(setq ycoord (+ ycoord -1.33))
					(setq xcoordoffset (- xcoord 2))
					
					)								
				)	
				(setq insertionPoint (list xcoord ycoord))
				(setq layer "Riser")
				(setq attributevaluelist nil)
				;;;call GSR2-entmod-blockinsert-attributes to insert the block
				(GSR2-entmod-blockinsert-attributes attributevaluelist insertionPoint layer rungProduct)

				;calculate maxes
				(if
					(> xcoord xcoordmax)
					(setq xcoordmax xcoord)
				)
				(if
					(> ycoord ycoordmax)
					(setq ycoordmax ycoord)
				)
				
				)
			)




				
			(setq DLMREMAINDERRowIndex (+ DLMREMAINDERRowIndex 1))
		  




			(setq cntr1 (+ cntr1 1))

		)
		

		
	  	(setq DLMREMAINDERColumnIndex 0)
		(setq DLMREMAINDERRowIndex (+ DLMREMAINDERRowIndex 1))
		)
	)


	;;;coordinate drop
	(if
	  	(/= rungdlmremainderplacement 0)
	  	(progn
		;the coordinate drop is outside the rDLMRemainder loop, this ensures the ycoord drops only when rDLMRemainder exits
		(setq xcoord 0)
		(setq ycoord (+ ycoord -1.33))
		)
	)


	  
  	(setq xcoord xcoordleftlimit)
  	(setq direction 1)
	(setq shifter (* direction xcoordspacer))	  
	;grab REMAINDER
  	(if	(/= rungremainderplacement 0)
	  	(progn


		(setq cntr1 0)
		(setq cmax1 upperBoundary)
	  	(setq REMAINDERColumnIndex 0)
	  	(setq REMAINDERRowIndex 0)
	  	(while	(< cntr1 cmax1)
			
			(setq rungProduct (vlax-safearray-get-element REMAINDERArray REMAINDERColumnIndex REMAINDERRowIndex))
				
			;pre increment xcoord when there are vertical products
;;;			  	(if	(/= VerticalRung nil)
;;;				  	(progn
;;;			  		(setq xcoord (+ xcoord 5))
;;;					)
;;;				)
		   	
		  	(if	(/= rungProduct "")
			  	(progn
			  	(setq xcoord (+ xcoord shifter))
				(cond
					((> xcoord xcoordmax)
					(setq direction 1)
					(setq shifter (* direction xcoordspacer))
					(setq ycoordbefore ycoord)
					(setq xcoord xcoordleftlimit)
					(setq xcoord (+ xcoord shifter))
					(setq ycoord (+ ycoord -1.33))
					(setq xcoordoffset (- xcoord 2))
			
					)
					
					((= xcoord xcoordleftlimit)
					(setq direction 1)
					(setq shifter (* direction xcoordspacer))
					(setq ycoordbefore ycoord)
					(setq xcoord (+ xcoord shifter))
					(setq ycoord (+ ycoord -1.33))
					(setq xcoordoffset (- xcoord 2))
					
					)								
				)
				(setq insertionPoint (list xcoord ycoord))

				;;;	before insertion, find out in block is defined in template
				;;; 	if block is not defined, use placeholder and store the block name in the attribute
				(setq blockexists (tblsearch "block" rungProduct))

				(cond																	;conditional block
				  	;;;when block exists
				  	((/= blockexists nil)														;conditional statement
					(setq layer "Riser")
					(setq attributevaluelist nil)
					;;;call GSR2-entmod-blockinsert-attributes to insert the block
					(GSR2-entmod-blockinsert-attributes attributevaluelist insertionPoint layer rungProduct)
					)																;end conditional statement

					;;;when block does not exist
				  	((= blockexists nil)														;conditional statement
					(setq layer "Riser")
					(setq attributevaluelist (list rungProduct))											;turn the name of the rungProduct into a list
					;;;call GSR2-entmod-blockinsert-attributes to insert the block
					(GSR2-entmod-blockinsert-attributes attributevaluelist insertionPoint layer ConstantRiserPlaceholder)								 
					)																;end conditional statement

				)


				;calculate maxes
				(if
					(> xcoord xcoordmax)
					(setq xcoordmax xcoord)
				)
				(if
					(> ycoord ycoordmax)
					(setq ycoordmax ycoord)
				)
				
				)
			)




				
			(setq REMAINDERRowIndex (+ REMAINDERRowIndex 1))

		  
		  	;post increment xcoord when there are no vertical products
;;;			  	(if	(= VerticalRung nil)
;;;			  		(setq xcoord (+ xcoord 5))
;;;				)					  



			(setq cntr1 (+ cntr1 1))

		)
		

		
	  	(setq REMAINDERColumnIndex 0)
  		(setq REMAINDERRowIndex (+ REMAINDERRowIndex 1))			
		)
	)

	  
	;;;coordinate drop
	(if
	  	(/= rungremainderplacement 0)
	  	(progn
		;the coordinate drop is outside the rDLMRemainder loop, this ensures the ycoord drops only when rDLMRemainder exits
		(setq xcoord 5)
		(setq ycoord (+ ycoord -1.33))
		)
	)


	  
	  
;;;	  	up to this point, block width is equal to the number of columns used in the matrix
;;;	  	multiply RBBLOCKWIDTH by the x dimension of each product block in the room, which is 5
;;;	  	(setq RBBLOCKWIDTH (* RBBLOCKWIDTH 5))
	  	(setq RBBLOCKWIDTH xcoordmax)
		(setq RBBLOCKWIDTH (+ RBBLOCKWIDTH 5))
	  
;;;	  	account for text after block
	  	(if	(= VerticalRung nil)
	  	(setq RBBLOCKWIDTH (+ RBBLOCKWIDTH 0))
		)
	  	(if	(/= VerticalRung nil)
	  	(setq RBBLOCKWIDTH (+ RBBLOCKWIDTH 0))
		)
;;;	  	account for text above block
	  	(setq RBBLOCKHEIGHT (- ycoord 1.13))
	)




  	(entmake
	(list
	(cons 0 "ENDBLK")
	)
	)  															;finish block defition
	(setq RBBLOCKHEIGHT (abs RBBLOCKHEIGHT))


	(setq returnlist (list RBBLOCKWIDTH RBBLOCKHEIGHT)) 

  
)






(defun GSR2-draw-line (startpoint endpoint line-layer line-linetype)
;;;arguments
;;;	point-list
;;;	line-layer
;;;	line-linetype
;;;
;;;Global from
;;;	None
;;;
;;;Global to
;;;	None
  	(entmakex
	  		(append
			(list
			(cons 0 "LINE")	      
			(cons 6 line-linetype)
			(cons 8 line-layer)
                       	(cons 10 startpoint)
			(cons 11 endpoint)
			)
			)
	)
)




(defun GSR2-RungBuilder-destroyer (/)

  (FOREACH X' (
		GLOBALDLMMSTPDEVICES
		GLOBALDLMDAYLIGHT
	       	GLOBALDLMINTERFACES GLOBALDLMOCCCORNERMOUNT GLOBALDLMOCCSENSORS
	       	GLOBALDLMSPLITTERS GLOBALDLMSWITCHES INSERTIONPOINT NUMBEROFDLMREMAINDERRUNG
	       	NUMBEROFREMAINDERRUNG GLOBALNUMBEROFRUNG1 GLOBALNUMBEROFRUNG2 GLOBALNUMBEROFRUNG3
	       	RBdlmremainder RBPRODUCT RBPRODUCT2 RBPRODUCTS RBPRODUCTSPASS1 RBPRODUCTSPASS2 RBPRODUCTSPASS3 RBPRODUCTSPASS4 RBPRODUCTSPASS5 GLOBALREMAINDER
		GLOBALDLMSEGMENTMANAGERS GLOBALDLMNETWORKBRIDGES GLOBALDLMZONECONTROLLERS GLOBALDLMPANELS GLOBALDLMCONTROLLERS GLOBALDLMPLUGCONTROLLERS 
	       	GLOBALDLMNETWORKDEVICES  GLOBALNUMBEROFRUNGFINAL Broom RBnetworksegment RBnumberofproductsinroom RBcabinetinformationlist RBDLMSEGMENTMANAGERS RBDLMNETWORKBRIDGES RBDLMZONECONTROLLERS RBDLMPANELS RBDLMCONTROLLERS RBDLMPLUGCONTROLLERS
		RBControllers RBDLMINTERFACES RBDLMOCCSENSORS RBDLMOCCCORNERMOUNT RBDLMDAYLIGHT RBDLMSWITCHES RBDLMSPLITTERS RBREMAINDER
		RBNUMBEROFRUNG1 RBNUMBEROFRUNG2 RBNUMBEROFRUNG3 RBNUMBEROFRUNGFINAL
		RBDLMNETWORKDEVICES
		RBDLMMSTPDEVICES
		DLMSEGMENTMANAGERS DLMNETWORKBRIDGES DLMZONECONTROLLERS DLMPANELS DLMCONTROLLERS DLMPLUGCONTROLLERS Controllers DLMINTERFACES DLMOCCSENSORS
	       	DLMOCCCORNERMOUNT DLMDAYLIGHT DLMSWITCHES DLMSPLITTERS REMAINDER NUMBEROFRUNG1 NUMBEROFRUNG2 NUMBEROFRUNG3 NUMBEROFRUNGFINAL

	       
	       
   ) (SET X NIL))
  
	(princ)

)












; --- riserdraworder Sub Function ---
; place all blocks on top of wires
; Alex Lundin 04-28-2017
(defun GSR2-riserdraworder	( /
				ACADOBJ ARR ARRROWLOWERBOUNDARY ARRROWUPPERBOUNDARY CMAX CNTR COLUMN DOC EDICTIONARY
			 	EXTDICT MODELSPACE SENTITYOBJ SETITEM SORTTBL SPACE SS
		 	)		
;;;arguments
;;;	Any calling function:
;;;	n
;;;
;;;Global from
;;;	None
;;;Global to
;;;  	None
  
;;;	(setq blocks(ssget "x" '((0 . "INSERT")(8 . "Riser"))))							;select all blocks on WATTSTOPPER layer, store to blocks varaible
;;;		(cond
;;;		  	((/= blocks nil)
;;;			(command "draworder" "p" "" "front")
;;;			)
;;;		)
;;;  	(command "regenall")

	(setq doc (vla-get-activedocument (vlax-get-acad-object))
	space (vla-get-block (vla-get-activelayout doc))
	extdict (vla-getextensiondictionary space)
	sorttbl (vla-getobject extdict "ACAD_SORTENTS")
	)
  	(setq ss nil)
	;;create ss for this example
	(setq ss (ssget "x" '((0 . "INSERT")(8 . "Riser"))))

;;;  	(cond
;;;	  	((/= ss nil)

		;;create a list of the vla-objects in the ss
		(vlax-for x (vla-get-activeselectionset doc)
		(setq sslist (cons x sslist))
		)
	  		  
		;;Move them to the Top of the draworder
	  	(if
		  	(/= sslist nil)
			(progn

		      	(setq acadObj (vlax-get-acad-object))
			(setq doc (vla-get-ActiveDocument acadObj))
			(setq modelSpace (vla-get-ModelSpace doc))
		    	;; Get an extension dictionary and, if necessary, add a SortentsTable object
		    	(setq eDictionary (vla-GetExtensionDictionary modelSpace))

		    	;; Prevent failed GetObject calls from throwing an exception
		    	(setq sentityObj (vl-catch-all-apply 'vla-GetObject (list eDictionary "ACAD_SORTENTS")))
		  
		    	(if (= (type sentityObj)'VL-CATCH-ALL-APPLY-ERROR)
		       	;; No SortentsTable object, so add one
		       	(setq sentityObj (vla-AddObject eDictionary "ACAD_SORTENTS" "AcDbSortentsTable"))
		    	)
			;;create array
			(setq arrRowLowerBoundary 0)
			(setq arrRowUpperBoundary 0)
			(setq arr (vlax-make-safearray vlax-vbObject (cons arrRowLowerBoundary arrRowUpperBoundary)))

			(setq cntr 0)
			(setq column 0)
			(setq cmax (sslength ss))
			(while
			  	(< cntr cmax)
			  	(setq setitem (nth cntr sslist))
			  	(vlax-safearray-put-element arr 0 setitem)
			  	(vla-MoveToTop sentityObj arr)
			  

			  	(setq cntr (+ cntr 1))
			)
			

			;; Move the circle object to the bottom
			(vla-Update acadObj)
			)
		)
;;;		)
;;;	)
	(setq ss nil)
  
  	;;create ss for this example
	(setq ss (ssget "x" '((0 . "LINE"))))

  	(if
	  	(/= ss nil)
	  	(progn
		;;create a list of the vla-objects in the ss
		(vlax-for x (vla-get-activeselectionset doc)
		(setq sslist (cons x sslist))
		)

		;;Move them to the Top of the draworder
	  	(if
		  	(/= sslist nil)
			(progn

		      	(setq acadObj (vlax-get-acad-object))
			(setq doc (vla-get-ActiveDocument acadObj))
			(setq modelSpace (vla-get-ModelSpace doc))
		    	;; Get an extension dictionary and, if necessary, add a SortentsTable object
		    	(setq eDictionary (vla-GetExtensionDictionary modelSpace))

		    	;; Prevent failed GetObject calls from throwing an exception
		    	(setq sentityObj (vl-catch-all-apply 'vla-GetObject (list eDictionary "ACAD_SORTENTS")))
		  
		    	(if (= (type sentityObj)'VL-CATCH-ALL-APPLY-ERROR)
		       	;; No SortentsTable object, so add one
		       	(setq sentityObj (vla-AddObject eDictionary "ACAD_SORTENTS" "AcDbSortentsTable"))
		    	)
			;;create array
			(setq arrRowLowerBoundary 0)
			(setq arrRowUpperBoundary 0)
			(setq arr (vlax-make-safearray vlax-vbObject (cons arrRowLowerBoundary arrRowUpperBoundary)))

			(setq cntr 0)
			(setq column 0)
			(setq cmax (sslength ss))
			(while
			  	(< cntr (- 1 cmax))
			  	(setq setitem (nth cntr sslist))
			  	(vlax-safearray-put-element arr 0 setitem)
			  	(vla-MoveToBottom sentityObj arr)
			  

			  	(setq cntr (+ cntr 1))
			)
			

			;; Move the circle object to the bottom
			(vla-Update acadObj)
			)
		)
	  	)
 	)
	(setq ss nil)
    	;;create ss for this example
	(setq ss (ssget "x" '((0 . "PLINE"))))
	(if
	  	(/= ss nil)
	  	(progn
		;;create a list of the vla-objects in the ss
		(vlax-for x (vla-get-activeselectionset doc)
		(setq sslist (cons x sslist))
		)
		;;Move them to the Top of the draworder
	  	(if
		  	(/= sslist nil)
			(progn

		      	(setq acadObj (vlax-get-acad-object))
			(setq doc (vla-get-ActiveDocument acadObj))
			(setq modelSpace (vla-get-ModelSpace doc))
		    	;; Get an extension dictionary and, if necessary, add a SortentsTable object
		    	(setq eDictionary (vla-GetExtensionDictionary modelSpace))

		    	;; Prevent failed GetObject calls from throwing an exception
		    	(setq sentityObj (vl-catch-all-apply 'vla-GetObject (list eDictionary "ACAD_SORTENTS")))
		  
		    	(if (= (type sentityObj)'VL-CATCH-ALL-APPLY-ERROR)
		       	;; No SortentsTable object, so add one
		       	(setq sentityObj (vla-AddObject eDictionary "ACAD_SORTENTS" "AcDbSortentsTable"))
		    	)
			;;create array
			(setq arrRowLowerBoundary 0)
			(setq arrRowUpperBoundary 0)
			(setq arr (vlax-make-safearray vlax-vbObject (cons arrRowLowerBoundary arrRowUpperBoundary)))

			(setq cntr 0)
			(setq column 0)
			(setq cmax (sslength ss))
			(while
			  	(< cntr cmax)
			  	(setq setitem (nth cntr sslist))
			  	(vlax-safearray-put-element arr 0 setitem)
			  	(vla-MoveToBottom sentityObj arr)
			  

			  	(setq cntr (+ cntr 1))
			)
			

			;; Move the circle object to the bottom
			(vla-Update acadObj)
			)
		)
	  	)
	)
  	(command "regenall")
  	(princ)
)




; --- GSR2-RoundUp Sub Function ---
; take any decimal value and round up to the next whole integer
; Alex Lundin 03-17-2017
(defun GSR2-RoundUp ( n )
;;;arguments
;;;	Any calling function:
;;;	n
;;;
;;;Global from
;;;	None
;;;Global to
;;;  	None
  	(cond
	  	((/= n nil)
			(if (or (minusp n) (zerop (rem n 1)))
			(fix n)
			(fix (1+ n))
			)
		 )
	)
)


; --- GSR2-RoundUp Sub Function ---
; take any decimal value and round up to the next whole integer
; Alex Lundin 03-17-2017
(defun RoundDown ( n )
;;;arguments
;;;	Any calling function:
;;;	n
;;;
;;;Global from
;;;	None
;;;Global to
;;;  	None
  	(cond
	  	((/= n nil)
			(if (or (minusp n) (zerop (rem n 1)))
			(fix n)
			(fix (1- n))
			)
		 )
	)
)