;;; --- GRD Function ---
;;; Alex Lundin 08-21-2017
(defun c:GRD 	(
	      	/
		CUSTOMBLOCKSET FILE1 TR-RETURNLIST LMRJBLOCKNAME LMRJUPSIDEDOWNBLOCKNAME ROOMCHECKER ROOMDETAILSROOMLIST SSRETURNLIST
		 ROOMDETAILSROOMLISTANDBLOCKINFO TRRETURNLIST RNFRETURNROOM TRROOMSANDBLOCKINFORMATION
	       	)
	(vl-load-com)																		;open f1 for reading -- similar to oppen in eblock function

	(setq *acad (vlax-get-acad-object))
	(setq acdoc (vla-get-ActiveDocument *acad))

  	(setq drawingName (vl-filename-base (vla-get-fullname acdoc)))
	

	;;;let user know if text file cannot be found
  	(if																			;-cond block
	  
	  	(/= drawingName "RD-DLM")															;-- cond statement if error handler for no text file
	  	(progn
	  	(alert
		(strcat
			"GRD must be called from a RD-DLM template file."
			"\n"
			"\nExiting now."
			"\nClick OK."
		)
		)
		(princ "GRD must be called from a RD-DLM template file.")
		(exit)
		)																		;-- end cond statement
	)


  
  
				  
	;;;open text file for reading
  	(setq file1 (open (strcat (getvar 'DWGPREFIX) "Room_Details_Extraction.txt")  "r"))

	;;;let user know if text file cannot be found
  	(if																			;-cond block
	  
	  	(= file1 nil)																	;-- cond statement if error handler for no text file
	  	(progn
	  	(alert
		(strcat
			"Room_Details_Extraction.txt not found in folder with RD-DLM"
			"\nCreate a source of all blocks on the project, such as RISERLAYOUT."
			"\nThen run EBLOCKRD on that drawing to create the extraction."
			"\n"
			"\nExiting now."
			"\nClick OK."
		)
		)
		(princ "Room_Details_Extraction.txt not found in folder with SR-DLM")
		(exit)
		)																		;-- end cond statement
	)

  	(command "_.Layer" "_Make" "RD-ROOMS" "_Color" "7" "" "LType" "Continuous" "" "")
  	(command "_.Layer" "_Make" "RD-ROOMS-DESIGN-REVIEW" "_Color" "30" "" "LType" "Continuous" "" "")
  	(command "_.Layer" "_Make" "RD-BOM" "_Color" "7" "" "LType" "Continuous" "" "")
  	(command "_.Layer" "_Make" "RD-EXTRACTION-VERIFICATION" "_Color" "30" "" "LType" "Continuous" "" "Plot" "N" "RD-EXTRACTION-VERIFICATION" "")
	;;;delete previous blocks on riser layer from any previous risers and purge
	(setq customBlockSet (ssget "X" '((8 . "RD-ROOMS" ))))													;selection set
	(cond
	  	((/= customBlockSet nil) 
  		(command "erase" "p" "")
		)
  	)
	(setq customBlockSet (ssget "X" '((8 . "RD-ROOMS-DESIGN-REVIEW" ))))											;selection set
	(cond
	  	((/= customBlockSet nil) 
  		(command "erase" "p" "")
		)
  	)  
	(setq customBlockSet (ssget "X" '((8 . "RD-BOM" ))))													;selection set
	(cond
	  	((/= customBlockSet nil) 
  		(command "erase" "p" "")
		)
  	)
	(setq customBlockSet (ssget "X" '((8 . "RD-EXTRACTION-VERIFICATION" ))))										;selection set
	(cond
	  	((/= customBlockSet nil) 
  		(command "erase" "p" "")
		)
  	)  
  	(command "-PURGE" "A" "*Z-RD*" "N")

																				;- end while
	(GRD-get-user-input)

	(setq TRreturnList (GRD-text-reader LMRJBlockName LMRJUpsideDownBlockName))

  	(setq roomDetailsRoomListAndBlockInfo (reverse TRreturnList))												;reverse singlelineroomlist to account for cons property which builds list backwards
  	
  	(setq roomChecker (length roomDetailsRoomListAndBlockInfo))
  	(cond																			;- cond block
	  	((>= roomChecker 1)																;-- cond statement
		(setq ssReturnList (GRD-standalonesort roomDetailsRoomListAndBlockInfo))
		)																		;-- end cond statement
	)  




  	(if
		(/= ssReturnList nil)
	  	(progn
	  	(GRD-standalone-inserter ssReturnList)
		)
	)

;;;;;;	burst all blocks into individual products
;;;;;;	this is the only way draw order works
  	(setq ss nil)
  	(setq ss (ssget "x" '((0 . "INSERT")(8 . "RD-ROOMS"))))
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
;;;
;;;  	(setq ss nil)
;;;  	(setq ss (ssget "x" '((0 . "INSERT")(8 . "Riser-Rooms"))))
;;;  	(if
;;;	  	(/= ss nil)
;;;	  	(progn
;;;		(setq cntr 0)
;;;		(setq cmax (sslength ss))
;;;		(while
;;;		  	(< cntr cmax)
;;;		  	(setq item (ssname ss cntr))
;;;			(command "_.explode" item)
;;;		  	(setq cntr (+ cntr 1))
;;;		)
;;;		)
;;;	)
;;;  
;;;  	(c:roomdetailsdraworder)
  	
(vlax-release-object *acad acdoc)

)

;end GRD

(defun *error* (msg)
(princ)
)


(defun GRD-get-user-input ( / )
	;;; the only way to allow these variables to affect all get functions is to keep them global
  	;;; this is the only function allowed to modify these variables
  	;;; they are destroyed at end of function run
  	;;; only allow the program to get them once
	(print "Choose sheet size: ")
  	(print "1 - 8.5 X 11")
;;;  	(print "2 - 11 X 17")
;;;  	(print "3 - 22 X 34")
;;;  	(print "4 - 36 X 48")
  	(print)
  	(setq GRDglobalSheetSize (getint "Enter the number that matches your choice: "))

  	(princ)
  	(print "Each room controller has a text attribute slot. ")
  	(print "This attribute holds the circuit identifier for the electrical circuit feeding the room controller.")
  	(print "The Generate Room Details function can leave space between each controller for this information.")
  	(princ "\n")
  	(print "Choose room controller enchancements: ")
  	(print "1 - Leave space for manually entering CIRCUIT_IN attribute")
  	(print "2 - Don't leave space for manually entering CIRCUIT_IN attribute")
  	(print)
  	(setq GRDglobalControllerCircuitIn (getint "Enter the number that matches your choice: "))

  
  
  
)
(defun GRD-get-constant-block-info ( / LMRJBlockName LMRJUpsideDownBlockName LMRJBlockHeight returnlist)

    	(setq LMRJBlockName "LMRJ")
  	(setq LMRJUpsideDownBlockName "LMRJ-UPSIDE-DOWN")
	(setq LMRJBlockHeight 11.6498)

  	(setq returnlist (list LMRJBlockName LMRJUpsideDownBlockName LMRJBlockHeight))
)


(defun	GRD-get-general-controller-information
	( / CONTROLLERCABLELEFTEDGEEXTENSION CONTROLLERCABLERIGHTEDGEEXTENSION HEIGHTBETWEENLMRJPORTANDCONNECTOR HORIZONTALSPACEBETWEENCONTROLLERS LMRJBLOCKHEIGHT RETURNLIST VERTICALSPACEBETWEENCONTROLLERS WIDTHBETWEENCONTROLLERLMRJPORTS XCOORDCONTROLLERMAX XCOORDCONTROLLERMIN XCOORDCONTROLLERSTART YCOORDCONTROLLERSTART)

  	;;;common variables for all controllers
	(setq widthBetweenControllerLMRJPorts 8.1706)
  	(setq heightBetweenLMRJPortAndConnector 4.3610)
  	(setq LMRJBlockHeight 11.6498)
  
  	;;;set horizontal space between
	;;;this depends on use input global variable
  	(if
	  	(= GRDglobalControllerCircuitIn 1)
	  	(progn
		(setq horizontalSpaceBetweenControllers 76)
		)
	)
  	(if
	  	(= GRDglobalControllerCircuitIn 2)
	  	(progn
		(setq horizontalSpaceBetweenControllers 30)
		)
	)
  
  	;;;set vertical space between controllers
  	(setq verticalSpaceBetweenControllers 40)
  	
  	;;;coordinate variables
  	(setq xCoordControllerStart 180)
  	(setq yCoordControllerStart -54.3901)  
  	(setq xCoordControllerMin 179)


  	;;;set max
	;;;this depends on use input global variable
  	(if
	  	(= GRDglobalSheetSize 1)
	  	(progn
		(setq xCoordControllerMax 550)
		)
	)
  	(if
	  	(= GRDglobalSheetSize 2)
	  	(progn
		(setq xCoordControllerMax 600)
		)
	)
  	(if
	  	(= GRDglobalSheetSize 3)
	  	(progn
		(setq xCoordControllerMax 800)
		)
	)
  	(if
	  	(= GRDglobalSheetSize 4)
	  	(progn
		(setq xCoordControllerMax 1000)
		)
	)
  
	
  
  	;;;cable variables
  	(setq controllerCableLeftEdgeExtension 85)
	(setq controllerCableRightEdgeExtension 218)
  
	(setq returnlist ( list widthBetweenControllerLMRJPorts heightBetweenLMRJPortAndConnector LMRJBlockHeight horizontalSpaceBetweenControllers verticalSpaceBetweenControllers xCoordControllerStart yCoordControllerStart xCoordControllerMin xCoordControllerMax controllerCableLeftEdgeExtension controllerCableRightEdgeExtension))
)





(defun	GRD-get-general-bridge-information
	( / DISTANCETOBRIDGETOP HEIGHTBETWEENBRIDGELMRJPORTANDCONNECTOR HEIGHTOFFSETFROMBRIDGELMRJPORTSFORFILLETRADIUS HEIGHTOFFSETFROMBRIDGELMRJPORTSFORFILLETRADIUS2 HORIZONTALCONNECTIONEXTENSION HORIZONTALSPACEBETWEENBRIDGES RETURNLIST VERTICALSPACEBETWEENBRIDGES WIDTHBETWEENBRIDGELMRJPORTS WIDTHOFFSETFROMBRIDGELMRJPORTSFORFILLETRADIUS XCOORDBRIDGESSTART YCOORDBRIDGESSTART )

	;;; common variables for Bridge devices
	(setq widthBetweenBridgeLMRJPorts 8.1706)
  	(setq heightBetweenBridgeLMRJPortAndConnector 4.3610)

  	;;; offsets from connector so fillet doesn't cross pins of input devices
  	(setq widthOffsetFromBridgeLMRJPortsForFilletRadius 0.6978)
  	(setq heightOffsetFromBridgeLMRJPortsForFilletRadius 2.8088)
  	(setq heightOffsetFromBridgeLMRJPortsForFilletRadius2 3.1912)

  	;;; starting coordinates and space between bridges
	(setq horizontalSpaceBetweenBridges 0)
	(setq verticalSpaceBetweenBridges 126)
  	(setq DistanceToBridgeTop 147)
  	(setq xCoordBridgesStart 30)
  	(setq yCoordBridgesStart -54.3901)
  	(setq HorizontalConnectionExtension 25)


  	
	(setq returnlist (list widthBetweenBridgeLMRJPorts heightBetweenBridgeLMRJPortAndConnector widthOffsetFromBridgeLMRJPortsForFilletRadius heightOffsetFromBridgeLMRJPortsForFilletRadius heightOffsetFromBridgeLMRJPortsForFilletRadius2 horizontalSpaceBetweenBridges verticalSpaceBetweenBridges DistanceToBridgeTop xCoordBridgesStart yCoordBridgesStart HorizontalConnectionExtension))
)





; --- GRD-text-reader Sub Function ---
; determines method for reading information from text file
; Alex Lundin 03-17-2017
(defun GRD-text-reader	(
			TR-LMRJBlockName TR-LMRJUpsideDownBlockName	
			/
			 ROOMDETAILSROOMLISTANDBLOCKINFO TRRETURNLIST RNFRETURNROOM TRROOMSANDBLOCKINFORMATION
			DRAWINGLOCATION FILE1 NUMBEROFPRODUCTS PRODUCT PRODUCTS ROOMLOOPCHECK SINGLELINEROOMLIST trfunctionreturnlist TR-roomList
			BLOCKHEIGHT BLOCKWIDTH CONTROLLERS DLMCONTROLLERS DLMDAYLIGHT DLMINTERFACES DLMNETWORKBRIDGES DLMOCCCORNERMOUNT DLMOCCSENSORS DLMPANELS DLMPLUGCONTROLLERS DLMSEGMENTMANAGERS DLMSPLITTERS DLMSWITCHES DLMZONECONTROLLERS NUMBEROFCONTROLLERS NUMBEROFPRODUCTSINROOM NUMBEROFRUNG1 NUMBEROFRUNG2 NUMBEROFRUNG3 NUMBEROFRUNGFINAL PCRETURNLIST RBRETURNLIST REMAINDER RHRETURNLIST STANDALONESUBLIST TR-ROOM TR-ROOMFORblock-builder TR-STANDALONEDOTTEDPAIRS USEABLECONTROLLERPORTS
			)


	;;;open text file for reading
  	(setq file1 (open (strcat (getvar 'DWGPREFIX) "Room_Details_Extraction.txt")  "r"))
																			;- end cond block

	;;;read the first line of the text file in, which is the location of the drawing that was extracted from
	;;;this is the priming read for the following while loop
  	(setq drawingLocation (read-line file1))														;read in first line of Riser_Extraction.txt which will be drawing location
  	(setq TR-room (read-line file1))															;priming read for TR-room so it's not empty
  	(setq roomLoopCheck TR-room)																;roomLoopCheck will always hold the previous rooms value inside the while loop
	(setq RNFreturnRoom (GRD-roomnameformat roomLoopCheck))
  	(setq TR-roomList( cons RNFreturnRoom TR-roomList))													;create a list of all rooms from the extraction
  
  	(while	(/= roomLoopCheck nil)																;- while loop for reading text document, end when roomLoopCheck is nil

		;;;first if statment
		;;;this will call the subfunctions when true
		;;;logic follows:
		;;;if TR-room does not equal roomLoopCheck, call subfunctions to operate on the list of products from the previous room
		;;;TR-room is the current value read from the text file and is compared to roomLoopCheck, the prevous room
		;;;after the subfunctions run, nil list called products and store TR-room to the singlelineroomlist
		(if
		  	(/= TR-room roomLoopCheck)														;-- if statement to identify new room
			(progn																	;---progn wrap


	  		(setq RHreturnlist (GRD-RiserHeirarchy products))
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
			
			(setq PCreturnlist(GRD-PortCalculator products NumberofCONTROLLERS))
			(setq NUMBEROFRUNG1 (nth 0 PCreturnlist))
			(setq NUMBEROFRUNG2 (nth 1 PCreturnlist))
			(setq NUMBEROFRUNG3 (nth 2 PCreturnlist))
			(setq NUMBEROFRUNGFINAL(nth 3 PCreturnlist))
			(setq UseableControllerPorts(nth 4 PCreturnlist))
			
			(setq numberofproductsinroom (length products))

			(setq RNFreturnRoom (GRD-roomnameformat roomLoopCheck))
  			(setq RBreturnlist (GRD-block-builder  RNFreturnRoom roomLoopCheck DLMSEGMENTMANAGERS DLMNETWORKBRIDGES DLMZONECONTROLLERS DLMPANELS DLMCONTROLLERS DLMPLUGCONTROLLERS CONTROLLERS DLMINTERFACES DLMOCCSENSORS DLMOCCCORNERMOUNT DLMDAYLIGHT DLMSWITCHES DLMSPLITTERS REMAINDER UseableControllerPorts))
			(setq blockwidth (nth 0 RBreturnlist))
			(setq blockheight (nth 1 RBreturnlist))
			
			(setq BLOCKHEIGHT (RoundUp BLOCKHEIGHT))
			(setq standalonesublist (cons BLOCKHEIGHT standalonesublist))
			(setq standalonesublist (cons BLOCKWIDTH standalonesublist))
			(setq standalonesublist (cons RNFreturnRoom standalonesublist))
			(setq trRoomsAndBlockInformation (cons standalonesublist trRoomsAndBlockInformation))
			(setq standalonesublist nil)
			(setq products nil)

			(setq TR-roomList(cons RNFreturnRoom TR-roomList))
			)																	;---end progn
		)																		;-- end if


		  	(setq numberOfProducts (read-line file1))
	  		(if
			  	(/= numberOfProducts nil)
			  	(progn
				(setq numberOfProducts (atoi numberOfProducts))
			  	(repeat numberOfProducts
				  	(progn
				  	(setq product (read-line file1))
				  	(if	(/= product nil)												;-- if statment to protect list from nil
						(setq products (cons product products))										;build products list
					)															;read the next line into product
					)
				)
				)
			)
		  	(setq roomLoopCheck TR-room)														;store TR-room to roomLoopCheck

		;;;final step, read next room from text file
		;;;now TR-room has the current room
		;;;roomLoopCheck has the room from the last iteration for testing purposes
		(setq TR-room (read-line file1))	  
	  	
	)
	(close file1)
	(setq  trfunctionreturnlist trRoomsAndBlockInformation)
  	
)






(defun GRD-standalonesort ( sortList  / ssFunctionReturnList )
	(setq ssFunctionReturnList (vl-sort sortList (function (lambda (x y)(< (car x)(car y))))))					;numerical sort of segmentdottedpairs, this organizes them by segment number
)





(defun GRD-standalone-inserter
       			(
			si-ssReturnList
		      	/
		      	blockexists XCOORDMAX HORIZONTALSPACEBETWEENBLOCKS VERTICALSPACEBETWEENROWS
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
;;;	GRD-standalonesort:
;;;	globalSTANDALONENUMERICAL
;;;	segmentbuilder:
;;;	globalSEGMENTSTOINSERT
;;;Global to
;;;  	None
  	
  
  	;;;standalone insert
    	(setq xcoord 0)
  	(setq xcoordMax 10000)
	(setq ycoord 0)
  	(setq horizontalSpaceBetweenBlocks 300)
  	(setq verticalSpaceBetweenRows 500)
  
  	(setq insertionPoint (list xcoord ycoord))
  	(setq c 0)
  	(setq cmax (length si-ssReturnList))
	(setq standaloneheightmax 0)
  	(while 	(< c cmax)
	  	(setq roomItem (nth c si-ssReturnList))
	  

	  		(if	(/= roomItem nil)
			  	(progn
			  	(setq roomname (car roomItem))
				(setq blockname (strcat "Z-RD-" roomname))
			  	(setq BLOCKWIDTH (nth 1 roomItem))
			  	(setq BLOCKHEIGHT (nth 2 roomItem))
		  		(entmakex											;entmakex function
				(list												;list of all required items
				(cons 0 "INSERT")										;type of entity
			        (cons 2 blockname)										;name of block to insert
				(cons 8 "RD-ROOMS")
			       	(cons 10 insertionPoint)									;block insertion point
			       	(cons 41 1)											; Scale Factor
			       	(cons 42 1)											; Scale Factor?
			      	(cons 43 1)											; Scale Factor?
				)
				)
				(setq xcoord (+ (+ BLOCKWIDTH xcoord ) horizontalSpaceBetweenBlocks ))
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
					(>= xcoord xcoordMax)
					)
				  	(progn	
					;;;move ycoord down
					(setq xcoord 0)
					(setq ycoord (-(- ycoord standaloneheightmax)verticalSpaceBetweenRows))
					(setq insertionPoint (list xcoord ycoord))
					(setq standaloneheightmax 0)
					)
				)
				
				)
			)
	  	(setq c (+ 1 c))
	)


  


)




(defun GRD-roomnameformat (string / CHARACTER CMAX CNTR FORMATCHECK STRING1 STRING2 STRING2CNTR STRINGCNTR formattedstring STRING3CNTR STRINGTOADD STRINGTOREMOVE)
;;;	arguments:
;;;  	string, sent in from EBLOCK
;;;  
;;;  	global variables:
;;;  	formattedstring
;;;  		used in EBLOCK immediatly after this function is called
;;;  		important not to localize this variable
;;;  		this variable is set to nil every time GRD-roomnameformat is called
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
	  	(setq string2cntr 2)
	  	(setq string3cntr (+ stringcntr 2))														;this string2cntr is used for the second half of the word
  		(setq stringToRemove (substr string stringcntr string2cntr))											;store stringToRemove to variable
	  

	  
		(if
			(equal stringToRemove "\\P")

		  	(progn
	  		(setq string1 (substr string 1 cntr))												;string1 is the portion of the string before the stringToRemove
			(setq string2 (substr string string3cntr cmax))												;string2 is the portion of the string after the stringToRemove
			
			(setq stringToAdd "--")															;if string is not any of the declared values, then set stringToRemove to _
			(setq string (strcat  string1 stringToAdd string2))
			
			)
		)
	    
	  	
	  	(setq cntr (+ 1 cntr))
	)

	(setq formattedString string)
  )








(defun GRD-bridge-builder
       			(
			xCoordMax yCoordMin yCoordMax BridgeAndInputList
			/
			 GENERALBRIDGEINFORMATIONLIST GENERALCONTROLLERINFORMATIONLIST XCOORDBRIDGESSTART YCOORDBRIDGESSTART
			GRD-CONSTANTBLOCKINFOLIST LMRJBLOCKNAME LMRJUPSIDEDOWNBLOCKNAME
			BLOCKEXISTS BLOCKNAME BRIDGEANDINPUT BRIDGEANDINPUTCONNECTIONPOINT1 BRIDGEANDINPUTCONNECTIONPOINT2 BRIDGEANDINPUTCONNECTIONPOINT3 BRIDGEANDINPUTCONNECTIONPOINT4 BRIDGEANDINPUTCONNECTIONPOINT5 BRIDGEANDINPUTCONNECTIONPOINT6 BRIDGEANDINPUTCONNECTIONPOINT7 BRIDGEANDINPUTCONNECTIONPOINT8 BRIDGEANDINPUTFORCONTROLLERCONNECTIONPOINT1 BRIDGEANDINPUTFORCONTROLLERCONNECTIONPOINT2 BRIDGEANDINPUTFORCONTROLLERCONNECTIONPOINT3 BRIDGEANDINPUTPORT1 BRIDGEANDINPUTPORT1PREVIOUS BRIDGEANDINPUTPORT2 BRIDGEANDINPUTPORT2PREVIOUS BRIDGEANDINPUTPORTFORCORNERMOUNTCONNECTION BRIDGEANDINPUTPORTFORPREVIOUSCONNECTION CLS CMAX CNTR INSERTIONPOINT POINT-LIST POLYLINE-LAYER POLYLINE-LINETYPE POLYLINE-WIDTH RETURNLIST SPLITTERCONNECTIONPOINTS xCoordBridges xCoordBridgesPORT1 xCoordBridgesPORT2 widthOffsetFromBridgeLMRJPortsForFilletRadius widthBetweenBridgeLMRJPorts horizontalSpaceBetweenBridges HorizontalConnectionExtension XCOORDCONTROLLERSTART XCOORDCURRENT XCOORDINPUT xDirectionBridges xShifterBridges yCoordBridges heightOffsetFromBridgeLMRJPortsForFilletRadius heightOffsetFromBridgeLMRJPortsForFilletRadius2 yCoordBridgesPORTS heightBetweenBridgeLMRJPortAndConnector verticalSpaceBetweenBridges YCOORDCURRENT DistanceToBridgeTop YCOORDINPUT verticalSpaceBetweenBridges yDirectionBridges LMRJBlockHeight yShifterBridges yShifterBridgesPREVIOUS
			)


	;;; use function to get constant block information
  	;;; makes adding new blocks easier
  	;;; keeps functions modular
  	(setq GRD-constantBlockInfoList (GRD-get-constant-block-info))
  	(setq LMRJBlockName (nth 0 GRD-constantBlockInfoList))
	(setq LMRJUpsideDownBlockName (nth 1 GRD-constantBlockInfoList))
  	(setq LMRJBlockHeight (nth 2 GRD-constantBlockInfoList))

	(setq cls 0)
  	(setq polyline-layer "RD-ROOMS")
  	(setq polyline-width 0.6)
  	(setq polyline-linetype "bylayer")
  
  	;;; use function to get information for starting controller functions
  	;;; makes editing values easier
  	;;; keeps functions modular
  	(setq generalControllerInformationList (GRD-get-general-controller-information))
	(setq xCoordControllerStart (nth 5 generalControllerInformationList))
  
  	;;; use function to get information for starting bridge functions
  	;;; makes editing values easier
  	;;; keeps functions modular
	(setq generalBridgeInformationList (GRD-get-general-bridge-information))
	(setq widthBetweenBridgeLMRJPorts (nth 0 generalBridgeInformationList))
	(setq heightBetweenBridgeLMRJPortAndConnector (nth 1 generalBridgeInformationList))
	(setq widthOffsetFromBridgeLMRJPortsForFilletRadius (nth 2 generalBridgeInformationList))
	(setq heightOffsetFromBridgeLMRJPortsForFilletRadius (nth 3 generalBridgeInformationList))
	(setq heightOffsetFromBridgeLMRJPortsForFilletRadius2 (nth 4 generalBridgeInformationList))
	(setq horizontalSpaceBetweenBridges (nth 5 generalBridgeInformationList))
	(setq verticalSpaceBetweenBridges (nth 6 generalBridgeInformationList))
	(setq DistanceToBridgeTop (nth 7 generalBridgeInformationList))
	(setq xCoordBridgesStart (nth 8 generalBridgeInformationList))
	(setq yCoordBridgesStart (nth 9 generalBridgeInformationList))
	(setq HorizontalConnectionExtension (nth 10 generalBridgeInformationList))

  	
  
	;;; calculate shifters
  	(setq xDirectionBridges 1)
  	(setq yDirectionBridges -1)
  	(setq xShifterBridges (* xDirectionBridges horizontalSpaceBetweenBridges))
  	(setq yShifterBridges (* yDirectionBridges verticalSpaceBetweenBridges))

  	
  
	;;;  	reset indexs and counters
	(setq cntr 0)
  	(setq cmax (length BridgeAndInputList))
  
  	;;; set starting points
  	(setq xCoordBridges xCoordBridgesStart)
	(setq yCoordBridges yCoordBridgesStart)     
  	(setq xCoordCurrent xCoordBridges)
  	(setq yCoordCurrent yCoordBridges)
  	(setq insertionpoint (list xCoordBridges yCoordBridges))

  
	;;;  	while loop for the Network Bridge and Input Devices
		(while	(< cntr cmax)															;while loop -- loop through the controllers
			(setq yShifterBridges (* yDirectionBridges verticalSpaceBetweenBridges))
		  
			(setq BridgeAndInput(nth cntr BridgeAndInputList))										;pull first product off list into BridgeAndInput




			;;; if catch
		  	(if
			  	;;; first item
			  	(= cntr 0)
				(progn
				;;; if catch
			  	(if
				  	;;; item is bridge
				  	(= BridgeAndInput "LMBC-300")
					(progn
				  	(setq xCoordBridges 30)
				  	(setq yCoordBridges -115)
					(setq insertionpoint (list xCoordBridges yCoordBridges))
					(setq blockname (strcat "RD-" BridgeAndInput))
					(GRD-entmod-blockinsert-attributes nil insertionpoint "RD-ROOMS" blockname)
					
				  	(setq xCoordBridgesPort1 xCoordBridges)
				  	(setq xCoordBridgesPort2 (+ xCoordBridgesPort1 widthBetweenBridgeLMRJPorts))
					(setq yCoordBridgesPorts (+ (+ yCoordBridges heightBetweenBridgeLMRJPortAndConnector) LMRJBlockHeight )) ; bridge is upside down, so add yspacer
				  	(setq BridgeAndInputPort1 (list xCoordBridgesPort1 yCoordBridgesPorts))
				  	(setq BridgeAndInputPort2 (list xCoordBridgesPort2 yCoordBridgesPorts))
					(GRD-entmod-blockinsert-attributes nil BridgeAndInputPort1 "RD-ROOMS" LMRJUpsideDownBlockName)
					(GRD-entmod-blockinsert-attributes nil BridgeAndInputPort2 "RD-ROOMS" LMRJUpsideDownBlockName)
				  	(setq insertionpoint (list xCoordBridges yCoordBridges))
					(setq yShifterBridges -82.2223)

					(setq BridgeAndInputforControllerConnectionPoint1 (list (+ xCoordBridgesPort2 widthOffsetFromBridgeLMRJPortsForFilletRadius) (- yCoordBridgesPorts  heightOffsetFromBridgeLMRJPortsForFilletRadius)))
					(setq BridgeAndInputforControllerConnectionPoint2 (list (+ xCoordBridgesPort2 widthOffsetFromBridgeLMRJPortsForFilletRadius) (+ yCoordBridgesPorts  heightOffsetFromBridgeLMRJPortsForFilletRadius2)))
					(setq BridgeAndInputforControllerConnectionPoint3 (list xCoordControllerStart (+ yCoordBridgesPorts  heightOffsetFromBridgeLMRJPortsForFilletRadius2)))

					(setq BridgeAndInputConnectionPoint1 (list (- xCoordBridgesPort1 widthOffsetFromBridgeLMRJPortsForFilletRadius) (- yCoordBridgesPorts  heightOffsetFromBridgeLMRJPortsForFilletRadius)))
					(setq BridgeAndInputConnectionPoint2 (list (- xCoordBridgesPort1 widthOffsetFromBridgeLMRJPortsForFilletRadius) (+ yCoordBridgesPorts  heightOffsetFromBridgeLMRJPortsForFilletRadius2)))
					(setq BridgeAndInputConnectionPoint3 (list (- xCoordBridgesPort1 HorizontalConnectionExtension) (+ yCoordBridgesPorts  heightOffsetFromBridgeLMRJPortsForFilletRadius2)))
					(setq BridgeAndInputConnectionPoint4 (list (- xCoordBridgesPort1 HorizontalConnectionExtension) (+ yCoordBridges (/ yShifterBridges 2))))
					)
				  	;;; item is anything other than bridge
				  	(progn
				  	(setq xCoordBridges 30)
				  	(setq yCoordBridges -97)
					(setq insertionpoint (list xCoordBridges yCoordBridges))
					(setq blockname (strcat "RD-" BridgeAndInput))
					(setq blockexists (tblsearch "block" blockname))

					(cond																	;conditional block
					  	;;;when block exists
					  	((/= blockexists nil)														;conditional statement

						;;;call GRD-entmod-blockinsert-attributes to insert the block
						(GRD-entmod-blockinsert-attributes nil insertionPoint "RD-ROOMS" blockname)
						)																;end conditional statement

						;;;when block does not exist
					  	((= blockexists nil)														;conditional statement
						;;;call GRD-entmod-blockinsert-attributes to insert the block
						(GRD-entmod-blockinsert-attributes (list BridgeAndInput) insertionPoint "RD-ROOMS" "RD-INTERFACE")								 
						)																;end conditional statement

					)
					

					  
				  	(setq xCoordBridgesPort1 xCoordBridges)
				  	(setq xCoordBridgesPort2 (+ xCoordBridgesPort1 widthBetweenBridgeLMRJPorts))
					(setq yCoordBridgesPorts (- (- yCoordBridges heightBetweenBridgeLMRJPortAndConnector) LMRJBlockHeight ))
				  	(setq BridgeAndInputPort1 (list xCoordBridgesPort1 yCoordBridgesPorts))
				  	(setq BridgeAndInputPort2 (list xCoordBridgesPort2 yCoordBridgesPorts))
					(GRD-entmod-blockinsert-attributes nil BridgeAndInputPort1 "RD-ROOMS" LMRJBlockName)
					(GRD-entmod-blockinsert-attributes nil BridgeAndInputPort2 "RD-ROOMS" LMRJBlockName)					
				  	(setq insertionpoint (list xCoordBridges yCoordBridges))


					(setq BridgeAndInputforControllerConnectionPoint1 (list (+ xCoordBridgesPort2 widthOffsetFromBridgeLMRJPortsForFilletRadius) (+ yCoordBridgesPorts  heightOffsetFromBridgeLMRJPortsForFilletRadius)))
					(setq BridgeAndInputforControllerConnectionPoint2 (list (+ xCoordBridgesPort2 widthOffsetFromBridgeLMRJPortsForFilletRadius) (- yCoordBridgesPorts  heightOffsetFromBridgeLMRJPortsForFilletRadius2)))
					(setq BridgeAndInputforControllerConnectionPoint3 (list xCoordControllerStart (- yCoordBridgesPorts  heightOffsetFromBridgeLMRJPortsForFilletRadius2)))
					
					(setq BridgeAndInputConnectionPoint1 (list (- xCoordBridgesPort1 widthOffsetFromBridgeLMRJPortsForFilletRadius) (+ yCoordBridgesPorts  heightOffsetFromBridgeLMRJPortsForFilletRadius)))
					(setq BridgeAndInputConnectionPoint2 (list (- xCoordBridgesPort1 widthOffsetFromBridgeLMRJPortsForFilletRadius) (- yCoordBridgesPorts  heightOffsetFromBridgeLMRJPortsForFilletRadius2)))
					(setq BridgeAndInputConnectionPoint3 (list (- xCoordBridgesPort1 HorizontalConnectionExtension) (- yCoordBridgesPorts  heightOffsetFromBridgeLMRJPortsForFilletRadius2)))
					(setq BridgeAndInputConnectionPoint4 (list (- xCoordBridgesPort1 HorizontalConnectionExtension) (+ yCoordBridges (/ yShifterBridges 2))))
					
					)
				)

				
				)
			  	;;; anything other than first item
			  	(progn

				;;; if catch
			  	(if
				  	;;; item is bridge
				  	(= BridgeAndInput "LMBC-300")
					(progn

					)
				  	;;; item is anything other than bridge
				  	(progn
					(setq insertionpoint (list xCoordBridges yCoordBridges))
					(setq blockname (strcat "RD-" BridgeAndInput))
					(setq blockexists (tblsearch "block" blockname))

					(cond																	;conditional block
					  	;;;when block exists
					  	((/= blockexists nil)														;conditional statement

						;;;call GRD-entmod-blockinsert-attributes to insert the block
						(GRD-entmod-blockinsert-attributes nil insertionPoint "RD-ROOMS" blockname)
						)																;end conditional statement

						;;;when block does not exist
					  	((= blockexists nil)														;conditional statement
						;;;call GRD-entmod-blockinsert-attributes to insert the block
						(GRD-entmod-blockinsert-attributes (list BridgeAndInput) insertionPoint "RD-ROOMS" "RD-INTERFACE")								 
						)																;end conditional statement

					)
				  	(setq xCoordBridgesPort1 xCoordBridges)
				  	(setq xCoordBridgesPort2 (+ xCoordBridgesPort1 widthBetweenBridgeLMRJPorts))
					(setq yCoordBridgesPorts (- (- yCoordBridges heightBetweenBridgeLMRJPortAndConnector) LMRJBlockHeight ))
				  	(setq BridgeAndInputPort1 (list xCoordBridgesPort1 yCoordBridgesPorts))
				  	(setq BridgeAndInputPort2 (list xCoordBridgesPort2 yCoordBridgesPorts))
					(GRD-entmod-blockinsert-attributes nil BridgeAndInputPort1 "RD-ROOMS" LMRJBlockName)
					(GRD-entmod-blockinsert-attributes nil BridgeAndInputPort2 "RD-ROOMS" LMRJBlockName)
				  	(setq insertionpoint (list xCoordBridges yCoordBridges))
					
					(setq BridgeAndInputPortforPreviousConnection BridgeAndInputPort1)
					(setq BridgeAndInputConnectionPoint5 (list (+ xCoordBridgesPort2 HorizontalConnectionExtension) (- yCoordBridges (/ yShifterBridgesPrevious 2))))
					(setq BridgeAndInputConnectionPoint6 (list (+ xCoordBridgesPort2 HorizontalConnectionExtension) (- yCoordBridgesPorts  heightOffsetFromBridgeLMRJPortsForFilletRadius2)))
					(setq BridgeAndInputConnectionPoint7 (list xCoordBridgesPort2 (- yCoordBridgesPorts  heightOffsetFromBridgeLMRJPortsForFilletRadius2)))
					(setq BridgeAndInputConnectionPoint8 (list xCoordBridgesPort2 (+ yCoordBridgesPorts  heightOffsetFromBridgeLMRJPortsForFilletRadius)))
					(setq point-list (list BridgeAndInputConnectionPoint1 BridgeAndInputConnectionPoint2 BridgeAndInputConnectionPoint3 BridgeAndInputConnectionPoint4 BridgeAndInputConnectionPoint5 BridgeAndInputConnectionPoint6 BridgeAndInputConnectionPoint7 BridgeAndInputConnectionPoint8))
					(GRD-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)

					(setq BridgeAndInputConnectionPoint1 (list (- xCoordBridgesPort1 widthOffsetFromBridgeLMRJPortsForFilletRadius) (+ yCoordBridgesPorts  heightOffsetFromBridgeLMRJPortsForFilletRadius)))
					(setq BridgeAndInputConnectionPoint2 (list (- xCoordBridgesPort1 widthOffsetFromBridgeLMRJPortsForFilletRadius) (- yCoordBridgesPorts  heightOffsetFromBridgeLMRJPortsForFilletRadius2)))
					(setq BridgeAndInputConnectionPoint3 (list (- xCoordBridgesPort1 HorizontalConnectionExtension) (- yCoordBridgesPorts  heightOffsetFromBridgeLMRJPortsForFilletRadius2)))
					(setq BridgeAndInputConnectionPoint4 (list (- xCoordBridgesPort1 HorizontalConnectionExtension) (+ yCoordBridges (/ yShifterBridges 2))))
					)
				)



				  

				)
			)

		  	;;; if catch
		  	(if
			  	;;; last item
			  	(= cntr (- cmax 1))
				(setq BridgeAndInputPortforCornerMountConnection BridgeAndInputPort1)

			)

		  	(setq yCoordCurrent (- yCoordBridges 25))

		  	;save pervious points for connection points 5,6,7,8
		  	(setq yShifterBridgesPrevious yShifterBridges)
			(setq yCoordBridges (+ yCoordBridges yShifterBridges))

			(setq BridgeAndInputPort1Previous BridgeAndInputPort1)
		  	(setq BridgeAndInputPort2Previous BridgeAndInputPort2)
		  	
			(setq cntr (+ cntr 1))														;increment counter

			(if
			  	(> xCoordCurrent xCoordMax)
			  	(progn
				(setq xCoordMax yCoordCurrent)
				)
			)
		  
			(if
			  	(< yCoordCurrent yCoordMin)
			  	(progn
				(setq yCoordMin yCoordCurrent)
				)
			)
		  
		)
		(setq SplitterConnectionPoints nil)

  		(setq returnlist (list xCoordMax yCoordMin BridgeAndInputforControllerConnectionPoint1 BridgeAndInputforControllerConnectionPoint2 BridgeAndInputforControllerConnectionPoint3 BridgeAndInputPortforCornerMountConnection SplitterConnectionPoints))
)














(defun GRD-block-builder 	(RB-RNFreturnRoom RBroom RBDLMSEGMENTMANAGERS RBDLMNETWORKBRIDGES RBDLMZONECONTROLLERS RBDLMPANELS RBDLMCONTROLLERS RBDLMPLUGCONTROLLERS
		    		RBControllers RBDLMINTERFACES RBDLMOCCSENSORS RBDLMOCCCORNERMOUNT RBDLMDAYLIGHT RBDLMSWITCHES RBDLMSPLITTERS RBREMAINDER RBUseableControllerPorts
				
		    		/
				 allProductsForCustomerVerification ALLPRODUCTSFORINSERTEDBOMSORTED PRODUCTITEM PRODUCTQTY RBPRODUCTLISTSORTED
				BBBLOCKHEIGHT BBBLOCKWIDTH BLOCKNAME BRIDGEANDINPUTFORCONTROLLERCONNECTIONPOINT1 BRIDGEANDINPUTFORCONTROLLERCONNECTIONPOINT2 BRIDGEANDINPUTFORCONTROLLERCONNECTIONPOINT3
				BRIDGEANDINPUTLIST BRIDGEANDINPUTPORTFORCORNERMOUNTCONNECTION CLS CMAX CNTR CONTROLLERLIST INSERTIONPOINT LISTSORTER ONEPORTLIST P1 P2 P3 P4 POINT-LIST POLYLINE-LAYER
				POLYLINE-LINETYPE POLYLINE-WIDTH PRODUCTNAME RBFUNCTIONRETURNLIST RBPRODUCTLIST RETURNEDLIST SPLITTERCONNECTIONPOINTS XCOORD XCOORDMAX YCOORD YCOORDMIN
				YCOORDCONTROLLERTOP YCOORDMAX VERTICALDISTANCEFROMDEFAULTORIGIN ALLPRODUCTSFORINSERTEDBOM RETURNLIST
		    		)

;;;	create compound lists
	(setq allProductsForInsertedBom	(append RBDLMNETWORKBRIDGES RBDLMCONTROLLERS RBDLMPLUGCONTROLLERS RBDLMINTERFACES RBDLMOCCSENSORS RBDLMOCCCORNERMOUNT RBDLMDAYLIGHT RBDLMSWITCHES RBDLMSPLITTERS))
  
	(setq BridgeAndInputList (append RBDLMNETWORKBRIDGES))
	(setq listsorter nil)
  	(foreach x BridgeAndInputList
	  	(if
		  	(/= x nil)
			(setq listsorter (cons x listsorter))
		)
	)
  	(setq BridgeAndInputList (reverse listsorter))

  
	(setq ControllerList (append RBDLMCONTROLLERS RBDLMPLUGCONTROLLERS))
	(setq listsorter nil)
  	(foreach x ControllerList
	  	(if
		  	(/= x nil)
			(setq listsorter (cons x listsorter))
		)
	)
  	(setq ControllerList (reverse listsorter))
  

	(setq OnePortList (append RBDLMOCCCORNERMOUNT RBDLMDAYLIGHT))
	(setq listsorter nil)
  	(foreach x OnePortList
	  	(if
		  	(/= x nil)
			(setq listsorter (cons x listsorter))
		)
	)
  	(setq OnePortList (reverse listsorter))

	(setq listsorter nil)
	(setq rbproductList (append RBDLMSEGMENTMANAGERS RBDLMNETWORKBRIDGES RBDLMZONECONTROLLERS RBDLMPANELS RBDLMCONTROLLERS RBDLMPLUGCONTROLLERS RBControllers RBDLMINTERFACES RBDLMOCCSENSORS RBDLMOCCCORNERMOUNT RBDLMDAYLIGHT RBDLMSWITCHES RBDLMSPLITTERS RBREMAINDER))
  	(foreach x rbproductList
	  	(if
		  	(/= x nil)
			(setq listsorter (cons x listsorter))
		)
	)
  	(setq rbproductList (reverse listsorter))
  
  	(setq xcoord 0)
	(setq ycoord 0)
	(setq insertionPoint (list xcoord ycoord))
  
  	(setq blockName (strcat "Z-RD-" RB-RNFreturnRoom))												;create string of roomname
	;;; BLOCK Header definition starts here:
	(entmake (list (cons 0 "BLOCK")(cons 2 blockName)(cons 70 2)(cons 10 insertionPoint)))								;begin block definition
	 
  	(setq xcoord 50)
	(setq ycoord -2.0)
	(setq insertionPoint (list xcoord ycoord))
  

    	(entmake
    	(list
      	(cons 0 "MTEXT")         		;; Entity Name
      	(cons 100 "AcDbEntity")  		;; Subclass Marker
      	(cons 410 "Model")       		;; Space
      	(cons 8 "Riser")         		;; Layer
      	(cons 100 "AcDbMText")   		;; Subclass Marker
      	(cons 10 insertionPoint) 		;; Insertion Point
      	(cons 40 3.0)            		;; Text Height
      	(cons 71 1)              		;; Attachment Point (top-center)
      	(cons 1 rbroom)    			;; Text Content
      	(cons 7 "Arial")			;; text style
      	)
   	)
 
	(setq ycoord (+ ycoord -0.4))

  	
  	(setq xCoordMax 0)
  	(setq yCoordMin 0)
  	(setq yCoordMax 0)
  
 	(setq returnedList(GRD-bridge-builder xCoordMax yCoordMin yCoordMax BridgeAndInputList))
	(setq xCoordMax (nth 0 returnedList))
  	(setq yCoordMin (nth 1 returnedList))
	(setq BridgeAndInputforControllerConnectionPoint1 (nth 2 returnedList)) 
	(setq BridgeAndInputforControllerConnectionPoint2 (nth 3 returnedList))
	(setq BridgeAndInputforControllerConnectionPoint3 (nth 4 returnedList))
	(setq BridgeAndInputPortforCornerMountConnection (nth 5 returnedList))
	(setq SplitterConnectionPoints (nth 6 returnedList))


  
	(setq returnedList(GRD-controller-builder xCoordMax yCoordMin yCoordMax ControllerList BridgeAndInputforControllerConnectionPoint1 BridgeAndInputforControllerConnectionPoint2 BridgeAndInputforControllerConnectionPoint3 RBUseableControllerPorts RBDLMINTERFACES RBDLMOCCSENSORS RBDLMSWITCHES RBDLMSPLITTERS OnePortList))
	(setq xCoordMax (nth 0 returnedList))
	(setq yCoordMin (nth 1 returnedList))
  	(setq yCoordMax (nth 2 returnedList))

	(if
	  	(< xCoordMax 200)
	  	(progn
		(setq xCoordMax 200)
		)
	)
  
	(if
	  	(= yCoordMin 0)
	  	(progn
		(setq yCoordMin -50)
		)
	)

  	(setq yCoordMax (+ yCoordMax 20))
  
	(setq p1 (list 0 yCoordMax))
  	(setq p2 (list xCoordMax yCoordMax))
  	(setq p3 (list xCoordMax yCoordMin))
  	(setq p4 (list 0 yCoordMin))
	(setq point-list (list p1 p2 p3 p4 p1))
  

  	(setq allProductsForInsertedBom (reverse allProductsForInsertedBom))
  	(setq returnList (GRD-combine-items-into-quantity-list allProductsForInsertedBom))
  	(setq allProductsForInsertedBomSorted returnList)
  
  	(setq cntr 0)
  	(setq cmax (length allProductsForInsertedBomSorted))
  	(setq xcoord 15)
  	(setq ycoord 15)
  	(while
	  	(< cntr cmax)


	  	;;; formate bill of materials header
	  	(if
		  	(= cntr 0)
		  	(progn
		  	(setq insertionPoint (list xcoord ycoord))
		  	(setq textString "Bill Of Materials")
			(entmake
			(list
			(cons 0 "MTEXT")         		;; Entity Name
			(cons 100 "AcDbEntity")  		;; Subclass Marker
			(cons 410 "Model")       		;; Space
			(cons 8 "RD-BOM")   ;; Layer
			(cons 100 "AcDbMText")   		;; Subclass Marker
			(cons 10 insertionPoint) 		;; Insertion Point
			(cons 40 3.0)            		;; Text Height
			(cons 71 1)              		;; Attachment Point (top-center)
			(cons 1 textString)    			;; Text Content
			(cons 7 "Arial")			;; text style
			)
			)

			(setq ycoord (- ycoord 5.0))
			)
		)




	  
		(setq productItem (nth cntr allProductsForInsertedBomSorted))
		(setq productName (car productItem))
	  	(setq productQTY (cdr productItem))
	  	(setq productQTY (itoa productQTY))
	  	(setq insertionPoint (list xcoord ycoord))
	  
		(entmake
		(list
		(cons 0 "MTEXT")         		;; Entity Name
		(cons 100 "AcDbEntity")  		;; Subclass Marker
		(cons 410 "Model")       		;; Space
		(cons 8 "RD-BOM")         		;; Layer
		(cons 100 "AcDbMText")   		;; Subclass Marker
		(cons 10 insertionPoint) 		;; Insertion Point
		(cons 40 3.0)            		;; Text Height
		(cons 71 1)              		;; Attachment Point (top-center)
		(cons 1 productName)    		;; Text Content
		(cons 7 "Arial")			;; text style
		)
		)
	  
	  	(setq xcoord (- xcoord 10.0))
	  	(setq insertionPoint (list xcoord ycoord))
	  	
		(entmake
		(list
		(cons 0 "MTEXT")         		;; Entity Name
		(cons 100 "AcDbEntity")  		;; Subclass Marker
		(cons 410 "Model")       		;; Space
		(cons 8 "RD-BOM")         		;; Layer
		(cons 100 "AcDbMText")   		;; Subclass Marker
		(cons 10 insertionPoint) 		;; Insertion Point
		(cons 40 3.0)            		;; Text Height
		(cons 71 1)              		;; Attachment Point (top-center)
		(cons 1 productQTY)    			;; Text Content
		(cons 7 "Arial")			;; text style
		)
		)

	  	(setq xcoord (+ xcoord 10.0))
	  	(setq ycoord (- ycoord 5.0))
	  
		(setq cntr (+ cntr 1))

 	)





  	(setq rbproductList (reverse rbproductList))
  	(setq returnList (GRD-combine-items-into-quantity-list rbproductList))
  	(setq rbproductListSorted returnList)

	(setq cntr 0)
  	(setq cmax (length  rbproductListSorted))
  	(setq xcoord 15)
  	(setq ycoord (- yCoordMin 5.0))
  	(while
	  	(< cntr cmax)

	  	;;; format bill of materials header
	  	(if
		  	(= cntr 0)
		  	(progn
		  	(setq insertionPoint (list xcoord ycoord))
		  	(setq textString "Engineer Verification Bom:\\PContains everything from floor plan extraction.\\PEven items out of Room Details current scope of work.")
			(entmake
			(list
			(cons 0 "MTEXT")         		;; Entity Name
			(cons 100 "AcDbEntity")  		;; Subclass Marker
			(cons 410 "Model")       		;; Space
			(cons 8 "RD-EXTRACTION-VERIFICATION")   ;; Layer
			(cons 100 "AcDbMText")   		;; Subclass Marker
			(cons 10 insertionPoint) 		;; Insertion Point
			(cons 40 3.0)            		;; Text Height
			(cons 71 1)              		;; Attachment Point (top-center)
			(cons 1 textString)    			;; Text Content
			(cons 7 "Arial")			;; text style
			)
			)

			(setq ycoord (- ycoord 15.0))
			)
		)
		(setq productItem (nth cntr rbproductListSorted))

		(setq productName (car productItem))
	  	(setq productQTY (cdr productItem))
	  	(setq productQTY (itoa productQTY))
	  
	  	(setq insertionPoint (list xcoord ycoord))
	  
		(entmake
		(list
		(cons 0 "MTEXT")         		;; Entity Name
		(cons 100 "AcDbEntity")  		;; Subclass Marker
		(cons 410 "Model")       		;; Space
		(cons 8 "RD-EXTRACTION-VERIFICATION")   ;; Layer
		(cons 100 "AcDbMText")   		;; Subclass Marker
		(cons 10 insertionPoint) 		;; Insertion Point
		(cons 40 3.0)            		;; Text Height
		(cons 71 1)              		;; Attachment Point (top-center)
		(cons 1 productName)    		;; Text Content
		(cons 7 "Arial")			;; text style
		)
		)
	  
	  	(setq xcoord (- xcoord 10.0))
	  	(setq insertionPoint (list xcoord ycoord))
	  	
		(entmake
		(list
		(cons 0 "MTEXT")         		;; Entity Name
		(cons 100 "AcDbEntity")  		;; Subclass Marker
		(cons 410 "Model")       		;; Space
		(cons 8 "RD-EXTRACTION-VERIFICATION")   ;; Layer
		(cons 100 "AcDbMText")   		;; Subclass Marker
		(cons 10 insertionPoint) 		;; Insertion Point
		(cons 40 3.0)            		;; Text Height
		(cons 71 1)              		;; Attachment Point (top-center)
		(cons 1 productQTY)    			;; Text Content
		(cons 7 "Arial")			;; text style
		)
		)

	  	(setq xcoord (+ xcoord 10.0))
	  	(setq ycoord (- ycoord 5.0))
		(setq cntr (+ cntr 1))

 	)

  	;;;account for text
	(setq ycoord (- ycoord 5.0))
  
  	;;; add extra space to maxs to move them off the edge of block
  	(setq xCoordMax (+ xCoordMax 40.0))
  	(setq yCoordMin (- ycoord 40.0))


	(setq bbBlockWidth xCoordMax)
  	(setq bbBlockHeight (abs yCoordMin))


	(cond
	  	((= GRDglobalSheetSize 1)
		(setq polyline-layer "Modelspace_Viewports_8X11.5")
		)
	  	((= GRDglobalSheetSize 2)
		(setq polyline-layer "Modelspace_Viewports_11X17")
		)
	  	((= GRDglobalSheetSize 3)
		(setq polyline-layer "Modelspace_Viewports_22X34")
		)
	  	((= GRDglobalSheetSize 4)
		(setq polyline-layer "Modelspace_Viewports_36X48")
		)
	)
  
	(setq cls 0)
  	(setq polyline-width 0.6)
  	(setq polyline-linetype "bylayer")
  
	(GRD-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)
  	(entmake
	(list
	(cons 0 "ENDBLK")
	)
	)  															;finish block defition

	(if
	  	(/= yCoordMax 20)
	  	(progn
		(setq VerticalDistanceFromDefaultOrigin (- 20 yCoordMax))
		(GRD-move-entities-in-block blockName  VerticalDistanceFromDefaultOrigin)
		(setq bbBlockHeight (abs (+ yCoordMin VerticalDistanceFromDefaultOrigin)))
		)
	)
	(setq rbfunctionreturnlist (list bbBlockWidth bbBlockHeight)) 

  
)



(defun GRD-combine-items-into-quantity-list ( nameList / LISTNEWLENGTH LISTTOTALLENGTH OUTLIST RETURNLIST )


      (setq listTotalLength  (length nameList))
      (while nameList												;while nameList still exists
	(setq outList (cons (cons (car nameList)								;create the outList from the first item of nameList and the number of items that match it, accumlate each of these values for the entire list
				  (- listTotalLength								;find the number of items that match the first item, by subtract the listTotalLength, minus the listNewLength
				     (setq listTotalLength (length (setq nameList				;set nameList to new list with all of the items that matched the first element removed
								  (vl-remove (car nameList) nameList)		;start evaluation of this section here, remove any items that match the first element of nameList, car returns the first element
							   ) ;_ end of setq
						   ) ;_ end of length
				     ) ;_ end of setq
				  ) ;_ end of -
			    ) ;_ end of cons
			    outList
		      ) ;_ end of cons
	) ;_ end of setq
      ) ;_ end of while

	(setq returnlist outList)
  
)





(defun GRD-move-entities-in-block ( blk  VerticalDistanceFromDefaultOrigin / ent lst DXF0 DXF10 DXF11 DXF8 ENTDXF NEWYCOORD XCOORD YCOORD)
    ;; Define the function, declare local variables
    
    (if ;; If the following returns a non-nil value
        ;; i.e. if the block exists in the drawing

        (setq ent (tblobjname "block" blk)) ;; get the BLOCK entity

        (while (setq ent (entnext ent))
            ;; Step through the entities in the block definition
	  
		(setq entdxf(entget ent))						;set dxf codes of ent to entdxf
		(setq dxf0 (cdr (assoc 0 entDXF )))					;set dxf0 to the 0 element of the item that has 0 as it's first element, this is the object type
		(setq dxf10 (cdr (assoc 10 entDXF )))					;set dxf8 to the second element of the item that has 8 as it's first element, this is the insertion point
		(setq dxf8 (cdr (assoc 8 entDXF )))
	  	(if
		  	(= dxf0 "INSERT")
		  	(progn
			(setq xcoord (nth 0 dxf10))
			(setq ycoord (nth 1 dxf10))
			(setq newYcoord (+ ycoord VerticalDistanceFromDefaultOrigin))
			(setq entdxf							;change the dxfcodes on entdxf
				(subst
				(cons 10 (list xcoord newYcoord))
				(assoc 10 entdxf)
				entdxf)
			)
			(entmod entdxf)
			
			)
		)
	  
	  	(if
		  	(AND (= dxf0 "MTEXT"))
		  	(progn
			(setq xcoord (nth 0 dxf10))
			(setq ycoord (nth 1 dxf10))
			(setq newYcoord (+ ycoord VerticalDistanceFromDefaultOrigin))
			(setq entdxf							;change the dxfcodes on entdxf
				(subst
				(cons 10 (list xcoord newYcoord))
				(assoc 10 entdxf)
				entdxf)
			)
			(entmod entdxf)
			)
		)
	  
	  	(if
		  	(AND (= dxf0 "LWPOLYLINE"))
			(progn




			(foreach flag entdxf                                              				;look for every flag (10 dxf) in the dxfRectangle list
				(if	(= 10 (car flag))                                         			;if the 11 dxf exists 
					(progn
					(setq dxf10 (cdr flag))
					(setq xcoord (nth 0 dxf10))
					(setq ycoord (nth 1 dxf10))
					(setq newYcoord (+ ycoord VerticalDistanceFromDefaultOrigin))
					(setq entdxf							;change the dxfcodes on entdxf
						(subst
						(cons 10 (list xcoord newYcoord))
						flag
						entdxf)
					)
					
					)
				)                                                                                	;close the if statement
			  	
			)

			(entmod entdxf)




			)		  
		)
	  
	  	(if
		  	(AND (= dxf0 "ATTRIB"))
			(progn


			(foreach flag entdxf                                              				;look for every flag (10 dxf) in the dxfRectangle list
				(if	(= 10 (car flag))                                         			;if the 11 dxf exists 
					(progn
					(setq dxf10 (cdr flag))
					(setq xcoord (nth 0 dxf10))
					(setq ycoord (nth 1 dxf10))
					(setq newYcoord (+ ycoord VerticalDistanceFromDefaultOrigin))
					(setq entdxf							;change the dxfcodes on entdxf
						(subst
						(cons 10 (list xcoord newYcoord))
						flag
						entdxf)
					)
					
					)
				)                                                                                	;close the if statement
			  	
			)
			(foreach flag entdxf                                              				;look for every flag (10 dxf) in the dxfRectangle list
				(if	(= 11 (car flag))                                         			;if the 11 dxf exists 
					(progn
					(setq dxf11 (cdr flag))
					(setq xcoord (nth 0 dxf11))
					(setq ycoord (nth 1 dxf11))
					(setq newYcoord (+ ycoord VerticalDistanceFromDefaultOrigin))
					(setq entdxf							;change the dxfcodes on entdxf
						(subst
						(cons 11 (list xcoord newYcoord))
						flag
						entdxf)
					)
					
					)
				)                                                                                	;close the if statement
			  	
			)
			(entmod entdxf)




			)		  
		)	  

            
        ) ;; end WHILE
        
    ) ;; end IF
    

    
) ;; end DEFUN



(defun GRD-controller-builder
       				(
				xCoordMax yCoordMin yCoordMax ControllerList BridgeAndInputforControllerConnectionPoint1 BridgeAndInputforControllerConnectionPoint2 BridgeAndInputforControllerConnectionPoint3 RBUseableControllerPorts RBINTERFACES RBDLMOCCSENSORS RBDLMSWITCHES RBSPLITTERS RBOnePortList  	

				/
				 YCOORDCONTROLLERTOP
				 GENERALCONTROLLERINFORMATIONLIST GRD-CONSTANTBLOCKINFOLIST LMRJBLOCKNAME LMRJUPSIDEDOWNBLOCKNAME YCOORDCONTROLLERSTART
				BLOCKNAME CDRETURNLIST CLS CMAX CMAX2 CMAX3 CMAX4 CNTR CNTR2 CNTR3 CNTR4 CNTRFORLEFT CONTROLLER numberOfLMRJPortsOnCurrentController FIRSTLMRJPOINT INSERTIONPOINT LARETURNLIST LASTCONTROLLER LOOPONETIME ONEPORTLIST PERPORTOCC PERPORTSW POINT-LIST POINTCONTROLLERCONNECTION1 POINTCONTROLLERCONNECTION1PREVIOUS POINTCONTROLLERCONNECTION1PREVIOUS2 POINTCONTROLLERCONNECTION1PREVIOUS3 POINTCONTROLLERCONNECTION1PREVIOUS4 POINTCONTROLLERCONNECTION2 POINTCONTROLLERCONNECTION3 POINTCONTROLLERCONNECTION4 POINTCONTROLLERTOBRIDGEINPUTCONNECTION4 POINTVERTICALCONTROLLERCONNECTION1 POINTVERTICALCONTROLLERCONNECTION2 POINTVERTICALCONTROLLERCONNECTION3 POINTVERTICALCONTROLLERCONNECTION4 POINTVERTICALCONTROLLERCONNECTION5 POINTVERTICALCONTROLLERCONNECTION6 POLYLINE-LAYER POLYLINE-LINETYPE POLYLINE-WIDTH RBRETURNLIST RBRETURNYDISTANCE RBRETURNYDISTANCEMAX RETURNLIST SECONDLMRJPOINT XCOORDCONTROLLER XCOORDCONTROLLERMAX XCOORDCONTROLLERMIN widthBetweenControllerLMRJPorts XCOORDCONTROLLERPREVIOUS horizontalSpaceBetweenControllers XCOORDCONTROLLERSTART XCOORDCURRENT controllerCableLeftEdgeExtension XCOORDPORT2 XCOORDPORT3 XCOORDPORTFIRST XCOORDPORTFIRSTFORLEFT XCOORDPORTFIRSTFUTURE XCOORDPORTFIRSTPREVIOUS XCOORDPORTLAST XCOORDPORTLASTFUTURE XCOORDPORTLASTPREVIOUS controllerCableRightEdgeExtension XDIRECTIONCONTROLLER DistanceToLeftSideOfController DistanceToLeftSideOfControllerFUTURE DistanceToLeftSideOfControllerPREVIOUS DistanceToRightSideOfController DistanceToRightSideOfControllerFUTURE DistanceToRightSideOfControllerPREVIOUS XSHIFTERCONTROLLER YCOORDCONTROLLER ControllerHeight ControllerHeightMAX heightBetweenLMRJPortAndConnector verticalSpaceBetweenControllers YCOORDCURRENT DistanceToControllerBottom DistanceToControllerBottomPREVIOUS DistanceToControllerTop DistanceToControllerTop yCoordLMRJConnector yCoordLMRJConnectorFORLEFT YDIRECTIONCONTROLLER LMRJBlockHeight YSHIFTERCONTROLLER
				)

	;;; use function to get constant block information
  	;;; makes adding new blocks easier
  	;;; keeps functions modular
  	(setq GRD-constantBlockInfoList (GRD-get-constant-block-info))
  	(setq LMRJBlockName (nth 0 GRD-constantBlockInfoList))
	(setq LMRJUpsideDownBlockName (nth 1 GRD-constantBlockInfoList))
  	(setq LMRJBlockHeight (nth 2 GRD-constantBlockInfoList))

	;;; use function to get information for starting controller functions
  	;;; makes editing values easier
  	;;; keeps functions modular
  	(setq generalControllerInformationList (GRD-get-general-controller-information))
	(setq widthBetweenControllerLMRJPorts (nth 0 generalControllerInformationList))
	(setq heightBetweenLMRJPortAndConnector (nth 1 generalControllerInformationList))
	(setq LMRJBlockHeight (nth 2 generalControllerInformationList))
	(setq horizontalSpaceBetweenControllers (nth 3 generalControllerInformationList))
	(setq verticalSpaceBetweenControllers (nth 4 generalControllerInformationList))
	(setq xCoordControllerStart (nth 5 generalControllerInformationList))
	(setq yCoordControllerStart (nth 6 generalControllerInformationList))
	(setq xCoordControllerMin (nth 7 generalControllerInformationList))
	(setq xCoordControllerMax (nth 8 generalControllerInformationList))
	(setq controllerCableLeftEdgeExtension (nth 9 generalControllerInformationList))
	(setq controllerCableRightEdgeExtension (nth 10 generalControllerInformationList))
	       
	(setq cls 0)
  	(setq polyline-layer "RD-ROOMS")
  	(setq polyline-width 0.6)
  	(setq polyline-linetype "bylayer")


  	;;; calculate starting variables for loop
  	;;; these calculations are best done directly before loop, not in a getter function
	(setq xCoordController xCoordControllerStart)
	(setq yCoordController yCoordControllerStart)
  	(setq insertionpoint (list xCoordController yCoordController))
  	(setq xDirectionController 1)
  	(setq yDirectionController -1)

 ;;;RBINTERFACES RBDLMOCCSENSORS RBDLMSWITCHES RBSPLITTERS
  	(if
	  	(AND
		(/= RBUseableControllerPorts nil)(> RBUseableControllerPorts 1)
		)
	  	(progn

		(if
			(/= RBINTERFACES nil)
			(progn
			(setq perPortInt (/ (* (length RBINTERFACES) 1.0 ) (- RBUseableControllerPorts 1)))
			(setq perPortInt (RoundUp perPortInt))
			)
		)
		
		(if
			(/= RBDLMOCCSENSORS nil)
			(progn
			(setq perPortOcc (/ (* (length RBDLMOCCSENSORS) 1.0 ) (- RBUseableControllerPorts 1) ))
			(setq perPortOcc (RoundUp perPortOcc))
			)
		)
		
		(if
			(/= RBDLMSWITCHES nil)
			(progn
			(setq perPortSw (/ (* (length RBDLMSWITCHES) 1.0 ) (- RBUseableControllerPorts 1) ))
			(setq perPortSw (RoundUp perPortSw))
			)
		)
		)
	)
  
  	(if

		(= RBUseableControllerPorts 1)
	  	(progn
		(if
			(/= RBINTERFACES nil)
			(progn
			(setq perPortInt (length RBINTERFACES))
			)
		)
		
		(if
			(/= RBDLMOCCSENSORS nil)
			(progn
			(setq perPortOcc (length RBDLMOCCSENSORS))
			)
		)
		
		(if
			(/= RBDLMSWITCHES nil)
			(progn
			(setq perPortSw (length RBDLMSWITCHES))
			)
		)
		
		)
	)


  
  	;;; reset counters
	(setq cntr 0)
  	(setq cntr1 0)
  	(setq cntr2 0)
  	(setq cntr3 0)
  	(setq cntr4 0)
  	;;; calculate max's
	(setq cmax (length ControllerList))
  	(setq cmax1 (length RBINTERFACES))
	(setq cmax2 (length RBDLMOCCSENSORS))
	(setq cmax3 (length RBDLMSWITCHES))
	(setq cmax4 (length RBOnePortList))

  	;;; initialize maxs, mins and current coordinate holders
   	(setq xCoordCurrent xCoordController)
  	(setq yCoordCurrent yCoordController)
  	(setq RBreturnYdistanceMax 0)
  	(setq RBreturnYdistance 0)

  	;;; assume lastController is false, 0
  	;;; loop will calculate
  	(setq lastController 0)
  
	;;;  	while loop for the controllers
		(while	(< cntr cmax)															;while loop -- loop through the controllers

		  
			(setq Controller(nth cntr ControllerList))											;pull first product off list into Controller

		  	(if
			  	(= cntr (- cmax 1))
				(setq lastController 1)
			)
			

		  	(setq insertionpoint (list xCoordController yCoordController))
			(setq xCoordPort2 nil)
			(setq xCoordPort3 nil)

			(setq blockname (strcat "RD-" Controller))
			(GRD-entmod-blockinsert-attributes nil insertionpoint "RD-ROOMS" blockname)		  


			(setq cdReturnList (GRD-controller-dimensions Controller xCoordController yCoordController))
			
			(setq  xCoordPortFirst (nth 0 cdReturnList))
			(setq  xCoordPort2 (nth 1 cdReturnList))
			(setq  xCoordPort3 (nth 2 cdReturnList))
			(setq  xCoordPortLast (nth 3 cdReturnList))
			(setq  DistanceToLeftSideOfController (nth 4 cdReturnList))
			(setq  DistanceToRightSideOfController (nth 5 cdReturnList))
			(setq  DistanceToControllerTop (nth 6 cdReturnList))
			(setq  DistanceToControllerBottom (nth 7 cdReturnList))
			(setq  yCoordLMRJConnector (nth 8 cdReturnList))
			(setq  ControllerHeight (nth 9 cdReturnList))
		  	(setq  numberOfLMRJPortsOnCurrentController (nth 10 cdReturnList))
						

			(setq yCoordControllerTop (+ yCoordController DistanceToControllerTop))


		  	(if
			  	(> yCoordControllerTop YcoordMax)
			  	(progn
				(setq YcoordMax yCoordControllerTop)
				)
			)
		  
		  	;;; if catch
		  	(if
			  	;;;first item
			  	(= cntr 0)	
			  	(progn																				
				(if
				  	;;; if points exists
				  	(/= BridgeAndInputforControllerConnectionPoint1 nil)
				  	;connect to last Bridge or Input device
				  	(progn
					(setq pointControllertoBridgeInputConnection4 (list xCoordPortFirst yCoordLMRJConnector))
					(setq point-list (list BridgeAndInputforControllerConnectionPoint1 BridgeAndInputforControllerConnectionPoint2 BridgeAndInputforControllerConnectionPoint3 pointControllertoBridgeInputConnection4))
					(GRD-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)
					(setq firstLMRJPoint pointControllertoBridgeInputConnection4)
					(GRD-entmod-blockinsert-attributes nil firstLMRJPoint "RD-ROOMS" LMRJBlockName)
					)


				)

				  

				)
			)

		  	;;; if catch
		  	(if
			  	;;;first item
			  	(= cntr 0)	
			  	(progn																				
				(if
				  	;;; if points don't exist
				  	(= BridgeAndInputforControllerConnectionPoint1 nil)
				  	;;; use it for DLM items
					(progn
					;;; EDITED 09-28-2017
					(setq BridgeAndInputPortforCornerMountConnection (list xCoordPortFirst yCoordController))
					)


				)

				  

				)
			)






		  
			(setq RBreturnList (GRD-rung-builder Controller perPortInt cntr1 cmax1 RBINTERFACES perPortOcc cntr2 cmax2 RBDLMOCCSENSORS perPortSw cntr3 cmax3 RBDLMSWITCHES RBOnePortList cntr4 cmax4 RBSPLITTERS RBUseableControllerPorts lastController xCoordPort2 xCoordPort3 xCoordPortFirst xCoordPortLast yCoordLMRJConnector DistanceToControllerBottom numberOfLMRJPortsOnCurrentController xDirectionController BridgeAndInputPortforCornerMountConnection))
			(setq cntr1 (nth 0 RBreturnList))
		  	(setq cntr2 (nth 1 RBreturnList))
		  	(setq cntr3 (nth 2 RBreturnList))
		  	(setq cntr4 (nth 3 RBreturnList))
		  	(setq RBreturnYdistance (nth 4 RBreturnList))



		  
		  	(if
			  	(> RBreturnYdistance RBreturnYdistanceMax)
			  	(progn
				(setq RBreturnYdistanceMax RBreturnYdistance)
				)
			)

			(setq xCoordCurrent (+ (+ xCoordPortFirst DistanceToRightSideOfController) controllerCableRightEdgeExtension))
			(setq yCoordCurrent (- (- (- yCoordLMRJConnector DistanceToControllerBottom) verticalSpaceBetweenControllers ) RBreturnYdistanceMax))


		  
			(setq xCoordControllerPrevious xCoordController )

			(if
			  	(/= cntr (- cmax 1))
				(progn
			  	(setq loopOneTime 1)
				(setq laReturnList (GRD-controller-builder-look-ahead xDirectionController loopOneTime xCoordController xCoordControllerMax xCoordControllerMin xCoordPortFirst xCoordPortLast DistanceToLeftSideOfController DistanceToRightSideOfController DistanceToControllerBottom ControllerList yCoordController cntr cmax))
				(setq  xShifterController  (nth 0 laReturnList))
				(setq  ControllerHeightMax (nth 1 laReturnList))
				(setq  xCoordPortFirstFuture (nth 2 laReturnList))
				(setq  xCoordPortLastFuture (nth 3 laReturnList))
			  	(setq DistanceToLeftSideOfControllerFuture (nth 4 laReturnList))
				(setq DistanceToRightSideOfControllerFuture (nth 5 laReturnList))
	 		  
			  	;;; if catch
			  	;;; first part of both inbounds situations
			  	;;; save connection and LMRJ points
			  	(if
				  	;;; moving right
				  	(= xDirectionController 1)																	;test for moving left or right
					(progn																				;connection points for moving right
					
					(setq pointControllerConnection1 (list xCoordPortLast yCoordLMRJConnector))
					(setq pointControllerConnection2 (list xCoordPortLast (- yCoordLMRJConnector DistanceToControllerBottom)))
				  	(setq xCoordControllerPrevious xCoordController)

					;;; use variables set by look ahead to next controller to determine this connection point, rather than look at previous
					(setq xShifterController (+ (+ (* xDirectionController horizontalSpaceBetweenControllers) DistanceToRightSideOfController ) DistanceToLeftSideOfControllerFuture))
				  	(setq xCoordController (+ xShifterController xCoordController))
				  	(setq pointControllerConnection3 (list xCoordController (- yCoordLMRJConnector DistanceToControllerBottom)))
				  	(setq pointControllerConnection4 (list xCoordController yCoordLMRJConnector))
					(setq point-list (list pointControllerConnection1 pointControllerConnection2 pointControllerConnection3  pointControllerConnection4))				;save points for connecting line between controllers
					(setq firstLMRJPoint pointControllerConnection1)														;save points for LMRJ block
					(setq secondLMRJPoint pointControllerConnection4)
					)
				  	;;; moving left
					(progn																				;connection points for moving left
					;;;start connection, this is the only cable done in two parts
					;;; use variables set by look ahead to next controller to determine this connection point, rather than look at previous
					(setq xShifterController (- (- (* xDirectionController horizontalSpaceBetweenControllers) DistanceToRightSideOfControllerFuture) DistanceToLeftSideOfController))

				  	(setq xCoordControllerPrevious xCoordController)
				  	(setq xCoordController (+ xShifterController xCoordController))


					(setq xCoordPortFirstforLeft xCoordPortFirst)
					(setq yCoordLMRJConnectorforLeft yCoordLMRJConnector)
					(setq cntrforLeft (+ cntr 1))
					(if
					  	(< cntrforLeft cmax)
					  	(progn
						(setq Controller(nth cntrforLeft ControllerList))
						
						(setq cdReturnList (GRD-controller-dimensions Controller xCoordController yCoordController))
						(setq  xCoordPortFirst (nth 0 cdReturnList))
						(setq  xCoordPort2 (nth 1 cdReturnList))
						(setq  xCoordPort3 (nth 2 cdReturnList))
						(setq  xCoordPortLast (nth 3 cdReturnList))
						(setq  DistanceToControllerBottom (nth 7 cdReturnList))
						(setq  yCoordLMRJConnector (nth 8 cdReturnList))
						
						(setq pointControllerConnection1 (list xCoordPortLast yCoordLMRJConnector))
					  	(setq pointControllerConnection2 (list xCoordPortLast (- yCoordLMRJConnector DistanceToControllerBottom)))
						(setq firstLMRJPoint pointControllerConnection1)													;save points for LMRJ block
					  	(setq secondLMRJPoint pointControllerConnection1Previous4)
						(setq point-list (list pointControllerConnection1 pointControllerConnection2 pointControllerConnection1Previous3 pointControllerConnection1Previous4))

						
						(setq pointControllerConnection1Previous3 (list xCoordPortFirst (- yCoordLMRJConnector DistanceToControllerBottom)))
	 					(setq pointControllerConnection1Previous4 (list xCoordPortFirst yCoordLMRJConnector))
																				
						
						(setq  xCoordPortFirst xCoordPortFirstforLeft)
						(setq  yCoordLMRJConnector yCoordLMRJConnectorforLeft)
						(setq Controller(nth cntr ControllerList))
						

						)
					)
					
					)
				)
			  
			  	



			  

			  	;;; if catch
			  	;;; for out of bounds situation
			  	;;; draw connection points here
			  	;;; else catch
			  	;;; second part of in bounds situations, draw connections from saved points
			  	(if
				  	;;; out of bounds
				  	(OR
					(> xCoordController xCoordControllerMax)(< xCoordController xCoordControllerMin)										;if xcoordinate is out of bounds
					)
				  	(progn																				;move back in bounds
					(setq xCoordController xCoordControllerPrevious)


					(setq xDirectionController (* xDirectionController -1))
					
				  	(setq loopOneTime 0)
					(setq laReturnList (GRD-controller-builder-look-ahead xDirectionController loopOneTime xCoordController xCoordControllerMax xCoordControllerMin xCoordPortFirst xCoordPortLast DistanceToLeftSideOfController DistanceToRightSideOfController DistanceToControllerBottom ControllerList yCoordController cntr cmax))

					(setq  ControllerHeightMax (nth 1 laReturnList))
					(setq  xCoordPortFirstFuture (nth 2 laReturnList))
					(setq  xCoordPortLastFuture (nth 3 laReturnList))
					(setq  ControllerHeight ControllerHeightMax)
					(setq xDirectionController (* xDirectionController -1))
					;;; if catch
				  	(if
					  	;;; moving right
					  	(= xDirectionController 1)																;test for moving left or right
						(progn																			;connection points for moving right
						(setq pointVerticalControllerConnection1 (list xCoordPortLast yCoordLMRJConnector))
						(setq pointVerticalControllerConnection2 (list xCoordPortLast (- yCoordLMRJConnector DistanceToControllerBottomPrevious)))
						(setq pointVerticalControllerConnection3 (list (+ xCoordPortLast controllerCableRightEdgeExtension) (- yCoordLMRJConnector DistanceToControllerBottomPrevious)))
						(setq pointVerticalControllerConnection4 (list (+ xCoordPortLast controllerCableRightEdgeExtension) (- (- (- (- yCoordLMRJConnector DistanceToControllerBottomPrevious) verticalSpaceBetweenControllers ) ControllerHeight ) RBreturnYdistanceMax)))
						(setq pointVerticalControllerConnection5 (list xCoordPortLastFuture (- (- (- (- yCoordLMRJConnector DistanceToControllerBottomPrevious) verticalSpaceBetweenControllers ) ControllerHeight) RBreturnYdistanceMax)))
						(setq pointVerticalControllerConnection6 (list xCoordPortLastFuture (- (+ (- (- (- yCoordLMRJConnector DistanceToControllerBottomPrevious) verticalSpaceBetweenControllers ) ControllerHeight ) DistanceToControllerBottom) RBreturnYdistanceMax)))
						(setq point-list (list pointVerticalControllerConnection1 pointVerticalControllerConnection2 pointVerticalControllerConnection3 pointVerticalControllerConnection4 pointVerticalControllerConnection5 pointVerticalControllerConnection6))
						(setq firstLMRJPoint pointVerticalControllerConnection1)
						(setq secondLMRJPoint pointVerticalControllerConnection6)

						(GRD-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)
						(setq pointControllerConnection1Previous (list xCoordPortFirstFuture (- (+ (- (- (- yCoordLMRJConnector DistanceToControllerBottomPrevious) verticalSpaceBetweenControllers ) ControllerHeight ) DistanceToControllerBottom) RBreturnYdistanceMax)))
						(setq pointControllerConnection1Previous2 (list xCoordPortFirstFuture (- (- (- (- yCoordLMRJConnector DistanceToControllerBottomPrevious) verticalSpaceBetweenControllers ) ControllerHeight) RBreturnYdistanceMax)))

						(setq pointControllerConnection1Previous4 pointControllerConnection1Previous)
						(setq pointControllerConnection1Previous3 pointControllerConnection1Previous2)
						)
					  
					  	;;;;moving left
						(progn																			;connection points for moving right
						;;;finish connection started during move left, this is the only cable done in two parts
					
						(setq pointVerticalControllerConnection1 (list xCoordPortFirst yCoordLMRJConnector))
						(setq pointVerticalControllerConnection2 (list xCoordPortFirst (- yCoordLMRJConnector DistanceToControllerBottomPrevious)))
						(setq pointVerticalControllerConnection3 (list (- xCoordPortFirst controllerCableLeftEdgeExtension) (- yCoordLMRJConnector DistanceToControllerBottomPrevious)))
						(setq pointVerticalControllerConnection4 (list (- xCoordPortFirst controllerCableLeftEdgeExtension) (- (- (- (- yCoordLMRJConnector DistanceToControllerBottomPrevious) verticalSpaceBetweenControllers ) ControllerHeight) RBreturnYdistanceMax)))
						(setq pointVerticalControllerConnection5 (list xCoordPortFirst (- (- (- (- yCoordLMRJConnector DistanceToControllerBottomPrevious) verticalSpaceBetweenControllers ) ControllerHeight) RBreturnYdistanceMax)))
						(setq pointVerticalControllerConnection6 (list xCoordPortFirst (- (+ (- (- (- yCoordLMRJConnector DistanceToControllerBottomPrevious) verticalSpaceBetweenControllers ) ControllerHeight ) DistanceToControllerBottom) RBreturnYdistanceMax)))
						(setq point-list (list pointVerticalControllerConnection1 pointVerticalControllerConnection2 pointVerticalControllerConnection3 pointVerticalControllerConnection4 pointVerticalControllerConnection5 pointVerticalControllerConnection6))
						(setq firstLMRJPoint pointVerticalControllerConnection1)
						(setq secondLMRJPoint pointVerticalControllerConnection6)

						(GRD-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)
						)
					)

					(setq xCoordCurrent (+ (+  xCoordPortFirstPrevious DistanceToRightSideOfController) controllerCableRightEdgeExtension))
					(setq yCoordCurrent (- (- (- (- yCoordLMRJConnector DistanceToControllerBottomPrevious) verticalSpaceBetweenControllers ) ControllerHeight ) RBreturnYdistanceMax))

					(setq yShifterController (* yDirectionController (+ (+ verticalSpaceBetweenControllers ControllerHeightMax) RBreturnYdistanceMax)))
					(setq ControllerHeightMax 0)
					(setq yCoordController (+ yShifterController yCoordController))

					(setq xDirectionController (* xDirectionController -1))
					(setq RBreturnYdistanceMax 0)
					  
					)

					
				  
				)
		  		;;; else, in bounds and finish drawing connections
			  	(progn
			  	(if
					(/= cntr (- cmax 1))
				  	(progn


					  
					(GRD-draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)									
					(GRD-entmod-blockinsert-attributes nil firstLMRJPoint "RD-ROOMS" LMRJBlockName)
					(GRD-entmod-blockinsert-attributes nil secondLMRJPoint "RD-ROOMS" LMRJBlockName)
					)
				)
				)
				)
			)


			 

			(if
			  	(> xCoordCurrent xCoordMax)
			  	(progn
				(setq xCoordMax xCoordCurrent)
				)
			)
			(if
			  	(< yCoordCurrent yCoordMin)
			  	(progn
				(setq yCoordMin yCoordCurrent)
				)
			)

		  
			(setq xCoordPortFirstPrevious xCoordPortFirst)
			(setq xCoordPortLastPrevious xCoordPortLast)
			(setq DistanceToLeftSideOfControllerPrevious DistanceToLeftSideOfController)
			(setq DistanceToRightSideOfControllerPrevious DistanceToRightSideOfController)
		  
		  	(setq DistanceToControllerBottomPrevious DistanceToControllerBottom)

			(setq xCoordPortFirst nil)
			(setq xCoordPort2 nil)
			(setq xCoordPort3 nil)
			(setq xCoordPortLast nil)
		  
			(setq cntr (+ cntr 1))														;increment counter

		)
		
		(if
			(= ControllerList nil)
			(progn
			(setq perPortInt (length RBINTERFACES))
			(setq perPortOcc (length RBDLMOCCSENSORS))
			(setq perPortSw (length RBDLMSWITCHES))
			(setq Controller nil)
			(setq xCoordPort2 100)
			(setq xCoordPort3 nil)
			;Controller perPortOcc cntr2 cmax2 RBDLMOCCSENSORS
			;perPortSw cntr3 cmax3 RBDLMSWITCHES RBOnePortList cntr4 cmax4
			(setq RBUseableControllerPorts nil)
			(setq lastController 1)
			(setq xCoordPortFirst nil)
			(setq xCoordPortLast 108.1706)
			(setq yCoordLMRJConnector -70)
			(setq DistanceToControllerBottom 20)
			(setq numberOfLMRJPortsOnCurrentController 1)
			(setq xDirectionController 1)


		  	;;; set the connection point to nil when it's not allowed to be used
		  	;;; which is anytime code reaches this point, no controllers at all
		  	(if
			  	(AND
				(/= BridgeAndInputPortforCornerMountConnection nil)
				)
				(progn
				(setq BridgeAndInputPortforCornerMountConnection nil)
				)
			)
			
			(setq RBreturnList (GRD-rung-builder Controller perPortInt cntr1 cmax1 RBINTERFACES perPortOcc cntr2 cmax2 RBDLMOCCSENSORS perPortSw cntr3 cmax3 RBDLMSWITCHES RBOnePortList cntr4 cmax4 RBSPLITTERS RBUseableControllerPorts lastController xCoordPort2 xCoordPort3 xCoordPortFirst xCoordPortLast yCoordLMRJConnector DistanceToControllerBottom numberOfLMRJPortsOnCurrentController xDirectionController BridgeAndInputPortforCornerMountConnection))
			(setq cntr1 (nth 0 RBreturnList))
		  	(setq cntr2 (nth 1 RBreturnList))
		  	(setq cntr3 (nth 2 RBreturnList))
		  	(setq cntr4 (nth 3 RBreturnList))
		  	(setq RBreturnYdistance (nth 4 RBreturnList))

		  	(if
			  	(> RBreturnYdistance RBreturnYdistanceMax)
			  	(progn
				(setq RBreturnYdistanceMax RBreturnYdistance)
				(setq yCoordMin (- yCoordLMRJConnector RBreturnYdistanceMax)) 
				)
			)

			
			(if
			  	(> xCoordCurrent xCoordMax)
			  	(progn
				(setq xCoordMax xCoordCurrent)
				)
			)
			(if
			  	(< yCoordCurrent yCoordMin)
			  	(progn
				(setq yCoordMin yCoordCurrent)
				)
			)
			
			)
		)

		(setq returnlist (list xCoordMax yCoordMin YcoordMax))
)



(defun GRD-rung-builder 	(
				RB-Controller RB-perControllerInt RB-cntr1  RB-cmax1 RB-INTERFACES RB-perControllerOcc RB-cntr2  RB-cmax2 RB-DLMOCCSENSORS RB-perControllerSw RB-cntr3 RB-cmax3 RB-DLMSWITCHES RB-OnePortList RB-cntr4 RB-cmax4 RB-SPLITTERS RB-UseableControllerPorts RB-lastController RB-xCoordPort2 RB-xCoordPort3
				RB-xCoordPortFirst RB-xCoordPortLast RB-yCoordLMRJConnector RB-DistanceToControllerBottom  RB-numberOfLMRJPortsOnCurrentController
				RB-xDirectionController RB-BridgeAndInputPortforCornerMountConnection
				 /
				 GRD-CONSTANTBLOCKINFOLIST LMRJBLOCKHEIGHT LMRJBLOCKNAME LMRJUPSIDEDOWNBLOCKNAME RBLMRJPOINT4PREVIOUS RBP4CNTR RBP4CNTRMAX RBVERTICALDROPFROMSTARTSPLITTERRUNG RBXCOORDLMRJCONNECTION4PREVIOUS RBXCOORDPORT4PRODUCTSTART RBYCOORDPORT4START
				RBCURRENTUSEABLECONTROLLERPORTS RBPRODUCT RBXCOORD RBYCOORD rbfunctionRETURNLIST
				INSERTIONPOINT RBCOLUMNDIRECTION RBCOLUMNWIDTH RBROWCOUNTER RBROWHEIGHT RBROWMAX RBSPACEBELOWCONTROLLER RBYCOORDSTART RBYDISTANCE
				 BCLS RBrowDirectionpPort1 RBCOLUMNDIRECTIONPPORT2 RBCONNECTIONPOINT1 RBCONNECTIONPOINT2 RBCONNECTIONPOINT3 RBCONNECTIONPOINT4 RBCURRENTLMRJPOINT
				 RBCURRENTLMRJPOINT2 RBI RBPOINT-LIST RBPOLYLINE-LAYER RBPOLYLINE-LINETYPE RBPOLYLINE-WIDTH RBXCOORDCONNECTIONSPACER RBXCOORDLMRJCONNECTION
				 RBXCOORDLMRJCONNECTION2 RBXCOORDLMRJCONNECTION2PREVIOUS RBYCOORDCONNECTIONEXTENSION RBYCOORDCONNECTIONSPACER RBYCOORDLMRJCONNECTION
				 RBYCOORDLMRJCONNECTIONPREVIOUS RBYDISTANCEMAX Port4List PORT4PRODUCT RBROWDIRECTIONPPORT4 RBXCOORDPORT4
				 PORT1LIST PORT1PRODUCT RBCLS RBCONNECTIONPOINT5 RBCONNECTIONPOINT6 RBLMRJPOINT RBLMRJPOINT2 RBLMRJPOINT2PREVIOUS RBP1CNTR RBROWDROPCONNECTIONLIST RBXCOORDCONNECTIONEXTENSION
				 PORT2LIST PORT2PRODUCT PORT3LIST PORT3PRODUCT RBP2CNTR RBP3CNTR RBROWDIRECTIONPPORT3 BLOCKNAME RB-OCCREMAINDER RB-SWREMAINDER RBP2CNTRMAX RBP3CNTRMAX RBROWEXTENSION
			  	)

	 			
				
		;;; use function to get constant block information
		;;; makes adding new blocks easier
		;;; keeps functions modular
		(setq GRD-constantBlockInfoList (GRD-get-constant-block-info))
		(setq LMRJBlockName (nth 0 GRD-constantBlockInfoList))
		(setq LMRJUpsideDownBlockName (nth 1 GRD-constantBlockInfoList))
		(setq LMRJBlockHeight (nth 2 GRD-constantBlockInfoList))
  


  		;function call variables
	  	(setq RBcls 0)
	  	(setq RBpolyline-layer "RD-ROOMS")
	  	(setq RBpolyline-width 0.6)
	  	(setq RBpolyline-linetype "bylayer")
  
  		;calculator varaibles
  		(setq RBspaceBelowController 30)
  		(setq RBrowCounter 0)
  		(setq RBrowMax 1)
  		(setq RBrowHeight 66)
  		(setq RBcolumnWidth 40)
  		(setq RBrowDirectionpPort1 -1)
  		(setq RBrowextension 40)

  		;calculator variables for connections
  		(setq RBxCoordConnectionSpacer 3)
  		(setq RByCoordConnectionSpacer 32)
  		(setq RByCoordConnectionExtension 6.3)
		(setq RBxCoordConnectionExtension 18)

		;calculator variables for splitter rung, port 4
  		(setq RBverticalDropFromStartSplitterRung 6.3)
  		(setq portConfigurationForLastController 0)
		(if
		  	(AND (= RB-lastController 1))

		  	(progn

		  	(if
			  	(AND (= RB-xCoordPort2 nil) (= RB-xDirectionController -1)(= portConfigurationForLastController 0))
			  	(progn
				(setq RBxCoordPort4 RB-xCoordPortFirst)
				(setq RBxCoordPort4ProductStart RBxCoordPort4)
				(setq portConfigurationForLastController 1)
				
				)
			)
		  	(if
			  	(AND (= RB-xCoordPort2 nil) (= RB-xDirectionController 1)(= portConfigurationForLastController 0))
			  	(progn
				(setq RBxCoordPort4 RB-xCoordPortLast)
				(setq RBxCoordPort4ProductStart RBxCoordPort4)
				(setq portConfigurationForLastController 1)
				)
			)
			(if
			  	(AND (= RB-xCoordPort3 nil) (= RB-xDirectionController -1)(= portConfigurationForLastController 0))
			  	(progn
				(setq RBxCoordPort4 RB-xCoordPortFirst)
				(setq RBxCoordPort4ProductStart (- RBxCoordPort4 101.8))
 				(setq portConfigurationForLastController 1)
				)
			)			
			(if
			  	(AND (= RB-xCoordPort3 nil) (= RB-xDirectionController 1)(= portConfigurationForLastController 0))
			  	(progn
				(setq RBxCoordPort4 RB-xCoordPortLast)
				(setq RBxCoordPort4ProductStart (+ RBxCoordPort4 20))
				(setq portConfigurationForLastController 1)
				)
			)
			
			(if
			  	(AND (/= RB-xCoordPort3 nil)(/= RB-xCoordPort2 nil)(= RB-xDirectionController -1)(= portConfigurationForLastController 0))
			  	(progn
				(setq RBxCoordPort4 RB-xCoordPortFirst)
				(setq RBxCoordPort4ProductStart (- RBxCoordPort4 101.8))
				(setq portConfigurationForLastController 1)
				)
			)
			(if
			  	(AND (/= RB-xCoordPort3 nil)(/= RB-xCoordPort2 nil)(= RB-xDirectionController 1)(= portConfigurationForLastController 0))
			  	(progn
				(setq RBxCoordPort4 RB-xCoordPortLast)
				(setq RBxCoordPort4ProductStart (+ RBxCoordPort4 141.8))
				(setq portConfigurationForLastController 1)
				)
			)

;;;			(if
;;;			  	(/= RB-numberOfLMRJPortsOnCurrentController 2)
;;;			  	(progn
;;;				(setq RB-numberOfLMRJPortsOnCurrentController (- RB-numberOfLMRJPortsOnCurrentController 2))
;;;				(setq RB-perControllerInt (/ (- RB-cmax1 RB-cntr1) RB-numberOfLMRJPortsOnCurrentController))
;;;				(setq RB-perControllerOcc (/ (- RB-cmax2 RB-cntr2) RB-numberOfLMRJPortsOnCurrentController))
;;;				(setq RB-perControllerSw (/(- RB-cmax3 RB-cntr3) RB-numberOfLMRJPortsOnCurrentController))
;;;
;;;				(setq RB-intRemainder (rem (- RB-cmax1 RB-cntr1) RB-numberOfLMRJPortsOnCurrentController))
;;;				(setq RB-occRemainder (rem (- RB-cmax2 RB-cntr2) RB-numberOfLMRJPortsOnCurrentController))
;;;				(setq RB-swRemainder (rem (- RB-cmax3 RB-cntr3) RB-numberOfLMRJPortsOnCurrentController))
;;;
;;;				(setq RB-perControllerInt (+ RB-perControllerInt RB-intRemainder))
;;;				(setq RB-perControllerOcc (+ RB-perControllerOcc RB-occRemainder))
;;;				(setq RB-perControllerSw (+ RB-perControllerSw RB-swRemainder))
;;;				)
;;;			  	(progn
;;;				(setq RB-perControllerInt (- RB-cmax1 RB-cntr1))
;;;				(setq RB-perControllerOcc (- RB-cmax2 RB-cntr2))
;;;				(setq RB-perControllerSw (- RB-cmax3 RB-cntr3))
;;;				)
;;;			)
			)
		)


  		;;; RB-BridgeAndInputPortforCornerMountConnection, it's only useable on port2
		(if
			(and
		  	(/= RB-BridgeAndInputPortforCornerMountConnection nil)(= RB-xCoordPort2 nil)
			)
		  	(progn
			(setq RB-xCoordPort2 (nth 0 RB-BridgeAndInputPortforCornerMountConnection))
			)
		)
  		(if
		  	(= RB-perControllerInt nil)
		  	(progn
			(setq RB-perControllerInt 0)
			)
		)
  		(if
		  	(= RB-perControllerOcc nil)
		  	(progn
			(setq RB-perControllerOcc 0)
			)
		)
  		(if
		  	(= RB-perControllerSw nil)
		  	(progn
			(setq RB-perControllerSw 0)
			)
		)
		
  		
		(setq RBycoordStart (- RB-yCoordLMRJConnector RB-DistanceToControllerBottom))
		(setq RBycoord RBycoordStart)

  		(setq RByDistanceMax 0)
  
		(setq RBrowDropConnectionList nil)

	  	(if
		  	(/= RB-xCoordPort2 nil)
		  	(progn
	
			(repeat RB-perControllerInt 													;while loop -- loop through the interfaces
				(progn
				(if
					(< RB-cntr1 RB-cmax1)
					(progn
					(setq Port2Product (nth RB-cntr1 RB-INTERFACES))						;pull first interface off list into Controller
					(setq Port2List (cons Port2Product Port2List))
					(setq RB-cntr1 ( + RB-cntr1 1))
					)
				)
				)
			)


			  
			(repeat RB-perControllerOcc													;while loop -- loop through the interfaces
				(progn
				(if
					(< RB-cntr2 RB-cmax2)
					(progn
					(setq Port2Product (nth RB-cntr2 RB-DLMOCCSENSORS))						;pull first interface off list into Controller
					(setq Port2List (cons Port2Product Port2List))
					(setq RB-cntr2 ( + RB-cntr2 1))
					)
				)
				)
			)
			(repeat RB-perControllerSw													;while loop -- loop through the interfaces
				(progn
				(if
					(< RB-cntr3 RB-cmax3)
					(progn
					(setq Port2Product (nth RB-cntr3 RB-DLMSWITCHES))										;pull first interface off list into Controller
					(setq Port2List (cons Port2Product Port2List))
					(setq RB-cntr3 ( + RB-cntr3 1))
					)
				)
				)
			)
			(repeat 1																	;while loop -- loop through the interfaces
				(progn
				(if
					(< RB-cntr4 RB-cmax4)
					(progn
					(setq Port2Product (nth RB-cntr4 RB-OnePortList))						;pull first interface off list into Controller
					(setq Port2List (cons Port2Product Port2List))
					(setq RB-cntr4 ( + RB-cntr4 1))
					)
				)
				)
			)
			)
		)
  
		
	  	(if
		  	(AND
			(/= Port2List nil)
			)
		  	(progn
			(setq Port2List (reverse Port2List))
			(setq RBxCoordLMRJConnection2Previous RB-xCoordPort2)
			(setq RBycoordLMRJConnectionPrevious RB-yCoordLMRJConnector)			  
			(setq RBLMRJPoint2Previous (list RBxCoordLMRJConnection2Previous RBycoordLMRJConnectionPrevious)) 
			
			(setq RBycoord (- RBycoord RBspaceBelowController))
	  	  	(setq RBxcoord (- RB-xCoordPort2 (/ RBcolumnWidth 2)))
			
			(setq RBp2cntr 0)
			(setq RBp2cntrmax (length Port2List))
				
				(repeat (length Port2List)													;while loop -- loop through the interfaces
				  	(progn
					(setq insertionpoint (list RBxcoord RBycoord))
					(setq RBproduct (nth RBp2cntr Port2List))									;pull first interface off list into Controller
					;;;entmodinsertfix
					(setq blockname (strcat "RD-" RBproduct))
					(GRD-entmod-blockinsert-attributes nil insertionpoint "RD-ROOMS" blockname)
					


					(setq RBycoordLMRJConnection (- RBycoord RByCoordConnectionSpacer))
					(setq RBxCoordLMRJConnection (+ RBxcoord (* (* RBrowDirectionpPort1 -1) RBxCoordConnectionSpacer)))
					(setq RBxCoordLMRJConnection2 (+ RBxcoord (* (* RBrowDirectionpPort1 1) RBxCoordConnectionSpacer)))
					(setq RBLMRJPoint (list RBxCoordLMRJConnection RBycoordLMRJConnection))
					(setq RBLMRJPoint2 (list RBxCoordLMRJConnection2 RBycoordLMRJConnection))
					




					
					(setq RBp2cntr (+ RBp2cntr 1))
					(setq RBrowCounter (+ RBrowCounter 1))
					(if
					  	(> RBrowCounter RBrowMax)
					  	(progn
						(setq RBconnectionPoint1 (list RBxCoordLMRJConnection RBycoordLMRJConnection))
						(setq RBconnectionPoint2 (list RBxCoordLMRJConnection (- RBycoordLMRJConnection RByCoordConnectionExtension)))
						(setq RBconnectionPoint3 (list RBxCoordLMRJConnection2Previous (- RBycoordLMRJConnection RByCoordConnectionExtension)))
						(setq RBconnectionPoint4 (list RBxCoordLMRJConnection2Previous RBycoordLMRJConnectionPrevious))
						(setq RBpoint-list(list RBconnectionPoint1 RBconnectionPoint2 RBconnectionPoint3 RBconnectionPoint4))
						(GRD-draw-lwpolyline RBpoint-list RBcls RBpolyline-layer RBpolyline-width RBpolyline-linetype)
						(GRD-entmod-blockinsert-attributes nil RBLMRJPoint "RD-ROOMS" LMRJBlockName)
						(GRD-entmod-blockinsert-attributes nil RBLMRJPoint2Previous "RD-ROOMS" LMRJBlockName)
						
						(setq RBxCoordLMRJConnection2Previous RBxCoordLMRJConnection2)
						(setq RBycoordLMRJConnectionPrevious RBycoordLMRJConnection)
						(setq RBLMRJPoint2Previous (list RBxCoordLMRJConnection2Previous RBycoordLMRJConnectionPrevious))
						
						;;; if there is another device left
						(if
						(< RBp2cntr RBp2cntrmax)
						(progn
							(GRD-entmod-blockinsert-attributes nil RBLMRJPoint "RD-ROOMS" LMRJBlockName)
							(setq RBconnectionPoint1 (list RBxCoordLMRJConnection2Previous RBycoordLMRJConnection))
							(setq RBconnectionPoint2 (list RBxCoordLMRJConnection2Previous (- RBycoordLMRJConnection RByCoordConnectionExtension)))
							(setq RBconnectionPoint3 (list (+ RBxCoordLMRJConnection2Previous (* RBrowDirectionpPort1 RBxCoordConnectionExtension)) (- RBycoordLMRJConnection RByCoordConnectionExtension)))
							(setq RBconnectionPoint4 (list (+ RBxCoordLMRJConnection2Previous (* RBrowDirectionpPort1 RBxCoordConnectionExtension)) (- (- RBycoordLMRJConnection RByCoordConnectionExtension) RBrowHeight)))
							(setq RBconnectionPoint5 (list RBxCoordLMRJConnection2Previous (- (- RBycoordLMRJConnection RByCoordConnectionExtension) RBrowHeight)))
							(setq RBconnectionPoint6 (list RBxCoordLMRJConnection2Previous (- RBycoordLMRJConnection RBrowHeight)))
							(setq RBrowDropConnectionList(list RBconnectionPoint1 RBconnectionPoint2 RBconnectionPoint3 RBconnectionPoint4 RBconnectionPoint5 RBconnectionPoint6))
							(GRD-draw-lwpolyline RBrowDropConnectionList RBcls RBpolyline-layer RBpolyline-width RBpolyline-linetype)
							(GRD-entmod-blockinsert-attributes nil RBconnectionPoint1 "RD-ROOMS" LMRJBlockName)
							(GRD-entmod-blockinsert-attributes nil RBconnectionPoint6 "RD-ROOMS" LMRJBlockName)
						)
						)  

						(setq RBrowCounter 0)
						(setq RBycoord (- RBycoord RBrowHeight))
						(setq RBrowDirectionpPort1 (* RBrowDirectionpPort1 -1))

						)
					  	(progn
						(if
						  	(= RBrowDropConnectionList nil)
						  	(progn
							(setq RBconnectionPoint1 (list RBxCoordLMRJConnection RBycoordLMRJConnection))
							(setq RBconnectionPoint2 (list RBxCoordLMRJConnection (- RBycoordLMRJConnection RByCoordConnectionExtension)))
							(setq RBconnectionPoint3 (list RBxCoordLMRJConnection2Previous (- RBycoordLMRJConnection RByCoordConnectionExtension)))
							(setq RBconnectionPoint4 (list RBxCoordLMRJConnection2Previous RBycoordLMRJConnectionPrevious))
							(setq RBpoint-list(list RBconnectionPoint1 RBconnectionPoint2 RBconnectionPoint3 RBconnectionPoint4))
							(GRD-draw-lwpolyline RBpoint-list RBcls RBpolyline-layer RBpolyline-width RBpolyline-linetype)
							(GRD-entmod-blockinsert-attributes nil RBLMRJPoint "RD-ROOMS" LMRJBlockName)
							(GRD-entmod-blockinsert-attributes nil RBLMRJPoint2Previous "RD-ROOMS" LMRJBlockName)
							)
						)
						
						
						(setq RBxcoord (+ RBxcoord (* RBcolumnWidth RBrowDirectionpPort1)))
						(setq RBrowDropConnectionList nil)
						)
					)
					
					
					(setq RBxCoordLMRJConnection2Previous RBxCoordLMRJConnection2)
					(setq RBycoordLMRJConnectionPrevious RBycoordLMRJConnection)
					(setq RBLMRJPoint2Previous (list RBxCoordLMRJConnection2Previous RBycoordLMRJConnectionPrevious))  
					
					)
				)
			
			)
		)

  
  		(setq RByDistance (- RBycoord RBycoordStart))
  		(setq RByDistance (abs RByDistance))
  		(if	(> RByDistance RByDistanceMax)
		  	(progn
  			(setq RByDistanceMax RByDistance)
			)
		)



	  	(if
		  	(/= RB-xCoordPort3 nil)
		  	(progn

			(repeat RB-perControllerInt 													;while loop -- loop through the interfaces
				(progn
				(if
					(< RB-cntr1 RB-cmax1)
					(progn
					(setq Port3Product (nth RB-cntr1 RB-INTERFACES))						;pull first interface off list into Controller
					(setq Port3List (cons Port3Product Port3List))
					(setq RB-cntr1 ( + RB-cntr1 1))
					)
				)
				)
			)

			  
			(repeat RB-perControllerOcc													;while loop -- loop through the interfaces
				(progn
				(if
					(< RB-cntr2 RB-cmax2)
					(progn
					(setq Port3Product (nth RB-cntr2 RB-DLMOCCSENSORS))						;pull first interface off list into Controller
					(setq Port3List (cons Port3Product Port3List))
					(setq RB-cntr2 ( + RB-cntr2 1))
					)
				)
				)
			)
			(repeat RB-perControllerSw													;while loop -- loop through the interfaces
				(progn
				(if
				  	(< RB-cntr3 RB-cmax3)
					(progn
					(setq Port3Product (nth RB-cntr3 RB-DLMSWITCHES))										;pull first interface off list into Controller
					(setq Port3List (cons Port3Product Port3List))
					(setq RB-cntr3 ( + RB-cntr3 1))
					)
				)
				
				
				)
			)
			(repeat 1																	;while loop -- loop through the interfaces
				(progn
				(if
					(< RB-cntr4 RB-cmax4)
					(progn
					(setq Port3Product (nth RB-cntr4 RB-OnePortList))						;pull first interface off list into Controller
					(setq Port3List (cons Port3Product Port3List))
					(setq RB-cntr4 ( + RB-cntr4 1))
					)
				)
				)
			)
			)
		)

  
  		(setq RBrowCounter 0)
  		(setq RBrowMax 2)
  		(setq RBrowDirectionpPort3 1)
  		(setq RBycoord RBycoordStart)
  		(setq RBrowDropConnectionList nil)
  
	  	(if
		  	(/= Port3List nil)
		  	(progn
			(setq Port3List (reverse Port3List))
			(setq RBxCoordLMRJConnection2Previous RB-xCoordPort3)
			(setq RBycoordLMRJConnectionPrevious RB-yCoordLMRJConnector)			  
			(setq RBLMRJPoint2Previous (list RBxCoordLMRJConnection2Previous RBycoordLMRJConnectionPrevious)) 
			
			(setq RBycoord (- RBycoord RBspaceBelowController))
	  	  	(setq RBxcoord (+ RB-xCoordPort3 (/ RBcolumnWidth 2)))
			
			(setq RBp3cntr 0)
			(setq RBp3cntrmax (length Port3List))
			
				(repeat (length Port3List)													;while loop -- loop through the interfaces
				  	(progn
					(setq insertionpoint (list RBxcoord RBycoord))
					(setq RBproduct (nth RBp3cntr Port3List))									;pull first interface off list into Controller
					;;;entmodinsertfix
					(setq blockname (strcat "RD-" RBproduct))
					(GRD-entmod-blockinsert-attributes nil insertionpoint "RD-ROOMS" blockname)

					(setq RBycoordLMRJConnection (- RBycoord RByCoordConnectionSpacer))
					(setq RBxCoordLMRJConnection (+ RBxcoord (* (* RBrowDirectionpPort3 -1) RBxCoordConnectionSpacer)))
					(setq RBxCoordLMRJConnection2 (+ RBxcoord (* (* RBrowDirectionpPort3 1) RBxCoordConnectionSpacer)))
					(setq RBLMRJPoint (list RBxCoordLMRJConnection RBycoordLMRJConnection))
					(setq RBLMRJPoint2 (list RBxCoordLMRJConnection2 RBycoordLMRJConnection))
					




					
					(setq RBp3cntr (+ RBp3cntr 1))
					(setq RBrowCounter (+ RBrowCounter 1))
					(if
					  	(> RBrowCounter RBrowMax)
					  	(progn
						(setq RBconnectionPoint1 (list RBxCoordLMRJConnection RBycoordLMRJConnection))
						(setq RBconnectionPoint2 (list RBxCoordLMRJConnection (- RBycoordLMRJConnection RByCoordConnectionExtension)))
						(setq RBconnectionPoint3 (list RBxCoordLMRJConnection2Previous (- RBycoordLMRJConnection RByCoordConnectionExtension)))
						(setq RBconnectionPoint4 (list RBxCoordLMRJConnection2Previous RBycoordLMRJConnectionPrevious))
						(setq RBpoint-list(list RBconnectionPoint1 RBconnectionPoint2 RBconnectionPoint3 RBconnectionPoint4))
						(GRD-draw-lwpolyline RBpoint-list RBcls RBpolyline-layer RBpolyline-width RBpolyline-linetype)
						(GRD-entmod-blockinsert-attributes nil RBLMRJPoint "RD-ROOMS" LMRJBlockName)
						(GRD-entmod-blockinsert-attributes nil RBLMRJPoint2Previous "RD-ROOMS" LMRJBlockName)
						
						(setq RBxCoordLMRJConnection2Previous RBxCoordLMRJConnection2)
						(setq RBycoordLMRJConnectionPrevious RBycoordLMRJConnection)
						(setq RBLMRJPoint2Previous (list RBxCoordLMRJConnection2Previous RBycoordLMRJConnectionPrevious))
						
						;;; if there is another device left
						(if
						(< RBp3cntr RBp3cntrmax)
						(progn
							(GRD-entmod-blockinsert-attributes nil RBLMRJPoint "RD-ROOMS" LMRJBlockName)
							(setq RBconnectionPoint1 (list RBxCoordLMRJConnection2Previous RBycoordLMRJConnection))
							(setq RBconnectionPoint2 (list RBxCoordLMRJConnection2Previous (- RBycoordLMRJConnection RByCoordConnectionExtension)))
							(setq RBconnectionPoint3 (list (+ RBxCoordLMRJConnection2Previous (* RBrowDirectionpPort3 RBxCoordConnectionExtension)) (- RBycoordLMRJConnection RByCoordConnectionExtension)))
							(setq RBconnectionPoint4 (list (+ RBxCoordLMRJConnection2Previous (* RBrowDirectionpPort3 RBxCoordConnectionExtension)) (- (- RBycoordLMRJConnection RByCoordConnectionExtension) RBrowHeight)))
							(setq RBconnectionPoint5 (list RBxCoordLMRJConnection2Previous (- (- RBycoordLMRJConnection RByCoordConnectionExtension) RBrowHeight)))
							(setq RBconnectionPoint6 (list RBxCoordLMRJConnection2Previous (- RBycoordLMRJConnection RBrowHeight)))
							(setq RBrowDropConnectionList(list RBconnectionPoint1 RBconnectionPoint2 RBconnectionPoint3 RBconnectionPoint4 RBconnectionPoint5 RBconnectionPoint6))
							(GRD-draw-lwpolyline RBrowDropConnectionList RBcls RBpolyline-layer RBpolyline-width RBpolyline-linetype)
							(GRD-entmod-blockinsert-attributes nil RBconnectionPoint1 "RD-ROOMS" LMRJBlockName)
							(GRD-entmod-blockinsert-attributes nil RBconnectionPoint6 "RD-ROOMS" LMRJBlockName)
						)
						)  

						(setq RBrowCounter 0)
						(setq RBycoord (- RBycoord RBrowHeight))
						(setq RBrowDirectionpPort3 (* RBrowDirectionpPort3 -1))

						)
					  	(progn
						(if
						  	(= RBrowDropConnectionList nil)
						  	(progn
							(setq RBconnectionPoint1 (list RBxCoordLMRJConnection RBycoordLMRJConnection))
							(setq RBconnectionPoint2 (list RBxCoordLMRJConnection (- RBycoordLMRJConnection RByCoordConnectionExtension)))
							(setq RBconnectionPoint3 (list RBxCoordLMRJConnection2Previous (- RBycoordLMRJConnection RByCoordConnectionExtension)))
							(setq RBconnectionPoint4 (list RBxCoordLMRJConnection2Previous RBycoordLMRJConnectionPrevious))
							(setq RBpoint-list(list RBconnectionPoint1 RBconnectionPoint2 RBconnectionPoint3 RBconnectionPoint4))
							(GRD-draw-lwpolyline RBpoint-list RBcls RBpolyline-layer RBpolyline-width RBpolyline-linetype)
							(GRD-entmod-blockinsert-attributes nil RBLMRJPoint "RD-ROOMS" LMRJBlockName)
							(GRD-entmod-blockinsert-attributes nil RBLMRJPoint2Previous "RD-ROOMS" LMRJBlockName)
							)
						)
						
						
						(setq RBxcoord (+ RBxcoord (* RBcolumnWidth RBrowDirectionpPort3)))
						(setq RBrowDropConnectionList nil)
						)
					)
					
					
					(setq RBxCoordLMRJConnection2Previous RBxCoordLMRJConnection2)
					(setq RBycoordLMRJConnectionPrevious RBycoordLMRJConnection)
					(setq RBLMRJPoint2Previous (list RBxCoordLMRJConnection2Previous RBycoordLMRJConnectionPrevious))  
					
					)
				)
			
			)
		)

	  	(if
		  	;;; only true on last controller
		  	(/= RBxCoordPort4 nil)
		  	(progn

			

				;;; when splitters exist, connect this rung to port 4
			(if
			  	(/= RB-SPLITTERS nil)
			  	(progn
				(setq RBxcoord RBxCoordPort4ProductStart)
				(setq RBycoord (- (- RB-yCoordLMRJConnector RB-DistanceToControllerBottom) 30 ))
				(setq RByCoordPort4ProductStart RBycoord)
				(setq RBxCoordLMRJConnection4Previous RBxCoordPort4)
				(setq RBycoordLMRJConnectionPrevious RB-yCoordLMRJConnector)			  
				(setq RBLMRJPoint1 (list RBxCoordPort4 RB-yCoordLMRJConnector))
				(setq RBLMRJPoint2 (list RBxCoordPort4 (- RB-yCoordLMRJConnector RB-DistanceToControllerBottom)))
				(setq RBLMRJPoint3 (list RBxCoordPort4ProductStart (- RB-yCoordLMRJConnector RB-DistanceToControllerBottom)))
				(setq RBLMRJPoint4 (list RBxCoordPort4ProductStart (- (+ RByCoordPort4ProductStart (/ RBrowHeight 2)) RBverticalDropFromStartSplitterRung)))
				(setq RBConnectionToPort4List (list RBLMRJPoint1 RBLMRJPoint2 RBLMRJPoint3 RBLMRJPoint4))
				
				(GRD-draw-lwpolyline RBConnectionToPort4List  RBcls RBpolyline-layer RBpolyline-width RBpolyline-linetype)
				(GRD-entmod-blockinsert-attributes nil RBLMRJPoint1 "RD-ROOMS" LMRJBlockName)
				(GRD-entmod-blockinsert-attributes nil RBLMRJPoint4 "RD-ROOMS" LMRJUpsideDownBlockName)

				(setq RBp4cntr 0)
				(setq RBp4cntrmax (length RB-SPLITTERS))

				
				(repeat (length RB-SPLITTERS)
					(progn
					(setq insertionpoint (list RBxcoord RBycoord))
					(setq RBproduct (nth RBp4cntr RB-SPLITTERS))									;pull first interface off list into Controller
					;;;entmodinsertfix
					(setq blockname (strcat "RD-" RBproduct))
					(GRD-entmod-blockinsert-attributes nil insertionpoint "RD-ROOMS" blockname)

					(if
					  	(/= RBp4cntr 0)
					  	(progn
						(setq RBLMRJPoint1 (list RBxCoordPort4ProductStart (+ (+ RBycoord (/ RBrowHeight 2)) RByCoordConnectionExtension)))
						(setq RBLMRJPoint2 (list RBxCoordPort4ProductStart (- (+ RBycoord (/ RBrowHeight 2)) RByCoordConnectionExtension)))
						(setq RBConnectionBetweenDevices (list RBLMRJPoint1 RBLMRJPoint2))
						(GRD-draw-lwpolyline RBConnectionBetweenDevices  RBcls RBpolyline-layer RBpolyline-width RBpolyline-linetype)
						(GRD-entmod-blockinsert-attributes nil RBLMRJPoint1 "RD-ROOMS" LMRJBlockName)
						(GRD-entmod-blockinsert-attributes nil RBLMRJPoint2 "RD-ROOMS" LMRJUpsideDownBlockName)
						)
					)

					
					(setq RBp4cntr (+ RBp4cntr 1))
					(setq RBycoord (- RBycoord RBrowHeight))
					
					)
				)

				;;; for determining color of connection lines
				(setq RBp4cntr 0)
				(setq RBp4cntrmax (length RB-SPLITTERS))

				
				(setq RBxcoord RBxCoordPort4ProductStart)
				(setq RBxcoord (+ RBxcoord (* RBcolumnWidth RB-xDirectionController)))
				(setq RBycoord RByCoordPort4ProductStart)
				(while 	(< RB-cntr4 RB-cmax4)
					(setq insertionpoint (list RBxcoord RBycoord))
					(setq RBproduct (nth RB-cntr4 RB-OnePortList))									;pull first interface off list into Controller
					;;;entmodinsertfix
					(setq blockname (strcat "RD-" RBproduct))
					
				  	(cond
					  	;;; if there is splitter to connect to
					  	((< RBp4cntr RBp4cntrmax)
						(setq RBlayer "RD-ROOMS")
						(setq RBLMRJPoint1 (list RBxcoord (- RBycoord RByCoordConnectionSpacer)))
						(setq RBLMRJPoint2 (list RBxcoord (- (- RBycoord RByCoordConnectionSpacer)RByCoordConnectionExtension)))
						(setq RBLMRJPoint3 (list (+ (+ RBxcoord (* (* RBcolumnWidth RB-xDirectionController) -1 )(* (* RBxCoordConnectionSpacer RB-xDirectionController) 2 ))) (- (- RBycoord RByCoordConnectionSpacer)RByCoordConnectionExtension)))
						(setq RBLMRJPoint4 (list (+ (+ RBxcoord (* (* RBcolumnWidth RB-xDirectionController) -1 )(* (* RBxCoordConnectionSpacer RB-xDirectionController) 2 ))) (+ (- (- RBycoord RByCoordConnectionSpacer)RByCoordConnectionExtension) 11.6)))
						)
					  	;;; if we are one past the max, connect to the previous since the last splitter can hold two
					  	((= RBp4cntr RBp4cntrmax)
						(setq RBlayer "RD-ROOMS")
						(setq RBLMRJPoint1 (list RBxcoord (- RBycoord RByCoordConnectionSpacer)))
						(setq RBLMRJPoint2 (list RBxcoord (- (- RBycoord RByCoordConnectionSpacer) RByCoordConnectionExtension)))
						(setq RBLMRJPoint3 (list (+ RBxcoord (* (* RBcolumnWidth RB-xDirectionController) -1 ) ) (- (- RBycoord RByCoordConnectionSpacer) RByCoordConnectionExtension)))
						(setq RBLMRJPoint4 (list (+ RBxcoord (* (* RBcolumnWidth RB-xDirectionController) -1 ) ) (+ (+ (- (- RBycoord RByCoordConnectionSpacer) RByCoordConnectionExtension)RBrowHeight)11.6)))
						)
						(t
						(setq RBlayer "RD-ROOMS-DESIGN-REVIEW")

						(setq RBLMRJPoint1 (list RBxcoord (- RBycoord RByCoordConnectionSpacer)))
						(setq RBLMRJPoint2 (list RBxcoord (- (- RBycoord RByCoordConnectionSpacer)RByCoordConnectionExtension)))
						(setq RBLMRJPoint3 (list (+ (+ RBxcoord (* (* RBcolumnWidth RB-xDirectionController) -1 )(* (* RBxCoordConnectionSpacer RB-xDirectionController) 2 ))) (- (- RBycoord RByCoordConnectionSpacer)RByCoordConnectionExtension)))
						(setq RBLMRJPoint4 (list (+ (+ RBxcoord (* (* RBcolumnWidth RB-xDirectionController) -1 )(* (* RBxCoordConnectionSpacer RB-xDirectionController) 2 ))) (+ (- (- RBycoord RByCoordConnectionSpacer)RByCoordConnectionExtension) 11.6)))
						 
						)
					)
				  	(GRD-entmod-blockinsert-attributes nil insertionpoint RBlayer blockname)
				  
					(setq RBConnectionBetweenDevices (list RBLMRJPoint1 RBLMRJPoint2 RBLMRJPoint3 RBLMRJPoint4))
					(GRD-draw-lwpolyline RBConnectionBetweenDevices  RBcls RBlayer RBpolyline-width RBpolyline-linetype)			  
					(GRD-entmod-blockinsert-attributes nil RBLMRJPoint1 RBlayer LMRJBlockName)
					(GRD-entmod-blockinsert-attributes nil RBLMRJPoint4 RBlayer LMRJBlockName)
				  
				  	(setq RBp4cntr (+ RBp4cntr 1))
					(setq RBycoord (- RBycoord RBrowHeight))						
					(setq RB-cntr4 ( + RB-cntr4 1))
				)
				)
			)
			(if
			  		(AND (< RB-cntr4 RB-cmax4)(= RB-SPLITTERS nil))
					;;; if there are 1 port devices left
					;;; and if splitters don't exist
					;;; connect first 1 port device to port 4
					;;; then insert rest for design review
					(progn
					  
					(setq RBxcoord RBxCoordPort4ProductStart)
					(setq RBycoord (- (- RB-yCoordLMRJConnector RB-DistanceToControllerBottom) 30 ))
					(setq RByCoordPort4ProductStart RBycoord)
			  

					(setq RBport4cntr1portDevices 0)
					(setq RBxcoord RBxCoordPort4ProductStart)
					(setq RBxcoord (+ RBxcoord (* RBcolumnWidth RB-xDirectionController)))
					(setq RBycoord RByCoordPort4ProductStart)
					(while 	(< RB-cntr4 RB-cmax4)
						(setq insertionpoint (list RBxcoord RBycoord))
						(setq RBproduct (nth RB-cntr4 RB-OnePortList))									;pull first interface off list into Controller
						;;;entmodinsertfix
						(setq blockname (strcat "RD-" RBproduct))
						
						(cond
							;;; if this is first 1 port device, connect to port 4
							((= RBport4cntr1portDevices 0)
							(setq RBlayer "RD-ROOMS")
							(setq RBLMRJPoint1 (list RBxcoord (- RBycoord RByCoordConnectionSpacer)))
							(setq RBLMRJPoint2 (list RBxcoord (- (- RBycoord RByCoordConnectionSpacer)RByCoordConnectionExtension)))
							(setq RBLMRJPoint3 (list (+ (+ RBxcoord (* (* RBcolumnWidth RB-xDirectionController) -1 )(* (* RBxCoordConnectionSpacer RB-xDirectionController) 2 ))) (- (- RBycoord RByCoordConnectionSpacer)RByCoordConnectionExtension)))
							(setq RBLMRJPoint4 (list (+ (+ RBxcoord (* (* RBcolumnWidth RB-xDirectionController) -1 )(* (* RBxCoordConnectionSpacer RB-xDirectionController) 2 )))(- RB-yCoordLMRJConnector RB-DistanceToControllerBottom)))
							(setq RBLMRJPoint5 (list RBxCoordPort4 (- RB-yCoordLMRJConnector RB-DistanceToControllerBottom)))
							(setq RBLMRJPoint6 (list RBxCoordPort4 RB-yCoordLMRJConnector))
							(setq RBConnectionBetweenDevices (list RBLMRJPoint1 RBLMRJPoint2 RBLMRJPoint3 RBLMRJPoint4 RBLMRJPoint5 RBLMRJPoint6))
							(GRD-entmod-blockinsert-attributes nil RBLMRJPoint1 RBlayer LMRJBlockName)
							(GRD-entmod-blockinsert-attributes nil RBLMRJPoint6 RBlayer LMRJBlockName)
							)
							(t
							(setq RBlayer "RD-ROOMS-DESIGN-REVIEW")
							(setq RBLMRJPoint1 (list RBxcoord (- RBycoord RByCoordConnectionSpacer)))
							(setq RBLMRJPoint2 (list RBxcoord (- (- RBycoord RByCoordConnectionSpacer)RByCoordConnectionExtension)))
							(setq RBLMRJPoint3 (list (+ (+ RBxcoord (* (* RBcolumnWidth RB-xDirectionController) -1 )(* (* RBxCoordConnectionSpacer RB-xDirectionController) 2 ))) (- (- RBycoord RByCoordConnectionSpacer)RByCoordConnectionExtension)))
							(setq RBLMRJPoint4 (list (+ (+ RBxcoord (* (* RBcolumnWidth RB-xDirectionController) -1 )(* (* RBxCoordConnectionSpacer RB-xDirectionController) 2 ))) (+ (- (- RBycoord RByCoordConnectionSpacer)RByCoordConnectionExtension) 11.6)))
							(setq RBConnectionBetweenDevices (list RBLMRJPoint1 RBLMRJPoint2 RBLMRJPoint3 RBLMRJPoint4))
							(GRD-entmod-blockinsert-attributes nil RBLMRJPoint1 RBlayer LMRJBlockName)
							(GRD-entmod-blockinsert-attributes nil RBLMRJPoint4 RBlayer LMRJBlockName)							
							)
						)
						(GRD-entmod-blockinsert-attributes nil insertionpoint RBlayer blockname)
					  
						(GRD-draw-lwpolyline RBConnectionBetweenDevices  RBcls RBlayer RBpolyline-width RBpolyline-linetype)			  

					  
						(setq RBport4cntr1portDevices (+ RBport4cntr1portDevices 1))
						(setq RBycoord (- RBycoord RBrowHeight))						
						(setq RB-cntr4 ( + RB-cntr4 1))
					)

					)
				)








				
				


			


			
			)
		)
  
  		(setq RByDistance (- RBycoord RBycoordStart))
  		(setq RByDistance (abs RByDistance))
  		(if	(> RByDistance RByDistanceMax)
		  	(progn
  			(setq RByDistanceMax RByDistance)
			)
		)






;;;connect from beginning port 4 to beginning of splitter rung
;;;loop through splitter rung
;;;loop until end of 1 ports



  
  		(setq RByDistance (- RBycoord RBycoordStart))
  		(setq RByDistance (abs RByDistance))
		      
  		(if	(> RByDistance RByDistanceMax)
		  	(progn
  			(setq RByDistanceMax RByDistance)
			)
		)
  


  		(setq RByDistanceMax (+ RByDistanceMax (* RBrowextension 1.6)))
		(setq RByDistanceMax (abs RByDistanceMax))
		
		(setq rbfunctionRETURNLIST (list RB-cntr1 RB-cntr2 RB-cntr3 RB-cntr4 RByDistanceMax))
)






(defun GRD-controller-dimensions
       				(
				 cd-Controller cd-xCoordController cd-yCoordController
				/
				cdXcoordport2
				cdXcoordport3
				cdXcoordportfirst cdXcoordportlast cdDistanceToLeftSideOfController cdDistanceToRightSideOfController
				cdControllerHeight
				cdDistanceToControllerBottom
				cdDistanceToControllerTop
				cdyCoordLMRJConnector cdports
				 CDNUMBEROFLMRJPORTSONCURRENTCONTROLLER GRD-CONSTANTBLOCKINFOLIST LMRJBLOCKHEIGHT LMRJBLOCKNAME LMRJUPSIDEDOWNBLOCKNAME
				 GENERALCONTROLLERINFORMATIONLIST HEIGHTBETWEENLMRJPORTANDCONNECTOR WIDTHBETWEENCONTROLLERLMRJPORTS
				cdfunctionreturnList CDFUNCTIONRETURNLIST
				)

			
			
			;;; use function to get constant block information
			;;; makes adding new blocks easier
			;;; keeps functions modular
			(setq GRD-constantBlockInfoList (GRD-get-constant-block-info))
			(setq LMRJBlockName (nth 0 GRD-constantBlockInfoList))
			(setq LMRJUpsideDownBlockName (nth 1 GRD-constantBlockInfoList))
			(setq LMRJBlockHeight (nth 2 GRD-constantBlockInfoList))			
			
			;;; use function to get information for starting controller functions
			;;; makes editing values easier
			;;; keeps functions modular
			(setq generalControllerInformationList (GRD-get-general-controller-information))
			(setq widthBetweenControllerLMRJPorts (nth 0 generalControllerInformationList))
			(setq heightBetweenLMRJPortAndConnector (nth 1 generalControllerInformationList))

  
		  	(if
			  	(OR (= cd-Controller "LMRC-222")(= cd-Controller "LMRC-221"))
				(progn
				(setq cdXcoordportfirst cd-xCoordController)
				(setq cdXcoordport2 (+ cd-xCoordController (*  widthBetweenControllerLMRJPorts 1)))
				(setq cdXcoordport3 (+ cd-xCoordController (* widthBetweenControllerLMRJPorts 2)))
				(setq cdXcoordportlast (+ cd-xCoordController (* widthBetweenControllerLMRJPorts 3)))				  
				(setq cdDistanceToLeftSideOfController 71)
				(setq cdDistanceToRightSideOfController 200)
				;(setq cdDistanceToControllerTop 170)
				(setq cdDistanceToControllerTop 54)
			  	(setq cdDistanceToControllerBottom 52)
				(setq cdNumberOfLMRJPortsOnCurrentController 4)
				)
			)

		  	(if
			  	(OR (= cd-Controller "LMRC-213-347v")(= cd-Controller "LMRC-212-347v")(= cd-Controller "LMRC-211-347v")(= cd-Controller "LMRC-213")(= cd-Controller "LMRC-212")(= cd-Controller "LMRC-211")(= cd-Controller "LMPL-201"))
				(progn
				(setq cdXcoordportfirst cd-xCoordController)
				(setq cdXcoordport2 (+ cd-xCoordController (*  widthBetweenControllerLMRJPorts 1)))
				(setq cdXcoordport3 (+ cd-xCoordController (* widthBetweenControllerLMRJPorts 2)))
				(setq cdXcoordportlast (+ cd-xCoordController (* widthBetweenControllerLMRJPorts 3)))
				(setq cdDistanceToLeftSideOfController 71)
				(setq cdDistanceToRightSideOfController 125)
				(setq cdDistanceToControllerTop 54)
			  	(setq cdDistanceToControllerBottom 52)
				(setq cdNumberOfLMRJPortsOnCurrentController 4)
				)
			)
	  
		  	(if
				(OR (= cd-Controller "LMRC-102")(= cd-Controller "LMRC-101"))
			  	(progn
				(setq cdXcoordportfirst cd-xCoordController)
				(setq cdXcoordport2 (+ cd-xCoordController (*  widthBetweenControllerLMRJPorts 1)))
				(setq cdXcoordport3 nil)
				(setq cdXcoordportlast (+ cd-xCoordController (* widthBetweenControllerLMRJPorts 2)))
				(setq cdDistanceToLeftSideOfController 44)
				(setq cdDistanceToRightSideOfController 139)
				(setq cdDistanceToControllerTop 20)
			  	(setq cdDistanceToControllerBottom 52)
				(setq cdNumberOfLMRJPortsOnCurrentController 3)
				)
			)
	  
		  	(if
				(OR (= cd-Controller "LMPL-101"))
			  	(progn
			 	(setq cdXcoordportfirst cd-xCoordController)
				(setq cdXcoordport2 (+ cd-xCoordController (*  widthBetweenControllerLMRJPorts 1)))
				(setq cdXcoordport3 nil)
				(setq cdXcoordportlast (+ cd-xCoordController (* widthBetweenControllerLMRJPorts 2)))
				(setq cdDistanceToLeftSideOfController 50)
				(setq cdDistanceToRightSideOfController 90)
				(setq cdDistanceToControllerTop 44)
			  	(setq cdDistanceToControllerBottom 52)
				(setq cdNumberOfLMRJPortsOnCurrentController 3)
				)
			)
	  
	  	  	(if
			  	(OR (= cd-Controller "LMRC-112-M")(= cd-Controller "LMRC-111-M")(= cd-Controller "LMRC-112")(= cd-Controller "LMRC-111"))
				(progn
			 	(setq cdXcoordportfirst cd-xCoordController)
				(setq cdXcoordport2 nil)
				(setq cdXcoordport3 nil)
				(setq cdXcoordportlast (+ cd-xCoordController (* widthBetweenControllerLMRJPorts 1)))
				(setq cdDistanceToLeftSideOfController 95)
				(setq cdDistanceToRightSideOfController 90)
				(setq cdDistanceToControllerTop 161)
			  	(setq cdDistanceToControllerBottom 52)
				(setq cdNumberOfLMRJPortsOnCurrentController 2)
				)
			)


			(setq cdyCoordLMRJConnector (- (- cd-yCoordController heightBetweenLMRJPortAndConnector) LMRJBlockHeight ))
		  	(setq cdControllerHeight (+ cdDistanceToControllerTop cdDistanceToControllerBottom))


  
  			(setq cdfunctionreturnList (list cdXcoordportfirst cdXcoordport2 cdXcoordport3 cdXcoordportlast cdDistanceToLeftSideOfController cdDistanceToRightSideOfController cdDistanceToControllerTop cdDistanceToControllerBottom cdyCoordLMRJConnector cdControllerHeight cdNumberOfLMRJPortsOnCurrentController))

)


(defun GRD-controller-builder-look-ahead
       		( 
		la-Xdirectioncontroller  la-loopOneTime la-xCoordController la-xCoordControllerMax la-xCoordControllerMin la-xCoordPortFirst la-xCoordPortLast la-DistanceToLeftSideOfController 
		la-DistanceToRightSideOfController la-DistanceToControllerBottom la-ControllerList
		la-yCoordController  la-Cntr la-CntrMax

		/
		 CONTROLLERCABLELEFTEDGEEXTENSION CONTROLLERCABLERIGHTEDGEEXTENSION GENERALCONTROLLERINFORMATIONLIST GRD-CONSTANTBLOCKINFOLIST HEIGHTBETWEENLMRJPORTANDCONNECTOR HORIZONTALSPACEBETWEENCONTROLLERS LACDRETURNLIST LACNTR2 LACONTROLLER LACONTROLLERHEIGHT LACONTROLLERHEIGHTMAX LADISTANCETOCONTROLLERBOTTOMPREVIOUS LADISTANCETOCONTROLLERTOP LADISTANCETOLEFTSIDEOFCONTROLLERFUTURE LADISTANCETOLEFTSIDEOFCONTROLLERPREVIOUS LADISTANCETORIGHTSIDEOFCONTROLLERFUTURE LADISTANCETORIGHTSIDEOFCONTROLLERPREVIOUS LAFUNCTIONRETURNLIST LAONETIMECHECK LAXCOORDCONTROLLERPREVIOUS LAXCOORDPORT2 LAXCOORDPORT3 LAXCOORDPORTFIRSTFUTURE LAXCOORDPORTFIRSTPREVIOUS LAXCOORDPORTLASTFUTURE LAXCOORDPORTLASTPREVIOUS LAXSHIFTERCONTROLLER LAYCOORDLMRJCONNECTOR LMRJBLOCKHEIGHT LMRJBLOCKNAME LMRJUPSIDEDOWNBLOCKNAME VERTICALSPACEBETWEENCONTROLLERS WIDTHBETWEENCONTROLLERLMRJPORTS XCOORDCONTROLLERMAX XCOORDCONTROLLERMIN XCOORDCONTROLLERSTART YCOORDCONTROLLERSTART	
		)
  
		;;; use function to get constant block information
		;;; makes adding new blocks easier
		;;; keeps functions modular
		(setq GRD-constantBlockInfoList (GRD-get-constant-block-info))
		(setq LMRJBlockName (nth 0 GRD-constantBlockInfoList))
		(setq LMRJUpsideDownBlockName (nth 1 GRD-constantBlockInfoList))
		(setq LMRJBlockHeight (nth 2 GRD-constantBlockInfoList))



		;;; use function to get information for starting controller functions
	  	;;; makes editing values easier
	  	;;; keeps functions modular
	  	(setq generalControllerInformationList (GRD-get-general-controller-information))
		(setq widthBetweenControllerLMRJPorts (nth 0 generalControllerInformationList))
		(setq heightBetweenLMRJPortAndConnector (nth 1 generalControllerInformationList))
		(setq horizontalSpaceBetweenControllers (nth 3 generalControllerInformationList))


  
		(setq laCntr2 0)
		(setq laOnetimecheck 1)
  		(setq laControllerHeightmax 0)
  		(setq laControllerHeight 0)
  		(setq laxShifterController 0)
		  	;;; look ahead loop, simulates the size of the rung until the coordinates have to drop
		  	;;; calculates the maxes based on the size of the controllers
		  	;;; saves them for connecting items
		  	;;; restores variables back to values before loop after simluation for maxes takes place
		  	(while
			  	(AND
				(< la-xCoordController la-xCoordControllerMax)(> la-xCoordController la-xCoordControllerMin)(= laOnetimecheck 1)(< la-Cntr (- la-CntrMax 1))
				)
			  	(progn
				
				(setq laXcoordportfirstprevious la-xCoordPortFirst)
				(setq laXcoordportlastprevious la-xCoordPortLast)
				(setq laDistanceToLeftSideOfControllerprevious la-DistanceToLeftSideOfController)
				(setq laDistanceToRightSideOfControllerprevious la-DistanceToRightSideOfController)
			  
			  	(setq laDistanceToControllerBottomprevious la-DistanceToControllerBottom)

				(setq la-xCoordPortFirst nil)
				(setq laXcoordport2 nil)
				(setq laXcoordport3 nil)
				(setq la-xCoordPortLast nil)

				(setq la-Cntr (+ la-Cntr 1))
				(setq laCntr2 (+ laCntr2 1))
				(setq laController(nth la-Cntr la-ControllerList))
				(setq laCdreturnlist (GRD-controller-dimensions laController la-xCoordController la-yCoordController))
				
				(setq  la-xCoordPortFirst (nth 0 laCdreturnlist))
				(setq  laXcoordport2 (nth 1 laCdreturnlist))
				(setq  laXcoordport3 (nth 2 laCdreturnlist))
				(setq  la-xCoordPortLast (nth 3 laCdreturnlist))
				(setq  la-DistanceToLeftSideOfController (nth 4 laCdreturnlist))
				(setq  la-DistanceToRightSideOfController (nth 5 laCdreturnlist))
				(setq  laDistanceToControllerTop (nth 6 laCdreturnlist))
				(setq  la-DistanceToControllerBottom (nth 7 laCdreturnlist))
				(setq  layCoordLMRJConnector (nth 8 laCdreturnlist))
				(if
				  	(= laCntr2 1)
				  	(progn
					(setq laxCoordPortFirstFuture la-xCoordPortFirst)
					(setq laDistanceToLeftSideOfControllerFuture la-DistanceToLeftSideOfController)
					(setq laDistanceToRightSideOfControllerFuture la-DistanceToRightSideOfController)
					)
				)
				(if
				  	(= laCntr2 1)
				  	(progn
					(setq laxCoordPortLastFuture la-xCoordPortLast)
					)
				)				
				(setq  laControllerHeight (nth 9 laCdreturnlist))

			  	;;; if catch
			  	;;; first part of both inbounds situations
			  	(if
				  	;;; moving right
				  	(= la-Xdirectioncontroller 1)																	;test for moving left or right
					(progn																				;connection points for moving right
				  	(setq laXcoordcontrollerprevious la-xCoordController)

					(setq laxShifterController (+ (+ (* la-Xdirectioncontroller horizontalSpaceBetweenControllers) laDistanceToRightSideOfControllerprevious )la-DistanceToLeftSideOfController))
				  	(setq la-xCoordController (+ laxShifterController la-xCoordController))

					)
				  	;;; moving left
					(progn																				;connection points for moving left

					(setq laxShifterController (- (- (* la-Xdirectioncontroller horizontalSpaceBetweenControllers) la-DistanceToRightSideOfController) laDistanceToLeftSideOfControllerprevious))
				  	(setq laXcoordcontrollerprevious la-xCoordController)
				  	(setq la-xCoordController (+ laxShifterController la-xCoordController))

					)
				)
				
			  	(if
				  	(> laControllerHeight laControllerHeightmax)
					(progn
					(setq laControllerHeightmax laControllerHeight)
					)
				)
				
				)
			  	(setq laOnetimecheck  (+ la-loopOneTime laOnetimecheck))
			)


  			(setq lafunctionreturnList (list laxShifterController laControllerHeightmax laxCoordPortFirstFuture laxCoordPortLastFuture laDistanceToLeftSideOfControllerFuture laDistanceToRightSideOfControllerFuture))

)























; --- GRD-entmod-blockinsert-attributes Sub Function ---
; inserts block
; inserts any existing attributes from the block definition table
; file out attributes from list sent in from calling function
; Arguments:
; attributelist		list of attribute values passed in from GRD-block-builder 
; Alex Lundin 03-31-2017
(defun GRD-entmod-blockinsert-attributes
       				(
				attributelist blkinsertionpoint layername blockname 
				/
				Attdefs ATTBLK ATTBLKNAME ATTRIBUTEDXF11 ATTRIBUTEINSERTIONPOINT ATTRIBUTEXCOORD ATTRIBUTEYCOORD BLOCKXCOORD
				BLOCKYCOORD DATA ENAME NEWINSERTIONPOINT NEWXCOORD NEWYCOORD NEXTENT GRD-entmod-blockinsert-attributes-cntr ATTRIBUTEDXF10 ATTRIBUTEVALUE NEWINSERTIONPOINTDXF10 NEWINSERTIONPOINTDXF11
				)
;;;arguments
;;;  	attributevalue - value to place into block
;;;  	blkinsertionpoint - insertion point for filled out block
;;;	layername - name of layer to insert block on
;;;	riserblockname - name of cabinet passed in from GRD-block-builder 


	;;BLOCKTABLE search, call placeholder later
  
	(setq attblkname blockname)
	(setq GRD-entmod-blockinsert-attributes-cntr 0)
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

					(if
				  	(/= attributelist nil)
					  	(progn
						(setq attributevalue (nth GRD-entmod-blockinsert-attributes-cntr attributelist))	;get first attribute value from the list of them
						(setq GRD-entmod-blockinsert-attributes-cntr (+ GRD-entmod-blockinsert-attributes-cntr 1));increase counter for next loop
						)
					)
					(if
				  	(= attributelist nil)
					  	(progn
						(setq attributevalue "")							;get first attribute value from the list of them
						)
					)						
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
			(entmake '((0 . "SEQEND")(8 . "RD-ROOMS")))    								;---third add, entmake SEQEND to signify final attribute at end of block
		)
		)														;--cond statement
		(T nil)
	)															;-end cond


)





; --- GRD-RiserHeirarchy Sub Function ---
; accept a list of products in from Main and seperates them into RH sublists for other functions
; Alex Lundin 03-17-2017
(defun GRD-RiserHeirarchy (RHproducts /
RHCONTROLLERS RHDLMCONTROLLERS RHDLMDAYLIGHT RHDLMINTERFACES RHDLMNETWORKBRIDGES RHDLMOCCCORNERMOUNT RHDLMOCCSENSORS RHDLMPANELS RHDLMPLUGCONTROLLERS RHDLMSEGMENTMANAGERS RHDLMSPLITTERS RHDLMSWITCHES RHDLMZONECONTROLLERS RHREMAINDER rhfunctionRETURNLIST

		       CMAX CNTR PLACEMENTCHECK RHPRODUCT)
;;;  		arguments					
;;;  		GSR:						
;;;		products into RHproducts			
;;;                                                       	
;;;  		RH variables used from other functions	
;;;  		None						
;;;								
;;;  		RH variables for other functions		
;;;		PortCalculator:					
;;;  		RHControllers				
;;;  		block-builder:					
;;;  		RHDLMCONTROLLERS RHDLMDAYLIGHT RHDLMINTERFACES RHDLMNETWORKBRIDGES RHDLMOCCCORNERMOUNT RHDLMOCCSENSORS RHDLMPANELS RHDLMPLUGCONTROLLERS RHDLMSPLITTERS RHDLMSWITCHES RHDLMZONECONTROLLERS RHREMAINDER
;;;		Segmentbuilder:
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
		  	(OR (= RHproduct "LMRC-222")(= RHproduct "LMRC-221")(= RHproduct "LMRC-213-347v")(= RHproduct "LMRC-212-347v")(= RHproduct "LMRC-211-347v")(= RHproduct "LMRC-213")(= RHproduct "LMRC-212")(= RHproduct "LMRC-211")(= RHproduct "LMRC-112-M")(= RHproduct "LMRC-111-M")(= RHproduct "LMRC-112")(= RHproduct "LMRC-111")(= RHproduct "LMRC-102")(= RHproduct "LMRC-101"))
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
			(OR (= RHproduct "LMRL-100")(= RHproduct "LMIO-301")(= RHproduct "LMIO-201")(= RHproduct "LMIO-102")(= RHproduct "LMIO-101")(= RHproduct "LMDI-100")(= RHproduct "LMPB-100"))
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
			(OR (= RHproduct "LMLS-105")(= RHproduct "LMLS-305")(= RHproduct "LMLS-400")(= RHproduct "LMLS-500")(= RHproduct "LMLS-600")(= RHproduct "LMPO-200")(= RHproduct "LMPS-6000")(= RHproduct "LMFC-011")(= RHproduct "LMIR-100"))
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

  
  	(setq rhfunctionRETURNLIST (list RHDLMSEGMENTMANAGERS RHDLMNETWORKBRIDGES RHDLMZONECONTROLLERS RHDLMPANELS RHDLMCONTROLLERS RHDLMPLUGCONTROLLERS RHControllers RHDLMINTERFACES RHDLMOCCSENSORS RHDLMOCCCORNERMOUNT RHDLMDAYLIGHT RHDLMSWITCHES RHDLMSPLITTERS RHREMAINDER))
)









;;;expects 0 as class value in variable cls
(defun GRD-draw-lwpolyline (point-list cls polyline-layer polyline-width polyline-linetype)
;;;arguments
;;;	point-list
;;;	cls
;;;	polyline-layer
;;;	polyline-width
;;;	polyline-linetype
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


(defun GRD-draw-line (startpoint endpoint line-layer line-linetype)
;;;arguments
;;;	point-list
;;;	line-layer
;;;	line-linetype
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

















; --- riserdraworder Sub Function ---
; place all blocks on top of wires
; Alex Lundin 04-28-2017
(defun c:roomdetailsdraworder	( /
				ACADOBJ ARR ARRROWLOWERBOUNDARY ARRROWUPPERBOUNDARY CMAX CNTR COLUMN DOC EDICTIONARY
			 	EXTDICT MODELSPACE SENTITYOBJ SETITEM SORTTBL SPACE SS SSLIST
		 	)

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






; --- RoundUp Sub Function ---
; take any decimal value and round up to the next whole integer
; Alex Lundin 03-17-2017
(defun GRD-RoundUp ( n )
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


; --- RoundUp Sub Function ---
; take any decimal value and round up to the next whole integer
; Alex Lundin 03-17-2017
(defun GRD-RoundDown ( n )
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


; --- PortCalculator Sub Function ---
; accept a list of products in from Main and calculate types of ports avaiable for each item
; this function also calculates the number of each type of rung in global variables
; Alex Lundin 03-17-2017
(defun GRD-PortCalculator (PCproducts PCnumberofcontrollers / CMAX CNTR PCPRODUCT PORTS PORTSFORCONTROLLERCONNECTION useableControllerPorts PCCONTROLLERS PCNUMBEROFRUNG1 PCNUMBEROFRUNG2 PCNUMBEROFRUNG3 PCNUMBEROFRUNGFINAL pcfunctionRETURNLIST)
;;;  	arguments passed in
;;;  	GSR:
;;;	products into PCproducts
;;;	number of controllers into PCnumberofcontrollers
  
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
			(setq PCNUMBEROFRUNG3 (+ 2 PCNUMBEROFRUNG3))
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
	  	(/= PCnumberofcontrollers 0)
		(progn
		(setq portsForControllerConnection (* 2 PCnumberofcontrollers))
		(setq portsForControllerConnection (- portsForControllerConnection 1))
		(setq useableControllerPorts (- ports portsForControllerConnection))
		(setq PCNUMBEROFRUNGFINAL 1)
		)
	)
  	(setq pcfunctionRETURNLIST (list PCNUMBEROFRUNG1 PCNUMBEROFRUNG2 PCNUMBEROFRUNG3 PCNUMBEROFRUNGFINAL useableControllerPorts))
)