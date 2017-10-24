;;; --- GSR Function ---
;;; Alex Lundin 06-07-2017
(defun c:GRD 	(
	      	/
		CUSTOMBLOCKSET FILE1 TR-RETURNLIST
	       	)
	(vl-load-com)																		;open f1 for reading -- similar to oppen in eblock function

  	(setq finalLMRJBlockName "LMRJ")
  	(setq finalLMRJUpsideDownBlockName "LMRJ-UPSIDE-DOWN")
  
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

																			;- end while


	(setq TR-returnList (text-reader))

	(princ)
;;;  	(setq roomDetailsRoomList TR-returnList)
;;;  	(setq roomDetailsRoomList (reverse roomDetailsRoomList))													;reverse singlelineroomlist to account for cons property which builds list backwards
;;;  	
;;;
;;;  
;;;  	
;;;;;;  	this conditional block handles operations when there are standalone rooms on the project
;;;;;;  	the first statement will happen anytime there is more than one standalone room
;;;;;;  	it will sort the segments first, then parse them individually
;;;;;;  	the second statement will happen anytime there is one standalone room
;;;;;;  	this will only parse the room since it won't need to be numerically sorted because there is only one
;;;  	(setq standalonechecker (length globalSTANDALONEDOTTEDPAIRS))
;;;  	(cond																			;- cond block
;;;	  	((>= standalonechecker 1)															;-- cond statement
;;;		(standalonesort)
;;;		)																		;-- end cond statement
;;;	)  
;;;
;;;
;;;
;;;
;;;;;;  	if statement to call standalone-and-segment-inserter if atleast one of the lists that is used to insert from has items
;;;;;;  	progn wrap is not needed for only one statement on the if, but I included it incase we need to add more lines here
;;;  	(if
;;;	  	(or
;;;		(/= globalSEGMENTSTOINSERT nil) (/= globalSTANDALONENUMERICAL nil)
;;;		)
;;;	  	(progn
;;;	  	(standalone-inserter)
;;;		)
;;;	)
;;;
;;;;;;	burst all blocks into individual products
;;;;;;	this is the only way draw order works
;;;  	(setq ss nil)
;;;  	(setq ss (ssget "x" '((0 . "INSERT")(8 . "Riser-Segments"))))
;;;  	(if
;;;	  	(/= ss nil)
;;;	  	(progn
;;;		(setq cntr 0)
;;;		(setq cmax (sslength ss))
;;;		(while
;;;		  	(< cntr cmax)
;;;		  	(setq item (ssname ss cntr))
;;;		 	(command "_.explode" item)
;;;		  	(setq cntr (+ cntr 1))
;;;		)
;;;		)
;;;  		
;;;	)
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
;;;  	(c:riserdraworder)
  	


)

;end GSR





; --- text-reader Sub Function ---
; determines method for reading information from text file
; Alex Lundin 03-17-2017
(defun text-reader	(

			/
			DRAWINGLOCATION FILE1 NUMBEROFPRODUCTS PRODUCT PRODUCTS ROOMLOOPCHECK SINGLELINEROOMLIST returnlist TR-roomList
			BLOCKHEIGHT BLOCKWIDTH CONTROLLERS DLMCONTROLLERS DLMDAYLIGHT DLMINTERFACES DLMNETWORKBRIDGES DLMOCCCORNERMOUNT DLMOCCSENSORS DLMPANELS DLMPLUGCONTROLLERS DLMSEGMENTMANAGERS DLMSPLITTERS DLMSWITCHES DLMZONECONTROLLERS NUMBEROFCONTROLLERS NUMBEROFPRODUCTSINROOM NUMBEROFRUNG1 NUMBEROFRUNG2 NUMBEROFRUNG3 NUMBEROFRUNGFINAL PCRETURNLIST RBRETURNLIST REMAINDER RHRETURNLIST STANDALONESUBLIST TR-ROOM TR-ROOMFORRUNGBUILDER TR-STANDALONEDOTTEDPAIRS USEABLECONTROLLERPORTS
			)


	;;;open text file for reading
  	(setq file1 (open (strcat (getvar 'DWGPREFIX) "Room_Details_Extraction.txt")  "r"))

	;;;let user know if text file cannot be found
  	(cond																			;-cond block
	  	((= file1 nil)																	;-- cond statement if error handler for no text file
		(princ "Room_Details_Extraction.txt not found in folder with SR-1")
		(quit)
		)																		;-- end cond statement
	)																			;- end cond block

	;;;read the first line of the text file in, which is the location of the drawing that was extracted from
	;;;this is the priming read for the following while loop
  	(setq drawingLocation (read-line file1))														;read in first line of Riser_Extraction.txt which will be drawing location
  	(setq TR-room (read-line file1))															;priming read for TR-room so it's not empty
  	(setq roomLoopCheck TR-room)																;roomLoopCheck will always hold the previous rooms value inside the while loop
	(setq singlelineroomlist (cons TR-room singlelineroomlist))												;create a list of all rooms from the extraction
  
  	(while	(/= TR-room nil)																;- while loop for reading text document, end when roomLoopCheck is nil

		;;;first if statment
		;;;this will call the subfunctions when true
		;;;logic follows:
		;;;if TR-room does not equal roomLoopCheck, call subfunctions to operate on the list of products from the previous room
		;;;TR-room is the current value read from the text file and is compared to roomLoopCheck, the prevous room
		;;;after the subfunctions run, nil list called products and store TR-room to the singlelineroomlist
		(if
		  	(/= TR-room roomLoopCheck)														;-- if statement to identify new room
			(progn																	;---progn wrap


	  		(setq RHreturnlist (RiserHeirarchy products))
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
			
			(setq PCreturnlist(PortCalculator products NumberofCONTROLLERS))
			(setq NUMBEROFRUNG1 (nth 0 PCreturnlist))
			(setq NUMBEROFRUNG2 (nth 1 PCreturnlist))
			(setq NUMBEROFRUNG3 (nth 2 PCreturnlist))
			(setq NUMBEROFRUNGFINAL(nth 3 PCreturnlist))
			(setq UseableControllerPorts(nth 4 PCreturnlist))
			
			(setq numberofproductsinroom (length products))

  			(setq RBreturnlist (RungBuilder roomLoopCheck DLMSEGMENTMANAGERS DLMNETWORKBRIDGES DLMZONECONTROLLERS DLMPANELS DLMCONTROLLERS DLMPLUGCONTROLLERS Controllers DLMINTERFACES DLMOCCSENSORS DLMOCCCORNERMOUNT DLMDAYLIGHT DLMSWITCHES DLMSPLITTERS REMAINDER UseableControllerPorts))
			(setq blockwidth (nth 0 RBreturnlist))
			(setq blockheight (nth 1 RBreturnlist))
			
			(setq BLOCKHEIGHT(RoundUp BLOCKHEIGHT))
			(setq standalonesublist (cons BLOCKHEIGHT standalonesublist))
			(setq standalonesublist (cons BLOCKWIDTH standalonesublist))
			(setq standalonesublist (cons TR-roomforrungbuilder standalonesublist))
			(setq TR-STANDALONEDOTTEDPAIRS (cons standalonesublist TR-STANDALONEDOTTEDPAIRS))
			(setq standalonesublist nil)
			(setq products nil)


			)																	;---end progn
		)																		;-- end if


		  	(setq numberOfProducts (read-line file1))
			(setq numberOfProducts (atoi numberOfProducts))
		  	(repeat numberOfProducts
			  	(progn
			  	(setq product (read-line file1))
			  	(if	(/= product nil)														;-- if statment to protect list from nil
					(setq products (cons product products))												;build products list
				)																	;read the next line into product
				)
			)
		  	(setq roomLoopCheck TR-room)															;store TR-room to roomLoopCheck

		;;;final step, read next room from text file
		;;;now TR-room has the current room
		;;;roomLoopCheck has the room from the last iteration for testing purposes
		(setq TR-room (read-line file1))
	  	(setq TR-roomList( cons TR-room TR-roomList))
	)

	(setq returnlist TR-roomList)
  	(close file1)
)


(defun RungBuilder (RBroom RBDLMSEGMENTMANAGERS RBDLMNETWORKBRIDGES RBDLMZONECONTROLLERS RBDLMPANELS RBDLMCONTROLLERS RBDLMPLUGCONTROLLERS
		    RBControllers RBDLMINTERFACES RBDLMOCCSENSORS RBDLMOCCCORNERMOUNT RBDLMDAYLIGHT RBDLMSWITCHES RBDLMSPLITTERS RBREMAINDER RBUseableControllerPorts

		    /
BLOCKNAME CMAX CMAX2 CMAX3 CMAX4 CMAX5 CNTR CNTR2 CNTR3 CNTR4 CNTR5 CONTROLLER CONTROLLERLIST INSERTIONPOINT LASTCONTROLLER LISTSORTER MAXBLOCKXCOORD ONEPORTLIST PERPORTOCC PERPORTSW RBBLOCKHEIGHT RBBLOCKWIDTH RETURNLIST VERTICALRUNG XCOORD XCOORDCONTROLLER XCOORDCONTROLLERMAX XCOORDCONTROLLERMIN XCOORDCONTROLLERPREVIOUS XCOORDCONTROLLERSPACER XCOORDMAX XDIRECTIONCONTROLLER XSHIFTERCONTROLLER YCOORD YCOORDCONTROLLER YCOORDCONTROLLERSPACER YCOORDDISTANCETOCONTROLLERTOP YCOORDMAX YDIRECTIONCONTROLLER YSHIFTERCONTROLLER
BRIDGEANDINPUT LASTBRIDGEANDINPUT XCOORDBRIDGEANDINPUT XCOORDBRIDGEANDINPUTSPACER XDIRECTIONBRIDGEANDINPUT XSHIFTERBRIDGEANDINPUT YCOORDBRIDGEANDINPUT YCOORDBRIDGEANDINPUTSPACER YCOORDCONTROLLERHEIGHT YCOORDDISTANCETOBRIDGEANDINPUTTOP YCOORDSHIFTERBRIDGEANDINPUTSPACER YDIRECTIONBRIDGEANDINPUT YSHIFTERBRIDGEANDINPUT
BRIDGEANDINPUTFORCONTROLLERCONNECTIONPOINT1 BRIDGEANDINPUTFORCONTROLLERCONNECTIONPOINT2 BRIDGEANDINPUTLIST BRIDGEANDINPUTPORT1 BRIDGEANDINPUTPORT1PREVIOUS BRIDGEANDINPUTPORT2 BRIDGEANDINPUTPORT2PREVIOUS BRIDGEANDINPUTPORTFORCORNERMOUNTCONNECTION BRIDGEANDINPUTPORTFORPREVIOUSCONNECTION CLS FIRSTLMRJPOINT POINT-LIST POINTCONTROLLERCONNECTION1 POINTCONTROLLERCONNECTION2 POINTCONTROLLERCONNECTION2PREVIOUS POINTCONTROLLERCONNECTION3 POINTCONTROLLERCONNECTION4 POINTVERTICALCONTROLLERCONNECTION1 POINTVERTICALCONTROLLERCONNECTION2 POINTVERTICALCONTROLLERCONNECTION3 POINTVERTICALCONTROLLERCONNECTION4 POINTVERTICALCONTROLLERCONNECTION5 POINTVERTICALCONTROLLERCONNECTION6 POLYLINE-LAYER POLYLINE-LINETYPE POLYLINE-WIDTH SECONDLMRJPOINT XCOORDBRIDGEANDINPUTPORT1 XCOORDBRIDGEANDINPUTPORT2 XCOORDBRIDGEANDINPUTPORTRADIUSOFFSET XCOORDBRIDGEANDINPUTPORTSPACER XCOORDCONTROLLEREDGEEXTENSION XCOORDCONTROLLERPORTSPACER XCOORDINPUT XCOORDPORT2 XCOORDPORT3 XCOORDPORTFIRST XCOORDPORTFIRSTPREVIOUS XCOORDPORTLAST XCOORDPORTLASTPREVIOUS YCOORDBRIDGEANDINPUTEXTENSIONFROMLMRJ YCOORDBRIDGEANDINPUTPORTRADIUSOFFSET YCOORDBRIDGEANDINPUTPORTS YCOORDBRIDGEANDINPUTPORTSIZE YCOORDBRIDGEANDINPUTPORTSPACER YCOORDCONTROLLERPORTSIZE YCOORDCONTROLLERPORTSPACER YCOORDDISTANCETOCONTROLLERBOTFROMCONNECTION YCOORDDISTANCETOCONTROLLERBOTFROMCONNECTIONPREVIOUS YCOORDDISTANCETOCONTROLLERTOPFROMCONNECTION YCOORDINPUT YCOORDPORTS

		    )

 
;;;arguments
;;;  	GSR:
;;;  	products into RBproducts
;;;	GLOBALroomforrungbuilder into room
  
;;;	create compound lists

  
	(setq BridgeAndInputList (append RBDLMNETWORKBRIDGES RBDLMINTERFACES))
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
  
  	(setq xcoord 0)
	(setq ycoord 0)
	(setq insertionPoint (list xcoord ycoord))
  
  	(setq blockName (strcat "Z-RD-" rbroom))													;create string of roomname
	;;; BLOCK Header definition starts here:
	(entmake (list (cons 0 "BLOCK")(cons 2 blockName)(cons 70 2)(cons 10 insertionPoint)))								;begin block definition
	 
  	(setq xcoord 0)
	(setq ycoord 0.4)
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

  
  								
	(setq xCoordControllerStart 180)

	

	;set maxs to 0
	(setq xcoordmax 0)
	(setq ycoordmax 0)



  	;;;set shifters and spacers
  	
	(setq xCoordBridgeAndInputSpacer 0)
	(setq xDirectionBridgeAndInput 1)
	(setq xShifterBridgeAndInput (* xDirectionBridgeAndInput xCoordBridgeAndInputSpacer))
  	(setq yCoordBridgeAndInputSpacer 20)
  	(setq yCoordDistanceToBridgeAndInputTop 147)

  
  	(setq yDirectionBridgeAndInput -1)
  	(setq yCoordShifterBridgeAndInputSpacer 126)
  	(setq yShifterBridgeAndInput (* yDirectionBridgeAndInput yCoordShifterBridgeAndInputSpacer))
	
  	(setq xCoordBridgeAndInput 30)
  	(setq xCoordConnectionExtension 25)
  	(setq yCoordBridgeAndInput -54.3901)
  	(setq insertionpoint (list xCoordInput yCoordInput))
  

  
	;;;  	reset indexs and counters
	(setq cntr 0)
  	(setq cmax (length BridgeAndInputList))
  
	(setq xCoordBridgeAndInputPortSpacer 8.1706)
  	(setq yCoordBridgeAndInputPortSpacer 4.3610)
  	(setq xCoordBridgeAndInputPortRadiusOffset 0.6978)
	(setq yCoordBridgeAndInputPortSize 11.6498)
  	(setq yCoordBridgeAndInputPortRadiusOffset 2.8088)
  	(setq yCoordBridgeAndInputPortRadiusOffset2 3.1912)
  
	(setq cls 0)
  	(setq polyline-layer "0")
  	(setq polyline-width 0.6)
  	(setq polyline-linetype "bylayer")  
	;;;  	while loop for the Network Bridge and Input Devices
		(while	(< cntr cmax)															;while loop -- loop through the controllers
			(setq yShifterBridgeAndInput (* yDirectionBridgeAndInput yCoordShifterBridgeAndInputSpacer))
		  
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
				  	(setq xCoordBridgeAndInput 30)
				  	(setq yCoordBridgeAndInput -115)
					(setq insertionpoint (list xCoordBridgeAndInput yCoordBridgeAndInput))
					(entmod-blockinsert-attributes nil insertionpoint "0" BridgeAndInput)
					
				  	(setq xCoordBridgeAndInputPort1 xCoordBridgeAndInput)
				  	(setq xCoordBridgeAndInputPort2 (+ xCoordBridgeAndInputPort1 xCoordBridgeAndInputPortSpacer))
					(setq yCoordBridgeAndInputPorts (+ (+ yCoordBridgeAndInput yCoordBridgeAndInputPortSpacer) yCoordBridgeAndInputPortSize )) ; bridge is upside down, so add yspacer
				  	(setq BridgeAndInputPort1 (list xCoordBridgeAndInputPort1 yCoordBridgeAndInputPorts))
				  	(setq BridgeAndInputPort2 (list xCoordBridgeAndInputPort2 yCoordBridgeAndInputPorts))
					(entmod-blockinsert-attributes nil BridgeAndInputPort1 "0" finalLMRJUpsideDownBlockName)
					(entmod-blockinsert-attributes nil BridgeAndInputPort2 "0" finalLMRJUpsideDownBlockName)
				  	(setq insertionpoint (list xCoordBridgeAndInput yCoordBridgeAndInput))
					(setq yShifterBridgeAndInput -82.2223)

					(setq BridgeAndInputforControllerConnectionPoint1 (list (+ xCoordBridgeAndInputPort2 xCoordBridgeAndInputPortRadiusOffset) (- yCoordBridgeAndInputPorts  yCoordBridgeAndInputPortRadiusOffset)))
					(setq BridgeAndInputforControllerConnectionPoint2 (list (+ xCoordBridgeAndInputPort2 xCoordBridgeAndInputPortRadiusOffset) (+ yCoordBridgeAndInputPorts  yCoordBridgeAndInputPortRadiusOffset2)))
					(setq BridgeAndInputforControllerConnectionPoint3 (list xCoordControllerStart (+ yCoordBridgeAndInputPorts  yCoordBridgeAndInputPortRadiusOffset2)))

					(setq BridgeAndInputConnectionPoint1 (list (- xCoordBridgeAndInputPort1 xCoordBridgeAndInputPortRadiusOffset) (- yCoordBridgeAndInputPorts  yCoordBridgeAndInputPortRadiusOffset)))
					(setq BridgeAndInputConnectionPoint2 (list (- xCoordBridgeAndInputPort1 xCoordBridgeAndInputPortRadiusOffset) (+ yCoordBridgeAndInputPorts  yCoordBridgeAndInputPortRadiusOffset2)))
					(setq BridgeAndInputConnectionPoint3 (list (- xCoordBridgeAndInputPort1 xCoordConnectionExtension) (+ yCoordBridgeAndInputPorts  yCoordBridgeAndInputPortRadiusOffset2)))
					(setq BridgeAndInputConnectionPoint4 (list (- xCoordBridgeAndInputPort1 xCoordConnectionExtension) (+ yCoordBridgeAndInput (/ yShifterBridgeAndInput 2))))
					)
				  	;;; item is anything other than bridge
				  	(progn
				  	(setq xCoordBridgeAndInput 30)
				  	(setq yCoordBridgeAndInput -115)
					(setq insertionpoint (list xCoordBridgeAndInput yCoordBridgeAndInput))
					(entmod-blockinsert-attributes nil insertionpoint "0" BridgeAndInput)
					  
				  	(setq xCoordBridgeAndInputPort1 xCoordBridgeAndInput)
				  	(setq xCoordBridgeAndInputPort2 (+ (* (+ xCoordBridgeAndInputPort1 xCoordBridgeAndInputPortSpacer) xCoordBridgeAndInputPortRadiusOffset 2 )))
					(setq yCoordBridgeAndInputPorts (- (- yCoordBridgeAndInput yCoordBridgeAndInputPortSpacer) yCoordBridgeAndInputPortSize ))
				  	(setq BridgeAndInputPort1 (list xCoordBridgeAndInputPort1 yCoordBridgeAndInputPorts))
				  	(setq BridgeAndInputPort2 (list xCoordBridgeAndInputPort2 yCoordBridgeAndInputPorts))
				  	(setq insertionpoint (list xCoordBridgeAndInput yCoordBridgeAndInput))  
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
					(setq insertionpoint (list xCoordBridgeAndInput yCoordBridgeAndInput))
					(entmod-blockinsert-attributes nil insertionpoint "0" BridgeAndInput)
				  	(setq xCoordBridgeAndInputPort1 xCoordBridgeAndInput)
				  	(setq xCoordBridgeAndInputPort2 (+ xCoordBridgeAndInputPort1 xCoordBridgeAndInputPortSpacer))
					(setq yCoordBridgeAndInputPorts (- (- yCoordBridgeAndInput yCoordBridgeAndInputPortSpacer) yCoordBridgeAndInputPortSize ))
				  	(setq BridgeAndInputPort1 (list xCoordBridgeAndInputPort1 yCoordBridgeAndInputPorts))
				  	(setq BridgeAndInputPort2 (list xCoordBridgeAndInputPort2 yCoordBridgeAndInputPorts))
					(entmod-blockinsert-attributes nil BridgeAndInputPort1 "0" finalLMRJBlockName)
					(entmod-blockinsert-attributes nil BridgeAndInputPort2 "0" finalLMRJBlockName)
				  	(setq insertionpoint (list xCoordBridgeAndInput yCoordBridgeAndInput))
					
					(setq BridgeAndInputPortforPreviousConnection BridgeAndInputPort1)
					(setq BridgeAndInputConnectionPoint5 (list (+ xCoordBridgeAndInputPort2 xCoordConnectionExtension) (- yCoordBridgeAndInput (/ yShifterBridgeAndInputPrevious 2))))
					(setq BridgeAndInputConnectionPoint6 (list (+ xCoordBridgeAndInputPort2 xCoordConnectionExtension) (- yCoordBridgeAndInputPorts  yCoordBridgeAndInputPortRadiusOffset2)))
					(setq BridgeAndInputConnectionPoint7 (list xCoordBridgeAndInputPort2 (- yCoordBridgeAndInputPorts  yCoordBridgeAndInputPortRadiusOffset2)))
					(setq BridgeAndInputConnectionPoint8 (list xCoordBridgeAndInputPort2 (+ yCoordBridgeAndInputPorts  yCoordBridgeAndInputPortRadiusOffset)))
					(setq point-list (list BridgeAndInputConnectionPoint1 BridgeAndInputConnectionPoint2 BridgeAndInputConnectionPoint3 BridgeAndInputConnectionPoint4 BridgeAndInputConnectionPoint5 BridgeAndInputConnectionPoint6 BridgeAndInputConnectionPoint7 BridgeAndInputConnectionPoint8))
					(draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)

					(setq BridgeAndInputConnectionPoint1 (list (- xCoordBridgeAndInputPort1 xCoordBridgeAndInputPortRadiusOffset) (+ yCoordBridgeAndInputPorts  yCoordBridgeAndInputPortRadiusOffset)))
					(setq BridgeAndInputConnectionPoint2 (list (- xCoordBridgeAndInputPort1 xCoordBridgeAndInputPortRadiusOffset) (- yCoordBridgeAndInputPorts  yCoordBridgeAndInputPortRadiusOffset2)))
					(setq BridgeAndInputConnectionPoint3 (list (- xCoordBridgeAndInputPort1 xCoordConnectionExtension) (- yCoordBridgeAndInputPorts  yCoordBridgeAndInputPortRadiusOffset2)))
					(setq BridgeAndInputConnectionPoint4 (list (- xCoordBridgeAndInputPort1 xCoordConnectionExtension) (+ yCoordBridgeAndInput (/ yShifterBridgeAndInput 2))))
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

		  

		  	;save pervious points for connection points 5,6,7,8
		  	(setq yShifterBridgeAndInputPrevious yShifterBridgeAndInput)
			(setq yCoordBridgeAndInput (+ yCoordBridgeAndInput yShifterBridgeAndInput))

			(setq BridgeAndInputPort1Previous BridgeAndInputPort1)
		  	(setq BridgeAndInputPort2Previous BridgeAndInputPort2)
		  	
			(setq cntr (+ cntr 1))														;increment counter

		)



	;;;  	reset indexs and counters
	(setq cntr 0)
  	(setq cntr2 0)
  	(setq cntr3 0)
  	(setq cntr4 0)
  	(setq cntr5 0)
  	(setq cmax (length ControllerList))
  	(setq cmax2 (length RBDLMINTERFACES))
	(setq cmax3 (length RBDLMOCCSENSORS))
	(setq cmax4 (length RBDLMSWITCHES))

  	(setq perPortOcc (/ (length RBDLMOCCSENSORS) RBUseableControllerPorts))
  	(setq perPortSw (/ (length RBDLMSWITCHES) RBUseableControllerPorts))
  	(setq lastController 0)


  
  	;;;set shifters and spacers
  	
	(setq xCoordControllerSpacer 270)
	(setq xDirectionController 1)
	(setq xShifterController (* xDirectionController xCoordControllerSpacer))
  	(setq yCoordControllerSpacer 20)
  	(setq yCoordDistanceToControllerTop 147)
  	(setq yDirectionController -1)
  	(setq yShifterController (* yDirectionController (+ yCoordControllerSpacer yCoordDistanceToControllerTop)))
  	(setq xCoordControllerMin 179)
	(setq xCoordControllerMax 456)       
	
  	(setq xCoordController 180)
  	(setq yCoordController -54.3901)
  	(setq insertionpoint (list xCoordController yCoordController))
	(setq xCoordControllerPortSpacer 8.1706)
  	(setq yCoordControllerPortSpacer 4.3610)
  	(setq yCoordControllerPortSize 11.6498)
  
	(setq cls 0)
  	(setq polyline-layer "0")
  	(setq polyline-width 0.6)
  	(setq polyline-linetype "bylayer")
  
	;;;  	while loop for the controllers
		(while	(< cntr cmax)															;while loop -- loop through the controllers

		  
			(setq Controller(nth cntr ControllerList))											;pull first product off list into Controller

		  	(if
			  	(= cntr (- cmax 1))
				(setq lastController 1)
			)
			;(setq CBreturnList (controller-builder Controller perPortOcc RBDLMOCCSENSORS cntr3 cmax3 perPortSw RBDLMSWITCHES cntr4 cmax4 OnePortList cntr5 cmax5 RBUseableControllerPorts lastController))

		  	(setq insertionpoint (list xCoordController yCoordController))


			(entmod-blockinsert-attributes nil insertionpoint "0" Controller)
		  
		  	(if
			  	(OR (= Controller "LMRC-222")(= Controller "LMRC-221"))
				(progn
				(setq yCoordDistanceToControllerTop 54.3901)
				(setq yCoordControllerHeight 106)

				)
			)

		  	(if
			  	(OR (= Controller "LMRC-213-347v")(= Controller "LMRC-212-347v")(= Controller "LMRC-211-347v")(= Controller "LMRC-213")(= Controller "LMRC-212")(= Controller "LMRC-211")(= Controller "LMPL-201"))
				(progn
				(setq yCoordDistanceToControllerTopFromConnection 54.3901)
				(setq yCoordDistanceToControllerBotFromConnection 45)
				(setq yCoordControllerHeight 106)
				(setq xCoordPortFirst xCoordController)
				(setq xCoordPort2 (+ xCoordController (*  xCoordControllerPortSpacer 1)))
				(setq xCoordPort3 (+ xCoordController (* xCoordControllerPortSpacer 2)))
				(setq xCoordPortLast (+ xCoordController (* xCoordControllerPortSpacer 3)))
				(setq yCoordPorts (- (- yCoordController yCoordControllerPortSpacer) yCoordControllerPortSize ))
				)
			)
	  
		  	(if
				(OR (= Controller "LMRC-102")(= Controller "LMRC-101"))
			  	(progn
				(setq yCoordDistanceToControllerTop 54.3901)
				(setq yCoordControllerHeight 106)
				)
			)
	  
		  	(if
				(OR (= Controller "LMPL-101"))
			  	(progn
				(setq yCoordDistanceToControllerTop 54.3901)
				(setq yCoordControllerHeight 106)
				)
			)
	  
	  	  	(if
			  	(OR (= Controller "LMRC-112-M")(= Controller "LMRC-111-M")(= Controller "LMRC-112")(= Controller "LMRC-111"))
				(progn
				(setq yCoordDistanceToControllerTop 147)
				(setq yCoordControllerHeight 147)
				)
			)
		  


		  	;;; if catch
		  	(if
			  	;;;first item
			  	;;;START HERE 2
			  	(= cntr 0)	
			  	(progn																				;connect to last Bridge or Input device
				(setq pointControllertoBridgeInputConnection4 (list xCoordPortFirst yCoordPorts))
				(setq point-list (list BridgeAndInputforControllerConnectionPoint1 BridgeAndInputforControllerConnectionPoint2 BridgeAndInputforControllerConnectionPoint3 pointControllertoBridgeInputConnection4))
				(draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)
				(setq firstLMRJPoint pointControllertoBridgeInputConnection4)
				(entmod-blockinsert-attributes nil firstLMRJPoint "0" finalLMRJBlockName)
				)
			)
		  
			
		  
		  


		  	;;; if catch
		  	;;; first part of both inbounds situations
		  	;;; save connection and LMRJ points
		  	(if
			  	;;; moving right
			  	(= xDirectionController 1)																	;test for moving left or right
				(progn																				;connection points for moving right
				(setq pointControllerConnection1 (list xCoordPortLast yCoordPorts))
				(setq pointControllerConnection2 (list xCoordPortLast (- yCoordPorts yCoordDistanceToControllerBotFromConnection)))
			  	(setq xCoordControllerPrevious xCoordController)
			  	(setq xCoordController (+ xShifterController xCoordController))
			  	(setq pointControllerConnection3 (list xCoordController (- yCoordPorts yCoordDistanceToControllerBotFromConnection)))
			  	(setq pointControllerConnection4 (list xCoordController yCoordPorts))
				(setq point-list (list pointControllerConnection1 pointControllerConnection2 pointControllerConnection3  pointControllerConnection4))				;save points for connecting line between controllers
				(setq firstLMRJPoint pointControllerConnection1)														;save points for LMRJ block
				(setq secondLMRJPoint pointControllerConnection4)
				)
			  	;;; moving left
				(progn																				;connection points for moving left
				;;;start connection, this is the only cable done in two parts
				(setq pointControllerConnection1 (list xCoordPortFirstPrevious yCoordPorts))
				(setq pointControllerConnection2 (list xCoordPortFirstPrevious (- yCoordPorts yCoordDistanceToControllerBotFromConnection)))
			  	(setq xCoordControllerPrevious xCoordController)
			  	(setq xCoordController (+ xShifterController xCoordController))
				(setq point-list (list pointControllerConnection1  pointControllerConnection2))											;save points for connecting line between controllers
			  	(if
				  	(/= cntr (- cmax 1))
				  	(progn
					(draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)									;draw first part of moving left line
					(setq pointControllerConnection2Previous pointControllerConnection2)  											;save last point for later
					(setq firstLMRJPoint pointControllerConnection1)  
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
				(setq xCoordControllerEdgeExtension 120)


				;;; if catch
			  	(if
				  	;;; moving right
				  	(= xDirectionController 1)																;test for moving left or right
					(progn																			;connection points for moving right
					(setq pointVerticalControllerConnection1 (list xCoordPortLast yCoordPorts))
					(setq pointVerticalControllerConnection2 (list xCoordPortLast (- yCoordPorts yCoordDistanceToControllerBotFromConnectionPrevious)))
					(setq pointVerticalControllerConnection3 (list (+ xCoordPortLast xCoordControllerEdgeExtension) (- yCoordPorts yCoordDistanceToControllerBotFromConnectionPrevious)))
					(setq pointVerticalControllerConnection4 (list (+ xCoordPortLast xCoordControllerEdgeExtension) (- (- (- yCoordPorts yCoordDistanceToControllerBotFromConnectionPrevious) yCoordControllerSpacer ) yCoordControllerHeight)))
					(setq pointVerticalControllerConnection5 (list xCoordPortLast (- (- (- yCoordPorts yCoordDistanceToControllerBotFromConnectionPrevious) yCoordControllerSpacer ) yCoordControllerHeight)))
					(setq pointVerticalControllerConnection6 (list xCoordPortLast (+ (- (- (- yCoordPorts yCoordDistanceToControllerBotFromConnectionPrevious) yCoordControllerSpacer ) yCoordControllerHeight ) yCoordDistanceToControllerBotFromConnection)))
					(setq point-list (list pointVerticalControllerConnection1 pointVerticalControllerConnection2 pointVerticalControllerConnection3 pointVerticalControllerConnection4 pointVerticalControllerConnection5 pointVerticalControllerConnection6))
					(setq firstLMRJPoint pointVerticalControllerConnection1)
					(setq secondLMRJPoint pointVerticalControllerConnection6)
					)
				  
				  	;;;;moving left
					(progn																			;connection points for moving right
					;;;finish connection started during move left, this is the only cable done in two parts
					;;; remember that the firstLMRJPoint is already set
				  	(setq pointControllerConnection3 (list xCoordPortLast (- yCoordPorts yCoordDistanceToControllerBotFromConnection)))
				  	(setq pointControllerConnection4 (list xCoordPortLast yCoordPorts))
					(setq point-list (list pointControllerConnection2Previous pointControllerConnection3 pointControllerConnection4 ))
					(draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)
					(setq secondLMRJPoint pointControllerConnection4)
					(entmod-blockinsert-attributes nil secondLMRJPoint "0" finalLMRJBlockName)
					
					(setq pointVerticalControllerConnection1 (list xCoordPortFirst yCoordPorts))
					(setq pointVerticalControllerConnection2 (list xCoordPortFirst (- yCoordPorts yCoordDistanceToControllerBotFromConnectionPrevious)))
					(setq pointVerticalControllerConnection3 (list (- xCoordPortFirst xCoordControllerEdgeExtension) (- yCoordPorts yCoordDistanceToControllerBotFromConnectionPrevious)))
					(setq pointVerticalControllerConnection4 (list (- xCoordPortFirst xCoordControllerEdgeExtension) (- (- (- yCoordPorts yCoordDistanceToControllerBotFromConnectionPrevious) yCoordControllerSpacer ) yCoordControllerHeight)))
					(setq pointVerticalControllerConnection5 (list xCoordPortFirst (- (- (- yCoordPorts yCoordDistanceToControllerBotFromConnectionPrevious) yCoordControllerSpacer ) yCoordControllerHeight)))
					(setq pointVerticalControllerConnection6 (list xCoordPortFirst (+ (- (- (- yCoordPorts yCoordDistanceToControllerBotFromConnectionPrevious) yCoordControllerSpacer ) yCoordControllerHeight ) yCoordDistanceToControllerBotFromConnection)))
					(setq point-list (list pointVerticalControllerConnection1 pointVerticalControllerConnection2 pointVerticalControllerConnection3 pointVerticalControllerConnection4 pointVerticalControllerConnection5 pointVerticalControllerConnection6))
					(setq firstLMRJPoint pointVerticalControllerConnection1)
					(setq secondLMRJPoint pointVerticalControllerConnection6)
					)
				)

				

				
					
				(setq yShifterController (* yDirectionController (+ yCoordControllerSpacer yCoordControllerHeight)))
				(setq yCoordController (+ yShifterController yCoordController))
				
				(setq xDirectionController (* xDirectionController -1))
				(setq xShifterController (* xDirectionController xCoordControllerSpacer))
				 
				  
				)

				
			  
			)
	  		;;; else, in bounds and finish drawing connections
		  	(progn
		  	(if
			  	(/= cntr (- cmax 1))
			  	(progn
				(draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)									
				(entmod-blockinsert-attributes nil firstLMRJPoint "0" finalLMRJBlockName)
				(entmod-blockinsert-attributes nil secondLMRJPoint "0" finalLMRJBlockName)
				)
			)
			)

		  
			(setq xCoordPortFirstPrevious xCoordPortFirst)
			(setq xCoordPortLastPrevious xCoordPortLast)		  
		  	(setq yCoordDistanceToControllerBotFromConnectionPrevious yCoordDistanceToControllerBotFromConnection)
			(setq cntr (+ cntr 1))														;increment counter

		)





  








  	(entmake
	(list
	(cons 0 "ENDBLK")
	)
	)  															;finish block defition


	(setq returnlist (list 1 1)) 

  
)


; --- entmod-blockinsert-attributes Sub Function ---
; inserts block
; inserts any existing attributes from the block definition table
; file out attributes from list sent in from calling function
; Arguments:
; attributelist		list of attribute values passed in from rungbuilder
; Alex Lundin 03-31-2017
(defun entmod-blockinsert-attributes
       				(
				attributelist blkinsertionpoint layername blockname 
				/
				Attdefs ATTBLK ATTBLKNAME ATTRIBUTEDXF11 ATTRIBUTEINSERTIONPOINT ATTRIBUTEXCOORD ATTRIBUTEYCOORD BLOCKXCOORD
				BLOCKYCOORD DATA ENAME NEWINSERTIONPOINT NEWXCOORD NEWYCOORD NEXTENT entmod-blockinsert-attributes-cntr ATTRIBUTEDXF10 ATTRIBUTEVALUE NEWINSERTIONPOINTDXF10 NEWINSERTIONPOINTDXF11
				)
;;;arguments
;;;  	attributevalue - value to place into block
;;;  	blkinsertionpoint - insertion point for filled out block
;;;	layername - name of layer to insert block on
;;;	riserblockname - name of cabinet passed in from rungbuilder


	;;BLOCKTABLE search, call placeholder later
  
	(setq attblkname blockname)
	(setq entmod-blockinsert-attributes-cntr 0)
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
						(setq attributevalue (nth entmod-blockinsert-attributes-cntr attributelist))	;get first attribute value from the list of them
						(setq entmod-blockinsert-attributes-cntr (+ entmod-blockinsert-attributes-cntr 1));increase counter for next loop
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
			(entmake '((0 . "SEQEND")(8 . "0")))    								;---third add, entmake SEQEND to signify final attribute at end of block
		)
		)														;--cond statement
		(T nil)
	)															;-end cond


)



(defun controller-builder 	(
				CB-Controller CB-perControllerInt CB-RBDLMINTERFACES CB-cntr2  CB-cmax2 CB-perControllerOcc CB-RBDLMOCCSENSORS CB-cntr3 CB-cmax3 CB-perControllerSw CB-RBDLMSWITCHES CB-cntr4 CB-cmax4 CB-OnePortList CB-cntr5 CB-cmax5 CB-UseableControllerPorts CB-lastController
				 
				/
				CBCURRENTUSEABLECONTROLLERPORTS CBPRODUCT CBXCOORD CBYCOORD RETURNLIST
			  	)


	  	(if
		  	(OR (= CB-Controller "LMRC-222")(= CB-Controller "LMRC-221"))
			(progn
			(setq CBCurrentUseableControllerPorts 2)
			)
		)

	  	(if
		  	(OR (= CB-Controller "LMRC-213-347v")(= CB-Controller "LMRC-212-347v")(= CB-Controller "LMRC-211-347v")(= CB-Controller "LMRC-213")(= CB-Controller "LMRC-212")(= CB-Controller "LMRC-211")(= CB-Controller "LMPL-201"))
			(progn
			(setq CBCurrentUseableControllerPorts 2)
			)
		)
  
  	  	(if
		  	(OR (= CB-Controller "LMPL-201"))
			(progn
			(setq CBCurrentUseableControllerPorts 2)
			)
		)
  
	  	(if
			(OR (= CB-Controller "LMRC-102")(= CB-Controller "LMRC-101"))
		  	(progn
			(setq CBCurrentUseableControllerPorts 1)
			)
		)
  
	  	(if
			(OR (= CB-Controller "LMPL-101"))
		  	(progn
			(setq CBCurrentUseableControllerPorts 1)
			)
		)
  
  	  	(if
		  	(OR (= CB-Controller "LMRC-112-M")(= CB-Controller "LMRC-111-M")(= CB-Controller "LMRC-112")(= CB-Controller "LMRC-111"))
			(progn
			(setq CBCurrentUseableControllerPorts 0)
			)
		)



	  	(if
		  	(= CB-lastController 0)
		  	(progn
			(repeat CBCurrentUseableControllerPorts
			  	(progn

				(repeat CB-perControllerOcc													;while loop -- loop through the interfaces
				  	(progn
					(setq CBproduct (nth CB-cntr3 CB-RBDLMOCCSENSORS))									;pull first interface off list into Controller
					(setq CB-cntr3 ( + CB-cntr3 1))
					)
				)

				
				(repeat CB-perControllerSw													;while loop -- loop through the interfaces
				  	(progn
					(setq CBproduct (nth CB-cntr4 CB-RBDLMSWITCHES))									;pull first interface off list into Controller
					(setq CB-cntr4 ( + CB-cntr4 1))
					)
				)
				)
			)
			)
		)



	  	(if
		  	(= CB-lastController 1)
		  	(progn
			(setq CBCurrentUseableControllerPorts ( + CBCurrentUseableControllerPorts 1 ))

			(repeat CBCurrentUseableControllerPorts
			  	(progn

				(repeat CB-perControllerOcc													;while loop -- loop through the interfaces
				  	(progn
					(setq CBproduct (nth CB-cntr3 CB-RBDLMOCCSENSORS))									;pull first interface off list into Controller
					(setq CB-cntr3 ( + CB-cntr3 1))
					)
				)
				
				(while
				  	(< CB-cntr3 CB-cmax3)													;while loop -- loop through the interfaces
					(setq CBproduct (nth CB-cntr3 CB-RBDLMOCCSENSORS))									;pull first interface off list into Controller
					(setq CB-cntr3 ( + CB-cntr3 1))
				)

				(repeat CB-perControllerSw													;while loop -- loop through the interfaces
				  	(progn
					(setq CBproduct (nth CB-cntr4 CB-RBDLMSWITCHES))									;pull first interface off list into Controller
					(setq CB-cntr4 ( + CB-cntr4 1))
					)
				)			  
				
				(while
				  	(< CB-cntr4 CB-cmax4)													;while loop -- loop through the interfaces
					(setq CBproduct (nth CB-cntr4 CB-RBDLMSWITCHES))									;pull first interface off list into Controller
					(setq CB-cntr4 ( + CB-cntr4 1))
				)

				
				)
			)
			)
		  
		)
  


  

		(setq CBxcoord 0)
		(setq CBycoord 0)	
		(setq returnList (list CB-cntr2 CB-cntr3 CB-cntr4 CBxcoord CBycoord))
)






; --- RiserHeirarchy Sub Function ---
; accept a list of products in from Main and seperates them into RH sublists for other functions
; Alex Lundin 03-17-2017
(defun RiserHeirarchy (RHproducts /
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
;;;		PortCalculator:					
;;;  		RHControllers				
;;;  		Rungbuilder:					
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
			(OR (= RHproduct "LMLS-105")(= RHproduct "LMLS-305")(= RHproduct "LMLS-400")(= RHproduct "LMLS-500")(= RHproduct "LMLS-600")(= RHproduct "LMPO-200")(= RHproduct "LMPS-6000")(= RHproduct "LMFC-011"))
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


(defun standalonesort ( / )
	(setq globalSTANDALONENUMERICAL (vl-sort globalSTANDALONEDOTTEDPAIRS (function (lambda (x y)(< (car x)(car y))))))					;numerical sort of segmentdottedpairs, this organizes them by segment number
)












(defun standalone-inserter
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
;;;	standalonesort:
;;;	globalSTANDALONENUMERICAL
;;;	segmentbuilder:
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
				

						(draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)



						
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


		(draw-lwpolyline point-list cls polyline-layer polyline-width polyline-linetype)



		
		(setq viewportnumber (+ viewportnumber 1))
		
	  	(setq viewportyorigin vpycoord)
		(setq standalonewidthmax 0)
		)
	)

  


)






;;;expects 0 as class value in variable cls
(defun draw-lwpolyline (point-list cls polyline-layer polyline-width polyline-linetype)
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


(defun draw-line (startpoint endpoint line-layer line-linetype)
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
(defun c:riserdraworder	( /
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






; --- RoundUp Sub Function ---
; take any decimal value and round up to the next whole integer
; Alex Lundin 03-17-2017
(defun RoundUp ( n )
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


; --- PortCalculator Sub Function ---
; accept a list of products in from Main and calculate types of ports avaiable for each item
; this function also calculates the number of each type of rung in global variables
; Alex Lundin 03-17-2017
(defun PortCalculator (PCproducts PCnumberofcontrollers / CMAX CNTR PCPRODUCT PORTS PORTSFORCONTROLLERCONNECTION useableControllerPorts PCCONTROLLERS PCNUMBEROFRUNG1 PCNUMBEROFRUNG2 PCNUMBEROFRUNG3 PCNUMBEROFRUNGFINAL RETURNLIST)
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
	  	(/= GlobalControllers 0)
		(progn
		(setq portsForControllerConnection (* 2 PCnumberofcontrollers))
		(setq portsForControllerConnection (- portsForControllerConnection 1))
		(setq useableControllerPorts (- ports portsForControllerConnection))
		(setq PCNUMBEROFRUNGFINAL 1)
		)
	)
  	(setq returnlist (list PCNUMBEROFRUNG1 PCNUMBEROFRUNG2 PCNUMBEROFRUNG3 PCNUMBEROFRUNGFINAL useableControllerPorts))
)