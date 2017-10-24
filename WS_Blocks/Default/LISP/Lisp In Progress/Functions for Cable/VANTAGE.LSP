; --- VANTAGE Function ---
; This sequence is designed to allow the user to run MSTP between LMBC-300's.
; The custom dialog boxes give helpful information along with options for outputing them.
; Alex Lundin 01-11-2017


;;; subfunction call formats
;;; truename
;;;  	(setq ent [nameofvariable with entity name desired to convert to truename])					;replace bracketed area with the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
;;;	(setq truename (blocktruename ent))										;call blocktruename function with the ent parameter set in the LWPOLYLINE above, set the truename variable to the final result
;;; entmod-layer
;;; 	(setq layer ["name of layer"])											;set layer to correct layer
;;;  	(setq ent [nameofvariable with entity name desired to convert to truename])					;replace bracketed area with the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
;;;	(entmod-layer ent layer)											;call entmod-layer on ent and layer arugments, no return value needed
;;; move-splines
;;;	(move-splines layer-from layer-to)

;;;default Titleblock layers
;;;_WS_CONTROLLER_BUS
;;;_WS_ENCLOSURE_BUS
;;;_WS_STATION_BUS
;;;_WS_18_3_AWG
;;;_WS_RS232

(defun C:VANTAGE ( / 	120OHM 120OHMS BLOCK BLOCKS CLAYER CMAX CMAX2 CNTR CNTR2
		  	ELIST ENT ENTDXF ENTLAYER EXPERT HIDELAYER
		  	LAYER LAYERLIST LISTADDER OLDECHO SELECTFLAG SHOWLAYER
		  	LWPOLYLINE SPLINELIST SPLINES SS TRUENAME UNIQUELAYERLIST
			LAYERFROM LAYERTO OBJECTTYPE INSTRUCTIONS-DRAWING-SET INSTRUCTIONS-TEXT-SET OK-CLICK
			INSTRUCTIONS-DRAWING-SET INSTRUCTIONS-TEXT-SET XCOORD YCOORD INSTRUCTIONS-DRAWING-SET INSTRUCTIONS-TEXT-SET OK-CLICK
		  	DCL_ID INSERTIONPOINT INSTRUCTIONS-DRAWING-SET INSTRUCTIONS-TEXT-SET STRING1 STRING2 STRING3 TEXTFILE USERCLICK
		 )													;GLOBAL VARIABLES -- OS, DEFPOINTSLAYER
  															;START VANTAGE
  
	(vl-load-com)
	(setq oldecho (getvar "CMDECHO"))										;set oldecho varaible to hold Autocad CMDECHO
	(setvar "CMDECHO" 0)												;set CMDECHO to 0, supress commandline echo

  	(setq os (getvar 'osmode))											;GLOBAL for BRIDGES3 -- set old osnaps to os
	(command "._OSNAP" "center")
  	(setq DefpointsLayer (tblsearch "LAYER" "Defpoints"))								;GLOBAL for VANTAGE3 -- get layer definition
	;create VANTAGE layers
  	;first are empty layers for cable drawing, they are purged at the end
	(command "-layer" "make" "CONTROLLER_BUS" "color" "10" "" "LType" "Continuous" "" "")
  	(command "-layer" "make" "ENCLOSURE_BUS" "color" "114" "" "LType" "Continuous" "" "")
  	(command "-layer" "make" "AUXILIARY" "color" "255" "" "LType" "Continuous" "" "")
  	(command "-layer" "make" "PANEL_COMPONENTS" "color" "255" "" "LType" "Continuous" "" "")
  	(command "-layer" "make" "STATION_BUS" "color" "200" "" "LType" "Continuous" "" "")
	(command "-layer" "make" "3_18_AWG" "color" "20" "" "LType" "Continuous" "" "")
	(command "-layer" "make" "RS232" "color" "4" "" "LType" "Continuous" "" "")
  	;vantage layer is used for notes during process, notes are deleted at the end
  	(command "-layer" "make" "VANTAGE" "color" "30" "" "LType" "Continuous" "" "")


  
  	(setq dcl_id (load_dialog "VANTAGE.dcl"))									;load dialog
 
  	(if (not (new_dialog "VANTAGE" dcl_id)			;test for dialog
 
      		);not
 
    		(exit)						;exit if no dialog
 
  		);if

	(action_tile
	"cancel"						;if cancel button pressed
	"(done_dialog) (setq ok-click nil)"			;close dialog, set flag
	);action_tile

	(action_tile
	"accept"						;if O.K. pressed
	" (done_dialog)(setq ok-click T))"			;close dialog, set flag
	);action tile


  	(action_tile
	"rb1"							;if instructions in drawing checked
	"(setq instructions-drawing-set T))"			;close dialog, set flag
	);action tile
  
  	(action_tile
	"rb2"							;if instructions in text checked
	"(setq instructions-text-set T))"			;close dialog, set flag
	);action tile
  
	(start_dialog)						;start dialog
 
  	(unload_dialog dcl_id)					;unload

	
(setq string1      	
;;;     	"
;;;	\n
;;;     	\n___________________________________________Change Notes___________________________________________
;;;     	\n
;;;     	\nPlease read these prompts, they will help you navigate the changes.
;;;     	\nUpdated 12-22-2016
;;;     	\nWe are creating a more streamlined process and there are minor changes to mention.
;;;     	\nIn the past we used corner point osnaps for MSTP wire to connect bridges.
;;;     	\nIn an effort to keep manual cable and automated cable consistent, BRIDGES uses center osnaps now
       "
     	\n
	\n____________________________________________VANTAGE_____________________________________________
	\n
     	\nFollow this series through to \"VANTAGE3\".
     	\nThe codes duplicate product blocks onto the Defpoints layer. They are deleted during \"VANTAGE3\".
	\nIf you skip the \"VANTAGE3\" step, it will affect the total number of blocks counted in a excel data extraction.
	\nThis series of commands will isolate certain networking products to assist in drawing polylines.
	\nDuring the series, the codes will handle all snap settings and layer settings for you.
	\nThere are buttons that can place the instructions into the current drawing or to a text file if you prefer.
	\nFeel free to use the VANTAGEWIRE function with the first VANTAGE command, it will draw cable for you.
	\nThe VANTAGEWIRE function is still in development, so you may have to use a combination of manual and automated wiring.
     	\n
     	\n___________________________________________Manual Version___________________________________________
     	\n
     	\n1)
     	\nSelect any objects (such as walls) that you want to see while drawing Vantage cable.
     	\nIf you are using a PDF, select the PDF (you will not be able to isolate the walls).
     	\nThen press enter.
     	\nThe layers from the objects you selected as well as any segment managers and network bridges will be the only objects visible.
     	\n
     	\n2)
     	\nUse polylines for all wire on Vantage parts, even on the integration to Wattstopper ones.
        \nUse the \"PLINE\" command to draw your wire between vantage devices.
     	\nUse several fitpoints across the gap between devices. This makes editing the polylines faster later
     	\nConnect the final fitpoint of the polyline to the center of a device (use F3 to turn osnap on\/off).
     	\nPress enter to end the polyline.
     	\nPress space to start a new polyline, continue this new polyline from the same center point of the previous device.
     	\n
     	"
 )

 (setq string2
	"
     	\n3)
     	\nAfter you complete the lines, you can use the MATCHPROPERTIES command to set them to the correct layer.
     	\nType \"MA\".
     	\nClick the product block that the wire connects.
     	\nSelect all the wires.
     	\nThey are changed immediatley.
     	\nThis is helpful for switching layers.
       	\n
       	\n4)
     	\nConnect all main panels (red blocks) with controller bus (red wire).
     	\nMaximum of 15 processors connected with 2 wire controller bus..
     	\nMaximum of 31 processors connected with Power Over Ethernet (POE) connection.
     	\n
     	\n5)
	\nConnect the first secondary on each run to the respective main with enclosure bus (green wires).
	\nEach main panel can have 3 secondary runs from the processor.
	\nThe runs are called \"2\", \"3\" and \"4\".
	\nEach secondary run from a processor can have a \"A\" and a \"B\" panel.
     	\nThis means each main panel can support a total of 6 secondaries.
     	\nThe panel names are \"2A\", \"2B\",\"3A\", \"3B\", \"4A\" and \"4B\".
     	\nConnect all secondary panels (green blocks) on each run with enclosure bus (green wires).
     	\n
	"
	)
  
	(setq string3
       	"
     	\n6)
     	\nConnect all station bus items (purple blocks) with station bus (purple wire).
	\nStart the station bus wire from a IC-DIN-LITE or a IC-36.
	\n
     	\n7)
     	\nConnect any integrated 3_18_AWG items (orange blocks) with 3_18_AWG wire (orange wire) to a vantage device that allows this connection.
     	\nVantage parts that support 3_18_AWG wire:
     	\nEQ41TB-TI and EQ73TB-TI.
     	\nOnly wire the orange blocks that you would like integrated into the Vantage Riser.
     	\n
     	\n8)
     	\nConnect any integrated RS232 items (cyan blocks) with RS232 wire (cyan wire) to a vantage device that allows this connection.
     	\nVantage parts that support RS232 wire:
     	\nIC-36 and IC-DIN-LITE.
     	\nOne connection per controller.
     	\nOnly wire the cyan blocks that you would like integrated into the Vantage Riser.
     	\n
      	\n
     	\n__________________________________________Automated Version__________________________________________
     	\n
     	\n1)
     	\n
     	\n2)
     	\n
     	\n3)
     	\n
      	"
      	)
  
  
	(cond
	  	((= instructions-drawing-set T)
		(setq insertionpoint (getpoint "Click point to place instructions"))
		 
			(entmake
			(list
			(cons 0 "MTEXT")         	;; Entity Name
			(cons 100 "AcDbEntity")  	;; Subclass Marker
			(cons 410 "Model")       	;; Space
			(cons 8 "VANTAGE")         	;; Layer
			(cons 100 "AcDbMText")   	;; Subclass Marker
			(cons 10 insertionpoint) 	;; Insertion Point
			(cons 40 5)            		;; Text Height
			(cons 71 1)              	;; Attachment Point (middle-center)
			(cons 1 string1)     		;; Text Content
			(cons 7 "Arial")		;text style
			)
			)

		 	(setq xcoord (nth 0 insertionpoint))
		 	(setq ycoord (nth 1 insertionpoint))
		 	(setq xcoord (+ xcoord 450))
		 	(setq insertionpoint (list xcoord ycoord))
		 
		 	(entmake
			(list
			(cons 0 "MTEXT")         	;; Entity Name
			(cons 100 "AcDbEntity")  	;; Subclass Marker
			(cons 410 "Model")       	;; Space
			(cons 8 "VANTAGE")         	;; Layer
			(cons 100 "AcDbMText")   	;; Subclass Marker
			(cons 10 insertionpoint) 	;; Insertion Point
			(cons 40 5)            		;; Text Height
			(cons 71 1)              	;; Attachment Point (middle-center)
			(cons 1 string2)     		;; Text Content
			(cons 7 "Arial")		;text style
			)
			)
		 
		 	(setq xcoord (nth 0 insertionpoint))
		 	(setq ycoord (nth 1 insertionpoint))
		 	(setq xcoord (+ xcoord 450))
		 
		 	(setq insertionpoint (list xcoord ycoord))
			(entmake
			(list
			(cons 0 "MTEXT")         	;; Entity Name
			(cons 100 "AcDbEntity")  	;; Subclass Marker
			(cons 410 "Model")       	;; Space
			(cons 8 "VANTAGE")         	;; Layer
			(cons 100 "AcDbMText")   	;; Subclass Marker
			(cons 10 insertionpoint) 	;; Insertion Point
			(cons 40 5)            		;; Text Height
			(cons 71 1)              	;; Attachment Point (middle-center)
			(cons 1 string3)     		;; Text Content
			(cons 7 "Arial")		;text style
			)
			)
		)

	  	((= instructions-text-set T)
		(setq textfile (open (strcat (getvar 'DWGPREFIX) "WS_AUTOCAD_TEXT_VANTAGE_Instructions.txt")  "a"))
		(close textfile)
		(setq textfile (open (strcat (getvar 'DWGPREFIX) "WS_AUTOCAD_TEXT_VANTAGE_Instructions.txt")  "w"))
		(write-LWPOLYLINE "This text file contains instructions for the VANTAGE command" textfile)
		(write-LWPOLYLINE (strcat string1) textfile)
		(write-LWPOLYLINE (strcat string2) textfile)
		(write-LWPOLYLINE (strcat string3) textfile)
		(close textfile)
		)

	)



  

  
  	(setq elist (ssget  	(prompt "\nPress esc now if you'd like to change your layer settings.
 					\nSelect objects on walls layer and other layers you would like shown while drawing Vantage cable:")))	;set elist to the selection and prompt user
  
  	(command "-layer" "thaw" "_WS_CAT5E" "")									;thaw each layer to reset title block
	(command "-layer" "thaw" "_WS_LMBC-300" "")
	(command "-layer" "thaw" "_WS_LMLS-400" "")
	(command "-layer" "thaw" "_WS_LMLS-500" "")
	(command "-layer" "thaw" "_WS_LMLS-600" "")
	(command "-layer" "thaw" "_WS_LMPL" "")
	(command "-layer" "thaw" "_WS_LMRJ" "")
	(command "-layer" "thaw" "_WS_LMSM-3E" "")
	(command "-layer" "thaw" "_WS_LMSM-6E" "")
	(command "-layer" "thaw" "_WS_MSTP" "")
	(command "-layer" "thaw" "_WS_120_OHM" "")

  	(layerstate-delete "WS-Layout")
  	(layerstate-save "WS-Layout" nil nil)										;create Layer state
  
  	(setq listAdder "WATTSTOPPER")											;set listAddder variable to "WATTSTOPPER"
	(setq layerList (cons listAdder layerList))									;add listAdder to layerList
  
  	(cond														;-conditional block
	  	((/= elist nil)												;-conditional statement
  		(setq cmax (sslength elist))										;set max
		(setq cntr 0)												;set cntr
  		(while 	(< cntr cmax)											;--while loop
			(setq ent(ssname elist cntr))									;set ent to item of elist that corresponds to cntr
		  	(setq entDXF (entget ent))									;set entDXF to dxf codes of ent
		  	(setq entLayer (cdr(assoc 8 entDXF)))								;set entLAyer to second element of 8th dxfcode
			(setq layerList (cons entLayer layerList))							;add entLayer to layerList
			(setq cntr (+ cntr 1))										;increment cntr
		)													;--end while
		(while layerList											;--while loop
        		(setq x (car layerList)										;remove duplicates from layerList
             		layerList (vl-remove x (cdr layerList))
              		uniqueLayerList (cons x uniqueLayerList)							;call new list uniqueLayerList
        	)
    		)
    		(reverse uniqueLayerList)										;reverse uniqueLayerList to account for cons property
		)													;-end conditional statement
		(t													;-conditional t statement
		(setq uniqueLayerList layerList)
		)													;-end conditional t statement
	)														;-end conditional block
  	

  	;;;move product blocks to correct layers
	(setq blocks(ssget "x" '((0 . "INSERT")(8 . "WATTSTOPPER"))))							;select all blocks on WATTSTOPPER layer, store to blocks varaible

	(cond
	  	((/= blocks nil)											;-conditional statement
	 	(setq cntr 0)												;set counter variable cntr to 0 before each loop
 		(setq cmax (sslength blocks))
		(while 	(< cntr cmax)											;--while loop, loop continues will counter cntr is less than length of blocks selection set
			(setq block(ssname blocks cntr))								;set bridge varaible to the member in blocks that corresponds to the current counter cntr variable

			(setq ent block)										;set ent to the entity stored in block
			(setq truename (blocktruename ent))								;call blocktruename function with the ent parameter set in the LWPOLYLINE above, set the truename variable to the final result

			(if												;-inner if
			  	(OR											;-- or inside the if

				(= truename "V-LCAP44H")
				(= truename "V-LCAP44M")
				(= truename "V-LCAP32M")
				)											;-- end or
			  	(progn											;--progn wrap the if
			 	(setq layer "CONTROLLER_BUS")								;set layer to correct layer
			  	(setq ent block)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed
				)											;--end progn wrap
			)												;-end inner if
			(if												;-inner if
			  	(OR											;-- or inside the if
				(= truename "V-LCAP44HS")
				(= truename "V-LCAP32S")
				(= truename "V-LCAP44S")
				)											;-- end or
			  	(progn											;--progn wrap the if
			 	(setq layer "ENCLOSURE_BUS")								;set layer to correct layer
			  	(setq ent block)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed
				)											;--end progn wrap
			)												;-end inner if
		  	(if												;-inner if
			  	(OR											;-- or inside the if
				(= truename "V-LCAP44A")
				(= truename "V-LCAP44L")
				)											;-- end or
			  	(progn											;--progn wrap the if
			 	(setq layer "AUXILIARY")								;set layer to correct layer
			  	(setq ent block)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed
				)											;--end progn wrap
			)
			(if												;-inner if
			  	(OR											;-- or inside the if
				(= truename "V-STPSRW101")
				(= truename "V-STPSRW201")
				(= truename "V-STPERW101")
				(= truename "V-STPERW201")
				(= truename "V-IC-36")
				(= truename "V-IC-DIN-II-LITE-RF")
				(= truename "V-IC-DIN-II-LITE")
				)											;-- end or
			  	(progn											;--progn wrap the if
			 	(setq layer "PANEL_COMPONENTS")								;set layer to correct layer
			  	(setq ent block)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed
				)											;--end progn wrap
			)												;-end inner if
			(if												;-inner if
			  	(OR											;-- or inside the if
				(= truename "V-EASYTOUCH-II-1")
				(= truename "V-EASYTOUCH-II-2")
				(= truename "V-EASYTOUCH-II-3")
				(= truename "V-EASYTOUCH-II-4")
				(= truename "V-EASYTOUCH-II-5")
				(= truename "V-EQ40TB-TI")
				(= truename "V-DMX-DALI-GW")
				(= truename "V-EM-LIGHTSENSOR")
				(= truename "V-LVOS")
				)											;-- end or
			  	(progn											;--progn wrap the if
			 	(setq layer "STATION_BUS")								;set layer to correct layer
			  	(setq ent block)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed
				)											;--end progn wrap
			)												;-end inner if
			(if												;-inner if
			  	(OR											;-- or inside the if
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
				)											;-- end or
			  	(progn											;--progn wrap the if
			 	(setq layer "3_18_AWG")									;set layer to correct layer
			  	(setq ent block)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed
				)											;--end progn wrap
			)												;-end inner if
			(if												;-inner if
			  	(OR											;-- or inside the if
				(= truename "LMDI-100")
				)											;-- end or
			  	(progn											;--progn wrap the if
			 	(setq layer "RS232")									;set layer to correct layer
			  	(setq ent block)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed
				)											;--end progn wrap
			)												;-end inner if
		  
			(setq cntr(+ cntr 1))  										;increment counter cntr    
		)													;--end while
		)													;-end conditional statement
	)														;-endconditional block

  	(setq objecttype "LWPOLYLINE")
	(setq layerfrom	"_WS_CONTROLLER_BUS")
	(setq layerto	"CONTROLLER_BUS")
	(move-objects objecttype layerfrom layerto)

  	(setq objecttype "LWPOLYLINE")
	(setq layerfrom	"_WS_ENCLOSURE_BUS")
	(setq layerto	"ENCLOSURE_BUS")
	(move-objects objecttype layerfrom layerto)

  	(setq objecttype "LWPOLYLINE")
	(setq layerfrom	"_WS_STATION_BUS")
	(setq layerto	"STATION_BUS")
	(move-objects objecttype layerfrom layerto)

  	(setq objecttype "LWPOLYLINE")
	(setq layerfrom	"_WS_3_18_AWG")
	(setq layerto	"3_18_AWG")
	(move-objects objecttype layerfrom layerto)

  	(setq objecttype "LWPOLYLINE")
	(setq layerfrom	"_WS_RS232")
	(setq layerto	"RS232")
	(move-objects objecttype layerfrom layerto)

	(setq objecttype "LWPOLYLINE")
	(setq layerfrom	"_WS_PANEL_COMPONENTS")
	(setq layerto "PANEL_COMPONENTS")
  	(move-objects objecttype layerfrom layerto)
  
	(setq clayer (getvar "clayer") expert (getvar "expert"))							;get current layer
  	(setvar "expert" 1)												;set system variable
	(command "-layer" "make" "Defpoints" "Color" "251" "" "LType" "Continuous" "" "")
  	(command "-layer" "off" "*" "")										;turn off all layers, turn on clayer varialbe
    	(cond														;-conditional block						
		((/= uniqueLayerList nil)										;-conditional statement
		(setq cntr 0)
		(setq cmax (length uniqueLayerList))
		(while	(< cntr cmax)											;--while loop to show layers
			(setq showLayer (nth cntr uniqueLayerList))
  			(command "-layer" "on" showLayer "")
			(setq cntr (+ cntr 1))
		)													;--end while
		)													;-end conditional statement
	)														;-end conditional block
  
	(setq ss (ssget "x" '((8 . "Defpoints"))))									;create selection set from Defpoints layer
  	(cond														;-conditional block
		((/= ss nil)												;-conditional statement
		(command "-Layer" "make" "Defpoints2" "Color" "251" "" "LType" "Continuous" "" "")			;create Defpoints2 layer
		(command "-layer" "on" "0" "")									;turn 0 on to allow editing of Defpoints
		(command "copytolayer" "p" "" "Defpoints2" "d" "0,0")							;paste selection from Defpoints to Defpoints2
		(command "erase" "p" "")										;delete the objects on Defpoints
		(command "-layer" "off" "0" "")									;turn 0 off
		)													;-end conditional statement
	) 														;-end conditional block
  
	(cond														;-conditional block						
		((/= uniqueLayerList nil)										;-conditional statement		
		(setq cntr 0)
		(setq cmax (length uniqueLayerList))
		(while 	(< cntr cmax)											;--while loop
			(setq layer (nth cntr uniqueLayerList))
			(setq ss (ssget "x" (list (cons 8 layer))))
			(command "copytolayer" "p" "" "Defpoints" "d" "0,0")
			(setq cntr (+ cntr 1))
		)													;--end while loop
		(setq cntr2 0)
		(setq cmax2 (length uniqueLayerList))
		(while	(< cntr2 cmax2)											;-while loop to hide layers
			(setq hideLayer (nth cntr2 uniqueLayerList))
  			(command "-layer" "off" hideLayer "")
			(setq cntr2 (+ cntr2 1))
		)													;-end while loop
		)													;-end conditional statement
	)														;-end conditional block

  


  	(command "-layer" "off" "Defpoints2" "")									;toggle layers
  	(command "-layer" "on" "Defpoints" "")
  	(command "-layer" "on" "CONTROLLER_BUS" "")
	(command "-layer" "on" "ENCLOSURE_BUS" "")
  	(command "-layer" "on" "PANEL_COMPONENTS" "")
	(command "-layer" "on" "AUXILIARY" "")
  	(command "-layer" "on" "STATION_BUS" "")
  	(command "-layer" "on" "3_18_AWG" "")
  	(command "-layer" "on" "RS232" "")
  	(command "-layer" "on" "VANTAGE" "")
  
  	(command "clayer" "CONTROLLER_BUS")
  	(setvar "cmdecho" oldecho)
  	(princ)

)															;END BRIDGES



; --- VANTAGE2 Function ---
(defun C:VANTAGE2 	(
		 	/
		   	BLOCKS OLDECHO
		   	BLOCK CMAX CNTR DCL_ID ENT INSERTIONPOINT INSTRUCTIONS-DRAWING-SET INSTRUCTIONS-TEXT-SET LAYER OK-CLICK STRING1 TEXTFILE TRUENAME XCOORD YCOORD
			 LAYERCHECK
		   	)
  															;GLOBAL VARIABLES - NONE
  															;START VANTAGE2

  	(setq oldecho (getvar "CMDECHO"))										;set oldecho varaible to hold Autocad CMDECHO
	(setvar "CMDECHO" 0)												;set CMDECHO to 0, supress commandline echo

  	(setq dcl_id (load_dialog "VANTAGE2.dcl"))									;load dialog
 
  	(if (not (new_dialog "VANTAGE2" dcl_id)			;test for dialog
 
      		);not
 
    		(exit)						;exit if no dialog
 
  		);if

	(action_tile
	"cancel"						;if cancel button pressed
	"(done_dialog) (setq ok-click nil)"			;close dialog, set flag
	);action_tile

	(action_tile
	"accept"						;if O.K. pressed
	" (done_dialog)(setq ok-click T))"			;close dialog, set flag
	);action tile


  	(action_tile
	"rb1"							;if instructions in drawing checked
	"(setq instructions-drawing-set T))"			;close dialog, set flag
	);action tile
  
  	(action_tile
	"rb2"							;if instructions in text checked
	"(setq instructions-text-set T))"			;close dialog, set flag
	);action tile
  
	(start_dialog)						;start dialog
 
  	(unload_dialog dcl_id)					;unload

  	(setq string1
	"
	\n
	\n______________________________________________VANTAGE2______________________________________________
	\n
	\nVerify all the wires are correct and identify which components are in each panel. After you press OK:
	\n                                                                                                                                                           
	\n1)	
	\nAll remaining product blocks will be visible.
	\n
	\n2)	
	\nVerify that your cables are not crossing:
	\nWires - avoid crossing cables of any kind at all times.
	\nParts   - room controllers, sensors, switches, etc...
	\n
	\3)
	\nWhen possible, verify that your cables are not crossing:
  	\nWalls - run cabling through hallways when possible.
  	\n
	\n4)
	\nMove any cables that you need to.
	\nSelect the polyline with a left click, and then hover over a blue fit point to get sub options for the polyline fit points.
	\nFrom here you can stretch\/add\/remove fit points.
	\n
	\n5)	
	\nDraw boundaries around all the components in each panel.
	\nThis will make it clear what parts are in each cabinet.
	\nUse the \"RECT\" command to draw rectangles.
	\nThe _WS_VANTAGE_PANEL_BOUNDARY is a no plot layer, so it will not show on the print.
	\n
	\n6)	
	\nuse \"VANTAGE3\" when you are done.
	\n
	"
	)


	(cond
	  	((= instructions-drawing-set T)
		(setq insertionpoint (getpoint "Click point to place instructions"))
		 
			(entmake
			(list
			(cons 0 "MTEXT")         	;; Entity Name
			(cons 100 "AcDbEntity")  	;; Subclass Marker
			(cons 410 "Model")       	;; Space
			(cons 8 "VANTAGE")         	;; Layer
			(cons 100 "AcDbMText")   	;; Subclass Marker
			(cons 10 insertionpoint) 	;; Insertion Point
			(cons 40 5)            		;; Text Height
			(cons 71 1)              	;; Attachment Point (middle-center)
			(cons 1 string1)     		;; Text Content
			(cons 7 "Arial")		;text style
			)
			)

		 	(setq xcoord (nth 0 insertionpoint))
		 	(setq ycoord (nth 1 insertionpoint))
		)

	  	((= instructions-text-set T)
		(setq textfile (open (strcat (getvar 'DWGPREFIX) "WS_AUTOCAD_TEXT_VANTAGE2_Instructions.txt")  "a"))
		(close textfile)
		(setq textfile (open (strcat (getvar 'DWGPREFIX) "WS_AUTOCAD_TEXT_VANTAGE2_Instructions.txt")  "w"))
		(write-LWPOLYLINE "This text file contains instructions for the VANTAGE2 command" textfile)
		(write-LWPOLYLINE (strcat string1) textfile)
		(close textfile)
		)

	)

  	(command "-layer" "on" "WATTSTOPPER" "")



  	(command "-layer" "on" "_WS_CAT5E" "")										;turn layers on
	(command "-layer" "on" "_WS_LMBC-300" "")
	(command "-layer" "on" "_WS_LMLS-400" "")
	(command "-layer" "on" "_WS_LMLS-500" "")
	(command "-layer" "on" "_WS_LMLS-600" "")
	(command "-layer" "on" "_WS_LMPL" "")
	(command "-layer" "on" "_WS_LMRJ" "")
	(command "-layer" "on" "_WS_LMSM-3E" "")
	(command "-layer" "on" "_WS_LMSM-6E" "")
	(command "-layer" "on" "_WS_MSTP" "")
	(command "-layer" "on" "_WS_120_OHM" "")



  
	(C:WSFRONT)													;call WSFRONT
  	(setvar "cmdecho" oldecho)
  	(princ)
)															;END VANTAGE2



; --- VANTAGE3 Function ---
(defun C:VANTAGE3 ( / 	BLOCK BLOCKS CMAX CNTR DEFCOLOR DEFLT DEFPOINTS LAYEREXISTS OLDECHO
		   	LWPOLYLINE SPLINELIST SPLINES SS TRUENAME VLAOBJECT
		   DCL_ID ENT INSERTIONPOINT INSTRUCTIONS-TEXT-SET LAYER OK-CLICK STRING1 TEXTFILE XCOORD YCOORD
		   LAYERFROM LAYERTO OBJECTTYPE
		  )
  															;GLOBAL VARIABLES - NONE
  															;OS passed in from BRIDGES
															;START VANTAGE3
  	(setq oldecho (getvar "CMDECHO"))										;set oldecho varaible to hold Autocad CMDECHO
	(setvar "CMDECHO" 0)												;set CMDECHO to 0, supress commandline echo
	





  	(setq dcl_id (load_dialog "VANTAGE3.dcl"))									;load dialog
 
  	(if (not (new_dialog "VANTAGE3" dcl_id)			;test for dialog
 
      		);not
 
    		(exit)						;exit if no dialog
 
  		);if

	(action_tile
	"cancel"						;if cancel button pressed
	"(done_dialog) (setq ok-click nil)"			;close dialog, set flag
	);action_tile

	(action_tile
	"accept"						;if O.K. pressed
	" (done_dialog)(setq ok-click T))"			;close dialog, set flag
	);action tile


  	(action_tile
	"rb1"							;if instructions in drawing checked
	"(setq instructions-text-set T))"			;close dialog, set flag
	);action tile
  
  
	(start_dialog)						;start dialog
 
  	(unload_dialog dcl_id)					;unload

  	(setq string1
	"
	\n
	\n______________________________________________VANTAGE3______________________________________________
	\n
 	\n1)
 	\nThis command will restore your layers and devices to their prior states.
 	\nIt will also delete any MTEXT notes placed on the VANTAGE layer.
  	\nIf everything looks okay, use the \"AUTOB\" command.
	\n
	"
	)


	(cond

	  	((= instructions-text-set T)
		(setq textfile (open (strcat (getvar 'DWGPREFIX) "WS_AUTOCAD_TEXT_VANTAGE3_Instructions.txt")  "a"))
		(close textfile)
		(setq textfile (open (strcat (getvar 'DWGPREFIX) "WS_AUTOCAD_TEXT_VANTAGE3_Instructions.txt")  "w"))
		(write-LWPOLYLINE "This text file contains instructions for the VANTAGE3 command" textfile)
		(write-LWPOLYLINE (strcat string1) textfile)
		(close textfile)
		)

	)

  	;;;before moves call clear function from AUTOV file
  	;;;this command moves any blocks from the AUTOV steps to layers expected by the next lines of code
	(C:CLEAR)													;call CLEAR
  
  	(setq objecttype "LWPOLYLINE")
	(setq layerfrom	"CONTROLLER_BUS")
	(setq layerto	"_WS_CONTROLLER_BUS")
  	(move-objects objecttype layerfrom layerto)
  	(setq objecttype "INSERT")
  	(setq layerto	"WATTSTOPPER")
	(move-objects objecttype layerfrom layerto)
  	
  	
  	(setq objecttype "LWPOLYLINE")
	(setq layerfrom	"ENCLOSURE_BUS")
	(setq layerto	"_WS_ENCLOSURE_BUS")
  	(move-objects objecttype layerfrom layerto)
  	(setq objecttype "INSERT")
  	(setq layerto	"WATTSTOPPER")
	(move-objects objecttype layerfrom layerto)

  	(setq objecttype "LWPOLYLINE")
	(setq layerfrom	"AUXILIARY")
	(setq layerto	"WATTSTOPPER")
  	(move-objects objecttype layerfrom layerto)
  	(setq objecttype "INSERT")
  	(setq layerto	"WATTSTOPPER")
	(move-objects objecttype layerfrom layerto)

  	(setq objecttype "LWPOLYLINE")
	(setq layerfrom	"PANEL_COMPONENTS")
	(setq layerto	"_WS_PANEL_COMPONENTS")
  	(move-objects objecttype layerfrom layerto)
  	(setq objecttype "INSERT")
  	(setq layerto	"WATTSTOPPER")
	(move-objects objecttype layerfrom layerto)

  	(setq objecttype "LWPOLYLINE")
	(setq layerfrom	"STATION_BUS")
	(setq layerto	"_WS_STATION_BUS")
  	(move-objects objecttype layerfrom layerto)
  	(setq objecttype "INSERT")
  	(setq layerto	"WATTSTOPPER")
	(move-objects objecttype layerfrom layerto)

	(setq objecttype "LWPOLYLINE")
	(setq layerfrom	"3_18_AWG")
	(setq layerto	"_WS_3_18_AWG")
  	(move-objects objecttype layerfrom layerto)
  	(setq objecttype "INSERT")
  	(setq layerto	"WATTSTOPPER")
	(move-objects objecttype layerfrom layerto)

	(setq objecttype "LWPOLYLINE")
	(setq layerfrom	"RS232")
	(setq layerto	"_WS_RS232")
  	(move-objects objecttype layerfrom layerto)
  	(setq objecttype "INSERT")
  	(setq layerto	"WATTSTOPPER")
	(move-objects objecttype layerfrom layerto)


  	
  	(setq ss (ssget "x" '((8 . "Defpoints"))))
  	(cond														;-conditional block
		((/= ss nil)												;-conditional statement
		(command "-layer" "on" "0" "")
		(command "erase" "p" "")
		(command "-layer" "off" "0" "")
		(setq layerExists (tblsearch "LAYER" "Defpoints2"))
  		(cond													;--conditional block
			((/= layerExists nil)										;--conditional statement
			(setq ss (ssget "x" '((8 . "Defpoints2"))))
			(command "-layer" "on" "0" "")
			(command "clayer" "Defpoints")
			(command "copytolayer" "p" "" "Defpoints" "d" "0,0")
			(command "erase" "p" "")
			(command "_.purge" "LA" "Defpoints2" "N")
			)												;--end conditional statement
		)													;--end conditional block
		)													;-end conditional statement
	)														;-end conditional block

	(setq ss (ssget "x" '((0 . "MTEXT")(8 . "VANTAGE"))))
  	(cond														;-conditional block
		((/= ss nil)												;-conditional statement
		(command "erase" "p" "")	
		)													;-end conditional statement
	)														;-end conditional block
  
    	(command "clayer" "WATTSTOPPER")										;purge command block, sets WATTSTOPPER current layer and purges BRIDGES layer
	(command "_.purge" "LA" "CONTROLLER_BUS" "N")
	(command "_.purge" "LA" "ENCLOSURE_BUS" "N")
	(command "_.purge" "LA" "AUXILIARY" "N")
	(command "_.purge" "LA" "PANEL_COMPONENTS" "N")
	(command "_.purge" "LA" "STATION_BUS" "N")
	(command "_.purge" "LA" "18_3_AWG" "N")
	(command "_.purge" "LA" "RS232" "N")
	(command "_.purge" "LA" "VANTAGE" "N")
	(setvar 'osmode os)												;restore osnap
  	(setq DefColor (cadr (assoc 62 Defpoints)))
  	(setq DefLT (cadr (assoc 6 Defpoints)))
  	(command "-layer" "make" "Defpoints" "color" DefColor "LType" DefLT "")
  	(layerstate-restore "WS-Layout" nil nil)									;restore Layer state

	(setvar "cmdecho" oldecho)											;set CMDECHO to oldecho
  	(C:WSFRONT)													;call WSFRONT
	(princ)	
)															;end VANTAGE3

(defun *error* ()
	(layerstate-restore "WS-Layout" nil nil)									;restore Layer state

	(setvar "cmdecho" oldecho)											;set CMDECHO to oldecho
  	(C:WSFRONT)													;call WSFRONT
        (princ)
)


; sub function
; --- move-objects ---
; Arguments
; layer-from which is the layer the splines are on
; layer-to which is the layer to move the splines to
;Alex Lundin 12-28-2016
(defun move-objects	(object-type layer-from layer-to / objects object)

  	(setq objects(ssget "x" (list(cons 0 object-type)(cons 8 layer-from))))						;select objects on layer-from
	(cond														;-conditional block
	  	((/= objects nil)											;-conditional statement, exact same structure as above, moves bridges from BRIDGES to WATTSTOPPER layer
	 	(setq cntr 0)
		(setq cmax (sslength objects))
		(while 	(< cntr cmax)											;--while loop
			(setq object(ssname objects cntr))								;set bridge varaible to the member in blocks that corresponds to the current counter cntr variable

			(setq ent object)										;set ent to the entity stored in block
		  	(if	(= object-type "INSERT")								;only get truename if the object is a block
				(setq truename (blocktruename ent))							;call blocktruename function with the ent parameter set in the LWPOLYLINE above, set the truename variable to the final result
			)
			(setq layer layer-to)										;set layer to correct layer
			(setq ent object)										;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
			(entmod-layer ent layer)									;call entmod-layer on ent and layer arugments, no return value needed
  			(setq cntr(+ cntr 1))      
		)													;--end while
		)													;-end conditional statement
	)														;-end conditional block
)


; --- blocktruename Sub Function ---
; send entity name of block in through ent variable from calling function
; the ent argument from the caller gets passed into the ent variable in this function
; the truename is returned to the caller
; Arguments
; entity's name passed into ent variable
; Alex Lundin 12-21-2016
(defun blocktruename (ent / vlaobject truename)
  
 	(setq vlaobject (vlax-ename->vla-object ent))						;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name stored in ent
	(setq truename(vlax-get-property vlaobject						;set the truename variable to the effective name property
	(if (vlax-property-available-p vlaobject 'effectivename)				;only if the property called effective name exisits inside block 
		'effectivename									;this entire operation of setting truename fails when if statement is false
		'name
		)
	)
	) 

)


	
; --- entmod-layer Sub Function ---
; change objects layer
; Arguments
; entity's name passed into ent variable
; layer passed into layer variable
; Alex Lundin 12-21-2016

(defun entmod-layer	(ent layer / )

	(setq entdxf(entget ent))						;set dxf codes of ent to entdxf				
	(setq entdxf(subst (cons 8 layer)(assoc 8 entdxf) entdxf))		;change dxfcodes on entdxf
	(entmod entdxf)								;entmod function to modifiy the entity with the new values
)




