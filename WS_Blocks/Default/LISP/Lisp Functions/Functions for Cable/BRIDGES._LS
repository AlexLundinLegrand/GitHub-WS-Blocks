; --- BRIDGES Function ---
; This sequence is designed to allow the user to run MSTP between LMBC-300's.
; The custom dialog boxes give helpful information along with options for outputing them.
; Alex Lundin 07-14-2017
; Added functionality to restore layer state if it exists


;;; subfunction call formats
;;; truename
;;;  	(setq ent [nameofvariable with entity name desired to convert to truename])					;replace bracketed area with the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
;;;	(setq truename (blocktruename ent))										;call blocktruename function with the ent parameter set in the line above, set the truename variable to the final result
;;; entmod-layer
;;; 	(setq layer ["name of layer"])											;set layer to correct layer
;;;  	(setq ent [nameofvariable with entity name desired to convert to truename])					;replace bracketed area with the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
;;;	(entmod-layer ent layer)											;call entmod-layer on ent and layer arugments, no return value needed

(defun C:BRIDGES ( / 	120OHM 120OHMS BLOCK BLOCKS CLAYER CMAX CMAX2 CNTR CNTR2
		  	ELIST ENT ENTDXF ENTLAYER EXPERT HIDELAYER
		  	LAYER LAYERLIST LISTADDER OLDECHO SELECTFLAG SHOWLAYER
		  	SPLINE SPLINELIST SPLINES SS TRUENAME UNIQUELAYERLIST

			INSTRUCTIONS-DRAWING-SET INSTRUCTIONS-TEXT-SET XCOORD YCOORD
		  DCL_ID INSERTIONPOINT INSTRUCTIONS-DRAWING-SET INSTRUCTIONS-TEXT-SET STRING1 STRING2 STRING3 TEXTFILE USERCLICK
		 )													;GLOBAL VARIABLES -- OS, DEFPOINTSLAYER
  															;START BRIDGES
  
	(vl-load-com)
	(setq oldecho (getvar "CMDECHO"))										;set oldecho varaible to hold Autocad CMDECHO
	(setvar "CMDECHO" 0)												;set CMDECHO to 0, supress commandline echo
  
	(setq os (getvar 'osmode))											;GLOBAL for BRIDGES3 -- set old osnaps to os
  
	(setvar 'osmode 5)												;set osmode to 743
	;;;osmode integer is calculated from which osnaps you prefer
	;;;add the values for each item you would like
	;;;the result is the value of your osmode
	;;;options below"
	;;;0 NONe
	;;;1 ENDpoint
	;;;2 MIDpoint
	;;;4 CENter
	;;;8 NODe
	;;;16 QUAdrant
	;;;32 INTersection
	;;;64 INSertion
	;;;128 PERpendicular
	;;;256 TANgent
	;;;512 NEArest
	;;;1024 QUIck
	;;;2048 APParent Intersection
	;;;4096 EXTension
	;;;8192 PARallel

  	(setq DefpointsLayer (tblsearch "LAYER" "Defpoints"))								;GLOBAL for BRIDGES3 -- get layer definition
	(command "-layer" "make" "BRIDGES" "color" "30" "" "LType" "Continuous" "" "")					;create bridges layer
	(command "-layer" "make" "MSTP" "color" "30" "" "LType" "Continuous" "" "")					;create mstp layer



  
  	(setq dcl_id (load_dialog "BRIDGES.dcl"))									;load dialog
 
;;;  	(if (not (new_dialog "BRIDGES" dcl_id)			;test for dialog
;;; 
;;;      		);not
;;; 
;;;    		(exit)						;exit if no dialog
;;; 
;;;  		);if
  
 	(new_dialog "BRIDGES" dcl_id)			;test for dialog
 

  
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
     	\n___________________________________________Change Notes___________________________________________
     	\n
     	\nPlease read these prompts, they will help you navigate the changes.
     	\nUpdated 12-22-2016
     	\nWe are creating a more streamlined process and there are minor changes to mention.
     	\nIn the past we used corner point osnaps for MSTP wire to connect bridges.
     	\nIn an effort to keep manual cable and automated cable consistent, BRIDGES uses center osnaps now
     	\n
	\n____________________________________________BRIDGES_____________________________________________
     	\nFollow this series through to \"BRIDGES3\".
     	\nThe codes duplicate product blocks onto the Defpoints layer. They are deleted during \"BRIDGES3\".
	\nIf you skip the \"BRIDGES3\" step, it will affect the total number of blocks counted in a excel data extraction.
	\nThis series of commands is called BRIDGES and they will isolate certain networking products to assist in drawing splines.
	\nDuring the series, the codes will handle all snap settings and layer settings for you.
	\nThere are buttons that can place the instructions into the current drawing or to a text file if you prefer.
	\nFeel free to use the MSTPWIRE function with the first BRIDGES command, it will draw cable for you.
	\nThe MSTPWIRE function is still in development, so you may have to use a combination of manual and automated wiring.
     	"
 )

 (setq string2
	"
     	\n
     	\n___________________________________________Manual Version___________________________________________
     	\n
     	\n1)
     	\nSelect any objects (such as walls) that you want to see while drawing MSTP.
     	\nIf you are using a PDF, select the PDF (you will not be able to isolate the walls).
     	\nThen press enter.
     	\nThe layers from the objects you selected as well as any segment managers and network bridges will be the only objects visible.
     	\n
     	\n2)
     	\nStart each MSTP wire run at the segment manager block.
     	\nSeperate the starting points for each segment, so it's clear they connect to different points in the cabinet.
     	\nDraw the first wire going into the bridge, the first wire can only touch one bridge for \"AUTOB\" to work correctly.
     	\n
     	\n3)
     	\nUse the \"spline\" command to draw your MSTP wire between network devices.
     	\nUse several fitpoints across the gap between devices. This makes editing the splines faster later
     	\nConnect the final fitpoint of the spline to the center of a network bridge (use F3 to turn osnap on\/off).
     	\nPress enter to end the spline.
     	\nPress space to start a new spline, continue this new spline from the same center point of the previous network bridge.
     	\n
     	\n4)
     	\nRepeat steps 2 and 3 until all your segment managers and network bridges are wired.
     	\nFollow networking guidelines and best practices. After you are done, use the \"BRIDGES2\" command.
	"
	)
  
	(setq string3
       	"
      	\n
     	\n__________________________________________Automated Version__________________________________________
     	\n
     	\n1)
     	\nSelect any objects (such as walls) that you want to see while drawing MSTP.
     	\nIf you are using a PDF, select the PDF (you will not be able to isolate the walls).
     	\nThen press enter.
     	\nThe layers from the objects you selected as well as any segment managers and network bridges will be the only objects visible.
     	\n
     	\n2)
     	\nDraw the first wire going into the bridge, the first wire can only touch one bridge for \"AUTOB\" to work correctly.
     	\nUse MSTPWIRE function.
     	\nSelect a medium size group of network devices that are near each other to wire together.
     	\nSelect the first item on the run.
     	\n
     	\n3)
     	\nRepeat step 2 until all your segment managers and network bridges are wired.
     	\nFollow networking guidelines and best practices. After you are done, use the \"BRIDGES2\" command.
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
			(cons 8 "MSTP")         	;; Layer
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
			(cons 8 "MSTP")         	;; Layer
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
			(cons 8 "MSTP")         	;; Layer
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
		(setq textfile (open (strcat (getvar 'DWGPREFIX) "WS_AUTOCAD_TEXT_BRIDGES_Instructions.txt")  "a"))
		(close textfile)
		(setq textfile (open (strcat (getvar 'DWGPREFIX) "WS_AUTOCAD_TEXT_BRIDGES_Instructions.txt")  "w"))
		(write-line "This text file contains instructions for the BRIDGES command" textfile)
		(write-line (strcat string1) textfile)
		(write-line (strcat string2) textfile)
		(write-line (strcat string3) textfile)
		(close textfile)
		)

	)



  

  
  	(setq elist (ssget  	(prompt "\nPress esc now if you'd like to change your layer settings.
 					\nSelect objects on walls layer and other layers you would like shown while drawing MSTP:")))	;set elist to the selection and prompt user
  
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

  	(if
	  	(/= (layerstate-has "WS-Layout") nil)
	  	;if layerstate exists, restore
	  	(progn
	  	(layerstate-restore "WS-Layout")
		)
	  	;else create it
	  	(progn
  		(layerstate-save "WS-Layout" nil nil)
  		)
	)
  
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
  	



	(setq blocks(ssget "x" '((0 . "INSERT")(8 . "WATTSTOPPER"))))							;select all blocks on WATTSTOPPER layer, store to blocks varaible

	(cond
	  	((/= blocks nil)											;-conditional statement
	 	(setq cntr 0)												;set counter variable cntr to 0 before each loop
 		(setq cmax (sslength blocks))
		(while 	(< cntr cmax)											;--while loop, loop continues will counter cntr is less than length of blocks selection set
			(setq block(ssname blocks cntr))								;set bridge varaible to the member in blocks that corresponds to the current counter cntr variable

			(setq ent block)										;set ent to the entity stored in block
			(setq truename (blocktruename ent))								;call blocktruename function with the ent parameter set in the line above, set the truename variable to the final result

			(if												;-inner if
			  	(OR											;-- or inside the if
				(= truename "LMBC-300")(= truename "LMSM-3E")(= truename "LMSM-6E")(= truename "LMCP8")
				(= truename "LMCP12")(= truename "LMCP24")(= truename "LMCP48")(= truename "LMNC")
				(= truename "NB-ROUTER")
				)											;-- end or
			  	(progn											;--progn wrap the if
			 	(setq layer "BRIDGES")									;set layer to correct layer
			  	(setq ent block)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed
				)											;--end progn wrap
			)												;-end inner if

			(setq cntr(+ cntr 1))  										;increment counter cntr    
		)													;--end while
		)													;-end conditional statement
	)														;-endconditional block
	 

		 
	(setq splines(ssget "x" '((0 . "SPLINE")(8 . "_WS_MSTP"))))							;select all splines on _WS_MSTP
  	(cond														;-conditional block
		((/= splines nil)											;-conditional statement
		(setq cntr 0)												;set cntr 3 to 0		
		(setq cmax (sslength splines))
		(while 	(< cntr cmax )											;--while loop 					 
			(setq splinelist(ssname splines cntr))								;set splinelist to the name of the spline that corresponds to counter variable cntr3
			(setq spline(entget splinelist))
		  
		  	(setq layer "MSTP")										;set layer to correct layer
			(setq ent splinelist)										;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
			(entmod-layer ent layer)									;call entmod-layer on ent and layer arugments, no return value needed

			(setq cntr(+ cntr 1))								
		)													;--end while	  
  		)													;-end conditional statement
	)														;-end conditional block

	(setq 120ohms(ssget "x" '((0 . "INSERT")(8 . "_WS_120_OHM"))))							;select all blocks on _WS_120_OHM
  	(cond														;-conditional block
		((/= 120ohms nil)											;-conditional statement
		(setq cntr 0)												;set cntr 3 to 0		
	 	(setq cmax (sslength 120ohms))
		(while 	(< cntr cmax)											;--while loop in nested conditional
			(setq 120ohm(ssname 120ohms cntr))					

		  	(setq layer "BRIDGES")										;set layer to correct layer
			(setq ent 120ohm)										;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
			(entmod-layer ent layer)									;call entmod-layer on ent and layer arugments, no return value needed
		  
			(setq cntr(+ cntr 1))
		)													;--end while
  		)													;-end conditional statement	 
	) 														;-end conditional 
	
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

  
	;nil dialog box variables, because they are global
	(setq ok-click nil)
	(setq ok-click nil)
	(setq instructions-drawing-set nil)
	(setq instructions-text-set nil)

  	(command "-layer" "off" "Defpoints2" "")									;toggle layers
  	(command "-layer" "on" "Defpoints" "")	
	(command "-layer" "on" "BRIDGES" "")
	(command "-layer" "on" "MSTP" "")
  	(command "clayer" "MSTP")
  	(setvar "cmdecho" oldecho)
  	(princ)

)															;END BRIDGES

; --- BRIDGES2 Function ---
(defun C:BRIDGES2 	(
		 	/
		   	BLOCKS OLDECHO
		   	BLOCK CMAX CNTR DCL_ID ENT INSERTIONPOINT INSTRUCTIONS-DRAWING-SET INSTRUCTIONS-TEXT-SET LAYER OK-CLICK STRING1 TEXTFILE TRUENAME XCOORD YCOORD
		   	)
  															;GLOBAL VARIABLES - NONE
  															;START BRIDGES2

  	(setq oldecho (getvar "CMDECHO"))										;set oldecho varaible to hold Autocad CMDECHO
	(setvar "CMDECHO" 0)												;set CMDECHO to 0, supress commandline echo

  	(setq dcl_id (load_dialog "BRIDGES2.dcl"))									;load dialog
 	(new_dialog "BRIDGES2" dcl_id)
  
;;;  	(if (not (new_dialog "BRIDGES2" dcl_id)			;test for dialog
;;; 
;;;      		);not
;;; 
;;;    		(exit)						;exit if no dialog
;;; 
;;;  		);if

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
	\n______________________________________________BRIDGES2______________________________________________
	\n
	\nVerify the MSTP wire command is correct. After you press OK:
	\n                                                                                                                                                           
	\n1)	
	\nAll devices including networking will now be visible.
	\n
	\n2)	
	\nVerify that your spline is not crossing over any devices such as:
	\nParts   - room controllers, sensors, switches, etc...
	\nObjects - walls, or another spline.
	\n   	
	\n3)
	\nMove any MSTP wire that you need to.
	\nSelect the spline with a left click, and then use a right click to get sub options for the spline fit points.
	\nFrom here you can stretch\/add\/remove fit points.
	\n
	\n3)	
	\nAfter you are done, use the \"BRIDGES3\" command.
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
			(cons 8 "MSTP")         	;; Layer
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
		(setq textfile (open (strcat (getvar 'DWGPREFIX) "WS_AUTOCAD_TEXT_BRIDGES2_Instructions.txt")  "a"))
		(close textfile)
		(setq textfile (open (strcat (getvar 'DWGPREFIX) "WS_AUTOCAD_TEXT_BRIDGES2_Instructions.txt")  "w"))
		(write-line "This text file contains instructions for the BRIDGES2 command" textfile)
		(write-line (strcat string1) textfile)
		(close textfile)
		)

	)

	;nil dialog box variables, because they are global
	(setq ok-click nil)
	(setq ok-click nil)
	(setq instructions-drawing-set nil)
	(setq instructions-text-set nil)
  
    	(command "-layer" "on" "WATTSTOPPER" "")
	(C:WSFRONT)													;call WSFRONT
  	(setvar "cmdecho" oldecho)
  	(princ)
)															;END BRIDGES2





; --- BRIDGES3 Function ---
(defun C:BRIDGES3 ( / 	BLOCK BLOCKS CMAX CNTR DEFCOLOR DEFLT DEFPOINTS LAYEREXISTS OLDECHO
		   	SPLINE SPLINELIST SPLINES SS TRUENAME VLAOBJECT
		   DCL_ID ENT INSERTIONPOINT INSTRUCTIONS-TEXT-SET LAYER OK-CLICK STRING1 TEXTFILE XCOORD YCOORD
		  )
  															;GLOBAL VARIABLES - NONE
  															;OS passed in from BRIDGES
															;START BRIDGES3
  	(setq oldecho (getvar "CMDECHO"))										;set oldecho varaible to hold Autocad CMDECHO
	(setvar "CMDECHO" 0)												;set CMDECHO to 0, supress commandline echo
	





  	(setq dcl_id (load_dialog "BRIDGES3.dcl"))									;load dialog
 	(new_dialog "BRIDGES3" dcl_id)
  
;;;  	(if (not (new_dialog "BRIDGES3" dcl_id)			;test for dialog
;;; 
;;;      		);not
;;; 
;;;    		(exit)						;exit if no dialog
;;; 
;;;  		);if

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
	\n______________________________________________BRIDGES3______________________________________________
	\n
	
 	\n1)
 	\nThis command will restore your layers and devices to their prior states.
 	\nIt will also delete any MTEXT notes placed on the MSTP layer.
 	\nIf everything looks okay, use the \"AUTOB\" command.
	\n
	"
	)


	(cond

	  	((= instructions-text-set T)
		(setq textfile (open (strcat (getvar 'DWGPREFIX) "WS_AUTOCAD_TEXT_BRIDGES3_Instructions.txt")  "a"))
		(close textfile)
		(setq textfile (open (strcat (getvar 'DWGPREFIX) "WS_AUTOCAD_TEXT_BRIDGES3_Instructions.txt")  "w"))
		(write-line "This text file contains instructions for the BRIDGES3 command" textfile)
		(write-line (strcat string1) textfile)
		(close textfile)
		)

	)






  
  	(setq blocks(ssget "x" '((0 . "INSERT")(8 . "BRIDGES"))))							;select blocks on "BRIDGES" layer for next section
	(cond														;-conditional block
	  	((/= blocks nil)											;-conditional statement, exact same structure as above, moves bridges from BRIDGES to WATTSTOPPER layer
	 	(setq cntr 0)
		(setq cmax (sslength blocks))
		(while 	(< cntr cmax)											;--while loop
			(setq block(ssname blocks cntr))								;set bridge varaible to the member in blocks that corresponds to the current counter cntr variable

			(setq ent block)										;set ent to the entity stored in block
			(setq truename (blocktruename ent))								;call blocktruename function with the ent parameter set in the line above, set the truename variable to the final result



			(if												;-inner if
			  	(OR											;-- or inside the if
				(= truename "120ohm")
				)											;-- end or
			  	(progn											;--progn wrap the if
			 	(setq layer "_WS_120_OHM")								;set layer to correct layer
			  	(setq ent block)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed
				)											;--end progn wrap
			)												;-end inner if

			(if												;-inner if
			  	(OR											;-- or inside the if
				(/= truename "120ohm")
				)											;-- end or
			  	(progn											;--progn wrap the if
			 	(setq layer "WATTSTOPPER")								;set layer to correct layer
			  	(setq ent block)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed
				)											;--end progn wrap
			)												;-end inner if
		  
  			(setq cntr(+ cntr 1))      
		)													;--end while
		)													;-end conditional statement
	)														;-end conditional block

	(setq splines(ssget "x" '((0 . "SPLINE")(8 . "MSTP"))))
  	(cond														;-conditional statement
	  	((/= splines  nil)											;-conditional block
		(setq cntr 0)
		(setq cmax (sslength splines))
	 	(while	(< cntr cmax)											;--while loop
			(setq splinelist(ssname splines cntr))
			(setq spline(entget splinelist))
		  	(setq layer "_WS_MSTP")										;set layer to correct layer
			(setq ent splinelist)										;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
			(entmod-layer ent layer)									;call entmod-layer on ent and layer arugments, no return value needed
			(setq cntr(+ cntr 1))      
				
  		)													;--end while
		
		(command "clayer" "WATTSTOPPER")									;purge command block, sets WATTSTOPPER current layer and purges BRIDGES layer
		(command "_.purge" "LA" "BRIDGES" "N")
		(command "_.purge" "LA" "MSTP" "N")
		)													;-end conditional statement
	)														;-end conditional block
  
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

	(setq ss (ssget "x" '((0 . "MTEXT")(8 . "MSTP"))))
  	(cond														;-conditional block
		((/= ss nil)												;-conditional statement
		(command "erase" "p" "")	
		)													;-end conditional statement
	)														;-end conditional block
  
    	(command "clayer" "WATTSTOPPER")										;purge command block, sets WATTSTOPPER current layer and purges BRIDGES layer
	(command "_.purge" "LA" "BRIDGES" "N")
	(command "_.purge" "LA" "MSTP" "N")
	(prompt "\nPurge bridges layer complete.")
	(setvar 'osmode os)												;restore osnap
  	(setq DefColor (cadr (assoc 62 Defpoints)))
  	(setq DefLT (cadr (assoc 6 Defpoints)))
  	(command "-layer" "make" "Defpoints" "color" DefColor "LType" DefLT "")
  	(if
	  	(/= (layerstate-has "WS-Layout") nil)
	  	;if layerstate exists, restore
	  	(progn
	  	(layerstate-restore "WS-Layout")
		)
	)


	;nil dialog box variables, because they are global
	(setq ok-click nil)
	(setq ok-click nil)
	(setq instructions-drawing-set nil)
	(setq instructions-text-set nil)
  
	(setvar "cmdecho" oldecho)											;set CMDECHO to oldecho
  	(C:WSFRONT)													;call WSFRONT
	(princ)	
)															;end BRIDGES3








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