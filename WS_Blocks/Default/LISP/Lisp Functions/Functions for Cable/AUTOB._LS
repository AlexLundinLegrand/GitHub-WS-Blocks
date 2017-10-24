; --- AUTOBRIDGES Function ---
; Fill out bridges based on _WS_MSTP splines
; Alex Lundin 04-10-2017

;;; subfunction call formats
;;; truename
;;;  	(setq ent [nameofvariable with entity name desired to convert to truename])					;replace bracketed area with the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
;;;	(setq truename (blocktruename ent))										;call blocktruename function with the ent parameter set in the line above, set the truename variable to the final result
;;; entmod-layer
;;; 	(setq layer ["name of layer"])											;set layer to correct layer
;;;  	(setq ent [nameofvariable with entity name desired to convert to truename])					;replace bracketed area with the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
;;;	(entmod-layer ent layer)											;call entmod-layer on ent and layer arugments, no return value needed
(defun C:AUTOB (/ 120OHM 120OHMS ATTTAG ATTVAL BLAYER BLOCK BLOCK2 BLOCKS BLOCKS2 BRIDGE BRIDGEPOSITION BRIDGEPOSITIONINT CLAYER CNTR CNTR3 EN ENDXF ENDXF2 ENSUB ENTDXF EXPERT FITPOINTS FPLAST FPMAX GROUP0 MLAYER SEGMENT SPLINE SPLINE2 SPLINECNTR SPLINELIST SPLINELIST2 SPLINES SPLINES2 SPLINESMAX SSB SSS TRUENAME VERTICES VLAOBJECT)
	(vl-load-com)
  	(setq oldecho (getvar "CMDECHO"))
	(setvar "CMDECHO" 0)															;supress command line statement



  	(setq blayer (tblsearch "LAYER" "BRIDGES"))
  	;(if	(/= blayer nil)
		(command "_.Layer" "_Make" "BRIDGES" "_Color" "30" "" "LType" "Continuous" "" "")
	;)
  	(setq mlayer (tblsearch "LAYER" "MSTP"))
  	;(if	(/= mlayer nil)
  		(command "_.Layer" "_Make" "MSTP" "_Color" "30" "" "LType" "Continuous" "" "")
	;)



  	(setq dcl_id (load_dialog "AUTOB.dcl"))									;load dialog
	(new_dialog "AUTOB" dcl_id)
  
;;;  	(if (not (new_dialog "AUTOB" dcl_id)			;test for dialog
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
     	\n___________________________________________Change Notes___________________________________________
     	\n
     	\nThese prompts will help you navigate the changes.
     	\nUpdated 04-10-2017
     	\nAUTOB can now handle fitpoints or control vertices on splines.
     	"
 )
 (setq string2
	"
     	\n
     	\n_______________________________________________AUTOB_______________________________________________
     	\n
 	\nThis command will assist in numbering the segment number and positions of your network bridges.
 	\nFirst, use the \"BRIDGES\" series of commands to wire the network devices.
 	\nTo exit \"AUTOB\" use the \"AUTOB2\" command which will restore layers and drafting settings.
 	\n
 	\nAfter you press OK:
	\n1)
	\nAutoCAD will ask you for a segment number and starting position.
	\nTypically segments will be numbered 1 through 10 and then 1A through 10A.
	\n2)
	\nNext, AutoCAD will ask you for a starting position number.
	\nThis is the position number for the first network bridge in the segment you are working on.
	\nTypically this number will be 1, but if you are working on a segment coming from another page,
	\nyou will enter the number that the first bridge is in the segment it is connected to.
	\nFor example, if the first bridge on the segment on this page is the 7th bridge on its segment,
	\nenter \"7\" for starting position.
	\n3)
	\nNow all MSTP splines and network bridges will be orange.
	\nVerify the first spline of the segment only connects to one network bridge.
	\nSelect the first spline of the segment you are working on.
	\n4)
	\nNow AutoCAD will attempt to cylce through each spline on the segment and fill out the attributes of the network bridge.
	\nAll sucessful splines will turn blue and the network bridge will dissappear to the WATTSTOPPER layer.
	\nAny orange MSTP wires indicate splines to redraw.
	\nMost likely this is due to the spline not connecting to each other at the corners of each bridge.
	\nFind any orange splines and check that connection.
	\n5)
	\nWhen you are done with a segment, use the \"AUTO2\" command again and repeat steps 1 through 4 above.
	\n6)
	\nFinally, use the \"AUTOB2\" command when you are done with all your networking on this layout.
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
		 
;;;		 	(setq insertionpoint (list xcoord ycoord))
;;;			(entmake
;;;			(list
;;;			(cons 0 "MTEXT")         	;; Entity Name
;;;			(cons 100 "AcDbEntity")  	;; Subclass Marker
;;;			(cons 410 "Model")       	;; Space
;;;			(cons 8 "MSTP")         	;; Layer
;;;			(cons 100 "AcDbMText")   	;; Subclass Marker
;;;			(cons 10 insertionpoint) 	;; Insertion Point
;;;			(cons 40 5)            		;; Text Height
;;;			(cons 71 1)              	;; Attachment Point (middle-center)
;;;			(cons 1 string3)     		;; Text Content
;;;			(cons 7 "Arial")		;text style
;;;			)
;;;			)
		)

	  	((= instructions-text-set T)
		(setq textfile (open (strcat (getvar 'DWGPREFIX) "WS_AUTOCAD_TEXT_AUTOB_Instructions.txt")  "a"))
		(close textfile)
		(setq textfile (open (strcat (getvar 'DWGPREFIX) "WS_AUTOCAD_TEXT_AUTOB_Instructions.txt")  "w"))
		(write-line "This text file contains instructions for the AUTOB command" textfile)
		(write-line (strcat string1) textfile)
		(write-line (strcat string2) textfile)
		(close textfile)
		)

	)


	(setq segment (getstring "Enter segement number: Either 1-10 or 1A-10A"))
	(setq bridgePosition (getstring "Enter a positive number for the first position on the segment"))


  	(layerstate-delete "WS-Layout")
  	(layerstate-save "WS-Layout" nil nil)													;create Layer state
  
	(setq blocks(ssget "x" '((0 . "INSERT")(8 . "WATTSTOPPER"))))										;select all blocks on WATTSTOPPER layer, store to blocks varaible
	(setq cntr 0)
	(cond 	((/= blocks nil)														;conditional loop - first conditional predicate in parenthesis
	 
		(while 	(< cntr (sslength blocks))												;while loop inside - first conditional predicate, loop continues will counter cntr is less than length of blocks selection set
			(setq block(ssname blocks cntr))											;set bridge varaible to the member in blocks that corresponds to the current counter cntr variable

			(setq ent block)													;set ent to the entity stored in block
			(setq truename (blocktruename ent))											;call blocktruename function with the ent parameter set in the line above, set the truename variable to the final result

			(if															;-inner if
			  	(OR														;-- or inside the if
				(= truename "LMBC-300")(= truename "LMSM-3E")(= truename "LMSM-6E")(= truename "LMCP8")
				(= truename "LMCP12")(= truename "LMCP24")(= truename "LMCP48")(= truename "LMNC")(= truename "NB-ROUTER")
				)														;-- end or
			  	(progn														;--progn wrap the if
			 	(setq layer "BRIDGES")												;set layer to correct layer
			  	(setq ent block)												;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(entmod-layer ent layer)											;call entmod-layer on ent and layer arugments, no return value needed
				)														;--end progn wrap
			)															;-end inner if

			(setq cntr(+ cntr 1))  													;increment counter cntr    
		)																;close while
	 

		 
		(setq splines(ssget "x" '((0 . "SPLINE")(8 . "_WS_MSTP"))))									;select all splines on _WS_MSTP
		(setq cntr3 0)															;set cntr 3 to 0		
	 
  			(cond
			  	((/= splines nil)												;-nested conditional loop - first conditional predicate in parenthesis
					(while 	(< cntr3 (sslength splines))									;while loop in nested conditional same structure as above					 
						(setq splinelist(ssname splines cntr3))								;set splinelist to the name of the spline that corresponds to counter variable cntr3
						(setq spline(entget splinelist))					
			 			(setq layer "MSTP")										;set layer to correct layer
			  			(setq ent splinelist)										;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
						(entmod-layer ent layer)									;call entmod-layer on ent and layer arugments, no return value needed
						(setq cntr3(+ cntr3 1))
					)	  
				)														;-close nested conditional statement (first condition)


				;if splines is nil, try selectring from the MSTP layer
			  	((= splines nil)												;-nested conditional loop - first conditional predicate in parenthesis
				(setq splines(ssget "x" '((0 . "SPLINE")(8 . "MSTP"))))								;select all splines on MSTP
				 
				 	(if
					  	(/= splines nil)
					  	(progn
						(while 	(< cntr3 (sslength splines))									;while loop in nested conditional same structure as above					 
							(setq splinelist(ssname splines cntr3))								;set splinelist to the name of the spline that corresponds to counter variable cntr3
							(setq spline(entget splinelist))					
				 			(setq layer "MSTP")										;set layer to correct layer
				  			(setq ent splinelist)										;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
							(entmod-layer ent layer)									;call entmod-layer on ent and layer arugments, no return value needed
							(setq cntr3(+ cntr3 1))
						)
						)
					)
				)

				
  			)															;-close nested conditional block


		(setq 120ohms(ssget "x" '((0 . "INSERT")(8 . "_WS_120_OHM"))))									;select all blocks on _WS_120_OHM
		(setq cntr3 0)															;set cntr 3 to 0		
	 
  			(cond
			  	((/= 120ohms nil)												;-nested conditional loop (first conditional predicate in parenthesis)
					(while 	(< cntr3 (sslength 120ohms))									;while loop in nested conditional (same structure as above)					 
						(setq 120ohm(ssname 120ohms cntr3))					
			 			(setq layer "BRIDGES")										;set layer to correct layer
			  			(setq ent 120ohm)										;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
						(entmod-layer ent layer)									;call entmod-layer on ent and layer arugments, no return value needed
						(setq cntr3(+ cntr3 1))
					)	  
				)														;-close nested conditional statement-first condition	
  			)															;-close nested conditional block

		 
		) 																;END WHILE
	
	)																	;close outer conditional block



  
      	(setq clayer (getvar "clayer") expert (getvar "expert"))										;get current layer
  	(setvar "expert" 1)															;set system variable
  	(command "._layer" "_off" "*" "")													;turn off all layers, turn on clayer varialbe
	(command "._layer" "_on" "_WS_MSTP" "")													;turn off all layers
	(command "._layer" "_on" "MSTP" "")

	(command "._layer" "_on" "BRIDGES" "")
	
  
  	;first spline serves as priming read for spline loop, which is in exactly one while loop from this location
  	(setq spline(car(entsel "\n Select the first cable spline on the segment: ")))
	(setq layer "_WS_MSTP")															;set layer to correct layer
	(setq ent spline)															;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
	(entmod-layer ent layer)														;call entmod-layer on ent and layer arugments, no return value needed
	(command "._zoom" "e")


  	(setq blocks(ssget "x" '((0 . "INSERT")(8 . "BRIDGES"))))										;select all blocks on BRIDGES layer, store to blocks varaible
	(setq cntr 0)
	(cond 	((/= blocks nil)														;conditional loop -first conditional predicate in parenthesis
	 
		(while 	(< cntr (sslength blocks))												;while loop inside first conditional predicate), loop continues will counter cntr is less than length of blocks selection set
			(setq block(ssname blocks cntr))											;set bridge varaible to the member in blocks that corresponds to the current counter cntr variable

			(setq ent block)														;set ent to the entity stored in block
			(setq truename (blocktruename ent))											;call blocktruename function with the ent parameter set in the line above, set the truename variable to the final result

			(if															;-inner if
			  	(OR														;-- or inside the if
				(= truename "LMSM-3E")(= truename "LMSM-6E")(= truename "LMNC")(= truename "NB-ROUTER")
				)														;-- end or
			  	(progn														;--progn wrap the if
			 	(setq layer "WATTSTOPPER")											;set layer to correct layer
			  	(setq ent block)												;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(entmod-layer ent layer)											;call entmod-layer on ent and layer arugments, no return value needed
				)														;--end progn wrap
			)															;-end inner if

			(if															;-inner if
			  	(OR														;-- or inside the if
				(= truename "120OHM")
				)														;-- end or
			  	(progn														;--progn wrap the if
			 	(setq layer "_WS_120_OHM")											;set layer to correct layer
			  	(setq ent block)												;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(entmod-layer ent layer)											;call entmod-layer on ent and layer arugments, no return value needed
				)														;--end progn wrap
			)															;-end inner if


			(setq cntr(+ cntr 1))  													;increment counter cntr    
		)																;close while
		) 																;END WHILE
	
	)																	;close outer conditional block
  	
  	(setq splinesMax (sslength splines))													;set max value, not used for loop though
	(setq cntr 0)
  	(setq splinecntr 0)
  
	;;; spline is already primed by user and stored into spline variable
  		(while 	(/= spline nil)														;set loop to continue while spline is not nil, spline is set at end of loop everytime


																		;open conditional block
			  															;conditiona statement (when bridge is not nil)
					(setq entDXF(entget spline))
					(setq fitpoints (list))
					(foreach flag entDXF                                              					;look for every flag in the entDXF list
  						(if	(= 11 (car flag))                                         				;if the 11 dxf exists 
    							(setq fitPoints                                						;reset cornerPointsRectangle to
      							(append fitPoints                       						;the old cornerPointsRectangle 
        						(list                                                       				;plus a list containing
          						(cdr flag)                                             					;the cornerPoint point
        						)                                                                 			;close the list statement 
      							)                                                                      			;close the append statement
    							)                                                                           		;close the setq statement
  						)                                                                                		;close the if statement
					)                                                                                     			;close the foreach statement

		  			;;;if the spline was drawn in control vectices mode, then there will be no 11th dxf
		  			;;;this results in fitpoints list nil
		  			;;;so use the 10th dxf code to build the fitpoints list
					(if
					  	(= fitpoints nil)
					  	(progn
						(foreach flag entDXF                                              				;look for every flag (10 dxf) in the dxfRectangle list
	  						(if	(= 10 (car flag))                                         			;if the 11 dxf exists 
	    							(setq fitPoints                                					;reset cornerPointsRectangle to
	      							(append fitPoints                       					;the old cornerPointsRectangle 
	        						(list                                                       			;plus a list containing
	          						(cdr flag)                                             				;the cornerPoint point
	        						)                                                                 		;close the list statement 
	      							)                                                                      		;close the append statement
	    							)                                                                           	;close the setq statement
	  						)                                                                                	;close the if statement
						)
						)
					)
		  
  					(setq vertices nil)
  					(setq fPMax (length fitPoints))										;organize fitPoints
		  			(setq fpMaxminus2 (- fPMax 2))
		  
					(setq fPlast (last fitPoints ))
		  			(setq fpsecondtolast (nth fpMaxminus2 fitPoints))
		  			(setq fplasttwo (list fPlast fpsecondtolast))
		  			(setq fpfirst (car fitPoints))
		  			(setq fpsecond (nth 1 fitPoints))
		  			(setq fpfirsttwo (list fpfirst fpsecond))
					(setq vertices (cdr fitPoints))
					 

  					
					(setq ssB (ssget "_F" fplasttwo '((0 . "INSERT")(8 . "BRIDGES"))))
		  
;;;		  			(setq ssB (ssget "_F" fPlast '((0 . "INSERT")(8 . "BRIDGES"))))

		  			(if
					  	(/= ssB nil)
					  	(progn
						(setq ssS (ssget "_F" fplasttwo '((8 . "MSTP"))))
						)
					)
		  
		  			(if
					  	(= ssB nil)
					  	(progn
						(setq ssB (ssget "_F" fpfirsttwo '((0 . "INSERT")(8 . "BRIDGES"))))
						(setq ssS (ssget "_F" fpfirsttwo '((8 . "MSTP"))))
						)
					)
		  


					(setq bridge(ssname ssB 0))
			 		(setq layer "WATTSTOPPER")										;set layer to correct layer
			  		(setq ent bridge)											;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
					(entmod-layer ent layer)										;call entmod-layer on ent and layer arugments, no return value needed
		  
					(setq enDXF (entget bridge))
		  			(setq enSub (entnext bridge))
					(setq enDXF2 (entget enSub))
		  			(setq group0(cdr(assoc 0 enDXF2)))
					(setq ent bridge)
		  				(while	(= group0 "ATTRIB")									;start the conditional predicate 1 loop when group66 varaible = 1
							(setq enSub(entnext ent))								;set en2 to the next sub entity of ent
							(setq enDXF2(entget enSub))								;set enlist2 to the list of entities of en2
							(setq attTag(cdr(assoc 2 enDXF2)))							;set attTag varaible to the second, plus all remaining values of the second member of enlist2
							(setq attVal(cdr(assoc 1 enDXF2)))							;set attVal varaible to the second, plus all remaining values of the first memeber of enlist2
				 
								(cond										;conditional block
					  				((= attTag "SEGMENT")							;first conditional statement--when the attVal contains nothing 
									(entmod (subst (cons 1 segment) (assoc 1 enDXF2) enDXF2))		;change the value stored in the 1st dxf code of enDXF2 to the segment variable
									)  									;end first conditional statement

									((= attTag "POSITION")							;second conditional statement--when the attVal contains nothing 
				   					(entmod (subst (cons 1  bridgePosition) (assoc 1 enDXF2) enDXF2))	;change the value stored in the 1st dxf code of enDXF2 to the bridgePosition variable
									)									;end second conditional statement
									)									;end conditional block
				  
									(setq ent enSub)
				  					(setq enDXF2(entget enSub))
									(setq group0(cdr(assoc 0 enDXF2)))
								) 

		  
		  			(cond
						  	((/= ssS nil)										;if the spline selection set sss is not nil
							(setq spline(ssname ssS 0))								;set the spline variable to the first item in the set, and there is only one item in the set
			 				(setq layer "_WS_MSTP")									;set layer to correct layer
			  				(setq ent spline)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
							(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed
							)
							((= ssS nil)										;if the spline seleciton set is nil, we have reached the end of the run or found a break, so terminate the loop
							(setq spline nil)									;set spline to nil for while loop pre test, which will fail it
							)
					)
					  
							 
					(setq fitPoints(list)) 											;create an empty list to store cornerPoints in
					
		(setq bridgePositionInt (atoi bridgePosition))											;set bridgePositionInt to the integer value of the conversion of bridgePosition from it's string value
		(setq bridgePositionInt (+ bridgePositionInt 1))										;increment bridgePositionInt to next value
		(setq bridgePosition (itoa bridgePositionInt))											;set bridgePosition to the value bridgePositionInt converted to a string
		(setq splinecntr (+ splinecntr 1))	  
		)																;end while
  	
	;nil dialog box variables, because they are global
	(setq ok-click nil)
	(setq ok-click nil)
	(setq instructions-drawing-set nil)
	(setq instructions-text-set nil)


  
	(setq layer "WATTSTOPPER")
	(setvar "clayer" layer)															;sets WATTSTOPPER current layer
	(setvar "CMDECHO" oldecho)
  	(command "._zoom" "p")
  	(princ)
)																		;end function




; --- AUTOBRIDGES2 Function ---
; Moves blocks and restore layers
; Alex Lundin 12-21-2016
(defun C:AUTOB2 (/ BLOCK BLOCKS CMAX CNTR SPLINE SPLINELIST SPLINES TRUENAME VLAOBJECT)
  	(setq oldecho (getvar "CMDECHO"))
	(setvar "CMDECHO" 0)





  	(setq dcl_id (load_dialog "AUTOB2.dcl"))									;load dialog
	(new_dialog "AUTOB2" dcl_id)

  
;;;  	(if (not (new_dialog "AUTOB2" dcl_id)			;test for dialog
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

;;;	;;;radio button commented out in dcl file and here as well
;;;  	;;;left incase we need it later
;;;  	(action_tile
;;;	"rb1"							;if instructions in drawing checked
;;;	"(setq instructions-text-set T))"			;close dialog, set flag
;;;	);action tile
  
  
	(start_dialog)						;start dialog
 
  	(unload_dialog dcl_id)					;unload

  	(setq string1
	"
	\n
	\n_______________________________________________AUTOB2_____________________________________________
	\n
 	\nThis command will restore your layers and devices to their prior states.
 	\nIt will also delete any MTEXT notes placed on the MSTP layer.
	\n
	"
	)


	(cond

	  	((= instructions-text-set T)
		(setq textfile (open (strcat (getvar 'DWGPREFIX) "WS_AUTOCAD_TEXT_AUTOB2_Instructions.txt")  "a"))
		(close textfile)
		(setq textfile (open (strcat (getvar 'DWGPREFIX) "WS_AUTOCAD_TEXT_AUTOB2_Instructions.txt")  "w"))
		(write-line "This text file contains instructions for the AUTOB2 command" textfile)
		(write-line (strcat string1) textfile)
		(close textfile)
		)

	)








  
  	(setq blocks(ssget "x" '((0 . "INSERT")(8 . "BRIDGES"))))										;select blocks on "BRIDGES" layer for next section
	(cond																	;-conditional block
	  	((/= blocks nil)														;-conditional statement, exact same structure as above, moves bridges from BRIDGES to WATTSTOPPER layer
	 	(setq cntr 0)
		(setq cmax (sslength blocks))
		(while 	(< cntr cmax)														;--while loop
			(setq block(ssname blocks cntr))											;set bridge varaible to the member in blocks that corresponds to the current counter cntr variable

 			(setq ent block)													;set ent to the entity stored in block
			(setq truename (blocktruename ent))											;call blocktruename function with the ent parameter set in the line above, set the truename variable to the final result

			(if															;-inner if
			  	(OR														;-- or inside the if
				(/= truename "120OHM")
				)														;-- end or
			  	(progn														;--progn wrap the if
			 	(setq layer "WATTSTOPPER")											;set layer to correct layer
			  	(setq ent block)												;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(entmod-layer ent layer)											;call entmod-layer on ent and layer arugments, no return value needed
				)														;--end progn wrap
			)															;-end inner if

			(if															;-inner if
			  	(OR														;-- or inside the if
				(= truename "120OHM")
				)														;-- end or
			  	(progn														;--progn wrap the if
			 	(setq layer "_WS_120_OHM")											;set layer to correct layer
			  	(setq ent block)												;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(entmod-layer ent layer)											;call entmod-layer on ent and layer arugments, no return value needed
				)														;--end progn wrap
			)															;-end inner if
			  
  			(setq cntr(+ cntr 1))      
		)																;--end while
		)																;-end conditional statement
	)																	;-end conditional block


	(setq splines(ssget "x" '((0 . "SPLINE")(8 . "MSTP"))))
  	(cond																	;-conditional statement
	  	((/= splines  nil)														;-conditional block
		(setq cntr 0)
		(setq cmax (sslength splines))
	 	(while	(< cntr cmax)														;--while loop
			(setq splinelist(ssname splines cntr))
			(setq spline(entget splinelist))
			(setq layer "_WS_MSTP")													;set layer to correct layer
			(setq ent splinelist)													;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
			(entmod-layer ent layer)												;call entmod-layer on ent and layer arugments, no return value needed 
			(setq cntr(+ cntr 1))      
				
  		)																;--end while
		

		)																;-end conditional statement
	)																	;-end conditional block


  
	(setq ss (ssget "x" '((0 . "MTEXT")(8 . "MSTP"))))
  	(cond																	;-conditional block
		((/= ss nil)															;-conditional statement
		(command "erase" "p" "")	
		)																;-end conditional statement
	)																	;-end conditional block

	;nil dialog box variables, because they are global
	(setq ok-click nil)
	(setq ok-click nil)
	(setq instructions-drawing-set nil)
	(setq instructions-text-set nil)

  
	(setq layer "WATTSTOPPER")
	(setvar "clayer" layer)															;sets WATTSTOPPER current layer
	(command "_.purge" "LA" "BRIDGES" "N")													;purge command block,purges BRIDGES layer
	(command "_.purge" "LA" "MSTP" "N")
	(layerstate-restore "WS-Layout" nil nil)												;restore layer state
  	(setvar "CMDECHO" oldecho)
  	(princ)
)																		;END AUTOB2




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