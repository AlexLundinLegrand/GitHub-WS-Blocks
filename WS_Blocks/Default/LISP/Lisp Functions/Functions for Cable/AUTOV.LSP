; --- AUTOVANTAGE Function ---
; Alex Lundin 01-11-2017

;;; subfunction call formats
;;; truename
;;;  	(setq ent [nameofvariable with entity name desired to convert to truename])					;replace bracketed area with the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
;;;	(setq truename (blocktruename ent))										;call blocktruename function with the ent parameter set in the line above, set the truename variable to the final result
;;; entmod-layer
;;; 	(setq layer ["name of layer"])											;set layer to correct layer
;;;  	(setq ent [nameofvariable with entity name desired to convert to truename])					;replace bracketed area with the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
;;;	(entmod-layer ent layer)											;call entmod-layer on ent and layer arugments, no return value needed
(defun C:AUTOV 	(
		/
		)
	(vl-load-com)
  	(setq oldecho (getvar "CMDECHO"))
	(setvar "CMDECHO" 0)															;supress command line statement




  	;first object allows conditional block to choose correct sub function
  	(setq object(car(entsel "\n Select the first block or cable on the run: ")))
  	(setq ent object)															;set en variable to correct value to blocktruename function
	(setq entdxf (entget ent))														;set entdxf to the dxf codes of the en entity
	(setq dxf0 (assoc 0 entdxf))														;set dxf10 to the 10th dxf code of entdxf
  	(setq dxf8 (assoc 8 entdxf))
	(setq objecttype (cdr dxf0))
  	(setq objectlayer (cdr dxf8))
  	;;;get truename with subfunction if object is a block
;;;  	(if																	
;;;	  	(= objecttype "INSERT")
;;;	  	(progn
;;;		(setq truename (blocktruename ent))
;;;		)
;;;	 	)
;;; 	)

	;create VANTAGE layers
  	;first are empty layers for cable drawing, they are purged at the end
;;;	(command "-layer" "make" "CONTROLLER_BUS" "color" "10" "" "LType" "Continuous" "" "")
;;;  	(command "-layer" "make" "ENCLOSURE_BUS" "color" "114" "" "LType" "Continuous" "" "")
;;;  	(command "-layer" "make" "AUXILIARY" "color" "253" "" "LType" "Continuous" "" "")
;;;  	(command "-layer" "make" "PANEL_COMPONENTS" "color" "160" "" "LType" "Continuous" "" "")
;;;  	(command "-layer" "make" "STATION_BUS" "color" "200" "" "LType" "Continuous" "" "")
;;;	(command "-layer" "make" "3_18_AWG" "color" "20" "" "LType" "Continuous" "" "")
;;;	(command "-layer" "make" "RS232" "color" "4" "" "LType" "Continuous" "" "")


	;;;main panels, block first
	(if																	;-if
	  	(and																;--and inside if
		(= objecttype "INSERT")(= objectlayer "CONTROLLER_BUS")	
		)																;--end and
	  	(progn																;--progn wrap for the if statement
		(main-panels-block object)
		)																;--end progn wrap
	)																	;-end if
  
	;;;main panels, pline first
	(if																	;-if
	  	(and																;--and inside if
		(= objecttype "LWPOLYLINE")(= objectlayer "CONTROLLER_BUS")	
		)																;--end and
	  	(progn																;--progn wrap for the if statement
		(main-panels-polyline object)
		)																;--end progn wrap
	)																	;-end if
  
	;;;secondary panels
	(if																	;-if
	  	(and																;--and inside if
		(= objecttype "LWPOLYLINE")(= objectlayer "ENCLOSURE_BUS")	
		)																;--end and
	  	(progn																;--progn wrap for the if statement
		(secondary-panels object)
		)																;--end progn wrap
	)																	;-end if

	;;;panel components, pline first
	(if																	;-if
	  	(and																;--and inside if
		(= objecttype "LWPOLYLINE")(= objectlayer "PANEL_COMPONENTS")	
		)																;--end and
	  	(progn																;--progn wrap for the if statement
		(panel-components object)
		)																;--end progn wrap
	)
  
	;;;station bus
	(if																	;-if
	  	(and																;--and inside if
		(= objecttype "LWPOLYLINE")(= objectlayer "STATION_BUS")	
		)																;--end and
	  	(progn																;--progn wrap for the if statement
		(station-bus object)
		)																;--end progn wrap
	)																	;-end if
  
	;;;RS232
	(if																	;-if
	  	(and																;--and inside if
		(= objecttype "LWPOLYLINE")(= objectlayer "RS232")	
		)																;--end and
	  	(progn																;--progn wrap for the if statement
		(RS232-components object)
		)																;--end progn wrap
	)
  
  
		
	(setq layer "WATTSTOPPER")
	(setvar "clayer" layer)															;sets WATTSTOPPER current layer
	(setvar "CMDECHO" oldecho)
  	(command "._zoom" "p")
  	(princ)
)																		;end function




(defun C:AUTOVNOTES
       	(
	/
	)
  
	(vl-load-com)
  	(setq oldecho (getvar "CMDECHO"))
	(setvar "CMDECHO" 0)
  	(setq mlayer (tblsearch "LAYER" "VANTAGE"))
  
  	(if	(/= mlayer nil)
  		(command "-layer" "make" "VANTAGE" "color" "30" "" "LType" "Continuous" "" "")
	)

  
  	(setq dcl_id (load_dialog "AUTOVNOTES.dcl"))									;load dialog
  	(new_dialog "AUTOVNOTES" dcl_id)
 
;;;  	(if (not (new_dialog "AUTOVNOTES" dcl_id)			;test for dialog
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
	\n New Command
     	"
 )
 (setq string2
	"
     	\n
     	\n_______________________________________________AUTOV_______________________________________________
     	\n
 	\nThis command will assist in filling out attributes on layout blocks for Vantage Risers.
 	\nFirst, use the \"VANTAGE\" command to isolate the product blocks and existing wires.
	\nYou can stay in this layer state for the entire process of drawing wire and filling out attributes.
	\nTo reset blocks to the VANTAGE state after using \"AUTOV\", use the \"CLEAR\" command.
 	\nTo exit use the \"VANTAGE3\" command which will restore layers and drafting settings.
 	\n
 	\nAfter you type \"AUTOV\":
	\n1)
	\nYou can choose any VANTAGE wire or panel and the code will guide you through filling out the attributes.
	\n
	\n2)Main Panels
	\nChoose either a single block or polyline on the controller bus layer (red).
	\nEnter the starting number for the main panels.
	\nThe code will loop through the series of wires and blocks, filling out attributes until the final panel is reached.
	\n
	"
	)
  
(setq string3
       	"
	\n3)Secondary Panels
	\nChoose a single polyline on the enclosure bus layer (green) that connects to the main panel (red).
	\nEnter the starting number for the secondary run.
	\nThe options are 2,3 or 4.
	\nThe code will add and A or B to the end of the number.
	\nThe code will loop through the series of wires and blocks, filling out attributes until the final panel is reached.
	\n
	\n4)Auxiliary Panels
	\nChoose the first auxiliary panel (white).
	\nThe code will loop through the series blocks, filling out attributes until the final panel is reached.
	\n
	\n5)Panel Components
	\nChoose the polyline on the panel components layer (white) that connects to the panel (any kind of panel).
	\nThe code will loop through the series of wires and blocks, filling out attributes until the final component is reached.
	\n
	\n6)Station Bus
	\nMove the fitpoints of any wires necessary so only ONE STAION BUS WIRE connects to the processor.
	\nChoose a single station bus wire (purple) that connects to the processor (white).
	\nThe code will loop through the series of wires and blocks, filling out attributes until the final component is reached.
	\nThe code can reach 3_18_AWG wire (orange) as well.
	\n
	\n6)RS232
	\nChoose a single RS232 bus wire (cyan) that connects to the processor.
	\nThe code will loop through the series of wires and blocks, filling out attributes until the final component is reached.
	\n7)
	\nFinally, use the \"VANTAGE3\" command when you are done with all your networking on this layout.
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
		(setq textfile (open (strcat (getvar 'DWGPREFIX) "WS_AUTOCAD_TEXT_AUTOV_Instructions.txt")  "a"))
		(close textfile)
		(setq textfile (open (strcat (getvar 'DWGPREFIX) "WS_AUTOCAD_TEXT_AUTOV_Instructions.txt")  "w"))
		(write-line "This text file contains instructions for the AUTOV command" textfile)
		(write-line (strcat string1) textfile)
		(write-line (strcat string2) textfile)
		(write-line (strcat string3) textfile)
		(close textfile)
		)

	)
  
	(setq ok-click nil)
	(setq ok-click nil)
	(setq instructions-drawing-set nil)
	(setq instructions-text-set nil)
  
	(setvar "CMDECHO" oldecho)
  	(princ)
)



(defun station-bus
       	(
	object-as-argument
	/
	)


	
  	(setq vantagemain "0")
	(setq vantagesecondary "0")
	(setq vantageauxiliary "0")
  	(setq vantage318position "1")
  	(setq vantagestationbusrun (getstring "Enter station bus run: 1 or 2"))
  	(setq vantagestaionbuspositon (getstring "Enter station bus position:"))
	(command "._zoom" "e")
  
	(setq att1 "VMAIN")
	(setq att2 "VSECONDARY")
	(setq att3 "VAUXILIARY")
  	(setq att4 "VSTATIONBUS")
  	(setq att5 "VSTATIONBUSPOSITION")
  	(setq att6 "V318POSITION")
	
	(setq blockstartlayer "STATION_BUS")
	(setq blockfinallayer "_WS_STATION_BUS")
	(setq objectstartlayer "STATION_BUS")
	(setq objectfinallayer "_WS_STATION_BUS")
	(setq spline object-as-argument)


  	(setq dinstartlayer "PANEL_COMPONENTS")
  	(setq dinfinallayer "_WS_PANEL_COMPONENTS")


  	(setq 318blockstartlayer "3_18_AWG")
  	(setq 318blockfinallayer "_WS_3_18_AWG")
   	(setq 318cablestartlayer "3_18_AWG")
  	(setq 318cablefinallayer "_WS_3_18_AWG")
  
	(if																	;-if
	  	(/= spline nil)									
	  	(progn																;--progn wrap
	  				(setq entDXF(entget spline))
					(setq fitpoints (list))
					(foreach flag entDXF                                              					;look for every flag (10 dxf) in the dxfRectangle list
  						(if	(= 10 (car flag))                                         				;if the 11 dxf exists 
    							(setq fitPoints                                						;reset cornerPointsRectangle to
      							(append fitPoints                       						;the old cornerPointsRectangle 
        						(list                                                       				;plus a list containing
          						(cdr flag)                                             					;the cornerPoint point
        						)                                                                 			;close the list statement 
      							)                                                                      			;close the append statement
    							)                                                                           		;close the setq statement
  						)                                                                                		;close the if statement
					)                                                                                     			;close the foreach statement

  					(setq vertices nil)
  					(setq fPMax (length fitPoints))										;organize fitPoints
		  			(setq fpMaxminus2 (- fPMax 2))
		  
					(setq fplast (last fitPoints))
		  			(setq fpsecondtolast (nth fpMaxminus2 fitPoints))
		  			(setq fplasttwo (list fPlast fpsecondtolast))
		  			(setq fpfirst (car fitPoints))
		  			(setq fpsecond (nth 1 fitPoints))
		  			(setq fpfirsttwo (list fpfirst fpsecond))
					(setq vertices (cdr fitPoints))
					 
					;;;select from fitpoints
		  			(setq ssb (ssget "_F" fitPoints '((0 . "INSERT")(8 . "PANEL_COMPONENTS"))))

		  			(if
					  	(/= ssb nil)
					  	(progn
						(setq bridge(ssname ssb 0))
						(setq ent bridge)
						(setq truename (blocktruename ent))
						)
					)

		  			;;;if selection did not find the din-lite, check firsttwo
		  			(if
					  	(and
					  	(/= truename "V-IC-DIN-II-LITE")(/= truename "V-IC-DIN-II-LITE-RF")
						)
					  	(progn
						(setq ssb (ssget "_F" fpfirsttwo '((0 . "INSERT")(8 . "PANEL_COMPONENTS"))))
						)
					)
		  
		  			;;;if selection did not find the din-lite, check other end, lasttwo
		  			(if
					  	(and
					  	(/= truename "V-IC-DIN-II-LITE")(/= truename "V-IC-DIN-II-LITE-RF")
						)
					  	(progn
						(setq ssb (ssget "_F" fplasttwo '((0 . "INSERT")(8 . "PANEL_COMPONENTS"))))
						)
					)
		  

					;;;area to pull attributes from din-lite for station bus run
					(setq bridge(ssname ssb 0))
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
					  				((= attTag att1)							;first conditional statement--when the attVal contains nothing 
									(setq vantagemain attVal)						
									)  									;end first conditional statement

									((= attTag att2)							;second conditional statement--when the attVal contains nothing 
				   					(setq vantagesecondary attVal)
									)									;end second conditional statement
									
									((= attTag att3)							;second conditional statement--when the attVal contains nothing 
				   					(setq vantageauxiliary attVal)
									)									;end third conditional statement
								)										;end conditional block				  
		  				(setq ent enSub)
				  		(setq enDXF2(entget enSub))
						(setq group0(cdr(assoc 0 enDXF2)))
						) 

			 		(setq layer dinfinallayer)										;set layer to correct layer
			  		(setq ent bridge)											;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
					(entmod-layer ent layer)										;call entmod-layer on ent and layer arugments, no return value needed
		  )																;--end progn
	)																	;-end if
  

	(setq layer objectfinallayer )														;set layer to correct layer
	(setq ent spline)															;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
	(entmod-layer ent layer)														;call entmod-layer on ent and layer arugments, no return value needed


  	
  
	;;; spline is already primed by user as object variable and stored into spline variable as an arugment during the function call
  		(while 	(/= spline nil)														;set loop to continue while spline is not nil, spline is set at end of loop everytime

		(setq 318check 0)
																		;open conditional block
			  															;conditiona statement (when bridge is not nil)
					(setq entDXF(entget spline))
					(setq fitpoints (list))
					(foreach flag entDXF                                              					;look for every flag (10 dxf) in the dxfRectangle list
  						(if	(= 10 (car flag))                                         				;if the 11 dxf exists 
    							(setq fitPoints                                						;reset cornerPointsRectangle to
      							(append fitPoints                       						;the old cornerPointsRectangle 
        						(list                                                       				;plus a list containing
          						(cdr flag)                                             					;the cornerPoint point
        						)                                                                 			;close the list statement 
      							)                                                                      			;close the append statement
    							)                                                                           		;close the setq statement
  						)                                                                                		;close the if statement
					)                                                                                     			;close the foreach statement

  					(setq vertices nil)
  					(setq fPMax (length fitPoints))										;organize fitPoints
		  			(setq fpMaxminus2 (- fPMax 2))
		  
					(setq fplast (last fitPoints))
		  			(setq fpsecondtolast (nth fpMaxminus2 fitPoints))
		  			(setq fplasttwo (list fPlast fpsecondtolast))
		  			(setq fpfirst (car fitPoints))
		  			(setq fpsecond (nth 1 fitPoints))
		  			(setq fpfirsttwo (list fpfirst fpsecond))
					(setq vertices (cdr fitPoints))
					 
					;;;make selection set from all fitpoints first
		  			(setq ssb (ssget "_F" fitPoints '((0 . "INSERT")(8 . "STATION_BUS"))))
					(setq sso (ssget "_F" fitPoints '((0 . "LWPOLYLINE")(8 . "STATION_BUS"))))
					(setq ssonested (ssget "_F" fitPoints '((0 . "LWPOLYLINE")(8 . "3_18_AWG"))))
		  
		  			;;;if nil then try last two fitpoints
		  			(if	(= ssb nil)
					  	(progn
					  	(setq ssb (ssget "_F" fplasttwo '((0 . "INSERT")(8 . "STATION_BUS"))))
						)
		  			)

		  			(if
					  	(= ssb nil)
					  	(progn
						(setq sso (ssget "_F" fplasttwo '((0 . "LWPOLYLINE")(8 . "STATION_BUS"))))
						(setq ssonested (ssget "_F" fplasttwo '((0 . "LWPOLYLINE")(8 . "3_18_AWG"))))
						)
					)
		  
		  			(if
					  	(= ssb nil)
					  	(progn
						(setq ssb (ssget "_F" fpfirsttwo '((0 . "INSERT")(8 . "STATION_BUS"))))
						(setq sso (ssget "_F" fpfirsttwo '((0 . "LWPOLYLINE")(8 . "STATION_BUS"))))
						(setq ssonested (ssget "_F" fpfirsttwo '((0 . "LWPOLYLINE")(8 . "3_18_AWG"))))
						)
					)
		  


					(setq bridge(ssname ssb 0))
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
					  				((= attTag att1)							;first conditional statement--when the attVal contains nothing 
									(entmod (subst (cons 1 vantagemain) (assoc 1 enDXF2) enDXF2))		;change the value stored in the 1st dxf code of enDXF2 to the segment variable
									)  									;end first conditional statement

									((= attTag att2)							;second conditional statement--when the attVal contains nothing 
				   					(entmod (subst (cons 1  vantagesecondary) (assoc 1 enDXF2) enDXF2))	;change the value stored in the 1st dxf code of enDXF2 to the bridgePosition variable
									)									;end second conditional statement
									
									((= attTag att3)							;second conditional statement--when the attVal contains nothing 
				   					(entmod (subst (cons 1  vantageauxiliary) (assoc 1 enDXF2) enDXF2))	;change the value stored in the 1st dxf code of enDXF2 to the bridgePosition variable
									)									;end third conditional statement
									((= attTag att4)							;second conditional statement--when the attVal contains nothing 
				   					(entmod (subst (cons 1  vantagestationbusrun) (assoc 1 enDXF2) enDXF2))	;change the value stored in the 1st dxf code of enDXF2 to the bridgePosition variable
									)
									((= attTag att5)							;second conditional statement--when the attVal contains nothing 
				   					(entmod (subst (cons 1  vantagestaionbuspositon)(assoc 1 enDXF2) enDXF2))	;change the value stored in the 1st dxf code of enDXF2 to the bridgePosition variable
									)	
								)										;end conditional block				  
		  				(setq ent enSub)
				  		(setq enDXF2(entget enSub))
						(setq group0(cdr(assoc 0 enDXF2)))
						) 

			 		(setq layer blockfinallayer)										;set layer to correct layer
			  		(setq ent bridge)											;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
					(entmod-layer ent layer)										;call entmod-layer on ent and layer arugments, no return value needed


					
		  


  					;;; splinenested is primed for conditional
					;;; conditional statement for second selection set for 18_3_AWG cable
					(if															;-if
					  	(/= ssonested nil)										;--conditional statement
						(progn
						(setq splinenested(ssname ssonested 0))
						
								 				(setq layer 318cablefinallayer)								;set layer to correct layer
								  				(setq ent splinenested)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
												(entmod-layer ent layer)

							
	
						;;; splinenested is already primed above
					  		(while 	(/= splinenested nil)								;set loop to continue while splinenested is not nil, splinenested is set at end of loop everytime


					

						;nested area, need to fix
							  	(setq entDXF(entget splinenested))
							  
										(setq fitpoints (list))
										(foreach flag entDXF                                              					;look for every flag (10 dxf) in the dxfRectangle list
					  						(if	(= 10 (car flag))                                         				;if the 11 dxf exists 
					    							(setq fitPoints                                						;reset cornerPointsRectangle to
					      							(append fitPoints                       						;the old cornerPointsRectangle 
					        						(list                                                       				;plus a list containing
					          						(cdr flag)                                             					;the cornerPoint point
					        						)                                                                 			;close the list statement 
					      							)                                                                      			;close the append statement
					    							)                                                                           		;close the setq statement
					  						)                                                                                		;close the if statement
										)                                                                                     			;close the foreach statement

					  					(setq vertices nil)
					  					(setq fPMax (length fitPoints))										;organize fitPoints
							  			(setq fpMaxminus2 (- fPMax 2))
							  
										(setq fplast (last fitPoints))
							  			(setq fpsecondtolast (nth fpMaxminus2 fitPoints))
							  			(setq fplasttwo (list fPlast fpsecondtolast))
							  			(setq fpfirst (car fitPoints))
							  			(setq fpsecond (nth 1 fitPoints))
							  			(setq fpfirsttwo (list fpfirst fpsecond))
										(setq vertices (cdr fitPoints))
										 

							  			(setq ssbnested (ssget "_F" fitPoints '((0 . "INSERT")(8 . "3_18_AWG"))))
							  			(setq ssonested (ssget "_F" fitPoints '((0 . "LWPOLYLINE")(8 . "3_18_AWG"))))

							  			(if
										  	(= ssbnested nil)
										  	(progn
											(setq ssbnested (ssget "_F" fplasttwo '((0 . "INSERT")(8 . "3_18_AWG"))))
											(setq ssonested (ssget "_F" fplasttwo '((0 . "LWPOLYLINE")(8 . "3_18_AWG"))))
											)
										)
							  
							  			(if
										  	(= ssbnested nil)
										  	(progn
											(setq ssbnested (ssget "_F" fpfirsttwo '((0 . "INSERT")(8 . "3_18_AWG"))))
											(setq ssonested (ssget "_F" fpfirsttwo '((0 . "LWPOLYLINE")(8 . "3_18_AWG"))))
											)
										)


							  			
										(setq bridge(ssname ssbnested 0))
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
										  				((= attTag att1)							;first conditional statement--when the attVal contains nothing 
														(entmod (subst (cons 1 vantagemain) (assoc 1 enDXF2) enDXF2))		;change the value stored in the 1st dxf code of enDXF2 to the segment variable
														)  									;end first conditional statement

														((= attTag att2)							;second conditional statement--when the attVal contains nothing 
									   					(entmod (subst (cons 1  vantagesecondary) (assoc 1 enDXF2) enDXF2))	;change the value stored in the 1st dxf code of enDXF2 to the bridgePosition variable
														)									;end second conditional statement
														
														((= attTag att3)							;second conditional statement--when the attVal contains nothing 
									   					(entmod (subst (cons 1  vantageauxiliary) (assoc 1 enDXF2) enDXF2))	;change the value stored in the 1st dxf code of enDXF2 to the bridgePosition variable
														)									;end third conditional statement
														((= attTag att4)							;second conditional statement--when the attVal contains nothing 
									   					(entmod (subst (cons 1  vantagestationbusrun) (assoc 1 enDXF2) enDXF2))	;change the value stored in the 1st dxf code of enDXF2 to the bridgePosition variable
														)
														((= attTag att5)							;second conditional statement--when the attVal contains nothing 
									   					(entmod (subst (cons 1  vantagestaionbuspositon)(assoc 1 enDXF2) enDXF2))	;change the value stored in the 1st dxf code of enDXF2 to the bridgePosition variable
														)
														((= attTag att6)							;second conditional statement--when the attVal contains nothing 
									   					(entmod (subst (cons 1  vantage318position)(assoc 1 enDXF2) enDXF2))	;change the value stored in the 1st dxf code of enDXF2 to the bridgePosition variable
														)									
													)										;end conditional block
							  				(setq ent enSub)
									  		(setq enDXF2(entget enSub))
											(setq group0(cdr(assoc 0 enDXF2)))
											) 

								 		(setq layer 318blockfinallayer)										;set layer to correct layer
								  		(setq ent bridge)											;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
										(entmod-layer ent layer)										;call entmod-layer on ent and layer arugments, no return value needed
							  
							  			(cond
											  	((/= ssonested nil)										;if the splinenested selection set ssonested is not nil
												(setq splinenested(ssname ssonested 0))								;set the splinenested variable to the first item in the set, and there is only one item in the set
								 				(setq layer 318cablefinallayer)								;set layer to correct layer
								  				(setq ent splinenested)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
												(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed
												)
												((= ssonested nil)										;if the splinenested seleciton set is nil, we have reached the end of the run or found a break, so terminate the loop
												(setq splinenested nil)									;set splinenested to nil for while loop pre test, which will fail it
												)
										)
										  
												 
										(setq fitPoints(list)) 											;create an empty list to store cornerPoints in
							  
							(setq vantage318positionInt (atoi vantage318position))										;set bridgePositionInt to the integer value of the conversion of bridgePosition from it's string value
							(setq vantage318positionInt (+ vantage318positionInt 1))										;increment bridgePositionInt to next value
							(setq vantage318position (itoa vantage318positionInt))										;set bridgePosition to the value bridgePositionInt converted to a string
							(setq 318check 1)									;set check variable to 1

						;;;end nested
							  
							
							)											;end while

						 
						)												;--end progn
					)													;-end if

		  




		  


		  			;;;operate this part when check is true or false, doesn't matter
		  			(if
					  	(or
					  	(= 318check 1)(= 318check 0)
						)
					  	(progn
				  			(cond
								  	((/= sso nil)										;if the spline selection set sso is not nil
									(setq spline(ssname sso 0))								;set the spline variable to the first item in the set, and there is only one item in the set
					 				(setq layer objectfinallayer )								;set layer to correct layer
					  				(setq ent spline)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
									(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed
									)
									((= sso nil)										;if the spline seleciton set is nil, we have reached the end of the run or found a break, so terminate the loop
									(setq spline nil)									;set spline to nil for while loop pre test, which will fail it
									)
							)
					  	)
					)
		  
							 
					(setq fitPoints(list)) 											;create an empty list to store cornerPoints in
					
		(setq vantagestaionbuspositonInt (atoi vantagestaionbuspositon))								;set bridgePositionInt to the integer value of the conversion of bridgePosition from it's string value
		(setq vantagestaionbuspositonInt (+ vantagestaionbuspositonInt 1))								;increment bridgePositionInt to next value
		(setq vantagestaionbuspositon (itoa vantagestaionbuspositonInt))								;set bridgePosition to the value bridgePositionInt converted to a string	  
		)																;end while
  
)



(defun RS232-components
       	(
	object-as-argument
	/
	)


	
  	(setq vantagemain "0")
	(setq vantagesecondary "0")
	(setq vantageauxiliary "0")
  	(setq vantagers232position "1")
	(command "._zoom" "e")

	(setq att1 "VMAIN")
	(setq att2 "VSECONDARY")
	(setq att3 "VAUXILIARY")
  	(setq att4 "VRS232POSITION")

	
	(setq blockstartlayer "RS232")
	(setq blockfinallayer "_WS_RS232")
	(setq objectstartlayer "RS232")
	(setq objectfinallayer "_WS_RS232")
	(setq spline object-as-argument)


  	(setq controllerstartlayer "PANEL_COMPONENTS")
  	(setq controllerfinallayer "_WS_PANEL_COMPONENTS")
	(if																	;-if
	  	(/= spline nil)									
	  	(progn																;--progn wrap
	  				(setq entDXF(entget spline))
					(setq fitpoints (list))
					(foreach flag entDXF                                              					;look for every flag (10 dxf) in the dxfRectangle list
  						(if	(= 10 (car flag))                                         				;if the 11 dxf exists 
    							(setq fitPoints                                						;reset cornerPointsRectangle to
      							(append fitPoints                       						;the old cornerPointsRectangle 
        						(list                                                       				;plus a list containing
          						(cdr flag)                                             					;the cornerPoint point
        						)                                                                 			;close the list statement 
      							)                                                                      			;close the append statement
    							)                                                                           		;close the setq statement
  						)                                                                                		;close the if statement
					)                                                                                     			;close the foreach statement

  					(setq vertices nil)
  					(setq fPMax (length fitPoints))										;organize fitPoints
		  			(setq fpMaxminus2 (- fPMax 2))
		  
					(setq fplast (last fitPoints))
		  			(setq fpsecondtolast (nth fpMaxminus2 fitPoints))
		  			(setq fplasttwo (list fPlast fpsecondtolast))
		  			(setq fpfirst (car fitPoints))
		  			(setq fpsecond (nth 1 fitPoints))
		  			(setq fpfirsttwo (list fpfirst fpsecond))
					(setq vertices (cdr fitPoints))
					 
		  
					;;;make selection set from all fitpoints first
		  			(setq ssb (ssget "_F" fitPoints '((0 . "INSERT")(8 . "PANEL_COMPONENTS"))))
		  
		  			;;if nil then try last two fitpoints
		  			(if	(= ssb nil)
					  	(progn
					  	(setq ssb (ssget "_F" fplasttwo '((0 . "INSERT")(8 . "PANEL_COMPONENTS"))))
						)
		  			)
		  
		  			(if
					  	(/= ssb nil)
					  	(progn
						(setq bridge(ssname ssb 0))
						(setq ent bridge)
						(setq truename (blocktruename ent))
						)
					)

		  			;;;if selection did not find the din-lite, check other end, firsttwo
		  			(if
					  	(and
					  	(/= truename "V-IC-DIN-II-LITE")(/= truename "V-IC-DIN-II-LITE-RF")(/= truename "V-IC-36")
						)
					  	(progn
						(setq ssb (ssget "_F" fpfirsttwo '((0 . "INSERT")(8 . "PANEL_COMPONENTS"))))
						)
					)
		  

					;;;area to pull attributes from din-lite for station bus run
					(setq bridge(ssname ssb 0))
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
					  				((= attTag att1)							;first conditional statement--when the attVal contains nothing 
									(setq vantagemain attVal)						
									)  									;end first conditional statement

									((= attTag att2)							;second conditional statement--when the attVal contains nothing 
				   					(setq vantagesecondary attVal)
									)									;end second conditional statement
									
									((= attTag att3)							;second conditional statement--when the attVal contains nothing 
				   					(setq vantageauxiliary attVal)
									)									;end third conditional statement
								)										;end conditional block				  
		  				(setq ent enSub)
				  		(setq enDXF2(entget enSub))
						(setq group0(cdr(assoc 0 enDXF2)))
						) 

			 		(setq layer controllerfinallayer)										;set layer to correct layer
			  		(setq ent bridge)											;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
					(entmod-layer ent layer)										;call entmod-layer on ent and layer arugments, no return value needed
		  )																;--end progn
	)																	;-end if
  

	(setq layer objectfinallayer )														;set layer to correct layer
	(setq ent spline)															;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
	(entmod-layer ent layer)														;call entmod-layer on ent and layer arugments, no return value needed
  
	;;; spline is already primed by user as object variable and stored into spline variable as an arugment during the function call
  		(while 	(/= spline nil)														;set loop to continue while spline is not nil, spline is set at end of loop everytime


																		;open conditional block
			  															;conditiona statement (when bridge is not nil)
					(setq entDXF(entget spline))
					(setq fitpoints (list))
					(foreach flag entDXF                                              					;look for every flag (10 dxf) in the dxfRectangle list
  						(if	(= 10 (car flag))                                         				;if the 11 dxf exists 
    							(setq fitPoints                                						;reset cornerPointsRectangle to
      							(append fitPoints                       						;the old cornerPointsRectangle 
        						(list                                                       				;plus a list containing
          						(cdr flag)                                             					;the cornerPoint point
        						)                                                                 			;close the list statement 
      							)                                                                      			;close the append statement
    							)                                                                           		;close the setq statement
  						)                                                                                		;close the if statement
					)                                                                                     			;close the foreach statement

  					(setq vertices nil)
  					(setq fPMax (length fitPoints))										;organize fitPoints
		  			(setq fpMaxminus2 (- fPMax 2))
		  
					(setq fplast (last fitPoints))
		  			(setq fpsecondtolast (nth fpMaxminus2 fitPoints))
		  			(setq fplasttwo (list fPlast fpsecondtolast))
		  			(setq fpfirst (car fitPoints))
		  			(setq fpsecond (nth 1 fitPoints))
		  			(setq fpfirsttwo (list fpfirst fpsecond))
					(setq vertices (cdr fitPoints))
					 

					;;;make selection set from all fitpoints first
		  			(setq ssb (ssget "_F" fitpoints '((0 . "INSERT")(8 . "RS232"))))
		  			(setq sso (ssget "_F" fitpoints '((0 . "LWPOLYLINE")(8 . "RS232"))))
		  
		  			;;if nil then try last two fitpoints
		  			(if	(= ssb nil)
					  	(progn
					  	(setq ssb (ssget "_F" fplasttwo '((0 . "INSERT")(8 . "RS232"))))
						)
		  			)

		  			(if
					  	(/= ssb nil)
					  	(progn
						(setq sso (ssget "_F" fplasttwo '((0 . "LWPOLYLINE")(8 . "RS232"))))
						)
					)
		  
		  			(if
					  	(= ssb nil)
					  	(progn
						(setq ssb (ssget "_F" fpfirsttwo '((0 . "INSERT")(8 . "RS232"))))
						(setq sso (ssget "_F" fpfirsttwo '((0 . "LWPOLYLINE")(8 . "RS232"))))
						)
					)
		  


					(setq bridge(ssname ssb 0))
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
					  				((= attTag att1)							;first conditional statement--when the attVal contains nothing 
									(entmod (subst (cons 1 vantagemain) (assoc 1 enDXF2) enDXF2))		;change the value stored in the 1st dxf code of enDXF2 to the segment variable
									)  									;end first conditional statement

									((= attTag att2)							;second conditional statement--when the attVal contains nothing 
				   					(entmod (subst (cons 1  vantagesecondary) (assoc 1 enDXF2) enDXF2))	;change the value stored in the 1st dxf code of enDXF2 to the bridgePosition variable
									)									;end second conditional statement
									
									((= attTag att3)							;second conditional statement--when the attVal contains nothing 
				   					(entmod (subst (cons 1  vantageauxiliary) (assoc 1 enDXF2) enDXF2))	;change the value stored in the 1st dxf code of enDXF2 to the bridgePosition variable
									)									;end third conditional statement
									((= attTag att4)							;second conditional statement--when the attVal contains nothing 
				   					(entmod (subst (cons 1  vantagers232position) (assoc 1 enDXF2) enDXF2))	;change the value stored in the 1st dxf code of enDXF2 to the bridgePosition variable
									)

								)										;end conditional block				  
		  				(setq ent enSub)
				  		(setq enDXF2(entget enSub))
						(setq group0(cdr(assoc 0 enDXF2)))
						) 

			 		(setq layer blockfinallayer)										;set layer to correct layer
			  		(setq ent bridge)											;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
					(entmod-layer ent layer)										;call entmod-layer on ent and layer arugments, no return value needed
		  
		  			(cond
						  	((/= sso nil)										;if the spline selection set sso is not nil
							(setq spline(ssname sso 0))								;set the spline variable to the first item in the set, and there is only one item in the set
			 				(setq layer objectfinallayer )								;set layer to correct layer
			  				(setq ent spline)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
							(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed
							)
							((= sso nil)										;if the spline seleciton set is nil, we have reached the end of the run or found a break, so terminate the loop
							(setq spline nil)									;set spline to nil for while loop pre test, which will fail it
							)
					)
					  
							 
					(setq fitPoints(list)) 											;create an empty list to store cornerPoints in

		(setq vantagers232positionInt (atoi vantagers232position))											;set bridgePositionInt to the integer value of the conversion of bridgePosition from it's string value
		(setq vantagers232positionInt (+ vantagers232positionInt 1))											;increment bridgePositionInt to next value
		(setq vantagers232position (itoa vantagers232positionInt))											;set bridgePosition to the value bridgePositionInt converted to a string
		  
		)																;end while
  
)



(defun panel-components
       	(
	object-as-argument
	/
	)

	(setq maincheck 0)
	(setq secondarycheck 0)
	(setq auxiliarycheck 0)
	
  	(setq vantagemain "0")
	(setq vantagesecondary "0")
	(setq vantageauxiliary "0")

	(setq att1 "VMAIN")
	(setq att2 "VSECONDARY")
	(setq att3 "VAUXILIARY")
	(command "._zoom" "e")
	
	(setq blockstartlayer "PANEL_COMPONENTS")
	(setq blockfinallayer "_WS_PANEL_COMPONENTS")
	(setq objectstartlayer "PANEL_COMPONENTS")
	(setq objectfinallayer "_WS_PANEL_COMPONENTS")
	(setq spline object-as-argument)


  	(setq mainstartlayer "CONTROLLER_BUS")
  	(setq mainfinallayer "_WS_CONTROLLER_BUS")
  	(setq secondarystartlayer "ENCLOSURE_BUS")
	(setq secondaryfinallayer "_WS_ENCLOSURE_BUS")
	(setq auxiliarystartlayer "AUXILIARY")
	(setq auxiliaryfinallayer "_WS_AUXILIARY")


  
	(if																	;-if
	  	(/= spline nil)									
	  	(progn																;--progn wrap
	  				(setq entDXF(entget spline))
					(setq fitpoints (list))
					(foreach flag entDXF                                              					;look for every flag (10 dxf) in the dxfRectangle list
  						(if	(= 10 (car flag))                                         				;if the 11 dxf exists 
    							(setq fitpoints                                						;reset cornerPointsRectangle to
      							(append fitpoints                       						;the old cornerPointsRectangle 
        						(list                                                       				;plus a list containing
          						(cdr flag)                                             					;the cornerPoint point
        						)                                                                 			;close the list statement 
      							)                                                                      			;close the append statement
    							)                                                                           		;close the setq statement
  						)                                                                                		;close the if statement
					)                                                                                     			;close the foreach statement

  					(setq vertices nil)
  					(setq fPMax (length fitPoints))										;organize fitPoints
		  			(setq fpMaxminus2 (- fPMax 2))
		  
					(setq fplast (last fitpoints))
		  			(setq fpsecondtolast (nth fpMaxminus2 fitpoints))
		  			(setq fplasttwo (list fPlast fpsecondtolast))
		  			(setq fpfirst (car fitpoints))
		  			(setq fpsecond (nth 1 fitpoints))
		  			(setq fpfirsttwo (list fpfirst fpsecond))
					(setq vertices (cdr fitpoints))


		  			;;;make selection set from all fitpoints first on controller_bus
		  			(setq ssb (ssget "_F" fitpoints '((0 . "INSERT")(8 . "CONTROLLER_BUS"))))

		  			;;if selection set found something, set check variable
		  			(if	(/= ssb nil)
					  	(progn
						(setq maincheck 1)
						(setq secondarycheck 0)
						(setq auxiliarycheck 0)
						)
		  			)
		  
		  			;;if nil then try other panel layers
		  			(if	(= ssb nil)
					  	(progn
					  	(setq ssb (ssget "_F" fitpoints '((0 . "INSERT")(8 . "ENCLOSURE_BUS"))))
						(setq secondarycheck 1)
						(setq maincheck 0)
						(setq auxiliarycheck 0)
						)
		  			)
		  
		  			;;if nil then try other panel layers
		  			(if	(= ssb nil)
					  	(progn
					  	(setq ssb (ssget "_F" fitpoints '((0 . "INSERT")(8 . "AUXILIARY"))))
						(setq auxiliarycheck 1)
						(setq secondarycheck 0)
						(setq maincheck 0)						
						)
		  			)


		  
		  			(if
					  	(/= ssb nil)
					  	(progn
						(setq bridge(ssname ssb 0))
						(setq ent bridge)
						(setq truename (blocktruename ent))
						)
					)


		  

					;;;area to pull attributes from main panel for the components
					(setq bridge(ssname ssb 0))
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
					  				((= attTag att1)							;first conditional statement--when the attVal contains nothing 
									(setq vantagemain attVal)						
									)  									;end first conditional statement

									((= attTag att2)							;second conditional statement--when the attVal contains nothing 
				   					(setq vantagesecondary attVal)
									)									;end second conditional statement
									
									((= attTag att3)							;second conditional statement--when the attVal contains nothing 
				   					(setq vantageauxiliary attVal)
									)									;end third conditional statement
								)										;end conditional block				  
		  				(setq ent enSub)
				  		(setq enDXF2(entget enSub))
						(setq group0(cdr(assoc 0 enDXF2)))
						) 
					(if
					  	(= maincheck 1)
					  	(progn
				 		(setq layer mainfinallayer)									;set layer to correct layer
				  		(setq ent bridge)										;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
						(entmod-layer ent layer)									;call entmod-layer on ent and layer arugments, no return value needed
						)
					)
		  
					(if
					  	(= secondarycheck 1)
					  	(progn
				 		(setq layer secondaryfinallayer)								;set layer to correct layer
				  		(setq ent bridge)										;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
						(entmod-layer ent layer)									;call entmod-layer on ent and layer arugments, no return value needed
						)
					)
		  
					(if
					  	(= auxiliarycheck 1)
					  	(progn
				 		(setq layer auxiliaryfinallayer)								;set layer to correct layer
				  		(setq ent bridge)										;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
						(entmod-layer ent layer)									;call entmod-layer on ent and layer arugments, no return value needed
						)
					)		  

		  
		  )																;--end progn
	)																	;-end if
  

	(setq layer objectfinallayer )														;set layer to correct layer
	(setq ent spline)															;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
	(entmod-layer ent layer)														;call entmod-layer on ent and layer arugments, no return value needed


	;;; spline is already primed by user as object variable and stored into spline variable as an arugment during the function call
  		(while 	(/= spline nil)														;set loop to continue while spline is not nil, spline is set at end of loop everytime


																		;open conditional block
			  															;conditiona statement (when bridge is not nil)
					(setq entDXF(entget spline))
					(setq fitpoints (list))
					(foreach flag entDXF                                              					;look for every flag (10 dxf) in the dxfRectangle list
  						(if	(= 10 (car flag))                                         				;if the 11 dxf exists 
    							(setq fitpoints                                						;reset cornerPointsRectangle to
      							(append fitpoints                       						;the old cornerPointsRectangle 
        						(list                                                       				;plus a list containing
          						(cdr flag)                                             					;the cornerPoint point
        						)                                                                 			;close the list statement 
      							)                                                                      			;close the append statement
    							)                                                                           		;close the setq statement
  						)                                                                                		;close the if statement
					)                                                                                     			;close the foreach statement

  					(setq vertices nil)
  					(setq fPMax (length fitpoints))										;organize fitPoints
		  			(setq fpMaxminus2 (- fPMax 2))
		  
					(setq fplast (last fitpoints))
		  			(setq fpsecondtolast (nth fpMaxminus2 fitpoints))
		  			(setq fplasttwo (list fPlast fpsecondtolast))
		  			(setq fpfirst (car fitpoints))
		  			(setq fpsecond (nth 1 fitpoints))
		  			(setq fpfirsttwo (list fpfirst fpsecond))
					(setq vertices (cdr fitpoints))
					 

		  			;;;make selection set from all fitpoints first
		  			(setq ssb (ssget "_F" fitpoints '((0 . "INSERT")(8 . "PANEL_COMPONENTS"))))
		  			(setq sso (ssget "_F" fitpoints '((0 . "LWPOLYLINE")(8 . "PANEL_COMPONENTS"))))
		  
		  			;;if nil then try last two fitpoints
		  			(if	(= ssb nil)
					  	(progn
					  	(setq ssb (ssget "_F" fplasttwo '((0 . "INSERT")(8 . "PANEL_COMPONENTS"))))
					  	(setq sso (ssget "_F" fplasttwo '((0 . "LWPOLYLINE")(8 . "PANEL_COMPONENTS"))))
						)
		  			)

		  			;;if still nil then try first two
		  			(if
					  	(= ssb nil)
					  	(progn
						(setq ssb (ssget "_F" fpfirsttwo '((0 . "INSERT")(8 . "PANEL_COMPONENTS"))))
						(setq sso (ssget "_F" fpfirsttwo '((0 . "LWPOLYLINE")(8 . "PANEL_COMPONENTS"))))
						)
					)

		  			
					(setq bridge(ssname ssb 0))
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
					  				((= attTag att1)							;first conditional statement--when the attVal contains nothing 
									(entmod (subst (cons 1 vantagemain) (assoc 1 enDXF2) enDXF2))		;change the value stored in the 1st dxf code of enDXF2 to the segment variable
									)  									;end first conditional statement

									((= attTag att2)							;second conditional statement--when the attVal contains nothing 
				   					(entmod (subst (cons 1  vantagesecondary) (assoc 1 enDXF2) enDXF2))	;change the value stored in the 1st dxf code of enDXF2 to the bridgePosition variable
									)									;end second conditional statement
									
									((= attTag att3)							;second conditional statement--when the attVal contains nothing 
				   					(entmod (subst (cons 1  vantageauxiliary) (assoc 1 enDXF2) enDXF2))	;change the value stored in the 1st dxf code of enDXF2 to the bridgePosition variable
									)									;end third conditional statement
								)										;end conditional block
		  				(setq ent enSub)
				  		(setq enDXF2(entget enSub))
						(setq group0(cdr(assoc 0 enDXF2)))
						) 

			 		(setq layer blockfinallayer)										;set layer to correct layer
			  		(setq ent bridge)											;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
					(entmod-layer ent layer)										;call entmod-layer on ent and layer arugments, no return value needed
		  
		  			(cond
						  	((/= sso nil)										;if the spline selection set sso is not nil
							(setq spline(ssname sso 0))								;set the spline variable to the first item in the set, and there is only one item in the set
			 				(setq layer objectfinallayer )								;set layer to correct layer
			  				(setq ent spline)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
							(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed
							)
							((= sso nil)										;if the spline seleciton set is nil, we have reached the end of the run or found a break, so terminate the loop
							(setq spline nil)									;set spline to nil for while loop pre test, which will fail it
							)
					)
					  
							 
		   			
		)																;end while
  
)



(defun main-panels-block
	(
	object-as-argument
	/
	)
	(setq vantagemain (getstring "Enter main number: Positive integers only"))
	(setq vantagesecondary "0")
	(setq vantageauxiliary "0")
	(command "._zoom" "e")
  
	(setq att1 "VMAIN")
	(setq att2 "VSECONDARY")
	(setq att3 "VAUXILIARY")
	
	(setq blockstartlayer "CONTROLLER_BUS")
	(setq blockfinallayer "_WS_CONTROLLER_BUS")
	(setq objectstartlayer "CONTROLLER_BUS")
	(setq objectfinallayer "_WS_CONTROLLER_BUS")
	(setq block object-as-argument)
  
	(setq layer objectfinallayer)								;set layer to correct layer
	(setq ent block)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
	(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed

	(setq entdxf (entget ent))
	(setq dxf10 (assoc 10 entdxf))
	(setq xcoord (nth 1 dxf10))								;set xcoord to the nth 1 of dxf10
	(setq ycoord (nth 2 dxf10))								;set ycoord to the nth 2 of dxf10

  	(setq x1 (- xcoord 30))									;set x1 and x2 to almost the entire width of a panel
  	(setq x2 (+ xcoord 30))
  	(setq y1 (- ycoord 8))									;set y1 and y1 to almost the entire height of a panel
  	(setq y2 (+ ycoord 8))

  	(setq p1 (list x1 y1))									;make points, on the inside of the current panel block for selecting the first pline
  	(setq p2 (list x2 y2))
	(setq panelpoints (list p1 p2))								;make the list of panel points
  
  	(setq sso (ssget "_F" panelpoints '((0 . "LWPOLYLINE")(8 . "CONTROLLER_BUS"))))		;make a selection set object for the polyline connected to the main panel

	(cond
			((/= sso nil)								;if the spline selection set sso is not nil
			(setq spline(ssname sso 0))						;set the spline variable to the first item in the set, and there is only one item in the set
			(setq layer objectfinallayer)						;set layer to correct layer
			(setq ent spline)							;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
			(entmod-layer ent layer)						;call entmod-layer on ent and layer arugments, no return value needed
			)
			((= sso nil)								;if the spline seleciton set is nil, we have reached the end of the run or found a break, so terminate the loop
			(setq spline nil)							;set spline to nil for while loop pre test, which will fail it
			)
	)


					(setq bridge block)
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
					  				((= attTag att1)							;first conditional statement--when the attVal contains nothing 
									(entmod (subst (cons 1 vantagemain) (assoc 1 enDXF2) enDXF2))		;change the value stored in the 1st dxf code of enDXF2 to the segment variable
									)  									;end first conditional statement

									((= attTag att2)							;second conditional statement--when the attVal contains nothing 
				   					(entmod (subst (cons 1  vantagesecondary) (assoc 1 enDXF2) enDXF2))	;change the value stored in the 1st dxf code of enDXF2 to the bridgePosition variable
									)									;end second conditional statement
									
									((= attTag att3)							;second conditional statement--when the attVal contains nothing 
				   					(entmod (subst (cons 1  vantageauxiliary) (assoc 1 enDXF2) enDXF2))	;change the value stored in the 1st dxf code of enDXF2 to the bridgePosition variable
									)									;end third conditional statement
								)										;end conditional block				  
		  				(setq ent enSub)
				  		(setq enDXF2(entget enSub))
						(setq group0(cdr(assoc 0 enDXF2)))
						) 

			 		(setq layer blockfinallayer)										;set layer to correct layer
			  		(setq ent bridge)											;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
					(entmod-layer ent layer)										;call entmod-layer on ent and layer arugments, no return value needed
		  
		(setq vantagemainInt (atoi vantagemain))											;set bridgePositionInt to the integer value of the conversion of bridgePosition from it's string value
		(setq vantagemainInt (+ vantagemainInt 1))											;increment bridgePositionInt to next value
		(setq vantagemain (itoa vantagemainInt))											;set bridgePosition to the value bridgePositionInt converted to a string	  



  
	;;; spline is already primed by user as object variable and stored into spline variable as an arugment during the function call
  		(while 	(/= spline nil)														;set loop to continue while spline is not nil, spline is set at end of loop everytime


																		;open conditional block
			  															;conditiona statement (when bridge is not nil)
					(setq entDXF(entget spline))
					(setq fitpoints (list))
					(foreach flag entDXF                                              					;look for every flag (10 dxf) in the dxfRectangle list
  						(if	(= 10 (car flag))                                         				;if the 11 dxf exists 
    							(setq fitPoints                                						;reset cornerPointsRectangle to
      							(append fitPoints                       						;the old cornerPointsRectangle 
        						(list                                                       				;plus a list containing
          						(cdr flag)                                             					;the cornerPoint point
        						)                                                                 			;close the list statement 
      							)                                                                      			;close the append statement
    							)                                                                           		;close the setq statement
  						)                                                                                		;close the if statement
					)                                                                                     			;close the foreach statement

  					(setq vertices nil)
  					(setq fPMax (length fitPoints))										;organize fitPoints
		  			(setq fpMaxminus2 (- fPMax 2))
		  
					(setq fPlast (last fitPoints))
		  			(setq fpsecondtolast (nth fpMaxminus2 fitPoints))
		  			(setq fplasttwo (list fPlast fpsecondtolast))
		  			(setq fpfirst (car fitPoints))
		  			(setq fpsecond (nth 1 fitPoints))
		  			(setq fpfirsttwo (list fpfirst fpsecond))
					(setq vertices (cdr fitPoints))
					 

		  			;;;make selection set from all fitpoints first
		  			(setq ssb (ssget "_F" fitpoints '((0 . "INSERT")(8 . "CONTROLLER_BUS"))))
		  			(setq sso (ssget "_F" fitpoints '((0 . "LWPOLYLINE")(8 . "CONTROLLER_BUS"))))
		  
		  			;;if nil then try last two fitpoints
		  			(if	(= ssb nil)
					  	(progn
					  	(setq ssb (ssget "_F" fplasttwo '((0 . "INSERT")(8 . "CONTROLLER_BUS"))))
						)
		  			)

		  			(if
					  	(/= ssb nil)
					  	(progn
						(setq sso (ssget "_F" fplasttwo '((0 . "LWPOLYLINE")(8 . "CONTROLLER_BUS"))))
						)
					)
		  
		  			(if
					  	(= ssb nil)
					  	(progn
						(setq ssb (ssget "_F" fpfirsttwo '((0 . "INSERT")(8 . "CONTROLLER_BUS"))))
						(setq sso (ssget "_F" fpfirsttwo '((0 . "LWPOLYLINE")(8 . "CONTROLLER_BUS"))))
						)
					)
		  


					(setq bridge(ssname ssb 0))
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
					  				((= attTag att1)							;first conditional statement--when the attVal contains nothing 
									(entmod (subst (cons 1 vantagemain) (assoc 1 enDXF2) enDXF2))		;change the value stored in the 1st dxf code of enDXF2 to the segment variable
									)  									;end first conditional statement

									((= attTag att2)							;second conditional statement--when the attVal contains nothing 
				   					(entmod (subst (cons 1  vantagesecondary) (assoc 1 enDXF2) enDXF2))	;change the value stored in the 1st dxf code of enDXF2 to the bridgePosition variable
									)									;end second conditional statement
									
									((= attTag att3)							;second conditional statement--when the attVal contains nothing 
				   					(entmod (subst (cons 1  vantageauxiliary) (assoc 1 enDXF2) enDXF2))	;change the value stored in the 1st dxf code of enDXF2 to the bridgePosition variable
									)									;end third conditional statement
								)										;end conditional block				  
		  				(setq ent enSub)
				  		(setq enDXF2(entget enSub))
						(setq group0(cdr(assoc 0 enDXF2)))
						) 

			 		(setq layer blockfinallayer)										;set layer to correct layer
			  		(setq ent bridge)											;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
					(entmod-layer ent layer)										;call entmod-layer on ent and layer arugments, no return value needed
		  
		  			(cond
						  	((/= sso nil)										;if the spline selection set sso is not nil
							(setq spline(ssname sso 0))								;set the spline variable to the first item in the set, and there is only one item in the set
			 				(setq layer objectfinallayer )								;set layer to correct layer
			  				(setq ent spline)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
							(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed
							)
							((= sso nil)										;if the spline seleciton set is nil, we have reached the end of the run or found a break, so terminate the loop
							(setq spline nil)									;set spline to nil for while loop pre test, which will fail it
							)
					)
					  
							 
					(setq fitPoints(list)) 											;create an empty list to store cornerPoints in
					
		(setq vantagemainInt (atoi vantagemain))											;set bridgePositionInt to the integer value of the conversion of bridgePosition from it's string value
		(setq vantagemainInt (+ vantagemainInt 1))											;increment bridgePositionInt to next value
		(setq vantagemain (itoa vantagemainInt))											;set bridgePosition to the value bridgePositionInt converted to a string	  
		)																;end while

)

;;;same as main-panels-block except it expects a pline first
(defun main-panels-polyline
	(
	object-as-argument
	/
	)
	(setq vantagemain (getstring "Enter main number: Positive integers only"))
	(setq vantagesecondary "0")
	(setq vantageauxiliary "0")
	(command "._zoom" "e")
  
	(setq att1 "VMAIN")
	(setq att2 "VSECONDARY")
	(setq att3 "VAUXILIARY")
	
	(setq blockstartlayer "CONTROLLER_BUS")
	(setq blockfinallayer "_WS_CONTROLLER_BUS")
	(setq objectstartlayer "CONTROLLER_BUS")
	(setq objectfinallayer "_WS_CONTROLLER_BUS")
	(setq block object-as-argument)
  
	(setq layer objectfinallayer)								;set layer to correct layer
	(setq ent spline)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
	(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed


	;;; spline is already primed by user as object variable and stored into spline variable as an arugment during the function call
  		(while 	(/= spline nil)														;set loop to continue while spline is not nil, spline is set at end of loop everytime


																		;open conditional block
			  															;conditiona statement (when bridge is not nil)
					(setq entDXF(entget spline))
					(setq fitpoints (list))
					(foreach flag entDXF                                              					;look for every flag (10 dxf) in the dxfRectangle list
  						(if	(= 10 (car flag))                                         				;if the 11 dxf exists 
    							(setq fitPoints                                						;reset cornerPointsRectangle to
      							(append fitPoints                       						;the old cornerPointsRectangle 
        						(list                                                       				;plus a list containing
          						(cdr flag)                                             					;the cornerPoint point
        						)                                                                 			;close the list statement 
      							)                                                                      			;close the append statement
    							)                                                                           		;close the setq statement
  						)                                                                                		;close the if statement
					)                                                                                     			;close the foreach statement

  					(setq vertices nil)
  					(setq fPMax (length fitPoints))										;organize fitPoints
		  			(setq fpMaxminus2 (- fPMax 2))
		  
					(setq fPlast (last fitPoints))
		  			(setq fpsecondtolast (nth fpMaxminus2 fitPoints))
		  			(setq fplasttwo (list fPlast fpsecondtolast))
		  			(setq fpfirst (car fitPoints))
		  			(setq fpsecond (nth 1 fitPoints))
		  			(setq fpfirsttwo (list fpfirst fpsecond))
					(setq vertices (cdr fitPoints))
					 

		  			;;;make selection set from all fitpoints first
		  			(setq ssb (ssget "_F" fitpoints '((0 . "INSERT")(8 . "CONTROLLER_BUS"))))
		  			(setq sso (ssget "_F" fitpoints '((0 . "LWPOLYLINE")(8 . "CONTROLLER_BUS"))))
		  
		  			;;if nil then try last two fitpoints
		  			(if	(= ssb nil)
					  	(progn
					  	(setq ssb (ssget "_F" fplasttwo '((0 . "INSERT")(8 . "CONTROLLER_BUS"))))
						)
		  			)

		  			(if
					  	(/= ssb nil)
					  	(progn
						(setq sso (ssget "_F" fplasttwo '((0 . "LWPOLYLINE")(8 . "CONTROLLER_BUS"))))
						)
					)
		  
		  			(if
					  	(= ssb nil)
					  	(progn
						(setq ssb (ssget "_F" fpfirsttwo '((0 . "INSERT")(8 . "CONTROLLER_BUS"))))
						(setq sso (ssget "_F" fpfirsttwo '((0 . "LWPOLYLINE")(8 . "CONTROLLER_BUS"))))
						)
					)
		  


					(setq bridge(ssname ssb 0))
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
					  				((= attTag att1)							;first conditional statement--when the attVal contains nothing 
									(entmod (subst (cons 1 vantagemain) (assoc 1 enDXF2) enDXF2))		;change the value stored in the 1st dxf code of enDXF2 to the segment variable
									)  									;end first conditional statement

									((= attTag att2)							;second conditional statement--when the attVal contains nothing 
				   					(entmod (subst (cons 1  vantagesecondary) (assoc 1 enDXF2) enDXF2))	;change the value stored in the 1st dxf code of enDXF2 to the bridgePosition variable
									)									;end second conditional statement
									
									((= attTag att3)							;second conditional statement--when the attVal contains nothing 
				   					(entmod (subst (cons 1  vantageauxiliary) (assoc 1 enDXF2) enDXF2))	;change the value stored in the 1st dxf code of enDXF2 to the bridgePosition variable
									)									;end third conditional statement
								)										;end conditional block				  
		  				(setq ent enSub)
				  		(setq enDXF2(entget enSub))
						(setq group0(cdr(assoc 0 enDXF2)))
						) 

			 		(setq layer blockfinallayer)										;set layer to correct layer
			  		(setq ent bridge)											;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
					(entmod-layer ent layer)										;call entmod-layer on ent and layer arugments, no return value needed
		  
		  			(cond
						  	((/= sso nil)										;if the spline selection set sso is not nil
							(setq spline(ssname sso 0))								;set the spline variable to the first item in the set, and there is only one item in the set
			 				(setq layer objectfinallayer )								;set layer to correct layer
			  				(setq ent spline)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
							(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed
							)
							((= sso nil)										;if the spline seleciton set is nil, we have reached the end of the run or found a break, so terminate the loop
							(setq spline nil)									;set spline to nil for while loop pre test, which will fail it
							)
					)
					  
							 
					(setq fitPoints(list)) 											;create an empty list to store cornerPoints in
					
		(setq vantagemainInt (atoi vantagemain))											;set bridgePositionInt to the integer value of the conversion of bridgePosition from it's string value
		(setq vantagemainInt (+ vantagemainInt 1))											;increment bridgePositionInt to next value
		(setq vantagemain (itoa vantagemainInt))											;set bridgePosition to the value bridgePositionInt converted to a string	  
		)																;end while

)

(defun secondary-panels
       	(
	object-as-argument
	/
	)


	
  	(setq vantagemain "0")
	(setq vantagesecondary "0")
	(setq vantageauxiliary "0")
  	(setq vantagesecondarynumber (getstring "Enter secondary run: 2, 3 or 4"))
	(command "._zoom" "e")
  
	(setq att1 "VMAIN")
	(setq att2 "VSECONDARY")
	(setq att3 "VAUXILIARY")

	
	(setq blockstartlayer "ENCLOSURE_BUS")
	(setq blockfinallayer "_WS_ENCLOSURE_BUS")
	(setq objectstartlayer "ENCLOSURE_BUS")
	(setq objectfinallayer "_WS_ENCLOSURE_BUS")
	(setq spline object-as-argument)


  	(setq mainstartlayer "CONTROLLER_BUS")
  	(setq mainfinallayer "_WS_CONTROLLER_BUS")
	(if																	;-if
	  	(/= spline nil)									
	  	(progn																;--progn wrap
	  				(setq entDXF(entget spline))
					(setq fitpoints (list))
					(foreach flag entDXF                                              					;look for every flag (10 dxf) in the dxfRectangle list
  						(if	(= 10 (car flag))                                         				;if the 11 dxf exists 
    							(setq fitPoints                                						;reset cornerPointsRectangle to
      							(append fitPoints                       						;the old cornerPointsRectangle 
        						(list                                                       				;plus a list containing
          						(cdr flag)                                             					;the cornerPoint point
        						)                                                                 			;close the list statement 
      							)                                                                      			;close the append statement
    							)                                                                           		;close the setq statement
  						)                                                                                		;close the if statement
					)                                                                                     			;close the foreach statement

  					(setq vertices nil)
  					(setq fPMax (length fitPoints))										;organize fitPoints
		  			(setq fpMaxminus2 (- fPMax 2))
		  
					(setq fplast (last fitPoints))
		  			(setq fpsecondtolast (nth fpMaxminus2 fitPoints))
		  			(setq fplasttwo (list fPlast fpsecondtolast))
		  			(setq fpfirst (car fitPoints))
		  			(setq fpsecond (nth 1 fitPoints))
		  			(setq fpfirsttwo (list fpfirst fpsecond))
					(setq vertices (cdr fitPoints))
					 
		  			;;;make selection set from all fitpoints first
		  			(setq ssb (ssget "_F" fitPoints '((0 . "INSERT")(8 . "CONTROLLER_BUS"))))
		  
		  			;;if nil then try last two fitpoints
		  			(if	(= ssb nil)
					  	(progn
					  	(setq ssb (ssget "_F" fplasttwo '((0 . "INSERT")(8 . "CONTROLLER_BUS"))))
						)
		  			)

		  			(if
					  	(/= ssb nil)
					  	(progn
						(setq bridge(ssname ssb 0))
						(setq ent bridge)
						(setq truename (blocktruename ent))
						)
					)

		  			;;;if selection did not find any blocks, check other end, firsttwo
		  			(if
					  	(and
					  	(= ssb nil)
						)
					  	(progn
						(setq ssb (ssget "_F" fpfirsttwo '((0 . "INSERT")(8 . "CONTROLLER_BUS"))))
						)
					)
		  

					;;;area to pull attributes from din-lite for station bus run
					(setq bridge(ssname ssb 0))
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
					  				((= attTag att1)							;first conditional statement--when the attVal contains nothing 
									(setq vantagemain attVal)						
									)  									;end first conditional statement

									;;;dont pull secondary attribute from main, function will set it later
;;;									((= attTag att2)							;second conditional statement--when the attVal contains nothing 
;;;				   					(setq vantagesecondary attVal)
;;;									)									;end second conditional statement
									
									((= attTag att3)							;second conditional statement--when the attVal contains nothing 
				   					(setq vantageauxiliary attVal)
									)									;end third conditional statement
								)										;end conditional block				  
		  				(setq ent enSub)
				  		(setq enDXF2(entget enSub))
						(setq group0(cdr(assoc 0 enDXF2)))
						) 

			 		(setq layer mainfinallayer)										;set layer to correct layer
			  		(setq ent bridge)											;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
					(entmod-layer ent layer)										;call entmod-layer on ent and layer arugments, no return value needed
		  )																;--end progn
	)																	;-end if
  

	(setq layer objectfinallayer )														;set layer to correct layer
	(setq ent spline)															;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
	(entmod-layer ent layer)														;call entmod-layer on ent and layer arugments, no return value needed


  	(setq vantagesecondarycntr 0)
  
	;;; spline is already primed by user as object variable and stored into spline variable as an arugment during the function call
  		(while 	(/= spline nil)														;set loop to continue while spline is not nil, spline is set at end of loop everytime


																		;open conditional block
			  															;conditiona statement (when bridge is not nil)
					(setq entDXF(entget spline))
					(setq fitpoints (list))
					(foreach flag entDXF                                              					;look for every flag (10 dxf) in the dxfRectangle list
  						(if	(= 10 (car flag))                                         				;if the 11 dxf exists 
    							(setq fitPoints                                						;reset cornerPointsRectangle to
      							(append fitPoints                       						;the old cornerPointsRectangle 
        						(list                                                       				;plus a list containing
          						(cdr flag)                                             					;the cornerPoint point
        						)                                                                 			;close the list statement 
      							)                                                                      			;close the append statement
    							)                                                                           		;close the setq statement
  						)                                                                                		;close the if statement
					)                                                                                     			;close the foreach statement

  					(setq vertices nil)
  					(setq fPMax (length fitPoints))										;organize fitPoints
		  			(setq fpMaxminus2 (- fPMax 2))
		  
					(setq fplast (last fitPoints))
		  			(setq fpsecondtolast (nth fpMaxminus2 fitPoints))
		  			(setq fplasttwo (list fPlast fpsecondtolast))
		  			(setq fpfirst (car fitPoints))
		  			(setq fpsecond (nth 1 fitPoints))
		  			(setq fpfirsttwo (list fpfirst fpsecond))
					(setq vertices (cdr fitPoints))
					 

		  			;;;make selection set from all fitpoints first
		  			(setq ssb (ssget "_F" fitpoints '((0 . "INSERT")(8 . "ENCLOSURE_BUS"))))
		  			(setq sso (ssget "_F" fitpoints '((0 . "LWPOLYLINE")(8 . "ENCLOSURE_BUS"))))
		  
		  			;;if nil then try last two fitpoints
		  			(if	(= ssb nil)
					  	(progn
					  	(setq ssb (ssget "_F" fplasttwo '((0 . "INSERT")(8 . "ENCLOSURE_BUS"))))
						)
		  			)

		  			(if
					  	(/= ssb nil)
					  	(progn
						(setq sso (ssget "_F" fplasttwo '((0 . "LWPOLYLINE")(8 . "ENCLOSURE_BUS"))))
						)
					)
		  
		  			(if
					  	(= ssb nil)
					  	(progn
						(setq ssb (ssget "_F" fpfirsttwo '((0 . "INSERT")(8 . "ENCLOSURE_BUS"))))
						(setq sso (ssget "_F" fpfirsttwo '((0 . "LWPOLYLINE")(8 . "ENCLOSURE_BUS"))))
						)
					)

		  			;;;set the letter based on panel counter
					(if
					  	(= vantagesecondarycntr 0)
					  	(setq vantagesecondaryletter "A")
					)
		  
		  			(if
					  	(= vantagesecondarycntr 1)
					  	(setq vantagesecondaryletter "B")
					)

		  			;;;create the whole string
		  			(setq vantagesecondary (strcat vantagesecondarynumber vantagesecondaryletter))
		  			
					(setq bridge(ssname ssb 0))
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
					  				((= attTag att1)							;first conditional statement--when the attVal contains nothing 
									(entmod (subst (cons 1 vantagemain) (assoc 1 enDXF2) enDXF2))		;change the value stored in the 1st dxf code of enDXF2 to the segment variable
									)  									;end first conditional statement

									((= attTag att2)							;second conditional statement--when the attVal contains nothing 
				   					(entmod (subst (cons 1  vantagesecondary) (assoc 1 enDXF2) enDXF2))	;change the value stored in the 1st dxf code of enDXF2 to the bridgePosition variable
									)									;end second conditional statement
									
									((= attTag att3)							;second conditional statement--when the attVal contains nothing 
				   					(entmod (subst (cons 1  vantageauxiliary) (assoc 1 enDXF2) enDXF2))	;change the value stored in the 1st dxf code of enDXF2 to the bridgePosition variable
									)									;end third conditional statement
								)										;end conditional block
		  				(setq ent enSub)
				  		(setq enDXF2(entget enSub))
						(setq group0(cdr(assoc 0 enDXF2)))
						) 

			 		(setq layer blockfinallayer)										;set layer to correct layer
			  		(setq ent bridge)											;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
					(entmod-layer ent layer)										;call entmod-layer on ent and layer arugments, no return value needed
		  
		  			(cond
						  	((/= sso nil)										;if the spline selection set sso is not nil
							(setq spline(ssname sso 0))								;set the spline variable to the first item in the set, and there is only one item in the set
			 				(setq layer objectfinallayer )								;set layer to correct layer
			  				(setq ent spline)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
							(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed
							)
							((= sso nil)										;if the spline seleciton set is nil, we have reached the end of the run or found a break, so terminate the loop
							(setq spline nil)									;set spline to nil for while loop pre test, which will fail it
							)
					)
					  
							 
					(setq fitPoints(list)) 											;create an empty list to store cornerPoints in
		  
		(setq vantagesecondarycntr (+ vantagesecondarycntr 1)) 			
		)																;end while
  
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


; --- entmod-layer Sub Function ---
; change objects layer
; Arguments
; entity's name passed into ent variable
; layer passed into layer variable
; Alex Lundin 12-21-2016

	;create VANTAGE layers
  	;first are empty layers for cable drawing, they are purged at the end
;;;	(command "-layer" "make" "CONTROLLER_BUS" "color" "10" "" "LType" "Continuous" "" "")
;;;  	(command "-layer" "make" "ENCLOSURE_BUS" "color" "114" "" "LType" "Continuous" "" "")
;;;  	(command "-layer" "make" "AUXILIARY" "color" "253" "" "LType" "Continuous" "" "")
;;;  	(command "-layer" "make" "PANEL_COMPONENTS" "color" "160" "" "LType" "Continuous" "" "")
;;;  	(command "-layer" "make" "STATION_BUS" "color" "200" "" "LType" "Continuous" "" "")
;;;	(command "-layer" "make" "18_3_AWG" "color" "20" "" "LType" "Continuous" "" "")
;;;	(command "-layer" "make" "RS232" "color" "4" "" "LType" "Continuous" "" "")
(defun c:clear	( / )

  	(setq blocks nil)
  	(setq blocks(ssget "x" '((8 . "_WS_CONTROLLER_BUS"))))								;create selection set from all objects that are inserted LMBC-300 blocks on the WATTSTOPPER layer 
	(setq cntr 0)													;initialize counter to 0 for while loop
  
  	(if														;-if
	  	(/= blocks nil)	
	  	(progn													;--progn wrap
	  		(setq cmax (sslength blocks))									;set cmax to length of blocks selection set
			(while (< cntr cmax)										;set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
				(setq en(ssname blocks cntr))								;set variable en to the name of the block that matches the current counter value

			  	(setq layer "CONTROLLER_BUS")
				(setq ent en)										;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed

			  
			(setq cntr(+ cntr 1))										;once while loop reaches this point, increment counter varaible by one			
	  		)												;end while loop
		)													;--end progn
	)														;-end if

	(setq blocks nil)
  	(setq blocks(ssget "x" '((8 . "_WS_ENCLOSURE_BUS"))))								;create selection set from all objects that are inserted LMBC-300 blocks on the WATTSTOPPER layer 
	(setq cntr 0)													;initialize counter to 0 for while loop
  	(if														;-if
	  	(/= blocks nil)	
	  	(progn													;--progn wrap
	  		(setq cmax (sslength blocks))									;set cmax to length of blocks selection set
			(while (< cntr cmax)										;set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
				(setq en(ssname blocks cntr))								;set variable en to the name of the block that matches the current counter value

			  	(setq layer "ENCLOSURE_BUS")
				(setq ent en)										;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed

			  
			(setq cntr(+ cntr 1))										;once while loop reaches this point, increment counter varaible by one			
	  		)												;end while loop
		)													;--end progn
	)														;-end if

  	(setq blocks nil)
  	(setq blocks(ssget "x" '((8 . "_WS_AUXILIARY"))))								;create selection set from all objects that are inserted LMBC-300 blocks on the WATTSTOPPER layer 
	(setq cntr 0)													;initialize counter to 0 for while loop
  	(if														;-if
	  	(/= blocks nil)	
	  	(progn													;--progn wrap
	  		(setq cmax (sslength blocks))									;set cmax to length of blocks selection set
			(while (< cntr cmax)										;set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
				(setq en(ssname blocks cntr))								;set variable en to the name of the block that matches the current counter value

			  	(setq layer "AUXILIARY")
				(setq ent en)										;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed

			  
			(setq cntr(+ cntr 1))										;once while loop reaches this point, increment counter varaible by one			
	  		)												;end while loop
		)													;--end progn
	)														;-end if

  	(setq blocks nil)
  	(setq blocks(ssget "x" '((8 . "_WS_PANEL_COMPONENTS"))))								;create selection set from all objects that are inserted LMBC-300 blocks on the WATTSTOPPER layer 
	(setq cntr 0)													;initialize counter to 0 for while loop
  	(if														;-if
	  	(/= blocks nil)	
	  	(progn													;--progn wrap
	  		(setq cmax (sslength blocks))									;set cmax to length of blocks selection set
			(while (< cntr cmax)										;set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
				(setq en(ssname blocks cntr))								;set variable en to the name of the block that matches the current counter value

			  	(setq layer "PANEL_COMPONENTS")
				(setq ent en)										;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed

			  
			(setq cntr(+ cntr 1))										;once while loop reaches this point, increment counter varaible by one			
	  		)												;end while loop
		)													;--end progn
	)														;-end if


	(setq blocks nil)
  	(setq blocks(ssget "x" '((8 . "_WS_STATION_BUS"))))								;create selection set from all objects that are inserted LMBC-300 blocks on the WATTSTOPPER layer 
	(setq cntr 0)													;initialize counter to 0 for while loop
  	(if														;-if
	  	(/= blocks nil)	
	  	(progn													;--progn wrap
	  		(setq cmax (sslength blocks))									;set cmax to length of blocks selection set
			(while (< cntr cmax)										;set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
				(setq en(ssname blocks cntr))								;set variable en to the name of the block that matches the current counter value

			  	(setq layer "STATION_BUS")
				(setq ent en)										;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed

			  
			(setq cntr(+ cntr 1))										;once while loop reaches this point, increment counter varaible by one			
	  		)												;end while loop
		)													;--end progn
	)														;-end if

  	(setq blocks nil)
 	(setq blocks(ssget "x" '((8 . "_WS_3_18_AWG"))))								;create selection set from all objects that are inserted LMBC-300 blocks on the WATTSTOPPER layer 
	(setq cntr 0)													;initialize counter to 0 for while loop
  	(if														;-if
	  	(/= blocks nil)	
	  	(progn													;--progn wrap
	  		(setq cmax (sslength blocks))									;set cmax to length of blocks selection set
			(while (< cntr cmax)										;set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
				(setq en(ssname blocks cntr))								;set variable en to the name of the block that matches the current counter value

			  	(setq layer "3_18_AWG")
				(setq ent en)										;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed

			  
			(setq cntr(+ cntr 1))										;once while loop reaches this point, increment counter varaible by one			
	  		)												;end while loop
		)													;--end progn
	)														;-end if


  	(setq blocks nil)
 	(setq blocks(ssget "x" '((8 . "_WS_RS232"))))								;create selection set from all objects that are inserted LMBC-300 blocks on the WATTSTOPPER layer 
	(setq cntr 0)													;initialize counter to 0 for while loop
  	(if														;-if
	  	(/= blocks nil)	
	  	(progn													;--progn wrap
	  		(setq cmax (sslength blocks))									;set cmax to length of blocks selection set
			(while (< cntr cmax)										;set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
				(setq en(ssname blocks cntr))								;set variable en to the name of the block that matches the current counter value

			  	(setq layer "RS232")
				(setq ent en)										;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(entmod-layer ent layer)								;call entmod-layer on ent and layer arugments, no return value needed

			  
			(setq cntr(+ cntr 1))										;once while loop reaches this point, increment counter varaible by one			
	  		)												;end while loop
		)													;--end progn
	)														;-end if
)