;;; --- vantage-seperate-polylines-by-layer Sub Function ---
;;; Arguments
;;; none
;;; Alex Lundin 06-28-2017
(defun vantage-seperate-polylines-by-layer 	(
				      		polylines
				      		/
						318POLYLINES AUXILIARYPOLYLINES CNTR CONTROLLERBUSPOLYLINES DXF-1 DXF10 DXF5 DXF8 EN ENCLOSUREBUSPOLYLINES ENTDXF HANDLE LOCATION
						ORANGEFORCONNECTIONPOLYLINES PANELCOMPONENTPOLYLINES POEPOLYLINES RETURNLIST RS232POLYLINES STATIONBUSPOLYLINES XCOORD YCOORD
				       		)


	(setq cntr 0)

  	;;; this loop seperates all polylines into two selection sets

  		(if													;-conditional block
		  	(/= polylines nil)										;-when polylines are not nil
		  	(progn
			(while 	(< cntr (length polylines))								;set loop to continue while the counter varaible cntr is less that the number of objects in the selection set polylines

			(setq entdxf(nth cntr polylines))									;set variable en to the entity name of the block that matches the current counter value
			(setq dxf-1(assoc -1 entdxf))
			(setq en (cdr dxf-1))  

			(setq dxf5 (assoc 5 entdxf))									;set dxf5 to the 5th dxf code of entdxf
		  	(setq handle (cdr dxf5))									;set handle to the second element of dxf5
		  	(setq dxf10 (assoc 10 entdxf))									;set dxf10 to the 10th dxf code of entdxf
		  	(setq xcoord (nth 1 dxf10))									;set xcoord to the nth 1 of dxf10
		  	(setq ycoord (nth 2 dxf10))									;set ycoord to the nth 2 of dxf10
			(setq location (list xcoord ycoord))
  	
			(setq dxf8 (assoc 8 entdxf))
			(setq layer (cdr dxf8)) 
			(cond
				((= layer "_WS_V_3_18_AWG")

				(setq 318Polylines (cons entDXF 318Polylines))
								
				)
				((= layer "_WS_V_AUXILIARY")
				(setq auxiliaryPolylines (cons entDXF auxiliaryPolylines))
					
				)
				((= layer "_WS_V_CONTROLLER_BUS")
				(setq controllerBusPolylines (cons entDXF controllerBusPolylines))
				
				)
				((= layer "_WS_V_ENCLOSURE_BUS")
				(setq enclosureBusPolylines (cons entDXF enclosureBusPolylines))
			
				)
				((= layer "_WS_V_ORANGE_FOR_CONNECTIONS")
				(setq orangeForConnectionPolylines (cons entDXF orangeForConnectionPolylines))
				)
				((= layer "_WS_V_PANEL_COMPONENTS")
				(setq panelComponentPolylines (cons entDXF panelComponentPolylines))
			
				)
				((= layer "_WS_V_POE")
				
				(setq poePolylines (cons entDXF poePolylines))					
				)
				((= layer "_WS_V_RS232")
				(setq RS232Polylines (cons entDXF RS232Polylines))
				)
				((= layer "_WS_V_STATION_BUS")
				(setq stationBusPolylines (cons entDXF stationBusPolylines))
				)				
			)
	
			  
			(setq cntr(+ cntr 1))										;once while loop reaches this point, increment counter varaible by one			
	  		)												;end while loop
			)												;-end when polylines not nil conditional
		)
	
	(setq returnlist (list 318Polylines auxiliaryPolylines  controllerBusPolylines enclosureBusPolylines orangeForConnectionPolylines panelComponentPolylines poePolylines RS232Polylines stationBusPolylines))
)




;;;arguments
;;;adoc 	- vla active document
;;;blocks	- list of entities that are all blocks
;;;tags		- list of Attribute Tags to reset to default
(defun vantage-reset-block-attributes-to-default
	(adoc blocks Tags
	/
	ATTS ATTTAG-FROMBLOCK ATTTAG-FROMTABLE ATTTAGOBJ-FROMBLOCK ATTTAGOBJ-FROMTABLE ATTVALUE-FROMTABLE BLOCK CMAX CMAX2 CNTR CNTR2 E-TYPE EN ENAME-BLOCK ITEM LST SUBLIST TAG TRUENAME VALUE VLAOBJECT-BLOCK VLAOBJECT-BLOCK2 Y Z
	)
  
	(Setq lst '())                                                                             	; empty list
	(setq cntr 0)
  	(setq cmax (length blocks))
  	(while
	  	(< cntr cmax)
		(setq block (nth cntr blocks))
		(setq ename-block (cdr (assoc -1 block)))
		(setq e-type (cdr (assoc 0 block)))

	  	;;; block verification before continueing
		(if
			(= e-type "INSERT")
			(progn
			
			(setq trueName ( VANTAGE-BLOCKTRUENAME ename-block))

		  
			(setq vlaobject-block (vla-item (vla-get-blocks adoc) trueName))
		  
			(vlax-for AttEnt vlaobject-block                                                   
				(if (=  (vla-get-objectname AttEnt) "AcDbAttributeDefinition")         	; go through the entities pull out attributes
				(progn
				(setq attTagObj-FromTable AttEnt)
				(setq attTag-FromTable (vla-get-tagstring attTagObj-FromTable))
				(setq en ename-block)


				(setq cntr2 0)
				(setq cmax2 (length Tags))
				(while 	(< cntr2 cmax2)
					(setq Tag (nth cntr2 Tags))

					(if
						(= Tag attTag-FromTable)
						(progn
						(setq attValue-FromTable (vla-get-textstring AttEnt ))
						(setq sublist (cons  attTagObj-FromTable attValue-FromTable))
						(Setq lst (cons sublist LST))       	; make a list of the defaults
						(setq cntr2 cmax2)
						)
					)
					(setq cntr2 (+ cntr2 1))
				)

				


				) ; progn
				) ; if
			) ; for
		  
			(setq cntr (+ cntr 1))

			(if
				(/= lst nil)
				(progn
				(setq lst (reverse lst)) ; need reversed

				(setq vlaobject-block2 (vlax-ename->vla-object ename-block))
			  
				; now update object
				(setq atts (vlax-invoke vlaobject-block2  'getattributes)) 	 				; get all the attributes
				(setq z 0)
				(repeat (setq y (length atts ))                     						; repeat for how many attributes

					(setq attTagObj-FromBlock (nth z atts))
					(setq attTag-FromBlock (vla-get-tagstring attTagObj-FromBlock))
				  
					(setq cntr2 0)
					(setq cmax2 (length lst))
					(while
						(< cntr2 cmax2)
						(setq item(nth cntr2 lst))
						(setq attTagObj-FromTable(car item))
						(setq attTag-FromTable (vla-get-tagstring attTagObj-FromTable))
					  
						(setq value(cdr item))
						(if
							(= attTag-FromBlock attTag-FromTable)
							(progn
							(vla-put-textstring attTagObj-FromBlock value)
							(setq cntr2 cmax2)
							)
						)
						(setq cntr2 (+ cntr2 1))
					)
						; go through attributes and update
					(setq z (+ z 1))
				) ; repeat
				)
			)			
			
			
			)
		)
		(Setq lst '())
	  
	)

  


) ;defun



;;; --- VCONTROLLER ---
;;; Notes:
;;; Alex Lundin
;;; 08-07-2017
(defun c:VCONTROLLER
       	(
	/
	1PANELPROCESSORLISTFORSTATIONBUS 318LIST 318POLYLINES ACADOBJ ALLLISTS AUXILIARYPANELLIST AUXILIARYPOLYLINES AUXVALUE BLOCKS BUSRUN BUSRUNSTRING CMAX CNTR CONTROLLERBUSPOLYLINES CURRENTRS232DEVICE CURRENTRS232DEVICELOCATION CURRENTSTATIONBUSCOMPONENT CURRENTSTATIONBUSCOMPONENTLOCATION DISTANCEBETWEENDEVICES DISTANCEOFCURRENTSTATIONBUSCABLE DISTANCETOPROCESSOR DOC ENCLOSUREBUSPOLYLINES INDEXOFITEMWIREDFROM INDEXOFITEMWIREDTO INNERCOUNTER INNERCOUNTERMAX LAYER LAYERS MAINPANELLIST MAINVALUE MAXSTATIONBUSCABLEBETWEENDEVICES MAXSTATIONBUSCABLERUN NUMBEROFEQMAX NUMBEROFRS232PORTS NUMBEROFRS485PORTS NUMBEROFSTATIONBUSRUNS NUMBEROFSTATIONSMAX OLDECHO ORANGEFORCONNECTIONPOLYLINES PANELCOMPONENTLIST PANELCOMPONENTPOLYLINES PANELPROCESSOR PANELPROCESSORLIST PANELPROCESSORLISTFORRS232 PANELPROCESSORLISTFORSTATIONBUS PANELPROCESSORLOCATION POEDEVICELIST POEPOLYLINES POESWITCHLIST POINTLIST POLYLINES POLYLINESTODELETE PREVIOUSSTATIONBUSCOMPONENT PREVIOUSSTATIONBUSCOMPONENTLOCATION PROCESSORFORRS232 RETURNEDLIST RETURNLIST RS232DISTANCEBETWEENDEVICEMAX RS232LIST RS232POLYLINES RS232TOPROCESSOR RS232TOTALCABLERUNDISTANCE SECONDARYPANELLIST SECVALUE SELECTIONSET STATIONBUSDISTANCEBETWEENDEVICEMAX STATIONBUSLIST STATIONBUSPOLYLINES STATIONBUSPOSITION STATIONBUSPOSITIONASSTRING STATIONBUSRUNASSTRING STATIONBUSTOTALCABLERUNDISTANCE STATIONBUSTOTALCABLERUNDISTANCEMAX TAGS USERSET USERSTATIONBUSCOMBINED USERSTATIONBUSSET VALUES WIREDISTANCE WIRELAYER
	)
  
	;Default error handler for all functions at Legrand
	;The syntax for the function name must stay exactly as follows
	;The error function has asterisks on either side and accepts exactly 1 argument, msg
	;The AutoCAD LISP programming environment will look for this function and call it for any unexpected error during runtime
	(defun *error* ( msg )
	  	(vantage-move-blocks-from-wire-layers-to-Wattstopper)
	  	(vantage-layer-state-restorer)

	  
		;default msg, exit occurs naturally
		((= msg "Function cancelled")
		(princ "\nFunction canceled.")
		(princ "\nThis is most likely caused from terminating a custom command early.")
		(princ "\nExiting now.")
		)
		;default msg, exit occurs naturally
		((= msg "quit / exit abort")
		(princ "\nFunction exit.")
		(princ "\nThis is most likely caused from terminating a custom command early.")
		(princ "\nExiting now.")
		)			
  		;default msg, exit occurs naturally
		(t
		(princ "\nUnexpected error.")
	  	(princ "\nExiting now.")
		)
	)


  	(vl-load-com)
  	(princ "start")
	(setq oldecho (getvar "CMDECHO"))										;set oldecho varaible to hold Autocad CMDECHO
	(setvar "CMDECHO" 0)												;set CMDECHO to 0, supress commandline echo

  	(vantage-layer-state-setter)
  	(vantage-move-blocks-to-wire-layers)
  	(vantage-create-building-layout-layers)


	;save layers from the object model
	(setq acadObj (vlax-get-acad-object))
	(setq doc (vla-get-ActiveDocument acadObj))
  	(setq layers (vla-get-layers doc))
  
	(setq layer (vla-item layers "_WS_V_ORANGE_FOR_CONNECTIONS"))
	(vla-put-layeron layer :vlax-true)
	;hide panels and panel cable
	(setq layer (vla-item layers "_WS_V_CONTROLLER_BUS"))
	(vla-put-layeron layer :vlax-false)		
	(setq layer (vla-item layers "_WS_V_ENCLOSURE_BUS"))
	(vla-put-layeron layer :vlax-false)		
	(setq layer (vla-item layers "_WS_V_AUXILIARY"))
	(vla-put-layeron layer :vlax-false)
 
	(setq layer (vla-item layers "WATTSTOPPER"))
	(vla-put-layeron layer :vlax-false)
  
	(prompt "Select all blocks in area to wire:")
  	(setq selectionSet (ssget))
	(setq returnedList (vantage-seperate-set selectionSet))
  	(setq blocks (nth 0 returnedList))
  	(setq polylines (nth 1 returnedList))
  
  	(setq returnedList (vantage-seperate-polylines-by-layer polylines))

	(setq 318Polylines (nth 0 returnedList))
	(setq auxiliaryPolylines (nth 1 returnedList)) 
	(setq controllerBusPolylines (nth 2 returnedList)) 
	(setq enclosureBusPolylines (nth 3 returnedList)) 
	(setq orangeForConnectionPolylines (nth 4 returnedList)) 
	(setq panelComponentPolylines (nth 5 returnedList)) 
	(setq poePolylines (nth 6 returnedList)) 
	(setq RS232Polylines (nth 7 returnedList)) 
	(setq stationBusPolylines (nth 8 returnedList))

  	(setq polylinesToDelete (append 318Polylines RS232Polylines stationBusPolylines))
  
	(vantage-delete-existing-cable polylinesToDelete)
  
  	(setq Tags (list "VSTATIONBUS" "VSTATIONBUSPOSITION" "VRS232PORTS"))
  
	(vantage-reset-block-attributes-to-default doc blocks Tags)

	(setq returnedList (vantage-layout-block-info-list blocks))
	(setq mainpanellist (nth 0 returnedList))
	(setq secondarypanellist (nth 1 returnedList))
	(setq auxiliarypanellist (nth 2 returnedList))
	(setq panelcomponentlist (nth 3 returnedList))
	(setq stationbuslist (nth 4 returnedList))
  
  	;;; standard layer move keeps the LVOS-0-10-PWM-P-1 to panel component layer,
  	;;; move the entire station bus set to account for that
  	(vantage-move-block-list-to-layer stationbuslist "_WS_V_STATION_BUS")
  
	(setq poedevicelist (nth 5 returnedList))
	(setq 318list (nth 6 returnedList))
	(setq rs232list (nth 7 returnedList))
	(setq panelprocessorlist (nth 8 returnedList))
  	(setq poeswitchlist (nth 9 returnedList))
	(setq allLists (append mainpanellist secondarypanellist auxiliarypanellist panelcomponentlist stationbuslist poedevicelist 318list rs232list panelprocessorlist poeswitchlist))

  	
  	(princ)

		



	
	(setq layer (vla-item layers "_WS_V_RS232"))
	(vla-put-layeron layer :vlax-false) 	
	
	
	(setq panelprocessorlistForRS232 panelprocessorlist)
  	(setq RS232toProcessor rs232list)
  
  	(setq cntr 0)
	(setq cmax (length RS232toProcessor))
	
  
	;loop through all RS232 devices,
	;connect them to nearest processor
	(while
	  	(AND (< cntr cmax)(/= RS232toProcessor nil))
	  
	  	(setq currentRS232Device (nth 0 RS232toProcessor))
	  
		(if
		  	(AND (/= currentRS232Device nil))
			(progn

			;-start RS232 if
			(if
			  	(/= panelprocessorlistForRS232 nil)
				(progn

				
				(setq RS232DistanceBetweenDeviceMax 720)

				(setq innerCounter 0)
				(setq innerCounterMax (length panelprocessorlistForRS232))
				
				(setq RS232TotalCableRunDistance 0)			
				
				(setq wireLayer "_WS_V_RS232")
				
				;RS232 device
				(while	(< innerCounter innerCounterMax)
					
					
						(setq returnedList (vantange-find-closest currentRS232Device panelprocessorlistForRS232 RS232DistanceBetweenDeviceMax))
						(setq indexOfItemWiredFrom (nth 0 returnedList))
						(setq indexOfItemWiredTo (nth 1 returnedList))
						(setq pointList (nth 2 returnedList))
					  	(setq wireDistance (nth 3 returnedList))
					  
						(setq currentRS232Device (nth indexOfItemWiredFrom RS232toProcessor))
						(setq currentRS232DeviceLocation (nth 6 currentRS232Device))
				  		(setq processorForRS232 (nth indexOfItemWiredTo panelprocessorlistForRS232))
						(setq panelprocessorLocation (nth 6 processorForRS232))
				
						(setq distanceToProcessor (distance panelprocessorLocation currentRS232DeviceLocation))
														

						(setq Tags (list "VRS232PORTS"))
						(setq returnList (vmain-getAttributesAndValues processorForRS232 Tags))
						(setq Tags (nth 0 returnList))
						(setq Values (nth 1 returnList))

						(setq numberOfRS232Ports (nth 0 Values))
				  		(setq numberOfRS232Ports (atoi numberOfRS232Ports))
						(setq numberOfRS232Ports (- numberOfRS232Ports 1))
						
				  			
					
						; subtract connection to processor
				  		; remove processor from list to search
						(if
							(AND
							(> numberOfRS232Ports -1)(/= pointList nil)
							)
							(progn
							  
							(setq numberOfRS232Ports (itoa numberOfRS232Ports))
							(setq Values (list numberOfRS232Ports))
							
							(vmain-setAttributesAndValues processorForRS232 Tags Values)
							
							(setq Tags (list "VRS232PORTS"))	
							(vmain-setAttributesAndValues processorForRS232 Tags Values)


							(setq Tags (list "VMAIN" "VSECONDARY" "VAUXILIARY"))
							(setq returnList (vmain-getAttributesAndValues processorForRS232 Tags))
							(setq Tags (nth 0 returnList))
							(setq Values (nth 1 returnList))
							(setq mainValue (nth 0 Values))
							(setq secValue (nth 1 Values))
							(setq auxValue (nth 2 Values))
							
							(setq Values (list mainValue secValue auxValue))
							
							(vmain-setAttributesAndValues currentRS232Device Tags Values)



							
							(vantage-move-block-to-layer currentRS232Device "WATTSTOPPER")




							
							(setq RS232toProcessor (VANTAGE-REMOVENTH 0 RS232toProcessor))
					  		(setq RS232toProcessor (vantage-layout-block-info-list-recalulate-index RS232toProcessor))
							
							(VANTAGE-DRAW-LWPOLYLINE pointList 0 wireLayer 0 "BYLAYER" 256)								
							;(setq numberOfRS232Ports (- numberOfRS232Ports 1))
							(setq innerCounter innerCounterMax)
							)
						)				

				  	(setq innerCounter (+ innerCounter 1))
				;-end  while RS232
				)
				;-end progn RS232
				)
			;-end if RS232
			)


			;;; move processor back after all connections are drawn to it
			(vantage-move-block-to-layer processorForRS232 "_WS_V_PANEL_COMPONENTS")			
			;-end progn on if for panel processor not nil
			)	
		;-end if panel processor not nil
		)
	(setq innerCounter 0)
  	

	(setq cntr (+ cntr 1))

	;-RS232 loop
	)
	

	; alert user if devices on run are left
	(if	(/= (length RS232toProcessor ) 0)
		(progn
		(princ "\nReached maximum RS232 ports on processor.")
		(princ "\nSome of your RS232 devices are not connected to the processor.")
		)
	)



	;hide panels and panel cable
	(setq layer (vla-item layers "_WS_V_CONTROLLER_BUS"))
	(vla-put-layeron layer :vlax-false)		
	(setq layer (vla-item layers "_WS_V_ENCLOSURE_BUS"))
	(vla-put-layeron layer :vlax-false)		
	(setq layer (vla-item layers "_WS_V_AUXILIARY"))
	(vla-put-layeron layer :vlax-false)
	(setq layer (vla-item layers "_WS_V_PANEL_COMPONENTS"))
	(vla-put-layeron layer :vlax-false)
  
	;turn layer on
	(setq layer (vla-item layers "_WS_V_ORANGE_FOR_CONNECTIONS"))
	(vla-put-layeron layer :vlax-true)
  
  
  	;save initial panel processors from first selection
  	(setq panelprocessorlistForStationBus panelprocessorlist)
  

	;move objects to layer
  	(vantage-move-block-list-to-layer panelprocessorlistForStationBus "_WS_V_ORANGE_FOR_CONNECTIONS")
	
	;station bus section, allow user to choose starting processor
	(prompt "Select 1 processor to connect station bus devices too:")
  	(setq selectionSet (ssget))
	(setq returnedList (vantage-seperate-set selectionSet))
  	(setq blocks (nth 0 returnedList))
  	(setq polylines (nth 1 returnedList))
 
	(setq returnedList (vantage-layout-block-info-list blocks))
	(setq 1panelprocessorlistForStationBus (nth 8 returnedList))

	(setq cntr 0)
	(setq cmax (length 1panelprocessorlistForStationBus))		

	
	(setq panelprocessor (nth 0 1panelprocessorlistForStationBus))
  
	;move objects to layer
  	(vantage-move-block-list-to-layer panelprocessorlistForStationBus "_WS_V_PANEL_COMPONENTS")	
	
	;loop through all processor components
	(while
	  	(AND (/= panelprocessor nil))
	  
	  	(if
			(> cmax 1)
			(progn
			;;alert for multiple processors in set
			)
		)

	  	
		



	  
		(if
		  	(AND (/= panelprocessor nil))
			(progn


			(setq panelprocessorLocation (nth 6 panelprocessor))
		  

		  	;in inches
		  	(setq numberOfStationBusRuns (nth 7 panelprocessor))
		  	(setq numberOfStationsMax (nth 8 panelprocessor))
		  	(setq numberOfEQMax (nth 9 panelprocessor))
		  	(setq numberOfRS232Ports (nth 10 panelprocessor))
		  	(setq numberOfRS485Ports (nth 11 panelprocessor))
		  	(setq maxstationbuscablerun (nth 12 panelprocessor))
		  	(setq maxstationbuscablebetweendevices (nth 13 panelprocessor))



		  
		  	(setq stationBusDistanceBetweenDeviceMax maxstationbuscablebetweendevices)
			(setq stationBusTotalCableRunDistance 0)
		  	(setq stationBusTotalCableRunDistanceMax maxstationbuscablerun)


			(setq Tags (list "VMAIN" "VSECONDARY" "VAUXILIARY"))
			(setq returnList (vmain-getAttributesAndValues panelprocessor Tags))
			(setq Tags (nth 0 returnList))
			(setq Values (nth 1 returnList))
			(setq mainValue (nth 0 Values))
			(setq secValue (nth 1 Values))
			(setq auxValue (nth 2 Values))
			
			(setq Values (list mainValue secValue auxValue))
			
			

			  

			(vantage-show-block-in-orange panelprocessor)	


			;turn layer on
			(setq layer (vla-item layers "_WS_V_STATION_BUS"))
			(vla-put-layeron layer :vlax-true)
			
			;turn layer off
			(setq layer (vla-item layers "WATTSTOPPER"))
			(vla-put-layeron layer :vlax-false)
			
			;determine number of runs on controller
			(setq busRun 1)
			(setq busRunString (itoa busRun))


		  	;keep track of how long run total is
		  	;keep track of how long current line is
			;connect processor to nearest station bus component
			
			;connection loop, repeat twice because station bus is the only thing that must happen again
			(repeat numberOfStationBusRuns
				(progn


				  	(if
					  	(= busRun 1)
					  	(progn
						(princ (strcat "\nThis is station bus run " busRunString " :"))
						(princ "\nSelect Station bus components to wire to controller:")
						(setq userSet (ssget '((0 . "INSERT"))))
						
						(setq returnedList (vantage-seperate-set userSet))
						(setq blocks (nth 0 returnedList))
						(setq polylines (nth 1 returnedList))
						
					  	(setq returnedList (vantage-layout-block-info-list blocks))
					  	(setq userStationBusSet (nth 4 returnedList))
						(setq userStationBusCombined userStationBusSet)
						(setq stationBusPosition 0)
						)
					)
				  	(if
					  	(= busRun 2)
					  	(progn
						(princ (strcat "\nThis is station bus run " busRunString " :"))
						(princ "\nSelect Station bus components to wire to controller:")
						
						(setq userSet (ssget '((0 . "INSERT"))))

						(setq returnedList (vantage-seperate-set userSet))
						(setq blocks (nth 0 returnedList))
						(setq polylines (nth 1 returnedList))
						
					  	(setq returnedList (vantage-layout-block-info-list blocks))
					  	(setq userStationBusSet (nth 4 returnedList))
						(setq userStationBusCombined (append userStationBusCombined userStationBusSet))
						(setq stationBusPosition 0)
						)
					  
					)



		  
					;if user specified a station bus run
					(if
					  	(/= userStationBusSet nil)
						(progn
						(setq innerCounter 0)
						(setq innerCounterMax (length userStationBusSet))
						  
						(setq returnedList (vantange-find-closest panelprocessor userStationBusSet stationBusDistanceBetweenDeviceMax))
						(setq indexOfItemWiredFrom (nth 0 returnedList))
						(setq indexOfItemWiredTo (nth 1 returnedList))
						(setq pointList (nth 2 returnedList))
					  	(setq wireDistance (nth 3 returnedList))
			
					  	(setq currentStationBusComponent (nth indexOfItemWiredTo userStationBusSet))
					  	(setq currentStationBusComponentLocation (nth 6 currentStationBusComponent))
					  	(setq distanceToProcessor (distance panelprocessorLocation currentStationBusComponentLocation))
						(setq previousStationBusComponentLocation panelprocessorLocation)
						(setq distanceBetweenDevices (distance previousStationBusComponentLocation currentStationBusComponentLocation))
					  	
					  	(setq wireLayer "_WS_V_STATION_BUS")
					  	(setq stationBusTotalCableRunDistance distanceBetweenDevices)
					  	(if
						  	(AND (<= stationBusTotalCableRunDistance stationBusTotalCableRunDistanceMax)(<= distanceBetweenDevices stationBusDistanceBetweenDeviceMax)(/= currentStationBusComponent nil))
						  	(progn
							(setq stationBusPosition (+ stationBusPosition 1))
							;set values
							(setq stationBusRunAsString (itoa busRun))
							(setq stationBusPositionAsString (itoa stationBusPosition))
							(setq Values (list mainValue secValue auxValue stationBusRunAsString stationBusPositionAsString))
							(setq Tags (list "VMAIN" "VSECONDARY" "VAUXILIARY" "VSTATIONBUS" "VSTATIONBUSPOSITION"))
							(vmain-setAttributesAndValues currentStationBusComponent Tags Values)
							
							;draw wire if within limits
							(VANTAGE-DRAW-LWPOLYLINE pointList 0 wireLayer 0 "BYLAYER" 256)
							;save device as previous
							(setq previousStationBusComponent currentStationBusComponent)					
							;recalculate list
						  	(setq userStationBusSet (VANTAGE-REMOVENTH indexOfItemWiredTo userStationBusSet))
						  	(setq userStationBusSet (vantage-layout-block-info-list-recalulate-index userStationBusSet))
		 					;move block to hide from future selections by user
							(vantage-move-block-to-layer previousStationBusComponent "WATTSTOPPER")
							)
						)
						


					  	(while
						  	;connect current stationbus component to nearest station bus component
						  	(AND (/= userStationBusSet nil)(<= stationBusTotalCableRunDistance stationBusTotalCableRunDistanceMax)(/= previousStationBusComponent nil)(/= currentStationBusComponent nil)(< innerCounter innerCounterMax))

							(setq returnedList (vantange-find-closest previousStationBusComponent userStationBusSet stationBusDistanceBetweenDeviceMax))
						  	; this indexOfItemWiredFrom is calculated from a device that is already removed, don't use it!
							;(setq indexOfItemWiredFrom (nth 0 returnedList))
							(setq indexOfItemWiredTo (nth 1 returnedList))
							(setq pointList (nth 2 returnedList))
						  	(setq wireDistance (nth 3 returnedList))
							

							;save device as previous
							(setq previousStationBusComponent previousStationBusComponent)
						  	(setq previousStationBusComponentLocation (nth 6 previousStationBusComponent))
						  	(setq currentStationBusComponent (nth indexOfItemWiredTo userStationBusSet))
						  	(setq currentStationBusComponentLocation (nth 6 currentStationBusComponent))

						  	(setq distanceToProcessor (distance panelprocessorLocation currentStationBusComponentLocation))
							(setq distanceBetweenDevices (distance previousStationBusComponentLocation currentStationBusComponentLocation))
							(setq distanceOfCurrentStationBusCable (distance previousStationBusComponentLocation currentStationBusComponentLocation))

						  	;this line drives the current run, once maxed, this continues loops to next processor or next bus run because this run is maxed
						  	(setq stationBusTotalCableRunDistance (+ stationBusTotalCableRunDistance distanceOfCurrentStationBusCable ))
						  	(if
							  	(AND (<= stationBusTotalCableRunDistance stationBusTotalCableRunDistanceMax)(<= distanceBetweenDevices stationBusDistanceBetweenDeviceMax)(/= currentStationBusComponent nil))
							  	(progn
								(setq stationBusPosition (+ stationBusPosition 1))
								
								;set values
								(setq stationBusRunAsString (itoa busRun))
								(setq stationBusPositionAsString (itoa stationBusPosition))
								(setq Values (list mainValue secValue auxValue stationBusRunAsString stationBusPositionAsString))
								(setq Tags (list "VMAIN" "VSECONDARY" "VAUXILIARY" "VSTATIONBUS" "VSTATIONBUSPOSITION"))
								(vmain-setAttributesAndValues currentStationBusComponent Tags Values)
								
								;draw wire if within limits
								(VANTAGE-DRAW-LWPOLYLINE pointList 0 wireLayer 0 "BYLAYER" 256)
								;recalculate list
								(setq userStationBusSet (VANTAGE-REMOVENTH indexOfItemWiredTo userStationBusSet))
							  	(setq userStationBusSet (vantage-layout-block-info-list-recalulate-index userStationBusSet))
								;save device as previous
								(setq previousStationBusComponent currentStationBusComponent)
								;move block to hide from future selections by user
								(vantage-move-block-to-layer previousStationBusComponent "WATTSTOPPER")
								
								)
							)
						  	(setq innerCounter (+ innerCounter 1))
						  
						     
						)
						(setq busRun (+ busRun 1))
						(setq busRunString (itoa busRun))
						
						;-end progn station bus
						)
					;-end station bus runs if
					)
				;-end repeat progn
				)
			;-end repeat for number of station bus runs
			)

			;;; move processor back after all connections are drawn to it
			(vantage-move-block-to-layer panelprocessor "_WS_V_PANEL_COMPONENTS")
			

			;;; remove matching item from set for all station bus using find closest

			(setq returnedList (vantange-find-closest panelprocessor panelprocessorlistForStationBus 32000))
			(setq indexOfItemWiredFrom (nth 0 returnedList))
			(setq indexOfItemWiredTo (nth 1 returnedList))
			
			;recalculate list
			(setq panelprocessorlistForStationBus (VANTAGE-REMOVENTH indexOfItemWiredTo panelprocessorlistForStationBus))
		  	(setq panelprocessorlistForStationBus (vantage-layout-block-info-list-recalulate-index panelprocessorlistForStationBus))
			

			;-end progn on if for panel processor not nil
			)	
		;-end if panel processor not nil
		)
	(setq innerCounter 0)
	  
	;move objects to layer
  	(vantage-move-block-list-to-layer panelprocessorlistForStationBus "_WS_V_ORANGE_FOR_CONNECTIONS")
	  
	;station bus section, allow user to choose starting processor
	(prompt "Select 1 processor to connect station bus devices too:")
  	(setq selectionSet (ssget))
	(setq returnedList (vantage-seperate-set selectionSet))
  	(setq blocks (nth 0 returnedList))
  	(setq polylines (nth 1 returnedList))
 
	(setq returnedList (vantage-layout-block-info-list blocks))
	(setq 1panelprocessorlistForStationBus (nth 8 returnedList))
	  
	(setq panelprocessor (nth 0 1panelprocessorlistForStationBus))
	  
	(setq cntr 0)
	(setq cmax (length 1panelprocessorlistForStationBus))	

	(setq cntr (+ cntr 1))
	  
	;move objects to layer
  	(vantage-move-block-list-to-layer panelprocessorlistForStationBus "_WS_V_PANEL_COMPONENTS")
	  
	;-processor loop
	)  



	;turn layers off
	(setq layer (vla-item layers "_WS_V_STATION_BUS"))
	(vla-put-layeron layer :vlax-false)
  
	(vantage-layer-state-restorer)
  	(vantage-move-blocks-from-wire-layers-to-Wattstopper)
  	(setvar "CMDECHO" oldecho)
	(print "end")
  
)

(defun vantage-move-block-list-to-layer (inputlist layer / cntr cmax item)
	(setq cntr 0)
	(setq cmax (length inputlist))

  	(while 	(< cntr cmax)
		(setq item (nth cntr inputlist))
	  	(vantage-move-block-to-layer item layer)
		(setq cntr (+ cntr 1))
	)
  

)



;;; --- VMAIN ---
;;; Notes:
;;; Alex Lundin
;;; 08-07-2017
(defun c:VMAIN
       		(
		;arguments
		/
		;local variables
		318LIST AUXILIARYPANELLIST INDEXOFITEMWIREDFROM INDEXOFITEMWIREDTO LISTFORMAINTOMAIN MAINPANEL MAINPANELLIST MAINSAFTEY NUMBEROFMAINS OLDECHO PANELCOMPONENTLIST POELIST RETURNEDLIST RS232LIST SECONDARYPANELLIST STATIONBUSLIST
		WIRELAYER AUXILIARYCNTR AUXILIARYPANEL AUXILIARYSAFTEY LISTFORAUXILIARY LISTFORMAINTOAUXILIARY LISTORGANIZEDMAINS USERAUXILIARYSET
		BLOCKS CMAX CNTR LISTFORCOMPONENTTOPANEL LISTFORMAINTOSECONDARY LISTFORSECONDARYTOSECONDARY LISTOFALLPANELS NUMBEROFALLPANELS PANEL PANELCOMPONENTSAFTEY POINTLIST PREVIOUSCOMPONENT SECONDARYCNTR SECONDARYPANEL SECONDARYSAFTEY USERMAINSET USERSECONDARYSET
		panelprocessorlist CURRENTSTATIONBUSCOMPONENT CURRENTSTATIONBUSCOMPONENTLOCATION DISTANCEOFCURRENTSTATIONBUSCABLE DISTANCETOPROCESSOR FIRSTMAINPANEL INNERCOUNTER INNERCOUNTERMAX PANELPROCESSOR PANELPROCESSORLOCATION PREVIOUSSTATIONBUSCOMPONENT PREVIOUSSTATIONBUSCOMPONENTLOCATION STATIONBUSDISTANCETOPROCESSORMAX STATIONBUSTOTALCABLERUNDISTANCE STATIONBUSTOTALCABLERUNDISTANCEMAX WIREDISTANCE
		BUSRUN BUSRUNSTRING DISTANCEBETWEENDEVICES DXF8 ENTDXF LAYER NUMBEROFSTATIONBUSRUNS SELECTIONCOUNTER SELECTIONCOUNTERMAX SELECTIONITEM STATIONBUSDISTANCEBETWEENDEVICEMAX USER318SET USER318SETMAX USERRS232SET USERRS232SETMAX USERSET USERSTATIONBUSSET USERSTATIONBUSSETMAX
		CURRENTPOEDEVICE CURRENTPOEDEVICELOCATION CURRENTPOESWITCH CURRENTPOESWITCHLOCATION INNERCOUNTER2 INNERCOUNTERMAX2 NUMBEROFETHERNETPORTS NUMBEROFPOEPORTS POEDEVICELIST POEDISTANCEBETWEENDEVICEMAX POESWITCHITEM POESWITCHLIST POESWITCHLOCATION POETOTALCABLERUNDISTANCE PREVIOUSPOESWITCH PREVIOUSPOESWITCHLOCATION TOTALNUMBEROFETHERNETPORTS TOTALNUMBEROFPOEPORTS USEABLEETHERNETPORTS USEABLEPOEPORTS USERPOEDEVICESET USERPOESWITCHSET
		318DISTANCEBETWEENDEVICEMAX 318GROUPFIRST 318GROUPLAST 318GROUPTOSB 318GROUPTOSBOPTIONS CURRENT318COMPONENT CURRENT318COMPONENTLOCATION INDEXOF318ITEMWIREDTO INDEXOFSBITEMWIREDTO POINTLIST318 POINTLIST318TOSB POINTLISTTOSB PREVIOUS318COMPONENT USERSTATIONBUSCOMBINED WIREDISTANCE318TOSB WIREDISTANCETO318 WIREDISTANCETOSB
		CURRENTRS232DEVICE CURRENTRS232DEVICELOCATION NUMBEROFRS232PORTS PREVIOUSRS232DEVICE PREVIOUSRS232DEVICELOCATION RS232DISTANCEBETWEENDEVICEMAX RS232TOTALCABLERUNDISTANCE
		 DOC LAYERS 318ALREADYINTEGRATED ACADOBJ DISTANCELEFTONSECONDARYRUN MAXNUMBEROFCONTROLLERS MESSAGE POINTLIST318TOALREADYINTEGRATED318 SECONDARYMAXFLAG WIREDISTANCE318TOALREADYINTEGRATED318
		AUXILIARYMAXDISTANCEFLAG COMPONENTMAXDISTANCEFLAG COMPONENTMAXNUMBERFLAG CONTROLLERBUSDISTANCEMAX CONTROLLERBUSRUNCOUNTER CONTROLLERBUSRUNCOUNTERMAX MAINCOUNTER MAINCOUNTERASSTRING MAINMAXDISTANCEFLAG MAXNUMBEROFMODULES NUMBEROFMODULES POEDISTANCEMAX RETURNLIST SECONDARYLETTER SECONDARYMAXDISTANCEFLAG SECONDARYSTRING TAGS VALUES
		ALLLISTS AUXQTYVALUE COMPONENTSTOBOXIN COMPONENTTYPE CURRENTCOMPONENT ITEM MAINVALUE MAXNUMBEROFLVOS NUMBEROFLVOS NUMBEROFPROCESSORS NUMBEROFSECONDARIES POLYLINES PROCESSORTYPE SECVALUE SELECTIONSET USERMAINSEPERATEDSET
		)

	;Default error handler for all functions at Legrand
	;The syntax for the function name must stay exactly as follows
	;The error function has asterisks on either side and accepts exactly 1 argument, msg
	;The AutoCAD LISP programming environment will look for this function and call it for any unexpected error during runtime
	(defun *error* ( msg )
	  	(vantage-move-blocks-from-wire-layers-to-Wattstopper)
	  	(vantage-layer-state-restorer)
	  
		;default msg, exit occurs naturally
		((= msg "Function cancelled")
		(princ "\nFunction canceled.")
		(princ "\nThis is most likely caused from terminating a custom command early.")
		(princ "\nExiting now.")
		)
		;default msg, exit occurs naturally
		((= msg "quit / exit abort")
		(princ "\nFunction exit.")
		(princ "\nThis is most likely caused from terminating a custom command early.")
		(princ "\nExiting now.")
		)			
  		;default msg, exit occurs naturally
		(t
		(princ "\nUnexpected error.")
	  	(princ "\nExiting now.")
		)
	)

  	

  	(vl-load-com)
  	(princ "start")
	(setq oldecho (getvar "CMDECHO"))										;set oldecho varaible to hold Autocad CMDECHO
	(setvar "CMDECHO" 0)												;set CMDECHO to 0, supress commandline echo


    	;(vantage-layer-state-setter)
  	
  	(vantage-move-blocks-to-wire-layers)
  	(vantage-create-building-layout-layers)


	;save layers from the object model
	(setq acadObj (vlax-get-acad-object))
	(setq doc (vla-get-ActiveDocument acadObj))
  	(setq layers (vla-get-layers doc))
  
	(setq layer (vla-item layers "_WS_V_ORANGE_FOR_CONNECTIONS"))
	(vla-put-layeron layer :vlax-true)

	(prompt "Select all blocks in area to wire:")
  	(setq selectionSet (ssget))
	(setq returnedList (vantage-seperate-set selectionSet))
  	(setq blocks (nth 0 returnedList))
  	(setq polylines (nth 1 returnedList))
  	(vantage-delete-existing-cable polylines)

  	;reset certain attributes to default
    	(setq Tags (list "VMAIN" "VSECONDARY" "VAUXILIARY" "VAUXILIARYQTY"))
  	(vantage-reset-block-attributes-to-default doc blocks Tags)
  
	(setq returnedList (vantage-layout-block-info-list blocks))
	(setq mainpanellist (nth 0 returnedList))
	(setq secondarypanellist (nth 1 returnedList))
	(setq auxiliarypanellist (nth 2 returnedList))
	(setq panelcomponentlist (nth 3 returnedList))
	(setq stationbuslist (nth 4 returnedList))
	(setq poedevicelist (nth 5 returnedList))
	(setq 318list (nth 6 returnedList))
	(setq rs232list (nth 7 returnedList))
	(setq panelprocessorlist (nth 8 returnedList))
  	(setq poeswitchlist (nth 9 returnedList))
	(setq allLists (append mainpanellist secondarypanellist auxiliarypanellist panelcomponentlist stationbuslist poedevicelist 318list rs232list panelprocessorlist poeswitchlist))

  	

	(vantage-turn-building-layout-layers-off)
  

	  
	(setq mainpanellist (vantage-sort-by-distance mainpanellist))
	(setq numberofmains (length mainpanellist))
  	
	(setq maxNumberOfControllers 31)
	(if
	  	(> numberofmains maxNumberOfControllers)
	  	(progn
		(setq message "Too Many")
		(vmain-mainMaxErrorHandler message)
		)
	)
  	(setq numberofsecondaries (length secondarypanellist))
	(if
	  	(> numberofsecondaries (* numberofmains 6))
	  	(progn
		(setq message "Too Many")
		
		(vmain-secondaryMaxErrorHandler message)
		)
	)  
	(setq listForComponentToPanel panelcomponentlist)
	(setq listOfAllPanels (append mainpanellist secondarypanellist auxiliarypanellist))
  	(setq numberOfAllPanels (length listOfAllPanels))
	
  
  	;;; if there is atleast 1 main
	(if
	  	(>= numberofmains 1)
	  	(progn
		
		(setq listForMaintoMain mainpanellist)
		(setq layer (vla-item layers "_WS_V_CONTROLLER_BUS"))
		(vla-put-layeron layer :vlax-true)
		
		(prompt "Select the first main panel:")
	  	(setq SelectionSet (ssget '((0 . "INSERT"))))
		(setq returnedList (vantage-seperate-set SelectionSet))
	  	(setq userMainSeperatedSet (nth 0 returnedList))
		
		(setq returnedList (vantage-layout-block-info-list userMainSeperatedSet))
		(setq userMainSet (nth 0 returnedList))

	  	(setq mainpanel (nth 0 userMainSet))
		
		;turn layer on
;;;		(setq layer (vla-item layers "_WS_V_POE"))
;;;		(vla-put-layeron layer :vlax-true)
;;;		
;;;		(prompt "Select one POE switch for systems with more than fifteen main panels:")
;;;		(setq userPOESwitchSet (ssget '((0 . "INSERT"))))
;;;		(setq returnedList (vantage-layout-block-info-list userPOESwitchSet))
;;;		(setq userPOESwitchSet (nth 9 returnedList))



		(setq controllerBusRunCounter 1)
		(setq controllerBusRunCounterMax 31)
		(setq controllerBusDistanceMax 24000)
		(setq poeDistanceMax 36000)
		(setq mainMaxDistanceFlag 0)
		
		;;; this removes the first main selected by user from the list of all main to mains
		;;; the find closest will always return the item that mirrors the block selected by the user because it is closest
		(setq returnedList (vantange-find-closest mainpanel listForMaintoMain controllerBusDistanceMax))
		(setq indexOfItemWiredFrom (nth 0 returnedList))
		(setq indexOfItemWiredTo (nth 1 returnedList))
		
		(setq mainpanel (nth indexOfItemWiredTo listForMaintoMain))
		(setq listForMaintoMain (VANTAGE-REMOVENTH indexOfItemWiredTo listForMaintoMain))
		(setq listForMaintoMain (vantage-layout-block-info-list-recalulate-index listForMaintoMain))
		(setq listOrganizedMains (cons mainpanel listOrganizedMains))
		
		(setq Tags (list "VSECONDARY" "VAUXILIARY"))
		(setq returnList (vmain-getAttributesAndValues mainpanel Tags))
		(setq Tags (nth 0 returnList))
		(setq Values (nth 1 returnList))
		(setq mainCounter 1)
		
		(setq Tags (append Tags (list "VMAIN")))
		(setq Values (list "0" "0" (itoa mainCounter)))		

		
		(vmain-setAttributesAndValues mainpanel Tags Values)


		;main to main loop
		(while
		(/= listForMaintoMain nil)

			;(setq listForMaintoPOE userPOESwitchSet)

			;;; if the controller bus run has capacity left and distance is avaiable
			;;; continue wiring on the controller bus run
			(if
			  	(AND
				(<= controllerBusRunCounter controllerBusRunCounterMax)(> controllerBusDistanceMax 0)
				)
			  	(progn

				(setq returnedList (vantange-find-closest mainpanel listForMaintoMain controllerBusDistanceMax))
				(setq indexOfItemWiredFrom (nth 0 returnedList))
				(setq indexOfItemWiredTo (nth 1 returnedList))
				(setq pointList (nth 2 returnedList))
				(setq wireDistance (nth 3 returnedList))
				(setq wireLayer "_WS_V_CONTROLLER_BUS")
			
				
				(setq controllerBusDistanceMax (- controllerBusDistanceMax wireDistance)) 

				;;; if connection is legal
				(if
				  	(AND
					(/= pointList nil)(> controllerBusDistanceMax 0)
					)
				  	(progn
					(setq mainpanel (nth indexOfItemWiredTo listForMaintoMain))
					(setq mainCounter (+ mainCounter 1))
					(setq mainCounterAsString (itoa mainCounter))
					  
					(VANTAGE-DRAW-LWPOLYLINE pointList 0 wireLayer 0 "BYLAYER" 256)

					(setq Values (list "0" "0" mainCounterAsString))
					(setq controllerBusRunCounter (+ controllerBusRunCounter 1))
				
					(vmain-setAttributesAndValues mainpanel Tags Values)



					)
				  	;;; else connection is illegal because:
				  	;;; connection exceeded max distance
				  	(progn
					(setq mainMaxDistanceFlag 1)
					;;; attempt to wire to poe switch on next interation
					;(setq controllerBusDistanceMax 0)
					;;; end else for illegal connection
					)
				;;; end if for legal connections
				)


				
				;;; end if for controller bus run
				)
 
				;;; else, controller bus run is maxed, attempt to start new run
			  	;;; jump to next main panel that is farthest from supplied switch
;;;			  	(progn
;;;
;;;				(if
;;;				  	(/= listForMaintoPOE nil)
;;;				  	(progn
;;;					(setq currentPOEForMain (nth 0 listForMaintoPOE))
;;;					(setq returnedList (vantange-find-farthest currentPOEForMain listForMaintoMain poeDistanceMax))
;;;					(setq indexOfItemWiredFrom (nth 0 returnedList))
;;;					(setq indexOfItemWiredTo (nth 1 returnedList))
;;;
;;;					(setq mainpanel (nth indexOfItemWiredTo listForMaintoMain))
;;;					)
;;;				  	(progn
;;;
;;;					)
;;;				)
;;;				
;;;				;;; while to look for closest POE switch with remaining ports from user set
;;;				(while
;;;				  	(/= listForMaintoPOE nil)
;;;					(setq returnedList (vantange-find-closest mainpanel listForMaintoPOE poeDistanceMax))
;;;					(setq indexOfItemWiredFrom (nth 0 returnedList))
;;;					(setq indexOfItemWiredTo (nth 1 returnedList))
;;;					(setq pointList (nth 2 returnedList))
;;;					(setq wireDistance (nth 3 returnedList))
;;;					(setq wireLayer "_WS_V_POE")
;;;					(setq POEports 1)
;;;					;;; if wire distance is legal
;;;					(if
;;;
;;;						(/= pointList nil)
;;;
;;;					  	(progn
;;;
;;;						;;; if POEports are legal
;;;						(if
;;;						  	(> POEPorts 0)
;;;						  	(progn
;;;							(VANTAGE-DRAW-LWPOLYLINE pointList 0 wireLayer 0 "BYLAYER" 256)
;;;							(setq controllerBusRunCounter 1)
;;;							(setq controllerBusRunCounterMax 15)
;;;							(setq controllerBusDistanceMax 24000)
;;;
;;;							
;;;							
;;;							
;;;							
;;;							(setq Values (list "0" "0" mainCounterAsString))
;;;							(setq controllerBusRunCounter (+ controllerBusRunCounter 1))
;;;						
;;;							(vmain-setAttributesAndValues mainpanel Tags Values)
;;;							)
;;;						  	;;; else illegal POE ports
;;;						  	(progn
;;;							(setq mainMaxPOEPortFlag 1)
;;;							;;; end else POE ports illegal
;;;							)
;;;
;;;						;;; end if POE ports legal
;;;						)
;;;						)
;;;					  	;;; else wire distance is illegal because:
;;;					  	;;; POE switch is too far away
;;;					  	(progn
;;;						(setq mainMaxPOEDistanceFlag 1)
;;;						;;; end else for illegal wire distance
;;;						)
;;;					;;; end if for legal wire distance
;;;					)
;;;				  
;;;				(setq listForMaintoPOE (VANTAGE-REMOVENTH indexOfItemWiredTo listForMaintoPOE))
;;;				(setq listForMaintoPOE (vantage-layout-block-info-list-recalulate-index listForMaintoPOE))				  
;;;				;;; end while
;;;				)
;;;
;;;				;;; end else for starting new controller run from POE
;;;				)


			  
			;;; end if for continueing controller run
			)
		
			(setq listForMaintoMain (VANTAGE-REMOVENTH indexOfItemWiredTo listForMaintoMain))
			(setq listForMaintoMain (vantage-layout-block-info-list-recalulate-index listForMaintoMain))
			(setq listOrganizedMains (cons mainpanel listOrganizedMains))		
			
		)
				
		(setq layer (vla-item layers "_WS_V_CONTROLLER_BUS"))
		(vla-put-layeron layer :vlax-false)
		;turn layer on
		(setq layer (vla-item layers "_WS_V_POE"))
		(vla-put-layeron layer :vlax-false)
		
		(setq listOrganizedMains (reverse listOrganizedMains))
		(setq listOrganizedMains (vantage-layout-block-info-list-recalulate-index listOrganizedMains))
		;main to secondary loop
		(setq listForMaintoSecondary listOrganizedMains)


	  	;;; at this point, if the max flag is set
	  	;;; alert designer and let them choose what to do
	  	(if
		  	(= mainMaxDistanceFlag 1)
		  	(progn
			(setq message "Too Far")
			(vmain-mainMaxErrorHandler message)
			)
		)

		


		(setq listForSecondarytoSecondary secondarypanellist)
		(while
		  	(AND(/= listForMaintoSecondary nil))
			(setq layer (vla-item layers "_WS_V_CONTROLLER_BUS"))
			(vla-put-layeron layer :vlax-true)		  
			(setq layer (vla-item layers "_WS_V_ENCLOSURE_BUS"))
			(vla-put-layeron layer :vlax-true)

		  	(setq mainpanel (nth 0 listForMaintoSecondary))
			(setq indexOfItemWiredFrom (nth 4 mainpanel))
		  	(setq listForMaintoSecondary (VANTAGE-REMOVENTH indexOfItemWiredFrom listForMaintoSecondary))
		  	(setq listForMaintoSecondary (vantage-layout-block-info-list-recalulate-index listForMaintoSecondary))

			
		  
			(setq Tags (list "VMAIN" "VAUXILIARY"))
			(setq returnList (vmain-getAttributesAndValues mainpanel Tags))
			(setq Tags (nth 0 returnList))
			(setq Values (nth 1 returnList))
		  
		  	(setq secondarycntr 2)
		  	
		  	
		  	;wire this main to 3 secondaries
		  	(while
			  	(AND
				(<= secondarycntr 4)(/= listForSecondarytoSecondary nil)
				)
			  	(setq secondarySaftey 2400)
				(setq returnedList (vantange-find-closest  mainpanel listForSecondarytoSecondary secondarySaftey))
				
				(setq indexOfItemWiredFrom (nth 0 returnedList))
				(setq indexOfItemWiredTo (nth 1 returnedList))
				(setq pointList (nth 2 returnedList))
			  	(setq wireDistance (nth 3 returnedList))
				(setq secondarypanel (nth indexOfItemWiredTo listForSecondarytoSecondary))

			  	(setq secondarySaftey(- secondarySaftey wireDistance))
				(setq wireLayer "_WS_V_ENCLOSURE_BUS")
			  	(if
					(AND
				  	(/= pointList nil)(> secondarySaftey 0)
					)
				  	(progn
				  	(setq listForSecondarytoSecondary  (VANTAGE-REMOVENTH indexOfItemWiredTo listForSecondarytoSecondary))
					(setq listForSecondarytoSecondary (vantage-layout-block-info-list-recalulate-index listForSecondarytoSecondary))
					  
					(setq secondaryLetter "A")
					(setq secondaryString (strcat (itoa secondarycntr) secondaryLetter))
					(setq Tags (append Tags (list "VSECONDARY")))
					(setq Values (append Values (list secondaryString)))
					(vmain-setAttributesAndValues secondarypanel Tags Values)
					(VANTAGE-DRAW-LWPOLYLINE pointList 0 wireLayer 0 "BYLAYER" 256)

					;wire this secondary to one other secondary, remove from main to secondary list
					(if
					  	(and
						(/= secondarypanel nil)(/= listForSecondarytoSecondary nil)
						)
					  	(progn
						(setq returnedList (vantange-find-closest  secondarypanel listForSecondarytoSecondary secondarySaftey))
						(setq indexOfItemWiredFrom (nth 0 returnedList))
						(setq indexOfItemWiredTo (nth 1 returnedList))
						(setq pointList (nth 2 returnedList))
						(setq wireDistance (nth 3 returnedList))
						
						(setq secondarypanel (nth indexOfItemWiredTo listForSecondarytoSecondary ))
						(setq wireLayer "_WS_V_ENCLOSURE_BUS")
						(setq secondarySaftey(- secondarySaftey wireDistance))
					  	(if
						  	(AND
							(/= pointList nil)(> secondarySaftey 0)
							)
						  	(progn
						  	(setq listForSecondarytoSecondary  (VANTAGE-REMOVENTH indexOfItemWiredTo listForSecondarytoSecondary))
							(setq listForSecondarytoSecondary (vantage-layout-block-info-list-recalulate-index listForSecondarytoSecondary))
							(setq secondaryLetter "B")
							(setq secondaryString (strcat (itoa secondarycntr) secondaryLetter))
							(setq Tags (append Tags (list "VSECONDARY")))
							(setq Values (append Values (list secondaryString)))
							(vmain-setAttributesAndValues secondarypanel Tags Values)
							  
							(VANTAGE-DRAW-LWPOLYLINE pointList 0 wireLayer 0 "BYLAYER" 256)
							)
						  	(progn
							(setq secondaryMaxDistanceFlag 1)

							)
						)


						
						(VANTAGE-DRAW-LWPOLYLINE pointList 0 wireLayer 0 "BYLAYER" 256)


						)
					)
					
					)
				  	(progn
					(setq secondaryMaxDistanceFlag 1)
					)
				)


				  
			  	(setq secondarycntr (+ secondarycntr 1))
			)


		
		;(setq listForMaintoSecondary (reverse listForMaintoSecondary))
		;(setq listForMaintoSecondary (vantage-layout-block-info-list-recalulate-index listForMaintoSecondary))
		
		;-end while for MainToSecondary
		)
	  
		;- else there are no mains
	  	(progn
		(princ)
		;-progn wrap for no mains
		)
	;-if for Mains number
	)




	)
	 




	;;; at this point, if there are items left in the secondary list
	;;; alert designer and let them choose what to do
	(if
		(/= listForSecondarytoSecondary nil)
		(progn

	  	;;; if the max flag is set, then atleast 1 secondary was too far away
	  	;;; alert designer and let them choose what to do
	  	(if
		  	(= secondaryMaxDistanceFlag 1)
		  	(progn
			(setq message "Too Far")
			(vmain-secondaryMaxErrorHandler message)
			)
		  	;else max flag is not set, but there are still items left in the list, so too many
		  	(progn
			(setq message "Too Many")
			(vmain-secondaryMaxErrorHandler message)
			)
		)
		  

		)
	)
  
	;these connections happen regardless of number of main panels
  
  	(setq layer (vla-item layers "_WS_V_ENCLOSURE_BUS"))
	(vla-put-layeron layer :vlax-false)
	
	;main to Auxiliary loop
	(setq listForMaintoAuxiliary listOrganizedMains)
	(setq listForAuxiliary auxiliarypanellist)
  
	(setq AuxiliaryMaxDistanceFlag 0)


	(setq layer (vla-item layers "_WS_V_CONTROLLER_BUS"))
	(vla-put-layeron layer :vlax-true)				
	(setq layer (vla-item layers "_WS_V_AUXILIARY"))
	(vla-put-layeron layer :vlax-true)
  
	(while
	  	(AND (/= listForAuxiliary nil))
	  
	  
		(setq Auxiliarypanel(nth 0 listForAuxiliary))

	  
		(setq indexOfItemWiredFrom (nth 4 Auxiliarypanel))
	  	(setq listForAuxiliary (VANTAGE-REMOVENTH indexOfItemWiredFrom listForAuxiliary))
	  	(setq listForAuxiliary (vantage-layout-block-info-list-recalulate-index listForAuxiliary))
	  
	  	(setq Auxiliarycntr 0)
	  	;wire this aux to closest main
	  	(if
		  	(AND
			(/= listForMaintoAuxiliary nil)
			)
		  	(progn
		  	(setq AuxiliarySaftey 32000)
			(setq returnedList (vantange-find-closest Auxiliarypanel listForMaintoAuxiliary AuxiliarySaftey))
			
			(setq indexOfItemWiredFrom (nth 0 returnedList))
			(setq indexOfItemWiredTo (nth 1 returnedList))
			(setq pointList (nth 2 returnedList))
			(setq mainPanel (nth indexOfItemWiredTo listForMaintoAuxiliary))

			(setq wireLayer "_WS_V_AUXILIARY")

		  	(if
			  	(/= pointList nil)
			  	(progn
				(VANTAGE-DRAW-LWPOLYLINE pointList 0 wireLayer 0 "BYLAYER" 256)

				(setq Tags (list "VMAIN" "VSECONDARY" "VAUXILIARYQTY"))
				(setq returnList (vmain-getAttributesAndValues mainpanel Tags))
				(setq Tags (nth 0 returnList))
				(setq Values (nth 1 returnList))
				(setq mainValue (nth 0 Values))

				(setq secValue (nth 1 Values))

				(setq auxQTYValue (nth 2 Values))
				(setq auxQTYValue (atoi auxQTYValue))
				(setq auxQTYValue (+ auxQTYValue 1))
				(setq auxQTYValue (itoa auxQTYValue))
				(setq Values (list mainValue secValue auxQTYValue))
				
				(vmain-setAttributesAndValues mainpanel Tags Values)
				
				(setq Tags (list "VMAIN" "VSECONDARY" "VAUXILIARY"))	
				(vmain-setAttributesAndValues Auxiliarypanel Tags Values)

				
				)
			  	(progn
				(setq AuxiliaryMaxDistanceFlag 1)
				)
			)
		  	(setq Auxiliarycntr (+ Auxiliarycntr 1))
			)
		)
	
	)


  	;;; at this point, if the max flag is set, there is atleast 1 item Too Far
  	;;; alert designer and let them choose what to do
  	(if
	  	(= AuxiliaryMaxDistanceFlag 1)
	  	(progn
		(setq message "Too Far")
		(vmain-auxiliaryMaxErrorHandler message)
		)
	)
	
	(setq layer (vla-item layers "_WS_V_CONTROLLER_BUS"))
	(vla-put-layeron layer :vlax-true)		
	(setq layer (vla-item layers "_WS_V_ENCLOSURE_BUS"))
	(vla-put-layeron layer :vlax-true)		
	(setq layer (vla-item layers "_WS_V_AUXILIARY"))
	(vla-put-layeron layer :vlax-true)
  	(setq layer (vla-item layers "_WS_V_PANEL_COMPONENTS"))
	(vla-put-layeron layer :vlax-true)
  
	(setq cntr 0)
	(setq innerCounter 0)
	(setq cmax numberOfAllPanels)
  	(setq ComponentMaxDistanceFlag 0)
  	(setq ComponentMaxNumberFlag 0)
	;loop through all panels
	(while
	  	(< cntr cmax)
	  	(setq componentsToBoxIn nil)
	  	(setq panel (nth cntr listOfAllPanels))
	  	(setq previouscomponent panel)
	  	(setq maxNumberOfModules (nth 7 panel))
	  	(setq maxNumberOfLVOS (nth 8 panel))
	  	(setq processorType (nth 9 panel))
	  	(setq NumberOfModules 0)
	  	(setq NumberOfLVOS 0)
	  	(setq NumberOfprocessors 0)
	  	(setq componentsToBoxIn (cons panel componentsToBoxIn))
		;connect nearest panel components
		(while
		  	(/= previouscomponent nil)
		  	
		  	(setq panelComponentSaftey 80)
		  	
		  	(setq innerCounterMax (length listForComponentToPanel))
		  	(while
			  	;loop all the way through the panel components
			  	;end loop if we did not find something to connect to
			  	;(AND (< innerCounter innerCounterMax))
			  	(AND (< innerCounter innerCounterMax)(/= previouscomponent nil))

				(setq returnedList (vantange-find-closest previouscomponent listForComponentToPanel panelComponentSaftey))
				(setq indexOfItemWiredFrom (nth 0 returnedList))
				(setq indexOfItemWiredTo (nth 1 returnedList))
				(setq pointList (nth 2 returnedList))
			  	(setq wireDistance (nth 3 returnedList))
				(setq wireLayer "_WS_V_PANEL_COMPONENTS")

				(setq currentComponent (nth indexOfItemWiredTo listForComponentToPanel))
			  	(setq componentType (nth 7 currentComponent))

			  	(if
				  	(= componentType "module")
				  	(progn
					(setq NumberOfModules (+ NumberOfModules 1))
					)
				)

			  	(if
				  	(= componentType "lvos")
				  	(progn
					(setq NumberOfLVOS (+ NumberOfLVOS 1))
					)
				)
			  	(if
				  	(= componentType "processor")
				  	(progn
					(setq NumberOfprocessors (+ NumberOfprocessors 1))
					
					;;;if panel can't hold processor
					(if
					  	(= processorType "None")
					  	(progn
					  	(setq pointList nil)
						(setq ComponentMaxNumberFlag 1)
						)
					)
					)
				)			  

			  	;;;recalculate maxes for hybrids
			  	(if
				  	(= maxNumberOfLVOS 2)
				  	(progn
					;;; 2 lvos, no modules
					(if
					  	(= NumberOfLVOS 2)
					  	(progn
					  	(setq maxNumberOfModules 1)
						)
					)
					;;; 2 modules, no lvos
					(if
					  	(= NumberOfModules 2)
					  	(progn
					  	(setq maxNumberOfLVOS 0)
						)
					)
					)
				)
			  
			  	(if
				  	(> NumberOfModules maxNumberOfModules)
				  	(progn
				  	(setq pointList nil)
					(setq ComponentMaxNumberFlag 1)
					)
				)
			  	(if
				  	(> NumberOfLVOS maxNumberOfLVOS)
				  	(progn
				  	(setq pointList nil)
					(setq ComponentMaxNumberFlag 1)
					)
				)

			  	(if
				  	(> NumberOfprocessors 1)
				  	(progn
				  	(setq pointList nil)
					(setq ComponentMaxNumberFlag 1)
					)
				)
			  
			  	(if
				  	(/= pointList nil)
					(progn
					(VANTAGE-DRAW-LWPOLYLINE pointList 0 wireLayer 0 "BYLAYER" 256)
					(setq previouscomponent (nth indexOfItemWiredTo listForComponentToPanel))
				  	(setq listForComponentToPanel (VANTAGE-REMOVENTH indexOfItemWiredTo listForComponentToPanel))
					(setq listForComponentToPanel (vantage-layout-block-info-list-recalulate-index listForComponentToPanel))

					  
					(setq Tags (list "VMAIN" "VAUXILIARY" "VSECONDARY"))
					(setq returnList (vmain-getAttributesAndValues panel Tags))
					(setq Tags (nth 0 returnList))
					(setq Values (nth 1 returnList))
					
					(vmain-setAttributesAndValues previouscomponent Tags Values)
					
					(setq componentsToBoxIn (cons previouscomponent componentsToBoxIn))

					)
				)

				(if
				  	(> wireDistance panelComponentSaftey)
				  	(progn
					(setq ComponentMaxDistanceFlag 1)
					)
				)
				  
				
				(setq innerCounter (+ innerCounter 1))
			)
			(setq previouscomponent nil)


		)
	  	(setvar "clayer" "_WS_V_PANEL_COMPONENTS")
	  	(vmain-get-smallest-rectangle-around-set-list componentsToBoxIn)
	  	(setq innerCounter 0)
	  	(setq cntr (+ cntr 1))

	)

  
  	;;; at this point
  	;;; if there are any components left, they weren't close enough to the panels
  	;;; alert designer and let them choose what to do
  	(if
	  	(/= listForComponentToPanel nil)
	  	(progn
		(setq message "Too Far")
		(vmain-componentMaxErrorHandler message)


	  	;;; at this point, if the max flag is set, there is atleast 1 item Too Far
	  	;;; alert designer and let them choose what to do
	  	(if
		  	(= ComponentMaxNumberFlag 1)
		  	(progn
			(setq message "Too Many")
			(vmain-componentMaxErrorHandler message)
			)
		)
		)
	)
	
	(setq cntr 0)
	(setq cmax (length panelprocessorlist))		

  
	;hide panels and panel cable
	(setq layer (vla-item layers "_WS_V_CONTROLLER_BUS"))
	(vla-put-layeron layer :vlax-false)		
	(setq layer (vla-item layers "_WS_V_ENCLOSURE_BUS"))
	(vla-put-layeron layer :vlax-false)		
	(setq layer (vla-item layers "_WS_V_AUXILIARY"))
	(vla-put-layeron layer :vlax-false)
	(setq layer (vla-item layers "_WS_V_PANEL_COMPONENTS"))
	(vla-put-layeron layer :vlax-false)
	
	
	

  

	(vantage-layer-state-restorer)
  	(vantage-move-blocks-from-wire-layers-to-Wattstopper)
  	(setvar "CMDECHO" oldecho)
	(print "end")

)


(defun vantage-seperate-set (selectionSet / POLYLINES BLOCK BLOCKS CMAX CNTR DXF0 DXF8 ENT ENTDXF ENTTYPE LAYER TRUENAME)
	
  

	(cond
	  	((/= selectionSet nil)											;-conditional statement
	 	(setq cntr 0)												;set counter variable cntr to 0 before each loop
 		(setq cmax (sslength selectionSet))
		(while 	(< cntr cmax)											;--while loop, loop continues will counter cntr is less than length of set selection set
			(setq item(ssname selectionSet cntr))								;set bridge varaible to the member in set that corresponds to the current counter cntr variable

			(setq ent item)										;set ent to the entity stored in item

			(setq entDXF(entget ent))
		  	(setq dxf0 (cdr (assoc 0 entDXF )))

		  	(if
			  	(OR
				(= dxf0 "INSERT")
				)
			  	(progn
				(setq blocks(cons entDXF blocks))
				)
			)
		  	(if
			  	(OR
				(= dxf0 "LWPOLYLINE")
				)
			  	(progn
				(setq polylines(cons entDXF polylines))
				)
			)		  
			(setq cntr(+ cntr 1))  										;increment counter cntr    
		)													;--end while
		)													;-end conditional statement
	)														;-endconditional item
	
	(setq return (list blocks polylines))

)





; --- vmain-get-smallest-rectangle-around-set-list Function ---
; Accepts list of formated list of vantage blocks
; uses a inline function to create a bounding box around the first entity
; stores the data and continues to next entity
; at the end, the function finds the points that are the farthest apart and uses them for the rectangle
; Alex Lundin 12-16-2016
(defun vmain-get-smallest-rectangle-around-set-list ( VantageBlockList / LL MAXPT MINPT OLL OUR UR CMAX2 CNTR2 EN ITEM MAXPT MINPT OLL OUR)

  	;;; inline function to get bounding box for a object
	(defun obb (ent / ); = Object's Bounding Box corners 
	(vla-getboundingbox (vlax-ename->vla-object ent) 'minpt 'maxpt)
	(setq
	oLL (vlax-safearray->list minpt); Object's Lower Left
	oUR (vlax-safearray->list maxpt); Object's Upper Right
	); setq
	); defun -- obb
;;;(vl-load-com)
;;;(obb (ssname VantageBlockList 0))
;;;
;;;(ssdel (ssname VantageBlockList 0) VantageBlockList)

(setq cntr2 0)
(setq cmax2 (length VantageBlockList))

(while
	(< cntr2 cmax2)
	(setq item (nth cntr2 VantageBlockList))
	(setq en (nth 5 item))
  	(obb en)
  	(if
	  	(= cntr2 0)
	  	(progn
		(setq LL oLL UR oUR); initial overall LL & UR [of first object]
		)
	)
	(setq
	LL (mapcar 'min oLL LL); least of each component
	UR (mapcar 'max oUR UR); greatest of each component
	); setq	
	(setq cntr2(+ cntr2 1))

)


(if
  	(AND
	(/= LL nil)(/= UR nil)
	)
  	(progn
	(command "_.rectangle" "_none" LL "_none" UR)
	)
)
(princ)
)



(defun vmain-mainMaxErrorHandler ( inputList / CONTINUE VALIDCONTINUATIONCHOICE )

  	(if
	  	(= inputList "Too Far")
	  	(progn
	  	(alert
		(strcat
	  	"The mains selected are too far away from eachother"
		"\n"
		"\nClick OK."
		"\nType c to continue without wiring the mains that are too far away"
		"\nType e to exit"
		)
		)
		)
	)
  
  	(if
	  	(= inputList "Too Many")
	  	(progn
	  	(alert
		(strcat
	  	"There are too many controllers on this project"
		"\n"
		"\nClick OK."
		"\nType c to continue without wiring the mains that exceed the limit"
		"\nType e to exit"
		)
		)
		)
	)
  
	(setq validContinuationChoice 0)
	(while	(= validContinuationChoice 0)
		(setq continue (getstring "Type c to continue, type e to exit command."))
	  
	  	(if
			(or (= continue "e")(= continue "E"))
		  	(progn
			(exit)
			)
		  
		)
	  
	  	(if
			(or (= continue "c")(= continue "C"))
		  	(progn
			(setq validContinuationChoice 1)
			)
		  
		)
	  
		  
	)

)


(defun vmain-secondaryMaxErrorHandler ( inputList / CONTINUE VALIDCONTINUATIONCHOICE )
  	(if
	  	(= inputList "Too Many")
	  	(progn
	  	(alert
		(strcat
	  	"The secondaries selected exceed the maximum number of connections on the main"
		"\n"
		"\nClick OK."
		"\nType c to continue without wiring the secondaries that exceed the maximum number of connections"
		"\nType e to exit"
		)
		)
		)
	)

  	(if
	  	(= inputList "Too Far")
	  	(progn
	  	(alert
		(strcat
	  	"The secondaries selected are too far away from the main panel"
		"\n"
		"\nClick OK."
		"\nType c to continue without wiring the secondaries that are too far away"
		"\nType e to exit"
		)
		)
		)
	)
  

	(setq validContinuationChoice 0)
	(while	(= validContinuationChoice 0)
		(setq continue (getstring "Type c to continue without wiring secondaries, type e to exit command."))
	  
	  	(if
			(or (= continue "e")(= continue "E"))
		  	(progn
			(exit)
			)
		  
		)
	  
	  	(if
			(or (= continue "c")(= continue "C"))
		  	(progn
			(setq validContinuationChoice 1)
			)
		  
		)
	  
		  
	)

)



(defun vmain-auxiliaryMaxErrorHandler ( inputList / CONTINUE VALIDCONTINUATIONCHOICE )


  	(alert
	(strcat
  	"The auxiliaries selected are too far away from the main panel"
	"\n"
	"\nClick OK."
	"\nType c to continue without wiring the auxiliaries that are too far away"
	"\nType e to exit"
	)
	)
	(setq validContinuationChoice 0)
	(while	(= validContinuationChoice 0)
		(setq continue (getstring "Type c to continue without wiring auxiliaries that are too far, type e to exit command."))
	  
	  	(if
			(or (= continue "e")(= continue "E"))
		  	(progn
			(exit)
			)
		  
		)
	  
	  	(if
			(or (= continue "c")(= continue "C"))
		  	(progn
			(setq validContinuationChoice 1)
			)
		  
		)
	  
		  
	)

)


(defun vmain-componentMaxErrorHandler ( inputList / CONTINUE VALIDCONTINUATIONCHOICE )


  	(alert
	(strcat
  	"The components are too far away from the main panel"
	"\n"
	"\nClick OK."
	"\nType c to continue without wiring the components that are too far away"
	"\nType e to exit"
	)
	)
	(setq validContinuationChoice 0)
	(while	(= validContinuationChoice 0)
		(setq continue (getstring "Type c to continue without wiring the components, type e to exit command."))
	  
	  	(if
			(or (= continue "e")(= continue "E"))
		  	(progn
			(exit)
			)
		  
		)
	  
	  	(if
			(or (= continue "c")(= continue "C"))
		  	(progn
			(setq validContinuationChoice 1)
			)
		  
		)
	  
		  
	)

)


(defun vmain-poeSwitchMaxErrorHandler ( inputList / CONTINUE VALIDCONTINUATIONCHOICE )

  	(alert
	(strcat
  	"The POE switches selected are too far away from the controller"
	"\n"
	"\nClick OK."
	"\nType c to continue without wiring the POE switches that are too far away"
	"\nType e to exit"
	)
	)
	(setq validContinuationChoice 0)
	(while	(= validContinuationChoice 0)
		(setq continue (getstring "Type c to continue without wiring the POE switches that are too far away, type e to exit command."))
	  
	  	(if
			(or (= continue "e")(= continue "E"))
		  	(progn
			(exit)
			)
		  
		)
	  
	  	(if
			(or (= continue "c")(= continue "C"))
		  	(progn
			(setq validContinuationChoice 1)
			)
		  
		)
	  
		  
	)

)




(defun vmain-poeDeviceMaxErrorHandler ( inputList / CONTINUE VALIDCONTINUATIONCHOICE )

  	(if
	  	(= inputList "Too Many")
	  	(progn
	  	(alert
		(strcat
	  	"The POE devices exceed the maximum number of POE ports"
		"\n"
		"\nClick OK."
		"\nType c to continue without wiring the POE devices that exceed the maximum"
		"\nType e to exit"
		)
		)
		)
	)
	(setq validContinuationChoice 0)
	(while	(= validContinuationChoice 0)
		(setq continue (getstring "Type c to continue without wiring the POE devices, type e to exit command."))
	  
	  	(if
			(or (= continue "e")(= continue "E"))
		  	(progn
			(exit)
			)
		  
		)
	  
	  	(if
			(or (= continue "c")(= continue "C"))
		  	(progn
			(setq validContinuationChoice 1)
			)
		  
		)
	  
		  
	)

)




(defun vmain-RS232DeviceMaxErrorHandler ( inputList / CONTINUE VALIDCONTINUATIONCHOICE )

  	(if
	  	(= inputList "Too Many")
	  	(progn
	  	(alert
		(strcat
	  	"The RS232 devices exceed the maximum number of R2S232 ports"
		"\n"
		"\nClick OK."
		"\nType c to continue without wiring the RS232 devices that exceed the maximum"
		"\nType e to exit"
		)
		)
		)
	)
	(setq validContinuationChoice 0)
	(while	(= validContinuationChoice 0)
		(setq continue (getstring "Type c to continue without wiring the RS232 devices, type e to exit command."))
	  
	  	(if
			(or (= continue "e")(= continue "E"))
		  	(progn
			(exit)
			)
		  
		)
	  
	  	(if
			(or (= continue "c")(= continue "C"))
		  	(progn
			(setq validContinuationChoice 1)
			)
		  
		)
	  
		  
	)

)




(defun vmain-stationBusDeviceMaxErrorHandler ( inputList / CONTINUE VALIDCONTINUATIONCHOICE )
  	(if
	  	(= inputList "Too Far")
	  	(progn
	  	(alert
		(strcat
	  	"The Station Bus devices selected are too far away from the controller"
		"\n"
		"\nClick OK."
		"\nType c to continue without wiring the Station Bus devices that are too far away"
		"\nType e to exit"
		)
		)
		)
	)
  
  	(if
	  	(= inputList "Too Many")
	  	(progn
	  	(alert
		(strcat
	  	"The Station Bus devices exceed the number of devices per run"
		"\n"
		"\nClick OK."
		"\nType c to continue without wiring the Station Bus devices that maxed out the run"
		"\nType e to exit"
		)
		)
		)
	)
  

	(setq validContinuationChoice 0)
	(while	(= validContinuationChoice 0)
		(setq continue (getstring "Type c to continue without wiring the Station Bus devices, type e to exit command."))
	  
	  	(if
			(or (= continue "e")(= continue "E"))
		  	(progn
			(exit)
			)
		  
		)
	  
	  	(if
			(or (= continue "c")(= continue "C"))
		  	(progn
			(setq validContinuationChoice 1)
			)
		  
		)
	  
		  
	)

)




(defun vmain-318DeviceMaxErrorHandler ( inputList / CONTINUE VALIDCONTINUATIONCHOICE )

  	(alert
	(strcat
  	"The 318_AWG devices selected are too far away from any station bus device"
	"\n"
	"\nClick OK."
	"\nType c to continue without wiring the 318_AWG devices that are too far away"
	"\nType e to exit"
	)
	)
	(setq validContinuationChoice 0)
	(while	(= validContinuationChoice 0)
		(setq continue (getstring "Type c to continue without wiring the 318_AWG devices, type e to exit command."))
	  
	  	(if
			(or (= continue "e")(= continue "E"))
		  	(progn
			(exit)
			)
		  
		)
	  
	  	(if
			(or (= continue "c")(= continue "C"))
		  	(progn
			(setq validContinuationChoice 1)
			)
		  
		)
	  
		  
	)

)

(defun vantage-layer-state-restorer ( / ACADOBJ CMAX CNTR DOC LAYERNAME LAYEROBJINLOOP LAYERS VANTAGELAYERLIST)


    	(setvar "clayer" "WATTSTOPPER")	


  	(if
	  	(/= (layerstate-has "WS-Layout") nil)
	  	;if layerstate exists, restore and then delete so it is free for next run to overwrite
	  	(progn
	  	(layerstate-restore "WS-Layout")
		(layerstate-delete "WS-Layout")
		)
	)
	(setq vantageLayerList (list  "_WS_V_CONTROLLER_BUS" "_WS_V_ENCLOSURE_BUS" "_WS_V_AUXILIARY" "_WS_V_PANEL_COMPONENTS" "_WS_V_STATION_BUS" "_WS_V_3_18_AWG"
			       "_WS_V_RS232" "_WS_V_POE" "_WS_V_ORANGE_FOR_CONNECTIONS" 
			))
  

	(setq acadObj (vlax-get-acad-object))
	(setq doc (vla-get-ActiveDocument acadObj))

  	(setq layers (vla-get-layers doc))
  
  	(setq cntr 0)
  	(setq cmax (length vantageLayerList))
	(while
	  	(< cntr cmax)
		(setq layerName (nth cntr vantageLayerList))

	  	(setq layerObjInLoop (vla-item layers layerName))
	  	(vla-put-layeron layerObjInLoop :vlax-true)
	  	(vlax-release-object layerObjInLoop) 
		(setq cntr (+ cntr 1))
	)

)

(defun vantage-layer-state-setter ( / ACADOBJ DEFPOINTSLAYER DOC LAYERNAME LAYERS CLAYER CMAX CMAX2 CNTR CNTR2 ELIST ENT ENTDXF ENTLAYER EXPERT HIDELAYER LAYER LAYERLIST LISTADDER SHOWLAYER SS UNIQUELAYERLIST X)
  	(setq elist (ssget  	(prompt "\nPress esc now if you'd like to change your layer settings.
 					\nSelect objects on walls layer and other layers you would like shown during command:")))	;set elist to the selection and prompt user
 
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


  	;turn all layers off
	(vlax-for
		layer (vla-get-layers (vla-get-ActiveDocument (vlax-get-acad-object)))
		(setq layerName (vla-get-Name layer))
	  	(vla-put-layeron layer :vlax-false)
	)

  
	(setq listAdder (list "WATTSTOPPER" "_WS_V_CONTROLLER_BUS" "_WS_V_ENCLOSURE_BUS" "_WS_V_AUXILIARY" "_WS_V_PANEL_COMPONENTS" "_WS_V_STATION_BUS" "_WS_V_3_18_AWG"
			       "_WS_V_RS232" "_WS_V_POE" "_WS_V_ORANGE_FOR_CONNECTIONS"))											;set listAddder variable to "WATTSTOPPER"
	(setq layerList (append listAdder layerList))									;add listAdder to layerList
  
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
	(setq clayer (getvar "clayer") expert (getvar "expert"))							;get current layer
  	(setvar "expert" 1)												;set system variable


	(setq acadObj (vlax-get-acad-object))
	(setq doc (vla-get-ActiveDocument acadObj))

  	(setq layers (vla-get-layers doc))


  	;turn unique layers on
 	(setq cntr 0)
  	(setq cmax (length uniqueLayerList))
	(while
	  	(< cntr cmax)
		(setq layerName (nth cntr uniqueLayerList))
	  	(setq layer (vla-item layers layerName))
		(setq defPointslayer (vla-item layers "Defpoints"))

	  	;later expand to copy these objects to defpoints later
	  
	  	(vla-put-layeron layer :vlax-true)
	  	(setq cntr (+ cntr 1))
	)
  
	
  	(princ)
  
)



(defun vantage-move-block-to-layer (item layer / EN)
	(setq en (nth 5 item))
	(VANTAGE-ENTMOD-LAYER en layer)
  
)

(defun vantage-show-block-in-orange (item / EN LAYERTO)
	(setq en (nth 5 item))
  	(setq layerTo "_WS_V_ORANGE_FOR_CONNECTIONS")
	(VANTAGE-ENTMOD-LAYER en layerTo)
  
)

(defun vantage-delete-existing-cable ( POLYLINES /  BLOCK BLOCKS CMAX CNTR DXF0 DXF8 ENT ENTDXF ENTTYPE LAYER TRUENAME)
	

	(cond
	  	((/= polylines nil)											;-conditional statement
	 	(setq cntr 0)												;set counter variable cntr to 0 before each loop
 		(setq cmax (length polylines))
		(while 	(< cntr cmax)											;--while loop, loop continues will counter cntr is less than length of polylines selection set
			(setq entDXF(nth cntr polylines))								;set bridge varaible to the member in polylines that corresponds to the current counter cntr variable

			(setq ent (cdr (assoc -1 entDXF )))								;set ent to the entity stored in block

			(setq entDXF(entget ent))
		  	(setq dxf0 (cdr (assoc 0 entDXF )))
		  	(setq dxf8 (cdr (assoc 8 entDXF )))
			(setq entType dxf0)
		  	(setq layer dxf8)

		  	(if
			  	(OR
				(= layer "_WS_V_CONTROLLER_BUS")
				(= layer "_WS_V_ENCLOSURE_BUS")
				(= layer "_WS_V_AUXILIARY")
				(= layer "_WS_V_PANEL_COMPONENTS")
				(= layer "_WS_V_STATION_BUS")
				(= layer "_WS_V_POE")
				(= layer "_WS_V_3_18_AWG")
				(= layer "_WS_V_RS232")
				)
			  	(progn
				(if
				  	(= entType "LWPOLYLINE")
				  	(progn
					(entdel ent)
					)
				)
				)
			)
		  
			(setq cntr(+ cntr 1))  										;increment counter cntr    
		)													;--end while
		)													;-end conditional statement
	)														;-endconditional block

)





(defun vantage-move-blocks-from-wire-layers-to-Wattstopper
       		(
		/
		DXF8 ENTDXF LAYERTO
		BLOCK ENT LAYER TRUENAME BLOCKS CMAX CNTR LISTFORCOMPONENTTOPANEL LISTFORMAINTOSECONDARY LISTFORSECONDARYTOSECONDARY LISTOFALLPANELS
		NUMBEROFALLPANELS PANEL PANELCOMPONENTSAFTEY POINTLIST PREVIOUSCOMPONENT SECONDARYCNTR SECONDARYPANEL SECONDARYSAFTEY USERMAINSET USERSECONDARYSET)
  
  	;;;move product blocks to correct layers
	(setq blocks(ssget "x" '((0 . "INSERT"))))									;select all blocks

	(cond
	  	((/= blocks nil)											;-conditional statement
	 	(setq cntr 0)												;set counter variable cntr to 0 before each loop
 		(setq cmax (sslength blocks))
		(while 	(< cntr cmax)											;--while loop, loop continues will counter cntr is less than length of blocks selection set
			(setq block(ssname blocks cntr))								;set bridge varaible to the member in blocks that corresponds to the current counter cntr variable

			(setq ent block)										;set ent to the entity stored in block
			(setq truename (VANTAGE-BLOCKTRUENAME ent))								;call VANTAGE-BLOCKTRUENAME function with the ent parameter set in the LWPOLYLINE above, set the truename variable to the final result

			(setq entDXF(entget ent))
		  	(setq dxf8 (cdr (assoc 8 entDXF )))

		  	(setq layer dxf8)

		  	(if
			  	(OR
				(= layer "_WS_V_CONTROLLER_BUS")
				(= layer "_WS_V_ENCLOSURE_BUS")
				(= layer "_WS_V_AUXILIARY")
				(= layer "_WS_V_PANEL_COMPONENTS")
				(= layer "_WS_V_STATION_BUS")
				(= layer "_WS_V_POE")
				(= layer "_WS_V_3_18_AWG")
				(= layer "_WS_V_RS232")
				(= layer "_WS_V_ORANGE_FOR_CONNECTIONS")
				)
			  	(progn
				(setq layerTo "WATTSTOPPER")
				(VANTAGE-ENTMOD-LAYER ent layerTo)
				)
			)
		  
			(setq cntr(+ cntr 1))  										;increment counter cntr    
		)													;--end while
		)													;-end conditional statement
	)														;-endconditional block
)





(defun vantage-move-blocks-to-wire-layers
       		(
		/
		BLOCK ENT LAYER TRUENAME BLOCKS CMAX CNTR LISTFORCOMPONENTTOPANEL LISTFORMAINTOSECONDARY LISTFORSECONDARYTOSECONDARY LISTOFALLPANELS
		NUMBEROFALLPANELS PANEL PANELCOMPONENTSAFTEY POINTLIST PREVIOUSCOMPONENT SECONDARYCNTR SECONDARYPANEL SECONDARYSAFTEY USERMAINSET USERSECONDARYSET)
  
  	;;;move product blocks to correct layers
	(setq blocks(ssget "x" '((0 . "INSERT"))))									;select all blocks

	(cond
	  	((/= blocks nil)											;-conditional statement
	 	(setq cntr 0)												;set counter variable cntr to 0 before each loop
 		(setq cmax (sslength blocks))
		(while 	(< cntr cmax)											;--while loop, loop continues will counter cntr is less than length of blocks selection set
			(setq block(ssname blocks cntr))								;set bridge varaible to the member in blocks that corresponds to the current counter cntr variable

			(setq ent block)										;set ent to the entity stored in block
			(setq truename (VANTAGE-BLOCKTRUENAME ent))								;call VANTAGE-BLOCKTRUENAME function with the ent parameter set in the LWPOLYLINE above, set the truename variable to the final result

			(if												;-inner if
			  	(OR											;-- or inside the if

				(= truename "V-LCAP44H")
				(= truename "V-LCAP44M")
				(= truename "V-LCAP32M")
				)											;-- end or
			  	(progn											;--progn wrap the if
			 	(setq layer "_WS_V_CONTROLLER_BUS")							;set layer to correct layer
			  	(setq ent block)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(VANTAGE-ENTMOD-LAYER ent layer)							;call VANTAGE-ENTMOD-LAYER on ent and layer arugments, no return value needed
				)											;--end progn wrap
			)												;-end inner if
			(if												;-inner if
			  	(OR											;-- or inside the if
				(= truename "V-LCAP44HS")
				(= truename "V-LCAP32S")
				(= truename "V-LCAP44S")
				)											;-- end or
			  	(progn											;--progn wrap the if
			 	(setq layer "_WS_V_ENCLOSURE_BUS")							;set layer to correct layer
			  	(setq ent block)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(VANTAGE-ENTMOD-LAYER ent layer)							;call VANTAGE-ENTMOD-LAYER on ent and layer arugments, no return value needed
				)											;--end progn wrap
			)												;-end inner if
		  	(if												;-inner if
			  	(OR											;-- or inside the if
				(= truename "V-LCAP44A")
				(= truename "V-LCAP44L")
				)											;-- end or
			  	(progn											;--progn wrap the if
			 	(setq layer "_WS_V_AUXILIARY")								;set layer to correct layer
			  	(setq ent block)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(VANTAGE-ENTMOD-LAYER ent layer)							;call VANTAGE-ENTMOD-LAYER on ent and layer arugments, no return value needed
				)											;--end progn wrap
			)
			(if												;-inner if
			  	(OR											;-- or inside the if
				(= truename "V-STPSRW101")
				(= truename "V-STPSRW201")
				(= truename "V-STPERW101")
				(= truename "V-STPERW201")
				(= truename "V-FANMOD")
				(= truename "V-MDR8CW301")
				(= truename "V-SDM12-EM")
				(= truename "V-UDM08-EM")
				(= truename "V-LVOS-0-10-PWM-P-1")
				(= truename "V-IC-36")
				(= truename "V-IC-DIN-II-LITE-RF")
				(= truename "V-IC-DIN-II-LITE")
				)											;-- end or
			  	(progn											;--progn wrap the if
			 	(setq layer "_WS_V_PANEL_COMPONENTS")							;set layer to correct layer
			  	(setq ent block)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(VANTAGE-ENTMOD-LAYER ent layer)							;call VANTAGE-ENTMOD-LAYER on ent and layer arugments, no return value needed
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
				(= truename "V-LVOS-0-10-PWM-1")
				)											;-- end or
			  	(progn											;--progn wrap the if
			 	(setq layer "_WS_V_STATION_BUS")							;set layer to correct layer
			  	(setq ent block)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(VANTAGE-ENTMOD-LAYER ent layer)							;call VANTAGE-ENTMOD-LAYER on ent and layer arugments, no return value needed
				)											;--end progn wrap
			)												;-end inner if
			(if												;-inner if
			  	(OR											;-- or inside the if
				(= truename "COM-POE-SWITCH")
				(= truename "V-EQ73TB-TI")
				(= truename "V-EQ41TB-TI")
				)											;-- end or
			  	(progn											;--progn wrap the if
			 	(setq layer "_WS_V_POE")								;set layer to correct layer
			  	(setq ent block)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(VANTAGE-ENTMOD-LAYER ent layer)							;call VANTAGE-ENTMOD-LAYER on ent and layer arugments, no return value needed
				)											;--end progn wrap
			)
			(if												;-inner if
			  	(OR											;-- or inside the if
				(= truename "DT-300")
				)											;-- end or
			  	(progn											;--progn wrap the if
			 	(setq layer "_WS_V_3_18_AWG")								;set layer to correct layer
			  	(setq ent block)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(VANTAGE-ENTMOD-LAYER ent layer)							;call VANTAGE-ENTMOD-LAYER on ent and layer arugments, no return value needed
				)											;--end progn wrap
			)												;-end inner if
			(if												;-inner if
			  	(OR											;-- or inside the if
				(= truename "LMDI-100")
				)											;-- end or
			  	(progn											;--progn wrap the if
			 	(setq layer "_WS_V_RS232")								;set layer to correct layer
			  	(setq ent block)									;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
				(VANTAGE-ENTMOD-LAYER ent layer)							;call VANTAGE-ENTMOD-LAYER on ent and layer arugments, no return value needed
				)											;--end progn wrap
			)												;-end inner if
		  
			(setq cntr(+ cntr 1))  										;increment counter cntr    
		)													;--end while
		)													;-end conditional statement
	)														;-endconditional block
)





;;; --- vantage-sort-by-distance ---
;;; Arguments
;;; inputlist
;;; Return
;;; returnlist  - list sorted by 2 element then 3rd element, which are x and then y
;;;		- also recalculate indexes
;;; Alex Lundin 06-28-2017
(defun vantage-sort-by-distance (inputlist / DISTANCELIST NEWLYINDEXEDLIST RETURNLIST)
	(setq distancelist (vl-sort inputlist (function (lambda (x y)(< (nth 2 x)(nth 2 y))))))	;sort according to nth 4th element of the item, which is distance. Low to high
  	(setq distancelist (vl-sort inputlist (function (lambda (x y)(< (nth 3 x)(nth 3 y))))))	;sort according to nth 4th element of the item, which is distance. Low to high
  	(setq newlyIndexedList (vantage-layout-block-info-list-recalulate-index distancelist))
  
	(setq returnlist newlyIndexedList)
)


;;; --- vantange-find-closest ---
;;; Arguments
;;; item for connection
;;; innerlooplist to search for closest component
;;; Return
;;; index of item from the source list
;;; index of item2 from innerlooplist, if needed on return
;;; point list to connect items
;;; none
;;; Alex Lundin 06-28-2017
(defun	vantange-find-closest (item innerlooplist safteyfactor

				    /
			       DISTANCEOFFIRSTPOINTS DISTANCEOFSECONDPOINTS
				    POINT3 POINTLIST distanceOfWireToDraw
				    ATTTAG ATTVAL CMAX2 CNTR2 COPYIEDROOMDID DIST DISTANCELIST EN EN2 EN3 ENDXF ENDXF2 ENSUB ENT GROUP0 HANDLE HANDLE2 HANDLE3 ITEM2 ITEM3 NUMBER NUMBER2 NUMBER3 POINT1 POINT2 RETURNLIST SUBLIST TRUENAME TRUENAME2 TRUENAME3 XCOORD XCOORD2 XCOORD3 YCOORD YCOORD2 YCOORD3)

	
	(setq truename (nth 0 item))									;set truename to 1st element of item
	(setq handle (nth 1 item))									;set handle to 2nd element of item
	(setq xcoord (nth 2 item))									;set xcoord to 3rd element of item
	(setq ycoord (nth 3 item))									;set ycoord to 4th element of item
	(setq number (nth 4 item))									;set number to 5th element of item
	(setq en (nth 5 item))

	(setq cntr2 0)
	(setq cmax2 (length innerlooplist))								;set cmax2 to length of blocks selection set

	;;; this loop keeps outerlooplist in the same order while it's looping through
	;;; it also adds the current counter value to each item
	;;; this is the cntr2 value and it serves as the index for the list
	;;; after the list is completed, the next line sorts the list by distance
	;;; thats okay though because the orignal indexs are still stored on each item
	;;; these indexs on innerloop can be used to remove the same item from outerloop
		(while 	(< cntr2 cmax2)									;--while, set loop to continue while the counter varaible cntr2 is less that the number of objects in the innerlooplist
			(setq item2(nth cntr2 innerlooplist))						;set item to the nth member of innerlooplist that corresponds to the cntr2 variable
			(setq truename2 (nth 0 item2))							;set truename to 1st element of item
			(setq handle2 (nth 1 item2))							;set handle to 2nd element of item
			(setq xcoord2 (nth 2 item2))							;set xcoord to 3rd element of item
			(setq ycoord2 (nth 3 item2))							;set ycoord to 4th element of item
			(setq number2 (nth 4 item2))							;set number to 5th element of item
			(setq en2 (nth 5 item2))
		  
		  	;;; the section calculates the distances
		  	;;; greedy decision algorithm
		  	;;; use last block as reference, changes each time
		  	;;; set the first point to the xcoord and ycoord, they correspond to whatever the previous object was
			(setq point1 (list xcoord ycoord))
		  

		  
			(setq point2 (list xcoord2 ycoord2))						;set the second point to the current object in the inner loop x and y coord
		  	(setq dist (distance point1 point2))						;calculate distance
		  	(setq sublist (list truename2 handle2 xcoord2 ycoord2 dist cntr2 en2))		;create sublist for current item, I did not put the number2 back on because it is not useful at this point anymore. instead I put cntr2, which gives the index in the list, this will allow me to remove that item from the list later
		  	(setq distancelist (cons sublist distancelist))					;add sublist to innerlooplist
			(setq cntr2(+ cntr2 1))								;once while loop reaches this point, increment counter varaible by one			
		)											;--end while
	(setq distancelist (vl-sort distancelist (function (lambda (x y)(< (nth 4 x)(nth 4 y))))))	;sort according to nth 4th element of the item, which is distance. Low to high

	(setq item3(nth 0 distancelist))								;set item3 to the nth member of innerlooplist at 0, which is the first item, which is the shortest distance
	(setq truename3 (nth 0 item3))									;set truename3 to 1st element of item
	(setq handle3 (nth 1 item3))									;set handle3 to 2nd element of item
	(setq xcoord3 (nth 2 item3))									;set xcoord3 to 3rd element of item
	(setq ycoord3 (nth 3 item3))									;set ycoord3 to 4th element of item
	(setq dist (nth 4 item3))									;set dist to 5th element of item
	(setq number3 (nth 5 item3))									;set number3 to 6th element of item
	(setq en3 (nth 6 item3))

	(setq point1 (list xcoord ycoord))
	(setq point2 (list xcoord ycoord3))
	(setq point3 (list xcoord3 ycoord3))
	(setq pointList (list point1 point2 point3))

	(setq distanceOfFirstPoints (distance point1 point2))
  	(setq distanceOfSecondPoints (distance point2 point3))
	(setq distanceOfWireToDraw (+ distanceOfFirstPoints distanceOfSecondPoints))
  
	(if
	  	(<= distanceOfWireToDraw safteyfactor)
	  	(progn
		;set return when wire meets safteyfactor
		(setq pointList pointList)
		
		)
	  	(progn
		;set return when wire does not meet safteyfactor
		(setq pointList nil)
		)
	)


	(setq distancelist nil)										;nil distancelist


	(setq returnlist (list number number3 pointList distanceOfWireToDraw))

)


;;; --- vantange-find-farthest ---
;;; Arguments
;;; item for connection
;;; innerlooplist to search for closest component
;;; Return
;;; index of item from the source list
;;; index of item2 from innerlooplist, if needed on return
;;; point list to connect items
;;; none
;;; Alex Lundin 06-28-2017
(defun	vantange-find-farthest (item innerlooplist safteyfactor

				    /
			       DISTANCEOFFIRSTPOINTS DISTANCEOFSECONDPOINTS
				    POINT3 POINTLIST distanceOfWireToDraw
				    ATTTAG ATTVAL CMAX2 CNTR2 COPYIEDROOMDID DIST DISTANCELIST EN EN2 EN3 ENDXF ENDXF2 ENSUB ENT GROUP0 HANDLE HANDLE2 HANDLE3 ITEM2 ITEM3 NUMBER NUMBER2 NUMBER3 POINT1 POINT2 RETURNLIST SUBLIST TRUENAME TRUENAME2 TRUENAME3 XCOORD XCOORD2 XCOORD3 YCOORD YCOORD2 YCOORD3)

	
	(setq truename (nth 0 item))									;set truename to 1st element of item
	(setq handle (nth 1 item))									;set handle to 2nd element of item
	(setq xcoord (nth 2 item))									;set xcoord to 3rd element of item
	(setq ycoord (nth 3 item))									;set ycoord to 4th element of item
	(setq number (nth 4 item))									;set number to 5th element of item
	(setq en (nth 5 item))

	(setq cntr2 0)
	(setq cmax2 (length innerlooplist))								;set cmax2 to length of blocks selection set

	;;; this loop keeps outerlooplist in the same order while it's looping through
	;;; it also adds the current counter value to each item
	;;; this is the cntr2 value and it serves as the index for the list
	;;; after the list is completed, the next line sorts the list by distance
	;;; thats okay though because the orignal indexs are still stored on each item
	;;; these indexs on innerloop can be used to remove the same item from outerloop
		(while 	(< cntr2 cmax2)									;--while, set loop to continue while the counter varaible cntr2 is less that the number of objects in the innerlooplist
			(setq item2(nth cntr2 innerlooplist))						;set item to the nth member of innerlooplist that corresponds to the cntr2 variable
			(setq truename2 (nth 0 item2))							;set truename to 1st element of item
			(setq handle2 (nth 1 item2))							;set handle to 2nd element of item
			(setq xcoord2 (nth 2 item2))							;set xcoord to 3rd element of item
			(setq ycoord2 (nth 3 item2))							;set ycoord to 4th element of item
			(setq number2 (nth 4 item2))							;set number to 5th element of item
			(setq en2 (nth 5 item2))
		  
		  	;;; the section calculates the distances
		  	;;; greedy decision algorithm
		  	;;; use last block as reference, changes each time
		  	;;; set the first point to the xcoord and ycoord, they correspond to whatever the previous object was
			(setq point1 (list xcoord ycoord))
		  

		  
			(setq point2 (list xcoord2 ycoord2))						;set the second point to the current object in the inner loop x and y coord
		  	(setq dist (distance point1 point2))						;calculate distance
		  	(setq sublist (list truename2 handle2 xcoord2 ycoord2 dist cntr2 en2))		;create sublist for current item, I did not put the number2 back on because it is not useful at this point anymore. instead I put cntr2, which gives the index in the list, this will allow me to remove that item from the list later
		  	(setq distancelist (cons sublist distancelist))					;add sublist to innerlooplist
			(setq cntr2(+ cntr2 1))								;once while loop reaches this point, increment counter varaible by one			
		)											;--end while
	(setq distancelist (vl-sort distancelist (function (lambda (x y)(> (nth 4 x)(nth 4 y))))))	;sort according to nth 4th element of the item, which is distance. Low to high


	(setq pointList nil)

	;loop to see if anything else meets safteyfactor
	(setq cntr3 0)
	(setq cmax3 (length distancelist))
	(while
		(< cntr3 cmax3)
	  	(setq distanceOfWireToDraw nil)
		(setq item3(nth cntr3 distancelist))								;set item3 to the nth member of innerlooplist at 0, which is the first item, which is the shortest distance
		(setq truename3 (nth 0 item3))									;set truename3 to 1st element of item
		(setq handle3 (nth 1 item3))									;set handle3 to 2nd element of item
		(setq xcoord3 (nth 2 item3))									;set xcoord3 to 3rd element of item
		(setq ycoord3 (nth 3 item3))									;set ycoord3 to 4th element of item
		(setq dist (nth 4 item3))									;set dist to 5th element of item
		(setq number3 (nth 5 item3))									;set number3 to 6th element of item
		(setq en3 (nth 6 item3))
		(setq point1 (list xcoord ycoord))
		(setq point2 (list xcoord ycoord3))
		(setq point3 (list xcoord3 ycoord3))
		(setq pointList (list point1 point2 point3))
		(setq distanceOfFirstPoints (distance point1 point2))
	  	(setq distanceOfSecondPoints (distance point2 point3))
		(setq distanceOfWireToDraw (+ distanceOfFirstPoints distanceOfSecondPoints))

		(if
		  	(<= distanceOfWireToDraw safteyfactor)
		  	(progn
			;set return when wire meets safteyfactor
			(setq pointList pointList)
			(setq cntr3 cmax3)
			)
		)
	  
	  	(setq cntr3 (+ cntr3 1))
	)



  



	(setq distancelist nil)										;nil distancelist


	(setq returnlist (list number number3 pointList distanceOfWireToDraw))

)

;;; --- VANTAGE-DRAW-LWPOLYLINE---
;;; Notes:
;;; class-holder must be 0 for lwpolyline dxfcodes
;;; Alex Lundin 06-14-2017
(defun VANTAGE-DRAW-LWPOLYLINE (point-list class-holder polyline-layer polyline-width polyline-linetype polyline-color / enamepolyline RETURN)
;;; arguments
;;;	point-list			
;;;	class-holder	must be 0	
;;; 	polyline-layer			
;;;	polyline-width			
;;;	polyline-linetype		
  	(setq enamepolyline
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
			(cons 62 polyline-color)
                       	(cons 90 (length point-list))
                      	(cons 70 class-holder))
       			(mapcar (function (lambda (p) (cons 10 p))) point-list))
	)
	)
  	(setq return (list enamepolyline))
)




  
;;; --- VANTAGE-REMOVENTH ---
;;; Arguments
;;; n - index to remove
;;; l - list to remove from
;;; Return
;;; none
;;; Alex Lundin 06-28-2017
(defun VANTAGE-REMOVENTH
       		( n l / i )
    	(setq i -1)
    	(vl-remove-if '(lambda ( x ) (= (setq i (1+ i)) n)) l)
)




;;; --- vantage-layout-block-info-list-recalulate-index Sub Function ---
;;; Arguments
;;; none
;;; Alex Lundin 06-28-2017
(defun vantage-layout-block-info-list-recalulate-index (
					inputlist
				      /
					318LIST AUXILIARYPANELLIST BLOCKS CNTR CNTR318 CNTRAUXILIARY CNTRCOMPONENT CNTRMAIN CNTRPOE CNTRRS232 CNTRSECONDARY CNTRSTATIONBUS DXF10 DXF5 EN ENTDXF HANDLE MAINPANELLIST PANELCOMPONENTLIST POELIST RETURNLIST RS232LIST SECONDARYPANELLIST STATIONBUSLIST SUBLIST TRUENAME XCOORD YCOORD
					NEWLIST NUMBER  item location
				       )

	(setq cntr 0)													;initialize counter to 0 for while loop

  		(cond													;-conditional block
		  	((/= inputlist nil)										;-when blocks are not nil
			(while 	(< cntr (length inputlist))								;set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks

			(setq item (nth cntr inputlist))
		
			(setq truename(nth 0 item))									;set variable en to the entity name of the block that matches the current counter value
		  	(setq handle (nth 1 item))									;set handle to the second element of dxf5
		  	(setq xcoord (nth 2 item ))									;set xcoord to the nth 1 of dxf10
		  	(setq ycoord (nth 3 item ))									;set ycoord to the nth 2 of dxf10
			(setq en (nth 5 item))
			(setq location (nth 6 item))

			  	(setq item7 (nth 7 item))
				(setq item8 (nth 8 item))
				(setq item9 (nth 9 item))

			(setq sublist (list truename handle xcoord ycoord cntr en location item7 item8 item9))				;create sublist for current item	
			(setq newlist (cons sublist newlist))

			  
			(setq cntr(+ cntr 1))										;once while loop reaches this point, increment counter varaible by one			
	  		)												;end while loop
			)												;-end when blocks not nil conditional
		)
	
	(setq newlist (reverse newlist))

	(setq returnlist newlist)

)

;;; --- vantage-layout-block-info-list Sub Function ---
;;; Arguments
;;; none
;;; Alex Lundin 06-28-2017
(defun vantage-layout-block-info-list (
				      blocks
				      /
				      CNTRPOEDEVICES CNTRPOESWITCHES MAXCABLEBETWEENDEVICES MAXCABLERUN MAXSTATIONBUSCABLEBETWEENDEVICES MAXSTATIONBUSCABLERUN NUMBEROFBUSRUNS NUMBEROFEQMAX NUMBEROFETHERNETPORTS NUMBEROFPOEPORTS NUMBEROFRS232PORTS NUMBEROFRS485PORTS NUMBEROFSTATIONSMAX POEDEVICELIST POESWITCHLIST
					318LIST AUXILIARYPANELLIST CNTR CNTR318 CNTRAUXILIARY CNTRCOMPONENT CNTRMAIN CNTRPOE CNTRRS232 CNTRSECONDARY CNTRSTATIONBUS DXF10 DXF5 EN ENTDXF HANDLE MAINPANELLIST PANELCOMPONENTLIST POELIST RETURNLIST RS232LIST SECONDARYPANELLIST STATIONBUSLIST SUBLIST TRUENAME XCOORD YCOORD
					NEWLIST NUMBER X CNTRPANELPROCESSOR PANELPROCESSORLIST location
				       )


	(setq cntr 0)													;initialize counter to 0 for while loop
  	(setq cntrMain 0)
  	(setq cntrSecondary 0)
  	(setq cntrAuxiliary 0)
  	(setq cntrComponent 0)
  	(setq cntrStationBus 0)
  	(setq cntrPoeDevices 0)
  	(setq cntrPoeSwitches 0)
  	(setq cntr318 0)
  	(setq cntrRs232 0)
  	(setq cntrPanelProcessor 0)
  	;;; this loop seperates all blocks into two selection sets
  	;;; ss is emtpy ROOMID blocks
  	;;; ss1 is full ROOMID blocks
  		(cond													;-conditional block
		  	((/= blocks nil)										;-when blocks are not nil
			(while 	(< cntr (length blocks))								;set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks

			(setq entdxf(nth cntr blocks))									;set variable en to the entity name of the block that matches the current counter value
			(setq dxf-1(assoc -1 entdxf))
			(setq en (cdr dxf-1))  
			(setq truename (VANTAGE-BLOCKTRUENAME en))							;call VANTAGE-BLOCKTRUENAME function

			(setq dxf5 (assoc 5 entdxf))									;set dxf5 to the 5th dxf code of entdxf
		  	(setq handle (cdr dxf5))									;set handle to the second element of dxf5
		  	(setq dxf10 (assoc 10 entdxf))									;set dxf10 to the 10th dxf code of entdxf
		  	(setq xcoord (nth 1 dxf10))									;set xcoord to the nth 1 of dxf10
		  	(setq ycoord (nth 2 dxf10))									;set ycoord to the nth 2 of dxf10
			(setq location (list xcoord ycoord))

			(if
			  	(OR
				;44
				
				(= truename "V-LCAP44M")
				(= truename "V-LCAP44S")
				)
			  	(progn
				;4 modules
				(setq maxNumberOfModules 4)
				(setq maxNumberOfLVOS 0)
				)
			)

			(if
			  	(OR
				;32
				(= truename "V-LCAP32M")
				(= truename "V-LCAP32S")
				
				)
				(progn
				;2 modules
				(setq maxNumberOfModules 2)
				(setq maxNumberOfLVOS 0)
			  	)
			)

			
			(if
			  	(OR
				;hybrid 44
				(= truename "V-LCAP44H")
				(= truename "V-LCAP44HS")
				)
				(progn
				;2 lvos
				;2 mods, if no lvos
				(setq maxNumberOfModules 2)
				(setq maxNumberOfLVOS 2)
				)
			)			
			
			(if
			  	(OR
				;aux 44
				(= truename "V-LCAP44A")
				)
				(progn
				;6 lvos
				(setq maxNumberOfModules 0)
				(setq maxNumberOfLVOS 6)
				)
			)
			  
			(if
			  	(OR
				;mains
				(= truename "V-LCAP44H")
				(= truename "V-LCAP44M")
				(= truename "V-LCAP32M")
				)
			  	(progn
				(setq processorType "V-IC-36")
				(setq sublist (list truename handle xcoord ycoord cntrMain en location maxNumberOfModules maxNumberOfLVOS processorType))		;create sublist for current item
				(setq mainpanellist (cons sublist mainpanellist))					;add sublist to mainlist, which accumulates during the loop
				(setq cntrMain (+ cntrMain 1))
				)
			)

			(if
			  	(OR
				;secondaries
				(= truename "V-LCAP44HS")
				(= truename "V-LCAP32S")
				(= truename "V-LCAP44S")
				)
				(progn
				(setq processorType "Secondary Card")
				(setq sublist (list truename handle xcoord ycoord cntrSecondary en location maxNumberOfModules maxNumberOfLVOS processorType))		;create sublist for current item
				(setq secondarypanellist (cons sublist secondarypanellist))				;add sublist to mainlist, which accumulates during the loop
				(setq cntrSecondary (+ cntrSecondary 1))
			  	)
			)

			(if
			  	(OR
				;aux
				(= truename "V-LCAP44A")
				)
				(progn
				(setq processorType "None")
				(setq sublist (list truename handle xcoord ycoord cntrAuxiliary en location maxNumberOfModules maxNumberOfLVOS processorType))		;create sublist for current item
				(setq auxiliarypanellist (cons sublist auxiliarypanellist))				;add sublist to mainlist, which accumulates during the loop
				(setq cntrAuxiliary (+ cntrAuxiliary 1))
				)
			)
			(if
			  	(OR
				;modules
				(= truename "V-STPSRW101")
				(= truename "V-STPSRW201")
				(= truename "V-STPERW101")
				(= truename "V-STPERW201")
				(= truename "V-FANMOD")
				(= truename "V-MDR8CW301")
				(= truename "V-SDM12-EM")
				(= truename "V-UDM08-EM")
				)
			  	(progn
				(setq componentType "module")
				)
			)
			(if
			  	(OR
				;lvos
				(= truename "V-LVOS-0-10-PWM-P-1")
				)
			  	(progn
				(setq componentType "lvos")
				)
			)
			(if
			  	(OR
				;processor
				(= truename "V-IC-36")
				(= truename "V-IC-DIN-II-LITE-RF")
				(= truename "V-IC-DIN-II-LITE")
				)
			  	(progn
				(setq componentType "processor")
				)
			)			  
			(if
			  	(OR
				;panel components
				(= truename "V-STPSRW101")
				(= truename "V-STPSRW201")
				(= truename "V-STPERW101")
				(= truename "V-STPERW201")
				(= truename "V-FANMOD")
				(= truename "V-MDR8CW301")
				(= truename "V-SDM12-EM")
				(= truename "V-UDM08-EM")
				(= truename "V-LVOS-0-10-PWM-P-1")
				(= truename "V-IC-36")
				(= truename "V-IC-DIN-II-LITE-RF")
				(= truename "V-IC-DIN-II-LITE")
				)
			  	(progn
				(setq sublist (list truename handle xcoord ycoord cntrComponent en location componentType))		;create sublist for current item
				(setq panelcomponentlist (cons sublist panelcomponentlist))				;add sublist to mainlist, which accumulates during the loop
				(setq cntrComponent (+ cntrComponent 1))
				)
			)

			(if
			  	(OR
				;station bus
				(= truename "V-EASYTOUCH-II-1")
				(= truename "V-EASYTOUCH-II-2")
				(= truename "V-EASYTOUCH-II-3")
				(= truename "V-EASYTOUCH-II-4")
				(= truename "V-EASYTOUCH-II-5")
				(= truename "V-EQ40TB-TI")
				(= truename "V-LVOS-0-10-PWM-P-1")
				)
			  	(progn
				(setq sublist (list truename handle xcoord ycoord cntrStationBus en location))		;create sublist for current item
				(setq stationbuslist (cons sublist stationbuslist))					;add sublist to mainlist, which accumulates during the loop
				(setq cntrStationBus (+ cntrStationBus 1))
				)
			)
			  
			(if
			  	(OR
				;poe switches and routers for panels
				(= truename "COM-POE-SWITCH")
				(= truename "DA1458")
				(= truename "V-LCAP44H")
				(= truename "V-LCAP44HS")
				(= truename "V-LCAP44L")
				)
			  	(progn

				;define individual variables for each product
				(if
				  	(OR
					;LCAP H and L panels come with the com poe switch
					(= truename "COM-POE-SWITCH")
					(= truename "V-LCAP44H")
					(= truename "V-LCAP44HS")
					(= truename "V-LCAP44L")
					)
				  	(progn
					(setq numberOfPOEPorts 8)
					(setq numberOfEthernetPorts 0)
					)

				)
				(if
				  	(OR
					;poe switches and routers for panels
					(= truename "DA1458")
					)
				  	(progn
					(setq numberOfPOEPorts 4)
					(setq numberOfEthernetPorts 4)
					)

				)


				  
				(setq sublist (list truename handle xcoord ycoord cntrPoeSwitches en location numberOfPOEPorts numberOfEthernetPorts))		;create sublist for current item
				(setq poeswitchlist (cons sublist poeswitchlist))										;add sublist to mainlist, which accumulates during the loop
				(setq cntrPoeSwitches (+ cntrPoeSwitches 1))
				)

			)
			  
			(if
			  	(OR
				;poe devices
				(= truename "V-EQ73TB-TI")
				(= truename "V-EQ41TB-TI")
				)
			  	(progn
				(setq sublist (list truename handle xcoord ycoord cntrPoeDevices en location))		;create sublist for current item
				(setq poedevicelist (cons sublist poedevicelist))					;add sublist to mainlist, which accumulates during the loop
				(setq cntrPoeDevices (+ cntrPoeDevices 1))
				)

			)

			(if
			  	(OR
				;318
				(= truename "DT-300")
				)
			  	(progn
				(setq sublist (list truename handle xcoord ycoord cntr318 en location))			;create sublist for current item
				(setq 318list (cons sublist 318list))							;add sublist to mainlist, which accumulates during the loop
				(setq cntr318 (+ cntr318 1))
				)
			)

			(if
			  	(OR
				;rs232
				(= truename "LMDI-100")
				)
			  	(progn 
				(setq sublist (list truename handle xcoord ycoord cntrRs232 en location))		;create sublist for current item
				(setq rs232list (cons sublist rs232list))						;add sublist to mainlist, which accumulates during the loop
				(setq cntrRs232 (+ cntrRs232 1))
				)
			)


			(if
			  	(OR
				;processor
				(= truename "V-IC-36")
				(= truename "V-IC-DIN-II-LITE-RF")
				(= truename "V-IC-DIN-II-LITE")
				)
			  	(progn
				(if
				  	(OR
					(= truename "V-IC-36")
					)
					(progn
					(setq numberOfBusRuns 2)
					(setq numberOfStationsMax 60)
					(setq numberOfEQMax 18)
					(setq numberOfRS232Ports 5)
					(setq numberOfRS485Ports 2)					
					;in inches
					(setq maxstationbuscablerun 24000)
					(setq maxstationbuscablebetweendevices 12000)


					)
				)

				(if
				  	(OR
					(= truename "V-IC-DIN-II-LITE-RF")
					(= truename "V-IC-DIN-II-LITE")
					)
					(progn
					(setq numberOfBusRuns 1)
					(setq numberOfStationsMax 20)
					(setq numberOfEQMax 15)
					(setq numberOfRS232Ports 2)
					(setq numberOfRS485Ports 1)
					;in inches
					(setq maxstationbuscablerun 12000)
					(setq maxstationbuscablebetweendevices 6000)

					)
				)				
				(setq sublist (list truename handle xcoord ycoord cntrPanelProcessor en location numberOfBusRuns numberOfStationsMax numberOfEQMax numberOfRS232Ports numberOfRS485Ports maxstationbuscablerun maxstationbuscablebetweendevices))	;create sublist for current item
				(setq panelprocessorlist (cons sublist panelprocessorlist))				;add sublist to mainlist, which accumulates during the loop
				(setq cntrPanelProcessor (+ cntrPanelProcessor 1))
				)
			)
			  
			(setq cntr(+ cntr 1))										;once while loop reaches this point, increment counter varaible by one			
	  		)												;end while loop
			)												;-end when blocks not nil conditional
		)

	(setq mainpanellist (reverse mainpanellist))
  	(setq secondarypanellist (reverse secondarypanellist))
	(setq auxiliarypanellist (reverse auxiliarypanellist))
	(setq panelcomponentlist (reverse panelcomponentlist))
	(setq stationbuslist (reverse stationbuslist))
  	(setq poedevicelist (reverse poedevicelist))
	(setq 318list (reverse 318list))
	(setq rs232list (reverse rs232list))
	(setq panelprocessorlist (reverse panelprocessorlist))
  	(setq poeswitchlist (reverse poeswitchlist))
	(setq returnlist (list mainpanellist secondarypanellist auxiliarypanellist panelcomponentlist stationbuslist poedevicelist 318list rs232list panelprocessorlist poeswitchlist))

)




;;; --- vantage-turn-building-layout-layers-off Sub Function ---
;;; set layers for drawing
;;; Arguments
;;; none
;;; Alex Lundin 12-21-2016
(defun vantage-turn-building-layout-layers-off ( / ACADOBJ CMAX CNTR DOC LAYER LAYERCOLOR LAYERCOLORLIST LAYERNAME LAYEROBJINLOOP LAYERS VANTAGELAYERLIST)


 	;save layers from the object model
	(setq acadObj (vlax-get-acad-object))
	(setq doc (vla-get-ActiveDocument acadObj))

  	(setq layers (vla-get-layers doc))

  	(setq layerName "Defpoints")
	(setq layer (vla-item layers layerName))
  
	(setq vantageLayerList (list  "_WS_V_CONTROLLER_BUS" "_WS_V_ENCLOSURE_BUS" "_WS_V_AUXILIARY" "_WS_V_PANEL_COMPONENTS" "_WS_V_STATION_BUS" "_WS_V_3_18_AWG"
			       "_WS_V_RS232" "_WS_V_POE" "_WS_V_ORANGE_FOR_CONNECTIONS" 
			))
  
  
  	(setq cntr 0)
  	(setq cmax (length vantageLayerList))
	(while
	  	(< cntr cmax)
		(setq layerName (nth cntr vantageLayerList))
	  	(setq layerObjInLoop (vla-add layers layerName))
	  
	  	(vla-put-layeron layerObjInLoop :vlax-false)

		(setq cntr (+ cntr 1))
	)
)





;;; --- vantage-create-building-layout-layers Sub Function ---
;;; set layers for drawing
;;; Arguments
;;; none
;;; Alex Lundin 12-21-2016
(defun vantage-create-building-layout-layers ( / ACADOBJ CMAX CNTR DOC LAYER LAYERCOLOR LAYERCOLORLIST LAYERNAME LAYEROBJINLOOP LAYERS VANTAGELAYERLIST)


 	;save layers from the object model
	(setq acadObj (vlax-get-acad-object))
	(setq doc (vla-get-ActiveDocument acadObj))

  	(setq layers (vla-get-layers doc))

  	(setq layerName "Defpoints")
	(setq layer (vla-item layers layerName))
  
	(setq vantageLayerList (list  "_WS_V_CONTROLLER_BUS" "_WS_V_ENCLOSURE_BUS" "_WS_V_AUXILIARY" "_WS_V_PANEL_COMPONENTS" "_WS_V_STATION_BUS" "_WS_V_3_18_AWG"
			       "_WS_V_RS232" "_WS_V_POE" "_WS_V_ORANGE_FOR_CONNECTIONS" 
			))
  

	(setq layerColorList (list "10" "114" "255" "255" "200" "20" "4" "3" "30"))
  
  	(setq cntr 0)
  	(setq cmax (length vantageLayerList))
	(while
	  	(< cntr cmax)
		(setq layerName (nth cntr vantageLayerList))
	  	(setq layerColor (nth cntr layerColorList))

		;add layer, even works when layer already exists
	  	(setq layerObjInLoop (vla-add layers layerName))
	  	(vla-put-layeron layerObjInLoop :vlax-true)
	  	(vla-put-Plottable layerObjInLoop :vlax-false)
	  	(vla-put-color layerObjInLoop layerColor)
	  	(vlax-release-object layerObjInLoop) 
		(setq cntr (+ cntr 1))
	)
  
	;create VANTAGE layers with command line, much slower and less predictable
;;;	(command "-layer" "make" "_WS_V_CONTROLLER_BUS" "color" "10" "_WS_V_CONTROLLER_BUS" "LType" "Continuous" "_WS_V_CONTROLLER_BUS" "Plot" "N" "_WS_V_CONTROLLER_BUS" "")
;;;  	(command "-layer" "make" "_WS_V_ENCLOSURE_BUS" "color" "114" "_WS_V_ENCLOSURE_BUS" "LType" "Continuous" "_WS_V_ENCLOSURE_BUS" "Plot" "N" "_WS_V_ENCLOSURE_BUS" "")
;;;  	(command "-layer" "make" "_WS_V_AUXILIARY" "color" "255" "_WS_V_AUXILIARY" "LType" "Continuous" "_WS_V_AUXILIARY" "Plot" "N" "_WS_V_AUXILIARY" "")
;;;  	(command "-layer" "make" "_WS_V_PANEL_COMPONENTS" "color" "255" "_WS_V_PANEL_COMPONENTS" "LType" "Continuous" "_WS_V_PANEL_COMPONENTS" "Plot" "N" "_WS_V_PANEL_COMPONENTS" "")
;;;  	(command "-layer" "make" "_WS_V_STATION_BUS" "color" "200" "_WS_V_STATION_BUS" "LType" "Continuous" "_WS_V_STATION_BUS" "Plot" "N" "_WS_V_STATION_BUS" "")
;;;	(command "-layer" "make" "_WS_V_3_18_AWG" "color" "20" "_WS_V_3_18_AWG" "LType" "Continuous" "_WS_V_3_18_AWG" "Plot" "N" "_WS_V_3_18_AWG" "")
;;;	(command "-layer" "make" "_WS_V_RS232" "color" "4" "_WS_V_RS232" "LType" "Continuous" "_WS_V_RS232" "Plot" "N" "_WS_V_RS232" "")
;;;  	(command "-layer" "make" "_WS_V_POE" "color" "3" "_WS_V_POE" "LType" "Continuous" "_WS_V_POE" "Plot" "N" "_WS_V_POE" "")
;;;  	(command "-layer" "make" "_WS_V_ORANGE_FOR_CONNECTIONS" "color" "30" "_WS_V_ORANGE_FOR_CONNECTIONS" "LType" "Continuous" "_WS_V_POE" "Plot" "N" "_WS_V_ORANGE_FOR_CONNECTIONS" "")

	; move any cable from previous vantage layers to new ones
  	; not currently supporting this operation, deleting any previous cable at this point
;;;  	(setq objecttype "LWPOLYLINE")
;;;	(setq layerfrom	"_WS_CONTROLLER_BUS")
;;;  	(setq layerfrom	"_WS_V_CONTROLLER_BUS")
;;;  	(VANTAGE-MOVE-OBJECTS objecttype layerfrom layerto)
;;;  	
;;;  	
;;;  	(setq objecttype "LWPOLYLINE")
;;;	(setq layerfrom	"_WS_ENCLOSURE_BUS")
;;;  	(setq layerto	"_WS_V_ENCLOSURE_BUS")
;;;  	(VANTAGE-MOVE-OBJECTS objecttype layerfrom layerto)
;;;
;;;
;;;  	(setq objecttype "LWPOLYLINE")
;;;	(setq layerfrom	"_WS_PANEL_COMPONENTS")
;;;  	(setq layerto	"_WS_V_PANEL_COMPONENTS")
;;;  	(VANTAGE-MOVE-OBJECTS objecttype layerfrom layerto)
;;;
;;;  	(setq objecttype "LWPOLYLINE")
;;;	(setq layerfrom	"_WS_STATION_BUS")
;;;  	(setq layerto	"_WS_V_STATION_BUS")
;;;  	(VANTAGE-MOVE-OBJECTS objecttype layerfrom layerto)
;;;
;;;	(setq objecttype "LWPOLYLINE")
;;;	(setq layerfrom	"_WS_3_18_AWG")
;;;  	(setq layerto	"_WS_V_3_18_AWG")
;;;  	(VANTAGE-MOVE-OBJECTS objecttype layerfrom layerto)
;;;
;;;	(setq objecttype "LWPOLYLINE")
;;;	(setq layerfrom	"_WS_RS232")
;;;  	(setq layerto	"_WS_V_RS232")
;;;  	(VANTAGE-MOVE-OBJECTS objecttype layerfrom layerto)


)






; sub function
; --- VANTAGE-MOVE-OBJECTS ---
; Arguments
; layer-from which is the layer the splines are on
; layer-to which is the layer to move the splines to
;Alex Lundin 12-28-2016
(defun VANTAGE-MOVE-OBJECTS	(object-type layer-from layer-to / objects object CMAX CNTR ENT LAYER TRUENAME)

  	(setq objects(ssget "x" (list(cons 0 object-type)(cons 8 layer-from))))						;select objects on layer-from
	(cond														;-conditional block
	  	((/= objects nil)											;-conditional statement, exact same structure as above, moves bridges from BRIDGES to WATTSTOPPER layer
	 	(setq cntr 0)
		(setq cmax (sslength objects))
		(while 	(< cntr cmax)											;--while loop
			(setq object(ssname objects cntr))								;set bridge varaible to the member in blocks that corresponds to the current counter cntr variable

			(setq ent object)										;set ent to the entity stored in block
		  	(if	(= object-type "INSERT")								;only get truename if the object is a block
				(setq truename (VANTAGE-BLOCKTRUENAME ent))						;call VANTAGE-BLOCKTRUENAME function with the ent parameter set in the LWPOLYLINE above, set the truename variable to the final result
			)
			(setq layer layer-to)										;set layer to correct layer
			(setq ent object)										;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though
			(VANTAGE-ENTMOD-LAYER ent layer)								;call VANTAGE-ENTMOD-LAYER on ent and layer arugments, no return value needed
  			(setq cntr(+ cntr 1))      
		)													;--end while
		)													;-end conditional statement
	)														;-end conditional block
)





;;; --- VANTAGE-BLOCKTRUENAME Sub Function ---
;;; send entity name of block in through ent variable from calling function
;;; the ent argument from the caller gets passed into the ent variable in this function
;;; the truename is returned to the caller
;;; Arguments
;;; entity's name passed into ent variable
;;; Alex Lundin 12-21-2016
(defun VANTAGE-BLOCKTRUENAME (ent / vlaobject truename)
  
 	(setq vlaobject (vlax-ename->vla-object ent))									;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name stored in ent
	(setq truename(vlax-get-property vlaobject									;set the truename variable to the effective name property
	(if (vlax-property-available-p vlaobject 'effectivename)							;only if the property called effective name exisits inside block 
		'effectivename												;this entire operation of setting truename fails when if statement is false
		'name
		)
	)
	) 

)


	
;;; --- VANTAGE-ENTMOD-LAYER Sub Function ---
;;; change objects layer
;;; Arguments
;;; entity's name passed into ent variable
;;; layer passed into layer variable
;;; Alex Lundin 12-21-2016

(defun VANTAGE-ENTMOD-LAYER	(ent layer / ENTDXF)

	(setq entdxf(entget ent))											;set dxf codes of ent to entdxf				
	(setq entdxf(subst (cons 8 layer)(assoc 8 entdxf) entdxf))							;change dxfcodes on entdxf
	(entmod entdxf)													;entmod function to modifiy the entity with the new values
)


(defun vmain-setAttributesAndValues ( item Tags Values / ATTTAG ATTVAL CMAX2 CNTR2 ENLIST ENTTYPE TAG VALUE)
	(setq en (nth 5 item))

  	(setq en2(entnext en))
  	(setq enlist(entget en2))										;set enlist to the group codes of en2
  	(setq enttype 0)											;initialize entttype to 0 (makes while loop false by default)
  	(setq attTag 1)												;initialize attTag to 0 (makes condition false by default)
  	(setq enttype(cdr(assoc 0 enlist)))									;set enttype to the second element of the 0 group code in enlist
	(while 	(= enttype "ATTRIB")										;loop while enttype is "ATTRIB"
		(setq attTag(cdr(assoc 2 enlist)))								;set attTag to the second element of the second associative property (a blocks attribute name) of enlist
		(setq attVal(cdr(assoc 1 enlist)))								;set attVal to the second element of the first associative property (a value associated with the above attribute) of enlist
			(cond 	((/= attTag nil)								;conditional statement only when value is stored in attTag


		  		(setq cntr2 0)
		  		(setq cmax2 (length Tags))
		  		(while 	(< cntr2 cmax2)
				  	(setq Tag (nth cntr2 Tags))
					(setq Value (nth cntr2 Values))

				  	(if
					  	(= Tag attTag)
					  	(progn
						(setq enlist (subst (cons 1 Value)(assoc 1 enlist) enlist))
						(entmod enlist)
						)
					)
				  	(setq cntr2 (+ cntr2 1))
				)
				)
			)
		(setq en2(entnext en2))										;get the codes of the next sub entity of the titleblock (this will drill down farther with each loop)
		(setq enlist(entget en2))									;set enlist to the group codes of this new sub entity
		(setq enttype(cdr(assoc 0 enlist)))								;set the enttype to the second member of the first associative value of the group codes (entity type)
														;loop will repeat until group code is not equal to "ATTRIB"	  			
	)
)





(defun vmain-getAttributesAndValues ( item Tags / ATTTAG ATTVAL EN2 ENLIST ENTTYPE RETURNLIST TAGS VALUES)

  	(setq en (nth 5 item))

  	(setq en2(entnext en))	
  	(setq enlist(entget en2))										;set enlist to the group codes of en2
  	(setq enttype 0)											;initialize entttype to 0 (makes while loop false by default)
  	(setq attTag 1)												;initialize attTag to 0 (makes condition false by default)
  	(setq enttype(cdr(assoc 0 enlist)))									;set enttype to the second element of the 0 group code in enlist
	(while (= enttype "ATTRIB")										;loop while enttype is "ATTRIB"
		(setq attTag(cdr(assoc 2 enlist)))								;set attTag to the second element of the second associative property (a blocks attribute name) of enlist
		(setq attVal(cdr(assoc 1 enlist)))								;set attVal to the second element of the first associative property (a value associated with the above attribute) of enlist
			(cond 	((/= attTag nil)								;conditional statement only when value is stored in attTag


		  		(setq cntr2 0)
		  		(setq cmax2 (length Tags))
		  		(while 	(< cntr2 cmax2)
				  	(setq Tag (nth cntr2 Tags))

				  	(if
					  	(= Tag attTag)
					  	(progn

						(setq Values(append Values(list attVal)))			;add the attVal member to the list Values
						)
					)
				  	(setq cntr2 (+ cntr2 1))
				)
				)
			)
		(setq en2(entnext en2))										;get the codes of the next sub entity of the titleblock (this will drill down farther with each loop)
		(setq enlist(entget en2))									;set enlist to the group codes of this new sub entity
		(setq enttype(cdr(assoc 0 enlist)))								;set the enttype to the second member of the first associative value of the group codes (entity type)
														;loop will repeat until group code is not equal to "ATTRIB"	  			
	)
  

  	(setq returnlist (list Tags Values))
)