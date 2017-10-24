; --- WSCABLEALL Function ---
; Measure all splines and plines from default layers
; Output length to text file where drawing is located
; Alex Lundin 07-14-2017

(defun C:WSCABLEALL (/ splines cntr splineEN splineDXF vlaobject distanceCurrent distanceTotal dwgname  f1 CMAX LAYER LAYERLIST)
	(vl-load-com)
	(setvar "CMDECHO" 0)

  	(setq f1 (open (strcat (getvar 'DWGPREFIX) "Cable_Extraction.txt")  "w"))							;set f1 to to result of open on the string created from dwgprefix variable plus "Riser_Extraction.txt" open file for appending   
  	(write-line (strcat (getvar "DWGPREFIX" ) (getvar "DWGNAME" ) " Cable Extractor" ) f1)						;write first line of file (this contains information about the drawing we extract from)

	(setq layerList (list "_WS_CAT5E" "_WS_CAT5E_Extra" "_WS_MSTP" "_WS_CONTROLLER_BUS"
			      "_WS_ENCLOSURE_BUS" "_WS_RS232" "_WS_V_3_18_AWG" "_WS_V_CONTROLLER_BUS"
			      "_WS_V_ENCLOSURE_BUS" "_WS_V_POE" "_WS_V_RS232" "_WS_V_STATION_BUS" "_WS_3_18_AWG"
			      "_WS_CONTROLLER_BUS" "_WS_ENCLOSURE_BUS" "_WS_POE" "_WS_RS232" "_WS_STATION_BUS"))
  	(setq cntr 0)
  	(setq cmax (length layerList))
	(while	(<= cntr cmax)
		(setq layer (nth cntr layerList))
		(measureCable layer f1)
		(setq cntr(+ cntr 1))
	)


	(close f1)																;close f1
	(setvar "CMDECHO" 1)
)																		;end function

(defun measureCable ( layer f1 / CNTR cmax DISTANCECURRENT DISTANCETOTAL DISTANCETOTALCONVERTED DWGNAME SPLINEDXF SPLINEEN SPLINES VLAOBJECT)


	(setq splines (ssget "x" (list (cons 0 "SPLINE")(cons 8 layer))))									;same structure as above except for spline objects
	(setq cntr 0)
  	(setq cmax 0)
  	(if
	  	(/= splines nil)
	  	(progn
  		(setq cmax (sslength splines))
		)
	)
	(setq distanceTotal 0)
  
  	(while 	(< cntr cmax)
		(setq splineEN(ssname splines cntr))
		(setq splineDXF(entget splineEN))
		(setq vlaobject (vlax-ename->vla-object splineEN))										;set vlaobject to the converted vla name from the entity name of block
	  	
  		(setq distanceCurrent (vlax-curve-getDistAtPoint vlaobject (vlax-curve-getEndPoint vlaobject)))
	  	(setq distanceTotal (+ distanceCurrent distanceTotal))
		(setq cntr(+ cntr 1))      			
  	)
	(setq dwgname (vl-filename-base (getvar "dwgname")))
	
	(if	(/= distanceTotal 0)
		(progn
		(setq distanceTotalConverted (rtos distanceTotal 4 3))											;convert distance total to string
		(write-line (strcat "\nCable extraction from layer " layer ) f1)
		(write-line (strcat " -found approximately " distanceTotalConverted " feet of cable drawn as splines." ) f1)
		(write-line	(strcat "\n")	f1)
		)
	)
	(setq splines (ssget "x" (list (cons 0 "LWPOLYLINE")(cons 8 layer))))									;same structure as above except for spline objects
	(setq cntr 0)
  	(setq cmax 0)
  	(if
	  	(/= splines nil)
	  	(progn
  		(setq cmax (sslength splines))
		)
	)  
	(setq distanceTotal 0)
  
  	(while 	(< cntr cmax )
		(setq splineEN(ssname splines cntr))
		(setq splineDXF(entget splineEN))
		(setq vlaobject (vlax-ename->vla-object splineEN))										;set vlaobject to the converted vla name from the entity name of block
	  	
  		(setq distanceCurrent (vlax-curve-getDistAtPoint vlaobject (vlax-curve-getEndPoint vlaobject)))
	  	(setq distanceTotal (+ distanceCurrent distanceTotal))
		(setq cntr(+ cntr 1))      			
  	)
	
	
	(if	(/= distanceTotal 0)
		(progn
		(setq distanceTotalConverted (rtos distanceTotal 4 3))
		(write-line (strcat "\nCable extraction from layer " layer ) f1)									;write first line of file (this contains information about the drawing we extract from)
		(write-line (strcat " -found approximately " distanceTotalConverted " feet of cable drawn as plines." ) f1)
		(write-line	(strcat "\n")	f1)
		)
	)
)

; --- WSCABLE Function ---
; Measure all splines on _WS_MSTP layer
; Output length to text file where drawing is located
; Alex Lundin 03-04-2016

(defun C:WSCABLE (/ splines cntr splineEN splineDXF vlaobject distanceCurrent distanceTotal dwgname  f1 DISTANCETOTALCONVERTED)
(vl-load-com)
(setvar "CMDECHO" 0)

(setq splines(ssget "x" '((0 . "SPLINE")(8 . "_WS_MSTP"))))											;same structure as above except for spline objects
(setq cntr 0)
(setq distanceTotal 0)
  
  	(while 	(< cntr (sslength splines))
		(setq splineEN(ssname splines cntr))
		(setq splineDXF(entget splineEN))
		(setq vlaobject (vlax-ename->vla-object splineEN))										;set vlaobject to the converted vla name from the entity name of block
	  	
  		(setq distanceCurrent (vlax-curve-getDistAtPoint vlaobject (vlax-curve-getEndPoint vlaobject)))
	  	(setq distanceTotal (+ distanceCurrent distanceTotal))
		(setq cntr(+ cntr 1))      			
  	)
(setq dwgname (vl-filename-base (getvar "dwgname")))
(setq distanceTotalConverted (rtos distanceTotal 4 3))												;convert distance total to string
(if
  	(setq f1 (open (strcat (getvar 'DWGPREFIX) "Cable_MSTP_Extraction.txt")  "a"))								;set f1 to to result of open on the string created from dwgprefix variable plus "Riser_Extraction.txt" open file for appending   
	(write-line (strcat (getvar "DWGPREFIX" ) (getvar "DWGNAME" ) " -found approximately " distanceTotalConverted " feet of cable." ) f1)	;write first line of file (this contains information about the drawing we extract from)
)	

(close f1)																	;close f1
(setvar "CMDECHO" 1)
)																		;end function




; --- WSCABLE2 Function ---
; Measure all splines on _WS_CAT5E layer
; Output length to text file where drawing is located
; Alex Lundin 05-19-2016

(defun C:WSCABLE2 (/ splines cntr splineEN splineDXF vlaobject distanceCurrent distanceTotal dwgname  f1 DISTANCETOTALCONVERTED)
(vl-load-com)
(setvar "CMDECHO" 0)

(setq splines(ssget "x" '((0 . "SPLINE")(8 . "_WS_CAT5E"))))											;same structure as above except for spline objects
(setq cntr 0)
(setq distanceTotal 0)
  
  	(while 	(< cntr (sslength splines))
		(setq splineEN(ssname splines cntr))
		(setq splineDXF(entget splineEN))
		(setq vlaobject (vlax-ename->vla-object splineEN))										;set vlaobject to the converted vla name from the entity name of block
	  	
  		(setq distanceCurrent (vlax-curve-getDistAtPoint vlaobject (vlax-curve-getEndPoint vlaobject)))
	  	(setq distanceTotal (+ distanceCurrent distanceTotal))
		(setq cntr(+ cntr 1))      			
  	)
(setq dwgname (vl-filename-base (getvar "dwgname")))
(setq distanceTotalConverted (rtos distanceTotal 4 3))												;convert distance total to string
(if
  	(setq f1 (open (strcat (getvar 'DWGPREFIX) "Cable_CAT5E_Extraction.txt")  "a"))								;set f1 to to result of open on the string created from dwgprefix variable plus "Riser_Extraction.txt" open file for appending   
	(write-line (strcat (getvar "DWGPREFIX" ) (getvar "DWGNAME" ) " -found approximately " distanceTotalConverted " feet of cable." ) f1)	;write first line of file (this contains information about the drawing we extract from)
)	

(close f1)																	;close f1
(setvar "CMDECHO" 1)
)																		;end function





; --- WSCABLEM Function ---
; Measure all splines on _WS_CAT5E layer
; Output length to text file where drawing is located
; Alex Lundin 08-01-2016

(defun C:WSCABLEM (/ splines cntr splineEN splineDXF vlaobject distanceCurrent distanceTotal dwgname  f1)
(vl-load-com)
(setvar "CMDECHO" 0)


(command "_.Layer" "_Make" "WATTSTOPPER_CABLE_MEASUREMENTS" "_Color" "3" "" "LType" "Continuous" "" "")				;create WATTSTOPPER_CABLE_MEASUREMENTS LAYER
(command "clayer" "WATTSTOPPER_CABLE_MEASUREMENTS")											;set current layer to WATTSTOPPER_CABLE_MEASUREMENTS


(setq selectionSet1 (entsel "Select object, code will measure splines and plines from layer:"))
(setq itemEN(nth 0 selectionSet1))
(setq itemDXF(entget itemEN))
(setq dxf8 (cdr (assoc 8 itemDXF )))
(setq layer dxf8)

(setq plines (ssget "x" (list (cons 0  "LWPOLYLINE") (cons 8 layer))))
(setq splines1 (ssget "x" (list (cons 0 "SPLINE") (cons 8 layer))))
(setq splines (acet-ss-union (list plines splines1)))

(setq cntr 0)
(setq distanceTotal 0)
(setq fitPoints(list))
  	(while 	(< cntr (sslength splines))
		(setq splineEN(ssname splines cntr))
		(setq splineDXF(entget splineEN))
		(setq vlaobject (vlax-ename->vla-object splineEN))										;set vlaobject to the converted vla name from the entity name of block
  		(setq distanceCurrent (vlax-curve-getDistAtPoint vlaobject (vlax-curve-getEndPoint vlaobject)))
	  	(setq distanceTotal (+ distanceCurrent distanceTotal))

					(setq fitpoints (list))
					(foreach flag splineDXF                                              					;look for every flag (10 dxf) in the dxfRectangle list
  						(if	(= 10 (car flag))                                         				;if the 10 dxf exists 
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
	  				(setq middlePointNumber (/ fPMax 2))
	  				(setq fPfirst (car fitPoints))
	  				(setq fPMid (nth middlePointNumber fitPoints)) 
					(setq fPlast (last fitPoints ))
					(setq vertices (cdr fitPoints))
		 			(command "-mtext" fPMid "J" "MC" "R" "0" "H" "1" fPMid distanceCurrent "")
					(setq fitPoints(list)) 											;create an empty list to store cornerPoints in

		(setq cntr(+ cntr 1))      			
  	)
(command "clayer" "WATTSTOPPER")																		;close f1
(setvar "CMDECHO" 1)
)


; --- WSCABLEM2 Function ---
; Remove all spline measurements and purge layer
; Alex Lundin 08-01-2016
(defun C:WSCABLEM2 (/)
  
	(setq ss (ssget "x" '((8 . "WATTSTOPPER_CABLE_MEASUREMENTS"))))
  	(cond														;-conditional block
		((/= ss nil)												;-conditional statement
		(command "erase" "p" "")
		)													;-end conditional statement
	)														;-end conditional block
    	(command "clayer" "WATTSTOPPER")										;purge command block, sets WATTSTOPPER current layer and purges BRIDGES layer
	(command "_.purge" "LA" "WATTSTOPPER_CABLE_MEASUREMENTS" "N")
)


; --- WSCABLEI Function ---
; Measure individual splines on _WS_CAT5E layer
; Alex Lundin 06-23-2016

(defun C:WSCABLEI (/ DISTANCECURRENT SPLINEEN VLAOBJECT); splines cntr splineEN splineDXF vlaobject distanceCurrent distanceTotal dwgname  f1)
(vl-load-com)
(setvar "CMDECHO" 0)

(setq splineEN (car(entsel "Select one cable to measure:")))											;set splineEN to the first element from the enitity retrived with entsel
(setq vlaobject (vlax-ename->vla-object splineEN))												;set vlaobject to the converted vla name from the entity name of block
(setq distanceCurrent (vlax-curve-getDistAtPoint vlaobject (vlax-curve-getEndPoint vlaobject)))
(setq string (strcat "Cable length is: " (rtos distanceCurrent)))
(print string)
(princ)
)																		;end function









