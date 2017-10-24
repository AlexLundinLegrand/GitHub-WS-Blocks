;;; --- c:rdviewports ---
;;; creates layout tabs and viewports from modelspace rectangles generated from GRD function
;;; Arguments
;;; none
;;; Alex Lundin 06-28-2017
(defun c:rdviewports
       	(
	
	/
	CMAX CMIN CNTR CNTRREVERSE CNTRSTRING CORNERPOINTSRECTANGLE DXFRECTANGLE ENRECTANGLE FLAG LAYOUT LAYOUTS RECTANGLEC1 RECTANGLEC2 SSRECTANGLES
	DATE LAYOUT LAYOUTS NEWFILENAME ORIGINALFILENAME USERNAME LAYOUT LAYOUTS SORTEDMODELSPACEVIEWPORTS SORTEDMODELSPACEVIEWPORTSITEM
	ACDOC BLKS CENTER CUR-LAYOUT-NAME CURR FUNCTIONENDTIME FUNCTIONSTARTTIME HEIGHT LAYOUT LAYOUTS LAYTOCOPY NEW POINT1 POINT2
	 PREVIOUSAUTOMATICVPSETTING TIMEELAPSEDTIME TWIST VLA-LAYOUTS VP VP-SCALE WIDTH
	)
	(vl-load-com)


	(setq previousAutomaticVPsetting (getenv "CreateViewports"))
	(setenv "CreateViewports" "0")
	

  
	;delete all layouts
	(setq layouts															;create layouts object from the active document
	(vla-get-layouts
	(vla-get-activedocument (vlax-get-acad-object))
	)
	)
	(mapcar '(lambda (layout)													;loop through each layout using mapcar with layoutlist
	(vla-delete (vla-item layouts layout))												;use vla delete to remove the layout from the drawing with the layouts object created above
	)
	(layoutlist)
	)
  	(add-default-layout-tabs)
	(setq *acad (vlax-get-acad-object))
	(setq acdoc (vla-get-ActiveDocument *acad))
	(setq vla-layouts (vla-get-layouts acdoc))

	;selection set of all modelspace rectangles to make into viewports
	(setq ssRectangles (ssget "x" '((0 . "LWPOLYLINE")(8 . "Modelspace_Viewports_8X11.5")(410 . "MODEL"))))
	(setq functionStarttime (rtos (getvar "CDATE") 2 6))

  	
  	(if
	  	(/= ssRectangles nil)
	  	(progn
		(setq sortedModelspaceViewports (modelspace-viewport-info-list ssRectangles))
	  	;while loop through selection set
	  	;during loop:
	  	;grab current rectangle
	  	;store points
	  	;create new paperspace tab
	  	;zoom to the points stored from the modelspace rectangle
	  	;continue to next rectangle in the selection set
		(setq cntr 0)
	  	(setq cmax (sslength ssRectangles))
	  	(setq cntrReverse (- cmax 1))
	  	(setq cmin 0)
		(while	(< cntr cmax)
			(setq sortedModelspaceViewportsItem (nth cntr sortedModelspaceViewports))
			
		  	(setq enRectangle (nth 3 sortedModelspaceViewportsItem))
		  
			(setq dxfRectangle (entget enRectangle))
			  
			(setq flag (car(assoc 10 dxfRectangle)))
			  

			(setq cornerPointsRectangle(list)) 											;create an empty list to store cornerPoints in

			(foreach flag dxfRectangle                                              						;look for every flag (10 dxf) in the dxfRectangle list
			  (if(= 10 (car flag))                                         								;if the 10 dxf exists 
			    (setq cornerPointsRectangle                                 							;reset cornerPointsRectangle to
			      (append cornerPointsRectangle                       								;the old cornerPointsRectangle 
			        (list                                                       							;plus a list containing
			          (cdr flag)                                             							;the cornerPoint point
			        )                                                                 						;close the list statement 
			      )                                                                      						;close the append statement
			    )                                                                           					;close the setq statement
			  )                                                                                					;close the if statement
			)                                                                                     					;close the foreach statement

		  	;;; get corner points of modelspace rectangle for viewport creation
			(setq Rectanglec1 (nth 0 cornerPointsRectangle))
			(setq Rectanglec2 (nth 2 cornerPointsRectangle))
		  	(setq point1 (vlax-3d-point Rectanglec1))
		  	(setq point2 (vlax-3d-point Rectanglec2))

				 
	          	(setq cntrString (itoa (+ cntr 1)))
		  

			
			(setq cur-layout-name  (strcat cntrString "_8 X 11.5"))



			(setq
			laytocopy "8 X 11.5"
			blks nil
			twist 0.0
			vp-scale acVp1_1
			)


			;;; set current layout to copy from
		  	(setq curr (vla-item vla-layouts laytocopy))


		  	;;; add the new layout, define the center point
			(setq new (vla-add vla-layouts cur-layout-name))
			(vla-put-ActiveLayout acdoc new)
			(setq center (vlax-3d-point 4.2631 6.0169 0)
			width  8.4
			height 10.4
			)

		  	;;; create viewport object to add to document after definition is complete
			(setq vp (vla-AddPViewport
			     (vla-get-PaperSpace acdoc)
			     center
			     width
			     height
			   )
			)

		  	;;; start copy loop
			(vla-copyfrom new curr)
			(vlax-for blk (vla-get-block curr)
			(if (/= (vla-get-objectname blk) "AcDbViewport")
			(setq blks (cons blk blks))
			)
			)

		  	;;; copy blocks
			(if blks
			(progn
			(setq blks (reverse blks))
			(vlax-invoke acdoc 'CopyObjects blks (vla-get-block new))
			)
			)

		  	;;; define display and put viewport in the active document
			(vla-Display vp :vlax-true)
			(vla-put-mspace acdoc :vlax-true)
			(vla-put-activepviewport acdoc vp)


		  	;;; zoom viewport and set modelspace to false
			(vla-Zoomwindow *acad point1 point2)
			(vla-put-mspace acdoc :vlax-false)
		  
		  	;;; release and regen
			(vlax-Release-Object vp)
			(vla-Regen acdoc acActiveViewport)
			(princ)





		  	(setq cntr (+ cntr 1))
			(setq cntrReverse (- cntrReverse 1))


		  
		)
		)
	)

  	(vlax-Release-Object vla-layouts)
  	(vlax-Release-Object acdoc)
  	(vlax-Release-Object *acad)

	;;;THESE SHEET SIZES ARE UNDER CONSTRUCTION AND NOT READY YET
  	;;;
  	;;;
  
	;selection set of all modelspace rectangles to make into viewports
;;;	(setq ssRectangles (ssget "x" '((0 . "LWPOLYLINE")(8 . "Modelspace_Viewports_11X17")(410 . "MODEL"))))
;;;
;;;  	(if
;;;	  	(/= ssRectangles nil)
;;;	  	(progn
;;;		(setq sortedModelspaceViewports (modelspace-viewport-info-list ssRectangles))
;;;	  	;while loop through selection set
;;;	  	;during loop:
;;;	  	;grab current rectangle
;;;	  	;store points
;;;	  	;create new paperspace tab
;;;	  	;select the viewport in the new tab (there will be only one)
;;;	  	;zoom to the points stored from the modelspace rectangle
;;;	  	;continue to next rectangle in the selection set
;;;		(setq cntr 0)
;;;	  	(setq cmax (sslength ssRectangles))
;;;	  	(setq cntrReverse (- cmax 1))
;;;	  	(setq cmin 0)
;;;		(while	(< cntr cmax)
;;;			(setq sortedModelspaceViewportsItem (nth cntr sortedModelspaceViewports))
;;;			
;;;		  	(setq enRectangle (nth 3 sortedModelspaceViewportsItem))
;;;			       
;;;			(setq dxfRectangle (entget enRectangle))
;;;			  
;;;			(setq flag (car(assoc 10 dxfRectangle)))
;;;			  
;;;
;;;			(setq cornerPointsRectangle(list)) 											;create an empty list to store cornerPoints in
;;;
;;;			(foreach flag dxfRectangle                                              						;look for every flag (10 dxf) in the dxfRectangle list
;;;			  (if(= 10 (car flag))                                         								;if the 10 dxf exists 
;;;			    (setq cornerPointsRectangle                                 							;reset cornerPointsRectangle to
;;;			      (append cornerPointsRectangle                       								;the old cornerPointsRectangle 
;;;			        (list                                                       							;plus a list containing
;;;			          (cdr flag)                                             							;the cornerPoint point
;;;			        )                                                                 						;close the list statement 
;;;			      )                                                                      						;close the append statement
;;;			    )                                                                           					;close the setq statement
;;;			  )                                                                                					;close the if statement
;;;			)                                                                                     					;close the foreach statement
;;;
;;;			(setq Rectanglec1 (nth 0 cornerPointsRectangle))
;;;			(setq Rectanglec2 (nth 2 cornerPointsRectangle))
;;;
;;;			(command "-layout" "template" "C:/WS_Blocks/Default/Templates/RD-DLM.dwt" "11 X 17")
;;;	  
;;;				 
;;;	          	(setq cntrString (itoa (+ cntr 1)))
;;;			(command "_.layout" "rename" "11 X 17" (strcat cntrString "_11 X 17"))
;;;	          
;;;
;;;			(command "_.ctab" (strcat cntrString "_11 X 17"))
;;;		  	(command "_.MSPACE" "_.ZOOM" "_W" Rectanglec1 Rectanglec2)
;;;	  		(command "_.PSPACE")
;;;		  	(setq cntr (+ cntr 1))
;;;			(setq cntrReverse (+ cntrReverse 1))
;;;		)
;;;		)
;;;	)
;;;
;;;
;;;  
;;;	;selection set of all modelspace rectangles to make into viewports
;;;	(setq ssRectangles (ssget "x" '((0 . "LWPOLYLINE")(8 . "Modelspace_Viewports_22X34")(410 . "MODEL"))))
;;;  	(if
;;;	  	(/= ssRectangles nil)
;;;	  	(progn
;;;		(setq sortedModelspaceViewports (modelspace-viewport-info-list ssRectangles))
;;;	  	;while loop through selection set
;;;	  	;during loop:
;;;	  	;grab current rectangle
;;;	  	;store points
;;;	  	;create new paperspace tab
;;;	  	;select the viewport in the new tab (there will be only one)
;;;	  	;zoom to the points stored from the modelspace rectangle
;;;	  	;continue to next rectangle in the selection set
;;;		(setq cntr 0)
;;;	  	(setq cmax (sslength ssRectangles))
;;;	  	(setq cntrReverse (- cmax 1))
;;;	  	(setq cmin 0)
;;;		(while	(< cntr cmax)
;;;			(setq sortedModelspaceViewportsItem (nth cntr sortedModelspaceViewports))
;;;			
;;;		  	(setq enRectangle (nth 3 sortedModelspaceViewportsItem))
;;;		  
;;;			(setq dxfRectangle (entget enRectangle))
;;;			  
;;;			(setq flag (car(assoc 10 dxfRectangle)))
;;;			  
;;;
;;;			(setq cornerPointsRectangle(list)) 											;create an empty list to store cornerPoints in
;;;
;;;			(foreach flag dxfRectangle                                              						;look for every flag (10 dxf) in the dxfRectangle list
;;;			  (if(= 10 (car flag))                                         								;if the 10 dxf exists 
;;;			    (setq cornerPointsRectangle                                 							;reset cornerPointsRectangle to
;;;			      (append cornerPointsRectangle                       								;the old cornerPointsRectangle 
;;;			        (list                                                       							;plus a list containing
;;;			          (cdr flag)                                             							;the cornerPoint point
;;;			        )                                                                 						;close the list statement 
;;;			      )                                                                      						;close the append statement
;;;			    )                                                                           					;close the setq statement
;;;			  )                                                                                					;close the if statement
;;;			)                                                                                     					;close the foreach statement
;;;
;;;			(setq Rectanglec1 (nth 0 cornerPointsRectangle))
;;;			(setq Rectanglec2 (nth 2 cornerPointsRectangle))
;;;
;;;			(command "-layout" "template" "C:/WS_Blocks/Default/Templates/RD-DLM.dwt" "22 X 34")
;;;
;;;	          	(setq cntrString (itoa (+ cntr 1)))
;;;			(command "_.layout" "rename" "22 X 34" (strcat cntrString "_22 X 34"))
;;;				 
;;;
;;;
;;;			(command "_.ctab" (strcat cntrString "_22 X 34"))
;;;		  	(command "_.MSPACE" "_.ZOOM" "_W" Rectanglec1 Rectanglec2)
;;;	  		(command "_.PSPACE")
;;;		  	(setq cntr (+ cntr 1))
;;;			(setq cntrReverse (+ cntrReverse 1))
;;;		)
;;;		)
;;;	)
	

  	;;; exit function showing time elapsed
  	;;; restore vp settings
  	(setq functionEndtime (rtos (getvar "CDATE") 2 8))
  	(setq timeElapsedTime (- (atof functionEndtime) (atof functionStarttime) ))
  	(setq timeElapsedTime (rtos timeElapsedTime))
	(princ timeElapsedTime)
	(setenv "CreateViewports" previousAutomaticVPsetting)
    	;(command ".save" newfilename)

	;(command ".save" originalfilename)

)


;;; default error handler if page setups exist
(defun *error* (msg )

    	(setvar "cmdecho" 1)
	(command "y" "")
    	(command "y" "")
    	(command "y" "")
    	(command "y" "")
    	(command "y" "")
    	(command "y" "")


  
  	(command "-dwgunits" "" "" "" "Y" "Y")
	(command "filedia" "1")

    	(princ)
) ;_ end of defun

;;; --- add-default-layout-tabs Sub Function ---
;;; bring templates in if they exist
;;; Arguments
;;; none
;;; Alex Lundin 06-28-2017
(defun add-default-layout-tabs ( / FILECHECK TEXTF1 )

	(setvar "cmdecho" 0)
	(command "filedia" "0")

	(setq textf1 "C:\\WS_Blocks\\Default\\Templates\\Border Template 8.5X11 Wiring.dwg")
	(setq fileCheck (findfile textf1))
	(if
	  	(/= fileCheck nil)
	  	(progn
		(command "-dwgunits" "" "" "" "N" "N")
		(command "-layout" "template" "C:/WS_Blocks/Default/Templates/Border Template 8.5X11 Wiring.dwg" "8 X 11.5")

		(command "-dwgunits" "" "" "" "N" "N")
		(command "-layout" "template" "C:/WS_Blocks/Default/Templates/Border Template 8.5X11 Wiring.dwg" "11 X 17")
		  
		(command "-dwgunits" "" "" "" "N" "N")
		(command "-layout" "template" "C:/WS_Blocks/Default/Templates/Border Template 8.5X11 Wiring.dwg" "22 X 34")

		(command "-dwgunits" "" "" "" "N" "N")
		(command "-layout" "template" "C:/WS_Blocks/Default/Templates/Border Template 8.5X11 Wiring.dwg" "36 X 48")	  
		)
	  	(progn
		(princ "Templates not found, now exiting")
		(custom-exit)
		)
	)




)

(defun custom-exit ( )
	(quit)
)



;;; --- modelspace-viewport-info-list Sub Function ---
;;; Arguments
;;; modelSpaceViewPortPolylines 
;;; Alex Lundin 06-28-2017
(defun modelspace-viewport-info-list (
				      modelSpaceViewPortPolylines
				      /
					318LIST AUXILIARYPANELLIST CNTR CNTR318 CNTRAUXILIARY CNTRCOMPONENT CNTRMAIN CNTRPOE CNTRRS232 CNTRSECONDARY CNTRSTATIONBUS DXF10 DXF5 EN ENTDXF HANDLE MAINPANELLIST PANELCOMPONENTLIST POELIST RETURNLIST RS232LIST SECONDARYPANELLIST STATIONBUSLIST SUBLIST TRUENAME XCOORD YCOORD
					NEWLIST NUMBER X CNTRPANELPROCESSOR PANELPROCESSORLIST modelspaceviewportlist returnlist
				       )


	(setq cntr 0)													;initialize counter to 0 for while loop

  	;;; this loop organized all modelSpaceViewPortPolylines by x and y coordinates
  		(cond													;-conditional block
		  	((/= modelSpaceViewPortPolylines nil)								;-when modelSpaceViewPortPolylines are not nil
			(while 	(< cntr (sslength modelSpaceViewPortPolylines))						;set loop to continue while the counter varaible cntr is less that the number of objects in the selection set modelSpaceViewPortPolylines

			(setq en(ssname modelSpaceViewPortPolylines cntr))						;set variable en to the entity name of the block that matches the current counter value

		  	(setq entdxf (entget en))									;set entdxf to the dxf codes of the en entity
			(setq dxf5 (assoc 5 entdxf))									;set dxf5 to the 5th dxf code of entdxf
		  	(setq handle (cdr dxf5))									;set handle to the second element of dxf5
		  	(setq dxf10 (assoc 10 entdxf))									;set dxf10 to the 10th dxf code of entdxf
		  	(setq xcoord (nth 1 dxf10))									;set xcoord to the nth 1 of dxf10
		  	(setq ycoord (nth 2 dxf10))									;set ycoord to the nth 2 of dxf10
			

			(setq sublist (list handle xcoord ycoord en))							;create sublist for current item
			(setq modelspaceviewportlist (cons sublist modelspaceviewportlist))				;add sublist to mainlist, which accumulates during the loop
			(setq cntr (+ cntr 1))
			)
			)
		)
	  	
		(setq modelspaceviewportlist (vl-sort modelspaceviewportlist (function (lambda (x y)(< (nth 1 x)(nth 1 y))))))	;sort according to nth 4th element of the item, which is distance. Low to high
		(setq modelspaceviewportlist (vl-sort modelspaceviewportlist (function (lambda (x y)(> (nth 2 x)(nth 2 y))))))	;sort according to nth 4th element of the item, which is distance. Low to high	
		
		(setq returnlist modelspaceviewportlist)

)