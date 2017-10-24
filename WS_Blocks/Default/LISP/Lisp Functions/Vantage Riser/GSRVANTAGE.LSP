(defun *error* (msg)
(princ)
)

;;; --- c:msviewports ---
;;; creates layout tabs and viewports from modelspace rectangles generated from GRD function
;;; Arguments
;;; none
;;; Alex Lundin 06-28-2017
(defun c:msviewports
       	(
	
	/
	LAYOUT LAYOUTS *acad ACDOC  BLKS CENTER CMAX CMIN CNTR CNTRREVERSE CNTRSTRING CORNERPOINTSRECTANGLE CUR-LAYOUT-NAME CURR DXFRECTANGLE ENRECTANGLE FLAG FUNCTIONENDTIME FUNCTIONSTARTTIME HEIGHT LAYOUT LAYOUTS LAYTOCOPY NEW POINT1 POINT2 PREVIOUSAUTOMATICVPSETTING RECTANGLEC1 RECTANGLEC2 SORTEDMODELSPACEVIEWPORTS SORTEDMODELSPACEVIEWPORTSITEM SSRECTANGLES TIMEELAPSEDTIME TWIST VLA-LAYOUTS VP VP-SCALE WIDTH
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
	(setq *acad (vlax-get-acad-object))
	(setq acdoc (vla-get-ActiveDocument *acad))
	
  	(ms-add-default-layout-tabs acdoc)

	(setq vla-layouts (vla-get-layouts acdoc))

	;selection set of all modelspace rectangles to make into viewports
	(setq ssRectangles (ssget "x" '((0 . "LWPOLYLINE")(8 . "MS-Viewports")(410 . "MODEL"))))
	(setq functionStarttime (rtos (getvar "CDATE") 2 6))

  	(if
	  	(/= ssRectangles nil)
	  	(progn
		(setq sortedModelspaceViewports (modelspace-viewport-info-list ssRectangles))
		(setq sortedModelspaceViewports (reverse sortedModelspaceViewports))
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
		  

			
			(setq cur-layout-name  (strcat "Main Panel " cntrString "_22 X 34"))

			

			(setq
			laytocopy "22 X 34"
			blks nil
			twist 0.0
			vp-scale acVp1_2
			)


			;;; set current layout to copy from
		  	(setq curr (vla-item vla-layouts laytocopy))


		  	;;; add the new layout, define the center point
			(setq new (vla-add vla-layouts cur-layout-name))
			(vla-put-ActiveLayout acdoc new)

		  	(command "z" "e")
			(setq center (vlax-3d-point 15.7 11 0)
			width  31
			height 21
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
			(command "z" "e")
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
(defun ms-add-default-layout-tabs (adoc / FILECHECK TEXTF1 )

	(setvar "cmdecho" 0)
	(command "filedia" "0")
	(setq dwgname (vla-get-name adoc))
	(if
		(= dwgname "SL-VANTAGE.dwg")
		(progn
		(setq textf1 "C:\\WS_Blocks\\Default\\Templates\\SL-VANTAGE.dwg")
		(setq fileCheck (findfile textf1))
		(if
			(/= fileCheck nil)
			(progn

			(command "-dwgunits" "" "" "" "N" "N")
			(command "-layout" "template" "C:/WS_Blocks/Default/Templates/SL-VANTAGE.dwg" "22 X 34")
		  
			)
			(progn
			(princ "Templates not found, now exiting")
			(custom-exit)
			)
		)
		)
	)




)

(defun custom-exit ( )
	(quit)
)



;;; --- ms-modelspace-viewport-info-list Sub Function ---
;;; Arguments
;;; modelSpaceViewPortPolylines 
;;; Alex Lundin 06-28-2017
(defun ms-modelspace-viewport-info-list (
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


;;;08-16-2017
;;;gsrvantage
;;;generate vantage riser function
(defun c:gsrvantage 	( /
			     	customblockset drawinglocation file1 roomname textfileline vantageblock vantagemainlist
			     	vantagemainnumber vantagepanelsection vantagesecondarylist vantagesecondarynumber vantagesecondarypanelsection
			     	vantagestationbuslist vantagestationbusposition vantagestationbusrun vantagestationbussection vantagesublist
			 	vantagepanelcomponentlist vantagepanelcomponentsection
			 	vantage318list vantage318position vantage318section vantagedinlitelist vantagedinlitesection vantagers232list vantagers232section vantagestandalone
			 	vantagemainpanellist vantage318list vantagepanellist panelID VANTAGEPOEDEVICELIST VANTAGEPOEDEVICESECTION
			 PARSEDLISTOFSYSTEMS RETURNEDLIST LISTOFSYSTEMS *ACAD ACDOC BLOCKLIST DRAWINGNAME VANTAGEAUXILIARYNUMBER vantagepoeswitchlist VANTAGEPOEPOSITION vantagepoeswitchsection CONTROLLERBUSLAYER CONTROLLERBUSNOTE
			)




(setq *acad (vlax-get-acad-object))
(setq acdoc (vla-get-ActiveDocument *acad))

(setq drawingName (vl-filename-base (vla-get-fullname acdoc)))

  
;;;let user know if text file cannot be found
(if																			;-cond block
  
  	(/= drawingName "SL-VANTAGE")																	;-- cond statement if error handler for no text file
  	(progn
  	(alert
	(strcat
		"GSRVANTAGE must be called from a SL-VANTAGE template file."
		"\n"
		"\nExiting now."
		"\nClick OK."
	)
	)
	(princ "GSRVANTAGE must be called from a SL-Vantage template file.")
	(exit)
	)																		;-- end cond statement
)




			  
;;;open text file for reading
(setq file1 (open (strcat (getvar 'DWGPREFIX) "Vantage_Riser_Extraction.txt")  "r"))

;;;let user know if text file cannot be found
(if																			;-cond block
  
  	(= file1 nil)																	;-- cond statement if error handler for no text file
  	(progn
  	(alert
	(strcat
		"Vantage_Riser_Extraction.txt not found in folder with RD-DLM"
		"\nCreate a source of all blocks on the project, such as RISERLAYOUT."
		"\nThen run EBLOCKVANTAGE on that drawing to create the extraction."
		"\n"
		"\nExiting now."
		"\nClick OK."
	)
	)
	(princ "Vantage_Riser_Extraction.txt not found in folder with SL-Vantage")
	(exit)
	)																		;-- end cond statement
)




  
(setq vantagemainpanellist nil)
  ;;;	delete previous blocks on riser layer from any previous risers and purge
  
(deletepreviousriser)

;;;  	open text file for reading
  	(setq file1 (open (strcat (getvar 'dwgprefix) "Vantage_Riser_Extraction.txt")  "r"))

;;;  	let user know if text file cannot be found
  	(cond 	((= file1 nil)																	;- if error handler for no text file																		;-- progn
		(princ "Vantage_Riser_Extraction.txt not found in folder with SL-Vantage")
		(quit)
		)																		;-- end progn
	)																			;- end if

;;;  	read the first line of the text file in, which is the location of the drawing that was extracted from
  	(setq drawinglocation (read-line file1))

;;;  	read the next line of the text file in, which is the location of the drawing that was extracted from
  	(setq vantagepanelsection 0)
  	
  	(setq vantagestationbussection 0)
  

	(while
	  	(setq textfileline (read-line file1))
	  	(cond
		  	((= textfileline "Vantage Panels:")
			(setq vantagepanelsection 1)
			(setq vantagepanelcomponentsection 0)
			(setq vantagers232section 0)
	  		(setq vantagestationbussection 0)
			(setq vantagepoeswitchsection 0)
			(setq vantagepoedevicesection 0)
			(setq vantage318section 0)
			)
			((= textfileline "Vantage Panel Components:")
			(setq vantagepanelsection 0)
			(setq vantagepanelcomponentsection 1)
			(setq vantagers232section 0)
	  		(setq vantagestationbussection 0)
			(setq vantagepoeswitchsection 0)
			(setq vantagepoedevicesection 0)
			(setq vantage318section 0)
			)
			((= textfileline "Vantage RS232:")
			(setq vantagepanelsection 0)
			(setq vantagepanelcomponentsection 0)
			(setq vantagers232section 1)
	  		(setq vantagestationbussection 0)
			(setq vantagepoeswitchsection 0)
			(setq vantagepoedevicesection 0)
			(setq vantage318section 0)
			)
			((= textfileline "Vantage Station Bus:")
			(setq vantagepanelsection 0)
			(setq vantagepanelcomponentsection 0)
			(setq vantagers232section 0)
	  		(setq vantagestationbussection 1)
			(setq vantagepoeswitchsection 0)
			(setq vantagepoedevicesection 0)
			(setq vantage318section 0)
			)
			((= textfileline "Vantage POE Switches:")
			(setq vantagepanelsection 0)
			(setq vantagepanelcomponentsection 0)
			(setq vantagers232section 0)
	  		(setq vantagestationbussection 0)
			(setq vantagepoeswitchsection 1)
			(setq vantagepoedevicesection 0)
			(setq vantage318section 0)
			)
			((= textfileline "Vantage POE Devices:")
			(setq vantagepanelsection 0)
			(setq vantagepanelcomponentsection 0)
			(setq vantagers232section 0)
	  		(setq vantagestationbussection 0)
			(setq vantagepoeswitchsection 0)
			(setq vantagepoedevicesection 1)
			(setq vantage318section 0)
			)
			
			((= textfileline "Vantage 318:")
			(setq vantagepanelsection 0)
			(setq vantagepanelcomponentsection 0)
			(setq vantagers232section 0)
	  		(setq vantagestationbussection 0)
			(setq vantagepoeswitchsection 0)
			(setq vantagepoedevicesection 0)
			(setq vantage318section 1)
			)
			((= vantagepanelsection 1)
			(setq vantagemainnumber textfileline)
			(setq vantagesecondarynumber (read-line file1))
			(setq vantageauxiliarynumber (read-line file1))
			(setq vantageblock (read-line file1))
			(setq roomname (read-line file1))
			(setq panelID (read-line file1))
			(setq vantagesublist (list vantagemainnumber vantagesecondarynumber vantageauxiliarynumber vantageblock roomname panelID))							
			(setq vantagepanellist (cons vantagesublist vantagepanellist)) 
			(setq vantagesublist nil)															;reset the sublist when moving to the next block
			)
			((= vantagepanelcomponentsection 1)
			(setq vantagemainnumber textfileline)
			(setq vantagesecondarynumber (read-line file1))
			(setq vantageauxiliarynumber (read-line file1))
			(setq vantageblock (read-line file1))
			(setq roomname (read-line file1))
			(setq vantagesublist (list vantagemainnumber vantagesecondarynumber vantageauxiliarynumber vantageblock roomname))							
			(setq vantagepanelcomponentlist (cons vantagesublist vantagepanelcomponentlist)) 
			(setq vantagesublist nil)
			)
			((= vantagers232section 1)
			(setq vantagemainnumber textfileline)
			(setq vantagesecondarynumber (read-line file1))
			(setq vantageauxiliarynumber (read-line file1))
			(setq vantageblock (read-line file1))
			(setq roomname (read-line file1))
			(setq vantagesublist (list vantagemainnumber vantagesecondarynumber vantageauxiliarynumber vantageblock roomname))
			(setq vantagers232list (cons vantagesublist vantagers232list))
			(setq vantagesublist nil)
			)
			((= vantagestationbussection 1)
			(setq vantagemainnumber textfileline)
			(setq vantagesecondarynumber (read-line file1))
			(setq vantageauxiliarynumber (read-line file1))
			(setq vantageblock (read-line file1))
			(setq roomname (read-line file1))
			(setq vantagestationbusrun (read-line file1))
			(setq vantagestationbusposition (read-line file1))
			(setq vantagesublist (list vantagemainnumber vantagesecondarynumber vantageauxiliarynumber vantageblock roomname vantagestationbusrun vantagestationbusposition))							
			(setq vantagestationbuslist (cons vantagesublist vantagestationbuslist)) 
			(setq vantagesublist nil)
			)
			((= vantagepoeswitchsection 1)
			(setq vantagemainnumber textfileline)
			(setq vantagesecondarynumber (read-line file1))
			(setq vantageauxiliarynumber (read-line file1))
			(setq vantageblock (read-line file1))
			(setq roomname (read-line file1))
			(setq vantagesublist (list vantagemainnumber vantagesecondarynumber vantageauxiliarynumber vantageblock roomname))			 
			(setq vantagepoeswitchlist (cons vantagesublist vantagepoeswitchlist))
			(setq vantagesublist nil)
			)
			((= vantagepoedevicesection 1)
			(setq vantagemainnumber textfileline)
			(setq vantagesecondarynumber (read-line file1))
			(setq vantageauxiliarynumber (read-line file1))
			(setq vantageblock (read-line file1))
			(setq roomname (read-line file1))
			(setq vantagesublist (list vantagemainnumber vantagesecondarynumber vantageauxiliarynumber vantageblock roomname))			 
			(setq vantagepoedevicelist (cons vantagesublist vantagepoedevicelist))
			(setq vantagesublist nil)
			)
			((= vantage318section 1)
			(setq vantagemainnumber textfileline)
			(setq vantagesecondarynumber (read-line file1))
			(setq vantageauxiliarynumber (read-line file1))
			(setq vantageblock (read-line file1))
			(setq roomname (read-line file1))
			(setq vantagestationbusrun (read-line file1))
			(setq vantagestationbusposition (read-line file1))
			(setq vantage318position (read-line file1))
			(setq vantagesublist (list vantagemainnumber vantagesecondarynumber vantageauxiliarynumber vantageblock roomname vantagestationbusrun vantagestationbusposition vantage318position))			 
			(setq vantage318list (cons vantagesublist vantage318list))
			(setq vantagesublist nil)
			)
		)
	  
	  	
       )
	(close file1)
  	(if
	  	(/= vantagepanellist nil)
	  	(progn
		(setq vantagepanellist (vl-sort vantagepanellist (function (lambda (x y)(< (atoi (nth 2 x))(atoi (nth 2 y)))))))
		(setq vantagepanellist (vl-sort vantagepanellist (function (lambda (x y)(< (nth 1 x)(nth 1 y))))))
		(setq vantagepanellist (vl-sort vantagepanellist (function (lambda (x y)(< (atoi (car x))(atoi (car y)))))))
		;(vantagepanelbuilder vantagepanellist)
		)
	)
  	(if
	  	(/= vantagepanelcomponentlist nil)
	  	(progn
		(setq vantagepanelcomponentlist (vantagepanelcomponentsort vantagepanelcomponentlist))
	  	(setq vantagepanelcomponentlist (vl-sort vantagepanelcomponentlist (function (lambda (x y)(< (atoi (nth 2 x))(atoi (nth 2 y)))))))
	  	(setq vantagepanelcomponentlist (vl-sort vantagepanelcomponentlist (function (lambda (x y)(< (nth 1 x)(nth 1 y))))))
	  	(setq vantagepanelcomponentlist (vl-sort vantagepanelcomponentlist (function (lambda (x y)(< (atoi (car x))(atoi (car y)))))))
		
		
		;(vantagepanelcomponentbuilder vantagepanelcomponentlist)
		)
	)

  	(if
	  	(/= vantagers232list nil)
		(progn
	  	(setq vantagers232list (vl-sort vantagers232list (function (lambda (x y)(< (atoi (car x))(atoi (car y)))))))
	  	(setq vantagers232list (vl-sort vantagers232list (function (lambda (x y)(< (nth 1 x)(nth 1 y))))))
	  	(setq vantagers232list (vl-sort vantagers232list (function (lambda (x y)(< (atoi (nth 2 x))(atoi (nth 2 y)))))))
		;(vantagers232builder vantagers232list)
		)
	)

  	(if
	  	(/= vantagestationbuslist nil)
	  	(progn

		(setq vantagestationbuslist (vl-sort vantagestationbuslist (function (lambda (x y)(< (atoi (nth 5 x))(atoi (nth 5 y)))))))
		(setq vantagestationbuslist (vl-sort vantagestationbuslist (function (lambda (x y)(< (atoi (nth 6 x))(atoi (nth 6 y)))))))
		(setq vantagestationbuslist (vl-sort vantagestationbuslist (function (lambda (x y)(< (atoi (car x))(atoi (car y)))))))
		(setq vantagestationbuslist (vl-sort vantagestationbuslist (function (lambda (x y)(< (nth 1 x)(nth 1 y))))))
		(setq vantagestationbuslist (vl-sort vantagestationbuslist (function (lambda (x y)(< (atoi (nth 2 x))(atoi (nth 2 y)))))))		
	  	
	  	
		
		
		
		;(stationbussort vantagestationbuslist)
  		)
	)

  	(if
	  	(/= vantagepoeswitchlist nil)
	  	(progn

	  	(setq vantagepoeswitchlist (vl-sort vantagepoeswitchlist (function (lambda (x y)(< (atoi (car x))(atoi (car y)))))))

		)
	)
  	(if
	  	(/= vantagepoedevicelist nil)
	  	(progn

	  	(setq vantagepoedevicelist (vl-sort vantagepoedevicelist (function (lambda (x y)(< (atoi (car x))(atoi (car y)))))))

		)
	)	      
  	
    	(if
	  	(/= vantage318list nil)
	  	(progn
		(vantage318builder vantage318list)
		)
	)	
	(setq listOfSystems (list vantagepanellist vantagepanelcomponentlist vantagestationbuslist vantagers232list vantagepoeswitchlist vantagepoedevicelist))
  	(setq returnedlist (vantageparsewrapper listOfSystems))
	;(setq returnedlist (vantageparser vantagepanellist vantagepanelcomponentlist vantagestationbuslist vantagers232list))
  	(setq parsedListOfSystems returnedlist)
  	
  	(setq returnedList (vantageblockbuilder parsedListOfSystems))
  	(setq blockList returnedList)
  	;;; set note block names
	(setq controllerbusnote "_V_HEX_NOTE_6")


	;;; set layer names
	(setq controllerbuslayer "_WS_CONTROLLER_BUS")

  	(vantageBlockInserter blockList CONTROLLERBUSLAYER CONTROLLERBUSNOTE)
  	(draworder)
   	(c:msviewports)
  	(c:wswp)
  	(setvar "ctab" "Model")
(vlax-release-object *acad acdoc)  	
(princ)
)

(defun vantageBlockInserter ( blockList CONTROLLERBUSLAYER CONTROLLERBUSNOTE / CONTROLLERBUSCONNECTIONFLAG  MIDPOINTOFCONTROLLERBUS BLOCKHEIGHT BLOCKNAME BLOCKWIDTH CMAX CNTR INSERTIONPOINT ITEM XCOORD YCOORD MAINCABLEPOINT1 MAINCABLEPOINT2 YCOORDPREVIOUS)


	(setq cntr 0)
	(setq cmax (length blockList))
	(setq xcoord 0)
  	(setq ycoord 0)
  	(while
	  	(< cntr cmax)
		(setq item (nth cntr blockList))
		(setq blockName (nth 0 item))
	  	(setq blockWidth (nth 1 item))
	  	;(setq blockWidth (atoi blockWidth))
	  	(setq blockHeight (nth 2 item))
	  	;(setq blockHeight (atoi blockHeight))
	  	(setq ycoord (+ ycoord blockHeight))
	  	(setq insertionPoint (list xcoord ycoord))
		      
  		(entmakex											;entmakex function
		(list												;list of all required items
		(cons 0 "INSERT")										;type of entity
	        (cons 2 blockname)										;name of block to insert
		(cons 8 "Riser")
	       	(cons 10 insertionPoint)									;block insertion point
	       	(cons 41 1)											; Scale Factor
	       	(cons 42 1)											; Scale Factor?
	      	(cons 43 1)											; Scale Factor?
		)
		)
	  

	  	(setq controllerBusConnectionFlag (vl-string-search "CB" blockName 1))
	  
	  	;;; draw controller bus connection
	  	(if
		  	(AND
			(> cntr 0)(/= controllerBusConnectionFlag nil)
			)
		  	(progn

			;;; horizontal line
			(setq maincablepoint1 (list -1.75 ycoord))
			(setq maincablepoint2 (list -1.75 ycoordPrevious))
			(entmakex
			(list '
			(0 . "line")
			(cons 8 controllerbuslayer)      
			(cons 10 maincablepoint1)
			(cons 11 maincablepoint2)
			)
			)

			(setq maincablepoint1 (list -1.75 (+ ycoordPrevious 2.5850)))
			(setq midpointOfControllerBus (midpoint maincablepoint1 maincablepoint2))

			;;; note
			(entmakex													;entmakex function
			(list														;list of all required items
			(cons 0 "insert")												;type of entity
		        (cons 2 controllerbusnote)											;name of block to insert
			(cons 8 controllerbuslayer)											;block layer
		       	(cons 10 midpointOfControllerBus)												;block insertion point
		       	(cons 41 1)													; scale factor
		       	(cons 42 1)													; scale factor?
		      	(cons 43 1)													; scale factor?
			)
			)

			)
		)
	  	(setq ycoordPrevious ycoord)
	  	(setq cntr (+ cntr 1))
	)
  
)
(defun vantageparsewrapper (listOfSystems / PARSEDLISTOFSYSTEMS PARSEDSYSTEM panels components rs232 stationbus returnlist CMAX CNTR CURRENTMAINNUMBER ITEM MAINNUMBER SUBLIST CMAX2 CNTR2 CURRENTMAINASINT MAINNUMBERASINT NUMBEROFMAINS SUBLIST2)
  
	(setq cntr 0)
	(setq cmax (length listOfSystems))

  	(while
	  	(< cntr cmax)
		(setq item (nth cntr listOfSystems))

	  	(setq parsedSystem (vantageparser item))
	  	(setq parsedListOfSystems (cons parsedSystem parsedListOfSystems))

	  	(setq cntr (+ cntr 1))
	)
  
  	(setq parsedListOfSystems (reverse parsedListOfSystems))

  	(setq returnlist parsedListOfSystems)
)

(defun vantageparser (inputList / CMAX CMAX2 CNTR CNTR2 CURRENTMAINASINT CURRENTMAINNUMBER ITEM MAINNUMBER MAINNUMBERASINT OUTPUTLIST SUBLIST SUBLIST2)


	(setq cntr 0)
	(setq cmax (length inputList))
  	;;; start mains at 0
	(setq mainNumber "1")
  	(while
	  	(< cntr cmax)
		(setq item (nth cntr inputList))
	  	(setq currentMainNumber (nth 0 item))

		(setq currentMainAsInt (atoi currentMainNumber))
		(setq mainNumberAsInt (atoi mainNumber))


	  
		;;; if currentMainAsInt matches the mainNumberAsInt
	  	(if
		  	(= currentMainAsInt mainNumberAsInt)
		  	(progn
			(setq sublist (cons item sublist))
			)
		)
	  
		;;; if currentMainAsInt is greater than the mainNumberAsInt
	  	(if
		  	(> currentMainAsInt mainNumberAsInt)
		  	(progn

			;;; if sublist exists, it contains everything from the last mainNumber
			;;; add it to the outputlist
			(if
			  	(/= sublist nil)
			  	(progn
				(setq sublist (reverse sublist))
				(setq outputList (cons sublist outputList))
				(setq sublist nil)
				)

			)

			;;; add blank lists as placeholders when there is a gap between the main number of the input list
			(setq cntr2 1)
			(setq cmax2 (- currentMainAsInt mainNumberAsInt))
			(if
			  	(>= cmax2 cntr2)
			  	(progn
				(while
				  	(< cntr2 cmax2)
					(setq sublist2 nil)
				  	(setq outputList(cons sublist2 outputList))
				  	(setq cntr2 (+ cntr2 1))
				)
				)



			)

			;;; now any blank list placeholders are padding the outputlist
			;;; add the item to the sublist which accumulates until the currentMainAsInt is greater than the mainNumberAsInt
			(setq sublist (cons item sublist))
			(setq mainNumber (nth 0 item))
			(setq mainNumberAsInt (atoi mainNumber))			
			
			
			)
		)

	  	(setq cntr (+ cntr 1))
	)

	;;; add final sublist after loops if it exists
	(if
	  	(/= sublist nil)
	  	(progn
		(setq sublist (reverse sublist))
		(setq outputList (cons sublist outputList))
		(setq sublist nil)

		)
	)
	(setq outputList (reverse outputList))

  
)

(defun vantageblockbuilder ( listOfLists / CAT5ENOTE CAT5EPOELAYER CAT5EPOENOTE POEDEVICELISTS POEDEVICES POESWITCHES POESWITCHLISTS VANTAGEPOEMAINYCOORDSHIFT VANTAGEPOESWITCHESMAINXCOORDSHIFT VANTAGEPOESWITCHESMAINXCOORDSTART VANTAGEPOESWITCHESMAINYCOORDSHIFT
18AWGLAYER 20AWGLAYER 318LAYER 318NOTE ALLBLOCKITEMS ALLPRODUCTSFORINSERTEDBOMSORTED BLOCKHEIGHTMIN BLOCKMAXXCOORD BLOCKMAXYCOORD BLOCKMINXCOORD BLOCKMINYCOORD BLOCKNAMESONLY BLOCKWIDTHMAX BLOCKXCOORDBUFFER BLOCKYCOORDBUFFER CAT5ELAYER CLASS CMAX2 CNTR2 CONTROLLERBUSLAYER CONTROLLERBUSNOTE ENCLOSUREBUSLAYER ENCLOSUREBUSNOTE MSTPLAYER PLLAYER PLLINETYPE PLWIDTH PRODUCTITEM PRODUCTNAME PRODUCTQTY PTLIST RS232LAYER RS232NOTE RS232NOTEXCOORDSHIFT STATIONBUSLAYER STATIONBUSNOTE TEXTSTRING VANTAGE318AUXILIARYYCOORDSHIFT VANTAGE318MAINYCOORDSHIFT VANTAGE318SECONDARYYCOORDSHIFT VANTAGE32PANELHEIGHT VANTAGE44PANELHEIGHT VANTAGEAUXILIARYPANELXCOORDSHIFT VANTAGELVOSSHIFT VANTAGEMAINPANELXCOORDSHIFT VANTAGEMAINPANELYCOORDSHIFT VANTAGEMODULEDROPFROMPANELTOP VANTAGEMODULESHIFT VANTAGEPANELWIDTH VANTAGEPOEMAINXCOORDSHIFT VANTAGEPOEMAINXCOORDSTART VANTAGEPOEMAINYCOORDSTART VANTAGERS232AUXILIARYXCOORDSHIFT VANTAGERS232MAINXCOORDSHIFT VANTAGERS232SECONDARYXCOORDSHIFT VANTAGESECONDARYPANELXCOORDSHIFT VANTAGESECONDARYPANELYCOORDSHIFT VANTAGESTATIONBUSAUXILIARYXCOORDSHIFT VANTAGESTATIONBUSAUXILIARYXCOORDSTART VANTAGESTATIONBUSAUXILIARYYCOORDSHIFT VANTAGESTATIONBUSAUXILIARYYCOORDSTART VANTAGESTATIONBUSMAINXCOORDSHIFT VANTAGESTATIONBUSMAINXCOORDSTART VANTAGESTATIONBUSMAINYCOORDSHIFT VANTAGESTATIONBUSMAINYCOORDSTART VANTAGESTATIONBUSSECONDARYXCOORDSHIFT VANTAGESTATIONBUSSECONDARYXCOORDSTART VANTAGESTATIONBUSSECONDARYYCOORDSHIFT VANTAGESTATIONBUSSECONDARYYCOORDSTART VIEWPORTLAYER VIEWPORTPOINT1 VIEWPORTPOINT2 VIEWPORTPOINT3 VIEWPORTPOINT4 VIEWPORTPOINT5
			    RETURNEDLIST BLOCKHEIGHT BLOCKWIDTH CMAX CNTR LISTOFSYSTEMS PANELCOMPONENTLISTS PANELLISTS RS232LISTS STATIONBUSLISTS BLOCKNAME CNTRASSTRING COMPONENTS CONNECTIONTYPE INSERTIONPOINT LISTOFBLOCKS PANELS RETURNLIST STATIONBUS SUBLIST XCOORD YCOORD)

	  ;;;global to
	;;;  	vantagemainbuilder:
	;;;  	vantagemainlist
	;;;  	vantagemainbuilder:
	;;;  	vantagesecondarylist
	;;;	vantagestationbusbuilder
	;;;  	vantagestationbuslist
	(setq vantagemainpanelxcoordshift 5)
	(setq vantagemainpanelycoordshift 42)

	(setq vantagesecondarypanelxcoordshift 6)
	(setq vantagesecondarypanelycoordshift 14)

	(setq vantageauxiliarypanelxcoordshift 8)
	  
	(setq vantage44panelheight 11)
	(setq vantage32panelheight 6)
	(setq vantagepanelwidth 3.5)
	  
	(setq vantagemoduledropfrompaneltop 1)
	(setq vantagemoduleshift 1.4)
	(setq vantageLVOSshift 0.9)
	;;; these define the starts for the bus runs
	(setq vantagestationbusmainxcoordstart 1.45)
	(setq vantagestationbusmainycoordstart 3.8)
  	(setq VANTAGESTATIONBUSMAINXCOORDSHIFT 4.0)
	;;; don't edit these, they are calculated from the intial values
	(setq vantagestationbusauxiliaryxcoordstart (+ (* vantagesecondarypanelxcoordshift 2) vantageauxiliarypanelxcoordshift))
	(setq vantagestationbusauxiliaryycoordstart vantagesecondarypanelycoordshift)
	;;; don't edit these, they are calculated from the intial values
	(setq vantagestationbussecondaryxcoordstart (+ (* vantagesecondarypanelxcoordshift 2) vantageauxiliarypanelxcoordshift))
	(setq vantagestationbussecondaryycoordstart (* vantagesecondarypanelycoordshift 2))

	  
	;;; individual shift values for each run
	(setq vantagestationbusmainxcoordshift 4)
	(setq vantagestationbussecondaryxcoordshift 4)
	(setq vantagestationbusauxiliaryxcoordshift 4)
	(setq vantagestationbusmainycoordshift 2.5850)
	(setq vantagestationbussecondaryycoordshift 7)
	(setq vantagestationbusauxiliaryycoordshift 7)


	;;; the xcoordshift for 318 is calculated inside the builder function from the set station bus values above.
	(setq vantage318mainycoordshift 1.4)  
	(setq vantage318secondaryycoordshift 1.4)
	(setq vantage318auxiliaryycoordshift 1.4)


	;;; the xcoordshift for RS232, controls how far item offsets from panel
	(setq vantagers232mainxcoordshift 4.0)  
	(setq vantagers232secondaryxcoordshift 4.0)
	(setq vantagers232auxiliaryxcoordshift 4.0)
	(setq rs232notexcoordshift 1.4)

	;;; the shift values for POE
	
	(setq vantagepoeswitchesmainxcoordstart 1.45)
	(setq vantagepoeswitchesmainycoordstart 3.8)
	(setq vantagepoeswitchesmainxcoordshift 4.0)
  	(setq vantagepoeswitchesmainYcoordshift 2.5850)
  
	;;; set note block names
	(setq controllerbusnote "_V_HEX_NOTE_6")
	(setq enclosurebusnote "_V_HEX_NOTE_7")
	(setq stationbusnote "_V_HEX_NOTE_8")
	(setq 318note "_V_HEX_NOTE_9")
	(setq CAT5EPOEnote "_V_HEX_NOTE_10")
  	(setq CAT5Enote "_V_HEX_NOTE_3")

	;;; set layer names
	(setq controllerbuslayer "_WS_CONTROLLER_BUS")
	(setq enclosurebuslayer "_WS_ENCLOSURE_BUS")
	(setq stationbuslayer "_WS_STATION_BUS")
	(setq 318layer "_WS_3_18_AWG")
	(setq rs232layer "_WS_RS232")
	(setq mstplayer "_WS_MSTP")
	(setq 18awglayer "_WS_18_AWG")
	(setq 20awglayer "_WS_20_AWG")
	(setq cat5elayer "_WS_CAT5E")
  	(setq cat5ePOElayer "_WS_CAT5E_POE")
  	(setq ViewPortLayer "MS-Viewports")
  	(setq panelLists (nth 0 listOfLists))
  	(setq panelComponentLists (nth 1 listOfLists))
  	(setq stationBusLists (nth 2 listOfLists))
  	;(setq rs232Lists (nth 3 listOfLists))
  	(setq poeswitchlists (nth 4 listOfLists))
  	(setq poedevicelists (nth 5 listOfLists))
	(setq cntr 0)
  	(setq cmax (length panelLists))

  
  	(while
	  	(< cntr cmax)


	  
		(setq blockHeight 0)
	  	(setq blockWidth 0)
		(setq cntrAsString (itoa (+ cntr 1)))


	  	(setq xcoord 0)
		(setq ycoord 0)
		(setq insertionPoint (list xcoord ycoord))	  
	  	;;; CB		- controller bus only
	  	;;; EB		- ethernet bus only (LCAP32L)
	  	;;; CB+EB	- controlelr bus and ethernet bus
		(setq connectionType "CB")
	  	(setq blockName (strcat "Z-" connectionType "-" cntrAsString))								;create string of block
		;;; BLOCK Header definition starts here:
		(entmake (list (cons 0 "BLOCK")(cons 2 blockName)(cons 70 2)(cons 10 insertionPoint)))					;begin block definition


	  
		(setq panels (nth cntr panelLists))
		(setq components (nth cntr panelComponentLists))
		(setq stationBus (nth cntr stationBusLists))
	  	(setq poeswitches (nth cntr poeswitchlists))
	  	(setq poedevices (nth cntr poedevicelists))
	  
		(setq AllBlockItems (append panels components stationBus poeswitches poedevices))
	  	(setq returnList (vantage-get-4th AllBlockItems))
	  	(setq BlockNamesOnly returnList)
	  	;;;bom here
	  	(setq returnList (vantage-combine-items-into-quantity-list BlockNamesOnly))
	  	(setq allProductsForInsertedBomSorted returnList)
	  
	  	(setq cntr2 0)
	  	(setq cmax2 (length allProductsForInsertedBomSorted))
	  	(setq xcoord -12.5)
	  	(setq ycoord 1.6)
	  	(setq blockMaxYcoord (+ ycoord 0.285))
	  	(while
		  	(< cntr2 cmax2)


		  	;;; format bill of materials header
		  	(if
			  	(= cntr2 0)
			  	(progn
			  	(setq insertionPoint (list xcoord ycoord))
			  	(setq textString (strcat "Connect Each Main Panel System with Controller Bus\\PThis is Main Panel System " cntrAsString " :"))
				(entmake
				(list
				(cons 0 "MTEXT")         		;; Entity Name
				(cons 100 "AcDbEntity")  		;; Subclass Marker
				(cons 410 "Model")       		;; Space
				(cons 8 "Riser")   				;; Layer
				(cons 100 "AcDbMText")   		;; Subclass Marker
				(cons 10 insertionPoint) 		;; Insertion Point
				(cons 40 0.24)            		;; Text Height
				(cons 71 1)              		;; Attachment Point (top-center)
				(cons 1 textString)    			;; Text Content
				(cons 7 "Arial")			;; text style
				)
				)

				(setq ycoord (- ycoord 1.2))
				)
			)




		  
			(setq productItem (nth cntr2 allProductsForInsertedBomSorted))
			(setq productName (car productItem))
		  	(setq productQTY (cdr productItem))
		  	(setq productQTY (itoa productQTY))
		  	(setq insertionPoint (list xcoord ycoord))

			(entmake
			(list
			(cons 0 "MTEXT")         		;; Entity Name
			(cons 100 "AcDbEntity")  		;; Subclass Marker
			(cons 410 "Model")       		;; Space
			(cons 8 "Riser")         		;; Layer
			(cons 100 "AcDbMText")   		;; Subclass Marker
			(cons 10 insertionPoint) 		;; Insertion Point
			(cons 40 0.24)            		;; Text Height
			(cons 71 1)              		;; Attachment Point (top-center)
			(cons 1 productQTY)    			;; Text Content
			(cons 7 "Arial")			;; text style
			)
			)
		  
		  	(setq xcoord (+ xcoord 0.6))
		  	(setq insertionPoint (list xcoord ycoord))
		  	

			(entmake
			(list
			(cons 0 "MTEXT")         		;; Entity Name
			(cons 100 "AcDbEntity")  		;; Subclass Marker
			(cons 410 "Model")       		;; Space
			(cons 8 "Riser")         		;; Layer
			(cons 100 "AcDbMText")   		;; Subclass Marker
			(cons 10 insertionPoint) 		;; Insertion Point
			(cons 40 0.24)            		;; Text Height
			(cons 71 1)              		;; Attachment Point (top-center)
			(cons 1 productName)    		;; Text Content
			(cons 7 "Arial")			;; text style
			)
			)
		  	(setq xcoord (- xcoord 0.6))
		  	(setq ycoord (- ycoord 0.4))
		  
			(setq cntr2 (+ cntr2 1))

	 	)	  
	  


		(setq blockMinXcoord (- xcoord 1.0))
		
	  
		
	  	(setq returnedlist (vantagepanelbuilder panels ENCLOSUREBUSLAYER ENCLOSUREBUSNOTE VANTAGEAUXILIARYPANELXCOORDSHIFT VANTAGEPANELWIDTH VANTAGESECONDARYPANELXCOORDSHIFT VANTAGESECONDARYPANELYCOORDSHIFT))
	  	(setq blockWidth (nth 0 returnedlist))
	  	(setq blockHeight (nth 1 returnedlist))
		(setq blockWidthMax blockWidth)
	  	(setq blockHeightMin blockHeight)


	  	
	  	(if
		  	(/= components nil)
		  	(progn
	  		(vantagepanelcomponentbuilder components VANTAGEAUXILIARYPANELXCOORDSHIFT VANTAGELVOSSHIFT VANTAGEMODULEDROPFROMPANELTOP VANTAGEMODULESHIFT VANTAGESECONDARYPANELXCOORDSHIFT VANTAGESECONDARYPANELYCOORDSHIFT )
			)
		)

	  	
	  
	  	(if
		  	(/= stationBus nil)
		  	(progn
			  
		  	(setq returnedlist (vantagestationbusbuilder stationBus blockHeight STATIONBUSLAYER STATIONBUSNOTE VANTAGESTATIONBUSMAINXCOORDSTART VANTAGESTATIONBUSMAINXCOORDSHIFT VANTAGESTATIONBUSMAINYCOORDSHIFT ))
		  	(setq blockWidth (nth 0 returnedlist))
		  	(setq blockHeight (nth 1 returnedlist))
		  	(if
			  	(> blockWidth blockWidthMax)
			  	(progn
				(setq blockWidthMax blockWidth)
				)
			)
		  	(if
			  	(< blockHeight blockHeightMin )
			  	(progn
				(setq blockHeightMin blockHeight)
				)
			)		  	
			  
			)
		)

	  	(if
		  	(OR 
			(/= poeswitches nil)(/= poedevices nil)
			)
		  	(progn
			  
		  	(setq returnedlist (vantagePOEbuilder poeswitches poedevices blockHeight cat5elayer cat5ePOElayer cat5eNOTE CAT5EPOEnote vantagepoeswitchesmainxcoordstart vantagepoeswitchesmainxcoordshift vantagepoeswitchesmainYcoordshift))
		  	(setq blockWidth (nth 0 returnedlist))
		  	(setq blockHeight (nth 1 returnedlist))
		  	(if
			  	(> blockWidth blockWidthMax)
			  	(progn
				(setq blockWidthMax blockWidth)
				)
			)
		  	(if
			  	(< blockHeight blockHeightMin )
			  	(progn
				(setq blockHeightMin blockHeight)
				)
			)		  	
			  
			)
		)
	  
	  	(setq blockMaxXcoord (+ blockWidthMax 4.0))
	  	(setq blockMinYcoord (- blockHeightMin 0.285))

	  
	  	(setq viewPortPoint1 (list blockMaxXcoord blockMinYcoord))
	  	(setq viewPortPoint2 (list blockMaxXcoord blockMaxYcoord))
	  	(setq viewPortPoint3 (list blockMinXcoord blockMaxYcoord))
	  	(setq viewPortPoint4 (list blockMinXcoord blockMinYcoord))
	  	(setq viewPortPoint5 (list blockMaxXcoord blockMinYcoord))
	  
		(setq ptList (list viewPortPoint1 viewPortPoint2 viewPortPoint3 viewPortPoint4 viewPortPoint5 ))
		(setq class 0)
		(setq plLayer ViewPortLayer)
		(setq plWidth 0)
		(setq plLinetype "bylayer")
		(vantage-riser-draw-lwpolyline ptList class plLayer plWidth plLinetype)
	  
	  	;(setq stationBus (nth cntr stationBusLists))
	  	;(setq rs232 (nth cntr rs232Lists))
	 
		(setq blockXcoordBuffer 5)
	  	(setq blockWidthMax (+ blockWidthMax blockXcoordBuffer))
	  
	  	(setq blockHeightMin (abs blockHeightMin))
		(setq blockYcoordBuffer 5)
	  	(setq blockHeightMin (+ blockHeightMin blockYcoordBuffer))

		;;; note
		(entmakex													;entmakex function
		(list														;list of all required items
		(cons 0 "insert")												;type of entity
	        (cons 2 "_WIRE_LEGEND_2")											;name of block to insert
		(cons 8 "Riser")											;block layer
	       	(cons 10 viewPortPoint4)											;block insertion point
	       	(cons 41 1)													; scale factor
	       	(cons 42 1)													; scale factor?
	      	(cons 43 1)													; scale factor?
		)
		)
	  
		(entmake
		(list
		(cons 0 "ENDBLK")
		)
		)  															;finish block defition

	  	(setq sublist (list blockName blockWidthMax blockHeightMin))
	  	(setq listOfBlocks (cons sublist listOfBlocks))
		(setq cntr (+ cntr 1))
	)
	(setq listOfBlocks (reverse listOfBlocks)) 	
	(setq returnlist listOfBlocks)
)



(defun vantage-get-4th (inputlist / outputList 4TH CMAX CNTR ITEM OUTPUTLIST)
	(setq cntr 0)
  	(setq cmax (length inputlist))

  
  	(while
	  	(< cntr cmax)

		(setq item (nth cntr inputlist))
		(setq 4th (nth 3 item))
		(setq outputList( cons 4th outputList))
		(setq cntr (+ cntr 1))
	)
	(setq returnList outputList)
)
  
(defun vantage-combine-items-into-quantity-list ( nameList / LISTNEWLENGTH LISTTOTALLENGTH OUTLIST RETURNLIST )


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


    (defun *error* ( msg )
      	(close file1)
        (princ)
    )

(defun deletepreviousriser	(
				/
				selection
				)
  
  	(setq xcoord 0)
  	(setq ycoord 0)
  	(setq insertionpoint (list xcoord ycoord))
  
	(setq blocks (ssget "x" '((8 . "Riser" ))))													;selection set
  	(setq panels (ssget "x" '((8 . "Panels"))))
	(setq controllerbuslines (ssget "x" '((8 . "_WS_CONTROLLER_BUS" ))))
	(setq enclosurebuslines (ssget "x" '((8 . "_WS_ENCLOSURE_BUS" ))))
	(setq stationbuslines (ssget "x" '((8 . "_WS_STATION_BUS" ))))
	(setq 318lines (ssget "x" '((8 . "_WS_3_18_AWG" ))))
	(setq rs232lines (ssget "x" '((8 . "_WS_RS232" ))))
	(setq mstplines (ssget "x" '((8 . "_WS_MSTP" ))))
	(setq 18awglines (ssget "x" '((8 . "_WS_18_AWG" ))))
	(setq 20awglines (ssget "x" '((8 . "_WS_20_AWG" ))))
	(setq cat5elines (ssget "x" '((8 . "_WS_CAT5E" ))))
	(setq MSVIEWPORTS (ssget "x" '((8 . "MS-Viewports" ))))
	
  	(setq total (acet-ss-union (list MSVIEWPORTS blocks panels controllerbuslines enclosurebuslines stationbuslines 318lines rs232lines mstplines 18awglines 20awglines cat5elines)))	;add the new [setname] to this line (setq total (acet-ss-union (list blocks cable ohm matchline [setname])))	
	(if
	  	(/= total nil)
	  	(progn
		(command "copybase" insertionpoint total "")
  		(command "erase" "p" "")
		)
	)
)
(defun draworder		(
				/
				lines PANELS  blocks ss
				)
	(setq ss (ssget "x" '((0 . "INSERT" )(8 . "Riser" ))))

		(cond
		  	((/= ss  nil)

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
  	(setq panels (ssget "x" '((8 . "Panels" ))))
  		(cond
		  	((/= panels nil)
			(command "draworder" "p" "" "BACK")
			)
		)
	(setq lines (ssget "x" '((0 . "line" ))))

		(cond
		  	((/= lines nil)
			(command "draworder" "p" "" "BACK")
			)
		)
  
 	(setq plines (ssget "x" '((0 . "lwpolyline" ))))

		(cond
		  	((/= plines nil)
			(command "draworder" "p" "" "BACK")
			)
		)
  

  
  	(command "regenall")

)


(defun vantagepanelcomponentsort
       				(
				vantagepanelcomponentsortlist
				/
				VPCSCMAX VPCSCNTR VPCSITEM VPCSROOMNAME VPCSVANTAGEAUXILIARYNUMBER VPCSVANTAGEBLOCK VPCSVANTAGEMAINNUMBER VPCSVANTAGESECONDARYINTEGER VPCSVANTAGESECONDARYNUMBER VPCSXCOORD VPCSYCOORD VPCSPLACEMENTBIT
				RETURNLIST VPCSLVOSLIST VPCSMODULEPOSITION1LIST VPCSMODULEPOSITION2LIST VPCSMODULEPOSITION3LIST VPCSMODULEPOSITION4LIST VPCSMODULEPOSITION5LIST vpcsRemainderList 
				)

  				(setq vpcsxcoord 0)
				(setq vpcsycoord 0)	
  				(setq vpcscntr 0)
  				(setq vpcscmax (length vantagepanelcomponentsortlist))
  				(while	(< vpcscntr vpcscmax)



				  
					(setq vpcsitem (nth vpcscntr vantagepanelcomponentsortlist))
					(setq vpcsvantagemainnumber (nth 0 vpcsitem))
					(setq vpcsvantagemainnumber (atoi vpcsvantagemainnumber))
				  	(setq vpcsvantagesecondarynumber (nth 1 vpcsitem))
					;;; secondary is integer and character together in a string, so atoi first item which is the integer
				  	(setq vpcsvantagesecondaryinteger (substr vpcsvantagesecondarynumber 1 1))
				  	(setq vpcsvantagesecondaryinteger (atoi vpcsvantagesecondaryinteger))
				  	(setq vpcsvantageauxiliarynumber (nth 2 vpcsitem))
				  	(setq vpcsvantageauxiliarynumber (atoi vpcsvantageauxiliarynumber))	
					(setq vpcsvantageblock (nth 3 vpcsitem))
					(setq vpcsroomname (nth 4 vpcsitem))

					(setq vpcsPlacementBit 0)
				  	;;; component heirarchy
				  	;;; lvos first
				  	;;; hottest modules up top
					(if
					  	(OR
						;lvos
						(= vpcsvantageblock "V-LVOS-0-10-PWM-P-1")
						)
					  	(progn
						
						(setq vpcsLvosList(cons vpcsitem vpcsLvosList))
						(setq vpcsPlacementBit 1)
						)
					)

				  
					(if
					  	(OR
						;modules
						(= vpcsvantageblock "V-UDM08-EM")
						)
					  	(progn
						(setq vpcsModulePosition1List(cons vpcsitem vpcsModulePosition1List))
						(setq vpcsPlacementBit 1)
						)
					)
					(if
					  	(OR
						;modules
						(= vpcsvantageblock "V-SDM12-EM")
						)
					  	(progn
						(setq vpcsModulePosition2List (cons vpcsitem vpcsModulePosition2List))
						(setq vpcsPlacementBit 1)
						)
					)					
					(if
					  	(OR
						;modules
						(= vpcsvantageblock "V-FANMOD")
						)
					  	(progn
						(setq vpcsModulePosition3List(cons vpcsitem vpcsModulePosition3List))
						(setq vpcsPlacementBit 1)
						)
					)
					(if
					  	(OR
						;modules
						(= vpcsvantageblock "V-MDR8CW301")
						)
					  	(progn
						(setq vpcsModulePosition4List(cons vpcsitem vpcsModulePosition4List))
						(setq vpcsPlacementBit 1)
						)
					)
					(if
					  	(OR
						;modules
						(= vpcsvantageblock "V-STPSRW101")
						(= vpcsvantageblock "V-STPSRW201")
						(= vpcsvantageblock "V-STPERW101")
						(= vpcsvantageblock "V-STPERW201")
						)
					  	(progn
						(setq vpcsModulePosition5List(cons vpcsitem vpcsModulePosition5List))
						(setq vpcsPlacementBit 1)
						)
					)				  

				  	(if
					  	(/= vpcsPlacementBit 1)
					  	(progn
						(setq vpcsRemainderList (cons vpcsitem vpcsRemainderList))
						(setq vpcsPlacementBit 1)
						)
					)
				  	(setq vpcscntr (+ 1 vpcscntr))
				)
  

				(setq returnList(append vpcsLvosList vpcsModulePosition1List vpcsModulePosition2List vpcsModulePosition3List vpcsModulePosition4List vpcsModulePosition5List vpcsRemainderList))
	
  
)



(defun vantagePOEbuilder
       				(
				poeSwitchRun poedeviceRun ycoordStart cat5elayer cat5ePOElayer cat5eNOTE CAT5EPOEnote vantagepoeswitchesmainxcoordstart vantagepoeswitchesmainxcoordshift  vantagepoeswitchesmainYcoordshift 
				/
				vpoebblockname vpoebcmax vpoebcntr vpoebinsertionpoint vpoebitem vpoebmainprevious DROPBITCHECK VMBXCOORD XCOORDLIMITMAX XCOORDLIMITMIN YCOORDROOMID
				vpoebp1 vpoebp2 vpoebp3 vpoebp4 vpoebp5 vpoebroomname vpoebstationbuscablepoint1 vpoebstationbuscablepoint2
				vpoebpoeSwitchRunprevious vpoebvantageblock vpoebvantagemainnumberAsInteger vpoebvantagestationbuspositionAsInteger vpoebvantagepoeSwitchRunAsInteger
				vpoebxcoord vpoebxcoordprevious vpoebxshift vpoebycoord vpoebycoordprevious vpoebycoordshift
				VSBBAUXILIARYPREVIOUS VSBBSECONDARYPREVIOUS VSBBSUBLIST vpoebvantageauxiliarynumberAsInteger vpoebvantageauxiliarynumberAsIntegerSTRING VSBBVANTAGECONNECTIONBLOCK vpoebvantagesecondarynumberAsInteger VSBBVANTAGESECONDARYLETTER vpoebvantagesecondarynumberAsInteger vpoebvantagesecondarynumberAsIntegerCHECK vpoebvantagesecondarynumberAsIntegerSTRING VSBBXCOORDSHIFT
				vpoebxcoordMax vpoebycoordMin returnlist
				VSBBAUXILIARYNUMBERASINTEGERPREVIOUS VSBBMAINNUMBERASINTEGERPREVIOUS 
				VSBBMIDPOINT VSBBSECONDARYNUMBERASINTEGERPREVIOUS VSBBSTATIONBUSRUNASINTEGERPREVIOUS VSBBVANTAGEAUXILIARYNUMBERASSTRING 
				VSBBVANTAGEMAINNUMBERASSTRING VSBBVANTAGESECONDARYNUMBERANDLETTER VSBBVANTAGESECONDARYNUMBERASSTRING 
				VSBBVANTAGESTATIONBUSPOSITIONASSTRING VSBBVANTAGESTATIONBUSRUNASSTRING CONNECTIONBLOCKNAMESUFFIX CLASS PLLAYER PLLINETYPE PLWIDTH PTLIST VSBBSTATIONBUSCABLEPOINT3 VSBBSTATIONBUSCABLEPOINT4 VSBBXCOORD2
				)
  

				(if
				  	(/= poeSwitchRun nil)
				  	(progn


	  
				  	(setq vpoebxcoordMax vantagepoeswitchesmainxcoordstart)
	  				(setq vpoebycoordMin ycoordStart)
	  				(setq xcoordLimitMax 22)
	  				(setq xcoordLimitMin (- vantagepoeswitchesmainxcoordstart (* vantagepoeswitchesmainxcoordshift 0.5)))
	  
	  				(if
					  	(> ycoordStart 0)
					  	(progn
					  	(setq ycoordStart (* ycoordStart -1))
						)
					)
					
	  


	  
	      				(setq vpoebxcoord 1.45)
	  				(setq vpoebxcoordprevious vpoebxcoord)
					(setq vpoebxcoordshift (* vantagepoeswitchesmainxcoordshift 1))
	  				(setq vpoebycoordshift (* vantagepoeswitchesmainYcoordshift 1))
	  
					(setq vpoebycoord ycoordStart)
	  				(setq vpoebycoordprevious vpoebycoord)


	  				(setq vpoebcntr 0)
	  				(setq vpoebcmax (length poeSwitchRun))
					(setq vpoebpoeSwitchRunAsIntegerPrevious 0)

	  
	  				(while	(< vpoebcntr vpoebcmax)
	 
						(setq vpoebitem (nth vpoebcntr poeSwitchRun))
						(setq vpoebvantagemainnumberAsInteger (nth 0 vpoebitem))
						(setq vpoebvantagemainnumberAsString (atoi vpoebvantagemainnumberAsInteger))
					  	(setq vpoebvantagesecondarynumberAndLetter (nth 1 vpoebitem))
						;;; secondary is integer and character together in a string, so atoi first item which is the integer
					  	(setq vpoebvantagesecondarynumberAsString(substr vpoebvantagesecondarynumberAndLetter 1 1))
					  	(setq vpoebvantagesecondarynumberAsInteger (atoi vpoebvantagesecondarynumberAsString))
					  	(setq vpoebvantageauxiliarynumberAsString (nth 2 vpoebitem))
					  	(setq vpoebvantageauxiliarynumberAsInteger (atoi vpoebvantageauxiliarynumberAsString))	
						(setq vpoebvantageblock (nth 3 vpoebitem))
						(setq vpoebroomname (nth 4 vpoebitem))


						(setq vpoebycoord vpoebycoordprevious)



						;;; restart run with second note and reset counters for second run
					  	(if
						  	(and
							(/= vpoebpoeSwitchRunAsIntegerPrevious  vpoebvantagepoeSwitchRunAsInteger)
							)
						  	(progn

							(if
							  	(> vpoebcntr 0)
							  	(progn
								(setq vpoebycoord (- vpoebycoord vpoebycoordshift))
								)
							)
							
			      				(setq vpoebxcoord 1.45)
							(setq vpoebxcoordshift (* vantagepoeswitchesmainxcoordshift 1))
							  
							(setq vpoebinsertionpoint (list vpoebxcoord vpoebycoord))
							(setq connectionblocknamesuffix "poeSwitchRuntext")
							(setq vpoebvantageconnectionblock (strcat "0" "0" connectionblocknamesuffix))
							

							  	
						  	(entmakex														;entmakex function
							(list															;list of all required items
							(cons 0 "insert")													;type of entity
						        (cons 2 vpoebvantageconnectionblock)												;name of block to insert
							(cons 8 "Riser")													;block layer
						       	(cons 10 vpoebinsertionpoint)												;block insertion point
						       	(cons 41 1)														; scale factor
						       	(cons 42 1)														; scale factor?
						      	(cons 43 1)														; scale factor?
							)
							)

							(setq vpoebxcoordprevious vpoebxcoord)
							(setq vpoebxcoord (+ vpoebxcoord vpoebxcoordshift))
							(setq vpoebycoordprevious vpoebycoord)
							
							)
						)				  

					  
					  
					  	(setq vpoebinsertionpoint (list vpoebxcoord vpoebycoord))
					  

					  
					  				  
					  
					  	(entmakex														;entmakex function
						(list															;list of all required items
						(cons 0 "insert")													;type of entity
					        (cons 2 vpoebvantageblock)												;name of block to insert
						(cons 8 "Riser")													;block layer
					       	(cons 10 vpoebinsertionpoint)												;block insertion point
					       	(cons 41 1)														; scale factor
					       	(cons 42 1)														; scale factor?
					      	(cons 43 1)														; scale factor?
						)
						)



						(setq vpoebstationbuscablepoint1 (list vpoebxcoordprevious vpoebycoordprevious))  
						(setq vpoebstationbuscablepoint2 (list vpoebxcoord vpoebycoord))

						(entmakex
						(list '
						(0 . "line")
						(cons 8 cat5elayer)
						(cons 10 vpoebstationbuscablepoint1)
						(cons 11 vpoebstationbuscablepoint2)
						)
						)

					  	(setq vpoebmidpoint (midpoint vpoebstationbuscablepoint1 vpoebstationbuscablepoint2))

					  	(if
						  	(/= dropBitCheck 1)
						  	(progn
							  
							(if
							  	(= vpoebvantagestationbuspositionAsInteger 1)
							  	(progn
								(setq vpoebxcoord2 (+ vpoebxcoordprevious 2.5))
								(setq vpoebmidpoint (list vpoebxcoord2 vpoebycoord))
								)
							)
							
					  		(entmakex														;entmakex function
							(list															;list of all required items
							(cons 0 "insert")													;type of entity
						        (cons 2 stationbusnote)													;name of block to insert
						       	(cons 10 vpoebmidpoint)													;block insertion point
							(cons 8 cat5elayer)												;block layer
						       	(cons 41 1)														;scale factor
						       	(cons 42 1)														;scale factor?
						      	(cons 43 1)														;scale factor?
							)
							)

							)
						)
					  

					  	;;; insert Room ID

					  	(setq ycoordRoomID (+ vpoebycoord 1.2))
						(setq vpoebinsertionpoint (list vpoebxcoord ycoordRoomID))
						       
						(if
						  	(and
							(/= vpoebroomname nil)(/= vpoebroomname "")
							)
						  	(progn
							  
							;;;http://forums.augi.com/showthread.php?28641-Create-MTEXT-with-Lisp
						    	(entmake
						    	(list
						      	(cons 0 "MTEXT")         		;; Entity Name
						      	(cons 100 "AcDbEntity")  		;; Subclass Marker
						      	(cons 410 "Model")       		;; Space
						      	(cons 8 "Riser")         		;; Layer
						      	(cons 100 "AcDbMText")   		;; Subclass Marker
						      	(cons 10 vpoebinsertionpoint) 		;; Insertion Point
						      	(cons 40 0.24)             		;; Text Height
						      	(cons 71 5)              		;; Attachment Point (middle-center)
						      	(cons 1 vpoebroomname)    		;; Text Content
						      	(cons 7 "Arial")			;; text style
						      	)
						   	)
							
							)
						)
					  

					  	(if
							(> vpoebxcoord vpoebxcoordMax)
						  	(progn
							(setq vpoebxcoordMax vpoebxcoord)
							)
						)
					  	(if
							(< vpoebycoord vpoebycoordMin)
						  	(progn
							(setq vpoebycoordMin vpoebycoord)
							)
						)	
					  	
					  	
					  	(setq vpoebmainNumberAsIntegerPrevious vpoebvantagemainnumberAsInteger)
					  	(setq vpoebsecondaryNumberAsIntegerPrevious vpoebvantagesecondarynumberAsInteger)
						(setq vpoebauxiliaryNumberAsIntegerPrevious vpoebvantageauxiliarynumberAsInteger)
					  	(setq vpoebpoeSwitchRunAsIntegerPrevious vpoebvantagepoeSwitchRunAsInteger)
					  	(setq vpoebxcoordprevious vpoebxcoord)
					  	(setq vpoebxcoord (+ vpoebxcoord vpoebxcoordshift))
					  	(setq vpoebycoordprevious vpoebycoord)
						(setq vpoebcntr (+ 1 vpoebcntr))
						(setq dropBitCheck 0)
					  
					  	;;; restore xcoord, drop ycoord, flip shifter
					  	(if
						  	(AND
							(>= vpoebxcoord xcoordLimitMax)(< vpoebcntr vpoebcmax)
							)
						  	(progn
							 
							
							(setq vpoebxcoord (- vpoebxcoord vpoebxcoordshift))
							(setq vpoebstationbuscablepoint1 (list vpoebxcoord vpoebycoord))
							(setq vpoebxcoord (+ vpoebxcoord (* vpoebxcoordshift 0.5)))
							(setq vpoebstationbuscablepoint2 (list vpoebxcoord vpoebycoord))
							(setq vpoebycoord (- vpoebycoordMin vpoebycoordshift))
							(setq vpoebstationbuscablepoint3 (list vpoebxcoord vpoebycoord))						
							(setq vpoebxcoord (- vpoebxcoord (* vpoebxcoordshift 0.5)))
							(setq vpoebstationbuscablepoint4 (list vpoebxcoord vpoebycoord))
							
							(setq vpoebxcoordshift (* vpoebxcoordshift -1))
							(setq dropBitCheck 1)
							(setq vpoebxcoordprevious vpoebxcoord)
							(setq vpoebycoordprevious vpoebycoord)

							(setq ptList (list vpoebstationbuscablepoint1 vpoebstationbuscablepoint2 vpoebstationbuscablepoint3 vpoebstationbuscablepoint4))
							(setq class 0)
							(setq plLayer cat5elayer)
							(setq plWidth 0)
							(setq plLinetype "bylayer")
							(vantage-riser-draw-lwpolyline ptList class plLayer plWidth plLinetype)
							
							)
						)
					  	;;; restore xcoord, drop ycoord, flip shifter
					  	(if
							(AND
						  	(< vpoebxcoord xcoordLimitMin)(< vpoebcntr vpoebcmax)
							)
						  	(progn
							(setq vpoebxcoord (- vpoebxcoord vpoebxcoordshift))
							(setq vpoebstationbuscablepoint1 (list vpoebxcoord vpoebycoord))
							(setq vpoebxcoord (+ vpoebxcoord (* vpoebxcoordshift 0.5)))
							(setq vpoebstationbuscablepoint2 (list vpoebxcoord vpoebycoord))
							(setq vpoebycoord (- vpoebycoordMin vpoebycoordshift))
							(setq vpoebstationbuscablepoint3 (list vpoebxcoord vpoebycoord))						
							(setq vpoebxcoord (- vpoebxcoord (* vpoebxcoordshift 0.5)))
							(setq vpoebstationbuscablepoint4 (list vpoebxcoord vpoebycoord))
							
							(setq vpoebxcoordshift (* vpoebxcoordshift -1))
							(setq dropBitCheck 1)
							(setq vpoebxcoordprevious vpoebxcoord)
							(setq vpoebycoordprevious vpoebycoord)
							
							(setq ptList (list vpoebstationbuscablepoint1 vpoebstationbuscablepoint2 vpoebstationbuscablepoint3 vpoebstationbuscablepoint4))
							(setq class 0)
							(setq plLayer cat5elayer)
							(setq plWidth 0)
							(setq plLinetype "bylayer")
							(vantage-riser-draw-lwpolyline ptList class plLayer plWidth plLinetype)

							
							)
						)

					  	(if
						  	(= dropBitCheck 1)
						  	(progn
							(setq vpoebmidpoint (midpoint vpoebstationbuscablepoint1 vpoebstationbuscablepoint2))
							
					  		(entmakex														;entmakex function
							(list															;list of all required items
							(cons 0 "insert")													;type of entity
						        (cons 2 stationbusnote)													;name of block to insert
						       	(cons 10 vpoebmidpoint)													;block insertion point
							(cons 8 cat5elayer)												;block layer
						       	(cons 41 1)														;scale factor
						       	(cons 42 1)														;scale factor?
						      	(cons 43 1)														;scale factor?
							)
							)
							)
						)
					  	
					)

					(if
					  	(> vpoebcmax 0)
					  	(progn
						(setq vpoebycoordMin (- vpoebycoordMin vantagepoeswitchesmainYcoordshift))
						)
					)
					)
				)
				(if
				  	(/= poeDeviceRun nil)
				  	(progn

	  
				  	(setq vpoebxcoordMax vantagepoeswitchesmainxcoordstart)
	  				(setq vpoebycoordMin ycoordStart)
	  				(setq xcoordLimitMax 22)
	  				(setq xcoordLimitMin (- vantagepoeswitchesmainxcoordstart (* vantagepoeswitchesmainxcoordshift 0.5)))
	  
	  				(if
					  	(> ycoordStart 0)
					  	(progn
					  	(setq ycoordStart (* ycoordStart -1))
						)
					)
					
	  


	  
	      				(setq vpoebxcoord 1.45)
	  				(setq vpoebxcoordprevious vpoebxcoord)
					(setq vpoebxcoordshift (* vantagepoeswitchesmainxcoordshift 1))
	  				(setq vpoebycoordshift (* vantagepoeswitchesmainYcoordshift 1))
	  
					(setq vpoebycoord ycoordStart)
	  				(setq vpoebycoordprevious vpoebycoord)


	  				(setq vpoebcntr 0)
	  				(setq vpoebcmax (length poeDeviceRun))
					(setq vpoebpoeDeviceRunAsIntegerPrevious 0)

	  
	  				(while	(< vpoebcntr vpoebcmax)
	 
						(setq vpoebitem (nth vpoebcntr poeDeviceRun))
						(setq vpoebvantagemainnumberAsInteger (nth 0 vpoebitem))
						(setq vpoebvantagemainnumberAsString (atoi vpoebvantagemainnumberAsInteger))
					  	(setq vpoebvantagesecondarynumberAndLetter (nth 1 vpoebitem))
						;;; secondary is integer and character together in a string, so atoi first item which is the integer
					  	(setq vpoebvantagesecondarynumberAsString(substr vpoebvantagesecondarynumberAndLetter 1 1))
					  	(setq vpoebvantagesecondarynumberAsInteger (atoi vpoebvantagesecondarynumberAsString))
					  	(setq vpoebvantageauxiliarynumberAsString (nth 2 vpoebitem))
					  	(setq vpoebvantageauxiliarynumberAsInteger (atoi vpoebvantageauxiliarynumberAsString))	
						(setq vpoebvantageblock (nth 3 vpoebitem))
						(setq vpoebroomname (nth 4 vpoebitem))

					  
						(setq vpoebycoord vpoebycoordprevious)



						;;; restart run with second note and reset counters for second run
					  	(if
						  	(and
							(/= vpoebpoeDeviceRunAsIntegerPrevious  vpoebvantagepoeDeviceRunAsInteger)
							)
						  	(progn

							(if
							  	(> vpoebcntr 0)
							  	(progn
								(setq vpoebycoord (- vpoebycoord vpoebycoordshift))
								)
							)
							
			      				(setq vpoebxcoord 1.45)
							(setq vpoebxcoordshift (* vantagepoeswitchesmainxcoordshift 1))
							  
							(setq vpoebinsertionpoint (list vpoebxcoord vpoebycoord))
							(setq connectionblocknamesuffix "poeDeviceRuntext")
							(setq vpoebvantageconnectionblock (strcat "0" "0" connectionblocknamesuffix))
							

							  	
						  	(entmakex														;entmakex function
							(list															;list of all required items
							(cons 0 "insert")													;type of entity
						        (cons 2 vpoebvantageconnectionblock)												;name of block to insert
							(cons 8 "Riser")													;block layer
						       	(cons 10 vpoebinsertionpoint)												;block insertion point
						       	(cons 41 1)														; scale factor
						       	(cons 42 1)														; scale factor?
						      	(cons 43 1)														; scale factor?
							)
							)

							(setq vpoebxcoordprevious vpoebxcoord)
							(setq vpoebxcoord (+ vpoebxcoord vpoebxcoordshift))
							(setq vpoebycoordprevious vpoebycoord)
							
							)
						)				  

					  
					  
					  	(setq vpoebinsertionpoint (list vpoebxcoord vpoebycoord))
					  

					  
					  				  
					  
					  	(entmakex														;entmakex function
						(list															;list of all required items
						(cons 0 "insert")													;type of entity
					        (cons 2 vpoebvantageblock)												;name of block to insert
						(cons 8 "Riser")													;block layer
					       	(cons 10 vpoebinsertionpoint)												;block insertion point
					       	(cons 41 1)														; scale factor
					       	(cons 42 1)														; scale factor?
					      	(cons 43 1)														; scale factor?
						)
						)



						(setq vpoebstationbuscablepoint1 (list vpoebxcoordprevious vpoebycoordprevious))  
						(setq vpoebstationbuscablepoint2 (list vpoebxcoord vpoebycoord))

						(entmakex
						(list '
						(0 . "line")
						(cons 8 cat5elayer)
						(cons 10 vpoebstationbuscablepoint1)
						(cons 11 vpoebstationbuscablepoint2)
						)
						)

					  	(setq vpoebmidpoint (midpoint vpoebstationbuscablepoint1 vpoebstationbuscablepoint2))

					  	(if
						  	(/= dropBitCheck 1)
						  	(progn
							  
							(if
							  	(= vpoebvantagestationbuspositionAsInteger 1)
							  	(progn
								(setq vpoebxcoord2 (+ vpoebxcoordprevious 2.5))
								(setq vpoebmidpoint (list vpoebxcoord2 vpoebycoord))
								)
							)
							
					  		(entmakex														;entmakex function
							(list															;list of all required items
							(cons 0 "insert")													;type of entity
						        (cons 2 stationbusnote)													;name of block to insert
						       	(cons 10 vpoebmidpoint)													;block insertion point
							(cons 8 cat5elayer)												;block layer
						       	(cons 41 1)														;scale factor
						       	(cons 42 1)														;scale factor?
						      	(cons 43 1)														;scale factor?
							)
							)

							)
						)
					  

					  	;;; insert Room ID

					  	(setq ycoordRoomID (+ vpoebycoord 1.2))
						(setq vpoebinsertionpoint (list vpoebxcoord ycoordRoomID))
						       
						(if
						  	(and
							(/= vpoebroomname nil)(/= vpoebroomname "")
							)
						  	(progn
							  
							;;;http://forums.augi.com/showthread.php?28641-Create-MTEXT-with-Lisp
						    	(entmake
						    	(list
						      	(cons 0 "MTEXT")         		;; Entity Name
						      	(cons 100 "AcDbEntity")  		;; Subclass Marker
						      	(cons 410 "Model")       		;; Space
						      	(cons 8 "Riser")         		;; Layer
						      	(cons 100 "AcDbMText")   		;; Subclass Marker
						      	(cons 10 vpoebinsertionpoint) 		;; Insertion Point
						      	(cons 40 0.24)             		;; Text Height
						      	(cons 71 5)              		;; Attachment Point (middle-center)
						      	(cons 1 vpoebroomname)    		;; Text Content
						      	(cons 7 "Arial")			;; text style
						      	)
						   	)
							
							)
						)
					  

					  	(if
							(> vpoebxcoord vpoebxcoordMax)
						  	(progn
							(setq vpoebxcoordMax vpoebxcoord)
							)
						)
					  	(if
							(< vpoebycoord vpoebycoordMin)
						  	(progn
							(setq vpoebycoordMin vpoebycoord)
							)
						)	
					  	
					  	
					  	(setq vpoebmainNumberAsIntegerPrevious vpoebvantagemainnumberAsInteger)
					  	(setq vpoebsecondaryNumberAsIntegerPrevious vpoebvantagesecondarynumberAsInteger)
						(setq vpoebauxiliaryNumberAsIntegerPrevious vpoebvantageauxiliarynumberAsInteger)
					  	(setq vpoebpoeDeviceRunAsIntegerPrevious vpoebvantagepoeDeviceRunAsInteger)
					  	(setq vpoebxcoordprevious vpoebxcoord)
					  	(setq vpoebxcoord (+ vpoebxcoord vpoebxcoordshift))
					  	(setq vpoebycoordprevious vpoebycoord)
						(setq vpoebcntr (+ 1 vpoebcntr))
						(setq dropBitCheck 0)
					  
					  	;;; restore xcoord, drop ycoord, flip shifter
					  	(if
						  	(AND
							(>= vpoebxcoord xcoordLimitMax)(< vpoebcntr vpoebcmax)
							)
						  	(progn
							 
							
							(setq vpoebxcoord (- vpoebxcoord vpoebxcoordshift))
							(setq vpoebstationbuscablepoint1 (list vpoebxcoord vpoebycoord))
							(setq vpoebxcoord (+ vpoebxcoord (* vpoebxcoordshift 0.5)))
							(setq vpoebstationbuscablepoint2 (list vpoebxcoord vpoebycoord))
							(setq vpoebycoord (- vpoebycoordMin vpoebycoordshift))
							(setq vpoebstationbuscablepoint3 (list vpoebxcoord vpoebycoord))						
							(setq vpoebxcoord (- vpoebxcoord (* vpoebxcoordshift 0.5)))
							(setq vpoebstationbuscablepoint4 (list vpoebxcoord vpoebycoord))
							
							(setq vpoebxcoordshift (* vpoebxcoordshift -1))
							(setq dropBitCheck 1)
							(setq vpoebxcoordprevious vpoebxcoord)
							(setq vpoebycoordprevious vpoebycoord)

							(setq ptList (list vpoebstationbuscablepoint1 vpoebstationbuscablepoint2 vpoebstationbuscablepoint3 vpoebstationbuscablepoint4))
							(setq class 0)
							(setq plLayer cat5elayer)
							(setq plWidth 0)
							(setq plLinetype "bylayer")
							(vantage-riser-draw-lwpolyline ptList class plLayer plWidth plLinetype)
							
							)
						)
					  	;;; restore xcoord, drop ycoord, flip shifter
					  	(if
							(AND
						  	(< vpoebxcoord xcoordLimitMin)(< vpoebcntr vpoebcmax)
							)
						  	(progn
							(setq vpoebxcoord (- vpoebxcoord vpoebxcoordshift))
							(setq vpoebstationbuscablepoint1 (list vpoebxcoord vpoebycoord))
							(setq vpoebxcoord (+ vpoebxcoord (* vpoebxcoordshift 0.5)))
							(setq vpoebstationbuscablepoint2 (list vpoebxcoord vpoebycoord))
							(setq vpoebycoord (- vpoebycoordMin vpoebycoordshift))
							(setq vpoebstationbuscablepoint3 (list vpoebxcoord vpoebycoord))						
							(setq vpoebxcoord (- vpoebxcoord (* vpoebxcoordshift 0.5)))
							(setq vpoebstationbuscablepoint4 (list vpoebxcoord vpoebycoord))
							
							(setq vpoebxcoordshift (* vpoebxcoordshift -1))
							(setq dropBitCheck 1)
							(setq vpoebxcoordprevious vpoebxcoord)
							(setq vpoebycoordprevious vpoebycoord)
							
							(setq ptList (list vpoebstationbuscablepoint1 vpoebstationbuscablepoint2 vpoebstationbuscablepoint3 vpoebstationbuscablepoint4))
							(setq class 0)
							(setq plLayer cat5elayer)
							(setq plWidth 0)
							(setq plLinetype "bylayer")
							(vantage-riser-draw-lwpolyline ptList class plLayer plWidth plLinetype)

							
							)
						)

					  	(if
						  	(= dropBitCheck 1)
						  	(progn
							(setq vpoebmidpoint (midpoint vpoebstationbuscablepoint1 vpoebstationbuscablepoint2))
							
					  		(entmakex														;entmakex function
							(list															;list of all required items
							(cons 0 "insert")													;type of entity
						        (cons 2 stationbusnote)													;name of block to insert
						       	(cons 10 vpoebmidpoint)													;block insertion point
							(cons 8 cat5elayer)												;block layer
						       	(cons 41 1)														;scale factor
						       	(cons 42 1)														;scale factor?
						      	(cons 43 1)														;scale factor?
							)
							)
							)
						)
					  	
					)

					(if
					  	(> vpoebcmax 0)
					  	(progn
						(setq vpoebycoordMin (- vpoebycoordMin vantagepoeswitchesmainYcoordshift))
						)
					)
					)
				)
	
	
  				(setq returnlist (list vpoebxcoordMax vpoebycoordMin))					 	
)


(defun vantage318builder
       				(
				vantage318builderlist
				/
				v318bblockname v318bcmax v318bcntr v318binsertionpoint v318bitem v318bmainprevious
				v318bp1 v318bp2 v318bp3 v318bp4 v318bp5 v318broomname v318bstationbuscablepoint1 v318bstationbuscablepoint2
				v318bstationbusrunprevious v318bvantageblock v318bvantagemainnumber v318bvantagestationbusposition v318bvantagestationbusrun
				v318bxcoord v318bxcoordprevious v318bxshift v318bycoord v318bycoordprevious v318bycoordshift
				 V318BAUXILIARYPREVIOUS V318BCABLEPOINT1 V318BCABLEPOINT2 V318BMIDPOINT V318BSECONDARYPREVIOUS V318BVANTAGE318POSITION V318BVANTAGEAUXILIARYNUMBER V318BVANTAGESECONDARYINTEGER V318BVANTAGESECONDARYLETTER V318BVANTAGESECONDARYNUMBER V318BXCOORDSHIFT V318BYCOORDCONNECTION
				)
      				(setq v318bxcoord 0)
				(setq v318bycoord 0)
  				(setq v318bcntr 0)
  				(setq v318bcmax (length vantage318builderlist))
  				(while	(< v318bcntr v318bcmax)
				  	(setq v318bxcoord 0)
 
					(setq v318bitem (nth v318bcntr vantage318builderlist))
					(setq v318bvantagemainnumber (nth 0 v318bitem))
					(setq v318bvantagemainnumber (atoi v318bvantagemainnumber))
				  	(setq v318bvantagesecondarynumber (nth 1 v318bitem))
					;;; secondary is integer and character together in a string, so atoi first item which is the integer
				  	(setq v318bvantagesecondaryinteger (substr v318bvantagesecondarynumber 1 1))
				  	(setq v318bvantagesecondaryinteger (atoi v318bvantagesecondaryinteger))
				  	(setq v318bvantageauxiliarynumber (nth 2 v318bitem))
				  	(setq v318bvantageauxiliarynumber (atoi v318bvantageauxiliarynumber))	
					(setq v318bvantageblock (nth 3 v318bitem))
					(setq v318broomname (nth 4 v318bitem))
				  	(setq v318bvantagestationbusrun (nth 5 v318bitem))
				  	(setq v318bvantagestationbusrun (atoi v318bvantagestationbusrun))
					(setq v318bvantagestationbusposition (nth 6 v318bitem))
				  	(setq v318bvantagestationbusposition (atoi v318bvantagestationbusposition))
				  	(setq v318bvantage318position (nth 7 v318bitem))
				  	(setq v318bvantage318position (atoi v318bvantage318position))

				  
					(setq v318bycoord (* v318bvantagemainnumber vantagemainpanelycoordshift))


				  
				  	;;;if the secondary integer is not zero, then get the letter after the integer
				  	(cond
					  	((/= v318bvantagesecondaryinteger 0)
				  		(setq v318bvantagesecondaryletter (substr v318bvantagesecondarynumber 2 1))
						)
					)
				  
				  	(cond
					  	((/= v318bvantageauxiliarynumber 0)
						(setq v318bxcoordshift (* vantagestationbusauxiliaryxcoordstart 1))
						(setq v318bxcoord (+ v318bxcoord v318bxcoordshift))
						(setq v318bxcoordshift (* v318bvantagestationbusposition vantagestationbusauxiliaryxcoordshift))
						(setq v318bxcoord (+ v318bxcoord v318bxcoordshift))
						(setq v318bycoordshift (* vantagestationbusauxiliaryycoordstart -1))
						(setq v318bycoord (+ v318bycoord v318bycoordshift))
						(cond
							((/= v318bvantagestationbusrun 1)
							;;; use a temporary shift calculator variable, vsbbycoordshift, to calculate the distance to shift the run with the global variable
							;;; subtract 1 of the global variable amounts back from the temporary shift calculator, to account for the shift not occuring on the first run
							;;; now subtract the temporary shift variable from the current ycoord position
							(setq v318bycoordshift (* vantagestationbusauxiliaryycoordshift v318bvantagestationbusrun))
							(setq v318bycoordshift (- v318bycoordshift vantagestationbusauxiliaryycoordshift))
							(setq v318bycoord (- v318bycoord v318bycoordshift))
							)
						)
						 
						(setq v318bycoord (- v318bycoord (* vantage318auxiliaryycoordshift v318bvantage318position)))
						 
						(setq v318bycoordconnection (+ v318bycoord vantage318auxiliaryycoordshift))
						(setq v318bcablepoint1 (list v318bxcoord v318bycoordconnection))  
						(setq v318bcablepoint2 (list v318bxcoord v318bycoord))
						)
					)
				  
				  	(cond
					  	((/= v318bvantagesecondaryinteger 0)
						(setq v318bxcoordshift (* vantagestationbussecondaryxcoordstart 1))
						(setq v318bxcoord (+ v318bxcoord v318bxcoordshift))
						(setq v318bxcoordshift (* v318bvantagestationbusposition vantagestationbussecondaryxcoordshift))
						(setq v318bxcoord (+ v318bxcoord v318bxcoordshift))
						(setq v318bycoordshift (* vantagestationbussecondaryycoordstart -1))
						(setq v318bycoord (+ v318bycoord v318bycoordshift))

						(cond
							((/= v318bvantagestationbusrun 1)
							;;; use a temporary shift calculator variable, v318bycoordshift, to calculate the distance to shift the run with the global variable
							;;; subtract 1 of the global variable amounts back from the temporary shift calculator, to account for the shift not occuring on the first run
							;;; now subtract the temporary shift variable from the current ycoord position
							(setq v318bycoordshift (* vantagestationbussecondaryycoordshift v318bvantagestationbusrun))
							(setq v318bycoordshift (- v318bycoordshift vantagestationbussecondaryycoordshift))
							(setq v318bycoord (- v318bycoord v318bycoordshift))
							)
						)
						 
						(setq v318bycoord (- v318bycoord (* vantage318secondaryycoordshift v318bvantage318position)))
						 
						(setq v318bycoordconnection (+ v318bycoord vantage318secondaryycoordshift))
						(setq v318bcablepoint1 (list v318bxcoord v318bycoordconnection))  
						(setq v318bcablepoint2 (list v318bxcoord v318bycoord))
						)
					)
				  	(if
					  	(and
						(= v318bvantagesecondaryinteger 0)
						(= v318bvantageauxiliarynumber 0)
						)
					  	(progn

						(setq v318bxcoordshift (* vantagestationbusmainxcoordstart -1))
						(setq v318bxcoord (+ v318bxcoord v318bxcoordshift))
						(setq v318bxcoordshift (* v318bvantagestationbusposition (* vantagestationbusmainxcoordshift -1)))
						(setq v318bxcoord (+ v318bxcoord v318bxcoordshift))
						(setq v318bycoordshift (* vantagestationbusmainycoordstart -1))
						(setq v318bycoord (+ v318bycoord v318bycoordshift))
				  		(cond
							((/= v318bvantagestationbusrun 1)
							;;; use a temporary shift calculator variable, v318bycoordshift, to calculate the distance to shift the run with the global variable
							;;; subtract 1 of the global variable amounts back from the temporary shift calculator, to account for the shift not occuring on the first run
							;;; now subtract the temporary shift variable from the current ycoord position
							(setq v318bycoordshift (* vantagestationbusmainycoordshift v318bvantagestationbusrun))
							(setq v318bycoordshift (- v318bycoordshift vantagestationbusmainycoordshift))
							(setq v318bycoord (- v318bycoord v318bycoordshift))
							)
						)

						(setq v318bycoord (- v318bycoord (* vantage318mainycoordshift v318bvantage318position)))

						(setq v318bycoordconnection (+ v318bycoord vantage318mainycoordshift))
						(setq v318bcablepoint1 (list v318bxcoord v318bycoordconnection))  
						(setq v318bcablepoint2 (list v318bxcoord v318bycoord))
						)
					)



				  	
				  	(setq v318binsertionpoint (list v318bxcoord v318bycoord))
				  

				  
				  				  
				  
				  	(entmakex														;entmakex function
					(list															;list of all required items
					(cons 0 "insert")													;type of entity
				        (cons 2 v318bvantageblock)												;name of block to insert
					(cons 8 "Riser")													;block layer
				       	(cons 10 v318binsertionpoint)												;block insertion point

				       	(cons 41 1)														;scale factor
				       	(cons 42 1)														;scale factor?
				      	(cons 43 1)														;scale factor?
					)
					)

				  	


					(entmakex
					(list '
					(0 . "line")
					(cons 8 318layer)													;block layer
					(cons 10 v318bcablepoint1)
					(cons 11 v318bcablepoint2)
					)
					)

				  
				  	(setq v318bmidpoint (midpoint v318bcablepoint1 v318bcablepoint2))

			  		(entmakex														;entmakex function
					(list															;list of all required items
					(cons 0 "insert")													;type of entity
				        (cons 2 318note)													;name of block to insert
				       	(cons 10 v318bmidpoint)													;block insertion point
					(cons 8 318layer)													;block layer
				       	(cons 41 1)														;scale factor
				       	(cons 42 1)														;scale factor?
				      	(cons 43 1)														;scale factor?
					)
					)


				  
				  	(setq v318bmainprevious v318bvantagemainnumber)
				  	(setq v318bsecondaryprevious v318bvantagesecondaryinteger)
					(setq v318bauxiliaryprevious v318bvantageauxiliarynumber)
				  	(setq v318bstationbusrunprevious v318bvantagestationbusrun)
				  	(setq v318bxcoordprevious v318bxcoord)
				  	(setq v318bycoordprevious v318bycoord)
					(setq v318bcntr (+ 1 v318bcntr))
				)

 	
)



(defun vantagestationbusbuilder
       				(
				stationbusrun ycoordStart STATIONBUSLAYER STATIONBUSNOTE VANTAGESTATIONBUSMAINXCOORDSTART VANTAGESTATIONBUSMAINXCOORDSHIFT  VANTAGESTATIONBUSMAINYCOORDSHIFT 
				/
				vsbbblockname vsbbcmax vsbbcntr vsbbinsertionpoint vsbbitem vsbbmainprevious DROPBITCHECK VMBXCOORD XCOORDLIMITMAX XCOORDLIMITMIN YCOORDROOMID
				vsbbp1 vsbbp2 vsbbp3 vsbbp4 vsbbp5 vsbbroomname vsbbstationbuscablepoint1 vsbbstationbuscablepoint2
				vsbbstationbusrunprevious vsbbvantageblock vsbbvantagemainnumberAsInteger vsbbvantagestationbuspositionAsInteger vsbbvantagestationbusrunAsInteger
				vsbbxcoord vsbbxcoordprevious vsbbxshift vsbbycoord vsbbycoordprevious vsbbycoordshift
				VSBBAUXILIARYPREVIOUS VSBBSECONDARYPREVIOUS VSBBSUBLIST vsbbvantageauxiliarynumberAsInteger vsbbvantageauxiliarynumberAsIntegerSTRING VSBBVANTAGECONNECTIONBLOCK vsbbvantagesecondarynumberAsInteger VSBBVANTAGESECONDARYLETTER vsbbvantagesecondarynumberAsInteger vsbbvantagesecondarynumberAsIntegerCHECK vsbbvantagesecondarynumberAsIntegerSTRING VSBBXCOORDSHIFT
				vsbbxcoordMax vsbbycoordMin returnlist
				VSBBAUXILIARYNUMBERASINTEGERPREVIOUS VSBBMAINNUMBERASINTEGERPREVIOUS 
				VSBBMIDPOINT VSBBSECONDARYNUMBERASINTEGERPREVIOUS VSBBSTATIONBUSRUNASINTEGERPREVIOUS VSBBVANTAGEAUXILIARYNUMBERASSTRING 
				VSBBVANTAGEMAINNUMBERASSTRING VSBBVANTAGESECONDARYNUMBERANDLETTER VSBBVANTAGESECONDARYNUMBERASSTRING 
				VSBBVANTAGESTATIONBUSPOSITIONASSTRING VSBBVANTAGESTATIONBUSRUNASSTRING CONNECTIONBLOCKNAMESUFFIX CLASS PLLAYER PLLINETYPE PLWIDTH PTLIST VSBBSTATIONBUSCABLEPOINT3 VSBBSTATIONBUSCABLEPOINT4 VSBBXCOORD2
				)
  
				
				(setq stationbusrun (vl-sort stationbusrun (function (lambda (x y)(< (atoi (nth 6 x))(atoi (nth 6 y)))))))
  				(setq stationbusrun (vl-sort stationbusrun(function (lambda (x y)(< (atoi (nth 5 x))(atoi (nth 5 y)))))))
  
			  	(setq vsbbxcoordMax VANTAGESTATIONBUSMAINXCOORDSTART)
  				(setq vsbbycoordMin ycoordStart)
  				(setq xcoordLimitMax 22)
  				(setq xcoordLimitMin (- VANTAGESTATIONBUSMAINXCOORDSTART (* VANTAGESTATIONBUSMAINXCOORDSHIFT 0.5)))
  
  				(if
				  	(> ycoordStart 0)
				  	(progn
				  	(setq ycoordStart (* ycoordStart -1))
					)
				)
				
  


  
      				(setq vsbbxcoord 1.45)
  				(setq vsbbxcoordprevious vsbbxcoord)
				(setq vsbbxcoordshift (* VANTAGESTATIONBUSMAINXCOORDSHIFT 1))
  				(setq vsbbycoordshift (* VANTAGESTATIONBUSMAINyCOORDSHIFT 1))
  
				(setq vsbbycoord ycoordStart)
  				(setq vsbbycoordprevious vsbbycoord)


  				(setq vsbbcntr 0)
  				(setq vsbbcmax (length stationbusrun))
				(setq vsbbstationbusrunAsIntegerPrevious 0)

  
  				(while	(< vsbbcntr vsbbcmax)
 
					(setq vsbbitem (nth vsbbcntr stationbusrun))
					(setq vsbbvantagemainnumberAsInteger (nth 0 vsbbitem))
					(setq vsbbvantagemainnumberAsString (atoi vsbbvantagemainnumberAsInteger))
				  	(setq vsbbvantagesecondarynumberAndLetter (nth 1 vsbbitem))
					;;; secondary is integer and character together in a string, so atoi first item which is the integer
				  	(setq vsbbvantagesecondarynumberAsString(substr vsbbvantagesecondarynumberAndLetter 1 1))
				  	(setq vsbbvantagesecondarynumberAsInteger (atoi vsbbvantagesecondarynumberAsString))
				  	(setq vsbbvantageauxiliarynumberAsString (nth 2 vsbbitem))
				  	(setq vsbbvantageauxiliarynumberAsInteger (atoi vsbbvantageauxiliarynumberAsString))	
					(setq vsbbvantageblock (nth 3 vsbbitem))
					(setq vsbbroomname (nth 4 vsbbitem))
				  	(setq vsbbvantagestationbusrunAsString (nth 5 vsbbitem))
				  	(setq vsbbvantagestationbusrunAsInteger (atoi vsbbvantagestationbusrunAsString))
					(setq vsbbvantagestationbuspositionAsString (nth 6 vsbbitem))
				  	(setq vsbbvantagestationbuspositionAsInteger (atoi vsbbvantagestationbuspositionAsString))

				  
					(setq vsbbycoord vsbbycoordprevious)



					;;; restart run with second note and reset counters for second run
				  	(if
					  	(and
						(/= vsbbstationbusrunAsIntegerPrevious  vsbbvantagestationbusrunAsInteger)
						)
					  	(progn

						(if
						  	(> vsbbcntr 0)
						  	(progn
							(setq vsbbycoord (- vsbbycoord vsbbycoordshift))
							)
						)
						
		      				(setq vsbbxcoord 1.45)
						(setq vsbbxcoordshift (* VANTAGESTATIONBUSMAINXCOORDSHIFT 1))
						  
						(setq vsbbinsertionpoint (list vsbbxcoord vsbbycoord))
						(setq connectionblocknamesuffix "stationbusruntext")
						(setq vsbbvantageconnectionblock (strcat "0" "0" connectionblocknamesuffix))
						

						  	
					  	(entmakex														;entmakex function
						(list															;list of all required items
						(cons 0 "insert")													;type of entity
					        (cons 2 vsbbvantageconnectionblock)												;name of block to insert
						(cons 8 "Riser")													;block layer
					       	(cons 10 vsbbinsertionpoint)												;block insertion point
					       	(cons 41 1)														; scale factor
					       	(cons 42 1)														; scale factor?
					      	(cons 43 1)														; scale factor?
						)
						)

						(setq vsbbxcoordprevious vsbbxcoord)
						(setq vsbbxcoord (+ vsbbxcoord vsbbxcoordshift))
						(setq vsbbycoordprevious vsbbycoord)
						
						)
					)				  

				  
				  
				  	(setq vsbbinsertionpoint (list vsbbxcoord vsbbycoord))
				  

				  
				  				  
				  
				  	(entmakex														;entmakex function
					(list															;list of all required items
					(cons 0 "insert")													;type of entity
				        (cons 2 vsbbvantageblock)												;name of block to insert
					(cons 8 "Riser")													;block layer
				       	(cons 10 vsbbinsertionpoint)												;block insertion point
				       	(cons 41 1)														; scale factor
				       	(cons 42 1)														; scale factor?
				      	(cons 43 1)														; scale factor?
					)
					)



					(setq vsbbstationbuscablepoint1 (list vsbbxcoordprevious vsbbycoordprevious))  
					(setq vsbbstationbuscablepoint2 (list vsbbxcoord vsbbycoord))

					(entmakex
					(list '
					(0 . "line")
					(cons 8 stationbuslayer)
					(cons 10 vsbbstationbuscablepoint1)
					(cons 11 vsbbstationbuscablepoint2)
					)
					)

				  	(setq vsbbmidpoint (midpoint vsbbstationbuscablepoint1 vsbbstationbuscablepoint2))

				  	(if
					  	(/= dropBitCheck 1)
					  	(progn
						  
						(if
						  	(= vsbbvantagestationbuspositionAsInteger 1)
						  	(progn
							(setq vsbbxcoord2 (+ vsbbxcoordprevious 2.5))
							(setq vsbbmidpoint (list vsbbxcoord2 vsbbycoord))
							)
						)
						
				  		(entmakex														;entmakex function
						(list															;list of all required items
						(cons 0 "insert")													;type of entity
					        (cons 2 stationbusnote)													;name of block to insert
					       	(cons 10 vsbbmidpoint)													;block insertion point
						(cons 8 stationbuslayer)												;block layer
					       	(cons 41 1)														;scale factor
					       	(cons 42 1)														;scale factor?
					      	(cons 43 1)														;scale factor?
						)
						)

						)
					)
				  

				  	;;; insert Room ID

				  	(setq ycoordRoomID (+ vsbbycoord 1.2))
					(setq vsbbinsertionpoint (list vsbbxcoord ycoordRoomID))
					       
					(if
					  	(and
						(/= vsbbroomname nil)(/= vsbbroomname "")
						)
					  	(progn
						  
						;;;http://forums.augi.com/showthread.php?28641-Create-MTEXT-with-Lisp
					    	(entmake
					    	(list
					      	(cons 0 "MTEXT")         		;; Entity Name
					      	(cons 100 "AcDbEntity")  		;; Subclass Marker
					      	(cons 410 "Model")       		;; Space
					      	(cons 8 "Riser")         		;; Layer
					      	(cons 100 "AcDbMText")   		;; Subclass Marker
					      	(cons 10 vsbbinsertionpoint) 		;; Insertion Point
					      	(cons 40 0.24)             		;; Text Height
					      	(cons 71 5)              		;; Attachment Point (middle-center)
					      	(cons 1 vsbbroomname)    		;; Text Content
					      	(cons 7 "Arial")			;; text style
					      	)
					   	)
						
						)
					)
				  

				  	(if
						(> vsbbxcoord vsbbxcoordMax)
					  	(progn
						(setq vsbbxcoordMax vsbbxcoord)
						)
					)
				  	(if
						(< vsbbycoord vsbbycoordMin)
					  	(progn
						(setq vsbbycoordMin vsbbycoord)
						)
					)	
				  	
				  	
				  	(setq vsbbmainNumberAsIntegerPrevious vsbbvantagemainnumberAsInteger)
				  	(setq vsbbsecondaryNumberAsIntegerPrevious vsbbvantagesecondarynumberAsInteger)
					(setq vsbbauxiliaryNumberAsIntegerPrevious vsbbvantageauxiliarynumberAsInteger)
				  	(setq vsbbstationbusrunAsIntegerPrevious vsbbvantagestationbusrunAsInteger)
				  	(setq vsbbxcoordprevious vsbbxcoord)
				  	(setq vsbbxcoord (+ vsbbxcoord vsbbxcoordshift))
				  	(setq vsbbycoordprevious vsbbycoord)
					(setq vsbbcntr (+ 1 vsbbcntr))
					(setq dropBitCheck 0)
				  
				  	;;; restore xcoord, drop ycoord, flip shifter
				  	(if
					  	(AND
						(>= vsbbxcoord xcoordLimitMax)(< vsbbcntr vsbbcmax)
						)
					  	(progn
						 
						
						(setq vsbbxcoord (- vsbbxcoord vsbbxcoordshift))
						(setq vsbbstationbuscablepoint1 (list vsbbxcoord vsbbycoord))
						(setq vsbbxcoord (+ vsbbxcoord (* vsbbxcoordshift 0.5)))
						(setq vsbbstationbuscablepoint2 (list vsbbxcoord vsbbycoord))
						(setq vsbbycoord (- vsbbycoordMin vsbbycoordshift))
						(setq vsbbstationbuscablepoint3 (list vsbbxcoord vsbbycoord))						
						(setq vsbbxcoord (- vsbbxcoord (* vsbbxcoordshift 0.5)))
						(setq vsbbstationbuscablepoint4 (list vsbbxcoord vsbbycoord))
						
						(setq vsbbxcoordshift (* vsbbxcoordshift -1))
						(setq dropBitCheck 1)
						(setq vsbbxcoordprevious vsbbxcoord)
						(setq vsbbycoordprevious vsbbycoord)

						(setq ptList (list vsbbstationbuscablepoint1 vsbbstationbuscablepoint2 vsbbstationbuscablepoint3 vsbbstationbuscablepoint4))
						(setq class 0)
						(setq plLayer stationbuslayer)
						(setq plWidth 0)
						(setq plLinetype "bylayer")
						(vantage-riser-draw-lwpolyline ptList class plLayer plWidth plLinetype)
						
						)
					)
				  	;;; restore xcoord, drop ycoord, flip shifter
				  	(if
						(AND
					  	(< vsbbxcoord xcoordLimitMin)(< vsbbcntr vsbbcmax)
						)
					  	(progn
						(setq vsbbxcoord (- vsbbxcoord vsbbxcoordshift))
						(setq vsbbstationbuscablepoint1 (list vsbbxcoord vsbbycoord))
						(setq vsbbxcoord (+ vsbbxcoord (* vsbbxcoordshift 0.5)))
						(setq vsbbstationbuscablepoint2 (list vsbbxcoord vsbbycoord))
						(setq vsbbycoord (- vsbbycoordMin vsbbycoordshift))
						(setq vsbbstationbuscablepoint3 (list vsbbxcoord vsbbycoord))						
						(setq vsbbxcoord (- vsbbxcoord (* vsbbxcoordshift 0.5)))
						(setq vsbbstationbuscablepoint4 (list vsbbxcoord vsbbycoord))
						
						(setq vsbbxcoordshift (* vsbbxcoordshift -1))
						(setq dropBitCheck 1)
						(setq vsbbxcoordprevious vsbbxcoord)
						(setq vsbbycoordprevious vsbbycoord)
						
						(setq ptList (list vsbbstationbuscablepoint1 vsbbstationbuscablepoint2 vsbbstationbuscablepoint3 vsbbstationbuscablepoint4))
						(setq class 0)
						(setq plLayer stationbuslayer)
						(setq plWidth 0)
						(setq plLinetype "bylayer")
						(vantage-riser-draw-lwpolyline ptList class plLayer plWidth plLinetype)

						
						)
					)

				  	(if
					  	(= dropBitCheck 1)
					  	(progn
						(setq vsbbmidpoint (midpoint vsbbstationbuscablepoint1 vsbbstationbuscablepoint2))
						
				  		(entmakex														;entmakex function
						(list															;list of all required items
						(cons 0 "insert")													;type of entity
					        (cons 2 stationbusnote)													;name of block to insert
					       	(cons 10 vsbbmidpoint)													;block insertion point
						(cons 8 stationbuslayer)												;block layer
					       	(cons 41 1)														;scale factor
					       	(cons 42 1)														;scale factor?
					      	(cons 43 1)														;scale factor?
						)
						)
						)
					)
				  	
				)

				(if
				  	(> vsbbcmax 0)
				  	(progn
					(setq vsbbycoordMin (- vsbbycoordMin vantagestationbusmainycoordshift))
					)
				)
	
	
  				(setq returnlist (list vsbbxcoordMax vsbbycoordMin))					 	
)


(defun stationbussort		(vantagestationbuslist
				/
				sbsbitem sbscmax sbscntr sbsmainprevious sbsmaxchecker sbsroomname sbsstationbusrun sbsstandalone
				sbsstationbusrunprevious sbssublist sbsvantageblock sbsvantagemainnumber sbsvantagesbsstationbusrun sbsvantagestationbusposition
				SBSITEM SBSVANTAGEAUXILIARYNUMBER SBSVANTAGEAUXILIARYNUMBERPREVIOUS SBSVANTAGESECONDARYNUMBER SBSVANTAGESECONDARYNUMBERPREVIOUS
				)

  				(setq sbscntr 0)
  				(setq sbscmax (length vantagestationbuslist))
  				(setq sbsmaxchecker (+ sbscmax -1))

  					;;;priming read for previous variables
  					(setq sbsitem (nth sbscntr vantagestationbuslist))
					(setq sbsvantagemainnumber (nth 0 sbsitem))
				  	(setq sbsvantagesecondarynumber (nth 1 sbsitem))
				  	(setq sbsvantageauxiliarynumber (nth 2 sbsitem))	
					(setq sbsvantageblock (nth 3 sbsitem))
					(setq sbsroomname (nth 4 sbsitem))
				  	(setq sbsvantagesbsstationbusrun (nth 5 sbsitem))
					(setq sbsvantagestationbusposition (nth 6 sbsitem))
				  	(setq sbsmainprevious sbsvantagemainnumber)
				  	(setq sbsvantagesecondarynumberprevious sbsvantagesecondarynumber)
					(setq sbsvantageauxiliarynumberprevious sbsvantageauxiliarynumber)
				  	(setq sbsstationbusrunprevious sbsvantagesbsstationbusrun)

  
  				(while	(< sbscntr sbscmax)
					(setq sbsitem (nth sbscntr vantagestationbuslist))

					(setq sbsvantagemainnumber (nth 0 sbsitem))
				  	(setq sbsvantagesecondarynumber (nth 1 sbsitem))
				  	(setq sbsvantageauxiliarynumber (nth 2 sbsitem))	
					(setq sbsvantageblock (nth 3 sbsitem))
					(setq sbsroomname (nth 4 sbsitem))
				  	(setq sbsvantagesbsstationbusrun (nth 5 sbsitem))
					(setq sbsvantagestationbusposition (nth 6 sbsitem))

	
					(if
					  	(and
						(= sbsstationbusrunprevious sbsvantagesbsstationbusrun)(= sbsmainprevious sbsvantagemainnumber)
						)
						(progn
						(setq sbssublist (list sbsvantagemainnumber sbsvantagesecondarynumber sbsvantageauxiliarynumber sbsvantageblock sbsroomname sbsvantagesbsstationbusrun sbsvantagestationbusposition ))							
						(setq sbsstationbusrun (cons sbssublist sbsstationbusrun)) 
						(setq sbssublist nil)
						)
					)
				  	
				  	(if
						(or
						(/= sbsstationbusrunprevious sbsvantagesbsstationbusrun)(/= sbsmainprevious sbsvantagemainnumber)
						(= sbscntr sbsmaxchecker)(/= sbsvantagesecondarynumberprevious sbsvantagesecondarynumber)(/= sbsvantageauxiliarynumberprevious sbsvantageauxiliarynumber)
						)
					  	(progn
;;;						(setq sbssublist (list sbsvantagemainnumber sbsvantagesecondarynumber sbsvantageauxiliarynumber sbsvantageblock sbsroomname sbsvantagesbsstationbusrun sbsvantagestationbusposition ))
;;;						(setq sbsstationbusrun (cons sbssublist sbsstationbusrun)) 
;;;						(setq sbssublist nil)
						(setq sbsstationbusrun (vl-sort sbsstationbusrun (function (lambda (x y)(< (nth 6 x)(nth 6 y))))))
						(setq sbsstationbusrun (vl-sort sbsstationbusrun (function (lambda (x y)(< (nth 5 x)(nth 5 y))))))
						(setq sbsstationbusrun (vl-sort sbsstationbusrun (function (lambda (x y)(< (nth 2 x)(nth 2 y))))))
						(setq sbsstationbusrun (vl-sort sbsstationbusrun (function (lambda (x y)(< (nth 1 x)(nth 1 y))))))
						(setq sbsstationbusrun (vl-sort sbsstationbusrun (function (lambda (x y)(< (nth 0 x)(nth 0 y))))))
						(if	(/= sbsstationbusrun nil)
							(vantagestationbusbuilder sbsstationbusrun)
						)
						(setq sbsstationbusrun nil)
						(setq sbssublist (list sbsvantagemainnumber sbsvantagesecondarynumber sbsvantageauxiliarynumber sbsvantageblock sbsroomname sbsvantagesbsstationbusrun sbsvantagestationbusposition ))							
						(setq sbsstationbusrun (cons sbssublist sbsstationbusrun)) 
						(setq sbssublist nil)
						)
					)
					
				  	(setq sbsmainprevious sbsvantagemainnumber)
				  	(setq sbsvantagesecondarynumberprevious sbsvantagesecondarynumber)
					(setq sbsvantageauxiliarynumberprevious sbsvantageauxiliarynumber)
				  	(setq sbsstationbusrunprevious sbsvantagesbsstationbusrun)
				  	(setq sbscntr (+ 1 sbscntr))
				)




)





(defun vantagepanelcomponentbuilder
       				(
				vantagepanelcomponentbuilderlist VANTAGEAUXILIARYPANELXCOORDSHIFT VANTAGELVOSSHIFT VANTAGEMODULEDROPFROMPANELTOP VANTAGEMODULESHIFT VANTAGESECONDARYPANELXCOORDSHIFT VANTAGESECONDARYPANELYCOORDSHIFT 
				/
				maincablepoint1 maincablepoint2 vantagemainnumber vpcbblockname
				vpcbcmax vpcbcntr vpcbinsertionpoint vpcbitem vpcbroomname vpcbvantageblock
				vpcbvantagemainnumber vpcbxcoord vpcbycoord vpcbycoord1
				vantagemainsublist vpcbycoordprevious VPCBLVOSSHIFTDIRECTION VPCBVANTAGEBLOCKPREVIOUS
				VPCBSECONDARYYCOORDSHIFT VPCBVANTAGEAUXILIARYNUMBER VPCBVANTAGEAUXILIARYNUMBERNUMBERPREVIOUS VPCBVANTAGEMAINNUMBERPREVIOUS VPCBVANTAGESECONDARYINTEGER VPCBVANTAGESECONDARYINTEGERNUMBERPREVIOUS VPCBVANTAGESECONDARYLETTER VPCBVANTAGESECONDARYLETTERNUMBERPREVIOUS VPCBVANTAGESECONDARYNUMBER VPCBXCOORDPREVIOUS VPCBYCOORDSHIFT VPCBYCOORDTEMP
				)

  				(setq vpcbxcoord 0)
				(setq vpcbycoord 0)	
  				(setq vpcbcntr 0)
  				(setq vpcbcmax (length vantagepanelcomponentbuilderlist))
  				(setq vpcbLVOSshiftDirection -1)
  				(while	(< vpcbcntr vpcbcmax)



				  	(setq vpcbxcoord 0)
					(setq vpcbitem (nth vpcbcntr vantagepanelcomponentbuilderlist))
					(setq vpcbvantagemainnumber (nth 0 vpcbitem))
					(setq vpcbvantagemainnumber (atoi vpcbvantagemainnumber))
				  	(setq vpcbvantagesecondarynumber (nth 1 vpcbitem))
					;;; secondary is integer and character together in a string, so atoi first item which is the integer
				  	(setq vpcbvantagesecondaryinteger (substr vpcbvantagesecondarynumber 1 1))
				  	(setq vpcbvantagesecondaryinteger (atoi vpcbvantagesecondaryinteger))
				  	(setq vpcbvantageauxiliarynumber (nth 2 vpcbitem))
				  	(setq vpcbvantageauxiliarynumber (atoi vpcbvantageauxiliarynumber))	
					(setq vpcbvantageblock (nth 3 vpcbitem))
					(setq vpcbroomname (nth 4 vpcbitem))


				  	;;; start ycoord at 0
					(setq vpcbycoord 0)

				  
				  	;;;if the secondary integer is not zero, then get the letter after the integer
				  	(cond
					  	((/= vpcbvantagesecondaryinteger 0)
				  		(setq vpcbvantagesecondaryletter (substr vpcbvantagesecondarynumber 2 1))
						)
					)
				  


						 

				  
					(cond
					  	((= vpcbvantagesecondaryinteger 2)
						;;;nothing because ycoord is okay 
						)
					  	((= vpcbvantagesecondaryinteger 3)
						(setq vpcbsecondaryycoordshift (* vantagesecondarypanelycoordshift -1)) 
						(setq vpcbycoord (+ vpcbycoord vpcbsecondaryycoordshift))
						)
					  	((= vpcbvantagesecondaryinteger 4)
						(setq vpcbsecondaryycoordshift (* vantagesecondarypanelycoordshift -2))
						(setq vpcbycoord (+ vpcbycoord vpcbsecondaryycoordshift))
						)
					)
				  	(cond
					  	((= vpcbvantagesecondaryletter "A")
				  		(setq vpcbxcoord (+ vpcbxcoord (* vantagesecondarypanelxcoordshift 1)))
						)
					  	((= vpcbvantagesecondaryletter "B")
				  		(setq vpcbxcoord (+ vpcbxcoord (* vantagesecondarypanelxcoordshift 2)))
						)
					)

				  	(cond
					  	((/= vpcbvantageauxiliarynumber 0)
						(setq vpcbxcoord (+ vpcbxcoord (* vantagesecondarypanelxcoordshift 2)))
						(setq vpcbxcoord (+ vpcbxcoord (* vpcbvantageauxiliarynumber vantageauxiliarypanelxcoordshift)))
						)
					)
				  

					(setq vpcbycoord (+ vpcbycoord (* vantagemoduledropfrompaneltop -1)))
				  
					
				  
				  	;;; only use previous for calculation when we are not calculating for the bottom of panel components or LVOS
					(if
						(and
						(= vpcbvantagemainnumberprevious vpcbvantagemainnumber) 
				  		(= vpcbvantagesecondaryintegernumberprevious vpcbvantagesecondaryinteger)
				  		(= vpcbvantagesecondaryletternumberprevious vpcbvantagesecondaryletter)
				  		(= vpcbvantageauxiliarynumbernumberprevious vpcbvantageauxiliarynumber)
						(/= vpcbvantageblock "V-IC-36")
						(/= vpcbvantageblock "POE-SWITCH")
						(/= vpcbvantageblock "V-IC-DIN-II-LITE")
						(/= vpcbvantageblock "V-IC-DIN-II-LITE-RF")
						(/= vpcbvantageblock "V-LVOS-0-10-PWM-P-1")
						)
						(progn
						(setq vpcbycoord (+ vpcbycoordprevious (* vantagemoduleshift -1)))
						  
						)
					)					


				  	;;; set 1st LVOS position when we moved to different panel
					(if
						(OR
						(/= vpcbvantagemainnumberprevious vpcbvantagemainnumber) 
				  		(/= vpcbvantagesecondaryintegernumberprevious vpcbvantagesecondaryinteger)
				  		(/= vpcbvantagesecondaryletternumberprevious vpcbvantagesecondaryletter)
				  		(/= vpcbvantageauxiliarynumbernumberprevious vpcbvantageauxiliarynumber)
						
						)
						(progn
						(if
						  	(= vpcbvantageblock "V-LVOS-0-10-PWM-P-1")
						  	(progn
							(setq vpcbLVOSshiftDirection -1) 
							(setq vpcbxcoord (+ vpcbxcoord (* vantageLVOSshift vpcbLVOSshiftDirection)))
							;(setq vpcbLVOSshiftDirection (* vpcbLVOSshiftDirection -1))
							)
						)
						)
					)
				  
				  	;;; calculate LVOS position when we are in same panel
					(if
						(and
						(= vpcbvantagemainnumberprevious vpcbvantagemainnumber) 
				  		(= vpcbvantagesecondaryintegernumberprevious vpcbvantagesecondaryinteger)
				  		(= vpcbvantagesecondaryletternumberprevious vpcbvantagesecondaryletter)
				  		(= vpcbvantageauxiliarynumbernumberprevious vpcbvantageauxiliarynumber)
						(= vpcbvantageblock "V-LVOS-0-10-PWM-P-1")
						)
						(progn
						  
						;;;change shift direction after coordinate calculation
						
						(if
						  	(AND
							(= vpcbvantageblock "V-LVOS-0-10-PWM-P-1")
							)
						  	(progn
							(setq vpcbLVOSshiftDirection (* vpcbLVOSshiftDirection -1))
							)						

						)
						
						;;;shift if previous component was lvos, previous lvos was in left slot and current device is lvos
						(if
						  	(AND
							(= vpcbvantageblockPrevious "V-LVOS-0-10-PWM-P-1")(= vpcbvantageblock "V-LVOS-0-10-PWM-P-1")(= vpcbLVOSshiftDirection -1)
							)
						  	(progn
							;drop y coord
							(setq vpcbycoord (+ vpcbycoordprevious (* vantagemoduleshift -1)))
							(setq vpcbxcoord (+ vpcbxcoord (* vantageLVOSshift vpcbLVOSshiftDirection)))
							
							)
						)
						;;;shift if previous component was lvos, previous lvos was in right slot and current device is lvos
						(if
						  	(AND
							(= vpcbvantageblockPrevious "V-LVOS-0-10-PWM-P-1")(= vpcbvantageblock "V-LVOS-0-10-PWM-P-1")(= vpcbLVOSshiftDirection 1)
							)
						  	(progn
							;use previous ycoord
							(setq vpcbycoord vpcbycoordprevious)
							(setq vpcbxcoord (+ vpcbxcoord (* vantageLVOSshift vpcbLVOSshiftDirection)))
							)						
						)

						)
					)

				  
				  
				  	;;; save the current ycoord value to a temporary variable
				  	;;; place items at bottom of panel, regardless of any other factors
				  	(if	
					  	(or
						(= vpcbvantageblock "V-IC-36")
						)
						(progn
						(setq vpcbycoordtemp vpcbycoord)
						(setq vpcbycoord (+ vpcbycoord (* vantagemoduleshift -7)))
						)
					)
				  	(if	
					  	(or
						(= vpcbvantageblock "POE-SWITCH")
						)
						(progn
						(setq vpcbycoordtemp vpcbycoord)
						(setq vpcbycoord (+ vpcbycoord (* vantagemoduleshift -6)))
						)
					)
				  	(if	
					  	(or
						(= vpcbvantageblock "V-IC-DIN-II-LITE-RF")
						(= vpcbvantageblock "V-IC-DIN-II-LITE")
						)
						(progn
						(setq vpcbycoordtemp vpcbycoord)
						(setq vpcbycoord (+ vpcbycoord (* vantagemoduleshift -7)))
						)
					)

				  

				  
				  	(setq vpcbinsertionpoint (list vpcbxcoord vpcbycoord))


				  
			  		(entmakex														;entmakex function
					(list															;list of all required items
					(cons 0 "insert")													;type of entity
				        (cons 2 vpcbvantageblock)												;name of block to insert
					(cons 8 "Riser")													;block layer
				       	(cons 10 vpcbinsertionpoint)												;block insertion point
				       	(cons 41 1)														; scale factor
				       	(cons 42 1)														; scale factor?
				      	(cons 43 1)														; scale factor?
					)
					)


				  	;;; save x and y for next loop
				  	(setq vpcbxcoordprevious vpcbxcoord)
				  	(setq vpcbycoordprevious vpcbycoord)
				  
				  	;;;restore vpcbxcoordprevious to the correct value before placing item at bottom of panel 
				  	(if
					  	(or
						(= vpcbvantageblock "V-IC-36")
						(= vpcbvantageblock "POE-SWITCH")
						(= vpcbvantageblock "V-IC-DIN-II-LITE-RF")
						(= vpcbvantageblock "V-IC-DIN-II-LITE")
						)
						(progn
						(setq vpcbycoordprevious (+ vpcbycoordtemp vantagemoduleshift ))
						)
					)
				  	
				  	(setq vpcbxcoord 0)
				  	(setq vpcbycoord 0)
				  	

					(setq vpcbvantagemainnumberprevious vpcbvantagemainnumber) 
				  	(setq vpcbvantagesecondaryintegernumberprevious vpcbvantagesecondaryinteger)
				  	(setq vpcbvantagesecondaryletternumberprevious vpcbvantagesecondaryletter)
				  	(setq vpcbvantageauxiliarynumbernumberprevious vpcbvantageauxiliarynumber)
				  	(setq vpcbvantageblockPrevious vpcbvantageblock)
				  

				  
				  	(setq vpcbvantagemainnumber 0)
				  	(setq vpcbvantagesecondaryinteger 0)
				  	(setq vpcbvantageauxiliarynumber 0)
				  	(setq vpcbvantagesecondaryletter nil)
				  
				  	(setq vpcbcntr (+ 1 vpcbcntr))
				)
  

	
	
  
)



(defun vantagepanelbuilder
       				(
				vantagepanelbuilderlist ENCLOSUREBUSLAYER ENCLOSUREBUSNOTE VANTAGEAUXILIARYPANELXCOORDSHIFT VANTAGEPANELWIDTH VANTAGESECONDARYPANELXCOORDSHIFT VANTAGESECONDARYPANELYCOORDSHIFT 
				/
				maincablepoint1 maincablepoint2 vantagemainnumber vmbblockname YCOORDPANELID YCOORDROOMID
				vmbcmax vmbcntr vmbinsertionpoint vmbitem vmbroomname vmbvantageblock
				vmbvantagemainnumber vmbxcoord vmbycoord vmbycoord1
				vantagemainsublist returnlist VMBXCOORDMAX VMBYCOORDMIN vmbPanelID
				 SECONDARYCABLEPOINT1 SECONDARYCABLEPOINT2 SECONDARYCABLEPOINT3 SECONDARYCABLEPOINT4
				 VMB1/3GAP VMBGAPBETWEENPANELS VMBMIDPOINT VMBSECONDARYYCOORDSHIFT VMBVANTAGEAUXILIARYNUMBER VMBVANTAGESECONDARYINTEGER VMBVANTAGESECONDARYLETTER VMBVANTAGESECONDARYNUMBER VMBXCOORDMAINPANELEDGE VMBXCOORDSECONDARYCABLE VMBYCOORDMAINCABLE VMBYCOORDSECONDARYCABLE VMBYCOORDSHIFT
				)

  				(setq vmbxcoord 0)
  				(setq vmbxcoordMax 0)
				(setq vmbycoord 0)
  				(setq vmbycoordMin 0)
  				(setq vmbcntr 0)
  				(setq vmbcmax (length vantagepanelbuilderlist))
  
  				(while	(< vmbcntr vmbcmax)



				  
					(setq vmbitem (nth vmbcntr vantagepanelbuilderlist))
					(setq vmbvantagemainnumber (nth 0 vmbitem))
					(setq vmbvantagemainnumber (atoi vmbvantagemainnumber))
				  	(setq vmbvantagesecondarynumber (nth 1 vmbitem))
					;;; secondary is integer and character together in a string, so atoi first item which is the integer
				  	(setq vmbvantagesecondaryinteger (substr vmbvantagesecondarynumber 1 1))
				  	(setq vmbvantagesecondaryinteger (atoi vmbvantagesecondaryinteger))
				  	(setq vmbvantageauxiliarynumber (nth 2 vmbitem))
				  	(setq vmbvantageauxiliarynumber (atoi vmbvantageauxiliarynumber))	
					(setq vmbvantageblock (nth 3 vmbitem))
					(setq vmbroomname (nth 4 vmbitem))
					(setq vmbPanelID (nth 5 vmbitem))

				  	;;; start ycoord at 0
					(setq vmbycoord 0)

				  
				  	;;;if the secondary integer is not zero, then get the letter after the integer
				  	(cond
					  	((/= vmbvantagesecondaryinteger 0)
				  		(setq vmbvantagesecondaryletter (substr vmbvantagesecondarynumber 2 1))
						)
					)
				  


						 
					
				  	;;; calculate cable points
				  	;;; every panel branch location will use secondarycablepoint1, which will be the first connection from the top edge of the previous panel
				  	;;; only secondary panels "A" on branch 3 and 4 will use secondarycablepoint2, which is the point in between main and secondary before vertical drop
				  	;;; every panel branch location will use secondarycablepoint4, which is the final connection to the top edge of the panel
				  	;;; also calculate note points from the middle of the horizonal line
				  	;;; for secondary panels "A" on branch 3 and 4
				  	;;; first line is horizonal from secondarycablepoint1 to secondarycablepoint2
				  	;;; second line is vertical from secondarycablepoint2 to secondarycablepoint3
				  	;;; third line is horizontal line is secondarycablepoint3 secondarycablepoint4
				  	;;; for all other panels
				  	;;; first line is horizonal from secondarycablepoint1 to secondarycablepoint4

				  	(setq vmbxcoordmainpaneledge (* vantagepanelwidth 0.5))						; calculate edge of the main panel
				  	(setq vmbgapbetweenpanels (- vantagesecondarypanelxcoordshift vantagepanelwidth))			; set the gap between panels based on the global variables
					(setq vmb1/3gap (/ vmbgapbetweenpanels 3))								; divide by 3 to even the space between lines
				  
				  	(cond
					  	((= vmbvantagesecondaryinteger 2)
						(setq vmbycoordsecondarycable (- vmbycoord 0.25))						; drop from previous panel by .25
						(setq secondarycablepoint1 (list 1.75 vmbycoordsecondarycable))					; set point1 
						)
					  	((= vmbvantagesecondaryinteger 3)
						(setq vmbycoordsecondarycable (- vmbycoord 1.8750))						; drop from previous panel, further than branch 2
						(setq vmbxcoordsecondarycable 1.75)
						(setq secondarycablepoint1 (list vmbxcoordsecondarycable vmbycoordsecondarycable))
						 
						(setq vmbxcoordsecondarycable (+ vmbxcoordmainpaneledge (* vmb1/3gap 2)))			; calculate the cable point between panels using the edge of the main plus 2/3 of the gap
						(setq secondarycablepoint2 (list vmbxcoordsecondarycable vmbycoordsecondarycable))
						(setq vmbsecondaryycoordshift (* vantagesecondarypanelycoordshift -1)) 
						(setq vmbycoord (+ vmbycoord vmbsecondaryycoordshift))
						(setq vmbycoordsecondarycable (- vmbycoord 0.25))
						(setq secondarycablepoint3 (list vmbxcoordsecondarycable vmbycoordsecondarycable))
						)
					  	((= vmbvantagesecondaryinteger 4)
						(setq vmbycoordsecondarycable (- vmbycoord 3.5))						; drop from previous panel, further than branch 3
						(setq vmbxcoordsecondarycable 1.75)
						(setq secondarycablepoint1 (list vmbxcoordsecondarycable vmbycoordsecondarycable))
						 
						(setq vmbxcoordsecondarycable (+ vmbxcoordmainpaneledge (* vmb1/3gap 1)))			; calculate the cable point between panels using the middle of the panels plus 1/3 of the gap
						(setq secondarycablepoint2 (list vmbxcoordsecondarycable vmbycoordsecondarycable))
						(setq vmbsecondaryycoordshift (* vantagesecondarypanelycoordshift -2))			
						(setq vmbycoord (+ vmbycoord vmbsecondaryycoordshift))
						(setq vmbycoordsecondarycable (- vmbycoord 0.25))
						(setq secondarycablepoint3 (list vmbxcoordsecondarycable vmbycoordsecondarycable))
						)
					)
				  	(cond
					  	((= vmbvantagesecondaryletter "A")
				  		(setq vmbxcoord (+ vmbxcoord (* vantagesecondarypanelxcoordshift 1)))
						(setq vmbxcoordsecondarycable (+ vmbxcoord -1.75))
						(setq vmbycoordsecondarycable (- vmbycoord 0.25))
						(setq secondarycablepoint4 (list vmbxcoordsecondarycable vmbycoordsecondarycable))
						)
					  	((= vmbvantagesecondaryletter "B")
				  		(setq vmbxcoord (+ vmbxcoord (* vantagesecondarypanelxcoordshift 2)))
						(setq vmbxcoordsecondarycable (+ (+ 1.75 vmbxcoord (* vantagesecondarypanelxcoordshift -1))))
						(setq vmbycoordsecondarycable (- vmbycoord 0.25))
						(setq secondarycablepoint1 (list vmbxcoordsecondarycable vmbycoordsecondarycable))
						(setq vmbxcoordsecondarycable (+ vmbxcoord -1.75))
						(setq secondarycablepoint4 (list vmbxcoordsecondarycable vmbycoordsecondarycable))
						(setq secondarycablepoint2 nil)
						)
					)

				  	(cond
					  	((/= vmbvantageauxiliarynumber 0)
						(setq vmbxcoord (+ vmbxcoord (* vantagesecondarypanelxcoordshift 2)))
						(setq vmbxcoord (+ vmbxcoord (* vmbvantageauxiliarynumber vantageauxiliarypanelxcoordshift)))
						)
					)


	
				  	(setq vmbinsertionpoint (list vmbxcoord vmbycoord))
				  
			  		(entmakex														;entmakex function
					(list															;list of all required items
					(cons 0 "insert")													;type of entity
				        (cons 2 vmbvantageblock)												;name of block to insert
					(cons 8 "Panels")													;block layer
				       	(cons 10 vmbinsertionpoint)												;block insertion point
				       	(cons 41 1)														; scale factor
				       	(cons 42 1)														; scale factor?
				      	(cons 43 1)														; scale factor?
					)
					)

				  	;;; insert Room ID

				  	(setq ycoordRoomID (+ vmbycoord 0.7))
					(setq vmbinsertionpoint (list vmbxcoord ycoordRoomID))
					       
					(if
					  	(and
						(/= vmbroomname nil)(/= vmbroomname "")
						)
					  	(progn
						  
						;;;http://forums.augi.com/showthread.php?28641-Create-MTEXT-with-Lisp
					    	(entmake
					    	(list
					      	(cons 0 "MTEXT")         		;; Entity Name
					      	(cons 100 "AcDbEntity")  		;; Subclass Marker
					      	(cons 410 "Model")       		;; Space
					      	(cons 8 "Riser")         		;; Layer
					      	(cons 100 "AcDbMText")   		;; Subclass Marker
					      	(cons 10 vmbinsertionpoint) 		;; Insertion Point
					      	(cons 40 0.24)             		;; Text Height
					      	(cons 71 5)              		;; Attachment Point (middle-center)
					      	(cons 1 vmbroomname)    		;; Text Content
					      	(cons 7 "Arial")			;; text style
					      	)
					   	)
						
						)
					)
					       
					(setq ycoordPanelID (+ ycoordRoomID 0.5))
					(setq vmbinsertionpoint (list vmbxcoord ycoordPanelID))

					(if
					  	(and
					  	(/= vmbPanelID nil)(/= vmbPanelID "")
						)
					  	(progn
						  
						;;;http://forums.augi.com/showthread.php?28641-Create-MTEXT-with-Lisp
					    	(entmake
					    	(list
					      	(cons 0 "MTEXT")         		;; Entity Name
					      	(cons 100 "AcDbEntity")  		;; Subclass Marker
					      	(cons 410 "Model")       		;; Space
					      	(cons 8 "Riser")         		;; Layer
					      	(cons 100 "AcDbMText")   		;; Subclass Marker
					      	(cons 10 vmbinsertionpoint) 		;; Insertion Point
					      	(cons 40 0.24)            		;; Text Height
					      	(cons 71 5)              		;; Attachment Point (middle-center)
					      	(cons 1 vmbPanelID)   			;; Text Content
					      	(cons 7 "Arial")			;; text style
					      	)
					   	)

						)
					)
					       
				  	(if
						(> vmbxcoord vmbxcoordMax)
					  	(progn
						(setq vmbxcoordMax vmbxcoord)
						)
					)
				  	(if
						(< vmbycoord vmbycoordMin)
					  	(progn
						(setq vmbycoordMin vmbycoord)
						)
					)				  


				  
				  	(setq vmbxcoord 0)
				  	(setq vmbycoord 0)
				  	(setq vmbcntr (+ 1 vmbcntr))


				  

				  	;;; draw lines and note for 2A and 2B
					(if
					  	(and
						(= vmbvantagesecondaryinteger 2)
						)
						(progn

						;;; horizontal line  
						(entmakex
						(list '
						(0 . "line")
						(cons 8 enclosurebuslayer)
						(cons 10 secondarycablepoint1)
						(cons 11 secondarycablepoint4)
						)
						)

						(setq vmbmidpoint (midpoint secondarycablepoint1 secondarycablepoint4))

						;;; note
						(entmakex													;entmakex function
						(list														;list of all required items
						(cons 0 "insert")												;type of entity
					        (cons 2 enclosurebusnote)											;name of block to insert
						(cons 8 enclosurebuslayer)
					       	(cons 10 vmbmidpoint)												;block insertion point
					       	(cons 41 1)													; scale factor
					       	(cons 42 1)													; scale factor?
					      	(cons 43 1)													; scale factor?
						)
						)
						
						)
						 
					)

				  	;;; draw lines and note for panels 3A and 4A
					(if	
				  		(and
						(/= vmbvantagesecondaryinteger 2)(= vmbvantagesecondaryletter "A")
						)
						(progn

						;;; horizontal line	  
						(entmakex
						(list '
						(0 . "line")
						(cons 8 enclosurebuslayer)
						(cons 10 secondarycablepoint1)
						(cons 11 secondarycablepoint2)
						)
						)
						
						(setq vmbmidpoint (midpoint secondarycablepoint1 secondarycablepoint2))

						;;; note
						(entmakex													;entmakex function
						(list														;list of all required items
						(cons 0 "insert")												;type of entity
					        (cons 2 enclosurebusnote)											;name of block to insert
						(cons 8 enclosurebuslayer)											;block layer
					       	(cons 10 vmbmidpoint)												;block insertion point
					       	(cons 41 1)													; scale factor
					       	(cons 42 1)													; scale factor?
					      	(cons 43 1)													; scale factor?
						)
						)
						
						;;; vertical line
						(entmakex
						(list '
						(0 . "line")
						(cons 8 enclosurebuslayer)
						(cons 10 secondarycablepoint2)
						(cons 11 secondarycablepoint3)
						)
						)

						



						;;; horizontal line
						(entmakex
						(list '
						(0 . "line")
						(cons 8 enclosurebuslayer)
						(cons 10 secondarycablepoint3)
						(cons 11 secondarycablepoint4)
						)
						)
						
						)
						
					)

				  	;;;draw lines and note for 3B and 4B

					(if	
				  		(and
						(/= vmbvantagesecondaryinteger 2)(= vmbvantagesecondaryletter "B")
						)
						(progn

						;;; horizontal line  
						(entmakex
						(list '
						(0 . "line")
						(cons 8 enclosurebuslayer)
						(cons 10 secondarycablepoint1)
						(cons 11 secondarycablepoint4)
						)
						)

						(setq vmbmidpoint (midpoint secondarycablepoint1 secondarycablepoint4))

						;;; note
						(entmakex													;entmakex function
						(list														;list of all required items
						(cons 0 "insert")												;type of entity
					        (cons 2 enclosurebusnote)											;name of block to insert
						(cons 8 enclosurebuslayer)
					       	(cons 10 vmbmidpoint)												;block insertion point
					       	(cons 41 1)													; scale factor
					       	(cons 42 1)													; scale factor?
					      	(cons 43 1)													; scale factor?
						)
						)
						
						)
						
					)

				  
				  	(setq vmbvantagemainnumber 0)
				  	(setq vmbvantagesecondaryinteger 0)
				  	(setq vmbvantageauxiliarynumber 0)
				  	(setq vmbvantagesecondaryletter nil)
				  	(setq vmbmidpoint nil)
				)
  
				(if
				  	(> vmbcmax 0)
				  	(progn
					(setq vmbycoordMin (- vmbycoordMin vantagesecondarypanelycoordshift))
					)
				)
	
  				(setq returnlist (list vmbxcoordMax vmbycoordMin))
)





(defun vantagers232builder
       				(
				vantagers232builderlist
				/
				RS232BCMAX RS232BCNTR RS232BINSERTIONPOINT RS232BITEM RS232BROOMNAME RS232BRS232POSITION RS232BSECONDARYYCOORDSHIFT RS232BVANTAGEAUXILIARYNUMBER RS232BVANTAGEBLOCK RS232BVANTAGEMAINNUMBER RS232BVANTAGESECONDARYINTEGER RS232BVANTAGESECONDARYLETTER RS232BVANTAGESECONDARYNUMBER RS232BXCOORD RS232BYCOORD RS232BYCOORDSHIFT RS232CABLEPOINT1 RS232CABLEPOINT2 RS232MIDPOINT
				)

  

    				(setq rs232bxcoord 0)
				(setq rs232bycoord 0)	
  				(setq rs232bcntr 0)
  				(setq rs232bcmax (length vantagers232builderlist))
  				(while	(< rs232bcntr rs232bcmax)



				  
					(setq rs232bitem (nth rs232bcntr vantagers232builderlist))
					(setq rs232bvantagemainnumber (nth 0 rs232bitem))
					(setq rs232bvantagemainnumber (atoi rs232bvantagemainnumber))
				  	(setq rs232bvantagesecondarynumber (nth 1 rs232bitem))
					;;; secondary is integer and character together in a string, so atoi first item which is the integer
				  	(setq rs232bvantagesecondaryinteger (substr rs232bvantagesecondarynumber 1 1))
				  	(setq rs232bvantagesecondaryinteger (atoi rs232bvantagesecondaryinteger))
				  	(setq rs232bvantageauxiliarynumber (nth 2 rs232bitem))
				  	(setq rs232bvantageauxiliarynumber (atoi rs232bvantageauxiliarynumber))	
					(setq rs232bvantageblock (nth 3 rs232bitem))
					(setq rs232broomname (nth 4 rs232bitem))
					(setq rs232brs232position (nth 5 rs232bitem))


				  	(setq rs232bycoordshift (* rs232bvantagemainnumber vantagemainpanelycoordshift))
					(setq rs232bycoord (+ rs232bycoord rs232bycoordshift))
				  
				  	;;;if the secondary integer is not zero, then get the letter after the integer
				  	(cond
					  	((/= rs232bvantagesecondaryinteger 0)
				  		(setq rs232bvantagesecondaryletter (substr rs232bvantagesecondarynumber 2 1))
						)
					)
				  

	 
				  	;;; this section locates x and y for the specific panel
					(cond
					  	((= rs232bvantagesecondaryinteger 2)
						;;;nothing because ycoord is okay 
						)
					  	((= rs232bvantagesecondaryinteger 3)
						(setq rs232bsecondaryycoordshift (* vantagesecondarypanelycoordshift -1)) 
						(setq rs232bycoord (+ rs232bycoord rs232bsecondaryycoordshift))
						)
					  	((= rs232bvantagesecondaryinteger 4)
						(setq rs232bsecondaryycoordshift (* vantagesecondarypanelycoordshift -2))
						(setq rs232bycoord (+ rs232bycoord rs232bsecondaryycoordshift))
						)
					)
				  	(cond
					  	((= rs232bvantagesecondaryletter "A")
				  		(setq rs232bxcoord (+ rs232bxcoord (* vantagesecondarypanelxcoordshift 1)))
						)
					  	((= rs232bvantagesecondaryletter "B")
				  		(setq rs232bxcoord (+ rs232bxcoord (* vantagesecondarypanelxcoordshift 2)))
						)
					)

				  	(cond
					  	((/= rs232bvantageauxiliarynumber 0)
						(setq rs232bxcoord (+ rs232bxcoord (* vantagesecondarypanelxcoordshift 2)))
						(setq rs232bxcoord (+ rs232bxcoord (* rs232bvantageauxiliarynumber vantageauxiliarypanelxcoordshift)))
						)
					)
				  


				  	;;; this section locates x and y based on global variable for RS232
				  	;;; first drop to components
				  	;;; next drop to the IC-36 slot or DIN
				  	;;; finally shift based on what type of panel
				  
					(setq rs232bycoord (+ rs232bycoord (* vantagemoduledropfrompaneltop -1)))


	

				  
				  	;;; set location for RS232 block based on RS232position value
				  	;;; place items next to the IC-36 or DIN
				  	;;; using global variable for module shifts
				  	;;; this is where the first cable point connects to
				  	(if	
					  	(or
						(= rs232brs232position "1")
						)
						(progn
						(setq rs232bycoord (+ rs232bycoord (* vantagemoduleshift -5)))
						(setq rs232cablepoint1 (list rs232bxcoord rs232bycoord))
						)
					)
				  	(if	
					  	(or
						(= rs232brs232position "2")
						)
						(progn
						(setq rs232bycoord (+ rs232bycoord (* vantagemoduleshift -7)))
						(setq rs232cablepoint1 (list rs232bxcoord rs232bycoord))
						)
					)

				  	
					;;; shift with global variable based on panel type, main, secondary or auxiliary
					(if
						(and
						(/= rs232bvantagemainnumber 0)(= rs232bvantagesecondaryinteger 0)(= rs232bvantageauxiliarynumber 0)
						)
					  	(progn
						(setq rs232bxcoord (- rs232bxcoord vantagers232mainxcoordshift))
						)
					)

				  	(if
						(and
						(/= rs232bvantagesecondaryinteger 0)(= rs232bvantageauxiliarynumber 0)
						)
						(progn
						(setq rs232bxcoord (- rs232bxcoord vantagers232secondaryxcoordshift))
						)
					)





				  	(if
						(and
						(/= rs232bvantageauxiliarynumber 0)(= rs232bvantagesecondaryinteger 0)
						)
					  	(progn
						(setq rs232bxcoord (- rs232bxcoord vantagers232auxiliaryxcoordshift))  
						)
					)


				  
				  	;;; set insertionpoint and second cable point
				  	(setq rs232binsertionpoint (list rs232bxcoord rs232bycoord))
					(setq rs232cablepoint2 (list rs232bxcoord rs232bycoord))

				  
			  		(entmakex														;entmakex function
					(list															;list of all required items
					(cons 0 "insert")													;type of entity
				        (cons 2 rs232bvantageblock)												;name of block to insert
					(cons 8 "Riser")													;block layer
				       	(cons 10 rs232binsertionpoint)												;block insertion point
				       	(cons 41 1)														; scale factor
				       	(cons 42 1)														; scale factor?
				      	(cons 43 1)														; scale factor?
					)
					)


					(entmakex
					(list '
					(0 . "line")
					(cons 8 rs232layer)
					(cons 10 rs232cablepoint1)
					(cons 11 rs232cablepoint2)
					)
					)

				  	;;; use global variable to calculate xcoord from where RS232 block was inserted
					(setq rs332notexcoord (+ rs232bxcoord rs232notexcoordshift))
				  	(setq rs232notepoint (list rs332notexcoord rs232bycoord))

			  		(entmakex														;entmakex function
					(list															;list of all required items
					(cons 0 "insert")													;type of entity
				        (cons 2 rs232note)													;name of block to insert
				       	(cons 10 rs232notepoint)												;block insertion point
					(cons 8 rs232layer)
				       	(cons 41 1)														; scale factor
				       	(cons 42 1)														; scale factor?
				      	(cons 43 1)														; scale factor?
					)
					)				  
				  	(setq rs232bxcoord 0)
				  	(setq rs232bycoord 0)
				  	


				  
				  	(setq rs232bcntr (+ 1 rs232bcntr))
				)

	
	
  
)



(defun midpoint 	(
			p1 p2
			/
			midpointx midpointy returnList
			)

  	(setq midpointx (/ (+ (nth 0 p1)(nth 0 p2)) 2))		;set midpointx to the result of the first two elements from each point list, divied by 2
  	(setq midpointy (/ (+ (nth 1 p1)(nth 1 p2)) 2))		;set midpointy to the result of the second two elements from each point list, divied by 2
	(setq returnList (list midpointx midpointy))		;set middle point to the list of both
  								;leave midpoint as the last line so it gets returned to caller function
)



;;; --- vantage-riser-draw-lwpolyline---
;;; Notes:
;;; class-holder must be 0 for lwpolyline dxfcodes
;;; Alex Lundin 06-14-2017
(defun vantage-riser-draw-lwpolyline (point-list class-holder polyline-layer polyline-width polyline-linetype)
;;; arguments
;;;	point-list			
;;;	class-holder	must be 0	
;;; 	polyline-layer			
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
                      	(cons 70 class-holder))
       			(mapcar (function (lambda (p) (cons 10 p))) point-list))
	)
)