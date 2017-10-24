(defun *error* (msg)
(princ)
)




;;;06-26-2017
;;;gsrvantage
;;;generate vantage riser function
(defun c:gsrvantage 	( /
			     	customblockset drawinglocation file1 roomname textfileline vantageblock vantagemainlist
			     	vantagemainnumber vantagepanelsection vantagesecondarylist vantagesecondarynumber vantagesecondarypanelsection
			     	vantagestationbuslist vantagestationbusposition vantagestationbusrun vantagestationbussection vantagesublist
			 	vantagepanelcomponentlist vantagepanelcomponentsection
			 	vantage318list vantage318position vantage318section vantagedinlitelist vantagedinlitesection vantagers232list vantagers232section vantagestandalone
			 	vantagemainpanellist vantage318list vantagepanellist
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
(setq vantagestationbusmainxcoordstart 8)
(setq vantagestationbusmainycoordstart 3.8)  
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
(setq vantagestationbusmainycoordshift 7)
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
(setq vantagepoemainxcoordshift 4.0)
(setq vantagepoemainxcoordstart 8.0)
(setq vantagepoemainycoordstart 1.0)
  
;;; set note block names
(setq controllerbusnote "_V_HEX_NOTE_6")
(setq enclosurebusnote "_V_HEX_NOTE_7")
(setq stationbusnote "_V_HEX_NOTE_8")
(setq 318note "_V_HEX_NOTE_9")
(setq rs232note "_V_HEX_NOTE_10")

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
			(setq vantagepoesection 0)
			(setq vantage318section 0)
			)
			((= textfileline "Vantage Panel Components:")
			(setq vantagepanelsection 0)
			(setq vantagepanelcomponentsection 1)
			(setq vantagers232section 0)
	  		(setq vantagestationbussection 0)
			(setq vantagepoesection 0)
			(setq vantage318section 0)
			)
			((= textfileline "Vantage RS232:")
			(setq vantagepanelsection 0)
			(setq vantagepanelcomponentsection 0)
			(setq vantagers232section 1)
	  		(setq vantagestationbussection 0)
			(setq vantagepoesection 0)
			(setq vantage318section 0)
			)
			((= textfileline "Vantage Station Bus:")
			(setq vantagepanelsection 0)
			(setq vantagepanelcomponentsection 0)
			(setq vantagers232section 0)
	  		(setq vantagestationbussection 1)
			(setq vantagepoesection 0)
			(setq vantage318section 0)
			)
			((= textfileline "Vantage POE:")
			(setq vantagepanelsection 0)
			(setq vantagepanelcomponentsection 0)
			(setq vantagers232section 0)
	  		(setq vantagestationbussection 0)
			(setq vantagepoesection 1)
			(setq vantage318section 0)
			)			
			((= textfileline "Vantage 318:")
			(setq vantagepanelsection 0)
			(setq vantagepanelcomponentsection 0)
			(setq vantagers232section 0)
	  		(setq vantagestationbussection 0)
			(setq vantagepoesection 0)
			(setq vantage318section 1)
			)
			((= vantagepanelsection 1)
			(setq vantagemainnumber textfileline)
			(setq vantagesecondarynumber (read-line file1))
			(setq vantageauxiliarynumber (read-line file1))
			(setq vantageblock (read-line file1))
			(setq roomname (read-line file1))
			(setq vantagesublist (list vantagemainnumber vantagesecondarynumber vantageauxiliarynumber vantageblock roomname))							
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
			(setq vantagers232 (read-line file1))
			(setq vantagesublist (list vantagemainnumber vantagesecondarynumber vantageauxiliarynumber vantageblock roomname vantagers232))
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
			((= vantagepoesection 1)
			(setq vantagemainnumber textfileline)
			(setq vantageblock (read-line file1))
			(setq roomname (read-line file1))
			(setq vantagepoeposition (read-line file1))
			(setq vantagesublist (list vantagemainnumber vantageblock roomname vantagepoeposition))			 
			(setq vantagepoelist (cons vantagesublist vantagepoelist))
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
		(setq vantagepanellist (vl-sort vantagepanellist (function (lambda (x y)(< (car x)(car y))))))
		(vantagepanelbuilder vantagepanellist)
		)
	)
  	(if
	  	(/= vantagepanelcomponentlist nil)
	  	(progn
		(setq vantagepanelcomponentlist (vantagepanelcomponentsort vantagepanelcomponentlist))
	  	(setq vantagepanelcomponentlist (vl-sort vantagepanelcomponentlist (function (lambda (x y)(< (nth 2 x)(nth 2 y))))))
	  	(setq vantagepanelcomponentlist (vl-sort vantagepanelcomponentlist (function (lambda (x y)(< (nth 1 x)(nth 1 y))))))
	  	(setq vantagepanelcomponentlist (vl-sort vantagepanelcomponentlist (function (lambda (x y)(< (car x)(car y))))))
		(vantagepanelcomponentbuilder vantagepanelcomponentlist)
		)
	)

  	(if
	  	(/= vantagers232list nil)
		(progn
	  	(setq vantagers232list (vl-sort vantagers232list (function (lambda (x y)(< (car x)(car y))))))
	  	(setq vantagers232list (vl-sort vantagers232list (function (lambda (x y)(< (nth 1 x)(nth 1 y))))))
	  	(setq vantagers232list (vl-sort vantagers232list (function (lambda (x y)(< (nth 2 x)(nth 2 y))))))
		(vantagers232builder vantagers232list)
		)
	)

  	(if
	  	(/= vantagestationbuslist nil)
	  	(progn
		(setq vantagestationbuslist (vl-sort vantagestationbuslist (function (lambda (x y)(< (nth 6 x)(nth 6 y))))))
	  	(setq vantagestationbuslist (vl-sort vantagestationbuslist (function (lambda (x y)(< (nth 5 x)(nth 5 y))))))
	  	(setq vantagestationbuslist (vl-sort vantagestationbuslist (function (lambda (x y)(< (nth 2 x)(nth 2 y))))))
	  	(setq vantagestationbuslist (vl-sort vantagestationbuslist (function (lambda (x y)(< (nth 1 x)(nth 1 y))))))
		(setq vantagestationbuslist (vl-sort vantagestationbuslist (function (lambda (x y)(< (car x)(car y))))))
		(stationbussort vantagestationbuslist)
  		)
	)

  	(if
	  	(/= vantagepoelist nil)
	  	(progn
		(setq vantagepoelist (vl-sort vantagepoelist (function (lambda (x y)(< (nth 3 x)(nth 3 y))))))
	  	(setq vantagepoelist (vl-sort vantagepoelist (function (lambda (x y)(< (car x)(car y))))))
		(vantagepoebuilder vantagepoelist)
		)
	)
	      
  	
    	(if
	  	(/= vantage318list nil)
	  	(progn
		(vantage318builder vantage318list)
		)
	)	
  	
	
  	
  	
  	(draworder)
(vlax-release-object *acad acdoc)  	
(princ)
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

	
  	(setq total (acet-ss-union (list blocks controllerbuslines enclosurebuslines stationbuslines 318lines rs232lines mstplines 18awglines 20awglines cat5elines)))	;add the new [setname] to this line (setq total (acet-ss-union (list blocks cable ohm matchline [setname])))	
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
				lines
				)
	(setq lines (ssget "x" '((0 . "line" ))))

		(cond
		  	((/= lines nil)
			(command "draworder" "p" "" "BACK")
			)
		)
	(setq panels (ssget "x" '((8 . "Panels" ))))
  		(cond
		  	((/= panels nil)
			(command "draworder" "p" "" "BACK")
			)
		)

	(setq lines (ssget "x" '((8 . "_WS_CONTROLLER_BUS" ))))

		(cond
		  	((/= lines nil)
			(command "draworder" "p" "" "BACK")
			)
		)
  
  	(command "regenall")

)


(defun vantagepanelcomponentsort
       				(
				vantagepanelcomponentsortlist
				/
				VPCSCMAX VPCSCNTR VPCSITEM VPCSROOMNAME VPCSVANTAGEAUXILIARYNUMBER VPCSVANTAGEBLOCK VPCSVANTAGEMAINNUMBER VPCSVANTAGESECONDARYINTEGER VPCSVANTAGESECONDARYNUMBER VPCSXCOORD VPCSYCOORD
				RETURNLIST VPCSLVOSLIST VPCSMODULEPOSITION1LIST VPCSMODULEPOSITION2LIST VPCSMODULEPOSITION3LIST VPCSMODULEPOSITION4LIST VPCSMODULEPOSITION5LIST
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
						)
					)

				  
					(if
					  	(OR
						;modules
						(= vpcsvantageblock "V-UDM08-EM")
						)
					  	(progn
						(setq vpcsModulePosition1List(cons vpcsitem vpcsModulePosition1List))
						)
					)
					(if
					  	(OR
						;modules
						(= vpcsvantageblock "V-SDM12-EM")
						)
					  	(progn
						(setq vpcsModulePosition2List (cons vpcsitem vpcsModulePosition2List))
						)
					)					
					(if
					  	(OR
						;modules
						(= vpcsvantageblock "V-FANMOD")
						)
					  	(progn
						(setq vpcsModulePosition3List(cons vpcsitem vpcsModulePosition3List))
						)
					)
					(if
					  	(OR
						;modules
						(= vpcsvantageblock "V-MDR8CW301")
						)
					  	(progn
						(setq vpcsModulePosition4List(cons vpcsitem vpcsModulePosition4List))
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
						)
					)				  
				  
				  	(setq vpcscntr (+ 1 vpcscntr))
				)
  

				(setq returnList(append vpcsLvosList vpcsModulePosition1List vpcsModulePosition2List vpcsModulePosition3List vpcsModulePosition4List vpcsModulePosition5List))
	
  
)

(defun vantagepoebuilder
       				(
				poelist
				/
				vpoebblockname vpoebcmax vpoebcntr vpoebinsertionpoint vpoebitem vpoebmainprevious
				vpoebp1 vpoebp2 vpoebp3 vpoebp4 vpoebp5 vpoebroomname vpoebstationbuscablepoint1 vpoebstationbuscablepoint2
				vpoebpoelistprevious vpoebvantageblock vpoebvantagemainnumber vpoebvantagestationbusposition vpoebvantagepoeposition
				vpoebxcoord vpoebxcoordprevious vpoebxshift vpoebycoord vpoebycoordprevious vpoebycoordshift
				 vpoebAUXILIARYPREVIOUS vpoebSECONDARYPREVIOUS vpoebSUBLIST vpoebVANTAGEAUXILIARYNUMBER vpoebVANTAGEAUXILIARYNUMBERSTRING vpoebVANTAGECONNECTIONBLOCK vpoebVANTAGESECONDARYINTEGER vpoebVANTAGESECONDARYLETTER vpoebVANTAGESECONDARYNUMBER vpoebVANTAGESECONDARYNUMBERCHECK vpoebVANTAGESECONDARYNUMBERSTRING vpoebXCOORDSHIFT
				)

				(setq vpoebcntr 0)
  
				;;; priming read, to add connection block to front of any run
				(setq vpoebitem (nth vpoebcntr poelist))
				(setq vpoebvantagemainnumber (nth 0 vpoebitem))
				(setq vpoebvantagemainnumber (atoi vpoebvantagemainnumber))	
				(setq vpoebvantageblock (nth 1 vpoebitem))
				(setq vpoebroomname (nth 2 vpoebitem))
				(setq vpoebvantagepoeposition (nth 3 vpoebitem))
				(setq vpoebvantagepoeposition (atoi vpoebvantagepoeposition))
  	
  
      				(setq vpoebxcoord 0)
				(setq vpoebycoord 0)
  				
  				(setq vpoebcmax (length poelist))
  
  				(while	(< vpoebcntr vpoebcmax)
				  	(setq vpoebxcoord 0)
 
					(setq vpoebitem (nth vpoebcntr poelist))
					(setq vpoebitem (nth vpoebcntr poelist))
					(setq vpoebvantagemainnumber (nth 0 vpoebitem))
					(setq vpoebvantagemainnumber (atoi vpoebvantagemainnumber))	
					(setq vpoebvantageblock (nth 1 vpoebitem))
					(setq vpoebroomname (nth 2 vpoebitem))
					(setq vpoebvantagepoeposition (nth 3 vpoebitem))
					(setq vpoebvantagepoeposition (atoi vpoebvantagepoeposition))

				  
					(setq vpoebycoord (* vpoebvantagemainnumber vantagemainpanelycoordshift))


				  

					(setq vpoebxcoordshift (* vantagepoemainxcoordstart -1))
					(setq vpoebxcoord (+ vpoebxcoord vpoebxcoordshift))
					(setq vpoebxcoordshift (* vpoebvantagepoeposition (* vantagepoemainxcoordshift -1)))
					(setq vpoebxcoord (+ vpoebxcoord vpoebxcoordshift))
					(setq vpoebycoordshift (* vantagepoemainycoordstart -1))
					(setq vpoebycoord (+ vpoebycoord vpoebycoordshift))				  

				  

				  	(if
					  	(and
						(= vpoebvantagesecondaryinteger 0)
						(= vpoebvantageauxiliarynumber 0)
						)
					  	(progn


;;;				  		(cond
;;;							((/= vpoebvantagepoeposition 1)
;;;							;;; use a temporary shift calculator variable, vpoebycoordshift, to calculate the distance to shift the run with the global variable
;;;							;;; subtract 1 of the global variable amounts back from the temporary shift calculator, to account for the shift not occuring on the first run
;;;							;;; now subtract the temporary shift variable from the current ycoord position
;;;							(setq vpoebycoordshift (* vantagestationbusmainycoordshift vpoebvantagepoeposition))
;;;							(setq vpoebycoordshift (- vpoebycoordshift vantagestationbusmainycoordshift))
;;;							(setq vpoebycoord (- vpoebycoord vpoebycoordshift))
;;;							)
;;;						)

						
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






				  
;;;				  	(if
;;;					  	(and
;;;						(= vpoebpoelistprevious vpoebvantagepoeposition)(= vpoebmainprevious vpoebvantagemainnumber)(= vpoebsecondaryprevious vpoebvantagesecondaryinteger)(= vpoebauxiliaryprevious vpoebvantageauxiliarynumber)
;;;						)
;;;					  	(progn
;;;						(setq vpoebstationbuscablepoint1 (list vpoebxcoordprevious vpoebycoordprevious))  
;;;						(setq vpoebstationbuscablepoint2 (list vpoebxcoord vpoebycoord))
;;;
;;;						(entmakex
;;;						(list '
;;;						(0 . "line")
;;;						(cons 8 stationbuslayer)
;;;						(cons 10 vpoebstationbuscablepoint1)
;;;						(cons 11 vpoebstationbuscablepoint2)
;;;						)
;;;						)
;;;
;;;							
;;;						;;;place cable note if not first position
;;;						(if
;;;						(/= vpoebvantagestationbusposition 1)
;;;						(progn
;;;						  	(setq vpoebmidpoint (midpoint vpoebstationbuscablepoint1 vpoebstationbuscablepoint2))
;;;
;;;					  		(entmakex														;entmakex function
;;;							(list															;list of all required items
;;;							(cons 0 "insert")													;type of entity
;;;						        (cons 2 stationbusnote)													;name of block to insert
;;;						       	(cons 10 vpoebmidpoint)													;block insertion point
;;;							(cons 8 stationbuslayer)												;block layer
;;;						       	(cons 41 1)														;scale factor
;;;						       	(cons 42 1)														;scale factor?
;;;						      	(cons 43 1)														;scale factor?
;;;							)
;;;							)
;;;						)
;;;						)
;;;
;;;
;;;
;;;						
;;;						)
;;;					)				  



				  
				  
				  	(setq vpoebmainprevious vpoebvantagemainnumber)
				  	(setq vpoebsecondaryprevious vpoebvantagesecondaryinteger)
					(setq vpoebauxiliaryprevious vpoebvantageauxiliarynumber)
				  	(setq vpoebpoelistprevious vpoebvantagepoeposition)
				  	(setq vpoebxcoordprevious vpoebxcoord)
				  	(setq vpoebycoordprevious vpoebycoord)
					(setq vpoebcntr (+ 1 vpoebcntr))
				)

 	
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
				stationbusrun
				/
				vsbbblockname vsbbcmax vsbbcntr vsbbinsertionpoint vsbbitem vsbbmainprevious
				vsbbp1 vsbbp2 vsbbp3 vsbbp4 vsbbp5 vsbbroomname vsbbstationbuscablepoint1 vsbbstationbuscablepoint2
				vsbbstationbusrunprevious vsbbvantageblock vsbbvantagemainnumber vsbbvantagestationbusposition vsbbvantagestationbusrun
				vsbbxcoord vsbbxcoordprevious vsbbxshift vsbbycoord vsbbycoordprevious vsbbycoordshift
				 VSBBAUXILIARYPREVIOUS VSBBSECONDARYPREVIOUS VSBBSUBLIST VSBBVANTAGEAUXILIARYNUMBER VSBBVANTAGEAUXILIARYNUMBERSTRING VSBBVANTAGECONNECTIONBLOCK VSBBVANTAGESECONDARYINTEGER VSBBVANTAGESECONDARYLETTER VSBBVANTAGESECONDARYNUMBER VSBBVANTAGESECONDARYNUMBERCHECK VSBBVANTAGESECONDARYNUMBERSTRING VSBBXCOORDSHIFT
				)

				(setq vsbbcntr 0)
  
				;;; priming read, to add connection block to front of any run
				(setq vsbbitem (nth vsbbcntr stationbusrun))
				(setq vsbbvantagemainnumber (nth 0 vsbbitem))
				(setq vsbbvantagemainnumber (atoi vsbbvantagemainnumber))
				(setq vsbbvantagesecondarynumber (nth 1 vsbbitem))
				;;; secondary is integer and character together in a string, so atoi first item which is the integer
				(setq vsbbvantagesecondaryinteger (substr vsbbvantagesecondarynumber 1 1))
				(setq vsbbvantagesecondaryinteger (atoi vsbbvantagesecondaryinteger))
				(setq vsbbvantageauxiliarynumber (nth 2 vsbbitem))
				(setq vsbbvantageauxiliarynumber (atoi vsbbvantageauxiliarynumber))	
				(setq vsbbvantageblock (nth 3 vsbbitem))
				(setq vsbbroomname (nth 4 vsbbitem))
				(setq vsbbvantagestationbusrun (nth 5 vsbbitem))
				(setq vsbbvantagestationbusrun (atoi vsbbvantagestationbusrun))
				(setq vsbbvantagestationbusposition (nth 6 vsbbitem))
				(setq vsbbvantagestationbusposition (atoi vsbbvantagestationbusposition))
  	
				;;; make strings for connectionblockname
  				(setq vsbbvantagesecondarynumberstring (itoa vsbbvantagesecondaryinteger))
  				;;; create a "0" or "1" for block insert string
  				(cond
				  	((= vsbbvantagesecondarynumberstring "0")					; if we are not on a secondary panel
  					(setq vsbbvantagesecondarynumbercheck "0")					; set check variable to "0" for connection block insert
					)
					(t										; otherwise
  					(setq vsbbvantagesecondarynumbercheck "1")					; set check variable to "1" for connection block insert
					)
				)
  				(setq vsbbvantageauxiliarynumberstring (itoa vsbbvantageauxiliarynumber))
  				(setq connectionblocknamesuffix "stationbusruntext")
  				(setq vsbbvantageconnectionblock (strcat vsbbvantagesecondarynumbercheck vsbbvantageauxiliarynumberstring connectionblocknamesuffix))

  				;;; convert numbers back to strings to add the item back to the list in the correct format
  				(setq vsbbvantagemainnumber (itoa vsbbvantagemainnumber))
  				(setq vsbbvantagesecondaryinteger (itoa vsbbvantagesecondaryinteger))
  				(setq vsbbvantageauxiliarynumber (itoa vsbbvantageauxiliarynumber))
  				(setq vsbbvantagestationbusrun (itoa vsbbvantagestationbusrun))
  				(setq vsbbvantagestationbusposition "0")

  				;;; add this new item back to front of the stationbusrun list for the builder to use
				(setq vsbbsublist (list vsbbvantagemainnumber vsbbvantagesecondarynumber vsbbvantageauxiliarynumber vsbbvantageconnectionblock vsbbroomname vsbbvantagestationbusrun vsbbvantagestationbusposition))							
				(setq stationbusrun (cons vsbbsublist stationbusrun)) 
				(setq vsbbsublist nil)			


  
      				(setq vsbbxcoord 0)
				(setq vsbbycoord 0)
  				
  				(setq vsbbcmax (length stationbusrun))
  
  				(while	(< vsbbcntr vsbbcmax)
				  	(setq vsbbxcoord 0)
 
					(setq vsbbitem (nth vsbbcntr stationbusrun))
					(setq vsbbvantagemainnumber (nth 0 vsbbitem))
					(setq vsbbvantagemainnumber (atoi vsbbvantagemainnumber))
				  	(setq vsbbvantagesecondarynumber (nth 1 vsbbitem))
					;;; secondary is integer and character together in a string, so atoi first item which is the integer
				  	(setq vsbbvantagesecondaryinteger (substr vsbbvantagesecondarynumber 1 1))
				  	(setq vsbbvantagesecondaryinteger (atoi vsbbvantagesecondaryinteger))
				  	(setq vsbbvantageauxiliarynumber (nth 2 vsbbitem))
				  	(setq vsbbvantageauxiliarynumber (atoi vsbbvantageauxiliarynumber))	
					(setq vsbbvantageblock (nth 3 vsbbitem))
					(setq vsbbroomname (nth 4 vsbbitem))
				  	(setq vsbbvantagestationbusrun (nth 5 vsbbitem))
				  	(setq vsbbvantagestationbusrun (atoi vsbbvantagestationbusrun))
					(setq vsbbvantagestationbusposition (nth 6 vsbbitem))
				  	(setq vsbbvantagestationbusposition (atoi vsbbvantagestationbusposition))

				  
					(setq vsbbycoord (* vsbbvantagemainnumber vantagemainpanelycoordshift))


				  
				  	;;;if the secondary integer is not zero, then get the letter after the integer
				  	(cond
					  	((/= vsbbvantagesecondaryinteger 0)
				  		(setq vsbbvantagesecondaryletter (substr vsbbvantagesecondarynumber 2 1))
						)
					)
				  
				  	(cond
					  	((/= vsbbvantageauxiliarynumber 0)
						(setq vsbbxcoordshift (* vantagestationbusauxiliaryxcoordstart 1))
						(setq vsbbxcoord (+ vsbbxcoord vsbbxcoordshift))
						(setq vsbbxcoordshift (* vsbbvantagestationbusposition vantagestationbusauxiliaryxcoordshift))
						(setq vsbbxcoord (+ vsbbxcoord vsbbxcoordshift))
						(setq vsbbycoordshift (* vantagestationbusauxiliaryycoordstart -1))
						(setq vsbbycoord (+ vsbbycoord vsbbycoordshift))
						(cond
							((/= vsbbvantagestationbusrun 1)
							;;; use a temporary shift calculator variable, vsbbycoordshift, to calculate the distance to shift the run with the global variable
							;;; subtract 1 of the global variable amounts back from the temporary shift calculator, to account for the shift not occuring on the first run
							;;; now subtract the temporary shift variable from the current ycoord position
							(setq vsbbycoordshift (* vantagestationbusauxiliaryycoordshift vsbbvantagestationbusrun))
							(setq vsbbycoordshift (- vsbbycoordshift vantagestationbusauxiliaryycoordshift))
							(setq vsbbycoord (- vsbbycoord vsbbycoordshift))
							)
						)
						)
					)
				  
				  	(cond
					  	((/= vsbbvantagesecondaryinteger 0)
						(setq vsbbxcoordshift (* vantagestationbussecondaryxcoordstart 1))
						(setq vsbbxcoord (+ vsbbxcoord vsbbxcoordshift))
						(setq vsbbxcoordshift (* vsbbvantagestationbusposition vantagestationbussecondaryxcoordshift ))
						(setq vsbbxcoord (+ vsbbxcoord vsbbxcoordshift))
						(setq vsbbycoordshift (* vantagestationbussecondaryycoordstart -1))
						(setq vsbbycoord (+ vsbbycoord vsbbycoordshift))
						(cond
							((/= vsbbvantagestationbusrun 1)
							;;; use a temporary shift calculator variable, vsbbycoordshift, to calculate the distance to shift the run with the global variable
							;;; subtract 1 of the global variable amounts back from the temporary shift calculator, to account for the shift not occuring on the first run
							;;; now subtract the temporary shift variable from the current ycoord position
							(setq vsbbycoordshift (* vantagestationbussecondaryycoordshift vsbbvantagestationbusrun))
							(setq vsbbycoordshift (- vsbbycoordshift vantagestationbussecondaryycoordshift))
							(setq vsbbycoord (- vsbbycoord vsbbycoordshift))
							)
						)
						)
					)
				  	(if
					  	(and
						(= vsbbvantagesecondaryinteger 0)
						(= vsbbvantageauxiliarynumber 0)
						)
					  	(progn

						(setq vsbbxcoordshift (* vantagestationbusmainxcoordstart -1))
						(setq vsbbxcoord (+ vsbbxcoord vsbbxcoordshift))
						(setq vsbbxcoordshift (* vsbbvantagestationbusposition (* vantagestationbusmainxcoordshift -1)))
						(setq vsbbxcoord (+ vsbbxcoord vsbbxcoordshift))
						(setq vsbbycoordshift (* vantagestationbusmainycoordstart -1))
						(setq vsbbycoord (+ vsbbycoord vsbbycoordshift))
				  		(cond
							((/= vsbbvantagestationbusrun 1)
							;;; use a temporary shift calculator variable, vsbbycoordshift, to calculate the distance to shift the run with the global variable
							;;; subtract 1 of the global variable amounts back from the temporary shift calculator, to account for the shift not occuring on the first run
							;;; now subtract the temporary shift variable from the current ycoord position
							(setq vsbbycoordshift (* vantagestationbusmainycoordshift vsbbvantagestationbusrun))
							(setq vsbbycoordshift (- vsbbycoordshift vantagestationbusmainycoordshift))
							(setq vsbbycoord (- vsbbycoord vsbbycoordshift))
							)
						)

						
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






				  
				  	(if
					  	(and
						(= vsbbstationbusrunprevious vsbbvantagestationbusrun)(= vsbbmainprevious vsbbvantagemainnumber)(= vsbbsecondaryprevious vsbbvantagesecondaryinteger)(= vsbbauxiliaryprevious vsbbvantageauxiliarynumber)
						)
					  	(progn
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

							
						;;;place cable note if not first position
						(if
						(/= vsbbvantagestationbusposition 1)
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
					)				  



				  
				  
				  	(setq vsbbmainprevious vsbbvantagemainnumber)
				  	(setq vsbbsecondaryprevious vsbbvantagesecondaryinteger)
					(setq vsbbauxiliaryprevious vsbbvantageauxiliarynumber)
				  	(setq vsbbstationbusrunprevious vsbbvantagestationbusrun)
				  	(setq vsbbxcoordprevious vsbbxcoord)
				  	(setq vsbbycoordprevious vsbbycoord)
					(setq vsbbcntr (+ 1 vsbbcntr))
				)

 	
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
				vantagepanelcomponentbuilderlist
				/
				maincablepoint1 maincablepoint2 vantagemainnumber vpcbblockname
				vpcbcmax vpcbcntr vpcbinsertionpoint vpcbitem vpcbroomname vpcbvantageblock
				vpcbvantagemainnumber vpcbxcoord vpcbycoord vpcbycoord1
				vantagemainsublist vpcbycoordprevious
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

				  	(setq vpcbycoordshift (* vpcbvantagemainnumber vantagemainpanelycoordshift))
					(setq vpcbycoord (+ vpcbycoord vpcbycoordshift))

				  
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
				vantagepanelbuilderlist
				/
				maincablepoint1 maincablepoint2 vantagemainnumber vmbblockname
				vmbcmax vmbcntr vmbinsertionpoint vmbitem vmbroomname vmbvantageblock
				vmbvantagemainnumber vmbxcoord vmbycoord vmbycoord1
				vantagemainsublist
				 SECONDARYCABLEPOINT1 SECONDARYCABLEPOINT2 SECONDARYCABLEPOINT3 SECONDARYCABLEPOINT4
				 VMB1/3GAP VMBGAPBETWEENPANELS VMBMIDPOINT VMBSECONDARYYCOORDSHIFT VMBVANTAGEAUXILIARYNUMBER VMBVANTAGESECONDARYINTEGER VMBVANTAGESECONDARYLETTER VMBVANTAGESECONDARYNUMBER VMBXCOORDMAINPANELEDGE VMBXCOORDSECONDARYCABLE VMBYCOORDMAINCABLE VMBYCOORDSECONDARYCABLE VMBYCOORDSHIFT
				)

  				(setq vmbxcoord 0)
				(setq vmbycoord 0)	
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

				  	(setq vmbycoordshift (* vmbvantagemainnumber vantagemainpanelycoordshift))
					(setq vmbycoord (+ vmbycoord vmbycoordshift))

				  
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

				  	;;; draw lines and note for main panel
				  	(if
					  	(and
						(/= vmbvantagemainnumber 1)(= vmbvantagesecondaryinteger 0)(= vmbvantageauxiliarynumber 0)
						)
					  	(progn
				  		(setq vmbycoordmaincable (- vmbycoord vantagemainpanelycoordshift))

						;;; horizontal line
						(setq maincablepoint1 (list -1.75 vmbycoord))
						(setq maincablepoint2 (list -1.75 vmbycoordmaincable))
						(entmakex
						(list '
						(0 . "line")
						(cons 8 controllerbuslayer)      
						(cons 10 maincablepoint1)
						(cons 11 maincablepoint2)
						)
						)
						
						(setq vmbmidpoint (midpoint maincablepoint1 maincablepoint2))

						;;; note
						(entmakex													;entmakex function
						(list														;list of all required items
						(cons 0 "insert")												;type of entity
					        (cons 2 controllerbusnote)											;name of block to insert
						(cons 8 controllerbuslayer)											;block layer
					       	(cons 10 vmbmidpoint)												;block insertion point
					       	(cons 41 1)													; scale factor
					       	(cons 42 1)													; scale factor?
					      	(cons 43 1)													; scale factor?
						)
						)
					  	
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
			midpointx midpointy midpoint
			)

  	(setq midpointx (/ (+ (nth 0 p1)(nth 0 p2)) 2))		;set midpointx to the result of the first two elements from each point list, divied by 2
  	(setq midpointy (/ (+ (nth 1 p1)(nth 1 p2)) 2))		;set midpointy to the result of the second two elements from each point list, divied by 2
	(setq midpoint (list midpointx midpointy))		;set middle point to the list of both
  								;leave midpoint as the last line so it gets returned to caller function
)



