
 ;                                                    ;
 ;                                                    ;
 ;                                                    ;
 ;              WattStopper LISP Dialog               ;
 ;                                                    ;
 ;                                                    ;
 ;                                                    ;

(defun C:ws (/ *error*)
  (defun *error* (msg)
    (setvar "cmdecho" 1)
    (princ "\nRoutine terminated.")
    (princ)
  ) ;_ end of defun
  (alert
	(strcat
    	"\nWattStopper LISP (updated 07-19-2017)"
 	"\nCreated by Shea Sterner"
 	"\nMaintained and improved by Alex Lundin"
	)
  ) ;_ end of alert

	(setq f1 (open "C:\\WS_Blocks\\Default\\LISP\\AutoCAD_Codes.txt" "a"))						;set f1 to to result of open on the string created from dwgprefix variable plus "Riser_Extraction.txt" open file for appending 
	(close f1)													;close f1
  	(if	(setq File#1 (open "C:\\WS_Blocks\\Default\\LISP\\AutoCAD_Codes.txt" "w"))				;set File#1 variable to the same as f1 -- if statement to exit if we can't open specified file
		(progn													;progn to group all following statements into one block
		;these lines write text to file
		(write-line "Information about WS_Blocks folder structure" File#1)
		(write-line "There are 3 root folders:" File#1)
		(write-line "" File#1)
		(write-line "C:\\WS_Blocks\\Autosaves" File#1)
		(write-line "C:\\WS_Blocks\\Custom" File#1)
		(write-line "C:\\WS_Blocks\\Default" File#1)
		(write-line "" File#1)
		(write-line "Location of Standard Work, Room Details Index and Room Details Template:" File#1)
		(write-line "C:\\WS_Blocks\\Default\\CAD Documentation\\Standard Work" File#1)
		(write-line "C:\\WS_Blocks\\Default\\CAD Documentation\\Standard Work\\Room Detail Index.xls" File#1)
		(write-line "C:\\WS_Blocks\\Default\\Templates\\RD-DLM.dwg" File#1)
		(write-line "" File#1)
		(write-line "Locaton of Setup Documents:" File#1)
		(write-line "C:\\WS_Blocks\\Default\\CAD Documentation\\AutoCAD Setup Documents" File#1)
		(write-line "" File#1)
		(write-line "Locations of Tool Palette Groups for importing:" File#1)
		(write-line "C:\\WS_Blocks\\Default\\ToolPallete\\Palettes" File#1)
		(write-line "Default Palette Group.xpg" File#1)
		(write-line "C:\\WS_Blocks\\Custom\\ToolPallete\\Palettes" File#1)
		(write-line "Custom Palette Group.xpg" File#1)
		(write-line "" File#1)
		(write-line "Locations of drawing Templates:" File#1)
		(write-line "C:\\WS_Blocks\\Default\\Templates" File#1)
		(write-line "" File#1)
		(write-line "Information about network drive locations." File#1)	
		(write-line "Only copy the WS_Blocks folder from these two locations:" File#1)				
		(write-line "H:\\Drawings" File#1)
		(write-line "P:\\1 AutoCAD Main" File#1)
		(write-line "" File#1)
		(write-line "You can also use the windows batch processes to get enhancments quickly" File#1)
		(write-line "Close AutoCAD and then copy one of these locations into windows explorer" File#1)
		(write-line "H:\\Drawings\\WS_Blocks_Weekly.bat" File#1)
		(write-line "P:\\Drawings\\WS_Blocks_Weekly.bat" File#1)
		(write-line "" File#1)
		(write-line "Information about LISP commands" File#1)
		(write-line "039.............Run from Paper Space -- Scale paper space key blocks by .039" File#1)
		(write-line "AUTOB...........Fill out SEGEMENT and POSITION attributes on networking blocks based on cable run" File#1)
		(write-line "AUTOB2..........Move all blocks back to default layers after AUTOB, restore WS-LAYOUT layer state" File#1)
		(write-line "BRIDGES.........Start of BRIDGES sequence, follow alerts to run each command" File#1)
		(write-line "BRIDGES2........Moves blocks to certain layers so user can verify cable runs" File#1)
		(write-line "BRIDGES3........End of BRIDGES sequence, moves blocks and cable back to default layers" File#1)
		(write-line "EBLOCK..........Extract all ROOMID, SEGMENT and POSITION attribute information from blocks WATTSTOPPER layer to a text file inside the folder where the drawing is located" File#1)
		(write-line "EBLOCK2.........Same as above, this uses the previous riser extraction format" File#1)
		(write-line "EBLOCK3.........Same as above, this uses the previous riser extraction format" File#1)
		(write-line "EBLOCKRD........Same as above, for Room_Details_Extraction.txt" File#1)
		(write-line "GSR.............Generate Single Line Riser from Riser_Extraction.txt generated from EBLOCK function" File#1)
		(write-line "GSR2............Exact same functionality as GSR, this is the previous riser" File#1)
		(write-line "GSR3............Exact same functionality as GSR, this is the previous riser" File#1)
		(write-line "GRD.............Generate Room Details Point to Point wiring from Room_Details_Extraction.txt" File#1)
		(write-line "LDELETE.........Seach entire drawing for blocks called Legend, delete and purge them all" File#1)
		(write-line "NOID............Move empty ROOM ID blocks on Wattstopper to NOID layer, then non empty ROOM ID blocks on NOID to Wattstopper" File#1)
		(write-line "NOID2...........Move empty any items from NOID to Wattstopper layer regardless if ROOM ID has been filled out" File#1)
		(write-line "MATCHATTS.......Match certain attributes chosen by user between multiple blocks." File#1)
		(write-line "P1..............Polyline function for RD-DLM file." File#1)
		(write-line "PDFIMPORT.......Script creator for seting up a bactch of PDF external referenced drawings. Saves drawings as \"WS-1\" and so on" File#1)
		(write-line "PDFIMPORT2......Same as above, except uses PDF names as drawing names" File#1)
		(write-line "PDFSCALE........Utility for scaling PDFs converted to DXF" File#1)
		(write-line "REPLACE.........Replaces selected regular block(s) with WattStopper Dynamic Block(s)" File#1)
		(write-line "RISERDRAWORDER..Sets draw order for SL-DLM file" File#1)
		(write-line "RISERLAYOUT.....Creates script file to copy all blocks and cable from a set of WS-Layouts into a blank file" File#1)
		(write-line "RISERLAYOUT2....Same as above, this uses the previous riser commands and templates from June-2017" File#1)
		(write-line "RISERLAYOUT3....Same as above, this uses the previous riser commands and templates from August-2016" File#1)
		(write-line "RISERLAYOUT4....Same as above, this uses the very first riser commands and templates from 2016" File#1)
		(write-line "UPBS............Updates all blocks on WATTSTOPPER layer to current version found anywhere in support file search path." File#1)
		(write-line "WSBACK..........Bring all objects on WATTSTOPPER to back of draw order." File#1)
		(write-line "WSBINDALL.......Bind all xrefs and purge unused information" File#1)
		(write-line "WSCABLE.........Measure all splines on _WS_MSTP layer and outputs to text file" File#1)
		(write-line "WSCABLE2........Measure all splines on _WS_CAT5E layer and outputs to text file" File#1)
		(write-line "WSCABLEALL......Measure all splines and plines on default cable layers and outputs to text file" File#1)
		(write-line "WSCABLEI........Measure individual spline selected by user and output length to command line" File#1)
		(write-line "WSCABLEM........Measure all splines and plines from client selected layer" File#1)
		(write-line "WSCABLEM2.......Delete all dimensions from WSCABLEM function" File#1)
		(write-line "WSCOUNT.........Run from Model Space -- Count all blocks on WattStopper layer" File#1)
		(write-line "WSCPL...........Copy all cable and blocks from default layers, used to make manual adjustments to Riser-layout.dwg" File#1)
		(write-line "WSCPRID.........Find all empty ROOMID blocks and copy nearest ROOMID from a filled out block into the blank one" File#1)
		(write-line "WSFRONT.........Bring all objects on WATTSTOPPER to front of draw order." File#1)
		(write-line "WSHATCH.........Delete all solid hatch on drawing (used on PDF Conversions mostly)" File#1)
		(write-line "WSHATCH2........Delete all solid hatch on a selected layer in the drawing (used on PDF Conversions mostly)" File#1)
		(write-line "WSHLT...........Move all WATTSTOPPER blocks of a certain type to a separate layer called HIGHLIGHT" File#1)
		(write-line "WSHLT2..........Move all HIGHLIGHT blocks of a certain type back to WATTSTOPPER" File#1)		
		(write-line "WSLAYCUR........Set WattStopper layer as current" File#1)
		(write-line "WSLAYFRZ........Freeze all layers except WATTSTOPPER" File#1)
		(write-line "WSLAYLOCK.......Lock all layers except WattStopper" File#1)
		(write-line "WSLAYOFF........Turn off layers named or selected (supports nested layers)" File#1)
		(write-line "WSLAYOUT........Add a WattStopper layout with title block from WATTSTOPPER.dwt" File#1)
		(write-line "WSLAYTHW........Thaw all layers" File#1)
		(write-line "WSLAYUNLOCK.....Unlock all layers" File#1)
		(write-line "WSLEGEND........Run from Paper Space -- Automatic creation of WattStopper legend per active sheet" File#1)
		(write-line "WSLEGENDCOUNT...Run from Paper Space -- Automatic creation of WattStopper legend with block counts from Model Space" File#1)
		(write-line "WSLEGENDCOUNTPL.Run from Paper Space -- Automatic creation of WattStopper legend with block counts from Model Space inside a polyline" File#1)
		(write-line "WSMTB...........Pull all attributes and values from any block -- works best on title blocks" File#1)
		(write-line "WSMTB2..........Paste all values into the matching attributes -- the attributes must be exactly identical" File#1)
		(write-line "WSNEW...........Pull all attributes and values from any block, then delete that block and purge -- this is only meant for title blocks" File#1)
		(write-line "WSNEW2..........Import new title block, then paste all values into the matching attributes -- the attributes must be exactly identical" File#1)
		(write-line "WSQUOTETOOLBOM..Create XLM export of all product blocks on WATTSTOPPER layer in current drawing." File#1)
		(write-line "WSREPLACE.......Replaces selected WattStopper Dynamic Block(s) with WattStopper Dynamic Block(s), this will keep the Angle attribute" File#1)
		(write-line "WSREPLACE2......Replaces selected WattStopper Dynamic Block(s) with WattStopper Dynamic Block(s), this will keep the Angle1 attribute" File#1)
		(write-line "WSRID...........Search all blocks on WATTSTOPPER layer for a specific ROOMID attribute value." File#1)
		(write-line "WSSAVEAS........Saves file with 'WS-' prefix" File#1)
		(write-line "WSTBEXPORT......Create text file from all attributes and values from a titleblock. Also writes revision layer states" File#1)
		(write-line "WSTBIMPORT......Bring data in from the title block export into any block with matching attributes. Also toggles revision layer states" File#1)		
		(write-line "WSWP............Fixes block wipeout draw order." File#1) 
		)
	)
  	(close File#1)
  	(startapp "notepad" "C:\\WS_Blocks\\Default\\LISP\\AutoCAD_Codes.txt")
	(princ)
) ;_ end of defun

(setvar "attreq" 0)
 ;                                                    ;
 ;                                                    ;
 ;                                                    ;
 ;            WattStopper Block Count                 ;
 ;                                                    ;
 ;                                                    ;
 ;                                                    ;

(defun C:WSCOUNT (/
		  blSet	blList	   nameList   listLen	 outList    exFlag     oldExFlag  exPath     exApp	exWorkbook exFileexSheets	 exSheet    curId      newFile
		  curCell    curVal	oldecho	   enttype    entname	    position   elst1	  entlist    datalist	e1	   newdata    qty	 bna	    blknm      routlist
		  result     *error* CD-R CD-S DCL_ID DDIAG DT DY ENT FILE HR ITEM MN MO TM YR
		 )
  (defun *error* (msg)
    (setvar "cmdecho" 1)
    (princ "\nRoutine terminated.")
    (princ)
  ) ;_ end of defun


;;;-- Build the list


  (setq oldecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)

  (vl-load-com)

  (if (setq blSet (ssget "x" (list (cons 8 "WATTSTOPPER") (cons 410 "Model") (cons 0 "Insert"))))
    (progn
      (setq blList   (mapcar 'vlax-ename->vla-object
			     (vl-remove-if
			       'listp
			       (mapcar 'cadr (ssnamex blSet))
			     ) ;_ end of vl-remove-if
		     ) ;_ end of mapcar
	    nameList (vl-sort (mapcar '(lambda (X) (vla-get-effectivename x)) blList) '<)
	    listLen  (length nameList)
      ) ;_ end of setq
 ;End of Residential Symbol Handling
      (setq dataList (list))
      (setq ss (ssget "x" (list (cons 8 "WATTSTOPPER") (cons 410 "Model") (cons 0 "Insert"))))
      (setq position 0)
      (while (setq ent (ssname ss position))
	(setq entlist (entget ent))
	(setq elst1 entlist)
	(setq blknm (cdr (assoc 2 elst1)))
	(if (= "*U" (substr blknm 1 2))
	  (progn
	    (repeat 3
	      (setq elst1 (entget (cdr (assoc 360 elst1))))
	    ) ;_ end of repeat
	    (setq elst1 (entget (cdr (assoc 340 elst1))))
	    (setq entname (cdr (assoc 2 elst1)))
	  ) ;_ end of progn
	) ;_ end of if
	(setq position (1+ position)
	) ;_ end of setq
      ) ;_ end of while
      (setq datalist (acad_strlsort datalist))
      (foreach x datalist
	(setq Result
	       (if (setq Item (assoc x Result))
		 (subst (cons x (1+ (cdr Item))) Item Result)
		 (cons (cons x 1) Result)
	       ) ;_ end of if
	) ;_ end of setq
      ) ;_ end of foreach
      (setq Routlist (reverse Result))

 ;End of Residential Symbol Handling
      (while nameList
	(setq outList (cons (cons (car nameList)
				  (- listLen
				     (setq listLen (length (setq nameList
								  (vl-remove (car nameList) nameList)
							   ) ;_ end of setq
						   ) ;_ end of length
				     ) ;_ end of setq
				  ) ;_ end of -
			    ) ;_ end of cons
			    outList
		      ) ;_ end of cons
	) ;_ end of setq
      ) ;_ end of while

      (setq outlist (append outlist routlist))
      (setq outlist (reverse outlist))
      (foreach item outList
											    (progn
											      (setq qty (strcat (itoa (cdr item)) "   "))
											      (setq bna (strcat (car item)))
											      (setq newData (append newData (list (strcat qty bna))))
											    ) ;_ end of progn

      ) ;_ end of foreach

    ) ;_ end of progn
    (princ "\n*** Nothing blocks selected! ***")
  ) ;_ end of if   

;;;--- Load the dcl file
  (setq dcl_id (load_dialog "WSCOUNT.dcl"))
   
;;;--- Load the dialog definition if it is not already loaded
  (if (not (new_dialog "WSCOUNT" dcl_id))
    (progn
      (alert "The WSCOUNT.DCL file was not found.")
      (exit)
    ) ;_ end of progn
  ) ;_ end of if

;;;--- Add the first list to the first list_box
  (start_list "newdata" 3)
  (mapcar 'add_list newdata)
  (end_list)

;;;--- If an action event occurs, do this function
  (action_tile "accept" "(setq ddiag 2)(done_dialog)")
  (action_tile "cancel" "(setq ddiag 1)(done_dialog)")

;;;--- Display the dialog box
  (start_dialog)

;;;--- Unload the dialog box
  (unload_dialog dcl_id)

;;;--- If the user pressed the Cancel button
  (if (= ddiag 1)
    (progn
      (princ "\n WSCOUNT cancelled!")
    ) ;_ end of progn
  ) ;_ end of if

;;;--- If the user pressed the Okay button export as TXT file
  (if (= ddiag 2)
    (progn
      (if (setq	exPath (getfiled "Save Text File As"
				 (strcat (getvar "dwgprefix")
					 (substr (getvar "dwgname") 1 (- (strlen (getvar "dwgname")) 4))
					 "_BOM.txt"
				 ) ;_ end of strcat
				 "txt"
				 33
		       ) ;_ end of getfiled
	  ) ;_ end of setq
	(progn
	  (setq	CD-R (getvar "cdate")
		CD-S (rtos CD-R 2 4)
		YR   (substr CD-S 3 2)
		MO   (substr CD-S 5 2)
		DY   (substr CD-S 7 2)
		HR   (substr CD-S 10 2)
		MN   (substr CD-S 12 2)
		DT   (strcat MO "/" DY "/" YR)
		TM   (strcat HR ":" MN)
	  ) ;_ end of setq
	  (setq FILE (open expath "w"))
	  (princ (strcat "WattStopper Bill Of Materials \n\nBill Of Materials created as an estimate."
			 "\nFile:  "
			 (getvar "dwgname")
			 "\nDate:  "
			 DT
			 "\nTime:  "
			 TM
			 "\n\n\n"
		 ) ;_ end of strcat
		 FILE
	  ) ;_ end of princ
	  (princ "\n******** BLOCKS IN MODEL SPACE ON WATTSTOPPER LAYER ********\n " FILE)
	  (foreach item	outList
	    (princ (strcat "\n" (car item) "   " (itoa (cdr item))) FILE)
	  ) ;_ end of foreach
	  (princ "\n \n********************** END OF REPORT ***********************" FILE)
	  (close FILE)
	) ;_ end of progn
	(princ "\n*** Text file was not created! *** ")
      ) ;_ end of if
    ) ;_ end of progn
  ) ;_ end of if
   
;;;--- Reset Variables and Suppress the last echo for a clean exit
   
  (setvar "cmdecho" oldecho)
  (princ)
) ;_ end of defun






; --- WSUCS Helper Function ---
; if the _Title_Block_UCS layer exisits
; the set UCS to the point designated in each title block
; otherwise use previous UCS setting (this will allow use of older title blocks)
; Alex Lundin 12-30-2015
(defun C:WSUCS (/ CMAX CNTR DXFLEGENDPOINT DXFLINE LEGENDPOINT  LISTLEGENDPOINT LISTLINE SSLEGENDPOINT SSLINE TABLAYOUT TABLINE TABPOINT)
  														; must keep LEGENDX LEGENDX2 LEGENDY global
	(setq ssLine(ssget "x" '((8 . "_TB_UCS"))))								;create selection from all objects on the _Title_Block_UCS layer called ssLine
	(setq tabLayout (getvar "ctab"))									;set layouttab varaible to current layout tab AutoCAD varaible
	(cond 	((/= ssLine nil)
      		
		(setq cntr 0)											;set counter to 0
		(setq cmax (sslength ssLine))									;set countermax to number of items in line selction set

			(while ( < cntr cmax)									;while loop, continues through each item in ssLine set
  				(setq listLine (ssname ssLine cntr))						;set lineList to the object that corresponds to the current counter value in the line selection set
				(setq dxfLine (entget listLine))						;set dxfLine to the dxf codes of the current lineList object
				(setq tabLine (cdr(assoc 410 dxfLine)))						;set layerLine to the second element of the 410 dxf code of dxfLine
 					(if (= tabLayout tabLine)						;if statement when layerLine equals layouttab
	          				(command "_.ucs" "_non"						;access command line, start UCS command, ignore object snap
            					(trans (cdr (assoc 10 dxfLine)) 0 1) 				;trans passess the end point of the line, use 10 dxf of dxfLine for UCS command
            					"_non"  							;Ignore Object Snap for the following point
            					(trans (cdr (assoc 11 dxfLine)) 0 1) 				;same for 11 dxf of dxfLine
            					"" 								;type enter to close command line
        					) 								;end command line block
	  				)									;end if statement
	  
 				(setq ssLegendPoint(ssget "x" '((8 . "_TB_Legend_Insertion"))))			;same structure as above code for line selection set	
				(setq listLegendPoint (ssname ssLegendPoint cntr))				;
				(setq dxfLegendPoint (entget listLegendPoint))					;
				(setq tabPoint (cdr(assoc 410 dxfLegendPoint)))					;
  					(cond ((= tabLayout tabPoint)						;open conditional set (first predicate) when lineLayer equals layouttab
						(setq LegendPoint (cdr(assoc 10 dxfLegendPoint)))		;set LegendPoint to everything following the first element of 10 dxf of dxfLegendPoint
						(setq LegendX (nth 0 LegendPoint))				;set LegendX to first element of LegendPoint 
						(setq LegendX2 (+ LegendX 0.2))					;same as above, shift point to the left 0.2  
						(setq LegendY (nth 1 LegendPoint))				;set LegendY to second element of LegendPoint
						)
					)									
				(setq cntr(+ cntr 1))								;increment counter after each loop
			)											;end while loop
		)												;end first conditional predicate
		
	      	(t												;start else conditional predicate -- these variables must stay global for Legendcount functions
		(setq LegendX 30.8)										;set LegendX
		(setq LegendX2 31)										;set LegendX2
		(setq LegendY 13.508490)									;set LegendY 
		)												;end else conditional predicate
	)
  )														;END WSUCS


; --- wattstopperlayerproperties Helper Function ---
; set Wattstopper layer properties
; lineweight needs to be 0.00mm to allow blocks to print thin to PDF
; the lineweight is a 16 bit number and there is more information online to show how to use different numbers
; Alex Lundin 03-22-2017
(defun wattstopperlayerproperties( )
  
(setq layer "Wattstopper")											;set layer variable to Wattstopper for searching the table
(setq lineweight 0)												;set desired lineweight value
(setq layerdxfs (entget (tblobjname "layer" layer)))								;search the layer table for the specific layer name stored in layer
(entmod (subst (cons 370 lineweight)(assoc 370 layerdxfs) layerdxfs))						;use entmode to change the dxf codes from the layer table
  
)

; --- WSLS Helper Function ---
; if there are dynamic propeties in the title block
; stretch to correct length
; if there are no dynamic properties, thaw MSTP layer
; Alex Lundin 05-22-2017
(defun c:wsls(/ CMAX CMAX2 CNTR CNTR2 CURRENTTAB DXFBLOCK ENBLOCK LISTDLM MATCHDLM MATCHFLAG NAMEBLOCK NAMEDLM SSCAT5E SSTITLEBLOCK VLANAMEBLOCK)
  
  	(setq currentTab (getvar "CTAB"))											;set current tab to system variable
	(setq ssLegend (ssget "x" (list (cons 0 "INSERT") (cons 8 "WATTSTOPPER")(cons 410 currentTab))))
	(setq positionLegend (sslength ssLegend))
																;first set the LegendDistance
	(setq propName "LegendDistance") 											;<--- property name
	(setq propVal (* 0.4 positionLegend))											;<--- property value of double


  	(setq ssTitleBlock(ssget "x" (list (cons 0 "INSERT") (cons 8 "_WS_WattStopper")(cons 410 currentTab))))			;create selection set from all items inserted on the  _WS_WattStopper layer in the currentTab
  	(setq ssTitleBlock (ssname ssTitleBlock 0))										;set ssTitleBlock to the first element of the set (there will be only one item in the set as long as there is only one title block in the layout)
  
 	(setq blkObj (vlax-ename->vla-object ssTitleBlock)) 									;set blkObj to the VLA-OBJECT that represents the entity name of the first item in the ssTitleBlock selection set
        (setq propList (vl-remove-if-not '(lambda (x) (eq (vla-get-PropertyName x) propName ))   				;set propList include only vla-properties that match propName
     	(vlax-invoke blkObj 'getdynamicblockproperties)))

	(cond ((/= propList nil)												;very first conditional predicate (checks if title block has dynamic properties)
		
		(cond 		((/= ssTitleBlock nil)										;first condidtional predicate when ssTitleBlock is not nil
				(progn
 				(setq blkObj (vlax-ename->vla-object ssTitleBlock)) 						;set blkObj to the VLA-OBJECT that represents the entity name of the first item in the ssTitleBlock selection set
         			(setq propList (vl-remove-if-not '(lambda (x) (eq (vla-get-PropertyName x) propName ))   	;set propList include only vla-properties that match propName
     				(vlax-invoke blkObj 'getdynamicblockproperties)))  
    				(setq prop (car propList)) 									;set prop to first element in propList   
     				(vla-put-value prop (vlax-make-variant propVal 5)) 						;use vla-put-value function to store the propVal varaint into prop (5 is setting the variant to double)
      				(vla-update blkObj)										;update block
				)												;close progn
				)												;end first conditional predicate
 		)    														;end conditional block
	       
		(cond 		((/= ssTitleBlock nil)										;first condidtional predicate when ssTitleBlock is not nil
				(progn
				(command "layer" "_freeze" "_WS_CAT5E" "" "")							;freeze each layer to reset title block
				(command "layer" "_freeze" "_WS_LMBC-300" "" "")
				(command "layer" "_freeze" "_WS_LMLS-400" "" "")
				(command "layer" "_freeze" "_WS_LMLS-500" "" "")
				(command "layer" "_freeze" "_WS_LMLS-600" "" "")
				(command "layer" "_freeze" "_WS_LMPL" "" "")
				(command "layer" "_freeze" "_WS_LMRJ" "" "")
				(command "layer" "_freeze" "_WS_LMSM-3E" "" "")
				(command "layer" "_freeze" "_WS_LMSM-6E" "" "")
				(command "layer" "_freeze" "_WS_MSTP" "" "")
				(command "layer" "_freeze" "_WS_120_OHM" "" "")
				(command "layer" "_freeze" "_WS_RACCESS" "" "")
				
				;freeze the default layers
			  	(command "-layer" "Freeze" "_TB_Boundary" "")
			  	(command "-layer" "Freeze" "_TB_Legend_Insertion" "")
			  	(command "-layer" "Freeze" "_TB_UCS" "")
				
				;lock the default layers
			  	(command "-layer" "lock" "_TB_Boundary" "")
			  	(command "-layer" "lock" "_TB_Legend_Insertion" "")
			  	(command "-layer" "lock" "_TB_UCS" "")
				
				;turn off the default layers
			  	(command "-layer" "off" "_TB_Boundary" "")
			  	(command "-layer" "off" "_TB_Legend_Insertion" "")
			  	(command "-layer" "off" "_TB_UCS" "")
				)												;close progn
				)												;end first conditional predicate
 		)    														;end conditional block





	(vl-load-com)			
	(setq ssProduct(ssget "x" '((0 . "INSERT")(8 . "WATTSTOPPER"))))							;select all blocks on WATTSTOPPER layer
  	(setq cntr 0)														;set outer loop counter
  	(setq cmax (sslength ssProduct))											;set outer loop max to number of items in ssProduct
	       
  	(setq listDLM (list "RACCESS" "LMDX-100" "LMPC-100-5" "LMUC-100-2" "LMDC-100" "LMPX-100-1" "LMIO-201" "LIC48" "LICA8" "LIC24" "LMLS-600" "LIC8" "LMPC-100-1" "LMCP48" "LILM8" "LMCP24" "LMCP8" "LMCP12" "LILM24" "LMPX-100-4" "LMPC-100" "LMPX-100-3" "LILM48" "LMPX-100" "LICA24" "FG9026" "AW11-900" "TRA9023NP" "TRA24003NP" "FG9023" "ARC-PA0913B01" "HG903RD-RSP" "OD24-9" "AW15-900" "ARC-PA2419B01" "P8AX09-N" "LMZC-301" "LMRH-102" "LMRC-100" "LICA48" "LMFC-011" "LMRH-101" "LMPB-100" "WBT-900-IP" "LMUC-200" "LMRH-105" "LMIR-100" "LMDM-101" "LMPL-101" "LMSW-108" "LMSW-104" "LMSW-105" "LMIO-101" "LMIO-301" "LMSM-201" "LMPL-201" "LMBC-300" "LMRL-100" "LMSW-103" "LMLS-500" "LMRC-101" "LMLS-400" "LMLS-105" "LMLS-305" "LMRC-102" "LMSW-101" "LMSW-102" "LMRC-213" "LMRC-211" "LMRC-212" "LMSM-603" "LMIO-102" "LMPS-104" "NB-SWITCH" "LMSM-600" "NB-ROUTER" "LMDI-100" "LMSM-3E" "LMSM-6E" "LMRC-213-347v" "LMRC-212-347v" "LMRC-211-347v" "LMDW-102" "LMCT-100" "LMPO-200" "LMRJ-S8" "LMRJ-C8" "LMRJ-CS8" "LMPS-6000" "LMPW-102" "LMDW-101" "LMPW-101" "LMRC-221" "LMRC-222"))										;create a list to search each individual block for
  	(setq cntr2 0)														;set inner loop variables
	(setq cmax2 (vl-list-length listDLM))											
  	(setq matchDLM 0)													;set mactchDLM variable to 0, by default
	(setq matchFlag 0)
	       
	(while (< cntr cmax)													;outer loop for total number of items in ssProduct set
		(setq enBlock (ssname ssProduct cntr))										;get first blocks ename from the ssProduct selection set
	  	(setq dxfBlock (entget enBlock))										;get first blocks dxf codes from the enBlock variable
		(setq nameBlock (cdr(assoc 2 dxfBlock)))									;set nameBlock to the 2 dxf of dxfBlock


		(setq vlaNameBlock (vlax-ename->vla-object enBlock))
		    	(setq vlaNameBlock(vlax-get-property vlaNameBlock
				(if (vlax-property-available-p vlaNameBlock 'effectivename)
					'effectivename
					'name
					)
				))
		(if
		  	(= vlaNameBlock "RACCESS")
		  	(progn

			(command "layer" "_thaw" "_WS_RACCESS" "" "")
			)
		)
	  
	  	(while 		(< cntr2 cmax2)											;inner loop
				(setq nameDLM (nth cntr2 listDLM))								;set the nameDLM variable to the item of listDLM the corresponds to the current cntr2

			  		(cond											;conditional block
					  	((= nameDLM vlaNameBlock)							;if the nameDLM matches the vla true name of the block
					  	(setq matchDLM 1)								;set match variable to 1 (for thawing conditional step later)
						(setq cntr2 cmax2)								;set cntr2 to cmax2 (this jumps the innner loop to the end)
						)
					)											;end conditional block
			  	(setq cntr2 (+ cntr2 1))									;move to next item in listDLM
			)													;end inner loop
		(setq cntr2 0)													;reset cntr2, starts listDLM over
		(setq cntr (+ cntr 1))												;move to next item in ssProduct
	  
	)															;end outer loop


		(cond	((= matchDLM 1)												;first conditional predicate while matchDLM is not 0
			(command "layer" "_thaw" "_WS_LMRJ" "" "")								;thaw LMRJ
		)														;end first conditional predicate
		)														;end conditional block

	(setq ssCAT5E(ssget "x" '((8 . "_WS_CAT5E"))))										;select all cable on CAT5E layer
  
		(cond	((/= ssCAT5E nil)											;first conditional predicate while ssCAT5E is not nill
			(command "layer" "_thaw" "_WS_CAT5E" "" "")								;thaw CAT5E

		)														;end first conditional predicate
		)

	(setq ssMSTP(ssget "x" '((8 . "_WS_MSTP"))))										;select all cable on MSTP layer
  
		(cond	((/= ssMSTP nil)											;first conditional predicate while ssMSTP is not nill
			(command "layer" "_thaw" "_WS_MSTP" "" "")								;thaw MSTP

		)														;end first conditional predicate
		)


	(setq ssProduct(ssget "x" '((0 . "INSERT")(2 . "LMPL-101")(8 . "WATTSTOPPER"))))					;select all LMBC-300 on WATTSTOPPER layer
  
		(cond	((/= ssProduct nil)											;first conditional predicate while ssProduct is not nill
			(command "layer" "_thaw" "_WS_LMPL" "" "")								;thaw LMPL

		)														;end first conditional predicate
		)

	       
	(setq ssProduct(ssget "x" '((0 . "INSERT")(2 . "LMPL-201")(8 . "WATTSTOPPER"))))					;select all LMBC-300 on WATTSTOPPER layer
  
		(cond	((/= ssProduct nil)											;first conditional predicate while ssProduct is not nill
			(command "layer" "_thaw" "_WS_LMPL" "" "")								;thaw LMPL

		)														;end first conditional predicate
		)



	(setq ssProduct(ssget "x" '((0 . "INSERT")(2 . "LMBC-300")(8 . "WATTSTOPPER"))))					;select all LMBC-300 on WATTSTOPPER layer
  
		(cond	((/= ssProduct nil)											;first conditional predicate while ssProduct is not nill
			(command "layer" "_thaw" "_WS_LMRJ" "" "")								;thaw LMRJ
			(command "layer" "_thaw" "_WS_MSTP" "" "")								;thaw MSTP
			(command "layer" "_thaw" "_WS_120_OHM" "" "")								;thaw 120_OHM
			(command "layer" "_thaw" "_WS_LMBC-300" "" "")
		)														;end first conditional predicate
		)														;end conditional block

	(setq ssProduct(ssget "x" '((0 . "INSERT")(2 . "LMLS-400")(8 . "WATTSTOPPER"))))
		(cond	((/= ssProduct nil)
			(command "layer" "_thaw" "_WS_LMLS-400" "" "") 
		)														;end first conditional predicate
		)														;end conditional block


	(setq ssProduct(ssget "x" '((0 . "INSERT")(2 . "LMLS-500")(8 . "WATTSTOPPER"))))
		(cond	((/= ssProduct nil)
			(command "layer" "_thaw" "_WS_LMLS-500" "" "") 
		)														;end first conditional predicate
		)														;end conditional block


	(setq ssProduct(ssget "x" '((0 . "INSERT")(2 . "LMLS-600")(8 . "WATTSTOPPER"))))
		(cond	((/= ssProduct nil)
			(command "layer" "_thaw" "_WS_LMLS-600" "" "") 
		)														;end first conditional predicate
		)														;end conditional block

	(setq ssProduct(ssget "x" '((0 . "INSERT")(2 . "LMSM-3E")(8 . "WATTSTOPPER"))))
		(cond	((/= ssProduct nil)
			(command "layer" "_thaw" "_WS_LMSM-3E" "" "") 
		)														;end first conditional predicate
		)														;end conditional block
	       
	(setq ssProduct(ssget "x" '((0 . "INSERT")(2 . "LMSM-6E")(8 . "WATTSTOPPER"))))
		(cond	((/= ssProduct nil)
			(command "layer" "_thaw" "_WS_LMSM-6E" "" "") 
		)														;end first conditional predicate
		)														;end conditional block

	(setq ssProduct(ssget "x" '((0 . "INSERT")(2 . "LMNC")(8 . "WATTSTOPPER"))))
		(cond	((/= ssProduct nil)
			(command "layer" "_thaw" "_WS_LMSM-6E" "" "") 
		)														;end first conditional predicate
		)														;end conditional block

	       

  		)														;end very first conditional predicate
	(t (princ))														;thaw MSTP if very first conditional predicate fails
	)															;end very first conditional block (checks if title block has dynamic properties)
	(princ)															;supressTitleBlock result
)																;END WSLS




; --- 039 Helper Function ---
; if there are dynamic propeties in the title block
; stretch to correct length
; if there are no dynamic properties, thaw MSTP layer
; Alex Lundin 03-25-2016
(defun c:039(/ CMAX CNTR CURRENTSCALE ENT ENTDXF LAYOUTLEGEND SCALEFACTOR TAB)
	(setq tab (getvar 'ctab))
	(cond															;-conditional block
		((/= tab "Model")												;-conditional statement
	 	(setq layoutlegend (ssget "x" (list(cons 410 tab)(cons 0 "INSERT")(cons 8 "WATTSTOPPER"))))
		(setq scaleFactor 0.039)											;set the scale factor
		(setq cntr 0)													;initialize counter to 0 for while loop
  		(setq cmax (sslength layoutlegend))										;set cmax to length of layoutlegend selection set
			(while 	(< cntr cmax)											;-while loop to continue while the counter varaible cntr is less that the number of objects in the selection set layoutlegend
				(setq ent(ssname layoutlegend cntr))								;set variable ent to the name of the block that matches the current counter value
				(setq entDXF(entget ent))									
				(setq currentScale (cdr (assoc 41 entDXF )))
			  	(setq entDXF(subst (cons 41 scaleFactor)(assoc 41 entDXF) entDXF))				;change dxfCodes on entDXF
			  	(setq entDXF(subst (cons 42 scaleFactor)(assoc 42 entDXF) entDXF))
			  	(setq entDXF(subst (cons 43 scaleFactor)(assoc 43 entDXF) entDXF))
			  	(entmod entDXF)											;entmod function to modifiy the entity with the new values
				(vl-cmdf "_.AttSync" "select" ent "y")
			  	(setq cntr(+ cntr 1))										;once while loop reaches this point, increment counter varaible by one			
  			)													;-end while loop
		)														;-end conditional statement
		
		(t														;-conditional t statement
		(alert "Run from paperspace tab")										;alert user to error
		(exit)														;exit
		)														;-end conditional t statement
	)


)																;END 039





;;; --- LDELETE Helper Function ---
;;; Delete all blocks called legend in the drawing
;;; Useful for working with old WS drawings or drawings from Engineers containing a block called legend
;;; This function will delete and purge all blocks called legend
;;; Alex Lundin 05-05-2016
(defun c:LDELETE( / BLOCKS CMAX CNTR ENT ENTDXF TRUENAME VLAOBJECT L1 L2 L3)
  	(setq pspace(getvar "ctab"))
  	
  	(setq qty1 0)
 	(setq qty2 0)
 	(setq qty3 0)
	(setvar "ctab" "Model")											;start in model space
	(setq blocks (ssget "x" (list(cons 0 "INSERT"))))							;create selection set of all blocks
	(setq cntr 0)												;initialize counter to 0 for while loop
  	(setq cmax (sslength blocks))										;set cmax to length of blocks selection set
		(while 	(< cntr cmax)										;-while loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
			(setq ent(ssname blocks cntr))								;set variable ent to the name of the block that matches the current counter value
			(setq entDXF(entget ent))
  			(setq vlaobject (vlax-ename->vla-object ent))						;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of ent
    			(setq truename(vlax-get-property vlaobject						;set the truename variable  name of ent
        			(if (vlax-property-available-p vlaobject 'effectivename)			;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
            			'effectivename									;to the effective name property
            			'name
        			)
    			)
			)
		  	(cond											;-conditional block
			  	((= truename "legend")								;-conditional statement when truename equals legend
				(setq L1 truename)
				(setq qty1 (+ qty1 1))
				)										;-end conditional statement
				((= truename "LEGEND")								;-conditional statement when truename equals LEGEND
				(setq L2 truename)
				(setq qty2 (+ qty2 1))
				)
				((= truename "Legend")								;-conditional statement when truename equals Legend
				(setq L3 truename)
				(setq qty3 (+ qty3 1))
				)
			)											;-end conditional block
		  	(setq cntr(+ cntr 1))									;once while loop reaches this point, increment counter varaible by one			
  		)												;-end while loop


  
	(foreach layout (layoutlist)										;-foreach loop, store all layout names to layout variable with layoutlist function, use foreach loop to step through each layout
	  	(setvar "ctab" layout)										;set ctab variable to layout name stored in layout variable
		(setq blocks (ssget "x" (list(cons 0 "INSERT"))))						;create selection set from all blocks in layout
		(setq cntr 0)											;initialize counter to 0 for while loop
  		(setq cmax (sslength blocks))									;set cmax to length of blocks selection set
		(while 	(< cntr cmax)										;--while loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
			(setq ent(ssname blocks cntr))								;set variable ent to the name of the block that matches the current counter value
			(setq entDXF(entget ent))
  			(setq vlaobject (vlax-ename->vla-object ent))						;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of ent
    			(setq truename(vlax-get-property vlaobject						;set the truename variable  name of block
        			(if (vlax-property-available-p vlaobject 'effectivename)			;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
            			'effectivename									;to the effective name property
            			'name
        			)
    			)
			)
		  	(cond											;--conditional block
			  	((= truename "legend")								;--conditional statement when truename equals legend
				(setq qty1 (+ qty1 1))
				)										;--end conditional statement
				((= truename "LEGEND")								;-conditional statement when truename equals LEGEND
				(setq qty2 (+ qty2 1))
				)
				((= truename "Legend")								;-conditional statement when truename equals Legend
				(setq qty3 (+ qty3 1))
				)
			)											;--end conditional block
		  	(setq cntr(+ cntr 1))									;once while loop reaches this point, increment counter varaible by one			
  		)												;--end while loop
  	)													;-end foreach loop
  	(command "-purge" "A" "*legend*" "n")									;purge legend block definition
  	(cond
	  	((/= qty1 0)
		(setq qty1 (strcat (itoa qty1) "   "))
		(setq legendData (list (strcat qty1 L1)))
		)
		((/= qty2 0)
		(setq qty2 (strcat (itoa qty2) "   "))
		(setq legendData (list (strcat qty2 L2)))
		)
		((/= qty3 0)
		(setq qty3 (strcat (itoa qty3) "   "))
		(setq legendData (list (strcat qty3 L3)))
		)
	)
  	(setvar "ctab" pspace)
;--- Load the dcl file
  (setq dcl_id (load_dialog "LDELETE.dcl"))
   
;--- Load the dialog definition if it is not already loaded
  (if (not (new_dialog "LDELETE" dcl_id))
    (progn
      (alert "The LDELETE.DCL file was not found.")
      (exit)
    ) ;_ end of progn
  ) ;_ end of if

;--- Add the first list to the first list_box
  (start_list "keydata" 3)
  (mapcar 'add_list keyData)
  (end_list)
  (start_list "legenddata" 3)
  (mapcar 'add_list legendData)
  (end_list)


;--- If an action event occurs, do this function
  (action_tile "accept" "(setq ddiag 1)(done_dialog)")
  (action_tile "cancel" "(setq ddiag 2)(done_dialog)")
  (action_tile "accept2" "(setq ddiag 3)(done_dialog)")
  (action_tile "cancel2" "(setq ddiag 4)(done_dialog)")

;--- Display the dialog box
  (start_dialog)

;--- Unload the dialog box
  (unload_dialog dcl_id)
  
;--- If the user pressed the Purge button
  (if (= ddiag 1)
    (progn
      (alert "\n Attempting to purge any existing legend in paper space. ")
      (command "cmdecho" 0)
      (setq legend nil)
      (setq legend (ssget "x" (list(cons 2 "legend"))))								;create selection set of all blocks
      	(if	(/= legend nil)
	  	(command "erase" "p" "")
	)
      (command "-purge" "A" "*legend*" "n")									;purge legend block definition
    ) ;_ end of progn
  ) ;_ end of if
  
;--- If the user pressed the Cancel button
  (if (= ddiag 2)
    (progn
      (alert "\n LDELETE cancelled")
    ) ;_ end of progn
  ) ;_ end of if

  ;;;--- If the user pressed the Delete/Purge button
  (if (= ddiag 3)
    (progn
      	(alert "\n Deleting all blocks called legend in any space. Purging drawing after.")
      	(command "cmdecho" 0)
      	(setvar "ctab" "Model")											;start in model space
	(setq blocks (ssget "x" (list(cons 0 "INSERT"))))							;create selection set of all blocks
	(setq cntr 0)												;initialize counter to 0 for while loop
  	(setq cmax (sslength blocks))										;set cmax to length of blocks selection set
		(while 	(< cntr cmax)										;-while loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
			(setq ent(ssname blocks cntr))								;set variable ent to the name of the block that matches the current counter value
			(setq entDXF(entget ent))
  			(setq vlaobject (vlax-ename->vla-object ent))						;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of ent
    			(setq truename(vlax-get-property vlaobject						;set the truename variable  name of ent
        			(if (vlax-property-available-p vlaobject 'effectivename)			;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
            			'effectivename									;to the effective name property
            			'name
        			)
    			)
			)
		  	(cond											;-conditional block
			  	((= truename "legend")								;-conditional statement when truename equals legend
				(command "erase" ent "")							;erase the ent variable
				)										;-end conditional statement
				((= truename "LEGEND")								;-conditional statement when truename equals LEGEND
				(command "erase" ent "")							;erase the ent variable
				)
				((= truename "Legend")								;-conditional statement when truename equals Legend
				(command "erase" ent "")							;erase the ent variable
				)
			)											;-end conditional block
		  	(setq cntr(+ cntr 1))									;once while loop reaches this point, increment counter varaible by one			
  		)												;-end while loop


  
	(foreach layout (layoutlist)										;-foreach loop, store all layout names to layout variable with layoutlist function, use foreach loop to step through each layout
	  	(setvar "ctab" layout)										;set ctab variable to layout name stored in layout variable
		(setq blocks (ssget "x" (list(cons 0 "INSERT"))))						;create selection set from all blocks in layout
		(setq cntr 0)											;initialize counter to 0 for while loop
  		(setq cmax (sslength blocks))									;set cmax to length of blocks selection set
		(while 	(< cntr cmax)										;--while loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
			(setq ent(ssname blocks cntr))								;set variable ent to the name of the block that matches the current counter value
			(setq entDXF(entget ent))
  			(setq vlaobject (vlax-ename->vla-object ent))						;helper function to handle Anonymous names -- set vlaobject to the converted vla name from the entity name of ent
    			(setq truename(vlax-get-property vlaobject						;set the truename variable  name of block
        			(if (vlax-property-available-p vlaobject 'effectivename)			;only if the property called effective name exisits inside block (this entire block fails when if statement is false)
            			'effectivename									;to the effective name property
            			'name
        			)
    			)
			)
		  	(cond											;--conditional block
			  	((= truename "legend")								;--conditional statement when truename equals legend
				(command "erase" ent "")							;erase the ent variable
				)										;--end conditional statement
				((= truename "LEGEND")								;-conditional statement when truename equals LEGEND
				(command "erase" ent "")							;erase the ent variable
				)
				((= truename "Legend")								;-conditional statement when truename equals Legend
				(command "erase" ent "")							;erase the ent variable
				)
			)											;--end conditional block
		  	(setq cntr(+ cntr 1))									;once while loop reaches this point, increment counter varaible by one			
  		)												;--end while loop
  	)													;-end foreach loop
  	(command "-purge" "A" "*legend*" "n")									;purge legend block definition
      	(setvar "ctab" pspace)
    ) ;_ end of progn
  ) ;_ end of if

;--- If the user pressed the Cancel button
  (if (= ddiag 4)
    (progn
      (alert "\n LDELETE cancelled")
    ) ;_ end of progn
  ) ;_ end of if


   	(setq keyData nil)
  	(setq legendData nil)
;--- Reset Variables and Suppress the last echo for a clean exit
)														;END LDELETE





;;; --- VPCHECK Helper Function ---
;;; Check two system variables to see if we are in a viewport
;;; Alex Lundin 05-05-2016
(defun c:VPCHECK( / cvport tmode)
	(setq cvport (getvar "cvport"))
  	(setq tmode (getvar "tilemode"))
  	(if 	(/= 1 cvport)
	  	(if	(= 0 tmode)
		  	(progn
		  	(alert "\n Run from outside the viewport.")
	 		(exit)
		  	(exit)
			)
		)
	)
)



;; Get Visibility State from Anonymous Block Name
(defun anonymous-block-vis-state ( blk / ref )
(if
(and
(setq blk (tblobjname "block" blk))
(setq blk (entget (cdr (assoc 330 (entget blk)))))
(progn
(while (and (setq ref (assoc 331 blk)) (null (entget (cdr ref))))
(setq blk (cdr (member ref blk)))
)
ref
)
)
(get-visibility-state (vlax-ename->vla-object (cdr ref)))
)
)
;; Get Visibility Parameter Name
;; Returns the name of the Visibility Parameter of a Dynamic Block (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; Returns: [str] Name of Visibility Parameter, else nil
(defun get-visibility-parameter-name ( blk / vis )
(if
(and
(vlax-property-available-p blk 'effectivename)
(setq blk
(vla-item
(vla-get-blocks (vla-get-document blk))
(vla-get-effectivename blk)
)
)
(= :vlax-true (vla-get-isdynamicblock blk))
(= :vlax-true (vla-get-hasextensiondictionary blk))
(setq vis
(vl-some
'(lambda ( pair )
(if
(and
(= 360 (car pair))
(= "BLOCKVISIBILITYPARAMETER"
(cdr (assoc 0 (entget (cdr pair))))
)
)
(cdr pair)
)
)
(dictsearch
(vlax-vla-object->ename (vla-getextensiondictionary blk))
"ACAD_ENHANCEDBLOCK"
)
)
)
)
(cdr (assoc 301 (entget vis)))
)
)
;; Get Dynamic Block Property Value
;; Returns the value of a Dynamic Block property (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; prp - [str] Dynamic Block property name (case-insensitive)
(defun get-dynamic-property-value ( blk prp )
(setq prp (strcase prp))
(vl-some '(lambda ( x ) (if (= prp (strcase (vla-get-propertyname x))) (vlax-get x 'value)))
(vlax-invoke blk 'getdynamicblockproperties)
)
)
;; Get Dynamic Block Visibility State
;; Returns the value of the Visibility Parameter of a Dynamic Block (if present)
;; blk - [vla] VLA Dynamic Block Reference object
;; Returns: [str] Value of Visibility Parameter, else nil
(defun get-visibility-state ( blk )
(get-dynamic-property-value blk (get-visibility-parameter-name blk))
)

 ;                                                    ;
 ;                                                    ;
 ;                                                    ;
 ;           WattStopper Legend Creation              ;
 ;                   with Count                       ;
 ;             Writen by Shea Sterner		      ;
 ;         Updated by Alex Lundin 05-05-2016          ;
 ;                                                    ;
 ;                                                    ;
 ;                                                    ;
(defun c:wslegendcount (/ 	blSet blList nameList listLen outList oldecho oldosc num olp instpt instpt2 pt1 pt2 blknm *error*
			 	tabLayout line cntr cmax lineList dxfLine tabLine ssLegendPoint listLegendPoint dxfLegendPoint tabPoint LegendPoint LegendX LegendX2 LegendY
				BNA DATALIST DELETE E1 ELST1 ENT ENTLIST ENTNAME ENTTYPE LAYOUTLEGEND MISSINGKEYBLOCK OLOSC POSITION QTY SS TAB
			)

  (defun *error* (msg)
    (setvar "cmdecho" 1)
    (setvar olosc)
    (princ "\nRoutine terminated.")
    (princ)
  ) ;_ end of defun

	(wattstopperlayerproperties)
  	(C:VPCHECK)
  	(command "-purge" "A" "*legend*" "n")
	(command "-purge" "A" "*key*" "n")
	(C:WSUCS)
  	(command "-layer" "set" "WATTSTOPPER" "")

  
 ;save current system settings and edit for routine
  (setq oldecho (getvar "cmdecho"))
  (setq oldosc (getvar "osnapcoord"))
  (setvar "cmdecho" 0)
  (setvar "osnapcoord" 1)

 ;delete existing legend if in paperspace
(setq tab (getvar 'ctab))
(cond
	((/= tab "Model")
	 (setq delete "yes")
	 (setq layoutlegend (ssget "x" '((8 . "WATTSTOPPER"))))
	 (command "erase" "p" "")
	) 
	(t
	(setq delete "no")
	(alert "Run from paperspace tab")										;alert user to error
	(exit)														;exit
	)
)

  
 ;create selection set and create list of blocks on WattStopper layer in Model Space
  (if (setq blSet (ssget "x" (list (cons 8 "WATTSTOPPER") (cons 410 "Model") (cons 0 "Insert"))))
    (progn
      (setq blList   (mapcar 'vlax-ename->vla-object
			     (vl-remove-if
			       'listp
			       (mapcar 'cadr (ssnamex blSet))
			     ) ;_ end of vl-remove-if
		     ) ;_ end of mapcar
	    nameList (vl-sort (mapcar '(lambda (X) (vla-get-effectivename x)) blList) '<)
	    listLen  (length nameList)
      ) ;_ end of setq
 ;End of Residential Symbol Handling
      (setq dataList (list))
      (setq ss (ssget "x" (list (cons 8 "WATTSTOPPER") (cons 410 "Model") (cons 0 "Insert"))))
      (setq position 0)
      (while (setq ent (ssname ss position))
	(setq entlist (entget ent))
	(setq elst1 entlist)
	(setq blknm (cdr (assoc 2 elst1)))
	(if (= "*U" (substr blknm 1 2))
	  (progn
	    (repeat 3
	      (setq elst1 (entget (cdr (assoc 360 elst1))))
	    ) ;_ end of repeat
	    (setq elst1 (entget (cdr (assoc 340 elst1))))
	    (setq entname (cdr (assoc 2 elst1)))
	  ) ;_ end of progn
	) ;_ end of if
	(setq position (1+ position)
	) ;_ end of setq
      ) ;_ end of while
      (setq datalist (acad_strlsort datalist))
      (setq namelist (append namelist datalist))
 ;End of Residential Symbol Handling


      (while nameList
	(setq outList (cons (cons (car nameList)
				  (- listLen
				     (setq listLen (length (setq nameList
								  (vl-remove (car nameList) nameList)
							   ) ;_ end of setq
						   ) ;_ end of length
				     ) ;_ end of setq
				  ) ;_ end of -
			    ) ;_ end of cons
			    outList
		      ) ;_ end of cons
	) ;_ end of setq
      ) ;_ end of while

 ;insert and erase legend.dwg for block definitions
      (command "-insert" "legend" "0,0" "1" "1" "0")
      (command "erase" "last" "")

      
 ;insert key_"" block for each list item
      (setq num LegendY)
      (foreach olp (reverse outlist)
											      (cond
												((/= (tblsearch "block" (strcat "key_" (car olp))) nil)
												  (setq qty (strcat (itoa (cdr olp)) "   "))
											          (setq bna (strcat (car olp)))
											          (setq instpt2 (list LegendX num))						;<---- LegendX passed in by WSUCS
											          (command "-text" "s" "WS8th" "j" "mc" instpt2 "0" qty)
												  (setq instpt (list LegendX2 num))						;<---- LegendX2 passed in by WSUCS
												  (command "-insert" (strcat "key_" (car olp)) instpt "1" "1" "0")
												  (setq num (- num 0.4 ))
												)
												(t
												(setq qty (strcat (itoa (cdr olp)) "   "))
											        (setq bna (strcat (car olp)))
												(setq keyData (append keyData (list (strcat qty bna))))
												)
											      ) ;_ end of cond

      ) ;_ end of foreach
    ) ;_ end of progn
  ) ;_ end of if

 ;restore system settings
  (setvar "cmdecho" oldecho)
  (setvar "osnapcoord" oldosc)
	(cond													
	  	((/= keyData nil)
		(alert "There is a missing key block(s).
			\n This is a result of a either out dated legend
			\n or a block called legend not created by Wattstopper.
			\n The legend deleter will count all instances of the block called legend.
			\n Every block inside the drawing must be processed, individual layouts as well.
			\n The code will cycle through each layout during the command.
			\n This may take some time...
			\n The following dialog box will show you which key blocks are missing
			\n as well as how many blocks named Legend are in the file.
			\n You can use the buttons to fix the drawing.
			\n After this function runs you can use the legend functions.
			"
		)
		(C:LDELETE)											;call LDELETE if missing key_blocks
		)
	)
  (command "-purge" "A" "*legend*" "n")
  (command "-purge" "A" "*key*" "n")
  (C:WSLS)													;call WSLS function
  (princ)
) ;_ end of defun





 ;                                                    ;
 ;                                                    ;
 ;                                                    ;
 ;           WattStopper Legend Creation              ;
 ;                   with Count                       ;
 ;             Writen by Shea Sterner		      ;
 ;         Updated by Alex Lundin 05-05-2016          ;
 ;                                                    ;
 ;                                                    ;
 ;                                                    ;
(defun c:wslegendcountfix (/ 	blSet blList nameList listLen outList oldecho oldosc num olp instpt instpt2 pt1 pt2 blknm *error*
			 	tabLayout line cntr cmax lineList dxfLine tabLine ssLegendPoint listLegendPoint dxfLegendPoint tabPoint LegendPoint LegendX LegendX2 LegendY
				BNA DATALIST DELETE E1 ELST1 ENT ENTLIST ENTNAME ENTTYPE LAYOUTLEGEND MISSINGKEYBLOCK OLOSC POSITION QTY SS TAB
			)

  (defun *error* (msg)
    (setvar "cmdecho" 1)
    (setvar olosc)
    (princ "\nRoutine terminated.")
    (princ)
  ) ;_ end of defun

	(wattstopperlayerproperties)
  	(C:VPCHECK)
  	(command "-purge" "A" "*legend*" "n")
	(command "-purge" "A" "*key*" "n")
	(C:WSUCS)
  	(command "-layer" "set" "WATTSTOPPER" "")

  
 ;save current system settings and edit for routine
  (setq oldecho (getvar "cmdecho"))
  (setq oldosc (getvar "osnapcoord"))
  (setvar "cmdecho" 0)
  (setvar "osnapcoord" 1)

 ;delete existing legend if in paperspace
(setq tab (getvar 'ctab))
(cond
	((/= tab "Model")
	 (setq delete "yes")
	 (setq layoutlegend (ssget "x" '((8 . "WATTSTOPPER"))))
	 (command "erase" "p" "")
	) 
	(t
	(setq delete "no")
	(alert "Run from paperspace tab")										;alert user to error
	(exit)														;exit
	)
)

  
 ;create selection set and create list of blocks on WattStopper layer in Model Space
  (if (setq blSet (ssget "x" (list (cons 8 "WATTSTOPPER") (cons 410 "Model") (cons 0 "Insert"))))
    (progn
      (setq blList   (mapcar 'vlax-ename->vla-object
			     (vl-remove-if
			       'listp
			       (mapcar 'cadr (ssnamex blSet))
			     ) ;_ end of vl-remove-if
		     ) ;_ end of mapcar
	    nameList (vl-sort (mapcar '(lambda (X) (vla-get-effectivename x)) blList) '<)
	    listLen  (length nameList)
      ) ;_ end of setq
 ;End of Residential Symbol Handling
      (setq dataList (list))
      (setq ss (ssget "x" (list (cons 8 "WATTSTOPPER") (cons 410 "Model") (cons 0 "Insert"))))
      (setq position 0)
      (while (setq ent (ssname ss position))
	(setq entlist (entget ent))
	(setq elst1 entlist)
	(setq blknm (cdr (assoc 2 elst1)))
	(if (= "*U" (substr blknm 1 2))
	  (progn
	    (repeat 3
	      (setq elst1 (entget (cdr (assoc 360 elst1))))
	    ) ;_ end of repeat
	    (setq elst1 (entget (cdr (assoc 340 elst1))))
	    (setq entname (cdr (assoc 2 elst1)))
	  ) ;_ end of progn
	) ;_ end of if
	(setq position (1+ position)
	) ;_ end of setq
      ) ;_ end of while
      (setq datalist (acad_strlsort datalist))
      (setq namelist (append namelist datalist))
 ;End of Residential Symbol Handling


      (while nameList
	(setq outList (cons (cons (car nameList)
				  (- listLen
				     (setq listLen (length (setq nameList
								  (vl-remove (car nameList) nameList)
							   ) ;_ end of setq
						   ) ;_ end of length
				     ) ;_ end of setq
				  ) ;_ end of -
			    ) ;_ end of cons
			    outList
		      ) ;_ end of cons
	) ;_ end of setq
      ) ;_ end of while

 ;insert and erase legend.dwg for block definitions
      (command "-insert" "legend2" "0,0" "1" "1" "0")
      (command "erase" "last" "")

      
 ;insert key_"" block for each list item
      (setq num LegendY)
      (foreach olp (reverse outlist)
											      (cond
												((/= (tblsearch "block" (strcat "key_" (car olp))) nil)
												  (setq qty (strcat (itoa (cdr olp)) "   "))
											          (setq bna (strcat (car olp)))
											          (setq instpt2 (list LegendX num))						;<---- LegendX passed in by WSUCS
											          (command "-text" "s" "WS8th" "j" "mc" instpt2 "0" qty)
												  (setq instpt (list LegendX2 num))						;<---- LegendX2 passed in by WSUCS
												  (command "-insert" (strcat "key_" (car olp)) instpt "1" "1" "0")
												  (setq num (- num 0.4 ))
												)
												(t
												(setq qty (strcat (itoa (cdr olp)) "   "))
											        (setq bna (strcat (car olp)))
												(setq keyData (append keyData (list (strcat qty bna))))
												)
											      ) ;_ end of cond

      ) ;_ end of foreach
    ) ;_ end of progn
  ) ;_ end of if

 ;restore system settings
  (setvar "cmdecho" oldecho)
  (setvar "osnapcoord" oldosc)
	(cond													
	  	((/= keyData nil)
		(alert "There is a missing key block(s).
			\n This is a result of a either out dated legend
			\n or a block called legend not created by Wattstopper.
			\n The legend deleter will count all instances of the block called legend.
			\n Every block inside the drawing must be processed, individual layouts as well.
			\n The code will cycle through each layout during the command.
			\n This may take some time...
			\n The following dialog box will show you which key blocks are missing
			\n as well as how many blocks named Legend are in the file.
			\n You can use the buttons to fix the drawing.
			\n After this function runs you can use the legend functions.
			"
		)
		(C:LDELETE)											;call LDELETE if missing key_blocks
		)
	)
  (command "-purge" "A" "*legend*" "n")
  (command "-purge" "A" "*key*" "n")
  (C:WSLS)													;call WSLS function
  (princ)
) ;_ end of defun



 ;                                                    ;
 ;                                                    ;
 ;                                                    ;
 ;           WattStopper Legend Creation              ;
 ;                   with Count                       ;
 ;             Writen by Shea Sterner		      ;
 ;         Updated by Alex Lundin 05-17-2016          ;
 ;                                                    ;
 ;                                                    ;
 ;                                                    ;
(defun c:wslegendcountpl (/ 	blSet blList nameList listLen outList oldecho oldosc num olp instpt instpt2 pt1 pt2 blknm *error*
			 	tabLayout line cntr cmax lineList dxfLine tabLine ssLegendPoint listLegendPoint dxfLegendPoint tabPoint LegendPoint LegendX LegendX2 LegendY
				BNA DATALIST DELETE E1 ELST1 ENT ENTLIST ENTNAME ENTTYPE LAYOUTLEGEND MISSINGKEYBLOCK OLOSC POSITION QTY SS TAB
			  	;elistLayers listAdder layerList uniqueLayerList WS-LEGEND-PLINE en enDXF plpoints blocks
			  
			)

  (defun *error* (msg)
    (setvar "cmdecho" 1)
    (setvar olosc)
    (princ "\nRoutine terminated.")
    (princ)
  ) ;_ end of defun
	(wattstopperlayerproperties)
	(layerstate-delete "WS-Layout")
  	(layerstate-save "WS-Layout" nil nil)										;create Layer state
  	(C:VPCHECK)
  	(command "-purge" "A" "*legend*" "n")
	(command "-purge" "A" "*key*" "n")
	(C:WSUCS)
  	(command "-layer" "set" "WATTSTOPPER" "")

  
 ;save current system settings and edit for routine
  (setq oldecho (getvar "cmdecho"))
  (setq oldosc (getvar "osnapcoord"))
  (setvar "cmdecho" 0)
  (setvar "osnapcoord" 1)

 ;delete existing legend if in paperspace
(setq tab (getvar 'ctab))
(cond
	((/= tab "Model")
	 (setq delete "yes")
	 (setq layoutlegend (ssget "x" '((8 . "WATTSTOPPER"))))
	 (command "erase" "p" "")
	) 
	(t
	(setq delete "no")
	(alert "Run from paperspace tab")													;alert user to error
	(exit)																	;exit
	)
)

	(setvar "ctab" "MODEL")
;;;	(setq elistLayers (ssget  (alert "\nSelect objects on layers to show during selection:")))						;set elistWalls to the selection and prompt user
;;;	(setq listAdder "WATTSTOPPER")
;;;  	(setq layerList (cons listAdder layerList))												;add listAdder to layerList
;;;  
;;;  	(cond																	;-conditional block
;;;	  	((/= elistLayers nil)														;-conditional statement
;;;  		(setq cmax (sslength elistLayers))												;set max
;;;		(setq cntr 0)															;set cntr
;;;  		(while 	(< cntr cmax)														;--while loop
;;;			(setq ent(ssname elistLayers cntr))											;set ent to item of elistWalls that corresponds to cntr
;;;		  	(setq entDXF (entget ent))												;set entDXF to dxf codes of ent
;;;		  	(setq entLayer (cdr(assoc 8 entDXF)))											;set entLAyer to second element of 8th dxfcode
;;;			(setq layerList (cons entLayer layerList))										;add entLayer to layerList
;;;			(setq cntr (+ cntr 1))													;increment cntr
;;;		)																;--end while
;;;		(while layerList														;--while loop
;;;        		(setq x (car layerList)													;remove duplicates from layerList
;;;             		layerList (vl-remove x (cdr layerList))
;;;              		uniqueLayerList (cons x uniqueLayerList)										;call new list uniqueLayerList
;;;        	)
;;;    		)
;;;    		(reverse uniqueLayerList)													;reverse uniqueLayerList to account for cons property
;;;		)																;-end conditional statement
;;;		(t																;-conditional t statement
;;;		(setq uniqueLayerList layerList)
;;;		)																;-end conditional t statement
;;;	)																	;-end conditional block
;;;	(command "._layer" "_off" "*" "")
;;;  	(cond																	;-conditional block						
;;;		((/= uniqueLayerList nil)													;-conditional statement		
;;;		(setq cntr 0)
;;;		(setq cmax (length uniqueLayerList))
;;;		(while	(< cntr cmax)														;-while loop to show layers
;;;			(setq showLayer (nth cntr uniqueLayerList))
;;;  			(command "._layer" "_on" showLayer "" "")
;;;			(setq cntr (+ cntr 1))
;;;		)																;-end while loop
;;;		)																;-end conditional statement
;;;	)																	;-end conditional block

	
  	(alert "Select one pline boundary to count from")

	
  	(setq WS-LEGEND-PLINE (ssget))													;selection set of all polyline objects on WS-WALLS
  
			(setq en(ssname WS-LEGEND-PLINE 0))										;set variable en to the name of the block that matches the current counter value
			(setq enDXF(entget en))												;set the varaible enlist to the list of entities from the en varaible
			(setq plpoints(list)) 												;create an empty list to store cornerPoints in
			(setq flag (car(assoc 10 enDXF)))										;set flag to the first item of the 10 assoc in enDXF which is 10.
		  	(foreach flag enDXF                                              						;look for every flag -- 10 dxf -- in the dxfRectangle list
  				(if	(= 10 (car flag))                                         					;if the 10 dxf exists 
    					(setq plpoints                                 							;reset cornerPointsRectangle to
      					(append plpoints                       								;the old cornerPointsRectangle 
        				(list                                                       					;plus a list containing
          				(cdr flag)                                             						;the cornerPoint point
        				)                                                                 				;close the list statement 
      					)                                                                      				;close the append statement
    				)                                                                           				;close the setq statement
  				)                                                                                			;close the if statement
			)                                                                                     				;close the foreach statement

		  
			(setq blocks(ssget "_CP" plpoints '((0 . "INSERT")(8 . "WATTSTOPPER" ))))					;selection set of all polyline objects on WS-WALLS
			(setq cntr2 0)
		  	(setq cmax2 (sslength blocks))
			(cond
			  	((= cmax2 0)
				(alert "There are no blocks in the pline"
				)
				)
				
			)
		  
			(setq blocks(ssget "_CP" plpoints '((0 . "INSERT")(8 . "WATTSTOPPER" ))))

  
																	;close outer conditional block
(layerstate-restore "WS-Layout" nil nil)												;restore layer state
(setvar "ctab" tab)
(setvar "clayer" "WATTSTOPPER") 
 ;create selection set and create list of blocks on WattStopper layer in Model Space
  (if (setq blSet blocks)
    (progn
      (setq blList   (mapcar 'vlax-ename->vla-object
			     (vl-remove-if
			       'listp
			       (mapcar 'cadr (ssnamex blSet))
			     ) ;_ end of vl-remove-if
		     ) ;_ end of mapcar
	    nameList (vl-sort (mapcar '(lambda (X) (vla-get-effectivename x)) blList) '<)
	    listLen  (length nameList)
      ) ;_ end of setq
 ;End of Residential Symbol Handling
      (setq dataList (list))
      (setq ss (ssget "x" (list (cons 8 "WATTSTOPPER") (cons 410 "Model") (cons 0 "Insert"))))
      (setq position 0)
      (while (setq ent (ssname ss position))
	(setq entlist (entget ent))
	(setq elst1 entlist)
	(setq blknm (cdr (assoc 2 elst1)))
	(if (= "*U" (substr blknm 1 2))
	  (progn
	    (repeat 3
	      (setq elst1 (entget (cdr (assoc 360 elst1))))
	    ) ;_ end of repeat
	    (setq elst1 (entget (cdr (assoc 340 elst1))))
	    (setq entname (cdr (assoc 2 elst1)))

	  ) ;_ end of progn
	) ;_ end of if
	(setq position (1+ position)
	) ;_ end of setq
      ) ;_ end of while
      (setq datalist (acad_strlsort datalist))
      (setq namelist (append namelist datalist))
 ;End of Residential Symbol Handling


      (while nameList
	(setq outList (cons (cons (car nameList)
				  (- listLen
				     (setq listLen (length (setq nameList
								  (vl-remove (car nameList) nameList)
							   ) ;_ end of setq
						   ) ;_ end of length
				     ) ;_ end of setq
				  ) ;_ end of -
			    ) ;_ end of cons
			    outList
		      ) ;_ end of cons
	) ;_ end of setq
      ) ;_ end of while

 ;insert and erase legend.dwg for block definitions
      (command "-insert" "legend" "0,0" "1" "1" "0")
      (command "erase" "last" "")

      
 ;insert key_"" block for each list item
      (setq num LegendY)
      (foreach olp (reverse outlist)
											      (cond
												((/= (tblsearch "block" (strcat "key_" (car olp))) nil)
												  (setq qty (strcat (itoa (cdr olp)) "   "))
											          (setq bna (strcat (car olp)))
											          (setq instpt2 (list LegendX num))						;<---- LegendX passed in by WSUCS
											          (command "-text" "s" "WS8th" "j" "mc" instpt2 "0" qty)
												  (setq instpt (list LegendX2 num))						;<---- LegendX2 passed in by WSUCS
												  (command "-insert" (strcat "key_" (car olp)) instpt "1" "1" "0")
												  (setq num (- num 0.4 ))
												)
												(t
												(setq qty (strcat (itoa (cdr olp)) "   "))
											        (setq bna (strcat (car olp)))
												(setq keyData (append keyData (list (strcat qty bna))))
												)
											      ) ;_ end of cond
      ) ;_ end of foreach
    ) ;_ end of progn
  ) ;_ end of if

 ;restore system settings
  (setvar "cmdecho" oldecho)
  (setvar "osnapcoord" oldosc)
	(cond													
	  	((/= keyData nil)
		(alert "There is a missing key block(s).
			\n This is a result of a either out dated legend
			\n or a block called legend not created by Wattstopper.
			\n The legend deleter will count all instances of the block called legend.
			\n Every block inside the drawing must be processed, individual layouts as well.
			\n The code will cycle through each layout during the command.
			\n This may take some time...
			\n The following dialog box will show you which key blocks are missing
			\n as well as how many blocks named Legend are in the file.
			\n You can use the buttons to fix the drawing.
			\n After this function runs you can use the legend functions.
			"
		)
		(C:LDELETE)											;call LDELETE if missing key_blocks
		)
	)
  (command "-purge" "A" "*legend*" "n")
  (command "-purge" "A" "*key*" "n")
  (C:WSLS)													;call WSLS function
  (princ)
) ;_ end of defun




 ;                                                    ;
 ;                                                    ;
 ;                                                    ;
 ;           WattStopper Legend Creation              ;
 ;                                                    ;
 ;                                                    ;
 ;                                                    ;

(defun c:wslegend (/ blSet blList nameList listLen outList oldecho oldosc num olp instpt pt1 pt2 blknm missingKeyBlock *error*
		   DATALIST DELETE E1 ELST1 ENT ENTLIST ENTNAME ENTTYPE LAYOUTLEGEND LEGENDX2 LEGENDY OLOSC POSITION SS TAB
		   )

  (defun *error* (msg)
    (setvar "cmdecho" 1)
    (setvar olosc)
    (princ "\nRoutine terminated.")
    (princ)
  ) ;_ end of defun
	(wattstopperlayerproperties)
	(setq qty 0)
	(C:VPCHECK)
    	(command "-purge" "A" "*legend*" "n")
	(command "-purge" "A" "*key*" "n")
	(C:WSUCS)
  	(command "-layer" "set" "WATTSTOPPER" "")
  
 ;save current system settings and edit for routine
  (setq oldecho (getvar "cmdecho"))
  (setq oldosc (getvar "osnapcoord"))
  (setvar "cmdecho" 0)
  (setvar "osnapcoord" 1)

 ;delete existing legend if in paperspace
(setq tab (getvar 'ctab))
(cond
	((/= tab "Model")
	 (setq delete "yes")
	 (setq layoutlegend (ssget "x" '((8 . "WATTSTOPPER"))))
	 (command "erase" "p" "")
	) 
	(t
	(setq delete "no")
	(alert "Run from paperspace tab")										;alert user to error
	(exit)														;exit
	)
)

  
 ;create selection set and create list of blocks on WattStopper layer in Model Space
  (if (setq blSet (ssget "x" (list (cons 8 "WATTSTOPPER") (cons 410 "Model") (cons 0 "Insert"))))
    (progn
      (setq blList   (mapcar 'vlax-ename->vla-object
			     (vl-remove-if
			       'listp
			       (mapcar 'cadr (ssnamex blSet))
			     ) ;_ end of vl-remove-if
		     ) ;_ end of mapcar
	    nameList (vl-sort (mapcar '(lambda (X) (vla-get-effectivename x)) blList) '<)
	    listLen  (length nameList)
      ) ;_ end of setq
 ;End of Residential Symbol Handling
      (setq dataList (list))
      (setq ss (ssget "x" (list (cons 8 "WATTSTOPPER") (cons 410 "Model") (cons 0 "Insert"))))
      (setq position 0)
      (while (setq ent (ssname ss position))
	(setq entlist (entget ent))
	(setq elst1 entlist)
	(setq blknm (cdr (assoc 2 elst1)))
	(if (= "*U" (substr blknm 1 2))
	  (progn
	    (repeat 3
	      (setq elst1 (entget (cdr (assoc 360 elst1))))
	    ) ;_ end of repeat
	    (setq elst1 (entget (cdr (assoc 340 elst1))))
	    (setq entname (cdr (assoc 2 elst1)))
	  ) ;_ end of progn
	) ;_ end of if
	(setq position (1+ position)
	) ;_ end of setq
      ) ;_ end of while
      (setq datalist (acad_strlsort datalist))
      (setq namelist (append namelist datalist))
 ;End of Residential Symbol Handling


      (while nameList
	(setq outList (cons (cons (car nameList)
				  (- listLen
				     (setq listLen (length (setq nameList
								  (vl-remove (car nameList) nameList)
							   ) ;_ end of setq
						   ) ;_ end of length
				     ) ;_ end of setq
				  ) ;_ end of -
			    ) ;_ end of cons
			    outList
		      ) ;_ end of cons
	) ;_ end of setq
      ) ;_ end of while

 ;insert and erase legend.dwg for block definitions
      (command "-insert" "legend" "0,0" "1" "1" "0")
      (command "erase" "last" "")


      
 ;insert key_"" block for each list item
     (setq num LegendY)
      (foreach olp (reverse outlist)
											      (cond
												((/= (tblsearch "block" (strcat "key_" (car olp))) nil)
												(setq instpt (list LegendX2 num))						;<---- LegendX2 passed in by WSUCS
												(command "-insert" (strcat "key_" (car olp)) instpt "1" "1" "0")
												(setq num (- num 0.4))
												)
												(t
												(setq qty (strcat (itoa (cdr olp)) "   "))
											        (setq bna (strcat (car olp)))
												(setq keyData (append keyData (list (strcat qty bna))))
												)
											      ) ;_ end of cond block
      ) ;_ end of foreach
    ) ;_ end of progn
  ) ;_ end of if

 ;restore system settings
  (setvar "cmdecho" oldecho)
  (setvar "osnapcoord" oldosc)
		(cond													
	  	((/= keyData nil)
		(alert "There is a missing key block(s).
			\n This is a result of a either out dated legend
			\n or a block called legend not created by Wattstopper.
			\n The legend deleter will count all instances of the block called legend.
			\n Every block inside the drawing must be processed, individual layouts as well.
			\n The code will cycle through each layout during the command.
			\n This may take some time...
			\n The following dialog box will show you which key blocks are missing
			\n as well as how many blocks named Legend are in the file.
			\n You can use the buttons to fix the drawing.
			\n After this function runs you can use the legend functions.
			"
		)
		(C:LDELETE)											;call LDELETE if missing key_blocks
		)
	)
  (command "-purge" "A" "*legend*" "n")
  (command "-purge" "A" "*key*" "n")
  (C:WSLS)													;call WSLS function  
  (princ)
) ;_ end of defun



 ;                                                    ;
 ;                                                    ;
 ;                                                    ;
 ;            WattStopper Layer Functions             ;
 ;                                                    ;
 ;                                                    ;
 ;                                                    ;

 ;lock all layers except WattStopper
(defun c:wslaylock (/ oldecho *error*)
  (defun *error* (msg)
    (setvar "cmdecho" 1)
    (princ "\nRoutine terminated.")
    (princ)
  ) ;_ end of defun
  (setq oldecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (if (tblsearch "layer" "WATTSTOPPER")
    (progn (command "-layer" "set" "WATTSTOPPER" "")
	   (command "-layer" "lock" "*" "")
	   (command "-layer" "unlock" "WATTSTOPPER" "unlock" "ws_coverage" "unlock" "WS_TITLEBLOCK" "")
    ) ;_ end of progn
    (progn
      (command "-layer"	"new" "WATTSTOPPER" "color" "160" "WATTSTOPPER"	"new" "ws_coverage" "color" "10" "ws_coverage" "off" "ws_coverage" "") ;_ end of command
      (command "-layer" "set" "WATTSTOPPER" "")
      (command "-layer" "lock" "*" "")
      (command "-layer" "unlock" "WATTSTOPPER" "unlock" "ws_coverage" "unlock" "WS_TITLEBLOCK" "")
    ) ;_ end of progn
  ) ;_ end of if
  (princ "\nAll layers have been locked except WattStopper")
  (setvar "cmdecho" oldecho)
  (princ)
) ;_ end of defun

 ; unlock all layers except defaults
(defun c:wslayunlock (/ oldecho *error*)
  (defun *error* (msg)
    (setvar "cmdecho" 1)
    (princ "\nRoutine terminated.")
    (princ)
  ) ;_ end of defun
  (setq oldecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (command "-layer" "unlock" "*" "")

	;lock the default layers
  	(command "-layer" "lock" "_TB_Boundary" "")
  	(command "-layer" "lock" "_TB_Legend_Insertion" "")
  	(command "-layer" "lock" "_TB_UCS" "")

  
  (princ "\nAll layers have been unlocked")
  (setvar "cmdecho" oldecho)
  (princ)
) ;_ end of defun

 ;freeze all layers except WattStopper
(defun c:wslayfrz (/ oldecho *error*)
  (defun *error* (msg)
    (setvar "cmdecho" 1)
    (princ "\nRoutine terminated.")
    (princ)
  ) ;_ end of defun
  (setq oldecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (if (tblsearch "layer" "WATTSTOPPER")
    (progn (command "-layer" "set" "WATTSTOPPER" "")
	   (command "-layer" "Freeze" "*" "")
	   (command "-layer" "Thaw" "WattStopper" "thaw" "ws_coverage" "")
    ) ;_ end of progn
    (progn
      (command "-layer"	"new" "WATTSTOPPER" "color" "160" "WATTSTOPPER"	"new" "WS_Coverage" "color" "10" "ws_coverage" "off" "ws_coverage" "") ;_ end of command
      (command "-layer" "set" "WATTSTOPPER" "")
      (command "-layer" "Freeze" "*" "")
      (command "-layer" "Thaw" "WattStopper" "thaw" "ws_coverage" "")
    ) ;_ end of progn
  ) ;_ end of if
  (princ "\nAll layers have been frozen except WattStopper")
  (setvar "cmdecho" oldecho)
  (princ)
) ;_ end of defun

 ;thaw all layers except defaults
(defun c:wslaythw (/ oldecho *error*)
  (defun *error* (msg)
    (setvar "cmdecho" 1)
    (princ "\nRoutine terminated.")
    (princ)
  ) ;_ end of defun
  (setq oldecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (command "-layer" "Thaw" "*" "")

	;freeze the default layers
  	(command "-layer" "Freeze" "_TB_Boundary" "")
  	(command "-layer" "Freeze" "_TB_Legend_Insertion" "")
  	(command "-layer" "Freeze" "_TB_UCS" "")
  
  (princ "\nAll layers have been thawed")
  (setvar "cmdecho" oldecho)
  (princ)
) ;_ end of defun

 ;set wattstopper layer as current
(defun c:wslaycur (/ oldecho *error*)
  (defun *error* (msg)
    (setvar "cmdecho" 1)
    (princ "\nRoutine terminated.")
    (princ)
  ) ;_ end of defun
  (setq oldecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (if (tblsearch "layer" "WATTSTOPPER")
    (command "-layer" "set" "WATTSTOPPER" "")
    (progn
      (command "-layer"	"new" "WATTSTOPPER" "color" "160" "WATTSTOPPER"	"new" "WS_Coverage" "color" "10" "ws_coverage" "off" "ws_coverage" "new" "WS_TITLEBLOCK" "color" "160" "WS_TITLEBLOCK" "") ;_ end of command
      (command "-layer" "set" "WATTSTOPPER" "")
    ) ;_ end of progn
  ) ;_ end of if
  (princ "\nWattStopper has been made current")
  (setvar "cmdecho" oldecho)
  (princ)
) ;_ end of defun

 ;turn off layers named or selected
(defun c:wslayoff (/ oldecho *error*)
  (defun *error* (msg)
    (setvar "cmdecho" 1)
    (princ "\nRoutine terminated.")
    (princ)
  ) ;_ end of defun
  (setq oldecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (command "layoff" "settings" "block" "entity")
  (princ "\nSelect objects to turn off layers:")
  (setvar "cmdecho" oldecho)
  (princ)
) ;_ end of defun

 ;bind all xrefs and purge unused information
(defun c:WSBINDALL (/ oldecho *error*)
  (defun *error* (msg)
    (setvar "cmdecho" 1)
    (princ "\nRoutine terminated.")
    (princ)
  ) ;_ end of defun
  (setq oldecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (command "BINDTYPE" "1")
  (command "-xref" "b" "*")
  (command "AUDIT" "Y")
  (command "-PURGE" "A" "*" "n")
  (command "BINDTYPE" "0")
  (setvar "cmdecho" oldecho)
  (princ)
) ;_ end of defun

(defun c:bind ()

  (if (setq SS (ssget "X" '((0 . "INSERT") (-4 . "!=") (8 . "WattStopper"))))
    (progn
      (setq CNT 0)
      (repeat (sslength SS)
	(setq EN (ssname SS CNT))
	(setq BN (cdr (assoc 2 (entget EN))))
	(if (setq BR (tblsearch "block" BN))
	  (progn
	    (if	(setq BP (cdr (assoc 1 BR)))
	      (progn
		(command "_.-XREF" "B" (vl-filename-base BP))
	      ) ;_ end of progn
	    ) ;_ end of if
	  ) ;_ end of progn
	) ;_ end of if
	(setq CNT (1+ CNT))
      ) ;_ end of repeat
      (command "_.-PURGE" "A" "" "N")
    ) ;_ end of progn
  ) ;_ end of if
) ;_ end of defun


 ;adds a WATTSTOPPER layout with titleblock from the WATTSTOPPER.dwt
(defun c:wslayout ()
(setvar "cmdecho" 0)
(command "filedia" "0")

;;;(command "-dwgunits" "" "" "" "N" "N")
;;;(command "-layout" "template" "C:/WS_Blocks/Templates/WATTSTOPPER.dwt" "COMPANY")

  
(command "-dwgunits" "" "" "" "N" "N")
(command "-layout" "template" "C:/WS_Blocks/Default/Templates/WATTSTOPPER.dwt" "COMPANY 22 X 34")

(command "-dwgunits" "" "" "" "N" "N")
(command "-layout" "template" "C:/WS_Blocks/Default/Templates/WATTSTOPPER.dwt" "COMPANY 36 x 48")
  
(command "._-PSETUPIN" "C:/WS_Blocks/Default/Templates/WATTSTOPPER.dwt" "*")

(defun *error* (msg)
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
  
(setvar "cmdecho" 0)
(command "-dwgunits" "" "" "" "Y" "Y")
(command "filedia" "1")
(princ)
)

 ;                                                    ;
 ;                                                    ;
 ;                                                    ;
 ;                  PDF Scaling Tool                  ;
 ;                                                    ;
 ;                                                    ;
 ;                                                    ;
(defun C:PDFscale (/ *error* what2 dist sclf oldecho)
  (setq oldecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setvar "insunits" 1)
  (command "-units" "4" "16" "1" "" "" "")

  (defun *error* (msg)
    (setvar "cmdecho" 1)
    (princ "\nRoutine terminated.")
    (princ)
  ) ;_ end of defun


  (initget "Yes No")
  (setq ans (getkword "\nRotate drawing? [Yes/No]: "))
  (if (/= nil ans)
    (if	(/= "No" ans)
      (if (= "Yes" ans)
	(progn
	  (setq newang (getangle "\nSelect angle to rotate drawing: "))
	  (command "rotate" "all" "" "0,0" (* 57.2958 newang))
	  (prompt (strcat "\nThe drawing was rotated."))
	) ;_ end of progn
      ) ;_ end of if
    ) ;_ end of progn
  ) ;_ end of if

  (initget 2)
  (setq what2 (getint "\n3ft door or 4ft light fixture or 2ft grid [3, 4, 2]: "))
  (if (= nil what2)
    (setq what2 4)
  ) ;_ end of if
  (setq scin (* 12 what2))
  (setq	dist (getdist "Pick two reference points: ")
	sclf (/ scin dist)
  ) ;_ end of setq
  (command "scale" "all" "" "0,0" sclf)
  (prompt (strcat "\nThe drawing was scaled by " (rtos sclf)))
  (setvar "cmdecho" 1)
  (princ)
) ;_ end of defun


(defun stringuppercase (string / )
	(setq string(strcase string))             														;sets string to uppercase
  	(princ string)																		;returns string to original function
)


 ;                                                    ;
 ;                                                    ;
 ;                                                    ;
 ;               WattStopper Replace                  ;
 ;                                                    ;
 ;                                                    ;
 ;                                                    ;



(defun c:REPLACE (/ ENT1 BL1 NWNM OLD NWRT OLDRT N I NEWBL ENT OBJ PROP PROPNAME DYPROPV)
  (prompt "Select blocks to replace: ")
  (setq ENT1 (ssget))
  (setq NEWBL (getstring "\nEnter new block name: "))
  (setq NEWBL ( stringuppercase NEWBL))
  (command "insert" NEWBL nil)
  (setq N (sslength ENT1))
  (setq I 0)
  (repeat N
    (setq BL1 (entget (ssname ENT1 I)))
    (setq ENT (ssname ENT1 I))
    (setq NWNM (cons 2 NEWBL))
 ;(setq NWRT (cons 50 BL1))
    (setq OLD (assoc 2 BL1))
 ;(setq OLDRT (assoc 50 BL1))
    (entmod (subst NWNM OLD BL1))
 ;(entmod (subst NWRT OLDRT BL1))
    (setq I (1+ I))
  ) ;_ end of repeat
  (prin1)
) ;_ end of defun



(defun c:WSREPLACE (/ ENT1 BL1 NWNM OLD NWRT OLDRT N I NEWBL ENT OBJ PROP PROPNAME DYPROPV)
  (vl-load-COM)
  (prompt "Select blocks to replace: ")
  (setq ENT1 (ssget))
  (setq NEWBL (getstring "\nEnter new block name: "))
  (setq NEWBL ( stringuppercase NEWBL))
  (command "insert" NEWBL nil)
  (setq N (sslength ENT1))
  (setq I 0)
  (repeat N
    (setq BL1 (entget (ssname ENT1 I)))
    (setq ENT (ssname ENT1 I))
;;;;
;;;;
    (setq Obj (vlax-ename->vla-object Ent))
    (if	(equal (vla-get-isdynamicblock Obj) :vlax-true)
      (progn
	(foreach prop (vlax-invoke Obj 'GetDynamicBlockProperties)
	  (setq PropName (vla-get-propertyname prop))
	  (if (member PropName
		      '("Angle"
		       )
	      ) ;_ end of member
	    (setq DyPropV (vlax-get prop 'Value))
	  ) ;_ end of if
	) ;_ end of foreach
	(setq NWNM (cons 2 NEWBL))
	(setq NWRT DyPropV)
	(setq OLD (assoc 2 BL1))
	(setq OLDRT (assoc 50 BL1))

	(entmod (subst NWNM OLD BL1))
	(myModifyBk ent1 i (list "Angle" NWRT))
	
      ) ;_ end of progn
    ) ;_ end of if
;;;;
;;;;
    (setq I (1+ I))
  ) ;_ end of repeat
  (prin1)
) ;_ end of defun


(defun c:WSREPLACE2 (/ ENT1 BL1 NWNM OLD NWRT OLDRT N I NEWBL ENT OBJ PROP PROPNAME DYPROPV)
  (vl-load-COM)
  (prompt "Select blocks to replace: ")
  (setq ENT1 (ssget))
  (setq NEWBL (getstring "\nEnter new block name: "))
  (setq NEWBL ( stringuppercase NEWBL))
  (command "insert" NEWBL nil)
  (setq N (sslength ENT1))
  (setq I 0)
  (repeat N
    (setq BL1 (entget (ssname ENT1 I)))
    (setq ENT (ssname ENT1 I))
;;;;
;;;;
    (setq Obj (vlax-ename->vla-object Ent))
    (if	(equal (vla-get-isdynamicblock Obj) :vlax-true)
      (progn
	(foreach prop (vlax-invoke Obj 'GetDynamicBlockProperties)
	  (setq PropName (vla-get-propertyname prop))
	  (if (member PropName
		      '("Angle1"
		       )
	      ) ;_ end of member
	    (setq DyPropV (vlax-get prop 'Value))
	  ) ;_ end of if
	) ;_ end of foreach
	(setq NWNM (cons 2 NEWBL))
	(setq NWRT DyPropV)
	(setq OLD (assoc 2 BL1))
	(setq OLDRT (assoc 50 BL1))

	(entmod (subst NWNM OLD BL1))
	(myModifyBk ent1 i (list "Angle1" NWRT))
	
      ) ;_ end of progn
    ) ;_ end of if
;;;;
;;;;
    (setq I (1+ I))
  ) ;_ end of repeat
  (prin1)
) ;_ end of defun



(defun myModifyBk (ss inx lstProp / index cnt oBkRef oProps i j oSBReferenceProperty)
  (vl-load-com)
  (setq e (ssname ss inx))
  (setq oBkRef (vlax-ename->vla-object e))

  (setq oProps (vlax-variant-value (vla-getdynamicblockproperties oBkRef)))

  (setq i (vlax-safearray-get-l-bound oProps 1))
  (while (<= i (vlax-safearray-get-u-bound oProps 1))
    (setq oSBReferenceProperty (vlax-safearray-get-element oProps i))
    (setq i (1+ i))
  ) ;_ end of while

  (setq j 0)
  (while (< j (length lstProp))
    (setq sProp (strcase (nth j lstProp)))
    (setPropValue oProps sProp (nth (+ 1 j) lstProp))
    (setq j (+ 2 j))
  ) ;while < j (length lstProp)

  (princ)
) ;_ end of defun

(defun setPropValue (oProps sProp Val / i oSBReferenceProperty sPName iFound)
  (setq i (vlax-safearray-get-l-bound oProps 1))
  (setq iFound 0)
  (while (and (<= i (vlax-safearray-get-u-bound oProps 1)) (= iFound 0))
    (setq oSBReferenceProperty (vlax-safearray-get-element oProps i))
    (setq sPName (vla-get-propertyname oSBReferenceProperty))
    (if	(= (strcase sPName) sProp)
      (progn
	(vla-put-value
	  oSBReferenceProperty
	  (vlax-make-variant
	    Val
	    (vlax-variant-type (vla-get-value oSBReferenceProperty))
	  ) ;_ end of vlax-make-variant
	) ;_ end of vla-put-value
	(setq iFound 1)
      ) ;_ end of progn
    ) ;_ end of if

    (setq i (1+ i))
  ) ;_ end of while
  (princ)
) ;_ end of defun

;;;;DTR converts degrees to radians. 
(defun dtr (a)
  (* pi (/ a 180.0))
) ;_ end of defun

;;RTD converts radians to degrees. 
(defun rtd (a)
  (/ (* a 180.0) pi)
) ;_ end of defun

 ;                                                    ;
 ;                                                    ;
 ;                                                    ;
 ;            WattStopper Prefix Save As              ;
 ;                                                    ;
 ;                                                    ;
 ;                                                    ;

(defun c:wssaveas (/ dwg path oldecho *error*)
  (defun *error* (msg)
    (setvar "cmdecho" 1)
    (princ "\nRoutine terminated.")
    (princ)
  ) ;_ end of defun
  (setq oldecho (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setq dwg (strcat (substr (getvar "dwgname") 1 (- (strlen (getvar "dwgname")) 4))))
  (setq path (getvar "dwgprefix"))
  (setq dwg (strcat "WS-" dwg ".dwg"))
  (if (findfile (strcat path "WS-" (substr dwg 7)))
    (progn
      (alert "Could not create file. File already exists.")
      (setvar "cmdecho" oldecho)
      (command nil)
    ) ;_ end of progn
    (progn
      (setq dwg (strcat path dwg))
      (command ".saveas" "" dwg)
      (alert (strcat "File saved sucessfully.\n" dwg))
    ) ;_ end of progn
  ) ;_ end of if


  (setvar "cmdecho" oldecho)
  (princ)
) ;_ end of defun





