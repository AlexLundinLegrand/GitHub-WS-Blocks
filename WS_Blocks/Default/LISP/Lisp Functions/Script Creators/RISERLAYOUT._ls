; --- WSCPLBATCH Function ---
; Copy all items from certain layers
; Alex Lundin 07-10-2017
(defun wscplbatch
       			(
		   	/
		  	BLOCKS BRIDGES CABLE CABLE2 MATCHLINE NOID OHM PSPACE ROOMID TOTAL EMPTYSET EMPTYSETLAYER EMPTYSETPOINT1 EMPTYSETPOINT2 EMPTYSETXCOORD1 EMPTYSETXCOORD2 EMPTYSETYCOORD1 LAYER
			DRAWINGEXTENTSLAYER DRAWINGEXTENTSSET DRAWINGNAME INSERTIONPOINT SS STARTINGCURRENTLAYER TEXTCONTENT TEXTHEIGHT
		   	)




  	(setq drawingName (vl-filename-base (getvar "dwgname")))
  
  	(setq startingcurrentlayer (getvar "clayer"))								; save current layer to layer variable
    	(setq pspace(getvar "ctab"))										; save current space to pspace variable
  
  	(setq emptysetlayer "emptysetlayer")									; set variables for making the line on the emptysetlayer
  	(setq emptysetxcoord1 0)
  	(setq emptysetycoord1 0)
  	(setq emptysetxcoord2 30)
  	(setq emptysetpoint1 (list emptysetxcoord1 emptysetycoord1))
  	(setq emptysetpoint2 (list emptysetxcoord2 emptysetycoord1))

	(setq drawingextentslayer "drawingextentslayer")							; create layer for extents information
	(command "_.Layer" "_Make" "drawingextentslayer" "_Color" "2" "" "LType" "Continuous" "" "")
	
  
	(setvar "ctab" "Model")											; set current tab to model
  
	(setq blocks(ssget "x" '((8 . "WATTSTOPPER")(410 . "Model"))))						; selection all blocks from WATTSTOPPER LAYER
  	(setq cable(ssget "x" '((8 . "_WS_MSTP")(410 . "Model"))))						; same as above for other layers
  	(setq ohm(ssget "x" '((8 . "_WS_120_OHM")(410 . "Model"))))						; AutoCAD lisp does not use brackets, so anything in brackets below must be replaced
	(setq cable2(ssget "x" '((8 . "_WS_CAT5E")(410 . "Model"))))						; below the [setname] is a varaible name of your choosing, and [layername] is the layer you want to copy to the set.
  	(setq matchline(ssget "x" '((8 . "WATTSTOPPER MATCHLINE")(410 . "Model"))))				; to add new line do this (setq [setname] (ssget "x" '((8 . "[layername]"))))
	(setq roomid(ssget "x" '((8 . "WS-ROOM-ID")(410 . "Model"))))
  	(setq noid(ssget "x" '((8 . "NOID")(410 . "Model"))))
  	(setq bridges(ssget "x" '((8 . "BRIDGES")(410 . "Model"))))
  	(setq total (acet-ss-union (list blocks cable ohm cable2 matchline roomid noid bridges)))		; combine multiple selection sets into one, here are steps to add more:
  														; add the new [setname] to this line (setq total (acet-ss-union (list blocks cable ohm matchline [setname])))

  	(cond
	  	((/= total nil)											; when total is not nil
  											
		
		(setvar "clayer" drawingextentslayer)
		 
		(setq insertionPoint (list 0 0))
		(setq textcontent drawingName)
		(setq layer drawingextentslayer)
		(setq textheight 15.0)
		(insert-text-batch insertionPoint textcontent layer textheight)
		
		
		(setq drawingextentsSet(ssget "x" '((8 . "drawingExtentsLayer")(410 . "Model"))))		; include drawing text before drawing rectangle
		(setq total (acet-ss-union (list blocks cable ohm cable2 matchline roomid noid bridges drawingextentsSet)))

		(setq ss total)
		(get-smallest-rectangle-around-set-batch ss)							; draw rectangle around set
		 
		(setq drawingextentsSet(ssget "x" '((8 . "drawingExtentsLayer")(410 . "Model"))))
		(setq total (acet-ss-union (list blocks cable ohm cable2 matchline roomid noid bridges drawingextentsSet)))
		 
		(command "copybase" "0,0" total "")								; copy from 0,0 basepoint
		(command "erase" drawingextentsSet "")								; erase created rectangle and text
		(setvar "clayer" startingcurrentlayer)
		(command "_.purge" "LA" " drawingextentslayer" "N")
		 
		)
		
		((= total nil)											; when total is nil
		(command "_.Layer" "_Make" "emptysetlayer" "_Color" "30" "" "LType" "Continuous" "" "")		; create emptysetlayer layer

		(setvar "clayer" emptysetlayer)
		 
		(setq insertionPoint (list 0 20))
		(setq textcontent "0 blocks found on WATTSTOPPER layer of this drawing:")
		(setq layer emptysetlayer)
		(setq textheight 15.0)
		(insert-text-batch insertionPoint textcontent layer textheight)
		 
		(setq insertionPoint (list 0 0))
		(setq textcontent drawingName)
		(setq layer emptysetlayer)
		(setq textheight 15.0)
		(insert-text-batch insertionPoint textcontent layer textheight)
		 
		(setq emptyset (ssget "x" '((8 . "emptysetlayer")(410 . "Model"))))				; selection of all items on the emptysetlayer layer in modelspace
		(get-smallest-rectangle-around-set-batch emptyset)

		(setq emptyset (ssget "x" '((8 . "emptysetlayer")(410 . "Model")))) 
		(command "copybase" "0,0" emptyset "")								; copy to clipboard
		(command "erase" emptyset "")									; erase all items on emptysetlayer with the previous command
		(setvar "clayer" startingcurrentlayer)
		(command "_.purge" "LA" "emptysetlayer" "N")							; purge emptysetlayer
		)
	)
  	(setvar "clayer" startingcurrentlayer)									; restore current layer
  	(setvar "ctab" pspace)											; restore current tab
  
)



; --- get-smallest-rectangle-around-set-batch Function ---
; Accepts selection set as arugment
; uses a inline function to create a bounding box around the first entity
; stores the data and continues to next entity
; at the end, the function finds the points that are the farthest apart and uses them for the rectangle
; Alex Lundin 12-16-2016
(defun get-smallest-rectangle-around-set-batch ( ss / LL MAXPT MINPT OLL OUR UR )

  	;;; inline function to get bounding box for a object
	(defun obb (ent / ); = Object's Bounding Box corners 
	(vla-getboundingbox (vlax-ename->vla-object ent) 'minpt 'maxpt)
	(setq
	oLL (vlax-safearray->list minpt); Object's Lower Left
	oUR (vlax-safearray->list maxpt); Object's Upper Right
	); setq
	); defun -- obb
(vl-load-com)
(obb (ssname ss 0))
(setq LL oLL UR oUR); initial overall LL & UR [of first object]
(ssdel (ssname ss 0) ss)
(repeat (sslength ss)
(obb (ssname ss 0))
(setq
LL (mapcar 'min oLL LL); least of each component
UR (mapcar 'max oUR UR); greatest of each component
); setq
(ssdel (ssname ss 0) ss)
); repeat
(command "_.rectangle" "_none" LL "_none" UR)
(princ)
)



; --- insert-text Function ---
; Paste all items from clipboard
; Alex Lundin 12-16-2016
(defun insert-text-batch
       	(insertionPoint textcontent layer textheight / )
    	(entmake
    	(list
      	(cons 0 "MTEXT")         		;; Entity Name
      	(cons 100 "AcDbEntity")  		;; Subclass Marker
      	(cons 410 "Model")       		;; Space
      	(cons 8 layer)         			;; Layer
      	(cons 100 "AcDbMText")   		;; Subclass Marker
      	(cons 10 insertionPoint) 		;; Insertion Point
      	(cons 40 textheight)            	;; Text Height
      	(cons 71 1)              		;; Attachment Point (top-center)
      	(cons 1 textcontent)    		;; Text Content
      	(cons 7 "Arial")			;; text style
      	)
   	)
)



; --- PCLIPBATCH Function ---
; Paste all items from clipboard
; Alex Lundin 12-16-2016
(defun pclipbatch (pastepoint / insertionpoint)										; defined without C: to allow pass by reference
  	(setvar "ctab" "Model")
  	(setq insertionpoint (strcat (rtos pastepoint) "," (rtos 0)))
	(command "pasteclip" insertionpoint)									; pastepoint is global for RISERLAYOUT function
	(princ)
)



; --- modelspaceswitch Function ---
; Switch to model space
; Alex Lundin 11-06-2016
(defun modelspaceswitch ( / )	
  	(setvar "ctab" "Model")
	(princ)
)




; --- RISERLAYOUT Function ---
; Alex Lundin 07-03-2017
(defun C:RISERLAYOUT 	(
			/
			*ACDOCS* *DWG* ACADOBJECT CLOSEFLAG DOCUMENTCOLLECTION DRAWINGNAMEASSTRING DRAWINGOBJECT OPENDRAWINGLISTASSTRIN
			OPENDRAWINGLISTASSTRING CURRENTDRAWINGLOCATION OPENDRAWINGLIST OPENDRAWINGTEST VALIDCONTINUATIONCHOICE CONTINUE TEXTF1
			TITLEBLOCKEXTRACTIONEXISTS CHARACTER1 CHARACTER2 CHARACTER3 CNTR DIR F1 F1EXISTS F2 F2EXISTS F3 F3EXISTS F4 F4EXISTS F5 F5EXISTS
			F6 F6EXISTS F7 F7EXISTS FIRST3 FOUNDDRAWINGWITHWSPREFIX OFILE PASTEPOINT SPACER WFILE FOUNDOPENDRAWING FULLDRAWINGNAME
			CMAX CURRENTDRAWINGOBJECT CURRENTDRAWINGSTRING DRAWINGSTOCLOSEASSTRINGLIST OPENDRAWINGANDOBJECT OPENDRAWINGASOBJECT OPENDRAWINGASSTRING
			)
  	(vl-load-com)

  
  	(alert
	(strcat
  	"RISERLAYOUT Script Creator LISP function."
	"\n"
  	"\nCustomer to verfiy two conditions before starting."
	"\n"
	"\n1.) The current drawing is NOT part of the folder you want to process."
	"\n"
	"\n"
	"\n2.) Building layouts match standard naming convention below."
	"\n	Standard naming convention."
	"\n	First three characters must be WS- (capital WS and hypen)."
	"\n	Correct Example: 	WS-E101"
	"\n	Incorrect Example: 	WS -E101"
	"\n"
	"\nPay attention to the criteria above, they will save you time."
	"\nClick OK."
	"\nType s to start, only if you are sure these conditions are met"
	"\nType e to exit"
	)
	)
	(setq validContinuationChoice 0)
	(while	(= validContinuationChoice 0)
		(setq continue (getstring "If starting conditions are met then type s to start, type e to exit."))
		(if
		  	(or (= continue "e")(= continue "E"))
		  	(progn
			(quit)
			)
		)		  
	  	(if
			(or (= continue "s")(= continue "S"))
		  	(progn
			(setq validContinuationChoice 1)
		  	;;; previous method of pointing to save directory

			)
		)
	)

	;;; get inputs
    	(alert
	(strcat
	  	"Script creation"
		"\n 1.)Save As Box"
		"\n	Navigate to the location of WS-Layout files."
		"\n 2.)Create Script"
		"\n	Give the script file a name and save in that same folder"
		"\n	with the building layout drawing files. "
		"\n 3.)Input"
		"\n	Enter distance in feet between layouts (12000 is a safe bet)."
		"\n 4.)Function"
		"\n	Will create the custom script and run it."
		"\n"
  		"\n Click OK."
	)      
	)
	(setq wfile (getfiled "\nSelect the directory to write the script file to: " "" "scr" 1))
  	(setq spacer (getint "Enter space to offset layouts in inches (12000 is a safe bet)."))

  	(if																					;- if directory setup block
	  	(and																				;-- and
		(/= wfile nil)																			;check conditions
		(/= spacer nil)																			;check conditions
		)																				;-- end and


		(progn																				;--progn

		
	 	(setq dir (vl-filename-directory wfile))


	  	;;; look for any existing drawing files with same names that this function will create
		(setq textf1 (strcat dir "\\AutoCAD_Title_Blocks.txt"))
		(setq titleBlockExtractionExists (findfile textf1))
		
		(if
		  	;;; if script found drawings it needs to delete
		  	;;; dont call script file
		  	(OR
			(= titleBlockExtractionExists nil)
			)
		  	(progn
			(alert																				;show alert and don't run script
			(strcat
			  	" Checkpoint"
			  	"\n Read message to determine what AutoCAD needs from you."
			  	"\n"
				"\n RISERLAYOUT did not find a AutoCAD_Title_Blocks.txt file."
				"\n The script you create can use the AutoCAD_Title_Blocks.txt file."
				"\n The script you create will update all title blocks in the folder with it."
			  	"\n You can exit now and make a text file from a title block WSTBEXPORT."
			  	"\n"
			  	"\n This halt condition communicates that:"
			  	"\n Your nested folder does not have enough information to update title blocks."
			  	"\n"
			  	"\n Click OK."
			  	"\n Type c to continue."
			  	"\n Type e to exit."
			)
			)
			(setq validContinuationChoice 0)
				(while	(= validContinuationChoice 0)
					(setq continue (getstring "Script did not find title block extraction for updating title blocks, type c to continue anyway, type e to exit."))
					(if
					  	(or (= continue "e")(= continue "E"))
					  	(progn
						(quit)
						)
					)
				  	(if
						(or (= continue "e")(= continue "E")(= continue "c")(= continue "C"))
					  	(progn
						(setq validContinuationChoice 1)
						)
					)
				)
			)
		)


		

		
	  	(setq cntr 1)																		;set cntr variable for pastepoint multiplier
		(setq ofile (open wfile "w"))																;open new script file in write file location

		;;; start writing script
  		(write-line (strcat "_.new \"C:\\WS_Blocks\\Default\\Templates\\WATTSTOPPER.dwt" "\" _.saveas 2007 \"" dir "\\Riser-Layout.dwg\" _.close") ofile)	;first line, create new Riser-Layout
	  	(write-line (strcat "_.new \"C:\\WS_Blocks\\Default\\Templates\\SL-DLM.dwg" "\" _.saveas 2007 \"" dir "\\SL-DLM.dwg\" _.close") ofile)
		(write-line (strcat "_.new \"C:\\WS_Blocks\\Default\\Templates\\SL-VANTAGE.dwg" "\" _.saveas 2007 \"" dir "\\SL-VANTAGE.dwg\" _.close") ofile)
	  	(write-line (strcat "_.new \"C:\\WS_Blocks\\Default\\Templates\\ROOMDETAILS.dwt" "\" _.saveas 2007 \"" dir "\\RD-DLM.dwg\" _.close") ofile)
	  	(setq foundDrawingWithWSprefix 0)															;use to determine if drawing with "WS-" is found
		(setq foundOpenDrawing 0)
		(setq openDrawingTest 0)
		(setq drawingsToCloseAsStringList "")
			;;; loop through all drawing files in script save location
			(foreach x (vl-directory-files (setq dir (vl-filename-directory wfile)) "*.dwg" 1)								;---foreach loop, for files in location
			(setq fullDrawingName (strcat  dir "\\" x))


			(setq openDrawingTest (vl-file-systime fullDrawingName))										;check if current drawing is already open

			;;; if a file is open
			;;; alert user and quit
			(if
			  	(= openDrawingTest nil)
			  	(progn
				(setq foundOpenDrawing 1)
				(setq drawingsToCloseAsStringList (strcat fullDrawingName drawingsToCloseAsStringList))
				)
			)
			  
			(cond																		;----cond block
			  	((/= x "Riser-Layout")															;----cond statement when file is not Riser-Layout
				(setq character1 (substr x 1 1))
				(setq character2 (substr x 2 1))
				(setq character3 (substr x 3 1))
				(setq first3 (strcat character1 character2 character3))
				 	;;; controls which drawings to open and copy blocks from
				 	(cond	((= first3 "WS-")
						 
						(setq foundDrawingWithWSprefix 1)											;found a drawing with "WS-" prefix
						(setq pastepoint (* spacer cntr))											;calculate pastepoint, which is a global variable in pclipbatch function
						(setq pastepoint (itoa pastepoint))											;convert pastepoint to text value
						(write-line (strcat "_.open \""  dir "\\" x "\" (SELECT-AND-FORMAT-ROOMID-WRAPPER WATTSTOPPER)(wscplbatch)(c:wstbimport) ._qsave _.close") ofile) 			;write the lines to copy and paste			
						(write-line (strcat "_.open \"" dir "\\Riser-Layout.dwg\" (pclipbatch " pastepoint ") ._qsave _.close") ofile)
						)
						
						
						((OR(= first3 "RD-")(= first3 "SL-")(= first3 "WS-"))
						(setq fileToSkip "true")
						)						
						
						;;; updates titleblock of any drawing that is not Riser-Layout, RD-, SL- or WS- drawing
						(t
						(write-line (strcat "_.open \""  dir "\\" x "\" (c:wstbimport) ._qsave _.close") ofile)
						)
					)
				)																	;----end cond statement
			)																		;----end cond block
			(setq cntr (+ cntr 1))																;increment counter that is used for pastepoint calculation
			(setq first3 nil)
			)																		;---end foreach
			
	  		(write-line (strcat "_.open \"" dir "\\RD-DLM.dwg\" (c:wstbimport) (modelspaceswitch)") ofile)
			(write-line (strcat "_.open \"" dir "\\SL-VANTAGE.dwg\" (c:wstbimport) (modelspaceswitch)") ofile)
	  		(write-line (strcat "_.open \"" dir "\\SL-DLM.dwg\" (c:wstbimport) (modelspaceswitch)") ofile)
			(write-line (strcat "_.open \"" dir "\\Riser-Layout.dwg\" (c:wstbimport) (c:upbs) (c:eblock) (c:eblockvantage) (c:eblockrd) ._qsave _.close") ofile)
		)																			;--end progn
	  	(progn
		(princ "\n<!> File Selection Error <!>")
		(close ofile)
		)
	)																				;- end if
  

	(close ofile)


  	(if
	  	;;; if script did not find layout drawings
	  	;;; dont call script file
	  	(= foundDrawingWithWSprefix 0)
	  	(progn
		(alert																				;show alert and don't run script
		(strcat
		  	"Checkpoint"
		  	"\nRead message to determine what AutoCAD needs from you."
		  	"\n"
			"\nRISERLAYOUT did not find layout drawings to copy blocks from."
		  	"\n"
			"\nFirst three characters of the building layout drawings must be"
			"\n	WS- (capital WS and hypen)."
			"\n"
			"\nYou can choose to run the script anyway."
		  	"\n"
		  	"\nThis halt condition communicates that:"
		  	"\nThe script you create will not find any product blocks"
			"\nThis results in a empty Riser-layout.dwg."
		  	"\n"
		  	"\nClick OK."
		  	"\nType c to continue."
		  	"\nType e to exit."
		)
		)


		(setq validContinuationChoice 0)
			(while	(= validContinuationChoice 0)
				(setq continue (getstring "Script will not find WS- drawings to copy blocks from, type c to continue, type e to exit."))
				(if
				  	(or (= continue "e")(= continue "E"))
				  	(progn
					(quit)
					)
				)
			  	(if
					(or (= continue "e")(= continue "E")(= continue "c")(= continue "C"))
				  	(progn
					(setq validContinuationChoice 1)
					)
				)
			)
		)
	)









  
  	(if
	  	;;; if script found open drawings
	  	;;; see if user wants to save and close them or exit this function
	  	(= foundOpenDrawing 1)
	  	(progn
		(alert																				;show alert and don't run script
		(strcat
		  	"Checkpoint"
		  	"\nRead message to determine what AutoCAD needs from you."
		  	"\n"
			"\nRISERLAYOUT must close any drawings that are open from the folder."
		  	"\nIt will save them first."
		  	"\n"
		  	"\nThis halt condition communicates that:"
		  	"\nThe script you create needs any opened drawings from the folder to close."
		  	"\n"
		  	"\nClick OK."
		  	"\nType s to attempt to save and close open files then continue."
		  	"\nType e to exit."
		)
		)
		
		
		(setq validContinuationChoice 0)
			(while	(= validContinuationChoice 0)
				(setq continue (getstring "RISERLAYOUT found open drawings in folder, type s to attempt to save and close open files then continue, type e to exit."))
				(if
				  	(or (= continue "e")(= continue "E"))
				  	(progn
					(quit)
					)
				)
			  	(if
					(or (= continue "s")(= continue "S"))
				  	(progn
					(setq validContinuationChoice 1)
					)
				)
			)

			(setq acadobject (vlax-get-Acad-Object))
			(setq documentcollection (vla-get-documents acadobject))
		
			;;; for loop to get all object names for any open drawing
			(vlax-for <doc> documentcollection
			(if (eq "" (setq *dwg* (vla-get-fullname <doc>))) (setq *dwg* (vla-get-name <doc>))) 
			(setq *acdocs* (cons (cons *dwg* <doc>) *acdocs*))
			)

			
			(setq currentDrawingObject (vla-get-ActiveDocument acadobject))
			(setq currentDrawingString (vla-get-fullname currentDrawingObject))
			(setq cntr 0)
			(setq cmax (length *acdocs*))
			;;; while loop through all open drawings
			(while 	(< cntr cmax)
				(setq openDrawingAndObject (nth cntr *acdocs*))
			  	(setq openDrawingAsString (car openDrawingAndObject))
			  	(setq openDrawingAsObject (cdr openDrawingAndObject))
			  	;;; search the list of drawings to close for the open drawingAsString
			  	(setq closeFlag (vl-string-search openDrawingAsString drawingsToCloseAsStringList))							
			  	
				(if	(/= closeFlag nil)
				  	(progn
					(if
					  	;;; if the current drawing
					  	(/= currentDrawingString openDrawingAsString)
						(progn
							(vla-save openDrawingAsObject)
						  	(vla-close openDrawingAsObject)
						)
						(progn
							(alert																				;show alert and don't run script
							(strcat
							  	"Exit condition"
							  	"\n"
								"\n"
								"\nAny drawing that is part of the finished script must be closed."
								"\nThe active drawing is part of that list."
								"\nIt must be closed now, before the script runs."
								"\nNormally, RISERLAYOUT calls your custom script as the last step."
								"\nThis is not possible, since the active drawing is closing."
								"\nAll LISP functions terminate when the active drawing closes."
								"\n"
								"\nThis function will handle the error and exit gracefully."
								"\nThe active drawing will stay open for you to close it."
								"\n"
								"\nNext time verify your starting conditions before using this function."
							  	"\n"
							  	"\nClick OK."
							  	"\nExiting now."
		
							)
							)
							(exit)
						)
					)
					)
				)
				(setq cntr (+ cntr 1))
			)
			;(vlax-release-object acadobject documentcollection)   
		)
	)








	  	;;; look for any existing drawing files with same names that this function will create
		(setq f1 (strcat dir "\\Riser-Layout.dwg"))
		(setq f1exists (findfile f1))
	  	(setq f2 (strcat dir "\\SL-DLM.dwg"))
		(setq f2exists (findfile f2))
	  	(setq f3 (strcat dir "\\SL-VANTAGE.dwg"))
		(setq f3exists (findfile f3))
	  	(setq f4 (strcat dir "\\RD-DLM.dwg"))
		(setq f4exists (findfile f4))

		;;; look for any existing drawing files with the previous naming conventions
	  	(setq f5 (strcat dir "\\SL-1.dwg"))
		(setq f5exists (findfile f5))
	  	(setq f6 (strcat dir "\\SL-1-Vantage.dwg"))
		(setq f6exists (findfile f6))
	  	(setq f7 (strcat dir "\\RD.dwg"))
		(setq f7exists (findfile f7))



		(if
		  	;;; if script found drawings it needs to delete
		  	;;; dont call script file
		  	(OR
			(/= f1exists nil)(/= f2exists nil)(/= f3exists nil)(/= f4exists nil)(/= f5exists nil)(/= f6exists nil)(/= f7exists nil)
			)
		  	(progn


			(alert																				;show alert and don't run script
			(strcat
			  	"Checkpoint"
			  	"\nRead message to determine what AutoCAD needs from you."
			  	"\n"
				"\nRISERLAYOUT needs to save files with names that already exist."
				"\nThe easiest way is to delete any files that have names this function uses."
				"\n"
				"\nYou can use this function to delete existing templates in your folder."
				"\nExamples are, Riser-layout.dwg, SL-DLM.dwg, SL-Vantage.dwg, ect."
				"\nYou can choose to delete them to continue."
			  	"\n"
			  	"\nThis halt condition communicates that:"
			  	"\nFiles must be deleted to continue."
			  	"\n"
			  	"\nClick OK."
			  	"\nType d to delete files and continue."
			  	"\nType e to exit."
			)
			)


			(setq validContinuationChoice 0)
				(while	(= validContinuationChoice 0)
					(setq continue (getstring "Script must save new templates but they already exist in the folder, type d to delete them and move forward, type e to exit."))
					(if
					  	(or (= continue "e")(= continue "E"))
					  	(progn
						(quit)
						)
					)
				  	(if
						(or (= continue "e")(= continue "E")(= continue "d")(= continue "D"))
					  	(progn
						(setq validContinuationChoice 1)
						)
					)
				)
			)

			

		)



		
		;;; delete any files found
		(if
		  	(/= f1exists nil)
			(vl-file-delete f1exists)
		)
		(if
		  	(/= f2exists nil)
			(vl-file-delete f2exists)
		)			
		(if
		  	(/= f3exists nil)
			(vl-file-delete f3exists)
		)	
		(if
		  	(/= f4exists nil)
			(vl-file-delete f4exists)
		)
		(if
		  	(/= f5exists nil)
			(vl-file-delete f5exists)
		)
		(if
		  	(/= f6exists nil)
			(vl-file-delete f6exists)
		)
		(if
		  	(/= f7exists nil)
			(vl-file-delete f7exists)
		)


  
  	(if
	  	;;; if script did find layout drawings
	  	(= foundDrawingWithWSprefix 1)
		;;; else script did find layout drawings
		(progn
		(alert																				;show alert for for script completion
		(strcat
		  	"Nice job!"
		  	"\nRISERLAYOUT found drawings with the correct naming convention."
			"\nRISERLAYOUT will now call your custom script and it will run."
			"\n"
			"\nYour custom script will create all the necessary templates"
			"\nThen it will assemble a source drawing with all blocks across the project."
			"\nFinally it will run all the extractors for you."
			"\n"
			"\nThe generate functions now have extensive error handling that can only be accessed from manual function calls."
			"\n"
			"\nCall each LISP function individually from the matching template:"
			"\n"
			"\nTemplate: 	RD-DLM		Function: GRD"
			"\nTemplate: 	SL-DLM 		Function: GSR"
			"\nTemplate: 	SL-VANTAGE	Function: GSRVANTAGE"
			"\n"
			"\nThere will be a orange note inside each template to remind you which function to call."
			"\nClick OK."
		)
		)		
		)
	)

  	














  	(command "script" wfile)																		;run script
	(princ)
)																						;END RISERLAYOUT


(defun *error* (msg)
(princ)
)

;;;							(setq drawingtotoggle (strcat dir "\\Riser-Layout.dwg\""))
;;;(write-line (strcat "(script-drawing-toggle " drawingtotoggle ") (pclipbatch " pastepoint ") ._qsave _.close") ofile)
(defun script-drawing-toggle (drawingtotoggle / doc)
	  (vlax-for doc (vla-get-Documents (vlax-get-acad-object))
	    (if (eq (vla-get-fullname doc) drawingtotoggle )
	      (vla-put-windowstate doc acmax)
	      (vla-put-windowstate doc acmin)
	    )
	  )
)




