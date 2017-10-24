
; --- WSCPLBATCH Function ---
; Copy all items from certain layers
; Alex Lundin 12-16-2016
(defun RL2-wscplbatch(
		    
		   / BLOCKS BRIDGES CABLE CABLE2 MATCHLINE NOID OHM PSPACE ROOMID TOTAL
		   )
  	(setq layer (getvar "clayer"))										; save current layer to layer variable
    	(setq pspace(getvar "ctab"))										; save current space to pspace variable
  
  	(setq emptysetlayer "emptysetlayer")									; set variables for making the line on the emptysetlayer
  	(setq emptysetxcoord1 0)
  	(setq emptysetycoord1 0)
  	(setq emptysetxcoord2 30)
  	(setq emptysetpoint1 (list emptysetxcoord1 emptysetycoord1))
  	(setq emptysetpoint2 (list emptysetxcoord2 emptysetycoord1))


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
  		(command "copybase" "0,0" total "")								; copy from 0,0 basepoint
		)
		
		((= total nil)											; when total is nil
		(command "_.Layer" "_Make" "emptysetlayer" "_Color" "30" "" "LType" "Continuous" "" "")		; create emptysetlayer layer
		 
		(entmakex											; entmakex for line object
		(list '
		(0 . "line")
		(cons 8 emptysetlayer)										; line layer
		(cons 10 emptysetpoint1)
		(cons 11 emptysetpoint2)
		)
		)		 
		 
		(setq emptyset (ssget "x" '((8 . "emptysetlayer")(410 . "Model"))))				; selection of all items on the emptysetlayer layer in modelspace
		(command "copybase" "0,0" emptyset "")								; copy to clipboard
		(command "erase" "p" "")									; erase all items on emptysetlayer with the previous command
		(command "_.purge" "LA" "emptysetlayer" "N")							; purge emptysetlayer
		)
	)
  	(setvar "clayer" layer)											; restore current layer
  	(setvar "ctab" pspace)											; restore current tab
  
)






; --- RL2-pclipbatch Function ---
; Paste all items from clipboard
; Alex Lundin 12-16-2016
(defun RL2-pclipbatch (pastepoint )										; defined without C: to allow pass by reference
  	(setvar "ctab" "Model")
  	(setq insertionpoint (strcat (rtos pastepoint) "," (rtos 0)))
	(command "pasteclip" insertionpoint)									; pastepoint is global for RISERLAYOUT function
	(princ)
)



; --- RL2-modelspaceswitch Function ---
; Switch to model space
; Alex Lundin 11-06-2016
(defun RL2-modelspaceswitch ()	
  	(setvar "ctab" "Model")
	(princ)
)




; --- RISERLAYOUT Function ---
; Alex Lundin 05-12-2017
(defun C:RISERLAYOUT2 ()
  	(vl-load-com)
  
  	(alert
	(strcat
	  	"RISERLAYOUT2 function."
	  	"\n"
	  	"\nThis creates a Riser of the June 2017 format."
	  	"\nMultiple rungs off controller devices for CAT5E rungs."
		"\n"
	  	"\nObsolete function, no longer supported."
	  	"\nUse newest RISERLAYOUT if this function set does not work."
	  	"\n"
		"\nPlace all your WS-layouts in a nested folder before using this code."
		"\n"
		"\nMake sure all of your drawing files follow the WS-XXXXX naming convention."
		"\nFirst three characters must be WS- (capital WS and hypen)."
	  	"\n"
  		"\n1.)Save As Box"
  		"\n 	Navigate to the location of WS-Layout files."
  		"\n2.)Create Script"
  		"\n 	Give the script file a name and save in that same folder. "
  		"\n3.)Input"
  		"\n 	Enter distance in feet between layouts (12000 is a safe bet)."
  		"\n4.)Function"
  		"\n 	Will create the custom script and run it."
	)      
	)


  	(if																					;- if directory setup block
	  	(and																				;-- and
		(setq wfile (getfiled "\nSelect the directory to write the script file to: " "" "scr" 1))									;set write file location
		(setq spacer (getint "Enter space to offset layouts in inches (12000 is a safe bet)."))										;get space between layouts from user
		)																				;-- end and

	  	
		(progn																				;--progn
		  	(setq cntr 1)																		;set cntr variable for pastepoint multiplier
			(setq ofile (open wfile "w"))																;open new script file in write file location
		  	(setq dir (vl-filename-directory wfile))
	  		(write-line (strcat "_.new \"C:\\WS_Blocks\\Default\\Templates\\WATTSTOPPER.dwt" "\" _.saveas 2007 \"" dir "\\Riser-Layout.dwg\" _.close") ofile)	;first line, create new Riser-Layout
		  	(write-line (strcat "_.new \"C:\\WS_Blocks\\Default\\Templates\\SL-1-GSR2.dwg" "\" _.saveas 2007 \"" dir "\\SL-1-GSR2.dwg\" _.close") ofile)
				(foreach x (vl-directory-files (setq dir (vl-filename-directory wfile)) "*.dwg" 1)								;---foreach loop, for files in location
				(cond																		;----cond block
				  	((/= x "Riser-Layout")															;----cond statement when file is not Riser-Layout
					(setq character1 (substr x 1 1))
					(setq character2 (substr x 2 1))
					(setq character3 (substr x 3 1))
					(setq first3 (strcat character1 character2 character3))
					 	(cond	((= first3 "WS-")
							(setq pastepoint (* spacer cntr))													;calculate pastepoint, which is a global variable in RL2-pclipbatch function
							(setq pastepoint (itoa pastepoint))													;convert pastepoint to text value
							(write-line (strcat "_.open \""  dir "\\" x "\" (RL2-wscplbatch) ._qsave _.close") ofile)							;write the lines to copy and paste
							(write-line (strcat "_.open \"" dir "\\Riser-Layout.dwg\" (RL2-pclipbatch " pastepoint ") ._qsave _.close") ofile)
							)
						)
					)																	;----end cond statement
				)																		;----end cond block
				(setq cntr (+ cntr 1))																;increment counter that is used for pastepoint calculation
				(setq first3 nil)
				)																		;---end foreach
		  		(write-line (strcat "_.open \"" dir "\\Riser-Layout.dwg\" (c:upbs) (c:eblock2) ._qsave _.close") ofile)
		  		(write-line (strcat "_.open \"" dir "\\SL-1-GSR2.dwg\" (RL2-modelspaceswitch) (c:gsr2) ._qsave") ofile)
			(close ofile)																		;close script
		)																				;--end progn
		(princ "\n<!> File Selection Error <!>")	
	)																					;- end if
	(command "script" wfile)								
	(princ)

)																						;END RISERLAYOUT

