; --- DWGSCALE Function ---
; Alex Lundin 02-01-2017
(defun C:CREATE-SCRIPT-SCALE-ENTIRE-DRAWING ()
  	(vl-load-com)
  	(alert																					; Dialog box with program informatoin
	  	"CREATE-SCRIPT-SCALE-ENTIRE-DRAWING																	
	  	
  		\n 1) Specify save location.
  		\n 2) Enter scale factor
  		\n 3) The script outputs a measurement that is formated as feet with ', just replace that in the text file and run.
  		\n 4) Everything else works for the most part
	       	"
	       
	)
  



  	(if																							; Directory setup

	  	(and
		(setq wfile (getfiled "\nSelect the directory to write the script file to: " "" "scr" 1))											; Sets directory location for script file
	  	(setq scale (getreal "Enter scale factor for layouts (e.g. for a scale of 1/8\" = 1'-0\", enter 96 below)."))									; Asks user for scale factor
	  	)
																					
	  	
		(progn																						; Starts program definition
			(setq scale (rtos scale))															
			(setq ofile (open wfile "w"))																		; Starts script text file
		  	(setq dir (vl-filename-directory wfile))																; Sets location of script file to previously chosen location


				(foreach x (vl-directory-files (setq dir (vl-filename-directory wfile)) "*.dwg" 1)								;---foreach loop, for files in location
				(write-line (strcat "_.open \""  dir "\\" x "\" (c:wshatch) (c:wslayout) (scaleall " scale ")  ._qsave _.close") ofile)
				)

		  
			(write-line "(princ)" ofile)																		; Writes a line to the script file that will end the script
			(close ofile)																				; Closes the script file															
		)																			
		(princ "\n<!> File Selection Error <!>")																	; Error checking
	)																					
;;;	(command "script" wfile)																				; Runs the created script file
  	(princ)																							; Ends the PDFIMPORT function

)
																						;END RISERLAYOUT


; --- modelspaceswitch Function ---
; Switch to model space
; Alex Lundin 11-06-2016
(defun modelspaceswitch ()	
  	(setvar "ctab" "Model")
	(princ)
)


; --- scaleall Function ---
; Switch to model space
; Alex Lundin 11-06-2016
(defun scaleall ( scale / allmodelspace scaleAsInteger )
  	(setq allmodelspace(ssget "x" '((410 . "Model"))))						; selection all blocks from WATTSTOPPER LAYER
  	(command "scale" allmodelspace "" "0,0" scale)
  	(command "-units" "4" "8" "1" "2" "0" "Y")
  	(command "_insunits" "1")
	(princ)
)