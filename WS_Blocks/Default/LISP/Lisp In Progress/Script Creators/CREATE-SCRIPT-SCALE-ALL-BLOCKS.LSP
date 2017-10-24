; --- DWGSCALE Function ---
; Alex Lundin 02-01-2017
(defun C:CREATE-SCRIPT-SCALE-ALL-BLOCKS ()
  	(vl-load-com)
  	(alert																					; Dialog box with program informatoin
	  	"CREATE-SCRIPT-SCALE-ALL-BLOCKS																	
  		
  		\n 1) After clicking OK, navigate to the location of the drawings with blocks to scale
  		\n 2) Enter scale factor
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
				(write-line (strcat "_.open \""  dir "\\" x "\" (scaleallblocks " scale ")  ._qsave _.close") ofile)
				)

		  
			(write-line "(princ)" ofile)																		; Writes a line to the script file that will end the script
			(close ofile)																				; Closes the script file															
		)																			
		(princ "\n<!> File Selection Error <!>")																	; Error checking
	)																					
	(command "script" wfile)																				; Runs the created script file


)



; --- scaleallblocks Function ---
; Switch to model space
; Alex Lundin 11-06-2016
(defun scaleallblocks (scale)


  
  	(setq allwattstopper(ssget "x" '((410 . "Model")(0 . "INSERT")(8 . "WATTSTOPPER"))))



    	(cond														;-conditional block
		((/= allwattstopper nil)											;-conditional statement
		(setq cntr 0)												;set cntr 3 to 0		
	 	(setq cmax (sslength allwattstopper))
		(while 	(< cntr cmax)											;--while loop in nested conditional
			(setq block(ssname allwattstopper cntr))					
			(setq ent block)										;set ent to the correct variable, this step is not necessary for the function, you can call on any parameter desired, it does keep calls consistent though





			(entmod-scale ent scale)


		  
			(setq cntr(+ cntr 1))
		)													;--end while
  		)													;-end conditional statement	 
	)


  
	(princ)
)


; --- entmod-scale Sub Function ---
; change objects layer
; Arguments
; entity's name passed into ent variable
; layer passed into layer variable
; Alex Lundin 12-21-2016

(defun entmod-scale	(ent scale / )


			(setq entdxf(entget ent))						;set dxf codes of ent to entdxf
			(setq entdxf(subst (cons 41 scale)(assoc 41 entdxf) entdxf))		;change dxfcodes on entdxf
		  	(entmod entdxf)

		  
		  	(setq entdxf(entget ent))						;set dxf codes of ent to entdxf
		  	(setq entdxf(subst (cons 42 scale)(assoc 42 entdxf) entdxf))		;change dxfcodes on entdxf
			(entmod entdxf)	
  

)