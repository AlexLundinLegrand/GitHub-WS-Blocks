; --- CREATESCRIPT Function ---
; Alex Lundin 05-27-2016
(defun C:CREATE-CUSTOM-SCRIPT ()
  	(vl-load-com)
  	(alert "CREATESCRIPT function.
  		\n 1.)Input        - Enter total number of commands.
  		\n 2.)Input        - Enter the names of the command in order.
  		\n 3.)Function - Will turn layers off to make bridges easily visible.
  		\n 4.)Function - Will move anything on Defpoints to Defpoints2 (keeps customer layer intact).
  		\n 5.)Function - Will copy walls to Defpoints (this layer does not allow osnaps).
  		\n 6.)Function - Will copy WATTSTOPPER layer to Defpoints too.
  		\n 7.)Function - Will save your osnap settings to restore at end of sequence.
  		\n 8.)Function - Will save set your osnaps to endpoints only.
  		\n 9.)Critical    - MSTP must only connect LMBC-300's or other network devices.
  		\n10.)Critical   - Spline must enter and exit from the exact same endpoint.
	       	\n11.)**Note   - Run MSTP through corridors when possible.
	       	\n12.)**Note   - Avoid crossing walls with MSTP when possible.
	       	\n13.)**Note   - Splines can be run in any order.
	       	\n14.)**Note   - Use several fit points to make next step easier.
	       	\n15.)**Note   - Run all the way through the BRIDGES sequence or osnaps will be lost.
		\n16.)Next       - Run BRIDGES2.
	       	"
	)
  
  	(setq functionNumber (getint "Enter the total number of commands to run per file:"))
	(setq cntr 0)													;initialize counter to 0 for while loop
  	(setq cmax functionNumber)											;set cmax to the number of functions
		(while (< cntr cmax)											;set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
			(setq functionName (getstring "Enter the name of the commands to run per file one at a time:"))
			(setq functionList (cons functionName functionList))
		(setq cntr(+ cntr 1))											;once while loop reaches this point, increment counter varaible by one			
  		)													;end while loop

  	(setq functionList (reverse functionList))



  	(if
	  	(and
		(setq wfile (getfiled "\nSelect File to Write to: " "" "scr" 1))
		(setq rfile (getfiled "\nSelect File from Directory to Read: " "" "dwg" 8))
		)
		(progn
			(setq ofile (open wfile "w"))
				(foreach x (vl-directory-files (setq dir (vl-filename-directory rfile)) "*.dwg" 1)
				
				(setq cntr 0)													;initialize counter to 0 for while loop
				(setq cmax functionList)											;set cmax to the number of functions
				(while (< cntr cmax)											;set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
					(setq functionName (nth cntr functionList))
					
					(write-line (strcat "_.open \""  rfile "\" (c:\"functionName\") _.close _.qsave) ofile)
					(setq cntr(+ cntr 1))											;once while loop reaches this point, increment counter varaible by one			
				)				
				
				
				
			  	)
			(close ofile)
		)
		(princ "\n<!> File Selection Error <!>")
	)
	;(command "script" wfile)
	(princ)

)															;END CREATESCRIPT
	