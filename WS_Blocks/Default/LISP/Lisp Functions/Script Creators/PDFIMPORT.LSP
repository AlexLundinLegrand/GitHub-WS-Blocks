; --- PDFIMPORT Function ---
; Daniel Sidelsky 05-01-2017
; Updated paths Alex Lundin 03-15-2017
(defun C:PDFIMPORT ()
  	(vl-load-com)
  	(alert																							; Dialog box with program information
	  	"PDFIMPORT - Last updated 05/01/2017 by Daniel Sidelsky. \n\nThis function will create DWG files (WS-1, WS-2, etc...) for each of your layout PDFs in the order they appear in the folder. You can rename your drawings after.
  		\n 1) Navigate to the location of your lighting plans and click SAVE.
  		\n 2) Enter the scale factor (e.g. for a scale of 1/8\" = 1'-0\", enter 96).
  		\n 3) Enter the number of plans per PDF (e.g. for a PDF with 10 plans, enter 10).
  		\n 4) The script will then create your DWG files, wait until your screen stops flashing.
	       	"
	       
	)
  



  	(if																							; Directory setup

	  	(and
		(setq wfile (getfiled "\nSelect the directory to write the script file to: " "" "scr" 1))											; Sets directory location for script file
	  	(setq scale (getint "Enter scale factor for layouts (e.g. for a scale of 1/8\" = 1'-0\", enter 96 below)."))									; Asks user for scale factor
		(setq pages (getint "Enter number of pages per PDF (e.g. for PDFs with a single sheet each, enter 1 below)."))									; Asks user for pages per PDF
	  	)
																					
	  	
		(progn																						; Starts program definition
																		
			(setq ofile (open wfile "w"))																		; Starts script text file
		  	(setq dir (vl-filename-directory wfile))																; Sets location of script file to previously chosen location
		  	(setq y 1)																				; Sets counter variable to 1
				(foreach x (vl-directory-files (setq dir (vl-filename-directory wfile)) "*.pdf" 1)										; Starts for loop, looping through all PDFs in chosen directory
					(setq pagecount 1)
				  	(repeat pages																		; Starts nested repeat loop, running for the number of pages in each PDF
					  	(write-line (strcat "_.new \"C:\\WS_Blocks\\Default\\Templates\\WATTSTOPPER.dwt" "\" _.saveas 2007 \"" dir "\\WS-" (itoa y) ".dwg\"") ofile)		; Writes a line to the script file that will create a new DWG file
					  	(write-line "(modelspaceswitch)" ofile)														; Writes a line to the script file that will swap to model space
					  	(write-line (strcat "(command \"-PDFATTACH\" \""  x "\" \"" (itoa pagecount) "\" \"0,0\" \"" (itoa scale) "\" \"0\") _.qsave _.close") ofile)	; Writes a line to the script file that will attach the PDF to the DWG
						(setq y (+ y 1))
						(setq pagecount (+ pagecount 1))														; Increments the counter variable
					)
				)
			(write-line "(princ)" ofile)																		; Writes a line to the script file that will end the script
			(close ofile)																				; Closes the script file															
		)																			
		(princ "\n<!> File Selection Error <!>")																	; Error checking
	)																					
	(command "script" wfile)																				; Runs the created script file
  	(princ)																							; Ends the PDFIMPORT function

)



; --- PDFIMPORT2 Function ---
; Original by Daniel Sidelsky 11-22-2016
; Modified by Alex Lundin 05-03-2017
; used when designer wants to keep XREF name as the drawing name.
(defun C:PDFIMPORT2 ()
  	(vl-load-com)
  	(alert																							; Dialog box with program informatoin
	  	"PDFIMPORT function by Daniel Sidelsky and Alex Lundin. Last updated 11/30/16.																		
	  	
	  	\nThis function will create separate DWG files (with names matching xrefs) for each of your layout PDFs in the order they appear in the folder.
	  	
		\nMake sure your lighting plans are saved as individual PDFs (one per page) in a separate folder.
  		
  		\n 1) After clicking OK, navigate to the location of the Lighting Layout PDFs and click SAVE.
  		\n 2) Enter a number for the scale factor of your layouts (e.g. for a scale of 1/8\" = 1'-0\", enter 96).
  		\n 3) Enter a number for the number of pages per PDF (e.g. for a PDF with 10 plans, enter 10).
  		\n 4) The script will then create your DWG files, wait until your screen stops flashing.
	       	"
	       
	)
  



  	(if																							; Directory setup

	  	(and
		(setq wfile (getfiled "\nSelect the directory to write the script file to: " "" "scr" 1))											; Sets directory location for script file
	  	(setq scale (getint "Enter scale factor for layouts (e.g. for a scale of 1/8\" = 1'-0\", enter 96 below)."))									; Asks user for scale factor
		(setq pages (getint "Enter number of pages per PDF (e.g. for PDFs with a single sheet each, enter 1 below)."))									; Asks user for pages per PDF
	  	)
																					
	  	
		(progn																						; Starts program definition
																		
			(setq ofile (open wfile "w"))																		; Starts script text file
		  	(setq dir (vl-filename-directory wfile))																; Sets location of script file to previously chosen location
		  	(setq y 1)																				; Sets counter variable to 1
				(foreach x (vl-directory-files (setq dir (vl-filename-directory wfile)) "*.pdf" 1)										; Starts for loop, looping through all PDFs in chosen directory
					(setq pagecount 1)
				  	(repeat pages																		; Starts nested repeat loop, running for the number of pages in each PDF
					  	(write-line (strcat "_.new \"C:\\WS_Blocks\\Default\\Templates\\WATTSTOPPER.dwt" "\" _.saveas 2007 \"" dir "\\" x ".dwg\"") ofile)		; Writes a line to the script file that will create a new DWG file
					  	(write-line "(modelspaceswitch)" ofile)														; Writes a line to the script file that will swap to model space
					  	(write-line (strcat "(command \"-PDFATTACH\" \""  x "\" \"" (itoa pagecount) "\" \"0,0\" \"" (itoa scale) "\" \"0\") _.qsave _.close") ofile)	; Writes a line to the script file that will attach the PDF to the DWG
						(setq y (+ y 1))
						(setq pagecount (+ pagecount 1))														; Increments the counter variable
					)
				)
			(write-line "(princ)" ofile)																		; Writes a line to the script file that will end the script
			(close ofile)																				; Closes the script file															
		)																			
		(princ "\n<!> File Selection Error <!>")																	; Error checking
	)																					
	(command "script" wfile)																				; Runs the created script file
  	(princ)																							; Ends the PDFIMPORT function

)






		