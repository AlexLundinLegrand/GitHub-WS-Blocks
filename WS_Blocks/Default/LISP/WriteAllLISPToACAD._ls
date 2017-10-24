; ---WriteAllLISPToACAD---
; Notes:
; Allows Developer to Automatically build a file that will load other lisp files
; This function creates new acad.lisp file from parameters set in this file
; AutoCAD will load the acad.lsp file, which is created here, into each AutoCAD session exactly one time at the beginning
; This function supports two directories at this point
; Alex Lundin 06-22-2017
(defun c:WriteAllLISPToACAD ( / DIRECTORIES DIRECTORY LISPFILES TYP WRITEFILE )
	;;; if statement
	(if
	  	;;; 4 and conditions must be satisfied
		(and
		  	;;; 1 write file must be found
			(setq writeFile "C:\\WS_Blocks\\Default\\LISP\\acad.lsp")
			;;; 2, directories list must exist
			(setq directories 
				'(
					"C:\\WS_Blocks\\Default\\LISP\\Lisp Functions"
					"C:\\WS_Blocks\\Custom\\LISP_Custom\\Lisp Functions"
				)
			)

			;;; 3, foreach loop happens for the directories, returning list of all lisp files found inside, including subfolders
			(foreach directory directories 
				(setq lispFiles
					(append lispFiles 
						(apply 'append
						(mapcar '(lambda ( typ ) (GetAllFiles directory nil typ)) '("*.lsp"))
						)
					)
				)	  
			)
			;;; 4, write file f must open for writing
			(setq writeFile (open writeFile "w"))
		)
	  	;;; now enter if statements
		(progn

		;;; print loop for loading all lisp files found in directories
		(foreach lispFile lispFiles
		(write-line
		  (strcat "(vl-load-all "
		    (vl-prin1-to-string lispFile)
			  "\)"

		  )
		  writeFile
		)
		)
		;;; write line to load acadoptions
		(write-line (strcat "(load \"acadoptions.lsp\")") writeFile)
		;;; write line to call acadoptions
		(write-line "(c:acadoptions)" writeFile)
		;;; write line to load WriteAllLISPFunctionsToACAD.lspFile
		(write-line (strcat "(vl-load-all \"WriteAllLISPToACAD.lsp\")") writeFile)
		(write-line (strcat "(load \"RUNTIMEUPDATE.LSP\")") writeFile)
		(write-line (strcat "(c:runtimeupdate)" writeFile)

		;;; write line to call acadoptions
		;(write-line "(c:WriteAllLISPToACAD.lsp)" writeFile)
		(close writeFile)
		;;; print result to command line
		(princ (strcat "\n<<-- WriteAllLISPtoACAD.lsp created a custom ACAD.lsp file with " (itoa (length lispFiles)) " custom LISP files found in support path -->>"))
		)
	  	;;; error hanlding for when if fails
		(princ "\n*1 of the 4 if conditions failed, open code to verify which one*")
	)
(princ)
)




; --- GetAllFiles ---
; Notes:
; folder structures change often
; so this subfunction, GetAllFiles finds all files in the supplied folder
; before that, it checks for subfolders, with _GetSubFolders function
; each of these functions can handle directories or individual files
; that's the beauty of lambda, the functions continue to be called until the end is reached
; the evaluation is different though
; since the arguments are undefined, the evaluation starts at the bottom most brace to set the argument
; then the lambda starts
; Alex Lundin 06-22-2017
(defun GetAllFiles ( Dir Subs Filetype / _GetSubFolders )
  
  (defun _GetSubFolders ( folder )
    (apply 'append
      (mapcar
        (function
          (lambda ( f )
	    ;;; call _GetSubFolders on f, this will continue until bottom is reached
	    ;;; build list of f values to return to main subfunction, which is sent into Dir variable below
            (cons (setq f (strcat folder "\\" f)) (_GetSubFolders f))
          )
        )
	;;; only get folders (-1) from supplied folder
	;;; evaluation of lambda starts here, set f to folder argument
        (vl-remove ".." (vl-remove "." (vl-directory-files folder nil -1)))
      )
    )
  )
 
  (apply 'append
    (mapcar
      ;;; evaluation starts here on last line, setting Dir to the list of folder plus subfolders, then evaluation lambda functions
      (function
	;;; lambda function to search filepath for folders
        (lambda ( Filepath )
          (mapcar
	    ;;; lambda function to add \\ to end of filename
            (function
              (lambda ( Filename ) (strcat Filepath "\\" Filename))
            )
	    ;;; only get files (1) of supplied filetype from supplied Filepath
            (vl-directory-files Filepath Filetype 1)
          )
        )
      )
      ;;; evaluation starts here, Filepath will be equal to an element of directory, not sure how many elements in directory
      ;;; so lambda function is used
      (cons Dir (_GetSubFolders Dir))
    )
  )
)
