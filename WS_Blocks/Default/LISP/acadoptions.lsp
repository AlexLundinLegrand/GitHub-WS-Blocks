;;; --- acadoptions ---
;;; Notes:
;;; this file is run through the acad.lsp
;;; the acad.lsp file is run one time per instance of AutoCAD
;;; so acadoptions is called one time too
;;; this file supports retrospective WS_Blocks folders with the ability to set paths based on them
;;; Alex Lundin 06-22-2017
(defun c:acadoptions ( / *FILES* ISNEWFOLDERSTRUCTURE ISTOOLPALETTEDEVELOPER LST SSFPSRETURN USERNAME)
  
	(vl-load-com)
	;;; create *files* variable from the preferences of the current instance of AutoCAD
	(setq *files* (vla-get-files (vla-get-preferences (vlax-get-acad-object))))							

	;;; always nil the CUI locations, helps for first run after updates.
	;;; set cui locations 
	(vla-put-menufile *files* "")
	(vla-put-enterprisemenufile *files* "")

  
	(setq isNewFolderStructure (findfile "C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Layout Blocks\\Dynamic_Sensors.dwg"))
  	(setq userName (getenv "USERNAME"))

  	(if
	  	(OR
		(= userName "alundi1a")
		)
	  	(progn
		(setq isToolPaletteDeveloper 1)
		)
	)

  	;;; if condition to handle new folder structure as well as previous
  	(if
	  	(/= isNewFolderStructure nil)
	  	(progn
		;;; new folder structure block
		;;; This builds the string of support file search paths for new structure
		(setq lst
		       '(
			"C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Layout Blocks\\Layout Block Definitions"
			"C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Layout Blocks"
			"C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Room Detail Blocks\\Room Detail Block Definitions"
			"C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Room Detail Blocks"
			"C:\\WS_Blocks\\Default\\LISP"
			"C:\\WS_Blocks\\Default\\LISP\\Lisp Functions"
			"C:\\WS_Blocks\\Default\\LISP\\DCL Dialog Boxes"
			"C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Product Overview Blocks"
			"C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Product Overview Blocks\\Product Overview Block Definitions"
			"C:\\WS_Blocks\\Custom\\CUI_Custom"
			"C:\\WS_Blocks\\Custom\\LISP_Custom"
			"C:\\WS_Blocks\\Custom\\ToolPallete\\Palettes"
			"C:\\WS_Blocks\\Default\\LISP\\Lisp Functions\\DLM Riser"
			"C:\\WS_Blocks\\Default\\LISP\\Lisp Functions\\Run Time Updates"
			 
		       )
		)
		;;; set palette location specifically for developers, else set for client
		(if
		  	(= isToolPaletteDeveloper 1)
		  	;;; developer palette location
		  	;;; the first folder shown is where AutoCAD sends the tool palette catalog information
		  	(progn
			(vla-put-ToolPalettePath *files* "C:\\WS_Blocks\\Default\\ToolPallete;C:\\WS_Blocks\\Custom\\ToolPallete;")
			)
		  	;;; else, client location
		  	;;; the first folder shown is where AutoCAD sends the tool palette catalog information
		  	(progn
			(vla-put-ToolPalettePath *files* "C:\\WS_Blocks\\Custom\\ToolPallete;C:\\WS_Blocks\\Default\\ToolPallete;")
			)
		)
		)

	  	(progn
		;;; previous folder structure block
		;;; This builds the string of support file search paths for previous structure
		(setq lst
		       '(
			"C:\\WS_Blocks\\Default\\Palettes\\Layout Blocks\\Layout Block Definitions"
			"C:\\WS_Blocks\\Default\\Palettes\\Layout Blocks"
			"C:\\WS_Blocks\\Default\\Palettes\\Room Detail Blocks\\Room Detail Block Definitions"
			"C:\\WS_Blocks\\Default\\Palettes\\Room Detail Blocks"			 
			"C:\\WS_Blocks\\Default\\LISP"
			"C:\\WS_Blocks\\Default\\LISP\\Lisp Functions"
			"C:\\WS_Blocks\\Default\\LISP\\DCL Dialog Boxes"
			"C:\\WS_Blocks\\Default\\Palettes\\Product Overview Blocks"
			"C:\\WS_Blocks\\Default\\Palettes\\Product Overview Blocks\\Product Overview Block Definitions"
			"C:\\WS_Blocks\\Custom\\CUI_Custom"
			"C:\\WS_Blocks\\Custom\\LISP_Custom"
		       )
		)
		;;; tool palette location is same for developer and client in previous structure
		(vla-put-ToolPalettePath *files* "C:\\WS_Blocks\\Default\\Palettes;C:\\WS_Blocks\\Custom\\Palettes;")
		)
	)	
  	;;; end if condition for handling folder structures

  
  
	;;; This actually applies the above string to the current session of AutoCAD through the sfsp function.
  	;;; use variable to store return value
	(setq ssfpsReturn (set-support-file-search-path lst))

	;;; if newer than AutoCAD 2013 then set trusted paths
	(if (>= (getvar "ACADVER") "19.1")
	  	(progn
		(setvar "trustedpaths"
			"C:\\WS_Blocks\\Default\\LISP;
			C:\\WS_Blocks\\Custom\\LISP_Custom;
			C:\\WS_Blocks\\Default\\LISP\\Lisp Functions;
			C:\\WS_Blocks\\Default\\LISP\\Lisp Functions\\DLM Riser;
			C:\\WS_Blocks\\Default\\LISP\\Lisp Functions\\Drawing Utilities;
			C:\\WS_Blocks\\Default\\LISP\\Lisp Functions\\Extractors;
			C:\\WS_Blocks\\Default\\LISP\\Lisp Functions\\File Reading;
			C:\\WS_Blocks\\Default\\LISP\\Lisp Functions\\Functions for Cable;
			C:\\WS_Blocks\\Default\\LISP\\Lisp Functions\\Functions for Layout Blocks;
			C:\\WS_Blocks\\Default\\LISP\\Lisp Functions\\Room Details;
			C:\\WS_Blocks\\Default\\LISP\\Lisp Functions\\Run Time Updates;
			C:\\WS_Blocks\\Default\\LISP\\Lisp Functions\\Script Creators;
			C:\\WS_Blocks\\Default\\LISP\\Lisp Functions\\Subfunction Library;
			C:\\WS_Blocks\\Default\\LISP\\Lisp Functions\\Subfunction Wrappers;
			C:\\WS_Blocks\\Default\\LISP\\Lisp Functions\\Templates;
			C:\\WS_Blocks\\Default\\LISP\\Lisp Functions\\Titleblocks;
			C:\\WS_Blocks\\Default\\LISP\\Lisp Functions\\Vantage Riser;")
		)
	)
	 
	  
	;;; set template, printer and AutoSave locations
	(vla-put-TemplateDwgPath *files* "C:\\WS_Blocks\\Default\\Templates")
	(vla-put-QNewTemplateFile *files* "C:\\WS_Blocks\\Default\\Templates\\WATTSTOPPER.dwt")
	(vla-put-printerstylesheetpath *files* "C:\\WS_Blocks\\Default\\Plotters\\Plot Styles")
	(vla-put-PrinterConfigPath *files* "C:\\WS_Blocks\\Default\\Plotters")
	(vla-put-autosavepath *files* "C:\\WS_Blocks\\Autosaves")
	  
	;;; set cui locations and load the .cuix files
	;;; first set the main slot with the WSMAIN.cuix file
	(vla-put-menufile *files* "C:\\WS_Blocks\\Custom\\CUI_Custom\\WSMAIN")
	;;; second set the enterprise slot with the WSENTERPRISE.cuix file 
	(vla-put-enterprisemenufile *files* "C:\\WS_Blocks\\Default\\CUI\\WSENTERPRISE")

	 
	  
	;;; release files object
	(vlax-release-object *files*)


  	;;; set variables for AutoCAD session
	(setvar "SAVETIME" 5)
	(setenv "DefaultFormatForSave" "36")
	(setenv "NoStartUpDialog" "1")
	(setvar "rememberfolders" 1)
	(setvar "ISAVEPERCENT" 1)
)



;;; --- set-support-file-search-path ---
;;; Notes:
;;; Accepts list of strings to place in support path
;;; Lambda function to edit string format for AutoCAD options menu
;;; Lambda function to keep existingSupportPaths
;;; Returns final list applied to support path
;;; Alex Lundin 06-22-2017
(defun set-support-file-search-path ( arg-list / x return)
;;; Arguments:
;;;	arg-list	- list of strings for search path, can have single \ or double slashes \\
;;; Return
;;;	original
  
  	;;; third, all arguments are set, so lambda evaluates the if expression
    (   (lambda (existingSupportPaths arg-list )
	  	;;; if expression to remove items from arg-list that are in existingSupportPaths and any paths that don't exists
		(if (setq arg-list
		    (vl-remove-if
		       '(lambda ( x )
		            (or (vl-string-search (strcase x) (strcase existingSupportPaths))
		                (not (findfile x))
		            )
		        )
		        arg-list
		    )
		)
		;;; set environment variable to modified arg-list followed by ; followed by existingSupportPaths
		(setenv "ACAD" (strcat  (apply 'strcat (mapcar '(lambda ( x ) (strcat x ";")) arg-list)) existingSupportPaths ";"))
		)
        )
      	;;; first, lamda function starts evaluation here, setting existingSupportPaths to original support paths followed by ;
        (vl-string-right-trim ";" (getenv "ACAD"))
	;;; second, lambda functions moves here, this line replaces and single slashes \ in arg-list with double \\
        (mapcar '(lambda ( x ) (vl-string-right-trim "\\" (vl-string-translate "\"" "\\" x))) arg-list)
    )

        
	(setq return (getenv "ACAD"))
)