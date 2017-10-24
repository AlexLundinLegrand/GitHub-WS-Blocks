;;;CUSTOM-INSERT-SHORTCUTS functions


; --- CIS Stub Function---
; Notes:
; Use one of the CIS:# functions to call this one and pass the correct arguemnts in
; Saves command echo information to restore after command
; Sets command echo to 0, to suppress command line
; Creates a full block name from drawing location and block name
; Inserts block with command line
; Restores command echo after command
; Arguments:
; PATHLOCATION
; BLOCKNAME
; XSCALE
; YSCALE
; ROTATION
; POINT
; 
; Alex Lundin 07-19-2017
(defun CIS
		(
		CIS-BLOCKNAME CIS-PATHLOCATION CIS-POINT CIS-ROTATION CIS-XSCALE CIS-YSCALE
		/
		FULLBLOCKNAME OLDECHO
		)

	;save command echo variable and set to 0 after
  	(setq oldEcho (getvar "cmdecho"))
	(setvar "cmdecho" 0)
	(setvar "CLAYER" "WATTSTOPPER")
	(setq fullBlockName (strcat CIS-pathLocation CIS-blockName))							;create entire string from pieces of info
	
  	(command "._insert" fullBlockName CIS-point CIS-xscale CIS-yscale CIS-rotation)					;use command line to insert block

  	;restore command echo
	(setvar "cmdecho" oldEcho)
	(princ)
)



; --- C:CIS ---
; Notes:
; Driver function for CIS
; Sets values and passes them into CIS as arguements
; Arguments: None
; 
; Alex Lundin 06-05-2017
(defun C:CIS0
		(

		/
		FULLBLOCKNAME OLDECHO
		)

  	(setq BLOCKNAME "LMSM-3E")
	(setq PATHLOCATION "C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Layout Blocks\\Layout Block Definitions\\")
	(setq ROTATION "0.00")
	(setq XSCALE "1")
	(setq YSCALE "1")	       
	(setq point (getpoint "Specify insertion point:"))								;let user enter point
  	(CIS BLOCKNAME PATHLOCATION POINT ROTATION XSCALE YSCALE)
)



; --- C:CIS ---
; Notes:
; Driver function for CIS
; Sets values and passes them into CIS as arguements
; Arguments: None
; 
; Alex Lundin 06-05-2017
(defun C:CIS1
		(

		/
		FULLBLOCKNAME OLDECHO
		)

  	(setq BLOCKNAME "LMBC-300")
	(setq PATHLOCATION "C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Layout Blocks\\Layout Block Definitions\\")
	(setq ROTATION "0.00")
	(setq XSCALE "1")
	(setq YSCALE "1")	       
	(setq point (getpoint "Specify insertion point:"))								;let user enter point
  	(CIS BLOCKNAME PATHLOCATION POINT ROTATION XSCALE YSCALE)
)

; --- C:CIS ---
; Notes:
; Driver function for CIS
; Sets values and passes them into CIS as arguements
; Arguments: None
; 
; Alex Lundin 06-05-2017
(defun C:CIS2
		(

		/
		FULLBLOCKNAME OLDECHO
		)

  	(setq BLOCKNAME "LMRC-213")
	(setq PATHLOCATION "C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Layout Blocks\\Layout Block Definitions\\")
	(setq ROTATION "0.00")
	(setq XSCALE "1")
	(setq YSCALE "1")	       
	(setq point (getpoint "Specify insertion point:"))								;let user enter point
  	(CIS BLOCKNAME PATHLOCATION POINT ROTATION XSCALE YSCALE)
)


; --- C:CIS ---
; Notes:
; Driver function for CIS
; Sets values and passes them into CIS as arguements
; Arguments: None
; 
; Alex Lundin 06-05-2017
(defun C:CIS3
		(

		/
		FULLBLOCKNAME OLDECHO
		)

  	(setq BLOCKNAME "LMRC-212")
	(setq PATHLOCATION "C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Layout Blocks\\Layout Block Definitions\\")
	(setq ROTATION "0.00")
	(setq XSCALE "1")
	(setq YSCALE "1")	       
	(setq point (getpoint "Specify insertion point:"))								;let user enter point
  	(CIS BLOCKNAME PATHLOCATION POINT ROTATION XSCALE YSCALE)
)


; --- C:CIS ---
; Notes:
; Driver function for CIS
; Sets values and passes them into CIS as arguements
; Arguments: None
; 
; Alex Lundin 06-05-2017
(defun C:CIS4
		(

		/
		FULLBLOCKNAME OLDECHO
		)

  	(setq BLOCKNAME "LMRC-211")
	(setq PATHLOCATION "C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Layout Blocks\\Layout Block Definitions\\")
	(setq ROTATION "0.00")
	(setq XSCALE "1")
	(setq YSCALE "1")	       
	(setq point (getpoint "Specify insertion point:"))								;let user enter point
  	(CIS BLOCKNAME PATHLOCATION POINT ROTATION XSCALE YSCALE)
)



; --- C:CIS ---
; Notes:
; Driver function for CIS
; Sets values and passes them into CIS as arguements
; Arguments: None
; 
; Alex Lundin 06-05-2017
(defun C:CIS5
		(

		/
		FULLBLOCKNAME OLDECHO
		)

  	(setq BLOCKNAME "LMPL-201")
	(setq PATHLOCATION "C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Layout Blocks\\Layout Block Definitions\\")
	(setq ROTATION "0.00")
	(setq XSCALE "1")
	(setq YSCALE "1")	       
	(setq point (getpoint "Specify insertion point:"))								;let user enter point
  	(CIS BLOCKNAME PATHLOCATION POINT ROTATION XSCALE YSCALE)
)


; --- C:CIS ---
; Notes:
; Driver function for CIS
; Sets values and passes them into CIS as arguements
; Arguments: None
; 
; Alex Lundin 06-05-2017
(defun C:CIS6
		(

		/
		FULLBLOCKNAME OLDECHO
		)

  	(setq BLOCKNAME "LMPL-101")
	(setq PATHLOCATION "C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Layout Blocks\\Layout Block Definitions\\")
	(setq ROTATION "0.00")
	(setq XSCALE "1")
	(setq YSCALE "1")	       
	(setq point (getpoint "Specify insertion point:"))								;let user enter point
  	(CIS BLOCKNAME PATHLOCATION POINT ROTATION XSCALE YSCALE)
)

; --- C:CIS ---
; Notes:
; Driver function for CIS
; Sets values and passes them into CIS as arguements
; Arguments: None
; 
; Alex Lundin 06-05-2017
(defun C:CIS7
		(

		/
		FULLBLOCKNAME OLDECHO
		)

  	(setq BLOCKNAME "ELCU-200")
	(setq PATHLOCATION "C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Layout Blocks\\Layout Block Definitions\\")
	(setq ROTATION "0.00")
	(setq XSCALE "1")
	(setq YSCALE "1")	       
	(setq point (getpoint "Specify insertion point:"))								;let user enter point
  	(CIS BLOCKNAME PATHLOCATION POINT ROTATION XSCALE YSCALE)
)


; --- C:CIS ---
; Notes:
; Driver function for CIS
; Sets values and passes them into CIS as arguements
; Arguments: None
; 
; Alex Lundin 06-05-2017
(defun C:CIS8
		(

		/
		FULLBLOCKNAME OLDECHO
		)

  	(setq BLOCKNAME "LMDC-100")
	(setq PATHLOCATION "C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Layout Blocks\\Layout Block Definitions\\")
	(setq ROTATION "0.00")
	(setq XSCALE "1")
	(setq YSCALE "1")	       
	(setq point (getpoint "Specify insertion point:"))								;let user enter point
  	(CIS BLOCKNAME PATHLOCATION POINT ROTATION XSCALE YSCALE)
)


; --- C:CIS ---
; Notes:
; Driver function for CIS
; Sets values and passes them into CIS as arguements
; Arguments: None
; 
; Alex Lundin 06-05-2017
(defun C:CIS9
		(

		/
		FULLBLOCKNAME OLDECHO
		)

  	(setq BLOCKNAME "LMPC-100")
	(setq PATHLOCATION "C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Layout Blocks\\Layout Block Definitions\\")
	(setq ROTATION "0.00")
	(setq XSCALE "1")
	(setq YSCALE "1")	       
	(setq point (getpoint "Specify insertion point:"))								;let user enter point
  	(CIS BLOCKNAME PATHLOCATION POINT ROTATION XSCALE YSCALE)
)


; --- C:CIS ---
; Notes:
; Driver function for CIS
; Sets values and passes them into CIS as arguements
; Arguments: None
; 
; Alex Lundin 06-05-2017
(defun C:CIS10
		(

		/
		FULLBLOCKNAME OLDECHO
		)

  	(setq BLOCKNAME "LMDX-100")
	(setq PATHLOCATION "C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Layout Blocks\\Layout Block Definitions\\")
	(setq ROTATION "0.00")
	(setq XSCALE "1")
	(setq YSCALE "1")	       
	(setq point (getpoint "Specify insertion point:"))								;let user enter point
  	(CIS BLOCKNAME PATHLOCATION POINT ROTATION XSCALE YSCALE)
)


; --- C:CIS ---
; Notes:
; Driver function for CIS
; Sets values and passes them into CIS as arguements
; Arguments: None
; 
; Alex Lundin 06-05-2017
(defun C:CIS11
		(

		/
		FULLBLOCKNAME OLDECHO
		)

  	(setq BLOCKNAME "LMPX-100")
	(setq PATHLOCATION "C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Layout Blocks\\Layout Block Definitions\\")
	(setq ROTATION "0.00")
	(setq XSCALE "1")
	(setq YSCALE "1")	       
	(setq point (getpoint "Specify insertion point:"))								;let user enter point
  	(CIS BLOCKNAME PATHLOCATION POINT ROTATION XSCALE YSCALE)
)


; --- C:CIS ---
; Notes:
; Driver function for CIS
; Sets values and passes them into CIS as arguements
; Arguments: None
; 
; Alex Lundin 06-05-2017
(defun C:CIS12
		(

		/
		FULLBLOCKNAME OLDECHO
		)

  	(setq BLOCKNAME "LMDM-101")
	(setq PATHLOCATION "C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Layout Blocks\\Layout Block Definitions\\")
	(setq ROTATION "0.00")
	(setq XSCALE "1")
	(setq YSCALE "1")	       
	(setq point (getpoint "Specify insertion point:"))								;let user enter point
  	(CIS BLOCKNAME PATHLOCATION POINT ROTATION XSCALE YSCALE)
)


; --- C:CIS ---
; Notes:
; Driver function for CIS
; Sets values and passes them into CIS as arguements
; Arguments: None
; 
; Alex Lundin 06-05-2017
(defun C:CIS13
		(

		/
		FULLBLOCKNAME OLDECHO
		)

  	(setq BLOCKNAME "LMSW-101")
	(setq PATHLOCATION "C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Layout Blocks\\Layout Block Definitions\\")
	(setq ROTATION "0.00")
	(setq XSCALE "1")
	(setq YSCALE "1")	       
	(setq point (getpoint "Specify insertion point:"))								;let user enter point
  	(CIS BLOCKNAME PATHLOCATION POINT ROTATION XSCALE YSCALE)
)

; --- C:CIS ---
; Notes:
; Driver function for CIS
; Sets values and passes them into CIS as arguements
; Arguments: None
; 
; Alex Lundin 06-05-2017
(defun C:CIS14
		(

		/
		FULLBLOCKNAME OLDECHO
		)

  	(setq BLOCKNAME "LMSW-102")
	(setq PATHLOCATION "C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Layout Blocks\\Layout Block Definitions\\")
	(setq ROTATION "0.00")
	(setq XSCALE "1")
	(setq YSCALE "1")	       
	(setq point (getpoint "Specify insertion point:"))								;let user enter point
  	(CIS BLOCKNAME PATHLOCATION POINT ROTATION XSCALE YSCALE)
)


; --- C:CIS ---
; Notes:
; Driver function for CIS
; Sets values and passes them into CIS as arguements
; Arguments: None
; 
; Alex Lundin 06-05-2017
(defun C:CIS15
		(

		/
		FULLBLOCKNAME OLDECHO
		)

  	(setq BLOCKNAME "LMSW-105")
	(setq PATHLOCATION "C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Layout Blocks\\Layout Block Definitions\\")
	(setq ROTATION "0.00")
	(setq XSCALE "1")
	(setq YSCALE "1")	       
	(setq point (getpoint "Specify insertion point:"))								;let user enter point
  	(CIS BLOCKNAME PATHLOCATION POINT ROTATION XSCALE YSCALE)
)


; --- C:CIS ---
; Notes:
; Driver function for CIS
; Sets values and passes them into CIS as arguements
; Arguments: None
; 
; Alex Lundin 06-05-2017
(defun C:CIS16
		(

		/
		FULLBLOCKNAME OLDECHO
		)

  	(setq BLOCKNAME "LMDW-102")
	(setq PATHLOCATION "C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Layout Blocks\\Layout Block Definitions\\")
	(setq ROTATION "0.00")
	(setq XSCALE "1")
	(setq YSCALE "1")	       
	(setq point (getpoint "Specify insertion point:"))								;let user enter point
  	(CIS BLOCKNAME PATHLOCATION POINT ROTATION XSCALE YSCALE)
)


; --- C:CIS ---
; Notes:
; Driver function for CIS
; Sets values and passes them into CIS as arguements
; Arguments: None
; 
; Alex Lundin 06-05-2017
(defun C:CIS17
		(

		/
		FULLBLOCKNAME OLDECHO
		)

  	(setq BLOCKNAME "LMPW-102")
	(setq PATHLOCATION "C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Layout Blocks\\Layout Block Definitions\\")
	(setq ROTATION "0.00")
	(setq XSCALE "1")
	(setq YSCALE "1")	       
	(setq point (getpoint "Specify insertion point:"))								;let user enter point
  	(CIS BLOCKNAME PATHLOCATION POINT ROTATION XSCALE YSCALE)
)