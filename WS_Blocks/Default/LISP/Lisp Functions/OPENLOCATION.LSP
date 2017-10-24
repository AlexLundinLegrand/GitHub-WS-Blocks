; --- openLocation ---
; Notes:
; Opens a folder or file location from one of the stubs
; 08-18-2017
; Alex Lundin 
(defun openLocation
       		(
		;arguments
		theAppToUseFromStub theLocationFromStub
		/
		;local variables

		)
;;; Arguments:
;;;	1
;;;	2

	(startapp theAppToUseFromStub theLocationFromStub)
  	(princ)
)

; --- c:VRULES ---
; Notes:
; Opens rules for VANTAGE RISER
; 08-18-2017
; Alex Lundin 
(defun c:VRULES
       		(
		;arguments
		/
		;local variables
		theAppToUseInStub theLocationInStub
		)
	(setq theAppToUseInStub "notepad")
	(setq theLocationInStub "C:\\WS_Blocks\\Default\\LISP\\Lisp Functions\\Vantage Riser\\VRULES.txt")
	(openLocation theAppToUseInStub theLocationInStub)
  	(princ)
		
)

; --- c:STW ---
; Notes:
; Opens folder location of Standard Work
; 08-18-2017
; Alex Lundin 
(defun c:STW
       		(
		;arguments
		/
		;local variables
		theAppToUseInStub theLocationInStub
		)

	(setq theAppToUseInStub "explorer")
	(setq theLocationInStub "C:\\WS_Blocks\\Default\\CAD Documentation\\Standard Work\\")
	(openLocation theAppToUseInStub theLocationInStub)
  	(princ)	
)

