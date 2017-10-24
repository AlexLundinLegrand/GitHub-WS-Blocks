; --- wspagesetup Helper Function ---
; Bring in page setups from WATTSTOPPER.dwt
; Alex Lundin 07-13-2016

(defun c:wspagesetup ()
(setvar "cmdecho" 0)
(command "filedia" "0")
(command "._-PSETUPIN" "C:/WS_Blocks/Templates/WATTSTOPPER.dwt" "*")

(defun *error* (msg)
    	(setvar "cmdecho" 1)
	(command "y" "")
    	(command "y" "")
    	(command "y" "")
    	(command "y" "")
    	(command "y" "")
    	(command "y" "")
  	(command "-dwgunits" "" "" "" "Y" "Y")
	(command "filedia" "1")
    	(princ)
) ;_ end of defun  	
  
(setvar "cmdecho" 0)
(command "-dwgunits" "" "" "" "Y" "Y")
(command "filedia" "1")
(princ)
)