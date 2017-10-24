; --- WSCPL Function ---
; Copy all items from certain layers
; Alex Lundin 11-06-2016
(defun c:wscpl()
	(setq blocks(ssget "x" '((8 . "WATTSTOPPER"))))								;selection all blocks from WATTSTOPPER LAYER
  	(setq cable(ssget "x" '((8 . "_WS_MSTP"))))								;same as above for other layers
  	(setq ohm(ssget "x" '((8 . "_WS_120_OHM"))))								;AutoCAD lisp does not use brackets, so anything in brackets below must be replaced
	(setq cable2(ssget "x" '((8 . "_WS_CAT5E"))))								;below the [setname] is a varaible name of your choosing, and [layername] is the layer you want to copy to the set.
  	(setq cable3(ssget "x" '((8 . "_WS_CAT5E_Extra"))))
  	(setq matchline(ssget "x" '((8 . "WATTSTOPPER MATCHLINE"))))						;to add new line do this (setq [setname] (ssget "x" '((8 . "[layername]"))))
	(setq roomid(ssget "x" '((8 . "WS-ROOM-ID"))))
	(setq noid(ssget "x" '((8 . "NOID"))))
  	(setq bridges(ssget "x" '((8 . "BRIDGES"))))
  	(setq total (acet-ss-union (list blocks cable ohm cable2 cable3 matchline roomid noid bridges)))	;add the new [setname] to this line (setq total (acet-ss-union (list blocks cable ohm matchline [setname])))	
  	(command "copybase" "\\" total "")
  
)





; --- WSCPLCUSTOM Function ---
; Copy all items from certain layers
; Alex Lundin 01-06-2016
(defun c:wscplcustom()
  	(setq custom(ssget "x" '((8 . "RN_LAYER"))))								;to add new line do this (setq [setname] (ssget "x" '((8 . "[layername]"))))
  	(setq total (acet-ss-union (list custom)))								;add the new [setname] to this line (setq total (acet-ss-union (list blocks cable ohm matchline [setname])))	
  	(command "copybase" "\\" total "")
  
)






; --- PCLIP Function ---
; Paste all items from clipboard
; Alex Lundin 01-06-2016
(defun c:pclip ()
  	(setvar "ctab" "Model")
	(command "pasteclip" "0,0")
	(princ)
)
