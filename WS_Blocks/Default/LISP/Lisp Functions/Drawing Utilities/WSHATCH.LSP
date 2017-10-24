; --- WSHATCH Function ---
; Delete all solid hatch in drawing
; Alex Lundin 02-14-2016
(defun c:wshatch ()
  (setq ssHatch(ssget "x" '((0 . "HATCH")(2 . "SOLID"))))								;set ssHatch to the selection set from all hatch objects in the drawing
  (command "_erase" "p" "")												;delete previous selection set
)

; --- WSHATCH2 Function ---
; Delete all solid hatch in drawing
; Alex Lundin 02-14-2016
(defun c:wshatch2 (/ ssHatch dxfHatch layerHatch );ssHatchDel );enHatchItem dxfHatchItem layerHatchItem)
  	(vl-load-com)
	(setq ssHatch(car(entsel "\n Select hatch layer to delete: ")))							;set ssHatch variable to the selection set from all hatch objects that are inserted blocks on the wattstopper layer
	(setq dxfHatch(entget ssHatch))											;set dxfHatch varaible to the dxf codes from the hatch entity selected (ssHatch)
  	(setq layerHatch(cdr(assoc 8 dxfHatch)))									;set layerHatch variable to the second elemenet (cdr function) of the 8th associative property of dxfHatch (8th dxf code, layer name)
  	(princ ssHatchDel)
  	(setq ssHatchDel(ssget "x" '((0 . "HATCH"))))									;set ssHatchDel variable to the selection set of all hatch items in drawing 
	(setq cntr 0)													;set counter to 0
  	(setq cmax(sslength ssHatchDel))										;set counter max to total number of items in ssHatch selection set
  	(while (< cntr cmax)												;while loop, continue until counter is greater than cmax
		(setq enHatchItem (ssname ssHatchDel cntr))								;set enHatchitem to the entity name (ssname function) of the ssHatchDel seleection set item that corresponds to the current value of the counter
		(cond													;conditional block
		  	((/= enHatchItem nil)										;first conditional when enHatchitem is not nil
	  		(setq dxfHatchItem (entget enHatchItem))							;set dxfHatchItem to the dxf codes (entget function) of enHatchItem
	  		(setq layerHatchItem (cdr(assoc 8 dxfHatchItem)))						;set layerHatchItem to the second element (cdr function) of the 8th associative property of dxfHatchItem (8th dxf code, layer name)
	  		(if 	(= layerHatchItem layerHatch)								;if the layer of the hatch item (layerHatchItem variable) matches the layer of the first selected hatch (layerHatch variable) 
		  		(entdel enHatchItem)									;delete entity (enHatchItem)
			)												;end if
			)												;end conditional
		)													;end conditional block
	(setq cntr (+ 1 cntr))												;increment counter at end of loop (moves counter so enHatchItem grabs next item from ssHatchDel set at beginning of loop with ssname function)
	)
  	
)