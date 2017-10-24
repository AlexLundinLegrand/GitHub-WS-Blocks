;;; --- driver---
;;; Notes:
;;; Demonstrates user entity selection with entsel
;;; Alex Lundin 06-14-2017
(defun 	c:driver ( / ENT LAYERTOMOVETO)

	(setq ent(car(entsel "\nSelect an object: ")))				;set ent to the first element of the list returned by entsel, which allows user to select one object
	(setq layerToMoveTo "0")
  	(entmod-layer ent layerToMoveTo)
)



;;; --- entmod-layer Sub Function ---
;;; change objects layer
;;; Alex Lundin 12-21-2016
(defun entmod-layer	(ent layer / ENTDXF)
;;; Arguments
;;;	ent - entities name passed into ent variable
;;;	layer - passed into layer variable
	(setq entdxf(entget ent))						;set dxf codes of ent to entdxf	
	(setq entdxf(subst (cons 8 layer)(assoc 8 entdxf) entdxf))		;change entdxf's 8th dxfcode value with the subst function
	(entmod entdxf)								;entmod function to modifiy the entity with the new values
)