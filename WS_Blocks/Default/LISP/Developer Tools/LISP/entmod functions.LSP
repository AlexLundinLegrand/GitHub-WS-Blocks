; --- entmod-layer Sub Function ---
; change objects layer
; Arguments
; entities name passed into ent variable
; layer passed into layer variable
; Alex Lundin 12-21-2016

(defun entmod-layer	(ent layer / )

	(setq entdxf(entget ent))						;set dxf codes of ent to entdxf				
	(setq entdxf(subst (cons 8 layer)(assoc 8 entdxf) entdxf))		;change dxfcodes on entdxf
	(entmod entdxf)								;entmod function to modifiy the entity with the new values
)