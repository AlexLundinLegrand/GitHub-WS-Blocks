;;; --- MIRRORALLBLOCKS Function ---
;;; Alex Lundin 03-15-2017
;;; This function is designed to mirror all blocks in the drawing to flip any text inside the block back to default orientation
(defun C:MIRRORALLBLOCKS ()
	(vl-load-com)


	(setq blocks (ssget "x" (list(cons 410 "MODEL")(cons 0 "INSERT")(cons 8 "WATTSTOPPER"))))						;create selection set from all objects that are inserted LMBC-300 blocks on the WATTSTOPPER layer
	(setq cntr 0)													;initialize counter to 0 for while loop
  	(setq cmax (sslength blocks))											;set cmax to length of blocks selection set
  
	(while 	(< cntr cmax)												;set loop to continue while the counter varaible cntr is less that the number of objects in the selection set blocks
		(setq en(ssname blocks cntr))										;set variable en to the name of the block that matches the current counter value
		(setq enDXF(entget en))											;set the varaible enlist to the list of entities from the en varaible
		(setq ename (cdr (assoc -1 enDXF )))
		(setq entityList (cons ename entityList))
	  
		(setq insertionpoint (cdr (assoc 10 enDXF)))								;get blocks insertion point through 
	  	(setq insertionpoint-x (car insertionpoint))
	  	(setq insertionpoint-y (cadr insertionpoint))
		(setq p1 (list insertionpoint-x insertionpoint-y 0.0))							;set p1

	  	(setq insertionpoint-y2 (+ insertionpoint-y 150.0))
		(setq p2 (list insertionpoint-x insertionpoint-y2 0.0))							;set p2

	  	(setq en-as-vlaobject (vlax-ename->vla-object en))

	  	(COMMAND "._MIRROR" en "" p1 p2 "Y")
	  
		(setq cntr(+ cntr 1))											;once while loop reaches this point, increment counter varaible by one
  	)														;end while loop


)


