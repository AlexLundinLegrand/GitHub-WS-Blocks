;;;arguments
;;;adoc 	- vla active document
;;;tags		- list of Attribute Tags to reset to default
(defun c:blk-default (adoc Tags /  obj bname lst ent z)
  
	(Setq lst '())                                                                             	; empty list
  
	(vlax-for block (vla-get-blocks adoc)                                           		; get the block definition
		(if (=   (vla-get-name block) bname)                                          		; check its name
			(vlax-for AttEnt block                                                   
				(IF (=  (vla-get-objectname AttEnt) "AcDbAttributeDefinition")         	; go through the entities pull out attributes
				(progn

				(setq ent (entget ename))

			  	(setq en2(entnext en))
			  	(setq enlist(entget en2))										;set enlist to the group codes of en2
			  	(setq enttype 0)											;initialize entttype to 0 (makes while loop false by default)
			  	(setq attTag 1)												;initialize attTag to 0 (makes condition false by default)
			  	(setq enttype(cdr(assoc 0 enlist)))									;set enttype to the second element of the 0 group code in enlist
				(while 	(= enttype "ATTRIB")										;loop while enttype is "ATTRIB"
					(setq attTag(cdr(assoc 2 enlist)))								;set attTag to the second element of the second associative property (a blocks attribute name) of enlist
					(setq attVal(cdr(assoc 1 enlist)))								;set attVal to the second element of the first associative property (a value associated with the above attribute) of enlist
						(cond 	((/= attTag nil)								;conditional statement only when value is stored in attTag


					  		(setq cntr2 0)
					  		(setq cmax2 (length Tags))
					  		(while 	(< cntr2 cmax2)
							  	(setq Tag (nth cntr2 Tags))

							  	(if
								  	(= Tag attTag)
								  	(progn
									(Setq lst (cons (vla-get-textstring AttEnt ) LST))       	; make a list of the defaults
									(setq cntr2 cmax2)
									)
								)
							  	(setq cntr2 (+ cntr2 1))
							)
							)
						)
					(setq en2(entnext en2))										;get the codes of the next sub entity of the titleblock (this will drill down farther with each loop)
					(setq enlist(entget en2))									;set enlist to the group codes of this new sub entity
					(setq enttype(cdr(assoc 0 enlist)))								;set the enttype to the second member of the first associative value of the group codes (entity type)
																	;loop will repeat until group code is not equal to "ATTRIB"	  			
				)



				) ; progn
				) ; if
			) ; for
		) ; if
	) ; for
  
	(setq lst (reverse lst)) ; need reversed

  
	; now update object
	(setq atts (vlax-invoke obj  'getattributes)) 	 						; get all the attributes
	(setq z 0)
	(repeat (setq y (length atts ))                     						; repeat for how many attributes
		(vla-put-textstring (nth z  atts)  (nth  z lst)) 					; go through attributes and update
		(setq z (+ z 1))
	) ; repeat

) ;defun