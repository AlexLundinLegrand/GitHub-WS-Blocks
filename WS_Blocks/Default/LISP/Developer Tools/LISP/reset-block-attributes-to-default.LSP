;;;arguments
;;;adoc - vla active document
;;;obj	- ename of block
(defun c:blk-default ( / adoc obj bname lst ent z)
  
	(setq adoc (vla-get-activedocument (vlax-get-acad-object)))        				; open up database
	(setq obj  (vlax-ename->vla-object (car (entsel "pick a block")))) 				; get block
	(setq bname (vla-get-name obj))                                               			; get block name


  
	(Setq lst '())                                                                             	; empty list
  
	(vlax-for block (vla-get-blocks adoc)                                           		; get the block definition
		(if (=   (vla-get-name block) bname)                                          		; check its name
			(vlax-for ent block                                                   
				(IF (=  (vla-get-objectname ent ) "AcDbAttributeDefinition")         	; go through the entities pull out attributes
				(Setq lst (cons (vla-get-textstring ent ) LST))                        	; make a list of the defaults
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