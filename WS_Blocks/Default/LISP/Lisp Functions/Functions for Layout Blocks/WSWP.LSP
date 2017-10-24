; --- WSWP Function ---
; Fixes wipeout draw order
(defun c:WSWP ( / acdoc )
;; Lee Mac 20.06.11
(setq acdoc (vla-get-activedocument (vlax-get-acad-object)))

(vlax-for block (vla-get-blocks acdoc)
(if
(and
(eq :vlax-false (vla-get-islayout block))
(eq :vlax-false (vla-get-isxref block))
)
(
(lambda ( / lst )
(vlax-for obj block
(if (eq "AcDbWipeout" (vla-get-objectname obj))
(setq lst (cons obj lst))
)
)
(if lst
(vla-movetobottom (LM:SortentsTable block)
(vlax-make-variant
(vlax-safearray-fill
(vlax-make-safearray vlax-vbobject (cons 0 (1- (length lst)))) lst
)
)
)
)
)
)
)
)
(vla-regen acdoc acallviewports)
(princ)
)

(defun LM:SortentsTable ( space / dict result ) 
(cond
(
(not
(vl-catch-all-error-p
(setq result
(vl-catch-all-apply 'vla-item
(list (setq dict (vla-GetExtensionDictionary space)) "ACAD_SORTENTS")
)
)
)
)
result
)
( (vla-AddObject dict "ACAD_SORTENTS" "AcDbSortentsTable") )
)
)
(vl-load-com) (princ)
