; --- BG Function ---
; This function toggle modelspace background colors between black and white. 
; Chan Hem 3-22-2017


(defun c:bg ( / col )
    (if bg:flg
        (setq col 16777215 bg:flg nil)	;sets to white and toggles flag
        (setq col 0        bg:flg  t )	;sets to black and toggles flag
    )
    (foreach prp '(graphicswinmodelbackgrndcolor modelcrosshaircolor) ;toggles crosshair color
        (vlax-put-property (acdisp) prp (setq col (- 16777215 col))) 
    )
    (princ)
)
(defun acdisp nil
    (eval
        (list 'defun 'acdisp 'nil
            (vla-get-display (vla-get-preferences (vlax-get-acad-object)))
        )
    )
    (acdisp)
)
(vl-load-com) (princ)