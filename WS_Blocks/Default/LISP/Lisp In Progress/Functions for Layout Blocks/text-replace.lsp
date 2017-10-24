(defun c:txttoblk( / ss)
  (vl-load-com)
  (setq blk "BLOCK") ; Put block name here, with directory if block is not already in drawing
  (setq ss (ssget "X" '((0 . "TEXT")(1 . "F"))))
  (if ss
    (progn
      (setq ss (vl-remove-if 'listp (mapcar 'cadr (ssnamex ss))))
      (mapcar '(lambda (x) (vl-cmdf "-insert" blk (cdr (assoc 10 (entget x))) 1 1 (* (/ 180 pi) (cdr (assoc 50 (entget x)))))
         (entdel x)) ss)
      )
    (princ "\nNo text entities found.")
    )
  (princ)
  )