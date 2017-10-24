; --- wblockm Function ---
; Export all blocks to seperate drawing files in designated location
; Used in combination with defining routine
;http://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/batch-wblock-of-blocks-in-drawing/td-p/789689
; 01-05-2017
(defun c:wblockm ()
(setq cmdecho (getvar "CMDECHO"))
(setvar "CMDECHO" 0)

;;;Use these lines if you'd like to have user set path
;;;(if (not dos_getdir)
;;;(setq path (getstring "\nDS> Target Folder: " T))
;;;(setq path (dos_getdir "Target Folder" (getvar "DWGPREFIX")))
;;;)
  
(setq path "C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Layout Blocks\\Layout Block Definitions")

(if (/= path nil)
(progn
(if (= (substr path (strlen path) 1) "\\")
(setq path (substr path 1 (1- (strlen path))))
) 
(princ "\nDS> Building List of Blocks ... ")
(setq lst nil)
(setq itm (tblnext "BLOCK" T))
(while (/= itm nil)
(setq nam (cdr (assoc 2 itm)))
(setq pass T)
(if (/= (cdr (assoc 1 itm)) nil)
(setq pass nil)
(progn
(setq ctr 1)
(repeat (strlen nam)
(setq chk (substr nam ctr 1))
(if (or (= chk "*")(= chk "|"))
(setq pass nil)
)
(setq ctr (1+ ctr))
)
)
)
(if (= pass T)
(setq lst (cons nam lst))
)
(setq itm (tblnext "BLOCK"))
)
(setq lst (acad_strlsort lst))
(princ "Done.")
;
(foreach blk lst
(setq fn (strcat path (chr 92) blk))
(if (findfile (strcat fn ".dwg"))
(command "_.WBLOCK" fn "_Y" blk)
(command "_.WBLOCK" fn blk)
)
)
)
)
;
(setvar "CMDECHO" cmdecho)
(princ)
)															;END WBLOCKM




; --- upbs Function ---
;;;http://forums.autodesk.com/t5/visual-lisp-autolisp-and-general/update-blocks-amp-attributes-lisp/td-p/4399069
;;: Redefine All Blocks - Lee Mac
; 01-05-2017
(defun c:upbs ( / bln dir doc dwg lst obj org spc )
(setq dir "C:\\WS_Blocks\\Default\\ToolPallete\\Palettes\\Layout Blocks\\Layout Block Definitions") ;; Directory of Block Library; nil to use Support Path
(if dir
(setq dir (strcat (vl-string-right-trim "\\" (vl-string-translate "/" "\\" dir)) "\\"))
(setq dir "")
)
(cond
( (= 4 (logand 4 (cdr (assoc 70 (tblsearch "layer" (getvar 'clayer))))))
(princ "\nCurrent layer locked.")
)
( (setq doc (vla-get-activedocument (vlax-get-acad-object))
spc (vla-get-modelspace doc)
org (vlax-3D-point 0 0)
)
(terpri)
(vlax-for blk (vla-get-blocks doc)
(if
(and
(= :vlax-false (vla-get-isxref blk))
(= :vlax-false (vla-get-islayout blk))
(not (wcmatch (setq bln (vla-get-name blk)) "`**,*|*"))
)
(if (setq dwg (findfile (strcat dir bln ".dwg")))
(progn
(princ (strcat "Redefining block: " dwg "\n"))
(setq obj (vla-insertblock spc org dwg 1.0 1.0 1.0 0.0))
(if (= :vlax-true (vla-get-hasattributes obj))
(setq lst (vl-list* "," bln lst))
)
(vla-delete obj)
)
(princ (strcat "Unable to locate block: " dir bln ".dwg\n"))
)
)
)

(textscr)
(vla-regen doc acallviewports)
)
)
(c:globalattsync)
(princ)
)											;END UPBS
(vl-load-com) (princ)