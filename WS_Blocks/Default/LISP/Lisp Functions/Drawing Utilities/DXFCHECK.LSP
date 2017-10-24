; --- DXFcheck Function ---
; Alex Lundin 03-07-2016
; used to understand dxfcodes while developing
; allows programmer to select a object and then store the dxf codes to the endxf variable
(defun c:dxfcheck( / Entity enDXF)
	(setq Entity (car (entsel "\nSelect entity:")))
  	(setq enDXF (entget Entity))
  	(print enDXF)

)