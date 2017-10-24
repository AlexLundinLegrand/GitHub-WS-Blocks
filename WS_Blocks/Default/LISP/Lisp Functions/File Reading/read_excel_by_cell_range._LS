(vl-load-com); load ActiveX dynamic library
 
;;local defuns:
;; subfunction: read diapazone of cells by given file name, sheet number and CellAddressRange of range
;; usage: (setq data_list (read_excel_range "C:\\Programming\\FORUMS\\Cadtutor\\excel_test2007.xlsx" 1 "A1:C10000"))
(defun read_excel_by_cell_range (FilePath ExcelSheetNumber CellAddressRange / ExcelApp ExcelDataHolder ExcelCurrentSheet vla-Object-CellRange ExcelWorkBook)
(vl-load-com)
(setq ExcelApp (vlax-get-or-create-object "Excel.Application"))
(vla-put-visible ExcelApp :vlax-true)
(vlax-put-property ExcelApp 'DisplayAlerts :vlax-true)
(setq ExcelWorkBook (vl-catch-all-apply
     'vla-open
     (list (vlax-get-property ExcelApp "WorkBooks") FilePath)
     )
      )
(setq ExcelCurrentSheet (vl-catch-all-apply
     'vlax-get-property 
     (list (vlax-get-property ExcelWorkBook "Sheets")
    "Item"
    ExcelSheetNumber
    )
     )
      )
(vlax-invoke-method ExcelCurrentSheet "Activate")
(setq vla-Object-CellRange (vlax-get-property
   (vlax-get-property ExcelCurrentSheet 'Cells)
   "Range"
   CellAddressRange)
)
   
(setq ExcelDataHolder
       	(vlax-safearray->list
		(vlax-variant-value
		(vlax-get-property vla-Object-CellRange 'Value2)
		)
	)
)
	  
	  
(setq
  ExcelDataHolder (mapcar
     (function (lambda (x) (mapcar 'vlax-variant-value x)))
     ExcelDataHolder
     )
  )
(setq
  ExcelDataHolder (vl-remove-if
     (function (lambda (x) (member nil x)))
     ExcelDataHolder
     )
  )
(vl-catch-all-apply
  'vlax-invoke-method
  (list ExcelWorkBook "Close" :vlax-false)
  )
  (gc)
(vl-catch-all-apply
  'vlax-invoke-method
  (list ExcelApp "Quit")
  )
(mapcar
  (function
    (lambda (x)
      (vl-catch-all-apply
 (function (lambda ()
      (progn
        (if (not (vlax-object-released-p x))
   (progn
     (vlax-release-object x)
     (setq x nil)
     )
   )
        )
      )
    )
 )
      )
    )
  (list vla-Object-CellRange ExcelCurrentSheet ExcelWorkBook ExcelApp)
  )
  (gc)
  ExcelDataHolder
)