;; main program
(defun c:test (/ adoc acsp address data_list fname mtext_obj pname pt pt_str sheetnum)
  (setq adoc (vla-get-activedocument
        (vlax-get-acad-object)
      )
  )
  (if (= 1 (vlax-get-property adoc 'activespace))
    (setq acsp (vla-get-modelspace adoc))
    (setq acsp (vla-get-paperspace adoc))
  )
  ;; points range address
  (setq address "A1:C100") ;<--3 columns (if tab has headers the you have to starts from A2 to avoid first row with headers)
  ;; sheet number
  (setq sheetnum 1);<-- you can use instead a sheet name as well: (setq sheetnum "Sheet1")
 
 
 (if 
 ;(setq fname (getfiled "Select Excel File:" (getvar "dwgprefix") "XLSX;XLS" 4))
 
 ;; Or use hard coded path for your test (change path before):
 (setq fname "C:\\WS_Blocks\\Default\\LISP\\Lisp Functions\\Functions for Layout Blocks\\Incorrect Part Names.xlsx")   
  (progn
(setq data_list (read_excel_by_cell_range fname sheetnum address))
 ;; display result (for debug only)
 ;;(alert (apply 'strcat (mapcar '(lambda (a)(strcat a "\n"))  (mapcar 'vl-princ-to-string data_list))))
(foreach line data_list
  ;; assume the first row is for point name 
  	(setq pname (nth 0 line))
	(setq pname1 (nth 1 line))
  	(setq pname2 (nth 2 line))
 )
)
   )
  (princ)
  )
(prompt "\n\t\t>>>\tType \"CVIMP\" or \"cvimp\" to execute\t<<<")
(prin1)
;;===============================================================;;