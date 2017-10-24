WSCOUNT : dialog { 
          label = "WS Count"; 
          : column { 
            : boxed_row { 
              : list_box {
                label ="Qty      Block Name";
                key = "newdata";
                height = 18;
                width = 15;
                multiple_select = false;
                fixed_width_font = true;
                value = "";
              }
             }
            : boxed_column {
              : button {
                key = "accept";
                label = " Export (txt)";
                is_default = false;
              }
              : button {
                key = "cancel";
                label = " Cancel ";
                is_default = true;
                is_cancel = true;
              }
            }
          } 

}
