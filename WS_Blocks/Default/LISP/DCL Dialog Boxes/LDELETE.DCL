LDELETE : dialog { 
          label = "LEGEND DELETER DIALOG BOX"; 
          : column {
          label = "Missing Key Blocks - Result of a out dated legend";
            : boxed_row {
            label = "Click purge to attempt to purge this paper space tab";
              : list_box {
                label ="Qty      Block Name";
                key = "keydata";
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
                label = " Purge";
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



       	   
          : column {
          label = "Existing Legends - Result of running WSLEGENDCOUNT from paper space possibly";
            : boxed_row {
            label = "Click delete/purge to search and destroy all blocks called Legend"; 
              : list_box {
                label ="Qty      Block Name";
                key = "legenddata";
                height = 18;
                width = 15;
                multiple_select = false;
                fixed_width_font = true;
                value = "";
              }
             }
            : boxed_column {
              : button {
                key = "accept2";
                label = " Delete/Purge";
                is_default = false;
              }
              : button {
                key = "cancel2";
                label = " Cancel ";
                is_default = true;
                is_cancel = true;
              }
            }
          }

}
