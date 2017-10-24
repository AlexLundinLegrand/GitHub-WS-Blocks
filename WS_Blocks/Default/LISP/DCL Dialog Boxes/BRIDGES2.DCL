BRIDGES2 : dialog {				//dialog name
      	label = "BRIDGES2" ;			//give it a label
 
       
	: paragraph {				//define paragraph


     	: text_part {
     	label = "";
     	}
        : text_part {
     	label = "___________________________________________Change Notes___________________________________________";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "Please read these prompts, they will help you navigate the changes.";
     	}
     	: text_part {
     	label = "Updated 12-22-2016";
     	}
     	: text_part {
     	label = "";
     	}
        : text_part {
     	label = "____________________________________________BRIDGES2____________________________________________";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "Verify the MSTP wire command is correct. After you press OK:";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "1)";
     	}
     	: text_part {
     	label = "All devices including networking will now be visible.";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "2)";
     	}
     	: text_part {
     	label = "Verify that your spline is not crossing over any devices such as:";
     	}
     	: text_part {
     	label = "Parts   - room controllers, sensors, switches, etc...";
     	}
     	: text_part {
     	label = "Objects - walls, or another spline.";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "3)";
     	}
     	: text_part {
     	label = "Move any MSTP wire that you need to.";
     	}
     	: text_part {
     	label = "Select the spline with a left click, and then use a right click to get sub options for the spline fit points.";
     	}
     	: text_part {
     	label = "From here you can stretch\/add\/remove fit points.";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "4)";
     	}
     	: text_part {
     	label = "After you are done, use the \"BRIDGES3\" command.";
     	}
     	: text_part {
     	label = "";
     	}     		



//     	: text_part {				//define more text
//     	label = "";				//some more text
//     	}


     	}						//end paragraph


        : radio_button {				//define radion button
     	  key = "rb1" ;					//give it a name
     	  label = "Place instructions in drawing" ;	//give it a label
        }						//end definition
 
     	: radio_button {				//define radio button
     	  key = "rb2" ;					//give it a name
     	  label = "Place instructions in text file" ;	//give it a label
     	}						//end definition


     	
	: button {
       	 label = "OK";
	 key = "accept";
             alignment = left;
             width = 5;
             is_default = true;
	}
	
	: button {
       	label = "Cancel";
	key = "cancel";
	mnemonic = "C";
   	alignment = right;
   	width = 5;
	}

	
     }						//end dialog