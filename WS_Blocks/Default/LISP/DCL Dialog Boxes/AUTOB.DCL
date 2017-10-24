AUTOB : dialog {				//dialog name
      	label = "AUTOB" ;			//give it a label
 
       
	: paragraph {				//define paragraph


// example of how text is formated below
//     	: text_part {				//define more text
//     	label = "";				//some more text
//     	}


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
     	label = "The most noticable change to AUTOB is the customer no longer needs to select all splines.";
     	}
     	: text_part {
     	label = "Just select first spline on the segment and the code handles the rest.";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "_______________________________________________AUTOB_______________________________________________";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "This command will assist in numbering the segment number and positions of your network bridges.";
     	}
     	: text_part {
     	label = "First, use the \"BRIDGES\" series of commands to wire the network devices.";
     	}
     	: text_part {
     	label = "To exit \"AUTOB\" use the \"AUTOB2\" command which will restore layers and drafting settings.";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "After you press OK:";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "1)";
     	}
     	: text_part {
     	label = "AutoCAD will ask you for a segment number and starting position.";
     	}
     	: text_part {
     	label = "Typically segments will be numbered 1 through 10 and then 1A through 10A.";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "2)";
     	}
     	: text_part {
     	label = "Next, AutoCAD will ask you for a starting position number.";
     	}
     	: text_part {
     	label = "This is the position number for the first network bridge in the segment you are working on.";
     	}
     	: text_part {
     	label = "Typically this number will be 1, but if you are working on a segment coming from another page,";
     	}
     	: text_part {
     	label = "you will enter the number that the first bridge is in the segment it is connected to.";
     	}
     	: text_part {
     	label = "For example, if the first bridge on the segment on this page is the 7th bridge on its segment,";
     	}
     	: text_part {
     	label = "enter \"7\" for starting position.";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "3)";
     	}
     	: text_part {
     	label = "Now all MSTP splines and network bridges will be orange.";
     	}
      	: text_part {
     	label = "Verify the first spline of the segment only connects to one network bridge.";
     	}    	
     	: text_part {
     	label = "Select the first spline of the segment you are working on.";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "4)";
     	}
     	: text_part {
     	label = "Now AutoCAD will attempt to cylce through each spline on the segment and fill out the attributes of the network bridge.";
     	}  	
     	: text_part {
     	label = "All sucessful splines will turn blue and the network bridge will dissappear to the WATTSTOPPER layer.";
     	}
     	: text_part {
     	label = "Any orange MSTP wires indicate splines to redraw.";
     	}
     	: text_part {
     	label = "Most likely this is due to the spline not connecting to each other at the corners of each bridge.";
     	}
     	: text_part {
     	label = "Find any orange splines and check that connection.";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "5)";
     	}
     	: text_part {
     	label = "When you are done with a segment, use the \"AUTO2\" command again and repeat steps 1 through 4 above.";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "6)";
     	}
     	: text_part {
     	label = "Finally, use the \"AUTOB2\" command when you are done with all your networking on this layout.";
     	}
     	: text_part {
     	label = "";
     	}


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