BRIDGES : dialog {				//dialog name
      	label = "BRIDGES" ;			//give it a label
 
       
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
     	label = "We are creating a more automated process and there are minor changes to mention.";
     	}
     	: text_part {
     	label = "In the past we used corner point osnaps for MSTP wire to connect bridges.";
     	} 
     	: text_part {
     	label = "In an effort to keep manual cable and automated cable consistent, BRIDGES uses center osnaps now.";
     	}
     	: text_part {
     	label = "";
     	}
     	
     	: text_part {
     	label = "____________________________________________BRIDGES_____________________________________________";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "Follow this series through to \"BRIDGES3\".";
     	}
     	: text_part {
     	label = "The codes duplicate product blocks onto the Defpoints layer. They are deleted during \"BRIDGES3\".";
     	}     	
     	: text_part {
     	label = "If you skip the \"BRIDGES3\" step, it will affect the total number of blocks counted in a excel data extraction.";
     	}      	
     	: text_part {
     	label = "This series of commands called BRIDGES will isolate certain networking products to assist in drawing splines.";
     	}
     	: text_part {
     	label = "During the series, the codes will handle all snap settings and layer settings for you.";
     	}
     	: text_part {
     	label = "There are buttons that can place the instructions into the current drawing or to a text file if you prefer.";
     	}
     	: text_part {
     	label = "Feel free to use the MSTPWIRE function with the first BRIDGES command, it will draw cable for you.";
     	}     	
     	: text_part {
     	label = "The MSTPWIRE function is still in development, so you may have to use a combination of manual and automated wiring.";
     	}
     	: text_part {
     	label = "";
     	}
     	
        : text_part {
     	label = "___________________________________________Manual Version___________________________________________";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "1)";
     	}
     	: text_part {
     	label = "Select any objects (such as walls) that you want to see while drawing MSTP.";
     	}
     	: text_part {
     	label = "If you are using a PDF, select the PDF (you will not be able to isolate the walls).";
     	}
     	: text_part {
     	label = "Then press enter.";
     	}
     	: text_part {
     	label = "The layers from the objects you selected as well as any segment managers and network bridges will be the only objects visible.";
     	}
      	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "2)";
     	}
     	: text_part {
     	label = "Start each MSTP wire run at the segment manager block.";
     	}
     	: text_part {
     	label = "Seperate the starting points for each segment, so it's clear they connect to different points in the cabinet";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "3)";
     	}
        : text_part {
     	label = "Draw the first wire going into the bridge, the first wire can only touch one bridge for \"AUTOB\" to work correctly.";
     	}  	
     	: text_part {
     	label = "Use the \"spline\" command to draw your MSTP wire between network devices.";
     	}
     	: text_part {
     	label = "Use several fitpoints across the gap between devices. This makes editing the splines faster later";
     	}
     	: text_part {
     	label = "Connect the final fitpoint of the spline to the center of a network bridge (use F3 to turn osnap on\/off).";
     	}
     	: text_part {
     	label = "Press enter to end the spline.";
     	}
     	: text_part {
     	label = "Press space to start a new spline, continue this new spline from the same center point of the previous network bridge.";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "4)";
     	}
     	: text_part {
     	label = "Repeat steps 2 and 3 until all your segment managers and network bridges are wired.";
     	}
     	: text_part {
     	label = "Follow networking guidelines and best practices. After you are done, use the \"BRIDGES2\" command.";
     	}    
  	

     	: text_part {
     	label = "";
     	}
        : text_part {
     	label = "__________________________________________Automated Version__________________________________________";
     	}
     	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "1)";
     	}
     	: text_part {
     	label = "Select any objects (such as walls) that you want to see while drawing MSTP.";
     	}
     	: text_part {
     	label = "If you are using a PDF, select the PDF (you will not be able to isolate the walls).";
     	}
     	: text_part {
     	label = "Then press enter.";
     	}
     	: text_part {
     	label = "The layers from the objects you selected as well as any segment managers and network bridges will be the only objects visible.";
     	}
      	: text_part {
     	label = "";
     	}
     	: text_part {
     	label = "2)";
     	}
        : text_part {
     	label = "Draw the first wire going into the bridge, the first wire can only touch one bridge for \"AUTOB\" to work correctly.";
     	}      	
     	: text_part {
     	label = "Use MSTPWIRE function.";
     	}
     	: text_part {
     	label = "Select a medium size group of network devices that are near each other to wire together.";
     	}
     	: text_part {
     	label = "Select the first item on the run.";
     	}
     	: text_part {
     	label = "";
     	}  
     	: text_part {
     	label = "3)";
     	}
     	: text_part {
     	label = "Repeat step 2 until all your segment managers and network bridges are wired.";
     	}
     	: text_part {
     	label = "Follow networking guidelines and best practices. After you are done, use the \"BRIDGES2\" command.";
     	}
     	: text_part {
     	label = "";
     	}    
  	
	



//     	: text_part {				//define more text
//     	label = "";				//some more text
//     	}


     	}					//end paragraph


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