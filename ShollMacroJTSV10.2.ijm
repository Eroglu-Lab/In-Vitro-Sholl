/*
author Justin Savage 
js664@duke.edu
Version 10.2
3/4/20

A simple ImageJ macro for batch processing sholl analysis
The macro first asks for a source directory which should be
a file that contains the image folders for analysis of
one condition with the Overlay image as the last image in the folder. 
Files are then moved into new directories, ZProjected and 
converted to RGB and split to analyze the second channel 
(usually green). The next user input asks or the user to 
set the threshold and create a point on the nucleus of 
the cell. Other edits to the image are allowed while the 
text box is displayed as long as the proper threshold and 
nucleus location are selected when the user clicks OK. 
This proceeds for each image and then the group sholl 
profiles script is ran and the merged profile displayed. 
Simply close this box and select the desired save location. 

The macro does make a few important assumptions about the input
image that may need to changed for certain applications:
1. the input image has at least 2 channels and the one to be 
analyzed is the second one (usually green)
2. the scale of the image is 6.18 pixels/um
3. the maximum radius is 150 um from the nucleus

The macro also used the bio-formats importer to open images so that 
plugin must be installed

Visit https://imagej.net/Sholl_Analysis#Batch_Processing for more
info on that plugin including some coding help that may be useful
for editing this macro
*/

//Asks user for scale of images
Dialog.create("What is the image scale?")
Dialog.addNumber("scale: ", 2.6494, 4, 8, "pixels/um");
Dialog.show();
scale = Dialog.getNumber();

//Asks user for source directory
dir1  = getDirectory("Choose Source Directory ");
list1  = getFileList(dir1);

//Creates new file for merged images within source directory 
//and copies the images into this file

dir2 = dir1+"newMerged"+File.separator;
File.makeDirectory(dir2);
print("made newMerged folder");
for(i=0; i<list1.length; i++) {
	file1 = list1[i];
	print("file1: " + file1);
	list2 = getFileList(dir1+file1);
	print(list2.length);
	print("i: " + i);
	for(j=0; j<list2.length; j++){
		file2 = list2[j];
		print("j: " + j);
		//file2Name = File.getName(file2);
		if(j == (list2.length - 1)){
			print("ready to rename");
			File.copy(dir1+file1+file2, dir2+file2);
		}
	}
}



//Creates a new file for the ZProjected images and runs the 
//ZProjection and RGB conversion
dirZ = dir1+"newZProjected"+File.separator; 
File.makeDirectory(dirZ);
ZProjecter(dir2,dirZ);

//Makes new folder for green channel only images and runs the SplitChannels
//function
dir3 = dir1+"newGreen"+File.separator;
File.makeDirectory(dir3);
list1  = getFileList(dir2);
for (i = 0; i < list1.length; i++) {
	print(list1[i]);
	SplitChannels(dir2, dir3, list1[i]);
}
dir3updated = dir3;
list2  = getFileList(dir3updated);

//Makes new folder for data and runs the ShollFunction function
dir4 = dir1+"newData"+File.separator;
File.makeDirectory(dir4);

for (i = 0; i < list2.length; i++) {
	print(list2[i]);
	ShollFunction(dir3updated, dir4, list2[i]);
	close();
}

//runs the group shall profiles script
run("Group Sholl Profiles...", "dir=[" + dir4 + "] pattern= extension=.csv xcol_idx1based=1 ycol_idx1based=2 output_type=[Merged data] verbose=false");


//helper function to split channels and save the second one
function SplitChannels(dir1, dir2, file) {
run("Bio-Formats Importer", 
"open=[" + dir1 + file + "] autoscale color_mode=Default rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT");
title = getTitle();
selectWindow(title);
run("Split Channels");
close();
saveAs("Tiff", dir2 + title + "green");
close("*");
}

//helper function to set scale, threshold and nucleus and then run 
//sholl analysis 
function ShollFunction(dir2, dir3, file) {
	run("Bio-Formats Importer", 
"open=[" + dir2 + file + "] autoscale color_mode=Default rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT");
title = getTitle();
selectWindow(title);

//The following line sets the scale of the image

run("Set Scale...", "distance=scale known=1 pixel=1 unit=um global");

setAutoThreshold("Default dark");
run("Threshold...");
setTool("point");
selectBool = false;
waitForUser("Set Threshold, click cell nucleus and then click OK");
selectBool = false;
while(selectBool == false){
	if(getValue("selection.size") == 0){
		waitForUser("Select the cell nucleus!");
	}
	else{
		run("Sholl Analysis...", "starting=10 ending=150 radius_step=1 #_samples=1 integration=Mean enclosing=1 #_primary=1 infer fit linear polynomial=[Best fitting degree] most normalizer=Area save directory=["+dir3 + "] do not display saved files");
	selectBool = true;
	break;
	}	
}

function ZProjecter(dir1, dir2){
//ZProjecter is based on a macro written by Richard Srioworarat
	list  = getFileList(dir1);
	
	for (i = 0; i < list.length; i++) {
		file = list[i];
		print(list[i]);
		run("Bio-Formats Importer", "open=[" + dir1 + file + "] autoscale color_mode=Default rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT");
		Stack.getDimensions(width, height, channels, slices, frames);
		num = floor(slices / 3);
		
		for (j = 1; j <= num; j++) {
			end = j * 3;
			start = end - 2;
			run("Z Project...", "start=" + j + " stop=" + end + " projection=[Max Intensity]");
			run("Channels Tool...");
			Stack.setDisplayMode("composite");
			run("Stack to RGB");
			saveAs("tiff", dir2 + substring(file,0,lengthOf(file)-4) + "-" +j);
			close();
	}
	}
	}

}