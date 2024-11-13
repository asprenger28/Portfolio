/* The following options are described in HomeworkOptionsAnnotated.sas in compass.
   Please refer to that file to determine which settings you wish to use or modify
   for your report.
*/
*ods html close;
*options nodate nonumber leftmargin=1in rightmargin=1in;
*title;
*ods escapechar="~";
*ods graphics on / width=4in height=3in;
*ods rtf file='FileNameWithFullPath.rtf'
        nogtitle startpage=no;
*ods noproctitle;

/* The raw data in Indian Liver Patient Dataset (ILPD).csv is from

   https://archive.ics.uci.edu/ml/datasets/ILPD+(Indian+Liver+Patient+Dataset) 

   Dua, D. and Graff, C. (2019). 
   UCI Machine Learning Repository [http://archive.ics.uci.edu/ml]. 
   Irvine, CA: University of California, School of Information and Computer Science. 
*/
proc import datafile="C:\Stat 448\Indian Liver Patient Dataset (ILPD).csv"
	out= liver
	dbms = csv
	replace;
	getnames=no;
run;
/* after importing, rename the variables to match the data description */
data liver;
	set liver;
	Age=VAR1; Gender=VAR2; TB=VAR3;	DB=VAR4; Alkphos=VAR5;
	Alamine=VAR6; Aspartate=VAR7; TP=VAR8; ALB=VAR9; AGRatio=VAR10;
	if VAR11=1 then LiverPatient='Yes';
		Else LiverPatient='No';
	drop VAR1--VAR11; 
	cell=gender||LiverPatient;

run;


data liver2;
  set liver;
  	cell=gender||LiverPatient;
    if not missing(Age) and not missing(gender) and not missing(TB) and not missing(DB) and 
       not missing(Alkphos) and not missing(Alamine) and not missing(Aspartate) and 
       not missing(TP)  and not missing(AGRatio) and not missing(LiverPatient);
  if TB <40;
  if Alkphos<1900;
  if Alamine<1900;
  if Aspartate<2900;
run;



*1);
proc means data=liver2;
    var TB--AGRatio  age;
    class LiverPatient;
run;
proc sgplot data=liver2;
   vbox Alkphos  / category=LiverPatient boxwidth=0.5;
run;
proc sgplot data=liver2;
   vbox Alamine  / category=LiverPatient boxwidth=0.5;
run;
proc sgplot data=liver2;
      vbox Aspartate / category=LiverPatient boxwidth=0.5;
run;
*2);

proc logistic data=liver2 desc ;
class LiverPatient gender ;
	model LiverPatient = TB--AGRatio gender age/selection=backward;
	ods select OddsRatios ParameterEstimates 
		GlobalTests ModelInfo FitStatistics;
run;



*3a;
proc glm data=liver2;
	class gender age;
	model TP = TB--Aspartate ALB AGRatio gender age/ ss3;
	where LiverPatient='Yes';
	ods select OverallANOVA FitStatistics ModelANOVA;

run;
**with age taken out;
proc glm data=liver2;
	class gender ;
	model TP = TB--Aspartate ALB AGRatio gender / ss3;
	where LiverPatient='Yes';
	ods select  ModelANOVA;

run;
**with age and gender;
proc glm data=liver2;
	model TP = TB--Aspartate ALB AGRatio  / ss3;
	where LiverPatient='Yes';
	ods select  ModelANOVA;
run;
**with age, gender, and Alkphos taken out;
proc glm data=liver2;
	model TP = TB DB Alamine Aspartate ALB AGRatio  / ss3;
	where LiverPatient='Yes';
	ods select  ModelANOVA;
run;
**with age, gender,Alkphos, and TB taken out(final model);
proc glm data=liver2;
	model TP =  DB Alamine Aspartate ALB AGRatio  / ss3;
	where LiverPatient='Yes';
	ods select OverallANOVA FitStatistics ModelANOVA ParameterEstimates;
run;


 4;
proc princomp data=liver2  out=pcout;
 var TB--AGRatio age;
run;

proc sgplot data=pcout;
  scatter y=prin1 x=prin2 / markerchar=LiverPatient;
run;
proc sgplot data=pcout;
  scatter y=prin1 x=prin3 / markerchar=LiverPatient;

run;


proc cluster data=pcout method=complete ccc std pseudo print=15 plots=all outtree=newtree;
  var Prin1--Prin3;
  copy LiverPatient;
  ods select ClusterHistory Dendrogram CccPsfAndPsTSqPlot;
run;


proc tree data=newtree out=newclusters n=8 noprint;
  copy LiverPatient Prin1--Prin3;
run;
proc sort data=newclusters;
 by cluster;
run;
proc freq data=newclusters;
  tables cluster*LiverPatient/ nopercent norow nocol;
run;


**5);
proc stepdisc data=liver2 sle=.05 sls=.05;
   	class cell;
 var TB--AGRatio age;
ods select Summary;
run;

proc discrim data=liver2 pool=test crossvalidate manova;
  	class cell;
  	var DB Alkphos age Aspartate ;
   	ods select ChiSq ClassifiedCrossVal ErrorCrossVal;
run;

