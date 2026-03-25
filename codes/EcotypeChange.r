#Change the genotype file of crops in DSSAT.

EcotypeChange<-function(GD, DSSATD, OD, CropName, GenotypeFileName, EcotypeID, RunNumber, RandomMatrix)
{
eval(parse(text=paste('EcotypeFilePath="',GD,'/',GenotypeFileName,'.ECO"',sep = '')));

ReadLine<-readLines(EcotypeFilePath, n=-1)
EcotypeFile<-as.character(ReadLine); #Get the genotype file saved as a template.

LineNumber<-grep(pattern=paste0("^",EcotypeID), EcotypeFile); #Get the number of the line where the cultivar "GLUECUL" is located.
HeaderLineNumber<-grep(pattern="^@ECO", EcotypeFile); #Get the number of the line where the cultivar "GLUECUL" is located.
OldLine<-EcotypeFile[LineNumber];#Get the line according to the line number.
HeaderLine<-EcotypeFile[HeaderLineNumber]
R<-RunNumber;#Get what parameter set will be used to change the genotype file.

#  ParameterStep<-6;
#  ValuePosition = regexpr("ECONAME.*MG TM |ECONAME\\.* ",HeaderLine)
#  ValuePosition1<-ValuePosition[1] + attr(ValuePosition,"match.length") + 1 - ParameterStep
#  ValuePosition2<-ValuePosition1 + ParameterStep - 2

  ParamNames <- colnames(RandomMatrix)
  ParamNamesWithSpace <- format(ParamNames,width=5,justify='right')
#  for (i in (TotalParameterNumber+1):ncol(RandomMatrix))
  for (i in 1:ncol(RandomMatrix))
  {
  Position <- regexpr(ParamNamesWithSpace[i],HeaderLine)
  ValuePosition1<-Position
  ValuePosition2<-Position+attr(Position,"match.length")-1

  eval(parse(text = paste("Parameter<-RandomMatrix[R,",i,"]",sep = '')));
  #To solve the format problem for parameters with negative values. Modified by He, 2015-6-18.
  if(Parameter < 0 & Parameter > -1.0)                                                          #
  {                                                                                             #
  ParameterFormat<-sprintf('%1.3f', Parameter);                                                 #
  ParameterFormat<-paste(substring(ParameterFormat,1,1), substring(ParameterFormat,3), sep=''); #
  } else if (Parameter <= -1.0 & Parameter > -10.0)                                             # 
  {                                                                                             #
  ParameterFormat<-sprintf('%2.2f', Parameter);                                                 #
  } else if (Parameter <= -10.0 & Parameter > -100.0)                                           #
  {                                                                                             #
  ParameterFormat<-sprintf('%3.1f', Parameter);                                                 #
  }                                                                                             #
  
  if(Parameter >= 0 & Parameter < 1)
  {
  ParameterFormat<-sprintf('%0.4f', Parameter);
  ParameterFormat<-substring(ParameterFormat,2)
  } else if(Parameter >= 0 & Parameter < 10)
  {
  ParameterFormat<-sprintf('%1.3f', Parameter);
  } else if (Parameter >= 10 & Parameter < 100)
  {
  ParameterFormat<-sprintf('%2.2f', Parameter);
  } else if (Parameter >= 100)
  {
  ParameterFormat<-sprintf('%3.1f', Parameter);
  }

  substr(OldLine, ValuePosition1, ValuePosition2)<-ParameterFormat;
  }

  EcotypeFile[LineNumber]<-OldLine;#Replace the old line with new generated line in the Genotype file.
                                                   
eval(parse(text=paste("NewEcotypeFilePath='",OD,"/",GenotypeFileName,".ECO'",sep = '')));
write(EcotypeFile, file=NewEcotypeFilePath);
#Save the new genotype file as "eco" file in the GLWork directory.

}




 
