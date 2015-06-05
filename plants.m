BeginPackage["IdentifyPlant`"]
(* Exported symbols added here with SymbolName::usage *) 

IdentifyPoisonousOrNot::usage = "IdentifyPoisonousOrNot[ImagePath,jsonFilePath] will tell if a plant is poisonous or  not" 
IdentifyPoisonousOrNotForm::usage = ""

Begin["`Private`"] (* Begin Private Context *)

IdentifyPoisonousOrNot[ImagePath_,jsonFilePath_]:= Module[{jsonData,plantNameEntity,plantName,result,temp},
	(*jsonData = Import[jsonFilePath];*)
	jsonData = Import["https://raw.githubusercontent.com/MaxGreenwald/wolframProjects/master/plants.json"];

	(*get the name  of the plant from image identify*)
	plantNameEntity = ImageIdentify[Import[ImagePath],Entity["Word","Plant"],1,{"Entity","Probability"}];
	
	temp = ToString[plantNameEntity[[1]][[1]]];

	If[temp ==  "Missing[NotAvailable]",
		Print["Invalid Input!!! as we think it is not a plant"];
		Return;
		,
		(
			plantName = CommonName[plantNameEntity[[1]][[1]]];
			Print[plantName];

	(*Check if plant name is present in our dataset*)
	result = Map[Compare[#,plantName]&,jsonData];
	(*Print[result];*)

		If[MemberQ[result,True] == True,
			Print["Plant is Poisonous don't eat it!!!"]
			, Print["Plant is safe with "<> ToString[plantNameEntity[[1]][[2]]*100]<>" % probability to eat!!!!"]
		]
		)
	]
]

(*Private helper function*)
Compare[list_,name_]:= Module[{actualNames,combineString},
	actualNames ={"FIELD1","FIELD2"}/.list;
	combineString = actualNames[[1]]<>";"<>actualNames[[2]];
    Print[combineString];
    MemberQ[Map[StringMatchQ[#,name,IgnoreCase->True]&,StringSplit[combineString,"; "]],True]
]

IdentifyPoisonousOrNotForm[Image_]:= Module[{jsonData,plantNameEntity,plantName,result,temp},
	jsonData = Import["https://raw.githubusercontent.com/MaxGreenwald/wolframProjects/master/plants.json"];

	(*get the name  of the plant from image identify*)
	plantNameEntity = ImageIdentify[Image,Entity["Word","Plant"],1,{"Entity","Probability"}];
	
	temp = ToString[plantNameEntity[[1]][[1]]];

	If[temp ==  "Missing[NotAvailable]",
		Return["Invalid Input!!! To the best of our knowledge, this picture is NOT a plant"];
		,
		(
			plantName = CommonName[plantNameEntity[[1]][[1]]];
			Print[plantName];

			(*Check if plant name is present in our dataset*)
			result = Map[Compare[#,plantName]&,jsonData];
			(*Print[result];*)

		If[MemberQ[result,True] == True,
			Return[ "Uh oh, this is a "<>plantName <> " plant. This plant is poisonous, please don't eat it!!!"]
			, Return["Mmmm this is a " <> plantName<>" plant. Or at least we think it a " <> plantName <>  " with a probability of"<> ToString[plantNameEntity[[1]][[2]]*100]<>"%. Ah what the hey, go ahead and eat it!!!!"]
		]
		)
	]
]

End[] (* End Private Context *)
EndPackage[]
