(* Wolfram Language Package *)

(* Created by the Wolfram Workbench 9 Nov 2020 *)

BeginPackage["TomecTools`"]

(* Clear definitions from package symbols in public and private context. *)
ClearAll["`*","`*`*"];

ReshapeArray;

(* Implementation of the package *)
Begin["`Private`"]

ReshapeArray//ClearAll
ReshapeArray::usage="ReshapeArray[array, shape] reshape array allowing one dimension to be Automatic.";
ReshapeArray::badautoshape="Only one dimension is permitted to be Automatic";
ReshapeArray::badshape="Specified shape does not match with the dimensions of array.";
ReshapeArray::baddim="This array is not suitable for reshaping: check dimensions.";

ReshapeArray//SyntaxInformation={"ArgumentsPattern"->{_,_}};

ReshapeArray[mesh_ElementMesh,list_List]:=Fold[AddMeshMarkers[#1,#2]&,mesh,list];

(* TODO: Add FrontEnd autocomplete for type argument. *)
ReshapeArray[array_List,shape_List]:=Module[
	{dimensions,flatArray,noElements,n,a,newShape},
	
	If[
		Count[shape,Automatic]>1,
		Message[ReshapeArray::badautoshape];Return[array,Module]
	];
	dimensions=Dimensions[array];
	flatArray=Flatten[array];
	noElements=Apply[Times,dimensions];
	If[
		Length@flatArray===noElements,
		Null,
		Message[ReshapeArray::baddim];Return[array,Module]
	];
	n=Apply[Times,shape]/.Automatic->1;
	a=noElements/n;
	newShape=shape/.Automatic->a;
	If[
		Apply[Times,newShape]===noElements,
		Null,
		Message[ReshapeArray::badshape];Return[array,Module]
	];
	ArrayReshape[array,newShape]
];


End[]

EndPackage[]

