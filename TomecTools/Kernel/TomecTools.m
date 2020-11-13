(* ::Package:: *)

(* Wolfram Language Package *)

(* Created by the Wolfram Workbench 9 Nov 2020 *)

BeginPackage["TomecTools`"]

(* Clear definitions from package symbols in public and private context. *)
ClearAll["`*","`*`*"];

ReshapeArray;
DropFirst;

(* Implementation of the package *)
Begin["`Private`"]


DropFirst//ClearAll
DropFirst::usage="DropFirst[list] returns the list without its first element.";

DropFirst//SyntaxInformation={"ArgumentsPattern"->{_}};

DropFirst[list_List]:=Drop[list,1]


ReshapeArray//ClearAll
ReshapeArray::usage="ReshapeArray[array, shape] reshape array allowing one dimension to be Automatic.";
ReshapeArray::badautoshape="Only one dimension is permitted to be Automatic";
ReshapeArray::badshape="Specified shape does not match with the dimensions of array.";
ReshapeArray::baddim="This array is not suitable for reshaping: check dimensions.";

ReshapeArray//SyntaxInformation={"ArgumentsPattern"->{_,_}};

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


ToTikzFormat//ClearAll
ToTikzFormat::usage="ToTikzFormat[list] converts a list into a LaTeX package Tikz format.";
ToTikzFormat::badshape="List must be a point or a set of points.";

ToTikzFormat//SyntaxInformation={"ArgumentsPattern"->{_}};

ToTikzFormat[list_List]:=Module[
	{dimensions, s},
	
	dimensions=Dimensions[list];
	s="";
	
	If[
		Last@dimensions!=2,
		Message[ReshapeArray::badshape];Return[Null,Module]
	];
	Switch[
		Length@dimensions,
		1,s=StringReplace[
			ToString[N@list],
			{"{" -> "(", "}" -> ")"}
		]<>" ",
		2,Do[
			s=s<>StringReplace[
				ToString[N@list[[i]]],
				{"{" -> "(", "}" -> ")"}
			]<>" ",
			{i,First@dimensions}
		],
		_,Message[ReshapeArray::badshape];Return[Null,Module]
	];
	StringDrop[s,-1]
];


CopyToTikzFormat//ClearAll
CopyToTikzFormat::usage="ToTikzFormat[list] converts a list into a LaTeX package Tikz format and copies it into the clipboard.";

CopyToTikzFormat//SyntaxInformation={"ArgumentsPattern"->{_}};

CopyToTikzFormat[list_List]:=CopyToClipboard[ToTikzFormat[list]];


End[]

EndPackage[]

