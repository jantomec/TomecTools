(* ::Package:: *)

(* ::Title:: *)
(*TomecTools package*)


(* ::Section::Closed:: *)
(*Header*)


(* Wolfram Language Package *)

(* Created by the Wolfram Workbench 9 Nov 2020 *)

BeginPackage["TomecTools`"]

(* Clear definitions from package symbols in public and private context. *)
ClearAll["`*","`*`*"];

ReshapeArray;
DropFirst;
RemoveRowsColumns;
DataDilution;

ToTikzFormat;
CopyToTikzFormat;

SplineApproximation;

(* Implementation of the package *)
Begin["`Private`"]


(* ::Section::Closed:: *)
(*Array Manipulation*)


(* ::Subsection::Closed:: *)
(*DropFirst*)


DropFirst//ClearAll
DropFirst::usage="DropFirst[list] returns the list without its first element.";

DropFirst//SyntaxInformation={"ArgumentsPattern"->{_}};

DropFirst[list_List]:=Drop[list,1]


(* ::Subsection::Closed:: *)
(*RemoveRowsColumns*)


RemoveRowsColumns//ClearAll;
RemoveRowsColumns::usage="RemoveRowsColumns[array, shape] removes column and row at specified positions.";

RemoveRowsColumns//SyntaxInformation={"ArgumentsPattern"->{_,_,_}};

RemoveRowsColumns[expr_List,row_List,col_List]:=Module[
	{M=expr},
	M=Delete[M,ArrayReshape[row,{Length@row,1}]];
	Transpose[
		Delete[Transpose[M],ArrayReshape[col,{Length@col,1}]]
	]
];


(* ::Subsection::Closed:: *)
(*ReshapeArray*)


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

(* ::Subsection::Closed:: *)
(*DataDilution*)
DataDilutionHelper[list_, threshold_] := Module[
	{r, pos, nl, new},
	r = Diagonal[DistanceMatrix[list], 1];
	pos = Flatten[Position[r, _?(# < threshold &)]];
	new = (list[[pos]] + list[[pos + 1]])/2;
	nl = list;
	nl[[Riffle[pos, pos + 1]]] = Riffle[new, new];
	DeleteDuplicates[nl]
]

DataDilution//ClearAll
DataDilution::usage="DataDilution[list, threshold] reduces deviation of data points by removing redundant points.";

DataDilution[list_, threshold_] := Module[
	{nl, new},
	nl = list;
	While[
		new = DataDilutionHelper[nl, threshold];
		nl != new,
		nl = new
	];
	nl
]


(* ::Section::Closed:: *)
(*Compatibility*)


(* ::Subsection::Closed:: *)
(*ToTikzFormat*)


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


(* ::Subsection::Closed:: *)
(*CopyToTikzFormat*)


CopyToTikzFormat//ClearAll
CopyToTikzFormat::usage="ToTikzFormat[list] converts a list into a LaTeX package Tikz format and copies it into the clipboard.";

CopyToTikzFormat//SyntaxInformation={"ArgumentsPattern"->{_}};

CopyToTikzFormat[list_List]:=CopyToClipboard[ToTikzFormat[list]];

(* ::Section::Closed:: *)
(*Data Approximation*)


(* ::Subsection::Closed:: *)
(*SplineApproximation*)

SplineApproximation//ClearAll
SplineApproximation::usage="SplineApproximation creates a piecewise continous spline approximation of data.";

SplineApproximation[data_, nPieces_Integer, order_, continuationOrder_] := Module[
	{
		a, b, c, cond, pars, f, fit, modelf
    },
    a = First[First[data]];
    b = First[Last[data]];
    cond = Partition[Subdivide[a, b, nPieces], 2, 1];
    f[x_] = Piecewise[
    	Table[
    		{
    			Sum[Subscript[c, {i, j}] x^j, {j, 0, order}],
    			cond[[i, 1]] <= x <= cond[[i, 2]]
    		},
    		{i, nPieces}
    	]
    ];
    pars = Flatten[Table[
    	Subscript[c, {i, j}], {j, 0, order}, {i, nPieces}]
    ];
    fit = FindFit[
    	data,
    	Prepend[
    		Flatten[
    			Table[
    				Limit[
    					Derivative[j][f][x], x -> #, 
    					Direction -> "FromAbove"
    				] == Limit[
    					Derivative[j][f][x], x -> #, 
            			Direction -> "FromBelow"] & /@ Most[cond[[All, 2]]],
        		{j, 0, continuationOrder}
        		]
       		],
      		f[x]
      	],
      	pars, x
    ];
    modelf[x_] = f[x] /. fit;
    modelf
];

SplineApproximation[data_, sep_List, order_, continuationOrder_] := Module[
	{
		a, b, nPieces, c, cond, pars, f, fit, modelf
	},
	a = First[First[data]];
	b = First[Last[data]];
    nPieces = Length[sep] + 1;
    cond = Partition[Join[{a}, sep, {b}], 2, 1];
    f[x_] = Piecewise[
    	Table[
    		{
    			Sum[Subscript[c, {i, j}] x^j, {j, 0, order}],
    			cond[[i, 1]] <= x <= cond[[i, 2]]
    		},
    		{i, nPieces}
    	]
    ];
    pars = Flatten[Table[Subscript[c, {i, j}], {j, 0, order}, {i, nPieces}]];
    fit = FindFit[
    	data,
    	Prepend[
    		Flatten[
    			Table[
    				Limit[
    					Derivative[j][f][x], x -> #, 
    					Direction -> "FromAbove"
    				] == Limit[
    					Derivative[j][f][x], x -> #, 
            			Direction -> "FromBelow"] & /@ Most[cond[[All, 2]]],
        		{j, 0, continuationOrder}
        		]
       		],
      		f[x]
      	],
      	pars, x
    ];
    modelf[x_] = f[x] /. fit;
    modelf
];

(* ::Section::Closed:: *)
(*End*)


End[]

EndPackage[]

