(* ::Package:: *)

BeginPackage["IbexCovReader`"]

IbexCovReader::usage = 
	"Read an Ibex COV file.
	The function readCovFile[filename] reads a COV file and return an association of the data contained in the COV file.
	Indices use the Mathematica conventions, starting at 1. An indice 'a' corresponds to the indice 'a-1' in Ibex.
	extractBoxes[cov, list] extract boxes from the Cov a list of indices.
	List of public functions:
		readCovFile[file, formatMaxLevel:5],
		extractBoxes[cov, indicesList],
		extractBoxes[cov, keyName] (e.g. 'solutionIndices'),
		extractInnerBoxes[cov],
		extractPendingBoxes[cov],
		extractBoundaryBoxes[cov],
		extractManifoldBoundaryBoxes[cov],
		extractNotInnerBoxes[cov],
		extractUnknownBoxes[cov],
		extractSolutions[cov],
		extractSolutionBoxes[cov],
		extractUnicityBoxes[cov],
		extractSummary[cov],
		setByteOrdering[$ByteOrdering].
	
	List of keys in the Cov (their presence depends on the format of the file and problem caracteristics):
		n,
		nBoxes,
		boxes,
		nInners,
		innerIndices,
		boundaryType,
		nBoundaries,
		boundaryIndices,
		nEqs, 
		nIneqs,
		manifoldBoundaryType,
		nSolutions,
		solutions(indice, parametricProof, unicityBox),
		solutionIndices,
		nManifoldBoundaries,
		manifoldBoundaryIndices or manifoldBoundaryIndices(indice, parametricProof),
		varNames,
		solverStatus,
		time,
		cells,
		nPendings,
		pendingsIndices,
		optimizerStatus,
		coveringType,
		uplo,
		uploEpsBoxes,
		loup,
		feaspointFound,
		feasiblePoint.
";

readCovFile::usage = "Read a Cov file to the maximum specified depth (default 5).";
readCovFile[filename_, levelMax_:5]:= Module[{stream, cov},
	stream = OpenRead[filename, BinaryFormat->True];
	cov = Private`readStream[stream, levelMax];
	Close[stream];
	cov
];
setByteOrdering::usage = "Set the byte ordering when reading a Cov file (default -1, little endian).";
setByteOrdering[bo_]:= byteOrdering = ByteOrdering -> bo;

extractBoxes::usage = "Extract boxes from the Cov, either with a list of indices, or the name of a list of indices in the Cov.";
extractBoxes[dataset_, list_List]:= Extract[dataset["boxes"], List/@list];
extractBoxes[dataset_, setname_String]:= Extract[dataset["boxes"], List/@dataset[setname]];

extractInnerBoxes[dataset_]:= extractBoxes[dataset, "innerIndices"];
extractPendingBoxes[dataset_]:= extractBoxes[dataset, "pendingIndices"];
extractBoundaryBoxes[dataset_]:= extractBoxes[dataset, "boundaryIndices"];
extractManifoldBoundaryBoxes[dataset_]:= extractBoxes[dataset, "manifoldBoundaryIndices"]; 
extractNotInnerBoxes[dataset_]:= Delete[dataset["boxes"], List/@dataset["innerIndices"]];
extractUnknownBoxes[dataset_]:= Module[{
	indicesToRemove = Join@@DeleteMissing[dataset[#]& /@ {"innerIndices", "boundaryIndices"}]},
	Delete[dataset["boxes"], List/@indicesToRemove]
];
extractSolutions[dataset_]:= dataset["solutions"];
extractUnicityBoxes[dataset_]:= #["unicityBox"]&/@cov["solutions"];
extractSolutionBoxes[dataset_]:= extractBoxes[dataset, "solutionIndices"];
extractSummary[dataset_]:= KeyDrop[dataset, {
	"boxes", "innerIndices", "boundaryIndices", 
	"manifoldBoundaryIndices", "pendingIndices", "solutionIndices", "solutions", "parametricProofs"}];
Begin["Private`"]



byteOrdering = ByteOrdering -> -1;

solverStatusMap = <|
	0 -> "CompleteSearchValidated",
	1 -> "CompleteSearchInfeasible",
	2 -> "IncompleteSearchMinimalWidth",
	3 -> "IncompleteSearchTimeout",
	4 -> "IncompleteSearchBufferOverflow",
	5 -> "IncompleteSearchUserBreak"
|>;

boundaryTypeMap = <|
	0 -> "BoundaryBoxContainsAnInnerPoint",
	1 -> "BoundaryBoxContainsAtLeastOneInnerAndOneOuter"
|>;

manifoldBoundaryTypeMap = <|
	0 -> "OnlyEqualities",
	1 -> "EqualitiesAndGradientsOfActiveConstraintsLinearlyIndependend",
	2 -> "IntersectionOfManifoldAndBoxIsHomeomorphicToAHalfBallOfRn"
|>;

optimizerStatusMap = <|
	0 -> "Success",
	1 -> "InfeasibleProblem",
	2 -> "NoFeasiblePointFound",
	3 -> "UnboundedObjective",
	4 -> "Timeout",
	5 -> "UnreachedPrecision"
|>;

optimizerCoveringMap = <|
	0 -> "CoveringOfOriginalSpace",
	1 -> "CoveringOfExtendedSpace"
|>;

formatMapL0 = <| {0,1} -> readCovV1 |>;
formatMapL1 = <| {0,1} -> readCovListV1 |>;
formatMapL2 = <| {0,1} -> readCovIUListV1, {1,1} -> readCovOptimDataV1 |>;
formatMapL3 = <| {0,1} -> readCovIBUListV1 |>;
formatMapL4 = <| {0,1} -> readCovManifoldV1 |>;
formatMapL5 = <| {0,1} -> readCovSolverDataV1toV2, {0,2} -> readCovSolverDataV1toV2 |>;
formatMap = <|
	0 -> formatMapL0,
	1 -> formatMapL1,
	2 -> formatMapL2,
	3 -> formatMapL3,
	4 -> formatMapL4,
	5 -> formatMapL5
|>;

IbexCovReader::unknownFormat = 
	"Unknown format encoutered at level `1`, id `2`, version `3`.
	The format may be unknown or not implemented.
	Reading only data before this part file.";
	
IbexCovReader::notAValidCovFile =
	"Stream `1` is not a valid COV stream or file.";

IbexCovReader::eofReached =
	"Unexpectedly reached EndOfFile."



checkEndOfFile[arg_List]:= If[
	MemberQ[arg, EndOfFile],
	Message[IbexCovReader::eofReached]; Abort[],
	arg]
checkEndOfFile[arg_]:= If[
	SameQ[arg, EndOfFile],
	Message[IbexCovReader::eofReached]; Abort[],
	arg]

readUInt[stream_]:= checkEndOfFile@BinaryRead[stream, "UnsignedInteger32", byteOrdering];
readIndice[stream_]:= readUInt[stream]+1;
readUIntList[stream_, n_]:= checkEndOfFile@BinaryReadList[stream, "UnsignedInteger32", n, byteOrdering];
readIndiceList[stream_, n_]:= Table[readIndice[stream], n];
readReal[stream_]:= checkEndOfFile@BinaryRead[stream, "Real64", byteOrdering];
readRealList[stream_, n_]:= checkEndOfFile@BinaryReadList[stream, "Real64", n, byteOrdering];
readInterval[stream_]:= Interval[readRealList[stream, 2]];
readBox[stream_, n_]:= Interval /@ Partition[readRealList[stream, 2*n], 2];
readBoxList[stream_, n_, nBoxes_]:= Table[readBox[stream, n], nBoxes];
(*readNullTerminatedString[stream_]:= ExportString[checkEndOfFile@ReadByteArray[stream, ByteArray[{0}]],"Character8"];*)
readNullTerminatedString[stream_]:= checkEndOfFile@BinaryRead[stream, "TerminatedString"];

(* We drop the last character (null character) to be able to compare with Mma string, that do not terminate with a null character *)
readSignature[stream_]:= ExportString[Drop[checkEndOfFile@BinaryReadList[stream, "Character8", 20], -1], "Character8"];

checkSignature[signature_String]:= signature == "IBEX COVERING FILE ";

readCovV1[stream_]:= With[{signature = readSignature[stream], formatLevel = readUInt[stream]},
	If[Not@checkSignature[signature], 
		Message[IbexCovReader::notAValidCovFile, stream];
		Print[signature];
		Abort[];
	];
	<| 
		"signature" -> signature,
		"formatLevel" -> formatLevel,
		"formatIds" -> readUIntList[stream, formatLevel+1],
		"formatVersions" -> readUIntList[stream, formatLevel+1],
		"n" -> readUInt[stream]
	|>
];

readCovListV1[stream_, dataset_]:= With[{n = dataset["n"], nBoxes = readUInt[stream]},
	<| "nBoxes" -> nBoxes, "boxes" -> readBoxList[stream, n, nBoxes] |>
];

readCovOptimDataV1[stream_, dataset_]:= With[{n = dataset["n"]},
	varNames = Table[readNullTerminatedString[stream], n];
	status = readUInt[stream];
	covering = readUInt[stream];
	uplo = readReal[stream];
	uploEps = readReal[stream];
	loup = readReal[stream];
	feaspoint = readUInt[stream];
	time = readReal[stream];
	cells = readUInt[stream];
	<|
		"varNames" -> varNames,
		"optimizerStatus" -> status,
		"optimizerStatusReadable" -> optimizerStatusMap[status],
		"coveringType" -> covering,
		"coveringTypeReadable" -> optimizerCoveringMap[covering],
		"uplo" -> uplo,
		"uploEpsBoxes" -> uploEps,
		"loup" -> loup,
		"feaspointFound" -> feaspoint == 1,
		"time" -> time,
		"cells" -> cells,
		"feasiblePoint" -> If[feaspoint == 1, First@dataset["boxes"], None]
	|>
];

readCovIUListV1[stream_, dataset_]:= With[{nInners = readUInt[stream]},
	<| "nInners" -> nInners, "innerIndices" -> readIndiceList[stream, nInners] |>
];

readCovIBUListV1[stream_, dataset_]:= With[{type = readUInt[stream], nBoundaries = readUInt[stream]},
	<| "boundaryType" -> type, "boundaryTypeReadable" -> boundaryTypeMap[type], "nBoundaries" -> nBoundaries, "boundaryIndices" -> readIndiceList[stream, nBoundaries] |>
];
End[]


Begin["ManifoldV1`"]
readUInt = Private`readUInt;
readUIntList = Private`readUIntList;
readBox = Private`readBox;
readIndice = Private`readIndice;
readManifoldSolution[stream_, n_, nEqs_]:= With[{indice = readIndice[stream]},
	solution = 
		If[nEqs < n,
			<|
				"indice" -> indice,
				"parametricProof" -> readUIntList[stream, n-nEqs], 
				"unicityBox" -> readBox[stream, n]
			|>
			,
			<| "indice" -> indice, "unicityBox" -> readBox[stream, n] |>
		];
	{indice, solution}
];

readExistingSolutionSet[stream_, n_, nEqs_]:= Module[{
	nSols = readUInt[stream], ms},
	ms = Table[readManifoldSolution[stream, n, nEqs], nSols];
	{indices, solutions} = Transpose@ms;
	<| "nSolutions" -> nSols, "solutions" -> solutions, "solutionIndices" -> indices |>
];

readSolutionSet[stream_, n_, nEqs_]:=With[{},
	If[nEqs > 0, readExistingSolutionSet[stream, n, nEqs], <||>]
];
End[]


Begin["Private`"]
readCovManifoldV1[stream_, dataset_]:= With[{nEqs = readUInt[stream], nIneqs = readUInt[stream], type = readUInt[stream], n = dataset["n"]},
	newDataset = <| 
		"nEqs" -> nEqs, 
		"nIneqs" -> nIneqs, 
		"manifoldBoundaryType" -> type, 
		"manifoldBoundaryTypeReadable" -> manifoldBoundaryTypeMap[type] 
	|>;
	solutionsDataset = ManifoldV1`readSolutionSet[stream, n, nEqs];
	nBoundaries = readUInt[stream];
	{boundaryIndices, parametricProofs} = If[nBoundaries == 0, {{}, {}},
		If[0 < nEqs < n,
			Transpose@Table[{readIndice[stream], readUIntList[stream, n-nEqs]}, nBoundaries],
			{readIndiceList[stream, nBoundaries], {}}
		]
	];
	Join[newDataset, solutionsDataset, <| "nManifoldBoundaries" -> nBoundaries, "manifoldBoundaryIndices" -> boundaryIndices, "parametricProofs" -> parametricProofs |>]
];

readCovSolverDataV1toV2[stream_, dataset_]:= With[{n = dataset["n"]},
	varNames = Table[readNullTerminatedString[stream], n];
	status = readUInt[stream];
	time = readReal[stream];
	cells = readUInt[stream];
	nPendings = readUInt[stream];
	pendingIndices = readIndiceList[stream, nPendings];
	<|
		"varNames" -> varNames,
		"status" -> status,
		"statusReadable" -> solverStatusMap[status],
		"time" -> time,
		"cells" -> cells,
		"nPendings" -> nPendings,
		"pendingIndices" -> pendingIndices
	|>
];

readStream[stream_, levelMax_]:= Module[{},
	dataset = readCovV1[stream];
	ids = dataset["formatIds"];
	versions = dataset["formatVersions"];
	For[level = 1, level <= dataset["formatLevel"] && level <= levelMax, level++,
		{id, version} = {ids[[level+1]], versions[[level+1]]};
		func = formatMap[level][{id, version}];
		If[MissingQ[func], 
			Message[IbexCovReader::unknownFormat, level, id, version]; 
			Break[]
		];
		dataset = Join[dataset, func[stream, dataset]];
	];
	dataset
]

End[]

EndPackage[]



