 
;; This files refers to MuPAD 2.5 and is used by mupad.el.

(defconst mupad-all-completions ; anames(All) minus anames(DOM_DOMAIN)
'("*" "+" "-" "/" "<" "=" ">" "D" "E" "I" "O" "**" "^" "<=" "<>" ">="
"C_" "Ci" "Ei" "Q_" "R_" "Im" "Re" "==>" "Ax" "Z_" "Si" "id" "fp" "ln"
"RGB" "is" "op" "Cat" "Dom" "SEED" "_if" "gcd" "Sum" "_in" "abs" "ode"
"adt" "csc" "arg" "rec" "sec" "has" "lcm" "erf" "map" "_or" "log"
"tan" "val" "min" "cos" "cot" "max" "lhs" "fun" "new" "sin" "int"
"psi" "exp" "rhs" "zip" "sum" "LEVEL" "ORDER" "Pref" "_and" "igcd"
"diff" "card" "pade" "beta" "frac" "read" "ceil" "fact" "_mod" "erfc"
"csch" "Type" "_div" "sech" "ilcm" "_for" "hold" "eval" "help" "tanh"
"bool" "func" "info" "misc" "args" "cosh" "irem" "coth" "gcov" "time"
"_not" "modp" "sign" "sinh" "mods" "last" "zeta" "hull" "_xor" "prog"
"null" "norm" "subs" "iquo" "expr" "plot" "nops" "slot" "type" "poly"
"DIGITS" "sort" "sqrt" "_case" "Axiom" "fread" "coeff" "dirac" "gamma"
"LIBPATH" "debug" "fname" "table" "alias" "gcdex" "_pref" "rdabs"
"match" "_save" "logic" "dilog" "flock" "denom" "share" "_less"
"float" "evalp" "fopen" "level" "_next" "gprof" "array" "limit"
"_mult" "rtime" "_quit" "floor" "_plus" "reset" "lterm" "bytes"
"numer" "round" "solve" "error" "point" "write" "split" "trunc"
"print" "stats" "extop" "input" "HISTORY" "isqrt" "plot2d" "plot3d"
"RootOf" "MAXDEPTH" "MAXLEVEL" "faclib" "_break" "gcdlib" "signIm"
"_frame" "Series" "_range" "degree" "igamma" "arccsc" "lcoeff"
"arcsec" "coerce" "igcdex" "anames" "divide" "_equal" "_index"
"linalg" "tcoeff" "_while" "append" "arctan" "arccos" "arccot"
"fclose" "getpid" "_fnest" "_seqin" "factor" "arcsin" "expand"
"select" "freeze" "random" "intlib" "length" "assign" "maprat"
"module" "indets" "numlib" "_union" "_equiv" "normal" "_minus"
"series" "_power" "unlock" "assume" "ground" "lllint" "assert"
"fprint" "expose" "matrix" "finput" "linopt" "rdmult" "revert"
"nterms" "rdplus" "subsex" "import" "taylor" "iszero" "subsop"
"asympt" "return" "crypto" "export" "system" "output" "meijerG"
"TEXTWIDTH" "besselI" "besselJ" "besselK" "package" "_delete"
"_negate" "_divide" "_concat" "arccsch" "besselY" "loadlib" "ldegree"
"arcsech" "Content" "_for_in" "combine" "_repeat" "loadmod" "arctanh"
"_seqgen" "arccosh" "_assign" "arccoth" "pdivide" "collect" "frandom"
"arcsinh" "ifactor" "Network" "domains" "unalias" "radsimp" "listlib"
"numeric" "discont" "funcenv" "plotlib" "warning" "_invert" "builtin"
"isprime" "detools" "content" "polylib" "genpoly" "hastype" "rdround"
"sysname" "getprop" "product" "protect" "domtype" "nthterm" "rewrite"
"context" "polylog" "version" "student" "polygon" "extnops" "history"
"PACKAGEPATH" "Factored" "tbl2text" "text2tbl" "_fconcat" "lambertV"
"Category" "lambertW" "_procdef" "int2text" "text2int" "matchlib"
"_leequal" "rdnegate" "binomial" "generate" "rddivide" "combinat"
"nthcoeff" "genident" "pathname" "contfrac" "_implies" "partfrac"
"groebner" "loadproc" "_unequal" "indexval" "_pattern" "_lazy_or"
"contains" "solvelib" "ithprime" "rectform" "external" "icontent"
"unfreeze" "interval" "register" "strmatch" "_exprseq" "infinity"
"plotfunc" "userinfo" "linsolve" "operator" "powermod" "simplify"
"testargs" "_stmtseq" "unassume" "universe" "protocol" "PRETTYPRINT"
"sysorder" "orthpoly" "testtype" "property" "unexport" "newDomain"
"degreevec" "_lazy_and" "heaviside" "poly2list" "undefined"
"text2list" "mapcoeffs" "expr2text" "text2expr" "_for_down"
"piecewise" "conjugate" "readbytes" "unloadmod" "_subtract"
"rdgetsign" "lmonomial" "_op_power" "protected" "bernoulli"
"stringlib" "hypergeom" "sysdelete" "nextprime" "transform"
"lasterror" "substring" "traperror" "sysassign" "unprotect"
"extsubsop" "textinput" "clientCall" "plotfunc2d" "plotfunc3d"
"_rootFrame" "patchlevel" "evalassign" "_intersect" "newfuncarg"
"rdisfinite" "multcoeffs" "rdsubtract" "properties" "writebytes"
"outputType" "ftextinput" "MathContent" "rdint2float" "_op_fconcat"
"irreducible" "_eval_entry" "copyClosure" "rationalize" "nthmonomial"
"interpolate" "newpurefunc" "setuserinfo" "_parentFrame"
"clientObject" "rdfloat2text" "rdtext2float" "_mult_intern"
"outputFormat" "_op_plusmult" "sparsematrix" "_constructor"
"_check_global" "_act_proc_env" "ContentLayout" "sysevalassign"
"_parser_config" "assignElements" "_domainfunccall" "_userinfo_level"
"complexInfinity" "_print_userinfo" "_parser_callback"
"AxiomConstructor" "DomainConstructor" "_lastContentOutput"
"CategoryConstructor"
;; Not mupad-types-list, not mupad-libraries, not options, not keywords
))

(defconst mupad-keywords-list ;; Do *not* add spaces for font-lock to work.
;; use also  indexval(stdlib, "SYSTEM_CONSTANTS") ;
;; use also  indexval(stdlib, "LIBRARY_CONSTANTS") ;
'("and" "axiom" "category" "begin" "break" "case" "div" "do"
"downto" "E" "elif" "else" "end_case" "end_for" "end_if" "end_proc"
"end_repeat" "end_while" "FAIL" "FALSE" "for" "from" "fun" "func" "I"
"if" "%if" "in" "intersect" "local" "minus" "mod" "name" "next" "NIL"
"not" "of" "option" "or" "otherwise" "proc" "quit" "repeat" "step"
"then" "to" "TRUE" "union" "until" "UNKNOWN" "while" "domain"
"end_domain" "PI" "EULER" "CATALAN" "MSDOS" "EMPTY" "OPTIMAL" "Global"
"UNBOUNDED" "infinity" "undefined" "toBeDefined" "complexInfinity"))

(defconst mupad-options-list ;; obtained via stdlib::OPTIONS 
;; or indexval(stdlib, "OPTIONS") ;
'( "GC"
"NC" "GL" "PF" "GT" "X11" "On" "Up" "CDF" "RK4" "PDF" "QRD" "SVD"
"CK45" "CK54" "Gif" "All" "Bin" "Off" "Mem" "Log" "Lin" "Rem" "Any"
"Box" "New" "Not" "Quo" "RKF34" "RKF43" "RKF78" "RKF87" "NoNL" "QDet"
"xCK45" "xCK54" "XMin" "YMin" "XMax" "ZMin" "Hard" "YMax" "ZMax"
"Name" "Seed" "Lode" "Real" "Mode" "Grid" "Flat" "Line" "Left" "Args"
"Mesh" "Path" "None" "Tree" "Axes" "Crit" "Open" "Proc" "Term" "Nary"
"List" "Soft" "Vars" "Rest" "Expr" "User" "Test" "Only" "Root" "Text"
"RKF45a" "RKF54a" "EULER1" "RKF45b" "RKF54b" "xRKF34" "xRKF43"
"xRKF78" "xRKF87" "ULine" "VLine" "Coeff" "InSet" "Named" "DOPRI45"
"DOPRI54" "Beams" "Hlode" "Above" "Logic" "Merge" "Plain" "Exact"
"Monic" "Index" "Lines" "Order" "Right" "Shift" "Ticks" "Color"
"Title" "Gauss" "Curve" "First" "Quiet" "Terms" "Write" "Steps"
"Style" "andor" "Types" "xRKF45a" "xRKF54a" "xRKF45b" "xRKF54b"
"DegLex" "Banded" "BUTCHER6" "PreMap" "NoArgs" "IntMod" "Degree"
"Filled" "Delete" "Forced" "Labels" "Coeffs" "Cyclic" "Append"
"Domain" "Height" "Closed" "Scales" "Frames" "Factor" "xDOPRI45"
"xDOPRI54" "Ranges" "Center" "Random" "Length" "Sample" "ToZero"
"OutSet" "Binary" "Origin" "Corner" "Format" "Normal" "Solved"
"Prefix" "Values" "Always" "Ansatz" "Raster" "Colors" "Convex"
"Widths" "Bottom" "Titles" "Subset" "Unique" "Curves" "Approx"
"Taylor" "Points" "Arrows" "Scene2d" "System" "Scene3d" "Krylov"
"Pretty" "sincos" "RATIO_YX" "NoCheck" "AndMesh" "Alldata" "Fechner"
"Generic" "Concave" "Scaling" "Special" "Include" "PostMap" "Circles"
"Default" "Notched" "RatExpr" "Surface" "Eweight" "Reorder" "Centers"
"Discont" "Natural" "Laurent" "Splines" "Vweight" "Columns" "Restore"
"Squares" "Postfix" "AndULine" "AndVLine" "Puiseux" "NotAKnot"
"DrawMode" "DiffVars" "MaxCalls" "GenOrder" "TempFile" "Attached"
"Labeling" "Averaged" "Diagonal" "LexOrder" "ShowGrid" "VectorOf"
"Adaptive" "Capacity" "Periodic" "Extended" "FontSize" "Complete"
"NoErrors" "Vertical" "Minimize" "Symbolic" "PolyExpr" "Infinity"
"Multiple" "Impulses" "Unsorted" "Unquoted" "Stepsize" "Superset"
"Contours" "sinhcosh" "DegInvLex" "DegRevLex" "MaxDegree" "GridLines"
"KeepOrder" "WireFrame" "LineWidth" "NonNested" "Undefined"
"Connected" "NoWarning" "IndexList" "ToNearest" "LineStyle"
"RealsOnly" "Duplicate" "ShowTitle" "Dimension" "Automatic"
"FontStyle" "Protected" "Precision" "Frobenius" "InputOnly"
"Recursive" "LodeOverRF" "Primitive" "Transform" "Symmetric"
"Piechart2d" "Piechart3d" "HiddenLine" "OldFromNew" "BackGround"
"DualPrices" "FocalPoint" "PlotDevice" "SquareFree" "SolidLines"
"AxesOrigin" "FontFamily" "ForeGround" "ViewingBox" "Undirected"
"PointWidth" "ToInfinity" "PointStyle" "HlodeOverRF" "ReturnType"
"Transposed" "Horizontal" "Population" "Properties" "Smoothness"
"Continuous" "MaxRecLevel" "UsePrimeTab" "DashedLines" "DegreeOrder"
"SearchLevel" "AxesInFront" "AxesScaling" "CameraPoint" "ModuleTrace"
"NonNegative" "FloatFormat" "Irreducible" "DomainsOnly" "Independent"
"LinesPoints" "NewtonCotes" "Constrained" "MinStepsize" "NoOperators"
"MaxStepsize" "Interactive" "UnitVectors" "Homogeneous" "Transparent"
"ColorPatches" "SceneOptions" "AllExponents" "OutputDomain"
"Unsimplified" "CurvesPoints" "Unrestricted" "DefaultXRange"
"DefaultYRange" "FilledCircles" "GaussLegendre" "NoLeftVectors"
"FilledSquares" "UnConstrained" "RelativeError" "AbsoluteError"
"TitlePosition" "SplinesPoints" "DegInvLexOrder" "Interpolation"
"GridLinesColor" "GridLinesWidth" "LieGroupAction" "GridLinesStyle"
"RealValuesOnly" "HardwareFloats" "FixedPrecision" "PrincipalValue"
"BravaisPearson" "NoRightVectors" "GaussChebyshev" "TrailingZeroes"
"SoftwareFloats" "MinorExpansion" "StartingValues" "Transformation"
"MultiSolutions" "MaxRandomPoints" "ToMinusInfinity" "Diagonalization"
"TaylorExpansion" "ShowAssumptions" "ProtectLevelNone"
"RestrictedSearch" "SymbolicAnalysis" "BackSubstitution"
"IgnoreProperties" "StartingStepsize" "ProtectLevelError"
"AcceptSingularity" "IgnoreSpecialCases" "GaussTschebyscheff"
"UnrestrictedSearch" "ProtectLevelWarning" "DontRewriteBySystem"
"InverseTransformation" ))

(defconst mupad-types-list ;; anames(DOM_DOMAIN) 
'( "Plot" "DOM_NIL"
"DOM_RAT" "DOM_VAR" "DOM_INT" "DOM_SET" "DOM_FAIL" "DOM_EXEC"
"DOM_BOOL" "DOM_PROC" "DOM_NULL" "DOM_LIST" "DOM_EXPR" "DOM_POLY"
"DOM_TABLE" "DOM_FRAME" "DOM_IDENT" "DOM_FLOAT" "DOM_ARRAY"
"DOM_POINT" "DOM_DOMAIN" "DOM_STRING" "specfunc" "DOM_COMPLEX"
"DOM_POLYGON" "DOM_INTERVAL" "DOM_FUNC_ENV" "DOM_PROC_ENV"
"ClientObject" ))

(defconst mupad-libraries-list ;; all libraries except Types
;;stdlib::LIBRARIES ; 36 ;; ont disparu : piecewise domains O Pref misc
;;pourquoi pas misc ??  ;; ajoute : ode ?? n'existe pas !  
;; SURTOUT OTER stdlib !!!
'("student"
"specfunc" "Cat" "prog" "linopt" "Type" "RGB" "Dom" "Ax" "fp"
"property" "combinat" "module" "adt" "numlib" "detools" "matchlib"
"polylib" "output" "transform" "Series" "generate" "stats" "Network"
"orthpoly" "stringlib" "solvelib" "intlib" "numeric" "listlib"
"groebner" "import" "misc" "linalg" "plot" ))

(message "loading mupad-libs done")

(defconst mupad-libraries-completion-alist ;; all libraries with methods
'(
("Series" .
( "Puiseux" "gseries" ))
("Ax" .
("canonicalOrder" "canonicalRep" "canonicalUnitNormal"
"closedUnitNormals" "efficientOperation" "indetElements"
"noZeroDivisors" "normalRep" "systemRep" ))
("Cat" .  ( "AbelianGroup"
"AbelianMonoid" "AbelianSemiGroup" "Algebra" "BaseCategory"
"CancellationAbelianMonoid" "CommutativeRing" "DifferentialFunction"
"DifferentialRing" "DifferentialVariable" "EntireRing"
"EuclideanDomain" "FactorialDomain" "Field" "FiniteCollection"
"FiniteMonoidRing" "GcdDomain" "Group" "HomogeneousFiniteCollection"
"HomogeneousFiniteProduct" "IntegralDomain" "InvolutiveDivision"
"LeftModule" "Matrix" "Module" "Monoid" "MonoidRing"
"OrderedAbelianMonoid" "OrderedMonoid" "OrderedSet"
"PartialDifferentialRing" "PermutationGroup" "Polynomial"
"PrincipalIdealDomain" "QuotientField" "RightModule" "Ring" "Rng"
"SemiGroup" "Set" "SkewField" "SolvableAlgebra" "SquareMatrix"
"UnivariatePolynomial" "UnivariateSkewPolynomial" "VectorSpace" ))
("Dom" . 
( "AlgebraicExtension" "ArithmeticalExpression" "BaseDomain"
"Complex" "DifferentialExpression" "DifferentialFunction"
"DifferentialPolynomial" "DifferentialVariable" "DihedralGroup"
"DistributedPolynomial" "Expression" "ExpressionField"
"ExteriorAlgebra" "ExteriorProduct" "Float" "FloatIV" "Fraction"
"FreeModule" "GaloisField" "HomogenisedWeylAlgebra" "Ideal" "ImageSet"
"Integer" "IntegerMod" "Interval" "JanetDivision" "JetVectorField"
"LiftedDivision" "LinearDifferentialFunction"
"LinearDifferentialOperator" "LinearOrdinaryDifferentialOperator"
"Matrix" "MatrixGroup" "MonoidAlgebra" "MonoidOperatorAlgebra"
"MonomOrdering" "Multiset" "MultivariatePolynomial"
"MultivariateSeries" "Numerical" "PermutationGroup" "Polynomial"
"PommaretDivision" "PowerProduct" "Product" "Quaternion" "Rational"
"Real" "RestrictedDifferentialVariable" "SparseMatrix"
"SparseMatrixF2" "SquareMatrix" "SymmetricGroup" "TensorAlgebra"
"TensorProduct" "UnivariatePolynomial" "UnivariateSkewPolynomial"
"VectorField" "WeylAlgebra" ))
("Network" . 
( "addEdge" "addVertex"
"admissibleFlow" "allShortPath" "blockflow" "changeEdge"
"changeVertex" "complete" "convertSSQ" "cycle" "delEdge" "delVertex"
"eCapacity" "eWeight" "edge" "epost" "epre" "inDegree" "isEdge"
"isVertex" "longPath" "maxFlow" "minCost" "minCut" "outDegree"
"printGraph" "random" "residualNetwork" "shortPath" "shortPathTo"
"showGraph" "topSort" "vWeight" "vertex" ))
("RGB" . 
( "AliceBlue"
"AlizarinCrimson" "Antique" "Aquamarine" "AquamarineMedium"
"AureolineYellow" "Azure" "Banana" "Beige" "Bisque" "Black"
"BlanchedAlmond" "Blue" "BlueLight" "BlueMedium" "BlueViolet" "Brick"
"Brown" "BrownOadder" "BrownOchre" "Burlywood" "BurntSienna"
"BurntUmber" "Cadet" "CadmiumLemon" "CadmiumOrange" "CadmiumRedDeep"
"CadmiumRedLight" "CadmiumYellow" "CadmiumYellowLight" "Carrot"
"Cerulean" "Chartreuse" "Chocolate" "ChromeOxideGreen" "CinnabarGreen"
"Cobalt" "CobaltGreen" "CobaltVioletDeep" "ColdGray" "ColdGrey"
"ColorNames" "Coral" "CoralLight" "CornflowerBlue" "Cornsilk" "Cyan"
"CyanWhite" "DarkOrange" "DeepOchre" "DeepPink" "DimGray" "DimGrey"
"DodgerBlue" "Eggshell" "EmeraldGreen" "EnglishRed" "Firebrick"
"Flesh" "FleshOchre" "Floral" "ForestGreen" "Gainsboro" "GeraniumLake"
"Ghost" "Gold" "GoldOchre" "Goldenrod" "GoldenrodDark"
"GoldenrodLight" "GoldenrodPale" "Gray" "Green" "GreenDark"
"GreenPale" "GreenYellow" "GreenishUmber" "Grey" "Honeydew" "HotPink"
"IndianRed" "Indigo" "Ivory" "IvoryBlack" "Khaki" "KhakiDark"
"LampBlack" "Lavender" "LavenderBlush" "LawnGreen" "LemonChiffon"
"LightBeige" "LightGoldenrod" "LightGray" "LightGrey" "LightSalmon"
"LimeGreen" "Linen" "MadderLakeDeep" "Magenta" "ManganeseBlue"
"Maroon" "MarsOrange" "MarsYellow" "Melon" "MidnightBlue" "Mint"
"MintCream" "MistyRose" "Moccasin" "NaplesYellowDeep" "Navajo" "Navy"
"NavyBlue" "OldLace" "Olive" "OliveDrab" "OliveGreenDark" "Orange"
"OrangeRed" "Orchid" "OrchidDark" "OrchidMedium" "PapayaWhip" "Peach"
"PeachPuff" "Peacock" "PermanentGreen" "PermanentRedViolet" "Peru"
"Pink" "PinkLight" "Plum" "PowderBlue" "PrussianBlue" "Purple"
"PurpleMedium" "Raspberry" "RawSienna" "RawUmber" "Red" "RoseMadder"
"RosyBrown" "RoyalBlue" "SaddleBrown" "Salmon" "SandyBrown" "SapGreen"
"SeaGreen" "SeaGreenDark" "SeaGreenLight" "SeaGreenMedium" "Seashell"
"Sepia" "Sienna" "SkyBlue" "SkyBlueDeep" "SkyBlueLight" "SlateBlue"
"SlateBlueDark" "SlateBlueLight" "SlateBlueMedium" "SlateGray"
"SlateGrayDark" "SlateGrayLight" "SlateGrey" "SlateGreyDark"
"SlateGreyLight" "Smoke" "Snow" "SpringGreen" "SpringGreenMedium"
"SteelBlue" "SteelBlueLight" "TerreVerte" "Thistle" "Titanium"
"Tomato" "Turquoise" "TurquoiseBlue" "TurquoiseDark" "TurquoiseMedium"
"TurquoisePale" "Ultramarine" "UltramarineViolet" "VanDykeBrown"
"VenetianRed" "Violet" "VioletDark" "VioletRed" "VioletRedMedium"
"VioletRedPale" "ViridianLight" "WarmGray" "WarmGrey" "Wheat" "White"
"Yellow" "YellowBrown" "YellowGreen" "YellowLight" "YellowOchre"
"Zinc" ))
("Type" . 
( "AlgebraicConstant" "AnyType" "Arithmetical"
"Boolean" "Complex" "Constant" "ConstantIdents" "Equation" "Even"
"Function" "Imaginary" "IndepOf" "Indeterminate" "Integer" "Interval"
"ListOf" "ListProduct" "NegInt" "NegRat" "Negative" "NonNegInt"
"NonNegRat" "NonNegative" "NonZero" "Numeric" "Odd" "PolyExpr"
"PolyOf" "PosInt" "PosRat" "Positive" "Prime" "Product" "Property"
"RatExpr" "Rational" "Real" "Relation" "Residue" "SequenceOf" "Series"
"Set" "SetOf" "Singleton" "TableOfEntry" "TableOfIndex" "Union"
"Unknown" "Zero" ))
("adt" . 
( "Queue" "Stack" "Tree" ))
("combinat"
. 
( "bell" "cartesianProduct" "catalan" "compositions" "dyckWords"
"generators" "integerVectors" "integerVectorsWeighted" "modStirling"
"partitions" "permutations" "stirling1" "stirling2" "subsets"
"subwords" "tableaux" "warnDeprecated" "words" ))
("detools" . 
(
"CKsolve" "arbFuns" "autoreduce" "cartan" "charODESystem" "charSolve"
"characteristics" "derList2Tree" "detSys" "euler" "hasHamiltonian"
"hasPotential" "hilbert" "modode" "ncDetSys" "pdesolve" "transform" ))
("fp" . 
( "apply" "bottom" "curry" "expr_unapply" "fixargs" "fixedpt"
"fold" "nest" "nestvals" "unapply" ))
("generate" . 
( "C" "Macrofort"
"TeX" "fortran" "optimize " ))
("groebner" . 
( "dimension" "gbasis"
"normalf" "spoly" "stronglyIndependentSets" ))
("import" . 
(
"readdata" "readlisp" ))
("intlib" . 
( "byparts" "changevar" ))
("linalg" . 
( "addCol" "addRow" "adjoint" "angle" "basis" "charmat"
"charpoly" "col" "companion" "concatMatrix" "crossProduct" "curl"
"delCol" "delRow" "det" "divergence" "eigenvalues" "eigenvectors"
"expr2Matrix" "factorCholesky" "factorLU" "factorQR" "frobeniusForm"
"gaussElim" "gaussJordan" "grad" "hermiteForm" "hessenberg" "hessian"
"hilbert" "intBasis" "inverseLU" "invhilbert" "isHermitean" "isPosDef"
"isUnitary" "jacobian" "jordanForm" "laplacian" "matdim" "matlinsolve"
"matlinsolveLU" "minpoly" "multCol" "multRow" "ncols" "nonZeros"
"normalize" "nrows" "nullspace" "ogCoordTab" "orthog" "permanent"
"pseudoInverse" "randomMatrix" "rank" "row" "scalarProduct" "setCol"
"setRow" "smithForm" "stackMatrix" "submatrix" "substitute" "sumBasis"
"swapCol" "swapRow" "sylvester" "tr" "transpose" "vandermondeSolve"
"vecdim" "vectorPotential" "wiedemann" ))
("linopt" . 
( "Transparent"
"corners" "dual" "feasible" "maximize" "minimize" "plot_data"
"standardForm" ))
("listlib" . 
( "insert" "insertAt" "merge"
"removeDupSorted" "removeDuplicates" "setDifference" "singleMerge"
"sublist" ))
("matchlib" . 
( "analyze" "match" ))
("misc" . 
(
"breakmap" "genassop" "maprec" "pslq" ))
("module" . 
( "age"
"displace" "func" "help" "load" "max" "stat" "which" ))
("numeric" .
( "butcher" "complexRound" "cubicSpline" "cubicSpline2d" "det"
"eigenvalues" "eigenvectors" "expMatrix" "fMatrix" "factorCholesky"
"factorLU" "factorQR" "fft" "fsolve" "gldata" "gtdata" "indets" "int"
"inverse" "invfft" "leastSquares" "linsolve" "matlinsolve" "ncdata"
"ode2vectorfield" "odesolve" "odesolve2" "odesolveGeometric"
"polyroots" "polysysroots" "quadrature" "rationalize" "realroot"
"realroots" "singularvalues" "singularvectors" "solve" "sort"
"spectralradius" "startHardwareFloats" ))
("numlib" . 
( "Lambda"
"Omega" "contfrac" "decimal" "divisors" "ecm" "fibonacci" "fromAscii"
"g_adic" "ichrem" "igcdmult" "invphi" "ispower" "isquadres" "issqr"
"jacobi" "lambda" "legendre" "lincongruence" "mersenne" "moebius"
"mpqs" "mroots" "msqrts" "numdivisors" "numprimedivisors" "omega"
"order" "phi" "pollard" "prevprime" "primedivisors" "primroot"
"proveprime" "sigma" "sqrt2cfrac" "sumdivisors" "tau" "toAscii" ))
("orthpoly" . 
( "chebyshev1" "chebyshev2" "curtz" "gegenbauer"
"hermite" "jacobi" "laguerre" "legendre" ))
("output" . 
( "ordinal"
"tableForm" "tree " ))
("plot" . 
( "Curve2d" "Curve3d" "Ellipse2d"
"Function2d" "Function3d" "Group" "HOrbital" "Lsys" "Point"
"Pointlist" "Polygon" "Rectangle2d" "Scene" "Surface3d" "Turtle"
"bars" "boxplot" "contour" "copy" "cylindrical" "data" "density"
"implicit" "inequality" "line" "matrixplot" "modify" "ode"
"piechart2d" "piechart3d" "polar" "spherical" "surface" "vector"
"vectorfield" "xrotate" "yrotate" ))
("polylib" . 
( "Dpoly" "Poly"
"cyclotomic" "decompose" "discrim" "divisors" "elemSym" "makerat"
"minpoly" "primitiveElement" "primpart" "randpoly" "realroots"
"representByElemSym" "resultant" "sortMonomials" "splitfield"
"sqrfree" ))
("prog" . 
( "allFunctions" "calltree" "changes" "check"
"checkDisableMessage" "error" "exprtree" "find" "getOptions" "getname"
"init" "memuse" "ntime" "profile" "tcov" "test" "testexit" "testfunc"
"testinit" "testnum" "trace" "traced" "untrace" ))
("property" . 
(
"Null" "hasprop" "implies" "simpex" ))
("solvelib" . 
( "BasicSet"
"Union" "conditionalSort" "getElement" "isFinite" "pdioe" "preImage"
))
("specfunc" . 
( "Ci" "Ei" "Si" "abs" "arccos" "arccosh" "arccot"
"arccoth" "arccsc" "arccsch" "arcsec" "arcsech" "arcsin" "arcsinh"
"arctan" "arctanh" "arg" "bernoulli" "besselI" "besselJ" "besselK"
"besselY" "beta" "binomial" "ceil" "cos" "cosh" "cot" "coth" "csc"
"csch" "dilog" "dirac" "erf" "erfc" "exp" "fact" "floor" "frac"
"gamma" "heaviside" "hypergeom" "igamma" "lambertV" "lambertW" "ln"
"log" "meijerG" "polylog" "psi" "round" "sec" "sign" "signIm" "sin"
"sinh" "sqrt" "tan" "tanh" "trunc" "zeta" ))
("stats" . 
( "betaCDF"
"betaPDF" "betaQuantile" "betaRandom" "binomialCDF" "binomialPF"
"binomialQuantile" "binomialRandom" "calc" "cauchyCDF" "cauchyPDF"
"cauchyQuantile" "cauchyRandom" "chisquareCDF" "chisquarePDF"
"chisquareQuantile" "chisquareRandom" "col" "concatCol" "concatRow"
"correlation" "covariance" "csGOFT" "empiricalCDF" "empiricalQuantile"
"equiprobableCells" "erlangCDF" "erlangPDF" "erlangQuantile"
"erlangRandom" "exponentialCDF" "exponentialPDF" "exponentialQuantile"
"exponentialRandom" "fCDF" "fPDF" "fQuantile" "fRandom" "gammaCDF"
"gammaPDF" "gammaQuantile" "gammaRandom" "geometricCDF"
"geometricMean" "geometricPF" "geometricQuantile" "geometricRandom"
"harmonicMean" "hypergeometricCDF" "hypergeometricPF"
"hypergeometricQuantile" "hypergeometricRandom" "ksGOFT" "kurtosis"
"linReg" "logisticCDF" "logisticPDF" "logisticQuantile"
"logisticRandom" "mean" "meandev" "median" "modal" "moment"
"normalCDF" "normalPDF" "normalQuantile" "normalRandom" "obliquity"
"poissonCDF" "poissonPF" "poissonQuantile" "poissonRandom"
"quadraticMean" "reg" "row" "sample" "sample2list" "selectRow"
"sortSample" "stdev" "swGOFT" "tCDF" "tPDF" "tQuantile" "tRandom"
"tTest" "tabulate" "uniformCDF" "uniformPDF" "uniformQuantile"
"uniformRandom" "unzipCol" "variance" "weibullCDF" "weibullPDF"
"weibullQuantile" "weibullRandom" "zipCol" ))
("stringlib" . 
(
"contains" "format" "formatf" "lower" "match" "pos" "remove" "subs"
"subsop" "upper" ))
("student" . 
("Kn" "equateMatrix" "isFree"
"plotRiemann" "plotSimpson" "plotTrapezoid" "riemann" "simpson"
"trapezoid" ))
("transform" . 
( "fourier" "invfourier" "invlaplace"
"laplace" )) ))

(message "loading mupad-cpl done")

(provide'mupad-cpl)