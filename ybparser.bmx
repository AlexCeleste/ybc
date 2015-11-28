
' YBC parser
' from the B grammar found in "User's Reference to B"

SuperStrict

Import "TMeta.bmx"			'Get this here: http://www.blitzbasic.com/codearcs/codearcs.php?code=3090


Function YBParseFile:TParseNode(file:String)
	Global p:YBParser, l:TLexer
	
	If FileType(file) = 0 Then Throw "File '" + file + "' does not exist" ElseIf FileType(file) <> 1 Then Throw "'" + file + "' is not a valid source file"
	If p = Null Then p = New YBParser
	If l = Null Then l = YBLexer.Get()
	
	Local toks:TList = ListFromArray(l.ScanFile(file))
	
	Local k:TLink = toks.FirstLink() ; If k Then k = k.NextLink()	'Normalize token stream (remove duplicate separators and handle character escapes)
	While k
		Local t:TToken = TToken(k.Value())
		If t.ttype = "separator" And TToken(k.PrevLink().Value()).ttype = "separator"
			k.Remove()
		ElseIf t.ttype = "cconst" Or t.ttype = "sconst"
			t.value = ApplyCharEscapes(t.value)
		EndIf
		k = k.NextLink()
	Wend
	
	Local tree:TParseNode = p.Parse(TToken[](toks.ToArray()))
	
	Local newTop:TList = CreateList()	'Normalize toplevel (remove null statements)
	For Local n:TParseNode = EachIn tree.elem
		If n.rule <> "NullSt" Then newTop.AddLast n
	Next
	tree.elem = TParseNode[](newTop.ToArray())
	
	Return tree
End Function


Type YBParser Extends TMetaParser Final
	
	Field grammar:TMap {..
		Program = "Definition* : @ ^"..
	..
		Definition = "FunDef | VarDef | NullSt"..
			VarDef = "%name VarDefDim? VarDefVal* IVal? %!separator : @name @dim @inits < -"..
				VarDefDim = "%lbracket ! Constant? %!rbracket : @ @dim @ ^"..	'intentionally not dropped
				VarDefVal = "IVal %comma : @ - ^"..
			FunDef = "%name %lparen ! NameRep* %name? %!rparen Statement : @name - @args < - @body"..
				NameRep = "%name %comma : @ - ^"..
	..
		IVal = "Constant | %name"..
		Constant = "%iconst | %cconst | %sconst"..
	..
		Statement = "LocalVarSt | ExtrnVarSt | CaseSt | BreakSt | DefaultSt | LabelSt | BlockSt | IfSt | WhileSt | SwitchSt | GotoSt | ReturnSt | ValSt | NullSt"..
			LocalVarSt = "%auto ! LocalVarRep* LocalVarDef %!separator : - @names < -"..
				LocalVarDef = "%name (VarDefDim | Constant?) : @name @dim"..
				LocalVarRep = "LocalVarDef %comma : @ - ^"..
			ExtrnVarSt = "%extrn ! NameRep* %!name %separator : - @names < -"..
			LabelSt = "%name %colon : @name -"..
			CaseSt = "%case ! Constant %!colon : - @case -"..
			BreakSt = "%break %!separator : @break -"..
			DefaultSt = "%default %!colon : @default -"..
			BlockSt = "%lbrace ! StatementList %!rbrace : - @ - ^"..
				StatementList = "Statement*"..
			IfSt = "%if ! %!lparen RValue %!rparen Statement ElseSt? : - - @expr - @then @else"..
				ElseSt = "%else Statement : - @ ^"..
			WhileSt = "%while ! %!lparen RValue %!rparen Statement : - - @expr - @body"..
			SwitchSt = "%switch ! RValue Statement : - @expr @body"..
			GotoSt = "%goto RValue %!separator : - @dst -"..
			ReturnSt = "%return ! ReturnVal? %!separator : @ @val -"..	'not dropped
				ReturnVal = "%lparen ! RValue %!rparen : - @ - ^"..
			ValSt = "RValue %!separator : @val -"..
			NullSt = "%separator : @nil"..
	..
		RValue = "AssignLeft* TernaryExpr : @L @R ^"..
			AssignLeft = "UnaryExpr AssignOp : @lv @op"..
			AssignOp = "%equals | %aor | %aand | %aeq | %aneq | %alt | %aleq | %agt | %ageq | %ashl | %ashr | %aminus | %aplus | %amod | %atimes | %adiv"..
		TernaryExpr = "OrExpr TernaryBody? : @expr @tail ^"..
			TernaryBody = "%qmark ! RValue %!colon RValue : - @then - @else"..
		OrExpr = "AndExpr (%or AndExpr)* : @L @R ^"..
		AndExpr = "EqExpr (%and EqExpr)* : @L @R ^"..
		EqExpr = "RelExpr ((%eq | %neq) RelExpr)* : @L @R ^"..
		RelExpr = "ShiftExpr ((%lt | %leq | %gt | %geq) ShiftExpr)* : @L @R ^"..
		ShiftExpr = "AddExpr ((%shl | %shr) AddExpr)* : @L @R ^"..
		AddExpr = "MulExpr ((%plus | %minus) MulExpr)* : @L @R ^"..
		MulExpr = "UnaryExpr ((%times | %div | %mod) UnaryExpr)* : @L @R ^"..
		UnaryExpr = "(%minus | %plus | %and | %not | %times | %incr | %decr)* PostfixExpr : @op @val ^"..
		PostfixExpr = "AtomicExpr (Index | FunCall | %incr | %decr)* : @val @op ^"..
			Index = "%lbracket ! RValue %!rbracket : - @elem -"..
			FunCall = "%lparen ! RValueRep* RValue? %!rparen : @ @args < @"..	'not dropped
				RValueRep = "RValue %comma : @ - ^"..
		AtomicExpr = "Constant | %name | ParenExpr"..
			ParenExpr = "%lparen ! RValue %!rparen : - @ - ^"..
	}
	
End Type

Type YBLexer Final
	Function Get:TLexer()
		Global Store(_:TLexer) = TLexAction.Store, Mode(_:TLexer) = TLexAction.Mode, Discard(_:TLexer) = TLexAction.Discard
		
		Global l:TLexer = TLexer.withRules([..
			R("[0-9]*", Store, "iconst"),..				'int
			R("'([^']|\*')*'", Store, "cconst"),..		'char
			R("~q([^~q]|\*~q)*~q", Store, "sconst"),..	'string
		..
			R("/\*", Mode, "COMMENT", ""),..	'C89-style comments only
			R(".", Discard, "", "COMMENT"),..
			R("\*/", Mode, "", "COMMENT"),..
		..
			R("\+\+", Store, "incr"),..		'Any Regex operators need to be escaped with \
			R("--", Store, "decr"),..
			R("!",  Store, "not"),..
			R("\|", Store, "or"),..
			R("&",  Store, "and"),..
			R("==", Store, "eq"),..
			R("!=", Store, "neq"),..
			R("<",  Store, "lt"),..
			R("<=", Store, "leq"),..
			R(">",  Store, "gt"),..
			R(">=", Store, "geq"),..
			R("<<", Store, "shr"),..
			R(">>", Store, "shl"),..
			R("-",  Store, "minus"),..
			R("\+", Store, "plus"),..
			R("%",  Store, "mod"),..
			R("\*", Store, "times"),..
			R("/",  Store, "div"),..
			R("\?", Store, "qmark"),..
			R(":",  Store, "colon"),..
			R(";",  Store, "separator"),..
			R("\(", Store, "lparen"),..
			R("\)", Store, "rparen"),..
			R("\[", Store, "lbracket"),..
			R("\]", Store, "rbracket"),..
			R("\{", Store, "lbrace"),..
			R("\}", Store, "rbrace"),..
			R(",",  Store, "comma"),..
		..
			R("=",   Store, "equals"),..	'assignment
			R("=\|", Store, "aor"),..
			R("=&",  Store, "aand"),..
			R("===", Store, "aeq"),..
			R("=!=", Store, "aneq"),..
			R("=<",  Store, "alt"),..
			R("=<=", Store, "aleq"),..
			R("=>",  Store, "agt"),..
			R("=>=", Store, "ageq"),..
			R("=<<", Store, "ashr"),..
			R("=>>", Store, "ashl"),..
			R("=-",  Store, "aminus"),..
			R("=\+", Store, "aplus"),..
			R("=%",  Store, "amod"),..
			R("=\*", Store, "atimes"),..
			R("=/",  Store, "adiv"),..
		..
			R("auto",    Store, "auto"),..	'Keywords
			R("extrn",   Store, "extrn"),..
			R("case",    Store, "case"),..
			R("default", Store, "default"),..
			R("break",   Store, "break"),..
			R("if",      Store, "if"),..
			R("else",    Store, "else"),..
			R("while",   Store, "while"),..
			R("switch",  Store, "switch"),..
			R("goto",    Store, "goto"),..
			R("return",  Store, "return"),..
		..
			R("[a-z_.][a-z0-9_.]*", Store, "name"),..
		..
			R("[^[:space:]]", TLexAction.Error)..		'Raise an error over any other printable character
		])
		l.SetCaseSensitivity False
		l.SetGuardMode False
		
		Return l
		Function R:TLexRule(r:String, a(l:TLexer), res:String = "", m:String = "")
			Return TLexRule.Create(r, a, res, m)
		End Function
	End Function
End Type


Private

Function ApplyCharEscapes:String(s:String)
	s = s[1..s.Length - 1]
	s = s.Replace("*0", "\0")
	s = s.Replace("*e", "\0x03")
	s = s.Replace("*(", "{")
	s = s.Replace("*)", "}")
	s = s.Replace("*t", "\t")
	s = s.Replace("**", "*")
	s = s.Replace("*'", "'")
	s = s.Replace("*~q", "\~q")
	s = s.Replace("*n", "\n")
	Return s
End Function

Public

