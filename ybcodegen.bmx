
' YBC code generator
' This sloppy code generator works directly off the parse tree with no intermediate form
' No optimizations performed. If we want them we'll use LLVM

SuperStrict

Import "TMeta.bmx"
Include "ybexpressions.bmx"

Type YBVarDef Final
	Field name:String
	Field dsz:Int, isz:Int, isvec:Int, inits:TList
	Method New() ; inits = CreateList() ; End Method
End Type
Type YBFunDef Final
	Field name:String, framesz:Int, calleesave:Int, callersave:Int, stackargs:Int, tempdepth:Int, tempmax:Int
	Field autos:TList, extrns:TList, unknowns:TList, labels:TList, ins:TList, swstack:TList, scstack:TList
	Method New()
		ins = CreateList() ; autos = CreateList() ; extrns = CreateList()
		unknowns = CreateList() ; labels = CreateList() ; swstack = CreateList() ; scstack = CreateList()
	End Method
End Type

Type YBCodeGen Final
	Global syms:TList = CreateList(), vars:TList = CreateList(), funs:TList = CreateList(), strs:TList = CreateList()
	
	Function Build(tree:TParseNode)
		labelCounter = 0
		' Generate code directly from expression tree
		For Local n:TParseNode = EachIn tree.elem
			' Add the name of a definition to the global export table
			Local name:String = n.GetElem("name").term.value
			syms.AddLast(name)
			
			If n.rule = "VarDef"
				' For variables, emit .long * number of initialized elements plus .fill (init-size), 4 for the rest
				'   or just .long K for scalars
				BuildVar n, name
			Else
				' For functions, work through statement by statement
				'   fold up constant expressions
				'   emit a template for each control structure
				'   load the value of expression statements into EAX
				'   add extrn and auto variables to the function's symbol tables
				'   look up names against auto then extrn tables; if not found, add to the "undeclared" table and treat as a local label
				'	warn about case statements outside switch
				'	error if the unknown stack has any labels left in it at the end
				BuildFun n, name
			EndIf
		Next	
	End Function
	
	Function Platformize:String(name:String)
		?MacOS
			name = "_" + name
		?Win32
			name = "_" + name
		?
		Return name
	End Function
	
	Function SetWarningLevel(level:Int)
		Select level
			Case 0 ; CGError.WErrors = 0 ; CGError.WSilent = 1
			Case 1 ; CGError.WErrors = 0 ; CGError.WSilent = 0
			Case 2 ; CGError.WErrors = 1 ; CGError.WSilent = 0
		End Select
	End Function
End Type


Private

Type AutoVar Final
	Field name:String, pos:Int, sz:Int
End Type
Type Unknown Final
	Field name:String, t:TToken
End Type
Type Switch Final
	Field cases:Int[], caselabels:String[], pastBody:String, prev:TLink, defd:Int, issw:Int
End Type

Global labelCounter:Int

Function BuildVar(n:TParseNode, name:String)
	Local v:YBVarDef = New YBVarDef, sz:TParseNode = n.GetElem("dim"), inits:TParseNode = n.GetElem("inits")
	v.name = name ; CheckDups name, n.GetElem("name").term
	If sz
		v.isvec = 1
		sz = sz.GetElem("dim") ; If sz Then v.dsz = Int(sz.term.value)
	EndIf
	If inits
		v.isz = inits.elem.Length
		For Local i:TParseNode = EachIn inits.elem
			Select i.term.ttype
				Case "sconst"
					v.inits.AddLast "STR$_" + YBCodeGen.strs.Count()
					YBCodeGen.strs.AddLast i.term.value
				Case "iconst"
					v.inits.AddLast i.term.value
				Case "name"
					v.inits.AddLast YBCodeGen.Platformize(i.term.value)
				Case "cconst"
					v.inits.AddLast String.FromInt(CharConstIVal(i.term.value))
			End Select
		Next
		If inits.elem.Length > 1 Then v.isvec = 1
	EndIf
	YBCodeGen.vars.AddLast v
End Function

Function BuildFun(n:TParseNode, name:String)
	Local f:YBFunDef = New YBFunDef
	f.name = name ; CheckDups name, n.GetElem("name").term
	Local args:TParseNode = n.GetElem("args"), body:TParseNode = n.GetElem("body")
	If args
		Local k:Int = -2
		For Local a:TParseNode = EachIn args.elem	'Start by adding all args to the auto stack
			Local v:AutoVar = New AutoVar
			v.name = a.term.value ; v.pos = k ; v.sz = 1 ; f.autos.AddLast v
			k :- 1	'Passed arguments are on the stack below the frame
		Next
	EndIf
	If body
		BuildStmtList f, body
	EndIf
	If f.unknowns.Count()
		For Local u:Unknown = EachIn f.unknowns
			Print CGError.Make(f, u.t, "undeclared identifier '" + u.name + "' used in expression").ToString()
		Next
		Throw "unable to link unknown names"
	EndIf
	YBCodeGen.funs.AddLast f
End Function

Function BuildStmtList(f:YBFunDef, body:TParseNode)
	Local elem:TParseNode[]
	If body.rule = "StatementList" Then elem = body.elem Else elem = [body]
	f.scstack.AddLast CreateList()
	For Local s:TParseNode = EachIn elem
		Select s.rule
			Case "LocalVarSt" ; BuildLocalVarSt f, s
			Case "ExtrnVarSt" ; BuildExtrnVarSt f, s
			Case "LabelSt"    ; BuildLabelSt f, s
			Case "CaseSt"     ; BuildCaseSt f, s
			Case "BreakSt"    ; BuildBreakSt f, s
			Case "DefaultSt"  ; BuildDefaultSt f, s
			Case "StatementList" ; BuildStmtList f, s
			Case "IfSt"       ; BuildIfSt f, s
			Case "WhileSt"    ; BuildWhileSt f, s
			Case "SwitchSt"   ; BuildSwitchSt f, s
			Case "GotoSt"     ; BuildGotoSt f, s
			Case "ReturnSt"   ; BuildReturnSt f, s
			Case "ValSt"      ; Expr.Compile f, s.GetElem("val")
			Case "NullSt"     ;
		End Select
	Next
	For Local v:AutoVar = EachIn TList(f.scstack.Last())
		f.autos.RemoveLast
	Next
	For Local v:String = EachIn TList(f.scstack.RemoveLast())
		f.extrns.RemoveLast
	Next
End Function

Function BuildLocalVarSt(f:YBFunDef, s:TParseNode)
	For Local a:TParseNode = EachIn s.GetElem("names").elem
		Local top:Int = 0 ; If f.framesz > 0 Then top = AutoVar(f.autos.Last()).pos
		Local v:AutoVar = New AutoVar
		v.name = a.GetElem("name").term.value ; v.sz = 0
		Local sz:TParseNode = a.GetElem("dim") ; If sz
			If sz.term = Null Then sz = sz.GetElem("dim")	'bracketed
			If sz.term.ttype = "sconst"
				Throw CGError.Make(f, a.GetElem("name").term, "cannot create a vector with a string as its size; did you mean to use a char constant?")
			Else
				If sz.term.ttype = "iconst" Then v.sz = Int(sz.term.value) Else v.sz = CharConstIVal(sz.term.value)
			EndIf
			v.pos = top + v.sz + 1
			f.ins.AddLast "lea " + -(v.pos * 4 - 4) + "(%ebp), %eax ; mov %eax, " + -(v.pos * 4) + "(%ebp)"
		Else
			v.pos = top + 1
		EndIf
		f.framesz = Max(f.framesz, v.pos)
		For Local v2:AutoVar = EachIn TList(f.scstack.Last())
			If v2.name = v.name Then Throw CGError.Make(f, a.GetElem("name").term, "duplicate auto variable '" + v.name + "' in scope block")
		Next
		f.autos.AddLast v ; TList(f.scstack.Last()).AddLast v
	Next
End Function

Function BuildExtrnVarSt(f:YBFunDef, s:TParseNode)
	For Local v:TParseNode = EachIn s.GetElem("names").elem
		For Local v2:String = EachIn TList(f.scstack.Last())
			If v2 = v.term.value Then Throw CGError.Make(f, v.term, "duplicate extrn variable '" + v2 + "' in scope block")
		Next
		f.extrns.AddLast v.term.value ; TList(f.scstack.Last()).AddLast v.term.value
	Next
End Function

Function BuildLabelSt(f:YBFunDef, s:TParseNode)
	Local l:String = s.GetElem("name").term.value
	For Local u:Unknown = EachIn f.unknowns
		If l = u.name Then f.unknowns.Remove u
	Next
	f.labels.AddLast l
	f.ins.AddLast "^" + f.name + "$" + l + ":"
End Function

Function BuildCaseSt(f:YBFunDef, s:TParseNode)
	Local sw:Switch = GetLastSwitch(f), c:TParseNode = s.GetElem("case"), cval:Int = 0
	Select c.term.ttype
		Case "iconst"
			cval = Int(c.term.value)
		Case "cconst"
			cval = CharConstIVal(c.term.value)
		Case "sconst" ; Throw CGError.Make(f, c.term, "cannot use a string literal as a case value; try a char constant instead")
	End Select
	If sw
		For Local i:Int = 0 Until sw.cases.Length
			If sw.cases[i] = cval Then Throw CGError.Make(f, c.term, "duplicate case value '" + c.term.value + "' in switch block")
		Next
		sw.cases :+ [cval] ; sw.caselabels :+ [GetAnonLabel()]
		f.ins.AddLast "^" + sw.caselabels[sw.caselabels.Length - 1] + ":"
	Else
		CGError.Warn f, c.term, "'case' statement without surrounding switch, does nothing"
	EndIf
End Function

Function BuildBreakSt(f:YBFunDef, s:TParseNode)
	Local sw:Switch = Switch(f.swstack.Last())	'because it could be a while
	If sw
		f.ins.AddLast "jmp " + sw.pastBody
	Else
		CGError.Warn f, s.GetElem("break").term, "'break' statement without surrounding switch or loop, does nothing"
	EndIf
End Function

Function BuildDefaultSt(f:YBFunDef, s:TParseNode)
	Local sw:Switch = GetLastSwitch(f)
	If sw
		If sw.defd Then Throw CGError.Make(f, s.GetElem("default").term, "duplicate 'default' target in switch block")
		Local d:String = GetAnonLabel()
		sw.prev = f.ins.InsertBeforeLink("jmp " + d, sw.prev)
		f.ins.AddLast "^" + d + ":"
		sw.defd = 1
	Else
		CGError.Warn f, s.GetElem("default").term, "'default' statement without surrounding switch, does nothing"
	EndIf
End Function

Function BuildSwitchSt(f:YBFunDef, s:TParseNode)
	Local sw:Switch = New Switch ; sw.issw = 1
	Expr.Compile f, s.GetElem("expr")
	sw.prev = f.ins.LastLink()
	f.swstack.AddLast sw
	sw.pastBody = GetAnonLabel()
	sw.prev = f.ins.InsertAfterLink("jmp " + sw.pastBody, sw.prev)
	BuildStmtList f, s.GetElem("body")
	f.ins.AddLast "^" + sw.pastBody + ":"
	For Local c:Int = 0 Until sw.cases.Length
		f.ins.InsertBeforeLink("cmp $" + sw.cases[c] + ", %eax", sw.prev)
		f.ins.InsertBeforeLink("jz " + sw.caselabels[c], sw.prev)
	Next
	f.swstack.RemoveLast
End Function

Function GetLastSwitch:Switch(f:YBFunDef)
	Local sl:TLink = f.swstack.LastLink()
	While Switch(sl.Value()).issw = 0 And sl <> Null
		sl = sl.PrevLink()
	Wend
	If sl Then Return Switch(sl.Value()) Else Return Null
End Function

Function BuildIfSt(f:YBFunDef, s:TParseNode)
	Expr.Compile f, s.GetElem("expr")
	Local pastThen:String = GetAnonLabel(), pastElse:String = GetAnonLabel(), e:TParseNode = s.GetElem("else")
	f.ins.AddLast "cmp $0, %eax"
	f.ins.AddLast "jz " + pastThen
	BuildStmtList f, s.GetElem("then")
	If e Then f.ins.AddLast "jmp " + pastElse
	f.ins.AddLast "^" + pastThen + ":"
	If e
		BuildStmtList f, e
		f.ins.AddLast "^" + pastElse + ":"
	EndIf
End Function

Function BuildWhileSt(f:YBFunDef, s:TParseNode)
	Local top:String = GetAnonLabel(), pastBody:String = GetAnonLabel()
	Local w:Switch = New Switch ; f.swstack.AddLast w ; w.pastBody = pastBody
	f.ins.AddLast "^" + top + ":"
	Expr.Compile f, s.GetElem("expr")
	f.ins.AddLast "cmp $0, %eax"
	f.ins.AddLast "jz " + pastBody
	BuildStmtList f, s.GetElem("body")
	f.ins.AddLast "jmp " + top
	f.ins.AddLast "^" + pastBody + ":"
	f.swstack.RemoveLast()
End Function

Function BuildGotoSt(f:YBFunDef, s:TParseNode)
	Expr.Compile f, s.GetElem("dst")
	f.ins.AddLast "jmp *%eax"
End Function

Function BuildReturnSt(f:YBFunDef, s:TParseNode)
	Local val:TParseNode = s.GetElem("val")
	If val Then Expr.Compile f, val
	f.ins.AddLast "jmp return$" + f.name
End Function

Function CheckDups(name:String, t:TToken)
	For Local f:YBFunDef = EachIn YBCodeGen.funs
		If f.name = name Then Throw CGError.Make(Null, t, "duplicate toplevel definition '" + name + "'")
	Next
	For Local v:YBVarDef = EachIn YBCodeGen.vars
		If v.name = name Then Throw CGError.Make(Null, t, "duplicate toplevel definition '" + name + "'")
	Next
End Function

Function CharConstIVal:Int(cc:String)
	cc = cc.Replace("\0", Chr(0))
	cc = cc.Replace("\0x03", Chr(3))
	cc = cc.Replace("\t", Chr("~t"[0]))
	cc = cc.Replace("\~q", Chr("~q"[0]))
	cc = cc.Replace("\n", Chr("~n"[0]))
	
	Local cval:Int = 0
	For Local ch:Int = 0 Until cc.Length
		cval :+ (cc[ch] Shl (8 * ch))
	Next
	Return cval
End Function

Function GetAnonLabel:String()
	Local s:String = "_$" + labelCounter
	labelCounter :+ 1
	Return s
End Function

Type CGError
	Field msg:String
	Method ToString:String()
		Return "Code generator: " + msg
	End Method
	Function Make:CGError(f:YBFunDef, t:TToken, msg:String = "")
		Local e:CGError = New Self, def:String ; If f Then def = "::" + f.name
		e.msg = "error in " + t.file + def + " at line " + t.l + ", col " + t.c + ":  " + msg
		Return e
	End Function
	Function Warn(f:YBFunDef, t:TToken, msg:String)
		If WErrors
			Throw Make(f, t, msg)
		ElseIf Not WSilent
			Local def:String =  "" ; If f Then def = "::" + f.name
			Print "warning: in " + t.file + def + " at line " + t.l + ", col " + t.c + ":  " + msg
		EndIf
	End Function
	Global WErrors:Int, WSilent:Int
End Type

Public

