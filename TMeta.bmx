
' Generic parsing and lexing system, with BNF-style lightweight input
'=====================================================================

Import BaH.Regex
SuperStrict

Import Brl.Retro
Import Brl.LinkedList
Import Brl.Map
Import Brl.Reflection

Public

' TMetaParser
'=============

Type TMetaParser Extends TParser Abstract
	Field _rules:TMap, _start:String, _linked:Int
	Global pp:PMeta
	Method Parse:TParseNode(toks:TToken[])
		If Not _linked Then Link()
		Return Super.Parse(toks)
	End Method
	Method Link()
		Local metaData:TToken[] = MetaLexer.ScanString(TTypeId.ForObject(Self).FindField("grammar")._meta)
		_rules = CreateMap()
		If pp = Null Then pp = New PMeta	'Not safe to initialize globally
		For Local i:Int = 0 Until metaData.Length Step 2
			Local val:String = metaData[i + 1].value[1..]
			AddRule(metaData[i].value, val[..val.Length - 1])
		Next
		_start = metaData[0].value
		_linked = True
	End Method
	Method AddRule(key:String, rs:String)
		Local rp:String[] = rs.Split(":")
		Local rule:TParseRule = CompileRule(pp.Parse(BNFLexer.ScanString(rp[0])))
		If rp.Length > 1 Then rule.f(rp[1].Trim())
		_rules.Insert(key, rule)
	End Method
	Method CompileRule:TParseRule(r:TParseNode)
		If r.term
			If r.term.tType = "err" Then Return Commit()
			If r.term.value[0..2] = "%!" Then Return ExpectTT(r.term.value[2..])
			If r.term.value[0] = "%"[0] Then Return CheckTT(r.term.value[1..])
			Return Self.R(r.term.value)
		EndIf
		Select r.rule
			Case "ralt"
				Local fst:TParseRule = CompileRule(r.elem[0])
				If r.elem[1].elem[0].term
					r.elem[1] = r.elem[1].elem[1]
					Return Alt([ fst, CompileRule(r.elem[1]) ])
				Else
					For Local i:Int = 0 Until r.elem[1].elem.Length
						r.elem[1].elem[i] = r.elem[1].elem[i].elem[1]
					Next
					Local rst:TParseRule[] = MapCompile(r.elem[1].elem)
					rst = rst[..rst.Length + 1]
					For Local i:Int = rst.Length - 1 To 1 Step -1
						rst[i] = rst[i - 1]
					Next
					rst[0] = fst
					Return Alt(rst)
				EndIf
			Case "rcat"
				Return Cat(MapCompile(r.elem))
			Case "unar"
				Local arg:TParseRule = CompileRule(r.elem[0])
				Select r.elem[1].term.tType
					Case "plus" ; Return Plus([arg])
					Case "opt"  ; Return Opt([arg])
					Case "star" ; Return Rep([arg])
				End Select
		End Select
	End Method
	Method MapCompile:TParseRule[](a:TParseNode[])
		Local ret:TParseRule[] = New TParseRule[a.Length]
		For Local i:Int = 0 Until a.Length
			ret[i] = CompileRule(a[i])
		Next
		Return ret
	End Method
	Method startRule:TParseRule()
		Return TParseRule(_rules.ValueForKey(_start))
	End Method
	Method GetNamedRule:TParseRule(r:String)
		Return TParseRule(_rules.ValueForKey(r))
	End Method
End Type

Private

Global MetaLexer:TLexer = TLexer.withRules([..
	R("[a-z_][a-z0-9_]*", TLexAction.Store, "key"),..
	R("~q[^~q]*~q", TLexAction.Store, "value")..
])

Global BNFLexer:TLexer = TLexer.withRules([..
	R("[a-z_][a-z0-9_]*", TLexAction.Store, "name"),..
	R("%[a-z_][a-z0-9_]*", TLexAction.Store, "terminal"),..
	R("%![a-z_][a-z0-9_]*", TLexAction.Store, "terminal"),..
..
	R("\+", TLexAction.Store, "plus"),..
	R("\|", TLexAction.Store, "or"),..
	R("\?", TLexAction.Store, "opt"),..
	R("\*", TLexAction.Store, "star"),..
	R("=",  TLexAction.Store, "eql"),..
	R("!",  TLexAction.Store, "err"),..
	R("@",  TLexAction.Store, "any"),..
	R("\(", TLexAction.Store, "lparen"),..
	R("\)", TLexAction.Store, "rparen")..
])

Function R:TLexRule(r:String, a(l:TLexer), res:String = "", m:String = "")
	Return TLexRule.Create(r, a, res, m)
End Function

Type PMeta Extends TParser
	Field root:TParseRule, rcat:TParseRule, ralt:TParseRule, unar:TParseRule, atom:TParseRule, nest:TParseRule
	
	Method startRule:TParseRule()
		Return root
	End Method
	
	Method New()
		root = R("ralt")
		ralt = Cat([ R("rcat"), Rep([ CheckTT("or"), R("rcat") ]) ])
		rcat = Plus([ Alt([ CheckTT("err"), CheckTT("any"), R("unar") ]) ])
		unar = Cat([ R("atom"), Opt([ Alt([ CheckTT("plus"), CheckTT("opt"), CheckTT("star") ]) ]) ])
		atom = Alt([ CheckTT("name"), CheckTT("terminal"), R("nest") ])
		nest = Cat([ CheckTT("lparen"), R("ralt"), CheckTT("rparen") ]).f("- @ - ^")
	End Method
End Type

Public

' TParser
'=========

Type TParseNode
	Field name:String, rule:String, term:TToken
	Field elem:TParseNode[]
	Method n:TParseNode(s:String)
		name = s ; Return Self
	End Method
	Method ToString:String()
		Function show:String(n:TParseNode, pad:Int)
			Local id:String = LSet("", pad)
			If n.name = "#Nil" Then Return id + "[NIL]~n"
			If n.term Then Return id + n.name + ": { '" + n.term.value + "' : " + n.term.tType + " }~n"
			
			Local s:String = id + n.name + ": " + n.rule + "~n"
			For Local e:TParseNode = EachIn n.elem
				s :+ show(e, pad + 2)
			Next
			Return s
		End Function
		Return show(Self, 0)
	End Method
	Method GetElem:TParseNode(name:String)
		For Local e:Int = 0 Until elem.Length
			If elem[e].name = name Then Return elem[e]
		Next
		Return Null
	End Method
	
	Function Leaf:TParseNode(t:ttoken)
		Local n:TParseNode = New Self ; n.term = t ; Return n
	End Function
	Function Node:TParseNode(b:TParseNode[])
		Local n:TParseNode = New Self ; n.elem = b ; Return n
	End Function
	Function Nil:TParseNode()
		Local n:TParseNode = New Self ; n.name = "#Nil" ; Return n
	End Function
End Type

Type TParser Abstract
	Method Parse:TParseNode(toks:TToken[])
		Self.toks = toks ; ct = 0 ; epstk = Null ; edstk = Null ; rdepth = 0
		Local ret:TParseNode = startRule().run(Self)
		If ret Then ret.name = "@root"
		Return ret
	End Method
	
	Method Cat:TParseRule(rs:TParseRule[]) Final
		Return TParseRule.Make("Cat", Self, "", rs)
	End Method
	Method Alt:TParseRule(rs:TParseRule[]) Final
		Return TParseRule.Make("Alt", Self, "", rs)
	End Method
	Method Opt:TParseRule(rs:TParseRule[]) Final
		Return TParseRule.Make("Opt", Self, "", rs)
	End Method
	Method Rep:TParseRule(rs:TParseRule[]) Final
		Return TParseRule.Make("Rep", Self, "", rs)
	End Method
	Method Err:TParseRule(msg:String) Final
		Return TParseRule.Make("Err", Self, msg, Null)
	End Method
	Method Plus:TParseRule(rs:TParseRule[]) Final
		Return TParseRule.Make("Plus", Self, "", rs)
	End Method
	
	Method CheckTT:TParseRule(t:String) Final
		Return TParseRule.Make("CTT", Self, t, Null)
	End Method
	Method CheckVal:TParseRule(v:String) Final
		Return TParseRule.Make("CV", Self, v, Null)
	End Method
	Method ExpectTT:TParseRule(t:String) Final
		Return TParseRule.Make("ETT", Self, t, Null)
	End Method
	
	Method Commit:TParseRule() Final
		Return TParseRule.Make("Commit", Self, "", Null)
	End Method
	Method Any:TParseRule() Final
		Return TParseRule.Make("Any", Self, "", Null)
	End Method
	Method R:TParseRule(n:String) Final
		Return TParseRule.Make("Named", Self, n, Null)
	End Method
	
	Field toks:TToken[], ct:Int, epstk:Int[], edstk:Int[], rdepth:Int
	Method _ctok:TToken() Final
		If ct < toks.Length Then Return toks[ct] Else Return Null
	End Method
	Method _incr:TParseNode() Final
		Local tok:TToken = toks[ct] ; ct :+ 1
		Return TParseNode.Leaf(tok)
	End Method
	Method _back(pos:Int) Final
		If epstk
			Local ep:Int = epstk[epstk.Length - 1]
			If pos < ep Then Throw ParseError.Make(toks[ep], "error trying to complete '" + toks[ep - 1].value + "'")
		EndIf
		ct = pos
	End Method
	Method _popErrs() Final
		Local c:Int = 0
		For Local p:Int = edstk.Length - 1 To 0 Step -1
			If edstk[p] = rdepth Then c :+ 1 Else Exit
		Next
		If c
			edstk = edstk[.. edstk.Length - c]
			epstk = epstk[.. edstk.Length]
		EndIf
		rdepth :- 1
	End Method
	Method _pushErr() Final
		edstk :+ [rdepth] ; epstk :+ [ct]
	End Method
	Method GetNamedRule:TParseRule(r:String)
		Return TParseRule(TTypeId.ForObject(Self).FindField(r).Get(Self))
	End Method
	
	Method startRule:TParseRule() Abstract
End Type

Type TParseRule Abstract
	Field r:String, args:TParseRule[], _f:String[]
	Method run:TParseNode(p:TParser) Abstract
	Method f:TParseRule(filt:String)
		If args.Length = 1 And CatRule(args[0]) <> Null Then args[0].f filt
	End Method
	
	Function Make:TParseRule(subt:String, p:TParser, r:String, args:TParseRule[])
		Local t:TParseRule = TParseRule(TTypeId.ForName(subt + "Rule").NewObject())
		If args And args.Length > 1 And subt <> "Cat" And subt <> "Alt"
			args = [p.Cat(args)]
		EndIf
		t.r = r ; t.args = args
		Return t
	End Function
End Type

Private

Type CatRule Extends TParseRule
	Method run:TParseNode(p:TParser)
		Local pos:Int = p.ct, el:TParseNode[] = New TParseNode[args.Length], cct:Int = 0
		p.rdepth :+ 1
		For Local d:Int = 0 Until args.Length
			el[d - cct] = args[d].run(p)
			If el[d - cct] = Null
				p._back(pos) ; p._popErrs ; Return Null
			ElseIf el[d - cct] = CommitRule.Nil
				cct :+ 1
			EndIf
		Next
		If cct Then el = el[0..(el.Length - cct)]
		p._popErrs
		Return filter(el, p)
	End Method
	Method filter:TParseNode(el:TParseNode[], p:TParser)
		Local ls:TList = CreateList(), ct:Int = 0, noFold:Int = False
		If _f
			If _f.Length <= el.Length Then Throw ParseError.Make(p._ctok(), "filter pattern is wrong length for rule")
			For Local e:Int = 0 Until el.Length
				If el[e] <> OptRule.Nil
					If _f[e][0] = "@"[0]
						If _f[e].Length > 1 Then el[e].name = _f[e][1..] Else el[e].name = "@" + ct
						ls.AddLast el[e] ; ct :+ 1
					ElseIf _f[e][0] = "<"[0]
						Local prev:TParseNode = el[e - 1]
						If prev <> OptRule.Nil
							If prev.elem
								el[e].name = "@" + prev.elem.Length
								prev.elem = prev.elem + [el[e]]
							Else
								el[e].name = "@1"
								prev = TParseNode.Node([prev, el[e]])
								prev.name = prev.elem[0].name ; prev.elem[0].name = "@0"
								ls.RemoveLast() ; ls.AddLast prev
							EndIf
						Else
							el[e].name = "@0"
							prev = TParseNode.Node([el[e]])
							If _f[e - 1].Length > 1 Then prev.name = _f[e - 1][1..] Else prev.name = "@" + ct
							ls.AddLast prev ; ct :+ 1
						EndIf
					EndIf
				EndIf
			Next
			If _f[el.Length] <> "^" Then noFold = True
		Else
			For Local e:Int = 0 Until el.Length
				If el[e] <> OptRule.Nil
					ls.AddLast(el[e])
					el[e].name = "@" + ct ; ct :+ 1
				EndIf
			Next
		EndIf
		el = TParseNode[](ls.ToArray())
		If el = Null Then Return OptRule.Nil
		If (el.Length > 1) Or noFold Then Return TParseNode.Node(el) Else Return el[0]
	End Method
	Method f:TParseRule(filt:String)
		_f = filt.Split(" ")
		If _f[_f.Length - 1] <> "^"
			_f = _f[.._f.Length + 1] ; _f[_f.Length - 1] = ""
		EndIf
		Return Self
	End Method
End Type

Type AltRule Extends TParseRule
	Method run:TParseNode(p:TParser)
		Local pos:Int = p.ct
		For Local d:TParseRule = EachIn args
			Local n:TParseNode = d.run(p) ; If n Then Return n
			p._back(pos)
		Next
		Return Null
	End Method
	Method f:TParseRule(filt:String)
		RuntimeError "Cannot apply a filter string to an Alt rule"
	End Method
End Type

Type OptRule Extends TParseRule
	Global Nil:TParseNode = TParseNode.Nil()
	Method run:TParseNode(p:TParser)
		Local n:TParseNode = args[0].run(p)
		If n Then Return n Else Return Nil
	End Method
End Type

Type RepRule Extends TParseRule
	Method run:TParseNode(p:TParser)
		Return RunF(Self, p)
	End Method
	Function RunF:TParseNode(s:TParseRule, p:TParser)
		Local match:TParseNode, ls:TList
		Repeat
			Local pos:Int = p.ct
			match = s.args[0].run(p)
			If match = Null Or match = OptRule.Nil
				p._back(pos) ; Exit
			Else
				If ls = Null Then ls = CreateList()
				ls.AddLast(match)
			EndIf
		Forever
		If ls = Null
			Return OptRule.Nil
		Else
			Local ret:TParseNode[] = TParseNode[](ls.ToArray())
		'	If ret.Length > 1
				For Local i:Int = 0 Until ret.Length
					ret[i].name = "@" + i
				Next
				Return TParseNode.Node(ret)
		'	Else
		'		Return ret[0]
		'	EndIf
		EndIf
	End Function
End Type

Type ErrRule Extends TParseRule
	Method run:TParseNode(p:TParser)
		Throw ParseError.Make(p._ctok(), r)
	End Method
End Type

Type PlusRule Extends TParseRule
	Method run:TParseNode(p:TParser)
		Local ret:TParseNode = RepRule.RunF(Self, p)
		If ret = OptRule.Nil Then Return Null Else Return ret
	End Method
End Type

Type CTTRule Extends TParseRule
	Method run:TParseNode(p:TParser)
		If p._ctok() And p._ctok().tType = r Then Return p._incr() Else Return Null
	End Method
End Type

Type CVRule Extends TParseRule
	Method run:TParseNode(p:TParser)
		If p._ctok() And p._ctok().value = r Then Return p._incr() Else Return Null
	End Method
End Type

Type ETTRule Extends TParseRule
	Method run:TParseNode(p:TParser)
		If p._ctok() And p._ctok().tType = r Then Return p._incr() Else Throw ParseError.Gen(p._ctok(), r)
	End Method
End Type

Type CommitRule Extends TParseRule
	Global Nil:TParseNode = TParseNode.Nil()
	Method run:TParseNode(p:TParser)
		p._pushErr ; Return Nil
	End Method
End Type

Type AnyRule Extends TParseRule
	Method run:TParseNode(p:TParser)
		If p._ctok() Then Return p._incr() Else Return Null
	End Method
End Type

Type NamedRule Extends TParseRule
	Method run:TParseNode(p:TParser)
		Local ret:TParseNode = p.GetNamedRule(r).run(p)
		If ret And ret.rule = "" Then ret.rule = r
		Return ret
	End Method
End Type

Type ParseError
	Field msg:String
	Method ToString:String()
		Return "TParser: " + msg
	End Method
	Function Make:ParseError(t:TToken, msg:String = "")
		Local e:ParseError = New Self
		e.msg = "error in " + t.file + " at line " + t.l + ", col " + t.c + ":  " + msg
		Return e
	End Function
	Function Gen:ParseError(t:TToken, ex:String)
		Return Make(t, "expecting {" + ex + "} but found {" + t.tType + "}")
	End Function
End Type

Public

' TLexer
'========

Type TToken
	Field value:String, tType:String
	Field file:String, l:Int, c:Int
	Function Make:TToken(v:String, ty:String, f:String, l:Int, c:Int)
		Local t:TToken = New TToken
		t.value = v ; t.tType = ty ; t.file = f ; t.l = l ; t.c = c
		Return t
	End Function
End Type

Type TLexer
	Field rules:TLexRule[]
	Field cFile:LexFile
	Field out:TList, matchR:TLexRule, matchS:String
	Field csMode:Int, guardMode:Int, mode:String
	Field istk:TList, prev:TList
	
	Function withRules:TLexer(r:TLexRule[])
		Local l:TLexer = New TLexer
		l.rules = r
		l.Reset
		Return l
	End Function
	
	Method SetCaseSensitivity(cs:Int)
		csMode = cs
	End Method
	Method SetGuardMode(gm:Int)
		guardMode = gm
	End Method
	Method Reset()
		istk = CreateList() ; prev = CreateList() ; out = Null ; cFile = Null ; mode = ""
	End Method
	
	Method ScanFile:TToken[](name:String)
		Return TToken[](ScanLFile(Self, LexFile.fromFile(name)).ToArray())
	End Method
	Method ScanString:TToken[](s:String)
		Return TToken[](ScanLFile(Self, LexFile.fromString(s)).ToArray())
	End Method
	
	Method GuardFileName(f:LexFile)
		If guardMode Then prev.AddLast(f.dir + f.name)
	End Method
End Type

Type TLexRule
	Field rule:TRegEx, pattern:String
	Field action(l:TLexer), result:String, mode:String
	Function Create:TLexRule(rs:String, act(l:TLexer), result:String = "", mode:String = "")
		Local r:TLexRule = New TLexRule
		r.rule = TRegEx.Create("\G" + rs, Null) ; r.pattern = rs
		If result = "" And act = TLexAction.Store Then result = rs
		r.action = act ; r.result = result ; r.mode = mode
		Return r
	End Function
End Type

Type TLexAction
	Function Store(l:TLexer)
		Local fname:String = l.cFile.name ; If fname <> "<string>" Then fname :+ " [" + l.cFile.dir + fname + "]"
		l.out.AddLast TToken.Make(l.matchS, l.matchR.result, fname, l.cFile.cLine, l.cFile.cCol)
	End Function
	Function Mode(l:TLexer)
		l.mode = l.matchR.result
	End Function
	Function Error(l:TLexer)
		Throw LexError.Make(l, l.matchR.result)
	End Function
	Function Discard(l:TLexer)
	End Function
	Function Incl(l:TLexer)
		l.matchS = FilterIncludeString(l.matchS, l.matchR.result)	'Shorten the token to just the file path
		TryIncludeFile(l, l.matchS)
	End Function
End Type

Private

Type LexFile
	Field dir:String, name:String
	Field stream:String, sLen:Int, cPtr:Int, cLine:Int, cCol:Int
	Function fromFile:LexFile(name:String)
		name = RealPath(name)
		Local f:LexFile = New LexFile
		f.name = StripDir(name) ; f.dir = ExtractDir(name) + "/"
		f.stream = LoadText(name) ; f.sLen = Len(f.stream) ; f.cLine = 1 ; f.cCol = 1
		Return f
	End Function
	Function fromString:LexFile(s:String)
		Local f:LexFile = New LexFile
		f.name = "<string>" ; f.dir = ""
		f.stream = s ; f.sLen = Len(s) ; f.cLine = 1 ; f.cCol = 1
		Return f
	End Function
	Method Increment(count:Int)
		For Local c:Int = 1 To count
			If stream[cPtr] < 32	'Only count printable characters in the column field
				If stream[cPtr] = 10 Then cLine :+ 1 ; cCol = 1
			Else
				cCol :+ 1
			EndIf
			cPtr :+ 1
		Next
	End Method
End Type
Type LexError
	Field msg:String
	Function Make:LexError(l:TLexer, msg:String)
		Local e:LexError = New LexError, fname:String = l.cFile.name ; If fname <> "<string>" Then fname :+ " [" + l.cFile.dir + fname + "]"
		e.msg = "error in " + fname + " at line " + l.cFile.cLine + ", col " + l.cFile.cCol + ":  "
		If msg <> "" Then e.msg :+ msg Else e.msg :+ "unexpected character '" + l.cFile.stream[l.cFile.cPtr] + "'"
		Return e
	End Function
	Method ToString:String()
		Return "TLexer: " + msg
	End Method
End Type

Function ScanLFile:TList(l:TLexer, f:LexFile)
	l.cFile = f
	l.istk.AddLast f
	l.GuardFileName f
	
	TRegEx.options = New TRegExOptions
	TRegEx.options.targetIsMultiline = False
	TRegEx.options.caseSensitive = l.csMode
	
	l.mode = ""
	l.out = CreateList()
	Repeat
		Local token:String, rule:TLexRule
		
		While l.cFile.cPtr < l.cFile.sLen
			Local cf:LexFile = l.cFile
			token = "" ; rule = Null
			
			For Local r:TLexRule = EachIn l.rules
				If l.mode = r.mode
					Local cMatch:TRegExMatch = r.rule.Find(cf.stream, cf.cPtr)
					If cMatch
						If Len(cMatch.SubExp()) > Len(token) Then token = cMatch.SubExp() ; rule = r
					EndIf
				EndIf
			Next
			
			If rule		'Something matched successfully!
				l.matchS = token ; l.matchR = rule
				rule.action(l)
				cf.Increment Len(token)
			Else
				cf.Increment 1
			EndIf
			l.matchR = Null ; l.matchS = ""
		Wend
		
		If l.cFile <> f		'Pop back to the previous file in the Include 
			l.istk.RemoveLast
			l.cFile = LexFile(l.istk.Last())
		Else
			Exit	'If it's f, we're done
		EndIf
	Forever
	
	Local out:TList = l.out
	l.Reset
	Return out
End Function

' Use a simple set of filter chars to chop the path out of an Include directive
Function FilterIncludeString:String(inc:String, filter:String)
	For Local i:Int = 1 To Len(filter)
		Local p:Int = Instr(inc, Mid(filter, i, 1))
		If p Then inc = Mid(inc, p + 1) ; Exit
	Next
	For Local i:Int = 1 To Len(filter)
		Local p:Int = Instr(inc, Mid(filter, i, 1))
		If p Then inc = Left(inc, p - 1) ; Exit
	Next
	Return inc
End Function

' Try to include a source file, guards and recursion checks permitting
Function TryIncludeFile(l:TLexer, file:String)
	file = RealPath(file)
	
	If l.guardMode	'Auto-guarded includes: check if it's been used already, if so ignore it
		For Local name:String = EachIn l.prev
			If name = file Then Return	'...return without actually changing it
		Next
	EndIf
	
	For Local f:LexFile = EachIn l.istk		'Check against the currently-open files
		If f.dir + f.name = file
			Throw LexError.Make(l, "Cannot recursively include '" + file)
		EndIf
	Next
	
	l.cFile = LexFile.fromFile(file)
	l.istk.AddLast l.cFile ; l.GuardFileName l.cFile
End Function

Public

