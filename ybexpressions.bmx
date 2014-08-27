
' YBC value-expression code generator
' factored out for easier reading: this file generates code for all value expressions

' RValue ops:
' To compute a binary operation (all binary operators plus array index)
'    compute the value on the left and push it to the stack
'    compute the value on the right (%eax)
'    apply the operation
' To compute a unary operation (all unary operators plus function call)
'    compute the operand
'    apply the operation
' To compute a ternary operation
'    treat it similarly to an If statement
' To compute a function call
'    compute each argument and push them to the (actual) stack
' To compute an atom
'    move constants into %eax
'    move locals into %eax
'    move global addresses into %eax then dereference in-place

' LValue ops:
' To compute an assignment
'    compute the lea of the left and push it to the stack
'    compute the value on the right
'    pop the value on the left to %edx
'    mov %eax, (%edx)
' To compute a compound assignment
'    compute the lea of the left and push it to the stack
'    compute the value on the right
'    pop the value on the left to %edx
'    <op> (%edx), %eax ; mov %eax, (%edx)
' To compute an inc/dec prefix
'    compute the lea of the operand
'    inc (%eax) ; mov (%eax), %eax
' To compute an inc/dec postfix
'    compute the lea of the operand
'    move it to %edx
'    mov (%edx), %eax ; inc (%edx)


Type Expr Final
	Function Compile(f:YBFunDef, s:TParseNode, isFCall:Int = 0)	'The meat: compile value expressions
		Select s.rule
			Case "OrExpr", "AndExpr", "EqExpr", "RelExpr", "ShiftExpr", "AddExpr", "MulExpr" ; Expr.Binary f, s
			Case "TernaryExpr" ; Ternary f, s
			Case "PostfixExpr" ; PostFix f, s
			Case "UnaryExpr" ; Unary f, s
			Case "AtomicExpr" ; VarName f, s, isFCall
			Case "Constant" ; Constant f, s
			Case "RValue" ; Assign f, s
		End Select
	End Function
	
	Function Binary(f:YBFunDef, s:TParseNode)
		Local l:TParseNode = s.GetElem("L"), r:TParseNode = s.GetElem("R")
		For Local ri:TParseNode = EachIn r.elem
			If l.rule = "Constant" And ri.elem[1].rule = "Constant" And l.term.ttype <> "sconst" And ri.elem[1].term.ttype <> "sconst"
				Local val:Int = SelectOp(ri.elem[0].term.value, IntVal(l), IntVal(ri.elem[1]))
				l = TParseNode.Leaf(TToken.Make(val, "iconst", l.term.file, l.term.l, l.term.c)) ; l.rule = "Constant"
				r.elem = r.elem[1..]
			Else
				Exit
			EndIf
		Next
		If r.elem.Length
			Compile f, l
			PushTemp f ; Local d:String
			For Local i:Int = 0 Until r.elem.Length - 1
				Compile f, r.elem[i].elem[1]
				d = PeekTemp(f)		'Have to peek rather than pop because SelectBin can push too
				SelectBin f, r.elem[i].elem[0].term.value, d, "%eax"
				PopTemp f ; PushTemp f
			Next
			r = r.elem[r.elem.Length - 1]
			Compile f, r.elem[1]
			d = PeekTemp(f)
			SelectBin f, r.elem[0].term.value, d, "%eax"
			PopTemp f
		Else
			f.ins.AddLast "mov $" + l.term.value + ", %eax"
		EndIf
	End Function
	
	Function Ternary(f:YBFunDef, s:TParseNode)
		Compile f, s.GetElem("expr")
		Local pastThen:String = GetAnonLabel(), pastElse:String = GetAnonLabel(), tail:TParseNode = s.GetElem("tail")
		f.ins.AddLast "cmp $0, %eax"
		f.ins.AddLast "jz " + pastThen
		Compile f, tail.GetElem("then")
		f.ins.AddLast "jmp " + pastElse
		f.ins.AddLast "^" + pastThen + ":"
		Compile f, tail.GetElem("else")
		f.ins.AddLast "^" + pastElse + ":"
	End Function
	
	Function PostFix(f:YBFunDef, s:TParseNode)
		Local ops:TParseNode[] = s.GetElem("op").elem, isLV:Int = 0
		Select ops[0].rule
			Case "Index", "FunCall"
				Compile f, s.GetElem("val"), (ops[0].rule = "FunCall")
			Default	'++/--
				LoadLVal f, s.GetElem("val"), ops[0].term ; isLV = 1 ; f.ins.AddLast "mov (%edx), %eax"
		End Select
		For Local op:TParseNode = EachIn ops
			Select op.rule
				Case "Index"
					PushTemp f ; Compile f, op.GetElem("elem") ; Local REG:String = PopTemp(f)
					If f.tempdepth < 4
						f.ins.AddLast "lea (" + REG + ", %eax, 4), %edx ; mov (%edx), %eax"
					Else
						f.ins.AddLast "mov " + REG + ", %edx ; lea (%edx, %eax, 4), %edx ; mov (%edx), %eax"
					EndIf
				Case "FunCall"
					f.callersave = 1
					Local args:TParseNode = op.GetElem("args")
					If args
						f.stackargs = Max(f.stackargs, args.elem.Length)
						PushTemp f
						For Local arg:Int = 0 Until args.elem.Length
							Compile f, args.elem[arg] ; PushTemp f
						Next
						For Local arg:Int = args.elem.Length - 1 To 0 Step -1
							Local REG:String = PopTemp(f)
							If f.tempdepth < 4
								f.ins.AddLast "mov " + REG + ", " + (arg * 4) + "(%esp)"
							Else
								f.ins.AddLast "mov " + REG + ", %eax ; mov %eax, " + (arg * 4) + "(%esp)"
							EndIf
						Next
						Local REG:String = PopTemp(f)
						f.ins.AddLast "mov %ecx, TEMP{0} ; mov %edx, TEMP{-1}"
						If f.tempdepth < 4 Then f.ins.AddLast "call *" + REG Else f.ins.AddLast "mov " + REG + ", %eax ; call *%eax"
					Else
						f.ins.AddLast "mov %ecx, TEMP{0} ; mov %edx, TEMP{-1} ; call *%eax"
					EndIf
					f.ins.AddLast "mov TEMP{0}, %ecx ; mov TEMP{-1}, %edx"
				Default
					If Not isLV Then Throw CGError.Make(f, op.term, "target of '" + op.term.value + "' operator is not assignable")
					If op.term.value = "++"
						f.ins.AddLast "mov (%edx), %eax ; incl (%edx)"
					Else
						f.ins.AddLast "mov (%edx), %eax ; decl (%edx)"
					EndIf
			End Select
			isLV = (op.rule = "Index")
		Next
	End Function
	
	Function Unary(f:YBFunDef, s:TParseNode)
		Local ops:TParseNode = s.GetElem("op"), fst:TToken = ops.elem[ops.elem.Length - 1].term, isLV:Int = 0
		Select fst.value
			Case "-", "+", "!", "*"
				Compile f, s.GetElem("val")
			Case "&", "--", "++"
				LoadLVal f, s.GetElem("val"), fst ; isLV = 1 ; f.ins.AddLast "mov (%edx), %eax"
		End Select
		For Local o:Int = ops.elem.Length - 1 To 0 Step -1
			Local op:String = ops.elem[o].term.value
			Select op
				Case "-"
					f.ins.AddLast "neg %eax"
				Case "+"
				Case "!"
					f.ins.AddLast "cmp $0, %eax ; sete %al ; and $1, %eax"
				Case "*"
					f.ins.AddLast "mov %eax, %edx ; mov (%eax), %eax"
				Case "&", "--", "++"
					If Not isLV Then Throw CGError.Make(f, ops.elem[o].term, "target of '" + op + "' operator is not assignable")
					If op = "&"
						f.ins.AddLast "mov %edx, %eax"
					ElseIf op = "--"
						f.ins.AddLast "decl (%edx) ; mov (%edx), %eax"
					Else
						f.ins.AddLast "incl (%edx) ; mov (%edx), %eax"
					EndIf
			End Select
			isLV = (op = "*")
		Next
	End Function
	
	Function VarName(f:YBFunDef, s:TParseNode, isFCall:Int)
		Local found:Int = 0, name:String = s.term.value
		For Local v:AutoVar = EachIn f.autos
			If name = v.name Then f.ins.AddLast "mov " + -(v.pos * 4) + "(%ebp), %eax" ; found = 1 ; Exit
		Next
		Local gname:String = YBCodeGen.Platformize(name) ; If isFCall Then gname = "$" + gname
		If Not found
			For Local v:String = EachIn f.extrns
				If name = v Then f.ins.AddLast "mov " + gname + ", %eax" ; found = 1 ; Exit
			Next
		EndIf
		If Not found
			For Local v:String = EachIn f.labels
				If name = v Then f.ins.AddLast "mov $" + f.name + "$" + v + ", %eax" ; found = 1 ; Exit
			Next
		EndIf
		If Not found
			If isFCall
				f.ins.AddLast "mov " + gname + ", %eax"
			Else
				Local u:Unknown = New Unknown ; u.name = name ; u.t = s.term
				f.unknowns.AddLast u
				f.ins.AddLast "mov $" + f.name + "$" + name + ", %eax"
			EndIf
		EndIf
	End Function
	
	Function Constant(f:YBFunDef, s:TParseNode)
		If s.term.ttype = "iconst"
			f.ins.AddLast "mov $" + s.term.value + ", %eax"
		ElseIf s.term.ttype = "cconst"
			f.ins.AddLast "mov $" + CharConstIVal(s.term.value) + ", %eax"
		Else
			f.ins.AddLast "mov $STR$_" + YBCodeGen.strs.Count() + ", %eax"
			YBCodeGen.strs.AddLast s.term.value
		EndIf
	End Function
	
	Function Assign(f:YBFunDef, s:TParseNode)
		Local l:TParseNode = s.GetElem("L")
		Compile f, s.GetElem("R")
		For Local d:Int = l.elem.Length - 1 To 0 Step -1
			Local l2:TParseNode = l.elem[d].elem[0], op:String = l.elem[d].elem[1].term.value
			If l2.rule = "UnaryExpr" Or l2.rule = "PostfixExpr" Then PushTemp f
			LoadLVal f, l2, l.elem[d].elem[1].term
			If l2.rule = "UnaryExpr" Or l2.rule = "PostfixExpr" Then f.ins.AddLast "mov " + PopTemp(f) + ", %eax"
			If op <> "=" Then SelectBin f, op[1..], "(%edx)", "%eax"
			If op <> "=-" Then f.ins.AddLast "mov %eax, (%edx)"
		Next
	End Function
	
	Function LoadLVal(f:YBFunDef, l2:TParseNode, et:TToken)
		Select l2.rule
			Case "AtomicExpr"	'var name
				Local found:Int = 0, name:String = l2.term.value
				For Local v:AutoVar = EachIn f.autos
					If name = v.name Then f.ins.AddLast "lea " + -(v.pos * 4) + "(%ebp), %edx" ; found = 1 ; Exit
				Next
				If Not found
					For Local v:String = EachIn f.extrns
						If name = v Then f.ins.AddLast "mov $" + YBCodeGen.Platformize(name) + ", %edx" ; found = 1 ; Exit
					Next
				EndIf
				If Not found
					For Local v:String = EachIn f.extrns
						If name = v Then f.ins.AddLast "mov " + YBCodeGen.Platformize(name) + ", %edx" ; Exit
					Next
					If Not found Then Throw CGError.Make(f, et, "cannot treat label or undeclared variable '" + l2.term.value + "' as assignable")
				EndIf
				
			Case "UnaryExpr"	'*var
				Local uops:TParseNode = l2.GetElem("op")
				If uops.elem[0].term.value <> "*" Then ..
					Throw CGError.Make(f, et, "cannot assign to expression that does not describe a location")
				If uops.elem.Length = 1
					Compile f, l2.GetElem("val")
				Else
					uops.elem = uops.elem[1..] ; Compile f, uops
				EndIf
				f.ins.AddLast "mov %eax, %edx"
				
			Case "PostfixExpr"	'var[0]
				Local pops:TParseNode = l2.GetElem("op"), ind:TParseNode = pops.elem[pops.elem.Length - 1]
				If ind.rule <> "Index" Then ..
					Throw CGError.Make(f, et, "cannot assign to expression that does not describe a location")
				Compile f, ind.GetElem("elem")
				PushTemp f
				If pops.elem.Length = 1
					Compile f, l2.GetElem("val")
				Else
					pops.elem = pops.elem[0..pops.elem.Length - 1] ; Compile f, l2
				EndIf
				Local REG:String = PopTemp(f) ; If f.tempdepth < 4
					f.ins.AddLast "lea (%eax, " + REG + ", 4), %edx"
				Else
					f.ins.AddLast "mov " + REG + ", %edx ; lea (%eax, %edx, 4), %edx"
				EndIf
				
			Default
				Throw CGError.Make(f, et, "expression does not describe an assignable location")
		End Select
	End Function
	
	Global regstk:String[] = ["%ecx", "%ebx", "%esi", "%edi"]
	Function PushTemp(f:YBFunDef)
		Local r:String ; If f.tempdepth < 4 Then r = regstk[f.tempdepth] Else r = "TEMP{" + (f.tempdepth - 3) + "}"
		f.ins.AddLast "mov %eax, " + r ; f.tempmax = Max(f.tempmax, f.tempdepth - 3)
		f.tempdepth :+ 1 ; If f.tempdepth > 1 Then f.calleesave = 1
	End Function
	Function PeekTemp:String(f:YBFunDef)
		If f.tempdepth < 4 Then Return regstk[f.tempdepth - 1] Else Return "TEMP{" + (f.tempdepth - 4) + "}"
	End Function
	Function PopTemp:String(f:YBFunDef)
		f.tempdepth :- 1
		If f.tempdepth < 4 Then Return regstk[f.tempdepth] Else Return "TEMP{" + (f.tempdepth - 3) + "}"
	End Function
	
	Function SelectOp:Int(o:String, l:Int, r:Int)
		Select o
			Case "|" ; Return l | r
			Case "&" ; Return l & r
			Case "==" ; Return l = r
			Case "!=" ; Return l <> r
			Case "<" ; Return l < r
			Case "<=" ; Return l <= r
			Case ">" ; Return l > r
			Case ">=" ; Return l >= r
			Case "<<" ; Return l Shl r
			Case ">>" ; Return l Shr r
			Case "-" ; Return l - r
			Case "+" ; Return l + r
			Case "%" ; Return l Mod r
			Case "*" ; Return l * r
			Case "/" ; Return l / r
		End Select
	End Function
	
	Function SelectBin(f:YBFunDef, o:String, l:String, r:String)
		Select o
			Case "|" ; f.ins.AddLast "or " + l + ", " + r
			Case "&" ; f.ins.AddLast "and " + l + ", " + r
			Case "==" ; f.ins.AddLast "cmp " + r + ", " + l + " ; sete %al ; and $1, %eax"
			Case "!=" ; f.ins.AddLast "cmp " + r + ", " + l + " ; setne %al ; and $1, %eax"
			Case "<" ; f.ins.AddLast "cmp " + r + ", " + l + " ; setl %al ; and $1, %eax"
			Case "<=" ; f.ins.AddLast "cmp " + r + ", " + l + " ; setle %al ; and $1, %eax"
			Case ">" ; f.ins.AddLast "cmp " + r + ", " + l + " ; setg %al ; and $1, %eax"
			Case ">=" ; f.ins.AddLast "cmp " + r + ", " + l + " ; setge %al ; and $1, %eax"
			Case "<<" ; f.ins.AddLast "shl " + l + ", " + r
			Case ">>" ; f.ins.AddLast "shr " + l + ", " + r
			Case "-" ; f.ins.AddLast "sub " + r + ", " + l + " ; mov " + l + ", %eax"
			Case "+" ; f.ins.AddLast "add " + l + ", " + r
			Case "*" ; f.ins.AddLast "imul " + l + ", " + r
			Case "%", "/"	'more complex
				f.stackargs = Max(f.stackargs, 2) ; f.callersave = 1
				PushTemp f
				f.ins.AddLast "mov " + l + ", %eax ; mov %eax, (%esp)"
				f.ins.AddLast "mov " + PopTemp(f) + ", %eax ; mov " + r + ", 4(%esp)"
				If o = "%" Then o = "mod" Else o = "div"
				f.ins.AddLast "mov %ecx, TEMP{0} ; mov %edx, TEMP{-1}"
				f.ins.AddLast "call lib$" + o
				f.ins.AddLast "mov TEMP{0}, %ecx ; mov TEMP{-1}, %edx"
		End Select
	End Function
	
	Function IntVal:Int(n:TParseNode)
		If n.term.ttype = "iconst" Then Return Int(n.term.value) Else Return CharConstIVal(n.term.value)
	End Function
End Type



