
' YBC "assembler"
' This module actually outputs assembly code, generates stubs for the runtime library,
' and uses as to generate machine code

SuperStrict

Import "ybcodegen.bmx"


Type YBAssembler Final
	Function Emit(file:String, syms:TList, funs:TList, vars:TList, strs:TList)
		' Finally, write code out to file
		generatedAssembly = WriteFile(file)
		
		wr "## Exported names:"
		wr ".text"
		For Local s:String = EachIn syms
			wr ".globl " + YBCodeGen.Platformize(s)
		Next
		wr "## ---"
		wr ""
		
		' Emit global variables
		wr ".data"
		For Local v:YBVarDef = EachIn vars
			wr YBCodeGen.Platformize(v.name) + ":"
			If v.isvec
				wr "~t.long VAR$" + v.name
			ElseIf v.inits.Count()
				wr "~t.long " + v.inits.RemoveFirst().ToString()
			Else
				wr "~t.long 0"
			EndIf
		Next
		wr ""
		
		' Emit function bodies
		wr ".text" ; wr ".align 4" ; wr ""
		For Local f:YBFunDef = EachIn funs
			wr YBCodeGen.Platformize(f.name) + ":"
			wr "~tpush %ebp"
			wr "~tmov %esp, %ebp"
			Local sz:Int = f.framesz + f.calleesave * 3 + f.callersave * 2 + f.stackargs + f.tempmax ; If sz
				While sz Mod 4 <> 2 ; sz :+ 1 ; Wend
				wr "~tsub $" + (4 * sz) + ", %esp"
			EndIf
			If f.calleesave
				wr "~tmov %ebx, -" + (4 * (f.framesz + 1)) + "(%ebp)"
				wr "~tmov %edi, -" + (4 * (f.framesz + 2)) + "(%ebp)"
				wr "~tmov %esi, -" + (4 * (f.framesz + 3)) + "(%ebp)"
			EndIf
			If f.tempmax Or f.callersave	'Replace any temp stack spills with actual memory locations
				Local i:TLink = f.ins.FirstLink(), fsz:Int = f.framesz + f.calleesave * 3 + f.callersave * 2
				While i
					Local s:String = String(i._value)
					While s.Contains("TEMP{")
						Local slot:Int = Int(s[s.Find("TEMP{") + 5..])
						s = s.Replace("TEMP{" + slot + "}", ((fsz + slot) * -4) + "(%ebp)")
					Wend
					i._value = s ; i = i.NextLink()
				Wend
			EndIf
			For Local i:String = EachIn f.ins
				If i[0] = "^"[0] Then wr i[1..] Else wr "~t" + i
			Next
			wr "return$" + f.name + ":"
			If f.calleesave
				wr "~tmov -" + (4 * (f.framesz + 1)) + "(%ebp), %ebx"
				wr "~tmov -" + (4 * (f.framesz + 2)) + "(%ebp), %edi"
				wr "~tmov -" + (4 * (f.framesz + 3)) + "(%ebp), %esi"
			EndIf
			If sz Then wr "~tadd $" + (4 * sz) + ", %esp"
			wr "~tpop %ebp"
			wr "~tret"
			wr ""
		Next
		
		' Emit global arrays
		wr ".data"
		wr ".align 4"
		For Local v:YBVarDef = EachIn vars
			If v.isvec
				wr "VAR$" + v.name + ":"
				If v.dsz + v.isz
					For Local i:String = EachIn v.inits
						wr "~t.long " + i
					Next
					Local rest:Int = Max(v.dsz, v.isz) - v.isz
					If rest > 0 Then wr "~t.fill " + rest + ", 4"
				Else
					wr "~t.long 0"
				EndIf
			EndIf
		Next
		wr ""
		
		' Emit constant strings
		Local c:Int = 0
		For Local s:String = EachIn strs
			wr "STR$_" + c + ":"
			wr "~t.asciz ~q" + s + "~q"
			c :+ 1;
		Next
		wr ""
		
		CloseStream generatedAssembly
	End Function
End Type


Private

Global generatedAssembly:TStream
Function wr(s:String)
	?Win32
	WriteString generatedAssembly, s + "~r~n"
	?Not Win32
	WriteString generatedAssembly, s + "~n"
	?
End Function

Public

