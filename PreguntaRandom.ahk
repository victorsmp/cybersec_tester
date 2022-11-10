#SingleInstance Force
#NoEnv
SetBatchLines -1
global respuesta_sel, pregunta, respuestaA, respuestaB, respuestaC, respuestaD, checkA, checkB, checkC, checkD, acertadas, total, framea, frameb, framec, framed
global respuestas := {}
global preguntas := {}
global preguntaActual := {}
preguntas.lista := Array()
gosub, cargarFicheroPreguntas
acertadas := 0
total := 0
gosub, mostrarGui
gosub, actualizarCampos
Return

mostrarGui:
	Gui Add, Picture, x0 y0 w1280 h720, res/img/bg.jpg
	Gui Font, s24 Bold +cBlue
	Gui Add, Text, vpregunta x16 y16 w1230 h80 +Center +BackgroundTrans, Introduce aquí la pregunta.
	Gui Font, s12 Bold +cBlack
	Gui Add, Radio, vcheckA x16 y115 w40 h68 +BackgroundTrans, A)
	Gui Add, Radio, vcheckB x16 y235 w40 h68 +BackgroundTrans, B)
	Gui Add, Radio, vcheckC x16 y355 w40 h68 +BackgroundTrans, C)
	Gui Add, Radio, vcheckD x16 y475 w40 h68 +BackgroundTrans, D)
	Gui Font, s12
	Gui Add, Text, +Border vrespuestaA gsetRespuestaA x60 y105 w1175 h100 +BackgroundTrans, Este cuadro alberga la opción de respuesta a)
	Gui Add, Text, +Border vrespuestaB gsetRespuestaB x60 y225 w1175 h100 +BackgroundTrans, Este cuadro alberga la opción de respuesta b)
	Gui Add, Text, +Border vrespuestaC gsetRespuestaC x60 y345 w1175 h100 +BackgroundTrans, Este cuadro alberga la opción de respuesta c)
	Gui Add, Text, +Border vrespuestaD gsetRespuestaD x60 y465 w1175 h100 +BackgroundTrans, Este cuadro alberga la opción de respuesta d)
	Gui Add, Picture, x55 y100 w1185 h110 +BackgroundTrans vframea Hidden, res/img/right.png
	Gui Add, Picture, x55 y220 w1185 h110 +BackgroundTrans vframeb Hidden, res/img/right.png
	Gui Add, Picture, x55 y340 w1185 h110 +BackgroundTrans vframec Hidden, res/img/right.png
	Gui Add, Picture, x55 y460 w1185 h110 +BackgroundTrans vframed Hidden, res/img/right.png
	Gui Font, s16 Bold
	Gui Add, Button, gsiguientePregunta x1141 y641 w125 h75, Siguiente Pregunta
	Gui Add, Text, vmarcador x16 y679 w200 h80 +Center +Left +BackgroundTrans, 0/0
	Gui Show, w1280 h720, Cuestionario VictorSMP
return

setRespuestaA:
	GuiControl,, checkA, 1
return

setRespuestaB:
	GuiControl,, checkB, 1
return

setRespuestaC:
	GuiControl,, checkC, 1
return

setRespuestaD:
	GuiControl,, checkD, 1
return

siguientePregunta:
	GuiControlGet, checkA,, checkA
	GuiControlGet, checkB,, checkB
	GuiControlGet, checkC,, checkC
	GuiControlGet, checkD,, checkD
	correcta := ""
	if(checkA)
		correcta := "a"
	else if(checkB)
		correcta := "b"
	else if(checkC)
		correcta := "c"
	else if(checkD)
		correcta := "d"
	if(correcta != "")
	{
		if(correcta == preguntaActual.correcta)
		{
			acertadas++
			SoundPlay, res/snd/Sounds_Correct.wav
		}
		else
		{
			gosub, mostrarCorrecta
			SoundPlay, res/snd/Sounds_Wrong.wav
			MsgBox,,Fallaste, Respuesta Incorrecta
			gosub, ocultarFrames
		}
		total++
		gosub, actualizarCampos
	}
return

cargarFicheroPreguntas:
	FileEncoding, UTF-8
	FileSelectFile, rutaFichero,, %A_ScriptDir%
	FileRead, preguntas, %rutaFichero%
	preguntas := JSON.Load(preguntas)
return

preguntaAleatoria:
	Random, index, 1, preguntas.lista.length()
	preguntaActual := preguntas.lista[index]
return

actualizarCampos:
	gosub, preguntaAleatoria
	GuiControl,, pregunta, % preguntaActual.pregunta
	GuiControl,, respuestaA, % preguntaActual.a
	GuiControl,, respuestaB, % preguntaActual.b
	GuiControl,, respuestaC, % preguntaActual.c
	GuiControl,, respuestaD, % preguntaActual.d
	GuiControl,, checkA, 0
	GuiControl,, checkB, 0
	GuiControl,, checkC, 0
	GuiControl,, checkD, 0
	GuiControl,, marcador, % acertadas " / " total
return

ocultarFrames:
	GuiControl, Hide, framea
	GuiControl, Hide, frameb
	GuiControl, Hide, framec
	GuiControl, Hide, framed
return

mostrarCorrecta:
	if(preguntaActual.correcta == "a")
		GuiControl, Show, framea
	if(preguntaActual.correcta == "b")
		GuiControl, Show, frameb
	if(preguntaActual.correcta == "c")
		GuiControl, Show, framec
	if(preguntaActual.correcta == "d")
		GuiControl, Show, framed
return

GuiEscape:
GuiClose:
    ExitApp
	
class JSON
{
	class Load extends JSON.Functor
	{
		Call(self, ByRef text, reviver:="")
		{
			this.rev := IsObject(reviver) ? reviver : false
			this.keys := this.rev ? {} : false

			static quot := Chr(34), bashq := "\" . quot
			     , json_value := quot . "{[01234567890-tfn"
			     , json_value_or_array_closing := quot . "{[]01234567890-tfn"
			     , object_key_or_object_closing := quot . "}"

			key := ""
			is_key := false
			root := {}
			stack := [root]
			next := json_value
			pos := 0

			while ((ch := SubStr(text, ++pos, 1)) != "") {
				if InStr(" `t`r`n", ch)
					continue
				if !InStr(next, ch, 1)
					this.ParseError(next, text, pos)

				holder := stack[1]
				is_array := holder.IsArray

				if InStr(",:", ch) {
					next := (is_key := !is_array && ch == ",") ? quot : json_value

				} else if InStr("}]", ch) {
					ObjRemoveAt(stack, 1)
					next := stack[1]==root ? "" : stack[1].IsArray ? ",]" : ",}"

				} else {
					if InStr("{[", ch) {
						static json_array := Func("Array").IsBuiltIn || ![].IsArray ? {IsArray: true} : 0
						(ch == "{")
							? ( is_key := true
							  , value := {}
							  , next := object_key_or_object_closing )
						; ch == "["
							: ( value := json_array ? new json_array : []
							  , next := json_value_or_array_closing )
						
						ObjInsertAt(stack, 1, value)

						if (this.keys)
							this.keys[value] := []
					
					} else {
						if (ch == quot) {
							i := pos
							while (i := InStr(text, quot,, i+1)) {
								value := StrReplace(SubStr(text, pos+1, i-pos-1), "\\", "\u005c")

								static tail := A_AhkVersion<"2" ? 0 : -1
								if (SubStr(value, tail) != "\")
									break
							}

							if (!i)
								this.ParseError("'", text, pos)

							  value := StrReplace(value,  "\/",  "/")
							, value := StrReplace(value, bashq, quot)
							, value := StrReplace(value,  "\b", "`b")
							, value := StrReplace(value,  "\f", "`f")
							, value := StrReplace(value,  "\n", "`n")
							, value := StrReplace(value,  "\r", "`r")
							, value := StrReplace(value,  "\t", "`t")

							pos := i
							
							i := 0
							while (i := InStr(value, "\",, i+1)) {
								if !(SubStr(value, i+1, 1) == "u")
									this.ParseError("\", text, pos - StrLen(SubStr(value, i+1)))

								uffff := Abs("0x" . SubStr(value, i+2, 4))
								if (A_IsUnicode || uffff < 0x100)
									value := SubStr(value, 1, i-1) . Chr(uffff) . SubStr(value, i+6)
							}

							if (is_key) {
								key := value, next := ":"
								continue
							}
						
						} else {
							value := SubStr(text, pos, i := RegExMatch(text, "[\]\},\s]|$",, pos)-pos)

							static number := "number", integer :="integer"
							if value is %number%
							{
								if value is %integer%
									value += 0
							}
							else if (value == "true" || value == "false")
								value := %value% + 0
							else if (value == "null")
								value := ""
							else

								this.ParseError(next, text, pos, i)

							pos += i-1
						}

						next := holder==root ? "" : is_array ? ",]" : ",}"
					} ; If InStr("{[", ch) { ... } else

					is_array? key := ObjPush(holder, value) : holder[key] := value

					if (this.keys && this.keys.HasKey(holder))
						this.keys[holder].Push(key)
				}
			
			} ; while ( ... )

			return this.rev ? this.Walk(root, "") : root[""]
		}

		ParseError(expect, ByRef text, pos, len:=1)
		{
			static quot := Chr(34), qurly := quot . "}"
			
			line := StrSplit(SubStr(text, 1, pos), "`n", "`r").Length()
			col := pos - InStr(text, "`n",, -(StrLen(text)-pos+1))
			msg := Format("{1}`n`nLine:`t{2}`nCol:`t{3}`nChar:`t{4}"
			,     (expect == "")     ? "Extra data"
			    : (expect == "'")    ? "Unterminated string starting at"
			    : (expect == "\")    ? "Invalid \escape"
			    : (expect == ":")    ? "Expecting ':' delimiter"
			    : (expect == quot)   ? "Expecting object key enclosed in double quotes"
			    : (expect == qurly)  ? "Expecting object key enclosed in double quotes or object closing '}'"
			    : (expect == ",}")   ? "Expecting ',' delimiter or object closing '}'"
			    : (expect == ",]")   ? "Expecting ',' delimiter or array closing ']'"
			    : InStr(expect, "]") ? "Expecting JSON value or array closing ']'"
			    :                      "Expecting JSON value(string, number, true, false, null, object or array)"
			, line, col, pos)

			static offset := A_AhkVersion<"2" ? -3 : -4
			throw Exception(msg, offset, SubStr(text, pos, len))
		}

		Walk(holder, key)
		{
			value := holder[key]
			if IsObject(value) {
				for i, k in this.keys[value] {
					v := this.Walk(value, k)
					if (v != JSON.Undefined)
						value[k] := v
					else
						ObjDelete(value, k)
				}
			}
			
			return this.rev.Call(holder, key, value)
		}
	}

	class Dump extends JSON.Functor
	{
		Call(self, value, replacer:="", space:="")
		{
			this.rep := IsObject(replacer) ? replacer : ""

			this.gap := ""
			if (space) {
				static integer := "integer"
				if space is %integer%
					Loop, % ((n := Abs(space))>10 ? 10 : n)
						this.gap .= " "
				else
					this.gap := SubStr(space, 1, 10)

				this.indent := "`n"
			}

			return this.Str({"": value}, "")
		}

		Str(holder, key)
		{
			value := holder[key]

			if (this.rep)
				value := this.rep.Call(holder, key, ObjHasKey(holder, key) ? value : JSON.Undefined)

			if IsObject(value) {
				static type := A_AhkVersion<"2" ? "" : Func("Type")
				if (type ? type.Call(value) == "Object" : ObjGetCapacity(value) != "") {
					if (this.gap) {
						stepback := this.indent
						this.indent .= this.gap
					}

					is_array := value.IsArray
					if (!is_array) {
						for i in value
							is_array := i == A_Index
						until !is_array
					}

					str := ""
					if (is_array) {
						Loop, % value.Length() {
							if (this.gap)
								str .= this.indent
							
							v := this.Str(value, A_Index)
							str .= (v != "") ? v . "," : "null,"
						}
					} else {
						colon := this.gap ? ": " : ":"
						for k in value {
							v := this.Str(value, k)
							if (v != "") {
								if (this.gap)
									str .= this.indent

								str .= this.Quote(k) . colon . v . ","
							}
						}
					}

					if (str != "") {
						str := RTrim(str, ",")
						if (this.gap)
							str .= stepback
					}

					if (this.gap)
						this.indent := stepback

					return is_array ? "[" . str . "]" : "{" . str . "}"
				}
			
			} else ; is_number ? value : "value"
				return ObjGetCapacity([value], 1)=="" ? value : this.Quote(value)
		}

		Quote(string)
		{
			static quot := Chr(34), bashq := "\" . quot

			if (string != "") {
				  string := StrReplace(string,  "\",  "\\")
				; , string := StrReplace(string,  "/",  "\/") ; optional in ECMAScript
				, string := StrReplace(string, quot, bashq)
				, string := StrReplace(string, "`b",  "\b")
				, string := StrReplace(string, "`f",  "\f")
				, string := StrReplace(string, "`n",  "\n")
				, string := StrReplace(string, "`r",  "\r")
				, string := StrReplace(string, "`t",  "\t")

				static rx_escapable := A_AhkVersion<"2" ? "O)[^\x20-\x7e]" : "[^\x20-\x7e]"
				while RegExMatch(string, rx_escapable, m)
					string := StrReplace(string, m.Value, Format("\u{1:04x}", Ord(m.Value)))
			}

			return quot . string . quot
		}
	}

	Undefined[]
	{
		get {
			static empty := {}, vt_empty := ComObject(0, &empty, 1)
			return vt_empty
		}
	}

	class Functor
	{
		__Call(method, ByRef arg, args*)
		{
			if IsObject(method)
				return (new this).Call(method, arg, args*)
			else if (method == "")
				return (new this).Call(arg, args*)
		}
	}
}