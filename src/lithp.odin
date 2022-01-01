package lithp

import "core:fmt"
import "core:os"
import "core:slice"
import "core:strings"
import "core:strconv"
import scan "core:text/scanner"

main :: proc() {

	when !#config(bake_header, true) {
		program_header_buf, ok_header := os.read_entire_file("src/program_header.asm")
		program_header := transmute(string)program_header_buf
		if !ok_header {fmt.println("Error: could not load program header\n")os.exit(1)}
	}

	file_name := os.args[1]
	scanner := scan.Scanner{}
	defer delete(scanner.src)
	if raw_file, ok := os.read_entire_file(file_name); ok {
		file_content := string(raw_file)
		scan.init(&scanner, file_content, file_name)
	} else {
		fmt.printf("Could not open file: `%s`\n", file_name)
		return
	}
	tree := parse_tree(&scanner)
	for expr in &tree {determine_types(&expr)}

	b := strings.make_builder()
	strings.write_string(&b, program_header)
	strings.write_string(&b, "\nstart:\n    mov rax, 0xdeadbeef")

	for expr in &tree {
		when #config(verbose_typing, false) {fmt.println(expr.type)}
		compile_to_assembly(&b, expr)recurse_delete(expr)
	}

	strings.write_string(&b, program_footer)
	for l in literals {
		fmt.sbprintf(&b, "string_literal_{}:\n    db ", literals[l])
		to_ascii_code(&b, l[1:len(l) - 1])
		fmt.sbprint(&b, "\n")
	}

	asm_file: string 
	if strings.has_suffix(file_name, ".lithp") {
		asm_file = fmt.tprintf("{}.asm", file_name[:len(file_name) - 6])
	} else {
		asm_file = fmt.tprintf("{}.asm", file_name)
	}
	if ok := os.write_entire_file(asm_file, transmute([]u8)strings.to_string(b)); !ok {
		fmt.println("Error writing to file\n")
		os.exit(1)
	}
}

literals := map[string]int{}

get_size :: proc(typ: Type) -> (size: int) {
	switch t in typ {
	case Primitive:
		switch t {
		case .Unit:
			return 0
		case .Int:
			return 8
		case .String:
			return 16
		case .Keyword:
			fmt.println("Error: trying to determine the size of a keyword\n")
			os.exit(1)
		}
	case FunctionType:
		return 8

	case GenericType:
		fmt.println("Error: trying to determine the size of a generic type\n")
		os.exit(1)

	case []Type:
		for e in t {size += get_size(e)}
		return size
	}
	fmt.println("Error: unreachable code\n")
	return 0
}

pretty_print :: proc(p: scan.Position) -> string {
	return fmt.tprintf("{}:{}:{}", p.filename, p.line, p.column)
}

to_ascii_code :: proc(b: ^strings.Builder, s: string) {
	escape := map[u8]u8 {
		'n' = '\n',
		'v' = '\v',
		't' = '\t',
	}
	code: int 
	for i := 0; i < len(s); i += 1 {
		if s[i] == '\\' {code = int(escape[s[i + 1]])i += 1} else {code = int(s[i])}
		fmt.sbprintf(b, "{}{}", code, i != len(s) - 1 ? "," : "\n")
	}
}

unescaped_len :: proc(s: string) -> (l: int) {
	for i := 1; i < len(s) - 1; i += 1 {
		if s[i] != '\\' {l += 1}
	}
	return l
}


compile_to_assembly :: proc(b: ^strings.Builder, e: Expression) {
	using strings
	#partial switch value in e.value {
	case int:
		fmt.sbprintf(b, "    push rax\n    mov rax, {}\n", value)

	case string:
		if value not_in literals {literals[value] = len(literals)}
		fmt.sbprintf(b, "    push rax\n    push string_literal_{}\n    mov rax, {}\n", literals[value], unescaped_len(value))

	case Function:
		fmt.sbprintf(b, "    push rax\n    mov rax, {}\n", function_types[value].name)

	case Keyword:
		fmt.printf("Error: loose keyword in the code generation step: `{}` at {}\n", value, pretty_print(e.loc))
		os.exit(1)

	case []Expression:
		fmt.sbprintf(b, "  ;; start expression: {}\n", pretty_print(e.loc))
		if len(value) == 0 {
			fmt.printf("Error: empty expression in the code generation step at {}\n", pretty_print(e.loc))
		}

		// Unit types have no size, so they need to take back the top of the stack
		// in order not to break the calling convention.
		// some of the pushes can probably be avoided anyway.
		defer if t, ok := e.type.(Primitive); ok && t == .Unit {write_string(b, "    pop rax\n")}
		#partial switch head in value[0].type {
		case FunctionType:
			// put all the arguments on the stack
			size := 0
			for arg in value[1:] {compile_to_assembly(b, arg)size += get_size(arg.type)}

			// assume the size is always known at compile time
			fmt.sbprintf(b, "    push rax\n    mov rax, {}\n", size)

			if t, ok := value[0].value.(Function); ok {
				// we know the value of the function pointer, since it is a constant
				fmt.sbprintf(b, "    call {}\n", function_types[t].name)
			} else {
				// evaluate the function, this will leave its pointer on the stack
				compile_to_assembly(b, value[0])
				// pop the function and call it.
				write_string(b, "    call rax\n")
			}

		case Primitive:
			#partial switch head {
			case .Keyword:
				switch value[0].value.(Keyword) {
				case "if":
					@(static)
					if_count: int 
					id := if_count
					if_count += 1
					// condition
					compile_to_assembly(b, value[1])
					fmt.sbprintf(b, "    test rax, rax\n    pop rax\n    jz if_false_{}\n", id)
					// if true
					compile_to_assembly(b, value[2])
					fmt.sbprintf(b, "    jmp if_end_{}\n", id)
					// if false
					fmt.sbprintf(b, "if_false_{}:\n", id)
					compile_to_assembly(b, value[3])
					// end
					fmt.sbprintf(b, "if_end_{}:\n", id)
				}
			}
		}
	}
}

GenericType :: distinct int
Primitive :: enum  {Unit,      Int,    String,       Keyword}
Type      :: union {Primitive, []Type, FunctionType, GenericType}

FunctionType :: struct {
	id:   Function,
	name: string,
	eval: proc(_: []Type) -> Type,
}

recurse_delete_type :: proc(typ: Type) {
	#partial switch ts in typ {
	case []Type:
		for t in ts {recurse_delete(t)}
		delete(ts)
	}
}

recurse_delete_expr :: proc(expr: Expression) {
	recurse_delete(expr.type)
	#partial switch es in expr.value {
	case []Expression:
		for e in es {recurse_delete(e)}
		delete(es)
	}
}

recurse_delete :: proc{recurse_delete_type, recurse_delete_expr}

Expression :: struct {
	type:  Type,
	loc:   scan.Position,
	value: union {int, string, Function, Keyword, []Expression},
}

Function :: distinct string
Keyword :: distinct string

parse_tree :: proc(scanner: ^scan.Scanner) -> []Expression {
	call_stack := [dynamic][dynamic]Expression{{}}
	sign := 1

	for scan.scan(scanner) != scan.EOF {
		tok := scan.token_text(scanner)
		when #config(verbose_parsing, false) {fmt.printf("token: `{}`\n", tok)}
		loc := scan.position(scanner)
		switch {

		case tok == "(":
			append(&call_stack, [dynamic]Expression{})

		case tok == ")":
			expr := Expression{value = pop(&call_stack)[:], loc = loc}
			append(slice.last_ptr(call_stack[:]), expr)

		case tok == "-" && is_numeric(scan.peek(scanner)):
			sign = -1

		case is_int_literal(tok):
			expr, ok := strconv.parse_int(tok)
			if !ok {fmt.printf("invalid int literal at `{}`\n", scan.position(scanner))os.exit(1)}
			append(slice.last_ptr(call_stack[:]), Expression{value = sign * expr, loc = loc})
			sign = 1

		case is_string_literal(tok) : append(slice.last_ptr(call_stack[:]), Expression{value = tok, loc = loc})
		case tok in keywords        : append(slice.last_ptr(call_stack[:]), Expression{value = Keyword(tok), loc = loc})
		case                        : append(slice.last_ptr(call_stack[:]), Expression{value = Function(tok), loc = loc})
		}
	}

	if len(call_stack) > 1 {
		fmt.println("Error: unclosed parentheses")
		os.exit(1)
	}

	if len(call_stack) < 1 {
		fmt.println("Error: too many closing parentheses")
		os.exit(1)
	}

	return call_stack[0][:]
}

compare_types :: proc(a, b: Type, bindings : ^map[GenericType]Type = nil) -> bool {
  switch t in a {
    case Primitive:
    if b, ok := b.(Primitive); ok && b == t { return false } 

    case []Type:
    if b, ok := b.([]Type); ok {
      if len(b) < len(t) {return false}
      for elem, i in t { compare_types(elem, b[i]) or_return }
    } else { return false }
    
    case FunctionType:
    fmt.println("Error: FunctionType is deprecated")
    os.exit(1)
    
    case GenericType:
    if bindings == nil {
      fmt.println("Error: generic types encountered in unexpected context")
      os.exit(1)
    }
    if t not_in bindings { 
      bindings[t] = b
      return true
    } else {
      // check that the bindings are coherent
      return compare_types(bindings[t], b, bindings)
    }
  }
  return false
}

determine_types :: proc(using e: ^Expression) {
	switch val in value {
	case int:
		type = .Int

	case string:
		type = .String

	case Function:
		ok: bool 
		type, ok = function_types[val]
		if !ok { fmt.printf("Error: could not determine the type of `{}` at {}", val, pretty_print(loc)) }

	case Keyword:
		type = .Keyword

	case []Expression:
		if len(val) == 0 {
			fmt.printf("Error: empty expression at {}\n", pretty_print(loc))
			os.exit(1)
		}
		for i in 0 ..< len(val) {determine_types(&val[i])}
		switch t in val[0].type {
    case GenericType: fmt.printf("Error: generic type found in concrete type evaluation {}\n", pretty_print(val[0].loc))
		case Primitive:
			switch t {
			case .Unit:
				fmt.printf("Error: can't call unit ad {}\n", pretty_print(val[0].loc))
			case .Int:
				fmt.printf("Error: can't call int at {}\n", pretty_print(val[0].loc))
				os.exit(1)

			case .String:
				fmt.printf("Error: can't call string at {}\n", pretty_print(val[0].loc))
				os.exit(1)

			case .Keyword:
				switch val[0].value.(Keyword) {
				case "if":
					if !compare_types(val[1].type, .Int) {
						fmt.printf("Error: condition of `if` at {} has type {}, should be Int.\n", pretty_print(val[1].loc), val[1].type)
						os.exit(1)
					}

					if !compare_types(val[2].type, val[3].type) {
						fmt.printf(
							"Error: branches of `if` at {} have different types: {} vs {}\n",
							pretty_print(val[0].loc),
							val[2].type,
							val[3].type,
						)
						os.exit(1)
					}
					type = val[2].type
				}
			}

		case []Type:
			fmt.printf("Error: can't call list {} at {}", t, pretty_print(val[0].loc))
			os.exit(1)

		case FunctionType:
			type = t.eval(slice.mapper(val[1:], proc(e: Expression) -> Type {return e.type}, context.temp_allocator))
		}
	}
}

keywords := map[string]bool{"if" = true}

recurse_clone :: proc(ts: []Type) -> []Type {
	res := make([]Type, len(ts))
	for t, i in ts {
		#partial switch t in t {
		case []Type:
			res[i] = recurse_clone(t)
			continue
		}
		res[i] = t
	}
	return res
}

return_unit :: proc(ts: []Type) -> Type {return Primitive.Unit}
return_int  :: proc(ts: []Type) -> Type {return Primitive.Int}
call_type   :: proc(ts: []Type) -> Type {return function_types[ts[0].(FunctionType).id].eval(ts[1:])}
quote_type  :: proc(ts: []Type) -> Type {return recurse_clone(ts)}

function_types := map[Function]FunctionType {
	"marco"   = {"marco",   "marco",           return_unit},
	"syscall" = {"syscall", "builtin_syscall", return_int },
	"call"    = {"call",    "builtin_call",    call_type  },
	"quote"   = {"quote",   "builtin_quote",   quote_type },
	"+"       = {"+",       "builtin_add",     return_int },
	"-"       = {"-",       "builtin_sub",     return_int },
	"*"       = {"*",       "builtin_mul",     return_int },
	"/"       = {"/",       "builtin_div",     return_int },
}

integer_operations_signature := FuncSignature{[]Type{.Int},           .Int}
quote_signature              := FuncSignature{GenericType(0),         GenericType(0)}
syscall_signature            := FuncSignature{[]Type{.Int},           Primitive.Int}
car_signature                := FuncSignature{[]Type{GenericType(0)}, GenericType(0)}

FuncSignature  :: distinct [2]Type
check_signature :: proc (input_type: Type, signature: FuncSignature) -> (return_type: Type, ok: bool) {
  bindings := map[GenericType]Type{}
  if compare_types(signature[0], input_type, &bindings) {
    return substitute_types(signature[1], bindings) or_return, true 
  }
  return {}, false
}

substitute_types :: proc (t: Type, bindings: map[GenericType]Type) -> (result: Type, ok: bool) {
  switch t in t {
    case GenericType: return bindings[t] or_return, true
    case Primitive: return t, true
    case []Type:
      res := make([]Type, len(t))
      for elem, i in t {res[i] = substitute_types(elem, bindings) or_return}
      return res, true
    case FunctionType:
      fmt.println("Error: FunctionType should not occur as it is deprecated")
      return {}, false
  }
  return {}, false
}


is_numeric        :: proc(r: rune  ) -> bool {return '0' <= r && r <= '9'}
is_int_literal    :: proc(s: string) -> bool {return is_numeric(rune(s[0]))}
is_string_literal :: proc(s: string) -> bool {return s[0] == '"' && s[len(s) - 1] == '"'}

when #config(bake_header, true) {
	program_header :: string(#load("program_header.asm"))
}

program_footer :: `
    mov rdi, rax
    mov rax, 60
    syscall

segment readable writeable
error_string:      db "RUNTIME ERROR: invalid call", 10
polo:              db "Polo", 10
syscall_jumptable: dq crash, syscall1, syscall2, syscall3, syscall4, syscall5, syscall6, syscall7
`
