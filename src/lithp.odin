package lithp

import "core:fmt"
import "core:os"
import "core:slice"
import "core:strings"
import "core:strconv"
import scan "core:text/scanner"

main :: proc () {
  file_name := os.args[1]
  scanner := scan.Scanner{}
  if raw_file, ok := os.read_entire_file(file_name); ok {
    file_content := string(raw_file)
    scan.init(&scanner, file_content, file_name)
  } else {
    fmt.printf("Could not open file: `%s`\n", file_name)
    return
  }
  if tree, ok := parse_tree(&scanner); ok {
    builder := strings.Builder{}
    strings.init_builder(&builder)

    for expr in tree do if !compile_s_expr(expr, &builder) { os.exit(1) }

    fmt.print(program_header)
    fmt.print(builtin_procs)
    fmt.println(repeated_instruction("add", "add %rbx, %rax"))
    fmt.println(repeated_instruction("sub", "sub %rbx, %rax"))
    fmt.println(repeated_instruction("div", "div %rbx, %rax"))
    fmt.println(repeated_instruction("mul", "imul %rbx"))
    fmt.println("_start:")

    fmt.print(strings.to_string(builder))
    fmt.print(program_footer)
    for literal, i in literals {
      fmt.printf("string_literal_{}: .ascii {}\n", i, literal)
    }
  } else {
    fmt.println("Parsing error")
  }
}


escaped_len :: proc (s: string) -> int {
  total := 0
  for r in s[1 : len(s) - 1] { if r != '\\' {total += 1} }
  return total 
}

literals := map[string]int{}
num_literals := 0

compile_s_expr :: proc (val: value,  builder: ^strings.Builder) -> (ok: bool) {
  switch typed_value in val {
    case int:
        strings.write_string(builder, "# push int\n")
        strings.write_string(builder, fmt.tprintf(
            "    push ${}\n" if abs(typed_value) <= int(max(i32)) else  "    mov  ${}, %%rax\n    push %%rax\n",
          typed_value))
    case string:
      if typed_value[0] == '"' {
        if typed_value not_in literals {
          literals[typed_value] = num_literals
          num_literals += 1
        }
        strings.write_string(builder, "# push string\n")
        strings.write_string(
          builder,
          fmt.tprintf(
            "    push $string_literal_{}\n    push ${}\n", 
            literals[typed_value], escaped_len(typed_value)))
      } else {
        fmt.printf("ERROR: code is not data at the moment (lisp btw), so `{}` is not yet allowed.\n", typed_value)
        return false
      }
    case symbol:
      strings.write_string(builder, fmt.tprintf("    push ${}\n", symbol_table[typed_value] ))
    case s_expr:
      strings.write_string(builder, "# start expression\n")
      switch head in typed_value[0] {
        case symbol:
          if head not_in symbol_table {
            fmt.printf("ERROR: unknown symbol `{}`\n")
            return false
          }

          size, ok_size := arg_len(typed_value)
          if !ok_size {fmt.println("ERROR computing size"); return false}
          size -= 1
          for child_expr in typed_value[1:] do compile_s_expr(child_expr, builder) or_return
          strings.write_string(builder, fmt.tprintf("    push ${}\n", size * 8))
          strings.write_string(builder, fmt.tprintf("    call {}\n", symbol_table[head]))
        case string: 
          if head in special_ops {
            special_ops[head](builder, typed_value)
          } else {
            fmt.printf("ERROR: `{}` cannot be called.", head); return false
          }
        case s_expr: fmt.printf("ERROR: `{}` cannot be called.", head); return false
        case int:    fmt.printf("ERROR: `{}` cannot be called.", head); return false
        case :       fmt.printf("ERROR: `{}` cannot be called.", head); return false
      }
    case: fmt.printf("ERROR: `{}` cannot be compiled.", typed_value)
  }

  return true
}

arg_len :: proc (v: value) -> (l: int, ok: bool) {
  switch typed in v {
    case int:    return 1, true
    case string: return 2, true
    case symbol: return 1, true 
    case s_expr:
    if len(typed) == 0 { return 0, false }
    switch head in typed[0] {
      case int:    return 0, false
      case s_expr: return 0, false
      case string:
      if head == "if" { 
        if len(typed) != 4 { return 0, false }
        return arg_len(typed[2]) or_return, true
      }
      case symbol:
      total := 0
      switch head {
        case "+", "*", "-", "/":
          total = len(typed)
        case "quote", "cons", "call", "syscall":
          total = 1
          for elem in typed[1:] { 
            total += arg_len(elem) or_return
          }
      }
      return total, true
    }
  } 
  return 0, false
}



symbol :: distinct string
value :: union { int, symbol, string, s_expr }
s_expr :: []value

parse_tree :: proc (scanner: ^scan.Scanner) -> (result: s_expr, ok: bool) {
  call_stack := [dynamic][dynamic]value{{}}

  minus_loc := max(int)
  for scan.scan(scanner) != scan.EOF {
    tok := scan.token_text(scanner)
    num, isnum := strconv.parse_int(tok)
    switch {
      case isnum:
        if minus_loc == scan.position(scanner).offset - 1 {
          last_frame := call_stack[len(call_stack) - 1]
          last_frame[len(last_frame) - 1] = -num
        } else {
          append(&call_stack[len(call_stack) - 1], num)
        }
      case tok == "(":
        append(&call_stack, [dynamic]value{})
      case tok == ")":
        if len(call_stack) < 2 { return {}, false }
        append(&call_stack[len(call_stack) - 2], s_expr(pop(&call_stack)[:]))
      case symbol(tok) in symbol_table:
        append(&call_stack[len(call_stack) - 1], symbol(tok))
        if tok == "-" {minus_loc = scan.position(scanner).offset}
      case:
        append(&call_stack[len(call_stack) - 1], tok)
    }
  }
  return call_stack[0][:], true
}

symbol_table := map[symbol]string {
  "+" =       "builtin_add",
  "-" =       "builtin_sub",
  "/" =       "builtin_div",
  "*" =       "builtin_mul",
  "syscall" = "builtin_syscall",
  "quote" =    "nop", 
  "cons" =    "builtin_cons", 
  "call" =    "builtin_call", 
  "crash" =   "bad_call", 
}

special_ops := map[string]proc(^strings.Builder, s_expr)-> bool {
  "if" = if_asm,
}

if_asm :: proc (builder: ^strings.Builder, s: s_expr)->(ok: bool){
  @static label_num: int
  if len(s) == 4 {
    label_num += 1
    label_num_cache := label_num
    strings.write_string(
      builder, 
      fmt.tprintf("    jmp if_cond_%d\nif_false_%d:\n", label_num_cache, label_num_cache))
    compile_s_expr(s[3], builder) or_return
    strings.write_string(
      builder, 
      fmt.tprintf("    jmp if_end_%d\nif_true_%d:\n", label_num_cache, label_num_cache))
    compile_s_expr(s[2], builder) or_return
    strings.write_string(
      builder, 
      fmt.tprintf("    jmp if_end_%d\nif_cond_%d:\n", label_num_cache, label_num_cache))
    compile_s_expr(s[1], builder) or_return
    strings.write_string(
      builder,
      fmt.tprintf(
`    pop %%rax
    cmp $0, %%rax
    je if_false_%d
    jmp if_true_%d
if_end_%d:
`, label_num_cache, label_num_cache, label_num_cache))
    return true
  } else {
    return false
  }
}

repeated_instruction :: proc (name, op_line: string) -> string {
  return fmt.tprintf(
`
builtin_{}:
    pop %%r11           # old instruction pointer
    pop %%rsi           # len in bytes

    cmp $0, %%rsi
    je bad_call

    add %%rsp, %%rsi    # pointer to after last value (new stack)
    mov %%rsi, %%rbp
    sub $8, %%rsi       # pointer to last value

    mov (%%rsi), %%rax
{}_loop: 
    sub $8, %%rsi
    cmp %%rsp, %%rsi    # if we go over rsp, end
    jl {}_end

    mov (%%rsi), %%rbx  # take value from the stack
    {}                  # update the accumulator

    jmp {}_loop
{}_end:
    mov %%rbp, %%rsp    # update stack pointer
    push %%rax
    jmp *%%r11          # ret
`, name, name, name, op_line, name, name )
}

builtin_procs := string(#load("builtin_procs.asm"))

program_header := " .global _start\n.text\n"


program_footer :: `
    pop %rdi
    mov $60, %rax
    syscall

.data
error_string: 
  .ascii "RUNTIME ERROR: invalid operands to builtin function"
`

