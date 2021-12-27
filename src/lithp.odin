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
  return len(s) - 2
}

literals := map[string]int{}
num_literals := 0

compile_s_expr :: proc (val: value,  builder: ^strings.Builder) -> (ok: bool) {
  switch typed_value in val {
    case int:
        strings.write_string(builder, fmt.tprintf(
            "    push ${}\n" if abs(typed_value) <= int(max(i32)) else  "    mov  ${}, %%rax\n    push %%rax\n",
          typed_value))
    case string:
      if typed_value[0] == '"' {
        if typed_value not_in literals {
          literals[typed_value] = num_literals
          num_literals += 1
        }
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
      fmt.printf("ERROR: code is not data at the moment (lisp btw), so `{}` is not yet allowed.\n", typed_value)
      return false
    case s_expr:
      switch head in typed_value[0] {
        case symbol:
          if head not_in symbol_table {
            fmt.printf("ERROR: unknown symbol `{}`\n")
            return false
          }

          strings.write_string(builder, fmt.tprintf("    push ${}\n", arg_len(typed_value)))
          for child_expr in typed_value[1:] do compile_s_expr(child_expr, builder) or_return
          strings.write_string(builder, fmt.tprintf("    mov %%rsp, %%rbp\n    add ${}, %%rbp\n", arg_len(typed_value) * 8))

          strings.write_string(builder, symbol_table[head])
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


arg_len :: proc (s: s_expr) -> int {
  total := 0
  for elem in s[1:] {
    switch typed in elem {
      case int:    total += 1
      case string: total += 2
      case symbol:
      case s_expr: total += 1 // we assume all expressions evaluate to a single word
      case: 
    }
  }
  return total
}

symbol :: distinct string
value :: union { int, symbol, string, s_expr }
s_expr :: []value

parse_tree :: proc (scanner: ^scan.Scanner) -> (result: s_expr, ok: bool) {
  call_stack := [dynamic][dynamic]value{{}}

  for scan.scan(scanner) != scan.EOF {
    tok := scan.token_text(scanner)
    num, isnum := strconv.parse_int(tok)
    switch {
      case isnum:
        append(&call_stack[len(call_stack) - 1], num)
      case tok == "(":
        append(&call_stack, [dynamic]value{})
      case tok == ")":
        if len(call_stack) < 2 { return {}, false }
        append(&call_stack[len(call_stack) - 2], s_expr(pop(&call_stack)[:]))
      case symbol(tok) in symbol_table:
        append(&call_stack[len(call_stack) - 1], symbol(tok))
      case:
        append(&call_stack[len(call_stack) - 1], tok)
    }
  }
  return call_stack[0][:], true
}

symbol_table := map[symbol]string {
  "+" =       "    call builtin_add\n    push %rax\n",
  "-" =       "    call builtin_sub\n    push %rax\n",
  "/" =       "    call builtin_div\n    push %rax\n",
  "*" =       "    call builtin_mul\n    push %rax\n",
  "syscall" = "    call builtin_syscall\n    push %rax\n",
}

special_ops := map[string]proc(^strings.Builder, s_expr)-> bool {
  "if" = if_asm,
}

if_asm :: proc (builder: ^strings.Builder, s: s_expr)->(ok: bool){
  @static label_num: int
  if len(s) == 4 {
    label_num += 1
    strings.write_string(
      builder, 
      fmt.tprintf("    jmp if_cond_%d\nif_false_%d:\n", label_num, label_num))
    compile_s_expr(s[3], builder) or_return
    strings.write_string(
      builder, 
      fmt.tprintf("    jmp if_end_%d\nif_true_%d:\n", label_num, label_num))
    compile_s_expr(s[2], builder) or_return
    strings.write_string(
      builder, 
      fmt.tprintf("    jmp if_end_%d\nif_cond_%d:\n", label_num, label_num))
    compile_s_expr(s[1], builder) or_return
    strings.write_string(
      builder,
      fmt.tprintf(
`    pop %%rax
    cmp $0, %%rax
    je if_false_%d
    jmp if_true_%d
if_end_%d:
`, label_num, label_num, label_num))
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
    mov %%rbp, %%rsi
    sub $8, %%rsi

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
    add $8, %%rsp       # throw away the old length
    jmp *%%r11          # ret
`, name, name, name, op_line, name, name )
}


program_header := fmt.tprintf(
`.global _start
.text

%s
%s
%s
%s
bad_call:
    mov $1, %%rax
    mov $1, %%rdi
    mov $error_string, %%rsi
    mov $51, %%rdx
    syscall
    mov $60, %%rax
    mov $1, %%rdi
    syscall

builtin_syscall:
    pop %%r11           # instruction pointer
    mov (%%rbp), %%rax

    cmp $0, %%rax
    je bad_call

    cmp $1, %%rax   # TODO: put a jump table here
    je builtin_syscall_0
    cmp $2, %%rax
    je builtin_syscall_1
    cmp $3, %%rax
    je builtin_syscall_2
    cmp $4, %%rax
    je builtin_syscall_3
    cmp $5, %%rax
    je builtin_syscall_4
    cmp $6, %%rax
    je builtin_syscall_5
    cmp $7, %%rax
    pop %%r9
builtin_syscall_5:
    pop %%r8
builtin_syscall_4:
    pop %%r10
builtin_syscall_3:
    pop %%rdx
builtin_syscall_2:
    pop %%rsi
builtin_syscall_1:
    pop %%rdi
builtin_syscall_0:
    pop %%rax
    add $8, %%rsp   # throw away the list length
    push %%r11      # push the instruction pointer
    syscall
    ret

_start:
`, 
repeated_instruction("add", "add %rbx, %rax"),
repeated_instruction("sub", "sub %rbx, %rax"),
repeated_instruction("div", "div %rbx, %rax"),
repeated_instruction("mul", "imul %rbx"),
)


program_footer :: `
    pop %rdi
    mov $60, %rax
    syscall

.data
error_string: 
  .ascii "RUNTIME ERROR: invalid operands to builtin function"
`

