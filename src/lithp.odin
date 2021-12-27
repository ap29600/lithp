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
    for expr in tree {
      fmt.print(program_header)
      fmt.print(compile_s_expr(expr) or_else "invalid\n")
      fmt.print(program_footer)

    }
    // fmt.println("Ok")
  } else {
    fmt.println("Parsing error")
  }
}


// [false_code][true_code][cond][if]


compile_s_expr :: proc (val: value, short_form := false) -> (asm_string: string, ok: bool) {
  result := strings.Builder{}
  strings.init_builder(&result)

  switch typed_value in val {
    case int:
      if typed_value > int(max(i32)) {
        strings.write_string_builder(
          &result, 
          fmt.tprintf(
            "    mov $%d, %%rax\n    push %%rax\n",
            typed_value))
      } else {
        strings.write_string_builder(
          &result, 
          fmt.tprintf(
            "    push $%d\n",
            typed_value))
      }
    case string: 
      fmt.println("ERR: unsupported string")
      return "", false
    case symbol:
      strings.write_string_builder(
        &result,
        symbol_table[typed_value][0 if short_form else 1])
    case s_expr:
      //fmt.println(typed_value)
      switch car in typed_value[0] {
        case string:
          if car in special_ops {
            strings.write_string_builder(
              &result,
              special_ops[car](typed_value) or_return)
          } else {
            return "", false
          }
        case symbol:
          for i := len(typed_value) - 1; i >= 0; i -= 1 {
            if i == 0 && len(typed_value) != 3 {
              strings.write_string_builder(
                &result,
                fmt.tprintf("    mov $%d, %%rdi\n", len(typed_value) - 1))
            }
            strings.write_string_builder(
              &result,
              compile_s_expr(typed_value[i], len(typed_value) == 3) or_return)
          } 
        case int:    fmt.println("ERROR: unsupported operator of type `int`"); return "", false 
        case s_expr: fmt.println("ERROR: unsupported operator of type `s_expr`"); return "", false
        case:        fmt.println("ERROR: unsupported operator of type `nil`"); return "", false 
      }
      
  }

  return strings.to_string(result), true
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

symbol_table := map[symbol][2]string {
  "+" = {"    pop %rax\n    pop %rbx\n    add %rbx, %rax\n    push %rax\n", "    call builtin_add\n    push %rax\n"},
  "-" = {"    pop %rax\n    pop %rbx\n    sub %rbx, %rax\n    push %rax\n", "    call builtin_sub\n    push %rax\n"},
  "/" = {"    pop %rax\n    pop %rbx\n    div %rbx, %rax\n    push %rax\n", "    call builtin_div\n    push %rax\n"},
  "*" = {"    pop %rax\n    pop %rbx\n    mul %rbx\n    push %rax\n", "    call builtin_mul\n    push %rax\n"},
}

special_ops := map[string]proc(s_expr)-> (string, bool) {
  "if" = generate_if_asm,
}

generate_if_asm :: proc (s: s_expr)->(result: string, ok: bool){
  @static label_num: int
  if len(s) == 4 {
    label_num += 1
    return fmt.tprintf(`
  jmp if_cond_%d
if_false_%d:
%s
    jmp if_end_%d
if_true_%d:
%s
    jmp if_end_%d
if_cond_%d:
%s
    pop %%rax
    cmp $0, %%rax
    je if_false_%d
    jmp if_true_%d
if_end_%d:
`,  label_num, label_num, 
        compile_s_expr(s[3]) or_return,
        label_num, label_num,
        compile_s_expr(s[2]) or_return,
        label_num, label_num,
        compile_s_expr(s[1]) or_return,
        label_num, label_num, label_num), true
  } else {
    return "", false
  }
}

single_instruction :: proc (name, op_line: string) -> string {
  return fmt.tprintf(
`# instruction %s
    pop %%rax
    pop %%rbx
    %s
    push %%rax
`, name, op_line)
}

repeated_instruction :: proc (name, op_line: string) -> string {
  return fmt.tprintf(
`builtin_%s:
    pop %%rbp    # get back the return address
    cmp $0, %%rdi
    jle bad_call

    pop %%rax
    dec %%rdi

    jz . + 8
    pop %%rbx
    %s
    jmp . - 9
    jmp *%%rbp    #return
`, 
    name, op_line)
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

