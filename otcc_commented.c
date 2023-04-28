#include <stdio.h>
#include <string.h>
#include <stdlib.h>

/* current input character for tokenizing */
int tokch;

/* input file pointer: FILE* as an int (C is great! lol) */
int input_fp;

/* symbol table key buffer: all textual symbols and keywords are appended to this
   buffer. A single space is used as a delimiter for symbols. This buffer is linearly
   searched for matching symbols. Keywords are inserted first amd thus it's easy to
   differentiate them by the resulting serach index. An additional quirk: macro-expansion
   text is also appending to this array and re-consumed in the tokch advance to expand
   macros. Tricky tricky fun fun!
*/
int symkey_base;
int symkey_end;

/* pointer to an array of symbol data. Each entry is a pair of ints.
   The pair is used in a few different ways depending on symbol type.
   The symbol type is generally implied by the parser syntax:
     - define-macro symbol: <1> <pointer-to-expansion-str>
     - global-variable:     <absolute-address> <...unused...>
     - local-variable:      <stack-offset> <...unused...>
     - function:            <absolute-address> <backpatch-list>
     - library-symbol       <0> <...unused...>
     - unresolved-function  <0> <backpatch-list>
*/
int symval_base;

/* token type, abused in lots of fabulous ways:
     - single-char tokens  <8-bit ascii code>
     - two-char operator:  <1>
     - literal numbers:    <2>   (with literal value stored in "tok_data")
     - keywords:           <some value in [256,536]>
     - idents:             <pointer to int-pair in symval_base buffer>
*/
int tok;

/* additional token data, semantics depending on context */
int tok_data;

/* for each ident token: points to the ident C-string in the symtab buffer */
int ident_ptr;

/* for each operator token: precedence level */
int prec;

/* codegen buffer where x86-32 code is emitted
   everything in [codegen_base, codegen_ptr] is generated x86-32 code
   (perhaps still with missing backpatching)
*/
int codegen_base;
int codegen_ptr;

/* current offset into the stackframe for the function current being compiled
   this is reset to 0 everytime a new function decl starts */
int stackframe_offset;

/* global data buffer, only end pointer is retained as the base pointer is never
   actually needed */
int globdata_end;

/* patch list for return statement */
int return_patch_addr;

/* pointer to macro expansion text currently being injected in the input char stream */
int macro_text;

/* when macro expansion is complete, this char should be emitted next */
int next_char_after_macro;

symkey_append_char(ch)
{
  /* append the symbtab text buffer */

  *(char*)symkey_end++ = ch;
}

char_next()
{
  /* get next char for tokenizing ... possibly macro expansion text */

  if (macro_text) { /* macro data to expand? */
    tokch = *(char*)macro_text++;
    if (tokch == 2) { /* macro terminaor sentinel */
      macro_text = 0; /* done! */
      tokch = next_char_after_macro;
    }
  }
  else {
    tokch = fgetc(input_fp);
  }
}


is_ident_char()
{
  return isalnum(tokch) | tokch == '_';
}

unescape_char()
{
  if (tokch == '\\') {
    char_next();
    if (tokch == 'n') tokch = '\n';
    /* NOTE: if the tokch is not 'n', we just use the next char. This means that
       \' \" \\ all "just work" with no additional code =) */
  }
}

tok_next()
{
  int op_data, op_char1, op_char2;

  /* Handle "#define NAME thing" preprocessing */
  while (isspace(tokch) | tokch == '#') {
    if (tokch == '#') {
      char_next(); /* consume '#' */
      tok_next();
      if (tok == 536) { /* is "define" ? */
        tok_next(); /* consume "define" */
        symkey_append_char(' ');
        *(int*)tok = 1;  /* 1 means tok is preproc define */
        *(int*)(tok+4) = symkey_end; /* expansion str in in the symkey buffer */
      }
      /* NOTE: if not "define", just ignore it */
      /* consume the rest of the preproc line */
      while (tokch != '\n') {
        symkey_append_char(tokch);
        char_next();
      }
      symkey_append_char(tokch); /* append '\n' */
      symkey_append_char(2); /* 2 is a sentinel for macro expansion */
    }
    char_next(); /* consume either <space> or preproc line-term '\n' */
  }

  prec = 0;
  tok = tokch;
  if (is_ident_char()) {
    /* copy all ident chars into symkey buf */
    symkey_append_char(' ');
    ident_ptr = symkey_end;
    while (is_ident_char()) {
      symkey_append_char(tokch);
      char_next();
    }

    if (isdigit(tok)) {
      /* number literal? */
      tok_data = strtol(ident_ptr, 0, 0);
      tok = 2; /* literal number */
    }
    else {
      /* keyword? */
      *(char*)symkey_end = ' ';

      /* Search symkey array: find offset in array of matching string */
      tok = strstr(symkey_base, ident_ptr-1) - symkey_base;
      *(char*)symkey_end = 0;

      /* Convert offset to a 'tok' value via a formula:
           "int"    => 256 + 8*0  => 256
           "if"     => 256 + 8*4  => 288
           "else"   => 256 + 8*7  => 312
           "while"  => 256 + 8*12 => 352
           "break"  => 256 + 8*18 => 400
           "return" => 256 + 8*24 => 448
           "for"    => 256 + 8*31 => 504
           "define" => 256 + 8*35 => 536
           "main"   => 256 + 8*42 => 592
           ... etc for user-defined symbols ...
      */
      tok = tok * 8 + 256;

      if (tok > 536) {
        /* Not a keyword.. assume ident */
        tok = symval_base + tok; /* tok = "ptr to symbol addr" */
        if (*(int*)tok == 1) { /* define macro needing expansion? */
          macro_text = *(int*)(tok + 4);
          next_char_after_macro = tokch;
          char_next();
          tok_next();
        }
      }
    }
  }

  else {
    char_next();
    if (tok == '\'') {
      tok = 2; /* literal number */
      unescape_char();
      tok_data = tokch;
      char_next(); /* consume literal */
      char_next(); /* consume trailing ' */
    }
    else if (tok == '/' & tokch == '*'){
      /* Drop comments */
      char_next();
      while (tokch) {
        while (tokch != '*') char_next();
        char_next(); /* consume '*' */
        if (tokch == '/') tokch = 0; /* terminate loop */
      }
      char_next(); /* consume '/' */
      tok_next(); /* retry! */
    }
    else {
      /* Here we have a big amazing encoding of all operators this compiler supports. In this single encoding,
         we have:
           - 23 operators
           - single and double char operators
           - operator precedence (for a Pratt-style Recursive-Descent Parser)
           - a special base-64 encoding of binary payloads:
             - often raw x86-32 machine code
             - or special-case integer data

         Format:
           <operator-char1> <operator-char2> <base-64-data> <precedence byte>

         For single char ops, '@' is used for operator-char2
         Base 64 is just a mapping of [0, 63] => [34, 97] which is decoded by "- 98" and then "+ 64"
         Finally, the precedence is given by a char >=98

         NOTE: This base64 range was clearly picked to be (1) contiguous and (2) in the visible char range.
         I'm surprised because it maps 0 to '"' and surprised no 0's occurred. I suspect this range was chosen
         as it happened to avoid the problematic \ escape char. Indeed, we have a few '.' chars that would map
         to \ if the [35, 98] range was instead selected!
      */
      op_data = "++#m--%am*@R<^1c/@%[_[H3c%@%[_[H3c+@.B#d-@%:_^BKd<<Z/03e>>`/03e<=0f>=/f<@.f>@1f==&g!='g&&k||#l&@.BCh^@.BSi|@.B+j~@/%Yd!@&d*@b";

      /*
        Expanded:

        "++#m"        =>  op1: '+'  op2: '+'  dat: "#"      (1)                             prec: 'm' (11)
        "--%am"       =>  op1: '-'  op2: '-'  dat: "%a"     (-1)                            prec: 'm' (11)
        "*@R<^1c"     =>  op1: '*'  op2: '@'  dat: "R<^1"   ("imul ecx,eax")                prec: 'c' ( 1)
        "/@%[_[H3c"   =>  op1: '/'  op2: '@'  dat: "%[_[H3" ("xchg ecx,eax; cdq; idiv ecx") prec: 'c' ( 1)
        "%@%[_[H3c"   =>  op1: '%'  op2: '@'  dat: "%[_[H3" ("xchg ecx,eax; cdq; idiv ecx") prec: 'c' ( 1)
        "+@.B#d"      =>  op1: '+'  op2: '@'  dat: ".B#"    ("add eax,ecx")                 prec: 'd' ( 2)
        "-@%:_^BKd"   =>  op1: '-'  op2: '@'  dat: "%:_^BK" ("sub eax,ecx; neg eax")        prec: 'd' ( 2)
        "<<Z/03e"     =>  op1: '<'  op2: '<'  dat: "Z/03"   ("xchg ecx,eax; shl eax,cl")    prec: 'e' ( 3)
        ">>`/03e"     =>  op1: '>'  op2: '>'  dat: "`/03"   ("xchg ecx,eax; sar eax,cl")    prec: 'e' ( 3)
        "<=0f"        =>  op1: '<'  op2: '='  dat: "0"      (14 => "setle")                 prec: 'f' ( 4)
        ">=/f"        =>  op1: '>'  op2: '='  dat: "/"      (13 => "setge")                 prec: 'f' ( 4)
        "<@.f"        =>  op1: '<'  op2: '@'  dat: "."      (12 => "setl")                  prec: 'f' ( 4)
        ">@1f"        =>  op1: '>'  op2: '@'  dat: "1"      (15 => "setg")                  prec: 'f' ( 4)
        "==&g"        =>  op1: '='  op2: '='  dat: "&"      ( 4 => "sete")                  prec: 'g' ( 5)
        "!='g"        =>  op1: '!'  op2: '='  dat: "'"      ( 5 => "setne")                 prec: 'g' ( 5)
        "&&k"         =>  op1: '&'  op2: '&'  dat: ""       (0 [early exit if 0])           prec: 'k' ( 9)
        "||#l"        =>  op1: '|'  op2: '|'  dat: "#"      (1 [early exit if 1])           prec: 'l' (10)
        "&@.BCh"      =>  op1: '&'  op2: '@'  dat: ".BC"    ("and eax,ecx")                 prec: 'h' ( 6)
        "^@.BSi"      =>  op1: '^'  op2: '@'  dat: ".BS"    ("xor eax,ecx")                 prec: 'i' ( 7)
        "|@.B+j"      =>  op1: '|'  op2: '@'  dat: ".B+"    ("or eax,ecx")                  prec: 'j' ( 8)
        "~@/%Yd"      =>  op1: '~'  op2: '@'  dat: "/%Y"    ("not eax")                     prec: 'd' ( 2)
        "!@&d"        =>  op1: '!'  op2: '@'  dat: "&"      ( 4 => "sete")                  prec: 'd' ( 2)
        "*@b"         =>  op1: '*'  op2: '@'  dat: ""       (0 [ignored])                   prec: 'b' ( 0)

       */

      /* Parse all operators
         Suspecting that this encodes raw x86 machine code to emit for each operator...
         Seems to be a Base-64 style encoding, all chars are visible range, or >= 32 (' ') and the "- 98" below
         and the later "+ 64"  hints that we're using the range [32, 97] as [0, 63].
         The calculation "z = z * 64 + C + 64" is unpacking this 6-bit encoding into a 32-bit int */
      /* Cleverly, the terminating char is assigned to "C" and later used.. what is this? Count? */


      while (op_char1 = *(char*)op_data++){
        op_char2 = *(char*)op_data++;
        tok_data = 0;
        while((prec = *(char*)op_data++ - 98) < 0) tok_data = tok_data * 64 + prec + 64;
        if (op_char1 == tok & (op_char2 == tokch | op_char2 == '@')) {
          if (op_char2 == tokch){
            char_next();
            tok = 1; /* two-char operator */
          }
          break;
        }
      }
    }
  }
}

emit(dat)
{
  /* Emit raw data into the codegen buf: as many bytes as are used */

  while (dat && dat != -1) {
    *(char*)codegen_ptr++ = dat;
    dat = dat >> 8;
  }
}

backpatch_all(patch_addr)
{
  /* Fun trick: during compilation we build a linked-list of unpactched targets
     in the codegen. When a patch is resolved, we simply walk the entire list
     and patch each entry until NULL is reached. This allows for arbitrarily
     deep usage of things like "break" or "return". Those just add themselves
     to the patch list and it gets patched up with everything else!
  */

  int next;
  while (patch_addr) {
    next = *(int*)patch_addr;
    *(int*)patch_addr = codegen_ptr - patch_addr - 4;
    patch_addr = next;
  }
}

emit_with_imm32(g,e)
{
  /* Emit raw data and then a 32-bit immediate: return address of immediate (for back-patching) */

  emit(g);
  *(int*)codegen_ptr = e;

  e = codegen_ptr;
  codegen_ptr = codegen_ptr+4;
  return e;
}

emit_num(num)
{
  /* mov eax,<num> */
  emit_with_imm32(184, num);
}

emit_jmp_uncond(target){
  /* jmp <32-bit-target> */
  return emit_with_imm32(233, target);
}

emit_jmp_cond(j, target)
{
  /* test eax,eax; j<cond> <32-bit target> */
  emit(1032325);
  return emit_with_imm32(132+j, target);
}

emit_compare(e)
{
  /* cmp ecx,eax
     mov eax,$0
     set<cond> al
  */
  emit(49465);
  emit_num(0);
  emit(15);
  emit(e+144);
  emit(192);
}

emit_mem_op(op, addr)
{
  /* Used to encode a number of operations, all operating on some variable address
     this can be used on either params/locals or globals:

                      globals                  locals                special note
     ----------------------------------------------------------------------------------------------------
       op: 0     "add [addr],imm8"        "add [ebp+off],imm8"      (imm8 needs to emitted seperately)
       op: 6     "mov [addr],eax"         "mov [ebp+off],eax"
       op: 8     "mov eax,[addr]"         "mov eax,[ebp+off]"
       op: 10    "lea eax,[addr]"         "lea eax [ebp+off]"
  */

  emit(op + 131);
  emit_with_imm32((addr < 512) << 7 | 5, addr); /* (addr < 512) is true for locals */
}

compile_unary(arg)
{
  /* Compile a unary expressions and "atoms" including: string-literals, number-literals,
     cast-and-deref operations, address-of, variable loads / stores, and function calls.
     Also, handle grouping "( EXPR )" recursions

     Parameter 'arg' could be named 'allow_lvalue' but the variable is abused later to
     mean 'param_offset' for pushing call args to the stack
  */

  int prec_save;
  int data_save;
  int tok_save;
  int sym_addr;

  /* sym_addr == 1 means that the address is in eax */
  sym_addr = 1;

  /* string literals */
  if (tok == '"') {
    emit_num(globdata_end); /* emit addr to literal */
    /* append all the string literal bytes to the global data buffer */
    while (tokch != '"') {
      unescape_char();
      *(char*)globdata_end++ = tokch;
      char_next();
    }
    *(char*)globdata_end = 0; /* null-term it */

    /* align up the globdata_end (so globals are properly aligned) */
    globdata_end = globdata_end + 4 & -4;

    char_next(); /* consume the trailing '"' */
    tok_next();  /* advance to next token */
  }

  else {
    prec_save = prec;
    data_save = tok_data;
    tok_save = tok;
    tok_next();
    if (tok_save == 2) { /* literal number */
      emit_num(data_save);
    }
    else if (prec_save == 2) { /* unary compares?? */
      compile_unary(0);
      emit_with_imm32(185, 0); /* mov ecx, 0 (use 0 to convery binary ops to unary ops) */
      if (tok_save == '!') emit_compare(data_save);
      else emit(data_save); /* unary '+', '-', '~' */
    }
    else if (tok_save == '(') { /* grouping? */
      compile_expr();
      tok_next(); /* consume ')' */
    }
    else if (tok_save == '*') { /* cast and deref ? */
      tok_next(); /* consume '(' */
      tok_save = tok; /* save the type token, e.g. "int" */
      tok_next(); /* consume type token */
      tok_next(); /* consume '*' or '(' */
      if (tok == '*') { /* function ptr? */
        tok_next(); /* consume '*' */
        tok_next(); /* consume ')' */
        tok_next(); /* consume '(' */
        tok_next(); /* consume ')' */
        tok_save = 0; /* 0 means "function pointer" */
      }
      tok_next(); /* consume ')' */
      compile_unary(0); /* compile value, producing address in eax */
      if (tok == '=') { /* assign lvalue? */
        tok_next(); /* consume '=' */
        emit(80); /* push eax (save lvalue addr) */
        compile_expr(); /* compile rvalue expression, result in eax */
        emit(89); /* pop ecx (restore lvalue addr) */
        emit(392+(tok_save == 256)); /* (store) mov [ecx],eax  ...or... mov [ecx],al */
      }
      else if (tok_save) { /* load value? (not a fn ptr) */
        if (tok_save == 256) emit(139); /* mov eax,[eax] (load 32-bit into 32-bit reg) */
        else emit(48655); /* movsx eax,[eax] (load 8-bit into 32-bit reg, sign-extended) */
        codegen_ptr++; /* emit trailing 00 byte to complete above instructions */
      }
    }
    else if (tok_save == '&') {
      emit_mem_op(10, *(int*)tok); /* lea <var-addr> */
      tok_next(); /* consume ident */
    }
    else { /* regular symbol / variable */
      sym_addr = *(int*)tok_save;
      if (!sym_addr) sym_addr = dlsym(0, ident_ptr);
      if (tok == '=' & arg){ /* store to var? (use 'arg' to decide if this is okay) */
        tok_next();
        compile_expr();
        emit_mem_op(6, sym_addr); /* (store) */
      }
      else if (tok != '(') { /* load from var? */
        emit_mem_op(8, sym_addr); /* (load) */
        if (prec == 11) { /* special handling for "++" and "--" codegen */
          emit_mem_op(0, sym_addr); /* emit "add" instr with imm8 (on mem directly) */
          emit(tok_data); /* emit the imm8: either +1 or -1 */
          tok_next(); /* consume the "++" or "--" */
        }
      }
    }
  }
  /* call? */
  if (tok == '(') {
    if (sym_addr == 1) emit(80); /* push eax (addr in eax, now on the stack) */
    data_save = emit_with_imm32(60545,0); /* sub esp, <patch 32-bit> */
    tok_next(); /* consume '(' */
    arg = 0;
    while (tok != ')') {
      compile_expr();
      emit_with_imm32(2393225,arg); /* mov [esp+<arg-offset>],eax */
      if (tok == ',') tok_next(); /* consume ',' */
      arg = arg + 4;
    }
    *(int*)data_save = arg; /* push up stack ptr sub */
    tok_next(); /* consume ')' */

    if (!sym_addr) { /* unresolved function call? */
      tok_save = tok_save + 4;
      *(int*)tok_save = emit_with_imm32(232, *(int*)tok_save); /* call imm32 */
    }
    else if (sym_addr == 1) { /* addr was in eax, then pushed to the stack, call it from the stack */
      emit_with_imm32(2397439, arg); /* call [esp+off] */
      arg = arg + 4;
    }
    else { /* we have an abs addr, call it with imm32 encoding */
      emit_with_imm32(232, sym_addr - codegen_ptr - 5); /* call imm32 */
    }

    /* cleanup stack (if required) */
    if (arg) emit_with_imm32(50305, arg); /* add esp, imm32 */
  }
}

/* expression precedence recursive function */
compile_expr_prec(level)
{
  /* A fun thing: level gets decremented right away, so there's actually no
     code here to handle the prec == 11 level, aka "++" and "--". How is that
     handled? Over in compile_unary() as a special case of course :-)

     In fact, prec==11 seems to be another fun hack also because "++" absolutely
     binds tighter than "&&" (prec: 10). So, prec=11 here is just an "ignore this" trick :-)
  */

  int data_save, tok_save, patch_chain;

  if (level-- == 1) compile_unary(1); /* base case */
  else {
    compile_expr_prec(level); /* process all expressions below this level */
    patch_chain = 0;
    while (level == prec) { /* process all expressions on this level */
      tok_save = tok;
      data_save = tok_data;
      tok_next();
      if (level > 8) { /* ops '&&' and '||' only */
        patch_chain = emit_jmp_cond(data_save, patch_chain);
        compile_expr_prec(level);
      }
      else { /* other levels */
        emit(80); /* push eax (save left-hand side) */
        compile_expr_prec(level);
        emit(89); /* pop ecx (restore left-hand side as ecx)*/
        if (level == 4 | level == 5) { /* comparison operation? */
          emit_compare(data_save);
        }
        else{ /* normal operation */
          emit(data_save);
          if (tok_save == '%') emit(146); /* xchg edx,eax (modulus result is in edx) */
        }
      }
    }
    if (patch_chain && level > 8) { /* patch early exit exprs */
      patch_chain = emit_jmp_cond(data_save, patch_chain); /* on failure, jump over the success code */
      /* succes code here */
      emit_num(data_save ^ 1); /* early_exit: 0 => mov eax, 1 | early_exit: 1 => mov eax, 0 */
      emit_jmp_uncond(5); /* jump over failure code */
      backpatch_all(patch_chain); /* patch all the early exits */
      emit_num(data_save); /* mov eax,early_exit_val */
    }
  }
}

compile_expr()
{
  /* we start on precedence level 11 and work down recursively */
  compile_expr_prec(11);
}

compile_expr_is_zero_jmp()
{
  /* if (<expr> == 0) goto <patch 32-bit> */
  compile_expr();
  return emit_jmp_cond(0,0); /* je <patch 32-bit> */
}

compile_statement(break_patch_addr)
{
  /* arg: pointer location for patching "break" stmts: caller will do the final patching */

  int patch_1, patch_2, tmp;
  if (tok == 288) { /* tok == "if" */
    tok_next(); /* consume "if" */
    tok_next(); /* consume "(" */
    patch_1 = compile_expr_is_zero_jmp(); /* compile condition and false jump, save patch addr */
    tok_next(); /* consume ")" */
    compile_statement(break_patch_addr); /* compile body statement */
    if (tok == 312){ /* tok == "else" */
      tok_next(); /* consume "else" */
      patch_2 = emit_jmp_uncond(0); /* compile jmp for if-body to jump over else-body, save patch addr */
      backpatch_all(patch_1); /* patch if false jump to enter else-body */
      compile_statement(break_patch_addr);
      backpatch_all(patch_2); /* patch if true jump over else to the end */
    }
    else {
      backpatch_all(patch_1); /* no else-stmt, simply patch the if-stmt false jump */
    }
  }
  else if (tok == 352 | tok == 504) { /* tok == "while" | tok == "for" */
    tmp = tok; /* save tok so we can advance */
    tok_next(); /* consume keyword */
    tok_next(); /* consume "(" */
    if (tmp == 352) { /* while? */
      patch_2 = codegen_ptr; /* save loop-start addr */
      patch_1 = compile_expr_is_zero_jmp(); /* compile condition and false jump, save patch addr */
    }
    else { /* for? */
      if (tok != ';') compile_expr(); /* compile init expr */
      tok_next(); /* consume ';' */
      patch_2 = codegen_ptr; /* save loop-start addr */
      patch_1 = 0;
      if (tok != ';') patch_1 = compile_expr_is_zero_jmp(); /* compile condition and false jump, save patch addr */
      tok_next(); /* consume ';' */
      if (tok != ')') {
        /* compile inc-expr */
        /* NOTE: This is super tortured and bad codegen. Presumably it was done because it lead to
           much less compiler source text vs a more conventional codegen layout */
        tmp = emit_jmp_uncond(0); /* jump over the inc-expr into the loop body */
        compile_expr(); /* compile inc-expr */
        emit_jmp_uncond(patch_2 - codegen_ptr - 5); /* jump backwards to cond-expr */
        backpatch_all(tmp); /* patch the jump over the inc-expr */
        patch_2 = tmp + 4; /* adjust loop-start to be the inc-expr */
      }
    }
    tok_next(); /* consume ')' */
    compile_statement(&patch_1); /* loop-body */
    emit_jmp_uncond(patch_2 - codegen_ptr - 5); /* jump back to loop-start */
    backpatch_all(patch_1); /* backpatch the loop-exit */
  }
  else if (tok == '{'){
    tok_next(); /* consume '{' */
    compile_decls(1); /* compile local decls for the block */
    while (tok != '}') compile_statement(break_patch_addr); /* compile all statements for the block */
    tok_next(); /* consume '}' */
  }
  else{
    if (tok == 448) { /* return? */
      tok_next(); /* consume "return" */
      if( tok != ';') compile_expr(); /* compile return expr */
      return_patch_addr = emit_jmp_uncond(return_patch_addr); /* chain patch on return location: multiple return-stmts are cool */
    }
    else if (tok == 400) { /* break? */
      tok_next(); /* consume "break" */
      *(int*)break_patch_addr = emit_jmp_uncond(*(int*)break_patch_addr); /* chain patch on break end-loop location: multiple break-stmts are cool */
    }
    else if (tok != ';') compile_expr(); /* compile expr-stmt */
    tok_next(); /* consume ';' */
  }
}

compile_decls(local_func)
{
  int local;
  while (tok == 256 | tok !=-1 & !local_func){
    if (tok == 256) { /* var decl? tok == "int" ? */
      tok_next(); /* consume "int" */
      while (tok != ';') {
        if (local_func) { /* local var? */
          stackframe_offset = stackframe_offset + 4;
          *(int*)tok = -stackframe_offset;
        }
        else{ /* global var? */
          *(int*)tok = globdata_end;
          globdata_end = globdata_end + 4;
        }
        tok_next(); /* consume ident */
        if (tok == ',') tok_next(); /* consume ',' (optional) */
      }
      tok_next(); /* consume ';' */
    }
    else{ /* function decl */
      backpatch_all(*(int*)(tok + 4));
      *(int*)tok = codegen_ptr;
      tok_next(); /* consume ident */
      tok_next(); /* consume '(' */
      local = 8;
      while (tok != ')') {
        *(int*)tok = local; /* save param offset */
        local = local + 4;
        tok_next(); /* consume ident */
        if (tok == ',') tok_next(); /* consume ',' (optional) */
      }
      tok_next(); /* consume ')' */
      return_patch_addr = stackframe_offset = 0;
      emit(15042901); /* push ebp; mov ebp, esp */
      local = emit_with_imm32(60545,0); /* sup esp, <patch 32-bit> */
      compile_statement(0);
      backpatch_all(return_patch_addr); /* patch up all jumps to the return postamble */
      emit(50121); /* leave; ret */
      *(int*)local = stackframe_offset; /* patch stack frame adjust */
    }
  }
}

main(argc, argv)
{
  input_fp = stdin;
  if (argc-- > 1) {
    argv = argv + 4;
    input_fp = fopen(*(int*)argv,"r");
  }

  symkey_end = strcpy(symkey_base = calloc(1,99999)," int if else while break return for define main ") + 48;
  globdata_end = calloc(1,99999);
  codegen_ptr = codegen_base = calloc(1,99999);
  symval_base = calloc(1,99999);

  char_next();
  tok_next();
  compile_decls(0);

  return (*(int(*)())*(int*)(symval_base+592))(argc, argv); /* Call main */
}
