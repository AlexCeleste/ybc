
 YBC: a compiler for B
=======================

**About:**

YBC ("Yasha's B Compiler") is a compiler for the B Programming Language, as specified in [Ken Thompson's B Manual](http://cm.bell-labs.com/cm/cs/who/dmr/kbman.html) and [Brian Kernighan's tutorial](http://cm.bell-labs.com/cm/cs/who/dmr/btut.html), or as close to it as possible. The compiler produces x86 code (32-bit only) which should be ABI-compatible with C.

The compiler is made available under the MIT/X11 licence, see [LICENSE file](LICENSE).

You will not (ever) be able to use this compiler to build antique B programs from the 70s. Fundamental differences (see below) make such a task impossible - this is a toy, intended for getting the feel of B (or, if you're a maniac, writing new programs).

The compiler outputs GAS assembly and assumes the existence of GCC (or Clang on Mac OS X) to assemble it.

[download for Mac OS X](https://github.com/Leushenko/ybc/releases/tag/v0.5-mac)  
[download for Linux](https://github.com/Leushenko/ybc/releases/tag/v0.5-linux)  
[download for Windows](https://github.com/Leushenko/ybc/releases/tag/v0.5-win)  


**Usage:**

    USAGE: ybc [options] <files>
    OPTIONS:
      -?, --help  Display this message
      -v          Show the compiler version
      -o          Set the name of the output executable (default 'a.out')
      -c          Produce separate .o files instead of an executable
      -s          Keep text assembly .s files
      -S          Only produce text assembly, do not assemble binaries
      --as        Set the command to use as the assembler
      --ld        Set the command to use as the linker
      --as-opt    Add an option to pass to the assembler (can repeat)
      --ld-opt    Add an option to pass to the linker (can repeat)
      --tree      Display the AST of the program source instead of compiling
      -w          Silence warnings
      --werr      Convert warnings to errors
      --warn      Notify but do not halt on warnings (default)

To produce a working executable/library, the produced .s or .o files must be linked with `b-lib.o`, which provides a few critical functions.


**Language:**

The language provided by the compiler falls as close as possible to the one described in the documents mentioned above (and to a lesser extent, the MH-TSS Reference). The descriptions of the language are somewhat vague and occasionally contradictory, but hopefully this is at least a vaguely faithful presentation.

1.  Major differences from the original:  
    - The original B was designed for a platform that addressed whole words. x86 doesn’t do this. Therefore on x86 it is not possible to have both `v[1]` refer to the second word element of a vector, *and* for `a[b] == b[a] == *(a + b)` to hold. As a result, the latter - despite being kept as a core principle of C - **does not apply** to this implementation; instead, the following rule holds:
    
            a[b] == *(a + b * WORD_SIZE)
    
        With no type information made available in the language at all, it simply isn’t possible to know what an argument represents and convert the pointer arithmetic accordingly (this also means that `*++p != p[1]`).
    
        (a fix would involve marshalling between B and the rest of the universe, which is too much effort)
    
    - There is also no library provided (except for the `char` function), on the grounds that you can just use libc instead.
    
    - This compiler currently doesn't support inconsistent short-circuiting behaviour of `&` and `|`; it isn't yet clear whether it should do so.

2.  Extension features:
    - The `break` and `default` keywords from the tutorial are present in the language.
    - It is possible to declare `auto` vectors with, or without, square brackets, suiting both documents.
    - Global functions do not need to be declared with `extrn` if they are only being invoked (if being used as a value they do need to be declared)
    - GCC-style computed `goto`s/labels-as-values are supported, insofar as a label can be used as a value, and `goto` accepts any expression as its argument (the manual is unclear on whether this is supposed to be allowed, so it is)
    - Single-statement functions do not need braces around the body (again, unclear, so permitted)
    - Functions automatically return the last value evaluated. Combined with the above, this means cute one-liners can be written in a more equational style:
    
            add(a, b) a + b;

3.  Pitfalls and differences from C
    - famously, compound assignment operators are the "wrong way around" in B:
    
            a =- 2;  /* Decrements a by 2, does not set it to -2 */
    
      So put spaces around your operators.
    - There is essentially no global scope in B. To use global variables, they must be redeclared *inside* the function body, with `extrn`. As an exception, this is not necessary for global names that appear only in the call position (so you don’t need to redeclare `printf` everywhere).
    - As above with the lack of pointer arithmetic, the complete lack of type information means that values do not "decay" where you might expect. Specifically, to take the address of a global function, you *must* use the `&` operator:
    
            auto f, mem; extrn malloc;
            f = &malloc;
            mem = f(256);
    
    - Don’t call function pointers stored in global *variables* (as opposed to their base global definitions) without either dereferencing them (with `*`) or storing them in a local variable (local variables can be called "bare", like in C - do not dereference manually).
    - Goes without saying but do not attempt to `goto` a function, to invoke a label, or to `goto` a label from outside its function. This will certainly break.
    - Do not attempt to assign to a function...
    - Names are allowed to include the dot character `.`, since it isn't in use otherwise (no floats, no structs). In fact, since it counts as a letter, you can have a function named `...` if you want.
    - The escape character in strings and char constants is `*`, not the backslash. (However, the string terminator is still `0`, for easy interop with libc. `'*e'` is unused.)

-------

Happy coding! (and please report any bugs)



Copyright (c) 2014 Alex Gilding
