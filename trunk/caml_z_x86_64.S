/*
   Assembly version for the fast path of some functions in Z:
   - x86_64 target
   - System 5 ABI and assembly syntax
   - GNU as

 
   This file is part of the Zarith library 
   http://forge.ocamlcore.org/projects/zarith .
   It is distributed under LGPL 2 licensing, with static linking exception.
   See the LICENSE file included in the distribution.
   
   Copyright (c) 2010-2011 Antoine Miné, Abstraction project.
   Abstraction is part of the LIENS (Laboratoire d'Informatique de l'ENS),
   a joint laboratory by:
   CNRS (Centre national de la recherche scientifique, France),
   ENS (École normale supérieure, Paris, France),
   INRIA Rocquencourt (Institut national de recherche en informatique, France).

 */

        
        /* helper functions */
        /* **************** */


        /* optional underscope prefix for symbols */
#ifdef Z_UNDERSCORE_PREFIX
#define SYMB(x) _##x
#else
#define SYMB(x)  x
#endif

        /* function prolog & epilog */

#if defined(Z_ELF)
#define FUNCTION_ALIGN 16
#endif
#if defined(Z_MACOS)
#define FUNCTION_ALIGN 4
#endif

#if defined(Z_ELF)
#define PROLOG(proc) \
        .text;   \
        .globl  SYMB(ml_as_z_##proc); \
        .type   SYMB(ml_as_z_##proc), @function; \
        .align  FUNCTION_ALIGN; \
SYMB(ml_as_z_##proc):

#define EPILOG(proc) \
        .size   SYMB(ml_as_z_##proc), .-SYMB(ml_as_z_##proc)
#endif

#if defined(Z_MACOS)
#define PROLOG(proc) \
        .text;   \
        .globl  SYMB(ml_as_z_##proc); \
        .align  FUNCTION_ALIGN; \
SYMB(ml_as_z_##proc):

#define EPILOG(proc)

#endif

        /* calling C functions */

#if defined(Z_ELF)
#define C_JMP(proc) \
        jmp     SYMB(ml_z_##proc@PLT)
#endif

#if defined(Z_MACOS)
#define C_JMP(proc) \
        jmp     SYMB(ml_z_##proc)
#endif

        /* operation counter */
        
#ifndef Z_PERF_COUNTER
#define OP
#else
#if defined(Z_ELF) || defined(Z_MACOS)
#define OP \
        mov     SYMB(ml_z_ops_as@GOTPCREL(%rip)), %rcx; \
        addq    $1, (%rcx)
#endif
#endif
        
        /* unary arithmetics */
        /* ***************** */

        /* neg */
        PROLOG(neg)
        test    $1, %edi
        jz      Lneg
        mov     %rdi, %rax
        not     %rax
        add     $3, %rax
        jo      Lneg
        OP
        ret
Lneg:
        C_JMP(neg)
        EPILOG(neg)

        
        /* abs */
        PROLOG(abs)
        test    $1, %edi
        jz      Labs
        mov     %rdi, %rax
        test    %rdi, %rdi
        jns     Labs2
        not     %rax
        add     $3, %rax
        jo      Lneg
Labs2:
        OP
        ret
Labs:
        C_JMP(abs)
        EPILOG(abs)

        
        /* succ */
        PROLOG(succ)
        test    $1, %edi
        jz      Lsucc
        mov     %rdi, %rax
        add     $2, %rax
        jo      Lsucc
        OP
        ret
Lsucc:
        C_JMP(succ)
        EPILOG(succ)

        
        /* pred */
        PROLOG(pred)
        test    $1, %edi
        jz      Lpred
        mov     %rdi, %rax
        sub     $2, %rax
        jo      Lpred
        OP
        ret
Lpred:
        C_JMP(pred)
        EPILOG(pred)



        
        /* binary arithmetics */
        /* ****************** */

        
        /* add */
        PROLOG(add)
        test    $1, %edi
        jz      Ladd
        test    $1, %esi
        jz      Ladd        
        lea     -1(%rdi), %rax
        add     %rsi, %rax
        jo      Ladd
        OP
        ret
Ladd:
        C_JMP(add)
        EPILOG(add)

        
        /* sub */
        PROLOG(sub)
        test    $1, %edi
        jz      Lsub
        test    $1, %esi
        jz      Lsub
        mov     %rdi, %rax
        sub     %rsi, %rax
        jo      Lsub
        inc     %rax
        OP
        ret
Lsub:
        C_JMP(sub)
        EPILOG(sub)

  
        /* mul */
        PROLOG(mul)
        test    $1, %edi
        jz      Lmul
        mov     %rsi, %rcx
        sar     %rcx
        jnc     Lmul
        lea     -1(%rdi), %rax
        imul    %rcx, %rax
        jo      Lmul
        inc     %rax
        OP
        ret
Lmul:
        C_JMP(mul)
        EPILOG(mul)

  
        /* div */
        PROLOG(div)
        mov     %rsi, %rcx
        cmp     $-1, %rsi
                   /* division by -1, the only one that can overflow */
        je      SYMB(ml_as_z_neg)
        sar     %rcx
        jnc     Ldiv	
        jz      Ldiv   /* division by zero */
        mov     %rdi, %rax
        sar     %rax
        jnc     Ldiv
        cqo
        idiv    %rcx
        lea     1(%rax, %rax), %rax
        OP
        ret
Ldiv:
        C_JMP(div)
        EPILOG(div)

        
        /* rem */
        PROLOG(rem)
        mov     %rdi, %rax
        sar     %rax
        jnc     Lrem
        mov     %rsi, %rcx
        sar     %rcx
        jnc     Lrem
        jz      Lrem   /* division by zero */
        cmp     $-1, %rcx
        je      Lremneg
        cqo
        idiv    %rcx
        lea     1(%rdx, %rdx), %rax
        OP
        ret
Lremneg:
        /* division by -1 */
        mov     $1, %eax
        OP
        ret
Lrem:
        C_JMP(rem)
        EPILOG(rem)

        
        /* bit operations */
        /* ************** */


        /* not */
        PROLOG(lognot)
        test    $1, %edi
        jz      Llognot
        lea     -1(%rdi), %rax
        not     %rax
        OP
        ret
Llognot:
        C_JMP(lognot)
        EPILOG(lognot)

        
        /* and */
        PROLOG(logand)
        mov     %rdi, %rax
        and     %rsi, %rax
        test    $1, %al
        jz      Llogand
        OP
        ret
Llogand:
        C_JMP(logand)
        EPILOG(logand)

        
         /* or */
        PROLOG(logor)
        test    $1, %edi
        jz      Llogor
        test    $1, %esi
        jz      Llogor
        mov     %rdi, %rax
        or      %rsi, %rax
        OP
        ret
Llogor:
        C_JMP(logor)
        EPILOG(logor)

        
         /* xor */
        PROLOG(logxor)
        test    $1, %edi
        jz      Llogxor
        test    $1, %esi
        jz      Llogxor
        mov     %rdi, %rax
        xor     %rsi, %rax
        inc     %rax
        OP
        ret
Llogxor:
        C_JMP(logxor)
        EPILOG(logxor)

        
         /* shift_left */
        PROLOG(shift_left)
        test    $1, %edi
        jz      Lshift_left
        mov     %rsi, %rcx
        sar     %rcx
        cmp     $63, %rcx
        jae     Lshift_left
        lea     -1(%rdi), %rax
        mov     %rax, %r8
        sal     %cl, %rax
        mov     %rax, %rdx
        sar     %cl, %rdx
        cmp     %r8, %rdx
        jne     Lshift_left /* overflow */
        inc     %rax
        OP
        ret
Lshift_left:
        C_JMP(shift_left)
        EPILOG(shift_left)


        /* shift_right */
        PROLOG(shift_right)
        test    $1, %edi
        jz      Lshift_right
        mov     %rsi, %rcx
	mov     $63, %eax
        sar     %rcx
        js      Lshift_right
        cmp     %rax, %rcx /* compare second argument to 63 */
        cmovae  %eax, %ecx /* if above or equal, then use 63 */
        mov     %rdi, %rax
        sar     %cl, %rax
        or      $1, %rax
        OP
        ret
Lshift_right:
        C_JMP(shift_right)
        EPILOG(shift_right)
        

