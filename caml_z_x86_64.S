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

        /* function prolog epilog */
        
        .macro  prolog proc
        .text
        .globl  ml_as_z_\proc
        .type   ml_as_z_\proc, @function
        .align  16
ml_as_z_\proc:
        .endm

        .macro  epilog proc
        .size   ml_as_z_\proc, .-ml_as_z_\proc
        .endm


        /* calling C functions */
        
        .macro  c_jmp proc
        jmp     ml_z_\proc\()@PLT
        .endm


        /* operation counter */
        .macro  op
        mov     ml_z_ops_as@GOTPCREL(%rip), %rcx
        addq    $1, (%rcx)
        .endm

        
        /* unary arithmetics */
        /* ***************** */

        /* neg */
        prolog  neg
        test    $1, %rdi
        jz      .Lneg
        mov     %rdi, %rax
        not     %rax
        add     $3, %rax
        jo      .Lneg
        op
        ret
.Lneg:
        c_jmp   neg
        epilog  neg

        
        /* abs */
        prolog  abs
        test    $1, %rdi
        jz      .Labs
        mov     %rdi, %rax
        test    %rdi, %rdi
        jns     .Labs2
        not     %rax
        add     $3, %rax
        jo      .Lneg
.Labs2:
        op
        ret
.Labs:
        c_jmp   abs
        epilog  abs

        
        /* succ */
        prolog  succ
        test    $1, %rdi
        jz      .Lsucc
        mov     %rdi, %rax
        add     $2, %rax
        jo      .Lsucc
        op
        ret
.Lsucc:
        c_jmp   succ
        epilog  succ

        
        /* pred */
        prolog  pred
        test    $1, %rdi
        jz      .Lpred
        mov     %rdi, %rax
        sub     $2, %rax
        jo      .Lpred
        op
        ret
.Lpred:
        c_jmp   pred
        epilog  pred



        
        /* binary arithmetics */
        /* ****************** */

        
        /* add */
        prolog  add
        test    $1, %rdi
        jz      .Ladd
        test    $1, %rsi
        jz      .Ladd        
        lea     -1(%rdi), %rax
        add     %rsi, %rax
        jo      .Ladd
        op
        ret
.Ladd:
        c_jmp   add
        epilog  add

        
        /* sub */
        prolog  sub
        test    $1, %rdi
        jz      .Lsub
        test    $1, %rsi
        jz      .Lsub
        mov     %rdi, %rax
        sub     %rsi, %rax
        jo      .Lsub
        inc     %rax
        op
        ret
.Lsub:
        c_jmp   sub
        epilog  sub

  
        /* mul */
        prolog  mul
        test    $1, %rdi
        jz      .Lmul
        test    $1, %rsi
        jz      .Lmul
        mov     %rsi, %rcx
        lea     -1(%rdi), %rax
        sar     %rcx
        imul    %rcx, %rax
        jo      .Lmul
        inc     %rax
        op
        ret
.Lmul:
        c_jmp   mul
        epilog  mul

  
        /* div */
        prolog  div
        test    $1, %rdi
        jz      .Ldiv
        test    $1, %rsi
        jz      .Ldiv
        mov     %rsi, %rcx
        mov     %rdi, %rax
        sar     %rcx
        jz      .Ldiv   /* division by zero */
        cmp     $-1, %rcx
        je      .Ldivneg
        sar     %rax
        cqo
        idiv    %rcx
        sal     %rax
        inc     %rax
        op
        ret
.Ldivneg:
        /* division by -1, the only one that can overflow */
        not     %rax
        add     $3, %rax
        jo      .Ldiv
        op
        ret
.Ldiv:
        c_jmp   div
        epilog  div

        
        /* rem */
        prolog  rem
        test    $1, %rdi
        jz      .Lrem
        test    $1, %rsi
        jz      .Lrem
        mov     %rsi, %rcx
        mov     %rdi, %rax
        sar     %rcx
        jz      .Lrem   /* division by zero */
        cmp     $-1, %rcx
        je      .Lremneg
        sar     %rax
        cqo
        idiv    %rcx
        sal     %rdx
        lea     1(%rdx), %rax
        op
        ret
.Lremneg:
        /* division by -1 */
        mov     $1, %rax
        op
        ret
.Lrem:
        c_jmp   rem
        epilog  rem

        
        /* bit operations */
        /* ************** */


        /* not */
        prolog  lognot
        test    $1, %rdi
        jz      .Llognot
        lea     -1(%rdi), %rax
        not     %rax
        op
        ret
.Llognot:
        c_jmp   lognot
        epilog  lognot

        
        /* and */
        prolog  logand
        mov     %rdi, %rax
        and     %rsi, %rax
        test    $1, %rax
        jz      .Llogand
        op
        ret
.Llogand:
        c_jmp   logand
        epilog  logand

        
         /* or */
        prolog  logor
        test    $1, %rdi
        jz      .Llogor
        test    $1, %rsi
        jz      .Llogor
        mov     %rdi, %rax
        or      %rsi, %rax
        op
        ret
.Llogor:
        c_jmp   logor
        epilog  logor

        
         /* xor */
        prolog  logxor
        test    $1, %rdi
        jz      .Llogxor
        test    $1, %rsi
        jz      .Llogxor
        mov     %rdi, %rax
        xor     %rsi, %rax
        inc     %rax
        op
        ret
.Llogxor:
        c_jmp   logxor
        epilog  logxor

        
         /* shift_left */
#        prolog  shift_left
        test    $1, %rdi
        jz      .Lshift_left
        mov     %rsi, %rcx
        sar     %rcx
        cmp     $63, %rcx
        jae     .Lshift_left
        lea     -1(%rdi), %rax
        mov     %rax, %r8
        sal     %cl, %rax
        mov     %rax, %rdx
        sar     %cl, %rdx
        cmp     %r8, %rdx
        jne     .Lshift_left /* overflow */
        inc     %rax
        op
        ret
.Lshift_left:
        c_jmp   shift_left
        epilog  shift_left


        /* shift_right */
        prolog  shift_right
        test    $1, %rdi
        jz      .Lshift_right
        mov     %rsi, %rcx
        sar     %rcx
        cmp     $63, %rcx
        jae     .Lshift_right2
        mov     %rdi, %rax
        sar     %cl, %rax
        or      $1, %rax
        op
        ret
.Lshift_right2:
        /* shift by 63 or more */
        test    %rdi, %rdi
        js      .Lshift_right3
        mov     $1, %rax
        op
        ret
.Lshift_right3:
        mov     $-1, %rax
        op
        ret
.Lshift_right:
        c_jmp   shift_right
        epilog  shift_right

