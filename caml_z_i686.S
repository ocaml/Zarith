/*
   Assembly version for the fast path of some functions in Z:
   - x86 target
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
        
        .macro  c_call proc
        call    ml_z_\proc\()
        .endm


        /* operation counter */
        .macro  op
        incl    ml_z_ops_as
        .endm

        
        /* unary arithmetics */
        /* ***************** */

        /* neg */
        prolog  neg
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Lneg
        not     %eax
        add     $3, %eax
        jo      .Lneg
        op
        ret
.Lneg:
        pushl   4(%esp)
        c_call  neg
        add     $4, %esp
        ret
        epilog  neg


        /* abs */
        prolog  abs
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Labs
        test    %eax, %eax
        jns     .Labs2
        not     %eax
        add     $3, %eax
        jo      .Labs
.Labs2:
        op
        ret
.Labs:
        pushl   4(%esp)
        c_call  abs
        add     $4, %esp
        ret
        epilog  abs

        
        /* succ */
        prolog  succ
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Lsucc
        add     $2, %eax
        jo      .Lsucc
        op
        ret
.Lsucc:
        pushl   4(%esp)
        c_call  succ
        add     $4, %esp
        ret
        epilog  succ


        /* pred */
        prolog  pred
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Lpred
        sub     $2, %eax
        jo      .Lpred
        op
        ret
.Lpred:
        pushl   4(%esp)
        c_call  pred
        add     $4, %esp
        ret
        epilog  pred


        
        
        /* binary arithmetics */
        /* ****************** */

        /* add */
        prolog  add
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Ladd
        mov     8(%esp), %ecx
        test    $1, %ecx
        jz      .Ladd
        dec     %eax
        add     %ecx, %eax
        jo      .Ladd
        op
        ret
.Ladd:
        pushl   8(%esp)
        pushl   8(%esp)
        c_call  add
        add     $8, %esp
        ret
        epilog  add

 
        /* sub */
        prolog  sub
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Lsub
        mov     8(%esp), %ecx
        test    $1, %ecx
        jz      .Lsub
        sub     %ecx, %eax
        jo      .Lsub
        inc     %eax
        op
        ret
.Lsub:
        pushl   8(%esp)
        pushl   8(%esp)
        c_call  sub
        add     $8, %esp
        ret
        epilog  sub

        
        /* mul */
        prolog  mul
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Lmul
        mov     8(%esp), %ecx
        test    $1, %ecx
        jz      .Lmul
        dec     %eax
        sar     %ecx
        imul    %ecx, %eax
        jo      .Lmul
        inc     %eax
        op
        ret
.Lmul:
        pushl   8(%esp)
        pushl   8(%esp)
        c_call  mul
        add     $8, %esp
        ret
        epilog  sub

        
        /* div */
        prolog  div
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Ldiv
        mov     8(%esp), %ecx
        test    $1, %ecx
        jz      .Ldiv
        sar     %ecx
        jz      .Ldiv   /* division by zero */
        cmp     $-1, %ecx
        je      .Ldivneg
        sar     %eax
        cdq
        idiv    %ecx
        sal     %eax
        inc     %eax
        op
        ret
.Ldivneg:
        /* division by -1, the only one that can overflow */
        not     %eax
        add     $3, %eax
        jo      .Ldiv
        op
        ret
.Ldiv:
        pushl   8(%esp)
        pushl   8(%esp)
        c_call  div
        add     $8, %esp
        ret
        epilog  div

        
        /* rem */
        prolog  rem
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Lrem
        mov     8(%esp), %ecx
        test    $1, %ecx
        jz      .Lrem
        sar     %ecx
        jz      .Lrem   /* division by zero */
        cmp     $-1, %ecx
        je      .Lremneg
        sar     %eax
        cdq
        idiv    %ecx
        sal     %edx
        lea     1(%edx), %eax
        op
        ret
.Lremneg:
        /* division by -1 */
        mov     $1, %eax
        op
        ret
.Lrem:
        pushl   8(%esp)
        pushl   8(%esp)
        c_call  rem
        add     $8, %esp
        ret
        epilog  rem

        
        /* bit operations */
        /* ************** */

        
        /* not */
        prolog  lognot
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Llognot
        dec     %eax
        not     %eax
        ret
.Llognot:
        pushl   %eax
        c_call  lognot
        add     $4, %esp
        ret
        epilog  lognot


        /* or */
        prolog  logor
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Llogor
        mov     8(%esp), %ecx
        test    $1, %ecx
        jz      .Llogor
        or      %ecx, %eax
        op
        ret
.Llogor:
        pushl   8(%esp)
        pushl   8(%esp)
        c_call  logor
        add     $8, %esp
        ret
        epilog  logor

        /* and */
        prolog  logand
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Llogand
        mov     8(%esp), %ecx
        test    $1, %ecx
        jz      .Llogand
        and     %ecx, %eax
        op
        ret
.Llogand:
        pushl   8(%esp)
        pushl   8(%esp)
        c_call  logand
        add     $8, %esp
        ret
        epilog  logand

        
        /* xor */
        prolog  logxor
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Llogxor
        mov     8(%esp), %ecx
        test    $1, %ecx
        jz      .Llogxor
        xor     %ecx, %eax
        inc     %eax
        op
        ret
.Llogxor:
        pushl   8(%esp)
        pushl   8(%esp)
        c_call  logxor
        add     $8, %esp
        ret
        epilog  logxor


        /* shift_left */
        prolog  shift_left
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Lshift_left
        mov     8(%esp), %ecx
        test    $1, %ecx
        jz      .Lshift_left
        sar     %ecx
        cmp     $31, %ecx
        jae     .Lshift_left
        dec     %eax
        sal     %cl, %eax
        mov     %eax, %edx
        sar     %cl, %edx
        inc     %edx
        cmp     4(%esp), %edx
        jne     .Lshift_left /* overflow */
        inc     %eax
        op
        ret
.Lshift_left:
        pushl   8(%esp)
        pushl   8(%esp)
        c_call  shift_left
        add     $8, %esp
        ret
        epilog  shift_left


        /* shift_right */
        prolog  shift_right
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Lshift_right
        mov     8(%esp), %ecx
        test    $1, %ecx
        jz      .Lshift_right
        sar     %ecx
        cmp     $31, %ecx
        jae     .Lshift_right2
        sar     %cl, %eax
        or      $1, %eax
        op
        ret
.Lshift_right2:
        /* shift by 31 or more */
        test    %eax, %eax
        js      .Lshift_right3
        mov     $1, %eax
        op
        ret
.Lshift_right3:
        mov     $-1, %eax
        op
        ret
.Lshift_right:
        pushl   8(%esp)
        pushl   8(%esp)
        c_call  shift_right
        add     $8, %esp
        ret
        epilog  shift_right
