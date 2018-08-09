(library (maf6502)
  (export
    make-cpu reset-cpu
    *pc-init* *s-init* *p-init* *a-init* *x-init* *y-init*
    cpu-pc cpu-s cpu-p cpu-a cpu-x cpu-y  
    cpu-pc-set! cpu-s-set! cpu-p-set! cpu-a-set! cpu-x-set! cpu-y-set!
    *memory-size* make-memory initialize-memory
    memory-get-byte memory-get-two-bytes
    opcode-lookup)
  (import (chezscheme))

;; -------
;; | CPU |
;; -------
;; The 6502 has 6 registers, each one byte in size:
;;   * pc: program counter
;;   * s:  stack pointer
;;   * p:  processor status:
;;        7  6  5  4  3  2  1  0
;;        S  V     B  D  I  Z  C
;;     + 0 - C - carry: carry out of the MSB of arithmetic ops
;;     + 1 - Z - zero:  set to 1 if any arithmetic or logical op results in zero;
;;                      0 otherwise
;;     + 2 - I - interrupt: whether interrupts are enabled/disabled
;;     + 3 - D - decimal mode
;;     + 4 - B - break: set when a software interrupt (BRK) is executed
;;     + 5 -- not used; supposed to logical 1 at all times
;;     + 6 - V - overflow: set whenever an arithmetic op overflows a byte
;;     + 7 - S - sign: set to 1 if the result of an operation is negative;
;;                     0 if positive
;;    * a:  accumulator
;;    * x:  X register
;;    * y:  Y register

;; NB: initial values are per cl-6502/cpu.lisp:
;; NB:   PC: #xFFFC
;; NB:   S:  #xFD
;; NB:   P:  #x24 (i.e., #b00111111)
;; NB: the A, X, and Y registers are initialized to 0
(define *pc-init* #xFFFC)
(define *s-init* #xFD)
(define *p-init* #b00100100)
(define *a-init* 0)
(define *x-init* 0)
(define *y-init* 0)

(define-record-type cpu
  (nongenerative)
  (sealed #t)
  (fields
    (mutable pc)
    (mutable s)
    (mutable p)
    (mutable a)
    (mutable x)
    (mutable y))
  (protocol
    (lambda (new)
      (case-lambda 
        [() (new *pc-init* *s-init* *p-init* *a-init* *x-init* *y-init*)]
        [(pc s p a x y) (new pc s p a x y)]))))

(define reset-cpu
  (lambda (cpu)
    (cpu-pc-set! cpu *pc-init*)
    (cpu-s-set! cpu *s-init*)
    (cpu-p-set! cpu *p-init*)
    (cpu-a-set! cpu *a-init*)
    (cpu-x-set! cpu *x-init*)
    (cpu-y-set! cpu *y-init*)))

(define *carryp* 0)
(define *zerop* 1)
(define *interruptp* 2)
(define *decimalp* 3)
(define *breakp* 4)
(define *overflowp* 6)
(define *signp* 7)

;; ----------
;; | Memory |
;; ----------

;; NB: many systems that used the 6502 did not have 64K of memory
;; NB: but we'd need a virtual memory system to avoid representing the whole address space
(define *memory-size* (make-parameter 65536))

(define make-memory
  (lambda ()
    (make-bytevector (*memory-size*) 0)))

(define initialize-memory
  (lambda (memory)
    (bytevector-fill! memory 0)))

(define memory-get-byte bytevector-u8-ref)
(define memory-get-two-bytes
  (lambda (memory address)
    (bytevector-u16-ref memory address 'little)))

;; -----------
;; | Opcodes |
;; -----------

(define opcode-function-vector (make-vector #xFF #f))

(define opcode-lookup
  (lambda (opcode)
    (vector-ref opcode-function-vector opcode)))

(define *clc-opcode* #x18)
(define *clc-bytes* 1)
(define clc
  (lambda (cpu memory)
    (cpu-p-set! cpu (fxlogbit0 *carryp* (cpu-p cpu)))
    (cpu-pc-set! cpu (+ (cpu-pc cpu) *clc-bytes*))))

(define-syntax get-operand
  (syntax-rules (immediate zero-page absolute)
    [(_ cpu memory immediate) (memory-get-byte memory (+ (cpu-pc cpu) 1))]
    [(_ cpu memory zero-page) (memory-get-byte memory
                                (memory-get-byte memory (+ (cpu-pc cpu) 1)))]
    [(_ cpu memory absolute) (memory-get-byte memory
                               (memory-get-two-bytes memory
                                 (+ (cpu-pc cpu) 1)))]))

(define-syntax define-and-instruction
  (syntax-rules ()
    [(_ opcode bytes address-mode)
     (vector-set! opcode-function-vector opcode
       (lambda (cpu memory)
         (let ([opnd (get-operand cpu memory address-mode)])
           (let ([result (fxlogand (cpu-a cpu) opnd)])
             (cpu-p-set! cpu
               ((if (fxzero? result) fxlogbit1 fxlogbit0) *zerop* (cpu-p cpu)))
             (when (fxlogbit? 7 result)
               (cpu-p-set! cpu (fxlogbit1 *signp* (cpu-p cpu))))
             (cpu-a-set! cpu result)
             (cpu-pc-set! cpu (+ (cpu-pc cpu) bytes))))))]))

(vector-set! opcode-function-vector *clc-opcode* clc)
(define-and-instruction #x29 2 immediate)
(define-and-instruction #x25 2 zero-page)
(define-and-instruction #x2D 3 absolute)
)

