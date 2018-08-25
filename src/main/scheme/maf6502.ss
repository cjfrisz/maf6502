(library (maf6502)
  (export
    make-cpu reset-cpu
    *pc-init* *s-init* *p-init* *a-init* *x-init* *y-init*
    carry zero interrupt decimal break overflow sign
    get-status-bit
    cpu-pc cpu-s cpu-p cpu-a cpu-x cpu-y
    cpu-pc-set! cpu-s-set! cpu-p-set! cpu-a-set! cpu-x-set! cpu-y-set!
    *memory-size* make-memory initialize-memory
    memory-get-byte memory-get-word
    load-program!
    opcode-lookup
    execute)
  (import
    (rename (chezscheme) (break cs:break)))

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

(define-syntax carry (identifier-syntax 0))
(define-syntax zero (identifier-syntax 1))
(define-syntax interrupt (identifier-syntax 2))
(define-syntax decimal (identifier-syntax 3))
(define-syntax break (identifier-syntax 4))
(define-syntax overflow (identifier-syntax 6))
(define-syntax sign (identifier-syntax 7))

(define-syntax get-status-bit
  (syntax-rules ()
    [(_ ?cpu ?bit) (fxlogand (fxsrl (cpu-p ?cpu) ?bit) 1)]))

(define-syntax flag-set?
  (syntax-rules ()
    [(_ ?cpu ?bit) (fxlogbit? ?bit (cpu-p ?cpu))]))

(define-syntax set-status-bit!
  (syntax-rules ()
    [(_ ?cpu flag)
     (identifier? #'flag)
     (let ([cpu ?cpu])
       (cpu-p-set! cpu (logbit1 flag (cpu-p cpu))))]))

(define-syntax clear-status-bit!
  (syntax-rules ()
    [(_ ?cpu flag)
     (identifier? #'flag)
     (let ([cpu ?cpu])
       (cpu-p-set! cpu (logbit0 flag (cpu-p cpu))))]))

(define-syntax update-flags!
  (syntax-rules ()
    [(_ ?cpu ()) (void)]
    [(_ ?cpu ([flag ?pred] [flag* ?pred*] ...))
     (let ([cpu ?cpu])
       (if ?pred
           (set-status-bit! cpu flag)
           (clear-status-bit! cpu flag))
       (update-flags! cpu ([flag* ?pred*] ...)))]))

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

(define-syntax memory-get-word
  (syntax-rules ()
    [(_ memory address) (bytevector-u16-ref memory address 'little)]))

(define memory-set-byte! bytevector-u8-set!)

(define-syntax memory-set-word!
  (syntax-rules ()
    [(_ memory address value) (bytevector-u16-set! memory address value 'little)]))

(define load-program!
    (lambda (program memory prg-start)
      (do ([i 0 (add1 i)])
          ((= i (bytevector-length program)))
          (bytevector-u8-set! memory (+ i prg-start) (bytevector-u8-ref program i)))))

;; -----------
;; | Opcodes |
;; -----------

(define-syntax get-operand-helper
  (syntax-rules (immediate zero-page absolute implied)
    [(_ cpu memory immediate) (cpu-pc cpu)]
    [(_ cpu memory zero-page) (memory-get-byte memory (cpu-pc cpu))]
    [(_ cpu memory absolute) (memory-get-word memory (cpu-pc cpu))]
    [(_ cpu memory relative)
     ;; NB: this has heretofore unrecorded cycles
     (let ([pc (cpu-pc cpu)])
       ;; NB: originally pulled from cl-6502
       ;; NB: come back and internalize this
       (let ([offset (memory-get-byte memory pc)])
         ;; NB: might want to pull (fxlogbit? 7 value) into a
         ;; NB: twos-complement-negative? syntax/predicate
         (if (fxlogbit? 7 offset)
             (wrap-word (fx- pc (fx- #xFF offset)))
             (wrap-word (fx+ pc (fx1+ offset))))))]))

(define-syntax get-operand
  (syntax-rules (implied)
    ;; NB: handle implied address mode here
    [(_ ?cpu ?memory implied ?raw?) (void)]
    ;; NB: tempted to split #t and #f cases of ?raw?, but just trust the optimizer
    [(_ ?cpu ?memory ?mode ?raw?)
     (if ?raw?
         (get-operand-helper ?cpu ?memory ?mode)
         (let ([memory ?memory])
           (memory-get-byte memory (get-operand-helper ?cpu memory ?mode))))]))


(define-syntax wrap-byte
  (syntax-rules ()
    [(_ value) (fxlogand value #xFF)]))

(define-syntax wrap-word
  (syntax-rules ()
    [(_ value) (fxlogand value #xFFFF)]))

(define-syntax stack-push-byte!
  (syntax-rules ()
    [(_  cpu memory value)
     (let ([s (cpu-s cpu)])
       (memory-set-byte! memory (+ #x100 s) (wrap-byte value))
       (cpu-s-set! cpu (wrap-byte (fx- s 1))))]))

(define-syntax stack-push-word!
  (syntax-rules ()
    [(_ cpu memory value)
     (begin
       (stack-push-byte! cpu memory (fxsrl value 8))
       (stack-push-byte! cpu memory (wrap-byte value)))]))

(define-syntax stack-pop-byte!
  (syntax-rules ()
    [(_ ?cpu ?memory)
     (let ([cpu ?cpu])
       (let ([s (wrap-byte (fx+ (cpu-s cpu) 1))])
         (cpu-s-set! cpu s)
         (memory-get-byte ?memory (+ #x100 s))))]))

(define opcode-function-vector
  (make-vector #xFF
    (lambda (cpu memory)
      (errorf 'maf6502 "unimplemented opcode"))))

(define opcode-lookup
  (lambda (opcode)
    (vector-ref opcode-function-vector opcode)))

(define execute
  (lambda (cpu memory)
    (do ([opcode (memory-get-byte memory (cpu-pc cpu)) (memory-get-byte memory (cpu-pc cpu))])
      ((fx= opcode #x0))
      ((opcode-lookup opcode) cpu memory))))

(define-syntax define-instruction
  (syntax-rules (advance-pc: raw-operand: lambda)
    [(_ mnemonic
       (advance-pc: advance-pc?)
       (raw-operand: raw?)
       ([opcode* bytes* address-mode*] ...)
       (lambda (cpu memory)
         (lambda (operand)
           ?body ?body* ...)))
     (begin
       (vector-set! opcode-function-vector opcode*
         (lambda (cpu memory)
           ;; NB: advance the PC past the opcode
           ;; NB: may seem odd, but comes into play with instructions for which
           ;; NB: the order of operations is specific and important, e.g., BRK
           (cpu-pc-set! cpu (fx1+ (cpu-pc cpu)))
           ((lambda (operand)
              ?body ?body* ...)
             ;; NB: for conditional branches, the relative address is always
             ;; NB: computed, even if the condition is false
             ;; NB: maybe thunkify get-operand and have it invoked in the
             ;; NB: define-instruction body.
             ;; NB: alternatively, maybe get-operand returns a thunk only for
             ;; NB: relative mode operands
             (get-operand cpu memory address-mode* raw?))
           (when advance-pc?
             (cpu-pc-set! cpu (fx+ (cpu-pc cpu) (fx1- bytes*))))))
       ...)]))

(define-instruction adc
  (advance-pc: #t)
  (raw-operand: #f)
  ([#x6D 3 absolute])
  (lambda (cpu memory)
    (lambda (opnd)
      (let ([result (+ (cpu-a cpu) opnd)])
        ;; NB: need to implement overflow handling
        (update-flags! cpu
          ([carry (fx> result #xFF)]
           [sign (fxlogbit? 7 result)]
           ;; NB: check semantics--set zero on carry?
           [zero (fxzero? (wrap-byte result))]))
        (cpu-a-set! cpu (wrap-byte result))))))

(define-instruction and
  (advance-pc: #t)
  (raw-operand: #f)
  ([#x29 2 immediate]
   [#x25 2 zero-page]
   [#x2D 3 absolute])
  (lambda (cpu memory)
    (lambda (opnd)
      (let ([result (fxlogand (cpu-a cpu) opnd)])
        (update-flags! cpu
          ([zero (fxzero? result)]
           [sign (fxlogbit? 7 result)]))
        (cpu-a-set! cpu result)))))

(define-instruction beq
  (advance-pc: #f)
  (raw-operand: #t)
  ([#xF0 2 relative])
  (lambda (cpu memory)
    (lambda (opnd)
      (cpu-pc-set! cpu
        (if (flag-set? cpu zero)
            opnd
            (fx1+ (cpu-pc cpu)))))))

(define-instruction bne
  (advance-pc: #f)
  (raw-operand: #t)
  ([#xD0 2 relative])
  (lambda (cpu memory)
    (lambda (opnd)
      (cpu-pc-set! cpu
        (if (flag-set? cpu zero)
            (fx1+ (cpu-pc cpu))
            opnd)))))

(define-instruction brk
  (advance-pc: #t)
  (raw-operand: #f)
  ([#x00 1 implied])
  (lambda (cpu memory)
    (lambda (_)
      (let ([pc (wrap-word (cpu-pc cpu))])
        (stack-push-word! cpu memory pc)
        (set-status-bit! cpu break)
        (stack-push-byte! cpu memory (cpu-s cpu))
        (set-status-bit! cpu interrupt)
        (cpu-pc-set! cpu (memory-get-word memory #xFFFE))))))

(define-instruction clc
  (advance-pc: #t)
  (raw-operand: #f)
  ([#x18 1 implied])
  (lambda (cpu memory)
    (lambda (_)
      (clear-status-bit! cpu carry))))

(define-instruction cmp
  (advance-pc: #t)
  (raw-operand: #f)
  ([#xC9 2 immediate])
  (lambda (cpu memory)
    (lambda (opnd)
      (let ([result (fx- (cpu-a cpu) opnd)])
        (update-flags! cpu
          ([sign (fxlogbit? 7 result)]
           [zero (fxzero? result)]
           [carry (fx< result 0)]))))))

;; NB: might want to make a define-compare-instruction syntax
(define-instruction cpx
  (advance-pc: #t)
  (raw-operand: #f)
  ([#xE0 2 immediate])
  (lambda (cpu memory)
    (lambda (opnd)
      (let ([result (fx- (cpu-x cpu) opnd)])
        (update-flags! cpu
          ([sign (fxlogbit? 7 result)]
           [zero (fxzero? result)]
           [carry (fx< result 0)]))))))

;; NB: might want to make a define-implied instruction
;; NB: might want to make a define-decrement instruction
(define-instruction dex
  (advance-pc: #t)
  (raw-operand: #f)
  ([#xCA 1 implied])
  (lambda (cpu memory)
    (lambda (_)
      (let ([result (fx1- (cpu-x cpu))])
        (update-flags! cpu
          ([zero (fxzero? result)]
           [sign (fxlogbit? 7 result)]))
        (cpu-x-set! cpu result)))))

(define-instruction jmp
  (advance-pc: #f)
  (raw-operand: #t)
  ([#x4C 3 absolute])
  (lambda (cpu memory)
    (lambda (opnd)
      ;; NB: the C64 manual specifies the semantics of JMP such that it seems
      ;; NB: like the PC should be set to the word value at location opnd,
      ;; NB: rather than opnd itself.
      ;; NB: this would obviate the notion of "raw?" for define-instruction,
      ;; NB: as well.
      ;; NB: this is likely a wart of the cl-6502 getting propagated here
      (cpu-pc-set! cpu opnd))))

(define-instruction lda
  (advance-pc: #t)
  (raw-operand: #f)
  ([#xA9 2 immediate])
  (lambda (cpu memory)
    (lambda (opnd)
      (update-flags! cpu
        ([zero (fxzero? opnd)]
         [sign (fxlogbit? 7 opnd)]))
      (cpu-a-set! cpu opnd))))

(define-instruction pha
  (advance-pc: #t)
  (raw-operand: #f)
  ([#x48 1 implied])
  (lambda (cpu memory)
    (lambda (_)
      (stack-push-byte! cpu memory (cpu-a cpu)))))

(define-instruction pla
  (advance-pc: #t)
  (raw-operand: #f)
  ([#x68 1 implied])
  (lambda (cpu memory)
    (lambda (_)
      (let ([result (stack-pop-byte! cpu memory)])
        (update-flags! cpu
          ([zero (fxzero? result)]
           [sign (fxlogbit? 7 result)]))
        (cpu-a-set! cpu result)))))

(define-instruction sta
  (advance-pc: #t)
  ;; NB: wow what a hack
  ;; NB: opnd is the address we store to, not the value at the address
  ;; NB: by setting raw-operand to #t, we pass in the address
  ;; NB: this is why cl-6502 has a separate setter from getter
  (raw-operand: #t)
  ([#x8D 3 absolute])
  (lambda (cpu memory)
    (lambda (opnd)
      (memory-set-byte! memory opnd (cpu-a cpu)))))

(define-instruction tax
  (advance-pc: #t)
  (raw-operand: #f)
  ([#xAA 1 implied])
  (lambda (cpu memory)
    (lambda (_)
      (let ([result (cpu-a cpu)])
        (update-flags! cpu
          ([zero (fxzero? result)]
           [sign (fxlogbit? 7 result)]))
        (cpu-x-set! cpu result)))))

(define-instruction tay
  (advance-pc: #t)
  (raw-operand: #f)
  ([#xA8 1 implied])
  (lambda (cpu memory)
    (lambda (_)
      (let ([result (cpu-a cpu)])
        (update-flags! cpu
          ([zero (fxzero? result)]
           [sign (fxlogbit? 7 result)]))
        (cpu-y-set! cpu result)))))

(define-instruction txa
  (advance-pc: #t)
  (raw-operand: #f)
  ([#x8A 1 implied])
  (lambda (cpu memory)
    (lambda (_)
      (let ([result (cpu-x cpu)])
        (update-flags! cpu
          ([zero (fxzero? result)]
           [sign (fxlogbit? 7 result)]))
        (cpu-a-set! cpu result)))))

(define-instruction tya
  (advance-pc: #t)
  (raw-operand: #f)
  ([#x98 1 implied])
  (lambda (cpu memory)
    (lambda (_)
      (let ([result (cpu-y cpu)])
        (update-flags! cpu
          ([zero (fxzero? result)]
           [sign (fxlogbit? 7 result)]))
        (cpu-a-set! cpu result)))))
)

