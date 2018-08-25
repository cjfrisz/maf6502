(import
  (rename (chezscheme) (break cs:break))
  (maf6502)
  (srfi :78 lightweight-testing))

(print-radix 16)

(define *expected-pc* (make-parameter *pc-init*))
(define *expected-s* (make-parameter *s-init*))
(define *expected-p* (make-parameter *p-init*))
(define *expected-a* (make-parameter *a-init*))
(define *expected-x* (make-parameter *x-init*))
(define *expected-y* (make-parameter *y-init*))

(define check-cpu
  (lambda (cpu)
    (check (cpu-pc cpu) => (*expected-pc*))
    (check (cpu-s cpu) => (*expected-s*))
    (parameterize ([print-radix 2])
      (check (cpu-p cpu) => (*expected-p*)))
    (check (cpu-a cpu) => (*expected-a*))
    (check (cpu-x cpu) => (*expected-x*))
    (check (cpu-y cpu) => (*expected-y*))))

(define check-memory
  (lambda (expected actual)
    (check (bytevector-length actual) => (bytevector-length expected))
    (do ([i 0 (add1 i)])
      ((< i (bytevector-length expected)))
      (check (bytevector-u8-ref actual i) => (bytevector-u8-ref expected i)))))

(check-set-mode! 'report-failed)

(define-syntax test
  (syntax-rules ()
    [(_ description
       ([register* register-value*] ...)
       ?cpu
       ([address* address-value* size*] ...))
     (let ([cpu ?cpu])
       (printf "~a:\n" description)
       (parameterize ([register* register-value*] ...)
         (let ([memory (make-memory)])
           (bytevector-u16-set! memory *pc-init* *prg-start* 'little)
           (case size*
             [(8) (bytevector-u8-set! memory address* address-value*)]
             [(16) (bytevector-u16-set! memory address* address-value* 'little)])
           ...
           (cpu-pc-set! cpu (memory-get-word memory (cpu-pc cpu)))
           ((opcode-lookup (memory-get-byte memory (cpu-pc cpu))) cpu memory)
           (check-cpu cpu))))]))

(define-syntax new-test
  (syntax-rules (expected initial)
    [(_ description
       (expected
         ([?register* ?register-value*] ...)
         ([?expect-address* ?expect-address-value* ?expect-size*] ...))
       (initial
         ?cpu
         ([?init-address* ?init-address-value* ?init-size*] ...)))
     (let ([cpu ?cpu])
       (printf "~a:\n" description)
       (parameterize ([?register* ?register-value*] ...)
         (define set-memory-values!
           (lambda (memory address* address-value* size*)
             (bytevector-u16-set! memory *pc-init* *prg-start* 'little)
             (for-each (lambda (address address-value size)
                         (case size
                           [(8) (bytevector-u8-set! memory address address-value)]
                           [(16) (bytevector-u16-set! memory address address-value 'little)]))
               address*
               address-value*
               size*)))
         (let ([init-memory (make-memory)]
               [expected-memory (make-memory)]
               [init-address* (list ?init-address* ...)]
               [init-address-value* (list ?init-address-value* ...)]
               [init-address-size* (list ?init-size* ...)]
               [expected-address* (list ?expect-address* ...)]
               [expected-address-value* (list ?expect-address-value* ...)]
               [expected-address-size* (list ?expect-size* ...)])
           (set-memory-values! init-memory init-address* init-address-value* init-address-size*)
           (set-memory-values! expected-memory expected-address* expected-address-value* expected-address-size*)
           (cpu-pc-set! cpu (memory-get-word init-memory (cpu-pc cpu)))
           ((opcode-lookup (memory-get-byte init-memory (cpu-pc cpu))) cpu init-memory)
           (check-cpu cpu)
           (check-memory expected-memory init-memory))))]))

(define-syntax new-test2
  (syntax-rules (expected initial)
    [(_ description
       (expected
         ([?register* ?register-value*] ...)
         ([?expect-address* ?expect-address-value* ?expect-size*] ...))
       (initial
         ?cpu
         ([?init-address* ?init-address-value* ?init-size*] ...)))
     (let ([cpu ?cpu])
       (printf "~a:\n" description)
       (parameterize ([?register* ?register-value*] ...)
         (define set-memory-values!
           (lambda (memory address* address-value* size*)
             (bytevector-u16-set! memory *pc-init* *prg-start* 'little)
             (for-each (lambda (address address-value size)
                         (case size
                           [(8) (bytevector-u8-set! memory address address-value)]
                           [(16) (bytevector-u16-set! memory address address-value 'little)]))
               address*
               address-value*
               size*)))
         (let ([init-memory (make-memory)]
               [expected-memory (make-memory)]
               [init-address* (list ?init-address* ...)]
               [init-address-value* (list ?init-address-value* ...)]
               [init-address-size* (list ?init-size* ...)]
               [expected-address* (list ?expect-address* ...)]
               [expected-address-value* (list ?expect-address-value* ...)]
               [expected-address-size* (list ?expect-size* ...)])
           (set-memory-values! init-memory init-address* init-address-value* init-address-size*)
           (set-memory-values! expected-memory expected-address* expected-address-value* expected-address-size*)
           (cpu-pc-set! cpu *prg-start*)
           (execute cpu init-memory)
           (check-cpu cpu)
           (check-memory expected-memory init-memory))))]))


(define *prg-start* #xC000)

(test "CLC 1"
  ([*expected-pc* (+ *prg-start* 1)]
   [*expected-p* #b00100100])
  (make-cpu *pc-init* *s-init* #b00100101 *a-init* *x-init* *y-init*)
  ([*prg-start* #x18 8]))

(test "AND immediate 1"
  ([*expected-pc* (+ *prg-start* 2)]
   [*expected-a* 2])
  (make-cpu *pc-init* *s-init* *p-init* 2 *x-init* *y-init*)
  ([*prg-start* #x29 8]
   [(+ *prg-start* 1) #x2 8]))

(test "AND immediate 2: negative result"
  ([*expected-pc* (+ *prg-start* 2)]
   [*expected-p* #b10100100]
   [*expected-a* #x82])
  (make-cpu *pc-init* *s-init* *p-init* #x82 *x-init* *y-init*)
  ([*prg-start* #x29 8]
   [(+ *prg-start* 1) #x82 8]))

(test "AND immediate 3: zero result"
  ([*expected-pc* (+ *prg-start* 2)]
   [*expected-p* #b00100110]
   [*expected-a* #x00])
  (make-cpu *pc-init* *s-init* *p-init* #x02 *x-init* *y-init*)
  ([*prg-start* #x29 8]
   [(+ *prg-start* 1) #x01 8]))

(test "AND zero-page 1"
  ([*expected-pc* (+ *prg-start* 2)]
   [*expected-a* 2])
  (make-cpu *pc-init* *s-init* *p-init* 2 *x-init* *y-init*)
  ([*prg-start* #x25 8]
   [(+ *prg-start* 1) #xAC 8]
   [#xAC #x02 8]))

(test "AND absolute 1"
  ([*expected-pc* (+ *prg-start* 3)]
   [*expected-a* 2])
  (make-cpu *pc-init* *s-init* *p-init* 2 *x-init* *y-init*)
  ([*prg-start* #x2D 8]
   [(+ *prg-start* 1) #xACBD 16]
   [#xACBD #x02 8]))

; lda #5        ; test input, aka, the only fact argument
(test "LDA immediate"
  ([*expected-pc* (+ *prg-start* 2)]
   [*expected-a* 5])
  (make-cpu *pc-init* *s-init* *p-init* *a-init* *x-init* *y-init*)
  ([*prg-start* #xA9 8]
   [(+ *prg-start* 1) #x05 8]))

;  cmp #0        ; special case zero (even though fact doesn't accept 0
(test "CMP with non-zero A"
  ([*expected-pc* (+ *prg-start* 2)]
   [*expected-p* (logbit0 zero *p-init*)]
   ;; NB: this indicates a weakness in the test macro
   ;; NB: should expect values for *initial* CPU unless otherwise specified
   ;; NB: currently expects CPU startup values unless otherwise specified
   [*expected-a* 1])
  (make-cpu *pc-init* *s-init* *p-init* 1 *x-init* *y-init*)
  ([*prg-start* #xC9 8]
   [(+ *prg-start* 1) #x00 8]))

(test "CMP with zero A"
  ([*expected-pc* (+ *prg-start* 2)]
   [*expected-p* (logbit1 zero *p-init*)])
  (make-cpu *pc-init* *s-init* *p-init* 0 *x-init* *y-init*)
  ([*prg-start* #xC9 8]
   [(+ *prg-start* 1) #x00 8]))

;  bne continue  ; skip the zero case
(test "BNE (not equal)"
  ([*expected-pc* (+ *prg-start* 2)]
   ;; NB: this indicates a weakness in the test macro
   ;; NB: should expect values for *initial* CPU unless otherwise specified
   ;; NB: currently expects CPU startup values unless otherwise specified
   [*expected-p* (logbit1 zero *p-init*)])
  (make-cpu *pc-init* *s-init* (logbit1 zero *p-init*) *a-init* *x-init* *y-init*)
  ([*prg-start* #xD0 8]
   [(+ *prg-start* 1) #x83 8]))

(test "BNE (equal)"
  ([*expected-pc* (+ *prg-start* #x75)]
   ;; NB: this indicates a weakness in the test macro
   ;; NB: should expect values for *initial* CPU unless otherwise specified
   ;; NB: currently expects CPU startup values unless otherwise specified
   [*expected-p* (logbit0 zero *p-init*)])
  (make-cpu *pc-init* *s-init* (logbit0 zero *p-init*) *a-init* *x-init* *y-init*)
  ([*prg-start* #xD0 8]
   [(+ *prg-start* 1) #x73 8]))

;  brk           ; FINISHED
(new-test "BRK"
  (expected
    ([*expected-pc* #x00]
     [*expected-p* (logbit1 interrupt (logbit1 break *p-init*))]
     [*expected-s* (- *s-init* 3)])
    ([(+ #x100 *s-init*) *prg-start* 16]
     [(+ #x100 (- *s-init* 2)) (+ #x100 (- *s-init*)) 16]))
  (initial
    (make-cpu *pc-init* *s-init* *p-init* *a-init* *x-init* *y-init*)
    ([*prg-start* #x00 8])))

;  tax           ; copy A to X to seed iteration
(test "TAX"
  ([*expected-pc* (+ *prg-start* #x01)]
   [*expected-a* 5]
   [*expected-x* 5])
  (make-cpu *pc-init* *s-init* *p-init* 5 *x-init* *y-init*)
  ([*prg-start* #xAA 8]))

;  dex           ; decrement X so we can do n * (n-1)
(test "DEX"
  ([*expected-pc* (+ *prg-start* #x01)]
   [*expected-x* 4])
  (make-cpu *pc-init* *s-init* *p-init* *a-init* 5 *y-init*)
  ([*prg-start* #xCA 8]))

;  cpx #1        ; base case: (n-1) == 1
(test "CPX with zero result"
  ([*expected-pc* (+ *prg-start* 2)]
   [*expected-p* (logbit1 zero *p-init*)]
   [*expected-x* 1])
  (make-cpu *pc-init* *s-init* *p-init* *a-init* 1 *y-init*)
  ([*prg-start* #xE0 8]
   [(+ *prg-start* 1) #x01 8]))

(test "CPX with non-zero result"
  ([*expected-pc* (+ *prg-start* 2)]
   [*expected-p* (logbit0 zero *p-init*)]
   [*expected-x* 2])
  (make-cpu *pc-init* *s-init* *p-init* *a-init* 2 *y-init*)
  ([*prg-start* #xE0 8]
   [(+ *prg-start* 1) #x01 8]))

;  beq end       ; if so, jump to the end
(test "BEQ (not equal)"
  ([*expected-pc* (+ *prg-start* #x73 #x2)]
   [*expected-p* (logbit1 zero *p-init*)])
  (make-cpu *pc-init* *s-init* (logbit1 zero *p-init*) *a-init* *x-init* *y-init*)
  ([*prg-start* #xF0 8]
   [(+ *prg-start* 1) #x73 8]))

(test "BEQ (equal)"
  ([*expected-pc* (+ *prg-start* 2)]
   [*expected-p* (logbit0 zero *p-init*)])
  (make-cpu *pc-init* *s-init* (logbit0 zero *p-init*) *a-init* *x-init* *y-init*)
  ([*prg-start* #xF0 8]
   [(+ *prg-start* 1) #x73 8]))

;  tay           ; otherwise, stash A in Y
(test "TAY"
  ([*expected-pc* (+ *prg-start* #x01)]
   [*expected-a* 5]
   [*expected-y* 5])
  (make-cpu *pc-init* *s-init* *p-init* 5 *x-init* *y-init*)
  ([*prg-start* #xA8 8]))

;  txa           ; copy our current iteration value to A
(test "TXA"
  ([*expected-pc* (+ *prg-start* #x01)]
   [*expected-x* 5]
   [*expected-a* 5])
  (make-cpu *pc-init* *s-init* *p-init* *a-init* 5 *y-init*)
  ([*prg-start* #x8A 8]))

;  pha           ; stash the iteration variable on the stack
(new-test "PHA"
  (expected
    ([*expected-pc* (+ *prg-start* 1)]
     [*expected-a* #x12]
     [*expected-s* (- *s-init* 1)])
    ([(+ *s-init* #x100) #x12 8]))
  (initial
    (make-cpu *pc-init* *s-init* *p-init* #x12 *x-init* *y-init*)
    ([*prg-start* #x48 8])))

;  tya           ; retrieve A's value from Y
(test "TYA"
  ([*expected-pc* (+ *prg-start* #x01)]
   [*expected-y* 5]
   [*expected-a* 5])
  (make-cpu *pc-init* *s-init* *p-init* *a-init* *x-init* 5)
  ([*prg-start* #x98 8]))

;  sta $0000     ; put A into memory
(new-test "STA"
  (expected
    ([*expected-pc* (+ *prg-start* 3)]
     [*expected-a* #x24])
    ([#x0000 #x24 8]))
  (initial
    (make-cpu *pc-init* *s-init* *p-init* #x24 *x-init* *y-init*)
    ([*prg-start* #x8D 8]
     [(+ *prg-start* 1) #x0000 16])))

;  adc $0000     ; otherwise, add the original operand value to A
(test "ADC absolute"
  ([*expected-pc* (+ *prg-start* 3)]
   [*expected-a* #x9])
  (make-cpu *pc-init* *s-init* *p-init* #x4 *x-init* *y-init*)
  ([*prg-start* #x6D 8]
   [(+ *prg-start* 1) #x0000 16]
   [#x0000 #x5 8]))

;  pla           ; retrieve our iterator value for factorial from the stack
(test "PLA"
  ([*expected-pc* (+ *prg-start* #x01)]
   [*expected-a* #x5]
   [*expected-s* *s-init*])
  (make-cpu *pc-init* (- *s-init* 1) *p-init* *a-init* *x-init* *y-init*)
  ([*prg-start* #x68 8]
   [(+ #x100 *s-init*) #x5 8]))

;  jmp iterate   ; go back to the top of fact's iteration
(test "JMP absolute"
  ([*expected-pc* #xADBC])
  (make-cpu *pc-init* *s-init* *p-init* *a-init* *x-init* *y-init*)
  ([*prg-start* #x4C 8]
   [(+ *prg-start* #x1) #xADBC 16]))

(new-test2 "CLC 2"
  (expected
    ([*expected-pc* (+ *prg-start* 1)]
     [*expected-p* #b00100100])
    ())
  (initial
    (make-cpu *pc-init* *s-init* #b00100101 *a-init* *x-init* *y-init*)
    ([*prg-start* #x18 8]
     [(+ *prg-start* 1) #x0 8])))

