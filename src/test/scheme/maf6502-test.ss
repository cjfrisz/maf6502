(import (chezscheme) (maf6502))

(define cpu-equal?
  (lambda (x y)
    (and (= (cpu-pc x) (cpu-pc y))
         (= (cpu-s x) (cpu-s y))
         (= (cpu-p x) (cpu-p y))
         (= (cpu-a x) (cpu-a y))
         (= (cpu-x x) (cpu-x y))
         (= (cpu-y x) (cpu-y y)))))

(define print-p
  (lambda (p)
    (printf "P: ( ")
    (for-each (lambda (bit name)
                (printf "~a: ~s, " name (fxlogbit? bit p)))
      '(0 1 2 3 4 6 7)
      '(carry zero interrupt decimal break overflow sign))
    (printf ")")))

(define print-cpu
  (lambda (cpu)
    (parameterize ([print-radix 16])
      (printf "PC: ~s\n" (cpu-pc cpu))
      (printf "S: ~s\n"  (cpu-s cpu)))
    (print-p (cpu-p cpu)) (newline)
    (parameterize ([print-radix 16])
      (printf "A: ~s\n" (cpu-a cpu))
      (printf "X: ~s\n" (cpu-x cpu))
      (printf "Y: ~s\n" (cpu-y cpu)))))

(define print-cpu-diffs
  (lambda (expected actual)
    (unless (= (cpu-pc expected) (cpu-pc actual))
      (printf "  PC: expected ~s, found ~s\n" (cpu-pc expected) (cpu-pc actual)))
    (unless (= (cpu-s expected) (cpu-s actual))
      (printf "   S: expected ~s, found ~s\n" (cpu-s expected) (cpu-s actual)))
    (unless (= (cpu-p expected) (cpu-p actual))
      (printf "   P:\n")
      (let ([expected-p (cpu-p expected)]
            [actual-p (cpu-p actual)])
        (for-each (lambda (bit name)
                    (let ([expected-set? (fxlogbit? bit expected-p)]
                          [actual-set? (fxlogbit? bit actual-p)])
                      (unless (eq? expected-set? actual-set?)
                        (printf "      ~a: expected ~s, found ~s\n"
                          name
                          expected-set?
                          actual-set?))))
        '(0 1 2 3 4 6 7)
        '(carry zero interrupt decimal break overflow sign))))
    (unless (= (cpu-a expected) (cpu-a actual))
      (printf "   A: expected ~s, found ~s\n" (cpu-a expected) (cpu-a actual)))
    (unless (= (cpu-x expected) (cpu-x actual))
      (printf "   X: expected ~s, found ~s\n" (cpu-x expected) (cpu-x actual)))
    (unless (= (cpu-y expected) (cpu-y actual))
      (printf "   Y: expected ~s, found ~s\n" (cpu-y expected) (cpu-y actual)))))
    

(define test
  (lambda (description pc s p a x y mem.val* cpu-actual memory-actual)
    (parameterize ([print-radix 16])
      (printf "~a: " description)
      (let ([cpu-expected (make-cpu pc s p a x y)]
            [memory-expected (bytevector-copy memory-actual)])
        (for-each (lambda (mem.val)
                    (bytevector-u8-set! memory-expected
                      (car mem.val)
                      (cdr mem.val)))
          mem.val*)
        ;; NB: this will be the basis of system init and CPU stepping
        (cpu-pc-set! cpu-actual (memory-get-two-bytes memory-actual (cpu-pc cpu-actual)))
        ((opcode-lookup (memory-get-byte memory-actual (cpu-pc cpu-actual)))
          cpu-actual
          memory-actual)
        (if (and (cpu-equal? cpu-actual cpu-expected)
                 (bytevector=? memory-actual memory-expected))
            (begin (printf "PASSED\n") 0)
            (begin
              (printf "FAILED\n") (unless (cpu-equal? cpu-actual cpu-expected)
                (printf "  CPU differences:\n")
                (print-cpu-diffs cpu-expected cpu-actual))
              (for-each (lambda (address expected actual)
                          (unless (= expected actual)
                            (printf "  ---------------\n")
                            (printf "  expected value at memory address ~s: ~s\n" address expected)
                            (printf "  actual value at memory address ~s: ~s\n" address actual)))
                ;; NB: could just use *memory-size* for argument to iota
                (iota (bytevector-length memory-expected))
                (bytevector->u8-list memory-expected)
                (bytevector->u8-list memory-actual))
              1))))))

(define *prg-start* #xC000)

(test "CLC 1"
  (+ *prg-start* 1) *s-init* #b00100100 *a-init* *x-init* *y-init*
  '()
  (make-cpu *pc-init* *s-init* #b00100101 *a-init* *x-init* *y-init*)
  (let ([memory (make-memory)])
    (bytevector-u16-set! memory *pc-init* *prg-start* 'little)
    (bytevector-u8-set! memory *prg-start* #x18)
    memory))

(test "AND immediate 1"
  (+ *prg-start* 2) *s-init* *p-init* 2 *x-init* *y-init*
  '()
  (make-cpu *pc-init* *s-init* *p-init* 2 *x-init* *y-init*)
  (let ([memory (make-memory)])
    (bytevector-u16-set! memory *pc-init* *prg-start* 'little)
    (bytevector-u8-set! memory *prg-start* #x29)
    (bytevector-u8-set! memory (+ *prg-start* 1) #x2)
    memory))

(test "AND immediate 2: negative result"
  (+ *prg-start* 2) *s-init* #b10100100 #x82 *x-init* *y-init*
  '()
  (make-cpu *pc-init* *s-init* *p-init* #x82 *x-init* *y-init*)
  (let ([memory (make-memory)])
    (bytevector-u16-set! memory *pc-init* *prg-start* 'little)
    (bytevector-u8-set! memory *prg-start* #x29)
    (bytevector-u8-set! memory (+ *prg-start* 1) #x82)
    memory))

(test "AND immediate 3: zero result"
  (+ *prg-start* 2) *s-init* #b00100110 #x00 *x-init* *y-init*
  '()
  (make-cpu *pc-init* *s-init* *p-init* #x02 *x-init* *y-init*)
  (let ([memory (make-memory)])
    (bytevector-u16-set! memory *pc-init* *prg-start* 'little)
    (bytevector-u8-set! memory *prg-start* #x29)
    (bytevector-u8-set! memory (+ *prg-start* 1) #x01)
    memory))

(test "AND zero-page 1"
  (+ *prg-start* 2) *s-init* *p-init* 2 *x-init* *y-init*
  '()
  (make-cpu *pc-init* *s-init* *p-init* 2 *x-init* *y-init*)
  (let ([memory (make-memory)])
    (bytevector-u16-set! memory *pc-init* *prg-start* 'little)
    (bytevector-u8-set! memory *prg-start* #x25)
    (bytevector-u8-set! memory (+ *prg-start* 1) #xAC)
    (bytevector-u8-set! memory #xAC #x02)
    memory))

(test "AND absolute 1"
  (+ *prg-start* 3) *s-init* *p-init* 2 *x-init* *y-init*
  '()
  (make-cpu *pc-init* *s-init* *p-init* 2 *x-init* *y-init*)
  (let ([memory (make-memory)])
    (bytevector-u16-set! memory *pc-init* *prg-start* 'little)
    (bytevector-u8-set! memory *prg-start* #x2D)
    (bytevector-u16-set! memory (+ *prg-start* 1) #xACBD 'little)
    (bytevector-u8-set! memory #xACBD #x02)
    memory))

