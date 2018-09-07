(import (maf6502 assembler parser))
(define run
  (lambda (fn)
    (let* ([ip (open-file-input-port fn)]
           [sfd (make-source-file-descriptor fn ip #t)]
           [ip (transcoded-port ip (native-transcoder))])
      (parameterize ([*sfd* sfd])
        (eval
          `(let ()
             (define-syntax define-ops
               (lambda (x)
                 (syntax-case x ()
                   [(_ op ...)
                    #`(begin
                        (define-syntax op
                          (lambda (x)
                            (let ([src (annotation-source (syntax->annotation x))])
                              (with-syntax ([bfp (source-object-bfp src)] [efp (source-object-efp src)])
                                (syntax-case x ()
                                  [(_ e (... ...)) #'`(op (bfp . efp) ,e (... ...))])))))
                        ...)])))
             (define-ops program instruction label label-ref absolute-address)
             ,(dynamic-wind
                void
                (lambda () (parse fn ip))
                (lambda () (close-input-port ip)))))))))

(define (maf6502-assembler-test)
  (define n 0)
  (define test
    (lambda (line* okay?)
      (set! n (+ n 1))
      (let ([fn (format "testfile~s" n)])
        (with-output-to-file fn
          (lambda () (for-each (lambda (line) (printf "~a\n" line)) line*))
          'replace)
        (let ([result (parameterize ([compile-profile #t] [compile-interpret-simple #f])
                        (guard (c [else c]) (run fn)))])
          (guard (c [else #f]) (profile-dump-html))
          (delete-file fn)
          (delete-file "profile.html")
          (delete-file (format "~a.html" fn))
          (unless (okay? result)
            (printf "test ~s failed\n" n)
            (printf "  test code:")
            (for-each (lambda (line) (printf "    ~a\n" line)) line*)
            (printf "  result:\n    ")
            (if (condition? result)
                (begin (display-condition result) (newline))
                (parameterize ([pretty-initial-indent 4])
                  (pretty-print result)))
            (newline))))))

  (define-syntax returns
    (syntax-rules ()
      [(_ k) (lambda (x) (equal? x 'k))]))

  (define-syntax oops
    (syntax-rules ()
      [(_ (c) e1 e2 ...)
       (lambda (c) (and (condition? c) e1 e2 ...))]))

  (test
    '(
       "lda #5"
       )
    (returns
      (program (0 . 6) (instruction (0 . 6) lda 5))))

  (test
    '(
       "cmp #0"
       )
    (returns
      (program (0 . 6) (instruction (0 . 6) cmp 0))))


  (test
    '(
       "bne continue"
       )
    (returns
      (program (0 . 12) (instruction (0 . 12) bne (label-ref (4 . 12) continue)))))

  (test
    '(
       "brk"
       )
    (returns
      (program (0 . 3) (instruction (0 . 3) brk))))

  (test
    '(
       "continue:"
       )
    (returns
      (program (0 . 9) (label (0 . 9) continue))))

  (test
    '(
       "tax"
       )
    (returns
      (program (0 . 3) (instruction (0 . 3) tax))))

  (test
    '(
       "dex"
       )
    (returns
      (program (0 . 3) (instruction (0 . 3) dex))))

  (test
    '(
       "cpx #1"
       )
    (returns
      (program (0 . 6) (instruction (0 . 6) cpx 1))))

  (test
    '(
       "beq end"
       )
    (returns
      (program (0 . 7) (instruction (0 . 7) beq (label-ref (4 . 7) end)))))

  (test
    '(
       "tay"
       )
    (returns
      (program (0 . 3) (instruction (0 . 3) tay))))

  (test
    '(
       "txa"
       )
    (returns
      (program (0 . 3) (instruction (0 . 3) txa))))

  (test
    '(
       "pha"
       )
    (returns
      (program (0 . 3) (instruction (0 . 3) pha))))

  (test
    '(
       "tya"
       )
    (returns
      (program (0 . 3) (instruction (0 . 3) tya))))

  (test
    '(
       "sta $0000"
       )
    (returns
      (program (0 . 9) (instruction (0 . 9) sta (absolute-address (4 . 9) 0)))))

  (test
    '(
       "adc $0000"
       )
    (returns
      (program (0 . 9) (instruction (0 . 9) adc (absolute-address (4 . 9) 0)))))

  (test
    '(
       "jmp loop"
       )
    (returns
      (program (0 . 8) (instruction (0 . 8) jmp (label-ref (4 . 8) loop)))))

  (test
    '(
       "pla"
       )
    (returns
      (program (0 . 3) (instruction (0 . 3) pla))))

  (test
    '(
       "lda #5        ; test input, aka, the only fact argument"
       "cmp #0        ; special case zero (even though fact doesn't accept 0) ;-)"
       "bne continue  ; skip the zero case"
       "lda #1        ; load 1 in A, our result register"
       "brk           ; FINISHED"
     "continue:"
       "tax           ; copy A to X to seed iteration"
     "iterate:"
       "dex           ; decrement X so we can do n * (n-1)"
       "cpx #1        ; base case: (n-1) == 1"
       "beq end       ; if so, jump to the end"
       "tay           ; otherwise, stash A in Y"
       "txa           ; copy our current iteration value to A"
       "pha           ; stash the iteration variable on the stack"
       "tya           ; retrieve A's value from Y"
       "sta $0000     ; put A into memory"
       "lda #0        ; initialize A as the result register"
     "loop:"
       "cpx #0        ; base case: X is zero and we're done iterating"
       "beq return    ; if we're done, jump back to fact"
       "adc $0000     ; otherwise, add the original operand value to A"
       "dex           ; decrement X"
       "jmp loop      ; jump back to the top of the multiplication loop"
     "return:"
       "tay           ; stash A in Y"
       "pla           ; retrieve our iterator value for factorial from the stack"
       "tax           ; put the iterator back into X"
       "tya           ; get the multiplication result from Y into A"
       "jmp iterate   ; go back to the top of fact's iteration"
     "end:"
       "brk"
       )
    (returns
      (program (0 . 1262)
        (instruction (0 . 6) lda 5)
        (instruction (56 . 62) cmp 0)
        (instruction (130 . 142) bne (label-ref (134 . 142) continue))
        (instruction (165 . 171) lda 1)
        (instruction (214 . 217) brk)
        (label (239 . 248) continue)
        (instruction (249 . 252) tax)
        (label (295 . 303) iterate)
        (instruction (304 . 307) dex)
        (instruction (355 . 361) cpx 1)
        (instruction (393 . 400) beq (label-ref (397 . 400) end))
        (instruction (432 . 435) tay)
        (instruction (472 . 475) txa)
        (instruction (526 . 529) pha)
        (instruction (584 . 587) tya)
        (instruction (626 . 635) sta (absolute-address (630 . 635) 0))
        (instruction (660 . 666) lda 0)
        (label (712 . 717) loop)
        (instruction (718 . 724) cpx 0)
        (instruction (780 . 790) beq (label-ref (784 . 790) return))
        (instruction (829 . 838) adc (absolute-address (833 . 838) 0))
        (instruction (892 . 895) dex)
        (instruction (920 . 928) jmp (label-ref (924 . 928) loop))
        (label (984 . 991) return)
        (instruction (992 . 995) tay)
        (instruction (1021 . 1024) pla)
        (instruction (1094 . 1097) tax)
        (instruction (1139 . 1142) tya)
        (instruction (1199 . 1210) jmp (label-ref (1203 . 1210) iterate))
        (label (1254 . 1258) end)
        (instruction (1259 . 1262) brk))))

  (delete-file "expr.md")
  (printf "~s tests ran\n" n)
  )

#!eof

The following should print only "<n> tests ran".

echo '(maf6502-assembler-test)' | scheme -q maf6502-assembler.ss

