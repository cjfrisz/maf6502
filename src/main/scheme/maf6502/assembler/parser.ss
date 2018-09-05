;;; Copyright 2018 Chris Frisz
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; 
;;; http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.
;;;
;;; Originally copied from ez-grammar-test.ss, copyright Cisco Systems, Inc.

(library (streams)
  (export stream-cons stream-car stream-cdr stream-nil stream-null?
    stream-map stream stream-append2 stream-append-all stream-last-forced)
  (import (chezscheme))

  (define stream-cons
    (lambda (x thunk)
      (cons x thunk)))

  (define stream-car
    (lambda (x)
      (car x)))

  (define stream-cdr
    (lambda (x)
      (when (procedure? (cdr x)) (set-cdr! x ((cdr x))))
      (cdr x)))

  (define stream-nil '())

  (define stream-null?
    (lambda (x)
      (null? x)))

  (define stream-map
    (lambda (f x)
      (if (stream-null? x)
          '()
          (stream-cons (f (stream-car x))
            (lambda ()
              (stream-map f (stream-cdr x)))))))

  (define stream
    (lambda xs
      xs))

  (define stream-append2
    (lambda (xs thunk)
      (if (null? xs)
          (thunk)
          (stream-cons (stream-car xs)
            (lambda ()
              (stream-append2 (stream-cdr xs) thunk))))))

  (define stream-append-all
    (lambda (stream$) ;; stream of streams
      (if (stream-null? stream$)
          stream$
          (stream-append2 (stream-car stream$)
            (lambda () (stream-append-all (stream-cdr stream$)))))))

  (define stream-last-forced
    (lambda (x)
      (and (not (null? x))
           (let loop ([x x])
             (let ([next (cdr x)])
               (if (pair? next)
                   (loop next)
                   (car x)))))))
)

(library (state-case)
  (export state-case eof)
  (import (chezscheme))

  ;;; from Chez Scheme Version 9.5.1 cmacros.ss
  (define-syntax state-case
    (lambda (x)
      (define state-case-test
        (lambda (cvar k)
          (with-syntax ((cvar cvar))
            (syntax-case k (-)
              (char
                (char? (datum char))
                #'(char=? cvar char))
              ((char1 - char2)
               (and (char? (datum char1)) (char? (datum char2)))
               #'(char<=? char1 cvar char2))
              (predicate
                (identifier? #'predicate)
                #'(predicate cvar))))))
      (define state-case-help
        (lambda (cvar clauses)
          (syntax-case clauses (else)
            (((else exp1 exp2 ...))
             #'(begin exp1 exp2 ...))
            ((((k ...) exp1 exp2 ...) . more)
             (with-syntax (((test ...)
                            (map (lambda (k) (state-case-test cvar k))
                              #'(k ...)))
                           (rest (state-case-help cvar #'more)))
               #'(if (or test ...) (begin exp1 exp2 ...) rest)))
            (((k exp1 exp2 ...) . more)
             (with-syntax ((test (state-case-test cvar #'k))
                           (rest (state-case-help cvar #'more)))
               #'(if test (begin exp1 exp2 ...) rest))))))
      (syntax-case x (eof)
        ((_ cvar (eof exp1 exp2 ...) more ...)
         (identifier? #'cvar)
         (with-syntax ((rest (state-case-help #'cvar #'(more ...))))
           #'(if (eof-object? cvar)
                 (begin exp1 exp2 ...)
                 rest))))))

  (define-syntax eof
    (lambda (x)
      (syntax-error x "misplaced aux keyword")))
)

(library (lexer)
  (export token? token-type token-value token-bfp token-efp lexer)
  (import (chezscheme) (state-case) (streams))

  (define-record-type token
    (nongenerative)
    (fields type value bfp efp))

  (define lexer
    (lambda (fn ip)
      (define $prev-pos 0)
      (define $pos 0)
      (define ($get-char)
        (set! $pos (+ $pos 1))
        (get-char ip))
      (define ($unread-char c)
        (set! $pos (- $pos 1))
        (unread-char c ip))
      (define ($ws!) (set! $prev-pos $pos))
      (define ($make-token type value)
        (let ([tok (make-token type value $prev-pos $pos)])
          (set! $prev-pos $pos)
          tok))
      (define ($lex-error c)
        (errorf #f "unexpected ~a at character ~s of ~a"
          (if (eof-object? c)
              "eof" 
              (format "character '~c'" c))
          $pos fn))
      (define-syntax lex-error
        (syntax-rules ()
          [(_ ?c)
           (let ([c ?c])
             ($lex-error c)
             (void))]))
      (let-values ([(sp get-buf) (open-string-output-port)])
        (define (return-token type value)
          (stream-cons ($make-token type value) lex))
        (define-syntax define-state-case
          (syntax-rules ()
            [(_ ?def-id ?char-id clause ...)
             (define (?def-id)
               (let ([?char-id ($get-char)])
                 (state-case ?char-id clause ...)))]))
        (define-state-case lex c
          [eof stream-nil]
          [char-whitespace? ($ws!) (lex)]
          [#\. (put-char sp c) (lex-directive)]
          [char-alphabetic? (put-char sp c) (lex-id)]
          [char-numeric? (put-char sp c) (lex-number)]
          [#\: (return-token 'colon #\:)]
          [#\# (return-token 'hash #\#)]
          [#\$ (return-token 'dollar #\$)]
          [#\; (lex-comment)]
          [else (lex-error c)])
        ;; NB: this module pattern is recurring, might need macro
        (module (lex-directive)
          (define (directive) (return-token 'directive (string->symbol (get-buf))))
          (define-state-case next c
            [eof (directive)]
            [#\. (put-char sp c) (lex-directive)]
            [char-alphabetic? (put-char sp c) (lex-directive)]
            [else ($unread-char c) (directive)])
          (define (lex-directive) (next)))
        (define-state-case lex-id c
          [eof (return-token 'id (string->symbol (get-buf)))]
          [char-alphabetic? (put-char sp c) (lex-id)]
          [else ($unread-char c) (return-token 'id (string->symbol (get-buf)))])
        (module (lex-number)
          (define finish-number
            (lambda ()
              (let ([str (get-buf)])
                (let ([n (string->number str 16)])
                  (unless n (errorf 'lexer "unexpected number ~a" str))
                  (return-token 'number n)))))
          (define-state-case lex-number c
            [eof (finish-number)]
            [char-numeric? (put-char sp c) (lex-number)]
            [else ($unread-char c) (finish-number)]))
        (define-state-case lex-comment c
          [eof (lex)]
          [#\newline ($ws!) (lex)]
          [else (lex-comment)])
        (lex))))

  (record-writer (record-type-descriptor token)
    (lambda (x p wr)
      (put-char p #\[)
      (wr (token-type x) p)
      (put-char p #\,)
      (put-char p #\space)
      (wr (token-value x) p)
      (put-char p #\])
      (put-char p #\:)
      (wr (token-bfp x) p)
      (put-char p #\-)
      (wr (token-efp x) p)))
)

(module parser ()
  (export parse *sfd*)
  (import (chezscheme) (streams) (lexer))
  (define *sfd*)
  (module (define-grammar is sat parse-consumed-all? parse-result-value grammar-trace make-src)
    (define (sep->parser sep)
      (cond
        [(char? sep) (sat (lambda (x) (and (eq? (token-type x) 'sep) (eq? (token-value x) sep))))]
        [(symbol? sep) (sat (lambda (x) (eq? (token-type x) sep)))]
        [else (errorf "don't know how to parse separator: ~s" sep)]))
    (meta define (constant? x) (let ([x (syntax->datum x)]) (or (string? x) (char? x))))
    (define constant->parser
      (lambda (const)
        (define (token-sat type val)
          (sat (lambda (x)
                 (let ([ans (and (token? x) (eqv? (token-type x) type) (eqv? (token-value x) val))])
                   (when (grammar-trace) (printf "    ~s is [~s, ~a]? => ~s~%" x type val ans))
                   ans))))
        (if (string? const)
            (case const
              [else (token-sat 'id (string->symbol const))])
            (case const
              [#\# (token-sat 'hash const)]
              [#\: (token-sat 'colon const)]
              [#\$ (token-sat 'dollar const)]
              [else (errorf 'constant->parser "don't know how to construct a parser for ~a" const)]))))
    (meta define (constant->markdown k)
      (format "~a" k))
    (define binop->parser
      (lambda (binop)
        (define (binop-sat type val)
          (is val
            (where [x <- item] (and (token? x) (eq? (token-type x) type) (eq? (token-value x) val)))))
        (define (unexpected) (errorf 'binop->parser "don't know how to construct a parser for ~a" binop))
        (unexpected)))
    (define make-src
      (lambda (bfp efp)
        (make-source-object *sfd* bfp efp)))
    (include "ez-grammar.ss")
    #;(grammar-trace #t))

  (define token
    (case-lambda
      [(type)
       (is (token-value x)
         (where
           [x <- (sat (lambda (x)
                        (let ([ans (eq? (token-type x) type)])
                          (when (grammar-trace) (printf "    ~s is ~s? => ~s~%" x type ans))
                          ans)))]))]
      [(type val)
       (is (token-value x)
         (where
           [x <- (sat (lambda (x)
                        (let ([ans (and
                                     (eq? (token-type x) type)
                                     (eqv? (token-value x) val))])
                          (when (grammar-trace) (printf "    ~s is [~s, ~s]? => ~s~%" x type val ans))
                          ans)))]))]))

  (define identifier (token 'id))
  (define label (token 'label))
  (define number (token 'number))

  (define-grammar asm6502 (markdown-directory ".")
    (TERMINALS
      (identifier (id) (DESCRIPTION ("Uh...")))
      (number (num) (DESCRIPTION ("A number literal"))))
    (absolute-mode-opcode (abs-op)
      [adc :: src "adc" =>
        (lambda (src)
          (make-annotation 'adc src 'adc))]
      [sta :: src "sta" =>
        (lambda (src)
          (make-annotation 'sta src 'sta))])
    (branch-opcode (branch-op)
      [beq :: src "beq" =>
        (lambda (src)
          (make-annotation 'beq src 'beq))]
      [bne :: src "bne" =>
        (lambda (src)
          (make-annotation 'bne src 'bne))])
    (immediate-mode-opcode (imm-op)
      [cmp :: src "cmp" =>
        (lambda (src)
          (make-annotation 'cmp src 'cmp))]
      [cpx :: src "cpx" =>
        (lambda (src)
          (make-annotation 'cpx src 'cpx))]
      [lda :: src "lda" =>
        (lambda (src)
          (make-annotation 'lda src 'lda))])
    (implied-mode-opcode (implied-op)
      [brk :: src "brk" =>
        (lambda (src)
          (make-annotation 'brk src 'brk))]
      [dex :: src "dex" =>
        (lambda (src)
          (make-annotation 'dex src 'dex))]
      [pha :: src "pha" =>
        (lambda (src)
          (make-annotation 'pha src 'pha))]
      [pla :: src "pla" =>
        (lambda (src)
          (make-annotation 'pla src 'pla))]
      [tax :: src "tax" =>
        (lambda (src)
          (make-annotation 'tax src 'tax))]
      [tay :: src "tay" =>
        (lambda (src)
          (make-annotation 'tay src 'tay))]
      [txa :: src "txa" =>
        (lambda (src)
          (make-annotation 'txa src 'txa))]
      [tya :: src "tya" =>
        (lambda (src)
          (make-annotation 'tya src 'tya))])
    (jump-opcode (jmp)
      [jmp :: src "jmp" =>
        (lambda (src)
          (make-annotation 'jmp src 'jmp))])
    (label-ref (lbl)
      [label-ref :: src id =>
        (lambda (src lbl)
          (make-annotation `(label-ref ',lbl) src
            `(label-ref ',lbl)))])
    (absolute-address (abs-addr)
      [absolute-addr :: src #\$ num =>
        (lambda (src addr)
          (make-annotation `(absolute-address ,addr) src
            `(absolute-address ,addr)))])
    (instruction (instr)
      [absolute-instruction :: src abs-op abs-addr =>
        (lambda (src op addr)
          (make-annotation `(instruction ',op ,addr) src
            `(instruction ',(annotation-stripped op) ,(annotation-stripped addr))))]
      [branch-to-label :: src branch-op lbl =>
        (lambda (src op lbl)
          (make-annotation `(instruction ',op ,lbl) src
            `(instruction ',(annotation-stripped op) ,(annotation-stripped lbl))))]
      [immediate-instruction :: src imm-op #\# num =>
        (lambda (src imm-op imm)
          ;; NB: probably want imm tagged as an immediate
          (make-annotation `(instruction ',imm-op ,imm) src
            `(instruction ',(annotation-stripped imm-op) ,imm)))]
      [implied-instruction :: src implied-op =>
        (lambda (src implied-op)
          (make-annotation `(instruction ',implied-op) src
            `(instruction ',implied-op)))]
      [jump-to-label :: src jmp lbl =>
        (lambda (src jmp lbl)
          (make-annotation `(instruction ',jmp ,lbl) src
            `(instruction ',(annotation-stripped jmp) ,(annotation-stripped lbl))))]
      [label-declaration :: src id #\: =>
        (lambda (src label)
          (make-annotation `(label ',label) src `(label ',label)))])
    (asm6502 (program)
      [program :: src (K+ instr) =>
        (lambda (src instr+)
          (make-annotation `(program ,@instr+) src
            `(program ,@(map annotation-stripped instr+))))]))

  (define parse
    (lambda (fn ip)
      (let ([token-stream (lexer fn ip)])
        (define (oops)
          (let ([last-token (stream-last-forced token-stream)])
            (if last-token
                (errorf 'parse "parse error at or before character ~s of ~a" (token-bfp last-token) fn)
                (errorf 'parse "no expressions found in ~a" fn))))
        ;;; return the first result, if any, for which the input stream was entirely consumed.
        (let loop ([res* (asm6502 token-stream)])
          (if (null? res*)
              (oops)
              (let ([res (car res*)])
                (if (parse-consumed-all? res)
                    (parse-result-value res)
                    (loop (cdr res*))))))))))

(define run
  (lambda (fn)
    (import parser)
    (let* ([ip (open-file-input-port fn)]
           [sfd (make-source-file-descriptor fn ip #t)]
           [ip (transcoded-port ip (native-transcoder))])
      (fluid-let ([*sfd* sfd])
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
