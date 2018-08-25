;-------------------------
; 6502asm.com version
;-------------------------
; lda #4
; 
; cmp #0
; bne continue
; lda #1
; jmp abyss ; for 6502asm.com
; continue:
; tax
; iterate:
; dex
; cpx #1
; beq end
; tay
; txa
; pha
; tya
; jmp multiply
; return:
; tay
; pla
; tax
; tya
; jmp iterate
; end:
; jmp abyss ; for 6502asm.com
; 
; multiply:
; sta $0000  ; stash A operand value in memory
; lda #0     ; initialize A as the result register
; loop:
; cpx #0     ; base case: X is zero and we're done iterating
; beq return    ; if we're done, jump to the end
; adc $0000  ; add the original operand value to A
; dex        ; decrement X
; jmp loop   ; jump back to the 
; 
; abyss:
; sta $200

;-------------------------
; maf6502 target version
;-------------------------
  lda #5        ; test input, aka, the only fact argument

  cmp #0        ; special case zero (even though fact doesn't accept 0
  bne continue  ; skip the zero case
  lda #1        ; load 1 in A, our result register
  brk           ; FINISHED
continue:
  tax           ; copy A to X to seed iteration
iterate:
  dex           ; decrement X so we can do n * (n-1)
  cpx #1        ; base case: (n-1) == 1
  beq end       ; if so, jump to the end
  tay           ; otherwise, stash A in Y
  txa           ; copy our current iteration value to A
  pha           ; stash the iteration variable on the stack
  tya           ; retrieve A's value from Y
  sta $0000     ; put A into memory
  lda #0        ; initialize A as the result register
loop:
  cpx #0        ; base case: X is zero and we're done iterating
  beq return    ; if we're done, jump back to fact
  adc $0000     ; otherwise, add the original operand value to A
  dex           ; decrement X
  jmp loop      ; jump back to the top of the multiplication loop
return:
  tay           ; stash A in Y
  pla           ; retrieve our iterator value for factorial from the stack
  tax           ; put the iterator back into X
  tya           ; get the multiplication result from Y into A
  jmp iterate   ; go back to the top of fact's iteration
end:
  brk

