.ORIG   x3000

;; SKIP HALTS
AND     R0,R0,#0
BRnp    #1
BRz     #1
HALT
ADD     R0,R0,1
BRz     #1
BRp     #1
HALT
ADD     R0,R0,#-2
BRp     #1
BRnp    #1
HALT

;; JUMP
        LEA     R0, NO_HALT
        JMP     R0
        HALT
NO_HALT AND     R0,R0,#0

;; JSR
JSR     #1
HALT
ADD     R7,R7,#3
RET

;; JSRR
LEA     R0, JSR_F
JSRR    R0
HALT
JSR_F ADD     R7,R7,#3
      RET

; ZERO R1
AND     R1,R1,#0
; SET R1 to 6
ADD     R1,R1,#10
ADD     R1,R1,#-4     ; R1 == 6
; INVERT R1 (to -6)
NOT     R1,R1
ADD     R1,R1,#1      ; R1 == -6
; SET R1 to 6
ADD     R1,R1,#9
ADD     R1,R1,#-3     ; R1 == 0
ADD     R1,R1,#11
ADD     R1,R1,#-5     ; R1 == 6
; MULTIPLY BY 10
ADD     R0,R1,R1      ; R0 == 2*R1
ADD     R0,R0,R0      ; R0 == 4*R1
ADD     R0,R0,R1      ; R0 == 5*R1
ADD     R0,R0,R0      ; R0 == 10*R1
; ADD 19
ADD     R0,R0,5       ; R0 == 10*R1 + 5
ADD     R0,R0,14      ; R0 == 10*R1 + 5 + 14
;; PRINT CHAR
TRAP    x21

;; LOAD AND PRINT K
AND     R1,R1,#0
ADD     R1,R1,#15
ADD     R1,R1,#15
ADD     R1,R1,#15
ADD     R1,R1,#15
ADD     R1,R1,#15
STI     R1,K_ADDR
LDI     R0,K_ADDR
TRAP    x21

;; LOAD AND PRINT E
LD         R0,E
TRAP       x21

;; LOAD AND PRINT E
LEA        R0,E
ADD        R0,R0,#-5
LDR        R0,R0,#5
TRAP       x21
LEA        R0,E
ADD        R0,R0,#5
LDR        R0,R0,#-5
TRAP       x21

;; STORE AND PRINT Y
LD         R5,X_A
ADD        R5,R5,#1
ST         R5,X_A
LD         R0,X_A
TRAP       x21

;; STORE AND PRINT Y
LD         R5,X_B 
ADD        R5,R5,#1
LEA        R6,X_B
ADD        R6,R6,#-5
STR        R5,R6,#5
LD         R0,X_B
TRAP       x21
LEA        R6,X_B
ADD        R6,R6,#5
STR        R5,R6,#-5
LD         R0,X_B
TRAP       x21

;; NEWLINE
AND     R0,R0,#0      ; R0 == 0
ADD     R0,R0,#10     ; R0 == 10
TRAP    x21

HALT
K_ADDR   .FILL    x4000
E        .FILL    #69
X_A      .FILL    #88
X_B      .FILL    #88
X_C      .FILL    #88
.END
