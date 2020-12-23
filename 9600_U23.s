       NAM    SYSMON 1.1
       OPT    O,NOC

********************************
*     SYSTEM MONITOR EPROM     *
*             FOR              *
*     CREATIVE MICRO SYSTEMS   *
*           9600 MPU           *
********************************

*COPYRIGHT (C) 1978 CREATIVE MICRO SYSTEMS, INC.

*****REVISION HISTORY*****
*
*VERSION 1.0 MAY 19, 1978 ORIGINAL RELEASE
*VERSION 1.1 JUNE 20, 1979 LINE 650 CORRECTED
*

*SYSTEM LINKAGES
*
ACIAS  EQU    $E3C0  CONTROL-STATUS REGISTER
ACIAD  EQU    $E3C1  DATA REGISTER
*
* POwER ON INITIALIZATION OR RESET
*
       ORG    $F800
START  EQU    *
       LDS    #STACK
       STS    SP
       CLR    ECHO   ECHO All INPU1 CHlRACTERS
       LDX    #SFE   SET SWI VECTOR
       STX    SWIPTR
       STX    NIO
       LDA A  #$17
       STA A  ACIAS  MASTER RESET ACIA
INZ    LDA A  #$16
* SET FOR 8 BITS + 1 STOP BIT
* DIV BY 64 ANO RTS BAR = LOW
* AND ALLOW SOFTWARE CONTROL
INZ1   STA A  ACIAT
*
* NORMAL CONTROL ENTRY POINT
*
CONTRL LDA A  ACIAT
       STA A  ACIAS
       LDS    #TSTACK
       CLR    TFLAG  TURN OFF TRACE IF ON
       CLR    BKFLG  CLEAR BOTH 8REAKPOINT FLAGS
       CLR    BKFLG2
       LDX    #MCL
       BSR    PDATA1 OUTPUT MONITOR PROMPT
       BSR    INCH   GET INPUT COMMAND
       TAB
       JSR    OUTS
*
* CHECK IF COMMAND IS IN JUMP TABLE
*
       LDX    #FUTABL
NXTCHR CMP B  0,X    DOES CHR MATCH
       BEQ    GOODCH
       INX
       INX
       INX
       CPX    #TBLEND
       BNE    NXTCHR
       JMP    CKCBA  CHECK FOR C, B, A OR X
GOODCH LDX    1,X
       JMP    0,X
* IRQ INTERUPT SEQUENCE
*
IO     LDX    IOV
       JMP    X
*      NMI SEQUENCE
POWDWN LDX    NIO
       JMP    X
* SWI INTERUPT SEQUENCE
*
SWI    LDX    SWIPTR
       JMP    X
LOAD19 LDA A  #'?
       BSR    OUTCH
C1     BRA    CONTRL
*
* BUILD AOORESS
BADDR  BSR    BYTE
       STA A  XHI
       BSR    BYTE
       STA A  XLOW
       LDX    XHI
       RTS
*
* INPUT BYTE
BYTE   BSR    INHEX
       ASL A
       ASL A
       ASL A
       ASL A
       TAB
       BSR    INHEX
       ABA
       RTS
*
OUTHL  LSR A
       LSR A
       LSR A
       LSR A
*
*
OUTHR  AND A  #$F
       ADD A  #$30
       CMP A  #$39
       BLS    OUTCH
       ADD A  #7
OUTCH  JMP    OUTEEE
INCH   JMP    INEEE
*
PDATA2 BSR    OUTCH
       INX
PDATA1 LDA A  X
       CMP A  #4
       BNE    PDATA2
       RTS
*
*CHANGE M AAA DD NN
CHANGE BSR    BADDR
CHA51  LDX    #MCL
       BSR    PDATA1
       BSR    OUTXHI
       BSR    OUT2HS
       STX    XHI
       BSR    INCH
       CMP A  #$20   SPACE
       BEQ    CHA51  NEXT ADD
TDEX   CMP A  #'^
       BEQ    CHA71
       BSR    INHEX+2
       BSR    BYTE+2
       DEX
       BRA    CHA61
CHA71  DEX
       DEX
       STX    XHI
       BRA    CHA51
*
CHA61  STA A  X
       CMP A  X
       BEQ    CHA51
XBK    BRA    LOAD19
*
INHEX  BSR    INCH
       SUB A  #$30
       BMI    C1     NOT HEX
       CMP A  #9
       BLE    IN1HG
       CMP A  #$11
       BMI    C1
       CMP A  #$16
       BGT    C1
       SUB A  #7
IN1HG  RTS
OUT2H  LDA A  X
OUT2HA BSR    OUTHL
       LDA A  X
       INX
       BRA    OUTHR
*
OUT4HS BSR    OUT2H
OUT2HS BSR    OUT2H
OUTS   LDA A  #$20
       BRA    OUTCH
*
* SET BREAKPOINT(S) AT ENTERED ADDRESS
*
BKPNT2 JSR    ADDR
       STX    PC1
       LDA A  X
       STA A  BKFLG2
       BEQ    XBK    A ZERO IS NO PLACE FOR A BREAK
       LDA A  #$3F
       STA A  X
BKPNT  JSR    ADDR
       STX    PB2
       LDA A  X
       STA A  BKFLG
       BEQ    XBK
       LDA A  #$3F
       STA A  X
       JSR    CRLF
*
* FALL INTO "G" COMMAND SEQUENCE
*
CONTG  LDS    SP
       RTI
*
* PRINT XHI ADDRESS SUBROUTINE
*
OUTXHI LDX    #XHI
       BSR    OUT4HS
       LDX    XHI
       RTS
*
* VECTORED SWI ROUTINE
*    ADJUST PROGRAM COUNTER FOR INTERUPT ADDRESS
*    ANO DUMP REGISTERS UNLESS IN THE TRACE MODE
*
SFE    STS    SP
       TSX
       TST    6,X
       BNE    *+4
       DEC    5,X
       DEC    6,X
       TST    TFLAG
       BEQ    PRINT
       JMP    SWTURN IF IN TRACE
*
* FALL INTO PRINT REGISTERS
*  ALSO CALLED AS SUBROUTINE BY TRACE
*
PRINT  LDX    SP
       LDA A  #6
       STA A  MCONT
       LDA B  1,X    GET CONDITION CODES
       ASL B
       ASL B
       LDX    #CSET  MNEMONI CONDITION CODES
DSOOP  LDA A  #'-
       ASL B
       BCC    DSOOP1
       LDA A  X
DSOOP1 JSR    OUTEEE
       INX
       DEC    MCONT
       BNE    DSOOP
       LDX    #BREG
       BSR    PDAT
       LDX    SP
       INX
       INX
       JSR    OUT2HS
       STX    TEMP
       LDX    #AREG
       BSR    PDAT
       LDX    TEMP
       JSR    OUT2HS
       STX    TEMP
       LDX    #XREG
       BSR    PDAT
       LDX    TEMP
       BSR    PRTS
       STX    TEMP
       TST    TFLAG
       BNE    PNTS
       LDX    #PCTR
       BSR    PDAT
       LDX    TEMP
       BSR    PRTS
PNTS   LDX    #SREG
       BSR    PDAT
       LDX    #SP
       TST    TFLAG
       BNE    PRINTS
       BSR    PRTS
* CHECK IF ANY BREAKPOINTS SET
       LDA A  BKFLG
       BEQ    C2
       LDX    PB2
       STA A  X      RESTORE INSTRUCTION
       LDA A  BKFLG2
       BEQ    C2
       LDX    PC1
       STA A  X
C2     BRA    CR8
PDAT   JMP    PDATA1
* SET ECHO FUNCTION
ECHON  CLR B
PRNTON NEG B         NOTE: -0 IS STILL 0
ECHOFF STA B  ECHO
       BRA    CR8
*
* PRINT STACK POINTER THAT WILL BE IN USE AFTER THE
*
PRINTS LDA B  X
       LDA A  1,X
       ADD A  #7
       ADC B  #0
       STA B  TEMP
       STA A  TEMP+1
       LDX    #TEMP
PRTS   JMP    OUT4HS
*
* CHARACTER INPUT ROUTINE
*
INEEE  LDA A  ACIAS
       ASR A
       BCC    INEEE
       LDA A  ACIAD
       AND A  #$7F
       CMP A  #$7F
       BEQ    INEEE  RUBOUT
       TST    ECHO
       BLE    OUTEEE
       RTS           IF ECHO OFF
CR8    BRA    CR9
*
* CHARACTER OUTPUT ROUTINE
*
OUTEEE TST    ECHO
       BGE    OUTCH2
       JMP    PRINTR IF ECHO IS NEGATIVE
OUTCH2 PSH B
OUTCH1 LDA B  ACIAS
       ASR B
       ASR B
       BCC    OUTCH1
       STA A  ACIAD
       PUL B
       RTS
*
* HERE ON "J" COMMAND
*
JUMP   LDX    #TOADD
       BSR    ENDADD+3
       LDS    #STACK
       JMP    X
*
* FILL MEMORY "I" COMMAND
*
IFILL  BSR    LIMITS
       BSR    VALUE
       LDX    BEGA
       DEX
FILLOP INX
       STA A  X
       CPX    ENDA
       BNE    FILLOP
       BRA    CR8
*
* INPUT DATA SUBROUTINES
*
BAD2   LDX    #FROMAD
       BRA    *+5
ENDADD LDX    #THRUAD
       JSR    PDATA1
       JMP    BADDR
LIMITS BSR    BAD2
       STX    BEGA
       BSR    ENDADD
       STX    ENDA
       JMP    CRLF
ADDR   LDX    #ADASC
       BRA    ENDADD+3
VALUE  LDX    #VALASC
       JSR    PDATA1
       JMP    BYTE
*
* BLOCK MOVE "M" COMMAND
*
MOVE   BSR    LIMITS
       LDX    #TOADD
       BSR    ENDADD+3
       LDX    BEGA
       DEX
MLOOP  INX
       LDA A  X
       STX    BEGA
       LDX    XHI
       STA A  X
       INX
       STX    XHI
       LDX    BEGA
       CPX    ENDA
       BNE    MLOOP
CR9    JMP    CONTRL
*
* SEARCH MEMORY "F" COMMAND
*
FIND   BSR    LIMITS
       BSR    VALUE
       TAB
       LDX    BEGA
       DEX
FLOOP  INX
       LDA A  X
       CBA
       BNE    NOPNT
       STX    XHI
       BSR    CRLF
       JSR    OUTXHI PRINT ALL ADDRESSES THAT CO...
NOPNT  CPX    ENDA
       BNE    FLOOP
       BRA    CR9
* SUBROUTINE TO INSERT BLANK SPACES
SKIP   LDA A  #$20
       JSR    OUTEEE
       DEC B
       BNE    SKIP
       RTS
* PRINT BYTE IN A REGISTER SUBROUTINE
PNTBYT STA A  BYTECT
       LDX    #BYTECT
       JMP    OUT2H
* CARRIAGE RETURN WITHOUT PROMPT
CRLF   LDX    #CRLFAS
       JMP    PDATA1
*
* DISASSEMBLE "D" COMMAND
*
DISSA  JSR    BAD2
       BRA    DISS
*
* TRACE "T" COMMAND
*
TRACE  JSR    BAD2
       BSR    CRLF
       LDX    SP
       LDA B  XHI    SET PROGRAM COUNTER TO ENTER
       STA B  6,X
       LDA A  XLOW
       STA A  7,X
KONTIN INC    TFLAG     TRACE ENTRY IS PC ALREADY S..
RETURN LDS    #TSTACK   USE SPERATE STACK FOR TRACE
       JSR    PRINT  DO "R" DUMP
       LDX    SP
       LDX    6,X    GET PROGRAM LOCATION
DISS   STX    PC1
DISIN  BSR    CRLF
       LDX    #PC1
       JSR    OUT4HS PRINT ADDRESS
       LDX    #BFLAG CLEAR ALL TRACE FLAGS
       LDA A  #5
CLEAR  CLR    0,X
       INX
       DEC A
       BNE    CLEAR
       LDX    PC1    PROGRAM COUNTER (PC)
       LDA B  X
       JSR    OUT2HS PRINT INSTR
       STX    PC1    SAVE INCREMENTED PC
       LDA A  X
       STA A  PB2    1ST INST BYTE
       LDA A  1,X
       STA A PB3     2ND INST BYTE
       STA B PB1     INSTRUCTION
       TBA
       JSR   TBLKUP  GET MNEMONIC
       LDA A  TEMP
       CMP A  #'*    VALID INST?
       BNE    OKOP   YES
       JMP    NOTBB  IGNORE IF INVALID
OKOP   LDA A  PB1
       CMP A  #$8D   BSR?
       BNE    NEXT
       INC    BFLAG  BRANCH FLAG
       BRA    PUT1
NEXT   AND A  #$F0
       CMP A  #$60
       BEQ    ISX    IF INDEXED
       CMP A  #$A0
       BEQ    ISX
       CMP A  #$E0
       BEQ    ISX
       CMP A  #$80
       BEQ    IMM    IF IMMEDIATE
       CMP A  #$C0
       BNE    PUT1
IMM    INC    MFLAG  IMMEDIATE FLAG
       LDX    #SPLBDO PRINT #$
       BRA    PUT
ISX    INC    XFLAG  INDEXED FLAG
       LDA A  PB2
       JSR    PNTBYT PRINT OPERAND
       LDX    #COMMX PRINT  ,X
PUT    JSR    PDATA1
PUT1   LDX    PC1
       LDA A  PB1
       CMP A  #$8C
       BEQ    BYT3
       CMP A  #$8E
       BEQ    BYT3
       CMP A  #$CE
       BEQ    BYT3
       AND A  #$F0
       CMP A  #$20
       BNE    NOTB
BBR    INC    BFLAG
       BRA    BYT2
NOTB   CMP A  #$60
       BCS    BYT1
       AND A  #$30
       CMP A  #$30
       BNE    BYT2
BYT3   INC    BITE3  3 BYTE INST TYPE FLAG
       TST    MFLAG
       BNE    BYT31
       LDA A  #'$
       JSR    OUTEEE
BYT31  LDA A  X
       INX
       STX    PC1
       JSR    PNTBYT
       LDX    PC1
       BRA    BYT21
BYT2   INC    BITE2         2 BYTE INST TYPE FLAG
BYT21  LDA A  0,X
       INX
       STX    PC1
       TST    XFLAG
       BNE    BYT1
       TST    BITE3
       BNE    BYT22
       TST    MFLAG
       BNE    BYT22
       TAB
       LDA A  #'$
       JSR    OUTEEE
       TBA
BYT22  JSR    PNTBYT
BYT1   TST    BFLAG
       BEQ    NOTBB
       LDA B  #3
       JSR    SKIP
       CLR A
       LDA B  PB2           COMPUTE TARGET
       BGE    DPOS          ADDRESS IF BRANCH
       LDA A  #$FF
DPOS   ADD B  PC2
       ADC A  PC1
       STA A  BPOINT
       STA B  BPOINT+1
       LDX    #BPOINT
       JSR    OUT4HS        TARGET ADD
* PRINT ASCII VALUE OF INST AND OPERANDS
NOTBB  LDA B  #13
       LDA A  #1
       TST    BITE2
       BEQ    DUMP3
       LDA B  #1
       TST    BFLAG
       BNE    DUMP2
       LDA B  #8
       TST    MFLAG
       BNE    DUMP2
       TST    XFLAG
       BNE    DUMP2
       LDA B  #9
DUMP2  LDA A  #2
       BRA    DUMP8
DUMP3  TST    BITE3
       BEQ    DUMP8
       LDA A  #3
       LDA B  #6
       TST    MFLAG
       BEQ    DUMP8
       LDA B  #5
DUMP8  PSH A
       JSR    SKIP
       PUL B
       LDX    #PB1
DUMP4  LDA A  0,X
       CMP A  #$20
       BLE    DUMP5
       CMP A  #$60
       BLE    NOTDOT
DUMP5  LDA A  #'.    PRINT "." IF OUTSIOE
NOTDOT INX           HEX 21 TO 5F RANGE
       JSR    OUTEEE
       DEC B
       BNE    DUMP4
NOT1   JSR    INEEE  GET COMMAND
       TAB
       JSR    OUTS
       CMP B  #$20
       BEQ    DOT    EXECUTE SPACE COMMAND
*
* CHECK INPUT COMMAND FOR
*  "A","B","C OR "X"
*
CKCBA  LDX    SP
       INX
       CMP B  #'C
       BEQ    RDC
       INX
       CMP B  #'B
       BEQ    RDC
       INX
       CMP B  #'A
       BNE    CHKX
RDC    JSR    OUT2HS PRINT PRESENT VALUE
       DEX
       JSR    BYTE   GET NEW VALUE
       STA A  X
       BRA    RETDID
CHKX   CMP B  #'X
       BNE    RETNOT
       INX
       JSR    OUT4HS
       JSR    BYTE
       LDX    SP
       STA A  4,X
       JSR    BYTE
       STA A  5,X
RETDID TST    TFLAG
       BEQ    RETNOT
       JSR    CRLF
       JSR    PRINT  DO "R" DUMP IF IN TRACE
       BRA    NOT1
RETNOT JMP    CONTRL
DOT    TST    TFLAG
       BNE    DOT1
       JMP    DISIN  IF IN DISASSEMBLER MODE
DOT1   LDA B  #$3F
       LDA A  PB1
       CMP A  #$8D   BSR
       BNE    TSTB
       LDX    BPOINT
       STX    PC1
       CLR    BFLAG  ONLY ONE SWI SET
       BRA    EXEC
TSTB   TST    BFLAG
       BEQ    TSTJ
       LDX    BPOINT
       LDA A  X
       STA A  BPOINT+2
       STA B  X      SET SWI
       BRA    EXEC
TSTJ   CMP A  #$6E   JINDEX
       BEQ    ISXD   IF INDEXED JUMP OR BRANCH
       CMP A  #$AD
       BEQ    ISXD
       CMP A  #$7E
       BEQ    ISJ
       CMP A  #$BD   [PRETTY SURE THIS IS $BD (JMP LONG)]
       BNE    NOTJ
ISJ    LDX    PB2
       STX    PC1
       BRA    EXEC
ISXD   LDX    SP     COMPUTE NEXT INS LOCATION
       LDA A  5,X
       ADD A  PB2
       STA A  PC2
       LDA A  4,X
       ADC A  #0
       STA A  PC1
       BRA    EXEC
NOTJ   LDX    SP
       CMP A  #$39
       BNE    NOTRTS
NOTJ1  LDX    8,X    GET RETURN ADDRESS FROM STACK
       BRA    EXR
NOTRTS CMP A  #$3B   RTI
       BNE    NOTRTI
       LDX    13,X   GET NEXT INSTRUCTION
EXR    STX    PC1
NOTRTI CMP A  #$3F   SWI
       BEQ    NONO
       CMP A  #$3E   WAI
       BEQ    NONO
EXEC   LDX    PC1
       LDA A  X
       STA A  OPSAVE
       STA B  X
       CMP B  X      CHECK FOR RAM
       BNE    CKROM
       JMP    CONTG  EXECUTE INSTRUCTION
NONO   JMP    LOAD19 DO ? EXIT OF TRACE MODE
CKROM  LDA A  PC1
       CMP A  #$F8   IS NEXT INS IN MONITOR ROM?
       BCS    NONO   IF NOT
* GET INSTRUCTION AND CHECK IF JMP OR JSR
       LDX    SP
       LDA A  PB1
       CMP A  #$7E   JUMP?
       BEQ    NOTJ1
       CMP A  #$BD   RTS? [JSR?]
       BNE    NONO
       LDX    6,X    GET INST AODR
       INX           AFTER JSR
       INX
       INX
       BRA    ISJ+3
*RETURN HERE ON SWI IF TRACE FLAG ON
SWTURN LDX    PC1
       LDA A  OPSAVE
       STA A  X      REPLACE SWI'S
       TST    BFLAG
       BEQ    DISPLY
       LDX    BPOINT
       LDA A  BPOINT+2
       STA A  X
DISPLY JMP    RETURN
*
* INSTRUCTION MNEMONIC LOOKUP
* ALGORITHM FOR ALL
* 6800 OP CODES
* ENTER WITH INS CODE IN A REGISTER
*
TBLKUP CMP A  #$40
       BCC    SIX
ONE    JSR    PNT3C
       LDA A  PB1
       CMP A  #$32
       BEQ    THREE
       CMP A  #$36
       BEQ    THREE
       CMP A  #$33
       BEQ    FOUR
       CMP A  #$37
       BEQ    FOUR
TWO    LDX    #BLANK
       BRA    FIVE
THREE  LDX    #PNTA
       BRA    FIVE
FOUR   LDX    #PNTB
FIVE   JMP    PDATA1
SIX    CMP A  #$4E
       BEQ    SEVEN
       CMP A  #$5E
       BNE    EIGHT
SEVEN  CLR A
       BRA    ONE
EIGHT  CMP A  #$80
       BCC    NINE
       AND A  #$4F
       JSR    PNT3C
       LDA A  TEMP
       CMP A  #'*
       BEQ    TWO
       LDA A  PB1
       CMP A  #$60
       BCC    TWO
       AND A  #$10
       BEQ    THREE
       BRA    FOUR
NINE   AND A  #$3F
       CMP A  #$0F
       BEQ    SEVEN
       CMP A  #$07
       BEQ    SEVEN
       AND A  #$0F
       CMP A  #$03
       BEQ    SEVEN
       CMP A  #$0C
       BGE    TEN
       ADD A  #$50
       JSR    PNT3C
       LDA A  PB1
       AND A  #$40
       BEQ    THREE
       BRA    FOUR
TEN    LDA A  PB1
       CMP A  #$8D   [$80?]
       BNE    ELEVEN
       LDA A  #$53
       BRA    ONE
ELEVEN CMP A  #$C0
       BCC    TWELVE
       CMP A  #$90
       BEQ    SEVEN
       AND A  #$0F
       ADD A  #$50
       BRA    THIRTE
TWELVE AND A  #$0F
       ADD A  #$52
       CMP A  #$60
       BLT    SEVEN
THIRTE JMP    ONE
PNT3C  CLR B
       STA A  TEMP
       ASL A
       ADD A  TEMP
       ADC B  #0
       LDX    #TBL
       STX    XTEMP
       ADD A  XTEMP+1
       ADC B  XTEMP
       STA B  XTEMP
       STA A  XTEMP+1
       LDX    XTEMP
       LDA A  0,X
       STA A  TEMP
       BSR    OUTA
       LDA A  1,X
       BSR    OUTA
       LDA A  2,X
OUTA   JMP    OUTEEE
*
* "V" COMMAND WILL DISPLAY A 128 BYTE
* BLOCK OF MEMORY
*
VIEW   JSR    BAD2
VSUB   LDA A  #8
       STA A  MCONT
VIEW5  JSR    CRLF
       JSR    OUTXHI
       LDA B  #16
VIEW1  JSR    OUT2HS
       DEC B
       BIT B  #3
       BNE    VIEW10
       JSR    OUTS
       CMP B  #0
VIEW10 BNE    VIEW1
       JSR    CRLF
       LDA B  #5
       JSR    SKIP
       LDX    XHI
       LDA B  #16
VIEW2  LDA A  X
       CMP A  #$20
       BCS    VIEW3
       CMP A  #$5F
       BCS    VIEW4
VIEW3  LDA A  #'.
VIEW4  BSR    OUTA
       INX
       DEC B
       BNE    VIEW2
       STX    XHI
       DEC    MCONT
       BNE    VIEW5
       JSR    INEEE
       CMP A  #$20
       BEQ    VSUB
       CMP A  #'V
       BEQ    VIEW
       JMP    CONTRL
*
* COMPACTED MNEMONIC TABLE
*
TBL    FCC    '***NOPNOP***'
       FCC    '******TAPTPA'
       FCC    'INXDEXCLVSEV'
       FCC    'CLCSECCLISEI'
       FCC    'SBACBA******'
       FCC    '******TABTBA'
       FCC    '***DAA***ABA'
       FCC    '************'
       FCC    'BRA***BHIBLS'
       FCC    'BCCBCSBNEBEQ'
       FCC    'BVCBVSBPLBMI'
       FCC    'BGEBLTBGTBLE'
       FCC    'TSXINSPULPUL'
       FCC    'DESTXSPSHPSH'
       FCC    '***RTS***RTI'
       FCC    '******WAISWI'
       FCC    'NEG******COM'
       FCC    'LSR***RORASR'
       FCC    'ASLROLDEC***'
       FCC    'INCTSTJMPCLR'
       FCC    'SUBCMPSBCBSR'
       FCC    'ANDBITLDASTA'
       FCC    'EORADCORAADD'
       FCC    'CPXJSRLDSSTS'
       FCC    'LDXSTX'
SPLBDO FCC    /#$/
       FCB    4
COMMX  FCB    $2C,$58,4
BLANK  FCC    /   /
       FCB    4
PNTA   FCC    / A /
       FCB    4
PNTB   FCC    / B /
       FCB    4
MCL    FCB    $D,$A,$15,$13,0,'*,4
BREG   FCB    $20,'B,'=,4
AREG   FCB    'A,'=,4
XREG   FCB    'X,'=,4
SREG   FCB    'S,'=,4
PCTR   FCB    'P,'C,'=,4
CSET   FCC    /HINZVC/
CRLFAS FCB    $D,$A,$15,0,4
ADASC  FCB    $0D,$0A
       FCC    'BKADD '
       FCB    4
FROMAD FCB    $0D,$0A
       FCC    'FROM ADDR '
       FCB    4
THRUAD FCB    $0D,$0A
       FCC    'THRU ADDR '
       FCB    4
TOADD  FCC    'TO ADDR '
       FCB    4
VALASC FCC    'VALUE '
       FCB    4
*
* COMMAND JUMP TABLE
*
FUTABL EQU    *
       FCC    /M/
       FDB    MOVE
       FCB    'E
       FDB    CHANGE
       FCC    /G/
       FDB    CONTG
       FCC    /R/
       FDB    PRINT
       FCC    /T/
       FDB    TRACE
       FCC    /H/
       FDB    PRNTON
       FCC    /V/
       FDB    VIEW
       FCC    /I/
       FDB    IFILL
       FCC    /J/
       FDB    JUMP
       FCB    'F
       FDB    FIND
       FCC    /Q/
       FDB    CONTRL DISK MONITOR VECTOR HERE
       FCC    /D/
       FDB    DISSA
       FCC    /K/
       FDB    KONTIN
       FCC    /1/
       FDB    BKPNT
       FCC    /2/
       FDB    BKPNT2
       FCC    /L/
       FDB    CONTRL LOAD ROUTINE VECTOR HERE
       FCC    /S/
       FDB    CONTRL SAVE ROUTINE VECTOR HERE
       FCC    /O/
       FDB    ECHON
       FCC    /N/
       FDB    ECHOFF
TBLEND EQU    *
*      ORG    $FFF8
       FDB    IO     IRQ VECTOR
       FDB    SWI    SWI VECTOR
       FDB    POWDWN NMI VECTOR
       FDB    START  RESET VECTOR
*
* RAM MONITOR VARIABLES
       ORG    $E7B0
STACK  RMB    30
TSTACK RMB    1      TRACE MODE STACK
IOV    RMB    2      I/O INTERUPT POINTER
BEGA   RMB    2
ENDA   RMB    2
NIO    RMB    2      NMI INTERUPT POINTER
SP     RMB    2      PROGRAM STACK POINTER
ACIAT  RMB    1      ACIA STATUS WORD
ECHO   RMB    1      ECHO FLAG
XHI    RMB    1
XLOW   RMB    1
TEMP   RMB    2
SWIPTR RMB    2      SWI POINTER
TFLAG  RMB    1
XTEMP  RMB    2
BPOINT RMB    3
BKFLG  RMB    1      BREAKPOINT INSTRUCITON
BKFLG2 RMB    1      BREAKPOINT INSTRUCITON
MCONT  RMB    1
BFLAG  RMB    1
MFLAG  RMB    1
XFLAG  RMB    1
BITE2  RMB    1
BITE3  RMB    1
PRINTR RMB    3      PUT JUMP TO USER PRINT ROUTINE
OPSAVE RMB    1
PB1    RMB    1
PB2    RMB    1
PB3    RMB    1
BYTECT RMB    1
PC1    RMB    1
PC2    RMB    1
       END

