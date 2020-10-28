* f9dasm: M6800/1/2/3/8/9 / H6309 Binary/OS9/FLEX9 Disassembler V1.80
* Loaded binary file XK-300_U12.bin

* D5BUG
* (SAME AS USED IN MEK6802D5)

* ROM LABEL:
*       ELENCO
*       ADVENT
*       ELECTRONICS
*       (312) 297-6200

*****************************************************
** Used Labels                                      *
*****************************************************

STKTOP  EQU     $E47E

PIA     EQU     $E484
KPCOL   EQU     $0
KPROW   EQU     $2
ANOD    EQU     $E484
CATH    EQU     $E486
PIAROW  EQU     $E486
PIADP   EQU     $E484
PIACR   EQU     $E485
PIAPB   EQU     $E486
PIADPB  EQU     $E486
PIACRA  EQU     $E485
PIACRB  EQU     $E487

*****************************************************
** Program Code / Data Areas                        *
*****************************************************

        ORG     $E419

* -------------------------------------------------------
* DEFS - DEFINITIONS & SCRATCH LOCATIONS
* -------------------------------------------------------
MNPTR   RMB     2                        *E419: 
KEY     RMB     1                        *E41B: 
KEYFLG  RMB     1                        *E41C: 
DISBUF  RMB     6                        *E41D: 
ROLPAS  RMB     1                        *E423: 
XSAVD   RMB     2                        *E424: 
XTMP1   RMB     2                        *E426: 
ME428   RMB     2                        *E428: 
MEMSAV  RMB     2                        *E42A: 
HEXBUF  RMB     3                        *E42C: 
USP     RMB     2                        *E42F: 
UCC     RMB     1                        *E431: 
UB      RMB     1                        *E432: 
UA      RMB     1                        *E433: 
UX      RMB     2                        *E434: 
UPC     RMB     2                        *E436: 
ROIFLG  RMB     1                        *E438: 
ROIBAK  RMB     2                        *E439: 
UPROG   RMB     1                        *E43B: 
UIRQV   RMB     2                        *E43C: 
FNCFL   RMB     1                        *E43E: 
FNCPNT  RMB     2                        *E43F: 
REGNO   RMB     1                        *E441: 
BKPNTR  RMB     2                        *E442: 
BRKNO   RMB     1                        *E444: 
BRKTAB  RMB     20                       *E445: 
BRKEND  EQU     $E459

* CASSETTE INTERFACE SCRATCH LOCATIONS
*  
BYTE    RMB     1                        *E459: 
CYCNT   RMB     2                        *E45A: 
GOOD1S  RMB     1                        *E45C: 
OLD     RMB     1                        *E45D: 
CHKSM   RMB     1                        *E45E: 
NBITS   RMB     1                        *E45F: 
BEGAD   RMB     2                        *E460: 
ENDAD   RMB     2                        *E462: 

        ORG     $F000 

        NAM RESET
        OPT CREF,LLEN=80

* -------------------------------------------------------
* RESET - COLD START ROUTINE
* -------------------------------------------------------
RESET   NOP                              *F000: 01             '.'     SET INTERRUPT MASK
        SEI                              *F001: 0F             '.'
        LDX     #$E3FF                   *F002: CE E3 FF       '...'   CLEAR RAM
CLRLOP  INX                              *F005: 08             '.'
        CLR     ,X                       *F006: 6F 00          'o.'
        CPX     #PIACRB                  *F008: 8C E4 87       '...'
        BNE     CLRLOP                   *F00B: 26 F8          '&.'
        LDX     #PIADP                   *F00D: CE E4 84       '...'   INITIALIZE SYSTEM PIA
        LDAA    #$7F                     *F010: 86 7F          '..'
        STAA    ,X                       *F012: A7 00          '..'
        LDAA    #$FF                     *F014: 86 FF          '..'
        STAA    $02,X                    *F016: A7 02          '..'
        LDAA    #$06                     *F018: 86 06          '..'
        STAA    $01,X                    *F01A: A7 01          '..'
        STAA    $03,X                    *F01C: A7 03          '..'
        LDX     #$E418                   *F01E: CE E4 18       '...'   DEFAULT USER STACK
        STX     USP                      *F021: FF E4 2F       '../'

* -------------------------------------------------------
* PROMPT - ROUTINE TO SET UP PROMPT CONDITIONS
* -------------------------------------------------------
PROMPT  LDS     #STKTOP                  *F024: 8E E4 7E       '..~'   INIT SYSTEM STACK
        LDAA    #$01                     *F027: 86 01          '..'    SET FIRST PASS
        STAA    ROLPAS                   *F029: B7 E4 23       '..#'
        CLR     UPROG                    *F02C: 7F E4 3B       '..;'   INIT FLAGS
        CLR     ROIFLG                   *F02F: 7F E4 38       '..8'
        CLR     KEYFLG                   *F032: 7F E4 1C       '...'
        CLR     FNCFL                    *F035: 7F E4 3E       '..>'
        LDAA    #$40                     *F038: 86 40          '.@'    DISPLAY PROMPT
        STAA    DISBUF                   *F03A: B7 E4 1D       '...'
        LDAA    #$1F                     *F03D: 86 1F          '..'
        JSR     CLRDS                    *F03F: BD F1 95       '...'
        LDX     #FUNSEL                  *F042: CE F0 E5       '...'   EXECUTE FUNCTION SELECT
        STX     MNPTR                    *F045: FF E4 19       '...'
        JSR     ENNMI                    *F048: BD F7 AE       '...'   ENABLE NMI
        JMP     PUT                      *F04B: 7E F0 BB       '~..'   & GO

* -------------------------------------------------------
* GET - ROUTINE TO READ A KEY
* -------------------------------------------------------
GET     LDX     #PIADP                   *F04E: CE E4 84       '...'   POINT AT PIA
        LDAA    #$FF                     *F051: 86 FF          '..'
        STAA    ,X                       *F053: A7 00          '..'    TO TURN OFF DISPLAYS
        LDAA    #$3F                     *F055: 86 3F          '.?'    COL 0, ALL ROWS
LPCOL   STAA    $02,X                    *F057: A7 02          '..'    STORE INFO TO KEY MATRIX
        TST     ,X                       *F059: 6D 00          'm.'    MSB IS MUX BIT
        BPL     COLFND                   *F05B: 2A 06          '*.'    BIT-7 LOW MEANS COL FOUND
        ADDA    #$40                     *F05D: 8B 40          '.@'    INC COL BITS TO MUX
        BCC     LPCOL                    *F05F: 24 F6          '$.'
        BRA     GET                      *F061: 20 EB          ' .'    KEY BOUNCED , START OVER
COLFND  ANDA    #$C0                     *F063: 84 C0          '..'    MASK TO SAVE ONLY COL
        STAA    KEY                      *F065: B7 E4 1B       '...'   WILL UPDATE LATER; JUST TEMP SAV
        LDAB    #$20                     *F068: C6 20          '. '    ROW5
LPROW   TBA                              *F06A: 17             '.'     COPY ROW INFO ROW A-REG
        ORAA    KEY                      *F06B: BA E4 1B       '...'   COMBINE WITH COL INFO
        STAA    $02,X                    *F06E: A7 02          '..'    DRIVE KEY MATRIX
        TST     ,X                       *F070: 6D 00          'm.'    MSB LOW = CLOSURE
        BPL     ROWFND                   *F072: 2A 05          '*.'
        LSRB                             *F074: 54             'T'     NEXT LOWER ROW BIT
        BNE     LPROW                    *F075: 26 F3          '&.'    LOOP TILL ALL ROWS TRIED
        BRA     GET                      *F077: 20 D5          ' .'    KEY BOUNCED; START OVER
ROWFND  CLRA                             *F079: 4F             'O'     PREPARE TO FIND BINARY ROW #
LPFND   LSRB                             *F07A: 54             'T'     LOOP BUILDS BINARY ROW #
        BCS     DUNROW                   *F07B: 25 03          '%.'    WHEN BIT FALLS OFF; A-REG HAS #
        INCA                             *F07D: 4C             'L'
        BRA     LPFND                    *F07E: 20 FA          ' .'
DUNROW  ROL     KEY                      *F080: 79 E4 1B       'y..'
        ROLA                             *F083: 49             'I'
        ROL     KEY                      *F084: 79 E4 1B       'y..'
        ROLA                             *F087: 49             'I'     A-REG IS 000RRRCC
* A-REG NOW CONTAINS OFFSET FOR KEY LOOK-UP
CLOP    TST     ,X                       *F088: 6D 00          'm.'    SEE IF KEY STILL DOWN
        BPL     CLOP                     *F08A: 2A FC          '*.'    WAIT TILL LET UP
        JSR     DLY25                    *F08C: BD F1 69       '..i'   DELAY TO DEBOUNCE
        LDX     #KYTBL                   *F08F: CE F0 A3       '...'   POINT AT TOP OF TABLE
        JSR     ADDAX                    *F092: BD F1 83       '...'
        LDAA    ,X                       *F095: A6 00          '..'    GET KEY CODE
        STAA    KEY                      *F097: B7 E4 1B       '...'   SAVE KEY VALUE
        LDAB    #$01                     *F09A: C6 01          '..'
        STAB    KEYFLG                   *F09C: F7 E4 1C       '...'   INDICATE KEY PENDING
        LDAB    PIADPB                   *F09F: F6 E4 86       '...'   TO CLEAR NMI
DIDDLE  RTS                              *F0A2: 39             '9'
* THIS RTS IS USED AS A DO-NOTHING SUB
* SO SYST CAN BE DISABLED EXCEPT DISPLAY

* -------------------------------------------------------
* KYTBL - KEY VALUE TABLE
* -------------------------------------------------------
KYTBL   FCB     $00,$0F,$0E,$0D,$01,$02  *F0A3: 00 0F 0E 0D 01 02 '......'
        FCB     $03,$0C,$04,$05,$06,$0B  *F0A9: 03 0C 04 05 06 0B '......'
        FCB     $07,$08,$09,$0A          *F0AF: 07 08 09 0A    '....'
        FCB     $84                      *F0B3: 84             '.'     'FS' FUNCTION SET
        FCB     $85                      *F0B4: 85             '.'     'FC' FUNCTION CLEAR
        FCB     $86                      *F0B5: 86             '.'     'P/L' PUNCH/LOAD
        FCB     $87                      *F0B6: 87             '.'     'T/B' TRACE/BREAK
        FCB     $80                      *F0B7: 80             '.'     'MD' MEMORY DISPLAY
        FCB     $81                      *F0B8: 81             '.'     'EX' ESCAPE
        FCB     $82                      *F0B9: 82             '.'     'RD' REGISTER DISPLAY
        FCB     $83                      *F0BA: 83             '.'     'GO' GO

* -------------------------------------------------------
* PUT - DISPLAYS DATA IN DISBUF & CALLS THE
* FUNCTIONING SUBROUTINE
* -------------------------------------------------------
PUT     LDAB    #$20                     *F0BB: C6 20          '. '    INIT DIG ENABLE PATTERN
LP1P    LDX     #DISBUF-3                *F0BD: CE E4 1A       '...'   POINT AT DISPLAY BUFFER
        TBA                              *F0C0: 17             '.'     MAKE EXTRA COPY
LP2P    INX                              *F0C1: 08             '.'     POINT AT NXT DIGIT INFO
        ASLA                             *F0C2: 48             'H'     ADD 1 TO 'X' FOR EACH SHIFT
        BCC     LP2P                     *F0C3: 24 FC          '$.'    LOOP DEVELOPS DIGIT INFO ADDR
        LDAA    ,X                       *F0C5: A6 00          '..'    GET SEG INFO
        COMA                             *F0C7: 43             'C'     ANODE DRIVERS ARE GND TRUE
        STAA    PIADP                    *F0C8: B7 E4 84       '...'   STORE ANODE INFO TO PIA
        STAB    PIADPB                   *F0CB: F7 E4 86       '...'   ENABLE DIGIT CATHODE
        JSR     DLY1                     *F0CE: BD F1 71       '..q'   ON FOR 1 MILLISECOND
        LDAA    #$FF                     *F0D1: 86 FF          '..'    1'S TURN OFF SEGS
        STAA    PIADP                    *F0D3: B7 E4 84       '...'   TURN OFF ALL SEGS
        STAA    PIADPB                   *F0D6: B7 E4 86       '...'   ENABLE ALL KPD ROWS
        PSHB                             *F0D9: 37             '7'     HAS ROTATING DIGIT ENABLE
        LDX     MNPTR                    *F0DA: FE E4 19       '...'   GET ADDRESS OF ACTIVE MAIN PROG
        JSR     ,X                       *F0DD: AD 00          '..'    EXECUTE IT
* SEE MANUAL
        PULB                             *F0DF: 33             '3'     RECOVER DIGIT ENABLE
        LSRB                             *F0E0: 54             'T'     NEXT DIGIT
        BNE     LP1P                     *F0E1: 26 DA          '&.'    NOT THRU WHOLE CYCLE
        BRA     PUT                      *F0E3: 20 D6          ' .'    PAST LAST DIGIT

* -------------------------------------------------------
* FUNSEL - ROUTINE TO SELECT A FUNCTION FROM A KEY INPUT
* -------------------------------------------------------
FUNSEL  TST     KEYFLG                   *F0E5: 7D E4 1C       '}..'   KEY PENDING?
        BNE     KEYNOW                   *F0E8: 26 01          '&.'
        RTS                              *F0EA: 39             '9'     -- RETURN -- NO KEY PENDING
KEYNOW  JSR     RDKEY                    *F0EB: BD F1 EF       '...'   GET & ACKNOWLEDGE KEY
        BMI     FUNKY                    *F0EE: 2B 15          '+.'    IF FUNCTION KEY
        TST     FNCFL                    *F0F0: 7D E4 3E       '}.>'
        BNE     UFNK                     *F0F3: 26 0B          '&.'
        JSR     ROLL4                    *F0F5: BD F1 CC       '...'   # ENTRY SO ROLL IT IN
        JSR     DYSCOD                   *F0F8: BD F1 20       '.. '   CONVERT TO 7-SEG
        LDAA    #$03                     *F0FB: 86 03          '..'
        JMP     CLRDS                    *F0FD: 7E F1 95       '~..'   BLANK LAST 2 DIGITS
UFNK    LDX     FNCPNT                   *F100: FE E4 3F       '..?'   POINT AT USER FUNCTION TBL
        BRA     HASH                     *F103: 20 03          ' .'
FUNKY   LDX     #SYSFNC                  *F105: CE F1 10       '...'   POINT AT SYSTEM FUNCTION TBL
HASH    ASLA                             *F108: 48             'H'     2 BYTES PER ENTRY
        JSR     ADDAX                    *F109: BD F1 83       '...'   DEVELOP POINTER
        LDX     ,X                       *F10C: EE 00          '..'    GET JMP ADDR
        JMP     ,X                       *F10E: 6E 00          'n.'    GO THERE
SYSFNC  FDB     MEMBEG                   *F110: F1 F6          '..'    'MD'
        FDB     PROMPT                   *F112: F0 24          '.$'    'EX'
        FDB     REGBEG                   *F114: F2 CA          '..'    'RD'
        FDB     GO                       *F116: F6 F3          '..'    'GO'
        FDB     FSET                     *F118: F4 C5          '..'
        FDB     FCLR                     *F11A: F4 D1          '..'    'FC'
        FDB     TAPBEG                   *F11C: F4 D7          '..'    'P/L'
        FDB     BRKBEG                   *F11E: F3 88          '..'    'T/B'

* -------------------------------------------------------
* MISC - MISC ROUTINES
* -------------------------------------------------------
* DECODE HEX TO 7-SEGMENT
DYSCOD  PSHA                             *F120: 36             '6'     SAVE REGS
        PSHB                             *F121: 37             '7'
        STX     XTMP1                    *F122: FF E4 26       '..&'
        LDX     #HEXBUF                  *F125: CE E4 2C       '..,'   POINT AT HEX INFO
LPOl    LDAA    ,X                       *F128: A6 00          '..'    GET HEX BYTE
        TAB                              *F12A: 16             '.'     MAKE EXTRA COPY
        LSRB                             *F12B: 54             'T'     RIGHT JUSTIFY HIGH NIBBLE
        LSRB                             *F12C: 54             'T'
        LSRB                             *F12D: 54             'T'
        LSRB                             *F12E: 54             'T'     HIGH ORDER DIGIT IN B-REG
        ANDA    #$0F                     *F12F: 84 0F          '..'    LOW ORDER DIGIT IN A-REG
        PSHB                             *F131: 37             '7'     SAVE ON STACK
        PSHA                             *F132: 36             '6'
        INX                              *F133: 08             '.'     NEXT HEX BYTE
        CPX     #USP                     *F134: 8C E4 2F       '../'   DONE ?
        BNE     LPOl                     *F137: 26 EF          '&.'    LOOP 3 TIMES
        LDX     #DISBUF+5                *F139: CE E4 22       '.."'   LAST DISPLAY BUFFER DIGIT
        LDAB    #$05                     *F13C: C6 05          '..'    LOOP INDEX
LPO2    STX     ME428                    *F13E: FF E4 28       '..('   SAVE TEMPORARILY
        LDX     #DYSTBL                  *F141: CE F1 59       '..Y'   POINT AT LOOK-UP TABLE
        PULA                             *F144: 32             '2'     GET A HEX DIGIT TO CONVERT
        JSR     ADDAX                    *F145: BD F1 83       '...'   POINT AT 7-SEG EQUIV
        LDAA    ,X                       *F148: A6 00          '..'    GET IT
        LDX     ME428                    *F14A: FE E4 28       '..('   RECOVER POINTER TO DISP BUFFER
        STAA    ,X                       *F14D: A7 00          '..'    STORE CONVERTED DIG
        DEX                              *F14F: 09             '.'     NEXT DISPLAY POS
        DECB                             *F150: 5A             'Z'     LOOP INDEX
        BPL     LPO2                     *F151: 2A EB          '*.'    CONTINUE FOR 6 DIGITS
        LDX     XTMP1                    *F153: FE E4 26       '..&'   RECOVER ENTRY STATUS
        PULB                             *F156: 33             '3'
        PULA                             *F157: 32             '2'
        RTS                              *F158: 39             '9'     -- RETURN --
*  
*  
DYSTBL  FCB     %00111111                *F159: 3F             '?'     '0'
        FCB     %00000110                *F15A: 06             '.'     '1'
        FCB     %01011011                *F15B: 5B             '['     '2'
        FCB     %01001111                *F15C: 4F             'O'     '3'
        FCB     %01100110                *F15D: 66             'f'     '4'
        FCB     %01101101                *F15E: 6D             'm'     '5'
        FCB     %01111101                *F15F: 7D             '}'     '6'
        FCB     %00000111                *F160: 07             '.'     '7'
        FCB     %01111111                *F161: 7F             '.'     '8'
        FCB     %01100111                *F162: 67             'g'     '9'
        FCB     %01110111                *F163: 77             'w'     'A'
        FCB     %01111100                *F164: 7C             '|'     'B'
        FCB     %00111001                *F165: 39             '9'     'C'
        FCB     %01011110                *F166: 5E             '^'     'D'
        FCB     %01111001                *F167: 79             'y'     'E'
        FCB     %01110001                *F168: 71             'q'     'F'
*  
* DELAY SUBS
DLY25   STX     XSAVD                    *F169: FF E4 24       '..$'   SAVE X ENTRY VALUE
        LDX     #$0AEA                   *F16C: CE 0A EA       '...'   25 MS ENTRY POINT
        BRA     DLYLP                    *F16F: 20 0B          ' .'
DLY1    STX     XSAVD                    *F171: FF E4 24       '..$'   SAVE ENTRY VAL
        LDX     #$006D                   *F174: CE 00 6D       '..m'   1 MS COUNT
        BRA     DLYLP                    *F177: 20 03          ' .'
DLYX    STX     XSAVD                    *F179: FF E4 24       '..$'   REQUIRED FOR SIMILARITY TO DLYl/25
DLYLP   DEX                              *F17C: 09             '.'
        BNE     DLYLP                    *F17D: 26 FD          '&.'    LOOP TILL X=0
        LDX     XSAVD                    *F17F: FE E4 24       '..$'   RECOVER ENTRY VALUE
        RTS                              *F182: 39             '9'     -- RETURN --
*  
* SUBROUTINE TO ADD X=X+A
ADDAX   STX     XSAVD                    *F183: FF E4 24       '..$'   TO ALLOW CALCS
        ADDA    XSAVD+1                  *F186: BB E4 25       '..%'   ADD LOW BYTES
        STAA    XSAVD+1                  *F189: B7 E4 25       '..%'   UPDATE
        BCC     ARND                     *F18C: 24 03          '$.'    IF NO CARRY; YOU'RE DONE
        INC     XSAVD                    *F18E: 7C E4 24       '|.$'   ADD CARRY TO HIGH BYTE
ARND    LDX     XSAVD                    *F191: FE E4 24       '..$'   RESULT TO X-REG
*  
* CLEAR DISPLAY PER A-REG
        RTS                              *F194: 39             '9'     -- RETURN --
CLRDS   STX     XTMP1                    *F195: FF E4 26       '..&'   SAVE ENTRY VALUE
        LDX     #DISBUF+5                *F198: CE E4 22       '.."'   RIGHTMOST DIGIT
CLRLP   LSRA                             *F19B: 44             'D'
        BCC     ARNCLR                   *F19C: 24 02          '$.'    IF BIT IN A-REG NOT SET
        CLR     ,X                       *F19E: 6F 00          'o.'
ARNCLR  DEX                              *F1A0: 09             '.'
        CPX     #KEYFLG                  *F1A1: 8C E4 1C       '...'
        BNE     CLRLP                    *F1A4: 26 F5          '&.'    CONTINUE 6 TIMES
        LDX     XTMP1                    *F1A6: FE E4 26       '..&'   RECOVER ENTRY VALUE
        RTS                              *F1A9: 39             '9'     -- RETURN --
                                         *  
ROLL2   STX     XTMP1                    *F1AA: FF E4 26       '..&'   SAVE ENTRY VALUE
        LDX     HEXBUF                   *F1AD: FE E4 2C       '..,'   ADDR TO ROLL
        TST     ROLPAS                   *F1B0: 7D E4 23       '}.#'   FIRST PASS ?
        BEQ     ARNCL2                   *F1B3: 27 07          ''.'
        CLR     ROLPAS                   *F1B5: 7F E4 23       '..#'   THIS WAS PASS 1
        CLR     ,X                       *F1B8: 6F 00          'o.'    CLEAR LOC ON FIRST PASS
        BRA     R2OUT                    *F1BA: 20 08          ' .'
ARNCL2  ASL     ,X                       *F1BC: 68 00          'h.'
        ASL     ,X                       *F1BE: 68 00          'h.'
        ASL     ,X                       *F1C0: 68 00          'h.'
        ASL     ,X                       *F1C2: 68 00          'h.'    SHIFT ROLL BYTE 4 PLACES
R2OUT   ORAA    ,X                       *F1C4: AA 00          '..'    COMBINE NEW DATA
        STAA    ,X                       *F1C6: A7 00          '..'    UPDATE LOC
        LDX     XTMP1                    *F1C8: FE E4 26       '..&'   RECOVER ENTRY VAL
        RTS                              *F1CB: 39             '9'     -- RETURN --
*  
* ROLL 4 HEX INTO HEXBUF
ROLL4   PSHB                             *F1CC: 37             '7'     SAVE ENTRY VALUES
        TST     ROLPAS                   *F1CD: 7D E4 23       '}.#'   PASS 1 ?
        BEQ     ARNCL4                   *F1D0: 27 0B          ''.'    NO,CONTINUE
                                         * LEFT JUSTIFY NEW DIGIT
        CLR     ROLPAS                   *F1D2: 7F E4 23       '..#'   YES,CLEAR FIRST PASS FLAG &
        CLR     HEXBUF                   *F1D5: 7F E4 2C       '..,'   CLR FIRST 4 DIGITS
        STAA    HEXBUF+1                 *F1D8: B7 E4 2D       '..-'   THEN PUT NEW DATA IN 4TH
        BRA     R4OUT                    *F1DB: 20 10          ' .'
ARNCL4  ASLA                             *F1DD: 48             'H'
        ASLA                             *F1DE: 48             'H'
        ASLA                             *F1DF: 48             'H'
        ASLA                             *F1E0: 48             'H'
        LDAB    #$03                     *F1E1: C6 03          '..'    LOOP INDEX
RO4LP   ROLA                             *F1E3: 49             'I'     ROLLA INTO HEXBUF
        ROL     HEXBUF+1                 *F1E4: 79 E4 2D       'y.-'
        ROL     HEXBUF                   *F1E7: 79 E4 2C       'y.,'
        DECB                             *F1EA: 5A             'Z'
        BPL     RO4LP                    *F1EB: 2A F6          '*.'
R4OUT   PULB                             *F1ED: 33             '3'     RECOVER B-REG
        RTS                              *F1EE: 39             '9'     RETURN

RDKEY   CLR     KEYFLG                   *F1EF: 7F E4 1C       '...'   READ & ACKNOWLEDGE KEY
        LDAA    KEY                      *F1F2: B6 E4 1B       '...'
        RTS                              *F1F5: 39             '9'

* -------------------------------------------------------
* MEMCH - MEMORY CHANGE/DISPLAY/OFFSET ROUTINE
* -------------------------------------------------------
MEMBEG  LDX     #MEMCH                   *F1F6: CE F2 05       '...'
        STX     MNPTR                    *F1F9: FF E4 19       '...'   INT MAIN POINTER
        CLR     FNCFL                    *F1FC: 7F E4 3E       '..>'   SET FUNCTION FLAG TO ZERO
        LDX     HEXBUF                   *F1FF: FE E4 2C       '..,'   POINT AT ADDR TO DISPLAY
        JMP     NEWMEM                   *F202: 7E F2 BA       '~..'   EXIT TO UPDATE DISPLAY
MEMCH   TST     KEYFLG                   *F205: 7D E4 1C       '}..'   SEE IF ANY KEY PENDING
        BNE     MEMNOW                   *F208: 26 01          '&.'
        RTS                              *F20A: 39             '9'     -- RETURN --
MEMNOW  JSR     RDKEY                    *F20B: BD F1 EF       '...'   GET & ACKNOWLEDGE KEY
        LDX     HEXBUF                   *F20E: FE E4 2C       '..,'   SAVES STEPS LATER
        LDAB    FNCFL                    *F211: F6 E4 3E       '..>'   SEE IF IN OFFSET MODE
        BEQ     NORMAL                   *F214: 27 77          ''w'    (NOT IN OFFSET MODE)
        BMI     CALDUN                   *F216: 2B 53          '+S'    IF OFFSET CALC FINISHED
        TSTA                             *F218: 4D             'M'     CHECK KEY
        BMI     OFFUN                    *F219: 2B 0D          '+.'    IF FUNCTION KEY
        JSR     ROLL4                    *F21B: BD F1 CC       '...'   ENTER NUMBER KEY
OFFOUT  JSR     DYSCOD                   *F21E: BD F1 20       '.. '   CONVERT TO 7-SEG
OFFEND  LDX     #$0077                   *F221: CE 00 77       '..w'   "A"
        STX     DISBUF+4                 *F224: FF E4 21       '..!'   STORE TO LAST DIGITS
OFFRET  RTS                              *F227: 39             '9'     -- RETURN --
OFFUN   CMPA    #$83                     *F228: 81 83          '..'    'GO' ?
        BNE     OFFRET                   *F22A: 26 FB          '&.'    IF NOT; EXIT
        LDX     HEXBUF                   *F22C: FE E4 2C       '..,'   GET DESTINATION OF BRANCH
        DEX                              *F22F: 09             '.'     ADJ INSTEAD OF ADJ'ING THE SOURCE
        STX     HEXBUF                   *F230: FF E4 2C       '..,'   UPDATE
        LDAA    HEXBUF+1                 *F233: B6 E4 2D       '..-'   LOW BYTE OF DESTINATION
        LDAB    HEXBUF                   *F236: F6 E4 2C       '..,'   HI BYTE
        SUBA    MEMSAV+1                 *F239: B0 E4 2B       '..+'   SUBTRACT LOW BYTES
        SBCB    MEMSAV                   *F23C: F2 E4 2A       '..*'   SUBTRACT W/ CARRY
        TSTA                             *F23F: 4D             'M'     CHECK POLARITY OF LOW ORDER RESULT
        BPL     ARNINC                   *F240: 2A 01          '*.'    IF LO POS DON'T INC HI
        INCB                             *F242: 5C             '\'     IF LOW WAS NEG INC HI  $FF - $00
ARNINC  TSTB                             *F243: 5D             ']'     IF B NOW ZERO; OFFSET IS IN RANGE
        BNE     BADOFF                   *F244: 26 11          '&.'    IF NOT; TOO FAR
        STAA    HEXBUF+2                 *F246: B7 E4 2E       '...'   SAVE RESULT
        JSR     DYSCOD                   *F249: BD F1 20       '.. '   CONVERT TO 7-SEG
        LDAA    #%00111100               *F24C: 86 3C          '.<'    CLEAR FIRST 4 DISPLAYS
        JSR     CLRDS                    *F24E: BD F1 95       '...'
        LDAA    #$80                     *F251: 86 80          '..'
        STAA    FNCFL                    *F253: B7 E4 3E       '..>'   INDICATE CALC DONE; & OK
        RTS                              *F256: 39             '9'     -- RETURN --
BADOFF  LDX     #$BAD0                   *F257: CE BA D0       '...'
        STX     HEXBUF                   *F25A: FF E4 2C       '..,'
        JSR     DYSCOD                   *F25D: BD F1 20       '.. '   WRITE "BAD" IN FIRST 3 DISPLAYS
        LDAA    #%00000111               *F260: 86 07          '..'
        JSR     CLRDS                    *F262: BD F1 95       '...'   CLEAR UNUSED DIGITS
        LDAA    #$FF                     *F265: 86 FF          '..'
        STAA    FNCFL                    *F267: B7 E4 3E       '..>'   INDICATE OFFSET NOT VALID
        RTS                              *F26A: 39             '9'     -- RETURN --
CALDUN  INCB                             *F26B: 5C             '\'     IF IT WAS $FF IT'S NOW 0
        BEQ     BADCAL                   *F26C: 27 13          ''.'    OFFSET WAS BAD
        LDX     MEMSAV                   *F26E: FE E4 2A       '..*'   RECOVER MEM ADDR
        CMPA    #$85                     *F271: 81 85          '..'    FUNCTION CLEAR KEY ?
        BEQ     MEMBAK                   *F273: 27 13          ''.'    YES,DONT SAVE OFFSET
        CMPA    #$83                     *F275: 81 83          '..'    'GO' ?
        BNE     OFFRET                   *F277: 26 AE          '&.'    'GO' IS ONLY VALID KEY HERE
        LDAA    HEXBUF+2                 *F279: B6 E4 2E       '...'   GET CALC'D OFFSET
        STAA    ,X                       *F27C: A7 00          '..'    STORE TO MEM
        INX                              *F27E: 08             '.'     ADV TO NEXT MEM ADDR
        BRA     MEMBAK                   *F27F: 20 07          ' .'    BACK TO MEM CHANGE
BADCAL  CMPA    #$80                     *F281: 81 80          '..'    'MD' ?
        BNE     OFFRET                   *F283: 26 A2          '&.'    'MD' IS THE ONLY VALID KEY HERE
        LDX     MEMSAV                   *F285: FE E4 2A       '..*'   RECOVER MEM ADDRESS
MEMBAK  CLR     FNCFL                    *F288: 7F E4 3E       '..>'   SIGNAL NOT IN OFFSET MODE
        BRA     NEWMEM                   *F28B: 20 2D          ' -'    RE-ENTER MEM CHANGE
NORMAL  TSTA                             *F28D: 4D             'M'     SET COND CODES
        BPL     NUM                      *F28E: 2A 25          '*%'    IF NUMBER KEY
        CMPA    #$80                     *F290: 81 80          '..'    'MD' ?
        BNE     NXMl                     *F292: 26 03          '&.'    NO,CHECK FOR "GO"
        DEX                              *F294: 09             '.'     YES,BACK UP
        BRA     NEWMEM                   *F295: 20 23          ' #'
NXMl    CMPA    #$83                     *F297: 81 83          '..'    'GO' ?
        BNE     NXM2                     *F299: 26 03          '&.'    NO,CHECK FOR "FS"
        INX                              *F29B: 08             '.'     YES,ADVANCE
        BRA     NEWMEM                   *F29C: 20 1C          ' .'
NXM2    CMPA    #$84                     *F29E: 81 84          '..'    'FS' ?
        BNE     MEMOUT                   *F2A0: 26 1D          '&.'    NO MORE VALID KEYS
        LDAA    #$3F                     *F2A2: 86 3F          '.?'
        JSR     CLRDS                    *F2A4: BD F1 95       '...'
        LDAA    #$01                     *F2A7: 86 01          '..'
        STAA    FNCFL                    *F2A9: B7 E4 3E       '..>'   SET OFFSET MODE
        STAA    ROLPAS                   *F2AC: B7 E4 23       '..#'   SET FIRST PASS
        STX     MEMSAV                   *F2AF: FF E4 2A       '..*'   SAVE MEM CHG POINTER
        JMP     OFFEND                   *F2B2: 7E F2 21       '~.!'
NUM     JSR     ROLL2                    *F2B5: BD F1 AA       '...'   ENTER NEW DIGIT
        BRA     MEMOUT                   *F2B8: 20 05          ' .'    DON'T SET FIRST PASS
NEWMEM  LDAA    #$01                     *F2BA: 86 01          '..'
        STAA    ROLPAS                   *F2BC: B7 E4 23       '..#'   SET FIRST PASS FLAG
MEMOUT  LDAA    ,X                       *F2BF: A6 00          '..'    GET DATA TO DISPLAY
        STAA    HEXBUF+2                 *F2C1: B7 E4 2E       '...'   UPDATE HEX BUFFER
        STX     HEXBUF                   *F2C4: FF E4 2C       '..,'   UPDATE ADDR
        JMP     DYSCOD                   *F2C7: 7E F1 20       '~. '   CONV TO 7-SEG

* -------------------------------------------------------
* REGDIS - REGISTER DISPLAY/CHANGE ROUTINE
* -------------------------------------------------------
REGBEG  TST     FNCFL                    *F2CA: 7D E4 3E       '}.>'   SEE IF IN VERIFY
        BEQ     NOTVRF                   *F2CD: 27 06          ''.'
        CLR     FNCFL                    *F2CF: 7F E4 3E       '..>'   SIGNAL VERIFY
        JMP     LDTAP                    *F2D2: 7E F4 DC       '~..'   GO VERIFY TAPE
NOTVRF  LDX     #REGDIS                  *F2D5: CE F2 EE       '...'
        STX     MNPTR                    *F2D8: FF E4 19       '...'   INIT MAIN POINTER
        LDX     #PUT                     *F2DB: CE F0 BB       '...'
        STX     STKTOP-1                 *F2DE: FF E4 7D       '..}'   WILL BE TO PUT
        LDS     #STKTOP-2                *F2E1: 8E E4 7C       '..|'   INIT STACKPOINTER
        CLR     REGNO                    *F2E4: 7F E4 41       '..A'   INIT REG # = UPC
        LDAA    #$01                     *F2E7: 86 01          '..'
        STAA    ROLPAS                   *F2E9: B7 E4 23       '..#'   INDICATE FIRST PASS
        BRA     REGOUT                   *F2EC: 20 40          ' @'    TO UPDATE DISPLAY
REGDIS  TST     KEYFLG                   *F2EE: 7D E4 1C       '}..'   SEE IF ANY KEY PENDING
        BNE     REGNOW                   *F2F1: 26 01          '&.'
        RTS                              *F2F3: 39             '9'     RETURN -- NO KEY
REGNOW  JSR     RDKEY                    *F2F4: BD F1 EF       '...'   GET & ACKNOWLEDGE KEY
        BMI     REGFNC                   *F2F7: 2B 05          '+.'    IF FUNCTION KEY
        JSR     ROLL4                    *F2F9: BD F1 CC       '...'
        BRA     REGOUT                   *F2FC: 20 30          ' 0'    UPDATE DISPLAY & EXIT
REGFNC  CMPA    #$80                     *F2FE: 81 80          '..'    'MD' ?
        BNE     NXRl                     *F300: 26 0D          '&.'
        LDAA    REGNO                    *F302: B6 E4 41       '..A'
        DECA                             *F305: 4A             'J'
        BPL     ARNRl                    *F306: 2A 02          '*.'
        LDAA    #$05                     *F308: 86 05          '..'    WRAP AROUND
ARNRl   STAA    REGNO                    *F30A: B7 E4 41       '..A'   UPDATE
        BRA     NEWREG                   *F30D: 20 10          ' .'    SET UP NEW REG ON EXIT
NXRl    CMPA    #$83                     *F30F: 81 83          '..'    'GO'
        BNE     RUNONE                   *F311: 26 11          '&.'    IGNORE INVALID ENTRY
        LDAA    REGNO                    *F313: B6 E4 41       '..A'
        INCA                             *F316: 4C             'L'
        CMPA    #$06                     *F317: 81 06          '..'    PAST ?
        BNE     ARNR2                    *F319: 26 01          '&.'
        CLRA                             *F31B: 4F             'O'     WRAP AROUND
ARNR2   STAA    REGNO                    *F31C: B7 E4 41       '..A'   UPDATE
NEWREG  LDAA    #$01                     *F31F: 86 01          '..'
        STAA    ROLPAS                   *F321: B7 E4 23       '..#'
RUNONE  CMPA    #$87                     *F324: 81 87          '..'    T/B KEY ?
        BNE     REGOUT                   *F326: 26 06          '&.'    NO,RETURN
        LDX     #REGBEG                  *F328: CE F2 CA       '...'   YES,SET UP RETURN ADDR
        JMP     ROI                      *F32B: 7E F7 01       '~..'
REGOUT  LDAA    REGNO                    *F32E: B6 E4 41       '..A'
        ASLA                             *F331: 48             'H'
        ASLA                             *F332: 48             'H'     4-BYTES PER BLOCK ENTRY
        LDX     #REGTBL                  *F333: CE F3 70       '..p'   TOP OF INFO TABLE
        JSR     ADDAX                    *F336: BD F1 83       '...'   POINT AT TABLE ENTRY
        LDAA    $03,X                    *F339: A6 03          '..'    GET 7-SEG INFO
        PSHA                             *F33B: 36             '6'     SAVE ON STACK
        LDAA    $02,X                    *F33C: A6 02          '..'
        PSHA                             *F33E: 36             '6'
        LDX     ,X                       *F33F: EE 00          '..'    GET ADDR OF DESIRED REG
        TST     ROLPAS                   *F341: 7D E4 23       '}.#'   SEE IF NEW REGF
        BEQ     NOTNEW                   *F344: 27 0A          ''.'
        LDAA    ,X                       *F346: A6 00          '..'    STORE CURRENT VAL TO DISPLAY STAA HEXBUF
        STAA    HEXBUF                   *F348: B7 E4 2C       '..,'
        LDAA    $01,X                    *F34B: A6 01          '..'
        STAA    HEXBUF+1                 *F34D: B7 E4 2D       '..-'
NOTNEW  JSR     DYSCOD                   *F350: BD F1 20       '.. '   TO CONVERT TO 7-SEG
        PULA                             *F353: 32             '2'     RECOVER DISPLAY CODES
        STAA    DISBUF+4                 *F354: B7 E4 21       '..!'   & STORE TO DISP BUFFER
        PULA                             *F357: 32             '2'
        STAA    DISBUF+5                 *F358: B7 E4 22       '.."'
        BPL     ARNR3                    *F35B: 2A 08          '*.'
        CLR     DISBUF                   *F35D: 7F E4 1D       '...'   CLEAR UNUSED DISPLAYS
        CLR     DISBUF+1                 *F360: 7F E4 1E       '...'
        BRA     ONLYl                    *F363: 20 05          ' .'
ARNR3   LDAA    HEXBUF                   *F365: B6 E4 2C       '..,'   UPDATE HIGH OF PSEUDO REG
        STAA    ,X                       *F368: A7 00          '..'
ONLYl   LDAA    HEXBUF+1                 *F36A: B6 E4 2D       '..-'
        STAA    $01,X                    *F36D: A7 01          '..'    UPDATE LOW BYTE
        RTS                              *F36F: 39             '9'     -- RETURN --
REGTBL  FDB     UPC                      *F370: E4 36          '.6'
        FCB     %01110011,%00111001      *F372: 73 39          's9'
        FDB     UB                       *F374: E4 32          '.2'
        FCB     %00000000,%11110111      *F376: 00 F7          '..'
        FDB     UCC                      *F378: E4 31          '.1'
        FCB     %00000000,%11111100      *F37A: 00 FC          '..'
        FDB     UX                       *F37C: E4 34          '.4'
        FCB     %00000110,%01011110      *F37E: 06 5E          '.^'
        FDB     USP                      *F380: E4 2F          './'
        FCB     %01101101,%01110011      *F382: 6D 73          'ms'
        FDB     UCC-1                    *F384: E4 30          '.0'
        FCB     %00111001,%10111001      *F386: 39 B9          '9.'

* -------------------------------------------------------
* BRKBEG - BREAKPOINT EDITOR
* -------------------------------------------------------
BRKBEG  TST     FNCFL                    *F388: 7D E4 3E       '}.>'   FUNCTION FLAG SET ?
        BNE     BRKEDT                   *F38B: 26 01          '&.'    YES,EDIT BREAKPIONTS
        RTS                              *F38D: 39             '9'     NO,TAKE NO ACTION
BRKEDT  LDX     #BRKPNT                  *F38E: CE F3 9C       '...'   SET MNPTR WITH BREAKPOINT ROUTINE
        STX     MNPTR                    *F391: FF E4 19       '...'
        LDAA    #$01                     *F394: 86 01          '..'    SET UP FOR ADDR INPUT
        STAA    ROLPAS                   *F396: B7 E4 23       '..#'
        JMP     DISBRK                   *F399: 7E F3 C0       '~..'   DISPLAY NEXT BKPT
BRKPNT  TST     KEYFLG                   *F39C: 7D E4 1C       '}..'   KEY PENDING ?
        BNE     BRKTST                   *F39F: 26 01          '&.'    YES,DECODE KEY ?
        RTS                              *F3A1: 39             '9'     NO,RETURN TO PUT
BRKTST  JSR     RDKEY                    *F3A2: BD F1 EF       '...'   GET & ACKNOWLEDGE KEY
        CMPA    #$0F                     *F3A5: 81 0F          '..'    HEX ?
        BHI     NOTHEX                   *F3A7: 22 06          '".'    NO,CHECK FOR FUNCTION
        JSR     ROLL4                    *F3A9: BD F1 CC       '...'   YES, ROLL INTO HEXBUF
        JMP     DYSCOD                   *F3AC: 7E F1 20       '~. '   DISPLAY & RETURN TO PUT
NOTHEX  CMPA    #$84                     *F3AF: 81 84          '..'    FS KEY ?
        BNE     CKFC                     *F3B1: 26 02          '&.'    NO,TRY FC
        BRA     BKTOTB                   *F3B3: 20 3C          ' <'    YES,ENTER AS BKPT & RETURN
CKFC    CMPA    #$85                     *F3B5: 81 85          '..'    FC KEY ?
        BNE     CKGO                     *F3B7: 26 03          '&.'    NO,CHECK FOR GO
        JMP     BKFMTB                   *F3B9: 7E F4 2E       '~..'   YES,REMOVE A BKPT
CKGO    CMPA    #$83                     *F3BC: 81 83          '..'    GO KEY ?
        BNE     DISDUN                   *F3BE: 26 30          '&0'    YES,DISPLAY NEXT BKPT & RETURN
*  
* DISBRK - DISPLAY NEXT BREAKPOINT
DISBRK  LDAA    BRKNO                    *F3C0: B6 E4 44       '..D'   GET # INTO HEXBU
        STAA    HEXBUF+2                 *F3C3: B7 E4 2E       '...'   ANY BREAKPOINTS ?
        BEQ     BACK                     *F3C6: 27 1B          ''.'    NO,RETURN
        LDX     BKPNTR                   *F3C8: FE E4 42       '..B'   YES,DISPLAY NEXT
BKLOOP  INX                              *F3CB: 08             '.'
        INX                              *F3CC: 08             '.'
        INX                              *F3CD: 08             '.'
        INX                              *F3CE: 08             '.'
        CPX     #BYTE                    *F3CF: 8C E4 59       '..Y'   END OF TAB
        BNE     NOTEND                   *F3D2: 26 03          '&.'    YES,WRAP AROUND
        LDX     #BRKTAB                  *F3D4: CE E4 45       '..E'   NO,GO TEST FOR B
NOTEND  TST     $03,X                    *F3D7: 6D 03          'm.'    BREAKPOINT ?
        BEQ     BKLOOP                   *F3D9: 27 F0          ''.'    NO,TRY NEXT LOC
        STX     BKPNTR                   *F3DB: FF E4 42       '..B'   YES,MOVE POINTER
        LDX     ,X                       *F3DE: EE 00          '..'    GET BKPT ADDR
        STX     HEXBUF                   *F3E0: FF E4 2C       '..,'   & DISPLAY IT
BACK    JSR     DYSCOD                   *F3E3: BD F1 20       '.. '
        TST     BRKNO                    *F3E6: 7D E4 44       '}.D'   ANY BREAKPOINTS ?
        BNE     DISDUN                   *F3E9: 26 05          '&.'    YES,RETURN
        LDAA    #$FE                     *F3EB: 86 FE          '..'    MASK ALL BUT LSD
        JSR     CLRDS                    *F3ED: BD F1 95       '...'
DISDUN  RTS                              *F3F0: 39             '9'     RETURN TO PUT

* BKTOTB-ENTER A BREAKPOINT FROM HEXBUF INTO
*        THE TABLE & UPDATE BRKNO
BKTOTB  JSR     FNDBRK                   *F3F1: BD F4 A0       '...'   BREAKPOINT EXIST ?
        BCS     FULL                     *F3F4: 25 32          '%2'    YES,RETURN
        BSR     BKNO                     *F3F6: 8D 46          '.F'    FIND OPEN SPACE
        LDAA    BRKNO                    *F3F8: B6 E4 44       '..D'   GET # OF BREAKPOINTS
        CMPA    #$05                     *F3FB: 81 05          '..'    FULL ?
        BGE     FULL                     *F3FD: 2C 29          ',)'    YES
* CHECK FOR RAM
        LDX     HEXBUF                   *F3FF: FE E4 2C       '..,'   TEST FOR RAM
        LDAA    ,X                       *F402: A6 00          '..'
        COMA                             *F404: 43             'C'
        COM     ,X                       *F405: 63 00          'c.'
        CMPA    ,X                       *F407: A1 00          '..'    RAM ?
        BNE     FULL                     *F409: 26 1D          '&.'    NO ,RETURN
        COMA                             *F40B: 43             'C'     YES, RESTORE DATA
        STAA    ,X                       *F40C: A7 00          '..'
* ENTER BKPT INTO TABLE
        LDX     BKPNTR                   *F40E: FE E4 42       '..B'   POINT INTO BREAKPOINT TAB
        STAA    $02,X                    *F411: A7 02          '..'    SAV OPCODE
        LDAA    HEXBUF                   *F413: B6 E4 2C       '..,'   GET OP CODE ADDR
        LDAB    HEXBUF+1                 *F416: F6 E4 2D       '..-'
        STAA    ,X                       *F419: A7 00          '..'    INSERT BREAKPOINT
        STAB    $01,X                    *F41B: E7 01          '..'
        INC     BRKNO                    *F41D: 7C E4 44       '|.D'   COUNT BREAKPOINT
        INC     $03,X                    *F420: 6C 03          'l.'
        INC     HEXBUF+2                 *F422: 7C E4 2E       '|..'   UPDATE BKPT NO.
        JSR     DYSCOD                   *F425: BD F1 20       '.. '
FULL    LDAA    #$01                     *F428: 86 01          '..'    RESET ROLPAS
        STAA    ROLPAS                   *F42A: B7 E4 23       '..#'
        RTS                              *F42D: 39             '9'     & RETURN

* BKFMTB - REMOVE A BREAKPOINT FROM BUFFER
*          & UPDATE BRKNO
BKFMTB  BSR     FNDBRK                   *F42E: 8D 70          '.p'    BKPT (DISBUF) IN TABLE ?
        BCC     DISBRK                   *F430: 24 8E          '$.'    NO , RETURN
        LDX     BKPNTR                   *F432: FE E4 42       '..B'   YES , GET ITS ADDR
        CLR     $03,X                    *F435: 6F 03          'o.'    & REMOVE IT.
        CLR     $02,X                    *F437: 6F 02          'o.'    REMOVE OP CODE
        DEC     BRKNO                    *F439: 7A E4 44       'z.D'   UPDATE COUNT
        BRA     DISBRK                   *F43C: 20 82          ' .'    DISPLAY BKPT & RETURN

* BKNO - FIND NUMBER OF BREAKPOINTS, UPDATE BRKNO
*          & PUT ADDR OF LAST OPEN SPACE INTO BKPNTR

BKNO    CLR     BRKNO                    *F43E: 7F E4 44       '..D'
        LDX     #BRKTAB                  *F441: CE E4 45       '..E'
BKLOP   TST     $03,X                    *F444: 6D 03          'm.'    BREAKPOINT HERE ?
        BEQ     NEXT1                    *F446: 27 05          ''.'    NO, TRY NEXT ENTRY
        INC     BRKNO                    *F448: 7C E4 44       '|.D'   YES,COUNT IT
        BRA     ISBKPT                   *F44B: 20 03          ' .'    SO DONT SAVE ADDR
NEXT1   STX     BKPNTR                   *F44D: FF E4 42       '..B'   & SAVE ADDR
ISBKPT  INX                              *F450: 08             '.'     POINT TO NEXT' ENTRY
        INX                              *F451: 08             '.'
        INX                              *F452: 08             '.'
        INX                              *F453: 08             '.'
        CPX     #BYTE                    *F454: 8C E4 59       '..Y'   DONE ?
        BNE     BKLOP                    *F457: 26 EB          '&.'    NO,CONTINUE
        LDAA    #$01                     *F459: 86 01          '..'    RESET ROLPAS
        STAA    ROLPAS                   *F45B: B7 E4 23       '..#'
        RTS                              *F45E: 39             '9'     YES

* -------------------------------------------------------
* INBKS - INSERT BREAKPOINTS FROM TABLE TO MEM
* -------------------------------------------------------
INBKS   TST     BRKNO                    *F45F: 7D E4 44       '}.D'   BREAKPOINTS ?
        BEQ     NOBPT                    *F462: 27 20          '' '    NO,RETURN
        LDX     #BRKTAB                  *F464: CE E4 45       '..E'   YES,INSTALL'EM
CKBPT   TST     $03,X                    *F467: 6D 03          'm.'    BREAKPOINT ?
        BEQ     NEXT2                    *F469: 27 10          ''.'    NO,TRY NEXT ENTRY
        STX     BKPNTR                   *F46B: FF E4 42       '..B'   SAVE X
        LDAA    #$3F                     *F46E: 86 3F          '.?'    SWI
        LDX     ,X                       *F470: EE 00          '..'    GET ADDR
        LDAB    ,X                       *F472: E6 00          '..'    GET OP CODE
        STAA    ,X                       *F474: A7 00          '..'    STORE SWI
        LDX     BKPNTR                   *F476: FE E4 42       '..B'   RESTORE X
        STAB    $02,X                    *F479: E7 02          '..'    SAVE OPCODE
NEXT2   INX                              *F47B: 08             '.'
        INX                              *F47C: 08             '.'
        INX                              *F47D: 08             '.'
        INX                              *F47E: 08             '.'
        CPX     #BYTE                    *F47F: 8C E4 59       '..Y'   DONE ?
        BNE     CKBPT                    *F482: 26 E3          '&.'    NO,CONTINUE
NOBPT   RTS                              *F484: 39             '9'
* -------------------------------------------------------
* OUTBKS - REMOVE BREAKPOINTS FROM MEM
* -------------------------------------------------------
OUTBKS  LDX     #BRKTAB                  *F485: CE E4 45       '..E'
REMOV1  LDAA    $02,X                    *F488: A6 02          '..'
        BEQ     NEXT3                    *F48A: 27 0A          ''.'
* REMOVE BREAKPOINT FROM RAM
        STX     BKPNTR                   *F48C: FF E4 42       '..B'
        LDX     ,X                       *F48F: EE 00          '..'
        STAA    ,X                       *F491: A7 00          '..'
        LDX     BKPNTR                   *F493: FE E4 42       '..B'
* NEXT ENTRY
NEXT3   INX                              *F496: 08             '.'
        INX                              *F497: 08             '.'
        INX                              *F498: 08             '.'
        INX                              *F499: 08             '.'
        CPX     #BYTE                    *F49A: 8C E4 59       '..Y'
        BNE     REMOV1                   *F49D: 26 E9          '&.'
        RTS                              *F49F: 39             '9'
*  
* FDBRK - FIND BREAKPOINT (HEXBUF) IN BRKTAB
*         BKPNTR POINTS AT BREAKPOINT & CARRY
*         IS SET IF BREKPOINT EXISTS,ELSE C IS ="0"
FNDBRK  LDAA    HEXBUF                   *F4A0: B6 E4 2C       '..,'   BREAKPOINT MSB
        LDAB    HEXBUF+1                 *F4A3: F6 E4 2D       '..-'   BREAKPOINT LSB
        LDX     #BRKTAB                  *F4A6: CE E4 45       '..E'   BREAKPOINT TAB
BRKLOP  CMPA    ,X                       *F4A9: A1 00          '..'    MATCH ?
        BEQ     CKLSB                    *F4AB: 27 0B          ''.'    YES
NEXT    INX                              *F4AD: 08             '.'     NO POINT TO NEXT .
        INX                              *F4AE: 08             '.'     DONE ?
        INX                              *F4AF: 08             '.'     NO,CONTINUE
        INX                              *F4B0: 08             '.'
        CPX     #BYTE                    *F4B1: 8C E4 59       '..Y'
        BNE     BRKLOP                   *F4B4: 26 F3          '&.'
        CLC                              *F4B6: 0C             '.'     YES,BUT NO BKPT
        RTS                              *F4B7: 39             '9'
CKLSB   CMPB    $01,X                    *F4B8: E1 01          '..'    MATCH ?
        BNE     NEXT                     *F4BA: 26 F1          '&.'    NO,TRY NEXT ENTRY
        TST     $03,X                    *F4BC: 6D 03          'm.'    BREAKPOINT ACTIVE ?
        BEQ     NEXT                     *F4BE: 27 ED          ''.'    NO,TRY AGAIN
        SEC                              *F4C0: 0D             '.'     YES,FOUND IT
        STX     BKPNTR                   *F4C1: FF E4 42       '..B'   SAVE ADDR
        RTS                              *F4C4: 39             '9'
* -------------------------------------------------------
* FSET - SET FUNCTION FLAG & DISPLAY "FS"
* -------------------------------------------------------
FSET    LDAA    #$01                     *F4C5: 86 01          '..'
        LDX     #$716D                   *F4C7: CE 71 6D       '.qm'
FOUT    STAA    FNCFL                    *F4CA: B7 E4 3E       '..>'
        STX     DISBUF+4                 *F4CD: FF E4 21       '..!'
        RTS                              *F4D0: 39             '9'
* -------------------------------------------------------
* FCLR - CLEAR FUNCTION FLAG & LAST 2 DIGITS
* -------------------------------------------------------
FCLR    CLRA                             *F4D1: 4F             'O'
        LDX     #$0000                   *F4D2: CE 00 00       '...'
        BRA     FOUT                     *F4D5: 20 F3          ' .'
* -------------------------------------------------------
* TAPES - SOFTWARE CASSETTE TAPE INTERFACE
* -------------------------------------------------------
TAPBEG  TST     FNCFL                    *F4D7: 7D E4 3E       '}.>'   SEE IF PUNCH OR LOAD
        BEQ     PCH                      *F4DA: 27 06          ''.'
LDTAP   JSR     LOAD                     *F4DC: BD F6 9C       '...'   DO LOAD (OR VERF)
        JMP     PROMPT                   *F4DF: 7E F0 24       '~.$'   WHEN DONE
PCH     LDX     #BEGEND                  *F4E2: CE F4 EC       '...'   POINT AT BEGEND ROUTINE
        STX     MNPTR                    *F4E5: FF E4 19       '...'   ACTIVATE
        LDAA    #$BB                     *F4E8: 86 BB          '..'
        BRA     CONOUT                   *F4EA: 20 1D          ' .'    DISPLAY BB IN LAST DISPLAYS
BEGEND  TST     KEYFLG                   *F4EC: 7D E4 1C       '}..'   SEE IF KEY PENDING
        BNE     ADNOW                    *F4EF: 26 01          '&.'
        RTS                              *F4F1: 39             '9'     -- RETURN -- NO KEY
ADNOW   JSR     RDKEY                    *F4F2: BD F1 EF       '...'   READ & ACKNOWLEDGE KEY
        BMI     FUNK                     *F4F5: 2B 05          '+.'    FUNCTION KEY
        JSR     ROLL4                    *F4F7: BD F1 CC       '...'   ENTER NEW NUMBER
        BRA     DYSOUT                   *F4FA: 20 16          ' .'    CONVERT TO 7-SEG & LEAVE
FUNK    LDAA    #$EE                     *F4FC: 86 EE          '..'
        CMPA    HEXBUF+2                 *F4FE: B1 E4 2E       '...'   END ADDR DONE ?
        BEQ     DOPCH                    *F501: 27 12          ''.'    GO DO PUNCH
        LDX     HEXBUF                   *F503: FE E4 2C       '..,'   SAVE ENTERED ADDR
        STX     BEGAD                    *F506: FF E4 60       '..`'
CONOUT  STAA    HEXBUF+2                 *F509: B7 E4 2E       '...'   'EE' OR 'BB' TO LAST DISPLAYS
        CLR     HEXBUF                   *F50C: 7F E4 2C       '..,'   CLEAR FIRST FOUR NIBBLES
        CLR     HEXBUF+1                 *F50F: 7F E4 2D       '..-'
DYSOUT  JMP     DYSCOD                   *F512: 7E F1 20       '~. '   CONV & RETURN
DOPCH   LDX     HEXBUF                   *F515: FE E4 2C       '..,'   SAVE ENTERED ADDR
        STX     ENDAD                    *F518: FF E4 62       '..b'
        JSR     PUNCH                    *F51B: BD F6 30       '..0'   PUNCH TAPE
        JMP     PROMPT                   *F51E: 7E F0 24       '~.$'   WHEN DONE
* -------------------------------------------------------
* FEDGE - ROUTINE TO LOCATE AN EDGE (POS OR NEG)
*         AND DETRMINE DISTANCE TO IT (TIME)
*         EXECUTION TIME TUNED
* -------------------------------------------------------
FEDGE   LDAA    #$05                     *F521: 86 05          '..'
        LDAB    PIADP                    *F523: F6 E4 84       '...'
        NOP                              *F526: 01             '.'
LOOPF   INCA                             *F527: 4C             'L'
        LDAB    PIACR                    *F528: F6 E4 85       '...'
        BPL     LOOPF                    *F52B: 2A FA          '*.'
        EORB    #$02                     *F52D: C8 02          '..'
        STAB    PIACR                    *F52F: F7 E4 85       '...'
        RTS                              *F532: 39             '9'
* -------------------------------------------------------
* TIN - READ 1 BYTE FROM TAPE
*       TIME TUNED
* -------------------------------------------------------
TIN     LDAA    #$FF                     *F533: 86 FF          '..'
        STAA    BYTE                     *F535: B7 E4 59       '..Y'
        CLR     CYCNT                    *F538: 7F E4 5A       '..Z'
        CLR     CYCNT+1                  *F53B: 7F E4 5B       '..['
        CLR     GOOD1S                   *F53E: 7F E4 5C       '..\'
        BSR     FEDGE                    *F541: 8D DE          '..'
MF543   TST     MF543                    *F543: 7D F5 43       '}.C'
NOTSH   TST     NOTSH                    *F546: 7D F5 46       '}.F'
        STAA    OLD                      *F549: B7 E4 5D       '..]'
        BSR     FEDGE                    *F54C: 8D D3          '..'
        CMPA    #$1B                     *F54E: 81 1B          '..'
        BGE     NOTSH                    *F550: 2C F4          ',.'
LOOPS   STAA    OLD                      *F552: B7 E4 5D       '..]'
        BSR     FEDGE                    *F555: 8D CA          '..'
        TAB                              *F557: 16             '.'
        ADDB    OLD                      *F558: FB E4 5D       '..]'
        CMPB    #$2B                     *F55B: C1 2B          '.+'
*  
* EDGE SENSE SET-UP TO SENSE TRAILING EDGE OF CYCLES
* & YOU ARE IN THE MIDDLE OF THE FIRST LONG CYCLE
        BLE     LOOPS                    *F55D: 2F F3          '/.'
        JMP     ZF562                    *F55F: 7E F5 62       '~.b'
ZF562   LDAB    PIADP                    *F562: F6 E4 84       '...'
        ADDA    #$05                     *F565: 8B 05          '..'
        BRA     SYNCIN                   *F567: 20 10          ' .'
LPOUT   LDAA    #$00                     *F569: 86 00          '..'
        BRA     LPMID                    *F56B: 20 00          ' .'
LPMID   CLR     CYCNT                    *F56D: 7F E4 5A       '..Z'
        STAA    CYCNT+1                  *F570: B7 E4 5B       '..['
        CLR     GOOD1S                   *F573: 7F E4 5C       '..\'
LPIN    LDAA    #$0A                     *F576: 86 0A          '..'
LOOP1   INCA                             *F578: 4C             'L'
SYNCIN  LDAB    PIACR                    *F579: F6 E4 85       '...'
        BPL     LOOP1                    *F57C: 2A FA          '*.'
        LDAB    PIADP                    *F57E: F6 E4 84       '...'
MF581   TST     MF581                    *F581: 7D F5 81       '}..'
        NOP                              *F584: 01             '.'
        CMPA    #$34                     *F585: 81 34          '.4'
        BLT     SHRT                     *F587: 2D 05          '-.'
        INC     GOOD1S                   *F589: 7C E4 5C       '|.\'
        BRA     WITHIN                   *F58C: 20 05          ' .'
SHRT    DEC     GOOD1S                   *F58E: 7A E4 5C       'z.\'
        BRA     WITHIN                   *F591: 20 00          ' .'
WITHIN  LDAB    CYCNT                    *F593: F6 E4 5A       '..Z'
        ADDA    CYCNT+1                  *F596: BB E4 5B       '..['
        STAA    CYCNT+1                  *F599: B7 E4 5B       '..['
        ADCB    #$00                     *F59C: C9 00          '..'
        STAB    CYCNT                    *F59E: F7 E4 5A       '..Z'
        BNE     CHKOVR                   *F5A1: 26 03          '&.'
        NOP                              *F5A3: 01             '.'
        BRA     NOTOVR                   *F5A4: 20 04          ' .'
CHKOVR  CMPA    #$17                     *F5A6: 81 17          '..'
        BGE     BITOVR                   *F5A8: 2C 0A          ',.'
NOTOVR  LDAB    #$05                     *F5AA: C6 05          '..'
ZF5AC   DECB                             *F5AC: 5A             'Z'
        BPL     ZF5AC                    *F5AD: 2A FD          '*.'
        JMP     ZF5B2                    *F5AF: 7E F5 B2       '~..'
ZF5B2   BRA     LPIN                     *F5B2: 20 C2          ' .'
*  
* END OF A BIT-TIME
BITOVR  ASL     GOOD1S                   *F5B4: 78 E4 5C       'x.\'
        ROR     BYTE                     *F5B7: 76 E4 59       'v.Y'
        BCC     TINDUN                   *F5BA: 24 08          '$.'
        CMPA    #$5D                     *F5BC: 81 5D          '.]'
        BLT     LPOUT                    *F5BE: 2D A9          '-.'
        LDAA    #$24                     *F5C0: 86 24          '.$'
        BRA     LPMID                    *F5C2: 20 A9          ' .'
*  
* DATA BYTE READ; CLEAN-UP AND LEAVE
TINDUN  LDAA    BYTE                     *F5C4: B6 E4 59       '..Y'
        ADDA    CHKSM                    *F5C7: BB E4 5E       '..^'
        STAA    CHKSM                    *F5CA: B7 E4 5E       '..^'
        LDAA    BYTE                     *F5CD: B6 E4 59       '..Y'
        RTS                              *F5D0: 39             '9'
* -------------------------------------------------------
* BIT1 - SEND A LOGIC 1 BIT-TIME
*        LESS 177 CLOCK CYCLES
*           TIME TUNED
* -------------------------------------------------------
BIT1    LDAB    #$0F                     *F5D1: C6 0F          '..'
LOOPB1  JSR     INVRT                    *F5D3: BD F5 FF       '...'
        LDAA    #$18                     *F5D6: 86 18          '..'
ZF5D8   DECA                             *F5D8: 4A             'J'
        BPL     ZF5D8                    *F5D9: 2A FD          '*.'
        BRA     ZF5DD                    *F5DB: 20 00          ' .'
ZF5DD   DECB                             *F5DD: 5A             'Z'
        BNE     LOOPB1                   *F5DE: 26 F3          '&.'
        JSR     INVRT                    *F5E0: BD F5 FF       '...'
        RTS                              *F5E3: 39             '9'
* -------------------------------------------------------
* BIT0 - SEND A LOGIC 0 BIT-TIME
*        LESS 177 CLOCK CYCLES
*           TIME TUNED
* -------------------------------------------------------
BIT0    LDAB    #$07                     *F5E4: C6 07          '..'
LOOPB0  JSR     INVRT                    *F5E6: BD F5 FF       '...'
        LDAA    #$38                     *F5E9: 86 38          '.8'
ZF5EB   DECA                             *F5EB: 4A             'J'
        BPL     ZF5EB                    *F5EC: 2A FD          '*.'
        NOP                              *F5EE: 01             '.'
        DECB                             *F5EF: 5A             'Z'
        BNE     LOOPB0                   *F5F0: 26 F4          '&.'
        JSR     INVRT                    *F5F2: BD F5 FF       '...'
        LDAA    #$1D                     *F5F5: 86 1D          '..'
ZF5F7   DECA                             *F5F7: 4A             'J'
        BPL     ZF5F7                    *F5F8: 2A FD          '*.'
        JMP     ZF5FD                    *F5FA: 7E F5 FD       '~..'
ZF5FD   NOP                              *F5FD: 01             '.'
        RTS                              *F5FE: 39             '9'
* -------------------------------------------------------
* INVRT - ROUTINE TO TRANSMIT A RISING
*         OR FALLING EDGE TO THE CASSETTE
*             TIME TUNED
* -------------------------------------------------------
INVRT   LDAA    #$80                     *F5FF: 86 80          '..'
        EORA    PIADPB                   *F601: B8 E4 86       '...'
        STAA    PIADPB                   *F604: B7 E4 86       '...'
        RTS                              *F607: 39             '9'
* -------------------------------------------------------
* PNCHB - PUNCH 1 BYTE TO TAPE. INCLUDES
*         START BIT,DATA,AND ALL BUT LAST HALF-CYCLE
*         OF STOP BITS
*                    TIME TUNED
* -------------------------------------------------------
PNCHB   STAA    BYTE                     *F608: B7 E4 59       '..Y'
        BSR     BIT0                     *F60B: 8D D7          '..'
        LDAA    #$09                     *F60D: 86 09          '..'
        STAA    NBITS                    *F60F: B7 E4 5F       '.._'
MF612   TST     MF612                    *F612: 7D F6 12       '}..'
LPPOUT  LDAA    #$13                     *F615: 86 13          '..'
ZF617   DECA                             *F617: 4A             'J'
        BPL     ZF617                    *F618: 2A FD          '*.'
        SEC                              *F61A: 0D             '.'
        ROR     BYTE                     *F61B: 76 E4 59       'v.Y'
        BCS     DO1                      *F61E: 25 05          '%.'
        BSR     BIT0                     *F620: 8D C2          '..'
        JMP     ENDBIT                   *F622: 7E F6 2A       '~.*'
DO1     BSR     BIT1                     *F625: 8D AA          '..'
        JMP     ENDBIT                   *F627: 7E F6 2A       '~.*'
ENDBIT  DEC     NBITS                    *F62A: 7A E4 5F       'z._'
        BPL     LPPOUT                   *F62D: 2A E6          '*.'
        RTS                              *F62F: 39             '9'
* -------------------------------------------------------
* PUNCH - FORMAT AND PUNCH A CASSETTE DATA FILE
*         INCLUDING LEADER AND CHECKSUM
*         EXECUTION TIME TUNED
* -------------------------------------------------------
PUNCH   LDX     #$0348                   *F630: CE 03 48       '..H'
LLOOP   LDAA    #$FF                     *F633: 86 FF          '..'
        LDAB    #$10                     *F635: C6 10          '..'
ZF637   DECB                             *F637: 5A             'Z'
        BPL     ZF637                    *F638: 2A FD          '*.'
        JSR     PNCHB                    *F63A: BD F6 08       '...'
        DEX                              *F63D: 09             '.'
        BNE     LLOOP                    *F63E: 26 F3          '&.'
        LDAA    #$53                     *F640: 86 53          '.S'
        LDAB    #$10                     *F642: C6 10          '..'
ZF644   DECB                             *F644: 5A             'Z'
        BPL     ZF644                    *F645: 2A FD          '*.'
        JSR     PNCHB                    *F647: BD F6 08       '...'
        NOP                              *F64A: 01             '.'
        CLR     CHKSM                    *F64B: 7F E4 5E       '..^'
        LDX     #BEGAD                   *F64E: CE E4 60       '..`'
ADLOOP  LDAA    ,X                       *F651: A6 00          '..'
        TAB                              *F653: 16             '.'
        ADDB    CHKSM                    *F654: FB E4 5E       '..^'
        STAB    CHKSM                    *F657: F7 E4 5E       '..^'
        NOP                              *F65A: 01             '.'
        LDAB    #$0D                     *F65B: C6 0D          '..'
ZF65D   DECB                             *F65D: 5A             'Z'
        BPL     ZF65D                    *F65E: 2A FD          '*.'
        JSR     PNCHB                    *F660: BD F6 08       '...'
        INX                              *F663: 08             '.'
        CPX     #BEGAD+4                 *F664: 8C E4 64       '..d'
        BNE     ADLOOP                   *F667: 26 E8          '&.'
        NOP                              *F669: 01             '.'
        NOP                              *F66A: 01             '.'
        LDX     BEGAD                    *F66B: FE E4 60       '..`'
DLOOP   LDAA    ,X                       *F66E: A6 00          '..'
        TAB                              *F670: 16             '.'
        ADDB    CHKSM                    *F671: FB E4 5E       '..^'
        STAB    CHKSM                    *F674: F7 E4 5E       '..^'
        STAB    CHKSM                    *F677: F7 E4 5E       '..^'
        LDAB    #$0B                     *F67A: C6 0B          '..'
ZF67C   DECB                             *F67C: 5A             'Z'
        BPL     ZF67C                    *F67D: 2A FD          '*.'
        JSR     PNCHB                    *F67F: BD F6 08       '...'
        JMP     ZF685                    *F682: 7E F6 85       '~..'
ZF685   CPX     ENDAD                    *F685: BC E4 62       '..b'
        BEQ     DUNDAT                   *F688: 27 03          ''.'
        INX                              *F68A: 08             '.'
        BRA     DLOOP                    *F68B: 20 E1          ' .'
DUNDAT  NEG     CHKSM                    *F68D: 70 E4 5E       'p.^'
        LDAA    CHKSM                    *F690: B6 E4 5E       '..^'
        LDAB    #$14                     *F693: C6 14          '..'
ZF695   DECB                             *F695: 5A             'Z'
        BPL     ZF695                    *F696: 2A FD          '*.'
        JSR     PNCHB                    *F698: BD F6 08       '...'
        RTS                              *F69B: 39             '9'
* -------------------------------------------------------
* LOAD - LOAD OR VERIFY A DATA FILE FROM
*        CASSETTE TAPE
* -------------------------------------------------------
LOAD    JSR     TIN                      *F69C: BD F5 33       '..3'
        CMPA    #$53                     *F69F: 81 53          '.S'
        BNE     LOAD                     *F6A1: 26 F9          '&.'
        LDX     #BEGAD                   *F6A3: CE E4 60       '..`'
        CLR     CHKSM                    *F6A6: 7F E4 5E       '..^'
LOPAD   JSR     TIN                      *F6A9: BD F5 33       '..3'
        STAA    ,X                       *F6AC: A7 00          '..'
        INX                              *F6AE: 08             '.'
        CPX     #BEGAD+4                 *F6AF: 8C E4 64       '..d'
        BNE     LOPAD                    *F6B2: 26 F5          '&.'
        LDX     BEGAD                    *F6B4: FE E4 60       '..`'
LOPDAT  JSR     TIN                      *F6B7: BD F5 33       '..3'
        TST     FNCFL                    *F6BA: 7D E4 3E       '}.>'
        BEQ     VERF                     *F6BD: 27 04          ''.'
        STAA    ,X                       *F6BF: A7 00          '..'
        BRA     LOPBOT                   *F6C1: 20 04          ' .'
VERF    CMPA    ,X                       *F6C3: A1 00          '..'
        BNE     BAD                      *F6C5: 26 11          '&.'
LOPBOT  CPX     ENDAD                    *F6C7: BC E4 62       '..b'
        BEQ     CHKCHK                   *F6CA: 27 03          ''.'
        INX                              *F6CC: 08             '.'
        BRA     LOPDAT                   *F6CD: 20 E8          ' .'
CHKCHK  JSR     TIN                      *F6CF: BD F5 33       '..3'
        TST     CHKSM                    *F6D2: 7D E4 5E       '}.^'
        BNE     BAD                      *F6D5: 26 01          '&.'
        RTS                              *F6D7: 39             '9'
BAD     STX     UX                       *F6D8: FF E4 34       '..4'
        STAA    UA                       *F6DB: B7 E4 33       '..3'
        TST     FNCFL                    *F6DE: 7D E4 3E       '}.>'
        BPL     STOP                     *F6E1: 2A 01          '*.'
        RTS                              *F6E3: 39             '9'
STOP    LDX     #$7177                   *F6E4: CE 71 77       '.qw'
        STX     DISBUF                   *F6E7: FF E4 1D       '...'
        LDX     #$0638                   *F6EA: CE 06 38       '..8'
        STX     DISBUF+2                 *F6ED: FF E4 1F       '...'
        JMP     ALTBAD                   *F6F0: 7E F7 35       '~.5'
* -------------------------------------------------------
* GO - GO TO USER PROGRAM
* -------------------------------------------------------
GO      TST     ROLPAS                   *F6F3: 7D E4 23       '}.#'
        BNE     CONTIN                   *F6F6: 26 06          '&.'
        LDX     HEXBUF                   *F6F8: FE E4 2C       '..,'
        STX     UPC                      *F6FB: FF E4 36       '..6'
CONTIN  LDX     #GO1                     *F6FE: CE F7 0B       '...'
ROI     STX     ROIBAK                   *F701: FF E4 39       '..9'
        LDAA    #$01                     *F704: 86 01          '..'
        STAA    ROIFLG                   *F706: B7 E4 38       '..8'
        BRA     GOTO                     *F709: 20 03          ' .'
GO1     JSR     INBKS                    *F70B: BD F4 5F       '.._'
GOTO    LDS     USP                      *F70E: BE E4 2F       '../'
        LDAA    #$55                     *F711: 86 55          '.U'
        PSHA                             *F713: 36             '6'
        PULA                             *F714: 32             '2'
        CMPA    #$55                     *F715: 81 55          '.U'
        BNE     BADSTK                   *F717: 26 10          '&.'
        LDAA    UPC+1                    *F719: B6 E4 37       '..7'
        PSHA                             *F71C: 36             '6'
        LDAA    UPC                      *F71D: B6 E4 36       '..6'
        PSHA                             *F720: 36             '6'
        LDAA    #$AA                     *F721: 86 AA          '..'
        PSHA                             *F723: 36             '6'
        PULA                             *F724: 32             '2'
        CMPA    #$AA                     *F725: 81 AA          '..'
        BEQ     GOEXIT                   *F727: 27 1E          ''.'
BADSTK  FCB     $CE                      *F729: CE             '.'
        NEGA                             *F72A: 40             '@'
        TST     $FF,X                    *F72B: 6D FF          'm.'
        ANDB    $1D,X                    *F72D: E4 1D          '..'
        LDX     #$7340                   *F72F: CE 73 40       '.s@'
        STX     DISBUF+2                 *F732: FF E4 1F       '...'
ALTBAD  LDX     #$5353                   *F735: CE 53 53       '.SS'
        STX     DISBUF+4                 *F738: FF E4 21       '..!'
        LDS     #STKTOP                  *F73B: 8E E4 7E       '..~'
        LDX     #DIDDLE                  *F73E: CE F0 A2       '...'
        STX     MNPTR                    *F741: FF E4 19       '...'
        JMP     PUT                      *F744: 7E F0 BB       '~..'
GOEXIT  LDX     UX                       *F747: FE E4 34       '..4'
        LDAB    UB                       *F74A: F6 E4 32       '..2'
        LDAA    UA                       *F74D: B6 E4 33       '..3'
        PSHA                             *F750: 36             '6'
        LDAA    #$01                     *F751: 86 01          '..'
        STAA    UPROG                    *F753: B7 E4 3B       '..;'
        TST     ROIFLG                   *F756: 7D E4 38       '}.8'
        BEQ     ABSOUT                   *F759: 27 12          ''.'
        LDAA    #$3C                     *F75B: 86 3C          '.<'
        STAA    PIACR                    *F75D: B7 E4 85       '...'
        LDAA    PIADPB                   *F760: B6 E4 86       '...'
        LDAA    #$0E                     *F763: 86 0E          '..'
        STAA    PIACRB                   *F765: B7 E4 87       '...'
        LDAA    #$34                     *F768: 86 34          '.4'
        STAA    PIACR                    *F76A: B7 E4 85       '...'
ABSOUT  LDAA    UCC                      *F76D: B6 E4 31       '..1'
        TAP                              *F770: 06             '.'
        PULA                             *F771: 32             '2'
        RTS                              *F772: 39             '9'
* -------------------------------------------------------
* INTERRUPTS - INTERRUPT HANDLING ROUTINES
* -------------------------------------------------------
NMINT   NOP                              *F773: 01             '.'
        SEI                              *F774: 0F             '.'
        LDAA    #$04                     *F775: 86 04          '..'
        STAA    PIACRB                   *F777: B7 E4 87       '...'
        LDAA    PIACRB                   *F77A: B6 E4 87       '...'
        BPL     SAVE                     *F77D: 2A 12          '*.'
        JSR     GET                      *F77F: BD F0 4E       '..N'
        CMPA    #$81                     *F782: 81 81          '..'
        BEQ     ABORT                    *F784: 27 03          ''.'
        BSR     ENNMI                    *F786: 8D 26          '.&'
        RTI                              *F788: 3B             ';'
ABORT   TST     UPROG                    *F789: 7D E4 3B       '}.;'
        BNE     SAVE                     *F78C: 26 03          '&.'
        JMP     PROMPT                   *F78E: 7E F0 24       '~.$'
SAVE    STS     USP                      *F791: BF E4 2F       '../'
        LDS     #STKTOP                  *F794: 8E E4 7E       '..~'
        BSR     SVSTAT                   *F797: 8D 23          '.#'
        BSR     ENNMI                    *F799: 8D 13          '..'
        CLR     UPROG                    *F79B: 7F E4 3B       '..;'
        TST     ROIFLG                   *F79E: 7D E4 38       '}.8'
        BEQ     NOTROI                   *F7A1: 27 08          ''.'
        CLR     ROIFLG                   *F7A3: 7F E4 38       '..8'
        LDX     ROIBAK                   *F7A6: FE E4 39       '..9'
        JMP     ,X                       *F7A9: 6E 00          'n.'
NOTROI  JMP     REGBEG                   *F7AB: 7E F2 CA       '~..'
ENNMI   LDAA    PIADPB                   *F7AE: B6 E4 86       '...'
        LDAA    #$07                     *F7B1: 86 07          '..'
        STAA    PIACRB                   *F7B3: B7 E4 87       '...'
        LDAA    #$FF                     *F7B6: 86 FF          '..'
        STAA    PIADPB                   *F7B8: B7 E4 86       '...'
        RTS                              *F7BB: 39             '9'
SVSTAT  LDS     USP                      *F7BC: BE E4 2F       '../'
        LDX     #UCC                     *F7BF: CE E4 31       '..1'
SVLOOP  PULA                             *F7C2: 32             '2'
        STAA    ,X                       *F7C3: A7 00          '..'
        INX                              *F7C5: 08             '.'
        CPX     #ROIFLG                  *F7C6: 8C E4 38       '..8'
        BNE     SVLOOP                   *F7C9: 26 F7          '&.'
        STS     USP                      *F7CB: BF E4 2F       '../'
        LDS     #STKTOP-2                *F7CE: 8E E4 7C       '..|'
        RTS                              *F7D1: 39             '9'
SWINT   NOP                              *F7D2: 01             '.'
        SEI                              *F7D3: 0F             '.'
        STS     USP                      *F7D4: BF E4 2F       '../'
        LDS     #STKTOP                  *F7D7: 8E E4 7E       '..~'
        BSR     SVSTAT                   *F7DA: 8D E0          '..'
        LDX     UPC                      *F7DC: FE E4 36       '..6'
        DEX                              *F7DF: 09             '.'
        STX     UPC                      *F7E0: FF E4 36       '..6'
        JSR     OUTBKS                   *F7E3: BD F4 85       '...'
        CLR     UPROG                    *F7E6: 7F E4 3B       '..;'
        JMP     REGBEG                   *F7E9: 7E F2 CA       '~..'
UIRQ    LDX     UIRQV                    *F7EC: FE E4 3C       '..<'
        JMP     ,X                       *F7EF: 6E 00          'n.'
        FCB     $00,$00,$00,$00,$00,$00  *F7F1: 00 00 00 00 00 00 '......'
        FCB     $00                      *F7F7: 00             '.'
        FDB     UIRQ,SWINT,NMINT,RESET   *F7F8: F7 EC F7 D2 F7 73 F0 00 '.....s..'

        END
