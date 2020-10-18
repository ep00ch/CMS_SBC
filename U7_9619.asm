* f9dasm: M6800/1/2/3/8/9 / H6309 Binary/OS9/FLEX9 Disassembler V1.76
* Loaded binary file U7_9619.bin

*    6809 INTERACTIVE DEBUG PROGRAM
* FOR CREATIVE MICRO SYSTEMS 9609/19 MODULE
* (C) 198? MICROWARE SYSTEMS CORPORATION
*          DES MOINES, IOWA


*****************************************************
** Used Labels                                      *
*****************************************************

TMINIT  EQU     $000C
ACEC_ON EQU     $00FF
ID1401  EQU     $1401
ID1700  EQU     $1700
ID1805  EQU     $1805
M2324   EQU     $2324
M293D   EQU     $293D
M2A2A   EQU     $2A2A
M4101   EQU     $4101
M4202   EQU     $4202
M44FF   EQU     $44FF
M5043   EQU     $5043
RAM_PAT EQU     $8008
M9D01   EQU     $9D01
M9D17   EQU     $9D17
M9D1E   EQU     $9D1E
M9D1F   EQU     $9D1F
M9D22   EQU     $9D22
M9D23   EQU     $9D23
P_BUFFLOC EQU     $9D82
P_START EQU     $9D84
P_END   EQU     $9D86
P_RAMSTART EQU     $9D88
P_ROMOFFS EQU     $9D8A
M9D8F   EQU     $9D8F
M9D90   EQU     $9D90
M9D92   EQU     $9D92
P_THIS  EQU     $9D93
P_DEVICE EQU     $9D96
P_TABLOC EQU     $9D97
P_DEVSZ EQU     $9D99
P_DEVEND EQU     $9D9B
M9D9C   EQU     $9D9C
P_HEAD  EQU     $9D9D
USERSP  EQU     $9F00
DBUGSP  EQU     $9F02
IOBUF   EQU     $9F04
BKPTBL  EQU     $9F06
INBKPT  EQU     $9F08
ACECHO  EQU     $9F09
USECRLF EQU     $9F0A
ACVECT  EQU     $9F0B
RAM_SW3_VEC EQU     $9F0D
RAM_SW2_VEC EQU     $9F0F
RAM_SWI_VEC EQU     $9F11
RAM_IRQ_VEC EQU     $9F13
RAM_FRQ_VEC EQU     $9F15
RAM_NMI_VEC EQU     $9F17
M9F19   EQU     $9F19
M9F1A   EQU     $9F1A
M9F1E   EQU     $9F1E
M9F20   EQU     $9F20
BYTCNT  EQU     $9F22
M9F23   EQU     $9F23
M9F27   EQU     $9F27
M9F28   EQU     $9F28
M9F29   EQU     $9F29
M9F2A   EQU     $9F2A
M9F2B   EQU     $9F2B
M9F2C   EQU     $9F2C
M9F2D   EQU     $9F2D
M9F2E   EQU     $9F2E
M9F53   EQU     $9F53
M9F55   EQU     $9F55
M9F58   EQU     $9F58
M9F59   EQU     $9F59
M9F5B   EQU     $9F5B
M9F6E   EQU     $9F6E
M9F6F   EQU     $9F6F
M9F79   EQU     $9F79
M9F87   EQU     $9F87
XTAG    EQU     $AA55

*****************************************************
** Program Code / Data Areas                        *
*****************************************************

        ORG     $E700

* ROM LABEL:
*       Debug 2418
*       FFFE=A0
*       11/6/99

* ORGANIZATION:
*   E000-E6FF    EMPTY
        ORG     $E000
        FILL    $FF,$700
*   E700-E7FF    RTC DATE/TIME
*   E800?EFFF    EPROM PROGRAMMER
*   F000?F7FF    ASSEMBLER
*   F800-FF7F    DEBUGGER
*   FF80-FFDF    I/O SPACE
*   FFE0-FFFF    VECTORS

* ### RTC CLOCK UTILITIES
* CB2 is EN' for decoder U33
* U is pointing to PIADDR0
* RTC DATA STORED IN A
* RTC ADDR DECrementeD AT B
_SETCLK JSR     CHKEOL                   *E700: BD FE 2A       '..*'
        BEQ     _DTCMDERR                *E703: 27 53          ''S'
        LDU     #PIADDR0                 *E705: CE FF C4       '...'   GET PIA0 ADDR
        LDB     #12                      *E708: C6 0C          '..'    START AT RTC DECADE ADDR
_SETCL1 JSR     _CHEXBIN                 *E70A: BD FE E0       '...'   CHECK VALID INPUT
        SUBA    #$30                     *E70D: 80 30          '.0'    thought _XCMDCHK did this already
        CMPB    #%00000101               *E70F: C1 05          '..'    ON 10 HR DIGIT REG...
        BNE     _SETCL2                  *E711: 26 02          '&.'
        ORA     #$08                     *E713: 8A 08          '..'    FORCE 24 HR CLOCK
_SETCL2 BSR     _RTC_WR                  *E715: 8D 06          '..'
        BCS     _SETCL2                  *E717: 25 FC          '%.'
        DECB                             *E719: 5A             'Z'     DECREMENT RTC ADDR
        BPL     _SETCL1                  *E71A: 2A EE          '*.'
        RTS                              *E71C: 39             '9'
* Set the RTC DATA from A Reg
_RTC_WR PSHS    D                        *E71D: 34 06          '4.'    SAVE A&B REGISTERS
        BSR     _RTCADDRWR               *E71F: 8D 17          '..'
        LDA     ,S                       *E721: A6 E4          '..'    GET A FROM A ON STACK
        ORA     #%00010000               *E723: 8A 10          '..'    SET PB4 HI (RTC-WT)
        STA     $02,U                    *E725: A7 42          '.B'    SAVE TO PIA0 PORB
        LDB     #%00110100               *E727: C6 34          '.4'    SELECT POR, SET CB2-LO
        STB     $03,U                    *E729: E7 43          '.C'    SAVE TO PIA0 CRB
        EXG     X,Y                      *E72B: 1E 12          '..'
        EXG     X,Y                      *E72D: 1E 12          '..'    16 CYCLE DELAY?
        LDB     #%00111100               *E72F: C6 3C          '.<'    SELECT POR, SET CB2-HI
        STB     $03,U                    *E731: E7 43          '.C'    SAVE TO PIA0 CRB
        LBSR    _RTC_SSR                 *E733: 17 00 A5       '...'
        PULS    PC,D                     *E736: 35 86          '5.'    RESTORE A&B, RTS
* Set the RTC ADDR from B Reg. PB4,5 stay low for RTC-ADDR write.
_RTCADDRWR PSHS    D                        *E738: 34 06          '4.'    SAVE A&B REGISTERS
        LDB     #%00111000               *E73A: C6 38          '.8'    SELECT DDR, SET CB2-HI
        STB     $03,U                    *E73C: E7 43          '.C'    SAVE TO PIA0 CRB
        LDA     #%00111111               *E73E: 86 3F          '.?'    PB7,6 in; PB5-0 out
        STA     $02,U                    *E740: A7 42          '.B'    SAVE TO PIA0 DDRB
        ORB     #%00000100               *E742: CA 04          '..'    SELECT POR
        STB     $03,U                    *E744: E7 43          '.C'    SAVE TO PIA0 CRB
        LDB     $01,S                    *E746: E6 61          '.a'    LOAD B FROM B ON STACK
        STB     $02,U                    *E748: E7 42          '.B'    SAVE TO PIA0 PORB
        LDB     #%00110100               *E74A: C6 34          '.4'    SELECT POR, SET CB2-LO
        STB     $03,U                    *E74C: E7 43          '.C'    SAVE TO PIA0 CRB
_DELAY3 LBRN    _DELAY3                  *E74E: 10 21 FF FC    '.!..'  3 CYCLE DELAY?
        LDB     #%00111100               *E752: C6 3C          '.<'    SELECT POR, SET CB2-HI
        STB     $03,U                    *E754: E7 43          '.C'    SAVE TO PIA0 CRB
        PULS    PC,D                     *E756: 35 86          '5.'    RESTORE A&D, RTS
_DTCMDERR LDA     #$32                     *E758: 86 32          '.2'
        JMP     _NCMDERR                 *E75A: 7E FD B2       '~..'
_PRCLK  LEAS    -$0D,S                   *E75D: 32 73          '2s'
        TFR     S,X                      *E75F: 1F 41          '.A'
        BSR     _RDCLK                   *E761: 8D 32          '.2'
        LDA     #'/                      *E763: 86 2F          './'
        PSHS    A                        *E765: 34 02          '4.'    PUSH '/' ON TO STACK
        BSR     _PRCL3                   *E767: 8D 13          '..'    PRINT DATE
        LDA     #$20                     *E769: 86 20          '. '
        JSR     A1OUTCHR                 *E76B: BD FE 79       '..y'   SPACE BTWN DT, TM
        LEAX    $01,X                    *E76E: 30 01          '0.'
        LDA     #':                      *E770: 86 3A          '.:'    USE ':' NOW
        STA     ,S                       *E772: A7 E4          '..'
        BSR     _PRCL3                   *E774: 8D 06          '..'    PRINT TIME
        LEAS    $0E,S                    *E776: 32 6E          '2n'
        LBSR    P_CRLF                   *E778: 17 04 DE       '...'
        RTS                              *E77B: 39             '9'
_PRCL3  LDB     #$03                     *E77C: C6 03          '..'    LOOP 3 TIMES
        BRA     _PRDDIG                  *E77E: 20 05          ' .'
_PRDIV  LDA     $02,S                    *E780: A6 62          '.b'
        JSR     A1OUTCHR                 *E782: BD FE 79       '..y'   PRINT / or : BTWN DIGITS
_PRDDIG BSR     _PR2X                    *E785: 8D 04          '..'
        DECB                             *E787: 5A             'Z'     DECREMENT LOOP COUNTER
        BNE     _PRDIV                   *E788: 26 F6          '&.'    KEEP OUTPUTTING
        RTS                              *E78A: 39             '9'
_PR2X   BSR     _PR1X                    *E78B: 8D 00          '..'
_PR1X   LDA     ,X+                      *E78D: A6 80          '..'    GET X, INC X
        ORA     #$30                     *E78F: 8A 30          '.0'    CONVERT TO ASCII #
        JSR     A1OUTCHR                 *E791: BD FE 79       '..y'   PRINT IT
        RTS                              *E794: 39             '9'     RETURN
_RDCLK  PSHS    X                        *E795: 34 10          '4.'
        LDU     #PIADDR0                 *E797: CE FF C4       '...'
_RDCL2  LDB     #12                      *E79A: C6 0C          '..'    START AT RTC DECADE ADDR
        LDX     ,S                       *E79C: AE E4          '..'
_RDCL3  BSR     _RTC_RD                  *E79E: 8D 0F          '..'    GET THE DIGIT
        BCS     _RDCL2                   *E7A0: 25 F8          '%.'
        CMPB    #$05                     *E7A2: C1 05          '..'    ON 10 HR DIGIT REG...
        BNE     _RDCL4                   *E7A4: 26 02          '&.'
        ANDA    #$07                     *E7A6: 84 07          '..'    JUST GET DIGIT
_RDCL4  STA     ,X+                      *E7A8: A7 80          '..'
        DECB                             *E7AA: 5A             'Z'     DECREMENT RTC ADDR
        BPL     _RDCL3                   *E7AB: 2A F1          '*.'    DO NEXT
        PULS    PC,X                     *E7AD: 35 90          '5.'    RESTORE X, RTS
_RTC_RD PSHS    D                        *E7AF: 34 06          '4.'    SAVE A&B REGISTERS
        LBSR    _RTCADDRWR               *E7B1: 17 FF 84       '...'
        LDB     #%00111000               *E7B4: C6 38          '.8'    SELECT DDR, SET CB2-HI
        STB     $03,U                    *E7B6: E7 43          '.C'    SAVE TO PIA0 CRB
        LDA     #%00110000               *E7B8: 86 30          '.0'    PB7,6 in; PB5,4 out PB3-0 in
        STA     $02,U                    *E7BA: A7 42          '.B'    SAVE TO PIA0 DDRB
        ORB     #%00000100               *E7BC: CA 04          '..'    SELECT POR
        STB     $03,U                    *E7BE: E7 43          '.C'    SAVE TO PIA0 CRB
        LDA     #%00100000               *E7C0: 86 20          '. '    SET PB5 HI (RTC-RD)
        STA     $02,U                    *E7C2: A7 42          '.B'    SAVE TO PIA0 PORB
        LDB     #%00110100               *E7C4: C6 34          '.4'    SELECT POR, SET CB2-LO (EN')
        STB     $03,U                    *E7C6: E7 43          '.C'    SAVE TO PIA0 CRB
        EXG     X,Y                      *E7C8: 1E 12          '..'
        EXG     X,Y                      *E7CA: 1E 12          '..'    16 CYCLE DELAY?
        LDA     $02,U                    *E7CC: A6 42          '.B'    LOAD PIA0 PORB
        ANDA    #$0F                     *E7CE: 84 0F          '..'    KEEP ONLY RTC DATA
        STA     ,S                       *E7D0: A7 E4          '..'    STORE VALUE IN A ON STACK
        LDB     #%00111100               *E7D2: C6 3C          '.<'    SELECT POR, SET CB2-HI
        STB     $03,U                    *E7D4: E7 43          '.C'    SAVE TO PIA0 CRB
        LBSR    _RTC_SSR                 *E7D6: 17 00 02       '...'
        PULS    PC,D                     *E7D9: 35 86          '5.'    RESTORE A&B, RTS
_RTC_SSR PSHS    D                        *E7DB: 34 06          '4.'
        LDB     #$0F                     *E7DD: C6 0F          '..'    RTC ADDR FOR STANDARD SIGNAL REG
        LBSR    _RTCADDRWR               *E7DF: 17 FF 56       '..V'
        LDB     #%00111000               *E7E2: C6 38          '.8'    SELECT DDR, SET CB2-HI
        STB     $03,U                    *E7E4: E7 43          '.C'    SAVE TO PIA0 CRB
        LDA     #%00110000               *E7E6: 86 30          '.0'    PB7,6 in; PB5,4 out PB3-0 in
        STA     $02,U                    *E7E8: A7 42          '.B'    SAVE TO PIA0 DDRB
        ORB     #%00000100               *E7EA: CA 04          '..'    SELECT POR
        STB     $03,U                    *E7EC: E7 43          '.C'    SAVE TO PIA0 CRB
        LDA     #%00100000               *E7EE: 86 20          '. '    SET PB5 HI (RTC-RD)
        STA     $02,U                    *E7F0: A7 42          '.B'    SAVE TO PIA0 PORB
        LDB     #%00110100               *E7F2: C6 34          '.4'    SELECT POR, SET CB2-LO (EN')
        STB     $03,U                    *E7F4: E7 43          '.C'    SAVE TO PIA0 CRB
        LDA     $02,U                    *E7F6: A6 42          '.B'    STORE SSR DATA IN A
        EORA    #%10000000               *E7F8: 88 80          '..'
        ASLA                             *E7FA: 48             'H'     SHIFT LEFT, SET CARRY?
        PULS    PC,D                     *E7FB: 35 86          '5.'    RESTORE A&D, RTS
        FCB     $FF,$FF,$FF              *E7FD: FF FF FF       '...'

* ### EPROM PROGRAMMER
* STAT, TEST, VERF, VIEW, CLR, READ, E, EXIT
* PARAMETERS: START  END  RAM-ADDR ROM-OFFSET
* 2716, 2516, 2732, 2532, 2732A, 2764, 27128

* PIA 1, PORT A - ADDR LINES (0-7)
* PIA 1, PORT B - ADDR LINES, HEAD ID
* PIA 2, PORT A - EPROM CONTROL LINES
* PIA 2, PORT B - DATA BUS
XTAGLOC FDB     XTAG                     *E800: AA 55          '.U'
XCMDTBL_VEC FDB     XCMDTBL                  *E802: EF DB          '..'
        LBRA    EPROM                    *E804: 16 00 5C       '..\'
P_EPRM3 LBRA    EPRM3                    *E807: 16 00 70       '..p'
P_DP    FCB     $9D,$00                  *E80A: 9D 00          '..'
* I/O VECTOR TABLE
PIA2PRA_VEC FCB     $FF,$CC                  *E80C: FF CC          '..'
PIA2CRA_VEC FCB     $FF,$CD                  *E80E: FF CD          '..'
PIA2PRB_VEC FCB     $FF,$CE                  *E810: FF CE          '..'
PIA2CRB_VEC FCB     $FF,$CF                  *E812: FF CF          '..'
PIA1PRA_VEC FCB     $FF,$C8                  *E814: FF C8          '..'
PIA1CRA_VEC FCB     $FF,$C9                  *E816: FF C9          '..'
PIA1PRB_VEC FCB     $FF,$CA                  *E818: FF CA          '..'
PIA1CRB_VEC FCB     $FF,$CB                  *E81A: FF CB          '..'
AC1DR_VEC FCB     $FF,$D4                  *E81C: FF D4          '..'
AC1SR_VEC FCB     $FF,$D5                  *E81E: FF D5          '..'
P_RX_RDY FCB     $08                      *E820: 08             '.'     MASK FOR RECEIVE  DATA REGISTER EMPTY BIT
P_TX_RDY FCB     $10                      *E821: 10             '.'     MASK FOR TRANSMIT DATA REGISTER FULL BIT

* MOS 6551 ACIA1 EPROM PROGRAMMER DATA TRANSFER SUBROUTINES
P_WAITBYTE BSR     P_GETBYTE                *E822: 8D 03          '..'
        BCS     P_WAITBYTE               *E824: 25 FC          '%.'
        RTS                              *E826: 39             '9'
P_GETBYTE PSHS    A                        *E827: 34 02          '4.'    SAVE OUTPUT BYTE TO STACK
        LDA     [>AC1SR_VEC,PCR]         *E829: A6 9D FF F1    '....'  GET ACIA STATUS
        BITA    >P_RX_RDY,PCR            *E82D: A5 8D FF EF    '....'  RX REG HAS A BYTE?
        BEQ     P_NOTRDY                 *E831: 27 0B          ''.'    IF NOT, ALERT CALLER
        PULS    A                        *E833: 35 02          '5.'    RESTORE OUTPUT BYTE FROM STACK
        LDA     [>AC1DR_VEC,PCR]         *E835: A6 9D FF E3    '....'  LOAD DATA BYTE FROM ACIA RECEIVE REG
        ANDA    #%01111111               *E839: 84 7F          '..'    MASK OUT HI BIT
        ANDCC   #$FE                     *E83B: 1C FE          '..'    CLEAR CARRY FLAG
        RTS                              *E83D: 39             '9'     RETURN WITH STATUS IN A, CARRY SET IS NOT READY
P_NOTRDY ORCC    #$01                     *E83E: 1A 01          '..'    SET CARRY FLAG
        PULS    PC,A                     *E840: 35 82          '5.'    RETURN
P_GETPUT PSHS    A                        *E842: 34 02          '4.'    SAVE OUTPUT BYTE TO STACK
        BSR     P_GETBYTE                *E844: 8D E1          '..'    CHECK IF NEXT BYTE IS AVAIL
        BCS     P_PUTBYTE                *E846: 25 0A          '%.'    IF NOT, SEND OUTPUT BYTE AND RETURN
        CMPA    #$1B                     *E848: 81 1B          '..'    RECEIVED ESC?
        BNE     P_PUTBYTE                *E84A: 26 06          '&.'    IF NOT, SEND OUTPUT BYTE AND RETURN
        BSR     P_WAITBYTE               *E84C: 8D D4          '..'    IF SO, WAIT FOR NEXT BYTE
        CMPA    #$1B                     *E84E: 81 1B          '..'    ANOTHER ESC?
        BEQ     P_EPRM3                  *E850: 27 B5          ''.'
P_PUTBYTE LDA     [>AC1SR_VEC,PCR]         *E852: A6 9D FF C8    '....'  GET ACIA STATUS
        BITA    >P_TX_RDY,PCR            *E856: A5 8D FF C7    '....'  TX REG EMPTY?
        BEQ     P_PUTBYTE                *E85A: 27 F6          ''.'    IF NOT, WAIT FOR EMPTY
        PULS    A                        *E85C: 35 02          '5.'    RESTORE OUTPUT BYTE FROM STACK
        STA     [>AC1DR_VEC,PCR]         *E85E: A7 9D FF BA    '....'  PUT DATA BYTE INTO ACIA TRANSMIT REG
        RTS                              *E862: 39             '9'

* EPROM PROGRAMMER ENTRY POINT
* FOR CMS 9614, 9617, 9618 HEADS
EPROM   CLRB                             *E863: 5F             '_'
        SETDP   $9D

        LDA     >P_DP,PCR                *E864: A6 8D FF A2    '....'
        TFR     A,DP                     *E868: 1F 8B          '..'    DIRECT PAGE FOR BUFFER
        TFR     D,X                      *E86A: 1F 01          '..'    DATA BUFFER DIRECT PAGE ADDRESS
        LDB     #$FF                     *E86C: C6 FF          '..'    SET LOOP COUNTER 255
EPRM2   CLR     ,X+                      *E86E: 6F 80          'o.'    CLEAR THE BUFFER
        DECB                             *E870: 5A             'Z'     DECREMENT COUNTER
        BNE     EPRM2                    *E871: 26 FB          '&.'    KEEP LOOPING IF >=0
        LBSR    P_INIT                   *E873: 17 05 E4       '...'
        LDA     #$FF                     *E876: 86 FF          '..'
        STA     P_DEVICE                 *E878: 97 96          '..'    RESET OFFSET STORE
EPRM3   LDD     >P_DP,PCR                *E87A: EC 8D FF 8C    '....'
        TFR     A,DP                     *E87E: 1F 8B          '..'    SET DIRECT PAGE TO BUFFER
        ADDD    #$00FF                   *E880: C3 00 FF       '...'
        TFR     D,S                      *E883: 1F 04          '..'    S=$9DFF = END OF BUFFER?
        LBSR    ZEF1B                    *E885: 17 06 93       '...'
        LBSR    P_CRLF                   *E888: 17 03 CE       '...'
        LDA     #$04                     *E88B: 86 04          '..'
        LBSR    P_PRMSG                  *E88D: 17 02 8C       '...'   PRINT PROMPT
        LBSR    P_ININIT                 *E890: 17 04 04       '...'
        BCS     EPRM3                    *E893: 25 E5          '%.'
        LBSR    P_CMDSRC                 *E895: 17 02 9E       '...'
        BCC     P_CMDRUN                 *E898: 24 07          '$.'
        LDA     #$05                     *E89A: 86 05          '..'
        LBSR    P_PRMSG                  *E89C: 17 02 7D       '..}'   PRINT "WHAT?"
        BRA     EPRM3                    *E89F: 20 D9          ' .'    BACK TO MAIN LOOP
P_CMDRUN LDD     ,X                       *E8A1: EC 84          '..'    JMP TO ADDR IN TABLE OFFSET BY ITS LOCATION
        JSR     D,X                      *E8A3: AD 8B          '..'    BASICALLY LBSR FROM TABLE LOCATION
        BRA     EPRM3                    *E8A5: 20 D3          ' .'    BACK TO MAIN LOOP WHEN DONE
P_CLR   LBSR    P_PARAM                  *E8A7: 17 03 21       '..!'
        LDD     P_END                    *E8AA: DC 86          '..'
        SUBD    P_START                  *E8AC: 93 84          '..'
        ADDD    #$0001                   *E8AE: C3 00 01       '...'
        TFR     D,Y                      *E8B1: 1F 02          '..'
        LDX     P_RAMSTART               *E8B3: 9E 88          '..'
        LDA     #$FF                     *E8B5: 86 FF          '..'
ZE8B7   STA     ,X+                      *E8B7: A7 80          '..'
        LEAY    -$01,Y                   *E8B9: 31 3F          '1?'
        BNE     ZE8B7                    *E8BB: 26 FA          '&.'
        RTS                              *E8BD: 39             '9'
P_VIEW  LBSR    P_PARAM                  *E8BE: 17 03 0A       '...'
        LDD     P_END                    *E8C1: DC 86          '..'
        SUBD    P_START                  *E8C3: 93 84          '..'
        LDX     P_START                  *E8C5: 9E 84          '..'
        LDY     P_RAMSTART               *E8C7: 10 9E 88       '...'
ZE8CA   PSHS    D                        *E8CA: 34 06          '4.'
        LBSR    P_CRLF                   *E8CC: 17 03 8A       '...'
        LBSR    P_X4HEX                  *E8CF: 17 02 95       '...'
        LDA     #$02                     *E8D2: 86 02          '..'    ..
        LBSR    P_SENDSPS                *E8D4: 17 02 C6       '...'   SEND 2 SPACES
        LDB     #$10                     *E8D7: C6 10          '..'
ZE8D9   LDA     ,Y+                      *E8D9: A6 A0          '..'
        LBSR    ZEB77                    *E8DB: 17 02 99       '...'
        LEAX    $01,X                    *E8DE: 30 01          '0.'
        DECB                             *E8E0: 5A             'Z'
        BNE     ZE8D9                    *E8E1: 26 F6          '&.'
        LEAY    -$10,Y                   *E8E3: 31 30          '10'
        LDA     #$02                     *E8E5: 86 02          '..'
        LBSR    P_SENDSPS                *E8E7: 17 02 B3       '...'   SEND 2 SPACES
        LDB     #$10                     *E8EA: C6 10          '..'
ZE8EC   LDA     ,Y+                      *E8EC: A6 A0          '..'
        ANDA    #$7F                     *E8EE: 84 7F          '..'
        CMPA    #$7F                     *E8F0: 81 7F          '..'
        BEQ     ZE8F8                    *E8F2: 27 04          ''.'
        CMPA    #$20                     *E8F4: 81 20          '. '
        BGE     ZE8FA                    *E8F6: 2C 02          ',.'
ZE8F8   LDA     #'.                      *E8F8: 86 2E          '..'
ZE8FA   LBSR    P_GETPUT                 *E8FA: 17 FF 45       '..E'
        DECB                             *E8FD: 5A             'Z'
        BNE     ZE8EC                    *E8FE: 26 EC          '&.'
        PULS    D                        *E900: 35 06          '5.'
        SUBD    #$0010                   *E902: 83 00 10       '...'
        BCC     ZE8CA                    *E905: 24 C3          '$.'
        LBSR    P_CRLF                   *E907: 17 03 4F       '..O'
        RTS                              *E90A: 39             '9'
P_VERF  LBSR    P_PARAM                  *E90B: 17 02 BD       '...'
        CLR     M9D92                    *E90E: 0F 92          '..'
        LDD     P_RAMSTART               *E910: DC 88          '..'
        STD     M9D90                    *E912: DD 90          '..'
ZE914   LBSR    P_CHKDEV                 *E914: 17 03 66       '..f'
        CLR     M9D8F                    *E917: 0F 8F          '..'
        LDD     P_START                  *E919: DC 84          '..'
        ADDD    P_ROMOFFS                *E91B: D3 8A          '..'
        ANDA    P_DEVEND                 *E91D: 94 9B          '..'
        ANDB    M9D9C                    *E91F: D4 9C          '..'
        TFR     D,Y                      *E921: 1F 02          '..'
        LDD     P_END                    *E923: DC 86          '..'
        SUBD    P_START                  *E925: 93 84          '..'
        FCB     $C3,$00,$01,$9E,$90      *E927: C3 00 01 9E 90 '.....'
ZE92C   PSHS    D                        *E92C: 34 06          '4.'
        LBSR    P_CHK_THIS1              *E92E: 17 05 92       '...'
        CMPA    ,X                       *E931: A1 84          '..'
        BEQ     ZE97D                    *E933: 27 48          ''H'
        TST     M9D8F                    *E935: 0D 8F          '..'
        BNE     ZE955                    *E937: 26 1C          '&.'
        PSHS    A                        *E939: 34 02          '4.'
        LBSR    P_CRLF                   *E93B: 17 03 1B       '...'
        LDA     #$07                     *E93E: 86 07          '..'
        LBSR    P_PRMSG                  *E940: 17 01 D9       '...'   PRINT "ADDR PR MM" x4
        LBSR    P_PRMSG                  *E943: 17 01 D6       '...'
        LBSR    P_PRMSG                  *E946: 17 01 D3       '...'
        LBSR    P_PRMSG                  *E949: 17 01 D0       '...'
        LDA     #$04                     *E94C: 86 04          '..'
        STA     M9D8F                    *E94E: 97 8F          '..'
        LBSR    P_CRLF                   *E950: 17 03 06       '...'
        PULS    A                        *E953: 35 02          '5.'
ZE955   TST     M9D92                    *E955: 0D 92          '..'
        BEQ     ZE962                    *E957: 27 09          ''.'
        EXG     X,Y                      *E959: 1E 12          '..'
        LBSR    P_X4HEX                  *E95B: 17 02 09       '...'
        EXG     X,Y                      *E95E: 1E 12          '..'
        BRA     ZE965                    *E960: 20 03          ' .'
ZE962   LBSR    ZEC8A                    *E962: 17 03 25       '..%'
ZE965   LBSR    ZEB77                    *E965: 17 02 0F       '...'
        LDA     ,X                       *E968: A6 84          '..'
        LBSR    ZEB77                    *E96A: 17 02 0A       '...'
        LDA     #$02                     *E96D: 86 02          '..'
        LBSR    P_SENDSPS                *E96F: 17 02 2B       '..+'   SEND 2 SPACES
        DEC     M9D8F                    *E972: 0A 8F          '..'
        BNE     ZE97D                    *E974: 26 07          '&.'
        LBSR    P_CRLF                   *E976: 17 02 E0       '...'
        LDA     #$04                     *E979: 86 04          '..'
        STA     M9D8F                    *E97B: 97 8F          '..'
ZE97D   TST     M9D92                    *E97D: 0D 92          '..'
        BNE     ZE983                    *E97F: 26 02          '&.'
        LEAX    $01,X                    *E981: 30 01          '0.'
ZE983   PULS    D                        *E983: 35 06          '5.'
        SUBD    #$0001                   *E985: 83 00 01       '...'
        BNE     ZE98B                    *E988: 26 01          '&.'
        RTS                              *E98A: 39             '9'
ZE98B   LEAY    $01,Y                    *E98B: 31 21          '1!'
        CMPY    P_DEVSZ                  *E98D: 10 9C 99       '...'
        BNE     ZE92C                    *E990: 26 9A          '&.'
        LDA     #$02                     *E992: 86 02          '..'
        LBSR    P_PRMSG                  *E994: 17 01 85       '...'   PRINT "EXCEEDED SIZE"
        RTS                              *E997: 39             '9'
P_TEST  LBSR    P_PARAM                  *E998: 17 02 30       '..0'
        LDA     #$01                     *E99B: 86 01          '..'
        STA     M9D92                    *E99D: 97 92          '..'
        LBSR    P_PARAM                  *E99F: 17 02 29       '..)'
        LEAX    MECF4,PCR                *E9A2: 30 8D 03 4E    '0..N'
        STX     M9D90                    *E9A6: 9F 90          '..'
        LBSR    ZE914                    *E9A8: 17 FF 69       '..i'
        RTS                              *E9AB: 39             '9'
P_STAT  LBSR    P_PARAM                  *E9AC: 17 02 1C       '...'
        LDA     P_DEVICE                 *E9AF: 96 96          '..'
        INCA                             *E9B1: 4C             'L'
        LEAX    MECE4,PCR                *E9B2: 30 8D 03 2E    '0...'
        LBSR    P_PRMS0                  *E9B6: 17 01 79       '..y'
        LBSR    P_CRLF                   *E9B9: 17 02 9D       '...'
        LDA     #$00                     *E9BC: 86 00          '..'
        LBSR    P_PRMSG                  *E9BE: 17 01 5B       '..['   PRINT "START  END"
        LBSR    P_CRLF                   *E9C1: 17 02 95       '...'
        LDX     P_START                  *E9C4: 9E 84          '..'
        LBSR    P_X4HEX                  *E9C6: 17 01 9E       '...'   SEND START
        LDA     #$02                     *E9C9: 86 02          '..'
        LBSR    P_SENDSPS                *E9CB: 17 01 CF       '...'   SEND 2 SPACES
        LDX     P_END                    *E9CE: 9E 86          '..'
        LBSR    P_X4HEX                  *E9D0: 17 01 94       '...'   SEND END
        LDA     #$02                     *E9D3: 86 02          '..'
        LBSR    P_SENDSPS                *E9D5: 17 01 C5       '...'   SEND 2 SPACES
        LDX     P_RAMSTART               *E9D8: 9E 88          '..'
        LBSR    P_X4HEX                  *E9DA: 17 01 8A       '...'   SEND RAM-ADDR
        LDA     #$05                     *E9DD: 86 05          '..'
        LBSR    P_SENDSPS                *E9DF: 17 01 BB       '...'   SEND 5 SPACES
        LDX     P_ROMOFFS                *E9E2: 9E 8A          '..'
        LBSR    P_X4HEX                  *E9E4: 17 01 80       '...'   SEND ROM-OFFSET
        LBSR    P_CRLF                   *E9E7: 17 02 6F       '..o'   SEND CRLF
        RTS                              *E9EA: 39             '9'
P_PRO   LBSR    P_PARAM                  *E9EB: 17 01 DD       '...'
        LBSR    P_CHKDEV                 *E9EE: 17 02 8C       '...'
        LDD     P_START                  *E9F1: DC 84          '..'
        ANDA    P_DEVEND                 *E9F3: 94 9B          '..'
        ANDB    M9D9C                    *E9F5: D4 9C          '..'
        ADDD    P_ROMOFFS                *E9F7: D3 8A          '..'
        TFR     D,Y                      *E9F9: 1F 02          '..'
        LDX     P_RAMSTART               *E9FB: 9E 88          '..'
        LDD     P_END                    *E9FD: DC 86          '..'
        SUBD    P_START                  *E9FF: 93 84          '..'
        ADDD    #$0001                   *EA01: C3 00 01       '...'
        PSHS    Y,X,D                    *EA04: 34 36          '46'
P_PROG1 PSHS    D                        *EA06: 34 06          '4.'
        LBSR    P_CHK_THIS1              *EA08: 17 04 B8       '...'
        CMPA    #$FF                     *EA0B: 81 FF          '..'
        BNE     P_PROG2                  *EA0D: 26 0B          '&.'
        LEAY    $01,Y                    *EA0F: 31 21          '1!'
        PULS    D                        *EA11: 35 06          '5.'
        SUBD    #$0001                   *EA13: 83 00 01       '...'
        BNE     P_PROG1                  *EA16: 26 EE          '&.'
        BRA     ZEA2E                    *EA18: 20 14          ' .'
P_PROG2 PULS    D                        *EA1A: 35 06          '5.'
        LDA     #$08                     *EA1C: 86 08          '..'    
        LBSR    P_PRMSG                  *EA1E: 17 00 FB       '...'   PRINT "CONTINUE?"
        LBSR    P_WAITBYTE               *EA21: 17 FD FE       '...'
        CMPA    #$59                     *EA24: 81 59          '.Y'    GOT "Y"?
        BEQ     P_PROG3                  *EA26: 27 03          ''.'    DO THE PROGRAMMING
        LBRA    P_EPRM3                  *EA28: 16 FD DC       '...'
P_PROG3 LBSR    P_GETPUT                 *EA2B: 17 FE 14       '...'   ECHO
ZEA2E   PULS    Y,X,D                    *EA2E: 35 36          '56'
ZEA30   PSHS    D                        *EA30: 34 06          '4.'
        LDA     ,X+                      *EA32: A6 80          '..'
        LBSR    P_CHK_THIS2              *EA34: 17 04 B4       '...'
        PULS    D                        *EA37: 35 06          '5.'
        SUBD    #$0001                   *EA39: 83 00 01       '...'
        BEQ     ZEA4A                    *EA3C: 27 0C          ''.'
        LEAY    $01,Y                    *EA3E: 31 21          '1!'
        CMPY    P_DEVSZ                  *EA40: 10 9C 99       '...'
        BNE     ZEA30                    *EA43: 26 EB          '&.'
        LDA     #$02                     *EA45: 86 02          '..'
        LBSR    P_PRMSG                  *EA47: 17 00 D2       '...'   PRINT "EXCEEDED SIZE"
ZEA4A   LDA     #$07                     *EA4A: 86 07          '..'
        LBSR    P_GETPUT                 *EA4C: 17 FD F3       '...'   SEND BELL
        LBSR    P_VERF                   *EA4F: 17 FE B9       '...'
        RTS                              *EA52: 39             '9'
P_READ  LBSR    P_PARAM                  *EA53: 17 01 75       '..u'
        LBSR    P_CHKDEV                 *EA56: 17 02 24       '..$'
        LDD     P_START                  *EA59: DC 84          '..'
        ANDA    P_DEVEND                 *EA5B: 94 9B          '..'
        ANDB    M9D9C                    *EA5D: D4 9C          '..'
        ADDD    P_ROMOFFS                *EA5F: D3 8A          '..'
        TFR     D,Y                      *EA61: 1F 02          '..'
        LDX     P_RAMSTART               *EA63: 9E 88          '..'
        LDD     P_END                    *EA65: DC 86          '..'
        SUBD    P_START                  *EA67: 93 84          '..'
        ADDD    #$0001                   *EA69: C3 00 01       '...'
ZEA6C   PSHS    D                        *EA6C: 34 06          '4.'
        LBSR    P_CHK_THIS1              *EA6E: 17 04 52       '..R'
        STA     ,X+                      *EA71: A7 80          '..'
        PULS    D                        *EA73: 35 06          '5.'
        SUBD    #$0001                   *EA75: 83 00 01       '...'
        BEQ     ZEA89                    *EA78: 27 0F          ''.'
        LEAY    $01,Y                    *EA7A: 31 21          '1!'
        CMPY    P_DEVSZ                  *EA7C: 10 9C 99       '...'
        BNE     ZEA6C                    *EA7F: 26 EB          '&.'
        LDA     #$02                     *EA81: 86 02          '..'
        LBSR    P_CRLF                   *EA83: 17 01 D3       '...'
        LBSR    P_PRMSG                  *EA86: 17 00 93       '...'   PRINT "EXCEEDED SIZE"
ZEA89   RTS                              *EA89: 39             '9'
P_E     LBSR    ZEBF3                    *EA8A: 17 01 66       '..f'
        BCS     ZEADD                    *EA8D: 25 4E          '%N'
        TFR     D,X                      *EA8F: 1F 01          '..'
        LDD     P_RAMSTART               *EA91: DC 88          '..'
        SUBD    P_START                  *EA93: 93 84          '..'
        LEAY    D,X                      *EA95: 31 8B          '1.'
ZEA97   LBSR    P_X4HEX                  *EA97: 17 00 CD       '...'
        LDA     ,Y                       *EA9A: A6 A4          '..'
        LBSR    ZEB77                    *EA9C: 17 00 D8       '...'
        LBSR    P_WAITBYTE               *EA9F: 17 FD 80       '...'
        BSR     ZEB13                    *EAA2: 8D 6F          '.o'
        CMPA    #$20                     *EAA4: 81 20          '. '
        BNE     ZEAB1                    *EAA6: 26 09          '&.'
        LEAX    $01,X                    *EAA8: 30 01          '0.'
        LEAY    $01,Y                    *EAAA: 31 21          '1!'
        LBSR    P_CRLF                   *EAAC: 17 01 AA       '...'
        BRA     ZEA97                    *EAAF: 20 E6          ' .'
ZEAB1   CMPA    #$5E                     *EAB1: 81 5E          '.^'
        BNE     ZEABE                    *EAB3: 26 09          '&.'
        LBSR    P_CRLF                   *EAB5: 17 01 A1       '...'
        LEAX    -$01,X                   *EAB8: 30 1F          '0.'
        LEAY    -$01,Y                   *EABA: 31 3F          '1?'
        BRA     ZEA97                    *EABC: 20 D9          ' .'
ZEABE   LBSR    ZEC3C                    *EABE: 17 01 7B       '..{'
        BCS     ZEADD                    *EAC1: 25 1A          '%.'
        LDB     #$10                     *EAC3: C6 10          '..'
        MUL                              *EAC5: 3D             '='
        PSHS    B                        *EAC6: 34 04          '4.'
        LBSR    P_WAITBYTE               *EAC8: 17 FD 57       '..W'
        BSR     ZEB13                    *EACB: 8D 46          '.F'
        LBSR    ZEC3C                    *EACD: 17 01 6C       '..l'
        BCS     ZEADE                    *EAD0: 25 0C          '%.'
        ADDA    ,S+                      *EAD2: AB E0          '..'
        STA     ,Y+                      *EAD4: A7 A0          '..'
        LEAX    $01,X                    *EAD6: 30 01          '0.'
        LBSR    P_CRLF                   *EAD8: 17 01 7E       '..~'
        BRA     ZEA97                    *EADB: 20 BA          ' .'
ZEADD   RTS                              *EADD: 39             '9'
ZEADE   PULS    PC,A                     *EADE: 35 82          '5.'
P_2532  LDB     #$02                     *EAE0: C6 02          '..'
        BRA     P_SETDEV                 *EAE2: 20 16          ' .'
P_27128 LDB     #$06                     *EAE4: C6 06          '..'
        BRA     P_SETDEV                 *EAE6: 20 12          ' .'
P_2764  LDB     #$05                     *EAE8: C6 05          '..'
        BRA     P_SETDEV                 *EAEA: 20 0E          ' .'
P_2732  LDB     #$03                     *EAEC: C6 03          '..'
        BRA     P_SETDEV                 *EAEE: 20 0A          ' .'
P_2732A LDB     #$04                     *EAF0: C6 04          '..'
        BRA     P_SETDEV                 *EAF2: 20 06          ' .'
P_2716  LDB     #$01                     *EAF4: C6 01          '..'
        BRA     P_SETDEV                 *EAF6: 20 02          ' .'
P_2516  LDB     #$00                     *EAF8: C6 00          '..'
P_SETDEV PSHS    B                        *EAFA: 34 04          '4.'    SAVE DEVICE
        LBSR    P_INIT                   *EAFC: 17 03 5B       '..['
        PULS    B                        *EAFF: 35 04          '5.'    RESTORE DEVICE
        LBSR    CHEKOFFS                 *EB01: 17 04 9D       '...'
        BCC     P_SETDEX                 *EB04: 24 08          '$.'
        LDA     #$01                     *EB06: 86 01          '..'
        BSR     P_PRMSG                  *EB08: 8D 12          '..'    PRINT "WRONG HEAD"
        LDA     #$FF                     *EB0A: 86 FF          '..'
        STA     P_DEVICE                 *EB0C: 97 96          '..'    RESET DEVICE TYPE
P_SETDEX RTS                              *EB0E: 39             '9'
P_EXIT  JMP     [svec_RST]               *EB0F: 6E 9F FF FE    'n...'
ZEB13   LBSR    P_ALNUM                  *EB13: 17 01 51       '..Q'
        BCS     ZEB1B                    *EB16: 25 03          '%.'
        LBSR    P_GETPUT                 *EB18: 17 FD 27       '..''
ZEB1B   RTS                              *EB1B: 39             '9'
* PRINT OUT ERROR MESSAGE FROM MESSAGE ID IN A
P_PRMSG PSHS    X,D                      *EB1C: 34 16          '4.'
        LEAX    P_MSG_TBL,PCR            *EB1E: 30 8D 01 D3    '0...'  LOAD POINTER TO MSG TBL INTO X
P_PRMS2 CMPA    #$08                     *EB22: 81 08          '..'
        BLS     P_PRMS3                  *EB24: 23 02          '#.'    CONTINUE IF MSG ID <8
        LDA     #$06                     *EB26: 86 06          '..'    OTHERWISE, UNKNOWN "ARRRGH"
P_PRMS3 ASLA                             *EB28: 48             'H'     CONVERT A TO WORD OFFSET (MULT BY 2)
        LDD     A,X                      *EB29: EC 86          '..'    GET THE POINTER TO THE MSG
        LEAX    D,X                      *EB2B: 30 8B          '0.'    GET THE ADDRESS OF THE MSG INTO X
        LBSR    P_SENDMSG                *EB2D: 17 00 8E       '...'   SEND IT
        PULS    PC,X,D                   *EB30: 35 96          '5.'    RETURN
P_PRMS0 PSHS    X,D                      *EB32: 34 16          '4.'
        BRA     P_PRMS2                  *EB34: 20 EC          ' .'

* PARSE INPUT, COMPARE WITH P_CMD_TBL
P_CMDSRC LDX     P_BUFFLOC                *EB36: 9E 82          '..'
        LEAY    P_CMD_TBL,PCR            *EB38: 31 8D 02 78    '1..x'
P_PARIN LDA     ,X+                      *EB3C: A6 80          '..'
        LBSR    P_ALNUM                  *EB3E: 17 01 26       '..&'   CHECK IF ALPHA NUMERIC
        BCS     P_PAREX                  *EB41: 25 17          '%.'
        TST     ,Y                       *EB43: 6D A4          'm.'
        BEQ     P_PARI2                  *EB45: 27 04          ''.'
        CMPA    ,Y+                      *EB47: A1 A0          '..'
        BEQ     P_PARIN                  *EB49: 27 F1          ''.'
P_PARI2 LDX     P_BUFFLOC                *EB4B: 9E 82          '..'
        TST     ,Y+                      *EB4D: 6D A0          'm.'
        BNE     P_PARI2                  *EB4F: 26 FA          '&.'
        LEAY    $02,Y                    *EB51: 31 22          '1"'
        TST     ,Y                       *EB53: 6D A4          'm.'
        BNE     P_PARIN                  *EB55: 26 E5          '&.'
P_PARER ORCC    #$01                     *EB57: 1A 01          '..'
        RTS                              *EB59: 39             '9'
P_PAREX LEAX    -$01,X                   *EB5A: 30 1F          '0.'
        TST     ,Y                       *EB5C: 6D A4          'm.'    CHECK FOR NULL STRING TERMINATOR
        BNE     P_PARER                  *EB5E: 26 F7          '&.'    IF NOT, NOT COMMAND ERR
        STX     P_BUFFLOC                *EB60: 9F 82          '..'
        LEAX    $01,Y                    *EB62: 30 21          '0!'    LOAD POINTER TO COMMAND ADDRESS INTO X
        ANDCC   #$FE                     *EB64: 1C FE          '..'    CLEAR CARRY, FOUND COMMAND!
        RTS                              *EB66: 39             '9'
* OUTPUT X AS 4 DIGIT HEX
P_X4HEX PSHS    X,A                      *EB67: 34 12          '4.'
        LDA     $01,S                    *EB69: A6 61          '.a'
        BSR     P_BIN2HEX                *EB6B: 8D 14          '..'
        LDA     $02,S                    *EB6D: A6 62          '.b'
        BSR     P_BIN2HEX                *EB6F: 8D 10          '..'
        LDA     #$01                     *EB71: 86 01          '..'    SEND 1 SPACE
        BSR     P_SENDSPS                *EB73: 8D 28          '.('
        PULS    PC,X,A                   *EB75: 35 92          '5.'
ZEB77   BSR     P_BIN2HEX                *EB77: 8D 08          '..'
        PSHS    A                        *EB79: 34 02          '4.'
        LDA     #$01                     *EB7B: 86 01          '..'
        BSR     P_SENDSPS                *EB7D: 8D 1E          '..'    SEND 1 SPACE
        PULS    PC,A                     *EB7F: 35 82          '5.'
P_BIN2HEX PSHS    A                        *EB81: 34 02          '4.'
        LSRA                             *EB83: 44             'D'     GET HI NIBBLE
        LSRA                             *EB84: 44             'D'
        LSRA                             *EB85: 44             'D'
        LSRA                             *EB86: 44             'D'
        BSR     P_SENDHEX                *EB87: 8D 06          '..'    THEN CONVERT IT
        LDA     ,S                       *EB89: A6 E4          '..'    RESTORE BYTE
        BSR     P_SENDHEX                *EB8B: 8D 02          '..'    CONVERT IT
        PULS    PC,A                     *EB8D: 35 82          '5.'    RESTORE A, RETURN
P_SENDHEX ANDA    #$0F                     *EB8F: 84 0F          '..'    LO NIBBLE
        ADDA    #$30                     *EB91: 8B 30          '.0'    CONVERT TO ASCII NUM
        CMPA    #'9                      *EB93: 81 39          '.9'    RANGE 0-9?
        BLS     P_SENDHE2                *EB95: 23 02          '#.'    DIGIT?
                                         * IF SO SKIP CORRECTION
        ADDA    #7                       *EB97: 8B 07          '..'    ADJUST FOR A-F
P_SENDHE2 LBSR    P_GETPUT                 *EB99: 17 FC A6       '...'
        RTS                              *EB9C: 39             '9'
P_SENDSPS PSHS    A                        *EB9D: 34 02          '4.'
        PSHS    A                        *EB9F: 34 02          '4.'    SAVE A AS COUNTER ON STACK
        LDA     #$20                     *EBA1: 86 20          '. '    SPACE CHAR IN A
P_SENDSP1 LBSR    P_GETPUT                 *EBA3: 17 FC 9C       '...'   SEND CHAR IN A
        DEC     ,S                       *EBA6: 6A E4          'j.'    DEC COUNTER
        BNE     P_SENDSP1                *EBA8: 26 F9          '&.'    LOOP
        PULS    A                        *EBAA: 35 02          '5.'    PULL EMPTY COUNTER
        PULS    PC,A                     *EBAC: 35 82          '5.'    RESTORE ORIGINAL COUNT, RETURN
P_SKIPSPX PSHS    X,A                      *EBAE: 34 12          '4.'
        LDX     P_BUFFLOC                *EBB0: 9E 82          '..'
P_SKIPSP LDA     ,X+                      *EBB2: A6 80          '..'
        CMPA    #$20                     *EBB4: 81 20          '. '
        BEQ     P_SKIPSP                 *EBB6: 27 FA          ''.'
        LEAX    -$01,X                   *EBB8: 30 1F          '0.'
        STX     P_BUFFLOC                *EBBA: 9F 82          '..'
        PULS    PC,X,A                   *EBBC: 35 92          '5.'
P_SENDMSG PSHS    X,A                      *EBBE: 34 12          '4.'
ZEBC0   LDA     ,X+                      *EBC0: A6 80          '..'    GET NEXT MSG LETTER
        BEQ     ZEBC9                    *EBC2: 27 05          ''.'    CHECK FOR NULL END
        LBSR    P_GETPUT                 *EBC4: 17 FC 7B       '..{'   ACIA OUTPUT MESSAGE LETTER
        BRA     ZEBC0                    *EBC7: 20 F7          ' .'    CONTINUE UNTIL NULL
ZEBC9   PULS    PC,X,A                   *EBC9: 35 92          '5.'    RETURN
P_PARAM PSHS    D                        *EBCB: 34 06          '4.'
        BSR     P_SKIPSPX                *EBCD: 8D DF          '..'
        BSR     ZEBF3                    *EBCF: 8D 22          '."'
        BCS     ZEBDF                    *EBD1: 25 0C          '%.'
        PSHS    D                        *EBD3: 34 06          '4.'
        SUBD    P_START                  *EBD5: 93 84          '..'
        ADDD    ,S                       *EBD7: E3 E4          '..'
        STD     P_RAMSTART               *EBD9: DD 88          '..'
        PULS    D                        *EBDB: 35 06          '5.'
        STD     P_START                  *EBDD: DD 84          '..'
ZEBDF   BSR     ZEBF3                    *EBDF: 8D 12          '..'
        BCS     ZEBE5                    *EBE1: 25 02          '%.'
        STD     P_END                    *EBE3: DD 86          '..'
ZEBE5   BSR     ZEBF3                    *EBE5: 8D 0C          '..'
        BCS     ZEBEB                    *EBE7: 25 02          '%.'
        STD     P_RAMSTART               *EBE9: DD 88          '..'
ZEBEB   BSR     ZEBF3                    *EBEB: 8D 06          '..'
        BCS     ZEBF1                    *EBED: 25 02          '%.'
        STD     P_ROMOFFS                *EBEF: DD 8A          '..'
ZEBF1   PULS    PC,D                     *EBF1: 35 86          '5.'
ZEBF3   PSHS    X                        *EBF3: 34 10          '4.'
        LDX     P_BUFFLOC                *EBF5: 9E 82          '..'
        LDD     #$0000                   *EBF7: CC 00 00       '...'
        PSHS    D                        *EBFA: 34 06          '4.'
        LDA     ,X                       *EBFC: A6 84          '..'
        CMPA    #$0D                     *EBFE: 81 0D          '..'
        BEQ     ZEC36                    *EC00: 27 34          ''4'
        BSR     P_SKIPSPX                *EC02: 8D AA          '..'
        LDX     P_BUFFLOC                *EC04: 9E 82          '..'
        LDA     ,X+                      *EC06: A6 80          '..'
        BSR     ZEC3C                    *EC08: 8D 32          '.2'
        BCS     ZEC36                    *EC0A: 25 2A          '%*'
        LDX     P_BUFFLOC                *EC0C: 9E 82          '..'
ZEC0E   LDA     ,X+                      *EC0E: A6 80          '..'
        BSR     ZEC3C                    *EC10: 8D 2A          '.*'
        BCS     ZEC2A                    *EC12: 25 16          '%.'
        ASL     $01,S                    *EC14: 68 61          'ha'
        ROL     ,S                       *EC16: 69 E4          'i.'
        ASL     $01,S                    *EC18: 68 61          'ha'
        ROL     ,S                       *EC1A: 69 E4          'i.'
        ASL     $01,S                    *EC1C: 68 61          'ha'
        ROL     ,S                       *EC1E: 69 E4          'i.'
        ASL     $01,S                    *EC20: 68 61          'ha'
        ROL     ,S                       *EC22: 69 E4          'i.'
        ADDA    $01,S                    *EC24: AB 61          '.a'
        STA     $01,S                    *EC26: A7 61          '.a'
        BRA     ZEC0E                    *EC28: 20 E4          ' .'
ZEC2A   CMPA    #$0D                     *EC2A: 81 0D          '..'
        BNE     ZEC30                    *EC2C: 26 02          '&.'
        LEAX    -$01,X                   *EC2E: 30 1F          '0.'
ZEC30   STX     P_BUFFLOC                *EC30: 9F 82          '..'
        ANDCC   #$FE                     *EC32: 1C FE          '..'
        PULS    PC,X,D                   *EC34: 35 96          '5.'
ZEC36   STX     P_BUFFLOC                *EC36: 9F 82          '..'
        ORCC    #$01                     *EC38: 1A 01          '..'
        PULS    PC,X,D                   *EC3A: 35 96          '5.'
ZEC3C   PSHS    A                        *EC3C: 34 02          '4.'
P_HEX2DEC SUBA    #$30                     *EC3E: 80 30          '.0'
        BCS     ZEC55                    *EC40: 25 13          '%.'
        CMPA    #$09                     *EC42: 81 09          '..'
        BLS     ZEC50                    *EC44: 23 0A          '#.'
        SUBA    #$07                     *EC46: 80 07          '..'
        CMPA    #$09                     *EC48: 81 09          '..'
        BLS     ZEC55                    *EC4A: 23 09          '#.'
        CMPA    #$0F                     *EC4C: 81 0F          '..'
        BGT     ZEC55                    *EC4E: 2E 05          '..'
ZEC50   LEAS    $01,S                    *EC50: 32 61          '2a'
        ANDCC   #$FE                     *EC52: 1C FE          '..'
        RTS                              *EC54: 39             '9'
ZEC55   ORCC    #$01                     *EC55: 1A 01          '..'
        PULS    PC,A                     *EC57: 35 82          '5.'
P_CRLF  PSHS    A                        *EC59: 34 02          '4.'
        LDA     #$0D                     *EC5B: 86 0D          '..'    CR CHAR
        LBSR    P_GETPUT                 *EC5D: 17 FB E2       '...'
        LDA     #$0A                     *EC60: 86 0A          '..'    LF CHAR
        LBSR    P_GETPUT                 *EC62: 17 FB DD       '...'
        PULS    PC,A                     *EC65: 35 82          '5.'
P_ALNUM CMPA    #$30                     *EC67: 81 30          '.0'    TEST LOWER BOUND
        BLT     P_ALNUER                 *EC69: 2D 0F          '-.'    EXIT IF NOT A DIGIT
        CMPA    #$39                     *EC6B: 81 39          '.9'    TEST HIGH BOUND
        BLE     P_ALNU2                  *EC6D: 2F 08          '/.'    PROCEED IF IT IS AN ASCII NUMBER
        CMPA    #$41                     *EC6F: 81 41          '.A'
        BLT     P_ALNUER                 *EC71: 2D 07          '-.'    EXIT IF LOWER THAN LETTER
        CMPA    #$5A                     *EC73: 81 5A          '.Z'
        BGT     P_ALNUER                 *EC75: 2E 03          '..'    EXIT IF HIGER THAN LETTER
P_ALNU2 ANDCC   #$FE                     *EC77: 1C FE          '..'    ONLY ACII LETTERS OR NUMBERS GET HERE
        RTS                              *EC79: 39             '9'
P_ALNUER ORCC    #$01                     *EC7A: 1A 01          '..'
        RTS                              *EC7C: 39             '9'
P_CHKDEV TST     P_DEVICE                 *EC7D: 0D 96          '..'    DEVICE TYPE NOT SET?
        BMI     P_DEVMSG                 *EC7F: 2B 01          '+.'    ALERT USER
        RTS                              *EC81: 39             '9'
P_DEVMSG LDA     #$03                     *EC82: 86 03          '..'
        LBSR    P_PRMSG                  *EC84: 17 FE 95       '...'   PRINT "SELECT DEVICE"
        LBRA    P_EPRM3                  *EC87: 16 FB 7D       '..}'
ZEC8A   PSHS    X,D,CC                   *EC8A: 34 17          '4.'
        LDD     P_START                  *EC8C: DC 84          '..'
        SUBD    P_RAMSTART               *EC8E: 93 88          '..'
        LEAX    D,X                      *EC90: 30 8B          '0.'
        LBSR    P_X4HEX                  *EC92: 17 FE D2       '...'
        PULS    PC,X,D,CC                *EC95: 35 97          '5.'
P_ININIT LDB     #$00                     *EC97: C6 00          '..'
        TFR     DP,A                     *EC99: 1F B8          '..'
        STD     P_BUFFLOC                *EC9B: DD 82          '..'    SAVE START BUFF LOCATION
        TFR     D,X                      *EC9D: 1F 01          '..'    START BUFF IN X (END IN S)
        CLR     ,-S                      *EC9F: 6F E2          'o.'    CLEAR END OF BUFFER
P_INHNDL LBSR    P_WAITBYTE               *ECA1: 17 FB 7E       '..~'   WAIT FOR INPUT
        CMPA    #$08                     *ECA4: 81 08          '..'    BACKSPACE?
        BNE     P_INHNDL1                *ECA6: 26 17          '&.'    IF NOT, JUMP AHEAD TO ???
        CMPX    P_BUFFLOC                *ECA8: 9C 82          '..'    IF ON THE CORRECT BYTE,
        BEQ     P_INHNDL                 *ECAA: 27 F5          ''.'    GET THE NEXT INPUT BYTE
        LBSR    P_GETPUT                 *ECAC: 17 FB 93       '...'   ECHO
        LDA     #$20                     *ECAF: 86 20          '. '    SPACE CHAR
        LBSR    P_GETPUT                 *ECB1: 17 FB 8E       '...'   TO ACIA
        LDA     #$08                     *ECB4: 86 08          '..'    BACK SPACE CHAR
        LBSR    P_GETPUT                 *ECB6: 17 FB 89       '...'   TO ACIA
        LEAX    -$01,X                   *ECB9: 30 1F          '0.'    GO TO PREVIOUS BUFFER BYTE
        DEC     ,S                       *ECBB: 6A E4          'j.'
        BRA     P_INHNDL                 *ECBD: 20 E2          ' .'    CHECK FOR NEXT INPUT
P_INHNDL1 CMPA    #$0D                     *ECBF: 81 0D          '..'    CARRIAGE RETURN?
        BNE     P_INHNDL2                *ECC1: 26 10          '&.'
        BSR     P_CRLF                   *ECC3: 8D 94          '..'    ECHO CRLF
        STA     ,X                       *ECC5: A7 84          '..'    STORE INPUT BYTE IN BUFFER
        LEAS    $01,S                    *ECC7: 32 61          '2a'
        CMPX    P_BUFFLOC                *ECC9: 9C 82          '..'    ON THE RIGHT BYTE?
        BEQ     P_INHNDLE                *ECCB: 27 03          ''.'
        ANDCC   #$FE                     *ECCD: 1C FE          '..'    CLEAR CARRY
        RTS                              *ECCF: 39             '9'     RETURN NORMALLY
P_INHNDLE ORCC    #$01                     *ECD0: 1A 01          '..'    SET CARRY
        RTS                              *ECD2: 39             '9'     RETURN INDICATING ERROR
P_INHNDL2 TST     ,S                       *ECD3: 6D E4          'm.'
        BMI     P_INHNDL                 *ECD5: 2B CA          '+.'
        CMPA    #$20                     *ECD7: 81 20          '. '
        BLT     P_INHNDL                 *ECD9: 2D C6          '-.'    SKIP CTRL CHARS
        STA     ,X+                      *ECDB: A7 80          '..'    STORE A, INCREMENT BUFFER LOC POINTER
        LBSR    P_GETPUT                 *ECDD: 17 FB 62       '..b'   GET NEXT INPUT CHAR
        INC     ,S                       *ECE0: 6C E4          'l.'
        BRA     P_INHNDL                 *ECE2: 20 BD          ' .'    LOOP
MECE4   FCB     $00,$87,$01,$0B,$01,$04  *ECE4: 00 87 01 0B 01 04 '......'
        FCB     $01,$19,$01,$12,$01      *ECEA: 01 19 01 12 01 '.....'
        FCC     " "                      *ECEF: 20             ' '
        FCB     $01                      *ECF0: 01             '.'
        FCC     "("                      *ECF1: 28             '('
        FCB     $01                      *ECF2: 01             '.'
        FCC     "/"                      *ECF3: 2F             '/'
MECF4   FCB     $FF                      *ECF4: FF             '.'
P_MSG_TBL FDB     $0012                    *ECF5: 00 12          '..'    #$00. OFFSET TO ED07 STRING "START  END"
        FDB     $0032                    *ECF7: 00 32          '.2'    #$01. OFFSET TO ED27 STRING "WRONG HEAD"
        FDB     $004D                    *ECF9: 00 4D          '.M'    #$02. OFFSET TO ED42 STRING "EXEEDED SIZE"
        FDB     $0076                    *ECFB: 00 76          '.v'    #$03. OFFSET TO ED6B STRING "PLEASE SELECT"
        FDB     $009D                    *ECFD: 00 9D          '..'    #$04. OFFSET TO ED92 STRING (PROMPT)
        FDB     $00A1                    *ECFF: 00 A1          '..'    #$05. OFFSET TO ED96 STRING "WHAT?"
        FDB     $00A7                    *ED01: 00 A7          '..'    #$06. OFFSET TO ED9C STRING "ARRRGH!!!"
        FDB     $00B1                    *ED03: 00 B1          '..'    #$07. OFFSET TO EDA6 STRING "ADDR PR MM"
        FDB     $0094                    *ED05: 00 94          '..'    #$08. OFFSET TO ED89 STRING "CHANGE?"
        FCC     "START  END  RAM-ADDR "  *ED07: 53 54 41 52 54 20 20 45 4E 44 20 20 52 41 4D 2D 41 44 44 52 20 'START  END  RAM-ADDR '
        FCC     "ROM-OFFSET"             *ED1C: 52 4F 4D 2D 4F 46 46 53 45 54 'ROM-OFFSET'
        FCB     $00                      *ED26: 00             '.'
        FCC     "WRONG HEAD FOR THAT D"  *ED27: 57 52 4F 4E 47 20 48 45 41 44 20 46 4F 52 20 54 48 41 54 20 44 'WRONG HEAD FOR THAT D'
        FCC     "EVICE"                  *ED3C: 45 56 49 43 45 'EVICE'
        FCB     $00                      *ED41: 00             '.'
        FCC     "YOU HAVE EXCEEDED THE"  *ED42: 59 4F 55 20 48 41 56 45 20 45 58 43 45 45 44 45 44 20 54 48 45 'YOU HAVE EXCEEDED THE'
        FCC     " SIZE OF THE DEVICE"    *ED57: 20 53 49 5A 45 20 4F 46 20 54 48 45 20 44 45 56 49 43 45 ' SIZE OF THE DEVICE'
        FCB     $00                      *ED6A: 00             '.'
        FCC     "PLEASE SELECT THE DEV"  *ED6B: 50 4C 45 41 53 45 20 53 45 4C 45 43 54 20 54 48 45 20 44 45 56 'PLEASE SELECT THE DEV'
        FCC     "ICE TYPE"               *ED80: 49 43 45 20 54 59 50 45 'ICE TYPE'
        FCB     $00                      *ED88: 00             '.'
        FCC     "CHANGE? "               *ED89: 43 48 41 4E 47 45 3F 20 'CHANGE? '
        FCB     $00                      *ED91: 00             '.'
        FCC     "P* "                    *ED92: 50 2A 20       'P* '
        FCB     $00                      *ED95: 00             '.'
        FCC     "WHAT?"                  *ED96: 57 48 41 54 3F 'WHAT?'
        FCB     $00                      *ED9B: 00             '.'
        FCC     "ARRRGH!!!"              *ED9C: 41 52 52 52 47 48 21 21 21 'ARRRGH!!!'
        FCB     $00                      *EDA5: 00             '.'
        FCC     "ADDR PR MM   "          *EDA6: 41 44 44 52 20 50 52 20 4D 4D 20 20 20 'ADDR PR MM   '
        FCB     $00                      *EDB3: 00             '.'
P_CMD_TBL FCC     "PROG"                   *EDB4: 50 52 4F 47    'PROG'
        FCB     $00                      *EDB8: 00             '.'
        FDB     $FC32                    *EDB9: FC 32          '.2'    $E9EB
        FCC     "STAT"                   *EDBB: 53 54 41 54    'STAT'
        FCB     $00                      *EDBF: 00             '.'
        FDB     $FBEC                    *EDC0: FB EC          '..'    $E9AC
        FCC     "TEST"                   *EDC2: 54 45 53 54    'TEST'
        FCB     $00                      *EDC6: 00             '.'
        FDB     $FBD1                    *EDC7: FB D1          '..'    $E998
        FCC     "VERF"                   *EDC9: 56 45 52 46    'VERF'
        FCB     $00                      *EDCD: 00             '.'
        FDB     $FB3D                    *EDCE: FB 3D          '.='    $E90B
        FCC     "VIEW"                   *EDD0: 56 49 45 57    'VIEW'
        FCB     $00                      *EDD4: 00             '.'
        FDB     $FAE9                    *EDD5: FA E9          '..'    $E8BE
        FCC     "CLR"                    *EDD7: 43 4C 52       'CLR'
        FCB     $00                      *EDDA: 00             '.'
        FDB     $FACC                    *EDDB: FA CC          '..'    $E8A7
        FCC     "READ"                   *EDDD: 52 45 41 44    'READ'
        FCB     $00                      *EDE1: 00             '.'
        FDB     $FC71                    *EDE2: FC 71          '.q'    $EA53
        FCB     'E,$00                   *EDE4: 45 00          'E.'
        FDB     $FCA4                    *EDE6: FC A4          '..'    $EA8A
        FCC     "2716"                   *EDE8: 32 37 31 36    '2716'
        FCB     $00                      *EDEC: 00             '.'
        FDB     $FD07                    *EDED: FD 07          '..'    $EAF4
        FCC     "2516"                   *EDEF: 32 35 31 36    '2516'
        FCB     $00                      *EDF3: 00             '.'
        FDB     $FD04                    *EDF4: FD 04          '..'    $EAF8
        FCC     "2732"                   *EDF6: 32 37 33 32    '2732'
        FCB     $00                      *EDFA: 00             '.'
        FDB     $FCF1                    *EDFB: FC F1          '..'    $EAEC
        FCC     "2532"                   *EDFD: 32 35 33 32    '2532'
        FCB     $00                      *EE01: 00             '.'
        FDB     $FCDE                    *EE02: FC DE          '..'    $EAE0
        FCC     "2732A"                  *EE04: 32 37 33 32 41 '2732A'
        FCB     $00                      *EE09: 00             '.'
        FDB     $FCE6                    *EE0A: FC E6          '..'    $EAF0
        FCC     "2764"                   *EE0C: 32 37 36 34    '2764'
        FCB     $00                      *EE10: 00             '.'
        FDB     $FCD7                    *EE11: FC D7          '..'    $EAE0
        FCC     "27128"                  *EE13: 32 37 31 32 38 '27128'
        FCB     $00                      *EE18: 00             '.'
        FDB     $FCCB                    *EE19: FC CB          '..'    $EAE4
        FCC     "EXIT"                   *EE1B: 45 58 49 54    'EXIT'
        FCB     $00                      *EE1F: 00             '.'
        FDB     $FCEF                    *EE20: FC EF          '..'    $EB0F
        FCC     "BYE"                    *EE22: 42 59 45       'BYE'
        FCB     $00                      *EE25: 00             '.'
        FDB     $FCE9                    *EE26: FC E9          '..'    $EB0F
        FCB     $00                      *EE28: 00             '.'
* $17 HEAD - 2K
P_CTRL_TBL FCB     $15,$05,$01,$0B,$08,$00  *EE29: 15 05 01 0B 08 00 '......'
        FCB     $17                      *EE2F: 17             '.'
* $14 HEAD
        FCC     "F"                      *EE30: 46             'F'
        FCB     $06,$02,$CA,$08,$00,$14  *EE31: 06 02 CA 08 00 14 '......'
        FCB     $14,$04,$00,$0A,$10,$00  *EE37: 14 04 00 0A 10 00 '......'
        FCB     $17,$16                  *EE3D: 17 16          '..'
        FCC     "G"                      *EE3F: 47             'G'
        FCB     $07,$CB,$10,$00,$14,$16  *EE40: 07 CB 10 00 14 16 '......'
        FCC     "G"                      *EE46: 47             'G'
        FCB     $07,$E3,$10,$00,$14      *EE47: 07 E3 10 00 14 '.....'
* $18 HEAD
        FCC     "vf I "                  *EE4C: 76 66 20 49 20 'vf I '
        FCB     $00,$18                  *EE51: 00 18          '..'
        FCC     "vf I@"                  *EE53: 76 66 20 49 40 'vf I@'
        FCB     $00,$18                  *EE58: 00 18          '..'
* CLEAR THE PIA I/O ADDRESS STORED AT E81A-E80C
P_INIT  LEAY    AC1DR_VEC,PCR            *EE5A: 31 8D F9 BE    '1...'  LOAD EFFECTIVE ADDRESS OF AC1CR_VEC
        LDA     #$08                     *EE5E: 86 08          '..'    8 PIA REGISTERS TO CLEAR
P_INI1  CLR     [,--Y]                   *EE60: 6F B3          'o.'    DECREMENT EXTENDED ADDRESS
        DECA                             *EE62: 4A             'J'     DEC COUNTER
        BNE     P_INI1                   *EE63: 26 FB          '&.'    CONTINUE TO CLEAR REGISTERS
* LOAD HEAD TYPE FROM PIA1, PORTB, BIT 6,7
        LDA     #$0F                     *EE65: 86 0F          '..'    SET 4-7 IN, 0-3 OUT
        STA     [PIA1PRB_VEC,PCR]        *EE67: A7 9D F9 AD    '....'  SAVE TO PIA1 DDRB
        LDA     #%00110100               *EE6B: 86 34          '.4'    SELECT POR, SET CB2-LO
        STA     [PIA1CRB_VEC,PCR]        *EE6D: A7 9D F9 A9    '....'  SAVE TO PIA1 CRB
        LDA     [PIA1PRB_VEC,PCR]        *EE71: A6 9D F9 A3    '....'  SAVE PIA1 PORB
        CLR     [PIA1CRB_VEC,PCR]        *EE75: 6F 9D F9 A1    'o...'  SELECT PIA1 DDRB, CB2 INPUT
* CHECK HEAD TYPE
* CMS 9614, 9617, 9618 AND INIT ACCORDINGLY
        ANDA    #%11000000               *EE79: 84 C0          '..'    CHECK ONLY PB6 AND PB7
        BEQ     P_IN3                    *EE7B: 27 12          ''.'    IF BOTH,
        CMPA    #%10000000               *EE7D: 81 80          '..'    CHECK IF PB7 HI
        BEQ     P_IN2                    *EE7F: 27 13          ''.'    IF SO
        CMPA    #%01000000               *EE81: 81 40          '.@'    CHECK IF PB6 HI
        BEQ     P_IN1                    *EE83: 27 1A          ''.'    IF SO
        LDA     #$01                     *EE85: 86 01          '..'
        LBSR    P_PRMSG                  *EE87: 17 FC 92       '...'   PRINT "WRONG HEAD"
        LDB     #$01                     *EE8A: C6 01          '..'
        ORCC    #$01                     *EE8C: 1A 01          '..'    SET CARRY FLAG
        RTS                              *EE8E: 39             '9'     RETURN WITH ERROR IN CARRY
P_IN3   LDD     #ID1700                  *EE8F: CC 17 00       '...'
        BRA     P_2INIT                  *EE92: 20 0E          ' .'
P_IN2   LDA     #%00111111               *EE94: 86 3F          '.?'    SET 6-7 IN, 0-5 OUT
        STA     [PIA1PRB_VEC,PCR]        *EE96: A7 9D F9 7E    '...~'  SAVE TO PIA1 DDRB
        LDD     #ID1805                  *EE9A: CC 18 05       '...'
        BRA     P_2INIT                  *EE9D: 20 03          ' .'
P_IN1   LDD     #ID1401                  *EE9F: CC 14 01       '...'
P_2INIT STA     P_HEAD                   *EEA2: 97 9D          '..'    STORE HEAD TYPE ($14, $17, $18)
        LDA     #%11111111               *EEA4: 86 FF          '..'    SET 0-7 OUTPUT
        STA     [PIA1PRA_VEC,PCR]        *EEA6: A7 9D F9 6A    '...j'  SAVE TO PIA1 DDRA
        STA     [PIA2PRA_VEC,PCR]        *EEAA: A7 9D F9 5E    '...^'  SAVE TO PIA2 DDRA
        LDA     #%00110100               *EEAE: 86 34          '.4'    SELECT POR, SET CA/B2-LO
        STA     [PIA2CRA_VEC,PCR]        *EEB0: A7 9D F9 5A    '...Z'  SAVE TO PIA2 CRA
        STA     [PIA1CRA_VEC,PCR]        *EEB4: A7 9D F9 5E    '...^'  SAVE TO PIA1 CRA
        STA     [PIA1CRB_VEC,PCR]        *EEB8: A7 9D F9 5E    '...^'  SAVE TO PIA1 CRB
        LBSR    CHEKOFFS                 *EEBC: 17 00 E2       '...'
        CLRB                             *EEBF: 5F             '_'
        ANDCC   #$FE                     *EEC0: 1C FE          '..'    CLEAR CARRY
        RTS                              *EEC2: 39             '9'     DONE
P_CHK_THIS1 LDA     P_THIS                   *EEC3: 96 93          '..'
        CMPA    #$01                     *EEC5: 81 01          '..'
        BEQ     ZEECC                    *EEC7: 27 03          ''.'
        LBSR    P_SET_THIS1              *EEC9: 17 00 7D       '..}'
ZEECC   CMPY    P_DEVSZ                  *EECC: 10 9C 99       '...'
        BGE     ZEEE3                    *EECF: 2C 12          ',.'
        TFR     Y,D                      *EED1: 1F 20          '. '
        STA     [PIA1PRB_VEC,PCR]        *EED3: A7 9D F9 41    '...A'
        STB     [PIA1PRA_VEC,PCR]        *EED7: E7 9D F9 39    '...9'
        LDA     [PIA2PRB_VEC,PCR]        *EEDB: A6 9D F9 31    '...1'
        CLRB                             *EEDF: 5F             '_'
        RTS                              *EEE0: 39             '9'
ZEEE1   PULS    A                        *EEE1: 35 02          '5.'
ZEEE3   BSR     ZEF1B                    *EEE3: 8D 36          '.6'
        CLRA                             *EEE5: 4F             'O'
        CLRB                             *EEE6: 5F             '_'
        COMB                             *EEE7: 53             'S'
        LDB     #$02                     *EEE8: C6 02          '..'
        RTS                              *EEEA: 39             '9'
P_CHK_THIS2 PSHS    A                        *EEEB: 34 02          '4.'
        LDB     P_THIS                   *EEED: D6 93          '..'
        CMPB    #$02                     *EEEF: C1 02          '..'
        BEQ     ZEEF6                    *EEF1: 27 03          ''.'
        LBSR    P_SET_THIS2              *EEF3: 17 00 85       '...'
ZEEF6   CMPY    P_DEVSZ                  *EEF6: 10 9C 99       '...'
        BGE     ZEEE1                    *EEF9: 2C E6          ',.'
        TFR     Y,D                      *EEFB: 1F 20          '. '
        STA     [PIA1PRB_VEC,PCR]        *EEFD: A7 9D F9 17    '....'
        STB     [PIA1PRA_VEC,PCR]        *EF01: E7 9D F9 0F    '....'
        PULS    A                        *EF05: 35 02          '5.'
        CMPA    #$FF                     *EF07: 81 FF          '..'
        BEQ     ZEF19                    *EF09: 27 0E          ''.'
        TST     [PIA2PRB_VEC,PCR]        *EF0B: 6D 9D F9 01    'm...'
        STA     [PIA2PRB_VEC,PCR]        *EF0F: A7 9D F8 FD    '....'
ZEF13   TST     [PIA2CRB_VEC,PCR]        *EF13: 6D 9D F8 FB    'm...'
        BPL     ZEF13                    *EF17: 2A FA          '*.'
ZEF19   CLRB                             *EF19: 5F             '_'
        RTS                              *EF1A: 39             '9'
ZEF1B   PSHS    X,D                      *EF1B: 34 16          '4.'
        LDA     #$00                     *EF1D: 86 00          '..'
        STA     P_THIS                   *EF1F: 97 93          '..'    RESET THIS STORE
        LDX     P_TABLOC                 *EF21: 9E 97          '..'    START OF THE TABLE
        LDA     $01,X                    *EF23: A6 01          '..'    GET A FROM TABLE ($06, $05, $66)
        STA     [PIA2PRA_VEC,PCR]        *EF25: A7 9D F8 E3    '....'  OUTPUT IT ON PIA 2 PORT A
        LDA     #%00111010               *EF29: 86 3A          '.:'    SELECT DDR, SET C2 HI ...
        STA     [PIA2CRB_VEC,PCR]        *EF2B: A7 9D F8 E3    '....'  ON PIA 2 PORT A
        LDB     #$FF                     *EF2F: C6 FF          '..'    ALL OUTPUTS ...
        STB     [PIA2PRB_VEC,PCR]        *EF31: E7 9D F8 DB    '....'  ON PIA 2 PORT A
        ORA     #%00000100               *EF35: 8A 04          '..'    SELECT POR ...
        STA     [PIA2CRB_VEC,PCR]        *EF37: A7 9D F8 D7    '....'  ON PIA 2 PORT A
        LDA     #$64                     *EF3B: 86 64          '.d'    DELAY MULTIPLIER
        LBSR    DELAY10MS                *EF3D: 17 00 8B       '...'   DELAY .1 SEC?
        LDA     ,X                       *EF40: A6 84          '..'    GET A FROM TABLE ($46, $15, $76)
        STA     [PIA2PRA_VEC,PCR]        *EF42: A7 9D F8 C6    '....'  SET PIA 2, PORT A OUTPUTS TO A
        CLRB                             *EF46: 5F             '_'
        PULS    PC,X,D                   *EF47: 35 96          '5.'    RESTORE, RETURN
P_SET_THIS1 PSHS    X,D                      *EF49: 34 16          '4.'
        LDA     #$01                     *EF4B: 86 01          '..'
        STA     P_THIS                   *EF4D: 97 93          '..'
        LDX     P_TABLOC                 *EF4F: 9E 97          '..'
        LDA     #$3E                     *EF51: 86 3E          '.>'
        LDB     P_DEVICE                 *EF53: D6 96          '..'
        CMPB    #$02                     *EF55: C1 02          '..'
        BNE     ZEF5B                    *EF57: 26 02          '&.'
        LDA     #%00101110               *EF59: 86 2E          '..'
ZEF5B   ANDA    #%11111011               *EF5B: 84 FB          '..'    SELECT DDR MASK ...
        STA     [PIA2CRB_VEC,PCR]        *EF5D: A7 9D F8 B1    '....'  ON PIA 2 PORT B
        CLR     [PIA2PRB_VEC,PCR]        *EF61: 6F 9D F8 AB    'o...'  SET TO ALL INPUTS
        ORA     #%00000100               *EF65: 8A 04          '..'    SELECT POR MASK ...
        STA     [PIA2CRB_VEC,PCR]        *EF67: A7 9D F8 A7    '....'  ON PIA 2 PORT B
        LDA     $02,X                    *EF6B: A6 02          '..'    GET A FROM TABLE ($02, $01, $20)
        STA     [PIA2PRA_VEC,PCR]        *EF6D: A7 9D F8 9B    '....'  OUTPUT IT ON PIA 2 PORT A
        LDA     #$4B                     *EF71: 86 4B          '.K'
        BSR     DELAY10MS                *EF73: 8D 56          '.V'    WAIT .075 SEC?
        LDA     [PIA2PRB_VEC,PCR]        *EF75: A6 9D F8 97    '....'  GET DATA  ON PIA 2 PORT B
        PULS    PC,X,D                   *EF79: 35 96          '5.'    RESTORE X,A&B, RTS
P_SET_THIS2 PSHS    X,A                      *EF7B: 34 12          '4.'    SAVE X,A
        LDA     #$02                     *EF7D: 86 02          '..'
        STA     P_THIS                   *EF7F: 97 93          '..'
        LDA     #%00101011               *EF81: 86 2B          '.+'    SELECT DDR
        STA     [PIA2CRB_VEC,PCR]        *EF83: A7 9D F8 8B    '....'  ON PIA 2 PORT B
        LDB     #$FF                     *EF87: C6 FF          '..'    SET TO ALL OUTPUTS
        STB     [PIA2PRB_VEC,PCR]        *EF89: E7 9D F8 83    '....'
        ORA     #%00000100               *EF8D: 8A 04          '..'    SELECT POR
        STA     [PIA2CRB_VEC,PCR]        *EF8F: A7 9D F8 7F    '....'  ON PIA 2 PORT B
        LDX     P_TABLOC                 *EF93: 9E 97          '..'
        LDA     $03,X                    *EF95: A6 03          '..'    GET A FROM TABLE ($CA, $0B, $49)
        STA     [PIA2PRA_VEC,PCR]        *EF97: A7 9D F8 71    '...q'  OUTPUT IT ON PIA 2 PORT A
        LDA     #$32                     *EF9B: 86 32          '.2'
        BSR     DELAY10MS                *EF9D: 8D 2C          '.,'    WAIT .05 SEC?
        PULS    PC,X,A                   *EF9F: 35 92          '5.'    restore X,A, RTS
CHEKOFFS PSHS    X,D                      *EFA1: 34 16          '4.'    store X,D
        STB     P_DEVICE                 *EFA3: D7 96          '..'    SAVE B ($01, $00, OR $05)
        LDA     #$07                     *EFA5: 86 07          '..'
        MUL                              *EFA7: 3D             '='     AxB into D ($07, $00, OR $23)
        LEAX    P_CTRL_TBL,PCR           *EFA8: 30 8D FE 7D    '0..}'  SAVE POINTER TO TABLE IN X
        LEAX    D,X                      *EFAC: 30 8B          '0.'    OFFSET TABLE BY MULTIPLIER IN D
        LDA     $06,X                    *EFAE: A6 06          '..'    OFFSET TABLE BY 6 MORE
        CMPA    P_HEAD                   *EFB0: 91 9D          '..'    MAKE SURE HEAD NO ($14, $17, $18) THERE
        BEQ     SAVEOFFS                 *EFB2: 27 07          ''.'    WE ARE IN THE RIGHT PLACE!
        COMB                             *EFB4: 53             'S'
        LDB     #$03                     *EFB5: C6 03          '..'
        STB     $01,S                    *EFB7: E7 61          '.a'
        BRA     RETOFFS                  *EFB9: 20 0E          ' .'
SAVEOFFS STX     P_TABLOC                 *EFBB: 9F 97          '..'    SAVE TABLE OFFSET IN MEM
        LDD     $04,X                    *EFBD: EC 04          '..'    GET THE SIZE ($08, $08, $20)
        STD     P_DEVSZ                  *EFBF: DD 99          '..'    SAVE SIZE IN MEM
        FCB     $83,$00,$01              *EFC1: 83 00 01       '...'
        STD     P_DEVEND                 *EFC4: DD 9B          '..'    SAVE SIZE-1 (END) IN MEM
        LBSR    ZEF1B                    *EFC6: 17 FF 52       '..R'
RETOFFS PULS    PC,X,D                   *EFC9: 35 96          '5.'    RESTORE, RETURN
DELAY10MS PSHS    X                        *EFCB: 34 10          '4.'
DLY10M1 LDX     #$007D                   *EFCD: 8E 00 7D       '..}'
DELAY8US LEAX    -$01,X                   *EFD0: 30 1F          '0.'
        BNE     DELAY8US                 *EFD2: 26 FC          '&.'
        DECA                             *EFD4: 4A             'J'
        BNE     DLY10M1                  *EFD5: 26 F6          '&.'
        PULS    PC,X                     *EFD7: 35 90          '5.'
        FCB     $00,$00                  *EFD9: 00 00          '..'
* EXTENDED COMMAND DISPATCH TABLE
XCMDTBL FCC     "P"                      *EFDB: 50             'P'
        FCB     $00                      *EFDC: 00             '.'
        FDB     EPROM                    *EFDD: E8 63          '.c'
        FCC     "S"                      *EFDF: 53             'S'
        FCB     $00                      *EFE0: 00             '.'
        FDB     _SETCLK                  *EFE1: E7 00          '..'
        FCC     "T"                      *EFE3: 54             'T'
        FCB     $00                      *EFE4: 00             '.'
        FDB     _PRCLK                   *EFE5: E7 5D          '.]'
        FCB     $00,$00,$FF,$FF,$FF,$FF  *EFE7: 00 00 FF FF FF FF '......'
        FCB     $FF,$FF,$FF,$FF,$FF,$FF  *EFED: FF FF FF FF FF FF '......'
        FCB     $FF,$FF,$FF,$FF,$FF,$FF  *EFF3: FF FF FF FF FF FF '......'
        FCB     $FF,$FF,$FF,$FF,$FF,$FF  *EFF9: FF FF FF FF FF FF '......'
        FCB     $FF                      *EFFF: FF             '.'

* ### ASSEMBLER
        FCB     $AA                      *F000: AA             '.'
        FCC     "U"                      *F001: 55             'U'
        FDB     $F006                    *F002: F0 06          '..'
        FCB     $00,$00                  *F004: 00 00          '..'
        FCC     "DI"                     *F006: 44 49          'DI'
        FCB     $00                      *F008: 00             '.'
        FDB     DICMD                    *F009: F0 1C          '..'
        FCC     "DB"                     *F00B: 44 42          'DB'
        FCB     $00                      *F00D: 00             '.'
        FDB     DBCMD                    *F00E: F0 39          '.9'
        FCC     "XD"                     *F010: 58 44          'XD'
        FCB     $00                      *F012: 00             '.'
        FDB     XDCMD                    *F013: F0 64          '.d'
        FCC     "TD"                     *F015: 54 44          'TD'
        FCB     $00                      *F017: 00             '.'
        FDB     TDCMD                    *F018: F0 60          '.`'
        FCB     $00,$00                  *F01A: 00 00          '..'

* "DI" COMMAND: DISASSEMBLE INSTRUCTION
DICMD   JSR     CHKEOL                   *F01C: BD FE 2A       '..*'
        BEQ     DICM2                    *F01F: 27 0C          ''.'
        JSR     HEX4BIN                  *F021: BD FE BF       '...'
        BCC     DICM1                    *F024: 24 05          '$.'
DIERR   LDA     #$32                     *F026: 86 32          '.2'
        JMP     _NCMDERR                 *F028: 7E FD B2       '~..'
DICM1   STD     M9D23                    *F02B: DD 23          '.#'
DICM2   LDY     #M9F23                   *F02D: 10 8E 9F 23    '...#'
        CLRA                             *F031: 4F             'O'
        CLR     M9D22                    *F032: 0F 22          '."'
        LBSR    ZF0AE                    *F034: 17 00 77       '..w'
        BRA     DBCMR                    *F037: 20 23          ' #'

* "DB" COMMAND: DISASSMEBLE BLOCK OF INSTRUCTIONS
DBCMD   JSR     CHKEOL                   *F039: BD FE 2A       '..*'
        BNE     DBCM1                    *F03C: 26 04          '&.'
        LDD     M9D23                    *F03E: DC 23          '.#'
        BRA     DBCM2                    *F040: 20 05          ' .'
DBCM1   JSR     HEX4BIN                  *F042: BD FE BF       '...'
        BCS     DIERR                    *F045: 25 DF          '%.'
DBCM2   PSHS    D                        *F047: 34 06          '4.'
        LDA     #$16                     *F049: 86 16          '..'
        STA     M9D22                    *F04B: 97 22          '."'
DBCM3   LEAY    ,S                       *F04D: 31 E4          '1.'
        CLRA                             *F04F: 4F             'O'
        LBSR    ZF0AE                    *F050: 17 00 5B       '..['
        STY     ,S                       *F053: 10 AF E4       '...'
        DEC     M9D22                    *F056: 0A 22          '."'
        BPL     DBCM3                    *F058: 2A F3          '*.'
        PULS    Y                        *F05A: 35 20          '5 '
DBCMR   STY     M9D23                    *F05C: 10 9F 23       '..#'
        RTS                              *F05F: 39             '9'
TDCMD   LDA     #$16                     *F060: 86 16          '..'
        STA     M9D22                    *F062: 97 22          '."'
XDCMD   JSR     CHKEOL                   *F064: BD FE 2A       '..*'
        BEQ     ZF06D                    *F067: 27 04          ''.'
        CLR     M9D1E                    *F069: 0F 1E          '..'
        CLR     M9D1F                    *F06B: 0F 1F          '..'
ZF06D   LDX     #SSBK2?X                 *F06D: 8E F0 7A       '..z'
        STX     M9D17                    *F070: 9F 17          '..'
        JMP     _NSSCMD                  *F072: 7E FD B8       '~..'
        LDA     #$31                     *F075: 86 31          '.1'
        JMP     _NCMDERR                 *F077: 7E FD B2       '~..'
SSBK2?X LDA     #$01                     *F07A: 86 01          '..'
* Looks same as SSBK @ FCD1
        STA     TIMER                    *F07C: B7 FF D8       '...'   STOP TIMER
        LDA     #$9F                     *F07F: 86 9F          '..'
        SETDP   $9F

        TFR     A,DP                     *F081: 1F 8B          '..'
        STS     USERSP                   *F083: 10 DF 00       '...'   SAVE USER STACK
        LEAY    $0A,S                    *F086: 31 6A          '1j'
        TST     INBKPT                   *F088: 0D 08          '..'
        BNE     ZF0A4                    *F08A: 26 18          '&.'
        LDS     DBUGSP                   *F08C: 10 DE 02       '...'
        LDA     #$FF                     *F08F: 86 FF          '..'
        BSR     ZF0B2                    *F091: 8D 1F          '..'
        LDA     BYTCNT                   *F093: 96 22          '."'
        BEQ     ZF09C                    *F095: 27 05          ''.'
        DEC     BYTCNT                   *F097: 0A 22          '."'
        JMP     _NSSCMD                  *F099: 7E FD B8       '~..'
ZF09C   LDX     #_NSSBK                  *F09C: 8E FD B5       '...'
        STX     RAM_NMI_VEC              *F09F: 9F 17          '..'
        JMP     _NCOMAND                 *F0A1: 7E FD AF       '~..'
ZF0A4   CLR     BYTCNT                   *F0A4: 0F 22          '."'
        LDX     #_NSSBK                  *F0A6: 8E FD B5       '...'
        STX     RAM_NMI_VEC              *F0A9: 9F 17          '..'
        CLR     INBKPT                   *F0AB: 0F 08          '..'
        RTI                              *F0AD: 3B             ';'
ZF0AE   FCB     $0F,$1E,$0F,$1F          *F0AE: 0F 1E 0F 1F    '....'
ZF0B2   FCB     $97                      *F0B2: 97             '.'
        FCC     ",O_"                    *F0B3: 2C 4F 5F       ',O_'
        FCB     $DD                      *F0B6: DD             '.'
        FCC     "'"                      *F0B7: 27             '''
        FCB     $DD                      *F0B8: DD             '.'
        FCC     ")"                      *F0B9: 29             ')'
        FCB     $0F                      *F0BA: 0F             '.'
        FCC     "+"                      *F0BB: 2B             '+'
        FCB     $9E,$1E,$9F              *F0BC: 9E 1E 9F       '...'
        FCC     " "                      *F0BF: 20             ' '
        FCB     $0F,$1E,$0F,$1F,$9E,$04  *F0C0: 0F 1E 0F 1F 9E 04 '......'
        FCB     $CC                      *F0C6: CC             '.'
        FCC     " H"                     *F0C7: 20 48          ' H'
        FCB     $A7,$80                  *F0C9: A7 80          '..'
        FCC     "Z"                      *F0CB: 5A             'Z'
        FCB     $26,$FB                  *F0CC: 26 FB          '&.'
        FCC     "0"                      *F0CE: 30             '0'
        FCB     $88,$BA,$EE,$A4,$BD,$FF  *F0CF: 88 BA EE A4 BD FF '......'
        FCC     "!"                      *F0D5: 21             '!'
        FCB     $1F                      *F0D6: 1F             '.'
        FCC     "2"                      *F0D7: 32             '2'
        FCB     $A6,$A4,$81,$11          *F0D8: A6 A4 81 11    '....'
        FCC     "':"                     *F0DC: 27 3A          '':'
        FCB     $81,$10                  *F0DE: 81 10          '..'
        FCC     "'Q0"                    *F0E0: 27 51 30       ''Q0'
        FCB     $02,$BD,$FF              *F0E3: 02 BD FF       '...'
        FCC     "'"                      *F0E6: 27             '''
        FCB     $A6                      *F0E7: A6             '.'
        FCC     "?4"                     *F0E8: 3F 34          '?4'
        FCB     $10,$8E,$F4,$B5,$84,$F0  *F0EA: 10 8E F4 B5 84 F0 '......'
        FCC     "DD"                     *F0F0: 44 44          'DD'
        FCB     $EE,$86                  *F0F2: EE 86          '..'
        FCC     "4@LL"                   *F0F4: 34 40 4C 4C    '4@LL'
        FCB     $EE,$86,$E6              *F0F8: EE 86 E6       '...'
        FCC     "?"                      *F0FB: 3F             '?'
        FCB     $C4,$0F,$1F,$98          *F0FC: C4 0F 1F 98    '....'
        FCC     "XX5"                    *F100: 58 58 35       'XX5'
        FCB     $10                      *F103: 10             '.'
        FCC     ":D"                     *F104: 3A 44          ':D'
        FCB     $E6,$C6                  *F106: E6 C6          '..'
        FCC     "%"                      *F108: 25             '%'
        FCB     $04                      *F109: 04             '.'
        FCC     "TTTT"                   *F10A: 54 54 54 54    'TTTT'
        FCB     $C4,$0F                  *F10E: C4 0F          '..'
        FCC     "X"                      *F110: 58             'X'
        FCB     $CE,$F5                  *F111: CE F5          '..'
        FCC     "U"                      *F113: 55             'U'
        FCB     $1E,$13                  *F114: 1E 13          '..'
        FCC     "n"                      *F116: 6E             'n'
        FCB     $95,$BD,$FF              *F117: 95 BD FF       '...'
        BRN     $F0C2                    *F11A: 21 A6          '!.'
        SWI                              *F11C: 3F             '?'
        PSHS    X                        *F11D: 34 10          '4.'
        LDX     #MFDF1                   *F11F: 8E FD F1       '...'
ZF122   CMPA    ,X++                     *F122: A1 81          '..'
        BEQ     ZF12C                    *F124: 27 06          ''.'
        TST     ,X                       *F126: 6D 84          'm.'
        BNE     ZF122                    *F128: 26 F8          '&.'
        BRA     ZF15B                    *F12A: 20 2F          ' /'
ZF12C   LDB     ,-X                      *F12C: E6 82          '..'
        LDX     #MF7F1                   *F12E: 8E F7 F1       '...'
        BRA     ZF163                    *F131: 20 30          ' 0'
        JSR     C4HEX                    *F133: BD FF 21       '..!'
        LDA     -$01,Y                   *F136: A6 3F          '.?'
        PSHS    X                        *F138: 34 10          '4.'
        CMPA    #$21                     *F13A: 81 21          '.!'
        BCS     ZF15B                    *F13C: 25 1D          '%.'
        CMPA    #$30                     *F13E: 81 30          '.0'
        BCC     ZF150                    *F140: 24 0E          '$.'
        TFR     A,B                      *F142: 1F 89          '..'
        ANDB    #$0F                     *F144: C4 0F          '..'
        ASLB                             *F146: 58             'X'
        ASLB                             *F147: 58             'X'
        LDX     #MF795                   *F148: 8E F7 95       '...'
        ABX                              *F14B: 3A             ':'
        LDB     #$0E                     *F14C: C6 0E          '..'
        BRA     ZF16E                    *F14E: 20 1E          ' .'
ZF150   LDX     #MFDC1                   *F150: 8E FD C1       '...'
ZF153   CMPA    ,X++                     *F153: A1 81          '..'
        BEQ     ZF15E                    *F155: 27 07          ''.'
        TST     ,X                       *F157: 6D 84          'm.'
        BNE     ZF153                    *F159: 26 F8          '&.'
ZF15B   LBRA    ZF210                    *F15B: 16 00 B2       '...'
ZF15E   LDB     ,-X                      *F15E: E6 82          '..'
        LDX     #MF7D5                   *F160: 8E F7 D5       '...'
ZF163   PSHS    B                        *F163: 34 04          '4.'
        LSRB                             *F165: 54             'T'
        LSRB                             *F166: 54             'T'
        ANDB    #$FC                     *F167: C4 FC          '..'
        ABX                              *F169: 3A             ':'
        PULS    B                        *F16A: 35 04          '5.'
        ANDB    #$0F                     *F16C: C4 0F          '..'
ZF16E   TFR     X,U                      *F16E: 1F 13          '..'
        LDX     ,S                       *F170: AE E4          '..'
        CMPB    #$09                     *F172: C1 09          '..'
        LBLT    ZF215                    *F174: 10 2D 00 9D    '.-..'
        BEQ     ZF1F6                    *F178: 27 7C          ''|'
        CMPB    #$0B                     *F17A: C1 0B          '..'
        LBLT    ZF2F0                    *F17C: 10 2D 01 70    '.-.p'
        BEQ     ZF1E2                    *F180: 27 60          ''`'
        CMPB    #$0D                     *F182: C1 0D          '..'
        BEQ     ZF1D6                    *F184: 27 50          ''P'
        LBSR    ZF22F                    *F186: 17 00 A6       '...'
        BSR     ZF1F1                    *F189: 8D 66          '.f'
        TFR     Y,D                      *F18B: 1F 20          '. '
        ADDD    -$02,Y                   *F18D: E3 3E          '.>'
ZF18F   JSR     BIN4HS                   *F18F: BD FF 23       '..#'
ZF192   LEAS    $02,S                    *F192: 32 62          '2b'
        LDU     M9F20                    *F194: DE 20          '. '
        BEQ     ZF1B2                    *F196: 27 1A          ''.'
        LDX     #M9F79                   *F198: 8E 9F 79       '..y'
        LDA     #$28                     *F19B: 86 28          '.('
        STA     ,X+                      *F19D: A7 80          '..'
        LDD     M9F20                    *F19F: DC 20          '. '
        TFR     D,U                      *F1A1: 1F 03          '..'
        JSR     BIN4HS                   *F1A3: BD FF 23       '..#'
        LDD     #M293D                   *F1A6: CC 29 3D       '.)='
        STD     ,-X                      *F1A9: ED 82          '..'
        LEAX    $02,X                    *F1AB: 30 02          '0.'
        LDD     ,U                       *F1AD: EC C4          '..'
        JSR     BIN4HS                   *F1AF: BD FF 23       '..#'
ZF1B2   LDX     #M9F87                   *F1B2: 8E 9F 87       '...'
        LDU     #MF47D                   *F1B5: CE F4 7D       '..}'
        LBSR    ZFF73                    *F1B8: 17 0D B8       '...'
        LDU     USERSP                   *F1BB: DE 00          '..'
        LDB     ,U                       *F1BD: E6 C4          '..'
        LDU     #SFLGS                   *F1BF: CE F4 75       '..u'
ZF1C2   ASLB                             *F1C2: 58             'X'     CHECK FLAG VIA CARRY BIT
        LDA     ,U+                      *F1C3: A6 C0          '..'
        BCC     ZF1C9                    *F1C5: 24 02          '$.'
        STA     ,X+                      *F1C7: A7 80          '..'
ZF1C9   TSTB                             *F1C9: 5D             ']'
        BNE     ZF1C2                    *F1CA: 26 F6          '&.'    BRANCH IF NOT DONE
        TST     M9F2C                    *F1CC: 0D 2C          '.,'
        BNE     ZF1D3                    *F1CE: 26 03          '&.'
        LDX     #M9F6E                   *F1D0: 8E 9F 6E       '..n'
ZF1D3   JMP     ZFF52                    *F1D3: 7E FF 52       '~.R'
ZF1D6   BSR     ZF22F                    *F1D6: 8D 57          '.W'
        LDA     #'#                      *F1D8: 86 23          '.#'
        STA     ,X+                      *F1DA: A7 80          '..'
        BSR     ZF1F1                    *F1DC: 8D 13          '..'
        LDD     -$02,Y                   *F1DE: EC 3E          '.>'
ZF1E0   BRA     ZF18F                    *F1E0: 20 AD          ' .'
ZF1E2   BSR     ZF22F                    *F1E2: 8D 4B          '.K'
        LDU     -$02,Y                   *F1E4: EE 3E          '.>'
        STU     M9F27                    *F1E6: DF 27          '.''
        BSR     ZF1F1                    *F1E8: 8D 07          '..'
        TFR     U,D                      *F1EA: 1F 30          '.0'
        JSR     BIN4HS                   *F1EC: BD FF 23       '..#'
        BRA     ZF207                    *F1EF: 20 16          ' .'
ZF1F1   LDA     #'$                      *F1F1: 86 24          '.$'
        STA     ,X+                      *F1F3: A7 80          '..'
        RTS                              *F1F5: 39             '9'
ZF1F6   BSR     ZF23D                    *F1F6: 8D 45          '.E'
        BSR     ZF1F1                    *F1F8: 8D F7          '..'
        LDB     -$01,Y                   *F1FA: E6 3F          '.?'
        STB     M9F28                    *F1FC: D7 28          '.('
        JSR     BIN2HS                   *F1FE: BD FF 29       '..)'
        LDU     USERSP                   *F201: DE 00          '..'
        LDA     $03,U                    *F203: A6 43          '.C'
        STA     M9F27                    *F205: 97 27          '.''
ZF207   LBSR    ZF28D                    *F207: 17 00 83       '...'
        LDD     M9F27                    *F20A: DC 27          '.''
ZF20C   STD     M9F1E                    *F20C: DD 1E          '..'
        BRA     ZF1E0                    *F20E: 20 D0          ' .'
ZF210   LDU     #MF4A1                   *F210: CE F4 A1       '...'
        BRA     ZF217                    *F213: 20 02          ' .'
ZF215   LDX     ,S                       *F215: AE E4          '..'
ZF217   BSR     ZF234                    *F217: 8D 1B          '..'
ZF219   LBRA    ZF192                    *F219: 16 FF 76       '..v'
        BSR     ZF23D                    *F21C: 8D 1F          '..'
        LDU     #SFLGS                   *F21E: CE F4 75       '..u'
        LDB     -$01,Y                   *F221: E6 3F          '.?'
ZF223   ASLB                             *F223: 58             'X'
        LDA     ,U+                      *F224: A6 C0          '..'
        BCC     ZF22A                    *F226: 24 02          '$.'
        STA     ,X+                      *F228: A7 80          '..'
ZF22A   TSTB                             *F22A: 5D             ']'
        BNE     ZF223                    *F22B: 26 F6          '&.'
ZF22D   BRA     ZF219                    *F22D: 20 EA          ' .'
ZF22F   LDX     $02,S                    *F22F: AE 62          '.b'
        JSR     C4HEX                    *F231: BD FF 21       '..!'
ZF234   LDX     #M9F5B                   *F234: 8E 9F 5B       '..['
        LBSR    ZFF73                    *F237: 17 0D 39       '..9'
        LEAX    $01,X                    *F23A: 30 01          '0.'
        RTS                              *F23C: 39             '9'
ZF23D   LDX     $02,S                    *F23D: AE 62          '.b'
        JSR     C2HEX                    *F23F: BD FF 27       '..''
        BRA     ZF234                    *F242: 20 F0          ' .'
        BSR     ZF23D                    *F244: 8D F7          '..'
        LDU     #MF495                   *F246: CE F4 95       '...'
        LDB     -$01,Y                   *F249: E6 3F          '.?'
        LSRB                             *F24B: 54             'T'
        LSRB                             *F24C: 54             'T'
        LSRB                             *F24D: 54             'T'
        LSRB                             *F24E: 54             'T'
        ASLB                             *F24F: 58             'X'
        LDD     B,U                      *F250: EC C5          '..'
        STA     ,X+                      *F252: A7 80          '..'
        CMPB    #$20                     *F254: C1 20          '. '
        BEQ     ZF25A                    *F256: 27 02          ''.'
        STB     ,X+                      *F258: E7 80          '..'
ZF25A   LDA     #$2C                     *F25A: 86 2C          '.,'
        STA     ,X+                      *F25C: A7 80          '..'
        LDB     -$01,Y                   *F25E: E6 3F          '.?'
        ANDB    #$0F                     *F260: C4 0F          '..'
        ASLB                             *F262: 58             'X'
        LDD     B,U                      *F263: EC C5          '..'
        STA     ,X+                      *F265: A7 80          '..'
        CMPB    #$20                     *F267: C1 20          '. '
        BEQ     ZF26D                    *F269: 27 02          ''.'
        STB     ,X+                      *F26B: E7 80          '..'
ZF26D   BRA     ZF22D                    *F26D: 20 BE          ' .'
        BSR     ZF23D                    *F26F: 8D CC          '..'
        LDA     -$02,Y                   *F271: A6 3E          '.>'
        CMPA    #$8D                     *F273: 81 8D          '..'
        BNE     ZF27B                    *F275: 26 04          '&.'
        LDA     #$42                     *F277: 86 42          '.B'
        STA     -$05,X                   *F279: A7 1B          '..'
ZF27B   LBSR    ZF1F1                    *F27B: 17 FF 73       '..s'
        PSHS    Y                        *F27E: 34 20          '4 '
        CLRA                             *F280: 4F             'O'
        LDB     -$01,Y                   *F281: E6 3F          '.?'
        BPL     ZF286                    *F283: 2A 01          '*.'
        COMA                             *F285: 43             'C'
ZF286   ADDD    ,S                       *F286: E3 E4          '..'
        LEAS    $02,S                    *F288: 32 62          '2b'
        LBRA    ZF18F                    *F28A: 16 FF 02       '...'
ZF28D   LDX     #M9F6F                   *F28D: 8E 9F 6F       '..o'
        LDU     #MF481                   *F290: CE F4 81       '...'
        LBRA    ZFF73                    *F293: 16 0C DD       '...'
        BSR     ZF23D                    *F296: 8D A5          '..'
        LDU     #MF485                   *F298: CE F4 85       '...'
        LDB     -$01,Y                   *F29B: E6 3F          '.?'
        PSHS    B                        *F29D: 34 04          '4.'
        LDD     ,U++                     *F29F: EC C1          '..'
        ASL     ,S                       *F2A1: 68 E4          'h.'
        BCC     ZF2AF                    *F2A3: 24 0A          '$.'
        STD     ,X++                     *F2A5: ED 81          '..'
        TST     ,S                       *F2A7: 6D E4          'm.'
        BEQ     ZF2DD                    *F2A9: 27 32          ''2'
        LDA     #$2C                     *F2AB: 86 2C          '.,'
        STA     ,X+                      *F2AD: A7 80          '..'
ZF2AF   LEAU    $02,U                    *F2AF: 33 42          '3B'
        ASL     ,S                       *F2B1: 68 E4          'h.'
        BCC     ZF2CD                    *F2B3: 24 18          '$.'
        LDA     -$02,Y                   *F2B5: A6 3E          '.>'
        CMPA    #$35                     *F2B7: 81 35          '.5'
        BLS     ZF2C1                    *F2B9: 23 06          '#.'
        LDD     -$02,U                   *F2BB: EC 5E          '.^'
        STA     ,X+                      *F2BD: A7 80          '..'
        BRA     ZF2C5                    *F2BF: 20 04          ' .'
ZF2C1   LDD     -$02,U                   *F2C1: EC 5E          '.^'
        STB     ,X+                      *F2C3: E7 80          '..'
ZF2C5   TST     ,S                       *F2C5: 6D E4          'm.'
        BEQ     ZF2DD                    *F2C7: 27 14          ''.'
        LDA     #$2C                     *F2C9: 86 2C          '.,'
        STA     ,X+                      *F2CB: A7 80          '..'
ZF2CD   LDD     ,U++                     *F2CD: EC C1          '..'
        ASL     ,S                       *F2CF: 68 E4          'h.'
        BCC     ZF2CD                    *F2D1: 24 FA          '$.'
        STA     ,X+                      *F2D3: A7 80          '..'
        CMPB    #$20                     *F2D5: C1 20          '. '
        BEQ     ZF2C5                    *F2D7: 27 EC          ''.'
        STB     ,X+                      *F2D9: E7 80          '..'
        BRA     ZF2C5                    *F2DB: 20 E8          ' .'
ZF2DD   LEAS    $01,S                    *F2DD: 32 61          '2a'
ZF2DF   BRA     ZF26D                    *F2DF: 20 8C          ' .'
        LBSR    ZF23D                    *F2E1: 17 FF 59       '..Y'
        LEAY    -$01,Y                   *F2E4: 31 3F          '1?'
        LDD     #M2324                   *F2E6: CC 23 24       '.#$'
        STD     ,X++                     *F2E9: ED 81          '..'
        JSR     C2HEX                    *F2EB: BD FF 27       '..''
        BRA     ZF2DF                    *F2EE: 20 EF          ' .'
ZF2F0   LDX     ,S                       *F2F0: AE E4          '..'
        JSR     C2HEX                    *F2F2: BD FF 27       '..''
        LDB     -$01,Y                   *F2F5: E6 3F          '.?'
        STB     M9F2D                    *F2F7: D7 2D          '.-'
        PSHS    B                        *F2F9: 34 04          '4.'
        BMI     ZF35B                    *F2FB: 2B 5E          '+^'
        LBSR    ZF234                    *F2FD: 17 FF 34       '..4'
        LDB     ,S                       *F300: E6 E4          '..'
        ANDB    #$1F                     *F302: C4 1F          '..'
        STB     M9F28                    *F304: D7 28          '.('
        BITB    #$10                     *F306: C5 10          '..'
        BEQ     ZF314                    *F308: 27 0A          ''.'
        ORB     #$F0                     *F30A: CA F0          '..'
        STB     M9F28                    *F30C: D7 28          '.('
        NEGB                             *F30E: 50             'P'
        COM     M9F27                    *F30F: 03 27          '.''
        LBSR    ZF403                    *F311: 17 00 EF       '...'
ZF314   LBSR    ZF1F1                    *F314: 17 FE DA       '...'
        JSR     BIN2HS                   *F317: BD FF 29       '..)'
        LDA     #$2C                     *F31A: 86 2C          '.,'
        STA     -$01,X                   *F31C: A7 1F          '..'
        PULS    A                        *F31E: 35 02          '5.'
        BSR     ZF344                    *F320: 8D 22          '."'
        STB     ,X+                      *F322: E7 80          '..'
        STA     M9F2A                    *F324: 97 2A          '.*'
ZF326   LBSR    ZF28D                    *F326: 17 FF 64       '..d'
        LDU     USERSP                   *F329: DE 00          '..'
        LDA     M9F2A                    *F32B: 96 2A          '.*'
        BNE     ZF335                    *F32D: 26 06          '&.'
        LEAU    $0C,U                    *F32F: 33 4C          '3L'
        TFR     U,D                      *F331: 1F 30          '.0'
        BRA     ZF337                    *F333: 20 02          ' .'
ZF335   LDD     A,U                      *F335: EC C6          '..'
ZF337   ADDD    M9F27                    *F337: D3 27          '.''
        TST     M9F2B                    *F339: 0D 2B          '.+'
        BEQ     ZF341                    *F33B: 27 04          ''.'
        TFR     D,U                      *F33D: 1F 03          '..'
        LDD     ,U                       *F33F: EC C4          '..'
ZF341   LBRA    ZF20C                    *F341: 16 FE C8       '...'
ZF344   PSHS    X                        *F344: 34 10          '4.'
        LSRA                             *F346: 44             'D'
        LSRA                             *F347: 44             'D'
        LSRA                             *F348: 44             'D'
        LSRA                             *F349: 44             'D'
        ANDA    #$06                     *F34A: 84 06          '..'
        LDX     #MF353                   *F34C: 8E F3 53       '..S'
        LDD     A,X                      *F34F: EC 86          '..'
        PULS    PC,X                     *F351: 35 90          '5.'
MF353   LSR     M9F58                    *F353: 04 58          '.X'
        ROR     M9F59                    *F355: 06 59          '.Y'
        ASL     M9F55                    *F357: 08 55          '.U'
        NEG     M9F53                    *F359: 00 53          '.S'
ZF35B   ASLB                             *F35B: 58             'X'
        ANDB    #$1F                     *F35C: C4 1F          '..'
        PSHS    U                        *F35E: 34 40          '4@'
        LDU     #MF575                   *F360: CE F5 75       '..u'
        JMP     [B,U]                    *F363: 6E D5          'n.'
        PULS    U                        *F365: 35 40          '5@'
        LEAS    $01,S                    *F367: 32 61          '2a'
        LBSR    ZF46C                    *F369: 17 01 00       '...'
        LBSR    ZF1F1                    *F36C: 17 FE 82       '...'
        LDD     -$02,Y                   *F36F: EC 3E          '.>'
        JSR     BIN4HS                   *F371: BD FF 23       '..#'
        LBSR    ZF28D                    *F374: 17 FF 16       '...'
        LDU     M9F27                    *F377: DE 27          '.''
        LDD     ,U                       *F379: EC C4          '..'
        BRA     ZF341                    *F37B: 20 C4          ' .'
        LBSR    ZF418                    *F37D: 17 00 98       '...'
        STB     ,X+                      *F380: E7 80          '..'
        LDA     #$2B                     *F382: 86 2B          '.+'
ZF384   STA     ,X+                      *F384: A7 80          '..'
ZF386   LEAS    $03,S                    *F386: 32 63          '2c'
        LBRA    ZF326                    *F388: 16 FF 9B       '...'
        LBSR    ZF418                    *F38B: 17 00 8A       '...'
        STB     ,X+                      *F38E: E7 80          '..'
        LDA     #$2B                     *F390: 86 2B          '.+'
        STA     ,X+                      *F392: A7 80          '..'
        BRA     ZF384                    *F394: 20 EE          ' .'
        LBSR    ZF418                    *F396: 17 00 7F       '...'
ZF399   BSR     ZF403                    *F399: 8D 68          '.h'
        DEC     M9F28                    *F39B: 0A 28          '.('
        COM     M9F27                    *F39D: 03 27          '.''
ZF39F   STB     ,X+                      *F39F: E7 80          '..'
        BRA     ZF386                    *F3A1: 20 E3          ' .'
        BSR     ZF418                    *F3A3: 8D 73          '.s'
        BSR     ZF403                    *F3A5: 8D 5C          '.\'
        DEC     M9F28                    *F3A7: 0A 28          '.('
        BRA     ZF399                    *F3A9: 20 EE          ' .'
        BSR     ZF418                    *F3AB: 8D 6B          '.k'
        BRA     ZF39F                    *F3AD: 20 F0          ' .'
        LDD     #M4202                   *F3AF: CC 42 02       '.B.'
ZF3B2   LDU     ,S                       *F3B2: EE E4          '..'
        STB     M9F29                    *F3B4: D7 29          '.)'
        BSR     ZF408                    *F3B6: 8D 50          '.P'
        STA     ,X+                      *F3B8: A7 80          '..'
        BSR     ZF41C                    *F3BA: 8D 60          '.`'
        STB     ,X+                      *F3BC: E7 80          '..'
        LDU     USERSP                   *F3BE: DE 00          '..'
        LDA     M9F29                    *F3C0: 96 29          '.)'
        BPL     ZF3CB                    *F3C2: 2A 07          '*.'
        NEGA                             *F3C4: 40             '@'
        LDD     A,U                      *F3C5: EC C6          '..'
        STD     M9F27                    *F3C7: DD 27          '.''
        BRA     ZF3D3                    *F3C9: 20 08          ' .'
ZF3CB   LDA     A,U                      *F3CB: A6 C6          '..'
        BPL     ZF3D1                    *F3CD: 2A 02          '*.'
        COM     M9F27                    *F3CF: 03 27          '.''
ZF3D1   STA     M9F28                    *F3D1: 97 28          '.('
ZF3D3   BRA     ZF386                    *F3D3: 20 B1          ' .'
        LDD     #M4101                   *F3D5: CC 41 01       '.A.'
        BRA     ZF3B2                    *F3D8: 20 D8          ' .'
        LDD     #M44FF                   *F3DA: CC 44 FF       '.D.'
        BRA     ZF3B2                    *F3DD: 20 D3          ' .'
        BSR     ZF433                    *F3DF: 8D 52          '.R'
        LDB     #$03                     *F3E1: C6 03          '..'
ZF3E3   CLRA                             *F3E3: 4F             'O'
        ADDD    M9F27                    *F3E4: D3 27          '.''
        STD     M9F27                    *F3E6: DD 27          '.''
        LDD     #M5043                   *F3E8: CC 50 43       '.PC'
        STD     ,X++                     *F3EB: ED 81          '..'
        LDA     #$0A                     *F3ED: 86 0A          '..'
        STA     M9F2A                    *F3EF: 97 2A          '.*'
ZF3F1   BRA     ZF386                    *F3F1: 20 93          ' .'
        BSR     ZF44F                    *F3F3: 8D 5A          '.Z'
        LDB     #$04                     *F3F5: C6 04          '..'
        BRA     ZF3E3                    *F3F7: 20 EA          ' .'
        BSR     ZF433                    *F3F9: 8D 38          '.8'
ZF3FB   STB     ,X+                      *F3FB: E7 80          '..'
        BRA     ZF3F1                    *F3FD: 20 F2          ' .'
        BSR     ZF44F                    *F3FF: 8D 4E          '.N'
        BRA     ZF3FB                    *F401: 20 F8          ' .'
ZF403   LDA     #$2D                     *F403: 86 2D          '.-'
        STA     ,X+                      *F405: A7 80          '..'
        RTS                              *F407: 39             '9'
ZF408   LBSR    ZF234                    *F408: 17 FE 29       '..)'
        LDB     M9F2D                    *F40B: D6 2D          '.-'
        BITB    #$10                     *F40D: C5 10          '..'
        BEQ     ZF417                    *F40F: 27 06          ''.'
        LDB     #$5B                     *F411: C6 5B          '.['
        STB     ,X+                      *F413: E7 80          '..'
        COM     M9F2B                    *F415: 03 2B          '.+'
ZF417   RTS                              *F417: 39             '9'
ZF418   LDU     $02,S                    *F418: EE 62          '.b'
        BSR     ZF408                    *F41A: 8D EC          '..'
ZF41C   LDA     #$2C                     *F41C: 86 2C          '.,'
        STA     ,X+                      *F41E: A7 80          '..'
        LDA     M9F2D                    *F420: 96 2D          '.-'
        LBSR    ZF344                    *F422: 17 FF 1F       '...'
        STA     M9F2A                    *F425: 97 2A          '.*'
        RTS                              *F427: 39             '9'
        LDU     ,S                       *F428: EE E4          '..'
        BSR     ZF408                    *F42A: 8D DC          '..'
        LDD     #M2A2A                   *F42C: CC 2A 2A       '.**'
        STD     ,X++                     *F42F: ED 81          '..'
        BRA     ZF3D3                    *F431: 20 A0          ' .'
ZF433   LDU     $02,S                    *F433: EE 62          '.b'
        JSR     C2HEX                    *F435: BD FF 27       '..''
        BSR     ZF408                    *F438: 8D CE          '..'
        LDB     -$01,Y                   *F43A: E6 3F          '.?'
        STB     M9F28                    *F43C: D7 28          '.('
        BPL     ZF445                    *F43E: 2A 05          '*.'
        COM     M9F27                    *F440: 03 27          '.''
        NEGB                             *F442: 50             'P'
        BSR     ZF403                    *F443: 8D BE          '..'
ZF445   LBSR    ZF1F1                    *F445: 17 FD A9       '...'
        JSR     BIN2HS                   *F448: BD FF 29       '..)'
ZF44B   LEAX    -$01,X                   *F44B: 30 1F          '0.'
        BRA     ZF41C                    *F44D: 20 CD          ' .'
ZF44F   LDU     $02,S                    *F44F: EE 62          '.b'
        BSR     ZF46C                    *F451: 8D 19          '..'
        LDD     -$02,Y                   *F453: EC 3E          '.>'
        PSHS    A                        *F455: 34 02          '4.'
        BPL     ZF462                    *F457: 2A 09          '*.'
        COMA                             *F459: 43             'C'
        COMB                             *F45A: 53             'S'
        ADDD    #$0001                   *F45B: C3 00 01       '...'
        STA     ,S                       *F45E: A7 E4          '..'
        BSR     ZF403                    *F460: 8D A1          '..'
ZF462   LBSR    ZF1F1                    *F462: 17 FD 8C       '...'
        PULS    A                        *F465: 35 02          '5.'
        JSR     BIN4HS                   *F467: BD FF 23       '..#'
        BRA     ZF44B                    *F46A: 20 DF          ' .'
ZF46C   LDD     ,Y                       *F46C: EC A4          '..'
        STD     M9F27                    *F46E: DD 27          '.''
        JSR     C4HEX                    *F470: BD FF 21       '..!'
        BRA     ZF408                    *F473: 20 93          ' .'
SFLGS   FCC     "EFHINZVC"               *F475: 45 46 48 49 4E 5A 56 43 'EFHINZVC' CONDITION CODE LABELS
MF47D   FCC     "CC= "                   *F47D: 43 43 3D 20    'CC= '
MF481   FCC     "EA=$"                   *F481: 45 41 3D 24    'EA=$'
MF485   FCC     "PCSUY X DPB A CC"       *F485: 50 43 53 55 59 20 58 20 44 50 42 20 41 20 43 43 'PCSUY X DPB A CC'
MF495   FCC     "D X Y U S PC"           *F495: 44 20 58 20 59 20 55 20 53 20 50 43 'D X Y U S PC'
MF4A1   FCC     "????A B CCDP????????"   *F4A1: 3F 3F 3F 3F 41 20 42 20 43 43 44 50 3F 3F 3F 3F 3F 3F 3F 3F '????A B CCDP????????'
        FCB     $F6,$D5,$F4,$F5,$F5,$95  *F4B5: F6 D5 F4 F5 F5 95 '......'
        FCB     $F4,$FD,$F5,$D5,$F5,$05  *F4BB: F4 FD F5 D5 F5 05 '......'
        FCB     $F6,$15,$F5,$0D,$F6      *F4C1: F6 15 F5 0D F6 '.....'
        FCC     "U"                      *F4C6: 55             'U'
        FCB     $F5,$15,$F6,$95,$F5,$15  *F4C7: F5 15 F6 95 F5 15 '......'
        FCB     $F6,$D5,$F5,$1D,$F6,$D5  *F4CD: F6 D5 F5 1D F6 D5 '......'
        FCB     $F5                      *F4D3: F5             '.'
        FCC     "%"                      *F4D4: 25             '%'
        FCB     $F7,$15,$F5              *F4D5: F7 15 F5       '...'
        FCC     "-"                      *F4D8: 2D             '-'
        FCB     $F7,$15,$F5              *F4D9: F7 15 F5       '...'
        FCC     "5"                      *F4DC: 35             '5'
        FCB     $F7,$15,$F5              *F4DD: F7 15 F5       '...'
        FCC     "="                      *F4E0: 3D             '='
        FCB     $F7,$15,$F5              *F4E1: F7 15 F5       '...'
        FCC     "E"                      *F4E4: 45             'E'
        FCB     $F7                      *F4E5: F7             '.'
        FCC     "U"                      *F4E6: 55             'U'
        FCB     $F5                      *F4E7: F5             '.'
        FCC     "M"                      *F4E8: 4D             'M'
        FCB     $F7                      *F4E9: F7             '.'
        FCC     "U"                      *F4EA: 55             'U'
        FCB     $F5                      *F4EB: F5             '.'
        FCC     "5"                      *F4EC: 35             '5'
        FCB     $F7                      *F4ED: F7             '.'
        FCC     "U"                      *F4EE: 55             'U'
        FCB     $F5                      *F4EF: F5             '.'
        FCC     "="                      *F4F0: 3D             '='
        FCB     $F7                      *F4F1: F7             '.'
        FCC     "U"                      *F4F2: 55             'U'
        FCB     $F5                      *F4F3: F5             '.'
        FCC     "E"                      *F4F4: 45             'E'
        FCB     $90,$09,$90,$99,$99,$90  *F4F5: 90 09 90 99 99 90 '......'
        FCB     $99,$99,$00,$11,$00,$EE  *F4FB: 99 99 00 11 00 EE '......'
        FCB     $01                      *F501: 01             '.'
        FCC     " !4UUUUUUUUffww"        *F502: 20 21 34 55 55 55 55 55 55 55 55 66 66 77 77 ' !4UUUUUUUUffww'
        FCB     $01,$11,$81,$01,$10,$01  *F511: 01 11 81 01 10 01 '......'
        FCB     $10,$11,$11,$10,$11,$01  *F517: 10 11 11 10 11 01 '......'
        FCB     $A0,$0A,$A0,$AA,$AA,$A0  *F51D: A0 0A A0 AA AA A0 '......'
        FCB     $AA,$AA,$B0,$0B,$B0,$BB  *F523: AA AA B0 0B B0 BB '......'
        FCB     $BB,$B0,$BB,$BB,$CC,$CD  *F529: BB B0 BB BB CC CD '......'
        FCB     $CC,$C0,$CC,$CC,$D5,$D0  *F52F: CC C0 CC CC D5 D0 '......'
        FCB     $99,$99,$99,$99,$99,$99  *F535: 99 99 99 99 99 99 '......'
        FCB     $99,$99,$AA,$AA,$AA,$AA  *F53B: 99 99 AA AA AA AA '......'
        FCB     $AA,$AA,$AA,$AA,$BB,$BB  *F541: AA AA AA AA BB BB '......'
        FCB     $BB,$BB,$BB,$BB,$BB,$BB  *F547: BB BB BB BB BB BB '......'
        FCB     $CC,$CD,$CC,$C0,$CC,$CC  *F54D: CC CD CC C0 CC CC '......'
        FCB     $D0,$D0,$F2,$10,$F2,$15  *F553: D0 D0 F2 10 F2 15 '......'
        FCB     $F2,$1C,$F2              *F559: F2 1C F2       '...'
        FCC     "D"                      *F55C: 44             'D'
        FCB     $F2                      *F55D: F2             '.'
        FCC     "D"                      *F55E: 44             'D'
        FCB     $F2                      *F55F: F2             '.'
        FCC     "o"                      *F560: 6F             'o'
        FCB     $F2,$F0,$F2,$96,$F2,$1C  *F561: F2 F0 F2 96 F2 1C '......'
        FCB     $F1,$F6,$F2,$F0,$F1,$E2  *F567: F1 F6 F2 F0 F1 E2 '......'
        FCB     $F2,$E1,$F1,$D6,$F1,$86  *F56D: F2 E1 F1 D6 F1 86 '......'
        FCB     $F2,$10                  *F573: F2 10          '..'
MF575   FCB     $F3                      *F575: F3             '.'
        FCC     "}"                      *F576: 7D             '}'
        FCB     $F3,$8B,$F3,$96,$F3,$A3  *F577: F3 8B F3 96 F3 A3 '......'
        FCB     $F3,$AB,$F3,$AF,$F3,$D5  *F57D: F3 AB F3 AF F3 D5 '......'
        FCB     $F4                      *F583: F4             '.'
        FCC     "("                      *F584: 28             '('
        FCB     $F3,$F9,$F3,$FF,$F4      *F585: F3 F9 F3 FF F4 '.....'
        FCC     "("                      *F58A: 28             '('
        FCB     $F3,$DA,$F3,$DF,$F3,$F3  *F58B: F3 DA F3 DF F3 F3 '......'
        FCB     $F4                      *F591: F4             '.'
        FCC     "("                      *F592: 28             '('
        FCB     $F3                      *F593: F3             '.'
        FCC     "e*** *** NOP SYNC*** "  *F594: 65 2A 2A 2A 20 2A 2A 2A 20 4E 4F 50 20 53 59 4E 43 2A 2A 2A 20 'e*** *** NOP SYNC*** '
        FCC     "*** LBRALBSR*** DAA "   *F5A9: 2A 2A 2A 20 4C 42 52 41 4C 42 53 52 2A 2A 2A 20 44 41 41 20 '*** LBRALBSR*** DAA '
ZF5BD   FCC     "ORCC*** ANDCSEX EXG T"  *F5BD: 4F 52 43 43 2A 2A 2A 20 41 4E 44 43 53 45 58 20 45 58 47 20 54 'ORCC*** ANDCSEX EXG T'
        FCC     "FR BRA BRN BHI BLS BC"  *F5D2: 46 52 20 42 52 41 20 42 52 4E 20 42 48 49 20 42 4C 53 20 42 43 'FR BRA BRN BHI BLS BC'
        FCC     "C BCS BNE BEQ BVC BVS"  *F5E7: 43 20 42 43 53 20 42 4E 45 20 42 45 51 20 42 56 43 20 42 56 53 'C BCS BNE BEQ BVC BVS'
        FCC     " BPL BMI BGE BLT BGT "  *F5FC: 20 42 50 4C 20 42 4D 49 20 42 47 45 20 42 4C 54 20 42 47 54 20 ' BPL BMI BGE BLT BGT '
        FCC     "BLE LEAXLEAYLEASLEAUP"  *F611: 42 4C 45 20 4C 45 41 58 4C 45 41 59 4C 45 41 53 4C 45 41 55 50 'BLE LEAXLEAYLEASLEAUP'
        FCC     "SHSPULSPSHUPULU*** RT"  *F626: 53 48 53 50 55 4C 53 50 53 48 55 50 55 4C 55 2A 2A 2A 20 52 54 'SHSPULSPSHUPULU*** RT'
        FCC     "S ABX RTI CWAIMUL ***"  *F63B: 53 20 41 42 58 20 52 54 49 20 43 57 41 49 4D 55 4C 20 2A 2A 2A 'S ABX RTI CWAIMUL ***'
        FCC     " SWI NEGA*** *** COMA"  *F650: 20 53 57 49 20 4E 45 47 41 2A 2A 2A 20 2A 2A 2A 20 43 4F 4D 41 ' SWI NEGA*** *** COMA'
        FCC     "LSRA*** RORAASRAASLAR"  *F665: 4C 53 52 41 2A 2A 2A 20 52 4F 52 41 41 53 52 41 41 53 4C 41 52 'LSRA*** RORAASRAASLAR'
        FCC     "OLADECA*** INCATSTA**"  *F67A: 4F 4C 41 44 45 43 41 2A 2A 2A 20 49 4E 43 41 54 53 54 41 2A 2A 'OLADECA*** INCATSTA**'
        FCC     "* CLRANEGB*** *** COM"  *F68F: 2A 20 43 4C 52 41 4E 45 47 42 2A 2A 2A 20 2A 2A 2A 20 43 4F 4D '* CLRANEGB*** *** COM'
        FCC     "BLSRB*** RORBASRBASLB"  *F6A4: 42 4C 53 52 42 2A 2A 2A 20 52 4F 52 42 41 53 52 42 41 53 4C 42 'BLSRB*** RORBASRBASLB'
        FCC     "ROLBDECB*** INCBTSTB*"  *F6B9: 52 4F 4C 42 44 45 43 42 2A 2A 2A 20 49 4E 43 42 54 53 54 42 2A 'ROLBDECB*** INCBTSTB*'
        FCC     "** CLRBNEG *** *** CO"  *F6CE: 2A 2A 20 43 4C 52 42 4E 45 47 20 2A 2A 2A 20 2A 2A 2A 20 43 4F '** CLRBNEG *** *** CO'
        FCC     "M LSR *** ROR ASR ASL"  *F6E3: 4D 20 4C 53 52 20 2A 2A 2A 20 52 4F 52 20 41 53 52 20 41 53 4C 'M LSR *** ROR ASR ASL'
        FCC     " ROL DEC *** INC TST "  *F6F8: 20 52 4F 4C 20 44 45 43 20 2A 2A 2A 20 49 4E 43 20 54 53 54 20 ' ROL DEC *** INC TST '
        FCC     "JMP CLR SUBACMPASBCAS"  *F70D: 4A 4D 50 20 43 4C 52 20 53 55 42 41 43 4D 50 41 53 42 43 41 53 'JMP CLR SUBACMPASBCAS'
        FCC     "UBDANDABITALDA STA EO"  *F722: 55 42 44 41 4E 44 41 42 49 54 41 4C 44 41 20 53 54 41 20 45 4F 'UBDANDABITALDA STA EO'
        FCC     "RAADCAORA ADDACMPXJSR"  *F737: 52 41 41 44 43 41 4F 52 41 20 41 44 44 41 43 4D 50 58 4A 53 52 'RAADCAORA ADDACMPXJSR'
        FCC     " LDX STX SUBBCMPBSBCB"  *F74C: 20 4C 44 58 20 53 54 58 20 53 55 42 42 43 4D 50 42 53 42 43 42 ' LDX STX SUBBCMPBSBCB'
        FCC     "ADDDANDBBITBLDB STB E"  *F761: 41 44 44 44 41 4E 44 42 42 49 54 42 4C 44 42 20 53 54 42 20 45 'ADDDANDBBITBLDB STB E'
        FCC     "ORBADCBORB ADDBLDD ST"  *F776: 4F 52 42 41 44 43 42 4F 52 42 20 41 44 44 42 4C 44 44 20 53 54 'ORBADCBORB ADDBLDD ST'
        FCC     "D LDU STU "             *F78B: 44 20 4C 44 55 20 53 54 55 20 'D LDU STU '
MF795   FCC     "*** LBRNLBHILBLSLBCCL"  *F795: 2A 2A 2A 20 4C 42 52 4E 4C 42 48 49 4C 42 4C 53 4C 42 43 43 4C '*** LBRNLBHILBLSLBCCL'
        FCC     "BCSLBNELBEQLBVCLBVSLB"  *F7AA: 42 43 53 4C 42 4E 45 4C 42 45 51 4C 42 56 43 4C 42 56 53 4C 42 'BCSLBNELBEQLBVCLBVSLB'
        FCC     "PLLBMILBGELBLTLBGTLBL"  *F7BF: 50 4C 4C 42 4D 49 4C 42 47 45 4C 42 4C 54 4C 42 47 54 4C 42 4C 'PLLBMILBGELBLTLBGTLBL'
        FCC     "E"                      *F7D4: 45             'E'
MF7D5   FCC     "SWI2CMPDCMPYLDY STY L"  *F7D5: 53 57 49 32 43 4D 50 44 43 4D 50 59 4C 44 59 20 53 54 59 20 4C 'SWI2CMPDCMPYLDY STY L'
        FCC     "DS STS "                *F7EA: 44 53 20 53 54 53 20 'DS STS '
MF7F1   FCC     "SWI3CMPUCMPS"           *F7F1: 53 57 49 33 43 4D 50 55 43 4D 50 53 'SWI3CMPUCMPS'
        FCB     $FF,$FF,$FF              *F7FD: FF FF FF       '...'

* HARDWARE REQUIREMENTS:

*   ROM  AT $F800 - $FFFF
*   RAM  AT $8000 - $9FFF
*   ACIA AT FFD4
*   PTM  AT FFD8   (ENABLED TO NMI)

* SW1 jumpers set ACIA_1 BAUD


* ### DEBUGGER
* Initialization Entry Point
hdlr_RST LDA     #$9F                     *F800: 86 9F          '..'    DIRECT PAGE ADDR
        LDB     #$FF                     *F802: C6 FF          '..'    START AT TOP OF DP
        TFR     A,DP                     *F804: 1F 8B          '..'    GET DP ADDRESS
        TFR     D,S                      *F806: 1F 04          '..'    ALLOCATE USER STACK
        LEAS    -12,S                    *F808: 32 74          '2t'    MAKE ROOM FOR REGS
        STS     USERSP                   *F80A: 10 DF 00       '...'   SAVE INITIAL USER SP
        LDX     #MFD01                   *F80D: 8E FD 01       '...'
        STX     $0A,S                    *F810: AF 6A          '.j'    DEFAULT USER PC
        LDA     #$80                     *F812: 86 80          '..'
        STA     ,S                       *F814: A7 E4          '..'    INITIAL USER CC REG
        LEAS    -$34,S                   *F816: 32 E8 CC       '2..'   GIVE TOTAL OF 64 BYTES
        STS     DBUGSP                   *F819: 10 DF 02       '...'   SET DEBUG TOP OF STACK
        TFR     S,X                      *F81C: 1F 41          '.A'    COPY MEM PTR TO X
        LEAX    -$78,X                   *F81E: 30 88 88       '0..'   ALLOCATE I/0 BUFFER
        CMPX    IOBUF                    *F821: 9C 04          '..'    ALREADY ALLOCATED?
        BEQ     _BKPINIT                 *F823: 27 07          ''.'
        STX     IOBUF                    *F825: 9F 04          '..'    SAVE ITS ADDRESS
        LDD     #ACEC_ON                 *F827: CC 00 FF       '...'
        STD     ACECHO                   *F82A: DD 09          '..'
_BKPINIT LEAX    -$15,X                   *F82C: 30 88 EB       '0..'   ALLOCATE BKPT TABLE
        STX     BKPTBL                   *F82F: 9F 06          '..'    SAVE ITS ADDRESS
INIT2   CLR     ,X+                      *F831: 6F 80          'o.'    CLEAR BKPT TABLE
        CMPX    IOBUF                    *F833: 9C 04          '..'
        BCS     INIT2                    *F835: 25 FA          '%.'    LOOP TIL CLEAR
        LDX     #BKPT                    *F837: 8E FC A0       '...'
        STX     RAM_SWI_VEC              *F83A: 9F 11          '..'    SETUP NMI VECTOR
        LDX     #SSBK                    *F83C: 8E FC D1       '...'
        STX     RAM_NMI_VEC              *F83F: 9F 17          '..'    SETUP SWI VECTOR
* I/O (ACIA1) INITIALIZATION
A1IOINIT LDX     #ACADDR1                 *F841: 8E FF D4       '...'   GET ACIA1 ADDRESS
        STX     ACVECT                   *F844: 9F 0B          '..'    INIT VECTOR
        CLR     -$0F,X                   *F846: 6F 11          'o.'    ($C5) CLEAR PIA0 CRA
        CLR     -$10,X                   *F848: 6F 10          'o.'    CLEAR PIA0 DDRA REG
        LDB     #%00111100               *F84A: C6 3C          '.<'
        STB     -$0F,X                   *F84C: E7 11          '..'    SET PIA0 CRA -IRQ, +POR, CA2 OUTPUT HI ON WRITE TO CRA
        LDB     -$10,X                   *F84E: E6 10          '..'    GET PIA0 PORA
        ANDB    #$0F                     *F850: C4 0F          '..'    GET LO NIBBLE (SW1, JP2)
        ORB     #%00010000               *F852: CA 10          '..'    USE INTERNAL BAUD RATE GEN
        LDA     #%00001011               *F854: 86 0B          '..'    DTR LOW, RTS LOW, NO IRQ, NO PARITY
        STD     $02,X                    *F856: ED 02          '..'    SET ACIA1 COMMAND & CONTROL REG
        LDX     #TIMER                   *F858: 8E FF D8       '...'   GET TIMER ADDRESS
        LDA     #$43                     *F85B: 86 43          '.C'    TIMER INIT VALUE
        STA     $01,X                    *F85D: A7 01          '..'    INIT TIMER
        LDD     #TMINIT                  *F85F: CC 00 0C       '...'   GET TIMEOUT VALUE
        STD     $04,X                    *F862: ED 04          '..'    SET VALUE INTO TIMER
        LDY     #TITLE                   *F864: 10 8E FD 1A    '....'  PROG NAME ADDR
        LBSR    CPYSTR                   *F868: 17 06 1D       '...'   MOVE TO BUFFER
A1INIT3 LBSR    WRTLIN                   *F86B: 17 05 CA       '...'
* FALL THROUGH TO COMMAND LOOP
* Command Loop
COMAND  LDY     #PROMPT                  *F86E: 10 8E FD 2E    '....'  GET PROMPT STRING
        LBSR    CPYSTR                   *F872: 17 06 13       '...'
        CLR     BYTCNT                   *F875: 0F 22          '."'
        CLR     INBKPT                   *F877: 0F 08          '..'
        CLR     M9F19                    *F879: 0F 19          '..'
        LBSR    RDLIN                    *F87B: 17 05 87       '...'   INPUT COMMAND LINE
        BCS     A1INIT3                  *F87E: 25 EB          '%.'    IGNORE IF DELETED LINE
        LDX     IOBUF                    *F880: 9E 04          '..'
        LEAY    CMDTBL,PCR               *F882: 31 8D 04 C4    '1...'  GET TABLE ADDR
        BSR     _CMDSRC                  *F886: 8D 2D          '.-'
        TSTA                             *F888: 4D             'M'     COMPARE TO TEXT
        BNE     CMND2                    *F889: 26 13          '&.'
        LDY     XTAGLOC                  *F88B: 10 BE E8 00    '....'
        CMPY    #XTAG                    *F88F: 10 8C AA 55    '...U'  CHECK FOR EPROM PROGRAM ROM
        BNE     ILLCMD                   *F893: 26 0D          '&.'    EXIT IF TABLE END
        LDY     XCMDTBL_VEC              *F895: 10 BE E8 02    '....'  START ADDR OF EXTENDED CMD TABLE
        BSR     _CMDSRC                  *F899: 8D 1A          '..'
        TSTA                             *F89B: 4D             'M'
        BEQ     ILLCMD                   *F89C: 27 04          ''.'    EXIT IF NOT IN EXTENDED CMD TABLE
CMND2   JSR     ,Y                       *F89E: AD A4          '..'    CALL COMMAND ROUTINE
        BRA     COMAND                   *F8A0: 20 CC          ' .'    GO FOR ANOTHER
* UNRECOGNIZED COMMAND HANDLER
ILLCMD  LDA     #$31                     *F8A2: 86 31          '.1'
        BSR     CMDERR                   *F8A4: 8D 02          '..'    REPORT IT...
        BRA     COMAND                   *F8A6: 20 C6          ' .'
* COMMAND ERROR PROCESSOR
CMDERR  LDY     #ERRMSG                  *F8A8: 10 8E FD 24    '...$'
        LBSR    CPYSTR                   *F8AC: 17 05 D9       '...'
        LBSR    A1OUTCHR                 *F8AF: 17 05 C7       '...'
        LBRA    WRTLIN                   *F8B2: 16 05 83       '...'
* COMMAND TABLE SEARCH
_CMDSRC LDU     -$02,Y                   *F8B5: EE 3E          '.>'
        BEQ     ZF8CD                    *F8B7: 27 14          ''.'
        LDD     ,U                       *F8B9: EC C4          '..'
        CMPD    #XTAG                    *F8BB: 10 83 AA 55    '...U'
        BNE     ZF8CD                    *F8BF: 26 0C          '&.'
        PSHS    Y                        *F8C1: 34 20          '4 '
        LDY     $02,U                    *F8C3: 10 AE 42       '..B'   INCR TABLE PTR
        BSR     ZF8CD                    *F8C6: 8D 05          '..'
        TSTA                             *F8C8: 4D             'M'
        BNE     ZF8FB                    *F8C9: 26 30          '&0'
        PULS    Y                        *F8CB: 35 20          '5 '
ZF8CD   PSHS    X                        *F8CD: 34 10          '4.'
ZF8CF   LDA     ,X+                      *F8CF: A6 80          '..'    GET MATCH CHAR
        CMPA    #$0D                     *F8D1: 81 0D          '..'
        BEQ     ZF8F2                    *F8D3: 27 1D          ''.'
        CMPA    #$20                     *F8D5: 81 20          '. '
        BEQ     ZF8F2                    *F8D7: 27 19          ''.'
        TST     ,Y                       *F8D9: 6D A4          'm.'
        BEQ     ZF8E9                    *F8DB: 27 0C          ''.'
        CMPA    ,Y+                      *F8DD: A1 A0          '..'
        BEQ     ZF8CF                    *F8DF: 27 EE          ''.'
ZF8E1   TST     ,Y                       *F8E1: 6D A4          'm.'
        BEQ     ZF8E9                    *F8E3: 27 04          ''.'
ZF8E5   LEAY    $01,Y                    *F8E5: 31 21          '1!'
        BRA     ZF8E1                    *F8E7: 20 F8          ' .'
ZF8E9   PULS    X                        *F8E9: 35 10          '5.'
        LEAY    $03,Y                    *F8EB: 31 23          '1#'
        LDA     ,Y                       *F8ED: A6 A4          '..'
        BNE     ZF8CD                    *F8EF: 26 DC          '&.'
        RTS                              *F8F1: 39             '9'
ZF8F2   TST     ,Y                       *F8F2: 6D A4          'm.'
        BNE     ZF8E5                    *F8F4: 26 EF          '&.'
        LDY     $01,Y                    *F8F6: 10 AE 21       '..!'
        LEAX    -$01,X                   *F8F9: 30 1F          '0.'
ZF8FB   LEAS    $02,S                    *F8FB: 32 62          '2b'
        RTS                              *F8FD: 39             '9'

* "A" COMMAND: EXAMINE/CHANGE A REGISTER
ACMD    LDB     #$01                     *F8FE: C6 01          '..'
        BRA     REG08CMD                 *F900: 20 09          ' .'

* "B" COMMAND: EXAMINE/CHANGE B REGISTER
BCMD    LDB     #$02                     *F902: C6 02          '..'
        BRA     REG08CMD                 *F904: 20 05          ' .'

* "CC" COMMAND: EXAMINE/CHANGE CC REGISTER
CCCMD   CLRB                             *F906: 5F             '_'
        BRA     REG08CMD                 *F907: 20 02          ' .'

* "DP" COMMAND: EXAMINE/CHANGE DP REGISTER
DPCMD   LDB     #$03                     *F909: C6 03          '..'
REG08CMD PSHS    X                        *F90B: 34 10          '4.'
        LDX     USERSP                   *F90D: 9E 00          '..'
        ABX                              *F90F: 3A             ':'
        TFR     X,Y                      *F910: 1F 12          '..'
        PULS    X                        *F912: 35 10          '5.'
        LBSR    CHKEOL                   *F914: 17 05 13       '...'
        BNE     ZF938                    *F917: 26 1F          '&.'
        LDA     #$3D                     *F919: 86 3D          '.='
        LBSR    A1OUTCHR                 *F91B: 17 05 5B       '..['
ZF91E   LDA     ,Y                       *F91E: A6 A4          '..'
        LBSR    PRHXSP                   *F920: 17 05 7E       '..~'
        LDB     #$02                     *F923: C6 02          '..'
        LBSR    GETLN                    *F925: 17 05 D2       '...'
        BCC     ZF936                    *F928: 24 0C          '$.'
        CMPA    #$20                     *F92A: 81 20          '. '
        BEQ     ZF94C                    *F92C: 27 1E          ''.'
        CMPA    #$2D                     *F92E: 81 2D          '.-'
        BEQ     ZF94C                    *F930: 27 1A          ''.'
        CMPA    #$0D                     *F932: 81 0D          '..'
        BEQ     ZF953                    *F934: 27 1D          ''.'
ZF936   LDX     IOBUF                    *F936: 9E 04          '..'
ZF938   LBSR    HEX2DEC                  *F938: 17 05 90       '...'
        BCS     RPT2ERR                  *F93B: 25 0A          '%.'
        STA     ,Y                       *F93D: A7 A4          '..'
        CMPA    ,Y                       *F93F: A1 A4          '..'
        BEQ     ZF956                    *F941: 27 13          ''.'
        LDA     #$33                     *F943: 86 33          '.3'
        BRA     ZF949                    *F945: 20 02          ' .'
RPT2ERR LDA     #$32                     *F947: 86 32          '.2'
ZF949   LBRA    CMDERR                   *F949: 16 FF 5C       '..\'
ZF94C   PSHS    A                        *F94C: 34 02          '4.'
        LBSR    WRTLIN                   *F94E: 17 04 E7       '...'
        PULS    A                        *F951: 35 02          '5.'
ZF953   ORCC    #$01                     *F953: 1A 01          '..'
        RTS                              *F955: 39             '9'
ZF956   ANDCC   #$FE                     *F956: 1C FE          '..'
        LBRA    WRTLIN                   *F958: 16 04 DD       '...'

* "SP" COMMAND: EXAMINE/CHANGE SP REGISTER
SPCMD   LDY     #USERSP                  *F95B: 10 8E 9F 00    '....'
        BRA     ZF978                    *F95F: 20 17          ' .'

* "U" COMMAND: EXAMINE/CHANGE U REGISTER
UCMD    LDB     #$08                     *F961: C6 08          '..'
        BRA     REG16CMD                 *F963: 20 0A          ' .'

* "X" COMMAND: EXAMINE/CHANGE X REGISTER
XCMD    LDB     #$04                     *F965: C6 04          '..'
        BRA     REG16CMD                 *F967: 20 06          ' .'

* "Y" COMMAND: EXAMINE/CHANGE Y REGISTER
YCMD    LDB     #$06                     *F969: C6 06          '..'
        BRA     REG16CMD                 *F96B: 20 02          ' .'

* "PC" COMMAND: EXAMINE/CHANGE PC REGISTER
PCCMD   LDB     #$0A                     *F96D: C6 0A          '..'
REG16CMD PSHS    X                        *F96F: 34 10          '4.'
        LDX     USERSP                   *F971: 9E 00          '..'
        ABX                              *F973: 3A             ':'
        TFR     X,Y                      *F974: 1F 12          '..'
        PULS    X                        *F976: 35 10          '5.'
ZF978   LBSR    CHKEOL                   *F978: 17 04 AF       '...'   CHECK FOR PARAMETER
        BNE     REGCM3                   *F97B: 26 17          '&.'    IF NOT, GET NEW ADDR
* DISPLAY REGISTER CONTENTS
        LDA     #$3D                     *F97D: 86 3D          '.='    OUTPUT "="
        LBSR    A1OUTCHR                 *F97F: 17 04 F7       '...'
        LDD     ,Y                       *F982: EC A4          '..'
        LBSR    PRHX2SP                  *F984: 17 05 16       '...'
        LDB     #$04                     *F987: C6 04          '..'
        LBSR    GETLN                    *F989: 17 05 6E       '..n'
        BCC     ZF992                    *F98C: 24 04          '$.'
        CMPA    #$0D                     *F98E: 81 0D          '..'
        BEQ     ZF99E                    *F990: 27 0C          ''.'
ZF992   LDX     IOBUF                    *F992: 9E 04          '..'
* CHANGE REGISTER CONTENTS
REGCM3  LBSR    HEX4BIN                  *F994: 17 05 28       '..('   CHECK FOR PARAMETER
        BCS     ECME                     *F997: 25 0E          '%.'    EXIT IF ERROR
        STD     ,Y                       *F999: ED A4          '..'
        LBSR    WRTLIN                   *F99B: 17 04 9A       '...'
ZF99E   RTS                              *F99E: 39             '9'

* "E" COMMAND: EDIT MEMORY
ECMD    LBSR    CHKEOL                   *F99F: 17 04 88       '...'
        BEQ     RPT2ERR                  *F9A2: 27 A3          ''.'
        LBSR    HEX4BIN                  *F9A4: 17 05 18       '...'
ECME    BCS     RPT2ERR                  *F9A7: 25 9E          '%.'
        TFR     D,Y                      *F9A9: 1F 02          '..'
ECM2    LBSR    PRHX2SP                  *F9AB: 17 04 EF       '...'
        LBSR    ZF91E                    *F9AE: 17 FF 6D       '..m'
        BCS     ECM4                     *F9B1: 25 06          '%.'
ECM3    LEAY    $01,Y                    *F9B3: 31 21          '1!'
        TFR     Y,D                      *F9B5: 1F 20          '. '
        BRA     ECM2                     *F9B7: 20 F2          ' .'
ECM4    CMPA    #$20                     *F9B9: 81 20          '. '
        BEQ     ECM3                     *F9BB: 27 F6          ''.'
        CMPA    #'-                      *F9BD: 81 2D          '.-'
        BNE     ECMR                     *F9BF: 26 04          '&.'
        LEAY    -$02,Y                   *F9C1: 31 3E          '1>'
        BRA     ECM3                     *F9C3: 20 EE          ' .'
ECMR    RTS                              *F9C5: 39             '9'

* "R" COMMAND: DISPLAY
* USER REGISTER CONTENTS

* SUBROUTINE DSPREG - DISPLAY
* USER REGISTER CONTENTS
RCMD    LDY     USERSP                   *F9C6: 10 9E 00       '...'
        LDX     #REGSTR                  *F9C9: 8E F9 F3       '...'
        BSR     DSPRG4                   *F9CC: 8D 1B          '..'
        TFR     Y,D                      *F9CE: 1F 20          '. '
        LEAX    $01,X                    *F9D0: 30 01          '0.'
DSPRG1  LBSR    PRHX2SP                  *F9D2: 17 04 C8       '...'
DSPRG2  TST     ,X                       *F9D5: 6D 84          'm.'
        BEQ     VCMR                     *F9D7: 27 6B          ''k'
        BSR     DSPRG4                   *F9D9: 8D 0E          '..'
        ASRA                             *F9DB: 47             'G'
        BCS     DSPRG3                   *F9DC: 25 07          '%.'
        LDA     ,Y+                      *F9DE: A6 A0          '..'
        LBSR    PRHXSP                   *F9E0: 17 04 BE       '...'
        BRA     DSPRG2                   *F9E3: 20 F0          ' .'
DSPRG3  LDD     ,Y++                     *F9E5: EC A1          '..'
        BRA     DSPRG1                   *F9E7: 20 E9          ' .'
DSPRG4  LDA     ,X+                      *F9E9: A6 80          '..'
        BMI     DSPRGR                   *F9EB: 2B 05          '+.'
        LBSR    A1OUTCHR                 *F9ED: 17 04 89       '...'
        BRA     DSPRG4                   *F9F0: 20 F7          ' .'
DSPRGR  RTS                              *F9F2: 39             '9'
REGSTR  FCC     " SP="                   *F9F3: 20 53 50 3D    ' SP='
        FCB     $81,$00                  *F9F7: 81 00          '..'
        FCC     " CC="                   *F9F9: 20 43 43 3D    ' CC='
        FCB     $80                      *F9FD: 80             '.'
        FCC     "A="                     *F9FE: 41 3D          'A='
        FCB     $80                      *FA00: 80             '.'
        FCC     "B="                     *FA01: 42 3D          'B='
        FCB     $80                      *FA03: 80             '.'
        FCC     " DP="                   *FA04: 20 44 50 3D    ' DP='
        FCB     $80                      *FA08: 80             '.'
        FCC     "X="                     *FA09: 58 3D          'X='
        FCB     $81                      *FA0B: 81             '.'
        FCC     "Y="                     *FA0C: 59 3D          'Y='
        FCB     $81                      *FA0E: 81             '.'
        FCC     "U="                     *FA0F: 55 3D          'U='
        FCB     $81                      *FA11: 81             '.'
        FCC     " PC="                   *FA12: 20 50 43 3D    ' PC='
        FCB     $81,$00                  *FA16: 81 00          '..'

* "V" COMMAND: VIEW FORMATTED HEX
* AND ASCII(?) DUMP OF MEMORY RANGE
VCMD    LBSR    BEGEND                   *FA18: 17 00 5C       '..\'   GET HEX ADDR START END
VCME    LBCS    RPT2ERR                  *FA1B: 10 25 FF 28    '.%.('  ERROR IF NOT 2 HEX ADDRESSES
        PSHS    U,D                      *FA1F: 34 46          '4F'
VCM2    LBSR    WRTLIN                   *FA21: 17 04 14       '...'
        LDB     #$10                     *FA24: C6 10          '..'    SET COUNTER TO 16
        PSHS    B                        *FA26: 34 04          '4.'    PUSH COUNTER ONTO STACK TO FREE B
        TFR     U,D                      *FA28: 1F 30          '.0'
        LBSR    PRHX2SP                  *FA2A: 17 04 70       '..p'
VCM3    LDA     ,U+                      *FA2D: A6 C0          '..'    GET THE NEXT VALUE IN MEMORY
        LBSR    PRHXSP                   *FA2F: 17 04 6F       '..o'   PRINT BYTE IN HEX & SPACE
        DEC     ,S                       *FA32: 6A E4          'j.'    DECREMENT THE COUNTER
        BNE     VCM3                     *FA34: 26 F7          '&.'
        PULS    B                        *FA36: 35 04          '5.'    RESTORE B FROM STACK
        CMPU    $02,S                    *FA38: 11 A3 62       '..b'
        BCS     VCM4                     *FA3B: 25 05          '%.'
        CMPU    ,S                       *FA3D: 11 A3 E4       '...'
        BCS     VCM2                     *FA40: 25 DF          '%.'
VCM4    LEAS    $04,S                    *FA42: 32 64          '2d'
VCMR    LBSR    WRTLIN                   *FA44: 17 03 F1       '...'
        RTS                              *FA47: 39             '9'

* "C" COMMAND: CLEAR AND TEST MEMORY
CCMD    BSR     BEGEND                   *FA48: 8D 2D          '.-'    GET ADDR PARAMETERS
        BCS     VCME                     *FA4A: 25 CF          '%.'    EXIT IF ERROR
        PSHS    D                        *FA4C: 34 06          '4.'    SAVE END ADDRESS ON STACK
CCMD2   CMPU    ,S                       *FA4E: 11 A3 E4       '...'   DONE YET?
        BLS     CCMD3                    *FA51: 23 02          '#.'    CONTINUE IF NOT
        PULS    PC,D                     *FA53: 35 86          '5.'    ELSE POP TEMP + RTS
CCMD3   LDD     #RAM_PAT                 *FA55: CC 80 08       '...'   LOAD COUNT AND PATTERN
        STA     ,U                       *FA58: A7 C4          '..'    STORE PATTERN
CCMD4   CMPA    ,U                       *FA5A: A1 C4          '..'    COMPARE PATTERN TO MEM
        BNE     CCMD5                    *FA5C: 26 08          '&.'    ERROR IF NO MATCH
        LSRA                             *FA5E: 44             'D'     ROTATE PATTERN
        LSR     ,U                       *FA5F: 64 C4          'd.'    ROTATE MEMORY
        DECB                             *FA61: 5A             'Z'     ALL BITS TESTED?
        BNE     CCMD4                    *FA62: 26 F6          '&.'    LOOP IF NOT
        BRA     CCMD6                    *FA64: 20 0D          ' .'    ELSE CONTINUE
CCMD5   LDA     #$2D                     *FA66: 86 2D          '.-'    OUTPUT "-"
        LBSR    A1OUTCHR                 *FA68: 17 04 0E       '...'   REPORT BAD LOCATION
        TFR     U,D                      *FA6B: 1F 30          '.0'    COPY CURRENT ADDR
        LBSR    PRHX2SP                  *FA6D: 17 04 2D       '..-'   CONVERT TO HEXT
        LBSR    WRTLIN                   *FA70: 17 03 C5       '...'   OUTPUT THIS LINE
CCMD6   LEAU    $01,U                    *FA73: 33 41          '3A'    INCREMENT CURRENT ADDRESS
        BRA     CCMD2                    *FA75: 20 D7          ' .'    BOTTOM OF LOOP

* SUBROUTINE BEGEND: GET BEGIN AND
* END ADDRESS PARAMETERS
* 2 HEX ADDRESS: BEGIN IN U, END IN D
BEGEND  LBSR    CHKEOL                   *FA77: 17 03 B0       '...'   NO PARAMETER?
        BEQ     BGND1                    *FA7A: 27 11          ''.'    EXIT WITH ERROR
        LBSR    HEX4BIN                  *FA7C: 17 04 40       '..@'   FIRST PARAMETER
        BCS     BGND1                    *FA7F: 25 0C          '%.'    EXIT IF ERROR
        TFR     D,U                      *FA81: 1F 03          '..'    COPY RESULT TO U
        LBSR    CHKEOL                   *FA83: 17 03 A4       '...'
        BEQ     BGND1                    *FA86: 27 05          ''.'
        LBSR    HEX4BIN                  *FA88: 17 04 34       '..4'   SECOND PARAMETER
        BCC     BGND2                    *FA8B: 24 02          '$.'
BGND1   ORCC    #$01                     *FA8D: 1A 01          '..'
BGND2   RTS                              *FA8F: 39             '9'

* "M" COMMAND: MOVE MEMORY VALUES
MCMD    LBSR    CHKEOL                   *FA90: 17 03 97       '...'   CHECK FOR NO PARAMETERS
MCME2   LBEQ    RPT2ERR                  *FA93: 10 27 FE B0    '.'..'  IF SO, REPORT ERROR
        LBSR    HEX4BIN                  *FA97: 17 04 25       '..%'   SCAN FOR 4 DIGIT HEX PARAMETER INTO D
MCME    LBCS    RPT2ERR                  *FA9A: 10 25 FE A9    '.%..'  IF NOT, REPORT ERROR
        TFR     D,Y                      *FA9E: 1F 02          '..'    SAVE SOURCE ADDRESS IN Y
        LBSR    BEGEND                   *FAA0: 17 FF D4       '...'   SCAN FOR 2 4 DIGIT HEX PARAMETERS INTO U, D
        BCS     MCME                     *FAA3: 25 F5          '%.'    IF NOT, REPORT ERROR
        ADDD    #$0000                   *FAA5: C3 00 00       '...'   CHECK IF LENGTH IS ZERO
        BEQ     MCME2                    *FAA8: 27 E9          ''.'    IF SO, REPORT ERROR
        TFR     D,X                      *FAAA: 1F 01          '..'    SAVE LENGTH IN X
MCM3    LDA     ,Y+                      *FAAC: A6 A0          '..'    LOAD SOURCE VALUE INTO A
        STA     ,U+                      *FAAE: A7 C0          '..'    SAVE IT TO THE DESTINATION
        LEAX    -$01,X                   *FAB0: 30 1F          '0.'    DECREMENT X
        BNE     MCM3                     *FAB2: 26 F8          '&.'    CONTINUE IF NOT ZERO
        RTS                              *FAB4: 39             '9'     RETURN

* "G" COMMAND: GO TO USER PROGRAM
* EXECUTE AT PC ADDRESS OR UPDATE
* PC IF PARAMETER IN COMMAND LINE.
GCMD    LBSR    CHKEOL                   *FAB5: 17 03 72       '..r'   FIRST CHAR=RETURN?
        BEQ     GCMD2                    *FAB8: 27 0A          ''.'    SKIP IF SO
        LBSR    HEX4BIN                  *FABA: 17 04 02       '...'   ELSE GET PARAM
        BCS     MCME                     *FABD: 25 DB          '%.'    EXIT IF ERROR
        LDY     USERSP                   *FABF: 10 9E 00       '...'   GET SP ADDR
        STD     $0A,Y                    *FAC2: ED 2A          '.*'    MAKE PC=PARAM RESULT
* INSERT BREAKPOINTS
GCMD2   LDY     BKPTBL                   *FAC4: 10 9E 06       '...'   GET BREAKPOINT TABLE ADDR
        LDB     #$07                     *FAC7: C6 07          '..'    MAX COUNT
GCMD3   LDU     ,Y                       *FAC9: EE A4          '..'    GET ADDRESS
        BEQ     GCMD4                    *FACB: 27 08          ''.'    EXIT IF NOT ACTIVE
        LDA     ,U                       *FACD: A6 C4          '..'    GET INSTR OPCODE
        STA     $02,Y                    *FACF: A7 22          '."'    SAVE IN TABLE
        LDA     #$3F                     *FAD1: 86 3F          '.?'    INSERT SWI
        STA     ,U                       *FAD3: A7 C4          '..'    IN PGM
GCMD4   LEAY    $03,Y                    *FAD5: 31 23          '1#'    MOVE TO NEXT TABLE ENTRY
        DECB                             *FAD7: 5A             'Z'     DONE YET?
        BNE     GCMD3                    *FAD8: 26 EF          '&.'    LOOP IF NOT
        LDS     USERSP                   *FADA: 10 DE 00       '...'   ELSE RESTORE USER SP
        RTI                              *FADD: 3B             ';'     AND GO...

* "J" COMMAND: JUMP TO ADDRESS
JCMD    LBSR    CHKEOL                   *FADE: 17 03 49       '..I'   FIRST CHAR=RET?
        BEQ     MCME2                    *FAE1: 27 B0          ''.'    REPORT ERROR
        LBSR    HEX4BIN                  *FAE3: 17 03 D9       '...'   ELSE GET PARAM
        BCS     MCME                     *FAE6: 25 B2          '%.'    EXIT IF ERROR
        TFR     D,X                      *FAE8: 1F 01          '..'    SAVE ADDRESS
        JSR     ,X                       *FAEA: AD 84          '..'    JUMP TO THE ADDRESS
        RTS                              *FAEC: 39             '9'

* "BK" COMMAND - SET OR DISPLAY BREAKPOINTS
BKCMD   LBSR    CHKEOL                   *FAED: 17 03 3A       '..:'   IS NEXT CHAR RETURN?
        BNE     BKCM2                    *FAF0: 26 1C          '&.'    IF NOT, CONTINUE
        LBSR    WRTLIN                   *FAF2: 17 03 43       '..C'   ELSE GET OUTPUT READY
        LDY     BKPTBL                   *FAF5: 10 9E 06       '...'   GET BKPT TABLE ADDRESS
        LDB     #$07                     *FAF8: C6 07          '..'    MAX NUMBER OF BREAKPOINTS
        PSHS    B                        *FAFA: 34 04          '4.'    PUSH COUNT
* BREAKPOINT DISPLAY LOOP
BKCM1   LDD     ,Y                       *FAFC: EC A4          '..'    GET NEXT ADDR
        BEQ     BKCM1A                   *FAFE: 27 03          ''.'    SKIP IF NOT ACTIVE
        LBSR    PRHX2SP                  *FB00: 17 03 9A       '...'   ELSE CONVERT TO HEX
BKCM1A  LEAY    $03,Y                    *FB03: 31 23          '1#'    MOVE TO NEXT ENTRY
        DEC     ,S                       *FB05: 6A E4          'j.'    DECREMENT COUNT
        BNE     BKCM1                    *FB07: 26 F3          '&.'    LOOP IF MORE TO DO
        LEAS    $01,S                    *FB09: 32 61          '2a'    CLEAN UP STACK
        LBRA    WRTLIN                   *FB0B: 16 03 2A       '..*'   OUTPUT LINE +RTS
* SET BREAKPOINT
BKCM2   LBSR    HEX4BIN                  *FB0E: 17 03 AE       '...'   PROCESS PARAM
BKCME   LBCS    RPT2ERR                  *FB11: 10 25 FE 32    '.%.2'  EXIT IF ERROR
        PSHS    D                        *FB15: 34 06          '4.'    SAVE RESULT
        BSR     FNDBKP                   *FB17: 8D 37          '.7'    SEARCH TABLE FOR THIS ADDR
        BEQ     BKCM2A                   *FB19: 27 0E          ''.'    SKIP IF ALREADY IN TABLE
        LDD     #$0000                   *FB1B: CC 00 00       '...'   SEARCH FOR EMPTY ENTRY
        BSR     FNDBKP                   *FB1E: 8D 30          '.0'    
        BEQ     BKCM2A                   *FB20: 27 07          ''.'    SKIP IF FOUND
        LDA     #$35                     *FB22: 86 35          '.5'    ELSE TABLE FULL ERROR
        LEAS    $02,S                    *FB24: 32 62          '2b'    CLEAN UP STACK
BKCMER  LBRA    CMDERR                   *FB26: 16 FD 7F       '...'
BKCM2A  PULS    D                        *FB29: 35 06          '5.'    POP ADDR
        STD     ,Y                       *FB2B: ED A4          '..'    PUT IN TABLE
        RTS                              *FB2D: 39             '9'     DONE

* "KB" COMMAND: KILL ONE OR ALL
* BREAKPOINTS
KBCMD   LBSR    CHKEOL                   *FB2E: 17 02 F9       '...'   NEXT CHAR=RET?
        BEQ     KBCM2                    *FB31: 27 12          ''.'    IF SO, GO CLEAR ALL
        LBSR    HEX4BIN                  *FB33: 17 03 89       '...'   ELSE GET PARAMETERS
        BCS     BKCME                    *FB36: 25 D9          '%.'    EXIT IF ERROR
        BSR     FNDBKP                   *FB38: 8D 16          '..'    FIND BREAKPOINT
        BEQ     KBCM1                    *FB3A: 27 04          ''.'    CONTINUE IF FOUND
        LDA     #$34                     *FB3C: 86 34          '.4'    SET ERROR CODE
        BRA     BKCMER                   *FB3E: 20 E6          ' .'    ERROR IF NOT FOUND
KBCM1   CLRA                             *FB40: 4F             'O'
        CLRB                             *FB41: 5F             '_'
        STD     ,Y                       *FB42: ED A4          '..'    CLEAR THIS ONE
        RTS                              *FB44: 39             '9'     DONE
KBCM2   LDY     BKPTBL                   *FB45: 10 9E 06       '...'   CLEAR ALL BREAKPOINTS
        LDB     #$15                     *FB48: C6 15          '..'    TABLE SIZE
KBCM2A  CLR     ,Y+                      *FB4A: 6F A0          'o.'    CLEAR LOOP
        DECB                             *FB4C: 5A             'Z'     DECR COUNT
        BNE     KBCM2A                   *FB4D: 26 FB          '&.'    LOOP TIL DONE
        RTS                              *FB4F: 39             '9'

* SUBROUTINE FNDBKP - SEARCH BREAKPOINT
* TABLE FOR ADDRESS
*  D = ADDRESS OF BREAKPOINT
* RETURNS TABLE ADDRESS IN Y AND Z BIT SET
* FOUND, ELSE Z BIT CLEARED
FNDBKP  PSHS    U                        *FB50: 34 40          '4@'    U IS LOCAL
        TFR     D,U                      *FB52: 1F 03          '..'    COPY BKPT ADDRESS
        LDB     #7                       *FB54: C6 07          '..'    MAX TABLE ENTRIES
        LDY     BKPTBL                   *FB56: 10 9E 06       '...'   GET TABLE ADDRESS
FNDBK2  CMPU    ,Y                       *FB59: 11 A3 A4       '...'   DOES THIS ONE MATCH?
        BEQ     FNDBK3                   *FB5C: 27 09          ''.'    IF SO, WE'RE DONE
        LEAY    $03,Y                    *FB5E: 31 23          '1#'    ELSE MOVE Y TO NEXT ENTRY
        DECB                             *FB60: 5A             'Z'     DECR COUNT
        BNE     FNDBK2                   *FB61: 26 F6          '&.'    LOOP IF NOT DONE
        LDB     #$0C                     *FB63: C6 0C          '..'    BKPT NOT FOUND ERROR
        ANDCC   #$FB                     *FB65: 1C FB          '..'    ELSE CLEAR Z BIT
FNDBK3  PULS    PC,U                     *FB67: 35 C0          '5.'

* "N" COMMAND: NEXT SINGLE STEP
* SINGLE STEP COMMAND PROCESSOR
SSCMD   CLR     M9F19                    *FB69: 0F 19          '..'
SSCM2   LDS     USERSP                   *FB6B: 10 DE 00       '...'   LOAD USER STACK PTR
        CLR     TIMER                    *FB6E: 7F FF D8       '...'   START THE TIMER
        RTI                              *FB71: 3B             ';'     GO TO PROGRAM
GTCMD   LBSR    CHKEOL                   *FB72: 17 02 B5       '...'   NEXT CHAR=RET?
        BEQ     GTCM2                    *FB75: 27 09          ''.'    IF SO, CONTINUE
        LBSR    HEX4BIN                  *FB77: 17 03 45       '..E'   ELSE GET PARAMETER
        LBCS    RPT2ERR                  *FB7A: 10 25 FD C9    '.%..'  REPORT PARSE ERROR
        STD     M9F1A                    *FB7E: DD 1A          '..'    STORE ADDRESS
GTCM2   LDD     M9F1A                    *FB80: DC 1A          '..'    LOAD  ADDRESS
        BNE     GTCM3                    *FB82: 26 04          '&.'    CHECK IF ZERO
        LDA     #$37                     *FB84: 86 37          '.7'    REPORT COMMAND ERROR
        BRA     BKCMER                   *FB86: 20 9E          ' .'
GTCM3   COM     M9F19                    *FB88: 03 19          '..'
        BRA     SSCM2                    *FB8A: 20 DF          ' .'
VLCMD   LDA     #$FF                     *FB8C: 86 FF          '..'
        STA     M9F2B                    *FB8E: 97 2B          '.+'
        BRA     LCM1                     *FB90: 20 02          ' .'
LCMD    CLR     M9F2B                    *FB92: 0F 2B          '.+'
LCM1    LDD     #$0000                   *FB94: CC 00 00       '...'
        PSHS    D                        *FB97: 34 06          '4.'
        PSHS    D                        *FB99: 34 06          '4.'
        PSHS    D                        *FB9B: 34 06          '4.'
        PSHS    A                        *FB9D: 34 02          '4.'
        LBSR    CHKEOL                   *FB9F: 17 02 88       '...'
        BEQ     LCM3                     *FBA2: 27 0C          ''.'
        LBSR    HEX4BIN                  *FBA4: 17 03 18       '...'
        BCC     LCM2                     *FBA7: 24 05          '$.'
        LEAS    $07,S                    *FBA9: 32 67          '2g'
        LBRA    RPT2ERR                  *FBAB: 16 FD 99       '...'
LCM2    STD     $04,S                    *FBAE: ED 64          '.d'
LCM3    LBSR    A0IOINIT                 *FBB0: 17 00 93       '...'
LCM4    LBSR    A0INPCHR                 *FBB3: 17 00 A3       '...'
        BCS     LCM12                    *FBB6: 25 62          '%b'
        CMPA    #$53                     *FBB8: 81 53          '.S'
        BNE     LCM4                     *FBBA: 26 F7          '&.'
LCM5    LBSR    A0INPCHR                 *FBBC: 17 00 9A       '...'
        BCS     LCM12                    *FBBF: 25 59          '%Y'
        CMPA    #$53                     *FBC1: 81 53          '.S'
        BEQ     LCM5                     *FBC3: 27 F7          ''.'
        CMPA    #$30                     *FBC5: 81 30          '.0'
        BEQ     LCM4                     *FBC7: 27 EA          ''.'
        CMPA    #$39                     *FBC9: 81 39          '.9'
        BEQ     LCM12                    *FBCB: 27 4D          ''M'
        CMPA    #$31                     *FBCD: 81 31          '.1'
        BNE     LCM4                     *FBCF: 26 E2          '&.'
        CLR     M9F2E                    *FBD1: 0F 2E          '..'
        LBSR    ZFC7B                    *FBD3: 17 00 A5       '...'
        BCS     LCM11                    *FBD6: 25 39          '%9'
        SUBA    #$02                     *FBD8: 80 02          '..'
        STA     ,S                       *FBDA: A7 E4          '..'
        LBSR    ZFC8D                    *FBDC: 17 00 AE       '...'
        BCS     LCM11                    *FBDF: 25 30          '%0'
        ADDD    $04,S                    *FBE1: E3 64          '.d'
        TFR     D,U                      *FBE3: 1F 03          '..'
LCM6    LBSR    ZFC7B                    *FBE5: 17 00 93       '...'
        BCS     LCM11                    *FBE8: 25 27          '%''
        DEC     ,S                       *FBEA: 6A E4          'j.'
        BEQ     LCM8                     *FBEC: 27 0E          ''.'
        TST     M9F2B                    *FBEE: 0D 2B          '.+'
        BNE     LCM7                     *FBF0: 26 02          '&.'
        STA     ,U                       *FBF2: A7 C4          '..'
LCM7    CMPA    ,U+                      *FBF4: A1 C0          '..'
        BEQ     LCM6                     *FBF6: 27 ED          ''.'
        INC     $03,S                    *FBF8: 6C 63          'lc'
        BRA     LCM6                     *FBFA: 20 E9          ' .'
LCM8    INC     M9F2E                    *FBFC: 0C 2E          '..'
        BNE     LCM11                    *FBFE: 26 11          '&.'
        LDA     #$26                     *FC00: 86 26          '.&'
        LBSR    A1OUTCHR                 *FC02: 17 02 74       '..t'
        INC     $01,S                    *FC05: 6C 61          'la'
LCM9    TST     $03,S                    *FC07: 6D 63          'mc'
        BEQ     LCM10                    *FC09: 27 04          ''.'
        INC     $06,S                    *FC0B: 6C 66          'lf'
        CLR     $03,S                    *FC0D: 6F 63          'oc'
LCM10   BRA     LCM4                     *FC0F: 20 A2          ' .'
LCM11   LDA     #$2F                     *FC11: 86 2F          './'    PRINT A SLASH
        LBSR    A1OUTCHR                 *FC13: 17 02 63       '..c'
        INC     $02,S                    *FC16: 6C 62          'lb'
        BRA     LCM9                     *FC18: 20 ED          ' .'
LCM12   LBSR    WRTLIN                   *FC1A: 17 02 1B       '...'
        LDA     $01,S                    *FC1D: A6 61          '.a'
        LBSR    PRHXSP                   *FC1F: 17 02 7F       '...'
        LDY     #OK_MSG                  *FC22: 10 8E FD 30    '...0'
        LBSR    CPYSTR                   *FC26: 17 02 5F       '.._'
        LDA     $02,S                    *FC29: A6 62          '.b'
        LBSR    PRHXSP                   *FC2B: 17 02 73       '..s'
        LDY     #BADMSG                  *FC2E: 10 8E FD 34    '...4'
        LBSR    CPYSTR                   *FC32: 17 02 53       '..S'
        LDA     $06,S                    *FC35: A6 66          '.f'
        LBSR    PRHXSP                   *FC37: 17 02 67       '..g'
        LDY     #VRFMSG                  *FC3A: 10 8E FD 39    '...9'
        LBSR    CPYSTR                   *FC3E: 17 02 47       '..G'
        LEAS    $07,S                    *FC41: 32 67          '2g'
        LBRA    WRTLIN                   *FC43: 16 01 F2       '...'
* CHECK PIA0-A SW1 DIP SWITCHES
A0IOINIT PSHS    X,D                      *FC46: 34 16          '4.'
        LDX     #ACADDR0                 *FC48: 8E FF D0       '...'   GET ACIA ADDRESS
                                         * SHIFT HI NIBBLE TO LO (SW1)
        LDB     -$0C,X                   *FC4B: E6 14          '..'    LOAD PIA0 PORA
        LSRB                             *FC4D: 54             'T'
        LSRB                             *FC4E: 54             'T'
        LSRB                             *FC4F: 54             'T'
        LSRB                             *FC50: 54             'T'     USE AS BAUD RATE
        ORB     #%00010000               *FC51: CA 10          '..'    USE INTERNAL BAUD RATE GEN
        LDA     #%00001011               *FC53: 86 0B          '..'    DTR LOW, RTS LOW, NO IRQ, NO PARITY
        STD     $02,X                    *FC55: ED 02          '..'    SET ACIA0 COMMAND & CONTROL REG
        PULS    PC,X,D                   *FC57: 35 96          '5.'
* INPUT CHARACTER FROM ACIA0
A0INPCHR PSHS    X                        *FC59: 34 10          '4.'
        LDX     #ACADDR0                 *FC5B: 8E FF D0       '...'
A0INPCH2 LDA     $01,X                    *FC5E: A6 01          '..'    READ ACIA STATUS REGISTER
        BITA    #$08                     *FC60: 85 08          '..'    CHECK RX FULL BIT
        BEQ     A0INPCH2                 *FC62: 27 FA          ''.'    WAIT TIL READY
        LDA     ,X                       *FC64: A6 84          '..'    GET CHAR
        ANDA    #$7F                     *FC66: 84 7F          '..'    MASK OUT PARITY
        CMPA    #$7F                     *FC68: 81 7F          '..'    CHECK FOR DELETE CHAR
        BEQ     A0INPCH2                 *FC6A: 27 F2          ''.'    IF DELETE GET NEXT CAR
        ANDCC   #$FE                     *FC6C: 1C FE          '..'    NO ERROR, CLEAR CARRY
        PULS    PC,X                     *FC6E: 35 90          '5.'    RETURN
ZFC70   LDX     IOBUF                    *FC70: 9E 04          '..'
        BSR     A0INPCHR                 *FC72: 8D E5          '..'
        STA     ,X+                      *FC74: A7 80          '..'
        BSR     A0INPCHR                 *FC76: 8D E1          '..'
        STA     ,X+                      *FC78: A7 80          '..'
        RTS                              *FC7A: 39             '9'
ZFC7B   BSR     ZFC70                    *FC7B: 8D F3          '..'
        LDX     IOBUF                    *FC7D: 9E 04          '..'
        LBSR    HEX2DEC                  *FC7F: 17 02 49       '..I'
        BCS     ZFC8C                    *FC82: 25 08          '%.'
        TFR     A,B                      *FC84: 1F 89          '..'
        ADDB    M9F2E                    *FC86: DB 2E          '..'
        STB     M9F2E                    *FC88: D7 2E          '..'
        ANDCC   #$FE                     *FC8A: 1C FE          '..'
ZFC8C   RTS                              *FC8C: 39             '9'
ZFC8D   BSR     ZFC7B                    *FC8D: 8D EC          '..'
        BCS     ZFC9D                    *FC8F: 25 0C          '%.'
        PSHS    A                        *FC91: 34 02          '4.'
        BSR     ZFC7B                    *FC93: 8D E6          '..'
        BCS     ZFC9E                    *FC95: 25 07          '%.'
        TFR     A,B                      *FC97: 1F 89          '..'
        PULS    A                        *FC99: 35 02          '5.'
        ANDCC   #$FE                     *FC9B: 1C FE          '..'
ZFC9D   RTS                              *FC9D: 39             '9'
ZFC9E   PULS    PC,A                     *FC9E: 35 82          '5.'    RETURN

* BREAKPOINT SERVICE ROUTINE

* SAVE USER STACK, SWITCH TO
* DEBUG STACK, REMOVE BREAKPOINTS
* AND DUMP USER REGISTERS
BKPT    LDA     #$9F                     *FCA0: 86 9F          '..'    DEBUG DP
        TFR     A,DP                     *FCA2: 1F 8B          '..'
        COM     INBKPT                   *FCA4: 03 08          '..'
        STS     USERSP                   *FCA6: 10 DF 00       '...'   SAVE USER STACK PTR
        LDD     $0A,S                    *FCA9: EC 6A          '.j'    GET USER'S PC
        SUBD    #$0001                   *FCAB: 83 00 01       '...'   DECREMENT IT
        STD     $0A,S                    *FCAE: ED 6A          '.j'    REPLACE IT
        LDS     DBUGSP                   *FCB0: 10 DE 02       '...'   LOAD DEBUG STACK
        LBSR    FNDBKP                   *FCB3: 17 FE 9A       '...'   ADDR IN TABLE?
        BEQ     BKPT2                    *FCB6: 27 05          ''.'    SKIP IF SO
        LDA     #$36                     *FCB8: 86 36          '.6'    ELSE REPORT ILLEGAL BREAKPOINT
        LBSR    CMDERR                   *FCBA: 17 FB EB       '...'
* REMOVE BREAKPOINTS FROM PGM
BKPT2   LDY     BKPTBL                   *FCBD: 10 9E 06       '...'
        LDB     #$07                     *FCC0: C6 07          '..'    MAX COUNT
BKPT3   LDX     ,Y                       *FCC2: AE A4          '..'    GET ADDR OF BKPT
        BEQ     BKPT4                    *FCC4: 27 04          ''.'    SKIP IF NOT ACTIVE
        LDA     $02,Y                    *FCC6: A6 22          '."'    GET SAVED OPCODE
        STA     ,X                       *FCC8: A7 84          '..'    RESTORE IT
BKPT4   LEAY    $03,Y                    *FCCA: 31 23          '1#'    MOVE TO NEXT ENTRY
        DECB                             *FCCC: 5A             'Z'     DONE YET?
        BNE     BKPT3                    *FCCD: 26 F3          '&.'    LOOP IF NOT
        BRA     DSPREG                   *FCCF: 20 22          ' "'    PRINT REGISTERS

* SINGLE STEP SERVICE ROUTINE

SSBK    LDA     #$01                     *FCD1: 86 01          '..'
        STA     TIMER                    *FCD3: B7 FF D8       '...'   STOP TIMER
        LDA     #$9F                     *FCD6: 86 9F          '..'    GET DEBUGGER DIRECT PAGE
        TFR     A,DP                     *FCD8: 1F 8B          '..'
        STS     USERSP                   *FCDA: 10 DF 00       '...'   SAVE USER STACK
        LDD     $0A,S                    *FCDD: EC 6A          '.j'
        LDS     DBUGSP                   *FCDF: 10 DE 02       '...'   LOAD DEBUGGER STACK
        TST     M9F19                    *FCE2: 0D 19          '..'
        BEQ     DSPREG                   *FCE4: 27 0D          ''.'

        TST     INBKPT                   *FCE6: 0D 08          '..'
        BNE     ZFCF9                    *FCE8: 26 0F          '&.'
        CMPD    M9F1A                    *FCEA: 10 93 1A       '...'
        LBNE    SSCM2                    *FCED: 10 26 FE 7A    '.&.z'
        CLR     M9F19                    *FCF1: 0F 19          '..'
DSPREG  LBSR    RCMD                     *FCF3: 17 FC D0       '...'   PRINT REGISTERS
        LBRA    COMAND                   *FCF6: 16 FB 75       '..u'   GOTO COMMAND LOOP
ZFCF9   CLR     M9F19                    *FCF9: 0F 19          '..'
        CLR     INBKPT                   *FCFB: 0F 08          '..'
        LDS     USERSP                   *FCFD: 10 DE 00       '...'
        RTI                              *FD00: 3B             ';'
MFD01   SWI                              *FD01: 3F             '?'
hdlr_SWI3 JMP     [RAM_SW3_VEC]            *FD02: 6E 9F 9F 0D    'n...'
hdlr_SWI2 JMP     [RAM_SW2_VEC]            *FD06: 6E 9F 9F 0F    'n...'
hdlr_SWI JMP     [RAM_SWI_VEC]            *FD0A: 6E 9F 9F 11    'n...'
hdlr_FIRQ JMP     [RAM_FRQ_VEC]            *FD0E: 6E 9F 9F 15    'n...'
hdlr_IRQ JMP     [RAM_IRQ_VEC]            *FD12: 6E 9F 9F 13    'n...'
hdlr_NMI JMP     [RAM_NMI_VEC]            *FD16: 6E 9F 9F 17    'n...'
TITLE   FCB     $0D,$0A                  *FD1A: 0D 0A          '..'
        FCC     "DEBUG19"                *FD1C: 44 45 42 55 47 31 39 'DEBUG19'
        FCB     $00                      *FD23: 00             '.'
ERRMSG  FCB     $0D,$0A                  *FD24: 0D 0A          '..'
        FCC     "ERROR "                 *FD26: 45 52 52 4F 52 20 'ERROR '
        FCB     $07,$00                  *FD2C: 07 00          '..'
PROMPT  FCC     ":"                      *FD2E: 3A             ':'
        FCB     $00                      *FD2F: 00             '.'
OK_MSG  FCC     "OK "                    *FD30: 4F 4B 20       'OK '
        FCB     $00                      *FD33: 00             '.'
BADMSG  FCC     "BAD "                   *FD34: 42 41 44 20    'BAD '
        FCB     $00                      *FD38: 00             '.'
VRFMSG  FCC     "VERF PROBLEMS"          *FD39: 56 45 52 46 20 50 52 4F 42 4C 45 4D 53 'VERF PROBLEMS'
        FCB     $07,$00,$F0              *FD46: 07 00 F0       '...'
* COMMAND DISPATCH TABLE
        FCB     $00                      *FD49: 00             '.'
CMDTBL  FCC     "A"                      *FD4A: 41             'A'
        FCB     $00                      *FD4B: 00             '.'
        FDB     ACMD                     *FD4C: F8 FE          '..'
        FCC     "B"                      *FD4E: 42             'B'
        FCB     $00                      *FD4F: 00             '.'
        FDB     BCMD                     *FD50: F9 02          '..'
        FCC     "CC"                     *FD52: 43 43          'CC'
        FCB     $00                      *FD54: 00             '.'
        FDB     CCCMD                    *FD55: F9 06          '..'
        FCC     "DP"                     *FD57: 44 50          'DP'
        FCB     $00                      *FD59: 00             '.'
        FDB     DPCMD                    *FD5A: F9 09          '..'
        FCC     "PC"                     *FD5C: 50 43          'PC'
        FCB     $00                      *FD5E: 00             '.'
        FDB     PCCMD                    *FD5F: F9 6D          '.m'
        FCC     "R"                      *FD61: 52             'R'
        FCB     $00                      *FD62: 00             '.'
        FDB     RCMD                     *FD63: F9 C6          '..'
        FCC     "SP"                     *FD65: 53 50          'SP'
        FCB     $00                      *FD67: 00             '.'
        FDB     SPCMD                    *FD68: F9 5B          '.['
        FCC     "U"                      *FD6A: 55             'U'
        FCB     $00                      *FD6B: 00             '.'
        FDB     UCMD                     *FD6C: F9 61          '.a'
        FCC     "X"                      *FD6E: 58             'X'
        FCB     $00                      *FD6F: 00             '.'
        FDB     XCMD                     *FD70: F9 65          '.e'
        FCC     "Y"                      *FD72: 59             'Y'
        FCB     $00                      *FD73: 00             '.'
        FDB     YCMD                     *FD74: F9 69          '.i'
        FCC     "E"                      *FD76: 45             'E'
        FCB     $00                      *FD77: 00             '.'
        FDB     ECMD                     *FD78: F9 9F          '..'
        FCC     "V"                      *FD7A: 56             'V'
        FCB     $00                      *FD7B: 00             '.'
        FDB     VCMD                     *FD7C: FA 18          '..'
        FCC     "C"                      *FD7E: 43             'C'
        FCB     $00                      *FD7F: 00             '.'
        FDB     CCMD                     *FD80: FA 48          '.H'
        FCC     "M"                      *FD82: 4D             'M'
        FCB     $00                      *FD83: 00             '.'
        FDB     MCMD                     *FD84: FA 90          '..'
        FCC     "G"                      *FD86: 47             'G'
        FCB     $00                      *FD87: 00             '.'
        FDB     GCMD                     *FD88: FA B5          '..'
        FCC     "J"                      *FD8A: 4A             'J'
        FCB     $00                      *FD8B: 00             '.'
        FDB     JCMD                     *FD8C: FA DE          '..'
        FCC     "BK"                     *FD8E: 42 4B          'BK'
        FCB     $00                      *FD90: 00             '.'
        FDB     BKCMD                    *FD91: FA ED          '..'
        FCC     "KB"                     *FD93: 4B 42          'KB'
        FCB     $00                      *FD95: 00             '.'
        FDB     KBCMD                    *FD96: FB 2E          '..'
        FCC     "N"                      *FD98: 4E             'N'
        FCB     $00                      *FD99: 00             '.'
        FDB     SSCMD                    *FD9A: FB 69          '.i'
        FCC     "GT"                     *FD9C: 47 54          'GT'
        FCB     $00                      *FD9E: 00             '.'
        FDB     GTCMD                    *FD9F: FB 72          '.r'
        FCC     "L"                      *FDA1: 4C             'L'
        FCB     $00                      *FDA2: 00             '.'
        FDB     LCMD                     *FDA3: FB 92          '..'
        FCC     "VL"                     *FDA5: 56 4C          'VL'
        FCB     $00                      *FDA7: 00             '.'
        FDB     VLCMD                    *FDA8: FB 8C          '..'
        FCB     $00,$00                  *FDAA: 00 00          '..'
_N???   FCB     $16,$FB,$1E              *FDAC: 16 FB 1E       '...'
_NCOMAND LBRA    COMAND                   *FDAF: 16 FA BC       '...'
_NCMDERR LBRA    CMDERR                   *FDB2: 16 FA F3       '...'
_NSSBK  LBRA    SSBK                     *FDB5: 16 FF 19       '...'
_NSSCMD LBRA    SSCMD                    *FDB8: 16 FD AE       '...'
        LBRA    LCMD                     *FDBB: 16 FD D4       '...'
        LBRA    LCM2                     *FDBE: 16 FD ED       '...'
MFDC1   FCC     "?"                      *FDC1: 3F             '?'
        FCB     $01,$83,$1D,$8C          *FDC2: 01 83 1D 8C    '....'
        FCC     "-"                      *FDC6: 2D             '-'
        FCB     $8E                      *FDC7: 8E             '.'
        FCC     "="                      *FDC8: 3D             '='
        FCB     $CE                      *FDC9: CE             '.'
        FCC     "]"                      *FDCA: 5D             ']'
        FCB     $93,$19,$9C              *FDCB: 93 19 9C       '...'
        FCC     ")"                      *FDCE: 29             ')'
        FCB     $9E                      *FDCF: 9E             '.'
        FCC     "9"                      *FDD0: 39             '9'
        FCB     $9F                      *FDD1: 9F             '.'
        FCC     "I"                      *FDD2: 49             'I'
        FCB     $DE                      *FDD3: DE             '.'
        FCC     "Y"                      *FDD4: 59             'Y'
        FCB     $DF                      *FDD5: DF             '.'
        FCC     "i"                      *FDD6: 69             'i'
        FCB     $A3,$1A,$AC              *FDD7: A3 1A AC       '...'
        FCC     "*"                      *FDDA: 2A             '*'
        FCB     $AE                      *FDDB: AE             '.'
        FCC     ":"                      *FDDC: 3A             ':'
        FCB     $AF                      *FDDD: AF             '.'
        FCC     "J"                      *FDDE: 4A             'J'
        FCB     $EE                      *FDDF: EE             '.'
        FCC     "Z"                      *FDE0: 5A             'Z'
        FCB     $EF                      *FDE1: EF             '.'
        FCC     "j"                      *FDE2: 6A             'j'
        FCB     $B3,$1B,$BC              *FDE3: B3 1B BC       '...'
        FCC     "+"                      *FDE6: 2B             '+'
        FCB     $BE                      *FDE7: BE             '.'
        FCC     ";"                      *FDE8: 3B             ';'
        FCB     $BF                      *FDE9: BF             '.'
        FCC     "K"                      *FDEA: 4B             'K'
        FCB     $FE                      *FDEB: FE             '.'
        FCC     "["                      *FDEC: 5B             '['
        FCB     $FF                      *FDED: FF             '.'
        FCC     "k"                      *FDEE: 6B             'k'
        FCB     $00,$00                  *FDEF: 00 00          '..'
MFDF1   FCC     "?"                      *FDF1: 3F             '?'
        FCB     $01,$83,$1D,$8C          *FDF2: 01 83 1D 8C    '....'
        FCC     "-"                      *FDF6: 2D             '-'
        FCB     $93,$19,$9C              *FDF7: 93 19 9C       '...'
        FCC     ")"                      *FDFA: 29             ')'
        FCB     $A3,$1A,$AC              *FDFB: A3 1A AC       '...'
        FCC     "*"                      *FDFE: 2A             '*'
        FCB     $B3,$1B,$BC              *FDFF: B3 1B BC       '...'
        FCC     "+"                      *FE02: 2B             '+'
        FCB     $00,$00                  *FE03: 00 00          '..'
RDLIN   LDX     >IOBUF                   *FE05: BE 9F 04       '...'
RDLN2   BSR     A1INPCHR                 *FE08: 8D 42          '.B'
        CMPA    #$08                     *FE0A: 81 08          '..'    BACKSPACE?
        BNE     RDLN3                    *FE0C: 26 09          '&.'    SKIP IF NOT
        CMPX    >IOBUF                   *FE0E: BC 9F 04       '...'   DONT BACKSPACE...
        BLS     RDLN2                    *FE11: 23 F5          '#.'    PAST BEG OF LINE
        LEAX    -$01,X                   *FE13: 30 1F          '0.'
        BRA     RDLN2                    *FE15: 20 F1          ' .'
RDLN3   CMPA    #$18                     *FE17: 81 18          '..'    CTRL-X, CANCEL
        BNE     RDLN4                    *FE19: 26 06          '&.'
        LDX     >IOBUF                   *FE1B: BE 9F 04       '...'
        ORCC    #$01                     *FE1E: 1A 01          '..'
        RTS                              *FE20: 39             '9'
RDLN4   STA     ,X+                      *FE21: A7 80          '..'
        CMPA    #$0D                     *FE23: 81 0D          '..'    WAS IT EOL?
        BNE     RDLN2                    *FE25: 26 E1          '&.'    IF NOT GET ANOTHER
        LEAX    -$01,X                   *FE27: 30 1F          '0.'
        RTS                              *FE29: 39             '9'
* SUBROUTINE CHKEOL
* SKIP SPACES IN INPUT BUFFER
* THEN CHECK IF NEXT CHARACTER
* IS A CARRIAGE RETURN
CHKEOL  BSR     SKIPSP                   *FE2A: 8D 03          '..'
        CMPA    #$0D                     *FE2C: 81 0D          '..'
        RTS                              *FE2E: 39             '9'
SKIPSP  LDA     ,X+                      *FE2F: A6 80          '..'
        CMPA    #$20                     *FE31: 81 20          '. '
        BEQ     SKIPSP                   *FE33: 27 FA          ''.'
        LEAX    -$01,X                   *FE35: 30 1F          '0.'
        RTS                              *FE37: 39             '9'
WRTLIN  PSHS    D                        *FE38: 34 06          '4.'
        LDA     #$0D                     *FE3A: 86 0D          '..'
        BSR     A1OUTCHR                 *FE3C: 8D 3B          '.;'
        TST     >USECRLF                 *FE3E: 7D 9F 0A       '}..'
        BEQ     WRTLN2                   *FE41: 27 04          ''.'
        LDA     #$0A                     *FE43: 86 0A          '..'
        BSR     A1OUTCHR                 *FE45: 8D 32          '.2'
WRTLN2  LDX     >IOBUF                   *FE47: BE 9F 04       '...'
        PULS    PC,D                     *FE4A: 35 86          '5.'
A1INPCHR PSHS    X                        *FE4C: 34 10          '4.'    SAVE XREG
        LDX     >ACVECT                  *FE4E: BE 9F 0B       '...'   GET ACIA ADDRESS
A1INPCH2 LDA     $01,X                    *FE51: A6 01          '..'    READ ACIA STATUS REGISTER
        ANDA    #$08                     *FE53: 84 08          '..'    MASK READY BIT
        BEQ     A1INPCH2                 *FE55: 27 FA          ''.'    WAIT TIL READY
        LDA     ,X                       *FE57: A6 84          '..'    GET CHAR
        ANDA    #$7F                     *FE59: 84 7F          '..'    MASK OUT PARITY
        PULS    X                        *FE5B: 35 10          '5.'    RESTORE X REG
        TST     >ACECHO                  *FE5D: 7D 9F 09       '}..'   TEST ECHO?
        BNE     ZFE64                    *FE60: 26 02          '&.'
        BSR     A1OUTCHR                 *FE62: 8D 15          '..'    OUTPUT IF ECHO ON
ZFE64   CMPA    #$0D                     *FE64: 81 0D          '..'    CR?
        BEQ     _ENDLN                   *FE66: 27 05          ''.'    IF SO SKIP LF CHECK
        CMPA    #$0A                     *FE68: 81 0A          '..'    LF?
        BEQ     A1INPCHR                 *FE6A: 27 E0          ''.'    GET NEXT
        RTS                              *FE6C: 39             '9'     ELSE RETURN
_ENDLN  TST     >USECRLF                 *FE6D: 7D 9F 0A       '}..'
        BEQ     ZFE78                    *FE70: 27 06          ''.'
        LDA     #$0A                     *FE72: 86 0A          '..'
        BSR     A1OUTCHR                 *FE74: 8D 03          '..'    SEND LF
        LDA     #$0D                     *FE76: 86 0D          '..'
ZFE78   RTS                              *FE78: 39             '9'
A1OUTCHR PSHS    X,D                      *FE79: 34 16          '4.'    SAVE REGISTERS
        LDX     >ACVECT                  *FE7B: BE 9F 0B       '...'   GET FE7B ADDRESS
OUTCH2  LDB     $01,X                    *FE7E: E6 01          '..'    GET THE ACIA STATUS
        ANDB    #$10                     *FE80: C4 10          '..'    BUSY?
        BEQ     OUTCH2                   *FE82: 27 FA          ''.'    IF SO, TRY AGAIN
        STA     ,X                       *FE84: A7 84          '..'    SEND A
        PULS    PC,X,D                   *FE86: 35 96          '5.'    RESTORE REGISTERS
CPYSTR  PSHS    D                        *FE88: 34 06          '4.'
CPYST2  LDA     ,Y+                      *FE8A: A6 A0          '..'
        BEQ     CPYST4                   *FE8C: 27 0D          ''.'
        CMPA    #$0A                     *FE8E: 81 0A          '..'
        BNE     CPYST3                   *FE90: 26 05          '&.'
        TST     >USECRLF                 *FE92: 7D 9F 0A       '}..'
        BEQ     CPYST2                   *FE95: 27 F3          ''.'
CPYST3  BSR     A1OUTCHR                 *FE97: 8D E0          '..'
        BRA     CPYST2                   *FE99: 20 EF          ' .'
CPYST4  PULS    PC,D                     *FE9B: 35 86          '5.'
* PRINT DREG SPACE IN HEX
PRHX2SP BSR     PR2HX                    *FE9D: 8D 09          '..'
        TFR     B,A                      *FE9F: 1F 98          '..'
PRHXSP  BSR     PR2HX                    *FEA1: 8D 05          '..'    PRINT A BYTE IN HEX & SPACE
        LDA     #$20                     *FEA3: 86 20          '. '
        BSR     A1OUTCHR                 *FEA5: 8D D2          '..'    PRINT SPACE
        RTS                              *FEA7: 39             '9'
PR2HX   PSHS    A                        *FEA8: 34 02          '4.'    PRINT BYTE IN HEX
        BSR     PRHINB                   *FEAA: 8D 04          '..'
        PULS    A                        *FEAC: 35 02          '5.'
        BRA     PRLONB                   *FEAE: 20 04          ' .'
PRHINB  LSRA                             *FEB0: 44             'D'     SHIFT HI NIBBLE TO LO
        LSRA                             *FEB1: 44             'D'
        LSRA                             *FEB2: 44             'D'
        LSRA                             *FEB3: 44             'D'
PRLONB  ANDA    #$0F                     *FEB4: 84 0F          '..'    PRINT HEX LO NIBBLE
        ADDA    #$90                     *FEB6: 8B 90          '..'
        DAA                              *FEB8: 19             '.'     Bug here in v09?.
                                         * If entered PRHX2SP with Half-carry set
                                         * (due to ASLA's in HEX2DEC called @ $F938), the first HEX DIGIT
                                         * could erroneously have bit 5 set.
                                         * So, E command value entry with odd HI nibble can cause this.
        ADCA    #$40                     *FEB9: 89 40          '.@'
        DAA                              *FEBB: 19             '.'
        BSR     A1OUTCHR                 *FEBC: 8D BB          '..'
ZFEBE   RTS                              *FEBE: 39             '9'
HEX4BIN BSR     HEX2DEC                  *FEBF: 8D 0A          '..'
        BCS     ZFEBE                    *FEC1: 25 FB          '%.'
        PSHS    A                        *FEC3: 34 02          '4.'
        BSR     HEX2DEC                  *FEC5: 8D 04          '..'
        TFR     A,B                      *FEC7: 1F 89          '..'
        PULS    PC,A                     *FEC9: 35 82          '5.'
HEX2DEC BSR     _CHEXBIN                 *FECB: 8D 13          '..'    CONVERT NEXT CHAR TO DECIMAL
        BCS     ZFEDF                    *FECD: 25 10          '%.'    RETURN IF NOT HEX
        ASLA                             *FECF: 48             'H'
        ASLA                             *FED0: 48             'H'
        ASLA                             *FED1: 48             'H'     MULTIPLY BY 16
        ASLA                             *FED2: 48             'H'
        TFR     A,B                      *FED3: 1F 89          '..'
        BSR     _CHEXBIN                 *FED5: 8D 09          '..'
        BCS     ZFEBE                    *FED7: 25 E5          '%.'    RETURN IF NOT HEX
        PSHS    B                        *FED9: 34 04          '4.'
        ADDA    ,S                       *FEDB: AB E4          '..'    ADD NEW DIGIT TO SUM
        LEAS    $01,S                    *FEDD: 32 61          '2a'
ZFEDF   RTS                              *FEDF: 39             '9'
_CHEXBIN LDA     ,X+                      *FEE0: A6 80          '..'

* Subroutine HEXDEC - convert hexadecimal
* ASCII char to decimal
HEXDEC  SUBA    #$30                     *FEE2: 80 30          '.0'    CONVERT FROM ASCII
        BMI     _SETC                    *FEE4: 2B 11          '+.'    IF <'0' SET CARRY, RETURN
        CMPA    #9                       *FEE6: 81 09          '..'
        BLE     _CLRC                    *FEE8: 2F 0A          '/.'    IF <='9', CLEAR CARRY, RETURN
        CMPA    #17                      *FEEA: 81 11          '..'    'A'
        BMI     _SETC                    *FEEC: 2B 09          '+.'    SET CARRY, RETURN IF < 'A'
        CMPA    #22                      *FEEE: 81 16          '..'    'F'
        BGT     _SETC                    *FEF0: 2E 05          '..'    SET CARRY, RETURN IF > 'F'
        SUBA    #$07                     *FEF2: 80 07          '..'    [A-F] ARE LEFT
_CLRC   ANDCC   #$FE                     *FEF4: 1C FE          '..'    CLEAR CARRY, RETURN
        RTS                              *FEF6: 39             '9'
_SETC   ORCC    #$01                     *FEF7: 1A 01          '..'    SET   CARRY, RETURN
        RTS                              *FEF9: 39             '9'
GETLN   LDX     >IOBUF                   *FEFA: BE 9F 04       '...'   GET A LINE FROM INPUT, MAX CHARS IN B
ZFEFD   LBSR    A1INPCHR                 *FEFD: 17 FF 4C       '..L'
        STA     ,X+                      *FF00: A7 80          '..'
        LBSR    HEXDEC                   *FF02: 17 FF DD       '...'
        BCS     ZFF0F                    *FF05: 25 08          '%.'
        DECB                             *FF07: 5A             'Z'
        BNE     ZFEFD                    *FF08: 26 F3          '&.'
        LDA     #$0D                     *FF0A: 86 0D          '..'
        STA     ,X+                      *FF0C: A7 80          '..'
        RTS                              *FF0E: 39             '9'
ZFF0F   LDA     ,-X                      *FF0F: A6 82          '..'
        CMPA    #$08                     *FF11: 81 08          '..'
        BNE     ZFF1E                    *FF13: 26 09          '&.'
        CMPX    >IOBUF                   *FF15: BC 9F 04       '...'
        BEQ     ZFEFD                    *FF18: 27 E3          ''.'
        LEAX    -$01,X                   *FF1A: 30 1F          '0.'
        BRA     ZFEFD                    *FF1C: 20 DF          ' .'
ZFF1E   ORCC    #$01                     *FF1E: 1A 01          '..'
        RTS                              *FF20: 39             '9'

*      TTL    6809 UTILITY SUBROUTINE PACKAGE

* Input Conversion Subroutines
* These subroutines convert an usnigned binary
* number (passed in B or D, or input buffer at Y)
* to an ASCII character
* string starting at the address passed in the
* X register, which will return with the address
* following the string. All other registers
* except CC are preserved

* SUBROUTINES C2HEX AND C4HEX -
* LOAD AUTOINCRMENT FROM Y AND
* CONVERT TO HEX
C4HEX   LDD     ,Y++                     *FF21: EC A1          '..'    LOAD D FROM INPUT BUFFER

* Subroutine BIN4HS - Convert word in D reg to
* four-character hexadecimal followed by a space.
BIN4HS  BSR     BIN4HX                   *FF23: 8D 0E          '..'    PERFORM CONVERSION
        BRA     PUTSPC                   *FF25: 20 04          ' .'    GO OUTPUT SPACE AND RTS

C2HEX   LDB     ,Y+                      *FF27: E6 A0          '..'    LOAD B FROM INPUT BUFFER

* Subroutine BIN2HS - Convert word in B reg to
* two-character hexadecimal followed by a space.
BIN2HS  BSR     BIN2HX                   *FF29: 8D 0E          '..'    PERFORM CONVERSION
* FALL THROUGH TO PUTSPC AND RETURN

* Subroutine PUTSPC - Put a space character in
* the output buffer and increment pointer (x)
PUTSPC  PSHS    A                        *FF2B: 34 02          '4.'
        LDA     #$20                     *FF2D: 86 20          '. '
        STA     ,X+                      *FF2F: A7 80          '..'
        PULS    PC,A                     *FF31: 35 82          '5.'

* Subroutine BIN4HX - Convert word in D reg to
* four-character hexadecimal.
BIN4HX  EXG     A,B                      *FF33: 1E 89          '..'
        BSR     BIN2HX                   *FF35: 8D 02          '..'
        TFR     A,B                      *FF37: 1F 89          '..'
* FALL THROUGH TO CONVERT LOW BYTE

* Subroutine BIN2HX - convert byte in B reg to
* two-character hexadecimal.
BIN2HX  PSHS    B                        *FF39: 34 04          '4.'
        ANDB    #$F0                     *FF3B: C4 F0          '..'
        LSRB                             *FF3D: 54             'T'
        LSRB                             *FF3E: 54             'T'
        LSRB                             *FF3F: 54             'T'
        LSRB                             *FF40: 54             'T'
        BSR     HEXCHR                   *FF41: 8D 04          '..'
        PULS    B                        *FF43: 35 04          '5.'
        ANDB    #$0F                     *FF45: C4 0F          '..'
HEXCHR  CMPB    #$09                     *FF47: C1 09          '..'
        BLS     HXCHR2                   *FF49: 23 02          '#.'
        ADDB    #$07                     *FF4B: CB 07          '..'
HXCHR2  ADDB    #$30                     *FF4D: CB 30          '.0'
        STB     ,X+                      *FF4F: E7 80          '..'
        RTS                              *FF51: 39             '9'
ZFF52   LDA     #$0D                     *FF52: 86 0D          '..'
        STA     ,X+                      *FF54: A7 80          '..'
        TST     >USECRLF                 *FF56: 7D 9F 0A       '}..'
        BEQ     ZFF5F                    *FF59: 27 04          ''.'
        LDA     #$0A                     *FF5B: 86 0A          '..'
        STA     ,X+                      *FF5D: A7 80          '..'
ZFF5F   PSHS    X                        *FF5F: 34 10          '4.'
        LDX     >IOBUF                   *FF61: BE 9F 04       '...'
ZFF64   LDA     ,X+                      *FF64: A6 80          '..'
        LBSR    A1OUTCHR                 *FF66: 17 FF 10       '...'
        CMPX    ,S                       *FF69: AC E4          '..'
        BCS     ZFF64                    *FF6B: 25 F7          '%.'
        LEAS    $02,S                    *FF6D: 32 62          '2b'
        LDX     >IOBUF                   *FF6F: BE 9F 04       '...'
        RTS                              *FF72: 39             '9'
ZFF73   PSHS    D                        *FF73: 34 06          '4.'
        LDD     ,U++                     *FF75: EC C1          '..'
        STD     ,X++                     *FF77: ED 81          '..'
        LDD     ,U++                     *FF79: EC C1          '..'
        STD     ,X++                     *FF7B: ED 81          '..'
        PULS    PC,D                     *FF7D: 35 86          '5.'
        FCB     $FF                      *FF7F: FF             '.'

* ### I/O OVERLAP
        FCB     $FF,$FF,$FF,$FF,$FF,$FF  *FF80: FF FF FF FF FF FF '......'
        FCB     $FF,$FF,$FF,$FF,$FF,$FF  *FF86: FF FF FF FF FF FF '......'
        FCB     $FF,$FF,$FF,$FF,$FF,$FF  *FF8C: FF FF FF FF FF FF '......'
        FCB     $FF,$FF,$FF,$FF,$FF,$FF  *FF92: FF FF FF FF FF FF '......'
        FCB     $FF,$FF,$FF,$FF,$FF,$FF  *FF98: FF FF FF FF FF FF '......'
        FCB     $FF,$FF,$FF,$FF,$FF,$FF  *FF9E: FF FF FF FF FF FF '......'
        FCB     $FF,$FF,$FF,$FF,$FF,$FF  *FFA4: FF FF FF FF FF FF '......'
        FCB     $FF,$FF,$FF,$FF,$FF,$FF  *FFAA: FF FF FF FF FF FF '......'
        FCB     $FF,$FF,$FF,$FF,$FF,$FF  *FFB0: FF FF FF FF FF FF '......'
        FCB     $FF,$FF,$FF,$FF,$FF,$FF  *FFB6: FF FF FF FF FF FF '......'
        FCB     $FF,$FF,$FF,$FF,$FF,$FF  *FFBC: FF FF FF FF FF FF '......'
        FCB     $FF,$FF                  *FFC2: FF FF          '..'
PIADDR0 FCB     $FF,$FF,$FF,$FF          *FFC4: FF FF FF FF    '....'
PIADDR1 FCB     $FF,$FF,$FF,$FF          *FFC8: FF FF FF FF    '....'
PIADDR2 FCB     $FF,$FF,$FF,$FF          *FFCC: FF FF FF FF    '....'
ACADDR0 FCB     $FF,$FF,$FF,$FF          *FFD0: FF FF FF FF    '....'
ACADDR1 FCB     $FF,$FF,$FF,$FF          *FFD4: FF FF FF FF    '....'
TIMER   FCB     $FF,$FF,$FF,$FF,$FF,$FF  *FFD8: FF FF FF FF FF FF '......'
        FCB     $FF,$FF                  *FFDE: FF FF          '..'

* ### VECTORS
        FCB     $FF,$FF,$FF,$FF,$FF,$FF  *FFE0: FF FF FF FF FF FF '......'
        FCB     $FF,$FF,$FF,$FF,$FF,$FF  *FFE6: FF FF FF FF FF FF '......'
        FCB     $FF,$FF,$FF,$FF          *FFEC: FF FF FF FF    '....'
        FCB     $00,$00                  *FFF0: 00 00          '..'    RESERVED
svec_SWI3 FDB     hdlr_SWI3                *FFF2: FD 02          '..'
svec_SWI2 FDB     hdlr_SWI2                *FFF4: FD 06          '..'
svec_FIRQ FDB     hdlr_FIRQ                *FFF6: FD 0E          '..'
svec_IRQ FDB     hdlr_IRQ                 *FFF8: FD 12          '..'
svec_SWI FDB     hdlr_SWI                 *FFFA: FD 0A          '..'
svec_NMI FDB     hdlr_NMI                 *FFFC: FD 16          '..'
svec_RST FDB     hdlr_RST                 *FFFE: F8 00          '..'

        END
