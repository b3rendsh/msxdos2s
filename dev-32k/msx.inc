; msx.inc
;
; MSX constants and definitions for DOS1 and DOS2
; ------------------------------------------------------------------------------

BOT16K	EQU	0C000H			; lower of 16 KB RAM
BOT32K	EQU	08000H			; lower of 32 KB RAM
BOOTSZ	EQU	256			; maximum size of MSX bootloader in bootsector
TMPSTK	EQU	BOT16K+BOOTSZ+256	; location of temporary stack
TBASE	EQU	00100H			; start of MSX-DOS

; ------------------------------------------------------------------------------
; P0: BIOS or BDOS 2 kernel

WBOOT  	EQU     0000H   	; --SLI
KBDOS   EQU     0005H
RDSLT   EQU     000CH
WRSLT	EQU	0014H
CALSLT  EQU     001CH
ENASLT  EQU     0024H
IDBYT0  EQU     002BH   	; ---L-
IDBYT2  EQU     002DH		; ---L-
CALLF	EQU	0030H
CHAR_16 EQU     0034H		; ----I
KEYINT	EQU	0038H
SSLOT   EQU     003BH  		; ----I
SSLOTE  EQU     004BH  		; -C---	
SSLOTL	EQU	0046H
LDIRMV  EQU     0059H  		; -C---
LDIRVM  EQU     005CH  		; -C---
ALL_SEG	EQU	005CH		; BDOS
FRE_SEG	EQU	005FH		; BDOS
DBUF    EQU     0080H  		; --S-I
A0080	EQU	0080H		; BDOS
A0083	EQU	0083H		; BDOS
A0086	EQU	0086H		; BODS
CHSNS   EQU     009CH
CHGET   EQU     009FH  		; -C--I
CHPUT   EQU     00A2H   	; -C--I
LPTOUT  EQU     00A5H
LPTSTT  EQU     00A8H
BREAKX  EQU     00B7H
CKCNTC  EQU     00BDH   	; -C---
ERAFNK  EQU     00CCH   	; -C---
TOTEXT  EQU     00D2H   	; -C---
SNSMAT  EQU     0141H   	; -C---
PHYDIO	EQU	0144H
KILBUF  EQU     0156H   	; ----I
CALBAS  EQU     0159H   	; JC---
DECSUB  EQU     268CH   	; -C---
DECADD  EQU     269AH   	; J----
DECDIV  EQU     289FH   	; -C---
VMOVE   EQU     2EF3H   	; -C---
VMOVFM  EQU     2F08H   	; J----
VMOVMF  EQU     2F10H   	; -C---
MAKINT  EQU     2F99H   	; JC---
CONDS   EQU     3042H   	; -C---
INT     EQU     30D1H		; integer part of float
SGNMUL  EQU     325CH   	; -C---
NULSTR  EQU     3FD6H   	; ----I

; ------------------------------------------------------------------------------
; P1: disk rom or basic rom

A4010	EQU	04010H		; DSKIO
A401F	EQU	0401FH		; MTOFF
BASENT  EQU     04022H		; Start DiskBASIC
A4043	EQU	04043H		; EXTBIO handler DOS2 
A4049	EQU	04049H		; H.TIMI handler
ERROR   EQU     0406FH          ; BASIC error
READYR  EQU     0409BH          ; restart BASIC
NTSTOP  EQU     04173H          ; execute statement
LINKER  EQU     04253H          ; recalculate linepointers
NEWSTT  EQU     04601H          ; execution loop
CHRGTR  EQU     04666H          ; CHRGTR
FLTLIN  EQU     046FFH          ; convert to SNG
INTID2  EQU     04756H          ; evaluate word operand and check for 0-32767 range
FINPRT  EQU     04AFFH          ; output back to screen
FRMEQL  EQU     04C5FH          ; evaluate =expression
FRMEVL  EQU     04C64H          ; evaluate expression
DOCNVF  EQU     0517AH          ; convert DAC to other type
GETIN2  EQU     0520FH          ; evaluate integer operand
GTBYTC  EQU     0521BH          ; evaluate next byte operand
GETBYT  EQU     0521CH          ; evaluate byte operand
CONINT  EQU     0521FH          ; convert to byte
JPFOUT  EQU     0537BH          ; convert DAC to text, unformatted (JPFOUT is not an offical name)
GETUIN  EQU     0542FH          ; evaluate address operand
GETUI   EQU     05432H          ; convert address to integer
SCCPTR  EQU     054F7H          ; convert pointers to linenumbers
GETYPR  EQU     05597H          ; GETYPR
PTRGET  EQU     05EA4H          ; get address of variable
STRINI  EQU     06627H          ; allocate temp string
GETSPA  EQU     0668EH          ; allocate string space
FRESTR  EQU     067D0H          ; free temporary string
FILEVL  EQU     06A0EH          ; evaluate filespecification
FILIDX  EQU     06A6DH          ; get i/o channel pointer
OPNFIL  EQU     06AFAH          ; open i/o channel
CLSFIL  EQU     06B24H          ; close i/o channel
CLSALL  EQU     06C1CH          ; close all i/o channels
NOSKCR  EQU     06E41H          ; resume character putback routine
BSAVE   EQU     06E92H          ; start of BSAVE routine
BLOAD   EQU     06EC6H          ; start of BLOAD routine
BLDFIN  EQU     06EF4H          ; finish BLOAD
ADRGET  EQU     06F0BH          ; evaluate address operand (BLOAD/SAVE)
M6F0B   EQU     06F0BH          ; evaluate address operand (BLOAD/SAVE) (DOS1)
PARDEV  EQU     06F15H          ; skip strong cassette devicecheck
CRDONZ  EQU     07323H          ; newline to OUTDO if not at start of line
CRDO    EQU     07328H          ; newline to OUTDO
M739A   EQU     0739AH          ; quit loading & start (headloop/executing)
M7D17   EQU     07D17H          ; continue start of MSX-BASIC without executing BASIC programs in ROM
M7D2F   EQU     07D2FH          ; address initialize BASIC screen
M7D31   EQU     07D31H          ; BASIC initscreen (without INITXT & CNSDFG)
M7DE9   EQU     07DE9H          ; start MSX-BASIC program in ROM
M7E14   EQU     07E14H          ; start MSX-BASIC program in ROM (DOS1)


; ------------------------------------------------------------------------------
; P2: workarea DOS 2

ISB062	EQU	0B062H
D_B064  EQU     0B064H  	; --SL-
I_B066  EQU     0B066H  	; ----I
ISB069  EQU     0B069H  	; ----I
D_B06C  EQU     0B06CH  	; --S-I
I_B0D0  EQU     0B0D0H  	; ----I
I_B1CF  EQU     0B1CFH  	; ----I
I_B1D0  EQU     0B1D0H  	; ----I
ISB1D1  EQU     0B1D1H  	; ----I
ISB1D2  EQU     0B1D2H  	; ----I
I_B2D4  EQU     0B2D4H  	; ----I
D_B400  EQU     0B400H  	; --SL-
I_B6D4  EQU     0B6D4H  	; ----I
ISB6F2  EQU     0B6F2H  	; ----I
DSB6FA  EQU     0B6FAH  	; --S--
DSB6FB  EQU     0B6FBH  	; --S--
DSB6FD  EQU     0B6FDH  	; ---L-
I_B8D4  EQU     0B8D4H  	; ----I
I_B8F4  EQU     0B8F4H  	; ----I
ISB901  EQU     0B901H  	; ----I
I_B910  EQU     0B910H  	; ----I
I_B91B  EQU     0B91BH  	; ----I
I_B926  EQU     0B926H  	; ----I
ISB931  EQU     0B931H  	; ----I
ISB933  EQU     0B933H  	; ----I
ISB973  EQU     0B973H  	; ----I
I_B975  EQU     0B975H  	; ----I
D_B976  EQU     0B976H  	; --SL-
D_B977  EQU     0B977H  	; --SL-
I_B99A  EQU     0B99AH  	; ----I
I_B9DA  EQU     0B9DAH  	; ----I
ISB9DB  EQU     0B9DBH  	; ----I
DSB9E8  EQU     0B9E8H  	; ---L-
D_B9EF  EQU     0B9EFH  	; ---LI
DSB9F1  EQU     0B9F1H  	; --S--
D_B9F3  EQU     0B9F3H  	; ---L-
I_B9F4  EQU     0B9F4H  	; ----I
D_B9F8  EQU     0B9F8H  	; ---L-
I_B9FA  EQU     0B9FAH  	; ----I
I_B9FF  EQU     0B9FFH  	; ----I
D_BA07  EQU     0BA07H  	; ---LI
DSBA08  EQU     0BA08H  	; ---L-
D_BA09  EQU     0BA09H  	; --S--
DSBA0B  EQU     0BA0BH  	; ---L-
I_BA1A  EQU     0BA1AH  	; ----I
I_BA23  EQU     0BA23H  	; ----I
I_BA25  EQU     0BA25H  	; ----I
D_BA33  EQU     0BA33H  	; --SL-
I_BA35  EQU     0BA35H  	; ----I
I_BA75  EQU     0BA75H  	; ----I
D_BB76  EQU     0BB76H  	; ---LI
D_BB77  EQU     0BB77H  	; --SL-
D_BB78  EQU     0BB78H  	; --SL-
D_BB7A  EQU     0BB7AH  	; --SL-
D_BB7B  EQU     0BB7BH  	; --SL-
D_BB7C  EQU     0BB7CH  	; --SL-
D_BB7D  EQU     0BB7DH  	; --SL-
D_BB7F  EQU     0BB7FH  	; --SL-
D_BB80  EQU     0BB80H  	; --SLI		IY variables base address (kernel uses: BB80 - BC00)
D_BB82  EQU     0BB82H  	; --SL-
D_BB84  EQU     0BB84H  	; --SL-
D_BB86  EQU     0BB86H  	; --SLI
D_BB87  EQU     0BB87H  	; --S--
DSBB89  EQU     0BB89H  	; --S--
D_BB8A  EQU     0BB8AH  	; --S-I
D_BB8B  EQU     0BB8BH  	; --SL-
D_BB8D  EQU     0BB8DH  	; --SLI
D_BB8E  EQU     0BB8EH  	; --S--
DSBB8F  EQU     0BB8FH  	; --S--
D_BB90  EQU     0BB90H  	; --SL-
D_BB91  EQU     0BB91H  	; --SL-
D_BB92  EQU     0BB92H  	; ---LI
DSBB93  EQU     0BB93H  	; --S--
D_BB95  EQU     0BB95H  	; --SL-
D_BB96  EQU     0BB96H  	; --SL-
D_BB98  EQU     0BB98H  	; --SL-
DSBB99  EQU     0BB99H  	; ---L-
D_BB9A  EQU     0BB9AH  	; --SL-
D_BB9C  EQU     0BB9CH  	; ---L-
D_BB9E  EQU     0BB9EH  	; --SL-
DSBBA2  EQU     0BBA2H  	; --S--
D_BBA3  EQU     0BBA3H  	; --SL-
D_BBA5  EQU     0BBA5H  	; --S-I
DSBBA6  EQU     0BBA6H		; --S--
D_BBA7  EQU     0BBA7H  	; --SL-
D_BBAA  EQU     0BBAAH  	; --SL-
D_BBAB  EQU     0BBABH  	; --SL-
D_BBAD  EQU     0BBADH  	; --SL-
DSBBAF  EQU     0BBAFH  	; --S--
D_BBB2  EQU     0BBB2H  	; --S--
D_BBB4  EQU     0BBB4H  	; --SL-
D_BBB6  EQU     0BBB6H  	; --SL-
D_BBB8  EQU     0BBB8H  	; --SL-
D_BBB9  EQU     0BBB9H  	; --SL-
D_BBBB  EQU     0BBBBH  	; --SL-
D_BBBC  EQU     0BBBCH  	; --SL-
D_BBBE  EQU     0BBBEH  	; --SL-
DSBBBF  EQU     0BBBFH  	; --S--
D_BBC0  EQU     0BBC0H  	; --SL-
D_BBC2  EQU     0BBC2H  	; --SL-
D_BBC4  EQU     0BBC4H  	; --SL-
I_BBC5  EQU     0BBC5H  	; ----I
I_BBC6  EQU     0BBC6H  	; ----I
I_BBD2  EQU     0BBD2H  	; ----I
D_BBDE  EQU     0BBDEH  	; --S-I
D_BBDF  EQU     0BBDFH  	; ---L-
D_BBE0  EQU     0BBE0H  	; --SL-
D_BBE1  EQU     0BBE1H  	; --SL-
D_BBE2  EQU     0BBE2H  	; --SL-
D_BBE4  EQU     0BBE4H  	; --SL-
D_BBE6  EQU     0BBE6H  	; --SL-
D_BBE8  EQU     0BBE8H  	; --SL-
D_BBEA  EQU     0BBEAH  	; --SL-
D_BBEB  EQU     0BBEBH  	; --SL-
D_BBED  EQU     0BBEDH  	; --SL-
D_BBEE  EQU     0BBEEH  	; --SLI
D_BBF0  EQU     0BBF0H  	; --SL-
D_BBF2  EQU     0BBF2H  	; --S-I
D_BBF4  EQU     0BBF4H  	; --SL-
ISBBF5  EQU     0BBF5H  	; ----I
D_BBF6  EQU     0BBF6H  	; --SL-
D_BBF8  EQU     0BBF8H  	; --SLI
I_BBFA  EQU     0BBFAH  	; ----I
D_BBFB  EQU     0BBFBH  	; ---LI
DSBBFD  EQU     0BBFDH  	; --S--
D_BBFE  EQU     0BBFEH  	; --SLI
ISBBFF  EQU     0BBFFH
I_BC00	EQU	0BC00H	        ; ----I		ramdisk boot sector
D_BE00  EQU     0BE00H          ; --SLI		number of ramdisk segments
I_BE02  EQU     0BE02H          ; ----I		ramdisk segment table

; ------------------------------------------------------------------------------
; P3: system variables, hooks, buffers and paging helper routines

YC000   EQU     0C000H          ; bootsector transferaddress
CC206   EQU     0C206H          ; debugger breakpoint handler

DATABA	EQU	0F1C9H		; start of disk system static work area
SPRTBUF	EQU	0F1C9H		; BDOS _STROUT
JUMPB   EQU     0F1D3H          ; start of handlers jump table
RD_LDI	EQU	0F1D3H		; transfer to/from page 1
P0_LDIR EQU     0F1D6H          ; transfer with page 0
P0_CALL EQU     0F1D9H          ; call main-bios
SFLUSH  EQU     0F1DCH          ; print string via chput
GO_DRV  EQU     0F1DFH          ; interslot call with prompt handler
JP_VEC	EQU	0F1E2H		; start DOS1 style handler
SIRQ    EQU     0F1E5H		; KEYINT handler
SRDSLT  EQU     0F1E8H		; RDSLT handler
SWRSLT  EQU     0F1EBH		; WRSLT handler
SCALSLT EQU     0F1EEH		; CALSLT handler
SENASLT EQU     0F1F1H		; ENASLT handler
SCALLF  EQU     0F1F4H		; CALLF handler
PUT_BD  EQU     0F1F7H          ; enable BDOS segments
PUT_US  EQU     0F1FAH          ; enable DOS segments
P0_RAM	EQU	0F1FDH		; enable DOS RAM on page 0
MAP_VE  EQU     0F200H
RD_SEG	EQU	0F206H		; RD_SEG handler
WR_SEG  EQU     0F209H          ; WR_SEG handler
PUT_P0	EQU	0F218H		; PUT_P0 handler
GET_P0	EQU	0F21BH		; GET_P0 handler
PUT_P1	EQU	0F21EH		; PUT_P1 handler
GET_P1	EQU	0F221H		; GET_P1 handler
PUT_P2	EQU	0F224H		; PUT_P2 handler
GET_P2	EQU	0F227H		; GET_P2 handler
CARPOS  EQU     0F237H          ; BDOS console output columnpos
STARTP  EQU     0F238H          ; console columnpos at start of lineinput
INSERT  EQU     0F239H          ; lineinput insert flag
CRLFPO  EQU     0F23AH          ; lineinput secret message flag
PFLAG   EQU     0F23BH          ; console output to printer flag
CUR_DRV	EQU	0F23CH		; default drive
DIRTYD  EQU     0F23CH          ; directory buffer dirty flag (DOS1)
DTA_AD  EQU     0F23DH          ; default transfer address
DMAADD  EQU     0F23DH          ; transferaddress (DOS1)
BUFSEC  EQU     0F23FH          ; sectornumber in data buffer
BUFDRN  EQU     0F241H          ; driveid of sector in data buffer
DIRTYB  EQU     0F242H          ; data buffer dirty flag
BUFDRI  EQU     0F243H          ; DPB pointer current operation
DIRBFI  EQU     0F245H          ; sectornumber (offset) in directory buffer
DIRBFD  EQU     0F246H          ; driveid of sector in directory buffer (DOS1)
CURDRV  EQU	0F247H		; default driveid (DOS1)
DAY     EQU     0F248H          ; current day (1..31) (DOS1)
MONTH   EQU     0F249H          ; current month (1..12) (DOS1)
YEAR    EQU     0F24AH          ; current year (offset to 1980) (DOS1)
DAYCNT  EQU     0F24CH          ; current days since 1-1-1980 (DOS1)
WEEKDA  EQU     0F24EH          ; current day of week (0=sunday) (DOS1)
HOOKBE  EQU     0F24FH		; -C--I
H_PROM  EQU     0F24FH          ; prompt for disk hook
H_BDOS  EQU     0F252H
H_UNPA  EQU     0F252H          ; get fat entry content hook (DOS1)
H_UP    EQU     0F255H
H_DEVN  EQU     0F255H          ; check if devicename hook (DOS1)
H_16CH  EQU     0F258H
H_CONT  EQU     0F258H          ; try next direntry hook (DOS1)
H_CHIN  EQU     0F25BH
H_GETE  EQU     0F25BH          ; get next direntry hook (DOS1)
H_CHOU  EQU     0F25EH
H_NEXT  EQU     0F25EH          ; next direntry hook (DOS1)
H_CHST  EQU     0F261H
H_MOVN  EQU     0F261H          ; validate FCB drive and filename hook (DOS1)
H_CHFL  EQU     0F264H
H_DOOP  EQU     0F264H          ; fcb open hook (DOS1)
H_LSTO  EQU     0F267H
H_FATR  EQU     0F267H          ; get latest FAT hook (DOS1)
H_LSTS  EQU     0F26AH
H_GETI  EQU     0F26AH          ; get pointer to DPB of current drive hook (DOS1)
H_FATW  EQU     0F26DH          ; write FAT hook
H_DREA  EQU     0F270H          ; read sector hook
H_FATA  EQU     0F273H          ; diskerror hook
H_DIRW  EQU     0F276H          ; write dirsector hook
H_DWRI  EQU     0F279H          ; write sector hook
H_MUL1  EQU     0F27CH          ; multiply hook
H_DIV3  EQU     0F27FH          ; divide hook
H_FNDC  EQU     0F282H          ; get absolute cluster hook
H_SKPC  EQU     0F285H          ; get next absolute cluster hook
H_BUFR  EQU     0F288H          ; partical sector read hook
H_BUFW  EQU     0F28BH          ; partical sector write hook
H_ENUF  EQU     0F28EH          ; start read recordoperation from disk hook
H_SETF  EQU     0F291H          ; finish read recordoperation from disk hook
H_SETC  EQU	0F294H          ; end read recordoperation from disk hook
H_WRTE  EQU     0F297H          ; record operation error at start hook
H_NORN  EQU     0F29AH          ; start write recordoperation to disk hook
H_WRTL  EQU     0F29DH          ; finish write recordoperation to disk hook
H_OPTI  EQU     0F2A0H          ; calculate sEQUencial sectors hook
H_FIGR  EQU     0F2A3H          ; get sectornumber of cluster hook
H_ALLO  EQU     0F2A6H          ; allocate FAT chain hook
H_RELB  EQU     0F2A9H          ; release FAT chain hook
H_GETC  EQU     0F2ACH          ; lineinput headloop hook
H_OUT   EQU     0F2AFH          ; console output hook
H_DATE  EQU     0F2B2H          ; get time and date for direntry hook
H_SETY  EQU     0F2B5H          ; setup days in februari hook
TIM_RA  EQU     0F2B8H  	; ---L-  number of VBLANK interrupts per 1/10 second
LASTEN  EQU     0F2B8H          ; current direntry number (DOS1)
TIM_CO  EQU     0F2B9H  	; ----I
NAME1   EQU     0F2B9H          ; filename1 (DOS1)
RANDOM  EQU     0F2BAH  	; --SL-  'random' number
ST_COU  EQU     0F2BDH  	; --S--
CH_COU  EQU     0F2BEH  	; --SL-
TIM_TI  EQU     0F2BFH  	; --SL-
TIMI_S  EQU     0F2C0H  	; J---I
ATTRIB  EQU     0F2C4H          ; original DR byte FCB
MAP_TAB EQU     0F2C5H          ; memory mapper table
NAME2   EQU     0F2C5H          ; filename2 (rename) (DOS1)
P0_SEG  EQU     0F2C7H          ; current segment page 0
P1_SEG  EQU     0F2C8H          ; current segment page 1
P2_SEG  EQU     0F2C9H          ; current segment page 2
P3_SEG  EQU     0F2CAH          ; current segment page 3
P0_TPA  EQU     0F2CBH          ; segment page 0 at BDOS entry
P1_TPA  EQU     0F2CCH          ; segment page 1 at BDOS entry
P2_TPA  EQU     0F2CDH          ; segment page 2 at BDOS entry
DATA_S  EQU     0F2CFH		; BDOS data segment
CODE_S  EQU     0F2D0H		; BDOS code segment
NAME3   EQU     0F2D0H          ; temporary save for NAME1 and ATTRIB (rename) (DOS1)
SP_IRQ  EQU     0F2D1H          ; temporary save stackpointer (KEYINT)
IRQ_ST  EQU     0F2D3H          ; pointer to temporary stack (KEYINT)
FCALSA  EQU     0F2D5H  	; J---I
SERR_M  EQU     0F2DAH  	; --S--
SPROMPT EQU     0F2DCH          ; --S--  pointer to prompt hander
CREATI  EQU     0F2DCH          ; flag ignore fileattributes (DOS1)
SECCLU  EQU     0F2DDH          ; current relative sector in cluster
SBDOS   EQU     0F2DEH  	; --S--
DSKERR  EQU     0F2DEH          ; result of recordoperation (DOS1)
TRANS   EQU     0F2DFH          ; flag increase current relative sector in cluster (0 means not)
SS_TEMP EQU     0F2E0H
PREREA  EQU     0F2E0H          ; flag flake read (0 means real read) (DOS1)
RD_ADDR EQU     0F2E1H  	; --SL-
THISDR  EQU     0F2E1H          ; current driveid (DOS1)
NEXTAD  EQU     0F2E2H          ; transferaddress for recordoperation
RD_SNU  EQU     0F2E3H  	; --SL-
RECPOS  EQU     0F2E4H          ; start record (32 bit) for recordoperation
RD_SCN  EQU     0F2E5H  	; --SL-
IX_BDOS EQU     0F2E6H          ; ---L-  temporary save IX register BDOS call
SP_BDOS EQU     0F2E8H  	; --SL-
RECCNT  EQU     0F2E8H          ; number of records for recordoperation (DOS1)
PS_BDOS EQU     0F2EAH		; same as F2EB, but for primary slots
LASTPO  EQU     0F2EAH          ; current relative cluster of file (DOS1)
SS_BDOS EQU     0F2EBH		; Secundary slot status when BDOS function handler was executed
DSK_CHK EQU     0F2ECH
CLUSNU  EQU     0F2ECH          ; current cluster of file (DOS1)
SECPOS  EQU     0F2EEH          ; start relative sector for recordoperation
VALSEC  EQU     0F2F0H          ; relative cluster after fileend for write recordoperation
BLDCHK  EQU     0F2F1H  	; --S--
BYTSEC  EQU     0F2F2H          ; start offset in sector for recordoperation
BSVCHK  EQU     0F2F3H  	; --S--
BYTPOS  EQU     0F2F4H          ; start fileposition (32 bit) for recordoperation
BUF_1   EQU     0F2F5H  	; --SL-
BUF_2   EQU     0F2F7H  	; --SL-
BYTCT1  EQU     0F2F8H          ; partical sector transfer at start
BUF_3   EQU     0F2F9H  	; --SL-
BYTCT2  EQU     0F2FAH          ; partical sector transfer at end
ERR_BUF EQU     0F2FBH  	; --S--
SECCNT  EQU     0F2FCH          ; number of complete sectors to transfer
BOOT_D  EQU     0F2FDH  	; --SL-
ST_BDOS EQU     0F2FEH          ; --SL- pointer to temporary stack (BDOS)
ENTFRE  EQU     0F2FEH          ; first free direntry (0FFH if none found) (DOS1)
READOP  EQU     0F2FFH          ; flag diskoperation (0 if read, 1 if write)
KDSK_V  EQU     0F300H  	; --S--
CONTPO  EQU     0F300H          ; pointer to remaining lineinput from CON read record operation (DOS1)
KAB_VE  EQU     0F302H  	; --S--
MAXCLS  EQU     0F302H          ; temporary store for maximium cluster (DOS1)
CPMCAL  EQU     0F306H  	; --S--
SRCHFC  EQU     0F307H          ; saved pointer to FCB search first, used for search next
SRCHIX  EQU     0F309H          ; saved pointer to DPB search first, used for search next
SRCHLO  EQU     0F30BH          ; saved current direntry number search first/next (FFh means invalid)
FCBEXT  EQU     0F30CH          ; original EX byte FCB
RAWFLG  EQU     0F30DH		; read after write (verify) flag
COUNTR  EQU     0F30EH  	; --S--
KANJTA  EQU     0F30FH  	; ----I  double byte header table
DOSVER 	EQU     0F313H  	; --SL-
FUTURE  EQU     0F313H          ; (DOS1)
P0_64K  EQU     0F314H  	; ---L-  TPA segment table
P1_64K  EQU     0F315H  	; ---L-
P2_64K  EQU     0F316H  	; ---L-
P3_64K  EQU     0F317H
DISKVE  EQU     0F323H  	; --SLI
BREAKV  EQU     0F325H  	; --SL-
AUXBOD  EQU     0F327H  	; ----I
BDOSBO  EQU     0F331H          ; BDOS hook (MSXHOOK style)
SDOS1   EQU     0F333H  	; --S--
KEYVLD  EQU     0F336H          ; flag saved input available (0 = none available)
KEYCHR  EQU     0F337H          ; save input
TIMFLG  EQU     0F338H  	; --S--
F_SPSA  EQU     0F339H          ; saved stackpointer format routine
PATHNAM EQU     0F33BH  	; --SL-
CURDAT  EQU	0F33BH          ; days since 1-1-1980, used when no clockchip (DOS1)
RECSIZE EQU     0F33DH  	; ---L-
RECSIZ  EQU     0F33DH          ; recordsize GET/PUT recordoperations (DOS1)
TARGET  EQU     0F33FH  	; --SL-
NOTFIR	EQU	0F340H		; first start DiskBASIC flag
RAMAD0  EQU     0F341H
RAMAD1  EQU     0F342H
RAMAD2	EQU	0F343H		; slotid DOS ram page 2
RAMAD3	EQU	0F344H		; slotid DOS ram page 3
FILMAX  EQU     0F345H          ; maximum number of diskbasic FCB's
DOSFLG	EQU	0F346H		; MSXDOS started flag
SNUMDR	EQU	0F347H		; number of drives
MASTER	EQU	0F348H		; slotid disksystem ROM
HIMSAV  EQU     0F349H  	; --SL-
DOSHIM  EQU     0F34BH  	; --SL-
SSECBUF EQU	0F34DH
SMAXSEC EQU     0F34FH  	; --S--
SBUFFE  EQU     0F34FH          ; pointer to datasectorbuffer (DOS1)
SDIRBUF EQU     0F351H  	; --S--
SDIRBU  EQU     0F351H		; pointer to directorysectorbuffer (DOS1)
RM_DPB  EQU     0F353H  	; --S-I
FCBBAS  EQU     0F353H          ; pointer to the diskbasic FCB's (DOS1)
SDPBLI  EQU     0F355H  	; ----I
RSLREG  EQU     0F365H  	; ----I
SDOSON	EQU	0F368H		; enable disksystem ROM on page 1
SDOSOF	EQU	0F36BH		; enable TPA RAM on page 2
XFER	EQU	0F36EH		; transfer to/from TPA RAM on page 2
SAUXIN  EQU     0F371H
SAUXOUT EQU     0F374H		; 
SAUXOU  EQU     0F374H          ; auxiliary output routine (DOS1)
GO_BIOS EQU     0F377H  	; J----
BLDCHK1 EQU     0F377H          ; routine diskbasic BLOAD (DOS1)
GO_BDOS EQU     0F37AH  	; J----
BSVCHK1 EQU     0F37AH          ; routine diskbasic BSAVE (DOS1)
BDOS  	EQU     0F37DH  	; -C---
VARWRK	EQU	0F380H		; Start of variable workarea
RDPRIM  EQU     0F380H
WRPRIM  EQU     0F385H
RETRTN	EQU	0F38BH		; on this adres is a simple RET instruction
CLPRIM  EQU     0F38CH
CLPRM1  EQU     0F398H
LINLEN	EQU	0F3B0H
CRTCNT  EQU     0F3B1H
CLIKSW	EQU	0F3DBH
CNSDFG	EQU	0F3DEH
REPCNT  EQU     0F3F7H          ; time interval until key-repeat
PUTPNT 	EQU	0F3F8H 		; Address in the keyboard buffer where a character will be written
GETPNT 	EQU	0F3FAH		; Address in the keyboard buffer where the next character is read
LPTPOS	EQU	0F415H
PRTFLG	EQU	0F416H
CURLIN	EQU	0F41CH
KBUF    EQU     0F41FH
YCONTP  EQU     KBUF+58         ; KBUF is reused for temporary buffer buffered input, size 128
YCONBF  EQU     KBUF+186        ; KBUF is reused for read CON device records, size 127+2 (DOS1)
BUF	EQU	0F55EH
TMPBUF	EQU	BUF+10		; temporary buffer (DOS1)
TTYPOS	EQU	0F661H
VALTYP	EQU	0F663H
MEMSIZ	EQU	0F672H
STKTOP	EQU	0F674H
TXTTAB	EQU	0F676H
TEMPPT	EQU	0F678H
TEMPST  EQU     0F67AH		; (DOS1)
DSCTMP	EQU	0F698H
FRETOP	EQU	0F69BH
AUTLIN	EQU	0F6ABH		; used for biggest sectorsize during disksystem init
MAXSEC	EQU	0F6ABH		; (DOS1)
SAVSTK	EQU	0F6B1H
VARTAB	EQU	0F6C2H
STREND	EQU	0F6C6H
DAC	EQU	0F7F6H
ARG	EQU	0F847H
MAXFIL	EQU	0F85FH
FILTAB	EQU	0F860H
NULBUF	EQU	0F862H
PTRFIL	EQU	0F864H
FILNAM	EQU	0F866H
NLONLY	EQU	0F87CH
SAVEND	EQU	0F87DH
HOKVLD	EQU	0FB20H		; EXTBIO valid flag (b0)
DRVTBL	EQU	0FB21H		; diskdriver table
HOOKSA	EQU	0FB29H		; diskdriver interrupt table
BASROM	EQU	0FBB1H
NEWKEY	EQU	0FBE5H
BOTTOM	EQU	0FC48H
HIMEM	EQU	0FC4AH
INTFLG  EQU     0FC9BH
JIFFY	EQU	0FC9EH
ESCCNT  EQU     0FCA7H
CAPST	EQU	0FCABH
KANAST	EQU	0FCACH
FLBMEM	EQU	0FCAEH
RUNBNF	EQU	0FCBEH
SAVENT	EQU	0FCBFH
EXPTBL	EQU	0FCC1H
SLTTBL	EQU	0FCC5H
SLTATR  EQU     0FCC9H
SLTWRK	EQU	0FD09H
PROCNM	EQU	0FD89H
DEVICE	EQU	0FD99H		; used temp for diskdriver count
DISKID	EQU	0FD99H
H_TIMI  EQU     0FD9FH
H_DSKO	EQU	0FDEFH
H_NAME	EQU	0FDF9H
H_KILL	EQU	0FDFEH
H_COPY	EQU	0FE08H
H_DSKF	EQU	0FE12H
H_DSKI	EQU	0FE17H
H_LSET	EQU	0FE21H
H_RSET	EQU	0FE26H
H_FIEL	EQU	0FE2BH
H_MKIS	EQU	0FE30H
H_MKSS	EQU	0FE35H
H_MKDS	EQU	0FE3AH
H_CVI	EQU	0FE3FH
H_CVS	EQU	0FE44H
H_CVD	EQU	0FE49H
H_GETP	EQU	0FE4EH
H_NOFO	EQU	0FE58H
H_NULO	EQU	0FE5DH
H_NTFL	EQU	0FE62H
H_BINS	EQU	0FE71H
H_BINL	EQU	0FE76H
H_FILE	EQU	0FE7BH
H_DGET	EQU	0FE80H
H_FILO	EQU	0FE85H
H_INDS	EQU	0FE8AH
H_LOC	EQU	0FE99H
H_LOF	EQU	0FE9EH
H_EOF	EQU	0FEA3H
H_BAKU	EQU	0FEADH
H_PARD	EQU	0FEB2H
H_NODE	EQU	0FEB7H
H_POSD	EQU	0FEBCH
H_RUNC	EQU	0FECBH
H_CLEA	EQU	0FED0H
H_LOPD	EQU	0FED5H
H_STKE	EQU	0FEDAH
H_ERRP	EQU	0FEFDH
H_PHYD	EQU	0FFA7H
H_FORM	EQU	0FFACH
EXTBIO  EQU     0FFCAH
DISINT	EQU	0FFCFH
ENAINT	EQU	0FFD4H
DFFFF   EQU     0FFFFH
YFFFF   EQU     0FFFFH		; (DOS1)

; ------------------------------------------------------------------------------
; Byte constants

_NCOMP	EQU	0FFH		; incompatible disk
_WRERR	EQU	0FEH		; write error
_DISK	EQU	0FDH		; disk error
_NRDY	EQU	0FCH		; not ready
_VERFY	EQU	0FBH		; verify error
_DATA	EQU	0FAH		; data error
_RNF	EQU	0F9H		; sector not found
_WPROT	EQU	0F8H		; write protected disk
_UFORM	EQU	0F7H		; unformatted disk
_NDOS	EQU	0F6H		; not a DOS disk
_WDISK	EQU	0F5H		; wrong disk
_WFILE	EQU	0F4H		; wrong disk for file
_SEEK	EQU	0F3H		; seek error
_IFAT	EQU	0F2H		; bad file allocation table
_NOUPB	EQU	0F1H		;
_IFORM	EQU	0F0H		; cannot format this drive

_INTER	EQU	0DFH		; internal error
_NORAM	EQU	0DEH		; not enough memory
_IBDOS	EQU	0DCH		; invalid BDOS call
_IDRV	EQU	0DBH		; invalid drive
_IFNM	EQU	0DAH		; invalid filename
_IPATH	EQU	0D9H		; invalid pathname
_PLONG	EQU	0D8H		; pathname too long
_NOFIL	EQU	0D7H		; file not found
_NODIR	EQU	0D6H		; directory not found
_DRFUL	EQU	0D5H		; boot directory full
_DKFUL	EQU	0D4H		; disk full
_DUPF	EQU	0D3H		; duplicate filename
_DIRE	EQU	0D2H		; invalid directory move
_FILRO	EQU	0D1H		; read only file
_DIRNE	EQU	0D0H		; directory not empty
_IATTR	EQU	0CFH		; invalid attributes
_DOT	EQU	0CEH		; invalid . or .. operation
_SYSX	EQU	0CDH		; system file exists
_DIRX	EQU	0CCH		; directory exists
_FILEX	EQU	0CBH		; file exists
_FOPEN	EQU	0CAH		; file is already in use
_OV64K	EQU	0C9H		; cannot transfer above 64K
_FILE	EQU	0C8H		; file allocation error
_EOF	EQU	0C7H		; end of file
_ACCV	EQU	0C6H		; file access violation
_IPROC	EQU	0C5H		; invalid process id
_NHAND	EQU	0C4H		; no spare file handles
_IHAND	EQU	0C3H		; invalid file handle
_NOPEN	EQU	0C2H		; file handle not open
_IDEV	EQU	0C1H		; invalid device operation
_IENV	EQU	0C0H		; invalid environment string
_ELONG	EQU	0BFH		; environment string too long
_IDATE	EQU	0BEH		; invalid date
_ITIME	EQU	0BDH		; invalid time
_RAMDX	EQU	0BCH		; ram disk already exists
_NRAMD	EQU	0BBH		; ram disk does not exist
_HDEAD	EQU	0BAH		; file handle has been deleted
_EOL	EQU	0B9H		; end of line
_ISBFN	EQU	0B8H		; invalid sub-function number

_STOP	EQU	09FH
_CTRLC	EQU	09EH
_ABORT	EQU	09DH
_OUTERR EQU	09CH
_INERR	EQU	09BH

_IPARM	EQU	08BH

; ------------------------------------------------------------------------------
; Structure information

; FCB structure

; +0	DR	drive			drive
; +1,8	F1-F8	file name		file name
; +9,3	T1-T3	filetype		filetype
; +12	EX	extent			extent (low byte)
; +13	S1	reserved		fileattribute
; +14	S2	reserved		extent high byte / record size low byte (block)
; +15	RC	record count in extent	record count in extent / record size high byte (block)
; +16,4	AL	allocation		Filesize
; +20,4	AL	allocation		volume-id
; +24	AL	allocation	
; +25	AL	allocation	
; +26,2	AL	allocation		start cluster
; +28,2	AL	allocation		current cluster
; +30,2	AL	allocation		current relative cluster
; +32	CR	record in extent	record in extent
; +33,3	R0-R2	random access record	random access record
; +36	R4	not used		random access record when record size <64


; FIB structure

; +0		fib indicator (0FFH)
; +1,13		file name as an ASCIIZ string
; +14		file attributes byte
; +15,2		time of last modification
; +17,2		date of last modification
; +19,2		start cluster
; +21,4		file size
; +25		logical drive
; +26,4		disk serial (if on disk)
; +28,2		pointer to device jump table (if device)
; +30		device flags
; +32,2		pointer to drive table
; +34,2		current directory sector
; +36		current directory entry in sector
; +37,2		cluster number of parent directory
; +39,2		start cluster of file
; +41,2		current cluster of file
; +43,2		current relative cluster of file
; +45,4		current file position
; +49		open mode
; +50


; drive table structure

; +0		slot id interface
; +1		offset (relative to 4000H) of jumptable of interface driver
; +2,2		pointer to Drive Parameter Block (DPB) of drive
; +4		flags
; +5		flags
; +6		drive id used within interface driver
; +7		
; +8		drivename
; +9		1/10 seconds+2 that disk cannot be changed (0 = init, 1 = error)
; +10		cluster mask
; +11		cluster shift
; +12,2		number of reserved sectors (bootarea)
; +14		number of FAT's
; +15		remainder of directory entries (no whole sector)
; +16		number of directorysectors (whole sectors)
; +17		number of sectors per FAT
; +18,2		first sector of rootdirectory
; +20,2		first sector of data area
; +22,2		number of clusters+1 on disk
; +24		dirty flag Bit 0 = 1, when on a DOS2 disk file(s) have been deleted
; +25,4		volume serial number (0FFFFFFFFH if none)
; +29		media descriptor byte of disk
; +30,2		starting cluster of current directory (bit 15 = 1 means root)
; +32,64	current directory (ASCIIZ) without "drive:\" prefix


; device entry

; +0,2		pointer to jump table
; +2
; +8		device flags
; +9,11		device name
; +10,33


; directory entry

; +0,8		file name (main)
; +8,3		file name (extension)
; +11		entry attribute
; +12,10	not used
; +22,2		time
; +24,2		date
; +26,2		starting cluster
; +28,4		file size
