; DRIVER-STUB.ASM
;
; Hard Disk ROM for MSX computer
; ------------------------------------------------------------------------------
; H.J. Berends:
; Stub file for Philips NMS8250 and SONY MSX-2 driver

; DEFINE NMS8250	; Uncomment to assemble for NMS8250 rom comparison

; symbols which are defined by the disk hardware driver

	PUBLIC	INIHRD
	PUBLIC	DRIVES
	PUBLIC	INIENV
	PUBLIC	DSKIO
	PUBLIC	DSKCHG
	PUBLIC	GETDPB
	PUBLIC	CHOICE
	PUBLIC	DSKFMT
	PUBLIC	MTOFF
	PUBLIC	OEMSTA
	PUBLIC	MYSIZE
	PUBLIC	SECLEN
	PUBLIC	DEFDPB

SECLEN	EQU	512

IFDEF NMS8250
MYSIZE	EQU	8
DEFDPB	EQU	074E7H	
DSKIO	EQU	0751EH
DRIVES	EQU	078A7H
INIHRD	EQU	07862H	
MTOFF	EQU	0786AH
INIENV	EQU	078F0H
DSKCHG	EQU	07925H	
GETDPB	EQU	0797CH	
CHOICE	EQU	079A6H	
DSKFMT	EQU	079CDH	
OEMSTA	EQU	07C8DH

ELSE
MYSIZE	EQU	9
DEFDPB	EQU	074E7H	
DSKIO	EQU	0751EH
DRIVES	EQU	07867H
INIHRD	EQU	07827H	
MTOFF	EQU	07DE2H
INIENV	EQU	078A7H
DSKCHG	EQU	078BAH	
GETDPB	EQU	07943H	
CHOICE	EQU	0795DH	
DSKFMT	EQU	079A0H	
OEMSTA	EQU	07DE0H
ENDIF