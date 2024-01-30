   OPT Z80
    OPT ZXNEXTREG  

voice_mode equ 1

   align 32

IM_2_Table:
        dw      linehandler      ; 0 - line interrupt
        dw      inthandler      ; 1 - uart0 rx
        dw      inthandler      ; 2 - uart1 rx
        dw      ctc0handler1     ; 3 - ctc 0
        dw      inthandler      ; 4 - ctc 1
        dw      inthandler      ; 5 - ctc 2
        dw      inthandler      ; 6 - ctc 3
        dw      inthandler      ; 7 - ctc 4
        dw      inthandler      ; 8 - ctc 5
        dw      inthandler      ; 9 - ctc 6
        dw      inthandler      ; 10 - ctc 7
        dw      vbl             ; 11 - ula
        dw      inthandler      ; 12 - uart0 tx
        dw      inthandler      ; 13 - uart1 tx
        dw      inthandler      ; 14
        dw      inthandler      ; 15

init_vbl:
    di

    nextreg $22,%000
    nextreg $23,192

    ld a,IM_2_Table>>8
    ld i,a

    nextreg $c0, 1+(IM_2_Table & %11100000) ;low byte IRQ table  | base vector = 0xa0, im2 hardware mode
   	
	nextreg $c4,1				; ULA interrupt
	nextreg $c5,0               ; enable CTC channel 0 interrupts, disable CTC channel 1-7 interrupts
	nextreg $c6,0

    nextreg $cc,%10000001   ;  ula will inetrrupt dma
    nextreg $cd,1            ; ct 0 will interrupt dma

if 0
	ld	bc,$183b		; set up channel 0			

    ld a, 1                     ; bit(0) 1 = control word                
    out (c), a                  ; disable CTC Channel 0

	ld	a,%11000101				; Bit7(1)= enable interrupts, Bit6(1)=timer mode, Bit2(1) = Timer constant next ,  Bit5(0)= prescaler of *16
	out	(c),a   

 	ld	a,175                 ; Time constant  value=(28,000,000/16)/freq  or  value=(28,000,000/256)/freq.
	out (c),a					; time constant = 256 (0).  Interval = 14*256/28MHz = 0.128 ms (7850Hz)
endif

    im 2


    ld a, 8             ; Peripheral 3 Settings
    call get_CR_a
    or %00001000        ;Enable DACs (A-B-C-D)
    nextreg 8,a

    ld a, $84
    call get_CR_a
    or %00000010
    nextreg $84,a           ; SoundDrive Mode 2

if voice_mode == 0
    ld hl, sample_space
    ld de, sample_space+2
    ld bc, sample_space_end-sample_space-2
    ld (hl),0
    inc hl
    ld (hl),255
    dec hl
    ldir
else
    ld hl, sample_space
    ld de, sample_space+8
    ld bc, sample_space_end-sample_space-8
    ld (hl),0
    inc hl
    ld (hl),0
    inc hl
    ld (hl),0
    inc hl
    ld (hl),0
    inc hl
    ld (hl),255
    inc hl
    ld (hl),255
    inc hl
    ld (hl),255
    inc hl
    ld (hl),255
    inc hl
    ld hl, sample_space
    ldir
endif

    ei
    ret

irq_counter: db 0
irq_last_count: db 0

wait_vbl:
    push af
    push hl
    ld hl,irq_counter
    ld a,(hl)
.loop:
    cp (hl)
 ;;   jr z,.loop
;    halt
;    ld hl,irq_counter
    ld a,(hl)
    ld (irq_last_count),a
    ld a,0
    ld (hl),a
    pop hl
    pop af
    ret

vbl:
    NextReg $c8,1
    ei
    reti

inthandler:
    push af  

    ld a, 1
    out ($fe), a 
    pop af 
    ei
    reti

linehandler:
    push af  

    push hl

    ld a, 1
    out ($fe), a 

    ld hl,irq_counter
    inc (hl)

    ld hl,(sample_base)
    ld l,0
    ld a,h 
    xor 4
    ld  h,a
    ld (sample_base),hl
    ld (samples),hl
    pop hl

    ld a, 3
    out ($fe), a 
    pop af 
    NextReg $c8,2
    ei
    reti

hmm: db 1
ctc0handler:
    push af  
    ld a, (hmm)
    neg
    and 7
    ld (hmm),a
    out ($fe), a 
    pop af 
    NextReg $c9,1
    ei
    reti
sample: db 0



sample_base: dw sample_space

samples: dw sample_space


ctc0handler1:
    push af  
    push hl
 
    NextReg $c9,1
 
 
    ld hl,(samples)
if  voice_mode == 0

    ld a, 5
    out ($fe), a 

    ld a,(hl)       ; 7 chanA
    out ($0f),a     ; 11 out  vs nextreg (),a = 17
    inc h           ;4

    ld a,(hl)       ;7
    out ($1f),a     ;11 chanB
    inc h          ;4

    ld a,(hl)       ;7
    out ($4f),a     ;11 chanC
    inc h          ;4

    ld a,(hl)       ;7
    out ($5f),a     ;11 chanD

    ld a, 0
    out ($fe), a 

    add hl,-256*3+1 ; 16

    ld a, 0
    out ($fe), a 

endif
if  voice_mode == 1
    ld a, 4
    out ($fe), a 

    ld a,(hl)       ; 7 chanA
    out ($0f),a     ; 11 out  vs nextreg (),a = 17
    inc l           ;4

    ld a,(hl)       ;7
    out ($1f),a     ;11 chanB
    inc l          ;4

    ld a,(hl)       ;7
    out ($4f),a     ;11 chanC
    inc l          ;4

    ld a,(hl)       ;7
    out ($5f),a     ;11 chanD
    inc hl          ;6

    ld a, 0
    out ($fe), a 


endif
if  voice_mode == 2
;   interleaved data
    ld a, 3
    out ($fe), a 

    push bc         ; 11

    ld bc,$f        ; 10
    outinb          ; 16

    ld c,$1f        ;7
    outinb          ;16

    ld c,$4f        ;7
    outinb          ;16

    ld c,$5f        ; 7
    outinb          ; 16
    pop bc          ; 10

    ld a, 0
    out ($fe), a 

 endif
    ld (samples),hl


    pop hl
    pop af 
    ei
    reti


get_CR_a:
	ld bc,$243b
	out (c),a
    inc b
	in a,(c)
	ret

 get_CR:
	ld bc,$243b
	out (c),d
    inc b
	in a,(c)
	ret

cr_69:	ds 1


org $b000
align 256

; 4 voices double buffered , max 256 bytes per voice per frame  
sample_space: ds 256*4 *2
sample_space_end:
    