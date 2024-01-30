REPEATED_BYTE macro
        LD (HL),C 
        INC L 
        endm


REPEATED_BYTE_5 macro
        REPEATED_BYTE
        REPEATED_BYTE
        REPEATED_BYTE
        REPEATED_BYTE
        REPEATED_BYTE
        endm

REPEATED_BYTE_30 macro
        REPEATED_BYTE_5
        REPEATED_BYTE_5
        REPEATED_BYTE_5
        REPEATED_BYTE_5
        REPEATED_BYTE_5
        REPEATED_BYTE_5
        endm


flip_screens:
if ULA_BACKBUFFER
	ld a,(cr_69)
        xor $40
        ld (cr_69),a
        nextreg $69,a   ; Display COntrol 1 : $40 - Shadwo Display 2

        and $40
        swapnib
        xor 14
        nextreg $52,a ; MMU slot 2   : Ram page 10 or 14
endif
        ret


ULACLS_SourceValue db 0

ULACLS_Start:
        db $83
        db  %01111101                           ; R0-Transfer mode, A -> B
        dw  ULACLS_SourceValue                 ; R0-Port A, Start address (source)
        dw  192*32                                           ; R0-Block length

        db  %00100100                    ; R1 - A fixed memory

        db  %00010000                    ; R2 - B incrementing memory

        db      %10101101                 ; R4-Continuous
        dw      $4000                     ; R4-Block Address

        db      $cf                                                     ; R6 - Load
        db      $87                                                     ; R6 - enable DMA;
ULACLS_End:


ULACOPY_Start:
        db $83
        db  %01111101                           ; R0-Transfer mode, A -> B
        dw  $4000                 ; R0-Port A, Start address (source)
        dw  192*32                                           ; R0-Block length

        db  %00010100                    ; R1 - A incrementing memory

        db  %00010000                    ; R2 - B incrementing memory

        db      %10101101                 ; R4-Continuous
        dw      $4000                     ; R4-Block Address

        db      $cf                                                     ; R6 - Load
        db      $87                                                     ; R6 - enable DMA;
ULACOPY_End:


CLS:
COPY:
        ; transfer the DMA "program" to the port
        ld      hl,ULACLS_Start
        ld      b,ULACLS_End - ULACLS_Start
        ld      c,DMA_PORT
        otir
        ret

xCOPY:   ; transfer the DMA "program" to the port
        ld      hl,ULACOPY_Start
        ld      b,ULACOPY_End - ULACOPY_Start
        ld      c,DMA_PORT
        otir
        ret





;in: DE - x0,y0; HL - x1,y1 
LineDraw:
if HALF_HEIGHT
	srl h
	srl d
endif
if HALF_WIDTH
	srl l
	srl e
endif

	ld	bc,$1c14	; $1c inc e; $14 - inc d
	ld	a,h
	sub	d
	jr	nc,.setdy
	neg
	inc	c	; $14->$15 = dec d
.setdy:	ld	h,a	; abs dy
	ld	a,l
	sub	e
	jr	nc,.setdx
	neg
	inc	b	; $1c->$1d = dec e
.setdx:	ld	l,a	
	cp	h	; cp dx and dy
	jr	nc,.noswap
	ld	l,b	; swap "changing of coords"
	ld	b,c
	ld	c,l
	ld	l,h	; swap dx and dy
	ld	h,a
.noswap:
	ld	(.next-1),bc	; auto modification of code; 

	srl	a	; a = max(dx , dy)/ 2	;
	ld	c,l	; loop by max abs(dx , dy)
	inc	c

.loop:
	ld b,a
        ld a,d
        cp 192
        jr nc,.no_draw
.draw:
        push hl
;my_break
        pixelad    ; de ->hl               

   	setae
        or (hl)
	ld (hl),a

        pop hl
.no_draw:
	ld a,b

	sub	h
	jr	nc,.next
	add	a,l
	inc	d


.next:	inc	e
	dec	c
	jr	nz,.loop
	ret


video_setup:
        nextreg $15,%00100111 ; no low rez , LSU , over border , sprite visible
        nextreg $1c,%00000100 ; reset layer 0 / layer 1 Clip Index
        nextreg $1a,0
        nextreg $1a,255
        nextreg $1a,0
        nextreg $1a,192

        ld a,14
        nextreg $52,a

        ld hl,$4000
        ld de,$4000+1
        ld bc, 192*32
        ld (hl),0
        ldir

        ld bc, 24*32-1
        ld (hl),15
        ldir

        ld a,10
        nextreg $52,a

        ld hl,$4000
        ld de,$4000+1
        ld bc, 192*32
        ld (hl),0
        ldir

        ld bc, 24*32-1
        ld (hl),15
        ldir

        call ULA_AltPalette

        ret

ULA_AltPalette:
        ld de,_alt_palette+1
        ld hl,_palette+2

        ld b, 15
.loop:
        ld a,16
        sub b

        ld (de),a
        inc de
        djnz .loop
        ret




AltFilledPoly
        ld a,(_colour)
if ULA_PALETTE
        ld hl,_alt_palette
        add hl,a
        ld a,(hl)
        swapnib
        srl a
endif
if ULA_PALETTE==0
        swapnib
        rr a
        and     $38
endif

        ld (_colour),a

	ld de, (min_max_y)

; get addr of ymin coords
	ld h, HI(Vector_Table_X1)
	ld l,d		

.next_line:
        ld a,191
        cp d
        ret c 

        ld c,(hl)		; start/min x
	inc h
	ld a,(hl)		; end/max x
        dec h
        inc hl

        cp c
        jp nc,.n_swap
        jp z,.n_swap

;        my_break

        ld      ixl,a
        ld      a,c
        ld      c,ixl
.n_swap
        push hl
        push de
        ld e,a
        call Line_Fill:		
        pop de
        pop hl

	inc d		;; move onto next scan line
	ld a, e		;; compare against max y
	cp d		; if no carry then we have lines left to draw
	jp nz,.next_line
	ret

;        d = y
;        c = start x
;        e = end x

Line_Fill:
        ld a,(_colour)          ; 8* colours give 8 byte texture
 
        ld hl,Vector_Texture_00 
        add hl,a
        ld a,7
        and d
        or l    
        ld l,a                ; add y to get texture for that line
        ld a,(hl)               ; texture
        ld iyl,a

        ld hl, Plot_Line_LHS
        ld a,7
        and c
        add hl,a
        ld a,(hl)
        ld ixh,a                ; x1 data

        ld hl, Plot_Line_RHS
        ld a,7
        and e
        add hl,a
        ld a,(hl)
        ld ixl,a                ; x2 data

        pixelad                 ;start byte
        ld a,l                  ;

        ld e, c                 ; end byte
        pixelad

        ld d,l
        ld e,a

        ld c, iyl

        ; h is high byte on screen

; Here:
; H = High byte of screen buffer address
; E = X2 (low byte of screen buffer address), IXL = data
; D = X1 (low byte of screen buffer address), IXH = data
; C = Texture
;
        LD A,E 				; Calculate line length in bytes
        SUB D 
        LD B,A	
        JR NZ, .diff_end_bytes	        ; If not zero, then skip to draw line


        LD L,D 				; Special case when both endpoints in same byte
        LD A,IXH 			; Get the LHS pixel data
        XOR IXL				; XOR with the RHS - this gives a mask
        LD D,A 				; Store the mask in D
        CPL				; Get the pixel data by inverting the mask
        AND C 				; AND it with the texture data
        LD E,A 				; Store the pixel data in E
        LD A,(HL)			; Get the screen data
        AND D 				; AND it with the mask
        OR E 				; OR it with the pixel data
        LD (HL),A			; Write back to the screen
        RET 

.diff_end_bytes:

        LD L,d				; Draw the LHS byte
        LD A,IXH 			; Get the pixel data
        AND C				; AND it with the texture data
        LD E,A 				; Store in D
        LD A,IXH 			; Get the pixel data again
        CPL 				; Invert the bits to turn it into a mask
        AND (HL)			; AND the mask with the screen data
        OR E 				; OR it with the pixel data
       LD (HL),A 			; Write back to the screen
        INC L 
;
; Draw the bulk of the line. This is an unrolled loop and works by skipping into the relevant
; bit of the unrolled routine
;
;        jr nc, .not_neg
;        ld a,b
;        neg
;        ld b,a
;.not_neg:
        LD A,31				; Calculate how far to skip 
        SUB B
        JR C,.no_middle				; If negative, then skip
        SLA A
        LD (.Draw_Horz_Line_Fast_M1+1),A	; Self-mod the JR instruction
.Draw_Horz_Line_Fast_M1:	JR .jmp				; Jump into relevant bit of DUP'd code
.jmp:
        REPEATED_BYTE_30
 .no_middle:
; 	        LD L,E 				; Finally do the RHS byte
        LD A,IXL			; Get the pixel data
        AND C				; AND it with the texture data
        LD E,A				; Store in E
        LD A,IXL			; Get the pixel data
        CPL 				; Invert the bits to turn it into a mask
        AND (HL)			; AND the mask with the screen data 
        OR E 				; OR it with the pixel data
        LD (HL),A			; Write back to the screen

;        POP HL 				; POP the screen address back off the stack
        RET

; End-points for the horizontal lines
        align 8
;
Plot_Line_LHS:		DB %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001
Plot_Line_RHS:		DB %10000000,%11000000,%11100000,%11110000,%11111000,%11111100,%11111110,%11111111

; Some sample textures
;
Vector_Texture_00:	
                        DB %00000000    ;1
                        DB %00000000
                        DB %00000000
                        DB %00000000
                        DB %00000000
                        DB %00000000
                        DB %00000000 
                        DB %00000000

                        DB %10001000    ;0
                        DB %00000000
                        DB %00000000
                        DB %00000000
                        DB %10001000
                        DB %00000000
                        DB %00000000 
                        DB %00000000

                        DB %10001000    ;2
                        DB %00000000
                        DB %00100010
                        DB %00000000
                        DB %10001000
                        DB %00000000
                        DB %00100010
                        DB %00000000

                        DB %10001000    ;3
                        DB %00000000
                        DB %10101010
                        DB %00000000
                        DB %10001000
                        DB %00000000
                        DB %10101010
                        DB %00000000


                        DB %10101010    ;4
                        DB %00000000
                        DB %10101010   
                        DB %00000000
                        DB %10101010   
                        DB %00000000
                        DB %10101010   
                        DB %00000000

                        DB %10101010    ;5
                        DB %01000100
                        DB %10101010   
                        DB %00000000
                        DB %10101010   
                        DB %00000000
                        DB %01000100
                        DB %00000000

                        DB %10101010    ;6
                        DB %01000100
                        DB %10101010   
                        DB %00010001
                        DB %10101010   
                        DB %00000000
                        DB %01000100
                        DB %00010001

                        DB %10101010    ;7
                        DB %01000100
                        DB %10101010   
                        DB %01010101
                        DB %10101010   
                        DB %00000000
                        DB %01000100
                        DB %01010101

                        DB %10101010    ;8
                        DB %01010101
                        DB %10101010   
                        DB %01010101
                        DB %10101010   
                        DB %01010101
                        DB %10101010
                        DB %01010101

                        DB %01010101    ;7
                        DB %10111011
                        DB %01010101   
                        DB %10101010
                        DB %01010101   
                        DB %11111111
                        DB %10111011
                        DB %10101010

                        DB %01010101    ;6
                        DB %10111011
                        DB %01010101   
                        DB %11101110
                        DB %01010101   
                        DB %11111111
                        DB %10111011
                        DB %11101110

                        DB %01010101    ;5
                        DB %10111011
                        DB %01010101   
                        DB %11111111
                        DB %01010101   
                        DB %11111111
                        DB %10111011
                        DB %11111111

                        DB %01010101    ;4
                        DB %11111111
                        DB %01010101   
                        DB %11111111
                        DB %01010101   
                        DB %11111111
                        DB %01010101   
                        DB %11111111

                        DB %01110111    ;3
                        DB %11111111
                        DB %01010101
                        DB %11111111
                        DB %01110111
                        DB %11111111
                        DB %01010101
                        DB %11111111

                        DB %01110111    ;2
                        DB %11111111
                        DB %11011101
                        DB %11111111
                        DB %01110111
                        DB %11111111
                        DB %11011101
                        DB %11111111

                        DB %01110111    ;1
                        DB %11111111
                        DB %11111111
                        DB %11111111
                        DB %01110111
                        DB %11111111
                        DB %11111111 
                        DB %11111111

                        DB %11111111    ;0
                        DB %11111111
                        DB %11111111
                        DB %11111111
                        DB %11111111
                        DB %11111111
                        DB %11111111 
                        DB %11111111




Vector_Texture_01:	DB %10101010,%01010101,%10101010,%01010101,%10101010,%01010101,%10101010,%01010101
Vector_Texture_02:	DB %10000001,%01000010,%00100100,%00011000,%00011000,%00100100,%01000010,%10000001
Vector_Texture_03:	DB %10001000,%01000100,%00100010,%00010001,%10001000,%01000100,%00100010,%00010001

_alt_palette:           ds 16
