
Fill_DMA	macro
	call DMAFILL
.exit:
	endm

Fill_Ends	macro
	ld a,b
	cp 72 & 63
	jr nz,.exit 
.exit:
	ld a,(_colour)
	ld (bc) , a
	ld c,iyl
	ld (bc) , a
	endm

Fill_Lines	macro
	push bc
	push de
	push hl


	ld a,(_colour)
	ld h,b
	ld d,h
	ld l,e

	ld (hl),a

	ld a, iyl	; length

	sub e
	ld b, 0
	ld c, a

	inc bc
	ld e,l
	inc l

	ldir
	pop hl
	pop de
	pop bc

	endm

Fill_Lines_noLdi	macro
	push af
	push hl
	ld hl,_colour
	ld b,(hl)

	ld hl, (DMACDest)
	ld l,c
	ld a,iyl

.loop:
	ld (hl) , b
	inc hl
	cp l
	jr nz,.loop
.endlp:
	pop hl
	pop af
	endm

flip_screens:
	ld hl, (first_layer_2_page)
	ld a ,h
	ld h,l
	ld l ,a
	ld	(first_layer_2_page),hl

	call set_front_back_buffers

	ret

dma_cls:
	; transfer the DMA "program" to the port
	push hl
	push bc
	ld      hl,CLSDMA_Start
	ld      b,CLSDMA_End - CLSDMA_Start
	ld      c,DMA_PORT
	otir
	pop bc
	pop hl
	ret

dma_copy:
	; transfer the DMA "program" to the port
	push hl
	push bc
	ld      hl,COPYDMA_Start
	ld      b,COPYDMA_End - COPYDMA_Start
	ld      c,DMA_PORT
	otir
	pop bc
	pop hl
	ret

COPY:
	ld a, %00000000     ; bank 0
	call set_screen_page
	ld c,0
	call set_background_screen
	call dma_copy

	ld a, %01000000     ; bank 1
	call set_screen_page
	ld c, 1
	call set_background_screen
	call dma_copy

	ld a, %10000000     ; bank 2
	call set_screen_page
	ld c, 2
	call set_background_screen
	call dma_copy
	call reset_background
	ret


CLS:	
	ld a,0
	ld (CLSDMA_SOURCEVALUE),a

	ld a, %00000000     ; bank 0
	call set_screen_page
	call dma_cls

;	ld a,(layer_2_flipped)
;	sub 3
;	ld (CLSDMA_SOURCEVALUE),a

	ld a, %01000000     ; bank 1
	call set_screen_page
	call dma_cls

;	ld a,(layer_2_flipped)
;	sub 2
;	ld (CLSDMA_SOURCEVALUE),a

	ld a, %10000000     ; bank 2
	call set_screen_page
	call dma_cls
	ret


set_screen_page_no_af_exg:
	exx 
    or %1011      ; shadow layer2 , is visible and layer 2 write paging
	out (c), a
	exx
    ret

set_screen_page_no_af:
	push bc
    or %1011      ; shadow layer2 , is visible and layer 2 write paging

    ld bc, $123b   
	out (c), a
	pop bc
    ret


set_screen_page:
	push af
	push bc
    or %1011      ; shadow layer2 , is visible and layer 2 write paging

    ld bc, $123b   
	out (c), a
	pop bc
	pop af
    ret

set_background_screen:
	ld a,(shadow_layer_2_page)
	add a,c
	nextreg $52,a
	inc a
	nextreg $53,a
	ret

save_background:
	ld d,$52
	call get_CR
	ld (page2),a

	ld d,$53
	call get_CR
	ld (page3),a

	ret


reset_background
	ld a,(page2)
	nextreg $52,a
	ld a,(page3)
	nextreg $53,a
	ret;


page2: db 0
page3: db 0
layer_2_flipped: db %00001000


CLSDMA_SourceValue:  db 0

CLSDMA_Start:
	db $83
	db  %01111101                           ; R0-Transfer mode, A -> B
	dw  CLSDMA_SourceValue                 ; R0-Port A, Start address (source)
	dw  $4000                                           ; R0-Block length

	db  %00100100                    ; R1 - A fixed memory

	db  %00010000                    ; R2 - B incrementing memory

	db      %10101101                 ; R4-Continuous
	dw      $0000                     ; R4-Block Address

	db      $cf                                                     ; R6 - Load
	db      $87                                                     ; R6 - enable DMA;
CLSDMA_End:

COPYDMA_Start:
	db $83
	db  %01111101                           ; R0-Transfer mode, A -> B
	dw  $4000                 				; R0-Port A, Start address (source)
	dw  $4000                               ; R0-Block length

	db  %00010100                    ; R1 - A fixed memory
;        db  %01010100                    ; R1 - A incrementing memory

	db  %00010000                    ; R2 - B incrementing memory

	db      %10101101                 ; R4-Continuous
	dw      $0000                     ; R4-Block Address

	db      $cf                                                     ; R6 - Load
	db      $87                                                     ; R6 - enable DMA;
COPYDMA_End:


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

	exx
    ld bc, $123b   
	exx
.loop:
    push de
	ex af,af'

	ld a,%11000000
	and d
	cp %11000000
	jr z,.no_draw

    call set_screen_page_no_af_exg

	ld a, %00111111
	and d
	ld d,a

	ld a, (_colour)
	ld (de) , a
.no_draw:

	ex af,af'
    pop de

	sub	h
	jr	nc,.next
	add	a,l
	inc	d
.next:	inc	e
	dec	c
	jr	nz,.loop
	ret



FilledPoly
	ld a,(_colour)
	ld (DMACCode_SourceValue),a

	ld a,1
	ld (DMACLength),a

	ld de, (min_max_y)

; get addr of ymin coords
	ld a,d		
	ld hl, min_max_scanlines
	add hl,a
	add hl,a
	// quick force to set the initial page
	ld a ,~63
	and d		; a = 0,64,128,192

	ld c,a
    call set_screen_page

	xor d		
	ld b,a		; d= 0 -> 63

;; not right place to jump to

.next_line:
	ld c,(hl)		; start/min x
	inc hl
	ld a,(hl)		; end/max x
	ld iyl,a
	inc hl
	sub c			; end-start
	
;	inc a			; ensure we dont have 0

	ld (DMACLength),a
	or a
	jr nz ,.fine
	inc a
.fine:
	ld a,63
	and d
	ld b,a
;;; problem here
	jr nz,.same_page
.new_page:
	ld a ,~63
	and d		; l = 0->63
	cp %11000000	; line a = 192
	jr z,.early_out
.do_stuff:
    call set_screen_page
.same_page:
	ld (DMACDest),bc
if 1
	push af
	push de
	ld a,(_colour)

	ld de, (DMACDest)
	ld e,c
	ld (de) , a

	ld e,iyl
	inc e
	ld (de) , a

	pop de
	pop af
else
	ld a,(DMACLength)
	or a
	call nz,DMAFILL
endif
	inc d		;; move onto next scan line
	ld a, e		;; compare against max y
	cp d		; if no carry then we have lines left to draw
	jr nz,.next_line

.early_out:
;	my_break
	ret

;------------------------------------------------------------------------------
; de = dest, a = fill value, bc = lenth
;------------------------------------------------------------------------------
DMAFill
	exx
    ld	hl,DMACCode
    ld	bc,256*(DMACCode_End-DMACCode) + DMA_PORT
    otir
	exx
    ret


DMACCode_SourceValue: dw 0
if 0

DMACCode: 	db $83
       	 	db %01111101	; Transfer , port a-> B , port a start ()l+h) , block length (l+h)
		 	dw DMACCode_SourceValue
DMACLength: dw 0
       		db %00100100,%00010000,%10101101
DMACDest: 	dw 0
        	db $cf,$87
DMACCode_End:

else

DMACCode:
        db $83
        db  %01111101                           ; R0-Transfer mode, A -> B
        dw  DMACCode_SourceValue                 ; R0-Port A, Start address (source)
DMACLength:
        dw  $0000                               ; R0-Block length

        db  %00100100                    ; R1 - A fixed memory
        db  %00010000                    ; R2 - B incrementing memory
        db  %10101101                 ; R4-Continuous
DMACDest:
        dw      $0000                     ; R4-Block Address

        db      $cf                                                     ; R6 - Load
        db      $87                                                     ; R6 - enable DMA;
DMACCode_End:
endif




first_layer_2_page: db 9
shadow_layer_2_page: db 12 



db "SEEDSEEDSEEDSEED"
min_max_scanlines: ds 2*200
db "SEEDSEEDSEEDSEED"


video_setup:
	call save_background

	ld d,$69
	call get_CR
	or $80
	ld (cr_69),a

	nextreg $69,a

	call set_front_back_buffers

    nextreg $15,%00000011 ; no low rez , LSU , sprites over border , sprite visible

    ret

set_front_back_buffers:
    ld a,(first_layer_2_page)
    nextreg $12 , a ; what was hidden is now visible
	ld a,(shadow_layer_2_page)
    nextreg $13 , a ; so this will now be hidden
	ret


 
; Draw a line into the vector table
; B = Y pixel position 1
; C = X pixel position 1
; D = Y pixel position 2
; E = X pixel position 2



AltFilledPoly
	ld a,(_colour)
	ld (DMACCode_SourceValue),a

	ld a,1
	ld (DMACLength),a

	ld de, (min_max_y)

; get addr of ymin coords
	ld h, HI(Vector_Table_X1)
	ld l,d		

	// quick force to set the initial page
	ld a ,~63
	and d		; a = 0,64,128,192
	cp ~63
	ret z

	ld c,a
    call set_screen_page

	xor d		
	ld b,a		; d= 0 -> 63

;; not right place to jump to

.next_line:
	ld c,(hl)		; start/min x
	inc h
	ld a,(hl)		; end/max x

	cp c
	jr nc,.no_swap

	ld iyl,c
	ld c,a
	ld a,iyl

.no_swap:

	dec h
	inc l
	ld iyl,a
	sub c			; end-start

	ld iyh,c
	ld c,a
	ld b ,0
	inc bc
	ld (DMACLength),bc
	ld c,iyh

	ld a,63
	and d
	ld b,a
;;; problem here
	jr nz,.same_page
.new_page:
	ld a ,~63
	and d		; l = 0->63
	cp %11000000	; line a = 192
	jr z,.early_out
.do_stuff:
    call set_screen_page
.same_page:
	ld (DMACDest),bc
;	Fill_Lines_noLdi
	Fill_DMA	
;	Fill_Ends
	inc d		;; move onto next scan line
	ld a, e		;; compare against max y
	cp d		; if no carry then we have lines left to draw
	jr nz,.next_line

.early_out:
	ret

			


