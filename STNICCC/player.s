
MY_BREAK	macro
        db $dd,01
		endm



; http://arsantica-online.com/st-niccc-competition/

    DMA_PORT    equ $6b ;//: zxnDMA

    OPT Z80
    OPT ZXNEXTREG   

include "options.s"

if IS_SNA==1
	opt	sna=start:StackStart
endif

if IS_NEX==1
    seg     CODE_SEG, 4:$0000,$8000
    seg     STNICC_SEG, $1e:$0000,$0000 
    

    seg     CODE_SEG
endif

	include "irq.s"

    org $8200                    ; Start of application
StackEnd:
	ds	128
StackStart:
    ds  2

StartAddress:
start:
    ld sp , StackStart

    ld a,1
    call get_CR_a
    ld b,a
    ld a,14
    call get_CR_a
    ld c,a
;    my_break

    call bcd_load_sprites

    ld a,0
    out($fe),a

    ld a,bank(_1)                     ; first page we have loaded the data from
    ld b,bank(_2)                     ; first page we have loaded the data from
    call mem_init

if IS_NEX == 0
    call load_file
    jr nc , go
error1:   
    xor 7
    and 7
    out($fe),a
    jr error1    
endif
go: 
    ld a,0
    out($fe),a

    nextreg 7,%11 ;/ 28mhz
    ld a, 5
    call video_setup
    call frame_reset
    call mem_restart
    call init_vbl
if ULA_GRAPHICS=0
    call palette_add_sprites
endif
    call palette_clear

frame_loop:
    call frame_draw
    call frame_inc
    call wait_vbl
    call show_palette
    call flip_screens
    call read_byte
    rrc a            ; bit zero does cls
    push af
    jr nc,.no_clear
    call CLS
    jr .cont
.no_clear:
    call COPY
.cont:
    pop af

    rrc a             ; bit 1 for palette
    jr nc , no_palette
    push af
    call read_palette

    pop af
no_palette:
    rrc a               ; bit 2 for indexed ploys
    jr c,index_mode
;====================================================

    

non_index_mode:
each_non_index_poly:
    call read_byte
    ld e,a
    inc a
    jr z, frame_loop  ;$ff
    inc a
    jr nz, not_non_index_skip64
    call skip_64K
    jr frame_loop
not_non_index_skip64:
    inc a
    jr nz,not_non_index_restart
    call mem_restart
    call frame_reset
    jr frame_loop
not_non_index_restart:
    ld a,$f0
    and e
    swapnib
    ld(_colour),a

    ld  a,(mem_page)
    ld (_verts_page),a

    ld a, $f        ;; number of vertices in this
    and e
    
    ld (_num_verts),a

    ld hl,(memory_ptr)      ; // copy memptr 
    ld (_verts_pairs),hl

    ld c,a
    ld b,0
    call skip_mem_bc_times_2

    jr nc , .same

    ; and move the address we are point to down $2000 ( $e000 -> $c000 )
    ld hl,(_verts_pairs)
    add hl,65536-8*1024
    ld (_verts_pairs),hl

.same:

;; ld a,(_colour)
    ld hl,(_verts_pairs)

  call draw_poly
.no_draw:
    jr each_non_index_poly

;====================================================

index_mode:

    call read_byte

    ;going to copy the verts pairs locally - avoiding greater th 8 k difference if possible

    ld hl,(memory_ptr)      ; // copy memptr 
    ld (_verts_pairs),hl

    ld c,a
    ld b,0
    add bc,a

    ld (index_vert_pairs),a

    push bc
    
    ld  a,(mem_page)
    ld e,a
    
    call skip_mem_bc

    jr nc, .same_page

;    MY_BREAK
    ld hl,(_verts_pairs)
    add hl,65536-8*1024
.same_page:

    ld de,localverts_list
    ld (_verts_pairs),de

    pop bc
    ldir

each_inner_poly:
    call read_byte
    ld e,a
    inc a
    jr z, loop_exit  ;$ff
    inc a
    jr nz, not_index_skip64
    call skip_64K
loop_exit:
    jp  frame_loop
not_index_skip64:
    inc a
    jr nz ,not_index_restart
    call mem_restart
    call frame_reset
    jp frame_loop
not_index_restart:
    call DBG_Break

    ld a,$f0
    and e
    swapnib
    ld(_colour),a

    ld a, $f        ;; number of vertices in this
    and e
    
    ld (_num_verts),a

    ld b,0
    ld c,a

    ld de,(memory_ptr)      ; // copy memptr 
    push de
    exx
    pop de              ; move it into de'
    exx

    call skip_mem_bc

    jr nc ,.no_wrap

    exx
    ld a, $e0
    add a,d
    ld d,a
    exx

 .no_wrap:
    ld de,deindex_list ; going to write out the vert pairs here

._deindex_loop:
    exx
    ld a,(de)
    inc de 
    exx
    ld hl,localverts_list 
    add hl,a
    add hl,a
    ldi 
    ldi 
    inc c
    jr nz, ._deindex_loop


    ld hl,deindex_list
    call draw_poly

    jr each_inner_poly

flash: db 0

read_palette:
    call read_byte
    ex af,af'
    call read_byte
    ex af,af'
    ld hl,_palette
    call .palette_8bits
    ex af,af'
    ld hl,_palette+8*2
    call .palette_8bits
    ret


.palette_8bits:  
    or a
    ret z
    add a,a
    jr nc,.skip
    push af
    
    call read_byte
    and $77
    ld b,a
    call read_byte
    and $77
    ld c,a

    call convertRGB


    ld (hl),a          ; save to palette entry
    inc hl
    ld (hl),e
    inc hl
    pop af
    jr .palette_8bits
.skip:
    inc hl
    inc hl
    jr .palette_8bits


; input b,c (00000RRR,0GGG0BBB) 
; output a, e (RRRGGGBB,0000000B)
; uses b

convertRGB:
    ld a,b
	swapnib			;a 0RRR0000	8
	add a, a		;a RRR00000	4
	ld b, a			;b RRR00000	4	16
					;

	sub a			;a 00000000	4
	srl c			;d 00GGG0BB	8
	adc a, a		;a 0000000B	4	
	ld e, a			;e 0000000B	4	20
					;
	ld a, c			;a 00GGG0BB	4
	and 3			;a 000000BB	7
	add a, c		;a 00GGGBB0	4	15
					;
	sra a			;a 000GGGBB	8
	or b			;a RRRGGBBB 4	12	63 
	ret

min_max_y: ds 2

draw_poly:
;    my_break
    push ix
    push iy
    push de

; find top bottom y

if POLY_FILLED
    Call AltSet_MinMax
endif

    ld a,(_num_verts)
    ld iyl,a
    dec iyl

    push hl
    pop ix

    ld e,(ix+0)
    ld d,(ix+1)

    ld (setdest),de
    ld (setstart),de
.loop:
setstart: equ *+1
    ld hl,$1234

    inc ix
    inc ix

    ld e,(ix+0)
    ld d,(ix+1)
 
    ex de,hl
    ld (setstart),hl
if POLY_FILLED
    call Draw_Line_Table_ini 
else
   call LineDraw
endif

    dec iyl
    jr nz,.loop
    ld de,(setstart)
    ld hl,(setdest)

if POLY_FILLED
   call Draw_Line_Table_ini	
else
   call LineDraw
endif

if POLY_FILLED
    call AltFilledPoly
endif

    pop de
    pop iy
    pop ix

    ret


setdest: db 0,0

filename: db "scene1.bin",0


index_vert_pairs:   db 0
 
localverts_list: ds 256*2
deindex_list: ds 16*2

_colour: ds 2

_num_verts:  dw  1

_verts_page:  db 1
_verts_pairs: dw 0
_index_list:  dw 0

_palette: ds 16*2

if ULA_GRAPHICS == 1
include "ULA.s"
else
include  "Layer2.s"
endif

include "memory.s"
include "loading.s"
include "bcd_number.s"
include "palette.s"

include "line_draw.s"

;include "coso_jag.s"
;include "mad_max_z80.s"

if IS_NEX==1
   seg STNICC_SEG
 _1:
    incbin "scene1.bin"
_2:  ds 1

 	savenex "player.nex",start
endif

