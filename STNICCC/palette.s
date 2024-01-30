palette_clear:
    push bc
    push hl
    nextreg $43,%00100000 ; sprites first palette
    nextreg $40,$f0         ; start at colour 0 and auto increment

    ld b, 255
._next1:
    nextreg $44,255
    nextreg $44,1
    djnz ._next1

    nextreg $43,%00010000 ; layer 2 first palette
    nextreg $40,$f0         ; start at colour 0 and auto increment
    ld b, 255
._next2:
    nextreg $44,0
    nextreg $44,0
    djnz ._next2

    nextreg $43,%01010000   ; layer 2 second palette
    nextreg $40,$f0         ; start at colour 0 and auto increment
    ld b, 255
._next3:
    nextreg $44,0
    nextreg $44,0
    djnz ._next3

    pop hl
    pop bc
    ret


show_palette:

    nextreg $43,%01010001 ; layer 2 second palette
    call .set_palette16
    nextreg $43,%00010001 ; layer 2 first palette
    call .set_palette16
    nextreg $43,%00100001 ; sprites first palette
.set_palette16
    nextreg $40,0         ; start at colour 0 and auto increment
    ld b, 16           ; 16 colours 
    ld hl,_palette
_next:
    ld a,(hl)
    inc hl
    nextreg $44,a
    ld a,(hl)
    inc hl
    nextreg $44,a
    djnz _next
    ret


 palette_add_sprites:
     ret

    ld a,192+8+32
    ld (pas_bcd_y),a

    ld a, 16
    call reset_sprite_index

    ld de, 0 ; = x 9:bit
    ld b,16

    ld hl, .hex_string
.mid_loop:
    push bc
    ld a,(hl)

    inc hl
    call .print
    add de,20
    pop bc
    djnz .mid_loop
    ret


.hex_string: db 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15

   ; de = posx 9bit
; a = index
.print
    add a,16
    ld b,a
    ld c,$57

    out (c),e  ; x:lo
pas_bcd_y: equ *+1
    ld a,9
    out (c),a ; y

    ld a,1      ; palette offset = 0
    and d       ; no mirror and no rotate
    out (c),a   ; bit 0 msb:x

    set 7,b     
    out (c),b   ; visible+ sprite_value

    ret



palette_create_sprites
    ld bc, $303b
    ld a,16
    out (c),a ; start at pattern 16
    ld bc,$005b
    ld a,16 ; colours 0 to 15
    ld e,0
.outer_loop:
    ld d,256
.inner_loop:
    out(c),e                ;; create 16 sprites of value 0 to 15
    dec d
    jr nz,.inner_loop
    inc e
    dec a
    jr nz, .outer_loop

    ret
