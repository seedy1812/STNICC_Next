
skip_mem_bc_times_2:
	sla c
	rl b
skip_mem_bc
	push hl
	ld hl,(memory_ptr)
	add hl,bc
	jr nc,skip_over

	add hl,$e000	;; increment upto last 8k
	push bc
	ld bc,mem_page
	ld a,(bc)
	nextreg $56, a	; set the page $c000 - $dfff
	inc a
	nextreg $57, a	; set the page $e000 - $ffff ( next page )
	ld (bc),a
	pop bc
	scf					; carry set means we have overlapped
skip_over:
	ld (memory_ptr),hl
	pop hl
	ret





skip_64K:
	ld a, (mem_page)
mem_start_page: equ *+1
    ld h ,0
    sub h
	add 8 ; each page is 8k - so skip 64 means 8
	and %11111000 ; clear out the bottom 3 bits so multiple of 8
    add a,h ; add the start page back on
	ld de, $e000
	jr set_memory

mem_init
    ld (mem_start_page),a
	ret

mem_restart:
	ld a,(mem_start_page)
	ld (mem_page),a
	ld de, $e000
	
	call set_memory
	ret

; de trashed
; return byte in a
read_byte:
memory_ptr: equ *+1
	ld de, $e000
	ld a, (de)
	inc e
	jr nz , same_page
	inc d
	jr nz ,same_page
	ld d, $e0
	ld (answer),a
mem_page  equ *+1
	ld a, 0
	inc a
set_memory:
	ld (mem_page), a
	nextreg $57, a
answer: equ *+1
	ld a,0
same_page:
	ld (memory_ptr), de
	ret
