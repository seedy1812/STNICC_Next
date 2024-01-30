stib		macro  \0 \1 \2
			ld (\0+(\1)),\2
			endm

stiw		macro  \0 \1 \2 \3
			ld (\0+(\1+0)),\2
			ld (\0+(\1+1)),\3
			endm

ldib		macro \0 \1 \2
			ld \2,(\0+(\1))
			endm

ldiw		macro  \0 , \1 , \2,\3
			ld \2,(\0+(\1+0))
			ld \3,(\0+(\1+1))
			endm

push_all macro
        push af
        push bc
        push de
        push hl

        ex af,af'
        exx

        push af
        push bc
        push de
        push hl

        push ix
        push iy

        endm

pop_all macro

        pop ix
        pop iy


        pop hl
        pop de
        pop bc
        pop af

        ex af,af'
        exx

        pop hl
        pop bc
        pop de
        pop af

        endm
        
