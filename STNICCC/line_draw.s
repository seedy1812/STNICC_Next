
 AltSet_MinMax:
    push hl

    inc hl
    ld d,(hl)
    ld e,d


    ld a,(_num_verts)
    dec a
    ld b,a
.chk_y
    inc hl
    inc hl

    ld a,(hl)

    cp d
    jr nc,.not_min
    ld d,a
.not_min

    cp e
    jr c,.not_max
    ld e,a
.not_max

    djnz .chk_y
	ld (min_max_y),de

	pop hl
	ret

    Draw_Line_Table_Ini
	push bc
	push de
	push hl

    ld b,d
    ld c,e

    ld d,h
    ld e,l
    
	call Draw_Line_Table

	pop hl
	pop de
	pop bc
	ret

;
Draw_Line_Table:	
			LD H, HI(Vector_Table_X1)	; Default to drawing in this table
			LD A,D				; Check whether we are going to be drawing up
			CP B
			JR NC, .l3
			INC H				; If we're drawing up, then draw in second table
			PUSH BC				; And use this neat trick to swaps BC and DE
			PUSH DE				; using the stack, forcing the line to be always
			POP BC				; drawn downwards
			POP DE

.l3:		LD L, B				; Y address -> index of table	
			LD A, C				; X address
			PUSH AF				; Stack the X address	
			LD A, D				; Calculate the line height in B
			SUB B
			LD B, A 
			LD A, E				; Calculate the line width
			SUB C 
			JR C, .ll4
; 
; This bit of code mods the main loop for drawing left to right
;
			LD C, A				; Store the line width
			LD A,$14			; Opcode for INC D
			JR  .ll5

;
; This bit of code mods the main loop for drawing right to left
;
.ll4:		NEG
			LD C,A
			LD A,$15			; Opcode for DEC D
;
; We've got the basic information at this point
;
.ll5:		LD (.Draw_Line_Table_Q1_M2), A	; Code for INC D or DEC D
			LD (.Draw_Line_Table_Q2_M2), A
			POP AF				; Pop the X address
			LD D, A				; And store in the D register
			LD A, B				; Check if B and C are 0
			OR C 
			JR NZ, .Draw_Line_Table_Q	; There is a line to draw, so skip to the next bit
			LD (HL), D 			; Otherwise just plot the point into the table
			RET
;			
; At this point
; HL = Table address
;  B = Line height
;  C = Line width
;  D = X Position
;
.Draw_Line_Table_Q:	
			LD A,B				; Work out which diagonal we are on
			CP C
			JR NC,.Draw_Line_Table_Q2
;
; This bit of code draws the line where B<C (more horizontal than vertical)
;
.Draw_Line_Table_Q1:	
			LD A,C
			LD (.Draw_Line_Table_Q1_M1+1), A	; Self-mod the code to store the line width
			LD C,B
			LD B,A
			LD E,B				; Calculate the error value
			SRL E
			LD (HL),D			; Store the X position
.l10:		LD A,E
			SUB C
			LD E,A
			JR NC,.Draw_Line_Table_Q1_M2
.Draw_Line_Table_Q1_M1:	
			ADD A, 0			; Add the line height (self modifying code)
			LD E,A
			LD (HL),D			; Store the X position
			INC L				; Go to next pixel position down
.Draw_Line_Table_Q1_M2:	
			INC D				; Increment or decrement the X coordinate (self-modding code)
			DJNZ .l10				; Loop until the line is drawn
			LD (HL),D
			RET
;
; This bit draws the line where B>=C (more vertical than horizontal, or diagonal)
;
.Draw_Line_Table_Q2:
			LD (.Draw_Line_Table_Q2_M1+1), A	; Self-mod the code to store the line width
			LD E,B				; Calculate the error value
			SRL E
.lq1:		LD (HL),D			; Store the X position
			LD A,E				; Get the error value
			SUB C				; Add the line length to it (X2-X1)
			JR NC,.lq2			; Skip the next bit if we don't get a carry
.Draw_Line_Table_Q2_M1: 	
			ADD A,0				; Add the line height (self modifying code)
.Draw_Line_Table_Q2_M2:	
			INC D				; Increment or decrement the X coordinate (self-modding code)
.lq2:		LD E,A				; Store the error value back in
			INC L				; And also move down
			DJNZ .lq1
			LD (HL),D
			RET

	align 256
Vector_Table_X1:		ds 256			; These tables needs to be on a page boundary and 
Vector_Table_X2:		ds 256	; next to each other


