	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Indicate processor 
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        processor 6502
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Include macro and TIA Reference libraries
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        include "vcs.h"
        include "macro.h"
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Variables
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        seg.U Variables
        org $80
Score			byte
HighScore		byte
FirstDigit		word
SecondDigit      	word
HighScoreSprite 	word
ScoreSprite             byte
Temp			byte
Random			byte
TopPipe			byte
LowerPipe		byte
PosYPlayer0		byte
Player0SpritePtr	word
Player0ColorPtr		word

FirstPF0		byte
FirstPF1		byte
FirstPF2		byte
SecondPF0		byte
SecondPF1		byte
SecondPF2		byte

PipeFlag		byte
CurrentPlayField	byte
counter			byte

pipeSpeed		byte
controlSpeed 		byte

isDrawingPlayer		byte

frameOffsetP0		byte

frameCounter 		byte

buttonWasPressed        byte



        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;  Constants
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PLAYER0_HEIGHT = 9
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;;  Macros
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Macro to LoadScore into Sprites
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        MAC LOAD_SCORE
        
        	lda #$00	
                sta COLUBK
                
                lda #$FF
                sta COLUPF
        
        	jsr LoadNumberPositions
        ENDM
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Macro to display Score
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        MAC DRAW_SCORE			;	CC     1ST     AFTER
                
                ldx #5				;2	2
ScoreBoard:
                ldy FirstDigit			;3	5	46
                lda Numbers,Y			;4	9	50
                and #%00001111			;2	11	52
                sta ScoreSprite			;3	14	55
                
                ldy SecondDigit			;3	17	58
                lda Numbers,Y			;4	21	62
                and #%11110000			;2	23	64
                ora ScoreSprite			;3	26	67
                sta ScoreSprite			;3	29	70

                sta WSYNC			;3      32	73
                sta PF1				;3      3	3
                
                ldy SecondDigit			;3      6	6
		jsr Waste12CC			;12	18	18
                jsr Waste12CC			;12	30	30
                
		inc FirstDigit			;5	35	35
                inc SecondDigit			;5	40	40
                
                lda #0				;2	42	42
                sta PF1				;3	45	45
           
     		sta WSYNC			;3	48	48
                
                lda ScoreSprite			;3	3	3
                sta PF1				;3	6	6
                
                jsr Waste12CC			;12	18	18
                jsr Waste12CC			;12	30	30
     		NOP				;2	32	32
                NOP				;2	34	34
                lda #0				;2	36	36
                sta PF1				;3	39	39
                
                
                dex				;2	41	2
                bne ScoreBoard			;2	43	4
                
                lda #0
                sta PF1
                
                REPEAT 5
                	sta WSYNC
                REPEND
                
               
                
        ENDM
        
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Macro to draw the world
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
        
        MAC DRAW_WORLD
        
        ;; Draw sky
        
        	lda #$AC		;
        	sta COLUBK		;
        	
                lda #$C8
                sta COLUPF
                
                
                
        	ldx #140		;
DrawSky:
						; IF BRANCH     ELSE 
	
        	lda FirstPF0			; 3?	6
                sta PF0				; 3	9
                lda FirstPF1			; 3?	12
                sta PF1				; 3	15
              					
                
		txa 				; 2	17
                sec				; 2	19
                sbc PosYPlayer0			; 3	22
                
                ldy FirstPF2			; 3?	25
                sty PF2				; 3	28
               	ldy SecondPF0			; 3?	31
                sty PF0				; 3	34
                ldy SecondPF1			; 3? 	37
		sty PF1
               
                
                cmp #9				; 3     40
                bcc DrawPlayer0			; 3	43
                jmp StopDrawingPlayer0		; 3	46
              	
DrawPlayer0:
		
                nop
		tay				; 2	47
		lda (Player0SpritePtr),Y	; 4	51
                sta GRP0			; 3	54

                
StopDrawingPlayer0:

		ldy SecondPF2			; 3?	57
                sty PF2				; 3	60
                
                cpx TopPipe			; 3	63         
                beq Waste20Lines		; 2	65

ContinueAfterWaste20Lines:
		
		dex				; 2	67
               	sta WSYNC			; 2	69
                
	
           	bne DrawSky			; 3     3
         
         ;; Draw PF ground
		lda #$C8	
                sta COLUBK	
                sta COLUPF		
        	lda #%11111111		
          	sta PF0			
        	sta PF1			
        	sta PF2			
        
        
        	REPEAT 2
                	sta WSYNC
        	REPEND
        
        	lda #0
        	sta PF0
       	 	sta PF1
       		sta PF2
        
        ;; Draw ground
        	
                
        	lda #$F5
                sta COLUBK
                
        	ldy #25
DrawGround:
                dey
                STA WSYNC

                bne DrawGround
                
                
                jmp DontWaste20Lines
Waste20Lines:
               	lda #0				
                sta PF2
                sta PF1
                sta PF0
                sta isDrawingPlayer
                txa
                sec
                sbc #40
                sta Temp
Waste20LinesLoop:
		dex
                sta WSYNC			; 3
		                

                txa 				; 2	17
                sec				; 2	19
                sbc PosYPlayer0			; 3	22
                
                ;cmp #0
                ;bpl .jmpDrawPlayer
                
                cmp #9				; 3     40
                bcc DrawPlayerTest		; 3	43
.jmpDrawPlayer:	
        	txa
                cmp Temp
                bne Waste20LinesLoop		; 3
                dex
                lda isDrawingPlayer
                cmp #1
                bne .jmpTemp
                dey				; 2	47
		lda Player0SpriteFrame1,Y	; 4	51
                cmp #%11111111
                beq .jmpTemp
                sta GRP0
.jmpTemp:
                jmp ContinueAfterWaste20Lines
DrawPlayerTest:
                tay				; 2	47
		lda Player0SpriteFrame1,Y	; 4	51
                sta GRP0
                txa
                cmp Temp
                bne .jmpTesting		; 3
                
                lda #1
                sta isDrawingPlayer
.jmpTesting
                jmp .jmpDrawPlayer
DontWaste20Lines:   
             
           
        ENDM
        
        
        
        
        
        
        
        seg Code
        org $F000
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Start Program
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
StartProgram:

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Clean memory macro at program start
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	CLEAN_START


        lda #0
        sta Score
 
	lda #%11010100
        sta Random
        
        
	lda #55
        sta PosYPlayer0
        
        lda #%00000000
        sta FirstPF0
        sta FirstPF1
        sta FirstPF2
        sta SecondPF0
        sta SecondPF1
        sta SecondPF2

	lda #1
        sta PipeFlag

	lda #$FF			
        sta COLUP0
        
        lda #2
        sta pipeSpeed
        
        lda #pipeSpeed
        sta controlSpeed

       	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Initialize Pointers
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        lda #<Player0SpriteFrame1
        sta Player0SpritePtr
        lda #>Player0SpriteFrame1
        sta Player0SpritePtr+1
        
        lda #<Player0Color
        sta Player0ColorPtr
        lda #>Player0Color
        sta Player0ColorPtr+1
        
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Beginning of frame
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

StartFrame:
	
        lda #2
        sta VSYNC
        sta VBLANK
        
	REPEAT 3
           STA WSYNC
        REPEND
        lda #0
        sta VSYNC
        
        LOAD_SCORE
        
        REPEAT 35
           STA WSYNC
        REPEND
        lda #0
        sta VBLANK 
        
        DRAW_SCORE
        
        DRAW_WORLD
 	
	lda #2
        sta VBLANK
	REPEAT 30
            STA WSYNC
        REPEND
        lda #0
        sta VBLANK
	
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Input Checks
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
checkPFColision:
	lda #%10000000
        bit CXP0FB
        beq EndColision
        lda #0
        sta Score
        
EndColision:
	sta CXCLR
	
        
CheckButton:
        lda #%10000000
        bit INPT4
	bne .checkIfButtonWasPressedLastFrame
        
        ;lda #controlSpeed			; ----------------------------------
        ;cmp #pipeSpeed
        ;bne EndInputCheck

        lda buttonWasPressed
        cmp #1
        beq EndInputCheck

        lda #1
        sta buttonWasPressed
        lda PosYPlayer0
        clc
        adc #20
        sta PosYPlayer0
        jmp EndInputCheck
.checkIfButtonWasPressedLastFrame:
        lda #0
        sta buttonWasPressed
        
        
EndInputCheck:
	sta WSYNC				; 3
        
        lda controlSpeed			
        cmp #0
        beq .jumpToIncrementScoreLogic
        dec controlSpeed
        jmp .NextFrame
.jumpToIncrementScoreLogic:
	lda frameCounter
        cmp #10
        bne .jumpToPipeMoveLogic
        sed
        lda Score
        clc
        adc #1
        sta Score
        cld
        
        
        
.jumpToPipeMoveLogic:
	inc frameCounter
       	lda PipeFlag				; 3
       	cmp #0					; 2
        bne .DrawNewPipe			; 2/3
        jmp .MovePipe				; 3
.DrawNewPipe:	
	lda #%11110000				; 2	
        sta SecondPF2				; 3
        lda #0					; 2
        sta PipeFlag				; 3
        sta frameCounter
        lda #5					; 2
        sta CurrentPlayField			; 3
        lda #4					; 2
        sta counter				; 3
        
        jsr GenerateRandomNumber			
        
        jmp .NextFrame				; 3
.MovePipe:
	lda pipeSpeed				; 2       load Speed to A
        sta controlSpeed			; 2	  save speed on control
        
        lda CurrentPlayField			; 2
        tay					; 2
        cmp #5					; 2
        beq .MovePF2				; 2/3
    	cmp #4					; 2
        beq .MovePF1				; 2/3
        cmp #3					; 2
        beq .MovePF0ToPF2			; 2/3
        cmp #2					; 2
        beq .MovePF2				; 2/3
        cmp #1					; 2
        beq .MovePF1				; 2/3
        cmp #0					; 2
        beq .MovePF0				; 2/3
        jmp .NextFrame				; 3

        
.MovePF2:
	lda FirstPF0,Y				; 3
        lsr					; 1	
        sta FirstPF0,Y				; 3
        cmp #%00001111				; 2
        bmi .MovePF2ToPF1			; 2/3
        jmp .NextFrame				; 3
.MovePF2ToPF1:
        dey					; 2
        lda FirstPF0,Y				; 3
        asl					; 1
        jmp .AddRightBitToPF			; 3
.MovePF1:
	
	lda FirstPF0,Y				; 3
        asl					; 1
        sta FirstPF0,Y				; 3
        lda counter				; 2
        cmp #0					; 2
        beq .MovePF1ToPF0			; 2/3
	dec counter				; 2
        jmp .NextFrame				; 3
.MovePF1ToPF0:
        dey					; 2	
        lda FirstPF0,Y				; 3
        lsr					; 1
        clc					; 2
	adc #%10000000				; 2
        sta FirstPF0,Y				; 3
        
       	cmp #%11110000				; 2
     	beq .DecrementCurrentPF			; 2/3
        jmp .NextFrame
	;jmp .AddLeftBitToPF
.MovePF0ToPF2:
	lda FirstPF0,Y				; 3
        lsr					; 1
        sta FirstPF0,Y				; 3
        dey					; 2
        lda FirstPF0,Y				; 3
        lsr					; 1
        clc					; 2					
	adc #%10000000				; 2
        sta FirstPF0,Y				; 3
        	
        cmp #%11110000				; 2
        beq .DecrementCurrentPF			; 2/3
        
        jmp .NextFrame				; 3
.MovePF0:
	lda FirstPF0,Y				; 3
        lsr					; 1
        sta FirstPF0,Y				; 3
        cmp #0					; 2
        beq .temp				; 2/3
        jmp .jmpTemp				; 3
.temp:
	lda #1					; 2
        sta PipeFlag				; 2
.jmpTemp:
	jmp .NextFrame				; 3
        
        
        
        
.AddRightBitToPF:
	clc					; 2
	adc #%00000001				; 2
        jmp .SavePlayField			; 3
.AddLeftBitToPF:
	clc					; 2
	adc #%10000000				; 2
.SavePlayField:
	sta FirstPF0,Y				; 3
        cmp #%00001111				; 2
     	bpl .DecrementCurrentPF			; 2/3
        jmp .NextFrame				; 3
.DecrementCurrentPF:
	lda #4					; 2
        sta counter				; 2
	dec CurrentPlayField			; 2
.NextFrame:
	dec PosYPlayer0
        
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Jump to next Frame
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	sta WSYNC				
	
	jmp StartFrame
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Subroutine to load Number sprite
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
LoadNumberPositions subroutine

        lda Score
        and #%00001111
        sta Temp
        asl
        asl
        adc Temp
        sta FirstDigit
        
        lda Score
        and #%11110000
        lsr
        lsr
        sta Temp
        lsr
        lsr
        adc Temp
        sta SecondDigit
        
        rts
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Subroutine to generate random number
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
GenerateRandomNumber subroutine
	lda Random
        asl
        eor Random
        asl
        eor Random
        asl
        asl
        eor Random
        asl
        rol Random
         
        lsr
        lsr

        ; sta Score

        clc
        adc #50
        sta TopPipe
	rts

        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Subroutine to waste 12 Clock cycles
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   
Waste10Liens subroutine
	rts

Waste12CC subroutine
	rts
        
        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Player0 Sprite Frames
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Player0SpriteFrame1:			; Frame 1
        .byte #%00000000;$0E
        .byte #%00110000;$0E
        .byte #%00111000;$FC
        .byte #%01111111;$FC
        .byte #%11111010;$FC
        .byte #%10110100;$FC
        .byte #%00110000;$0E
        .byte #%00110000;--
        .byte #%00100000;--
Player0SpriteFrame2:			; Frame 2
	.byte #%00000000;$0E
        .byte #%01000000;$0E
        .byte #%01100000;$0E
        .byte #%00110000;$FC
        .byte #%01111111;$FC
        .byte #%11111010;$FC
        .byte #%10110100;$FC
        .byte #%00110000;$0E
        .byte #%01100000;--
        .byte #%01000000;--

	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Player0 Colors
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Player0Color:
	.byte #$0E;$0E
        .byte #$0E;
        .byte #$0E;
        .byte #$FC;
        .byte #$FC;
        .byte #$FC;
        .byte #$FC;
        .byte #$0E;
        .byte #$0E;
        .byte #$0E;

        
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; Numbers
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        
Numbers: 
        .byte #%01110111
	.byte #%01010101
	.byte #%01010101
	.byte #%01010101
	.byte #%01110111

	.byte #%00100010
	.byte #%01100110
	.byte #%00100010
	.byte #%00100010
	.byte #%01110111

	.byte #%01110111
	.byte #%00010001
	.byte #%01110111
	.byte #%01000100
	.byte #%01110111

	.byte #%01110111
	.byte #%00010001
	.byte #%00110111
	.byte #%00010001
	.byte #%01110111

	.byte #%01010101
	.byte #%01010101
	.byte #%01110111
	.byte #%00010001
	.byte #%00010001

	.byte #%01110111
	.byte #%01000100
	.byte #%01110111
	.byte #%00010001
        .byte #%01110111

	.byte #%01110111
	.byte #%01000100
	.byte #%01110111
        .byte #%01010101
	.byte #%01110111

	.byte #%01110111
	.byte #%00010001
	.byte #%00100010
	.byte #%01000100
	.byte #%01000100

	.byte #%01110111
	.byte #%01010101
	.byte #%01110111
	.byte #%01010101
	.byte #%01110111

	.byte #%01110111
	.byte #%01010101
	.byte #%01110111
	.byte #%00010001
	.byte #%01110111


        
        org $FFFC
        .word StartProgram
        .word StartProgram
        