;JJ

	.inesprg 1   ; 1x 16KB PRG code
	.ineschr 1   ; 1x  8KB CHR data
	.inesmap 0   ; mapper 0 = NROM, no bank swapping
	.inesmir 1   ; background mirroring
  

;;;;;;;;;;;;;;;

;;;;;; VARIABLE DECLARATIONS ;;;;;;;;;;

	.rsset	$0000	;
DelayRegister		.rs	1	;
BackgroundCounter	.rs	2


	.bank 0
	.org $C000 
RESET:
	SEI          ; disable IRQs
	CLD          ; disable decimal mode
	LDX #$40
	STX $4017    ; disable APU frame IRQ
	LDX #$FF
	TXS          ; Set up stack
	INX          ; now X = 0
	STX $2000    ; disable NMI
	STX $2001    ; disable rendering
	STX $4010    ; disable DMC IRQs

vblankwait1:       ; First wait for vblank to make sure PPU is ready
	BIT $2002
	BPL vblankwait1

clrmem:
	LDA #$00
	STA $0000, x
	STA $0100, x
	STA $0200, x
	STA $0400, x
	STA $0500, x
	STA $0600, x
	STA $0700, x
	LDA #$FE
	STA $0300, x
	INX
	BNE clrmem
   
vblankwait2:      ; Second wait for vblank, PPU is ready after this
	BIT $2002
	BPL vblankwait2


LoadPalettes:
	LDA $2002             ; read PPU status to reset the high/low latch
	LDA #$3F
	STA $2006             ; write the high byte of $3F00 address
	LDA #$00
	STA $2006             ; write the low byte of $3F00 address
	LDX #$00              ; start out at 0
LoadPalettesLoop:
	LDA palette, x        ; load data from address (palette + the value in x)
						  ; 1st time through loop it will load palette+0
						  ; 2nd time through loop it will load palette+1
						  ; 3rd time through loop it will load palette+2
						  ; etc
	STA $2007             ; write to PPU
	INX                   ; X = X + 1
	CPX #$20              ; Compare X to hex $10, decimal 16
						  ; copying 16 bytes = 4 sprites
	BNE LoadPalettesLoop  ; Branch to LoadPalettesLoop if compare was Not Equal to zero
						; if compare was equal to 32, keep going down



LoadSprites:
	LDX #$00              ; start at 0
LoadSpritesLoop:
	LDA sprites, x        ; load data from address (sprites +  x)
	STA $0200, x          ; store into RAM address ($0200 + x)
	INX                   ; X = X + 1
	CPX #$60              ; Compare X to hex $60, decimal 96
	BNE LoadSpritesLoop   ; Branch to LoadSpritesLoop if compare was != zero
						  ; if compare == 32, keep going down

LoadBackground:
	LDA $2002			; Latch
	LDA #$22			; Upper address of load point
	STA $2006
	LDA #$20			; Lower address pf load point
	STA $2006
	LDX #$00
BackgroundLoop:
	LDA background, x
	STA $2007
	INX
	CPX #$FF			; Fix this loop?
	BNE BackgroundLoop
	
LoadAttribute:
	LDA $2002
	LDA #$23
	STA $2006
	LDA #$C0
	STA $2006
	LDX #$00
AttributeLoop:
	LDA attributes, x
	STA $2007
	INX
	CPX #$10
	BNE AttributeLoop
	
	
						
	LDA #%10010000   ; enable NMI, sprites from Pattern Table 1
	STA $2000

	LDA #%00011000   ; enable sprites
	STA $2001

	
Forever:
	JMP Forever     ;jump back to Forever, infinite loop
  
 

NMI:
	LDA #$00
	STA $2003       ; set the low byte (00) of the RAM address
	LDA #$02
	STA $4014       ; set the high byte (02) of the RAM address, start the transfer

	

LatchController:
	LDA #$01
	STA $4016
	LDA #$00
	STA $4016       ; tell both the controllers to latch buttons
	LDA #$01
	STA $4017
	LDA #$00
	STA $4017
	
;;; Christ, here we go...

;-------------------------------- PLAYER 1 CONTROLLER -----------------------


	
;--------------------------------  P1 Read A ---------------------------	
	LDA	$4016				;
	AND #%00000001  	; only look at bit 0
	BNE A1Fix
	JMP	ReadADone1   	; branch to ReadADone if button is NOT pressed (0)
						; add instructions here for when button IS pressed (1)
A1Fix:
	NOP

FloorCheck1:	
	LDA	$0200			;
	CMP #$80				;  YAYYYYY
	BEQ APressed1		;
	JMP ReadADone1
	RTS					;

APressed1:
	LDA #$00FF		
	STA DelayRegister
	
Jump1:
						; ;;;;;;;;;; HEAD LEFT ;;;;;;;;;;
	; ;LDA #%01000000  	; Flip Horizon
	; ;STA $0202		  	; Data Sprite 1
	LDA $0200       	; load sprite X position
	CLC			  		;
	SBC #$02		  	;
	STA $0200 	  		; Save Sprite 1 X position
						; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						; ;;;;;;;;;; HEAD RIGHT ;;;;;;;;;;
	; ;LDA #%01000000  	; Flip Horizon
	; ;STA $0206		  	; Data Sprite 1
	LDA $0204       	; load sprite X position
	CLC			  		;
	SBC #$02		  	;
	STA $0204 	  		; Save Sprite 1 X position
						; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						; ;;;;;;; SPINE ;;;;;;;;;;
	; ;LDA #%01000000  	; Flip Horizon
	; ;STA $020A			;
	LDA $0208		  	; ** Load sprite 2 X position
	CLC			  		;
	SBC #$02		  	;
	STA $0208		  	; Save Sprite 2 X position
						; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
						; ;;;;;;;; FRONT ;;;;;;;;;;;;;
	;LDA #%01000000  	; Flip Horizon
	;STA $020E		  	;  
	LDA $020C		  	; ** Load sprite 3 x position
	CLC             	; make sure the carry flag is clear
	SBC #$02        	; A = A + 1
	STA $020C		  	; ** Save sprite 3 x position
						; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						; ;;;;;;;;;; BACK LEG ;;;;;;;;;;
	;LDA #%01000000  	; Flip Horizon
	;STA $0212		  	;
	LDA $0210       	; ** Load Sprite 4 x position
	CLC			  		;
	SBC #$02		  	;
	STA $0210		  	;
						; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						; ;;;;;;;;; FRONT LEG ;;;;;;;;;;;;
	;LDA #%01000000		; Flip Horizontal
	;STA	$0216			;
	LDA $0214			; Load X position
	CLC					;
	SBC #$02			; Subtract to move left
	STA	$0214			; Store X
						; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

vblankjumping:
	BIT	$2002
	BPL	vblankjumping
						

	LDA $0200
	CMP #$32
	BNE Jump1
	BEQ FallDown


FallDown:
	LDA #$00FF		
	STA DelayRegister

Fall1:
						; ;;;;;;;;;; HEAD LEFT ;;;;;;;;;;
	LDA $0200       	; load sprite X position
	SEC			  		;
	ADC #$02		  	;
	STA $0200 	  		; Save Sprite 1 X position
						; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						; ;;;;;;;;;; HEAD RIGHT ;;;;;;;;;;
	LDA $0204       	; load sprite X position
	SEC			  		;
	ADC #$02		  	;
	STA $0204 	  		; Save Sprite 1 X position
						; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						; ;;;;;;; SPINE ;;;;;;;;;;
	LDA $0208		  	; ** Load sprite 2 X position
	SEC			  		;
	ADC #$02		  	;
	STA $0208		  	; Save Sprite 2 X position
						; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
						; ;;;;;;;; FRONT ;;;;;;;;;;;;;
	LDA $020C		  	; ** Load sprite 3 x position
	SEC             	; make sure the carry flag is clear
	ADC #$02        	; A = A + 1
	STA $020C		  	; ** Save sprite 3 x position
						; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						; ;;;;;;;;;; BACK LEG ;;;;;;;;;;
	LDA $0210       	; ** Load Sprite 4 x position
	SEC			  		;
	ADC #$02		  	;
	STA $0210		  	;
						; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						; ;;;;;;;;; FRONT LEG ;;;;;;;;;;;;
	LDA $0214			; Load X position
	SEC					;
	ADC #$02			; Subtract to move left
	STA	$0214			; Store X
						; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

vblankfalling:
	BIT	$2002
	BPL	vblankfalling
	
	LDA $0200
	CMP #$80
	BNE Fall1
	
	RTS					;
						
	
						
ReadADone1:        	; handling this button is done


TMPControllerRead1:
	LDA $4016		  ; P1 B 		- ATTACK
	LDA $4016		  ; P1 Select	- NA?
	LDA $4016		  ; P1 Start	- NA?
	LDA $4016		  ; P1 Up		- NA
	LDA $4016		  ; P1 Down		- Cloak?

;--------------------------------  P1 Read Left ---------------------------
	
ReadLeft1: 
	LDA $4016       	; P1 Left
	AND #%00000001  	; only look at bit 0
	BEQ ReadLeftDone1   	; branch to ReadADone if button is NOT pressed (0)
						; add instructions here for when button IS pressed (1)
				
						;;;;;;;;;; HEAD LEFT ;;;;;;;;;;
	;LDA #%01000000  	; Flip Horizontal
	;STA $0202		  	; Data Sprite 1
	LDA $0203       	; load sprite X position
	CLC			  		;
	SBC #$01		  	;
	STA $0203 	  		; Save Sprite 1 X position
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						;;;;;;;;;; HEAD RIGHT ;;;;;;;;;;
	;LDA #%01000000  	; Flip Horizontal
	;STA $0206		  	; Data Sprite 1
	LDA $0207       	; load sprite X position
	CLC			  		;
	SBC #$01		  	;
	STA $0207 	  		; Save Sprite 1 X position
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						;;;;;;; SPINE ;;;;;;;;;;
	;LDA #%01000000  	; Flip Horizon
	;STA $020A			;
	LDA $020B		  	; ** Load sprite 2 X position
	CLC			  		;
	SBC #$01		  	;
	STA $020B		  	; Save Sprite 2 X position
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
						;;;;;;;; FRONT ;;;;;;;;;;;;;
	;LDA #%01000000  	; Flip Horizon
	;STA $020E		  	;  
	LDA $020F		  	; ** Load sprite 3 x position
	CLC             	; make sure the carry flag is clear
	SBC #$01        	; A = A + 1
	STA $020F		  	; ** Save sprite 3 x position
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						;;;;;;;;;; BACK LEG ;;;;;;;;;;
	;LDA #%01000000  	; Flip Horizon
	;STA $0212		  	;
	LDA $0213       	; ** Load Sprite 4 x position
	CLC			  		;
	SBC #$01		  	;
	STA $0213		  	;
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						;;;;;;;;; FRONT LEG ;;;;;;;;;;;;
	;LDA #%01000000		; Flip Horizontal
	;STA	$0216			;
	LDA $0217			; Load X position
	CLC					;
	SBC #$01			; Subtract to move left
	STA	$0217			; Store X
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
						
ReadLeftDone1:        	; handling this button is done


;-------------------------------- P1 Read Right ------------------------- 


ReadRight1: 
	LDA $4016       	; player 1 - B
	AND #%00000001  	; only look at bit 0
	BEQ ReadRightDone1   ; branch to ReadBDone if button is NOT pressed (0)
						; add instructions here to do something when button IS pressed (1)
	
						;;;;;;;;;; HEAD LEFT ;;;;;;;;;;
	LDA $0203       	; load sprite X position
	SEC			  		;
	ADC #$01		  	; Move right
	STA $0203		  	; Save X
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
						;;;;;;; HEAD RIGHT ;;;;;;;;;;
	LDA $0207       	; ** load sprite 2 x position
	SEC			  		;
	ADC #$01		  	; Move right
	STA $0207		  	; Save X
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						;;;;;;;; SPINE ;;;;;;;;;;;;;;
	LDA $020B		  	; ** Load sprite 3 x position
	SEC             	; make sure carry flag is set
	ADC #$01        	; A = A - 1
	STA $020B		  	; ** save sprite 3 x position
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						;;;;;;;;; FRONT ;;;;;;;;;;;;;;;
	LDA $020F		  	;
	SEC			  		;
	ADC #$01		  	;
	STA $020F		  	;
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
	LDA $0213			;;;;;;;;;;; BACK LEG ;;;;;;;;;;
	SEC 				;
	ADC #$01			;
	STA $0213			;
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	LDA $0217			;;;;;;;;;;;; FRONT LEG ;;;;;;;;;;
	SEC 				;
	ADC #$01			;
	STA $0217			;
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						
						
						
						
ReadRightDone1:        	; handling this button is done

;-------------------------------- PLAYER 2 CONTROLLER -----------------------

;--------------------------------  P1 Read A ---------------------------	
	LDA	$4017				;
	AND #%00000001  	; only look at bit 0
	BNE A2Fix
	JMP	ReadADone2   	; branch to ReadADone if button is NOT pressed (0)
						; add instructions here for when button IS pressed (1)
A2Fix:
	NOP

FloorCheck2:	
	LDA	$0218			;
	CMP #$80				;  YAYYYYY
	BEQ APressed2		;
	JMP ReadADone2
	RTS					;

APressed2:
	LDA #$00FF		
	STA DelayRegister
	
Jump2:
						; ;;;;;;;;;; HEAD LEFT ;;;;;;;;;;
	LDA $0218       	; load sprite Y position
	CLC			  		;
	SBC #$02		  	;
	STA $0218 	  		; Save Sprite 1 Y position
						
						; ;;;;;;;;;; HEAD RIGHT ;;;;;;;;;;
	LDA $021C       	; 
	CLC			  		;
	SBC #$02		  	;
	STA $021C 	  		; 
						
						; ;;;;;;; SPINE ;;;;;;;;;;
	LDA $0220		  	;
	CLC			  		;
	SBC #$02		  	;
	STA $0220		  	; 
	
						; ;;;;;;;; FRONT ;;;;;;;;;;;;;
	LDA $0224		  	; 
	CLC             	; 
	SBC #$02        	; 
	STA $0224		  	; 
						
						; ;;;;;;;;;; BACK LEG ;;;;;;;;;;
	LDA $0228       	; 
	CLC			  		;
	SBC #$02		  	;
	STA $0228		  	;
						
						; ;;;;;;;;; FRONT LEG ;;;;;;;;;;;;
	LDA $022C			; 
	CLC					;
	SBC #$02			;
	STA	$022C			;


vblankjumping2:
	BIT	$2002
	BPL	vblankjumping2
						

	LDA $0218
	CMP #$32
	BNE Jump2
	BEQ FallDown2


FallDown2:
	LDA #$00FF		
	STA DelayRegister

Fall2:
						; ;;;;;;;;;; HEAD LEFT ;;;;;;;;;;
	LDA $0218       	; load sprite Y position
	SEC			  		;
	ADC #$02		  	;
	STA $0218 	  		; Save Sprite 1 Y position
						; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						; ;;;;;;;;;; HEAD RIGHT ;;;;;;;;;;
	LDA $021C       	; 
	SEC			  		;
	ADC #$02		  	;
	STA $021C 	  		; 
						
						; ;;;;;;; SPINE ;;;;;;;;;;
	LDA $0220		  	; 
	SEC			  		;
	ADC #$02		  	;
	STA $0220		  	; 
	
						; ;;;;;;;; FRONT ;;;;;;;;;;;;;
	LDA $0224		  	; 
	SEC             	; 
	ADC #$02        	; 
	STA $0224		  	; 

						; ;;;;;;;;;; BACK LEG ;;;;;;;;;;
	LDA $0228       	; 
	SEC			  		;
	ADC #$02		  	;
	STA $0228		  	;
						
						; ;;;;;;;;; FRONT LEG ;;;;;;;;;;;;
	LDA $022C			; 
	SEC					;
	ADC #$02			;
	STA	$022C			; 


vblankfalling2:
	BIT	$2002
	BPL	vblankfalling2
	
	LDA $0218
	CMP #$80
	BNE Fall2
	
	RTS					;
						
	
						
ReadADone2:        	  ; handling this button is done
	LDA $4017		  ; P1 B 		- ATTACK
	LDA $4017		  ; P1 Select	- NA?
	LDA $4017		  ; P1 Start	- NA?
	LDA $4017		  ; P1 Up		- NA
	
;---------------------------------- P2 Read Down ------------------------
	
	LDA $4017		  	; P1 Down		- Cloak
	; AND #%00000001  	; only look at bit 0
	; BEQ ReadDownDone2 ; branch to ReadBDone if button is NOT pressed (0)
						; add instructions here for when button IS pressed (1)
	
						; ;;;;;;;;;; HEAD LEFT ;;;;;;;;;;
	; LDA #$80       		; load sprite X position	  		
	; STA $0218		  	; Save X
						; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
						; ;;;;;;; HEAD RIGHT ;;;;;;;;;;
	; LDA #$80  	     	; ** load sprite 2 x position
	; STA $021C		  	; Save X
						; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						; ;;;;;;;; FRONT ;;;;;;;;;;;;;;
	; LDA #$88		  	; ** Load sprite 3 x position
	; STA $0220		  	; ** save sprite 3 x position
						; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						; ;;;;;;;;; SPINE ;;;;;;;;;;;;;;;
	; LDA #$88		  	;
	; STA $0227		  	;
						; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
	; LDA #$90			;;;;;;;;;;; FRONT LEG ;;;;;;;;;;
	; STA $0228			;
						; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	; LDA #$90			;;;;;;;;;;;; BACK LEG ;;;;;;;;;;
	; STA $022C			;
						; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						
						
						
						
; ReadDownDone2:        	; handling this button is done

	

;--------------------------------  P2 Read Left ---------------------------
	
ReadLeft2: 
	LDA $4017       	; P1 Left
	AND #%00000001  	; only look at bit 0
	BEQ ReadLeftDone2   	; branch to ReadADone if button is NOT pressed (0)
						; add instructions here to do something when button IS pressed (1)
				
						;;;;;;;;;; HEAD LEFT ;;;;;;;;;;
	;LDA #%01000000  	; Flip Horizon
	;STA $0202		  	; Data Sprite 1
	LDA $021B       	; load sprite X position
	CLC			  		;
	SBC #$01		  	;
	STA $021B 	  		; Save Sprite 1 X position
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						;;;;;;;;;; HEAD RIGHT ;;;;;;;;;;
	;LDA #%01000000  	; Flip Horizon
	;STA $0206		  	; Data Sprite 1
	LDA $021F       	; load sprite X position
	CLC			  		;
	SBC #$01		  	;
	STA $021F 	  		; Save Sprite 1 X position
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						;;;;;;; FRONT ;;;;;;;;;;
	;LDA #%01000000  	; Flip Horizon
	;STA $020A			;
	LDA $0223		  	; ** Load sprite 2 X position
	CLC			  		;
	SBC #$01		  	;
	STA $0223		  	; Save Sprite 2 X position
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
						;;;;;;;; SPINE ;;;;;;;;;;;;;
	;LDA #%01000000  	; Flip Horizon
	;STA $020E		  	;  
	LDA $0227		  	; ** Load sprite 3 x position
	CLC             	; make sure the carry flag is clear
	SBC #$01        	; A = A + 1
	STA $0227		  	; ** Save sprite 3 x position
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						;;;;;;;;;; FRONT LEG ;;;;;;;;;;
	;LDA #%01000000  	; Flip Horizon
	;STA $0212		  	;
	LDA $022B       	; ** Load Sprite 4 x position
	CLC			  		;
	SBC #$01		  	;
	STA $022B		  	;
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						;;;;;;;;; BACK LEG ;;;;;;;;;;;;
	;LDA #%01000000		; Flip Horizontal
	;STA	$0216			;
	LDA $022F			; Load X position
	CLC					;
	SBC #$01			; Subtract to move left
	STA	$022F			; Store X
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
	; LDA $0203				;
	; ADC #$08				;
	; CMP $0207				;
	; BEQ MirrorCorrectLeft	;
	
						
ReadLeftDone2:        	; handling this button is done


;-------------------------------- P2 Read Right ------------------------- 


ReadRight2: 
	LDA $4017       	; player 1 - B
	AND #%00000001  	; only look at bit 0
	BEQ ReadRightDone2   ; branch to ReadBDone if button is NOT pressed (0)
						; add instructions here to do something when button IS pressed (1)
	
						;;;;;;;;;; HEAD LEFT ;;;;;;;;;;
	LDA $021B       	; load sprite X position
	SEC			  		;
	ADC #$01		  	; Move right
	STA $021B		  	; Save X
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
						;;;;;;; HEAD RIGHT ;;;;;;;;;;
	LDA $021F       	; ** load sprite 2 x position
	SEC			  		;
	ADC #$01		  	; Move right
	STA $021F		  	; Save X
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						;;;;;;;; FRONT ;;;;;;;;;;;;;;
	LDA $0223		  	; ** Load sprite 3 x position
	SEC             	; make sure carry flag is set
	ADC #$01        	; A = A - 1
	STA $0223		  	; ** save sprite 3 x position
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						;;;;;;;;; SPINE ;;;;;;;;;;;;;;;
	LDA $0227		  	;
	SEC			  		;
	ADC #$01		  	;
	STA $0227		  	;
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
	LDA $022B			;;;;;;;;;;; FRONT LEG ;;;;;;;;;;
	SEC 				;
	ADC #$01			;
	STA $022B			;
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	
	LDA $022F			;;;;;;;;;;;; BACK LEG ;;;;;;;;;;
	SEC 				;
	ADC #$01			;
	STA $022F			;
						;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
						
						
						
						
						
ReadRightDone2:        	; handling this button is done

;--------------------- Collision Checks -------------------

; This is going to suck.
; I'm doing this in the order of "Cloak2Dagger"

CFace2DFaceX:
	LDA $0207
	CMP $021B
	BCS CFace2DFaceY
	JMP NoCollision
CFace2DFaceY:
	LDA $0204
	CMP	$0218
	BEQ	CFace2DBackX
	JMP NoCollision
CFace2DBackX:
	LDA $0207
	CMP $021F
	BCC Cloak_CollisionFront
	;BCC Dagger_CollisionFront

;Cloak_NoCollision: not used?
	
DFace2CFaceX:
	LDA $021B
	CMP $0207
	BCS DFace2CFaceY
	JMP NoCollision
DFace2CFaceY:
	LDA $0218
	CMP	$0204
	BEQ	DFace2CBackX
	JMP NoCollision
DFace2CBackX:
	LDA $021F
	CMP $0207
	;BCC Cloak_CollisionFront
	BCC Dagger_CollisionFront
	
NoCollision:
  
  RTI             ; return from interrupt


;;;;;;;;;;;;;;  

Cloak_CollisionFront:
	LDA	$021B		
	CLC
	SBC	#$00
	STA	$0207
	
	LDA $0223
	CLC
	SBC #$00
	STA $020F
	
	LDA $022B
	CLC
	SBC #$00
	STA $0217
	
	LDA $0207
	CLC
	SBC	#$07
	STA	$0203
	
	LDA $020F
	CLC
	SBC #$07
	STA $020B
	
	LDA $0217
	CLC
	SBC #$07
	STA $0213

	; Is the RTS simply looping the cloak collision and not the dagger?
	;JSR Dagger_CollisionFront

	RTS

Dagger_CollisionFront:
	LDA	$0207		
	CLC
	ADC	#$00
	STA	$021B
	
	LDA $020F
	CLC
	ADC #$00
	STA $0223
	
	LDA $0217
	CLC
	ADC #$00
	STA $022B
	
	LDA $0203
	CLC
	ADC	#$07
	STA	$021F
	
	LDA $020B
	CLC
	ADC #$07
	STA $0227
	
	LDA $0213
	CLC
	ADC #$07
	STA $022F

	RTS
	
	
  
  
  
	.bank 1
	.org $E000
palette:
	; Background Palette
	.db $0F,$20,$10,$00,$2D,$3D,$1D,$3E,$0F,$20,$10,$00,$2D,$3D,$1D,$3E
	; Sprite Palette
	.db $0F,$20,$10,$00,$2D,$3D,$1D,$3E,$0F,$20,$10,$00,$2D,$3D,$1D,$3E
	
background:
	.db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01 ; Row 1
	.db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01 ; 
	
	.db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01 ;
	.db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01 ;
	
	.db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01 ;
	.db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01 ;
	
	.db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01 ; 
	.db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01 ;
	
	.db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01 ; Row 1
	.db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01 ; 
	
	.db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01 ;
	.db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01 ;
	
	.db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01 ;
	.db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01 ;
	
	.db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01 ; 
	.db $01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01,$01 ;
	

	
attributes:
	
	.db %00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000,%00000000

sprites:
     ;vert tile attr horiz
	 ;	Y - TILE - ATTR - X
;----------------- Cloak Sprites -----------------
	.db $80, $00, $00, $40   ;headleft 	$0200 - $0203
	.db $80, $01, $00, $48	 ;headright	$0204 - $0207
	.db $88, $10, $00, $40   ;spine 	$0208 - $020B
	.db $88, $11, $00, $48   ;front		$020C - $020F
	.db	$90, $20, $00, $40	 ;back leg	$0210 - $0213
	.db $90, $21, $00, $48   ;front leg	$0214 - $0217
	
;----------------- Dagger Sprites ----------------
	.db $80, $44, $00, $B0   ;headleft 	$0218 - $021B
	.db $80, $45, $00, $B8	 ;headright	$021C - $021F
	.db $88, $54, $00, $B0   ;front 	$0220 - $0223
	.db $88, $55, $00, $B8   ;spine		$0224 - $0227
	.db	$90, $64, $00, $B0	 ;front leg	$0228 - $022B
	.db $90, $65, $00, $B8   ;back leg	$022C - $022F

	.org $FFFA     ;first of the three vectors starts here
	.dw NMI        ;when an NMI happens (once per frame if enabled) the 
				   ;processor will jump to the label NMI:
	.dw RESET      ;when the processor first turns on or is reset, it will jump
				   ;to the label RESET:
	.dw 0          ;external interrupt IRQ is not used in this tutorial

  
;;;;;;;;;;;;;;  
  
  
	.bank 2
	.org $0000
	.incbin "CND.chr"   ;includes 8KB graphics file
