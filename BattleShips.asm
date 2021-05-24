include Irvine32.inc
INCLUDE Macros.inc
; displayBoards
; disPlayerBoard
; disEnemyBaord
; strike
; placeShips
; PlaceEnemyShips

.data
	enemyBoard BYTE	25 DUP ('-')
	playerBoard BYTE 25 DUP ('-')
	shipCount DD 5
	enemyShipCount DD 5
	enemyShips BYTE 5 DUP (?)
	placeholder = '*'
	pin = '^'
	ship = 'S'
	Water = '~'
	hit = 'X'

.code
main PROC
call resetBoards
call welcomeScreen
mWrite "                                    Press any key to continue."
call readChar
call clrscr
call placeShips
call strike
exit
main endp

;----------Display PLAYER Board----------;
disPlayerBoard PROC uses ESI
	mov ESI, offset playerBoard
	mov ECX, lengthof playerBoard
	player:
		mov EDX, 0
		mov EAX, ECX
		mov EBX, 5
		div EBX
		cmp EDX, 0
		jne next2
		call CRLF
		mwrite "                                          "
		next2:
		mov AL, [ESI]
		call WriteChar
		mov AL, ' '
		call WriteChar
		INC ESI
		LOOP player
	RET
disPlayerBoard ENDP

;----------Display ENEMY Board----------;
disEnemyBoard PROC uses ESI
	mov ESI, offset enemyBoard
	mov ECX, lengthof enemyBoard
	player:
		mov EDX, 0
		mov EAX, ECX
		mov EBX, 5
		div EBX
		cmp EDX, 0
		jne next2
		call CRLF
		mwrite "                                          "
		next2:
		mov AL, [ESI]
		call WriteChar
		mov AL, ' '
		call WriteChar
		INC ESI
		LOOP player
	RET
disEnemyBoard ENDP

;----------Display Boards Procedure----------;
displayBoards PROC uses ESI
	mGotoxy 0,0
;---ENEMY BOARDS---;
	mov EAX, lightgreen
	call settextcolor
	mWriteln "                                         Enemy Board"
	call disEnemyBoard
	call CRLF
;---Player BOARDS---;
	call CRLF
	mov EAX, cyan
	call settextcolor
	mWriteln "                                         Player Board"
	call disPlayerBoard
	RET
displayBoards endp

;----------Strike Enemy Ships----------;
strike PROC
	mov ESI, OFFSET enemyBoard
	mov byte ptr [ESI],pin
	call displayBoards
key:
	mov EAX, 50
	call DELAY
	call ReadKey
	JZ key
	mov bx,'d'
	mov cx,'a'
	cmp dx, bx 
	JE right
	cmp dx, cx
	JE left
	cmp dx, VK_DOWN
	JE down
	cmp dx, VK_UP
	JE up
	cmp dx,VK_SPACE
	JE space
	jmp key

	right:
		mov EDX, 0
		skipR:
			inc EDX
			;check boundary condition
			mov edi,esi
			add edi,edx					;next step value (edi = esi + edx)
			mov ecx, offset enemyBoard				;offset of enemyBoard
			add ecx,25					;boundary condition = playerBoard(ptrr) + len(25)
			cmp edi,ecx
			JAE key
			mov BL, water		; '~'
			cmp bl, byte ptr [esi+EDX]
			JE skipR
			mov BL, hit
			cmp BL, byte ptr [esi+EDX]
			JE skipR
		nextR:
			add esi, EDX
			mov byte ptr [ESI], pin			; '^'
			neg EDX
			mov byte ptr [ESI+EDX], '-'		; replace last index with '-'
			mgotoxy 0,0
			call displayBoards
			jmp key
	left:
		mov BL, water	;'~'
		cmp bl,[esi-1]
		je key
		mov BL, hit		;'X'
		cmp bl,[esi-1]
		je key
		mov edx,esi
		dec esi
		;check boundary condition
			mov edi,esi
			mov ecx, offset enemyBoard			;offset of enemyBoard
			cmp edi,ecx
			JNB skippp
			next:
				mov esi,edx
				jmp key
		;
		skippp:
		mov byte ptr[ESI], pin
		mov byte ptr[ESI+1], '-'
		mgotoxy 0,0
		call displayBoards
		jmp key
	down:
		mov EDX, 0
		skipD:
			add EDX,5
			mov edi,esi
			add edi,edx					;next step value (edi = esi + edx)
			mov ecx, offset enemyBoard	;offset of enemyBoard
			add ecx,25					;boundary condition = playerBoard(ptrr) + len(25)
			cmp edi,ecx
			JAE key
			mov BL, water  ;  '~'
			cmp bl, byte ptr [esi+EDX]
			JE skipD
			mov BL, hit  ;  'X'
			cmp bl, byte ptr [esi+EDX]
			JE skipD
		nextD:
			add esi, EDX
			mov byte ptr [ESI], pin		; '^'
			neg EDX
			mov byte ptr [ESI+EDX], '-'	; replace last index with '-'
			mgotoxy 0,0
			call displayBoards
			jmp key
	up:
		mov BL, water	;'~'
		cmp bl,[esi-5]
		je key
		mov BL, hit		;'X'
		cmp bl,[esi-5]
		je key
		mov edx,esi
		sub esi, 5
		;check boundary condition
			mov edi,esi
			mov ecx, offset enemyBoard		;offset of playerBoard
			cmp edi,ecx
			JNB skipp
			nextt:
				mov esi,edx
				jmp key
		;
		skipp:
		mov byte ptr[ESI], pin
		mov byte ptr[ESI+5], '-'
		mgotoxy 0,0
		call displayBoards
		jmp key

	space:
		mov byte ptr [ESI], placeholder ; '*'
		mov EBX, 0
		mov esi, offset enemyBoard
		mov ecx, 25
		L2:
			mov AL, placeholder	;'*'
			cmp [ESI], AL	;if '*' is found in enemyShip Board
			JE markXorW		
			inc EBX		; else inc ebx (counter) = index in board
			inc esi
		loop L2
		markXorW:
			CLD
			mov EDI, offset enemyShips
			mov al, bl ; index at which '*' is placed to be substituted by 'X' or '~'
			mov ecx,5
			L3:
				cmp [edi],al
				jz attack
				inc edi
			LOOP L3
			mov esi,offset enemyBoard
			mov byte ptr [esi + EBX], '~'
			jmp skip
		attack:
			mov esi, offset enemyBoard
			mov byte ptr [esi + EBX],hit
			dec enemyShipCount
			cmp enemyShipCount, 0
			JNE skip
			call gameOver
		skip:
			call enemyStrike
			cmp shipCount, 0
			JNE skip2
			call gameOver
			skip2:
			call displayBoards
			jmp key
		RET
strike ENDP

;----------Place SHIPS in Players Board----------;
placeShips PROC
	mwriteln "                               Use arrow keys to move around the board."
	mwriteln "                                 INFO: Press SPACE to Place a Ship."
	call crlf
	mov ESI, OFFSET playerBoard
	mov byte ptr [ESI],pin
	mov EAX, cyan
	call settextcolor
	call disPlayerBoard
	key:
		mov EAX, 50
		call DELAY
		call ReadKey
		JZ key
		cmp dx, VK_RIGHT
		JE right
		cmp dx, VK_LEFT
		JE left
		cmp dx, VK_DOWN
		JE down
		cmp dx, VK_UP
		JE up
		cmp dx, VK_SPACE
		JE space
		jmp key
	right:
		mov EDX, 1
		skipR:
			;check boundary condition
			mov edi,esi
			add edi,edx					;next step value (edi = esi + edx)
			mov ecx, offset playerBoard				;offset of playerBoard
			add ecx,25					;boundary condition = playerBoard(ptrr) + len(25)
			cmp edi,ecx
			JAE key
			mov BL, ship	; 'S'
			cmp bl, byte ptr [esi+EDX]
			JNE nextR
			inc EDX
			jmp skipR
		nextR:
			add esi, EDX
			mov byte ptr [ESI], pin		; '^'
			neg EDX
			mov byte ptr [ESI+EDX], '-' ; replace last index with '-'
			mgotoxy 0,3
			call disPlayerBoard
			jmp key
	left:
		mov BL, ship
		cmp bl,[esi-1]
		je key
		mov edx,esi
		dec esi
		;check boundary condition
			mov edi,esi
			mov ecx, offset playerBoard			;offset of playerBoard
			cmp edi,ecx
			JNB skippp
			next:
				mov esi,edx
				jmp key
		;
		skippp:
		mov byte ptr[ESI], pin
		mov byte ptr[ESI+1], '-'
		mgotoxy 0,3
		call disPlayerBoard
		jmp key
	down:
		mov EDX, 5
		skipD:
			;check boundary condition
			mov edi,esi
			add edi,edx					;next step value (edi = esi + edx)
			mov ecx, offset playerBoard	;offset of playerBoard
			add ecx,25					;boundary condition = playerBoard(ptrr) + len(25)
			cmp edi,ecx
			JAE key
			;
			mov BL, ship	; 'S'
			cmp bl, byte ptr [esi+EDX]
			JNE nextD
			add EDX,5
			jmp skipD
		nextD:
			add esi, EDX
			mov byte ptr [ESI], pin		; '^'
			neg EDX
			mov byte ptr [ESI+EDX], '-'	; replace last index with '-'
			mgotoxy 0,3
			call disPlayerBoard
			jmp key
	up:
		mov BL, ship
		cmp bl,[esi-5]
		je key
		mov edx,esi
		sub esi, 5
		;check boundary condition
			mov edi,esi
			mov ecx, offset playerBoard		;offset of playerBoard
			cmp edi,ecx
			JNB skipp
			nextt:
				mov esi,edx
				jmp key
		;
		skipp:
		mov byte ptr[ESI], pin
		mov byte ptr[ESI+5], '-'
		mgotoxy 0,3
		call disPlayerBoard
		jmp key
	space:
		dec shipCount
		mov byte ptr [ESI], ship
		mov ESI, offset playerBoard
		mgotoxy 0,3
		call disPlayerBoard
		mov edx,0
		cmp shipCount,0
		je final
		jmp key
	final:
		mov shipcount, 5
		call placeEnemyShips
		call CLRSCR
		mGotoxy 0, 0

	RET
placeShips endp

;----------Place enemy ships----------;
placeEnemyShips PROC uses ESI
	mov ESI, offset enemyShips
	mov ECX, lengthof enemyShips
	call Randomize
	L1:
		call Randomize
		mov EAX, 25
		call RandomRange
		cmp eax,[esi-1]
		je L1
		cmp eax, 0
		je L1
		mov [ESI],EAX
		inc ESI
		dec ECX
		cmp ECX, 0
		JBE skipL1
	JMP L1
	skipL1:
	call CLRSCR

	ret
placeEnemyShips endp

;----------Startup----------;
welcomeScreen PROC
	mov eax, MAGENTA
	call settextcolor
	mWriteln "             BBBBB         A     TTTTTTTTTTT  TTTTTTTTTTT  L            EEEEEEEEE  "
	mWriteln "             B     B      A A         T            T       L            E          "
	mWriteln "             B      B    A   A        T            T       L            E          " 
	mWriteln "             B     B    A     A       T            T       L            E          "
	mWriteln "             BBBBBB    AAAAAAAAA      T            T       L            EEEEEE     "
	mWriteln "             B     B   A       A      T            T       L            E          "
	mWriteln "             B      B  A       A      T            T       L            E          "
	mWriteln "             B     B   A       A      T            T       L            E          "   
	mWriteln "             BBBBBB    A       A      T            T       LLLLLLLLLLL  EEEEEEEEE  "
	call CRLF
	mov eax, YELLOW
	call settextcolor
	mwriteln "                           SSSSSS   H     H   I  PPPPPP    SSSSSS "
	mwriteln "                           S    S   H     H   I  P     P   S    S "
	mwriteln "                           S    S   H     H   I  P      P  S      "
	mwriteln "                           S        H     H   I  P     P   S      "
	mwriteln "                           SSSSSS   HHHHHHH   I  PPPPPP    SSSSSS "
	mwriteln "                                S   H     H   I  P              S "
	mwriteln "                                S   H     H   I  P              S "
	mwriteln "                           S    S   H     H   I  P         S    S "
	mwriteln "                           SSSSSS   H     H   I  P         SSSSSS "
	mov eax, WHITE
	call settextcolor
	call CRLF
	mWriteln "                                               'INFO'"
	mWriteln "                                  '-' is empty space. Can be targeted."
	mWriteln "                                  'S' is Ship."
	mWriteln "                                  'X' is destroyed ship. Cannot be targeted."
	mWriteln "                                  '~' Wasted missile. Cannot be targeted."
	mWriteln "                                  '^' is pointer. Use arrow keys to navigate."
    call CRLF
	RET
welcomeScreen endp

;----------Game Over Display----------;
gameOver PROC
	call CLRSCR
	call displayBoards
	mGotoxy 0, 20
	cmp enemyShipCount, 0
	JNBE playerWon
	mov EAX, cyan
	call settextcolor
	mWriteln "                       Congratulations! Player has won. Press 'a', 'A' to play again."
	jmp next
	playerWon:
		mov EAX, lightgreen
		call settextcolor
		mWriteln "                        You Noob! The enemy has won. Press 'a', 'A' to play again."
	next:
	call readchar
	mov bl, 'a'
	mov bh, 'A'
	cmp al, bl
	JE again
	jmp next
	again:
	call clrscr
	call main
	ret
gameOver endp

;----------Enemy Random Strike Function----------;
enemyStrike PROC uses ESI
	start:
		mov ESI, 0
		call randomize
		mov EAX, 25
		call randomRange
		mov BL, byte ptr playerBoard[ESI + EAX]
		mov dl,hit
		cmp bl,dl
		je start
		mov dl,water
		cmp bl,dl
		je start
		mov BH, ship
		cmp BL, BH
		JNE next
		mov byte ptr playerBoard[ESI + EAX], hit
		dec shipCount
		jmp skip
	next:
		mov byte ptr playerBoard[ESI + EAX], water
	skip:
	RET
enemyStrike endp

resetBoards PROC
	mov esi,0
	mov ecx,lengthof playerBoard
	L1:
		mov playerBoard[esi],'-'
		mov enemyBoard[esi],'-'
		inc esi
	LOOP L1
	mov shipCount, 5
	mov enemyShipCount, 5
ret
resetBoards endp

;----------The End----------;
end main