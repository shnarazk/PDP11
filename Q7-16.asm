	MOV #1, R2
	CLR R3
	BIT R1, R2
	BEQ 1
	ADD R0, R3
	ASL R0
	ASR R1
	BNE -6
