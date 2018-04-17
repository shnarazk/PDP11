# PDP11

```
$ PDP11-exe < Q7-11.asm
#0 Initial state
M:[0,2,0,4,0,8,1,255,0,8,0,10,0,0,0,0], R:[0,2,0,4,0,6,1,16]
#1 CLR R0	; CLR (Register (Reg 0))
M:[0,2,0,4,0,8,1,255,0,8,0,10,0,0,0,0], R:[0,2,0,4,0,6,1,18]
#2 ADD #1, R0	; ADD (Immediate 1) (Register (Reg 0))
M:[0,2,0,4,0,8,1,255,0,8,0,10,0,0,0,0], R:[1,2,0,4,0,6,1,22]
#3 ADD #2, R0	; ADD (Immediate 2) (Register (Reg 0))
M:[0,2,0,4,0,8,1,255,0,8,0,10,0,0,0,0], R:[3,2,0,4,0,6,1,26]
#4 ADD #3, R0	; ADD (Immediate 3) (Register (Reg 0))
M:[0,2,0,4,0,8,1,255,0,8,0,10,0,0,0,0], R:[6,2,0,4,0,6,1,30]
#5 ADD #4, R0	; ADD (Immediate 4) (Register (Reg 0))
M:[0,2,0,4,0,8,1,255,0,8,0,10,0,0,0,0], R:[10,2,0,4,0,6,1,34]
```

#### TODO

- [x] Text.Parsec -- parse a language
- [x] Monad -- make an interpreter
- [x] MTL -- don't invent something old again
- [x] Lens -- use concise accessors
- [x] Hspec -- automatic test
- [x] Discord-hs -- make a bot
- [ ] Servant -- run as a web server
