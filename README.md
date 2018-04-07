# PDP11

```
$ PDP11-exe < Q7-11.asm
#0 Initial state
M: [0,2,0,4,0,8,1,255,0,0,0,0,0,0,0,0], R: [0,2,0,4,0,6,0,0]
#1 CLR R0       ; CLR (Register (Reg 0))
M: [0,2,0,4,0,8,1,255,0,0,0,0,0,0,0,0], R: [0,2,0,4,0,6,0,0]
#2 ADD #1, R0   ; ADD (Immediate 1) (Register (Reg 0))
M: [0,2,0,4,0,8,1,255,0,0,0,0,0,0,0,0], R: [1,2,0,4,0,6,0,0]
#3 ADD #2, R0   ; ADD (Immediate 2) (Register (Reg 0))
M: [0,2,0,4,0,8,1,255,0,0,0,0,0,0,0,0], R: [3,2,0,4,0,6,0,0]
#4 ADD #3, R0   ; ADD (Immediate 3) (Register (Reg 0))
M: [0,2,0,4,0,8,1,255,0,0,0,0,0,0,0,0], R: [6,2,0,4,0,6,0,0]
#5 ADD #4, R0   ; ADD (Immediate 4) (Register (Reg 0))
M: [0,2,0,4,0,8,1,255,0,0,0,0,0,0,0,0], R: [10,2,0,4,0,6,0,0]
```

#### TODO

- [x] Text.Parsec -- parse a Assembly
- [x] Monad -- make an interpreter
- [ ] MTL -- don't invent something old again
- [x] Lens -- use concise accessors
- [ ] Servant -- run as a web server
- [x] Discord-hs -- make a bot
