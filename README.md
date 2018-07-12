# PDP11

- https://programmer209.wordpress.com/2011/08/03/the-pdp-11-assembly-language/

```
If N is an address in memory then (N) is the data stored at the address N.
Syntax       Mode                          Action
Rn           Register                      Data = Rn
(Rn)+        Autoincrement                 Data = (Rn)
                                           Rn++
-(Rn)        Autodecrement                 Rn–
                                           Data = (Rn)
X(Rn)        Index                         Offset address X = (PC)
                                           PC += 2
                                           Base address = Rn
                                           Data = (Rn + X)
@Rn or (Rn)  Register Deferred             Data = (Rn)
@(Rn)+       Autoincrement Deferred        Data =((Rn))
                                           Rn++
@-(Rn)       Autodecrement Deferred        Rn–
                                           Data =((Rn))
@X(Rn)       Index Deferred                Offset address X = (PC)
                                           PC += 2
                                           Base address = Rn
                                           Data = ((Rn + X))
#n           Immediate                     Data = (PC) = n
@#A          Immediate Deferred (Absolute) Data = ((PC)) = (A)
A or X(PC)   Relative                      Offset address X = (PC)
                                           PC += 2
                                           Data = (PC + X) = (A)
@A or @X(PC) Relative Deferred             Offset address X = (PC)
                                           PC += 2
                                           Data = ((PC + X)) = ((A))
```

### examples

```
$ PDP11-exe  < Q7-14.asm
#0 Initial state
M:[0,2,0,4,0,8,1,255,0,8,0,10,0,0,0,0], R:[0,2,0,4,0,6,1,16]
#1 MOV #0, R1   ; MOV (Immediate 0) (Register (Reg 1))
M:[0,2,0,4,0,8,1,255,0,8,0,10,0,0,0,0], R:[0,0,0,4,0,6,1,20]
#2 CLR @(R1)+   ; CLR (Indirect (AutoInc (Reg 1)))
M:[0,2,0,0,0,8,1,255,0,8,0,10,0,0,0,0], R:[0,2,0,4,0,6,1,22]
#3 CLR @(R1)+   ; CLR (Indirect (AutoInc (Reg 1)))
M:[0,0,0,0,0,8,1,255,0,8,0,10,0,0,0,0], R:[0,4,0,4,0,6,1,24]
#4 CLR @(R1)+   ; CLR (Indirect (AutoInc (Reg 1)))
M:[0,0,0,0,0,8,1,255,0,0,0,10,0,0,0,0], R:[0,6,0,4,0,6,1,26]

MOV #0, R1                      0001_0011_1100_0001
                                0000_0000_0000_0000
CLR @(R1)+                      0000_1010_0001_1001
CLR @(R1)+                      0000_1010_0001_1001
CLR @(R1)+                      0000_1010_0001_1001
```

#### TODO

- [x] Text.Parsec -- parse a language
- [x] Monad -- make an interpreter
- [x] MTL -- don't invent something old again
- [x] Lens -- use concise accessors
- [x] Hspec -- automatic test
- [x] Discord-hs -- make a bot
- [x] Servant -- run as a web server
- [X] Bits -- Everything should be a bit stream
- [X] TemplateHaskell -- ditch stupid iterations
