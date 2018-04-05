# PDP11

```
$ PDP11-exe < Q7-11.asm
# Initial state
M: [0,10,20,40,80,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], R: [0,1,0,2,0,5,0,0]
CLR (Register (Reg 0))
M: [0,10,20,40,80,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], R: [0,1,0,2,0,5,0,0]
ADD (Immediate 1) (Register (Reg 0))
M: [0,10,20,40,80,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], R: [1,1,0,2,0,5,0,0]
ADD (Immediate 2) (Register (Reg 0))
M: [0,10,20,40,80,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], R: [3,1,0,2,0,5,0,0]
ADD (Immediate 3) (Register (Reg 0))
M: [0,10,20,40,80,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], R: [6,1,0,2,0,5,0,0]
ADD (Immediate 4) (Register (Reg 0))
M: [0,10,20,40,80,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0], R: [10,1,0,2,0,5,0,0]
$
```


#### TODO

1. Text.Parsec -- parse a Assembly
1. Monad -- make an interpreter
1. Lens -- use concise accessors
1. Servant -- run as a web server
1. Discord-hs -- make a bot
