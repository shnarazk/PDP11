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

- [x] Text.Parsec -- parse a Assembly
- [ ] Monad -- make an interpreter
- [ ] MTL -- don't invent something old again
- [ ] Lens -- use concise accessors
- [ ] Servant -- run as a web server
- [x] Discord-hs -- make a bot
