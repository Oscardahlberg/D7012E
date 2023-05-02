{- Test for Program -}
module TestProgram where

import Program
pr, pr1 :: Program.T
pr = fromString  "\
\read k;\
\read n;\
\m := 1;\
\while n-m do\
\  begin\
\    if m - m/k*k then\
\      skip;\
\    else\
\      write m;\
\    m := m + 1;\
\  end"

pr1 = fromString  "\
\read n;\
\read b;\
\m := 1;\
\s := 0;\
\p := 1;\
\while n do\
\  begin\
\    q := n/b;\
\    r := n - q*b;\
\    write r;\
\    s := p*r+s;\                    
\    p := p*10;\
\    n :=q;\
\  end\
\write s;"

pr2 = fromString  "\
\read k;\
\read n;\
\m := 1;\
\repeat\
\  begin\
\    if m - m/k*k then\
\      skip;\
\    else\
\      write m;\
\    m := m + 1;\
\  end\
\until n-m;\
\write m;"

sp = putStr (toString pr)

rp = Program.exec pr [3,16]

rp1 = Program.exec pr1 [1024, 2]

rp2 = Program.exec pr2 [3, 16]
