def var w1 as log.
def var w2 as log.
def var w3 as log.
def var w4 as log.
def var p1 as dec format "ZZZZZZZZZZ".
def var c1 as dec format "ZZZZZZZZZZ".
def var i1 as int format "ZZZZ".
input from c:\p-one\weeks.
repeat:
prompt-for pro-desp.wks[1] pro-desp.wks[2] pro-desp.wks[3] pro-desp.wks[4] pro-desp.propsl# 
    pro-desp.item# pro-desp.cust#.
w1 = input pro-desp.wks[1].
w2 = input pro-desp.wks[2].
w3 = input pro-desp.wks[3].
w4 = input pro-desp.wks[4].
p1 = input pro-desp.propsl#.
i1 = input pro-desp.item#.
c1 = input pro-desp.cust#.
find pro-desp where propsl# = p1 and cust# = c1 and item# = i1.
pro-desp.wks[1] = w1.
pro-desp.wks[2] = w2.
pro-desp.wks[3] = w3.
pro-desp.wks[4] = w4.
end.          
