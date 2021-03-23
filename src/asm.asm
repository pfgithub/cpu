zero = (li 0x0)
{
    a = (li 0x79A)
    b = (li 0x347A)
    c = (add a b)
}
{
    addr = (li 0o10)
    result = (load addr)
}
{
    replace_value = (li 0x0)
    replace_addr = (add (li 0d3) (add #pc (li :halt_lbl)))
    (store replace_addr replace_value)
    halt_lbl: (halt)
}
//{
//    (jump :skip)
//    (halt)
//    skip:
//}
{
    a = (li 0x12)
    b = (li -0x83)
    c = (add a  b)
}
#pc = (li 0d0)
(halt)