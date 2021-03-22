zero = (li 0)
{
    a = (li 0x79A)
    b = (li 0x347A)
    c = (add a b)
}
{
    addr = (li (<< 0x1 0x3))
    result = (load addr)
}
{
    replace_value = 0x0
    replace_addr = (add pc (li (<< 0x2 0x3)))
    (store replace_addr replace_value)
    (halt)
}
{
    (jump :skip)
    (halt)
    skip:
}
{
    a = (li 0x12)
    b = (li -0x83)
    c = add(a, b)
}
(halt)