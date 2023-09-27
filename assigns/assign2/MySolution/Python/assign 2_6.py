def string_merge(cs1, cs2):
    n1 = len(cs1)
    n2 = len(cs2)

    def foreach(i1, i2, work):
        if i1 < n1:
            if i2 < n2:
                c1 = cs1[i1]
                c2 = cs2[i2]
                if c1 <= c2:
                    work(c1)
                    foreach(i1 + 1, i2, work)
                else:
                    work(c2)
                    foreach(i1, i2 + 1, work)
            else:
                int1_foreach(n1 - i1, work_func(cs1, i1))
        else:
            int1_foreach(n2 - i2, work_func(cs2, i2))

    def string_make_fwork(fwork):
        result = []
        
        def work(c):
            result.append(c)
        
        foreach(0, 0, work)
        return ''.join(result)

    return string_make_fwork

def int1_foreach(n0, work_func):
    def work(i):
        work_func(i)

    i0 = 0
    while i0 < n0:
        work(i0)
        i0 += 1

def work_func(cs, i):
    def inner_work(c):
        result.append(c)

    inner_work(cs[i])

