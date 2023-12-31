###########################################################################
#
# MyPython.ml is a library
# built for CS320, Fall, 2023
#
###########################################################################

def int1_forall(n0, test_func):
    i0 = 0
    while(i0 < n0):
        if not test_func(i0):
            return False
        i0 = (i0 + 1)
    return True # test_func(i0)==True for all 0 <= i0 < n0

def int1_foreach(n0, work_func):
    i0 = 0
    while(i0 < n0):
        work_func(i0)
        i0 = (i0 + 1)
    return None # work_func(i0) is done for all 0 <= i0 < n0

def int1_rforeach(n0, work_func):
    i0 = 0
    while(i0 < n0):
        work_func(n0-1-i0)
        i0 = (i0 + 1)
    return None # work_func(i0) is done for all n0 > i0 >= 0

def int1_map_fnlist(xs, fopr_func):
    return foreach_to_map_fnlist(int1_foreach)(xs, fopr_func)
def int1_map_pylist(xs, fopr_func):
    return foreach_to_map_pylist(int1_foreach)(xs, fopr_func)

def int1_foldleft(xs, r0, fopr_func):
    return foreach_to_foldleft(int1_foreach)(xs, r0, fopr_func)
def int1_foldright(xs, r0, fopr_func):
    return rforeach_to_foldright(int1_rforeach)(xs, r0, fopr_func)

###########################################################################

# datatype 'a list =
# nil | cons of ('a * 'a list)

class fnlist:
    ctag = -1
    def get_ctag(self):
        return self.ctag
    def __iter__(self):
        return fnlist_iter(self)
    def __reversed__(self):
        return fnlist_reverse(self)
# end-of-class(fnlist)

class fnlist_iter:
    def __iter__(self):
        return self
    def __init__(self, itms):
        self.itms = itms
    def __next__(self):
        if (self.itms.ctag==0):
            raise StopIteration
        else:
            itm1 = self.itms.cons1
            self.itms = self.itms.cons2
            return itm1
    # end-of-[__next__]

###########################################################################

class fnlist_nil(fnlist):
    def __init__(self):
        self.ctag = 0
        # return None
# end-of-class(fnlist_nil)

class fnlist_cons(fnlist):
    def __init__(self, cons1, cons2):
        self.ctag = 1
        self.cons1 = cons1
        self.cons2 = cons2
        # return None
    def get_cons1(self):
        return self.cons1
    def get_cons2(self):
        return self.cons2
# end-of-class(fnlist_cons)

####################################################
def fnlist_sing(x0):
    res = fnlist_nil()
    res = fnlist_cons(x0, res)
    return res
####################################################
def fnlist_print(xs):
    nx = 0
    sep = "; "
    print("fnlist[",end='')
    while(xs.ctag > 0):
        if (nx > 0):
            print(sep,end='')        
        print(xs.cons1,end='')
        nx = nx + 1; xs = xs.cons2
    print("]", end='')
####################################################
def fnlist_reverse(xs):
    res = fnlist_nil()
    for x1 in xs:
        res = fnlist_cons(x1, res)
    return res
####################################################

############### end of [CS320-2023-Fall-classlib-MyPython.py] ###############


class mylist:
    def __init__(self, ctag, *args):
        self.ctag = ctag
        self.args = args

    def get_ctag(self):
        return self.ctag

    def __iter__(self):
        return mylist_iter(self)

    def __reversed__(self):
        return mylist_reverse(self)

class mylist_nil(mylist):
    def __init__(self):
        super().__init__(0)


class mylist_cons(mylist):
    def __init__(self, x1, xs):
        super().__init__(1, x1, xs)


def mylist_iter(lst):
    i0 = 0
    while i0 < len(lst.args):
        yield lst.args[i0]
        i0 += 1

class mylist_reverse:
    def __init__(self, lst):
        self.lst = lst


class mylist_snoc(mylist):
    def __init__(self, xs, x1):
        super().__init__(2, xs, x1)


class mylist_append2(mylist):
    def __init__(self, xs1, xs2):
        super().__init__(4, xs1, xs2)



class mylist_reverse(mylist):
    def __init__(self, lst):
        super().__init__(3, lst)

    def __iter__(self):
        i0 = len(self.args[0].args) - 1
        while i0 >= 0:
            yield self.args[0].args[i0]
            i0 -= 1

            
    def __iter__(self):
        i0 = len(self.lst.args) - 1
        while i0 >= 0:
            yield self.lst.args[i0]
            i0 -= 1

def mylist_reverse_iter(reverse_lst):
    i0 = len(reverse_lst.args) - 1
    while i0 >= 0:
        yield reverse_lst.args[i0]
        i0 -= 1


def mylist_foreach(xs, work):
    if xs.get_ctag() == 0:
        return
    elif xs.get_ctag() == 1:
        work(xs.args[0])
        mylist_foreach(xs.args[1], work)
    elif xs.get_ctag() == 2:
        mylist_foreach(xs.args[0], work)
        work(xs.args[1])
    elif xs.get_ctag() == 3:
        mylist_rforeach(xs.args[0], work)
    elif xs.get_ctag() == 4:
        mylist_foreach(xs.args[0], work)
        mylist_foreach(xs.args[1], work)

def mylist_rforeach(xs, work):
    if xs.get_ctag() == 0:
        return
    elif xs.get_ctag() == 1:
        mylist_rforeach(xs.args[1], work)
        work(xs.args[0])
    elif xs.get_ctag() == 2:
        work(xs.args[1])
        mylist_rforeach(xs.args[0], work)
    elif xs.get_ctag() == 3:
        mylist_foreach(xs.args[0], work)
    elif xs.get_ctag() == 4:
        mylist_rforeach(xs.args[1], work)
        mylist_rforeach(xs.args[0], work)

