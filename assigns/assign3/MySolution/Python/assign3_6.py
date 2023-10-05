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

def mylist_iter(lst):
    i0 = 0
    while i0 < len(lst.args):
        yield lst.args[i0]
        i0 += 1

class mylist_reverse:
    def __init__(self, lst):
        self.lst = lst

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

