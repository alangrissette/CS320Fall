
# Creating a mylist: [1, 2, 3]
mylist = MyListCons(1, MyListCons(2, MyListCons(3, MyListNil())))

# Define a work function
def work_func(x):
    print(x)

# Using mylist_foreach
mylist.mylist_foreach(work_func)

# Using mylist_rforeach
mylist.mylist_rforeach(work_func)
