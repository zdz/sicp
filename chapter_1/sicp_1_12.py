#!/usr/bin/python

def pascal(n):
    """
    pascal(n) == 2**n-1
    
    >>> pascal(1)
    1
    >>> pascal(2)
    3
    >>> pascal(3)
    7
    >>> pascal(4)
    15
    >>> pascal(5)
    31
    >>> pascal(168)
    374144419156711147060143317175368453031918731001855L
    """
    
    ret = 0
    k = 0
    a = [[1 for i in range(n)] for j in range(2)]
 
    for i in range(n):
        for j in range(0,i+1):
            if i == 0 or j == 0 or i == j:
                a[k][j] = 1
            else:
                a[k][j] = a[1-k][j] + a[1-k][j-1]
            ret += a[k][j]
        k = 1-k
    return ret
    
#./sicp_1_12.py -v
if __name__ == "__main__":
    import doctest
    doctest.testmod()

