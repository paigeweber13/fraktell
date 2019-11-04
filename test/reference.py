# reference code taken from wikipedia page for julia set:
# https://en.wikipedia.org/wiki/Julia_set#Pseudocode

def f(z: complex, c: complex):
    return z**2 + c

# r is escape radius: choose R > 0 such that R**2 - R >= sqrt(cx**2 + cy**2)
def julia(z: complex, c: complex, r: float):

    iteration = 0
    max_iteration = 1000
    
    while (z.real ** 2 + z.imag ** 2 < r**2  and iteration < max_iteration):
        z = f(z, c)

        iteration = iteration + 1 
    
    if iteration == max_iteration:
        return 0
    else:
        return iteration

def test():
    c = (-0.4+0.65j)
    r = 2

    for z in [(-1.5+1.0j), (-0.4+0.4j)]:
        k = julia(z, c, r)
        print('julia(z = ', z, ', c =', c, ', r =', r, '):', k)

    # step = 0.001
    # i = -1.5
    # while i < 1.5:
    #     j = -1.5
    #     while j < 1.5:
    #         z = complex(i, j)
    #         k = julia(z, c, r)
    #         if k == 0:
    #             print('julia(z = ', z, ', c =', c, ', r =', r, '):', k)
    #         j += step
    #     i += step

if __name__ == '__main__':
    test()
