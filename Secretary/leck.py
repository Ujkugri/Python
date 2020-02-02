import numpy as np

if __name__ == '__main__':

    for N in range(1, 101):
        sum = 0
        for k in range(N+1,101):
            sum += 1/(k-1)
        print(N, round(N*sum,2))
