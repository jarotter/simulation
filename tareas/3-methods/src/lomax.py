import scipy.integrate as integrate
import numpy as np

def lomax(a,b):
    return lambda x: np.where(x<0, 0, a*b**a/np.power(x+b,a+1))

mean = integrate.quad(lambda x: x*lomax(5,125000)(x), 0, np.inf)
esq = integrate.quad(lambda x: np.power(x,2)*lomax(5,125000)(x), 0, np.inf)
#var = mean**2-esq

print(mean)
#print(var)
