import matplotlib.pyplot as plt
import numpy as np



data = np.loadtxt('graph_hist.csv', delimiter=',')
#print(data[:, 0])
data = data[0:2000]

plt.hist(data, bins=80, density=True)
#plt.plot(data[0:500, 0], data[0:500, 1], marker='.', linestyle='-', color='b')

plt.show()


