import matplotlib.pyplot as plt
import numpy as np



data = np.loadtxt('graph_xi.csv', delimiter=',')
#print(data[:, 0])
data = data[0:2000]

plt.scatter(data[:, 0], data[:, 1])
#plt.plot(data[0:500, 0], data[0:500, 1], marker='.', linestyle='-', color='b')

plt.xlabel('i')
plt.ylabel('X_i')
plt.show()


