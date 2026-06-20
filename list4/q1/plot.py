import matplotlib.pyplot as plt
import numpy as np

# Intervalo muito menor para focar onde a curva tem variação visível
x = np.linspace(-2, 2, 100000000)

y = 5*x**3 - 5*x - 24

zero = [index for index, val in enumerate(y) if abs(val) <= 1]

zero = x[zero].mean()
print(zero)

#plt.plot(x, y)
# Cores e espessuras ajudam a não confundir os eixos com a curva
#plt.axhline(0, color='black', linewidth=0.8)
#plt.axvline(0, color='black', linewidth=0.8)
#plt.grid(True, linestyle='--', alpha=0.6) 

#plt.show()