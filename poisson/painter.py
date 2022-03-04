import numpy as np
import matplotlib.pyplot as plt
from matplotlib import cm
from mpl_toolkits.mplot3d import axes3d 
data = np.loadtxt('/home/taogang/desk/Practice/poisson/poisson.dat')
fig=plt.figure(figsize=(20,14),dpi=80)
ax=fig.add_subplot(projection='3d')
xx,yy=np.meshgrid(np.linspace(0,2,600),np.linspace(0,2,600))
surf=ax.plot_surface(xx,yy,data[:],rstride=1,cstride=1,cmap=cm.viridis,linewidth=0, antialiased=False)
ax.view_init(30,225)
ax.set_xlabel('x')
ax.set_ylabel('y')

plt.savefig('./777.jpg')
plt.show()



