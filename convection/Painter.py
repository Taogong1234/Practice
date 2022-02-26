import matplotlib.pyplot as plt
from matplotlib import animation 
import numpy as np

data1 = np.loadtxt('/home/taogang/desk/Practice/convection/outfx.dat')
data2 = np.loadtxt('/home/taogang/desk/Practice/convection/outgx.dat')
fig,ax=plt.subplots(figsize=(10,5),dpi=100)
xdata1=np.linspace(0,15,151)
xdata2=np.linspace(0,15,301)
ydata1=data1[0,:]
ydata2=data2[0,:]
line1,=ax.plot(xdata1,ydata1,lw=2, label='Computational')
line2,=ax.plot(xdata2,ydata2,lw=3, label='Analytical')

def init():
    plt.xlim(-1,16)
    plt.ylim(-0.2,1.2)
    plt.xlabel('x')
    plt.ylabel('f(x,t)')
    plt.xticks(np.linspace(0,15,16))
    plt.yticks(np.linspace(0,1,11))
    plt.legend()
    plt.title('Convection')
    return line1,line2

def upgrade(i):
    line1.set_ydata(data1[i,:])
    line2.set_ydata(data2[i,:])
    return line1,line2
    
ani=animation.FuncAnimation(fig=fig,func=upgrade,frames=301,init_func=init,interval=20,blit=False)
# ani.save('666.gif',writer='pillow')
ani.save('/home/taogang/desk/Practice/convection/666.mp4')
plt.show()
