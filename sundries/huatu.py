# import matplotlib.pyplot as plt
# from matplotlib import animation 
# import numpy as np

# data1 = np.loadtxt('/home/taogang/desk/Practice/sundries/outfx1.dat')
# data2 = np.loadtxt('/home/taogang/desk/Practice/sundries/outgx.dat')
# fig,ax=plt.subplots(figsize=(10,5),dpi=100)
# xdata1=np.linspace(0,2*np.pi,101)
# xdata2=np.linspace(0,2*np.pi,101)
# ydata1=data1[0,:]
# ydata2=data2[0,:]
# line1,=ax.plot(xdata1,ydata1,lw=2, label='Computational')
# line2,=ax.plot(xdata2,ydata2,lw=3, label='Analytical')

# def init():
#     plt.xlim(0,7)
#     plt.ylim(0,9)
#     plt.xlabel('x')
#     plt.ylabel('f(x,t)')
#     plt.xticks(np.linspace(0,7,8))
#     plt.yticks(np.linspace(0,9,10))
#     plt.legend()
#     plt.title('Convection')
#     return line1,line2

# def upgrade(i):
#     line1.set_ydata(data1[i,:])
#     line2.set_ydata(data2[i,:])
#     return line1,line2
    
# ani=animation.FuncAnimation(fig=fig,func=upgrade,frames=251,init_func=init,interval=20,blit=False)
# # ani.save('/home/taogang/desk/Practice/sundries/211.gif',writer='pillow')
# # ani.save('/home/taogang/desk/Practice/sundries/666.mp4')
# plt.show()



from cmath import pi
import matplotlib.pyplot as plt
from matplotlib import animation 
import numpy as np

data = np.loadtxt('/home/taogang/desk/Practice/sundries/outfx1.dat')  #位置需要矫正
fig,ax=plt.subplots(figsize=(10,5),dpi=100)
xdata=np.linspace(0,2*pi,101)
ydata=data[0,:]
line,=ax.plot(xdata,ydata,'y')

def init():
    # ax.set_xlim(-1,16)
    # ax.set_yticks(np.linspace(0,1,11))#注意加set、加yticks的s
    plt.xlim(0,2*pi)
    plt.ylim(0,7)
    plt.xlabel('x')
    plt.ylabel('fx')
    plt.xticks(np.linspace(0,7,8))
    plt.yticks(np.linspace(0,9,10))
    return line,

def upgrade(i):
    line.set_ydata(data[i,:])
    return line,
    
ani=animation.FuncAnimation(fig=fig,func=upgrade,frames=600,init_func=init,interval=20,blit=False)
# ani.save('111.mp4')# conda install -c conda-forge/label/broken ffmpeg
plt.show()