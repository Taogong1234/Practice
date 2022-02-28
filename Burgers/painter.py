import matplotlib.pyplot as plt
from matplotlib import animation 
import numpy as np

data1 = np.loadtxt('/home/taogang/desk/Practice/Burgers/outfx1.dat')
data2 = np.loadtxt('/home/taogang/desk/Practice/Burgers/outfx2.dat')
data3 = np.loadtxt('/home/taogang/desk/Practice/Burgers/outfx3.dat')
data4 = np.loadtxt('/home/taogang/desk/Practice/Burgers/outfx4.dat')
data5 = np.loadtxt('/home/taogang/desk/Practice/Burgers/outfx5.dat')
data6 = np.loadtxt('/home/taogang/desk/Practice/Burgers/outgx.dat')
data7 = np.loadtxt('/home/taogang/desk/Practice/Burgers/outgx.dat')
data8 = np.loadtxt('/home/taogang/desk/Practice/Burgers/outgx.dat')
data9 = np.loadtxt('/home/taogang/desk/Practice/Burgers/outgx.dat')
data10 = np.loadtxt('/home/taogang/desk/Practice/Burgers/outgx.dat')
data11 = np.loadtxt('/home/taogang/desk/Practice/Burgers/outfx6.dat')
data12 = np.loadtxt('/home/taogang/desk/Practice/Burgers/outgx.dat')


xdata1=np.linspace(0,2*np.pi,101)
xdata2=np.linspace(0,2*np.pi,101)

ydata1=data1[0,:]
ydata2=data2[0,:]
ydata3=data3[0,:]
ydata4=data4[0,:]
ydata5=data5[0,:]
ydata6=data6[0,:]
ydata7=data7[0,:]
ydata8=data8[0,:]
ydata9=data9[0,:]
ydata10=data10[0,:]
ydata11=data10[0,:]
ydata12=data10[0,:]

fig=plt.figure(figsize=(20,12), dpi=80)
plt.figure(1)
ax1 = plt.subplot(321)
line1,=ax1.plot(xdata1,ydata1,lw=2, label='lax')
ax1 = plt.subplot(321)
line6,=ax1.plot(xdata2,ydata6,lw=3, label='Analytical')
plt.legend()
ax2 = plt.subplot(322)
line2,=ax2.plot(xdata1,ydata2,lw=2, label='lax-wendroff')
ax2 = plt.subplot(322)
line7,=ax2.plot(xdata2,ydata7,lw=3, label='Analytical')
plt.legend()
ax3 = plt.subplot(323)
line3,=ax3.plot(xdata1,ydata3,lw=2, label='Maccormack')
ax3 = plt.subplot(323)
line8,=ax3.plot(xdata2,ydata8,lw=3, label='Analytical')
plt.legend()
ax4 = plt.subplot(324)
line4,=ax4.plot(xdata1,ydata4,lw=2, label='Beam-Warming')
ax4 = plt.subplot(324)
line9,=ax4.plot(xdata2,ydata9,lw=3, label='Analytical')
plt.legend()
ax5 = plt.subplot(325)
line5,=ax5.plot(xdata1,ydata5,lw=2, label='CIP')
ax5 = plt.subplot(325)
line10,=ax5.plot(xdata2,ydata10,lw=3, label='Analytical')
plt.legend()

ax6 = plt.subplot(326)
line11,=ax6.plot(xdata1,ydata11,lw=2, label='pingchou')
ax6 = plt.subplot(326)
line12,=ax6.plot(xdata2,ydata12,lw=3, label='Analytical')
plt.legend()

def init():
    ax1.set_xlim(0,7)
    ax1.set_ylim(0,9)
    ax1.set_xlabel('x')
    ax1.set_ylabel('f(x,t)')
    ax1.set_xticks(np.linspace(0,7,8))
    ax1.set_yticks(np.linspace(0,9,10))
    ax2.set_xlim(0,7)
    ax2.set_ylim(0,9)
    ax2.set_xlabel('x')
    ax2.set_ylabel('f(x,t)')
    ax2.set_xticks(np.linspace(0,7,8))
    ax2.set_yticks(np.linspace(0,9,10))
    ax3.set_xlim(0,7)
    ax3.set_ylim(0,9)
    ax3.set_xlabel('x')
    ax3.set_ylabel('f(x,t)')
    ax3.set_xticks(np.linspace(0,7,8))
    ax3.set_yticks(np.linspace(0,9,10))
    ax4.set_xlim(0,7)
    ax4.set_ylim(0,9)
    ax4.set_xlabel('x')
    ax4.set_ylabel('f(x,t)')
    ax4.set_xticks(np.linspace(0,7,8))
    ax4.set_yticks(np.linspace(0,9,10))
    ax5.set_xlim(0,7)
    ax5.set_ylim(0,9)
    ax5.set_xlabel('x')
    ax5.set_ylabel('f(x,t)')
    ax5.set_xticks(np.linspace(0,7,8))
    ax5.set_yticks(np.linspace(0,9,10))
    ax6.set_xlim(0,7)
    ax6.set_ylim(0,9)
    ax6.set_xlabel('x')
    ax6.set_ylabel('f(x,t)')
    ax6.set_xticks(np.linspace(0,7,8))
    ax6.set_yticks(np.linspace(0,9,10))
    return line1,line2,line3,line4,line5,line6,line7,line8,line9,line10,line11,line12

def upgrade(i):
    line1.set_ydata(data1[i,:])
    line2.set_ydata(data2[i,:])
    line3.set_ydata(data3[i,:])
    line4.set_ydata(data4[i,:])
    line5.set_ydata(data5[i,:])
    line6.set_ydata(data6[i,:])
    line7.set_ydata(data7[i,:])
    line8.set_ydata(data8[i,:])
    line9.set_ydata(data9[i,:])
    line10.set_ydata(data10[i,:])
    line11.set_ydata(data11[i,:])
    line12.set_ydata(data12[i,:])
 
    
    return line1,line2,line3,line4,line5,line6,line7,line8,line9,line10,line11,line12
    
ani=animation.FuncAnimation(fig=fig,func=upgrade,frames=181,init_func=init,interval=50,blit=False)
ani.save('/home/taogang/desk/Practice/Burgers/666.gif',writer='pillow')
plt.show()