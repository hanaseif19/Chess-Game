import numpy as np
import sys
sys.path.append(r'/Users/hana/.local/lib/python3.10/site-packages')
import sounddevice as sd
import matplotlib.pyplot as plt 
from scipy.fftpack import fft

f1= 261.63 # c4
f2=293.66  #d4
f3= 349.23# f4
f4= 329.63 # e4

#cc dc fe 

t= np.linspace(0 , 3, 12288)

u1=np.where(np.logical_and(t>=0, t<=0.3),1,0)
u2=np.where(np.logical_and(t>=0.5, t<=0.75),1,0)
u3=np.where(np.logical_and(t>=1.0, t<=1.25),1,0)
u4=np.where(np.logical_and(t>=1.5, t<=1.75),1,0)
u5=np.where(np.logical_and(t>=2.0, t<=2.25),1,0)
u6=np.where(np.logical_and(t>=2.5, t<=3),1,0)

x1=np.sin(2*np.pi*f1*t)*u1
x2=np.sin(2*np.pi*f1*t)*u2
x3=np.sin(2*np.pi*f2*t)*u3
x4=np.sin(2*np.pi*f1*t)*u4
x5=np.sin(2*np.pi*f3*t)*u5
x6=np.sin(2*np.pi*f4*t)*u6

# total song 
result=x1+x2+x3+x4+x5+x6

# plotting 100 samples only 
#plt.plot(t[0:100],result[0:100])
# total plot in time domain of song 
#plt.plot(t,result)
#sd.play(result,3072)
#plt.show()


#representing time domain in freq domain without noise
N=3072
#f represents the frequency domain
f= np. linspace(0 , 512 ,int(N/2))

#b3mal fourier transform lel data(y-axis)
freq_data=fft(result)

#el y axis akhdna el positive values mn x bs
# h el signal feh el freq domain mn 8air noise
h= 2/N * np.abs(freq_data[0:int(N/2)])

#plt.plot(f,h)
#plt.show()

#generating the noise by generating random frequencies then assigning them to the sin
#adding the noise to the time domaim
n1,n2=np. random.randint(0,512,2) #generates 2 number between 0 to 512 in an array

#print(n1)
#print(n2)

noise=np.sin(2*n1*np.pi*t)+np.sin(2*n2*np.pi*t)
result2=result+noise


#could use same f as x axis


#b3mal fourier transform lel data(y-axis)
freq_data2=fft(result2)

#el y axis akhdna el positive values mn x bs
m= 2/N * np.abs(freq_data2[0:int(N/2)])



i=1
N1=int(N/2)
max=m[0]
for i in range (N1) :
    if m[i]>max:
        j=i
        max=m[i]
    i=+1
"""  
print("index 1:" )
print(j)
print("freq index 1" )
print(f[j])
"""
l=1
max2=m[0]
for l in range (N1) :
    if l!=j:
      if m[l]>max2:
        b=l
        max2=m[l]
    l=+1

"""
print("index 2:" )
print(b)
print("freq index 2" )
print(f[b])   
"""
q1=int(f[j])
q2=int(f[b])  
  
xfiltered=result2-np.sin(2*np.pi*q1*t)-np.sin(2*np.pi*q2*t)

# filtered in freq domain
freq_data_filter=fft(xfiltered)
filteredfreq=2/N * np.abs(freq_data_filter[0:int(N/2)])

"""
# 1- SONG WITHOUT NOISE 
plt.subplot(2,1,1)
plt.plot(t,result)
plt.title("Song in time domain without noise")
plt.subplot(2,1,2)
plt.plot(f,h)
plt.title("Song in frequency domain without noise")
plt.subplots_adjust(wspace=0.4,hspace=0.4)

2- SONG WITH NOISE
plt.subplot(2,1,1)
plt.plot(t,xfiltered)
plt.title("Song in time domain with noise")
plt.subplot(2,1,2)
plt.plot(f,)
plt.title("Song in frequency domain with noise")
plt.subplots_adjust(wspace=0.4,hspace=0.4)
3- SONG AFTER NOISE CANCELLATION
plt.subplot(2,1,1)
plt.plot(t,xfiltered)
plt.title("Song in time domain after noise cancellation")
plt.subplot(2,1,2)
plt.plot(f,filteredfreq)
plt.title("Song in frequency domain after noise cancellation")
plt.subplots_adjust(wspace=0.4,hspace=0.4)
"""
sd.play(xfiltered,4*1024)