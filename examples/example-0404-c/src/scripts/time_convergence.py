import math
import numpy as np
from numpy import linalg as LA
import matplotlib.pyplot as plt
from matplotlib import cm
from mpl_toolkits.mplot3d import Axes3D
import exnode_reader
import sys
import os 
from scipy import stats

# name of the folder
folder="../../results/current_run/"

# get the grid size, specific time for computing the convergence rate and the end time
if len(sys.argv) >1:
   gridsize=sys.argv[1]
   time=float(sys.argv[2])
else:
   gridsize=64
   time=5.0

print "grid size: ", gridsize
print "time: ", time

# extract time step from the name of subfolders
def get_timestep(foldername):
  #print foldername
  j_e=len(foldername)
  #print j_e
  for j in range(j_e):
    if foldername[j]=='s':
      j_s=j+3
      #print j_s
  
  #print foldername[j_s:j_e]
  timestep=float("0."+foldername[j_s:j_e])
  #print "timestep",timestep
  return timestep

# get the data respective to "time" from subfolders of the current_run folder 
def get_data_TimeConv(time):

  global folder
  ls = os.listdir(folder)
  subfolders=sorted(ls)
  
 # print "subfolders:", subfolders
  num_runs=len(subfolders)
  print "No. of runs: ", num_runs
  
  #get the spatial coordinates
  x=exnode_reader.parse_file(folder+subfolders[0]+"/"+"Time_2_0.part0.exnode",[["Coordinate",1]]) #field_name and component_no
  #print "x: ",x

  Vm=[0.0 for i in range(num_runs)]
  timesteps=[0.0 for i in range(num_runs)]

  for i in range(num_runs):
    #print "i: ",i
    #print"subfolders: ", subfolders[i]
    timesteps[i]=get_timestep(subfolders[i])
    Indx=int(time/timesteps[i])
    #print Indx
    fileToread = folder+subfolders[i]+"/"+"Time_2_"+str(Indx)+".part0.exnode" #serial version
    #print 'file',fileToread

    Vm[i]=exnode_reader.parse_file(fileToread,[["Vm",1]])
    #print Vm[i]
  print "timesteps", timesteps
  return x,Vm,timesteps

x,Vm,timesteps=get_data_TimeConv(time)

def makeplot_TimeConv(x,y,labels):

   global folder
   imagename="{}".format("../../results/image-Time-Conv-n"+str(gridsize)+"-t"+str(time)+".png")
        
   fig=plt.figure(1)
   for i in range(len(y)):
      plt.plot(x,y[i],label=str(labels[i]))
   plt.title("Vm")
   plt.legend()
   plt.show()    
   plt.savefig(imagename)
   print "Figure saved to {}".format(imagename)

print str(timesteps) 
makeplot_TimeConv(x,Vm,timesteps)

# The smallest time step is considered as the reference
def find_err_L2_rel(Vm):
  num_runs=len(Vm)  
  err=[0.0 for i in range(num_runs-1)]
  Vm_ref=Vm[0]
  Vm_L2=LA.norm(Vm[0],2)
  #print Vm_L2
  for i in range(1,num_runs):
    #print i
    err[i-1]=LA.norm(Vm[i]-Vm[0],2)/Vm_L2
    #print err
  return err

def makeplot_TimeConv_err(x,y,labels):

  global folder
  imagename="{}".format("../../results/errL2-Time-Conv-n"+str(gridsize)+"-t"+str(time)+".png")     
  fig=plt.figure(1)

  for i in range(len(y)):
    plt.loglog(x,y[i],'ro',label=labels[i])
    slope, intercept, r_value, p_value, std_err = stats.linregress(np.log10(x),np.log10(y))
    print i, slope
  plt.title("Vm")
  plt.legend()
  plt.show()    
  plt.savefig(imagename)
  print "Figure saved to {}".format(imagename)

err_L2=[0.0 for i in range(1)]
err_L2[0]=find_err_L2_rel(Vm)

labels=[None for i in range(1)]
labels[0]="1st order"

num_runs=len(timesteps)
makeplot_TimeConv_err(timesteps[1:num_runs],err_L2,labels)




