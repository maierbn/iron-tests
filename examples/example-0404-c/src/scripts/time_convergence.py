import math
import numpy as np
from numpy import linalg as LA
import matplotlib as mpl
mpl.use('Agg')
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
   gridsize=512
   time=5.0

print "grid size: ", gridsize
print "time: ", time
###############################################################################
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
###############################################################################
# get the data for the time convergence study from the current_run folder 
def get_data_TimeConv(time):

  global folder
   
  ls = os.listdir(folder)
  condition_folder= lambda name: "l1x1" in name and gridsize in name
  ls_selected=list(np.extract(map(condition_folder,ls),ls))
  subfolders=sorted(ls_selected)
  #print "subfolders:", subfolders
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
  print "timesteps: ", timesteps
  return x,Vm,timesteps
###############################################################################
def makeplot_TimeConv(x,y,labels):

   global folder
   imagenamepng="{}".format(folder+"image-Time-Conv-n"+str(gridsize)+"-t"+str(time)+".png")
   imagenameeps="{}".format(folder+"image-Time-Conv-n"+str(gridsize)+"-t"+str(time)+".eps")
        
   fig=plt.figure(1,figsize=[8,7])
   for i in range(len(y)):
      plt.plot(x,y[i],label=str(labels[i]))
   #plt.title("Vm")
   plt.xlabel('Position along fiber, $s$',{'fontsize':20})
   plt.xticks(size=18)
   plt.ylabel('Transmembrane potential, $V_m$',{'fontsize':20})
   plt.yticks(size=18)
   plt.legend()
   #plt.show()    
   fig.savefig(imagenamepng)
   print "Figure saved to {}".format(imagenamepng)
   os.system("convert "+imagenamepng+" "+imagenameeps)
###############################################################################
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
###############################################################################
def makeplot_TimeConv_err(x,y,labels):

  global folder
  imagenamepng="{}".format(folder+"errL2-Time-Conv-n"+str(gridsize)+"-t"+str(time)+".png")
  imagenameeps="{}".format(folder+"errL2-Time-Conv-n"+str(gridsize)+"-t"+str(time)+".eps")
  fig=plt.figure(2,figsize=[8,7])

  symbols=['ro','bo']
  sl=[1,2]
  sl_labels=["slope 1","slope 2"]
  sl_lines=['r:','b:']  
  
  #intercepts=[1,2.35]

  for i in range(len(y)):
    print "i=",i,", x=",x,"y[i]=",y[i]
    #plt.loglog(x,y[i],'ro',label=labels[i])
    plt.plot(x,y[i],'ro',label=labels[i])
    plt.xscale('log')
    plt.yscale('log')
    slope, intercept, r_value, p_value, std_err = stats.linregress(np.log10(x),np.log10(y[i]))
    print "i, slope: ", i,slope
    plt.title("slope="+str(slope))
    #abline_values=[pow(10,sl[i]*j+intercepts[i]) for j  in np.log10(x[i])]  
    abline_values=[pow(10,slope*j+intercept) for j  in np.log10(x)]
    plt.loglog(x,abline_values,sl_lines[i],label=sl_labels[i],linewidth=3)

  #plt.title("Vm")
  plt.xlabel('Time step, $dt$',{'fontsize':20})
  plt.xticks(size=18)
  plt.xlim(right=0.1)
  plt.ylabel('Relative L2-norm error of $V_m$',{'fontsize':20})
  plt.yticks(size=18)
  plt.legend(loc='upper right')
  #plt.show()    
  fig.savefig(imagenamepng)
  print "Figure saved to {}".format(imagenamepng)
  os.system("convert "+imagenamepng+" "+imagenameeps)
###############################################################################

x,Vm,timesteps=get_data_TimeConv(time)
makeplot_TimeConv(x,Vm,timesteps)

err_L2=[0.0 for i in range(1)]
err_L2[0]=find_err_L2_rel(Vm)

labels=[None for i in range(1)]
labels[0]="1st order"

num_runs=len(timesteps)
makeplot_TimeConv_err(timesteps[1:num_runs],err_L2,labels)




