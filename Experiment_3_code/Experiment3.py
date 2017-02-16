from __future__ import division  # so that 1/3=0.333 instead of 1/3=0
from psychopy import visual, core, event, data, gui, sound
import time, random, os
import numpy
import scipy.io as scio
import sys, os, codecs, Image
from subprocess import call

sys.path.append(os.getcwd())

from Gabors import *

#TODO: in instructions, show an image of a face, masked face, noface, masked noface.
#TODO: create a set of practice trials.

#--------------- load the intro data -----------------------
content = scio.loadmat('Resources/target_data.mat')
target_weights = content['weights']
target_images = content['target']
content = 0

filters = gabor_bank(50, [3,6,11],[0,45,90,135],[0,90])

instr = codecs.open('Resources/instr1.txt','r', encoding='utf-8')
instr = instr.read()
instr = instr.split(u'~')

targtime = np.inf
num_prac = 40
num_exp = 2000
noisemean = 0
noisestd = 1
prac_targ_str = [0.2,0.08]

#---------------- Collect Subject Data ------------------------
getData = gui.Dlg(title = "Data Entry Window")
getData.addText("Emotion Sensitivity")
getData.addField("Participant Number:")
getData.addField("Session Number:")
getData.addField("Age:")
getData.addField("Gender:", choices=("Male", "Female", "Other"))
getData.show()
if getData.OK:
	subnum = getData.data[0]
	session = getData.data[1]
	age = getData.data[2]
	gen = getData.data[3]
else:
    core.quit()
    

condition = int(subnum) % 4

target_name = {
	1:'x',
	2:'+',
	3:'xi',
	0:'+i'
	}[condition]

#---------------- Make the data folder ------------------------
try:
	os.mkdir('data/')
except OSError as err:
    print("OS Warning: {0}. Storing Data in that folder.".format(err))

os.chdir(os.path.join(os.path.abspath(os.path.curdir),u'data/'))

#---------------- Create the save file ------------------------
fileName = subnum + '_' + session
dataFile = open(fileName + '_RC3.csv', 'a')
dataFile.writelines('Subject,Target name,Age,Sex,Signal strength,Session,Experiment Stage,Trial,Answer,Response,RT\n')


#---------------- Define the experiment and other useful functions -----------------------
def runexp(win, subnum,condition, gen, age, targtime, filters, weights, answers,target_images, phase = 0, other_vars = []):
     
    trialnum = 1
    stim = visual.ImageStim(win)
    rt = core.Clock()
    
    target_name = {
        1:'x',
        2:'+',
        3:'xi',
        0:'+i'
        }[condition]
    
    image = gabor_image(weights[:,0], filters)
    #-------- Now let's define what a trial will look like ----   
    while trialnum <= weights.shape[1]:
         
        response = 'NA'
        time = 'NA'
        
        if phase!=0 and answers[trialnum-1]==1:
            
            image = gabor_image(weights[:,trialnum-1], filters) + (target_images[:,:,condition]-np.mean(target_images[:,:,condition]))*other_vars[0]
            
            image[image>1]=1
            image[image<0]=0
            
        
        else:
            image = gabor_image(weights[:,trialnum-1], filters)
        
        im = Image.fromarray(numpy.uint8(image*255))
        stim.image = im
        stim.draw()
        win.flip()
        
        rt.reset()
        win.clearBuffer()
        
        time = rt.getTime()
        
        resp = event.waitKeys(keyList = ['z','slash', 'escape'], maxWait = targtime-time)
        
    	if resp:
			time = rt.getTime()										   # if the space bar was pressed, get the RT and draw the blank screen once the targtime runs out
			time = float(time)
			win.flip()
			
			if resp[0] == 'escape':                    #check if someone wants to quit the program (escape key)		
				stop = True
				return stop
				
    	else:
    		win.flip()
        	resp = event.waitKeys(keyList = ['z','slash', 'escape'])				  #wait until a response
        	time = rt.getTime()										  # if the space bar was pressed, get the RT and draw the blank screen once the targtime runs out
        	time = float(time)
        	
        	if resp[0] == 'escape':                     #check if someone wants to quit the program (escape key)
        	    stop = True
        	    return stop
        
    	response = {
			'z':0,
			'slash':1
			}[resp[0]]
				
		
        dataFile.writelines(str(subnum) + ',' + target_name + ',' + str(age) + ',' + str(gen))
        
        for i in range(0,len(other_vars)):
            dataFile.writelines(',' + str(other_vars[i]))
        
        dataFile.writelines(',' + str(phase) + ',' + str(trialnum) +  ',' + str(answers[trialnum-1]) + ',' + str(response) + ',' +  str(time) + ',' + '\n')
         
        trialnum += 1
         
         
def show_window(screen, hardstop = False):
    for i in range(0,len(screen)):
        
        screen[i].draw()
     
    win.flip()
    
    if hardstop:
        wait = event.waitKeys(keyList = ['return', 'escape'])
    else:
        wait = event.waitKeys(keyList = ['space', 'escape'])
    
    if wait[0] == 'escape':
        return True
    else:
        return False



#---------- Define Instructions and other constants -----------

win = visual.Window(size = (2560,1440), fullscr=True, screen=0, monitor='testMonitor', color='gray', units = 'cm') 
win.setMouseVisible(False)

easy_ex = (gabor_image(numpy.random.normal(noisemean,noisestd,1328), filters) + (target_images[:,:,condition]-np.mean(target_images[:,:,condition]))*prac_targ_str[0])
easy_ex[easy_ex < 0]=0
easy_ex[easy_ex > 1]=1

hard_ex = (gabor_image(numpy.random.normal(noisemean,noisestd,1328), filters) + (target_images[:,:,condition]-np.mean(target_images[:,:,condition]))*prac_targ_str[1])
hard_ex[hard_ex < 0]=0
hard_ex[hard_ex > 1]=1

#Define Instruction Screens
loading = visual.TextStim(win, text = 'Loading Stimuli...', wrapWidth = 30, color = 'black')
begin = [visual.TextStim(win, text = 'Please wait for instructions from the researcher...', wrapWidth = 30, color='black')]
inst1 = [visual.TextStim(win, text = instr[0], wrapWidth = 30, pos=(0,-0.5), color='black')]
inst2 = [visual.TextStim(win, text = instr[1], wrapWidth = 30, pos=(0,6), color='black'),
    visual.ImageStim(win, image = Image.fromarray(numpy.uint8(target_images[:,:,condition]*255)),pos = (-10,-1.5)),
    visual.ImageStim(win, image = Image.fromarray(numpy.uint8(easy_ex*255)),pos = (0,-1.5)),
    visual.ImageStim(win, image = Image.fromarray(numpy.uint8(hard_ex*255)),pos = (10,-1.5)),
    visual.TextStim(win, text = instr[2], wrapWidth = 30.5, pos=(0,-10), color='black')]
inst3 = [visual.TextStim(win, text = instr[3], wrapWidth = 30, pos=(0,0), color='black')]
inst4 = [visual.TextStim(win, text = instr[4], wrapWidth = 30, pos=(0,0), color='black')]
inst5 = [visual.TextStim(win, text = instr[5], wrapWidth = 30, pos=(0,0), color='black')]
end = visual.TextStim(win, text = 'The experiment is now complete. Please go find the researcher', wrapWidth = 30, pos=(0,0), color='black')

 #---------------- Begin the experiment ---------------------
stop = False

loading.draw()
win.flip()
win.clearBuffer()

#let's load the stimuli here, so that the experiment feels more seamless
prac_weights = numpy.zeros((target_weights.shape[0], num_prac))


easy_prac_weights = prac_weights + numpy.random.normal(noisemean, noisestd, (target_weights.shape[0],num_prac))
hard_prac_weights = prac_weights + numpy.random.normal(noisemean, noisestd, (target_weights.shape[0],num_prac))
exp_weights = np.zeros((target_weights.shape[0], num_exp)) + numpy.random.normal(noisemean, noisestd, (target_weights.shape[0],num_exp))

#now let's shuffle the stimulus order a little
easy_prac_order = numpy.random.permutation(easy_prac_weights.shape[1])-1
hard_prac_order = numpy.random.permutation(hard_prac_weights.shape[1])-1
exp_order = numpy.random.permutation(exp_weights.shape[1])-1

easy_prac_weights = easy_prac_weights[:,easy_prac_order]
easy_prac_answers = numpy.append(numpy.ones(num_prac/2, dtype=int), numpy.zeros(num_prac/2, dtype=int))[easy_prac_order]

hard_prac_weights = hard_prac_weights[:,hard_prac_order]
hard_prac_answers = numpy.append(numpy.ones(num_prac/2, dtype=int), numpy.zeros(num_prac/2, dtype=int))[hard_prac_order]

exp_weights = exp_weights[:,exp_order]
exp_answers = numpy.repeat(['NA'],num_exp)


#--------------- Okay for real this time - let's begin the experiment ----------------
stop = show_window(begin)

if stop:
    win.close()
    core.quit()

stop = show_window(inst1)

if stop:
    win.close()
    core.quit()
    

stop = show_window(inst2)

if stop:
    win.close()
    core.quit()
    

stop = show_window(inst3)

if stop:
    win.close()
    core.quit()
    


################## EASY PRACTICE TRIALS #################
phase = 1
 
stop = runexp(win, subnum, condition, gen, age, targtime,filters, easy_prac_weights, easy_prac_answers,target_images, phase, other_vars=[prac_targ_str[phase-1], session])
 
if stop:
    win.close()
    core.quit()
 
 
stop = show_window(inst4)
 
if stop:
    win.close()
    core.quit()
    
    

################## HARD PRACTICE TRIALS #################
phase = 2
 
stop = runexp(win, subnum, condition, gen, age, targtime,filters, hard_prac_weights, hard_prac_answers,target_images, phase, other_vars=[prac_targ_str[phase-1], session])
 
if stop:
    win.close()
    core.quit()
 
 
stop = show_window(inst5, hardstop=True)
 
if stop:
    win.close()
    core.quit()
    



############# EXPERIMENTAL TRIALS #################
phase = 0

stop = runexp(win, subnum, condition, gen, age, targtime,filters, exp_weights, exp_answers,target_images,phase, other_vars=['NA', session])

if stop:
    win.close()
    core.quit()


dataFile.close()
end.draw()
win.flip()
call("../Resources/Transfer_data.command")
time.sleep(10)
event.waitKeys(keyList=['escape'])
core.quit()