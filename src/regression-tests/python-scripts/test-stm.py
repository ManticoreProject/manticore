#!/usr/bin/env python
import sys, getopt, re, os, signal
import subprocess
import argparse, pdb
from time import gmtime, strftime
from email.mime.text import MIMEText
import smtplib
from time import sleep
import random

parser = argparse.ArgumentParser()

parser.add_argument("-program", type=str, help="Run a specific benchmark (runs all benchmarks if not specified)")
parser.add_argument("-iters", type=int, help="Number of iterations for each STM implementation", default="50")
parser.add_argument("-stm", type=str, help="Which STM implementation to use (all by default)", default="all")
parser.add_argument("-threads", type=int, help="Number of threads to use", default=4)
parser.add_argument('-rebuild', dest='rebuild', action='store_true')

args = parser.parse_args()

#stms = ["ffnorec", "norec", "orderedNoRec", "pnorec", "bounded", "full", "partial", "ordered", "tailff", "ff", "ffRefCount", "ffRefCountGC"]

if args.stm == "all":
	fileDir = os.path.dirname(os.path.realpath(__file__))
	os.chdir(fileDir)
	os.chdir("../../lib/basis/stm")
	subprocess.Popen('pmlc -o print_stms print-stms.pml; ./a.out > stms.txt', shell=True).wait()

	stms = open('stms.txt').read().split('\n')
	stms.remove('')

	os.chdir(fileDir)
else:
	stms = [args.stm]

benchmarks = ["counter.pml", "multipleTVars.pml", "ordered-linked-list.pml", "red-black.mlb"]

globalStats = {}

def mkMessage():
	global globalStats
	result = ''
	for prog, stats in globalStats.items():
		if stats[0] > 0:
			result = result + 'Regression test \"' + prog[0] + '\" with \"' + prog[1] + '\" stm failed ' + str(stats[0]) + ' times\n'
			result = result + '-------------Error Dump------------\n'
			result = result + stats[1] + '\n\n'
	return result

def sendErrorEmail():
	msg = MIMEText(mkMessage())
	msg['Subject'] = 'Failed Regression Tests!'
	msg['From'] = 'ml9951email@gmail.com'
	msg['To'] = 'ml9951@g.rit.edu'

	try:
		server = smtplib.SMTP('smtp.gmail.com',587) #port 465 or 587
		server.ehlo()
		server.starttls()
		server.ehlo()
		server.login('ml9951email','ml9951EmailPassword')
		res = server.sendmail('ml9951email@gmail.com', ['ml9951@g.rit.edu'],msg.as_string())
		server.close()
	except:
		print('Exception raised when trying to send email!')

def runSTM(stm, program):
	errorCount = 0
	errorDump = ''
	print('STM: ' + stm)
	for i in range(args.iters):
		sleep(random.uniform(0.0, 0.05))
		sys.stdout.write(str(i) + ', ')
		sys.stdout.flush()
		try:
			res1 = subprocess.Popen('./a.out -stm ' + stm + ' -p ' + str(args.threads) + ' > currentTime.txt', shell = True).wait()
			while res1 != 0:
				errorDump = errorDump + 'Error ' + str(errorCount) + ': \n' + open('currentTime.txt').read() + '\n\n'
				errorCount = errorCount + 1
				print('execution finished with return code: ' + str(res1))
				res1 = subprocess.Popen('./a.out -stm ' + stm + ' -p ' + str(args.threads) + ' > currentTime.txt', shell = True).wait()
		except Exception as e:
			print("Exception raised: " + str(e))
			return
	if errorCount == 0:
        	print(stm + ' passed!')
	globalStats[(program, stm)] = (errorCount, errorDump)

def benchmark(program):
	os.chdir('../goals/par-stm/')
	if args.rebuild:
		subprocess.Popen('pmlc ' + program, shell=True).wait()
	if args.stm == "all":
		for stm in stms:
			runSTM(stm, program)
	else:
		runSTM(args.stm, program)

def runBenchmarks():
	originalDirectory = os.path.dirname(os.path.realpath(__file__))
	os.chdir(originalDirectory)
	if args.program is None:
		for bench in benchmarks:
			print('Running ' + bench)
			benchmark(bench)
			os.chdir(originalDirectory)
	else:
		benchmark(args.program)
		os.chdir(originalDirectory)
    
def main():
	runBenchmarks()
	global globalStats
	for prog, stats in globalStats.items():
		if stats[0] > 0:
			sendErrorEmail()
			break


if __name__ == "__main__":
    main()    







