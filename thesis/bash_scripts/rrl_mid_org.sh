#!/bin/bash

# subject list

# for loop
for sub in rrl01 rrl03 rrl04 rrl05 rrl06 rrl07 rrl08 rrl09 rrl11 rrl12 rrl13 rrl14 rrl15 rrl16 rrl17 rrl18 rrl19 rrl20 rrl22 rrl23
do
	echo $sub
	#create filepath to subject raw folder
	serverdir='/mnt/acorn/projects/rrl/'$sub'/s1/fmri/mid'
	localdir='/cylon/afniproc/rrl/'$sub
	mkdir $localdir
	cd $serverdir
	ls | grep matrix
	cd /cylon/afniproc/rrl/scripts
done

