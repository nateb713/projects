#!/bin/bash

for sub in `ls /mnt/acorn/projects/rrl | grep rrl`; do
	echo $sub

	#cp rm '/cylon/afniproc/rrl/'$sub'/s1/fmri/mid/mid_mbnf+orig.BRIK'
	#cp rm '/cylon/afniproc/rrl/'$sub'/s1/fmri/mid/mid_mbnf+orig.HEAD'
	#cp rm '/cylon/afniproc/rrl/'$sub'/s2/fmri/mid/mid_mbnf+orig.BRIK'
	#cp rm '/cylon/afniproc/rrl/'$sub'/s2/fmri/mid/mid_mbnf+orig.HEAD'

	#mkdir '/cylon/afniproc/rrl/'$sub '/cylon/afniproc/rrl/'$sub'/s1' '/cylon/afniproc/rrl/'$sub'/s2'
	#mkdir '/cylon/afniproc/rrl/'$sub'/s1/mid' '/cylon/afniproc/rrl/'$sub'/s1/mil' '/cylon/afniproc/rrl/'$sub'/s1/drl'
	#mkdir '/cylon/afniproc/rrl/'$sub'/s2/mid' '/cylon/afniproc/rrl/'$sub'/s2/mil' '/cylon/afniproc/rrl/'$sub'/s2/drl'

	#copy mid_matrix.csv, afni mid task files runs 1 and 2, anatomy files
	#copy t1 files into fmri folder for s1 and s2
	#copy Brik and Header files, 4 each, for each subject (mid_s2_1+orig.BRIK, mid_s2_1+orig.HEAD, mid_s2_2+orig.BRIK, mid_s2_2+orig.HEAD)
	#look at mid_preprocess script file and adapt it to the data on local computer
	#also read comments in code to try to understand fmri preprocessing
	#remove subjects with incomplete data (Josiah listed relevant subjects in other scripts folder)
 
	#cp '/mnt/acorn/projects/rrl/'$sub'/s1/anat/'$sub'_s1_t1_1.nii.gz' '/cylon/afniproc/rrl/'$sub'/s1/anat/.'
        #cp '/mnt/acorn/projects/rrl/'$sub'/s2/anat/'$sub'_s1_t1_2.nii.gz' '/cylon/afniproc/rrl/'$sub'/s1/anat/.'
	#cp '/mnt/acorn/projects/rrl/'$sub'/s2/anat/'$sub'_s2_t1_1.nii.gz' '/cylon/afniproc/rrl/'$sub'/s2/anat/.'
	#cp '/mnt/acorn/projects/rrl/'$sub'/s2/anat/'$sub'_s2_t1_2.nii.gz' '/cylon/afniproc/rrl/'$sub'/s2/anat/.'

        #cp '/mnt/acorn/projects/rrl/'$sub'/s1/fmri/mid/mid_matrix.csv' '/cylon/afniproc/rrl/'$sub'/s1/fmri/mid/.'
        #cp '/mnt/acorn/projects/rrl/'$sub'/s2/fmri/mid/mid_matrix.csv' '/cylon/afniproc/rrl/'$sub'/s2/fmri/mid/.'

	#cp '/mnt/acorn/projects/rrl/'$sub'/s1/fmri/mid/mid_mbnf+orig.BRIK' '/cylon/afniproc/rrl/'$sub'/s1/fmri/mid/.'
        #cp '/mnt/acorn/projects/rrl/'$sub'/s1/fmri/mid/mid_mbnf+orig.HEAD' '/cylon/afniproc/rrl/'$sub'/s1/fmri/mid/.'
        #cp '/mnt/acorn/projects/rrl/'$sub'/s2/fmri/mid/mid_mbnf+orig.BRIK' '/cylon/afniproc/rrl/'$sub'/s2/fmri/mid/.'
        #cp '/mnt/acorn/projects/rrl/'$sub'/s2/fmri/mid/mid_mbnf+orig.HEAD' '/cylon/afniproc/rrl/'$sub'/s2/fmri/mid/.'

        #cp '/mnt/acorn/projects/rrl/'$sub'/s1/anat/'$sub'_s1_t1.nii.gz' '/cylon/afniproc/rrl/'$sub'/s1/fmri/.'
        #cp '/mnt/acorn/projects/rrl/'$sub'/s2/anat/'$sub'_s2_t1.nii.gz' '/cylon/afniproc/rrl/'$sub'/s2/fmri/.'

        #cp '/mnt/acorn/projects/rrl/'$sub'/s1/fmri/mil/mil_s1_1+orig.BRIK' '/cylon/afniproc/rrl/'$sub'/s1/fmri/mil/.'
        #cp '/mnt/acorn/projects/rrl/'$sub'/s1/fmri/mil/mil_s1_1+orig.HEAD' '/cylon/afniproc/rrl/'$sub'/s1/fmri/mil/.'
        #cp '/mnt/acorn/projects/rrl/'$sub'/s1/fmri/mil/mil_s1_2+orig.BRIK' '/cylon/afniproc/rrl/'$sub'/s1/fmri/mil/.'
        #cp '/mnt/acorn/projects/rrl/'$sub'/s1/fmri/mil/mil_s1_2+orig.HEAD' '/cylon/afniproc/rrl/'$sub'/s1/fmri/mil/.'
        #cp '/mnt/acorn/projects/rrl/'$sub'/s2/fmri/mil/mil_s2_1+orig.BRIK' '/cylon/afniproc/rrl/'$sub'/s2/fmri/mil/.'
        #cp '/mnt/acorn/projects/rrl/'$sub'/s2/fmri/mil/mil_s2_1+orig.HEAD' '/cylon/afniproc/rrl/'$sub'/s2/fmri/mil/.'
        #cp '/mnt/acorn/projects/rrl/'$sub'/s2/fmri/mil/mil_s2_2+orig.BRIK' '/cylon/afniproc/rrl/'$sub'/s2/fmri/mil/.'
        #cp '/mnt/acorn/projects/rrl/'$sub'/s2/fmri/mil/mil_s2_2+orig.HEAD' '/cylon/afniproc/rrl/'$sub'/s2/fmri/mil/.'

	#cp '/mnt/acorn/projects/rrl/'$sub'/s1/fmri/drl/drl_s1_1+orig.BRIK' '/cylon/afniproc/rrl/'$sub'/s1/fmri/drl/.'
        #cp '/mnt/acorn/projects/rrl/'$sub'/s1/fmri/drl/drl_s1_1+orig.HEAD' '/cylon/afniproc/rrl/'$sub'/s1/fmri/drl/.'
        #cp '/mnt/acorn/projects/rrl/'$sub'/s1/fmri/drl/drl_s1_2+orig.BRIK' '/cylon/afniproc/rrl/'$sub'/s1/fmri/drl/.'
        #cp '/mnt/acorn/projects/rrl/'$sub'/s1/fmri/drl/drl_s1_2+orig.HEAD' '/cylon/afniproc/rrl/'$sub'/s1/fmri/drl/.'
        #cp '/mnt/acorn/projects/rrl/'$sub'/s2/fmri/drl/drl_s2_1+orig.BRIK' '/cylon/afniproc/rrl/'$sub'/s2/fmri/drl/.'
        #cp '/mnt/acorn/projects/rrl/'$sub'/s2/fmri/drl/drl_s2_1+orig.HEAD' '/cylon/afniproc/rrl/'$sub'/s2/fmri/drl/.'
        #cp '/mnt/acorn/projects/rrl/'$sub'/s2/fmri/drl/drl_s2_2+orig.BRIK' '/cylon/afniproc/rrl/'$sub'/s2/fmri/drl/.'
        #cp '/mnt/acorn/projects/rrl/'$sub'/s2/fmri/drl/drl_s2_2+orig.HEAD' '/cylon/afniproc/rrl/'$sub'/s2/fmri/drl/.'

	#cp '/mnt/acorn/projects/rrl/'$sub'/s1/fmri/mid/anat+orig.HEAD' '/cylon/afniproc/rrl/'$sub'/s1/fmri/mid/.'
	#cp '/mnt/acorn/projects/rrl/'$sub'/s1/fmri/mid/anat+orig.BRIK' '/cylon/afniproc/rrl/'$sub'/s1/fmri/mid/.'
	#cp '/mnt/acorn/projects/rrl/'$sub'/s2/fmri/mid/anat+orig.HEAD' '/cylon/afniproc/rrl/'$sub'/s2/fmri/mid/.'
	#cp '/mnt/acorn/projects/rrl/'$sub'/s2/fmri/mid/anat+orig.BRIK' '/cylon/afniproc/rrl/'$sub'/s2/fmri/mid/.'

        #cp '/mnt/acorn/projects/rrl/'$sub'/s1/fmri/drl/'$sub'_s1_t1.nii.gz' '/cylon/afniproc/rrl/'$sub'/s1/fmri/drl/.'
        #cp '/mnt/acorn/projects/rrl/'$sub'/s2/fmri/drl/'$sub'_s2_t1.nii.gz' '/cylon/afniproc/rrl/'$sub'/s2/fmri/drl/.'
        cp '/mnt/acorn/projects/rrl/'$sub'/s1/fmri/mil/'$sub'_s1_t1.nii.gz' '/cylon/afniproc/rrl/'$sub'/s1/fmri/mil/.'
        #cp '/mnt/acorn/projects/rrl/'$sub'/s2/fmri/mil/'$sub'_s2_t1.nii.gz' '/cylon/afniproc/rrl/'$sub'/s2/fmri/mil/.'



done
