#!/bin/bash

for file  in `ls '/cylon/afniproc/rrl' | grep rrl`; do

	cd '/cylon/afniproc/rrl/'$file'/s1/fmri/drl'

	#convert anatomical scan to brick and header
	if [ -e anat+orig.HEAD ] ; then rm -rf anat+orig* ; fi

	3dcopy $file'_s1_t1.nii.gz' anat

	#convert files to ANFI, cut off leadin/leadout
	if [ -e epi1+orig.HEAD ] ; then rm -rf epi?+orig* ; fi

	#cut the first 6 TRs (12 s) and the final 4 TRs (8 s)
	#291 total TRs
	#index 0
	#start 6
	#end 287
	3dTcat -prefix drl1 'drl_s1_1+orig[6..286]'
	3dTcat -prefix drl2 'drl_s1_2+orig[6..286]'

	#refitting + slice time correction
	3drefit -TR 2.0 drl1+orig.
	3drefit -TR 2.0 drl2+orig.
	if [ -e drlts1+orig.HEAD ] ; then rm -rf drlts?+orig* ; fi

	3dTshift -slice 0 -tpattern altplus -prefix drlts1 drl1+orig.
	3dTshift -slice 0 -tpattern altplus -prefix drlts2 drl2+orig.

	rm -rf drl?+orig*

	#pre-clean 
	if [ -e drl+orig.HEAD ] ; then rm -rf drl+orig* ; fi

	#create concatenated dataset
	3dTcat -prefix drl drlts1+orig drlts2+orig
	rm -rf drlts?+orig*

	#correct for motion
	if [ -e drl_m+orig.HEAD ] ; then rm -rf drl_m+orig* ; fi 

	if [ -e 3dmotiondrl.1D ] ; then rm -rf 3dmotiondrl.1D ; fi

	3dvolreg -Fourier -twopass -prefix drl_m -base 3 -dfile 3dmotiondrl.1D drl+orig

	#smooth spatially
	if [ -e drl_mb+orig.HEAD ] ; then rm -rf drl_mb+orig* ; fi

	3dmerge -prefix drl_mb -1blur_fwhm 4 -doall drl_m+orig

	#normalize (calculate pct signal change / average) and filter
	if [ -e drl_mbn+orig.BRIK ] ; then rm -rf drl_mbn+orig* ; fi

	if [ -e drl_ave+orig.BRIK ] ; then rm -rf drl_ave+* ; fi

	3dTstat -prefix drl_ave 'drl_mb+orig[0..561]'
	3drefit -abuc drl_ave+orig
	3dcalc -datum float -a 'drl_mb+orig[0..561]' -b drl_ave+orig -expr "((a-b)/b)*100" -prefix drl_mbn

	if [ -e drl_mbnf+orig.HEAD ] ; then rm -rf drl_mbnf+orig.* ; fi

	3dTproject -prefix drl_mbnf -stopband 0 .011 -input drl_mbn+orig

	#talairach warping
	#preclean directory
	if [ -e anat+tlrc.HEAD ] ; then rm -rf anat+tlrc.* ; fi

	@auto_tlrc -warp_orig_vol -suffix NONE -base '/cylon/afniproc/rrl/masks/TT_N27+tlrc.' -input anat+orig.
	@auto_tlrc -warp_orig_vol -suffix mni -base '/cylon/afniproc/rrl/masks/mni_ns.nii.gz' -input anat+orig.

	#set the epi parent to the auto-warped anat
	3drefit -apar anat+orig drl_mbnf+orig

	cd '/cylon/afniproc/rrl/scripts'

done
