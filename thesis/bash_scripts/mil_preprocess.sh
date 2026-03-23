#!/bin/bash

for file in `ls '/cylon/afniproc/rrl' | grep rrl` ; do

	cd '/cylon/afniproc/rrl/'$file'/s1/fmri/mil'

	#convert anatomical scan to brick and header
	if [ -e anat+orig.HEAD ] ; then rm -rf anat+orig* ; fi

	3dcopy $file'_s1_t1.nii.gz' anat

	#convert files to ANFI, cut off leadin/leadout
	if [ -e mil1+orig.HEAD ] ; then rm -rf mil?+orig* ; fi

	#cut the first 6 TRs (12 s) and the final 4 TRs (8 s)
	#291 total TRs
	#index 0
	#start 6
	#end 287
	3dTcat -prefix mil1 'mil_s1_1+orig[6..286]'
	3dTcat -prefix mil2 'mil_s1_2+orig[6..286]'

	#refitting + slice time correction
	3drefit -TR 2.0 mil1+orig.
	3drefit -TR 2.0 mil2+orig.
	if [ -e milts1+orig.HEAD ] ; then rm -rf milts?+orig* ; fi

	3dTshift -slice 0 -tpattern altplus -prefix milts1 mil1+orig.
	3dTshift -slice 0 -tpattern altplus -prefix milts2 mil2+orig.

	rm -rf mil?+orig*

	#pre-clean 
	if [ -e mil+orig.HEAD ] ; then rm -rf mil+orig* ; fi

	#create concatenated dataset
	3dTcat -prefix mil milts1+orig milts2+orig
	rm -rf milts?+orig*

	#correct for motion
	if [ -e mil_m+orig.HEAD ] ; then rm -rf mil_m+orig* ; fi

	if [ -e 3dmotionmil.1D ] ; then rm -rf 3dmotionmil.1D ; fi

	3dvolreg -Fourier -twopass -prefix mil_m -base 3 -dfile 3dmotionmil.1D mil+orig

	#smooth spatially
	if [ -e mil_mb+orig.HEAD ] ; then rm -rf mil_mb+orig* ; fi

	3dmerge -prefix mil_mb -1blur_fwhm 4 -doall mil_m+orig

	#normalize (calculate pct signal change / average) and filter
	if [ -e mil_mbn+orig.BRIK ] ; then rm -rf mil_mbn+orig* ; fi

	if [ -e mil_ave+orig.BRIK ] ; then rm -rf mil_ave+* ; fi

	3dTstat -prefix mil_ave 'mil_mb+orig[0..561]'
	3drefit -abuc mil_ave+orig
	3dcalc -datum float -a 'mil_mb+orig[0..561]' -b mil_ave+orig -expr "((a-b)/b)*100" -prefix mil_mbn

	if [ -e mil_mbnf+orig.HEAD ] ; then rm -rf mil_mbnf+orig.* ; fi

	3dTproject -prefix mil_mbnf -stopband 0 .011 -input mil_mbn+orig

	#talairach warping
	#preclean directory
	if [ -e anat+tlrc.HEAD ] ; then rm -rf anat+tlrc.* ; fi

	@auto_tlrc -warp_orig_vol -suffix NONE -base '/cylon/afniproc/rrl/masks/TT_N27+tlrc.' -input anat+orig.
	@auto_tlrc -warp_orig_vol -suffix mni -base '/cylon/afniproc/rrl/masks/mni_ns.nii.gz' -input anat+orig.

	#set the epi parent to the auto-warped anat
	3drefit -apar anat+orig mil_mbnf+orig

	cd '/cylon/afniproc/rrl/scripts'

done
