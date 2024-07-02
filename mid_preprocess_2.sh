#!/bin/bash

for file  in `cat '/cylon/afniproc/rrl/mid.txt'`; do


        cd '/cylon/afniproc/rrl/'$file'/s2/fmri/mid'

        #convert anatomical scan to brick and header
        if [ -e anat+orig.HEAD ] ; then rm -rf anat+orig* ; fi


        3dcopy $file'_s2_t1.nii.gz' anat


        #cut the first 6 TRs (12 s) and the final 4 TRs (8 s)
        #291 total TRs
        #index 0
        #start 6
        #end 287
        3dTcat -prefix mid_1 'mid_s2_1+orig[6..286]'
        3dTcat -prefix mid_2 'mid_s2_2+orig[6..286]'

        #refitting + slice time correction
        3drefit -TR 2.0 mid_1+orig.
        3drefit -TR 2.0 mid_2+orig.
        if [ -e midts1+orig.HEAD ] ; then rm -rf midts?+orig* ; fi

        3dTshift -slice 0 -tpattern altplus -prefix midts1 mid_1+orig.
        3dTshift -slice 0 -tpattern altplus -prefix midts2 mid_2+orig.

        rm -rf mid?+orig*

        #pre-clean 
        if [ -e mid+orig.HEAD ] ; then rm -rf mid+orig* ; fi

        #create concatenated dataset
        3dTcat -prefix mid midts1+orig midts2+orig
        rm -rf midts?+orig*

        #correct for motion
        if [ -e mid_m+orig.HEAD ] ; then rm -rf mid_m+orig* ; fi

        if [ -e 3dmotionmid.1D ] ; then rm -rf 3dmotionmid.1D ; fi

        3dvolreg -Fourier -twopass -prefix mid_m -base 3 -dfile 3dmotionmid.1D mid+orig

        #smooth spatially
        if [ -e mid_mb+orig.HEAD ] ; then rm -rf mid_mb+orig* ; fi

        3dmerge -prefix mid_mb -1blur_fwhm 4 -doall mid_m+orig

        #normalize (calculate pct signal change / average) and filter
        if [ -e mid_mbn+orig.BRIK ] ; then rm -rf mid_mbn+orig* ; fi

        if [ -e mid_ave+orig.BRIK ] ; then rm -rf mid_ave+* ; fi

        3dTstat -prefix mid_ave 'mid_mb+orig[0..561]'
        3drefit -abuc mid_ave+orig
        3dcalc -datum float -a 'mid_mb+orig[0..561]' -b mid_ave+orig -expr "((a-b)/b)*100" -prfix mid_mbn

        if [ -e mid_mbnf+orig.HEAD ] ; then rm -rf mid_mbnf+orig.* ; fi

        3dTproject -prefix mid_mbnf -stopband 0 .011 -input mid_mbn+orig

        #talairach warping
        #preclean directory
        if [ -e anat+tlrc.HEAD ] ; then rm -rf anat+tlrc.* ; fi


        @auto_tlrc -warp_orig_vol -suffix NONE -base '/cylon/afniproc/rrl/masks/TT_N27+tlrc.' -input anat+orig.
        @auto_tlrc -warp_orig_vol -suffix mni -base '/cylon/afniproc/rrl/masks/mni_ns.nii.gz' -input anat+orig.

        #set the epi parent to the auto-warped anat
        3drefit -apar anat+orig mid_mbnf+orig

        cd '/cylon/afniproc/rrl/scripts'

done

