#!/bin/csh

set nonomatch

foreach subject ('rrl01' 'rrl03' 'rrl04' 'rrl05' 'rrl06' 'rrl07' 'rrl11' 'rrl12' 'rrl13' 'rrl14' 'rrl15' 'rrl16' 'rrl17' 'rrl18' 'rrl19' 'rrl20' 'rrl22' 'rrl23')
        foreach ses ('s1' 's2')
                cd '/cylon/afniproc/rrl/'$subject'/'$ses'/fmri/drl'

		set anatfile = anat
		set masks = ( csf wm dlpfc caudate acing ins nacc8mm mpfc )
		set regfiles = ( drl_mbnf )

		foreach regfile (${regfiles})

			foreach maskname ( ${masks} )
				if ( -e ${regfile}_${maskname}+orig.HEAD ) then
					rm ${regfile}_${maskname}+orig.*
				endif
			end

			3dfractionize -template ${regfile}+orig -input /cylon/afniproc/rrl/masks_2/csf_mask+tlrc -warp ${anatfile}+tlrc -clip 0.1 -preserve -prefix ${regfile}_csf
			3dfractionize -template ${regfile}+orig -input /cylon/afniproc/rrl/masks_2/wm_mask+tlrc -warp ${anatfile}+tlrc -clip 0.1 -preserve -prefix ${regfile}_wm
			3dfractionize -template ${regfile}+orig -input /cylon/afniproc/rrl/masks_2/dlpfc+tlrc -warp ${anatfile}+tlrc -clip 0.1 -preserve -prefix ${regfile}_dlpfc
			3dfractionize -template ${regfile}+orig -input /cylon/afniproc/rrl/masks_2/caudate+tlrc -warp ${anatfile}+tlrc -clip 0.1 -preserve -prefix ${regfile}_caudate
			3dfractionize -template ${regfile}+orig -input /cylon/afniproc/rrl/masks_2/acing+tlrc -warp ${anatfile}+tlrc -clip 0.1 -preserve -prefix ${regfile}_acing
			3dfractionize -template ${regfile}+orig -input /cylon/afniproc/rrl/masks_2/ins+tlrc -warp ${anatfile}+tlrc -clip 0.1 -preserve -prefix ${regfile}_ins
			3dfractionize -template ${regfile}+orig -input /cylon/afniproc/rrl/masks_2/nacc8mm+tlrc -warp ${anatfile}+tlrc -clip 0.1 -preserve -prefix ${regfile}_nacc8mm
			3dfractionize -template ${regfile}+orig -input /cylon/afniproc/rrl/masks_2/mpfc+tlrc -warp ${anatfile}+tlrc -clip 0.1 -preserve -prefix ${regfile}_mpfc


			foreach mask ( ${masks} )

				if ( -e l${mask}.tc ) then
					rm l${regfile}_${mask}_$ses.1D
				endif
				if ( -e r${mask}.tc ) then
					rm r${regfile}_${mask}_$ses.1D
				endif
				if ( -e b${mask}.tc ) then
					rm b${regfile}_${mask}_$ses.1D
				endif

				3dmaskave -mask ${regfile}_${mask}+orig -quiet -mrange 1 1 ${regfile}+orig > l${regfile}_${mask}_$ses.1D
                        	3dmaskave -mask ${regfile}_${mask}+orig -quiet -mrange 2 2 ${regfile}+orig > r${regfile}_${mask}_$ses.1D
                        	3dmaskave -mask ${regfile}_${mask}+orig -quiet -mrange 1 2 ${regfile}+orig > b${regfile}_${mask}_$ses.1D

			end

		end

	end
end
