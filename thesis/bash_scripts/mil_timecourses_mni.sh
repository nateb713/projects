#!/bin/csh

set nonomatch

foreach subject (`ls '/cylon/afniproc/rrl/ | grep rrl` )
foreach subject ('rrl01' 'rrl03' 'rrl04' 'rrl05' 'rrl06' 'rrl07' 'rrl11' 'rrl12' 'rrl13' 'rrl14' 'rrl15' 'rrl16' 'rrl17' 'rrl18' 'rrl19' 'rrl20' 'rrl22' 'rrl23')
	foreach ses ('s1' 's2')
		cd '/cylon/afniproc/rrl/'$subject'/'$ses'/fmri/mil'
		set anatfile = anatmni
		set masks = ( vta_mni_50 vta_mni_10 nacc8mm_mni ains_mni ains8mm_mni ains8mmkg_mni mpfc8mm_mni )
		set regfiles = ( mil_mbnf )

		foreach regfile (${regfiles})
			foreach maskname ( ${masks} )
				if ( -e ${regfile}_${maskname}+orig.HEAD ) then
					rm ${regfile}_${maskname}+orig.*
				endif
			end

			3dfractionize -template ${regfile}+orig -input /cylon/afniproc/rrl/masks_2/vta_mni_50+tlrc -warp ${anatfile}+tlrc -clip 0.1 -preserve -prefix ${regfile}_vta_mni_50
			3dfractionize -template ${regfile}+orig -input /cylon/afniproc/rrl/masks_2/vta_mni_10+tlrc -warp ${anatfile}+tlrc -clip 0.1 -preserve -prefix ${regfile}_vta_mni_10
			3dfractionize -template ${regfile}+orig -input /cylon/afniproc/rrl/masks_2/nacc8mm_mni+tlrc -warp ${anatfile}+tlrc -clip 0.1 -preserve -prefix ${regfile}_nacc8mm_mni
			3dfractionize -template ${regfile}+orig -input /cylon/afniproc/rrl/masks_2/ains_mni+tlrc -warp ${anatfile}+tlrc -clip 0.1 -preserve -prefix ${regfile}_ains_mni
			3dfractionize -template ${regfile}+orig -input /cylon/afniproc/rrl/masks_2/ains8mm_mni+tlrc -warp ${anatfile}+tlrc -clip 0.1 -preserve -prefix ${regfile}_ains8mm_mni
			3dfractionize -template ${regfile}+orig -input /cylon/afniproc/rrl/masks_2/ains8mmkg_mni+tlrc -warp ${anatfile}+tlrc -clip 0.1 -preserve -prefix ${regfile}_ains8mmkg_mni
			3dfractionize -template ${regfile}+orig -input /cylon/afniproc/rrl/masks_2/mpfc8mm_mni+tlrc -warp ${anatfile}+tlrc -clip 0.1 -preserve -prefix ${regfile}_mpfc8mm_mni

			foreach mask ( ${masks} )

				if ( -e l${mask}.tc ) then
					rm l${regfile}_${mask}.1D
				endif
				if ( -e r${mask}.tc ) then
					rm r${regfile}_${mask}.1D
				endif
				if ( -e b${mask}.tc ) then
					rm b${regfile}_${mask}.1D
				endif

				3dmaskave -mask ${regfile}_${mask}+orig -quiet -mrange 1 1 ${regfile}+orig > l${regfile}_${mask}_$ses.1D
                        	3dmaskave -mask ${regfile}_${mask}+orig -quiet -mrange 2 2 ${regfile}+orig > r${regfile}_${mask}_$ses.1D
                        	3dmaskave -mask ${regfile}_${mask}+orig -quiet -mrange 1 2 ${regfile}+orig > b${regfile}_${mask}_$ses.1D

			end

		end
	end
end
