COptions    = -ansi strict -proto strict -typecheck strict -w error ¶
              -d TARGET_VAR_{Variety} -d TARGET_VM_RM -sym full

":" Ä "::"

OSffx     = .c.{Variety}.o

{OSffx} 	Ä	.c
	MrC {DepDir}{Default}.c -o {TargDir}{Default}{OSffx} {COptions}

mmsw_{Variety}.o Ä assert{OSffx} ring{OSffx} liban{OSffx} mpm{OSffx} ¶
                   arenavm{OSffx} space{OSffx} pool{OSffx} poolmfs{OSffx} ¶
                   poolmv{OSffx} root{OSffx} format{OSffx} buffer{OSffx} ¶
                   lockan{OSffx} ref{OSffx} trace{OSffx} than{OSffx} ¶
                   protan{OSffx} shield{OSffx} mpsi{OSffx} ld{OSffx} ¶
                   vmrm{OSffx} ssan{OSffx}
	PPCLink {Deps} -xm l -o {Targ} -sym on

cleanall Ä
	Delete Å.o

clean Ä
	Delete Å.c.Å.o
	
mmsw Ä
	BuildProgram -d Variety=RO mmsw_RO.o
	BuildProgram -d Variety=DP mmsw_DP.o
	