# encoding: utf-8
# module scipy.linalg.cython_lapack
# from C:\Program Files (x86)\Python37-32\lib\site-packages\scipy\linalg\cython_lapack.cp37-win32.pyd
# by generator 1.147
"""
LAPACK functions for Cython
===========================

Usable from Cython via::

    cimport scipy.linalg.cython_lapack

This module provides Cython-level wrappers for all primary routines included
in LAPACK 3.4.0 except for ``zcgesv`` since its interface is not consistent
from LAPACK 3.4.0 to 3.6.0. It also provides some of the
fixed-api auxiliary routines.

These wrappers do not check for alignment of arrays.
Alignment should be checked before these wrappers are used.

Raw function pointers (Fortran-style pointer arguments):

- cbbcsd
- cbdsqr
- cgbbrd
- cgbcon
- cgbequ
- cgbequb
- cgbrfs
- cgbsv
- cgbsvx
- cgbtf2
- cgbtrf
- cgbtrs
- cgebak
- cgebal
- cgebd2
- cgebrd
- cgecon
- cgeequ
- cgeequb
- cgees
- cgeesx
- cgeev
- cgeevx
- cgehd2
- cgehrd
- cgelq2
- cgelqf
- cgels
- cgelsd
- cgelss
- cgelsy
- cgemqrt
- cgeql2
- cgeqlf
- cgeqp3
- cgeqr2
- cgeqr2p
- cgeqrf
- cgeqrfp
- cgeqrt
- cgeqrt2
- cgeqrt3
- cgerfs
- cgerq2
- cgerqf
- cgesc2
- cgesdd
- cgesv
- cgesvd
- cgesvx
- cgetc2
- cgetf2
- cgetrf
- cgetri
- cgetrs
- cggbak
- cggbal
- cgges
- cggesx
- cggev
- cggevx
- cggglm
- cgghrd
- cgglse
- cggqrf
- cggrqf
- cgtcon
- cgtrfs
- cgtsv
- cgtsvx
- cgttrf
- cgttrs
- cgtts2
- chbev
- chbevd
- chbevx
- chbgst
- chbgv
- chbgvd
- chbgvx
- chbtrd
- checon
- cheequb
- cheev
- cheevd
- cheevr
- cheevx
- chegs2
- chegst
- chegv
- chegvd
- chegvx
- cherfs
- chesv
- chesvx
- cheswapr
- chetd2
- chetf2
- chetrd
- chetrf
- chetri
- chetri2
- chetri2x
- chetrs
- chetrs2
- chfrk
- chgeqz
- chla_transtype
- chpcon
- chpev
- chpevd
- chpevx
- chpgst
- chpgv
- chpgvd
- chpgvx
- chprfs
- chpsv
- chpsvx
- chptrd
- chptrf
- chptri
- chptrs
- chsein
- chseqr
- clabrd
- clacgv
- clacn2
- clacon
- clacp2
- clacpy
- clacrm
- clacrt
- cladiv
- claed0
- claed7
- claed8
- claein
- claesy
- claev2
- clag2z
- clags2
- clagtm
- clahef
- clahqr
- clahr2
- claic1
- clals0
- clalsa
- clalsd
- clangb
- clange
- clangt
- clanhb
- clanhe
- clanhf
- clanhp
- clanhs
- clanht
- clansb
- clansp
- clansy
- clantb
- clantp
- clantr
- clapll
- clapmr
- clapmt
- claqgb
- claqge
- claqhb
- claqhe
- claqhp
- claqp2
- claqps
- claqr0
- claqr1
- claqr2
- claqr3
- claqr4
- claqr5
- claqsb
- claqsp
- claqsy
- clar1v
- clar2v
- clarcm
- clarf
- clarfb
- clarfg
- clarfgp
- clarft
- clarfx
- clargv
- clarnv
- clarrv
- clartg
- clartv
- clarz
- clarzb
- clarzt
- clascl
- claset
- clasr
- classq
- claswp
- clasyf
- clatbs
- clatdf
- clatps
- clatrd
- clatrs
- clatrz
- clauu2
- clauum
- cpbcon
- cpbequ
- cpbrfs
- cpbstf
- cpbsv
- cpbsvx
- cpbtf2
- cpbtrf
- cpbtrs
- cpftrf
- cpftri
- cpftrs
- cpocon
- cpoequ
- cpoequb
- cporfs
- cposv
- cposvx
- cpotf2
- cpotrf
- cpotri
- cpotrs
- cppcon
- cppequ
- cpprfs
- cppsv
- cppsvx
- cpptrf
- cpptri
- cpptrs
- cpstf2
- cpstrf
- cptcon
- cpteqr
- cptrfs
- cptsv
- cptsvx
- cpttrf
- cpttrs
- cptts2
- crot
- cspcon
- cspmv
- cspr
- csprfs
- cspsv
- cspsvx
- csptrf
- csptri
- csptrs
- csrscl
- cstedc
- cstegr
- cstein
- cstemr
- csteqr
- csycon
- csyconv
- csyequb
- csymv
- csyr
- csyrfs
- csysv
- csysvx
- csyswapr
- csytf2
- csytrf
- csytri
- csytri2
- csytri2x
- csytrs
- csytrs2
- ctbcon
- ctbrfs
- ctbtrs
- ctfsm
- ctftri
- ctfttp
- ctfttr
- ctgevc
- ctgex2
- ctgexc
- ctgsen
- ctgsja
- ctgsna
- ctgsy2
- ctgsyl
- ctpcon
- ctpmqrt
- ctpqrt
- ctpqrt2
- ctprfb
- ctprfs
- ctptri
- ctptrs
- ctpttf
- ctpttr
- ctrcon
- ctrevc
- ctrexc
- ctrrfs
- ctrsen
- ctrsna
- ctrsyl
- ctrti2
- ctrtri
- ctrtrs
- ctrttf
- ctrttp
- ctzrzf
- cunbdb
- cuncsd
- cung2l
- cung2r
- cungbr
- cunghr
- cungl2
- cunglq
- cungql
- cungqr
- cungr2
- cungrq
- cungtr
- cunm2l
- cunm2r
- cunmbr
- cunmhr
- cunml2
- cunmlq
- cunmql
- cunmqr
- cunmr2
- cunmr3
- cunmrq
- cunmrz
- cunmtr
- cupgtr
- cupmtr
- dbbcsd
- dbdsdc
- dbdsqr
- ddisna
- dgbbrd
- dgbcon
- dgbequ
- dgbequb
- dgbrfs
- dgbsv
- dgbsvx
- dgbtf2
- dgbtrf
- dgbtrs
- dgebak
- dgebal
- dgebd2
- dgebrd
- dgecon
- dgeequ
- dgeequb
- dgees
- dgeesx
- dgeev
- dgeevx
- dgehd2
- dgehrd
- dgejsv
- dgelq2
- dgelqf
- dgels
- dgelsd
- dgelss
- dgelsy
- dgemqrt
- dgeql2
- dgeqlf
- dgeqp3
- dgeqr2
- dgeqr2p
- dgeqrf
- dgeqrfp
- dgeqrt
- dgeqrt2
- dgeqrt3
- dgerfs
- dgerq2
- dgerqf
- dgesc2
- dgesdd
- dgesv
- dgesvd
- dgesvj
- dgesvx
- dgetc2
- dgetf2
- dgetrf
- dgetri
- dgetrs
- dggbak
- dggbal
- dgges
- dggesx
- dggev
- dggevx
- dggglm
- dgghrd
- dgglse
- dggqrf
- dggrqf
- dgsvj0
- dgsvj1
- dgtcon
- dgtrfs
- dgtsv
- dgtsvx
- dgttrf
- dgttrs
- dgtts2
- dhgeqz
- dhsein
- dhseqr
- disnan
- dlabad
- dlabrd
- dlacn2
- dlacon
- dlacpy
- dladiv
- dlae2
- dlaebz
- dlaed0
- dlaed1
- dlaed2
- dlaed3
- dlaed4
- dlaed5
- dlaed6
- dlaed7
- dlaed8
- dlaed9
- dlaeda
- dlaein
- dlaev2
- dlaexc
- dlag2
- dlag2s
- dlags2
- dlagtf
- dlagtm
- dlagts
- dlagv2
- dlahqr
- dlahr2
- dlaic1
- dlaln2
- dlals0
- dlalsa
- dlalsd
- dlamch
- dlamrg
- dlaneg
- dlangb
- dlange
- dlangt
- dlanhs
- dlansb
- dlansf
- dlansp
- dlanst
- dlansy
- dlantb
- dlantp
- dlantr
- dlanv2
- dlapll
- dlapmr
- dlapmt
- dlapy2
- dlapy3
- dlaqgb
- dlaqge
- dlaqp2
- dlaqps
- dlaqr0
- dlaqr1
- dlaqr2
- dlaqr3
- dlaqr4
- dlaqr5
- dlaqsb
- dlaqsp
- dlaqsy
- dlaqtr
- dlar1v
- dlar2v
- dlarf
- dlarfb
- dlarfg
- dlarfgp
- dlarft
- dlarfx
- dlargv
- dlarnv
- dlarra
- dlarrb
- dlarrc
- dlarrd
- dlarre
- dlarrf
- dlarrj
- dlarrk
- dlarrr
- dlarrv
- dlartg
- dlartgp
- dlartgs
- dlartv
- dlaruv
- dlarz
- dlarzb
- dlarzt
- dlas2
- dlascl
- dlasd0
- dlasd1
- dlasd2
- dlasd3
- dlasd4
- dlasd5
- dlasd6
- dlasd7
- dlasd8
- dlasda
- dlasdq
- dlasdt
- dlaset
- dlasq1
- dlasq2
- dlasq3
- dlasq4
- dlasq6
- dlasr
- dlasrt
- dlassq
- dlasv2
- dlaswp
- dlasy2
- dlasyf
- dlat2s
- dlatbs
- dlatdf
- dlatps
- dlatrd
- dlatrs
- dlatrz
- dlauu2
- dlauum
- dopgtr
- dopmtr
- dorbdb
- dorcsd
- dorg2l
- dorg2r
- dorgbr
- dorghr
- dorgl2
- dorglq
- dorgql
- dorgqr
- dorgr2
- dorgrq
- dorgtr
- dorm2l
- dorm2r
- dormbr
- dormhr
- dorml2
- dormlq
- dormql
- dormqr
- dormr2
- dormr3
- dormrq
- dormrz
- dormtr
- dpbcon
- dpbequ
- dpbrfs
- dpbstf
- dpbsv
- dpbsvx
- dpbtf2
- dpbtrf
- dpbtrs
- dpftrf
- dpftri
- dpftrs
- dpocon
- dpoequ
- dpoequb
- dporfs
- dposv
- dposvx
- dpotf2
- dpotrf
- dpotri
- dpotrs
- dppcon
- dppequ
- dpprfs
- dppsv
- dppsvx
- dpptrf
- dpptri
- dpptrs
- dpstf2
- dpstrf
- dptcon
- dpteqr
- dptrfs
- dptsv
- dptsvx
- dpttrf
- dpttrs
- dptts2
- drscl
- dsbev
- dsbevd
- dsbevx
- dsbgst
- dsbgv
- dsbgvd
- dsbgvx
- dsbtrd
- dsfrk
- dsgesv
- dspcon
- dspev
- dspevd
- dspevx
- dspgst
- dspgv
- dspgvd
- dspgvx
- dsposv
- dsprfs
- dspsv
- dspsvx
- dsptrd
- dsptrf
- dsptri
- dsptrs
- dstebz
- dstedc
- dstegr
- dstein
- dstemr
- dsteqr
- dsterf
- dstev
- dstevd
- dstevr
- dstevx
- dsycon
- dsyconv
- dsyequb
- dsyev
- dsyevd
- dsyevr
- dsyevx
- dsygs2
- dsygst
- dsygv
- dsygvd
- dsygvx
- dsyrfs
- dsysv
- dsysvx
- dsyswapr
- dsytd2
- dsytf2
- dsytrd
- dsytrf
- dsytri
- dsytri2
- dsytri2x
- dsytrs
- dsytrs2
- dtbcon
- dtbrfs
- dtbtrs
- dtfsm
- dtftri
- dtfttp
- dtfttr
- dtgevc
- dtgex2
- dtgexc
- dtgsen
- dtgsja
- dtgsna
- dtgsy2
- dtgsyl
- dtpcon
- dtpmqrt
- dtpqrt
- dtpqrt2
- dtprfb
- dtprfs
- dtptri
- dtptrs
- dtpttf
- dtpttr
- dtrcon
- dtrevc
- dtrexc
- dtrrfs
- dtrsen
- dtrsna
- dtrsyl
- dtrti2
- dtrtri
- dtrtrs
- dtrttf
- dtrttp
- dtzrzf
- dzsum1
- icmax1
- ieeeck
- ilaclc
- ilaclr
- iladiag
- iladlc
- iladlr
- ilaprec
- ilaslc
- ilaslr
- ilatrans
- ilauplo
- ilaver
- ilazlc
- ilazlr
- izmax1
- sbbcsd
- sbdsdc
- sbdsqr
- scsum1
- sdisna
- sgbbrd
- sgbcon
- sgbequ
- sgbequb
- sgbrfs
- sgbsv
- sgbsvx
- sgbtf2
- sgbtrf
- sgbtrs
- sgebak
- sgebal
- sgebd2
- sgebrd
- sgecon
- sgeequ
- sgeequb
- sgees
- sgeesx
- sgeev
- sgeevx
- sgehd2
- sgehrd
- sgejsv
- sgelq2
- sgelqf
- sgels
- sgelsd
- sgelss
- sgelsy
- sgemqrt
- sgeql2
- sgeqlf
- sgeqp3
- sgeqr2
- sgeqr2p
- sgeqrf
- sgeqrfp
- sgeqrt
- sgeqrt2
- sgeqrt3
- sgerfs
- sgerq2
- sgerqf
- sgesc2
- sgesdd
- sgesv
- sgesvd
- sgesvj
- sgesvx
- sgetc2
- sgetf2
- sgetrf
- sgetri
- sgetrs
- sggbak
- sggbal
- sgges
- sggesx
- sggev
- sggevx
- sggglm
- sgghrd
- sgglse
- sggqrf
- sggrqf
- sgsvj0
- sgsvj1
- sgtcon
- sgtrfs
- sgtsv
- sgtsvx
- sgttrf
- sgttrs
- sgtts2
- shgeqz
- shsein
- shseqr
- slabad
- slabrd
- slacn2
- slacon
- slacpy
- sladiv
- slae2
- slaebz
- slaed0
- slaed1
- slaed2
- slaed3
- slaed4
- slaed5
- slaed6
- slaed7
- slaed8
- slaed9
- slaeda
- slaein
- slaev2
- slaexc
- slag2
- slag2d
- slags2
- slagtf
- slagtm
- slagts
- slagv2
- slahqr
- slahr2
- slaic1
- slaln2
- slals0
- slalsa
- slalsd
- slamch
- slamrg
- slangb
- slange
- slangt
- slanhs
- slansb
- slansf
- slansp
- slanst
- slansy
- slantb
- slantp
- slantr
- slanv2
- slapll
- slapmr
- slapmt
- slapy2
- slapy3
- slaqgb
- slaqge
- slaqp2
- slaqps
- slaqr0
- slaqr1
- slaqr2
- slaqr3
- slaqr4
- slaqr5
- slaqsb
- slaqsp
- slaqsy
- slaqtr
- slar1v
- slar2v
- slarf
- slarfb
- slarfg
- slarfgp
- slarft
- slarfx
- slargv
- slarnv
- slarra
- slarrb
- slarrc
- slarrd
- slarre
- slarrf
- slarrj
- slarrk
- slarrr
- slarrv
- slartg
- slartgp
- slartgs
- slartv
- slaruv
- slarz
- slarzb
- slarzt
- slas2
- slascl
- slasd0
- slasd1
- slasd2
- slasd3
- slasd4
- slasd5
- slasd6
- slasd7
- slasd8
- slasda
- slasdq
- slasdt
- slaset
- slasq1
- slasq2
- slasq3
- slasq4
- slasq6
- slasr
- slasrt
- slassq
- slasv2
- slaswp
- slasy2
- slasyf
- slatbs
- slatdf
- slatps
- slatrd
- slatrs
- slatrz
- slauu2
- slauum
- sopgtr
- sopmtr
- sorbdb
- sorcsd
- sorg2l
- sorg2r
- sorgbr
- sorghr
- sorgl2
- sorglq
- sorgql
- sorgqr
- sorgr2
- sorgrq
- sorgtr
- sorm2l
- sorm2r
- sormbr
- sormhr
- sorml2
- sormlq
- sormql
- sormqr
- sormr2
- sormr3
- sormrq
- sormrz
- sormtr
- spbcon
- spbequ
- spbrfs
- spbstf
- spbsv
- spbsvx
- spbtf2
- spbtrf
- spbtrs
- spftrf
- spftri
- spftrs
- spocon
- spoequ
- spoequb
- sporfs
- sposv
- sposvx
- spotf2
- spotrf
- spotri
- spotrs
- sppcon
- sppequ
- spprfs
- sppsv
- sppsvx
- spptrf
- spptri
- spptrs
- spstf2
- spstrf
- sptcon
- spteqr
- sptrfs
- sptsv
- sptsvx
- spttrf
- spttrs
- sptts2
- srscl
- ssbev
- ssbevd
- ssbevx
- ssbgst
- ssbgv
- ssbgvd
- ssbgvx
- ssbtrd
- ssfrk
- sspcon
- sspev
- sspevd
- sspevx
- sspgst
- sspgv
- sspgvd
- sspgvx
- ssprfs
- sspsv
- sspsvx
- ssptrd
- ssptrf
- ssptri
- ssptrs
- sstebz
- sstedc
- sstegr
- sstein
- sstemr
- ssteqr
- ssterf
- sstev
- sstevd
- sstevr
- sstevx
- ssycon
- ssyconv
- ssyequb
- ssyev
- ssyevd
- ssyevr
- ssyevx
- ssygs2
- ssygst
- ssygv
- ssygvd
- ssygvx
- ssyrfs
- ssysv
- ssysvx
- ssyswapr
- ssytd2
- ssytf2
- ssytrd
- ssytrf
- ssytri
- ssytri2
- ssytri2x
- ssytrs
- ssytrs2
- stbcon
- stbrfs
- stbtrs
- stfsm
- stftri
- stfttp
- stfttr
- stgevc
- stgex2
- stgexc
- stgsen
- stgsja
- stgsna
- stgsy2
- stgsyl
- stpcon
- stpmqrt
- stpqrt
- stpqrt2
- stprfb
- stprfs
- stptri
- stptrs
- stpttf
- stpttr
- strcon
- strevc
- strexc
- strrfs
- strsen
- strsna
- strsyl
- strti2
- strtri
- strtrs
- strttf
- strttp
- stzrzf
- xerbla_array
- zbbcsd
- zbdsqr
- zcgesv
- zcposv
- zdrscl
- zgbbrd
- zgbcon
- zgbequ
- zgbequb
- zgbrfs
- zgbsv
- zgbsvx
- zgbtf2
- zgbtrf
- zgbtrs
- zgebak
- zgebal
- zgebd2
- zgebrd
- zgecon
- zgeequ
- zgeequb
- zgees
- zgeesx
- zgeev
- zgeevx
- zgehd2
- zgehrd
- zgelq2
- zgelqf
- zgels
- zgelsd
- zgelss
- zgelsy
- zgemqrt
- zgeql2
- zgeqlf
- zgeqp3
- zgeqr2
- zgeqr2p
- zgeqrf
- zgeqrfp
- zgeqrt
- zgeqrt2
- zgeqrt3
- zgerfs
- zgerq2
- zgerqf
- zgesc2
- zgesdd
- zgesv
- zgesvd
- zgesvx
- zgetc2
- zgetf2
- zgetrf
- zgetri
- zgetrs
- zggbak
- zggbal
- zgges
- zggesx
- zggev
- zggevx
- zggglm
- zgghrd
- zgglse
- zggqrf
- zggrqf
- zgtcon
- zgtrfs
- zgtsv
- zgtsvx
- zgttrf
- zgttrs
- zgtts2
- zhbev
- zhbevd
- zhbevx
- zhbgst
- zhbgv
- zhbgvd
- zhbgvx
- zhbtrd
- zhecon
- zheequb
- zheev
- zheevd
- zheevr
- zheevx
- zhegs2
- zhegst
- zhegv
- zhegvd
- zhegvx
- zherfs
- zhesv
- zhesvx
- zheswapr
- zhetd2
- zhetf2
- zhetrd
- zhetrf
- zhetri
- zhetri2
- zhetri2x
- zhetrs
- zhetrs2
- zhfrk
- zhgeqz
- zhpcon
- zhpev
- zhpevd
- zhpevx
- zhpgst
- zhpgv
- zhpgvd
- zhpgvx
- zhprfs
- zhpsv
- zhpsvx
- zhptrd
- zhptrf
- zhptri
- zhptrs
- zhsein
- zhseqr
- zlabrd
- zlacgv
- zlacn2
- zlacon
- zlacp2
- zlacpy
- zlacrm
- zlacrt
- zladiv
- zlaed0
- zlaed7
- zlaed8
- zlaein
- zlaesy
- zlaev2
- zlag2c
- zlags2
- zlagtm
- zlahef
- zlahqr
- zlahr2
- zlaic1
- zlals0
- zlalsa
- zlalsd
- zlangb
- zlange
- zlangt
- zlanhb
- zlanhe
- zlanhf
- zlanhp
- zlanhs
- zlanht
- zlansb
- zlansp
- zlansy
- zlantb
- zlantp
- zlantr
- zlapll
- zlapmr
- zlapmt
- zlaqgb
- zlaqge
- zlaqhb
- zlaqhe
- zlaqhp
- zlaqp2
- zlaqps
- zlaqr0
- zlaqr1
- zlaqr2
- zlaqr3
- zlaqr4
- zlaqr5
- zlaqsb
- zlaqsp
- zlaqsy
- zlar1v
- zlar2v
- zlarcm
- zlarf
- zlarfb
- zlarfg
- zlarfgp
- zlarft
- zlarfx
- zlargv
- zlarnv
- zlarrv
- zlartg
- zlartv
- zlarz
- zlarzb
- zlarzt
- zlascl
- zlaset
- zlasr
- zlassq
- zlaswp
- zlasyf
- zlat2c
- zlatbs
- zlatdf
- zlatps
- zlatrd
- zlatrs
- zlatrz
- zlauu2
- zlauum
- zpbcon
- zpbequ
- zpbrfs
- zpbstf
- zpbsv
- zpbsvx
- zpbtf2
- zpbtrf
- zpbtrs
- zpftrf
- zpftri
- zpftrs
- zpocon
- zpoequ
- zpoequb
- zporfs
- zposv
- zposvx
- zpotf2
- zpotrf
- zpotri
- zpotrs
- zppcon
- zppequ
- zpprfs
- zppsv
- zppsvx
- zpptrf
- zpptri
- zpptrs
- zpstf2
- zpstrf
- zptcon
- zpteqr
- zptrfs
- zptsv
- zptsvx
- zpttrf
- zpttrs
- zptts2
- zrot
- zspcon
- zspmv
- zspr
- zsprfs
- zspsv
- zspsvx
- zsptrf
- zsptri
- zsptrs
- zstedc
- zstegr
- zstein
- zstemr
- zsteqr
- zsycon
- zsyconv
- zsyequb
- zsymv
- zsyr
- zsyrfs
- zsysv
- zsysvx
- zsyswapr
- zsytf2
- zsytrf
- zsytri
- zsytri2
- zsytri2x
- zsytrs
- zsytrs2
- ztbcon
- ztbrfs
- ztbtrs
- ztfsm
- ztftri
- ztfttp
- ztfttr
- ztgevc
- ztgex2
- ztgexc
- ztgsen
- ztgsja
- ztgsna
- ztgsy2
- ztgsyl
- ztpcon
- ztpmqrt
- ztpqrt
- ztpqrt2
- ztprfb
- ztprfs
- ztptri
- ztptrs
- ztpttf
- ztpttr
- ztrcon
- ztrevc
- ztrexc
- ztrrfs
- ztrsen
- ztrsna
- ztrsyl
- ztrti2
- ztrtri
- ztrtrs
- ztrttf
- ztrttp
- ztzrzf
- zunbdb
- zuncsd
- zung2l
- zung2r
- zungbr
- zunghr
- zungl2
- zunglq
- zungql
- zungqr
- zungr2
- zungrq
- zungtr
- zunm2l
- zunm2r
- zunmbr
- zunmhr
- zunml2
- zunmlq
- zunmql
- zunmqr
- zunmr2
- zunmr3
- zunmrq
- zunmrz
- zunmtr
- zupgtr
- zupmtr
"""

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>

# functions

def _test_dlamch(*args, **kwargs): # real signature unknown
    pass

def _test_slamch(*args, **kwargs): # real signature unknown
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x0FDB7C30>'

__pyx_capi__ = {
    'cbbcsd': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FCFB9C8>'
    'cbdsqr': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBBA58>'
    'cgbbrd': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBBA10>'
    'cgbcon': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBBA70>'
    'cgbequ': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBBA40>'
    'cgbequb': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBBA28>'
    'cgbrfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBBA88>'
    'cgbsv': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBBAA0>'
    'cgbsvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBBAB8>'
    'cgbtf2': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_float_complex *, int *, int *, int *)" at 0x0FDBBAD0>'
    'cgbtrf': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_float_complex *, int *, int *, int *)" at 0x0FDBBAE8>'
    'cgbtrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBBB00>'
    'cgebak': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBBB18>'
    'cgebal': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBBB30>'
    'cgebd2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0FDBBB48>'
    'cgebrd': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBBB60>'
    'cgecon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBBB78>'
    'cgeequ': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBBB90>'
    'cgeequb': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBBBA8>'
    'cgees': None, # (!) real value is '<capsule object "void (char *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_cselect1 *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDBBBC0>'
    'cgeesx': None, # (!) real value is '<capsule object "void (char *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_cselect1 *, char *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDBBBD8>'
    'cgeev': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBBBF0>'
    'cgeevx': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBBC08>'
    'cgehd2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0FDBBC20>'
    'cgehrd': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBBC38>'
    'cgelq2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0FDBBC50>'
    'cgelqf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBBC68>'
    'cgels': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBBC80>'
    'cgelsd': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDBBC98>'
    'cgelss': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBBCB0>'
    'cgelsy': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBBCC8>'
    'cgemqrt': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBBCE0>'
    'cgeql2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0FDBBCF8>'
    'cgeqlf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBBD10>'
    'cgeqp3': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBBD28>'
    'cgeqr2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0FDBBD40>'
    'cgeqr2p': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0FDBBD58>'
    'cgeqrf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBBD70>'
    'cgeqrfp': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBBD88>'
    'cgeqrt': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBBDA0>'
    'cgeqrt2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBBDB8>'
    'cgeqrt3': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBBDD0>'
    'cgerfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBBDE8>'
    'cgerq2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0FDBBE00>'
    'cgerqf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBBE18>'
    'cgesc2': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDBBE30>'
    'cgesdd': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDBBE48>'
    'cgesv': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBBE60>'
    'cgesvd': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBBE78>'
    'cgesvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBBE90>'
    'cgetc2': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, int *, int *, int *, int *)" at 0x0FDBBEA8>'
    'cgetf2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, int *, int *)" at 0x0FDBBEC0>'
    'cgetrf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, int *, int *)" at 0x0FDBBED8>'
    'cgetri': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBBEF0>'
    'cgetrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBBF08>'
    'cggbak': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBBF20>'
    'cggbal': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBBF38>'
    'cgges': None, # (!) real value is '<capsule object "void (char *, char *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_cselect2 *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDBBF50>'
    'cggesx': None, # (!) real value is '<capsule object "void (char *, char *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_cselect2 *, char *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDBBF68>'
    'cggev': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBBF80>'
    'cggevx': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDBBF98>'
    'cggglm': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBBFB0>'
    'cgghrd': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBBFC8>'
    'cgglse': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBBFE0>'
    'cggqrf': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBD020>'
    'cggrqf': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBD038>'
    'cgtcon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *)" at 0x0FDBD050>'
    'cgtrfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBD068>'
    'cgtsv': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBD080>'
    'cgtsvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBD098>'
    'cgttrf': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBD0B0>'
    'cgttrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBD0C8>'
    'cgtts2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBD0E0>'
    'chbev': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBD0F8>'
    'chbevd': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDBD110>'
    'chbevx': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDBD128>'
    'chbgst': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBD140>'
    'chbgv': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBD158>'
    'chbgvd': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDBD170>'
    'chbgvx': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDBD188>'
    'chbtrd': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBD1A0>'
    'checon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *)" at 0x0FDBD1B8>'
    'cheequb': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *)" at 0x0FDBD1D0>'
    'cheev': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBD1E8>'
    'cheevd': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDBD200>'
    'cheevr': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDBD218>'
    'cheevx': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDBD230>'
    'chegs2': None, # (!) real value is '<capsule object "void (int *, char *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBD248>'
    'chegst': None, # (!) real value is '<capsule object "void (int *, char *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBD260>'
    'chegv': None, # (!) real value is '<capsule object "void (int *, char *, char *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBD278>'
    'chegvd': None, # (!) real value is '<capsule object "void (int *, char *, char *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDBD290>'
    'chegvx': None, # (!) real value is '<capsule object "void (int *, char *, char *, char *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDBD2A8>'
    'cherfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBD2C0>'
    'chesv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBD2D8>'
    'chesvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBD2F0>'
    'cheswapr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *, int *)" at 0x0FDBD308>'
    'chetd2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *)" at 0x0FDBD320>'
    'chetf2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *, int *)" at 0x0FDBD338>'
    'chetrd': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBD350>'
    'chetrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBD368>'
    'chetri': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBD380>'
    'chetri2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBD398>'
    'chetri2x': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBD3B0>'
    'chetrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBD3C8>'
    'chetrs2': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBD3E0>'
    'chfrk': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *)" at 0x0FDBD3F8>'
    'chgeqz': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBD410>'
    'chla_transtype': None, # (!) real value is '<capsule object "char (int *)" at 0x0FDBD428>'
    'chpcon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *)" at 0x0FDBD440>'
    'chpev': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBD458>'
    'chpevd': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDBD470>'
    'chpevx': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDBD488>'
    'chpgst': None, # (!) real value is '<capsule object "void (int *, char *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0FDBD4A0>'
    'chpgv': None, # (!) real value is '<capsule object "void (int *, char *, char *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBD4B8>'
    'chpgvd': None, # (!) real value is '<capsule object "void (int *, char *, char *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDBD4D0>'
    'chpgvx': None, # (!) real value is '<capsule object "void (int *, char *, char *, char *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDBD4E8>'
    'chprfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBD500>'
    'chpsv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBD518>'
    'chpsvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBD530>'
    'chptrd': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *)" at 0x0FDBD548>'
    'chptrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBD560>'
    'chptri': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBD578>'
    'chptrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBD590>'
    'chsein': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDBD5A8>'
    'chseqr': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBD5C0>'
    'clabrd': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBD5D8>'
    'clacgv': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, int *)" at 0x0FDBD5F0>'
    'clacn2': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDBD608>'
    'clacon': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBD620>'
    'clacp2': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBD638>'
    'clacpy': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBD650>'
    'clacrm': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDBD668>'
    'clacrt': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *)" at 0x0FDBD680>'
    'cladiv': None, # (!) real value is '<capsule object "__pyx_t_float_complex (__pyx_t_float_complex *, __pyx_t_float_complex *)" at 0x0FDBD698>'
    'claed0': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDBD6B0>'
    'claed7': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDBD6C8>'
    'claed8': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBD6E0>'
    'claein': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBD6F8>'
    'claesy': None, # (!) real value is '<capsule object "void (__pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *)" at 0x0FDBD710>'
    'claev2': None, # (!) real value is '<capsule object "void (__pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *)" at 0x0FDBD728>'
    'clag2z': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDBD740>'
    'clags2': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *)" at 0x0FDBD758>'
    'clagtm': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *)" at 0x0FDBD770>'
    'clahef': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBD788>'
    'clahqr': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBD7A0>'
    'clahr2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBD7B8>'
    'claic1': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_float_complex *)" at 0x0FDBD7D0>'
    'clals0': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBD7E8>'
    'clalsa': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDBD800>'
    'clalsd': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDBD818>'
    'clangb': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDBD830>'
    'clange': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDBD848>'
    'clangt': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *)" at 0x0FDBD860>'
    'clanhb': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDBD878>'
    'clanhe': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, char *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDBD890>'
    'clanhf': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, char *, char *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDBD8A8>'
    'clanhp': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, char *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDBD8C0>'
    'clanhs': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDBD8D8>'
    'clanht': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *)" at 0x0FDBD8F0>'
    'clansb': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDBD908>'
    'clansp': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, char *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDBD920>'
    'clansy': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, char *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDBD938>'
    'clantb': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDBD950>'
    'clantp': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, char *, char *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDBD968>'
    'clantr': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDBD980>'
    'clapll': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDBD998>'
    'clapmr': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBD9B0>'
    'clapmt': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBD9C8>'
    'claqgb': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, char *)" at 0x0FDBD9E0>'
    'claqge': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, char *)" at 0x0FDBD9F8>'
    'claqhb': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, char *)" at 0x0FDBDA10>'
    'claqhe': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, char *)" at 0x0FDBDA28>'
    'claqhp': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, char *)" at 0x0FDBDA40>'
    'claqp2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *)" at 0x0FDBDA58>'
    'claqps': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0FDBDA70>'
    'claqr0': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBDA88>'
    'claqr1': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *)" at 0x0FDBDAA0>'
    'claqr2': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, int *, __pyx_t_float_complex *, int *, int *, int *, __pyx_t_float_complex *, int *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBDAB8>'
    'claqr3': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, int *, __pyx_t_float_complex *, int *, int *, int *, __pyx_t_float_complex *, int *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBDAD0>'
    'claqr4': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBDAE8>'
    'claqr5': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBDB00>'
    'claqsb': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, char *)" at 0x0FDBDB18>'
    'claqsp': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, char *)" at 0x0FDBDB30>'
    'claqsy': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, char *)" at 0x0FDBDB48>'
    'clar1v': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDBDB60>'
    'clar2v': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *)" at 0x0FDBDB78>'
    'clarcm': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDBDB90>'
    'clarf': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *)" at 0x0FDBDBA8>'
    'clarfb': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBDBC0>'
    'clarfg': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *)" at 0x0FDBDBD8>'
    'clarfgp': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *)" at 0x0FDBDBF0>'
    'clarft': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0FDBDC08>'
    'clarfx': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *)" at 0x0FDBDC20>'
    'clargv': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBDC38>'
    'clarnv': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *)" at 0x0FDBDC50>'
    'clarrv': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDBDC68>'
    'clartg': None, # (!) real value is '<capsule object "void (__pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_float_complex *)" at 0x0FDBDC80>'
    'clartv': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *)" at 0x0FDBDC98>'
    'clarz': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *)" at 0x0FDBDCB0>'
    'clarzb': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBDCC8>'
    'clarzt': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0FDBDCE0>'
    'clascl': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBDCF8>'
    'claset': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0FDBDD10>'
    'clasr': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *)" at 0x0FDBDD28>'
    'classq': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDBDD40>'
    'claswp': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, int *, int *, int *, int *, int *)" at 0x0FDBDD58>'
    'clasyf': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBDD70>'
    'clatbs': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBDD88>'
    'clatdf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDBDDA0>'
    'clatps': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBDDB8>'
    'clatrd': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0FDBDDD0>'
    'clatrs': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBDDE8>'
    'clatrz': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *)" at 0x0FDBDE00>'
    'clauu2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBDE18>'
    'clauum': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBDE30>'
    'cpbcon': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBDE48>'
    'cpbequ': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBDE60>'
    'cpbrfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBDE78>'
    'cpbstf': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBDE90>'
    'cpbsv': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBDEA8>'
    'cpbsvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBDEC0>'
    'cpbtf2': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBDED8>'
    'cpbtrf': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBDEF0>'
    'cpbtrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBDF08>'
    'cpftrf': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBDF20>'
    'cpftri': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBDF38>'
    'cpftrs': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBDF50>'
    'cpocon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBDF68>'
    'cpoequ': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBDF80>'
    'cpoequb': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBDF98>'
    'cporfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBDFB0>'
    'cposv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBDFC8>'
    'cposvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBDFE0>'
    'cpotf2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF020>'
    'cpotrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF038>'
    'cpotri': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF050>'
    'cpotrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF068>'
    'cppcon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF080>'
    'cppequ': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF098>'
    'cpprfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF0B0>'
    'cppsv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF0C8>'
    'cppsvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF0E0>'
    'cpptrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBF0F8>'
    'cpptri': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBF110>'
    'cpptrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF128>'
    'cpstf2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF140>'
    'cpstrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF158>'
    'cptcon': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF170>'
    'cpteqr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF188>'
    'cptrfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF1A0>'
    'cptsv': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF1B8>'
    'cptsvx': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF1D0>'
    'cpttrf': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *)" at 0x0FDBF1E8>'
    'cpttrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF200>'
    'cptts2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0FDBF218>'
    'crot': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *)" at 0x0FDBF230>'
    'cspcon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *)" at 0x0FDBF248>'
    'cspmv': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0FDBF260>'
    'cspr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *)" at 0x0FDBF278>'
    'csprfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF290>'
    'cspsv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF2A8>'
    'cspsvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF2C0>'
    'csptrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF2D8>'
    'csptri': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBF2F0>'
    'csptrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF308>'
    'csrscl': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *)" at 0x0FDBF320>'
    'cstedc': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDBF338>'
    'cstegr': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDBF350>'
    'cstein': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDBF368>'
    'cstemr': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDBF380>'
    'csteqr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF398>'
    'csycon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *)" at 0x0FDBF3B0>'
    'csyconv': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBF3C8>'
    'csyequb': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *)" at 0x0FDBF3E0>'
    'csymv': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0FDBF3F8>'
    'csyr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBF410>'
    'csyrfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF428>'
    'csysv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF440>'
    'csysvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF458>'
    'csyswapr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *, int *)" at 0x0FDBF470>'
    'csytf2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *, int *)" at 0x0FDBF488>'
    'csytrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF4A0>'
    'csytri': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBF4B8>'
    'csytri2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF4D0>'
    'csytri2x': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF4E8>'
    'csytrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF500>'
    'csytrs2': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBF518>'
    'ctbcon': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF530>'
    'ctbrfs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF548>'
    'ctbtrs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF560>'
    'ctfsm': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0FDBF578>'
    'ctftri': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBF590>'
    'ctfttp': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0FDBF5A8>'
    'ctfttr': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF5C0>'
    'ctgevc': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF5D8>'
    'ctgex2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, int *)" at 0x0FDBF5F0>'
    'ctgexc': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, int *, int *)" at 0x0FDBF608>'
    'ctgsen': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, int *, int *, int *)" at 0x0FDBF620>'
    'ctgsja': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF638>'
    'ctgsna': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_float_complex *, int *, int *, int *)" at 0x0FDBF650>'
    'ctgsy2': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF668>'
    'ctgsyl': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, int *, int *)" at 0x0FDBF680>'
    'ctpcon': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF698>'
    'ctpmqrt': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBF6B0>'
    'ctpqrt': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBF6C8>'
    'ctpqrt2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF6E0>'
    'ctprfb': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBF6F8>'
    'ctprfs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF710>'
    'ctptri': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBF728>'
    'ctptrs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF740>'
    'ctpttf': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0FDBF758>'
    'ctpttr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF770>'
    'ctrcon': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF788>'
    'ctrevc': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, int *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF7A0>'
    'ctrexc': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *, int *, int *)" at 0x0FDBF7B8>'
    'ctrrfs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF7D0>'
    'ctrsen': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF7E8>'
    'ctrsna': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF800>'
    'ctrsyl': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDBF818>'
    'ctrti2': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF830>'
    'ctrtri': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF848>'
    'ctrtrs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF860>'
    'ctrttf': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBF878>'
    'ctrttp': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBF890>'
    'ctzrzf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF8A8>'
    'cunbdb': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF8C0>'
    'cuncsd': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, char *, char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDBF8D8>'
    'cung2l': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0FDBF8F0>'
    'cung2r': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0FDBF908>'
    'cungbr': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF920>'
    'cunghr': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF938>'
    'cungl2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0FDBF950>'
    'cunglq': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF968>'
    'cungql': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF980>'
    'cungqr': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF998>'
    'cungr2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0FDBF9B0>'
    'cungrq': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF9C8>'
    'cungtr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBF9E0>'
    'cunm2l': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBF9F8>'
    'cunm2r': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBFA10>'
    'cunmbr': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBFA28>'
    'cunmhr': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBFA40>'
    'cunml2': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBFA58>'
    'cunmlq': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBFA70>'
    'cunmql': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBFA88>'
    'cunmqr': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBFAA0>'
    'cunmr2': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBFAB8>'
    'cunmr3': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBFAD0>'
    'cunmrq': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBFAE8>'
    'cunmrz': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBFB00>'
    'cunmtr': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDBFB18>'
    'cupgtr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBFB30>'
    'cupmtr': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0FDBFB48>'
    'dbbcsd': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFB60>'
    'dbdsdc': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFB78>'
    'dbdsqr': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDBFB90>'
    'ddisna': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDBFBA8>'
    'dgbbrd': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDBFBC0>'
    'dgbcon': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFBD8>'
    'dgbequ': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDBFBF0>'
    'dgbequb': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDBFC08>'
    'dgbrfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFC20>'
    'dgbsv': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFC38>'
    'dgbsvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFC50>'
    'dgbtf2': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDBFC68>'
    'dgbtrf': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDBFC80>'
    'dgbtrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFC98>'
    'dgebak': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFCB0>'
    'dgebal': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDBFCC8>'
    'dgebd2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDBFCE0>'
    'dgebrd': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFCF8>'
    'dgecon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFD10>'
    'dgeequ': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDBFD28>'
    'dgeequb': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDBFD40>'
    'dgees': None, # (!) real value is '<capsule object "void (char *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_dselect2 *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDBFD58>'
    'dgeesx': None, # (!) real value is '<capsule object "void (char *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_dselect2 *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *, int *)" at 0x0FDBFD70>'
    'dgeev': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFD88>'
    'dgeevx': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDBFDA0>'
    'dgehd2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDBFDB8>'
    'dgehrd': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFDD0>'
    'dgejsv': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDBFDE8>'
    'dgelq2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDBFE00>'
    'dgelqf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFE18>'
    'dgels': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFE30>'
    'dgelsd': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDBFE48>'
    'dgelss': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFE60>'
    'dgelsy': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFE78>'
    'dgemqrt': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDBFE90>'
    'dgeql2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDBFEA8>'
    'dgeqlf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFEC0>'
    'dgeqp3': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFED8>'
    'dgeqr2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDBFEF0>'
    'dgeqr2p': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDBFF08>'
    'dgeqrf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFF20>'
    'dgeqrfp': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFF38>'
    'dgeqrt': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDBFF50>'
    'dgeqrt2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFF68>'
    'dgeqrt3': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFF80>'
    'dgerfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFF98>'
    'dgerq2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDBFFB0>'
    'dgerqf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDBFFC8>'
    'dgesc2': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDBFFE0>'
    'dgesdd': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC1020>'
    'dgesv': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1038>'
    'dgesvd': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1050>'
    'dgesvj': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1068>'
    'dgesvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1080>'
    'dgetc2': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC1098>'
    'dgetf2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC10B0>'
    'dgetrf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC10C8>'
    'dgetri': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC10E0>'
    'dgetrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC10F8>'
    'dggbak': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1110>'
    'dggbal': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1128>'
    'dgges': None, # (!) real value is '<capsule object "void (char *, char *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_dselect3 *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC1140>'
    'dggesx': None, # (!) real value is '<capsule object "void (char *, char *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_dselect3 *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *, int *)" at 0x0FDC1158>'
    'dggev': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1170>'
    'dggevx': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC1188>'
    'dggglm': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC11A0>'
    'dgghrd': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC11B8>'
    'dgglse': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC11D0>'
    'dggqrf': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC11E8>'
    'dggrqf': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1200>'
    'dgsvj0': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1218>'
    'dgsvj1': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1230>'
    'dgtcon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1248>'
    'dgtrfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1260>'
    'dgtsv': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1278>'
    'dgtsvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1290>'
    'dgttrf': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC12A8>'
    'dgttrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC12C0>'
    'dgtts2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC12D8>'
    'dhgeqz': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC12F0>'
    'dhsein': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC1308>'
    'dhseqr': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1320>'
    'disnan': None, # (!) real value is '<capsule object "int (__pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1338>'
    'dlabad': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1350>'
    'dlabrd': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1368>'
    'dlacn2': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1380>'
    'dlacon': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1398>'
    'dlacpy': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC13B0>'
    'dladiv': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC13C8>'
    'dlae2': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC13E0>'
    'dlaebz': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC13F8>'
    'dlaed0': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1410>'
    'dlaed1': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1428>'
    'dlaed2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *, int *)" at 0x0FDC1440>'
    'dlaed3': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1458>'
    'dlaed4': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1470>'
    'dlaed5': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1488>'
    'dlaed6': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC14A0>'
    'dlaed7': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC14B8>'
    'dlaed8': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC14D0>'
    'dlaed9': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC14E8>'
    'dlaeda': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1500>'
    'dlaein': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1518>'
    'dlaev2': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1530>'
    'dlaexc': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1548>'
    'dlag2': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1560>'
    'dlag2s': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC1578>'
    'dlags2': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1590>'
    'dlagtf': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC15A8>'
    'dlagtm': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC15C0>'
    'dlagts': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC15D8>'
    'dlagv2': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC15F0>'
    'dlahqr': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1608>'
    'dlahr2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1620>'
    'dlaic1': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1638>'
    'dlaln2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1650>'
    'dlals0': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1668>'
    'dlalsa': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1680>'
    'dlalsd': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1698>'
    'dlamch': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *)" at 0x0FDC16B0>'
    'dlamrg': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC16C8>'
    'dlaneg': None, # (!) real value is '<capsule object "int (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC16E0>'
    'dlangb': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC16F8>'
    'dlange': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1710>'
    'dlangt': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1728>'
    'dlanhs': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1740>'
    'dlansb': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1758>'
    'dlansf': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1770>'
    'dlansp': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1788>'
    'dlanst': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC17A0>'
    'dlansy': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC17B8>'
    'dlantb': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC17D0>'
    'dlantp': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC17E8>'
    'dlantr': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1800>'
    'dlanv2': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1818>'
    'dlapll': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1830>'
    'dlapmr': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1848>'
    'dlapmt': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1860>'
    'dlapy2': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (__pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1878>'
    'dlapy3': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (__pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1890>'
    'dlaqgb': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, char *)" at 0x0FDC18A8>'
    'dlaqge': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, char *)" at 0x0FDC18C0>'
    'dlaqp2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC18D8>'
    'dlaqps': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC18F0>'
    'dlaqr0': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1908>'
    'dlaqr1': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1920>'
    'dlaqr2': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1938>'
    'dlaqr3': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1950>'
    'dlaqr4': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1968>'
    'dlaqr5': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1980>'
    'dlaqsb': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, char *)" at 0x0FDC1998>'
    'dlaqsp': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, char *)" at 0x0FDC19B0>'
    'dlaqsy': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, char *)" at 0x0FDC19C8>'
    'dlaqtr': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC19E0>'
    'dlar1v': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC19F8>'
    'dlar2v': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1A10>'
    'dlarf': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1A28>'
    'dlarfb': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1A40>'
    'dlarfg': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1A58>'
    'dlarfgp': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1A70>'
    'dlarft': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1A88>'
    'dlarfx': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1AA0>'
    'dlargv': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1AB8>'
    'dlarnv': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1AD0>'
    'dlarra': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC1AE8>'
    'dlarrb': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1B00>'
    'dlarrc': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC1B18>'
    'dlarrd': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1B30>'
    'dlarre': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1B48>'
    'dlarrf': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1B60>'
    'dlarrj': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1B78>'
    'dlarrk': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1B90>'
    'dlarrr': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1BA8>'
    'dlarrv': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1BC0>'
    'dlartg': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1BD8>'
    'dlartgp': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1BF0>'
    'dlartgs': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1C08>'
    'dlartv': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1C20>'
    'dlaruv': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1C38>'
    'dlarz': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1C50>'
    'dlarzb': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1C68>'
    'dlarzt': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1C80>'
    'dlas2': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1C98>'
    'dlascl': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1CB0>'
    'dlasd0': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1CC8>'
    'dlasd1': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1CE0>'
    'dlasd2': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *, int *, int *, int *)" at 0x0FDC1CF8>'
    'dlasd3': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1D10>'
    'dlasd4': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1D28>'
    'dlasd5': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1D40>'
    'dlasd6': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1D58>'
    'dlasd7': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1D70>'
    'dlasd8': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1D88>'
    'dlasda': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1DA0>'
    'dlasdq': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1DB8>'
    'dlasdt': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, int *, int *)" at 0x0FDC1DD0>'
    'dlaset': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1DE8>'
    'dlasq1': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1E00>'
    'dlasq2': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1E18>'
    'dlasq3': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1E30>'
    'dlasq4': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1E48>'
    'dlasq6': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1E60>'
    'dlasr': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1E78>'
    'dlasrt': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1E90>'
    'dlassq': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1EA8>'
    'dlasv2': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1EC0>'
    'dlaswp': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *, int *)" at 0x0FDC1ED8>'
    'dlasy2': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1EF0>'
    'dlasyf': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1F08>'
    'dlat2s': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC1F20>'
    'dlatbs': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1F38>'
    'dlatdf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1F50>'
    'dlatps': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1F68>'
    'dlatrd': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1F80>'
    'dlatrs': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC1F98>'
    'dlatrz': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC1FB0>'
    'dlauu2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1FC8>'
    'dlauum': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC1FE0>'
    'dopgtr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3020>'
    'dopmtr': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3038>'
    'dorbdb': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3050>'
    'dorcsd': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC3068>'
    'dorg2l': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3080>'
    'dorg2r': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3098>'
    'dorgbr': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC30B0>'
    'dorghr': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC30C8>'
    'dorgl2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC30E0>'
    'dorglq': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC30F8>'
    'dorgql': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3110>'
    'dorgqr': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3128>'
    'dorgr2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3140>'
    'dorgrq': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3158>'
    'dorgtr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3170>'
    'dorm2l': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3188>'
    'dorm2r': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC31A0>'
    'dormbr': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC31B8>'
    'dormhr': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC31D0>'
    'dorml2': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC31E8>'
    'dormlq': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3200>'
    'dormql': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3218>'
    'dormqr': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3230>'
    'dormr2': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3248>'
    'dormr3': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3260>'
    'dormrq': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3278>'
    'dormrz': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3290>'
    'dormtr': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC32A8>'
    'dpbcon': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC32C0>'
    'dpbequ': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC32D8>'
    'dpbrfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC32F0>'
    'dpbstf': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3308>'
    'dpbsv': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3320>'
    'dpbsvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3338>'
    'dpbtf2': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3350>'
    'dpbtrf': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3368>'
    'dpbtrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3380>'
    'dpftrf': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3398>'
    'dpftri': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC33B0>'
    'dpftrs': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC33C8>'
    'dpocon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC33E0>'
    'dpoequ': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC33F8>'
    'dpoequb': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3410>'
    'dporfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3428>'
    'dposv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3440>'
    'dposvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3458>'
    'dpotf2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3470>'
    'dpotrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3488>'
    'dpotri': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC34A0>'
    'dpotrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC34B8>'
    'dppcon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC34D0>'
    'dppequ': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC34E8>'
    'dpprfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3500>'
    'dppsv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3518>'
    'dppsvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3530>'
    'dpptrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3548>'
    'dpptri': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3560>'
    'dpptrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3578>'
    'dpstf2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3590>'
    'dpstrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC35A8>'
    'dptcon': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC35C0>'
    'dpteqr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC35D8>'
    'dptrfs': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC35F0>'
    'dptsv': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3608>'
    'dptsvx': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3620>'
    'dpttrf': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3638>'
    'dpttrs': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3650>'
    'dptts2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3668>'
    'drscl': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3680>'
    'dsbev': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3698>'
    'dsbevd': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC36B0>'
    'dsbevx': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC36C8>'
    'dsbgst': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC36E0>'
    'dsbgv': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC36F8>'
    'dsbgvd': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC3710>'
    'dsbgvx': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC3728>'
    'dsbtrd': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3740>'
    'dsfrk': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC3758>'
    'dsgesv': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC3770>'
    'dspcon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3788>'
    'dspev': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC37A0>'
    'dspevd': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC37B8>'
    'dspevx': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC37D0>'
    'dspgst': None, # (!) real value is '<capsule object "void (int *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC37E8>'
    'dspgv': None, # (!) real value is '<capsule object "void (int *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3800>'
    'dspgvd': None, # (!) real value is '<capsule object "void (int *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC3818>'
    'dspgvx': None, # (!) real value is '<capsule object "void (int *, char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC3830>'
    'dsposv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC3848>'
    'dsprfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3860>'
    'dspsv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3878>'
    'dspsvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3890>'
    'dsptrd': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC38A8>'
    'dsptrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC38C0>'
    'dsptri': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC38D8>'
    'dsptrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC38F0>'
    'dstebz': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3908>'
    'dstedc': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC3920>'
    'dstegr': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC3938>'
    'dstein': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC3950>'
    'dstemr': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC3968>'
    'dsteqr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3980>'
    'dsterf': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3998>'
    'dstev': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC39B0>'
    'dstevd': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC39C8>'
    'dstevr': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC39E0>'
    'dstevx': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC39F8>'
    'dsycon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3A10>'
    'dsyconv': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3A28>'
    'dsyequb': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3A40>'
    'dsyev': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3A58>'
    'dsyevd': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC3A70>'
    'dsyevr': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC3A88>'
    'dsyevx': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC3AA0>'
    'dsygs2': None, # (!) real value is '<capsule object "void (int *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3AB8>'
    'dsygst': None, # (!) real value is '<capsule object "void (int *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3AD0>'
    'dsygv': None, # (!) real value is '<capsule object "void (int *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3AE8>'
    'dsygvd': None, # (!) real value is '<capsule object "void (int *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC3B00>'
    'dsygvx': None, # (!) real value is '<capsule object "void (int *, char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC3B18>'
    'dsyrfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3B30>'
    'dsysv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3B48>'
    'dsysvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC3B60>'
    'dsyswapr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC3B78>'
    'dsytd2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3B90>'
    'dsytf2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC3BA8>'
    'dsytrd': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3BC0>'
    'dsytrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3BD8>'
    'dsytri': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3BF0>'
    'dsytri2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3C08>'
    'dsytri2x': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3C20>'
    'dsytrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3C38>'
    'dsytrs2': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3C50>'
    'dtbcon': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3C68>'
    'dtbrfs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3C80>'
    'dtbtrs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3C98>'
    'dtfsm': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3CB0>'
    'dtftri': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3CC8>'
    'dtfttp': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3CE0>'
    'dtfttr': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3CF8>'
    'dtgevc': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3D10>'
    'dtgex2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3D28>'
    'dtgexc': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3D40>'
    'dtgsen': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC3D58>'
    'dtgsja': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3D70>'
    'dtgsna': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC3D88>'
    'dtgsy2': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC3DA0>'
    'dtgsyl': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC3DB8>'
    'dtpcon': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3DD0>'
    'dtpmqrt': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3DE8>'
    'dtpqrt': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3E00>'
    'dtpqrt2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3E18>'
    'dtprfb': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3E30>'
    'dtprfs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3E48>'
    'dtptri': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3E60>'
    'dtptrs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3E78>'
    'dtpttf': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3E90>'
    'dtpttr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3EA8>'
    'dtrcon': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3EC0>'
    'dtrevc': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3ED8>'
    'dtrexc': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3EF0>'
    'dtrrfs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3F08>'
    'dtrsen': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC3F20>'
    'dtrsna': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC3F38>'
    'dtrsyl': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3F50>'
    'dtrti2': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3F68>'
    'dtrtri': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3F80>'
    'dtrtrs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3F98>'
    'dtrttf': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3FB0>'
    'dtrttp': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC3FC8>'
    'dtzrzf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC3FE0>'
    'dzsum1': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (int *, __pyx_t_double_complex *, int *)" at 0x0FDC5020>'
    'icmax1': None, # (!) real value is '<capsule object "int (int *, __pyx_t_float_complex *, int *)" at 0x0FDC5038>'
    'ieeeck': None, # (!) real value is '<capsule object "int (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5050>'
    'ilaclc': None, # (!) real value is '<capsule object "int (int *, int *, __pyx_t_float_complex *, int *)" at 0x0FDC5068>'
    'ilaclr': None, # (!) real value is '<capsule object "int (int *, int *, __pyx_t_float_complex *, int *)" at 0x0FDC5080>'
    'iladiag': None, # (!) real value is '<capsule object "int (char *)" at 0x0FDC5098>'
    'iladlc': None, # (!) real value is '<capsule object "int (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC50B0>'
    'iladlr': None, # (!) real value is '<capsule object "int (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC50C8>'
    'ilaprec': None, # (!) real value is '<capsule object "int (char *)" at 0x0FDC50E0>'
    'ilaslc': None, # (!) real value is '<capsule object "int (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC50F8>'
    'ilaslr': None, # (!) real value is '<capsule object "int (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5110>'
    'ilatrans': None, # (!) real value is '<capsule object "int (char *)" at 0x0FDC5128>'
    'ilauplo': None, # (!) real value is '<capsule object "int (char *)" at 0x0FDC5140>'
    'ilaver': None, # (!) real value is '<capsule object "void (int *, int *, int *)" at 0x0FDC5158>'
    'ilazlc': None, # (!) real value is '<capsule object "int (int *, int *, __pyx_t_double_complex *, int *)" at 0x0FDC5170>'
    'ilazlr': None, # (!) real value is '<capsule object "int (int *, int *, __pyx_t_double_complex *, int *)" at 0x0FDC5188>'
    'izmax1': None, # (!) real value is '<capsule object "int (int *, __pyx_t_double_complex *, int *)" at 0x0FDC51A0>'
    'sbbcsd': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC51B8>'
    'sbdsdc': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC51D0>'
    'sbdsqr': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC51E8>'
    'scsum1': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (int *, __pyx_t_float_complex *, int *)" at 0x0FDC5200>'
    'sdisna': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5218>'
    'sgbbrd': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5230>'
    'sgbcon': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5248>'
    'sgbequ': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5260>'
    'sgbequb': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5278>'
    'sgbrfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5290>'
    'sgbsv': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC52A8>'
    'sgbsvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC52C0>'
    'sgbtf2': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC52D8>'
    'sgbtrf': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC52F0>'
    'sgbtrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5308>'
    'sgebak': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5320>'
    'sgebal': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5338>'
    'sgebd2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5350>'
    'sgebrd': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5368>'
    'sgecon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5380>'
    'sgeequ': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5398>'
    'sgeequb': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC53B0>'
    'sgees': None, # (!) real value is '<capsule object "void (char *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_sselect2 *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC53C8>'
    'sgeesx': None, # (!) real value is '<capsule object "void (char *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_sselect2 *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *, int *)" at 0x0FDC53E0>'
    'sgeev': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC53F8>'
    'sgeevx': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC5410>'
    'sgehd2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5428>'
    'sgehrd': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5440>'
    'sgejsv': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC5458>'
    'sgelq2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5470>'
    'sgelqf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5488>'
    'sgels': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC54A0>'
    'sgelsd': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC54B8>'
    'sgelss': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC54D0>'
    'sgelsy': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC54E8>'
    'sgemqrt': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5500>'
    'sgeql2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5518>'
    'sgeqlf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5530>'
    'sgeqp3': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5548>'
    'sgeqr2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5560>'
    'sgeqr2p': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5578>'
    'sgeqrf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5590>'
    'sgeqrfp': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC55A8>'
    'sgeqrt': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC55C0>'
    'sgeqrt2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC55D8>'
    'sgeqrt3': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC55F0>'
    'sgerfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5608>'
    'sgerq2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5620>'
    'sgerqf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5638>'
    'sgesc2': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5650>'
    'sgesdd': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC5668>'
    'sgesv': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5680>'
    'sgesvd': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5698>'
    'sgesvj': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC56B0>'
    'sgesvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC56C8>'
    'sgetc2': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDC56E0>'
    'sgetf2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC56F8>'
    'sgetrf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC5710>'
    'sgetri': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5728>'
    'sgetrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5740>'
    'sggbak': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5758>'
    'sggbal': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5770>'
    'sgges': None, # (!) real value is '<capsule object "void (char *, char *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_sselect3 *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC5788>'
    'sggesx': None, # (!) real value is '<capsule object "void (char *, char *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_sselect3 *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *, int *)" at 0x0FDC57A0>'
    'sggev': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC57B8>'
    'sggevx': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDC57D0>'
    'sggglm': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC57E8>'
    'sgghrd': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5800>'
    'sgglse': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5818>'
    'sggqrf': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5830>'
    'sggrqf': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5848>'
    'sgsvj0': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5860>'
    'sgsvj1': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5878>'
    'sgtcon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5890>'
    'sgtrfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC58A8>'
    'sgtsv': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC58C0>'
    'sgtsvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC58D8>'
    'sgttrf': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC58F0>'
    'sgttrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5908>'
    'sgtts2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5920>'
    'shgeqz': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5938>'
    'shsein': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC5950>'
    'shseqr': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5968>'
    'slabad': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5980>'
    'slabrd': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5998>'
    'slacn2': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC59B0>'
    'slacon': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC59C8>'
    'slacpy': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC59E0>'
    'sladiv': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC59F8>'
    'slae2': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5A10>'
    'slaebz': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5A28>'
    'slaed0': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5A40>'
    'slaed1': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5A58>'
    'slaed2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *, int *)" at 0x0FDC5A70>'
    'slaed3': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5A88>'
    'slaed4': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5AA0>'
    'slaed5': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5AB8>'
    'slaed6': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5AD0>'
    'slaed7': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5AE8>'
    'slaed8': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC5B00>'
    'slaed9': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5B18>'
    'slaeda': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5B30>'
    'slaein': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5B48>'
    'slaev2': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5B60>'
    'slaexc': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5B78>'
    'slag2': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5B90>'
    'slag2d': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC5BA8>'
    'slags2': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5BC0>'
    'slagtf': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5BD8>'
    'slagtm': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5BF0>'
    'slagts': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5C08>'
    'slagv2': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5C20>'
    'slahqr': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5C38>'
    'slahr2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5C50>'
    'slaic1': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5C68>'
    'slaln2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5C80>'
    'slals0': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5C98>'
    'slalsa': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5CB0>'
    'slalsd': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5CC8>'
    'slamch': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *)" at 0x0FDC5CE0>'
    'slamrg': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC5CF8>'
    'slangb': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5D10>'
    'slange': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5D28>'
    'slangt': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5D40>'
    'slanhs': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5D58>'
    'slansb': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5D70>'
    'slansf': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5D88>'
    'slansp': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5DA0>'
    'slanst': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5DB8>'
    'slansy': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5DD0>'
    'slantb': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5DE8>'
    'slantp': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5E00>'
    'slantr': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5E18>'
    'slanv2': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5E30>'
    'slapll': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5E48>'
    'slapmr': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5E60>'
    'slapmt': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5E78>'
    'slapy2': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (__pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5E90>'
    'slapy3': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_s (__pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5EA8>'
    'slaqgb': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, char *)" at 0x0FDC5EC0>'
    'slaqge': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, char *)" at 0x0FDC5ED8>'
    'slaqp2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5EF0>'
    'slaqps': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5F08>'
    'slaqr0': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5F20>'
    'slaqr1': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC5F38>'
    'slaqr2': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5F50>'
    'slaqr3': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5F68>'
    'slaqr4': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC5F80>'
    'slaqr5': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC5F98>'
    'slaqsb': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, char *)" at 0x0FDC5FB0>'
    'slaqsp': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, char *)" at 0x0FDC5FC8>'
    'slaqsy': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, char *)" at 0x0FDC5FE0>'
    'slaqtr': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7020>'
    'slar1v': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC7038>'
    'slar2v': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7050>'
    'slarf': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC7068>'
    'slarfb': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7080>'
    'slarfg': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC7098>'
    'slarfgp': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC70B0>'
    'slarft': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC70C8>'
    'slarfx': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC70E0>'
    'slargv': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC70F8>'
    'slarnv': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC7110>'
    'slarra': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC7128>'
    'slarrb': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7140>'
    'slarrc': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDC7158>'
    'slarrd': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7170>'
    'slarre': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7188>'
    'slarrf': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC71A0>'
    'slarrj': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC71B8>'
    'slarrk': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC71D0>'
    'slarrr': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC71E8>'
    'slarrv': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7200>'
    'slartg': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC7218>'
    'slartgp': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC7230>'
    'slartgs': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC7248>'
    'slartv': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7260>'
    'slaruv': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC7278>'
    'slarz': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC7290>'
    'slarzb': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC72A8>'
    'slarzt': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC72C0>'
    'slas2': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC72D8>'
    'slascl': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC72F0>'
    'slasd0': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7308>'
    'slasd1': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7320>'
    'slasd2': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *, int *, int *, int *)" at 0x0FDC7338>'
    'slasd3': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7350>'
    'slasd4': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7368>'
    'slasd5': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC7380>'
    'slasd6': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7398>'
    'slasd7': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC73B0>'
    'slasd8': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC73C8>'
    'slasda': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC73E0>'
    'slasdq': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC73F8>'
    'slasdt': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, int *, int *)" at 0x0FDC7410>'
    'slaset': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7428>'
    'slasq1': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7440>'
    'slasq2': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7458>'
    'slasq3': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC7470>'
    'slasq4': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC7488>'
    'slasq6': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC74A0>'
    'slasr': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC74B8>'
    'slasrt': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC74D0>'
    'slassq': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC74E8>'
    'slasv2': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC7500>'
    'slaswp': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *, int *)" at 0x0FDC7518>'
    'slasy2': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7530>'
    'slasyf': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7548>'
    'slatbs': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7560>'
    'slatdf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7578>'
    'slatps': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7590>'
    'slatrd': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC75A8>'
    'slatrs': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC75C0>'
    'slatrz': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC75D8>'
    'slauu2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC75F0>'
    'slauum': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7608>'
    'sopgtr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7620>'
    'sopmtr': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7638>'
    'sorbdb': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7650>'
    'sorcsd': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC7668>'
    'sorg2l': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7680>'
    'sorg2r': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7698>'
    'sorgbr': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC76B0>'
    'sorghr': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC76C8>'
    'sorgl2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC76E0>'
    'sorglq': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC76F8>'
    'sorgql': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7710>'
    'sorgqr': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7728>'
    'sorgr2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7740>'
    'sorgrq': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7758>'
    'sorgtr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7770>'
    'sorm2l': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7788>'
    'sorm2r': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC77A0>'
    'sormbr': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC77B8>'
    'sormhr': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC77D0>'
    'sorml2': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC77E8>'
    'sormlq': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7800>'
    'sormql': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7818>'
    'sormqr': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7830>'
    'sormr2': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7848>'
    'sormr3': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7860>'
    'sormrq': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7878>'
    'sormrz': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7890>'
    'sormtr': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC78A8>'
    'spbcon': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC78C0>'
    'spbequ': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC78D8>'
    'spbrfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC78F0>'
    'spbstf': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7908>'
    'spbsv': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7920>'
    'spbsvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7938>'
    'spbtf2': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7950>'
    'spbtrf': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7968>'
    'spbtrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7980>'
    'spftrf': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7998>'
    'spftri': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC79B0>'
    'spftrs': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC79C8>'
    'spocon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC79E0>'
    'spoequ': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC79F8>'
    'spoequb': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7A10>'
    'sporfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7A28>'
    'sposv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7A40>'
    'sposvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7A58>'
    'spotf2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7A70>'
    'spotrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7A88>'
    'spotri': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7AA0>'
    'spotrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7AB8>'
    'sppcon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7AD0>'
    'sppequ': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7AE8>'
    'spprfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7B00>'
    'sppsv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7B18>'
    'sppsvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7B30>'
    'spptrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7B48>'
    'spptri': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7B60>'
    'spptrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7B78>'
    'spstf2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7B90>'
    'spstrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7BA8>'
    'sptcon': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7BC0>'
    'spteqr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7BD8>'
    'sptrfs': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7BF0>'
    'sptsv': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7C08>'
    'sptsvx': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7C20>'
    'spttrf': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7C38>'
    'spttrs': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7C50>'
    'sptts2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7C68>'
    'srscl': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7C80>'
    'ssbev': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7C98>'
    'ssbevd': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDC7CB0>'
    'ssbevx': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC7CC8>'
    'ssbgst': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7CE0>'
    'ssbgv': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7CF8>'
    'ssbgvd': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDC7D10>'
    'ssbgvx': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC7D28>'
    'ssbtrd': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7D40>'
    'ssfrk': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *)" at 0x0FDC7D58>'
    'sspcon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7D70>'
    'sspev': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7D88>'
    'sspevd': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDC7DA0>'
    'sspevx': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC7DB8>'
    'sspgst': None, # (!) real value is '<capsule object "void (int *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7DD0>'
    'sspgv': None, # (!) real value is '<capsule object "void (int *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7DE8>'
    'sspgvd': None, # (!) real value is '<capsule object "void (int *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDC7E00>'
    'sspgvx': None, # (!) real value is '<capsule object "void (int *, char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC7E18>'
    'ssprfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7E30>'
    'sspsv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7E48>'
    'sspsvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7E60>'
    'ssptrd': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7E78>'
    'ssptrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7E90>'
    'ssptri': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7EA8>'
    'ssptrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7EC0>'
    'sstebz': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7ED8>'
    'sstedc': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDC7EF0>'
    'sstegr': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDC7F08>'
    'sstein': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC7F20>'
    'sstemr': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDC7F38>'
    'ssteqr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7F50>'
    'ssterf': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7F68>'
    'sstev': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC7F80>'
    'sstevd': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDC7F98>'
    'sstevr': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDC7FB0>'
    'sstevx': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC7FC8>'
    'ssycon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC7FE0>'
    'ssyconv': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC9020>'
    'ssyequb': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC9038>'
    'ssyev': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC9050>'
    'ssyevd': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDC9068>'
    'ssyevr': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDC9080>'
    'ssyevx': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDC9098>'
    'ssygs2': None, # (!) real value is '<capsule object "void (int *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC90B0>'
    'ssygst': None, # (!) real value is '<capsule object "void (int *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC90C8>'
    'ssygv': None, # (!) real value is '<capsule object "void (int *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC90E0>'
    'ssygvd': None, # (!) real value is '<capsule object "void (int *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDC90F8>'
    'ssygvx': None, # (!) real value is '<capsule object "void (int *, char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDC9110>'
    'ssyrfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC9128>'
    'ssysv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC9140>'
    'ssysvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC9158>'
    'ssyswapr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC9170>'
    'ssytd2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC9188>'
    'ssytf2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC91A0>'
    'ssytrd': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC91B8>'
    'ssytrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC91D0>'
    'ssytri': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC91E8>'
    'ssytri2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC9200>'
    'ssytri2x': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC9218>'
    'ssytrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC9230>'
    'ssytrs2': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC9248>'
    'stbcon': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC9260>'
    'stbrfs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC9278>'
    'stbtrs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC9290>'
    'stfsm': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC92A8>'
    'stftri': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC92C0>'
    'stfttp': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC92D8>'
    'stfttr': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC92F0>'
    'stgevc': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC9308>'
    'stgex2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC9320>'
    'stgexc': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC9338>'
    'stgsen': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDC9350>'
    'stgsja': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC9368>'
    'stgsna': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC9380>'
    'stgsy2': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC9398>'
    'stgsyl': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC93B0>'
    'stpcon': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC93C8>'
    'stpmqrt': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC93E0>'
    'stpqrt': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC93F8>'
    'stpqrt2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC9410>'
    'stprfb': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC9428>'
    'stprfs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC9440>'
    'stptri': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC9458>'
    'stptrs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC9470>'
    'stpttf': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC9488>'
    'stpttr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC94A0>'
    'strcon': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC94B8>'
    'strevc': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC94D0>'
    'strexc': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC94E8>'
    'strrfs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC9500>'
    'strsen': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *, int *)" at 0x0FDC9518>'
    'strsna': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *, int *)" at 0x0FDC9530>'
    'strsyl': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC9548>'
    'strti2': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC9560>'
    'strtri': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC9578>'
    'strtrs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC9590>'
    'strttf': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC95A8>'
    'strttp': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *)" at 0x0FDC95C0>'
    'stzrzf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, __pyx_t_5scipy_6linalg_13cython_lapack_s *, int *, int *)" at 0x0FDC95D8>'
    'xerbla_array': None, # (!) real value is '<capsule object "void (char *, int *, int *)" at 0x0FDC95F0>'
    'zbbcsd': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC9608>'
    'zbdsqr': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9620>'
    'zcgesv': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC9638>'
    'zcposv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC9650>'
    'zdrscl': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *)" at 0x0FDC9668>'
    'zgbbrd': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9680>'
    'zgbcon': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9698>'
    'zgbequ': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC96B0>'
    'zgbequb': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC96C8>'
    'zgbrfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC96E0>'
    'zgbsv': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC96F8>'
    'zgbsvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9710>'
    'zgbtf2': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_double_complex *, int *, int *, int *)" at 0x0FDC9728>'
    'zgbtrf': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_double_complex *, int *, int *, int *)" at 0x0FDC9740>'
    'zgbtrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9758>'
    'zgebak': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9770>'
    'zgebal': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9788>'
    'zgebd2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0FDC97A0>'
    'zgebrd': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC97B8>'
    'zgecon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC97D0>'
    'zgeequ': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC97E8>'
    'zgeequb': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9800>'
    'zgees': None, # (!) real value is '<capsule object "void (char *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_zselect1 *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC9818>'
    'zgeesx': None, # (!) real value is '<capsule object "void (char *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_zselect1 *, char *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC9830>'
    'zgeev': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9848>'
    'zgeevx': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9860>'
    'zgehd2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0FDC9878>'
    'zgehrd': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9890>'
    'zgelq2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0FDC98A8>'
    'zgelqf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC98C0>'
    'zgels': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC98D8>'
    'zgelsd': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC98F0>'
    'zgelss': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9908>'
    'zgelsy': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9920>'
    'zgemqrt': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDC9938>'
    'zgeql2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0FDC9950>'
    'zgeqlf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9968>'
    'zgeqp3': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9980>'
    'zgeqr2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0FDC9998>'
    'zgeqr2p': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0FDC99B0>'
    'zgeqrf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC99C8>'
    'zgeqrfp': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC99E0>'
    'zgeqrt': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDC99F8>'
    'zgeqrt2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9A10>'
    'zgeqrt3': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9A28>'
    'zgerfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9A40>'
    'zgerq2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0FDC9A58>'
    'zgerqf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9A70>'
    'zgesc2': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDC9A88>'
    'zgesdd': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC9AA0>'
    'zgesv': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9AB8>'
    'zgesvd': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9AD0>'
    'zgesvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9AE8>'
    'zgetc2': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, int *, int *, int *, int *)" at 0x0FDC9B00>'
    'zgetf2': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, int *, int *)" at 0x0FDC9B18>'
    'zgetrf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, int *, int *)" at 0x0FDC9B30>'
    'zgetri': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9B48>'
    'zgetrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9B60>'
    'zggbak': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9B78>'
    'zggbal': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9B90>'
    'zgges': None, # (!) real value is '<capsule object "void (char *, char *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_zselect2 *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDC9BA8>'
    'zggesx': None, # (!) real value is '<capsule object "void (char *, char *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_zselect2 *, char *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC9BC0>'
    'zggev': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9BD8>'
    'zggevx': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC9BF0>'
    'zggglm': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9C08>'
    'zgghrd': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9C20>'
    'zgglse': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9C38>'
    'zggqrf': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9C50>'
    'zggrqf': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9C68>'
    'zgtcon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *)" at 0x0FDC9C80>'
    'zgtrfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9C98>'
    'zgtsv': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9CB0>'
    'zgtsvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9CC8>'
    'zgttrf': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9CE0>'
    'zgttrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9CF8>'
    'zgtts2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDC9D10>'
    'zhbev': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9D28>'
    'zhbevd': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC9D40>'
    'zhbevx': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC9D58>'
    'zhbgst': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9D70>'
    'zhbgv': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9D88>'
    'zhbgvd': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC9DA0>'
    'zhbgvx': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC9DB8>'
    'zhbtrd': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDC9DD0>'
    'zhecon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *)" at 0x0FDC9DE8>'
    'zheequb': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *)" at 0x0FDC9E00>'
    'zheev': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9E18>'
    'zheevd': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC9E30>'
    'zheevr': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC9E48>'
    'zheevx': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC9E60>'
    'zhegs2': None, # (!) real value is '<capsule object "void (int *, char *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9E78>'
    'zhegst': None, # (!) real value is '<capsule object "void (int *, char *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9E90>'
    'zhegv': None, # (!) real value is '<capsule object "void (int *, char *, char *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9EA8>'
    'zhegvd': None, # (!) real value is '<capsule object "void (int *, char *, char *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDC9EC0>'
    'zhegvx': None, # (!) real value is '<capsule object "void (int *, char *, char *, char *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDC9ED8>'
    'zherfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9EF0>'
    'zhesv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9F08>'
    'zhesvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDC9F20>'
    'zheswapr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *, int *)" at 0x0FDC9F38>'
    'zhetd2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *)" at 0x0FDC9F50>'
    'zhetf2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *, int *)" at 0x0FDC9F68>'
    'zhetrd': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9F80>'
    'zhetrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9F98>'
    'zhetri': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *)" at 0x0FDC9FB0>'
    'zhetri2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9FC8>'
    'zhetri2x': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDC9FE0>'
    'zhetrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCB020>'
    'zhetrs2': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCB038>'
    'zhfrk': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *)" at 0x0FDCB050>'
    'zhgeqz': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCB068>'
    'zhpcon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *)" at 0x0FDCB080>'
    'zhpev': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCB098>'
    'zhpevd': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDCB0B0>'
    'zhpevx': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDCB0C8>'
    'zhpgst': None, # (!) real value is '<capsule object "void (int *, char *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0FDCB0E0>'
    'zhpgv': None, # (!) real value is '<capsule object "void (int *, char *, char *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCB0F8>'
    'zhpgvd': None, # (!) real value is '<capsule object "void (int *, char *, char *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDCB110>'
    'zhpgvx': None, # (!) real value is '<capsule object "void (int *, char *, char *, char *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDCB128>'
    'zhprfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCB140>'
    'zhpsv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCB158>'
    'zhpsvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCB170>'
    'zhptrd': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *)" at 0x0FDCB188>'
    'zhptrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCB1A0>'
    'zhptri': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCB1B8>'
    'zhptrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCB1D0>'
    'zhsein': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDCB1E8>'
    'zhseqr': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCB200>'
    'zlabrd': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCB218>'
    'zlacgv': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, int *)" at 0x0FDCB230>'
    'zlacn2': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDCB248>'
    'zlacon': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCB260>'
    'zlacp2': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCB278>'
    'zlacpy': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCB290>'
    'zlacrm': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDCB2A8>'
    'zlacrt': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *)" at 0x0FDCB2C0>'
    'zladiv': None, # (!) real value is '<capsule object "__pyx_t_double_complex (__pyx_t_double_complex *, __pyx_t_double_complex *)" at 0x0FDCB2D8>'
    'zlaed0': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDCB2F0>'
    'zlaed7': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDCB308>'
    'zlaed8': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCB320>'
    'zlaein': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCB338>'
    'zlaesy': None, # (!) real value is '<capsule object "void (__pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *)" at 0x0FDCB350>'
    'zlaev2': None, # (!) real value is '<capsule object "void (__pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *)" at 0x0FDCB368>'
    'zlag2c': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDCB380>'
    'zlags2': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *)" at 0x0FDCB398>'
    'zlagtm': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *)" at 0x0FDCB3B0>'
    'zlahef': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCB3C8>'
    'zlahqr': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCB3E0>'
    'zlahr2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCB3F8>'
    'zlaic1': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_double_complex *)" at 0x0FDCB410>'
    'zlals0': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCB428>'
    'zlalsa': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDCB440>'
    'zlalsd': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDCB458>'
    'zlangb': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDCB470>'
    'zlange': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDCB488>'
    'zlangt': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *)" at 0x0FDCB4A0>'
    'zlanhb': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDCB4B8>'
    'zlanhe': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, char *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDCB4D0>'
    'zlanhf': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, char *, char *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDCB4E8>'
    'zlanhp': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, char *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDCB500>'
    'zlanhs': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDCB518>'
    'zlanht': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *)" at 0x0FDCB530>'
    'zlansb': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDCB548>'
    'zlansp': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, char *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDCB560>'
    'zlansy': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, char *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDCB578>'
    'zlantb': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDCB590>'
    'zlantp': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, char *, char *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDCB5A8>'
    'zlantr': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_13cython_lapack_d (char *, char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDCB5C0>'
    'zlapll': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDCB5D8>'
    'zlapmr': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCB5F0>'
    'zlapmt': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCB608>'
    'zlaqgb': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, char *)" at 0x0FDCB620>'
    'zlaqge': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, char *)" at 0x0FDCB638>'
    'zlaqhb': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, char *)" at 0x0FDCB650>'
    'zlaqhe': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, char *)" at 0x0FDCB668>'
    'zlaqhp': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, char *)" at 0x0FDCB680>'
    'zlaqp2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *)" at 0x0FDCB698>'
    'zlaqps': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0FDCB6B0>'
    'zlaqr0': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCB6C8>'
    'zlaqr1': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *)" at 0x0FDCB6E0>'
    'zlaqr2': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, int *, __pyx_t_double_complex *, int *, int *, int *, __pyx_t_double_complex *, int *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCB6F8>'
    'zlaqr3': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, int *, __pyx_t_double_complex *, int *, int *, int *, __pyx_t_double_complex *, int *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCB710>'
    'zlaqr4': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCB728>'
    'zlaqr5': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCB740>'
    'zlaqsb': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, char *)" at 0x0FDCB758>'
    'zlaqsp': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, char *)" at 0x0FDCB770>'
    'zlaqsy': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, char *)" at 0x0FDCB788>'
    'zlar1v': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDCB7A0>'
    'zlar2v': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *)" at 0x0FDCB7B8>'
    'zlarcm': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDCB7D0>'
    'zlarf': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *)" at 0x0FDCB7E8>'
    'zlarfb': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCB800>'
    'zlarfg': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *)" at 0x0FDCB818>'
    'zlarfgp': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *)" at 0x0FDCB830>'
    'zlarft': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0FDCB848>'
    'zlarfx': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *)" at 0x0FDCB860>'
    'zlargv': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCB878>'
    'zlarnv': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *)" at 0x0FDCB890>'
    'zlarrv': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDCB8A8>'
    'zlartg': None, # (!) real value is '<capsule object "void (__pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_double_complex *)" at 0x0FDCB8C0>'
    'zlartv': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *)" at 0x0FDCB8D8>'
    'zlarz': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *)" at 0x0FDCB8F0>'
    'zlarzb': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCB908>'
    'zlarzt': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0FDCB920>'
    'zlascl': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCB938>'
    'zlaset': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0FDCB950>'
    'zlasr': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *)" at 0x0FDCB968>'
    'zlassq': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *)" at 0x0FDCB980>'
    'zlaswp': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, int *, int *, int *, int *, int *)" at 0x0FDCB998>'
    'zlasyf': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCB9B0>'
    'zlat2c': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, __pyx_t_float_complex *, int *, int *)" at 0x0FDCB9C8>'
    'zlatbs': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCB9E0>'
    'zlatdf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *)" at 0x0FDCB9F8>'
    'zlatps': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBA10>'
    'zlatrd': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0FDCBA28>'
    'zlatrs': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBA40>'
    'zlatrz': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *)" at 0x0FDCBA58>'
    'zlauu2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCBA70>'
    'zlauum': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCBA88>'
    'zpbcon': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBAA0>'
    'zpbequ': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBAB8>'
    'zpbrfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBAD0>'
    'zpbstf': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCBAE8>'
    'zpbsv': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCBB00>'
    'zpbsvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBB18>'
    'zpbtf2': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCBB30>'
    'zpbtrf': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCBB48>'
    'zpbtrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCBB60>'
    'zpftrf': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCBB78>'
    'zpftri': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCBB90>'
    'zpftrs': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCBBA8>'
    'zpocon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBBC0>'
    'zpoequ': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBBD8>'
    'zpoequb': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBBF0>'
    'zporfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBC08>'
    'zposv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCBC20>'
    'zposvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBC38>'
    'zpotf2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCBC50>'
    'zpotrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCBC68>'
    'zpotri': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCBC80>'
    'zpotrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCBC98>'
    'zppcon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBCB0>'
    'zppequ': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBCC8>'
    'zpprfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBCE0>'
    'zppsv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCBCF8>'
    'zppsvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, char *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBD10>'
    'zpptrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCBD28>'
    'zpptri': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCBD40>'
    'zpptrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCBD58>'
    'zpstf2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBD70>'
    'zpstrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBD88>'
    'zptcon': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBDA0>'
    'zpteqr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBDB8>'
    'zptrfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBDD0>'
    'zptsv': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCBDE8>'
    'zptsvx': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBE00>'
    'zpttrf': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *)" at 0x0FDCBE18>'
    'zpttrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCBE30>'
    'zptts2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0FDCBE48>'
    'zrot': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *)" at 0x0FDCBE60>'
    'zspcon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *)" at 0x0FDCBE78>'
    'zspmv': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0FDCBE90>'
    'zspr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *)" at 0x0FDCBEA8>'
    'zsprfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBEC0>'
    'zspsv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCBED8>'
    'zspsvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBEF0>'
    'zsptrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCBF08>'
    'zsptri': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCBF20>'
    'zsptrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCBF38>'
    'zstedc': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDCBF50>'
    'zstegr': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDCBF68>'
    'zstein': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDCBF80>'
    'zstemr': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *, int *)" at 0x0FDCBF98>'
    'zsteqr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCBFB0>'
    'zsycon': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *)" at 0x0FDCBFC8>'
    'zsyconv': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCBFE0>'
    'zsyequb': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *)" at 0x0FDCD020>'
    'zsymv': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0FDCD038>'
    'zsyr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCD050>'
    'zsyrfs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCD068>'
    'zsysv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD080>'
    'zsysvx': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCD098>'
    'zsyswapr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *, int *)" at 0x0FDCD0B0>'
    'zsytf2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *, int *)" at 0x0FDCD0C8>'
    'zsytrf': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD0E0>'
    'zsytri': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCD0F8>'
    'zsytri2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD110>'
    'zsytri2x': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD128>'
    'zsytrs': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD140>'
    'zsytrs2': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCD158>'
    'ztbcon': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCD170>'
    'ztbrfs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCD188>'
    'ztbtrs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD1A0>'
    'ztfsm': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0FDCD1B8>'
    'ztftri': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCD1D0>'
    'ztfttp': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0FDCD1E8>'
    'ztfttr': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD200>'
    'ztgevc': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCD218>'
    'ztgex2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, int *)" at 0x0FDCD230>'
    'ztgexc': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, int *, int *)" at 0x0FDCD248>'
    'ztgsen': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, int *, int *, int *)" at 0x0FDCD260>'
    'ztgsja': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD278>'
    'ztgsna': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_double_complex *, int *, int *, int *)" at 0x0FDCD290>'
    'ztgsy2': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCD2A8>'
    'ztgsyl': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, int *, int *)" at 0x0FDCD2C0>'
    'ztpcon': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCD2D8>'
    'ztpmqrt': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCD2F0>'
    'ztpqrt': None, # (!) real value is '<capsule object "void (int *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCD308>'
    'ztpqrt2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD320>'
    'ztprfb': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCD338>'
    'ztprfs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCD350>'
    'ztptri': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCD368>'
    'ztptrs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD380>'
    'ztpttf': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0FDCD398>'
    'ztpttr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD3B0>'
    'ztrcon': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCD3C8>'
    'ztrevc': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, int *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCD3E0>'
    'ztrexc': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *, int *, int *)" at 0x0FDCD3F8>'
    'ztrrfs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCD410>'
    'ztrsen': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD428>'
    'ztrsna': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCD440>'
    'ztrsyl': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *)" at 0x0FDCD458>'
    'ztrti2': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD470>'
    'ztrtri': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD488>'
    'ztrtrs': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD4A0>'
    'ztrttf': None, # (!) real value is '<capsule object "void (char *, char *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCD4B8>'
    'ztrttp': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCD4D0>'
    'ztzrzf': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD4E8>'
    'zunbdb': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD500>'
    'zuncsd': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, char *, char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_13cython_lapack_d *, int *, int *, int *)" at 0x0FDCD518>'
    'zung2l': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0FDCD530>'
    'zung2r': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0FDCD548>'
    'zungbr': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD560>'
    'zunghr': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD578>'
    'zungl2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0FDCD590>'
    'zunglq': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD5A8>'
    'zungql': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD5C0>'
    'zungqr': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD5D8>'
    'zungr2': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0FDCD5F0>'
    'zungrq': None, # (!) real value is '<capsule object "void (int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD608>'
    'zungtr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD620>'
    'zunm2l': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCD638>'
    'zunm2r': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCD650>'
    'zunmbr': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD668>'
    'zunmhr': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD680>'
    'zunml2': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCD698>'
    'zunmlq': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD6B0>'
    'zunmql': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD6C8>'
    'zunmqr': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD6E0>'
    'zunmr2': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCD6F8>'
    'zunmr3': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCD710>'
    'zunmrq': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD728>'
    'zunmrz': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD740>'
    'zunmtr': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, int *)" at 0x0FDCD758>'
    'zupgtr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCD770>'
    'zupmtr': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0FDCD788>'
}

__spec__ = None # (!) real value is "ModuleSpec(name='scipy.linalg.cython_lapack', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x0FDB7C30>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\scipy\\\\linalg\\\\cython_lapack.cp37-win32.pyd')"

__test__ = {}

