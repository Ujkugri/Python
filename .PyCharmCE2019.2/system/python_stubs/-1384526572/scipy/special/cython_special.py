# encoding: utf-8
# module scipy.special.cython_special
# from C:\Program Files (x86)\Python37-32\lib\site-packages\scipy\special\cython_special.cp37-win32.pyd
# by generator 1.147
"""
.. highlight:: cython

Cython API for Special Functions
================================

Scalar, typed versions of many of the functions in ``scipy.special``
can be accessed directly from Cython; the complete list is given
below. Functions are overloaded using Cython fused types so their
names match their ufunc counterpart. The module follows the following
conventions:

- If a function's ufunc counterpart returns multiple values, then the
  function returns its outputs via pointers in the final arguments
- If a function's ufunc counterpart returns a single value, then the
  function's output is returned directly.

The module is usable from Cython via::

    cimport scipy.special.cython_special

Error Handling
--------------

Functions can indicate an error by returning ``nan``; however they
cannot emit warnings like their counterparts in ``scipy.special``.

Available Functions
-------------------

- :py:func:`~scipy.special.agm`::

        double agm(double, double)

- :py:func:`~scipy.special.airy`::

        void airy(double, double *, double *, double *, double *)
        void airy(double complex, double complex *, double complex *, double complex *, double complex *)

- :py:func:`~scipy.special.airye`::

        void airye(double complex, double complex *, double complex *, double complex *, double complex *)
        void airye(double, double *, double *, double *, double *)

- :py:func:`~scipy.special.bdtr`::

        double bdtr(double, double, double)
        double bdtr(long, long, double)

- :py:func:`~scipy.special.bdtrc`::

        double bdtrc(double, double, double)
        double bdtrc(long, long, double)

- :py:func:`~scipy.special.bdtri`::

        double bdtri(double, double, double)
        double bdtri(long, long, double)

- :py:func:`~scipy.special.bdtrik`::

        double bdtrik(double, double, double)

- :py:func:`~scipy.special.bdtrin`::

        double bdtrin(double, double, double)

- :py:func:`~scipy.special.bei`::

        double bei(double)

- :py:func:`~scipy.special.beip`::

        double beip(double)

- :py:func:`~scipy.special.ber`::

        double ber(double)

- :py:func:`~scipy.special.berp`::

        double berp(double)

- :py:func:`~scipy.special.besselpoly`::

        double besselpoly(double, double, double)

- :py:func:`~scipy.special.beta`::

        double beta(double, double)

- :py:func:`~scipy.special.betainc`::

        double betainc(double, double, double)

- :py:func:`~scipy.special.betaincinv`::

        double betaincinv(double, double, double)

- :py:func:`~scipy.special.betaln`::

        double betaln(double, double)

- :py:func:`~scipy.special.binom`::

        double binom(double, double)

- :py:func:`~scipy.special.boxcox`::

        double boxcox(double, double)

- :py:func:`~scipy.special.boxcox1p`::

        double boxcox1p(double, double)

- :py:func:`~scipy.special.btdtr`::

        double btdtr(double, double, double)

- :py:func:`~scipy.special.btdtri`::

        double btdtri(double, double, double)

- :py:func:`~scipy.special.btdtria`::

        double btdtria(double, double, double)

- :py:func:`~scipy.special.btdtrib`::

        double btdtrib(double, double, double)

- :py:func:`~scipy.special.cbrt`::

        double cbrt(double)

- :py:func:`~scipy.special.chdtr`::

        double chdtr(double, double)

- :py:func:`~scipy.special.chdtrc`::

        double chdtrc(double, double)

- :py:func:`~scipy.special.chdtri`::

        double chdtri(double, double)

- :py:func:`~scipy.special.chdtriv`::

        double chdtriv(double, double)

- :py:func:`~scipy.special.chndtr`::

        double chndtr(double, double, double)

- :py:func:`~scipy.special.chndtridf`::

        double chndtridf(double, double, double)

- :py:func:`~scipy.special.chndtrinc`::

        double chndtrinc(double, double, double)

- :py:func:`~scipy.special.chndtrix`::

        double chndtrix(double, double, double)

- :py:func:`~scipy.special.cosdg`::

        double cosdg(double)

- :py:func:`~scipy.special.cosm1`::

        double cosm1(double)

- :py:func:`~scipy.special.cotdg`::

        double cotdg(double)

- :py:func:`~scipy.special.dawsn`::

        double dawsn(double)
        double complex dawsn(double complex)

- :py:func:`~scipy.special.ellipe`::

        double ellipe(double)

- :py:func:`~scipy.special.ellipeinc`::

        double ellipeinc(double, double)

- :py:func:`~scipy.special.ellipj`::

        void ellipj(double, double, double *, double *, double *, double *)

- :py:func:`~scipy.special.ellipkinc`::

        double ellipkinc(double, double)

- :py:func:`~scipy.special.ellipkm1`::

        double ellipkm1(double)

- :py:func:`~scipy.special.entr`::

        double entr(double)

- :py:func:`~scipy.special.erf`::

        double complex erf(double complex)
        double erf(double)

- :py:func:`~scipy.special.erfc`::

        double complex erfc(double complex)
        double erfc(double)

- :py:func:`~scipy.special.erfcx`::

        double erfcx(double)
        double complex erfcx(double complex)

- :py:func:`~scipy.special.erfi`::

        double erfi(double)
        double complex erfi(double complex)

- :py:func:`~scipy.special.eval_chebyc`::

        double complex eval_chebyc(double, double complex)
        double eval_chebyc(double, double)
        double eval_chebyc(long, double)

- :py:func:`~scipy.special.eval_chebys`::

        double complex eval_chebys(double, double complex)
        double eval_chebys(double, double)
        double eval_chebys(long, double)

- :py:func:`~scipy.special.eval_chebyt`::

        double complex eval_chebyt(double, double complex)
        double eval_chebyt(double, double)
        double eval_chebyt(long, double)

- :py:func:`~scipy.special.eval_chebyu`::

        double complex eval_chebyu(double, double complex)
        double eval_chebyu(double, double)
        double eval_chebyu(long, double)

- :py:func:`~scipy.special.eval_gegenbauer`::

        double complex eval_gegenbauer(double, double, double complex)
        double eval_gegenbauer(double, double, double)
        double eval_gegenbauer(long, double, double)

- :py:func:`~scipy.special.eval_genlaguerre`::

        double complex eval_genlaguerre(double, double, double complex)
        double eval_genlaguerre(double, double, double)
        double eval_genlaguerre(long, double, double)

- :py:func:`~scipy.special.eval_hermite`::

        double eval_hermite(long, double)

- :py:func:`~scipy.special.eval_hermitenorm`::

        double eval_hermitenorm(long, double)

- :py:func:`~scipy.special.eval_jacobi`::

        double complex eval_jacobi(double, double, double, double complex)
        double eval_jacobi(double, double, double, double)
        double eval_jacobi(long, double, double, double)

- :py:func:`~scipy.special.eval_laguerre`::

        double complex eval_laguerre(double, double complex)
        double eval_laguerre(double, double)
        double eval_laguerre(long, double)

- :py:func:`~scipy.special.eval_legendre`::

        double complex eval_legendre(double, double complex)
        double eval_legendre(double, double)
        double eval_legendre(long, double)

- :py:func:`~scipy.special.eval_sh_chebyt`::

        double complex eval_sh_chebyt(double, double complex)
        double eval_sh_chebyt(double, double)
        double eval_sh_chebyt(long, double)

- :py:func:`~scipy.special.eval_sh_chebyu`::

        double complex eval_sh_chebyu(double, double complex)
        double eval_sh_chebyu(double, double)
        double eval_sh_chebyu(long, double)

- :py:func:`~scipy.special.eval_sh_jacobi`::

        double complex eval_sh_jacobi(double, double, double, double complex)
        double eval_sh_jacobi(double, double, double, double)
        double eval_sh_jacobi(long, double, double, double)

- :py:func:`~scipy.special.eval_sh_legendre`::

        double complex eval_sh_legendre(double, double complex)
        double eval_sh_legendre(double, double)
        double eval_sh_legendre(long, double)

- :py:func:`~scipy.special.exp1`::

        double complex exp1(double complex)
        double exp1(double)

- :py:func:`~scipy.special.exp10`::

        double exp10(double)

- :py:func:`~scipy.special.exp2`::

        double exp2(double)

- :py:func:`~scipy.special.expi`::

        double complex expi(double complex)
        double expi(double)

- :py:func:`~scipy.special.expit`::

        double expit(double)
        float expit(float)
        long double expit(long double)

- :py:func:`~scipy.special.expm1`::

        double complex expm1(double complex)
        double expm1(double)

- :py:func:`~scipy.special.expn`::

        double expn(double, double)
        double expn(long, double)

- :py:func:`~scipy.special.exprel`::

        double exprel(double)

- :py:func:`~scipy.special.fdtr`::

        double fdtr(double, double, double)

- :py:func:`~scipy.special.fdtrc`::

        double fdtrc(double, double, double)

- :py:func:`~scipy.special.fdtri`::

        double fdtri(double, double, double)

- :py:func:`~scipy.special.fdtridfd`::

        double fdtridfd(double, double, double)

- :py:func:`~scipy.special.fresnel`::

        void fresnel(double, double *, double *)
        void fresnel(double complex, double complex *, double complex *)

- :py:func:`~scipy.special.gamma`::

        double complex gamma(double complex)
        double gamma(double)

- :py:func:`~scipy.special.gammainc`::

        double gammainc(double, double)

- :py:func:`~scipy.special.gammaincc`::

        double gammaincc(double, double)

- :py:func:`~scipy.special.gammainccinv`::

        double gammainccinv(double, double)

- :py:func:`~scipy.special.gammaincinv`::

        double gammaincinv(double, double)

- :py:func:`~scipy.special.gammaln`::

        double gammaln(double)

- :py:func:`~scipy.special.gammasgn`::

        double gammasgn(double)

- :py:func:`~scipy.special.gdtr`::

        double gdtr(double, double, double)

- :py:func:`~scipy.special.gdtrc`::

        double gdtrc(double, double, double)

- :py:func:`~scipy.special.gdtria`::

        double gdtria(double, double, double)

- :py:func:`~scipy.special.gdtrib`::

        double gdtrib(double, double, double)

- :py:func:`~scipy.special.gdtrix`::

        double gdtrix(double, double, double)

- :py:func:`~scipy.special.hankel1`::

        double complex hankel1(double, double complex)

- :py:func:`~scipy.special.hankel1e`::

        double complex hankel1e(double, double complex)

- :py:func:`~scipy.special.hankel2`::

        double complex hankel2(double, double complex)

- :py:func:`~scipy.special.hankel2e`::

        double complex hankel2e(double, double complex)

- :py:func:`~scipy.special.huber`::

        double huber(double, double)

- :py:func:`~scipy.special.hyp0f1`::

        double complex hyp0f1(double, double complex)
        double hyp0f1(double, double)

- :py:func:`~scipy.special.hyp1f1`::

        double complex hyp1f1(double, double, double complex)
        double hyp1f1(double, double, double)

- :py:func:`~scipy.special.hyp1f2`::

        void hyp1f2(double, double, double, double, double *, double *)

- :py:func:`~scipy.special.hyp2f0`::

        void hyp2f0(double, double, double, double, double *, double *)
        void hyp2f0(double, double, double, long, double *, double *)

- :py:func:`~scipy.special.hyp2f1`::

        double hyp2f1(double, double, double, double)
        double complex hyp2f1(double, double, double, double complex)

- :py:func:`~scipy.special.hyp3f0`::

        void hyp3f0(double, double, double, double, double *, double *)

- :py:func:`~scipy.special.hyperu`::

        double hyperu(double, double, double)

- :py:func:`~scipy.special.i0`::

        double i0(double)

- :py:func:`~scipy.special.i0e`::

        double i0e(double)

- :py:func:`~scipy.special.i1`::

        double i1(double)

- :py:func:`~scipy.special.i1e`::

        double i1e(double)

- :py:func:`~scipy.special.inv_boxcox`::

        double inv_boxcox(double, double)

- :py:func:`~scipy.special.inv_boxcox1p`::

        double inv_boxcox1p(double, double)

- :py:func:`~scipy.special.it2i0k0`::

        void it2i0k0(double, double *, double *)

- :py:func:`~scipy.special.it2j0y0`::

        void it2j0y0(double, double *, double *)

- :py:func:`~scipy.special.it2struve0`::

        double it2struve0(double)

- :py:func:`~scipy.special.itairy`::

        void itairy(double, double *, double *, double *, double *)

- :py:func:`~scipy.special.iti0k0`::

        void iti0k0(double, double *, double *)

- :py:func:`~scipy.special.itj0y0`::

        void itj0y0(double, double *, double *)

- :py:func:`~scipy.special.itmodstruve0`::

        double itmodstruve0(double)

- :py:func:`~scipy.special.itstruve0`::

        double itstruve0(double)

- :py:func:`~scipy.special.iv`::

        double complex iv(double, double complex)
        double iv(double, double)

- :py:func:`~scipy.special.ive`::

        double complex ive(double, double complex)
        double ive(double, double)

- :py:func:`~scipy.special.j0`::

        double j0(double)

- :py:func:`~scipy.special.j1`::

        double j1(double)

- :py:func:`~scipy.special.jv`::

        double complex jv(double, double complex)
        double jv(double, double)

- :py:func:`~scipy.special.jve`::

        double complex jve(double, double complex)
        double jve(double, double)

- :py:func:`~scipy.special.k0`::

        double k0(double)

- :py:func:`~scipy.special.k0e`::

        double k0e(double)

- :py:func:`~scipy.special.k1`::

        double k1(double)

- :py:func:`~scipy.special.k1e`::

        double k1e(double)

- :py:func:`~scipy.special.kei`::

        double kei(double)

- :py:func:`~scipy.special.keip`::

        double keip(double)

- :py:func:`~scipy.special.kelvin`::

        void kelvin(double, double complex *, double complex *, double complex *, double complex *)

- :py:func:`~scipy.special.ker`::

        double ker(double)

- :py:func:`~scipy.special.kerp`::

        double kerp(double)

- :py:func:`~scipy.special.kl_div`::

        double kl_div(double, double)

- :py:func:`~scipy.special.kn`::

        double kn(double, double)
        double kn(long, double)

- :py:func:`~scipy.special.kolmogi`::

        double kolmogi(double)

- :py:func:`~scipy.special.kolmogorov`::

        double kolmogorov(double)

- :py:func:`~scipy.special.kv`::

        double complex kv(double, double complex)
        double kv(double, double)

- :py:func:`~scipy.special.kve`::

        double complex kve(double, double complex)
        double kve(double, double)

- :py:func:`~scipy.special.log1p`::

        double complex log1p(double complex)
        double log1p(double)

- :py:func:`~scipy.special.log_ndtr`::

        double complex log_ndtr(double complex)
        double log_ndtr(double)

- :py:func:`~scipy.special.loggamma`::

        double loggamma(double)
        double complex loggamma(double complex)

- :py:func:`~scipy.special.logit`::

        double logit(double)
        float logit(float)
        long double logit(long double)

- :py:func:`~scipy.special.lpmv`::

        double lpmv(double, double, double)

- :py:func:`~scipy.special.mathieu_a`::

        double mathieu_a(double, double)

- :py:func:`~scipy.special.mathieu_b`::

        double mathieu_b(double, double)

- :py:func:`~scipy.special.mathieu_cem`::

        void mathieu_cem(double, double, double, double *, double *)

- :py:func:`~scipy.special.mathieu_modcem1`::

        void mathieu_modcem1(double, double, double, double *, double *)

- :py:func:`~scipy.special.mathieu_modcem2`::

        void mathieu_modcem2(double, double, double, double *, double *)

- :py:func:`~scipy.special.mathieu_modsem1`::

        void mathieu_modsem1(double, double, double, double *, double *)

- :py:func:`~scipy.special.mathieu_modsem2`::

        void mathieu_modsem2(double, double, double, double *, double *)

- :py:func:`~scipy.special.mathieu_sem`::

        void mathieu_sem(double, double, double, double *, double *)

- :py:func:`~scipy.special.modfresnelm`::

        void modfresnelm(double, double complex *, double complex *)

- :py:func:`~scipy.special.modfresnelp`::

        void modfresnelp(double, double complex *, double complex *)

- :py:func:`~scipy.special.modstruve`::

        double modstruve(double, double)

- :py:func:`~scipy.special.nbdtr`::

        double nbdtr(double, double, double)
        double nbdtr(long, long, double)

- :py:func:`~scipy.special.nbdtrc`::

        double nbdtrc(double, double, double)
        double nbdtrc(long, long, double)

- :py:func:`~scipy.special.nbdtri`::

        double nbdtri(double, double, double)
        double nbdtri(long, long, double)

- :py:func:`~scipy.special.nbdtrik`::

        double nbdtrik(double, double, double)

- :py:func:`~scipy.special.nbdtrin`::

        double nbdtrin(double, double, double)

- :py:func:`~scipy.special.ncfdtr`::

        double ncfdtr(double, double, double, double)

- :py:func:`~scipy.special.ncfdtri`::

        double ncfdtri(double, double, double, double)

- :py:func:`~scipy.special.ncfdtridfd`::

        double ncfdtridfd(double, double, double, double)

- :py:func:`~scipy.special.ncfdtridfn`::

        double ncfdtridfn(double, double, double, double)

- :py:func:`~scipy.special.ncfdtrinc`::

        double ncfdtrinc(double, double, double, double)

- :py:func:`~scipy.special.nctdtr`::

        double nctdtr(double, double, double)

- :py:func:`~scipy.special.nctdtridf`::

        double nctdtridf(double, double, double)

- :py:func:`~scipy.special.nctdtrinc`::

        double nctdtrinc(double, double, double)

- :py:func:`~scipy.special.nctdtrit`::

        double nctdtrit(double, double, double)

- :py:func:`~scipy.special.ndtr`::

        double complex ndtr(double complex)
        double ndtr(double)

- :py:func:`~scipy.special.ndtri`::

        double ndtri(double)

- :py:func:`~scipy.special.nrdtrimn`::

        double nrdtrimn(double, double, double)

- :py:func:`~scipy.special.nrdtrisd`::

        double nrdtrisd(double, double, double)

- :py:func:`~scipy.special.obl_ang1`::

        void obl_ang1(double, double, double, double, double *, double *)

- :py:func:`~scipy.special.obl_ang1_cv`::

        void obl_ang1_cv(double, double, double, double, double, double *, double *)

- :py:func:`~scipy.special.obl_cv`::

        double obl_cv(double, double, double)

- :py:func:`~scipy.special.obl_rad1`::

        void obl_rad1(double, double, double, double, double *, double *)

- :py:func:`~scipy.special.obl_rad1_cv`::

        void obl_rad1_cv(double, double, double, double, double, double *, double *)

- :py:func:`~scipy.special.obl_rad2`::

        void obl_rad2(double, double, double, double, double *, double *)

- :py:func:`~scipy.special.obl_rad2_cv`::

        void obl_rad2_cv(double, double, double, double, double, double *, double *)

- :py:func:`~scipy.special.owens_t`::

        double owens_t(double, double)

- :py:func:`~scipy.special.pbdv`::

        void pbdv(double, double, double *, double *)

- :py:func:`~scipy.special.pbvv`::

        void pbvv(double, double, double *, double *)

- :py:func:`~scipy.special.pbwa`::

        void pbwa(double, double, double *, double *)

- :py:func:`~scipy.special.pdtr`::

        double pdtr(double, double)
        double pdtr(long, double)

- :py:func:`~scipy.special.pdtrc`::

        double pdtrc(double, double)
        double pdtrc(long, double)

- :py:func:`~scipy.special.pdtri`::

        double pdtri(double, double)
        double pdtri(long, double)

- :py:func:`~scipy.special.pdtrik`::

        double pdtrik(double, double)

- :py:func:`~scipy.special.poch`::

        double poch(double, double)

- :py:func:`~scipy.special.pro_ang1`::

        void pro_ang1(double, double, double, double, double *, double *)

- :py:func:`~scipy.special.pro_ang1_cv`::

        void pro_ang1_cv(double, double, double, double, double, double *, double *)

- :py:func:`~scipy.special.pro_cv`::

        double pro_cv(double, double, double)

- :py:func:`~scipy.special.pro_rad1`::

        void pro_rad1(double, double, double, double, double *, double *)

- :py:func:`~scipy.special.pro_rad1_cv`::

        void pro_rad1_cv(double, double, double, double, double, double *, double *)

- :py:func:`~scipy.special.pro_rad2`::

        void pro_rad2(double, double, double, double, double *, double *)

- :py:func:`~scipy.special.pro_rad2_cv`::

        void pro_rad2_cv(double, double, double, double, double, double *, double *)

- :py:func:`~scipy.special.pseudo_huber`::

        double pseudo_huber(double, double)

- :py:func:`~scipy.special.psi`::

        double complex psi(double complex)
        double psi(double)

- :py:func:`~scipy.special.radian`::

        double radian(double, double, double)

- :py:func:`~scipy.special.rel_entr`::

        double rel_entr(double, double)

- :py:func:`~scipy.special.rgamma`::

        double complex rgamma(double complex)
        double rgamma(double)

- :py:func:`~scipy.special.round`::

        double round(double)

- :py:func:`~scipy.special.shichi`::

        void shichi(double complex, double complex *, double complex *)
        void shichi(double, double *, double *)

- :py:func:`~scipy.special.sici`::

        void sici(double complex, double complex *, double complex *)
        void sici(double, double *, double *)

- :py:func:`~scipy.special.sindg`::

        double sindg(double)

- :py:func:`~scipy.special.smirnov`::

        double smirnov(double, double)
        double smirnov(long, double)

- :py:func:`~scipy.special.smirnovi`::

        double smirnovi(double, double)
        double smirnovi(long, double)

- :py:func:`~scipy.special.spence`::

        double complex spence(double complex)
        double spence(double)

- :py:func:`~scipy.special.sph_harm`::

        double complex sph_harm(double, double, double, double)
        double complex sph_harm(long, long, double, double)

- :py:func:`~scipy.special.stdtr`::

        double stdtr(double, double)

- :py:func:`~scipy.special.stdtridf`::

        double stdtridf(double, double)

- :py:func:`~scipy.special.stdtrit`::

        double stdtrit(double, double)

- :py:func:`~scipy.special.struve`::

        double struve(double, double)

- :py:func:`~scipy.special.tandg`::

        double tandg(double)

- :py:func:`~scipy.special.tklmbda`::

        double tklmbda(double, double)

- :py:func:`~scipy.special.wofz`::

        double complex wofz(double complex)

- :py:func:`~scipy.special.wrightomega`::

        double complex wrightomega(double complex)

- :py:func:`~scipy.special.xlog1py`::

        double xlog1py(double, double)
        double complex xlog1py(double complex, double complex)

- :py:func:`~scipy.special.xlogy`::

        double xlogy(double, double)
        double complex xlogy(double complex, double complex)

- :py:func:`~scipy.special.y0`::

        double y0(double)

- :py:func:`~scipy.special.y1`::

        double y1(double)

- :py:func:`~scipy.special.yn`::

        double yn(double, double)
        double yn(long, double)

- :py:func:`~scipy.special.yv`::

        double complex yv(double, double complex)
        double yv(double, double)

- :py:func:`~scipy.special.yve`::

        double complex yve(double, double complex)
        double yve(double, double)

- :py:func:`~scipy.special.zetac`::

        double zetac(double)
"""

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import scipy.special._ufuncs as _ufuncs # C:\Program Files (x86)\Python37-32\lib\site-packages\scipy\special\_ufuncs.cp37-win32.pyd

# functions

def agm(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.agm """
    pass

def bdtr(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.bdtr """
    pass

def bdtrc(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.bdtrc """
    pass

def bdtri(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.bdtri """
    pass

def bdtrik(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.bdtrik """
    pass

def bdtrin(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.bdtrin """
    pass

def bei(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.bei """
    pass

def beip(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.beip """
    pass

def ber(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.ber """
    pass

def berp(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.berp """
    pass

def besselpoly(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.besselpoly """
    pass

def beta(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.beta """
    pass

def betainc(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.betainc """
    pass

def betaincinv(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.betaincinv """
    pass

def betaln(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.betaln """
    pass

def binom(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.binom """
    pass

def boxcox(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.boxcox """
    pass

def boxcox1p(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.boxcox1p """
    pass

def btdtr(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.btdtr """
    pass

def btdtri(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.btdtri """
    pass

def btdtria(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.btdtria """
    pass

def btdtrib(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.btdtrib """
    pass

def cbrt(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.cbrt """
    pass

def chdtr(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.chdtr """
    pass

def chdtrc(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.chdtrc """
    pass

def chdtri(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.chdtri """
    pass

def chdtriv(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.chdtriv """
    pass

def chndtr(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.chndtr """
    pass

def chndtridf(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.chndtridf """
    pass

def chndtrinc(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.chndtrinc """
    pass

def chndtrix(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.chndtrix """
    pass

def cosdg(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.cosdg """
    pass

def cosm1(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.cosm1 """
    pass

def cotdg(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.cotdg """
    pass

def dawsn(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.dawsn """
    pass

def ellipe(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.ellipe """
    pass

def ellipeinc(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.ellipeinc """
    pass

def ellipkinc(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.ellipkinc """
    pass

def ellipkm1(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.ellipkm1 """
    pass

def entr(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.entr """
    pass

def erf(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.erf """
    pass

def erfc(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.erfc """
    pass

def erfcx(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.erfcx """
    pass

def erfi(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.erfi """
    pass

def eval_chebyc(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.eval_chebyc """
    pass

def eval_chebys(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.eval_chebys """
    pass

def eval_chebyt(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.eval_chebyt """
    pass

def eval_chebyu(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.eval_chebyu """
    pass

def eval_gegenbauer(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.eval_gegenbauer """
    pass

def eval_genlaguerre(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.eval_genlaguerre """
    pass

def eval_hermite(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.eval_hermite """
    pass

def eval_hermitenorm(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.eval_hermitenorm """
    pass

def eval_jacobi(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.eval_jacobi """
    pass

def eval_laguerre(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.eval_laguerre """
    pass

def eval_legendre(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.eval_legendre """
    pass

def eval_sh_chebyt(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.eval_sh_chebyt """
    pass

def eval_sh_chebyu(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.eval_sh_chebyu """
    pass

def eval_sh_jacobi(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.eval_sh_jacobi """
    pass

def eval_sh_legendre(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.eval_sh_legendre """
    pass

def exp1(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.exp1 """
    pass

def exp10(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.exp10 """
    pass

def exp2(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.exp2 """
    pass

def expi(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.expi """
    pass

def expit(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.expit """
    pass

def expm1(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.expm1 """
    pass

def expn(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.expn """
    pass

def exprel(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.exprel """
    pass

def fdtr(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.fdtr """
    pass

def fdtrc(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.fdtrc """
    pass

def fdtri(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.fdtri """
    pass

def fdtridfd(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.fdtridfd """
    pass

def gamma(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.gamma """
    pass

def gammainc(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.gammainc """
    pass

def gammaincc(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.gammaincc """
    pass

def gammainccinv(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.gammainccinv """
    pass

def gammaincinv(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.gammaincinv """
    pass

def gammaln(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.gammaln """
    pass

def gammasgn(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.gammasgn """
    pass

def gdtr(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.gdtr """
    pass

def gdtrc(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.gdtrc """
    pass

def gdtria(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.gdtria """
    pass

def gdtrib(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.gdtrib """
    pass

def gdtrix(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.gdtrix """
    pass

def hankel1(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.hankel1 """
    pass

def hankel1e(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.hankel1e """
    pass

def hankel2(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.hankel2 """
    pass

def hankel2e(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.hankel2e """
    pass

def huber(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.huber """
    pass

def hyp0f1(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.hyp0f1 """
    pass

def hyp1f1(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.hyp1f1 """
    pass

def hyp2f1(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.hyp2f1 """
    pass

def hyperu(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.hyperu """
    pass

def i0(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.i0 """
    pass

def i0e(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.i0e """
    pass

def i1(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.i1 """
    pass

def i1e(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.i1e """
    pass

def inv_boxcox(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.inv_boxcox """
    pass

def inv_boxcox1p(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.inv_boxcox1p """
    pass

def it2struve0(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.it2struve0 """
    pass

def itmodstruve0(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.itmodstruve0 """
    pass

def itstruve0(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.itstruve0 """
    pass

def iv(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.iv """
    pass

def ive(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.ive """
    pass

def j0(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.j0 """
    pass

def j1(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.j1 """
    pass

def jv(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.jv """
    pass

def jve(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.jve """
    pass

def k0(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.k0 """
    pass

def k0e(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.k0e """
    pass

def k1(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.k1 """
    pass

def k1e(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.k1e """
    pass

def kei(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.kei """
    pass

def keip(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.keip """
    pass

def ker(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.ker """
    pass

def kerp(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.kerp """
    pass

def kl_div(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.kl_div """
    pass

def kn(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.kn """
    pass

def kolmogi(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.kolmogi """
    pass

def kolmogorov(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.kolmogorov """
    pass

def kv(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.kv """
    pass

def kve(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.kve """
    pass

def log1p(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.log1p """
    pass

def loggamma(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.loggamma """
    pass

def logit(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.logit """
    pass

def log_ndtr(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.log_ndtr """
    pass

def lpmv(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.lpmv """
    pass

def mathieu_a(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.mathieu_a """
    pass

def mathieu_b(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.mathieu_b """
    pass

def modstruve(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.modstruve """
    pass

def nbdtr(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.nbdtr """
    pass

def nbdtrc(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.nbdtrc """
    pass

def nbdtri(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.nbdtri """
    pass

def nbdtrik(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.nbdtrik """
    pass

def nbdtrin(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.nbdtrin """
    pass

def ncfdtr(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.ncfdtr """
    pass

def ncfdtri(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.ncfdtri """
    pass

def ncfdtridfd(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.ncfdtridfd """
    pass

def ncfdtridfn(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.ncfdtridfn """
    pass

def ncfdtrinc(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.ncfdtrinc """
    pass

def nctdtr(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.nctdtr """
    pass

def nctdtridf(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.nctdtridf """
    pass

def nctdtrinc(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.nctdtrinc """
    pass

def nctdtrit(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.nctdtrit """
    pass

def ndtr(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.ndtr """
    pass

def ndtri(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.ndtri """
    pass

def nrdtrimn(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.nrdtrimn """
    pass

def nrdtrisd(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.nrdtrisd """
    pass

def obl_cv(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.obl_cv """
    pass

def owens_t(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.owens_t """
    pass

def pdtr(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.pdtr """
    pass

def pdtrc(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.pdtrc """
    pass

def pdtri(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.pdtri """
    pass

def pdtrik(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.pdtrik """
    pass

def poch(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.poch """
    pass

def pro_cv(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.pro_cv """
    pass

def pseudo_huber(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.pseudo_huber """
    pass

def psi(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.psi """
    pass

def radian(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.radian """
    pass

def rel_entr(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.rel_entr """
    pass

def rgamma(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.rgamma """
    pass

def round(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.round """
    pass

def sindg(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.sindg """
    pass

def smirnov(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.smirnov """
    pass

def smirnovi(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.smirnovi """
    pass

def spence(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.spence """
    pass

def sph_harm(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.sph_harm """
    pass

def stdtr(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.stdtr """
    pass

def stdtridf(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.stdtridf """
    pass

def stdtrit(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.stdtrit """
    pass

def struve(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.struve """
    pass

def tandg(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.tandg """
    pass

def tklmbda(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.tklmbda """
    pass

def wofz(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.wofz """
    pass

def wrightomega(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.wrightomega """
    pass

def xlog1py(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.xlog1py """
    pass

def xlogy(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.xlogy """
    pass

def y0(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.y0 """
    pass

def y1(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.y1 """
    pass

def yn(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.yn """
    pass

def yv(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.yv """
    pass

def yve(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.yve """
    pass

def zetac(*args, **kwargs): # real signature unknown
    """ See the documentation for scipy.special.zetac """
    pass

def _airye_pywrap(*args, **kwargs): # real signature unknown
    pass

def _airy_pywrap(*args, **kwargs): # real signature unknown
    pass

def _bench_airy_d_cy(*args, **kwargs): # real signature unknown
    pass

def _bench_airy_D_cy(*args, **kwargs): # real signature unknown
    pass

def _bench_airy_d_py(*args, **kwargs): # real signature unknown
    pass

def _bench_airy_D_py(*args, **kwargs): # real signature unknown
    pass

def _bench_beta_dd_cy(*args, **kwargs): # real signature unknown
    pass

def _bench_beta_dd_py(*args, **kwargs): # real signature unknown
    pass

def _bench_erf_d_cy(*args, **kwargs): # real signature unknown
    pass

def _bench_erf_D_cy(*args, **kwargs): # real signature unknown
    pass

def _bench_erf_d_py(*args, **kwargs): # real signature unknown
    pass

def _bench_erf_D_py(*args, **kwargs): # real signature unknown
    pass

def _bench_exprel_d_cy(*args, **kwargs): # real signature unknown
    pass

def _bench_exprel_d_py(*args, **kwargs): # real signature unknown
    pass

def _bench_gamma_d_cy(*args, **kwargs): # real signature unknown
    pass

def _bench_gamma_D_cy(*args, **kwargs): # real signature unknown
    pass

def _bench_gamma_d_py(*args, **kwargs): # real signature unknown
    pass

def _bench_gamma_D_py(*args, **kwargs): # real signature unknown
    pass

def _bench_jv_dd_cy(*args, **kwargs): # real signature unknown
    pass

def _bench_jv_dD_cy(*args, **kwargs): # real signature unknown
    pass

def _bench_jv_dd_py(*args, **kwargs): # real signature unknown
    pass

def _bench_jv_dD_py(*args, **kwargs): # real signature unknown
    pass

def _bench_loggamma_D_cy(*args, **kwargs): # real signature unknown
    pass

def _bench_loggamma_D_py(*args, **kwargs): # real signature unknown
    pass

def _bench_logit_d_cy(*args, **kwargs): # real signature unknown
    pass

def _bench_logit_d_py(*args, **kwargs): # real signature unknown
    pass

def _bench_psi_d_cy(*args, **kwargs): # real signature unknown
    pass

def _bench_psi_D_cy(*args, **kwargs): # real signature unknown
    pass

def _bench_psi_d_py(*args, **kwargs): # real signature unknown
    pass

def _bench_psi_D_py(*args, **kwargs): # real signature unknown
    pass

def _ellipj_pywrap(*args, **kwargs): # real signature unknown
    pass

def _fresnel_pywrap(*args, **kwargs): # real signature unknown
    pass

def _hyp1f2_pywrap(*args, **kwargs): # real signature unknown
    pass

def _hyp2f0_pywrap(*args, **kwargs): # real signature unknown
    pass

def _hyp3f0_pywrap(*args, **kwargs): # real signature unknown
    pass

def _it2i0k0_pywrap(*args, **kwargs): # real signature unknown
    pass

def _it2j0y0_pywrap(*args, **kwargs): # real signature unknown
    pass

def _itairy_pywrap(*args, **kwargs): # real signature unknown
    pass

def _iti0k0_pywrap(*args, **kwargs): # real signature unknown
    pass

def _itj0y0_pywrap(*args, **kwargs): # real signature unknown
    pass

def _kelvin_pywrap(*args, **kwargs): # real signature unknown
    pass

def _mathieu_cem_pywrap(*args, **kwargs): # real signature unknown
    pass

def _mathieu_modcem1_pywrap(*args, **kwargs): # real signature unknown
    pass

def _mathieu_modcem2_pywrap(*args, **kwargs): # real signature unknown
    pass

def _mathieu_modsem1_pywrap(*args, **kwargs): # real signature unknown
    pass

def _mathieu_modsem2_pywrap(*args, **kwargs): # real signature unknown
    pass

def _mathieu_sem_pywrap(*args, **kwargs): # real signature unknown
    pass

def _modfresnelm_pywrap(*args, **kwargs): # real signature unknown
    pass

def _modfresnelp_pywrap(*args, **kwargs): # real signature unknown
    pass

def _obl_ang1_cv_pywrap(*args, **kwargs): # real signature unknown
    pass

def _obl_ang1_pywrap(*args, **kwargs): # real signature unknown
    pass

def _obl_rad1_cv_pywrap(*args, **kwargs): # real signature unknown
    pass

def _obl_rad1_pywrap(*args, **kwargs): # real signature unknown
    pass

def _obl_rad2_cv_pywrap(*args, **kwargs): # real signature unknown
    pass

def _obl_rad2_pywrap(*args, **kwargs): # real signature unknown
    pass

def _pbdv_pywrap(*args, **kwargs): # real signature unknown
    pass

def _pbvv_pywrap(*args, **kwargs): # real signature unknown
    pass

def _pbwa_pywrap(*args, **kwargs): # real signature unknown
    pass

def _pro_ang1_cv_pywrap(*args, **kwargs): # real signature unknown
    pass

def _pro_ang1_pywrap(*args, **kwargs): # real signature unknown
    pass

def _pro_rad1_cv_pywrap(*args, **kwargs): # real signature unknown
    pass

def _pro_rad1_pywrap(*args, **kwargs): # real signature unknown
    pass

def _pro_rad2_cv_pywrap(*args, **kwargs): # real signature unknown
    pass

def _pro_rad2_pywrap(*args, **kwargs): # real signature unknown
    pass

def _shichi_pywrap(*args, **kwargs): # real signature unknown
    pass

def _sici_pywrap(*args, **kwargs): # real signature unknown
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x0F136850>'

__pyx_capi__ = {
    '__pyx_fuse_0_0eval_chebyc': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F1451D0>'
    '__pyx_fuse_0_0eval_chebys': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145230>'
    '__pyx_fuse_0_0eval_chebyt': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145290>'
    '__pyx_fuse_0_0eval_chebyu': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F1452F0>'
    '__pyx_fuse_0_0eval_gegenbauer': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145350>'
    '__pyx_fuse_0_0eval_genlaguerre': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F1453B0>'
    '__pyx_fuse_0_0eval_jacobi': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, double, double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145410>'
    '__pyx_fuse_0_0eval_laguerre': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145470>'
    '__pyx_fuse_0_0eval_legendre': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F1454D0>'
    '__pyx_fuse_0_0eval_sh_chebyt': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145530>'
    '__pyx_fuse_0_0eval_sh_chebyu': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145590>'
    '__pyx_fuse_0_0eval_sh_jacobi': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, double, double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F1455F0>'
    '__pyx_fuse_0_0eval_sh_legendre': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145650>'
    '__pyx_fuse_0_1eval_chebyc': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F1451E8>'
    '__pyx_fuse_0_1eval_chebys': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F145248>'
    '__pyx_fuse_0_1eval_chebyt': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F1452A8>'
    '__pyx_fuse_0_1eval_chebyu': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F145308>'
    '__pyx_fuse_0_1eval_gegenbauer': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F145368>'
    '__pyx_fuse_0_1eval_genlaguerre': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F1453C8>'
    '__pyx_fuse_0_1eval_jacobi': None, # (!) real value is '<capsule object "double (double, double, double, double, int __pyx_skip_dispatch)" at 0x0F145428>'
    '__pyx_fuse_0_1eval_laguerre': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F145488>'
    '__pyx_fuse_0_1eval_legendre': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F1454E8>'
    '__pyx_fuse_0_1eval_sh_chebyt': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F145548>'
    '__pyx_fuse_0_1eval_sh_chebyu': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F1455A8>'
    '__pyx_fuse_0_1eval_sh_jacobi': None, # (!) real value is '<capsule object "double (double, double, double, double, int __pyx_skip_dispatch)" at 0x0F145608>'
    '__pyx_fuse_0_1eval_sh_legendre': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F145668>'
    '__pyx_fuse_0airy': None, # (!) real value is '<capsule object "void (__pyx_t_double_complex, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *)" at 0x0F134FC8>'
    '__pyx_fuse_0airye': None, # (!) real value is '<capsule object "void (__pyx_t_double_complex, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *)" at 0x0F145020>'
    '__pyx_fuse_0bdtr': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F145050>'
    '__pyx_fuse_0bdtrc': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F145080>'
    '__pyx_fuse_0bdtri': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F1450B0>'
    '__pyx_fuse_0dawsn': None, # (!) real value is '<capsule object "__pyx_t_double_complex (__pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F1450E0>'
    '__pyx_fuse_0erf': None, # (!) real value is '<capsule object "__pyx_t_double_complex (__pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145110>'
    '__pyx_fuse_0erfc': None, # (!) real value is '<capsule object "__pyx_t_double_complex (__pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145140>'
    '__pyx_fuse_0erfcx': None, # (!) real value is '<capsule object "__pyx_t_double_complex (__pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145170>'
    '__pyx_fuse_0erfi': None, # (!) real value is '<capsule object "__pyx_t_double_complex (__pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F1451A0>'
    '__pyx_fuse_0exp1': None, # (!) real value is '<capsule object "__pyx_t_double_complex (__pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F1456B0>'
    '__pyx_fuse_0expi': None, # (!) real value is '<capsule object "__pyx_t_double_complex (__pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F1456E0>'
    '__pyx_fuse_0expit': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F145710>'
    '__pyx_fuse_0expm1': None, # (!) real value is '<capsule object "__pyx_t_double_complex (__pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145758>'
    '__pyx_fuse_0expn': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F145788>'
    '__pyx_fuse_0fresnel': None, # (!) real value is '<capsule object "void (__pyx_t_double_complex, __pyx_t_double_complex *, __pyx_t_double_complex *)" at 0x0F1457B8>'
    '__pyx_fuse_0gamma': None, # (!) real value is '<capsule object "__pyx_t_double_complex (__pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F1457E8>'
    '__pyx_fuse_0hyp0f1': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145818>'
    '__pyx_fuse_0hyp1f1': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145848>'
    '__pyx_fuse_0hyp2f0': None, # (!) real value is '<capsule object "void (double, double, double, double, double *, double *)" at 0x0F145878>'
    '__pyx_fuse_0hyp2f1': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, double, double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F1458A8>'
    '__pyx_fuse_0iv': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F1458D8>'
    '__pyx_fuse_0ive': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145908>'
    '__pyx_fuse_0jv': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145938>'
    '__pyx_fuse_0jve': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145968>'
    '__pyx_fuse_0kn': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F145998>'
    '__pyx_fuse_0kv': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F1459C8>'
    '__pyx_fuse_0kve': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F1459F8>'
    '__pyx_fuse_0log1p': None, # (!) real value is '<capsule object "__pyx_t_double_complex (__pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145A28>'
    '__pyx_fuse_0log_ndtr': None, # (!) real value is '<capsule object "__pyx_t_double_complex (__pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145A58>'
    '__pyx_fuse_0loggamma': None, # (!) real value is '<capsule object "__pyx_t_double_complex (__pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145A88>'
    '__pyx_fuse_0logit': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F145AB8>'
    '__pyx_fuse_0nbdtr': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F145B00>'
    '__pyx_fuse_0nbdtrc': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F145B30>'
    '__pyx_fuse_0nbdtri': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F145B60>'
    '__pyx_fuse_0ndtr': None, # (!) real value is '<capsule object "__pyx_t_double_complex (__pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145B90>'
    '__pyx_fuse_0pdtr': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F145BC0>'
    '__pyx_fuse_0pdtrc': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F145BF0>'
    '__pyx_fuse_0pdtri': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F145C20>'
    '__pyx_fuse_0psi': None, # (!) real value is '<capsule object "__pyx_t_double_complex (__pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145C50>'
    '__pyx_fuse_0rgamma': None, # (!) real value is '<capsule object "__pyx_t_double_complex (__pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145C80>'
    '__pyx_fuse_0shichi': None, # (!) real value is '<capsule object "void (__pyx_t_double_complex, __pyx_t_double_complex *, __pyx_t_double_complex *)" at 0x0F145CB0>'
    '__pyx_fuse_0sici': None, # (!) real value is '<capsule object "void (__pyx_t_double_complex, __pyx_t_double_complex *, __pyx_t_double_complex *)" at 0x0F145CE0>'
    '__pyx_fuse_0smirnov': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F145D10>'
    '__pyx_fuse_0smirnovi': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F145D40>'
    '__pyx_fuse_0spence': None, # (!) real value is '<capsule object "__pyx_t_double_complex (__pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145D70>'
    '__pyx_fuse_0sph_harm': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, double, double, double, int __pyx_skip_dispatch)" at 0x0F145DA0>'
    '__pyx_fuse_0xlog1py': None, # (!) real value is '<capsule object "__pyx_t_double_complex (__pyx_t_double_complex, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145DD0>'
    '__pyx_fuse_0xlogy': None, # (!) real value is '<capsule object "__pyx_t_double_complex (__pyx_t_double_complex, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145E00>'
    '__pyx_fuse_0yn': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F145E30>'
    '__pyx_fuse_0yv': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145E60>'
    '__pyx_fuse_0yve': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145E90>'
    '__pyx_fuse_1_0eval_chebyc': None, # (!) real value is '<capsule object "__pyx_t_double_complex (long, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145200>'
    '__pyx_fuse_1_0eval_chebys': None, # (!) real value is '<capsule object "__pyx_t_double_complex (long, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145260>'
    '__pyx_fuse_1_0eval_chebyt': None, # (!) real value is '<capsule object "__pyx_t_double_complex (long, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F1452C0>'
    '__pyx_fuse_1_0eval_chebyu': None, # (!) real value is '<capsule object "__pyx_t_double_complex (long, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145320>'
    '__pyx_fuse_1_0eval_gegenbauer': None, # (!) real value is '<capsule object "__pyx_t_double_complex (long, double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145380>'
    '__pyx_fuse_1_0eval_genlaguerre': None, # (!) real value is '<capsule object "__pyx_t_double_complex (long, double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F1453E0>'
    '__pyx_fuse_1_0eval_jacobi': None, # (!) real value is '<capsule object "__pyx_t_double_complex (long, double, double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145440>'
    '__pyx_fuse_1_0eval_laguerre': None, # (!) real value is '<capsule object "__pyx_t_double_complex (long, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F1454A0>'
    '__pyx_fuse_1_0eval_legendre': None, # (!) real value is '<capsule object "__pyx_t_double_complex (long, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145500>'
    '__pyx_fuse_1_0eval_sh_chebyt': None, # (!) real value is '<capsule object "__pyx_t_double_complex (long, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145560>'
    '__pyx_fuse_1_0eval_sh_chebyu': None, # (!) real value is '<capsule object "__pyx_t_double_complex (long, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F1455C0>'
    '__pyx_fuse_1_0eval_sh_jacobi': None, # (!) real value is '<capsule object "__pyx_t_double_complex (long, double, double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145620>'
    '__pyx_fuse_1_0eval_sh_legendre': None, # (!) real value is '<capsule object "__pyx_t_double_complex (long, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F145680>'
    '__pyx_fuse_1_1eval_chebyc': None, # (!) real value is '<capsule object "double (long, double, int __pyx_skip_dispatch)" at 0x0F145218>'
    '__pyx_fuse_1_1eval_chebys': None, # (!) real value is '<capsule object "double (long, double, int __pyx_skip_dispatch)" at 0x0F145278>'
    '__pyx_fuse_1_1eval_chebyt': None, # (!) real value is '<capsule object "double (long, double, int __pyx_skip_dispatch)" at 0x0F1452D8>'
    '__pyx_fuse_1_1eval_chebyu': None, # (!) real value is '<capsule object "double (long, double, int __pyx_skip_dispatch)" at 0x0F145338>'
    '__pyx_fuse_1_1eval_gegenbauer': None, # (!) real value is '<capsule object "double (long, double, double, int __pyx_skip_dispatch)" at 0x0F145398>'
    '__pyx_fuse_1_1eval_genlaguerre': None, # (!) real value is '<capsule object "double (long, double, double, int __pyx_skip_dispatch)" at 0x0F1453F8>'
    '__pyx_fuse_1_1eval_jacobi': None, # (!) real value is '<capsule object "double (long, double, double, double, int __pyx_skip_dispatch)" at 0x0F145458>'
    '__pyx_fuse_1_1eval_laguerre': None, # (!) real value is '<capsule object "double (long, double, int __pyx_skip_dispatch)" at 0x0F1454B8>'
    '__pyx_fuse_1_1eval_legendre': None, # (!) real value is '<capsule object "double (long, double, int __pyx_skip_dispatch)" at 0x0F145518>'
    '__pyx_fuse_1_1eval_sh_chebyt': None, # (!) real value is '<capsule object "double (long, double, int __pyx_skip_dispatch)" at 0x0F145578>'
    '__pyx_fuse_1_1eval_sh_chebyu': None, # (!) real value is '<capsule object "double (long, double, int __pyx_skip_dispatch)" at 0x0F1455D8>'
    '__pyx_fuse_1_1eval_sh_jacobi': None, # (!) real value is '<capsule object "double (long, double, double, double, int __pyx_skip_dispatch)" at 0x0F145638>'
    '__pyx_fuse_1_1eval_sh_legendre': None, # (!) real value is '<capsule object "double (long, double, int __pyx_skip_dispatch)" at 0x0F145698>'
    '__pyx_fuse_1airy': None, # (!) real value is '<capsule object "void (double, double *, double *, double *, double *)" at 0x0F134FE0>'
    '__pyx_fuse_1airye': None, # (!) real value is '<capsule object "void (double, double *, double *, double *, double *)" at 0x0F145038>'
    '__pyx_fuse_1bdtr': None, # (!) real value is '<capsule object "double (long, long, double, int __pyx_skip_dispatch)" at 0x0F145068>'
    '__pyx_fuse_1bdtrc': None, # (!) real value is '<capsule object "double (long, long, double, int __pyx_skip_dispatch)" at 0x0F145098>'
    '__pyx_fuse_1bdtri': None, # (!) real value is '<capsule object "double (long, long, double, int __pyx_skip_dispatch)" at 0x0F1450C8>'
    '__pyx_fuse_1dawsn': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F1450F8>'
    '__pyx_fuse_1erf': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F145128>'
    '__pyx_fuse_1erfc': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F145158>'
    '__pyx_fuse_1erfcx': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F145188>'
    '__pyx_fuse_1erfi': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F1451B8>'
    '__pyx_fuse_1exp1': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F1456C8>'
    '__pyx_fuse_1expi': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F1456F8>'
    '__pyx_fuse_1expit': None, # (!) real value is '<capsule object "float (float, int __pyx_skip_dispatch)" at 0x0F145728>'
    '__pyx_fuse_1expm1': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F145770>'
    '__pyx_fuse_1expn': None, # (!) real value is '<capsule object "double (long, double, int __pyx_skip_dispatch)" at 0x0F1457A0>'
    '__pyx_fuse_1fresnel': None, # (!) real value is '<capsule object "void (double, double *, double *)" at 0x0F1457D0>'
    '__pyx_fuse_1gamma': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F145800>'
    '__pyx_fuse_1hyp0f1': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F145830>'
    '__pyx_fuse_1hyp1f1': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F145860>'
    '__pyx_fuse_1hyp2f0': None, # (!) real value is '<capsule object "void (double, double, double, long, double *, double *)" at 0x0F145890>'
    '__pyx_fuse_1hyp2f1': None, # (!) real value is '<capsule object "double (double, double, double, double, int __pyx_skip_dispatch)" at 0x0F1458C0>'
    '__pyx_fuse_1iv': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F1458F0>'
    '__pyx_fuse_1ive': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F145920>'
    '__pyx_fuse_1jv': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F145950>'
    '__pyx_fuse_1jve': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F145980>'
    '__pyx_fuse_1kn': None, # (!) real value is '<capsule object "double (long, double, int __pyx_skip_dispatch)" at 0x0F1459B0>'
    '__pyx_fuse_1kv': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F1459E0>'
    '__pyx_fuse_1kve': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F145A10>'
    '__pyx_fuse_1log1p': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F145A40>'
    '__pyx_fuse_1log_ndtr': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F145A70>'
    '__pyx_fuse_1loggamma': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F145AA0>'
    '__pyx_fuse_1logit': None, # (!) real value is '<capsule object "float (float, int __pyx_skip_dispatch)" at 0x0F145AD0>'
    '__pyx_fuse_1nbdtr': None, # (!) real value is '<capsule object "double (long, long, double, int __pyx_skip_dispatch)" at 0x0F145B18>'
    '__pyx_fuse_1nbdtrc': None, # (!) real value is '<capsule object "double (long, long, double, int __pyx_skip_dispatch)" at 0x0F145B48>'
    '__pyx_fuse_1nbdtri': None, # (!) real value is '<capsule object "double (long, long, double, int __pyx_skip_dispatch)" at 0x0F145B78>'
    '__pyx_fuse_1ndtr': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F145BA8>'
    '__pyx_fuse_1pdtr': None, # (!) real value is '<capsule object "double (long, double, int __pyx_skip_dispatch)" at 0x0F145BD8>'
    '__pyx_fuse_1pdtrc': None, # (!) real value is '<capsule object "double (long, double, int __pyx_skip_dispatch)" at 0x0F145C08>'
    '__pyx_fuse_1pdtri': None, # (!) real value is '<capsule object "double (long, double, int __pyx_skip_dispatch)" at 0x0F145C38>'
    '__pyx_fuse_1psi': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F145C68>'
    '__pyx_fuse_1rgamma': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F145C98>'
    '__pyx_fuse_1shichi': None, # (!) real value is '<capsule object "void (double, double *, double *)" at 0x0F145CC8>'
    '__pyx_fuse_1sici': None, # (!) real value is '<capsule object "void (double, double *, double *)" at 0x0F145CF8>'
    '__pyx_fuse_1smirnov': None, # (!) real value is '<capsule object "double (long, double, int __pyx_skip_dispatch)" at 0x0F145D28>'
    '__pyx_fuse_1smirnovi': None, # (!) real value is '<capsule object "double (long, double, int __pyx_skip_dispatch)" at 0x0F145D58>'
    '__pyx_fuse_1spence': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F145D88>'
    '__pyx_fuse_1sph_harm': None, # (!) real value is '<capsule object "__pyx_t_double_complex (long, long, double, double, int __pyx_skip_dispatch)" at 0x0F145DB8>'
    '__pyx_fuse_1xlog1py': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F145DE8>'
    '__pyx_fuse_1xlogy': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F145E18>'
    '__pyx_fuse_1yn': None, # (!) real value is '<capsule object "double (long, double, int __pyx_skip_dispatch)" at 0x0F145E48>'
    '__pyx_fuse_1yv': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F145E78>'
    '__pyx_fuse_1yve': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F145EA8>'
    '__pyx_fuse_2expit': None, # (!) real value is '<capsule object "long double (long double, int __pyx_skip_dispatch)" at 0x0F145740>'
    '__pyx_fuse_2logit': None, # (!) real value is '<capsule object "long double (long double, int __pyx_skip_dispatch)" at 0x0F145AE8>'
    'agm': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x00356488>'
    'bdtrik': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x02F6CEF0>'
    'bdtrin': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x02F6C5A8>'
    'bei': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x09388488>'
    'beip': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x09388590>'
    'ber': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x09388758>'
    'berp': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x09388650>'
    'besselpoly': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F0B0320>'
    'beta': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F0B0368>'
    'betainc': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134218>'
    'betaincinv': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F1341E8>'
    'betaln': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134230>'
    'binom': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134260>'
    'boxcox': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134278>'
    'boxcox1p': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134290>'
    'btdtr': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F1342A8>'
    'btdtri': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F1342C0>'
    'btdtria': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F1342D8>'
    'btdtrib': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F1342F0>'
    'cbrt': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134308>'
    'chdtr': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134320>'
    'chdtrc': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134338>'
    'chdtri': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134350>'
    'chdtriv': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134368>'
    'chndtr': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134380>'
    'chndtridf': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134398>'
    'chndtrinc': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F1343B0>'
    'chndtrix': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F1343C8>'
    'cosdg': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F1343E0>'
    'cosm1': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F1343F8>'
    'cotdg': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134410>'
    'ellipe': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134428>'
    'ellipeinc': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134440>'
    'ellipj': None, # (!) real value is '<capsule object "void (double, double, double *, double *, double *, double *)" at 0x0F134458>'
    'ellipkinc': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134470>'
    'ellipkm1': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134488>'
    'entr': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F1344A0>'
    'eval_hermite': None, # (!) real value is '<capsule object "double (long, double, int __pyx_skip_dispatch)" at 0x0F1344B8>'
    'eval_hermitenorm': None, # (!) real value is '<capsule object "double (long, double, int __pyx_skip_dispatch)" at 0x0F1344D0>'
    'exp10': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F1344E8>'
    'exp2': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134500>'
    'exprel': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134518>'
    'fdtr': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134530>'
    'fdtrc': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134548>'
    'fdtri': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134560>'
    'fdtridfd': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134578>'
    'gammainc': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134590>'
    'gammaincc': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F1345A8>'
    'gammainccinv': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F1345C0>'
    'gammaincinv': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F1345D8>'
    'gammaln': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F1345F0>'
    'gammasgn': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134608>'
    'gdtr': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134620>'
    'gdtrc': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134638>'
    'gdtria': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134650>'
    'gdtrib': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134668>'
    'gdtrix': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134680>'
    'hankel1': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F134698>'
    'hankel1e': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F1346B0>'
    'hankel2': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F1346C8>'
    'hankel2e': None, # (!) real value is '<capsule object "__pyx_t_double_complex (double, __pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F1346E0>'
    'huber': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F1346F8>'
    'hyp1f2': None, # (!) real value is '<capsule object "void (double, double, double, double, double *, double *)" at 0x0F134710>'
    'hyp3f0': None, # (!) real value is '<capsule object "void (double, double, double, double, double *, double *)" at 0x0F134728>'
    'hyperu': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134740>'
    'i0': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134758>'
    'i0e': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134770>'
    'i1': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134788>'
    'i1e': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F1347A0>'
    'inv_boxcox': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F1347B8>'
    'inv_boxcox1p': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F1347D0>'
    'it2i0k0': None, # (!) real value is '<capsule object "void (double, double *, double *)" at 0x0F1347E8>'
    'it2j0y0': None, # (!) real value is '<capsule object "void (double, double *, double *)" at 0x0F134800>'
    'it2struve0': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134818>'
    'itairy': None, # (!) real value is '<capsule object "void (double, double *, double *, double *, double *)" at 0x0F134830>'
    'iti0k0': None, # (!) real value is '<capsule object "void (double, double *, double *)" at 0x0F134848>'
    'itj0y0': None, # (!) real value is '<capsule object "void (double, double *, double *)" at 0x0F134860>'
    'itmodstruve0': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134878>'
    'itstruve0': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134890>'
    'j0': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F1348A8>'
    'j1': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F1348C0>'
    'k0': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F1348D8>'
    'k0e': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F1348F0>'
    'k1': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134908>'
    'k1e': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134920>'
    'kei': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134938>'
    'keip': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134950>'
    'kelvin': None, # (!) real value is '<capsule object "void (double, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *)" at 0x0F134968>'
    'ker': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134980>'
    'kerp': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134998>'
    'kl_div': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F1349B0>'
    'kolmogi': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F1349C8>'
    'kolmogorov': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F1349E0>'
    'lpmv': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F1349F8>'
    'mathieu_a': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134A10>'
    'mathieu_b': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134A28>'
    'mathieu_cem': None, # (!) real value is '<capsule object "void (double, double, double, double *, double *)" at 0x0F134A40>'
    'mathieu_modcem1': None, # (!) real value is '<capsule object "void (double, double, double, double *, double *)" at 0x0F134A58>'
    'mathieu_modcem2': None, # (!) real value is '<capsule object "void (double, double, double, double *, double *)" at 0x0F134A70>'
    'mathieu_modsem1': None, # (!) real value is '<capsule object "void (double, double, double, double *, double *)" at 0x0F134A88>'
    'mathieu_modsem2': None, # (!) real value is '<capsule object "void (double, double, double, double *, double *)" at 0x0F134AA0>'
    'mathieu_sem': None, # (!) real value is '<capsule object "void (double, double, double, double *, double *)" at 0x0F134AB8>'
    'modfresnelm': None, # (!) real value is '<capsule object "void (double, __pyx_t_double_complex *, __pyx_t_double_complex *)" at 0x0F134AD0>'
    'modfresnelp': None, # (!) real value is '<capsule object "void (double, __pyx_t_double_complex *, __pyx_t_double_complex *)" at 0x0F134AE8>'
    'modstruve': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134B00>'
    'nbdtrik': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134B18>'
    'nbdtrin': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134B30>'
    'ncfdtr': None, # (!) real value is '<capsule object "double (double, double, double, double, int __pyx_skip_dispatch)" at 0x0F134B48>'
    'ncfdtri': None, # (!) real value is '<capsule object "double (double, double, double, double, int __pyx_skip_dispatch)" at 0x0F134B60>'
    'ncfdtridfd': None, # (!) real value is '<capsule object "double (double, double, double, double, int __pyx_skip_dispatch)" at 0x0F134B78>'
    'ncfdtridfn': None, # (!) real value is '<capsule object "double (double, double, double, double, int __pyx_skip_dispatch)" at 0x0F134B90>'
    'ncfdtrinc': None, # (!) real value is '<capsule object "double (double, double, double, double, int __pyx_skip_dispatch)" at 0x0F134BA8>'
    'nctdtr': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134BC0>'
    'nctdtridf': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134BD8>'
    'nctdtrinc': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134BF0>'
    'nctdtrit': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134C08>'
    'ndtri': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134C20>'
    'nrdtrimn': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134C38>'
    'nrdtrisd': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134C50>'
    'obl_ang1': None, # (!) real value is '<capsule object "void (double, double, double, double, double *, double *)" at 0x0F134C68>'
    'obl_ang1_cv': None, # (!) real value is '<capsule object "void (double, double, double, double, double, double *, double *)" at 0x0F134C80>'
    'obl_cv': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134C98>'
    'obl_rad1': None, # (!) real value is '<capsule object "void (double, double, double, double, double *, double *)" at 0x0F134CB0>'
    'obl_rad1_cv': None, # (!) real value is '<capsule object "void (double, double, double, double, double, double *, double *)" at 0x0F134CC8>'
    'obl_rad2': None, # (!) real value is '<capsule object "void (double, double, double, double, double *, double *)" at 0x0F134CE0>'
    'obl_rad2_cv': None, # (!) real value is '<capsule object "void (double, double, double, double, double, double *, double *)" at 0x0F134CF8>'
    'owens_t': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134D10>'
    'pbdv': None, # (!) real value is '<capsule object "void (double, double, double *, double *)" at 0x0F134D28>'
    'pbvv': None, # (!) real value is '<capsule object "void (double, double, double *, double *)" at 0x0F134D40>'
    'pbwa': None, # (!) real value is '<capsule object "void (double, double, double *, double *)" at 0x0F134D58>'
    'pdtrik': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134D70>'
    'poch': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134D88>'
    'pro_ang1': None, # (!) real value is '<capsule object "void (double, double, double, double, double *, double *)" at 0x0F134DA0>'
    'pro_ang1_cv': None, # (!) real value is '<capsule object "void (double, double, double, double, double, double *, double *)" at 0x0F134DB8>'
    'pro_cv': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134DD0>'
    'pro_rad1': None, # (!) real value is '<capsule object "void (double, double, double, double, double *, double *)" at 0x0F134DE8>'
    'pro_rad1_cv': None, # (!) real value is '<capsule object "void (double, double, double, double, double, double *, double *)" at 0x0F134E00>'
    'pro_rad2': None, # (!) real value is '<capsule object "void (double, double, double, double, double *, double *)" at 0x0F134E18>'
    'pro_rad2_cv': None, # (!) real value is '<capsule object "void (double, double, double, double, double, double *, double *)" at 0x0F134E30>'
    'pseudo_huber': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134E48>'
    'radian': None, # (!) real value is '<capsule object "double (double, double, double, int __pyx_skip_dispatch)" at 0x0F134E60>'
    'rel_entr': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134E78>'
    'round': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134E90>'
    'sindg': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134EA8>'
    'stdtr': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134EC0>'
    'stdtridf': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134ED8>'
    'stdtrit': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134EF0>'
    'struve': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134F08>'
    'tandg': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134F20>'
    'tklmbda': None, # (!) real value is '<capsule object "double (double, double, int __pyx_skip_dispatch)" at 0x0F134F38>'
    'wofz': None, # (!) real value is '<capsule object "__pyx_t_double_complex (__pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F134F50>'
    'wrightomega': None, # (!) real value is '<capsule object "__pyx_t_double_complex (__pyx_t_double_complex, int __pyx_skip_dispatch)" at 0x0F134F68>'
    'y0': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134F80>'
    'y1': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134F98>'
    'zetac': None, # (!) real value is '<capsule object "double (double, int __pyx_skip_dispatch)" at 0x0F134FB0>'
}

__spec__ = None # (!) real value is "ModuleSpec(name='scipy.special.cython_special', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x0F136850>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\scipy\\\\special\\\\cython_special.cp37-win32.pyd')"

__test__ = {}

