# encoding: utf-8
# module sklearn.svm.libsvm
# from C:\Program Files (x86)\Python37-32\lib\site-packages\sklearn\svm\libsvm.cp37-win32.pyd
# by generator 1.147
"""
Binding for libsvm_skl
----------------------

These are the bindings for libsvm_skl, which is a fork of libsvm[1]
that adds to libsvm some capabilities, like index of support vectors
and efficient representation of dense matrices.

These are low-level routines, but can be used for flexibility or
performance reasons. See sklearn.svm for a higher-level API.

Low-level memory management is done in libsvm_helper.c. If we happen
to run out of memory a MemoryError will be raised. In practice this is
not very helpful since hight changes are malloc fails inside svm.cpp,
where no sort of memory checks are done.

[1] https://www.csie.ntu.edu.tw/~cjlin/libsvm/

Notes
-----
Maybe we could speed it a bit further by decorating functions with
@cython.boundscheck(False), but probably it is not worth since all
work is done in lisvm_helper.c
Also, the signature mode='c' is somewhat superficial, since we already
check that arrays are C-contiguous in svm.py

Authors
-------
2010: Fabian Pedregosa <fabian.pedregosa@inria.fr>
      Gael Varoquaux <gael.varoquaux@normalesup.org>
"""

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import warnings as warnings # C:\Program Files (x86)\Python37-32\lib\warnings.py
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py

# functions

def cross_validation(*args, **kwargs): # real signature unknown
    """
    Binding of the cross-validation routine (low-level routine)
    
        Parameters
        ----------
    
        X : array-like, dtype=float, size=[n_samples, n_features]
    
        Y : array, dtype=float, size=[n_samples]
            target vector
    
        svm_type : {0, 1, 2, 3, 4}
            Type of SVM: C SVC, nu SVC, one class, epsilon SVR, nu SVR
    
        kernel : {'linear', 'rbf', 'poly', 'sigmoid', 'precomputed'}
            Kernel to use in the model: linear, polynomial, RBF, sigmoid
            or precomputed.
    
        degree : int
            Degree of the polynomial kernel (only relevant if kernel is
            set to polynomial)
    
        gamma : float
            Gamma parameter in rbf, poly and sigmoid kernels. Ignored by other
            kernels. 0.1 by default.
    
        coef0 : float
            Independent parameter in poly/sigmoid kernel.
    
        tol : float
            Stopping criteria.
    
        C : float
            C parameter in C-Support Vector Classification
    
        nu : float
    
        cache_size : float
    
        random_seed : int, optional
            Seed for the random number generator used for probability estimates.
            0 by default.
    
        Returns
        -------
        target : array, float
    """
    pass

def decision_function(*args, **kwargs): # real signature unknown
    """
    Predict margin (libsvm name for this is predict_values)
    
        We have to reconstruct model and parameters to make sure we stay
        in sync with the python object.
    """
    pass

def fit(*args, **kwargs): # real signature unknown
    """
    Train the model using libsvm (low-level method)
    
        Parameters
        ----------
        X : array-like, dtype=float64, size=[n_samples, n_features]
    
        Y : array, dtype=float64, size=[n_samples]
            target vector
    
        svm_type : {0, 1, 2, 3, 4}, optional
            Type of SVM: C_SVC, NuSVC, OneClassSVM, EpsilonSVR or NuSVR
            respectively. 0 by default.
    
        kernel : {'linear', 'rbf', 'poly', 'sigmoid', 'precomputed'}, optional
            Kernel to use in the model: linear, polynomial, RBF, sigmoid
            or precomputed. 'rbf' by default.
    
        degree : int32, optional
            Degree of the polynomial kernel (only relevant if kernel is
            set to polynomial), 3 by default.
    
        gamma : float64, optional
            Gamma parameter in rbf, poly and sigmoid kernels. Ignored by other
            kernels. 0.1 by default.
    
        coef0 : float64, optional
            Independent parameter in poly/sigmoid kernel. 0 by default.
    
        tol : float64, optional
            Numeric stopping criterion (WRITEME). 1e-3 by default.
    
        C : float64, optional
            C parameter in C-Support Vector Classification. 1 by default.
    
        nu : float64, optional
            0.5 by default.
    
        epsilon : double, optional
            0.1 by default.
    
        class_weight : array, dtype float64, shape (n_classes,), optional
            np.empty(0) by default.
    
        sample_weight : array, dtype float64, shape (n_samples,), optional
            np.empty(0) by default.
    
        shrinking : int, optional
            1 by default.
    
        probability : int, optional
            0 by default.
    
        cache_size : float64, optional
            Cache size for gram matrix columns (in megabytes). 100 by default.
    
        max_iter : int (-1 for no limit), optional.
            Stop solver after this many iterations regardless of accuracy
            (XXX Currently there is no API to know whether this kicked in.)
            -1 by default.
    
        random_seed : int, optional
            Seed for the random number generator used for probability estimates.
            0 by default.
    
        Returns
        -------
        support : array, shape=[n_support]
            index of support vectors
    
        support_vectors : array, shape=[n_support, n_features]
            support vectors (equivalent to X[support]). Will return an
            empty array in the case of precomputed kernel.
    
        n_class_SV : array
            number of support vectors in each class.
    
        sv_coef : array
            coefficients of support vectors in decision function.
    
        intercept : array
            intercept in decision function
    
        probA, probB : array
            probability estimates, empty array for probability=False
    """
    pass

def predict(*args, **kwargs): # real signature unknown
    """
    Predict target values of X given a model (low-level method)
    
        Parameters
        ----------
        X : array-like, dtype=float, size=[n_samples, n_features]
        svm_type : {0, 1, 2, 3, 4}
            Type of SVM: C SVC, nu SVC, one class, epsilon SVR, nu SVR
        kernel : {'linear', 'rbf', 'poly', 'sigmoid', 'precomputed'}
            Type of kernel.
        degree : int
            Degree of the polynomial kernel.
        gamma : float
            Gamma parameter in rbf, poly and sigmoid kernels. Ignored by other
            kernels. 0.1 by default.
        coef0 : float
            Independent parameter in poly/sigmoid kernel.
    
        Returns
        -------
        dec_values : array
            predicted values.
    """
    pass

def predict_proba(*args, **kwargs): # real signature unknown
    """
    Predict probabilities
    
        svm_model stores all parameters needed to predict a given value.
    
        For speed, all real work is done at the C level in function
        copy_predict (libsvm_helper.c).
    
        We have to reconstruct model and parameters to make sure we stay
        in sync with the python object.
    
        See sklearn.svm.predict for a complete list of parameters.
    
        Parameters
        ----------
        X : array-like, dtype=float
        kernel : {'linear', 'rbf', 'poly', 'sigmoid', 'precomputed'}
    
        Returns
        -------
        dec_values : array
            predicted values.
    """
    pass

def set_verbosity_wrap(*args, **kwargs): # real signature unknown
    """ Control verbosity of libsvm library """
    pass

# no classes
# variables with complex values

LIBSVM_KERNEL_TYPES = [
    'linear',
    'poly',
    'rbf',
    'sigmoid',
    'precomputed',
]

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x0F52FD90>'

__spec__ = None # (!) real value is "ModuleSpec(name='sklearn.svm.libsvm', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x0F52FD90>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\sklearn\\\\svm\\\\libsvm.cp37-win32.pyd')"

__test__ = {}

