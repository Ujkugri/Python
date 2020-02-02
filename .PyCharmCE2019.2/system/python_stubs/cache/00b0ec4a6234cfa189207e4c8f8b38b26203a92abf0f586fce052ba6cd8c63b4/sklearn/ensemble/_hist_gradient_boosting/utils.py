# encoding: utf-8
# module sklearn.ensemble._hist_gradient_boosting.utils
# from C:\Program Files (x86)\Python37-32\lib\site-packages\sklearn\ensemble\_hist_gradient_boosting\utils.cp37-win32.pyd
# by generator 1.147
""" This module contains utility routines. """

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import sklearn.base as __sklearn_base


# functions

def get_equivalent_estimator(*args, **kwargs): # real signature unknown
    """
    Return an unfitted estimator from another lib with matching hyperparams.
    
        This utility function takes care of renaming the sklearn parameters into
        their LightGBM, XGBoost or CatBoost equivalent parameters.
    
        # unmapped XGB parameters:
        # - min_samples_leaf
        # - min_data_in_bin
        # - min_split_gain (there is min_split_loss though?)
    
        # unmapped Catboost parameters:
        # max_leaves
        # min_*
    """
    pass

def is_classifier(estimator): # reliably restored by inspect
    """
    Returns True if the given estimator is (probably) a classifier.
    
        Parameters
        ----------
        estimator : object
            Estimator object to test.
    
        Returns
        -------
        out : bool
            True if estimator is a classifier and False otherwise.
    """
    pass

def sum_parallel(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Enum(*args, **kwargs): # real signature unknown
    pass

# classes

class _BinMapper(__sklearn_base.BaseEstimator, __sklearn_base.TransformerMixin):
    """
    Transformer that maps a dataset into integer-valued bins.
    
        The bins are created in a feature-wise fashion, using quantiles so that
        each bins contains approximately the same number of samples.
    
        For large datasets, quantiles are computed on a subset of the data to
        speed-up the binning, but the quantiles should remain stable.
    
        If the number of unique values for a given feature is less than
        ``max_bins``, then the unique values of this feature are used instead of
        the quantiles.
    
        Parameters
        ----------
        max_bins : int, optional (default=256)
            The maximum number of bins to use. If for a given feature the number of
            unique values is less than ``max_bins``, then those unique values
            will be used to compute the bin thresholds, instead of the quantiles.
        subsample : int or None, optional (default=2e5)
            If ``n_samples > subsample``, then ``sub_samples`` samples will be
            randomly choosen to compute the quantiles. If ``None``, the whole data
            is used.
        random_state: int or numpy.random.RandomState or None,         optional (default=None)
            Pseudo-random number generator to control the random sub-sampling.
            See :term:`random_state`.
    """
    def fit(self, X, y=None): # reliably restored by inspect
        """
        Fit data X by computing the binning thresholds.
        
                Parameters
                ----------
                X : array-like, shape (n_samples, n_features)
                    The data to bin.
                y: None
                    Ignored.
        
                Returns
                -------
                self : object
        """
        pass

    def transform(self, X): # reliably restored by inspect
        """
        Bin data X.
        
                Parameters
                ----------
                X : array-like, shape (n_samples, n_features)
                    The data to bin.
        
                Returns
                -------
                X_binned : array-like, shape (n_samples, n_features)
                    The binned data.
        """
        pass

    def __init__(self, max_bins=256, subsample=200000, random_state=None): # reliably restored by inspect
        # no doc
        pass


# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x1134F950>'

__spec__ = None # (!) real value is "ModuleSpec(name='sklearn.ensemble._hist_gradient_boosting.utils', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x1134F950>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\sklearn\\\\ensemble\\\\_hist_gradient_boosting\\\\utils.cp37-win32.pyd')"

__test__ = {}

