# encoding: utf-8
# module sklearn.utils._random
# from C:\Program Files (x86)\Python37-32\lib\site-packages\sklearn\utils\_random.cp37-win32.pyd
# by generator 1.147
"""
Random utility function
=======================
This module complements missing features of ``numpy.random``.

The module contains:
    * Several algorithms to sample integers without replacement.
    * Fast rand_r alternative based on xor shifts
"""

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py

# functions

def check_random_state(seed): # reliably restored by inspect
    """
    Turn seed into a np.random.RandomState instance
    
        Parameters
        ----------
        seed : None | int | instance of RandomState
            If seed is None, return the RandomState singleton used by np.random.
            If seed is an int, return a new RandomState instance seeded with seed.
            If seed is already a RandomState instance, return it.
            Otherwise raise ValueError.
    """
    pass

def sample_without_replacement(*args, **kwargs): # real signature unknown
    """
    Sample integers without replacement.
    
        Select n_samples integers from the set [0, n_population) without
        replacement.
    
    
        Parameters
        ----------
        n_population : int,
            The size of the set to sample from.
    
        n_samples : int,
            The number of integer to sample.
    
        random_state : int, RandomState instance or None, optional (default=None)
            If int, random_state is the seed used by the random number generator;
            If RandomState instance, random_state is the random number generator;
            If None, the random number generator is the RandomState instance used
            by `np.random`.
    
        method : "auto", "tracking_selection", "reservoir_sampling" or "pool"
            If method == "auto", the ratio of n_samples / n_population is used
            to determine which algorithm to use:
            If ratio is between 0 and 0.01, tracking selection is used.
            If ratio is between 0.01 and 0.99, numpy.random.permutation is used.
            If ratio is greater than 0.99, reservoir sampling is used.
            The order of the selected integers is undefined. If a random order is
            desired, the selected subset should be shuffled.
    
            If method =="tracking_selection", a set based implementation is used
            which is suitable for `n_samples` <<< `n_population`.
    
            If method == "reservoir_sampling", a reservoir sampling algorithm is
            used which is suitable for high memory constraint or when
            O(`n_samples`) ~ O(`n_population`).
            The order of the selected integers is undefined. If a random order is
            desired, the selected subset should be shuffled.
    
            If method == "pool", a pool based algorithm is particularly fast, even
            faster than the tracking selection method. Hovewer, a vector containing
            the entire population has to be initialized.
            If n_samples ~ n_population, the reservoir sampling method is faster.
    
        Returns
        -------
        out : array of size (n_samples, )
            The sampled subsets of integer. The subset of selected integer might
            not be randomized, see the method argument.
    """
    pass

def _our_rand_r_py(*args, **kwargs): # real signature unknown
    """ Python utils to test the our_rand_r function """
    pass

def _sample_without_replacement_check_input(*args, **kwargs): # real signature unknown
    """ Check that input are consistent for sample_without_replacement """
    pass

def _sample_without_replacement_with_pool(*args, **kwargs): # real signature unknown
    """
    Sample integers without replacement.
    
        Select n_samples integers from the set [0, n_population) without
        replacement.
    
        Time complexity: O(n_population +  O(np.random.randint) * n_samples)
    
        Space complexity of O(n_population + n_samples).
    
    
        Parameters
        ----------
        n_population : int,
            The size of the set to sample from.
    
        n_samples : int,
            The number of integer to sample.
    
        random_state : int, RandomState instance or None, optional (default=None)
            If int, random_state is the seed used by the random number generator;
            If RandomState instance, random_state is the random number generator;
            If None, the random number generator is the RandomState instance used
            by `np.random`.
    
        Returns
        -------
        out : array of size (n_samples, )
            The sampled subsets of integer.
    """
    pass

def _sample_without_replacement_with_reservoir_sampling(*args, **kwargs): # real signature unknown
    """
    Sample integers without replacement.
    
        Select n_samples integers from the set [0, n_population) without
        replacement.
    
        Time complexity of
            O((n_population - n_samples) * O(np.random.randint) + n_samples)
        Space complexity of O(n_samples)
    
    
        Parameters
        ----------
        n_population : int,
            The size of the set to sample from.
    
        n_samples : int,
             The number of integer to sample.
    
        random_state : int, RandomState instance or None, optional (default=None)
            If int, random_state is the seed used by the random number generator;
            If RandomState instance, random_state is the random number generator;
            If None, the random number generator is the RandomState instance used
            by `np.random`.
    
        Returns
        -------
        out : array of size (n_samples, )
            The sampled subsets of integer. The order of the items is not
            necessarily random. Use a random permutation of the array if the order
            of the items has to be randomized.
    """
    pass

def _sample_without_replacement_with_tracking_selection(*args, **kwargs): # real signature unknown
    """
    Sample integers without replacement.
    
        Select n_samples integers from the set [0, n_population) without
        replacement.
    
        Time complexity:
            - Worst-case: unbounded
            - Average-case:
                O(O(np.random.randint) * \sum_{i=1}^n_samples 1 /
                                                  (1 - i / n_population)))
                <= O(O(np.random.randint) *
                       n_population * ln((n_population - 2)
                                         /(n_population - 1 - n_samples)))
                <= O(O(np.random.randint) *
                     n_population * 1 / (1 - n_samples / n_population))
    
        Space complexity of O(n_samples) in a python set.
    
    
        Parameters
        ----------
        n_population : int,
            The size of the set to sample from.
    
        n_samples : int,
            The number of integer to sample.
    
        random_state : int, RandomState instance or None, optional (default=None)
            If int, random_state is the seed used by the random number generator;
            If RandomState instance, random_state is the random number generator;
            If None, the random number generator is the RandomState instance used
            by `np.random`.
    
        Returns
        -------
        out : array of size (n_samples, )
            The sampled subsets of integer.
    """
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x02D4E830>'

__pyx_capi__ = {
    'DEFAULT_SEED': None, # (!) real value is '<capsule object "__pyx_t_7sklearn_5utils_7_random_UINT32_t" at 0x00EC6488>'
    'sample_without_replacement': None, # (!) real value is '<capsule object "PyObject *(__pyx_t_5numpy_int_t, __pyx_t_5numpy_int_t, int __pyx_skip_dispatch, struct __pyx_opt_args_7sklearn_5utils_7_random_sample_without_replacement *__pyx_optional_args)" at 0x006B4D88>'
}

__spec__ = None # (!) real value is "ModuleSpec(name='sklearn.utils._random', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x02D4E830>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\sklearn\\\\utils\\\\_random.cp37-win32.pyd')"

__test__ = {}

