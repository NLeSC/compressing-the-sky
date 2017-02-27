Compressing the Sky - test on CFHTLS time series data
=====================================================

Path-finding project with CWI Database Architectures Group
on compress and handling astronomical data (Lofar)
Duration of this extension of the project: January - February 2017

Guidance:
--------
- Select_constant_sources_in_CFHTLS_time_series_data.py is a Python 3 script.
- Replace '/home/hspreeuw/Dropbox/eScienceCenter/CWI/terse' in the os.chdir(''/home/hspreeuw/Dropbox/eScienceCenter/CWI/terse')
  to where you unpacked http://www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/data/pub/CFHTSG/DeepVarTerse.tar.gz.
- Select the statistical test that you would like to try by commenting out or uncommenting probability_of_constancy.
- Commenting out
                # magnitudes  = np.random.normal(12, 1, times.size)
                # errors            = 1 * np.ones((times.size))  
  will give you pure Gaussian noise
- The user can add e.g. a single 10 sigma spike to the time series data, so to either artificial (pure Gaussian noise) or CFHTLS data, by 
  uncommenting 
                # scaled_residuals[50] += 10
