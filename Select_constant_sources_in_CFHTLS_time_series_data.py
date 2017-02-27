import numpy as np
# import pylab as pyl
import os
# from scipy.stats import chisquare as chisq
from scipy.stats import chi2
# import statsmodels.tsa.stattools as ts
# from statsmodels.stats.diagnostic import acorr_ljungbox
# from statsmodels.stats.stattools import jarque_bera as jb
# from statsmodels.stats.stattools import omni_normtest

print()
print()

path_to_plots = '/home/hspreeuw/Dropbox/eScienceCenter/Compressing-the-Sky/plots/'

os.chdir('/home/hspreeuw/Dropbox/eScienceCenter/CWI/terse')

number_of_time_series                       =   0
number_of_variable_time_series              =   0
number_of_constant_time_series              =   0
confidence_limit                            =   0.05
minimal_number_of_timestamps                = 100
minimal_number_of_colours                   =   6

for cfhtls_file in os.listdir(os.getcwd()):
    # cfhtls_file = os.listdir(os.getcwd())[10]
    if os.path.getsize(cfhtls_file):
        cfhtls_array = np.loadtxt(cfhtls_file)

        # The frequency indices are listed in the last column.
        try:
            frequency_indices = np.unique(cfhtls_array[:, 3])
        except IndexError:
            frequency_indices = np.empty((0))
        # print()
        # print('Frequency indices are {0}'.format(frequency_indices))

        # All colours should have been measured to determine variability.
        if frequency_indices.size >= minimal_number_of_colours:

            number_of_colours_that_show_variability         = 0
            number_of_colours_that_do_not_show_variability  = 0

            for freq_index in frequency_indices:
                # first_frequency = first_array[first_array[:,3] == 1]
                time_series  =  np.delete(cfhtls_array[cfhtls_array[:, 3] == freq_index], 3, 1)
                times             =  time_series[:, 0]
                # times   =  np.arange(365)
                magnitudes  =   time_series[:, 1]
                # magnitudes  = np.random.normal(12, 1, times.size)
                errors            =   time_series[:, 2]
                # errors            = 1 * np.ones((times.size))

                # Now we are going to use errors as weights.
                # Apparently, some errors are zero, which shouldn't be.
                allowed_indices          =  np.nonzero(errors)
                times                    =  times[allowed_indices]
                errors                   =  errors[allowed_indices]

                if errors.size >= minimal_number_of_timestamps:
                    magnitudes          = magnitudes[allowed_indices]

                    weights = 1/errors**2

                    weighted_mean, sum_of_weights = np.average(magnitudes, weights = weights, returned=True)

                    scaled_residuals = (magnitudes-weighted_mean)/errors
                    # Add a 5 sigma "outburst" and check if it is "detected".
                    # scaled_residuals[50] += 10

                    manually_calculated_chi_squared_using_errors = np.sum(scaled_residuals**2)
                    print('Chi² = {0} and dof = {1}'.format(manually_calculated_chi_squared_using_errors, magnitudes.size -1))
                    probability_of_constancy = 1 - chi2.cdf(manually_calculated_chi_squared_using_errors, magnitudes.size -1)

                    # probability_of_constancy     = ts.adfuller(scaled_residuals, maxlag = magnitudes.size -2, autolag = 'AIC')[1]
                    # probability_of_constancy = jb(scaled_residuals)[1]
                    # ljung_box_output             = acorr_ljungbox(scaled_residuals)
                    # probability_of_constancy   = ljung_box_output[1][-1]
                    # probability_of_constancy = omni_normtest(scaled_residuals)[1]

                    print('p-value = {0}'.format(probability_of_constancy))

                    if probability_of_constancy < confidence_limit:

                        #                 Plot this time series.
                        #                 pyl.plot(times, weighted_mean * np.ones((magnitudes.size)), '--')
                        #                 pyl.errorbar(times, magnitudes, yerr = errors,  fmt = 'bo')
                        #                 pyl.title('This time series does not pass the test for constantness at {0}% confidence'.format(100 * (1- confidence_limit)))
                        #                 pyl.legend(['weighted mean', 'possibly variable source'])
                        #                 pyl.xlabel('Time (MJD)')
                        #                 pyl.ylabel('Magnitude')

                        number_of_colours_that_show_variability        += 1
                    else:
                        number_of_colours_that_do_not_show_variability += 1

            # Only update the bookkeeping if the time series were long enough for all colours.
            if number_of_colours_that_show_variability + number_of_colours_that_do_not_show_variability == frequency_indices.size:
                number_of_time_series += frequency_indices.size

                # A source is only considered varaiable if it is shown in all colours.
                if number_of_colours_that_show_variability == frequency_indices.size:
                    number_of_variable_time_series += number_of_colours_that_show_variability
                else:
                    number_of_constant_time_series += number_of_colours_that_show_variability

                # number_of_variable_time_series     += number_of_colours_that_show_variability
                #
                number_of_constant_time_series     += number_of_colours_that_do_not_show_variability

            #                 pyl.savefig(path_to_plots + 'time_series_' + str(number_of_variable_sources) + '.png', bbbox_inches = 'tight')
            #                 pyl.clf()
            print('Now a total of {0} out of {1} time series seem variable. So {2} seem constant.'.\
                  format(number_of_variable_time_series, number_of_time_series, number_of_constant_time_series))


