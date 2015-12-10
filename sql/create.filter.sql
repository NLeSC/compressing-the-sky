/*+-------------------------------------------------------------------------------+
 *| Conversion of magitudes in the CFHTLS ugriz filter system [1]                 |
 *| to physical fluxes goes via the SDSS ugriz filter system that                 |
 *| follows the AB magnitude system [2]. The relations between the                |
 *| filters are described by S. Gwyn, 2008 [3].                                   |
 *|                                                                               |
 *| As an example we use the magnitude of the CFHTLS g band.                      |
 *| In [2] and [3] it is given that                                               |
 *| g_Mega = g_SDSS - 0.153                                                       |
 *| and the SDSS filters follow AB mag system, so the flux is                     |
 *| f_AB = 10^{-0.4 * (mag_g_Mega + 0.153 -8.90)} Jy [4]                        |
 *|                                                                               |
 *| Magnitude errors [5]:                                                         |
 *| m +/- dm = m_0 - 2.5log(S +/- N)                                              |
 *|          = m_0 - 2.5 logS - 2.5log(1 +/- N/S)                                 |
 *|            ^^^^^^^^^^^^^^   ^^^^^^^^^^^^^^^^^                                 |
 *|          =       m                 dm                                         |
 *|                                                                               |
 *| dm \approx 2.5 log(1 + \frac{1}{S/N}) (NOTE: in log +/- not symmetric)        |
 *|    \approx 1.086 (N/S)                                                        |
 *|                                                                               |
 *|                                                                               |
 *| [1] www.cadc-ccda.hia-iha.nrc-cnrc.gc.ca/en/megapipe/docs/filt.html           |
 *| [2] en.wikipedia.org/wiki/AB_magnitude                                        |
 *| [3] www.jstor.org/stable/pdfplus/10.1086/526794.pdf                           |
 *| [4] 1 Jy = 10^{-26} W Hz^{-1} m^{-2} = 10^{-23} erg s^{-1} Hz^{-1} cm^{-2}    |
 *| [5] www.astro.wisc.edu/~mab/education/astro500/lectures/a500_lecture2_s13.pdf |
 *|                                                                               |
 *+-------------------------------------------------------------------------------+
 */

CREATE TABLE filter
  (id INT
  ,band VARCHAR(2)
  ,sdss_offset DECIMAL(4,3)
  )
;

/*
 * G: g_Mega = g_SDSS -0.153
 */
INSERT INTO filter
  (id 
  ,band 
  ,sdss_offset
  )
VALUES
   (1, 'U', -0.241)
  ,(2, 'G', -0.153)
  ,(3, 'R', -0.024)
  ,(4, 'I', -0.085)
  ,(5, 'I2', -0.003)
  ,(6, 'Z', 0.074)
;

