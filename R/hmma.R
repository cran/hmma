#'\code{hmma}: A package for the contruction of asymmetric hidden Markov models
#'
#'The \code{hmma} package is able to construct asymmetric hidden Markov models
#'(HMM-As) from data. HMM-As are similar to regular HMMs, but use Bayesian
#'Networks (BNs) in their emission distribution.
#'
#'HMMs  have  successfully  been  used  in  “speech recognition  systems,  in
#'numerous  applications  incomputational molecular biology, in data
#'compression, and in other areas of artificial intelligenceand pattern
#'recognition”.
#'
#'When limited data is available, HMMs may be unable to correctly capture these
#'distributions.Bueno et al. show that HMMs can be enriched by employing
#'state-specific Bayesian networks(BNs), i.e. BNs that may be different from
#'state to state. This enables the model to better capturecertain
#'independencies, depending on the state. The resulting models are called
#'asymmetrichidden Markov models (HMM-As). (see:
#'\url{http://dx.doi.org/10.1016/j.ijar.2017.05.011} for more information)
#'
#'@docType package
#'@name hmma
#'
NULL
