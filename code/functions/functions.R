######## project functions ###################

####################################
# decay generating functions.      #
####################################

# power law decay of titers
power_law_function = function(A1,k,time) {A1*time^k} #for decay, k<0

# sum of decaying titers after secondary response, such that time will be greater than tau
additive_power_law_function = function(A1, k, delta, tau, time){  #tau is the time between peak primary and peak secondary responses, delta is the fold boost from pre boost to peak titer on secondary response
  A2 = power_law_function(A1, k, time = tau)*delta - power_law_function(A1, k, time = tau + 1)
  return(power_law_function(A1 = A1, k = k, time = time) + power_law_function(A1 = A2, k = k, time = time - tau))} #sum of previous titers plus newly generated titers

###############################
#fractional error calculation #
###############################

# fractional error function 
fractional_error = function(k.true, k.estimated) {(k.estimated-k.true)/k.true}
