Tbalh <- 55
HDH <- sum(Heaviside(Tbalh- DAY_TMY3$Temp.Outside.F)*(Tbalh- DAY_TMY3$Temp.Outside.F))

HDD <- HDH / 24

Heaviside <- 
  function(x, a = 0) 
  {   
    # A function implemented by Diethelm Wuertz
    
    # Description:
    #   Computes the Heaviside or unit step function.
    
    # Arguments:
    #   x - a numeric vector.
    #   a - the location of the break.
    
    # Details:
    #   The Heaviside step function is 1 for x>a, 1/2 for x=a,
    #   and 0 for x<a.
    
    # Notes:
    #   Heaviside Unit Step Function and Related Functions
    #   See:  http://mathworld.wolfram.com/HeavisideStepFunction.html
    #   Note: sign(x) is part of R's base package
    
    # FUNCTION:
    
    # Compute H:
    result = (sign(x-a) + 1)/2
    
    # Return Value:
    result
  }