

def mirror(q1, g, h, jmp):
  k = len(q1)
  q2 = random.random.normal(q1, j, k)   # (mean, sd, length)
  residual = np.dot(g,q2) - h
  q10 = q1

  while(np.any(residual < 0)):
      epsilon = q2 - q10
      w = residual < 0
      alfa = {{h-np.dot(g, q10)}/np.dot(g, epsilon)}[w]
      whichalfa = (alfa == np.min(alfa))
      j = w[whichminalfa]
      d = -1*residual[j]/np.sum(g[j]^2)
      q2 = q2+2*d*g[j]
      residual = np.dot(q,g2) - h
      q10 = q10 + alfa[whichminalfa]*epsilon
  return(q2)


def xsample(A,B,E,F,G,H,sdB,iter,outputlength,burn.length, jmp,x0,test):
    
