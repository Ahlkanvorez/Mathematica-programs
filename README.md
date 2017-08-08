# Mathematica-programs

A collection of various programs in Mathematica, all for fun.

### What's in the repo?

#### Linear Algebra Function Approximations

This file contains a Mathematica implementation of the Gram-Schmidt algorithm, an algorithm for orthogonal projection, and of an algorithm for computing an optimal polynomial approximation of a given function. There is an example usage at the bottom of the file where the Sin function is approximated using the projection technique, and compared with a Taylor Series approximation of the same degree; the results are graphed, and the projection-based approximation is shown to be far superior upon executing the code and viewing the graph.

#### RSA Encryption

This file contains an implementation of the RSA Encryption algorithm, which is a public-key cryptography algorithm -- i.e. one which allows two people to exchange public keys so that they can each send the other encrypted messages that only the other person can read -- and an algorithm for generating a random co-prime integer within a range. There is an example usage at the bottom of the file where a long string of text is encrypted using random primes generated, and then the encrypted text and the correctly decrypted text are displayed.

#### Bar-Scatter-Graph

This file is a rather ugly but functional implementation of a custom graphing function, that can take statistical data and generate the graph dynamically. It proved useful to a friend who does Neuroscience research, and I thought it was fun writing it, so I've included it here. The resulting graph is a bar chart with a scatter-plot on top of the bar chart that has standard-error markings atop each scatter plot. Labels and titles are customizable.
