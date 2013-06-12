ghci -W -cpp \
 -XTypeSynonymInstances -XFlexibleInstances  \
 -XFlexibleContexts -XMultiParamTypeClasses \
 -XFunctionalDependencies -XRankNTypes -XUndecidableInstances\
 -XDeriveFunctor -XStandaloneDeriving $1
#-isrc:prover:frontend/:basic/:interpreter/:dist/build/autogen/:dist/build/hopes/hopes-tmp/ \
