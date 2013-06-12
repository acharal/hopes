ghc -o polyhopes --make -cpp \
 -XTypeSynonymInstances -XFlexibleInstances  \
 -XFlexibleContexts -XMultiParamTypeClasses \
 -XFunctionalDependencies -XRankNTypes -XUndecidableInstances\
 -XDeriveFunctor -XStandaloneDeriving Main.hs
#-isrc:prover:frontend/:basic/:interpreter/:dist/build/autogen/:dist/build/hopes/hopes-tmp/ \
