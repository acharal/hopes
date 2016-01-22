ghc -o polyhopes --make -cpp \
 -XTypeSynonymInstances -XFlexibleInstances  \
 -XFlexibleContexts -XMultiParamTypeClasses \
 -XFunctionalDependencies -XRankNTypes -XUndecidableInstances\
 -XDeriveFunctor -XStandaloneDeriving TestMain.hs \
-i../basic
#-isrc:prover:frontend/:basic/:interpreter/:dist/build/autogen/:dist/build/hopes/hopes-tmp/ \
