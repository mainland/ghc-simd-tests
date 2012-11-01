GHCBIN	?= $(HOME)/ghc/ghc-simd-build/inplace/bin
#GHCBIN	?= /5playpen/gmainlan/ghc-simd-build/inplace/bin
GHC	?= $(GHCBIN)/ghc-stage2
GHCPKG	?= $(GHCBIN)/ghc-pkg
CABAL	?= $(GHCBIN)/ghc-cabal

LLVMOPT = opt
LLVMLLC = llc

GHCFLAGS+=$(EXTRAGHCFLAGS)

GHCFLAGS+=-rtsopts -threaded -Odph
GHCFLAGS+=-O2 -fllvm -optlo-O3 -optc-O3
#GHCFLAGS+=-fcpr-off -fno-liberate-case
#GHCFLAGS+=-optc-march=corei7
#GHCFLAGS+=-optc-march=amdfam10

GHCFLAGS+= \
	-hide-all-packages \
	-package base \
	-package dph-lifted-vseg \
	-package ghc-prim \
	-package primitive \
	-package random \
	-package time \
	-package vector

#GHCFLAGS+=-pgmlo=$(LLVMOPT) -pgmlc=$(LLVMLLC)
#GHCFLAGS+=-optlc=--enable-tbaa=true
#GHCFLAGS+=-optlo=-loop-unroll -optlo=-indvars -optlo=-loop-simplify 

GHCFLAGS+=-dcore-lint

#GHCFLAGS+=-keep-llvm-file
#GHCFLAGS+=-keep-s-file
#GHCFLAGS+=-keep-tmp-files
#GHCFLAGS+=-fmax-simplifier-iterations=10

#GHCFLAGS+=-ddump-to-file
#GHCFLAGS+=-dverbose-core2core
#GHCFLAGS+=-ddump-cmmz
#GHCFLAGS+=-ddump-llvm
#GHCFLAGS+=-ddump-simpl
#GHCFLAGS+=-ddump-simpl-iterations
#GHCFLAGS+=-ddump-simpl-stats
#GHCFLAGS+=-ddump-stg
#GHCFLAGS+=-ddump-prep
GHCFLAGS+=-dsuppress-all -dppr-case-as-let -dppr-cols200

#GHCCOREFLAGS+=-ddump-occur-anal
#GHCCOREFLAGS+=-ddump-rule-firings
#GHCCOREFLAGS+=-ddump-rule-rewrites

#GHCCOREFLAGS+=-dsuppress-coercions
#GHCCOREFLAGS+=-dsuppress-idinfo
#GHCCOREFLAGS+=-dsuppress-module-prefixes 
#GHCCOREFLAGS+=-dsuppress-type-applications
#GHCCOREFLAGS+=-dsuppress-uniques

MULTIVECTORFLAGS+=-package multivector -package-db multivector/dist/package.conf.inplace

EXAMPLES = sum dotp saxpy prim roman seq-bench par-bench
EXAMPLEINCS = $(foreach EXAMPLE,$(EXAMPLES),-iexamples/$(EXAMPLE))

.PHONY : all
all : $(EXAMPLES)

.PHONY : clean
clean :
	rm -rf obj
	rm -rf $(EXAMPLES)
	rm -rf multivector/dist
	find examples util -name '*.s' | xargs rm -f
	find examples util -name '*.ll' | xargs rm -f
	find examples util -name '*.dump-*' | xargs rm -f

multivector/dist/package.conf.inplace :
	(cd multivector && cabal configure --disable-library-profiling --with-ghc=$(GHC) --with-ghc-pkg=$(GHCPKG) && cabal build)

INPLACE_PACKAGES = \
    multivector/dist/package.conf.inplace

SUM_SRC = \
    examples/sum/Sum/Float/cscalar.c \
    examples/sum/Sum/Float/CScalar.hs \
    examples/sum/Sum/Float/Manual.hs \
    examples/sum/Sum/Float/cmanual.c \
    examples/sum/Sum/Float/CManual.hs \
    examples/sum/Sum/Float/Multivector.hs \
    examples/sum/Sum/Float/Scalar.hs \
    examples/sum/Sum/Float/Vector.hs

DOTP_SRC = \
    examples/dotp/Dotp/Float/cscalar.c \
    examples/dotp/Dotp/Float/CScalar.hs \
    examples/dotp/Dotp/Float/Manual.hs \
    examples/dotp/Dotp/Float/cmanual.c \
    examples/dotp/Dotp/Float/CManual.hs \
    examples/dotp/Dotp/Float/Multivector.hs \
    examples/dotp/Dotp/Float/Scalar.hs \
    examples/dotp/Dotp/Float/Vector.hs \
    examples/dotp/Dotp/Float/VectorAlt1.hs \
    examples/dotp/Dotp/Float/VectorAlt2.hs \
    examples/dotp/Dotp/Float/VectorAlt3.hs \
    examples/dotp/Dotp/Float/VectorAlt4.hs \
    examples/dotp/Dotp/Double/Dph.hs \
    examples/dotp/Dotp/Double/DphPA.hs \
    examples/dotp/Dotp/Double/DphMulti.hs \
    examples/dotp/Dotp/Double/Manual.hs \
    examples/dotp/Dotp/Double/cmanual.c \
    examples/dotp/Dotp/Double/CManual.hs \
    examples/dotp/Dotp/Double/Multivector.hs \
    examples/dotp/Dotp/Double/Scalar.hs \
    examples/dotp/Dotp/Double/Vector.hs \
    examples/dotp/Dotp/Double/VectorAlt4.hs

SAXPY_SRC = \
    examples/saxpy/Saxpy/Float/Multivector.hs \
    examples/saxpy/Saxpy/Float/Scalar.hs \
    examples/saxpy/Saxpy/Float/Vector.hs

sum : examples/sum/Main.hs $(SUM_SRC) $(INPLACE_PACKAGES)
	$(GHC) $(GHCFLAGS) $(MULTIVECTORFLAGS) $< $(SUM_SRC) \
	    --make \
	    -odir obj/$* -hidir obj/$* -iexamples/sum/$* -iutil \
	    -o $@

prim : examples/prim/Main.hs
	$(GHC) $(GHCFLAGS) $< \
	    --make \
	    -odir obj/$* -hidir obj/$* -iexamples/$* -iutil \
	    -o $@

roman : examples/roman/Main.hs
	$(GHC) $(GHCFLAGS) $< \
	    --make \
	    -odir obj/$* -hidir obj/$* -iexamples/$* -iutil \
	    -o $@

dotp : examples/dotp/Main.hs $(DOTP_SRC) $(INPLACE_PACKAGES)
	$(GHC) $(GHCFLAGS) $(MULTIVECTORFLAGS) $< $(DOTP_SRC) \
	    --make \
	    -odir obj/$* -hidir obj/$* -iexamples/$* -iutil \
	    -o $@

saxpy : examples/saxpy/Main.hs $(SAXPY_SRC) $(INPLACE_PACKAGES)
	$(GHC) $(GHCFLAGS) $(MULTIVECTORFLAGS) $< $(SAXPY_SRC) \
	    --make \
	    -odir obj/$* -hidir obj/$* -iexamples/$* -iutil \
	    -o $@

seq-bench : benchmarks/seq-bench/Main.hs $(SUM_SRC) $(DOTP_SRC) $(INPLACE_PACKAGES)
	$(GHC) $(GHCFLAGS) $(MULTIVECTORFLAGS) $< $(SUM_SRC) $(DOTP_SRC) \
	    --make \
	    -odir obj/$* -hidir obj/$* -iexamples/sum -iexamples/dotp -ibench -iutil \
	    -o $@

par-bench : benchmarks/par-bench/Main.hs $(DOTP_SRC) $(INPLACE_PACKAGES)
	$(GHC) $(GHCFLAGS) $(MULTIVECTORFLAGS) $< $(DOTP_SRC) \
	    --make \
	    -odir obj/$* -hidir obj/$* -iexamples/sum -iexamples/dotp -ibench -iutil \
	    -o $@

%.core : %.hs
	$(GHC) $(GHCFLAGS) $(EXAMPLEINCS) --make -odir obj/$* -hidir obj/$* $<
	$(GHC) $(GHCFLAGS) $(EXAMPLEINCS) $(GHCCOREFLAGS) -iobj/$* -fforce-recomp -odir obj/$* -hidir obj/$* -c $< >$@ 2>&1

%.bc : %.ll
	$(LLVMOPT) -mem2reg $^ -o $@

%.s : %.bc
	$(LLVMLLC) -O1 -relocation-model=static $^ -o $@
