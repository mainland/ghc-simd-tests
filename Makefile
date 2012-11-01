GHCBIN	?= $(HOME)/ghc/ghc-simd-build/inplace/bin
#GHCBIN	?= /5playpen/gmainlan/ghc-simd-build/inplace/bin
GHC	?= $(GHCBIN)/ghc-stage2
GHCPKG	?= $(GHCBIN)/ghc-pkg
CABAL	?= $(GHCBIN)/ghc-cabal

LLVMOPT = opt
LLVMLLC = llc

GHCFLAGS+=$(EXTRAGHCFLAGS)

GHCFLAGS+=-Werror

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

EXAMPLES = prim roman sanity seq-bench par-bench
EXAMPLEINCS = $(foreach EXAMPLE,$(EXAMPLES),-iexamples/$(EXAMPLE))

.PHONY : all
all : $(EXAMPLES)

.PHONY : clean
clean :
	rm -rf obj
	rm -rf $(EXAMPLES)
	rm -rf multivector/dist
	find common examples tests benchmarks -name '*.s' | xargs rm -f
	find common examples tests benchmarks -name '*.ll' | xargs rm -f
	find common examples tests benchmarks -name '*.dump-*' | xargs rm -f

COMMON_SRC = \
    common/Dotp/Double/cmanual.c \
    common/Dotp/Double/CManual.hs \
    common/Dotp/Double/Dph.hs \
    common/Dotp/Double/DphMulti.hs \
    common/Dotp/Double/DphPA.hs \
    common/Dotp/Double/Manual.hs \
    common/Dotp/Double/Scalar.hs \
    common/Dotp/Double/Vector.hs \
    common/Dotp/Float/cmanual.c \
    common/Dotp/Float/CManual.hs \
    common/Dotp/Float/cscalar.c \
    common/Dotp/Float/CScalar.hs \
    common/Dotp/Float/Manual.hs \
    common/Dotp/Float/Scalar.hs \
    common/Dotp/Float/Vector.hs \
    common/Saxpy/Float/Scalar.hs \
    common/Saxpy/Float/Vector.hs \
    common/Sum/Double/Scalar.hs \
    common/Sum/Double/Vector.hs \
    common/Sum/Float/cmanual.c \
    common/Sum/Float/CManual.hs \
    common/Sum/Float/cscalar.c \
    common/Sum/Float/CScalar.hs \
    common/Sum/Float/Manual.hs \
    common/Sum/Float/Scalar.hs \
    common/Sum/Float/Vector.hs \
    common/Sum/Int64/Scalar.hs \
    common/Sum/Int64/Vector.hs \
    common/Util/Benchmark.hs \
    common/Util/Random.hs \
    common/Util/Statistics.hs \
    common/Util/Unsafe.hs

prim : examples/prim/Main.hs
	$(GHC) $(GHCFLAGS) $< \
	    --make \
	    -odir obj/prim/$* -hidir obj/prim/$* -icommon \
	    -o $@

roman : examples/roman/Main.hs
	$(GHC) $(GHCFLAGS) $< \
	    --make \
	    -odir obj/roman/$* -hidir obj/roman/$* -icommon \
	    -o $@

sanity : tests/sanity/Main.hs $(COMMON_SRC)
	$(GHC) $(GHCFLAGS) $< $(COMMON_SRC) \
	    --make \
	    -odir obj/sanity/$* -hidir obj/sanity/$* -icommon \
	    -o $@

seq-bench : benchmarks/seq-bench/Main.hs $(COMMON_SRC)
	$(GHC) $(GHCFLAGS) $< $(COMMON_SRC) \
	    --make \
	    -odir obj/seq-bench/$* -hidir obj/seq-bench/$* -icommon \
	    -o $@

par-bench : benchmarks/par-bench/Main.hs $(DOTP_SRC) $(INPLACE_PACKAGES)
	$(GHC) $(GHCFLAGS) $< $(COMMON_SRC) \
	    --make \
	    -odir obj/par-bench/$* -hidir obj/par-bench/$* -icommon \
	    -o $@

%.core : %.hs
	$(GHC) $(GHCFLAGS) $(EXAMPLEINCS) --make -odir obj/$* -hidir obj/$* $<
	$(GHC) $(GHCFLAGS) $(EXAMPLEINCS) $(GHCCOREFLAGS) -iobj/$* -fforce-recomp -odir obj/$* -hidir obj/$* -c $< >$@ 2>&1

%.bc : %.ll
	$(LLVMOPT) -mem2reg $^ -o $@

%.s : %.bc
	$(LLVMLLC) -O1 -relocation-model=static $^ -o $@
