GHCBIN	?= $(HOME)/ghc/ghc-simd-build/inplace/bin
#GHCBIN	?= /5playpen/gmainlan/ghc-simd-build/inplace/bin
GHC	?= $(GHCBIN)/ghc-stage2
GHCPKG	?= $(GHCBIN)/ghc-pkg
CABAL	?= $(GHCBIN)/ghc-cabal

BLITZ    ?= external/blitz-0.10
BOOST    ?= external/boost_1_53_0
EIGEN    ?= external/eigen-eigen-5097c01bcdc4
SALT     ?= external/SALT
GOTOBLAS ?= $(HOME)/software/GotoBLAS2
GCC      ?= $(HOME)/local/gcc-4.7.2-linux-x86_64/bin/gcc
ICC      ?= icc

LLVMOPT = opt
LLVMLLC = llc

GHCFLAGS+=$(EXTRAGHCFLAGS)

GHCFLAGS+=-Werror

GHCFLAGS+=-pgmc $(GCC)
GHCFLAGS+=-optc-O3 -optc-ftree-vectorize -optc-msse4 -optc-ffast-math -optc-funroll-loops -optc-mtune=corei7

#GHCFLAGS+=-pgmc $(ICC) -pgml $(ICC)
#GHCFLAGS+=-optc-O3 -optc-fast -optc-xhost

GHCFLAGS+=-rtsopts -threaded -Odph
GHCFLAGS+=-O2 -fllvm
GHCFLAGS+=-optlo-O3 -optlc-mcpu=corei7 -optlc-mattr=sse42
#GHCFLAGS+=-optc-ggdb -optc-fverbose-asm
#GHCFLAGS+=-optc-ftree-vectorizer-verbose=5
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

GHCFLAGS+=-I$(BLITZ)
GHCFLAGS+=-I$(SALT)
GHCFLAGS+=-I$(BOOST)
GHCFLAGS+=-I$(EIGEN)
GHCFLAGS+=-I$(GOTOBLAS) $(GOTOBLAS)/libgoto2.a
GHCFLAGS+=-lstdc++

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
#GHCFLAGS+=-ddump-cmm
#GHCFLAGS+=-ddump-cmm-cfg
#GHCFLAGS+=-ddump-cmm-proc
GHCFLAGS+=-ddump-asm
#GHCFLAGS+=-ddump-llvm
#GHCFLAGS+=-ddump-simpl
#GHCFLAGS+=-ddump-simpl-iterations
#GHCFLAGS+=-ddump-simpl-stats
#GHCFLAGS+=-ddump-stg
#GHCFLAGS+=-ddump-prep
GHCFLAGS+=-dsuppress-all -dppr-case-as-let -dppr-cols200
#GHCFLAGS+=-ddump-rule-firings -ddump-rule-rewrites

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
	rm -rf $(FIGS)
	rm -f data/seq-bench.dat data/par-bench.dat
	rm -rf multivector/dist
	find common examples tests benchmarks -name '*.s' | xargs rm -f
	find common examples tests benchmarks -name '*.ll' | xargs rm -f
	find common examples tests benchmarks -name '*.dump-*' | xargs rm -f

COMMON_SRC = \
    common/Dotp/Double/CBlas.hs \
    common/Dotp/Double/cmanual.c \
    common/Dotp/Double/CManual.hs \
    common/Dotp/Double/cscalar.c \
    common/Dotp/Double/CScalar.hs \
    common/Dotp/Double/Dph.hs \
    common/Dotp/Double/DphMulti.hs \
    common/Dotp/Double/DphPA.hs \
    common/Dotp/Double/Manual.hs \
    common/Dotp/Double/Scalar.hs \
    common/Dotp/Double/Vector.hs \
    common/Dotp/Float/CBlas.hs \
    common/Dotp/Float/cmanual.c \
    common/Dotp/Float/CManual.hs \
    common/Dotp/Float/cscalar.c \
    common/Dotp/Float/CScalar.hs \
    common/Dotp/Float/Manual.hs \
    common/Dotp/Float/Scalar.hs \
    common/Dotp/Float/Vector.hs \
    common/Saxpy/Float/Scalar.hs \
    common/Saxpy/Float/Vector.hs \
    common/Rbf/Double/blitz.cpp \
    common/Rbf/Double/boost.cpp \
    common/Rbf/Double/cmanual.c \
    common/Rbf/Double/cmanual_intermediate.c \
    common/Rbf/Double/eigen.cpp \
    common/Rbf/Double/salt.cpp \
    common/Rbf/Double/Blitz.hs \
    common/Rbf/Double/Boost.hs \
    common/Rbf/Double/CManual.hs \
    common/Rbf/Double/CManualIntermediate.hs \
    common/Rbf/Double/Eigen.hs \
    common/Rbf/Double/SALT.hs \
    common/Rbf/Double/Vector.hs \
    common/Rbf/Double/VectorAlt1.hs \
    common/Rbf/Double/VectorAlt2.hs \
    common/Sum/Double/cmanual.c \
    common/Sum/Double/CManual.hs \
    common/Sum/Double/cscalar.c \
    common/Sum/Double/CScalar.hs \
    common/Sum/Double/Manual.hs \
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
    common/Util/MmMalloc.c \
    common/Util/MmMalloc.hs \
    common/Util/Random.hs \
    common/Util/Statistics.hs \
    common/Vector.hs

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
	$(LLVMLLC) -O3 -relocation-model=static $^ -o $@

# obj/seq-bench/Dotp/Double/Vector.bc : common/Dotp/Double/Vector.ll
# 	$(LLVMOPT) $< -o $@ --enable-tbaa=true -O3

# obj/seq-bench/Dotp/Double/Vector.s : obj/seq-bench/Dotp/Double/Vector.bc
# 	$(LLVMLLC) -O3 '-relocation-model=static' $< -o $@ --enable-tbaa=true -mattr=+sse2

# obj/seq-bench/Dotp/Double/Vector.o : common/Dotp/Double/Vector.s
# 	$(GCC) -fno-stack-protector -Wl,--hash-size=31 -Wl,--reduce-memory-overheads -DTABLES_NEXT_TO_CODE -x assembler-with-cpp -c $< -o $@

obj/seq-bench/Rbf/Double/Vector.o : common/Rbf/Double/Vector.ll
	 $(GHC) $(GHCFLAGS) -v -c $< -o $@

#
# Figures
#

PLOT      = ./bin/plot.py

FIGS = \
	figs/dotp-serial.pdf \
	figs/dotp-serial-ratio.pdf \
	figs/dotp-serial-ratio-1.pdf \
	figs/dotp-serial-ratio-2.pdf \
	figs/rbf-serial-ratio.pdf \
	figs/dotp-parallel.pdf \
	figs/dotp-parallel-ratio.pdf

figs : $(FIGS)

data/seq-bench.dat : seq-bench
	./seq-bench rbf >$@

data/par-bench.dat : par-bench
	./par-bench +RTS -N8 >$@

figs/dotp-serial.pdf : data/seq-bench.dat $(PLOT)
	$(PLOT) --dotp $< -o $@

figs/dotp-serial-ratio.pdf : data/seq-bench.dat $(PLOT)
	$(PLOT) --dotp --ratio --ymax 3 $< -o $@

figs/dotp-serial-ratio-1.pdf : data/seq-bench.dat $(PLOT)
	$(PLOT) --dotp --ratio --nsets 1 $< -o $@

figs/dotp-serial-ratio-2.pdf : data/seq-bench.dat $(PLOT)
	$(PLOT) --dotp --ratio --nsets 2 $< -o $@

figs/rbf-serial-ratio.pdf : data/seq-bench.dat $(PLOT)
	$(PLOT) --rbf --ratio --alt 4 --legend-fontsize 9 --ymax 2 $< -o $@

figs/dotp-parallel.pdf : data/par-bench.dat $(PLOT)
	$(PLOT) --par-dotp $< -o $@

figs/dotp-parallel-ratio.pdf : data/par-bench.dat $(PLOT)
	$(PLOT) --par-dotp --ratio $< --ymin 0 --ymax 2.0 -o $@

figs/rbf-mflops-slow.png : $(PLOT)
	$(PLOT) --rbf --legend-fontsize 9 -o $@ \
		data/2013-02-07/seq-bench-home-gcc.dat data/2013-02-07/seq-bench-home-icc.dat

figs/rbf-mflops-fast.png : $(PLOT)
	$(PLOT) --rbf --legend-fontsize 9 --alt 1 -o $@ \
		data/2013-02-07/seq-bench-home-gcc.dat data/2013-02-07/seq-bench-home-icc.dat
