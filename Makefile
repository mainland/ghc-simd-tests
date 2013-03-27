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

GHCFLAGS+=-DEIGEN_VECTORIZE_SSE4_2 -DNDEBUG

GHCFLAGS+=-pgmc $(GCC)
GHCFLAGS+=-optc-O3 -optc-msse4.2 -optc-ffast-math -optc-ftree-vectorize -optc-funroll-loops

#GHCFLAGS+=-pgmc $(ICC) -pgml $(ICC)
#GHCFLAGS+=-optc-Qrestrict -optc-Ox -optc-Oi -optc-Ot -optc-Ob2 -optc-msse4.2 -optc-fp-model -optcfast

GHCFLAGS+=-rtsopts -threaded -Odph
GHCFLAGS+=-O2 -msse4.2
GHCFLAGS+=-fllvm -optlo-O3
#GHCFLAGS+=-fno-liberate-case -funfolding-use-threshold1000 -funfolding-keeness-factor1000

#GHCFLAGS+=-optc-ggdb -optc-fverbose-asm
#GHCFLAGS+=-optc-ftree-vectorizer-verbose=5
#GHCFLAGS+=-fcpr-off -fno-liberate-case
#GHCFLAGS+=-optc-march=corei7
#GHCFLAGS+=-optc-march=amdfam10

GHCFLAGS+= \
	-hide-all-packages \
	-package base \
	-package deepseq \
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
#GHCFLAGS+=-optlo=-vectorize
#GHCFLAGS+=-optlo=-vectorize-loops
#GHCFLAGS+=-optlo=-enable-unsafe-fp-math
#GHCFLAGS+=-optlo=-fp-contract=fast
#GHCFLAGS+=-optlo=-loop-unroll
#GHCFLAGS+=-optlo=-debug
#GHCFLAGS+=-optlo=-print-after-all -optlo=-print-before-all

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
#GHCFLAGS+=-ddump-asm
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

EXAMPLES = prim roman sanity seq-bench par-bench sse-bench
EXAMPLEINCS = $(foreach EXAMPLE,$(EXAMPLES),-iexamples/$(EXAMPLE))

.PHONY : all
all : $(EXAMPLES)

.PHONY : clean
clean :
	rm -rf obj
	rm -rf $(EXAMPLES)
	rm -rf $(FIGS)
	rm -f data/seq-bench.dat data/par-bench.dat data/sse-bench.dat
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
    common/Util/Time.hs \
    common/Util/time.c \
    common/Vector.hs

UTIL_SRC = \
    common/Util/Benchmark.hs \
    common/Util/MmMalloc.c \
    common/Util/MmMalloc.hs \
    common/Util/Random.hs \
    common/Util/Statistics.hs \
    common/Util/Time.hs \
    common/Util/time.c \
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

par-bench : benchmarks/par-bench/Main.hs $(COMMON_SRC) $(INPLACE_PACKAGES)
	$(GHC) $(GHCFLAGS) $< $(COMMON_SRC) \
	    --make \
	    -odir obj/par-bench/$* -hidir obj/par-bench/$* -icommon \
	    -o $@

SSE_BENCH_SRC = \
	benchmarks/sse-bench/Common.hs \
	benchmarks/sse-bench/Kahan/Scalar.hs \
	benchmarks/sse-bench/Kahan/SSE.hs \
	benchmarks/sse-bench/Sum/Scalar.hs \
	benchmarks/sse-bench/Sum/SSE.hs \
	benchmarks/sse-bench/Dotp/Scalar.hs \
	benchmarks/sse-bench/Dotp/SSE.hs \
	benchmarks/sse-bench/Saxpy/Scalar.hs \
	benchmarks/sse-bench/Saxpy/SSE.hs \
	benchmarks/sse-bench/Rbf/Scalar.hs \
	benchmarks/sse-bench/Rbf/SSE.hs \
	benchmarks/sse-bench/Kde/Scalar.hs \
	benchmarks/sse-bench/Kde/SSE.hs \
	benchmarks/sse-bench/VarianceUnbiased/Scalar.hs \
	benchmarks/sse-bench/VarianceUnbiased/SSE.hs \
	benchmarks/sse-bench/SMVM/Scalar.hs \
	benchmarks/sse-bench/Quickhull/Solver/Scalar.hs \
	benchmarks/sse-bench/Quickhull/Solver/SSE.hs

sse-bench : benchmarks/sse-bench/Main.hs $(SSE_BENCH_SRC) $(UTIL_SRC)
	$(GHC) $(GHCFLAGS) $< $(UTIL_SRC) \
	    --make -DSTORABLE=0 \
	    -odir obj/sse-bench/$* -hidir obj/sse-bench/$* -icommon -ibenchmarks/sse-bench \
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
# Benchmark data
#

data/seq-bench.dat : seq-bench
	./seq-bench rbf dotp >$@

data/par-bench.dat : par-bench
	./par-bench +RTS -N16 >$@

data/sse-bench.dat : sse-bench
	./sse-bench >$@

#
# Figures
#

PLOT      = ./bin/plot.py

FIGS = \
	figs/dotp-serial-time.pdf \
	figs/dotp-serial-ratio.pdf \
	figs/dotp-serial-mflops.pdf \
	figs/dotp-parallel-ratio.pdf \
	figs/rbf-serial-ratio.pdf \
	figs/rbf-mflops.pdf

.PHONY : figs
figs : $(FIGS)

.PHONY : figs-clean
figs-clean :
	rm -rf $(FIGS)

HASKELL_SEQDATA = data/2013-03-16/home-seq-bench.dat
C_SEQDATA       = data/2013-03-16/home-linux-gcc-rbf.dat
PARDATA         = data/2012-11-02/par-bench-cam05.dat

figs/dotp-serial-time.pdf : $(HASKELL_SEQDATA) $(PLOT)
	$(PLOT) --func dotp --time \
		--dataset haskell $< \
		--variant scalar --variant vector --variant cscalar --variant cmanual --variant cblas \
		--xmin=0 --xmax='2**24*1.5' --ymin='10**(-3)' --ymax=60 \
		-o $@

figs/dotp-serial-ratio.pdf : $(HASKELL_SEQDATA) $(PLOT)
	$(PLOT) --func dotp --ratio cscalar \
	        --variant cscalar --variant cmanual --variant vector --variant cblas \
		--errorbars \
		--ymin 0.4 --ymax 1.3 \
		--dataset haskell $< \
		-o $@

figs/dotp-serial-mflops.pdf : $(HASKELL_SEQDATA) $(PLOT)
	$(PLOT) --func dotp --mflops \
		--dataset haskell $< \
		--variant scalar --variant vector --variant cscalar --variant cmanual --variant cblas \
		--cache-label-at 3000 --l1 '32*1024' --l2 '256*1024' --l3 '8192*1024' \
		--flops-factor 2 --bytes-factor 16 \
		--ymax 10000 \
		--legend-loc 'upper right' \
		-o $@

figs/dotp-parallel-ratio.pdf : $(PARDATA) $(PLOT)
	$(PLOT) --func dotp --ratio cmanual \
		--data haskell $< \
		--variant cmanual --variant vector --variant dph --variant dphmulti \
		--xdata threads \
		-o $@

figs/rbf-serial-ratio.pdf : $(HASKELL_SEQDATA) $(PLOT)
	$(PLOT) --func rbf --ratio cmanual \
		--variant cmanual --variant vector \
		--errorbars \
		--dataset haskell $< \
		-o $@

figs/rbf-mflops.pdf : $(HASKELL_SEQDATA) $(C_SEQDATA) $(PLOT)
	$(PLOT) --func rbf --mflops \
		--variant vector --variant boost --variant blitz --variant eigen_abs_bad --variant eigen_abs_good \
		--data haskell $(HASKELL_SEQDATA) \
		--data gcc $(C_SEQDATA) \
		--ymax 12000 \
		--sort-legend-at '256*1024' \
		--cache-label-at 3000 --l1 '32*1024' --l2 '256*1024' --l3 '8192*1024' \
		--flops-factor 3 --bytes-factor 16 \
		-o $@

FIGDATA?=data/2013-03-18

.PHONY : dotp-ratio-plot
dotp-ratio-plot : $(FIGDATA)/seq-bench.dat
	$(PLOT) --func dotp --ratio cmanual \
		--variant cmanual --variant vector --variant cblas \
		--errorbars \
		--dataset haskell $(FIGDATA)/seq-bench.dat

.PHONY : rbf-mflops-plot
rbf-mflops-plot : $(FIGDATA)/seq-bench.dat $(FIGDATA)/linux-gcc-rbf.dat
	$(PLOT) --func rbf \
		--variant vector --variant boost --variant blitz --variant eigen --variant eigen_abs_bad --variant eigen_abs_good \
		--data haskell $(FIGDATA)/seq-bench.dat \
		--data gcc $(FIGDATA)/linux-gcc-rbf.dat \
		--mflops \
		--ymax 12000 \
		--sort-legend-at '256*1024' \
		--cache-label-at 3000 --l1 '32*1024' --l2 '256*1024' --l3 '8192*1024' \
		--flops-factor 3 --bytes-factor 16


.PHONY : dotp-parallel-ratio-plot
dotp-parallel-ratio-plot : data/par-bench.dat # data/2012-11-02/par-bench-cam05.dat
	$(PLOT) --func dotp --ratio cmanual \
		--data haskell $< \
		--variant cmanual --variant vector --variant dph --variant dphmulti \
		--xdata threads
