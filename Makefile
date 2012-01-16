GHC     = $(HOME)/ghc/ghc-working/inplace/bin/ghc-stage2
LLVMOPT = opt
LLVMLLC = llc

GHCFLAGS=-O2 -fllvm -Iinclude -dcore-lint
GHCFLAGS+=-pgmlo=$(LLVMOPT) -pgmlc=$(LLVMLLC)

GHCFLAGS+=-debug -rtsopts=all
#GHCFLAGS+=-keep-llvm-file
#GHCFLAGS+=-keep-s-file
#GHCFLAGS+=-keep-tmp-files
#GHCFLAGS+=-fmax-simplifier-iterations=10

GHCFLAGS+=-ddump-to-file
#GHCFLAGS+=-ddump-cmm
#GHCFLAGS+=-ddump-simpl
#GHCFLAGS+=-ddump-simpl-iterations
#GHCFLAGS+=-ddump-simpl-stats
#GHCFLAGS+=-ddump-stg

#GHCCOREFLAGS+=-ddump-occur-anal
#GHCCOREFLAGS+=-ddump-rule-firings
#GHCCOREFLAGS+=-ddump-rule-rewrites
#GHCCOREFLAGS+=-ddump-simpl
#GHCCOREFLAGS+=-ddump-simpl-iterations
#GHCCOREFLAGS+=-ddump-simpl-stats

#GHCCOREFLAGS+=-dsuppress-coercions
#GHCCOREFLAGS+=-dsuppress-idinfo
#GHCCOREFLAGS+=-dsuppress-module-prefixes 
#GHCCOREFLAGS+=-dsuppress-type-applications

EXAMPLES = sum intsum dotp saxpy prim roman
EXAMPLEINCS = $(foreach EXAMPLE,$(EXAMPLES),-iexamples/$(EXAMPLE))

.PHONY : all
all : $(EXAMPLES)

.PHONY : clean
clean :
	rm -rf obj
	rm -rf $(EXAMPLES)
	find . -name '*.s' | xargs rm -f
	find . -name '*.ll' | xargs rm -f
	find . -name '*.dump-*' | xargs rm -f

% : examples/%/Main.hs
	$(GHC) $(GHCFLAGS) --make -odir obj/$* -hidir obj/$* -iexamples/$* -o $@ $<

%.core : %.hs
	$(GHC) $(GHCFLAGS) $(EXAMPLEINCS) --make -odir obj/$* -hidir obj/$* $<
	$(GHC) $(GHCFLAGS) $(EXAMPLEINCS) $(GHCCOREFLAGS) -iobj/$* -fforce-recomp -odir obj/$* -hidir obj/$* -c $< >$@ 2>&1

%.bc : %.ll
	$(LLVMOPT) -mem2reg $^ -o $@

%.s : %.bc
	$(LLVMLLC) -O1 -relocation-model=static $^ -o $@
