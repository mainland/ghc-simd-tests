GHC     = $(HOME)/ghc/ghc-working/inplace/bin/ghc-stage2
LLVMOPT = opt
LLVMLLC = llc

GHCFLAGS=-O2 -fllvm -Iinclude -dcore-lint
#GHCFLAGS+=-fmax-simplifier-iterations=10

GHCCOREFLAGS+=-ddump-simpl
GHCCOREFLAGS+=-ddump-simpl-iterations

GHCCOREFLAGS+=-ddump-rule-firings
GHCCOREFLAGS+=-ddump-rule-rewrites

GHCCOREFLAGS+=-dsuppress-coercions
GHCCOREFLAGS+=-dsuppress-idinfo
GHCCOREFLAGS+=-dsuppress-module-prefixes 
GHCCOREFLAGS+=-dsuppress-type-applications

EXAMPLES = sum intsum dotp prim
EXAMPLEINCS = $(foreach EXAMPLE,$(EXAMPLES),-iexamples/$(EXAMPLE))

.PHONY : all
all : $(EXAMPLES)

.PHONY : clean
clean :
	rm -rf obj
	rm -rf $(EXAMPLES)

% : examples/%/Main.hs
	$(GHC) $(GHCFLAGS) --make -odir obj/$* -hidir obj/$* -iexamples/$* -o $@ $<

%.core : %.hs
	$(GHC) $(GHCFLAGS) $(EXAMPLEINCS) --make -odir obj/$* -hidir obj/$* $<
	$(GHC) $(GHCFLAGS) $(EXAMPLEINCS) $(GHCCOREFLAGS) -iobj/$* -fforce-recomp -odir obj/$* -hidir obj/$* -c $< >$@ 2>&1

%.bc : %.ll
	$(LLVMOPT) -mem2reg $^ -o $@

%.s : %.bc
	$(LLVMLLC) -O1 -relocation-model=static $^ -o $@
