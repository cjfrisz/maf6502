all : run-tests

target/main/scheme/%.so : src/main/scheme/%.ss
	mkdir -p $(@D)
	echo '(parameterize ([source-directories (cons "src/main/scheme/maf6502/assembler" (source-directories))])\
                (compile-file "$<" "$@"))'\
	  | scheme -q

target/test/scheme/%.so : src/test/scheme/%.ss target/test/scheme/srfi
	mkdir -p $(@D)
	echo '(compile-file "$<" "$@"))' | scheme -q --libdirs target/main/scheme:target/test/scheme

target/test/scheme/maf6502-test.so : src/test/scheme/maf6502-test.ss target/main/scheme/maf6502/cpu.so

target/test/scheme/parser-test.so : src/test/scheme/parser-test.ss target/main/scheme/maf6502/assembler/parser.so 

link-srfi-dirs : submodules/chez-srfi
	( cd submodules/chez-srfi && scheme --script link-dirs.chezscheme.sps )

target/test/scheme/srfi : link-srfi-dirs submodules/chez-srfi
	mkdir -p $(@D)
	ln -fs ../../../submodules/chez-srfi target/test/scheme/srfi

repl : src/test/scheme/maf6502-repl.ss target/main/scheme/maf6502.so
	scheme --libdirs target/main/scheme src/test/scheme/maf6502-repl.ss

run-tests : target/test/scheme/maf6502-test.so target/test/scheme/parser-test.so
	scheme --libdirs target/main/scheme:target/test/scheme --program target/test/scheme/maf6502-test.so
	echo '(maf6502-assembler-test)' | scheme -q --libdirs target/main/scheme:target/test/scheme target/test/scheme/parser-test.so

clean :
	rm -rf target/*

