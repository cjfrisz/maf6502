all : run-tests

target-dirs :
	mkdir -p target/{main,test}/scheme target/dependency

submodules/chez-srfi : target-dirs
	git submodule init
	git submodule update

link-srfi-dirs : submodules/chez-srfi
	( cd submodules/chez-srfi && scheme --script link-dirs.chezscheme.sps )

target/test/scheme/srfi : link-srfi-dirs submodules/chez-srfi target-dirs
	ln -fs ../../../submodules/chez-srfi target/test/scheme/srfi

target/main/scheme/maf6502.so : src/main/scheme/maf6502.ss target-dirs
	echo '(begin\
                (compile-profile #t)\
                (generate-allocation-counts #t)\
                (generate-instruction-counts #t)\
                (compile-file "src/main/scheme/maf6502.ss" "target/main/scheme/maf6502.so"))' | scheme -q --compile-imported-libraries

target/test/scheme/maf6502-test.so : src/test/scheme/maf6502-test.ss target/main/scheme/maf6502.so target/test/scheme/srfi target-dirs
	echo '(compile-file "src/test/scheme/maf6502-test.ss" "target/test/scheme/maf6502-test.so")'\
	  | scheme -q --compile-imported-libraries --libdirs target/main/scheme:target/test/scheme

repl : src/test/scheme/maf6502-repl.ss target/main/scheme/maf6502.so
	scheme --libdirs target/main/scheme src/test/scheme/maf6502-repl.ss

run-tests : target/test/scheme/maf6502-test.so src/main/scheme/maf6502-assembler.ss
	scheme --debug-on-exception --libdirs target/main/scheme:target/test/scheme --program target/test/scheme/maf6502-test.so
	( cd src/main/scheme ; echo '(maf6502-assembler-test)' | scheme -q maf6502-assembler.ss )

clean :
	rm -rf target/*

