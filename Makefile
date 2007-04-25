LLP=$(shell perl -MInline::Java=so_dirs)

all:

test:
	$(LLP) PERL_INLINE_JAVA_JNI=1 prove t


clean:
	rm -Rf _Inline
