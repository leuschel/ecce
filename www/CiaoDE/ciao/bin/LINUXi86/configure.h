#if !defined(ATOMGC)
#define ATOMGC
#endif

#if !defined(_REENTRANT)
#define _REENTRANT
#endif

#if !defined(THREADS)
#define THREADS
#endif

#if !defined(FOREIGN_FILES)
#define FOREIGN_FILES
#endif

#if !defined(LINUX)
#define LINUX
#endif

#if !defined(i86)
#define i86
#endif

#define BIGENDIAN 0
#define MallocBase 0x40000000
#define MIN_MEM_ALLOC 65536
#define USE_OWN_MALLOC 1
#define ENG_FLT_SIGNIF 15
#define ENG_FLT_ROUND 5e-07

#define IEEE754_MASK_NEGATIVE  0x80000000
#define IEEE754_INDEX_NEGATIVE 1
#define IEEE754_SHIFT_NEGATIVE 31
#define IEEE754_MASK_EXPONENT  0x7FF00000
#define IEEE754_INDEX_EXPONENT 1
#define IEEE754_SHIFT_EXPONENT 20
#define IEEE754_MASK_MANTISSA0  0x000FFFFF
#define IEEE754_INDEX_MANTISSA0 1
#define IEEE754_SHIFT_MANTISSA0 0
#define IEEE754_MASK_MANTISSA1  0xFFFFFFFF
#define IEEE754_INDEX_MANTISSA1 0
#define IEEE754_SHIFT_MANTISSA1 0
#define IEEE754_MANTISSA_LENGTH 52

#define USE_LONG_DOUBLE
#define IEEE854_MASK_NEGATIVE   0x00008000
#define IEEE854_INDEX_NEGATIVE  2
#define IEEE854_SHIFT_NEGATIVE  15
#define IEEE854_MASK_EXPONENT   0x00007FFF
#define IEEE854_INDEX_EXPONENT  2
#define IEEE854_SHIFT_EXPONENT  0
#define IEEE854_MASK_MANTISSA0_0  0x7FFFFFFF
#define IEEE854_INDEX_MANTISSA0_0 1
#define IEEE854_SPLIT_MANTISSA0_0 0
#define IEEE854_MASK_MANTISSA0_1  0x00000000
#define IEEE854_INDEX_MANTISSA0_1 0
#define IEEE854_SHIFT_MANTISSA0_1 0
#define IEEE854_MASK_MANTISSA1_0  0xFFFFFFFF
#define IEEE854_INDEX_MANTISSA1_0 0
#define IEEE854_SPLIT_MANTISSA1_0 0
#define IEEE854_MASK_MANTISSA1_1  0x00000000
#define IEEE854_INDEX_MANTISSA1_1 0
#define IEEE854_SHIFT_MANTISSA1_1 0
#define IEEE854_MANTISSA_LENGTH 63
