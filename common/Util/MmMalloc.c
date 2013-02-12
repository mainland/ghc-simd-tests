#include <xmmintrin.h>

void *mm_malloc(size_t size, size_t alignment);
void mm_free(void* ptr);

void *mm_malloc(size_t size, size_t alignment)
{
    return _mm_malloc(size, alignment);
}


void mm_free(void* ptr)
{
    _mm_free(ptr);
}
