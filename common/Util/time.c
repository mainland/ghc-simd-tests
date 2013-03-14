#if defined(_WIN32)
#include <Windows.h>
#else
#include <time.h>
#endif

double getCPUTime(void)
{
#if defined(_WIN32)
    FILETIME   dummyTime;
    FILETIME   userTime;
    SYSTEMTIME userSystemTime;

    if (GetProcessTimes(GetCurrentProcess(), &dummyTime, &dummyTime, &dummyTime, &userTime) == 0)
        return -1;

    if (FileTimeToSystemTime(&userTime, &userSystemTime) == 0)
        return -1;

    return ((double) userSystemTime.wHour) * 3600.0 +
           ((double) userSystemTime.wMinute) * 60.0 +
           ((double) userSystemTime.wSecond) +
           ((double) userSystemTime.wMilliseconds) / 1e3.0;
#else /* !defined(_WIN32) */
    struct timespec ts;

    if (clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts) != 0)
        return -1;

    return ((double) ts.tv_sec) + ((double) ts.tv_nsec) / 1e9;
#endif /* !defined(_WIN32) */
}

double getWallTime(void)
{
#if defined(_WIN32)
    FILETIME       tm;
    ULARGE_INTEGER t;

    /*
     * Time is given in 100-nanosecond intervals. Crazy!
     *
     * See:
     *
     * http://msdn.microsoft.com/en-us/library/windows/desktop/ms724397%28v=vs.85%29.aspx
     * http://msdn.microsoft.com/en-us/library/windows/desktop/hh706895%28v=vs.85%29.aspx
     * http://msdn.microsoft.com/en-us/library/windows/desktop/ms724284%28v=vs.85%29.aspx
     */
#if defined(NTDDI_WIN8) && NTDDI_VERSION >= NTDDI_WIN8
    GetSystemTimePreciseAsFileTime(&tm);
#else
    GetSystemTimeAsFileTime(&tm);
#endif

    t.LowPart  = tm.dwLowDateTime;
    t.HighPart = tm.dwHighDateTime;

    return ((double) t.QuadPart) / 1e7;
#else /* !defined(_WIN32) */
    struct timespec ts;

    if (clock_gettime(CLOCK_MONOTONIC_RAW, &ts) != 0)
        return -1;

    return ((double) ts.tv_sec) + ((double) ts.tv_nsec) / 1e9;
#endif
}
