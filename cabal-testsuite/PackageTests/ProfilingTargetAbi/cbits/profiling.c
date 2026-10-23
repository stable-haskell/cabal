int profiling_enabled(void)
{
#if defined(PROFILING)
    return 1;
#else
    return 0;
#endif
}
