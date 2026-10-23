int tool_is_profiled(void)
{
#if defined(PROFILING)
    return 1;
#else
    return 0;
#endif
}
