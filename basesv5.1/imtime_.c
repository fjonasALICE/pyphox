#include <sys/times.h>
#include <unistd.h>
long imtime_() 
{
	long    sysconf();
	long    ticks;

        ticks = sysconf(_SC_CLK_TCK);
	return ticks;
}
