#include <sys/times.h>
#include <unistd.h>
float	uxtime_() 
{
	struct	tms q;
	long 	t,s;
	long    sysconf();
	long    ticks;
	float   uxtime;


        ticks = sysconf(_SC_CLK_TCK);
	times(&q);
        t = q.tms_utime + q.tms_cutime
           +q.tms_stime + q.tms_cstime;
	return (float) t/ (float) ticks;
}
