#include <sys/times.h>
#include <unistd.h>
long ixtime_() 
{
	struct	tms q;
	long 	t,s;
	long    sysconf();
	long    ticks;
	float   uxtime;
	float   answer;

	times(&q);
        t = q.tms_utime + q.tms_cutime
           +q.tms_stime + q.tms_cstime;
	return t;
}
