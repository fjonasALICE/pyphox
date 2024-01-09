#include <time.h>
/* long	iutime() */
/*main()*/
void uxdate_(year,mon,day,hour,min)
int	*year, *mon, *day, *hour, *min;
{
	struct	tm q;
        struct  tm *localtime();
        time_t  tp;
        time_t  mktime();
        time_t  time();
        char    *ctime();
	char    *date;
        

        time(&tp);
        date = ctime(&tp);
        q = *localtime(&tp);
        *year = q.tm_year;
        *mon  = q.tm_mon + 1;
        *day  = q.tm_mday;
        *hour = q.tm_hour;
        *min  = q.tm_min;

	return;
}
