/* perf.c
 *
 * COPYRIGHT (c) 2009 The Manticore Project (http://manticore.cs.uchicago.edu)
 * All rights reserved.
 *
 */

#include "manticore-rt.h"
#include "options.h"
#include "perf.h"

#include <sys/syscall.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/uio.h>
#include <errno.h>

#include <linux/unistd.h>

#include <assert.h>
#include <unistd.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>

#include "vproc.h"

static bool	ReportStatsFlg = false;	// true for report enabled
static bool	CSVStatsFlg = false;	// true for CSV-format report
static bool	SMLStatsFlg = false;	// true for SML-format report

#define __NR_perf_counter_open 298

static inline int
perf_counter_open(struct perf_counter_attr *attr,
                      pid_t pid, int cpu, int group_fd,
                      unsigned long flags)
{
    return syscall(__NR_perf_counter_open, attr, pid, cpu, group_fd, flags);
}

/* process command-line args */
void ParsePerfOptions (Options_t *opts)
{
    const char *report = GetStringEqOpt (opts, "-perf", "summary");

    if (report != 0) {
        ReportStatsFlg = true;
        if (strstr(report, "csv") != 0) CSVStatsFlg = true;
        if (strstr(report, "sml") != 0) SMLStatsFlg = true;
    }
}

/* InitPerfCounters:
 *
 * Initialize the perf counters for the given vproc.
 */
void InitPerfCounters (VProc_t *vp)
{
    struct perf_counter_attr attr;
    int cpu = -1;

    memset (&attr, 0, sizeof(attr));
    attr.sample_type = PERF_SAMPLE_IP | PERF_SAMPLE_TID;
    attr.freq = 0;
    
    attr.type = PERF_TYPE_RAW;
    attr.config = 0xF74E0;
    attr.inherit = 0;
    attr.size = sizeof(struct perf_counter_attr);

    vp->refFD = perf_counter_open (&attr, 0, cpu, -1, 0);

    attr.config = 0xF74E1;
    vp->missFD  = perf_counter_open (&attr, 0, cpu, vp->refFD, 0);
}

void ReportPerfCounters () {
    if (! ReportStatsFlg)
	return;

    FILE *StatsOutFile = stderr;

    if (CSVStatsFlg) {
        if ((StatsOutFile = fopen ("perf.csv", "w")) == 0)
            StatsOutFile = stderr;

        for (int i = 0;  i < NumVProcs;  i++) {
            VProc_t *vp = VProcs[i];
            unsigned long long count1, count2;
            
            read(vp->missFD, &count1, sizeof(count1));
            read(vp->refFD, &count2, sizeof(count2));
            
            fprintf(StatsOutFile, "%d, %d, %d\n", i, count1, count2);
        }

        fclose (StatsOutFile);
    }
    else if (SMLStatsFlg) {
        if ((StatsOutFile = fopen ("perf.sml", "w")) == 0)
            StatsOutFile = stderr;

        for (int i = 0;  i < NumVProcs;  i++) {
            VProc_t *vp = VProcs[i];
            unsigned long long count1, count2;
            
            read(vp->missFD, &count1, sizeof(count1));
            read(vp->refFD, &count2, sizeof(count2));
            
            fprintf (StatsOutFile,
                     "PST{processor=%d, \n\
                      miss=%d, \n               \
                      references=%d} ::\n",
                     i, count1, count2);
        }

        fprintf (StatsOutFile, "nil\n");

        fclose (StatsOutFile);
    }
    else {
        for (int i = 0;  i < NumVProcs;  i++) {
            VProc_t *vp = VProcs[i];
            unsigned long long count1, count2;
            
            read(vp->missFD, &count1, sizeof(count1));
            read(vp->refFD, &count2, sizeof(count2));
            
            fprintf(stderr, "vproc %d, %d misses, %d reads\n", i, count1, count2);
        }
    }

}
