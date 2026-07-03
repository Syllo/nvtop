#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/get_process_info.h"
#include "nvtop/time.h"

#include <ctype.h>
#include <dirent.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>
#include <qcom_dsp.h>

#define MAX_NSP_THERMAL_ZONES 8

struct gpu_info_qcom_npu {
	struct gpu_info base;
	struct qcom_dsp_ctx *ctx;
	char nsp_thermal_paths[MAX_NSP_THERMAL_ZONES][64];
	int nsp_thermal_count;
};

static struct gpu_info_qcom_npu *qcom_npu_info = NULL;
extern struct gpu_vendor gpu_vendor_qcom_npu;

static bool gpuinfo_qcom_npu_init(void) {
	return true;
}

static void gpuinfo_qcom_npu_shutdown(void) {
	if (qcom_npu_info) {
		for (unsigned i = 0; i < qcom_npu_info->base.processes_count; i++) {
			free(qcom_npu_info->base.processes[i].cmdline);
			free(qcom_npu_info->base.processes[i].user_name);
		}
		free(qcom_npu_info->base.processes);
		if (qcom_npu_info->ctx)
			qcom_dsp_close(qcom_npu_info->ctx);
	}
	free(qcom_npu_info);
	qcom_npu_info = NULL;
}

static const char *gpuinfo_qcom_npu_last_error_string(void) {
	return "QCOM-NPU error";
}

static void add_qcom_npu_chip(struct list_head *devices, unsigned *count) {
	struct gpu_info_qcom_npu *this_npu = &qcom_npu_info[*count];

	this_npu->base.vendor = &gpu_vendor_qcom_npu;
	snprintf(this_npu->base.pdev, PDEV_LEN, "QCOM-NPU%d", *count);
	list_add_tail(&this_npu->base.list, devices);
	this_npu->base.processes_count = 0;
	this_npu->base.processes = NULL;
	this_npu->base.processes_array_size = 0;
	*count += 1;
}

static void find_nsp_thermal_zones(struct gpu_info_qcom_npu *npu) {
	DIR *dir = opendir("/sys/class/thermal");
	struct dirent *ent;

	npu->nsp_thermal_count = 0;
	if (!dir)
		return;

	while ((ent = readdir(dir)) != NULL) {
		if (npu->nsp_thermal_count >= MAX_NSP_THERMAL_ZONES)
			break;
		if (strncmp(ent->d_name, "thermal_zone", 12) != 0)
			continue;

		char type_path[80];
		snprintf(type_path, sizeof(type_path), "/sys/class/thermal/%.54s/type", ent->d_name);

		char type[32] = {0};
		FILE *fp = fopen(type_path, "r");
		if (!fp)
			continue;
		fgets(type, sizeof(type), fp);
		fclose(fp);
		type[strcspn(type, "\n")] = '\0';

		if (strncmp(type, "nsp", 3) != 0)
			continue;

		snprintf(npu->nsp_thermal_paths[npu->nsp_thermal_count],
			 sizeof(npu->nsp_thermal_paths[0]),
			 "/sys/class/thermal/%.38s/temp", ent->d_name);
		npu->nsp_thermal_count++;
	}
	closedir(dir);
}

static bool gpuinfo_qcom_npu_get_device_handles(struct list_head *devices_list, unsigned *count) {
	*count = 0;
	qcom_npu_info = calloc(1, sizeof(struct gpu_info_qcom_npu));
	if (!qcom_npu_info)
		return false;

	qcom_npu_info->ctx = qcom_dsp_open(DSP_NPU0);
	if (!qcom_npu_info->ctx) {
		free(qcom_npu_info);
		qcom_npu_info = NULL;
		return false;
	}

	add_qcom_npu_chip(devices_list, count);
	find_nsp_thermal_zones(qcom_npu_info);

	return true;
}

static void gpuinfo_qcom_npu_populate_static_info(struct gpu_info *_gpu_info) {
	struct gpu_info_qcom_npu *gpu_info = container_of(_gpu_info, struct gpu_info_qcom_npu, base);
	struct gpuinfo_static_info *static_info = &gpu_info->base.static_info;
	unsigned int arch_ver;

	static_info->integrated_graphics = true;
	static_info->encode_decode_shared = false;
	RESET_ALL(static_info->valid);

	arch_ver = qcom_dsp_prof_get_q6_arch_version(gpu_info->ctx);
	if (arch_ver) {
		snprintf(static_info->device_name, sizeof(static_info->device_name),
			 "Qualcomm Hexagon v%x NPU", arch_ver);
		SET_VALID(gpuinfo_device_name_valid, static_info->valid);
	}
}

static int read_int_from_file(const char *path) {
	int value = 0;
	FILE *fp = fopen(path, "r");
	if (fp) {
		fscanf(fp, "%d", &value);
		fclose(fp);
	}
	return value;
}

static int set_gpuinfo_dynamic_memory(struct gpuinfo_dynamic_info *dynamic_info) {
	FILE *fp = fopen("/proc/meminfo", "r");
	unsigned long mem_total = 0, mem_available = 0;
	char line[256];

	if (!fp)
		return -1;

	while (fgets(line, sizeof(line), fp)) {
		if (sscanf(line, "MemTotal: %lu kB", &mem_total) == 1) {
			mem_total *= 1024;
			SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, mem_total);
		} else if (sscanf(line, "MemAvailable: %lu kB", &mem_available) == 1) {
			mem_available *= 1024;
			SET_GPUINFO_DYNAMIC(dynamic_info, free_memory, mem_available);
		}
	}
	fclose(fp);

	if (mem_total > 0 && mem_available > 0) {
		SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, mem_total - mem_available);
		SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate,
		                    (dynamic_info->total_memory - dynamic_info->free_memory) * 100 / dynamic_info->total_memory);
	}
	return 0;
}

static void gpuinfo_qcom_npu_refresh_dynamic_info(struct gpu_info *_gpu_info) {
	struct gpu_info_qcom_npu *gpu_info = container_of(_gpu_info, struct gpu_info_qcom_npu, base);
	struct gpuinfo_dynamic_info *dynamic_info = &gpu_info->base.dynamic_info;
	struct sysmon_query_prof_data *data;
	int no_metrics = 0;

	data = qcom_dsp_get_prof_data(gpu_info->ctx, &no_metrics);
	if (!data || no_metrics <= 0) {
		fprintf(stderr, "qcom_dsp_get_prof_data failed\n");
		return;
	}

	float q6_util  = qcom_dsp_prof_get_q6_utilization(data);
	float hvx_util = qcom_dsp_prof_get_hvx_utilization(data);
	float hmx_util = qcom_dsp_prof_get_hmx_utilization(data);
	SET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate, (unsigned int)q6_util);
	SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed, qcom_dsp_prof_get_q6_clock(data) / 1000);
	SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed_max, qcom_dsp_prof_get_q6_clock(data) / 1000);
	SET_GPUINFO_DYNAMIC(dynamic_info, hvx_util_rate, (unsigned int)hvx_util);
	SET_GPUINFO_DYNAMIC(dynamic_info, hmx_util_rate, (unsigned int)hmx_util);
	SET_VALID(gpuinfo_hvx_util_rate_valid, dynamic_info->valid);
	SET_VALID(gpuinfo_hmx_util_rate_valid, dynamic_info->valid);

	int max_temp = 0;
	for (int i = 0; i < gpu_info->nsp_thermal_count; i++) {
		int t = read_int_from_file(gpu_info->nsp_thermal_paths[i]);
		if (t > max_temp)
			max_temp = t;
	}
	if (gpu_info->nsp_thermal_count > 0)
		SET_GPUINFO_DYNAMIC(dynamic_info, gpu_temp, max_temp / 1000);

	set_gpuinfo_dynamic_memory(dynamic_info);
}

static void gpuinfo_qcom_npu_get_running_processes(struct gpu_info *_gpu_info) {
	(void)_gpu_info;
}

struct gpu_vendor gpu_vendor_qcom_npu = {
	.init = gpuinfo_qcom_npu_init,
	.shutdown = gpuinfo_qcom_npu_shutdown,
	.last_error_string = gpuinfo_qcom_npu_last_error_string,
	.get_device_handles = gpuinfo_qcom_npu_get_device_handles,
	.populate_static_info = gpuinfo_qcom_npu_populate_static_info,
	.refresh_dynamic_info = gpuinfo_qcom_npu_refresh_dynamic_info,
	.refresh_running_processes = gpuinfo_qcom_npu_get_running_processes,
	.name = "QCOM-NPU",
	.unit_name = "NPU",
};

__attribute__((constructor)) static void init_extract_gpuinfo_qcom_npu(void) {
	register_gpu_vendor(&gpu_vendor_qcom_npu);
}
