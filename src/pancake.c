#include <stdio.h>

#define MAX_MAP ((1 << 11) - 1)

int
parse_int_list(char *input, int *output)
{
	return sscanf(input, "%d %d %d %d %d %d %d %d %d %d",
			output, output + 1, output + 2,
			output + 3, output + 4, output + 5,
			output + 6, output + 7, output + 8,
			output + 9);
}

int
to_key(int *values, int length)
{
	int key = 0, i;
	for (i = 0; i < length; ++i)
		if (i + 1 == values[i])
			key |= 1 << i;
	return key;
}

int
pancake_sort_count(int *left, int *right, int left_count, int right_count,
		int *left_map, int *right_map, int steps)
{
	return steps;
}

int
main(int argc, char **argv) {
	int order[10], input[10], steps;
	int order_count, input_count;
	int left_map[MAX_MAP], right_map[MAX_MAP];
	int i;

	if (argc != 3) {
		fprintf(stderr, "%s <order> <input>\n", argv[0]);
		return 1;
	}

	for (i = 0; i < MAX_MAP; ++i) {
		left_map[i] = 0;
		right_map[i] = 0;
	}

	left = to_key(
	order_count = parse_int_list(argv[1], order);
	input_count = parse_int_list(argv[2], input);
	printf("%d %d\n", order_count, input_count);

	if (order_count != input_count)
		return -1;

	steps = pancake_sort_count(order, input, order_count, input_count,
			right_map, left_map, 0);
	printf("%d\n", steps);

	return 0;
}
