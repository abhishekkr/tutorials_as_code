package sample

func NotEmptyLen(s []string) int {
	length := 0
	for _, elem := range s {
		if elem != "" {
			length += 1
		}
	}
	return length
}

func EmptyLen(s []string) int {
	length := 0
	for _, elem := range s {
		if elem == "" {
			length += 1
		}
	}
	return length
}
