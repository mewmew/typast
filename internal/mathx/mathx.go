package mathx

func NextPowerOfTwo(x uint64) uint64 {
	power := uint64(1)
	for power < x {
		power *= 2
		if power == 0 {
			panic("wrap around")
		}
	}
	return power
}
