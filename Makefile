build_unit:
	ghdl -a --std=08 source/*.vhd
	ghdl -a --std=08 test/*.vhd
	ghdl -e --std=08 $(unit)
	ghdl -r --std=08 $(unit) --vcd=$(unit).vcd