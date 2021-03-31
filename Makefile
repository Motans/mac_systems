build_unit:
	ghdl -a source/*.vhd
	ghdl -a test/*.vhd
	ghdl -e $(unit)
	ghdl -r  --std=08 $(unit) --vcd=$(unit).vcd