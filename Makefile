build_unit:
	ghdl -a source/*.vhd
	ghdl -a test/*.vhd
	ghdl -e $(unit)
	ghdl -r $(unit) --vcd=$(unit).vcd