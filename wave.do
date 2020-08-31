add  wave  -noupdate -expand -group "TB top" "sim:/*"
add  wave  -noupdate -expand -group "DUT"    "sim:/dut/*"

configure wave -namecolwidth 300
configure wave -valuecolwidth 120
# update
